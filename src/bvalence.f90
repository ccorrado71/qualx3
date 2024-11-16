module bond_valence
 
   implicit none

   integer, parameter, private :: BV_NOT_FOUND=1, BV_FOUND=2, BV_NOT_EXACT=3, BV_FOUND_UNK=4
   type bond_valence_t
     integer :: z1   = 0, z2   = 0  ! atomic numbers
     integer :: val1 = 0, val2 = 0  ! valence 
     real    :: ro   = 0.           ! bond valence parameter Ro
     real    :: b    = 0.           ! bond valence parameter B
     integer :: info = BV_NOT_FOUND ! Not found in databese (BV_NOT_FOUND), 
                                    ! found in database (BV_FOUND), found but ox. state is different (BV_NOT_EXACT)
     real    :: rmin=1.0,rmax=3.0   ! bond window 

   contains

     procedure :: prn

   end type 

   integer, parameter, private :: NUM_BVAL = 1370
   type(bond_valence_t), dimension(NUM_BVAL), private :: bval_table   ! table of bond valance parameters
!
!  See D. Altermatt and I.D. Brown, Acta Cryst. (1985). B41,240-244
   integer, parameter, private :: TYPE_ATOM_A=1,TYPE_ATOM_Ax=2,TYPE_ATOM_CT=3,  &
                                  TYPE_ATOM_CTx=4,TYPE_ATOM_CM=5,TYPE_ATOM_CMx=6,TYPE_ATOM_B=7
!                                                                    A   Ax   CT  CTx   CM  CMx    B
   integer, dimension(7,7), parameter, private :: talgo = reshape ([ 0,   0,   1,   1,   1,   1,   6, &   ! A
                                                                     0,   2,   1,   1,   1,   1,   6, &   ! Ax
                                                                     1,   1,   0,   0,   0,   4,   6, &   ! CT
                                                                     1,   1,   0,   3,   0,   4,   6, &   ! CTx
                                                                     1,   1,   0,   0,   0,   0,   6, &   ! CM
                                                                     1,   1,   4,   4,   0,   5,   6, &   ! CMx
                                                                     6,   6,   6,   6,   6,   6,   6  &   ! B
                                                                     ],shape(talgo))   ! A
   integer, parameter, private :: UNKNOWOX = 9  !unspecified, oxidation state

contains

   type(bond_valence_t) function bvparam_lookup(z1,z2,charge1,charge2) result(bv)
!
!  Lookup bond valence parameters ro,b from Z and charge
!
   integer, intent(in) :: z1,z2,charge1,charge2
   integer             :: i, val1, val2, z1tab,z2tab
   integer             :: diffval, diffval_min, diff1, diff2
!
   bv%info = BV_NOT_FOUND
   diffval_min = huge(1)
   i = 0
   loop_table: do 
      i = i + 1
      if (i > NUM_BVAL) exit loop_table
      if (bval_table(i)%z1 == z1 .and. bval_table(i)%z2 == z2) then
          z1tab = z1
          z2tab = z2
          val1 = charge1
          val2 = charge2
      elseif (bval_table(i)%z1 == z2 .and. bval_table(i)%z2 == z1) then
          z1tab = z2
          z2tab = z1
          val1 = charge2
          val2 = charge1
      else
          cycle
      endif
!
      diff1 = abs(bval_table(i)%val1 - val1)
      diff2 = abs(bval_table(i)%val2 - val2)
      !diffval = abs(bval_table(i)%val1 - val1) + abs(bval_table(i)%val2 - val2)
      diffval = diff1 + diff2
      !corr bv = bval_table(i)
      bv = bval_table_mat(i,z1)
      if (diffval == 0) then
!
!         Same ox. state 
          bv%info = BV_FOUND
          return
      else
          if ((diff1 == 0 .and. bval_table(i)%val2 == UNKNOWOX) .or.  &
              (diff2 == 0 .and. bval_table(i)%val1 == UNKNOWOX)) then
               bv%info = BV_FOUND_UNK  ! notify presence of unknown state
          else
               bv%info = BV_NOT_EXACT
          endif
!
!         Find the closest oc. state
          !bv%info = BV_NOT_EXACT
          diffval_min = diffval
          do 
            i = i + 1
            if (i > NUM_BVAL) exit loop_table
            if (bval_table(i)%z1 == z1tab) then
                if (bval_table(i)%z2 == z2tab) then
                    diff1 = abs(bval_table(i)%val1 - val1)
                    diff2 = abs(bval_table(i)%val2 - val2)
                    !diffval = abs(bval_table(i)%val1 - val1) + abs(bval_table(i)%val2 - val2)
                    diffval = diff1 + diff2
                    if (diffval < diffval_min) then
                        !corr bv = bval_table(i)
                        bv = bval_table_mat(i,z1)
                        if (diffval == 0) then
                            bv%info = BV_FOUND
                            return
                        else
                            if ((diff1 == 0 .and. bval_table(i)%val2 == UNKNOWOX) .or.  &
                                (diff2 == 0 .and. bval_table(i)%val1 == UNKNOWOX)) then
                                 bv%info = BV_FOUND_UNK  ! notify presence of unknown state
                            else
                                 bv%info = BV_NOT_EXACT
                            endif
                            diffval_min = diffval
                        endif
                    endif
                endif
            else
                exit loop_table
            endif
          enddo
      endif
   enddo loop_table
!
   if (bv%info == BV_NOT_FOUND) then
       bv%z1 = z1
       bv%z2 = z2
       bv%rmax = 0
   endif
!
   end function bvparam_lookup

!---------------------------------------------------------------------------------------------------------------------

   function bval_table_mat(arraypos,z1) result(bv)
   integer, intent(in)  :: arraypos,z1
   type(bond_valence_t) :: bv
!
   bv = bval_table(arraypos)
   if (bval_table(arraypos)%z1 == z1) return  ! swap not necessary
!
!  swap Z and val
   bv%z1 = bval_table(arraypos)%z2
   bv%z2 = bval_table(arraypos)%z1
   bv%val1 = bval_table(arraypos)%val2
   bv%val2 = bval_table(arraypos)%val1
!
   end function bval_table_mat

!---------------------------------------------------------------------------------------------------------------------
 
   subroutine prn(bv,kpr,head)
   USE elements
   class(bond_valence_t), intent(in) :: bv
   integer, intent(in)               :: kpr
   logical, intent(in), optional     :: head
   character(len=6)                  :: str1,str2
!
   !write(kpr,'(4(i10),2(f10.3))')bv%z1,bv%z2,bv%val1,bv%val2,bv%ro,bv%b
   if (present(head)) then
       if (head) then
           write(kpr,'(/2x,72("-")/,2x,a/,2x,a/,2x,72("-"))')                 &
                      '            Bond Valence Parameters and bond windows', &
                      'Atom1           Atom2                  Ro        B       rmin      rmax'
       endif
   endif
   str1 = elem_string(bv%z1,bv%val1)
   str2 = elem_string(bv%z2,bv%val2)
   write(kpr,'(2x,a,10x,a,10x,4(f10.3))')str1,str2,bv%ro,bv%b,bv%rmin,bv%rmax
!
   end subroutine prn

!---------------------------------------------------------------------------------------------------------------------

   subroutine bv_compute_table(elem,bvtab)
   USE elements
   type(element_type), dimension(:), allocatable :: elem
   type(bond_valence_t), dimension(:,:)          :: bvtab
   integer                                       :: i,j,nel
!
   nel = numelem(elem)
   do i=1,nel
      do j=i,nel
         call bv_table_set(bvtab,elem,i,j)
      enddo
   enddo
!
   end subroutine bv_compute_table

!---------------------------------------------------------------------------------------------------------------------

   real function bv_cutoff(bvtab)
   type(bond_valence_t), dimension(:,:) :: bvtab
!
   bv_cutoff = maxval(bvtab(:,:)%rmax) 
!
!  test for 0 value
   if (abs(bv_cutoff) < epsilon(1.0)) bv_cutoff = 3.0
!
   end function bv_cutoff

!---------------------------------------------------------------------------------------------------------------------

   subroutine bv_table_set(bvtab,elem,i,j)
!
!  Set element i,j in the bond valence table parameters
!
   USE elements
   USE atom_type_util
   type(element_type), dimension(:), allocatable :: elem
   type(bond_valence_t), dimension(:,:)          :: bvtab
   integer, intent(in)                           :: i,j
   integer                                       :: oxi,oxj,typeai,typeaj,oxii,oxjj
   real                                          :: bvmin,bvmax,rmax
   integer                                       :: oxa,oxc,za,zc,ga
   integer                                       :: oxcmx,oxct,zcmx
!
!  if oxidation number is undefined, set the most common
   oxi = elem(i)%charge
   if (oxi == 0) oxi = oxidation_number(elem(i)%z)
   oxj = elem(j)%charge   
   if (oxj == 0) oxj = oxidation_number(elem(j)%z)
   bvtab(i,j) = bvparam_lookup(elem(i)%z,elem(j)%z,oxi,oxj)
   if (bvtab(i,j)%info /= BV_NOT_FOUND) then
!
!      Set range rmin - rmax
       oxii = bvtab(i,j)%val1
       if (oxii == UNKNOWOX) oxii = oxi
       oxjj = bvtab(i,j)%val2
       if (oxjj == UNKNOWOX) oxjj = oxj
       typeai = type_of_ion(bvtab(i,j)%z1,oxii)
       typeaj = type_of_ion(bvtab(i,j)%z2,oxjj)
!corr       if (talgo(typeai,typeaj) == 0) then
!corr           bvtab(i,j)%rmax = 0
!corr       else
!corr           bvtab(i,j)%rmax = bond_distance_max(elem(i)%z,elem(j)%z)
!corr           bvtab(i,j)%rmin = bond_distance_min(elem(i)%z,elem(j)%z)
!corr       endif
       select case (talgo(typeai,typeaj))
         case (0) 
           bvtab(i,j)%rmin = 1.0
           bvtab(i,j)%rmax = 0
         case (1)
           if (is_anion(typeai)) then
               oxa = oxii
               oxc = oxjj
               ga = group_number(bvtab(i,j)%z1)
               za = bvtab(i,j)%z1
               zc = bvtab(i,j)%z2
           else
               oxa = oxjj
               oxc = oxii
               ga = group_number(bvtab(i,j)%z2)
               za = bvtab(i,j)%z2
               zc = bvtab(i,j)%z1
           endif
           if (ga == 5 .or. ga == 6) then
               bvmin = 0.1    ! Nb,Ta,Mo,W
           else
               bvmin = 0.038*oxc
           endif
           if (zc == C_at .and. (za == N_at .or. za == O_at)) then
               bvmax = 3.6    ! bond is C-O or C-N
           elseif (zc == H_at) then
               bvmax = 2.0    ! Cation is hydrogen
           else
               bvmax = (1+0.1*ga)*abs(oxa)
           endif
           bvtab(i,j)%rmax = bv_to_dist(bvmin,bvtab(i,j))
           bvtab(i,j)%rmin = bv_to_dist(bvmax,bvtab(i,j))
!
!          Corrado add this check for large rmax (e.g.: O-H distance)
           rmax = elem(i)%w_radius+elem(j)%w_radius
           if (bvtab(i,j)%rmax > rmax) then
               bvtab(i,j)%rmax = rmax
!cor               write(0,*)'Eli=',trim(elem(i)%lab),typeai,' Elj=',trim(elem(j)%lab),rmax,bvtab(i,j)%rmax
           endif
           if (bvtab(i,j)%rmin < 0) bvtab(i,j)%rmin = 0.6
         case (2)
           bvmin = 0.5
           bvmax = 3.0
           bvtab(i,j)%rmax = bv_to_dist(bvmin,bvtab(i,j))
           bvtab(i,j)%rmin = bv_to_dist(bvmax,bvtab(i,j))
         case (3,5)
           bvmin = 0.15 * max(oxii,oxjj)
           bvmax = 2.0 * min(oxii,oxjj)
           bvtab(i,j)%rmax = bv_to_dist(bvmin,bvtab(i,j))
           bvtab(i,j)%rmin = bv_to_dist(bvmax,bvtab(i,j))
         case (4)
           if (typeai == TYPE_ATOM_CMx) then
               oxcmx = oxii
               oxct = oxjj
               zcmx = bvtab(i,j)%z1
           else
               oxcmx = oxjj
               oxct = oxii
               zcmx = bvtab(i,j)%z2
           endif
           if (zcmx == C_at .and. oxcmx == 2) then  ! if CM* is C(+2)
               bvmin = 0.038 * oxct
               bvmax = 3.6
           else
               bvmin = 0.38 * oxcmx
               bvmax = 1.5
           endif
         case (6)
           bvtab(i,j)%rmin = 1.0
           bvtab(i,j)%rmax = 3.4
       end select
       !write(0,*)'G=',group_number(bvtab(i,j)%z1),group_number(bvtab(i,j)%z2)
       !write(0,*)'Eli=',trim(elem(i)%lab),typeai,' Elj=',trim(elem(j)%lab),typeaj,talgo(typeai,typeaj)
   endif
!
   bvtab(j,i) = bvtab(i,j)
   bvtab(j,i)%z1 = bvtab(i,j)%z2
   bvtab(j,i)%val1 = bvtab(i,j)%val2
   bvtab(j,i)%z2 = bvtab(i,j)%z1
   bvtab(j,i)%val2 = bvtab(i,j)%val1
   
   end subroutine bv_table_set

!---------------------------------------------------------------------------------------------------------------------

   real function bv_to_dist(bv,bvtab)
   real, intent(in)                 :: bv
   type(bond_valence_t), intent(in) :: bvtab
!
   bv_to_dist = bvtab%ro-log(bv)*bvtab%b
!
   end function bv_to_dist

!---------------------------------------------------------------------------------------------------------------------

   logical function is_anion(typei)
   integer, intent(in) :: typei
   is_anion = typei == TYPE_ATOM_A .or. typei == TYPE_ATOM_Ax
   end function is_anion

!---------------------------------------------------------------------------------------------------------------------

   subroutine bvtable_check(elem,bvtab)
!
!  Check on the valence bond table if an element is involved in more bonds with different oxidation states
!
   use connect_mod
   use elements
   use arrayutil
   type(element_type), dimension(:), allocatable, intent(in)        :: elem
   type(bond_valence_t), dimension(:,:), allocatable, intent(inout) :: bvtab
   type(bond_valence_t), dimension(:), allocatable                  :: bvec
   integer                                                          :: i,j,nv,val
   integer, dimension(:), allocatable                               :: oxvet,pvet
   integer, parameter                                               :: BV_FOUND1 = 2
   logical                                                          :: modf
!
   modf = .false.
   call bvtable_tovec(bvtab,bvec)
   call new_array(oxvet,size(bvec))
   call new_array(pvet,size(bvec))
   do i=1,numelem(elem)
      nv = 0
!
!     loop on bond table to find all bvec containing the element i
      do j=1,size(bvec)
         if (bvec(j)%rmax == 0) cycle
         if (elem(i)%z == bvec(j)%z1 .or. elem(i)%z == bvec(j)%z2) then
             if (elem(i)%z == bvec(j)%z1) then
                 val = bvec(j)%val1
             else
                 val = bvec(j)%val2
             endif
             if (val /= 0 .and. val /= 9) then
                 nv = nv + 1
                 oxvet(nv) = val
                 pvet(nv) = j
             endif
             !call bvec(j)%prn(0,.false.)
         endif
      enddo
      if (nv > 1) then
          if (any(oxvet(2:nv)*oxvet(1) < 0)) then     ! verify for opposite sign
              !write(0,*)'VAL=',oxvet(:nv),' ACTION REQUIRED'
!
!             Find in oxvet bv par. exactly found
              if (any(bvec(pvet(:nv))%info == BV_FOUND1)) then
                  !write(0,*)'BV exactly found!'
                  do j=1,nv
                     if (bvec(pvet(j))%info /= BV_FOUND1 .and. bvec(pvet(j))%info /= BV_FOUND_UNK) then
                         bvec(pvet(j))%rmax = 0.0    ! unset this bv
                         modf = .true.
                     endif
                  enddo
                  !if (modf) write(0,*)'ACTION PERFORMED'
              !else
              !    write(0,*)'ACTION NOT PERFORMED'
              endif
          !else
          !    write(0,*)'VAL=',oxvet(:nv),' ACTION NOT REQUIRED'
          endif
      endif
   enddo
!
   if (modf) then
       call vec_tobvtable(bvec,bvtab)
   endif
!
   end subroutine bvtable_check

!---------------------------------------------------------------------------------------------------------------------

   integer function type_of_ion(z,ox)  result(ta)
   USE elements
   integer, intent(in) :: z,ox
   integer             :: gn
!
   if (ox == 0) then
       ta = TYPE_ATOM_B
       return
   endif
!
   gn = group_number(z)

   if (ox < 0) then
       if (ox > gn - 18) then
           ta = TYPE_ATOM_Ax
       else
           ta = TYPE_ATOM_A
       endif
   else
       if (gn > 2 .and. gn <= 12) then  ! transition metal cation (CT)
           if (ox < gn) then
               ta = TYPE_ATOM_CTx       ! CT in lower oxidation state
           else
               ta = TYPE_ATOM_CT
           endif
       else                             ! main group cation (CM)
           if (ox < gn-10) then           
               ta = TYPE_ATOM_CMx       ! CM in lower oxidation state
           else
               ta = TYPE_ATOM_CM
           endif
       endif
   endif
!
   end function type_of_ion

!---------------------------------------------------------------------------------------------------------------------

   subroutine print_bv_table(bvtab,kpr)
   type(bond_valence_t), dimension(:,:), intent(in) :: bvtab
   integer, intent(in)                              :: kpr
   integer                                          :: i,j
   logical                                          :: head
!
   head = .true.
   do i=1,size(bvtab,1)
      do j=i,size(bvtab,2)
         if (bvtab(i,j)%z1 /= 0) then
             call bvtab(i,j)%prn(kpr,head)
             head = .false.
         endif
      enddo
   enddo
!
   end subroutine print_bv_table

!---------------------------------------------------------------------------------------------------------------------

   subroutine bvtable_tovec(bvtab,bvec)
!
!  Compact table of bond valence parameters in a vector
!
   type(bond_valence_t), dimension(:,:), allocatable, intent(in) :: bvtab
   type(bond_valence_t), dimension(:), allocatable, intent(out)  :: bvec
   integer                                                       :: i,j,n,jc
!   
   if (.not.allocated(bvtab)) return
!
   n = size(bvtab,1)
   allocate(bvec(n*(n+1)/2))
   jc = 1
   do j=1,n
      do i=1,j
         bvec(jc+i-1) = bvtab(i,j) 
      enddo
      jc = jc + j
   enddo
!
   end subroutine bvtable_tovec

!---------------------------------------------------------------------------------------------------------------------

   subroutine vec_tobvtable(bvec,bvtab)
!
!  Convert vector of bond valence parameters in a table
!
   type(bond_valence_t), dimension(:), allocatable, intent(in)    :: bvec
   type(bond_valence_t), dimension(:,:), allocatable, intent(out) :: bvtab
   integer                                                        :: i,j,n,jc
!
   if (.not.allocated(bvec)) return
   n =  (sqrt(1.0+8*size(bvec)) - 1)/2 ! size of matrix
   allocate(bvtab(n,n))
!
   jc = 1
   do j=1,n
      do i=1,j-1
         bvtab(i,j) = bvec(jc+i-1) 
         bvtab(j,i) = bvtab(i,j)
      enddo
      bvtab(j,j) = bvec(jc+j-1) 
      jc = jc + j
   enddo
!
   end subroutine vec_tobvtable

!---------------------------------------------------------------------------------------------------------------------

   subroutine bvres_from_string(str,atom,res,err)
!
!  str contains: At1 val [sigma wei]
!
   USE rrestr
   USE errormod
   USE atom_type_util
   character(len=*), intent(in)                                   :: str
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(restraint_type), dimension(:), allocatable, intent(inout) :: res
   type(error_type), intent(out)                                  :: err
   integer, dimension(size(atom))                                 :: vatom
   integer                                                        :: natom,i,nval
   real, dimension(3)                                             :: val,defval = [-999.,0.3,1.0]
   type(restraint_type)                                           :: vbres
!
   call get_atoms_from_string(str,atom,vatom,natom,val,defval,nval,dupl=.true.,err=err)
   if (val(1) == defval(1)) then
       call err%set('Undefined valence')
   endif
   if (.not.err%signal) then
       allocate(vbres%na(1))
       vbres%code = RESBV
       vbres%targ = val(1)
       vbres%sigma = val(2)
       vbres%wei = val(3)
       do i=1,natom
          vbres%na(1) = vatom(i)
          call add_restraint_to_list(res,vbres)
       enddo
   endif
   !write(0,*)'VAL=',val,' AT=',atom(vatom(:natom))%lab
!
   end subroutine bvres_from_string

!---------------------------------------------------------------------------------------------------------------------

   subroutine bvpar_from_string(str,bvtab,elem,err)
!
!  str contains: El1 El2 Ro B [rmin rmax]
!
   USE errormod
   USE strutil
   USE elements
   character(len=*), intent(in)                  :: str   
   type(bond_valence_t), dimension(:,:), intent(inout) :: bvtab
   type(element_type), dimension(:), allocatable :: elem
   type(error_type), intent(out)                 :: err
   real, dimension(:), allocatable               :: vet
   integer                                       :: iv
   character(len=:), allocatable                 :: str1
   character(len=:),allocatable                  :: el1,el2
   integer :: kel1,kel2
!
   str1 = str
   call cutsta(str1,line2=el1)
   kel1 = is_element(elem,el1)
   if (kel1 == 0) then
       call err%set('Error reading '//trim(el1))
       return
   endif
   call cutsta(str1,line2=el2)
   kel2 = is_element(elem,el2)
   if (kel2 == 0) then
       call err%set('Error reading '//trim(el2))
       return
   endif
   call getnum1(str1,vet,iv=iv,vsize=4)
   if (iv == 2 .or. iv == 4) then
       if (iv == 4) then
           if (vet(3) > vet(4)) then
               call err%set('Error')
               return
           endif
           bvtab(kel1,kel2)%rmin = vet(3)
           bvtab(kel1,kel2)%rmax = vet(4)   
           bvtab(kel2,kel1)%rmin = vet(3)
           bvtab(kel2,kel1)%rmax = vet(4)   
       endif
       bvtab(kel1,kel2)%ro = vet(1)   
       bvtab(kel1,kel2)%b = vet(2)   
       bvtab(kel2,kel1)%ro = vet(1)   
       bvtab(kel2,kel1)%b = vet(2)   
   else
       call err%set('Error')
   endif

   end subroutine bvpar_from_string

!---------------------------------------------------------------------------------------------------------------------

   subroutine compute_valence_atom(kat,atom,spg,gmat,bvtab,distmin,atomsym,val)
!   
!  Compute valence for atom kat
!
   USE atom_type_util
   USE spginfom
   USE cgeom
   integer, intent(in)                                  :: kat
   type(atom_type), dimension(:), intent(in)            :: atom
   type(spaceg_type), intent(in)                        :: spg
   real, dimension(3,3)                                 :: gmat
   type(bond_valence_t), dimension(:,:), intent(in)     :: bvtab   
   real, intent(in)                                     :: distmin
   real, dimension(size(atom),3,spg%nsymop), intent(in) :: atomsym ! size: natams,3,nsym
   real, dimension(3,spg%nsymop)                        :: xeq
   real, dimension(3,spg%nsymop*size(atom))             :: xfind
   integer, dimension(spg%nsymop*size(atom))            :: at_type
   real, intent(out)                                    :: val
   integer                                              :: nat
   integer                                              :: i,j,k,nzk,nzi,nfind
   real                                                 :: dd
   real, dimension(3)                                   :: ktra,xf
!
   val = 0
   nat = size(atom)
   if (kat > nat .or. nat == 0) return
   nzk = atom(kat)%kscatt()
!
   nfind = 0
   do i=1,nat
      nzi = atom(i)%kscatt()
      if (bvtab(nzi,nzk)%z1 > 0) then
          xeq = atomsym(i,:,:)
          loop_sym: do k=1,spg%nsymop
             if (k == 1 .and. i == kat) cycle
             call xdisteqs(atom(kat)%xc,xeq(:,k),gmat,dd,ktra)
             if (dd <= distmin .and. dd > 0.5) then
                 xf(:) = atom(kat)%xc + ktra
                 do j=1,nfind
                    if (at_type(j) == nzi) then
                        if (distanzaC(xfind(:,j),xf,gmat) < 0.1) cycle loop_sym
                    endif
                 enddo
                 nfind = nfind + 1
                 xfind(:,nfind) = xf
                 at_type(nfind) = nzi
                 !write(71,'(a,2f10.3,1x,a,3f10.3,i5)')'   VAL=',exp((bvtab(nzi,nzk)%ro - dd) / bvtab(nzi,nzk)%b),    &
                 !dd,trim(atom(i)%lab),atom(kat)%xc+ktra,kat
                 val = val + exp((bvtab(nzi,nzk)%ro - dd) / bvtab(nzi,nzk)%b)
             endif
          enddo loop_sym
      endif
   enddo
!
   end subroutine compute_valence_atom

!---------------------------------------------------------------------------------------------------------------------

   subroutine load_bval_table()
!
!  Load table of bond valence parameters. Table created from bvparm2016.cif
!  running the function read_bval_from_file
!
   bval_table(1) = bond_valence_t(89, 8, 3, -2, 2.24, 0.37)
   bval_table(2) = bond_valence_t(89, 9, 3, -1, 2.13, 0.37)
   bval_table(3) = bond_valence_t(89, 17, 3, -1, 2.63, 0.37)
   bval_table(4) = bond_valence_t(89, 35, 3, -1, 2.75, 0.40)
   bval_table(5) = bond_valence_t(47, 8, 1, -2, 1.842, 0.37)
   bval_table(6) = bond_valence_t(47, 16, 1, -2, 2.119, 0.37)
   bval_table(7) = bond_valence_t(47, 9, 1, -1, 1.80, 0.37)
   bval_table(8) = bond_valence_t(47, 17, 1, -1, 2.09, 0.37)
   bval_table(9) = bond_valence_t(47, 9, 2, -1, 1.79, 0.37)
   bval_table(10) = bond_valence_t(47, 9, 3, -1, 1.83, 0.37)
   bval_table(11) = bond_valence_t(47, 35, 9, -1, 2.22, 0.37)
   bval_table(12) = bond_valence_t(47, 53, 9, -1, 2.38, 0.37)
   bval_table(13) = bond_valence_t(47, 34, 9, -2, 2.26, 0.37)
   bval_table(14) = bond_valence_t(47, 52, 9, -2, 2.51, 0.37)
   bval_table(15) = bond_valence_t(47, 7, 9, -3, 1.85, 0.37)
   bval_table(16) = bond_valence_t(47, 15, 9, -3, 2.22, 0.37)
   bval_table(17) = bond_valence_t(47, 33, 9, -3, 2.30, 0.37)
   bval_table(18) = bond_valence_t(47, 1, 9, -1, 1.50, 0.37)
   bval_table(19) = bond_valence_t(13, 8, 3, -2, 1.651, 0.37)
   bval_table(20) = bond_valence_t(13, 16, 3, -2, 2.21, 0.37)
   bval_table(21) = bond_valence_t(13, 34, 3, -2, 2.27, 0.37)
   bval_table(22) = bond_valence_t(13, 52, 3, -2, 2.48, 0.37)
   bval_table(23) = bond_valence_t(13, 9, 3, -1, 1.545, 0.37)
   bval_table(24) = bond_valence_t(13, 17, 3, -1, 2.032, 0.37)
   bval_table(25) = bond_valence_t(13, 35, 3, -1, 2.20, 0.37)
   bval_table(26) = bond_valence_t(13, 53, 3, -1, 2.41, 0.37)
   bval_table(27) = bond_valence_t(13, 7, 3, -3, 1.79, 0.37)
   bval_table(28) = bond_valence_t(13, 15, 3, -3, 2.24, 0.37)
   bval_table(29) = bond_valence_t(13, 33, 3, -3, 2.30, 0.37)
   bval_table(30) = bond_valence_t(13, 1, 3, -1, 1.45, 0.37)
   bval_table(31) = bond_valence_t(95, 8, 3, -2, 2.11, 0.37)
   bval_table(32) = bond_valence_t(95, 9, 3, -1, 2.00, 0.37)
   bval_table(33) = bond_valence_t(95, 17, 3, -1, 2.48, 0.37)
   bval_table(34) = bond_valence_t(95, 35, 3, -1, 2.59, 0.40)
   bval_table(35) = bond_valence_t(95, 8, 4, -2, 2.08, 0.37)
   bval_table(36) = bond_valence_t(95, 9, 4, -1, 1.96, 0.40)
   bval_table(37) = bond_valence_t(95, 8, 5, -2, 2.07, 0.35)
   bval_table(38) = bond_valence_t(95, 9, 5, -1, 1.95, 0.40)
   bval_table(39) = bond_valence_t(95, 8, 6, -2, 2.05, 0.35)
   bval_table(40) = bond_valence_t(95, 9, 6, -1, 1.95, 0.40)
   bval_table(41) = bond_valence_t(95, 8, 5, -2, 2.12, 0.37)
   bval_table(42) = bond_valence_t(33, 16, 2, -2, 2.24, 0.37)
   bval_table(43) = bond_valence_t(33, 34, 2, -2, 2.38, 0.37)
   bval_table(44) = bond_valence_t(33, 8, 3, -2, 1.74, 0.50)
   bval_table(45) = bond_valence_t(33, 16, 3, -2, 2.272, 0.37)
   bval_table(46) = bond_valence_t(33, 34, 3, -2, 2.40, 0.37)
   bval_table(47) = bond_valence_t(33, 52, 3, -2, 2.65, 0.37)
   bval_table(48) = bond_valence_t(33, 9, 3, -1, 1.70, 0.37)
   bval_table(49) = bond_valence_t(33, 17, 3, -1, 2.16, 0.37)
   bval_table(50) = bond_valence_t(33, 35, 3, -1, 2.35, 0.37)
   bval_table(51) = bond_valence_t(33, 53, 3, -1, 2.58, 0.37)
   bval_table(52) = bond_valence_t(33, 6, 3, -4, 1.93, 0.37)
   bval_table(53) = bond_valence_t(33, 8, 5, -2, 1.767, 0.37)
   bval_table(54) = bond_valence_t(33, 16, 5, -2, 2.28, 0.37)
   bval_table(55) = bond_valence_t(33, 9, 5, -1, 1.620, 0.37)
   bval_table(56) = bond_valence_t(33, 17, 5, -2, 2.14, 0.37)
   bval_table(57) = bond_valence_t(79, 17, 1, -1, 2.02, 0.37)
   bval_table(58) = bond_valence_t(79, 53, 1, -1, 2.35, 0.37)
   bval_table(59) = bond_valence_t(79, 8, 3, -2, 1.89, 0.37)
   bval_table(60) = bond_valence_t(79, 16, 3, -2, 2.39, 0.35)
   bval_table(61) = bond_valence_t(79, 9, 3, -1, 1.89, 0.37)
   bval_table(62) = bond_valence_t(79, 17, 3, -1, 2.17, 0.37)
   bval_table(63) = bond_valence_t(79, 35, 3, -1, 2.32, 0.37)
   bval_table(64) = bond_valence_t(79, 53, 3, -1, 2.54, 0.37)
   bval_table(65) = bond_valence_t(79, 7, 3, -3, 1.94, 0.35)
   bval_table(66) = bond_valence_t(79, 9, 5, -1, 1.80, 0.37)
   bval_table(67) = bond_valence_t(79, 16, 9, -2, 2.03, 0.37)
   bval_table(68) = bond_valence_t(79, 34, 9, -2, 2.18, 0.37)
   bval_table(69) = bond_valence_t(79, 52, 9, -2, 2.41, 0.37)
   bval_table(70) = bond_valence_t(79, 35, 9, -1, 2.12, 0.37)
   bval_table(71) = bond_valence_t(79, 53, 9, -1, 2.34, 0.37)
   bval_table(72) = bond_valence_t(79, 7, 9, -3, 1.72, 0.37)
   bval_table(73) = bond_valence_t(79, 15, 9, -3, 2.14, 0.37)
   bval_table(74) = bond_valence_t(79, 33, 9, -3, 2.22, 0.37)
   bval_table(75) = bond_valence_t(79, 1, 9, -1, 1.37, 0.37)
   bval_table(76) = bond_valence_t(5, 8, 3, -2, 1.371, 0.37)
   bval_table(77) = bond_valence_t(5, 16, 3, -2, 1.815, 0.37)
   bval_table(78) = bond_valence_t(5, 34, 3, -2, 1.95, 0.37)
   bval_table(79) = bond_valence_t(5, 52, 3, -2, 2.20, 0.37)
   bval_table(80) = bond_valence_t(5, 9, 3, -1, 1.281, 0.37)
   bval_table(81) = bond_valence_t(5, 17, 3, -1, 1.74, 0.37)
   bval_table(82) = bond_valence_t(5, 35, 3, -1, 1.88, 0.37)
   bval_table(83) = bond_valence_t(5, 53, 3, -1, 2.10, 0.37)
   bval_table(84) = bond_valence_t(5, 7, 3, -3, 1.482, 0.37)
   bval_table(85) = bond_valence_t(5, 15, 3, -3, 1.920, 0.37)
   bval_table(86) = bond_valence_t(5, 33, 3, -3, 1.97, 0.37)
   bval_table(87) = bond_valence_t(5, 1, 3, -1, 1.14, 0.37)
   bval_table(88) = bond_valence_t(5, 6, 3, -4, 1.569, 0.28)
   bval_table(89) = bond_valence_t(5, 5, 3, 3, 1.402, 0.37)
   bval_table(90) = bond_valence_t(56, 8, 2, -2, 2.285, 0.37)
   bval_table(91) = bond_valence_t(56, 16, 2, -2, 2.769, 0.37)
   bval_table(92) = bond_valence_t(56, 34, 2, -2, 2.88, 0.37)
   bval_table(93) = bond_valence_t(56, 52, 2, -2, 3.08, 0.37)
   bval_table(94) = bond_valence_t(56, 9, 2, -1, 2.188, 0.37)
   bval_table(95) = bond_valence_t(56, 17, 2, -1, 2.69, 0.37)
   bval_table(96) = bond_valence_t(56, 35, 2, -1, 2.88, 0.37)
   bval_table(97) = bond_valence_t(56, 53, 2, -1, 3.13, 0.37)
   bval_table(98) = bond_valence_t(56, 7, 2, -3, 2.47, 0.37)
   bval_table(99) = bond_valence_t(56, 15, 2, -3, 2.88, 0.37)
   bval_table(100) = bond_valence_t(56, 33, 2, -3, 2.96, 0.37)
   bval_table(101) = bond_valence_t(56, 1, 2, -1, 2.22, 0.37)
   bval_table(102) = bond_valence_t(4, 8, 2, -2, 1.381, 0.37)
   bval_table(103) = bond_valence_t(4, 16, 2, -2, 1.83, 0.37)
   bval_table(104) = bond_valence_t(4, 34, 2, -2, 1.97, 0.37)
   bval_table(105) = bond_valence_t(4, 52, 2, -2, 2.21, 0.37)
   bval_table(106) = bond_valence_t(4, 9, 2, -1, 1.281, 0.37)
   bval_table(107) = bond_valence_t(4, 17, 2, -1, 1.76, 0.37)
   bval_table(108) = bond_valence_t(4, 35, 2, -1, 1.90, 0.37)
   bval_table(109) = bond_valence_t(4, 53, 2, -1, 2.10, 0.37)
   bval_table(110) = bond_valence_t(4, 7, 2, -3, 1.50, 0.37)
   bval_table(111) = bond_valence_t(4, 15, 2, -3, 1.95, 0.37)
   bval_table(112) = bond_valence_t(4, 33, 2, -3, 2.00, 0.37)
   bval_table(113) = bond_valence_t(4, 1, 2, -1, 1.11, 0.37)
   bval_table(114) = bond_valence_t(83, 8, 3, -2, 1.990, 0.48)
   bval_table(115) = bond_valence_t(83, 16, 3, -2, 2.570, 0.37)
   bval_table(116) = bond_valence_t(83, 34, 3, -2, 2.70, 0.35)
   bval_table(117) = bond_valence_t(83, 9, 3, -1, 1.99, 0.37)
   bval_table(118) = bond_valence_t(83, 17, 3, -1, 2.48, 0.37)
   bval_table(119) = bond_valence_t(83, 35, 3, -1, 2.597, 0.37)
   bval_table(120) = bond_valence_t(83, 53, 3, -1, 2.82, 0.37)
   bval_table(121) = bond_valence_t(83, 7, 3, -3, 2.02, 0.35)
   bval_table(122) = bond_valence_t(83, 8, 5, -2, 2.06, 0.37)
   bval_table(123) = bond_valence_t(83, 9, 5, -1, 1.97, 0.37)
   bval_table(124) = bond_valence_t(83, 17, 5, -1, 2.44, 0.37)
   bval_table(125) = bond_valence_t(83, 35, 9, -1, 2.62, 0.37)
   bval_table(126) = bond_valence_t(83, 53, 9, -1, 2.84, 0.37)
   bval_table(127) = bond_valence_t(83, 16, 9, -2, 2.55, 0.37)
   bval_table(128) = bond_valence_t(83, 34, 9, -2, 2.72, 0.37)
   bval_table(129) = bond_valence_t(83, 52, 9, -2, 2.87, 0.37)
   bval_table(130) = bond_valence_t(83, 7, 9, -3, 2.24, 0.37)
   bval_table(131) = bond_valence_t(83, 15, 9, -3, 2.63, 0.37)
   bval_table(132) = bond_valence_t(83, 33, 9, -3, 2.72, 0.37)
   bval_table(133) = bond_valence_t(83, 1, 9, -1, 1.97, 0.37)
   bval_table(134) = bond_valence_t(97, 8, 3, -2, 2.08, 0.37)
   bval_table(135) = bond_valence_t(97, 9, 3, -1, 1.96, 0.37)
   bval_table(136) = bond_valence_t(97, 17, 3, -1, 2.35, 0.37)
   bval_table(137) = bond_valence_t(97, 35, 3, -1, 2.56, 0.40)
   bval_table(138) = bond_valence_t(97, 8, 4, -2, 2.07, 0.35)
   bval_table(139) = bond_valence_t(97, 9, 4, -1, 1.93, 0.40)
   bval_table(140) = bond_valence_t(35, 8, 3, -2, 1.90, 0.37)
   bval_table(141) = bond_valence_t(35, 9, 3, -1, 1.75, 0.37)
   bval_table(142) = bond_valence_t(35, 8, 5, -2, 1.890, 0.571)
   bval_table(143) = bond_valence_t(35, 9, 5, -1, 1.76, 0.37)
   bval_table(144) = bond_valence_t(35, 8, 7, -2, 1.81, 0.37)
   bval_table(145) = bond_valence_t(35, 9, 7, -1, 1.72, 0.37)
   bval_table(146) = bond_valence_t(35, 17, 7, -1, 2.19, 0.37)
   bval_table(147) = bond_valence_t(6, 8, 2, -2, 1.366, 0.37)
   bval_table(148) = bond_valence_t(6, 17, 2, -1, 1.410, 0.37)
   bval_table(149) = bond_valence_t(6, 8, 4, -2, 1.390, 0.37)
   bval_table(150) = bond_valence_t(6, 6, 4, 4, 1.54, 0.37)
   bval_table(151) = bond_valence_t(6, 16, 4, -2, 1.80, 0.37)
   bval_table(152) = bond_valence_t(6, 9, 4, -1, 1.32, 0.37)
   bval_table(153) = bond_valence_t(6, 17, 4, -1, 1.76, 0.37)
   bval_table(154) = bond_valence_t(6, 35, 4, -1, 1.91, 0.37)
   bval_table(155) = bond_valence_t(6, 7, 4, -3, 1.442, 0.37)
   bval_table(156) = bond_valence_t(6, 34, 9, -2, 1.97, 0.37)
   bval_table(157) = bond_valence_t(6, 53, 9, -1, 2.12, 0.37)
   bval_table(158) = bond_valence_t(6, 35, 9, -1, 1.90, 0.37)
   bval_table(159) = bond_valence_t(6, 16, 9, -2, 1.82, 0.37)
   bval_table(160) = bond_valence_t(6, 52, 9, -2, 2.21, 0.37)
   bval_table(161) = bond_valence_t(6, 7, 9, -3, 1.47, 0.37)
   bval_table(162) = bond_valence_t(6, 15, 9, -3, 1.89, 0.37)
   bval_table(163) = bond_valence_t(6, 33, 9, -3, 1.99, 0.37)
   bval_table(164) = bond_valence_t(6, 1, 9, -1, 1.10, 0.37)
   bval_table(165) = bond_valence_t(20, 8, 2, -2, 1.967, 0.37)
   bval_table(166) = bond_valence_t(20, 16, 2, -2, 2.45, 0.37)
   bval_table(167) = bond_valence_t(20, 34, 2, -2, 2.56, 0.37)
   bval_table(168) = bond_valence_t(20, 52, 2, -2, 2.76, 0.37)
   bval_table(169) = bond_valence_t(20, 9, 2, -1, 1.842, 0.37)
   bval_table(170) = bond_valence_t(20, 17, 2, -1, 2.37, 0.37)
   bval_table(171) = bond_valence_t(20, 35, 2, -1, 2.507, 0.37)
   bval_table(172) = bond_valence_t(20, 53, 2, -1, 2.72, 0.37)
   bval_table(173) = bond_valence_t(20, 7, 2, -3, 2.14, 0.37)
   bval_table(174) = bond_valence_t(20, 15, 2, -3, 2.55, 0.37)
   bval_table(175) = bond_valence_t(20, 33, 2, -3, 2.62, 0.37)
   bval_table(176) = bond_valence_t(20, 1, 2, -1, 1.83, 0.37)
   bval_table(177) = bond_valence_t(48, 8, 2, -2, 1.904, 0.37)
   bval_table(178) = bond_valence_t(48, 16, 2, -2, 2.304, 0.37)
   bval_table(179) = bond_valence_t(48, 34, 2, -2, 2.40, 0.37)
   bval_table(180) = bond_valence_t(48, 52, 2, -2, 2.59, 0.37)
   bval_table(181) = bond_valence_t(48, 9, 2, -1, 1.811, 0.37)
   bval_table(182) = bond_valence_t(48, 17, 2, -1, 2.212, 0.37)
   bval_table(183) = bond_valence_t(48, 35, 2, -1, 2.334, 0.37)
   bval_table(184) = bond_valence_t(48, 53, 2, -1, 2.525, 0.37)
   bval_table(185) = bond_valence_t(48, 7, 2, -3, 1.96, 0.37)
   bval_table(186) = bond_valence_t(48, 15, 2, -3, 2.34, 0.37)
   bval_table(187) = bond_valence_t(48, 33, 2, -3, 2.43, 0.37)
   bval_table(188) = bond_valence_t(48, 1, 2, -1, 1.66, 0.37)
   bval_table(189) = bond_valence_t(58, 8, 3, -2, 2.151, 0.37)
   bval_table(190) = bond_valence_t(58, 16, 3, -2, 2.602, 0.37)
   bval_table(191) = bond_valence_t(58, 9, 3, -1, 2.036, 0.37)
   bval_table(192) = bond_valence_t(58, 17, 3, -1, 2.52, 0.37)
   bval_table(193) = bond_valence_t(58, 35, 3, -1, 2.65, 0.35)
   bval_table(194) = bond_valence_t(58, 53, 3, -1, 2.87, 0.40)
   bval_table(195) = bond_valence_t(58, 8, 4, -2, 2.028, 0.37)
   bval_table(196) = bond_valence_t(58, 16, 4, -2, 2.65, 0.35)
   bval_table(197) = bond_valence_t(58, 9, 4, -1, 1.995, 0.37)
   bval_table(198) = bond_valence_t(58, 7, 4, -3, 2.179, 0.37)
   bval_table(199) = bond_valence_t(58, 17, 9, -1, 2.41, 0.37)
   bval_table(200) = bond_valence_t(58, 35, 9, -1, 2.69, 0.37)
   bval_table(201) = bond_valence_t(58, 53, 9, -1, 2.92, 0.37)
   bval_table(202) = bond_valence_t(58, 16, 9, -2, 2.62, 0.37)
   bval_table(203) = bond_valence_t(58, 34, 9, -2, 2.74, 0.37)
   bval_table(204) = bond_valence_t(58, 52, 9, -2, 2.92, 0.37)
   bval_table(205) = bond_valence_t(58, 7, 9, -3, 2.254, 0.37)
   bval_table(206) = bond_valence_t(58, 15, 9, -3, 2.70, 0.37)
   bval_table(207) = bond_valence_t(58, 33, 9, -3, 2.78, 0.37)
   bval_table(208) = bond_valence_t(58, 1, 9, -1, 2.04, 0.37)
   bval_table(209) = bond_valence_t(98, 8, 3, -2, 2.07, 0.37)
   bval_table(210) = bond_valence_t(98, 9, 3, -1, 1.95, 0.37)
   bval_table(211) = bond_valence_t(98, 17, 3, -1, 2.45, 0.37)
   bval_table(212) = bond_valence_t(98, 35, 3, -1, 2.55, 0.40)
   bval_table(213) = bond_valence_t(98, 8, 4, -2, 2.06, 0.35)
   bval_table(214) = bond_valence_t(98, 9, 4, -1, 1.92, 0.40)
   bval_table(215) = bond_valence_t(17, 8, 3, -2, 1.722, 0.37)
   bval_table(216) = bond_valence_t(17, 9, 3, -1, 1.69, 0.37)
   bval_table(217) = bond_valence_t(17, 8, 5, -2, 1.703, 0.428)
   bval_table(218) = bond_valence_t(17, 8, 7, -2, 1.632, 0.37)
   bval_table(219) = bond_valence_t(17, 9, 7, -1, 1.55, 0.37)
   bval_table(220) = bond_valence_t(17, 17, 7, -1, 2.00, 0.37)
   bval_table(221) = bond_valence_t(98, 17, 3, -1, 2.45, 0.37)
   bval_table(222) = bond_valence_t(96, 8, 3, -2, 2.23, 0.37)
   bval_table(223) = bond_valence_t(96, 9, 3, -1, 2.12, 0.37)
   bval_table(224) = bond_valence_t(96, 17, 3, -1, 2.62, 0.37)
   bval_table(225) = bond_valence_t(96, 8, 4, -2, 2.08, 0.35)
   bval_table(226) = bond_valence_t(96, 9, 4, -1, 1.94, 0.40)
   bval_table(227) = bond_valence_t(27, 1, 1, -1, 1.000, 0.35)
   bval_table(228) = bond_valence_t(27, 8, 2, -2, 1.692, 0.37)
   bval_table(229) = bond_valence_t(27, 16, 2, -2, 1.94, 0.37)
   bval_table(230) = bond_valence_t(27, 9, 2, -1, 1.64, 0.37)
   bval_table(231) = bond_valence_t(27, 17, 2, -1, 2.033, 0.37)
   bval_table(232) = bond_valence_t(27, 7, 2, -3, 1.72, 0.37)
   bval_table(233) = bond_valence_t(27, 8, 3, -2, 1.637, 0.37)
   bval_table(234) = bond_valence_t(27, 16, 3, -2, 2.02, 0.37)
   bval_table(235) = bond_valence_t(27, 9, 3, -1, 1.62, 0.37)
   bval_table(236) = bond_valence_t(27, 17, 3, -1, 2.05, 0.37)
   bval_table(237) = bond_valence_t(27, 7, 3, -3, 1.69, 0.37)
   bval_table(238) = bond_valence_t(27, 6, 3, 2, 1.634, 0.37)
   bval_table(239) = bond_valence_t(27, 8, 4, -2, 1.729, 0.358)
   bval_table(240) = bond_valence_t(27, 9, 4, -1, 1.55, 0.37)
   bval_table(241) = bond_valence_t(27, 8, 9, -2, 1.655, 0.42)
   bval_table(242) = bond_valence_t(27, 35, 9, -1, 2.18, 0.37)
   bval_table(243) = bond_valence_t(27, 53, 9, -1, 2.37, 0.35)
   bval_table(244) = bond_valence_t(27, 16, 9, -2, 2.06, 0.37)
   bval_table(245) = bond_valence_t(27, 34, 9, -2, 2.24, 0.37)
   bval_table(246) = bond_valence_t(27, 52, 9, -2, 2.46, 0.37)
   bval_table(247) = bond_valence_t(27, 7, 9, -3, 1.84, 0.37)
   bval_table(248) = bond_valence_t(27, 15, 9, -3, 2.21, 0.37)
   bval_table(249) = bond_valence_t(27, 33, 9, -3, 2.28, 0.37)
   bval_table(250) = bond_valence_t(27, 1, 9, -1, 1.44, 0.37)
   bval_table(251) = bond_valence_t(24, 8, 2, -2, 1.761, 0.350)
   bval_table(252) = bond_valence_t(24, 9, 2, -1, 1.67, 0.37)
   bval_table(253) = bond_valence_t(24, 17, 2, -1, 2.09, 0.37)
   bval_table(254) = bond_valence_t(24, 35, 2, -1, 2.26, 0.37)
   bval_table(255) = bond_valence_t(24, 53, 2, -1, 2.48, 0.37)
   bval_table(256) = bond_valence_t(24, 7, 2, -3, 1.80, 0.37)
   bval_table(257) = bond_valence_t(24, 8, 3, -2, 1.724, 0.37)
   bval_table(258) = bond_valence_t(24, 16, 3, -2, 2.162, 0.37)
   bval_table(259) = bond_valence_t(24, 9, 3, -1, 1.657, 0.37)
   bval_table(260) = bond_valence_t(24, 17, 3, -1, 2.08, 0.37)
   bval_table(261) = bond_valence_t(24, 35, 3, -1, 2.28, 0.37)
   bval_table(262) = bond_valence_t(24, 7, 3, -3, 1.78, 0.37)
   bval_table(263) = bond_valence_t(24, 8, 4, -2, 1.783, 0.410)
   bval_table(264) = bond_valence_t(24, 9, 4, -1, 1.56, 0.37)
   bval_table(265) = bond_valence_t(24, 8, 5, -2, 1.76, 0.37)
   bval_table(266) = bond_valence_t(24, 8, 6, -2, 1.794, 0.37)
   bval_table(267) = bond_valence_t(24, 9, 6, -1, 1.74, 0.37)
   bval_table(268) = bond_valence_t(24, 17, 6, -1, 2.12, 0.37)
   bval_table(269) = bond_valence_t(24, 8, 9, -2, 1.79, 0.34)
   bval_table(270) = bond_valence_t(24, 35, 9, -1, 2.26, 0.37)
   bval_table(271) = bond_valence_t(24, 53, 9, -1, 2.45, 0.37)
   bval_table(272) = bond_valence_t(24, 16, 9, -2, 2.18, 0.37)
   bval_table(273) = bond_valence_t(24, 34, 9, -2, 2.29, 0.37)
   bval_table(274) = bond_valence_t(24, 52, 9, -2, 2.52, 0.37)
   bval_table(275) = bond_valence_t(24, 7, 9, -3, 1.85, 0.37)
   bval_table(276) = bond_valence_t(24, 15, 9, -3, 2.27, 0.37)
   bval_table(277) = bond_valence_t(24, 33, 9, -3, 2.34, 0.37)
   bval_table(278) = bond_valence_t(24, 1, 9, -1, 1.52, 0.37)
   bval_table(279) = bond_valence_t(55, 8, 1, -2, 2.417, 0.37)
   bval_table(280) = bond_valence_t(55, 16, 1, -2, 2.89, 0.37)
   bval_table(281) = bond_valence_t(55, 34, 1, -2, 2.98, 0.37)
   bval_table(282) = bond_valence_t(55, 52, 1, -2, 3.16, 0.37)
   bval_table(283) = bond_valence_t(55, 9, 1, -1, 2.33, 0.37)
   bval_table(284) = bond_valence_t(55, 17, 1, -1, 2.791, 0.37)
   bval_table(285) = bond_valence_t(55, 35, 1, -1, 2.95, 0.37)
   bval_table(286) = bond_valence_t(55, 53, 1, -1, 3.18, 0.37)
   bval_table(287) = bond_valence_t(55, 7, 1, -3, 2.83, 0.37)
   bval_table(288) = bond_valence_t(55, 15, 1, -3, 2.93, 0.37)
   bval_table(289) = bond_valence_t(55, 33, 1, -3, 3.04, 0.37)
   bval_table(290) = bond_valence_t(55, 1, 1, -1, 2.44, 0.37)
   bval_table(291) = bond_valence_t(29, 8, 1, -2, 1.610, 0.37)
   bval_table(292) = bond_valence_t(29, 16, 1, -2, 1.898, 0.37)
   bval_table(293) = bond_valence_t(29, 34, 1, -2, 1.900, 0.37)
   bval_table(294) = bond_valence_t(29, 9, 1, -1, 1.6, 0.37)
   bval_table(295) = bond_valence_t(29, 17, 1, -1, 1.858, 0.37)
   bval_table(296) = bond_valence_t(29, 35, 1, -1, 2.03, 0.37)
   bval_table(297) = bond_valence_t(29, 53, 1, -1, 2.108, 0.37)
   bval_table(298) = bond_valence_t(29, 7, 1, -3, 1.520, 0.37)
   bval_table(299) = bond_valence_t(29, 15, 1, -3, 1.774, 0.37)
   bval_table(300) = bond_valence_t(29, 33, 1, -3, 1.856, 0.37)
   bval_table(301) = bond_valence_t(29, 6, 1, -4, 1.446, 0.37)
   bval_table(302) = bond_valence_t(29, 8, 2, -2, 1.679, 0.36)
   bval_table(303) = bond_valence_t(29, 16, 2, -2, 2.054, 0.37)
   bval_table(304) = bond_valence_t(29, 34, 2, -2, 2.02, 0.37)
   bval_table(305) = bond_valence_t(29, 52, 2, -2, 2.27, 0.37)
   bval_table(306) = bond_valence_t(29, 9, 2, -1, 1.594, 0.37)
   bval_table(307) = bond_valence_t(29, 17, 2, -1, 2.00, 0.37)
   bval_table(308) = bond_valence_t(29, 35, 2, -1, 1.99, 0.37)
   bval_table(309) = bond_valence_t(29, 53, 2, -1, 2.16, 0.37)
   bval_table(310) = bond_valence_t(29, 7, 2, -3, 1.751, 0.37)
   bval_table(311) = bond_valence_t(29, 15, 2, -3, 1.97, 0.37)
   bval_table(312) = bond_valence_t(29, 33, 2, -3, 2.08, 0.37)
   bval_table(313) = bond_valence_t(29, 6, 2, -4, 1.72, 0.37)
   bval_table(314) = bond_valence_t(29, 1, 2, -1, 1.21, 0.37)
   bval_table(315) = bond_valence_t(29, 8, 3, -2, 1.735, 0.37)
   bval_table(316) = bond_valence_t(29, 9, 3, -1, 1.58, 0.37)
   bval_table(317) = bond_valence_t(29, 17, 3, -1, 2.078, 0.37)
   bval_table(318) = bond_valence_t(29, 7, 3, -3, 1.768, 0.37)
   bval_table(319) = bond_valence_t(29, 6, 3, -4, 1.84, 0.37)
   bval_table(320) = bond_valence_t(66, 8, 2, -2, 1.90, 0.37)
   bval_table(321) = bond_valence_t(66, 8, 3, -2, 2.001, 0.37)
   bval_table(322) = bond_valence_t(66, 9, 3, -1, 1.922, 0.37)
   bval_table(323) = bond_valence_t(66, 17, 3, -1, 2.410, 0.37)
   bval_table(324) = bond_valence_t(66, 35, 3, -1, 2.53, 0.40)
   bval_table(325) = bond_valence_t(66, 53, 3, -1, 2.76, 0.40)
   bval_table(326) = bond_valence_t(66, 35, 9, -1, 2.56, 0.37)
   bval_table(327) = bond_valence_t(66, 53, 9, -1, 2.77, 0.37)
   bval_table(328) = bond_valence_t(66, 16, 9, -2, 2.47, 0.37)
   bval_table(329) = bond_valence_t(66, 34, 9, -2, 2.61, 0.37)
   bval_table(330) = bond_valence_t(66, 52, 9, -2, 2.80, 0.37)
   bval_table(331) = bond_valence_t(66, 7, 9, -3, 2.124, 0.37)
   bval_table(332) = bond_valence_t(66, 15, 9, -3, 2.57, 0.37)
   bval_table(333) = bond_valence_t(66, 33, 9, -3, 2.64, 0.37)
   bval_table(334) = bond_valence_t(66, 1, 9, -1, 1.89, 0.37)
   bval_table(335) = bond_valence_t(68, 8, 2, -2, 1.88, 0.37)
   bval_table(336) = bond_valence_t(68, 16, 2, -2, 2.52, 0.37)
   bval_table(337) = bond_valence_t(68, 8, 3, -2, 1.988, 0.37)
   bval_table(338) = bond_valence_t(68, 16, 3, -2, 2.475, 0.37)
   bval_table(339) = bond_valence_t(68, 34, 3, -2, 2.58, 0.37)
   bval_table(340) = bond_valence_t(68, 9, 3, -1, 1.904, 0.37)
   bval_table(341) = bond_valence_t(68, 17, 3, -1, 2.390, 0.37)
   bval_table(342) = bond_valence_t(68, 35, 3, -1, 2.51, 0.40)
   bval_table(343) = bond_valence_t(68, 53, 3, -1, 2.75, 0.40)
   bval_table(344) = bond_valence_t(68, 35, 9, -1, 2.54, 0.37)
   bval_table(345) = bond_valence_t(68, 53, 9, -1, 2.75, 0.37)
   bval_table(346) = bond_valence_t(68, 16, 9, -2, 2.46, 0.37)
   bval_table(347) = bond_valence_t(68, 34, 9, -2, 2.59, 0.37)
   bval_table(348) = bond_valence_t(68, 52, 9, -2, 2.78, 0.37)
   bval_table(349) = bond_valence_t(68, 7, 9, -3, 2.086, 0.37)
   bval_table(350) = bond_valence_t(68, 15, 9, -3, 2.55, 0.37)
   bval_table(351) = bond_valence_t(68, 33, 9, -3, 2.63, 0.37)
   bval_table(352) = bond_valence_t(68, 1, 9, -1, 1.86, 0.37)
   bval_table(353) = bond_valence_t(99, 8, 3, -2, 2.08, 0.35)
   bval_table(354) = bond_valence_t(63, 8, 2, -2, 2.147, 0.37)
   bval_table(355) = bond_valence_t(63, 16, 2, -2, 2.584, 0.37)
   bval_table(356) = bond_valence_t(63, 9, 2, -1, 2.04, 0.37)
   bval_table(357) = bond_valence_t(63, 17, 2, -1, 2.53, 0.37)
   bval_table(358) = bond_valence_t(63, 35, 2, -1, 2.67, 0.37)
   bval_table(359) = bond_valence_t(63, 53, 2, -1, 2.90, 0.37)
   bval_table(360) = bond_valence_t(63, 7, 2, -3, 2.16, 0.37)
   bval_table(361) = bond_valence_t(63, 8, 3, -2, 2.074, 0.37)
   bval_table(362) = bond_valence_t(63, 16, 3, -2, 2.509, 0.37)
   bval_table(363) = bond_valence_t(63, 9, 3, -1, 1.961, 0.37)
   bval_table(364) = bond_valence_t(63, 17, 3, -1, 2.455, 0.37)
   bval_table(365) = bond_valence_t(63, 35, 3, -1, 2.57, 0.40)
   bval_table(366) = bond_valence_t(63, 53, 3, -1, 2.79, 0.40)
   bval_table(367) = bond_valence_t(63, 35, 9, -1, 2.61, 0.37)
   bval_table(368) = bond_valence_t(63, 53, 9, -1, 2.83, 0.37)
   bval_table(369) = bond_valence_t(63, 16, 9, -2, 2.53, 0.37)
   bval_table(370) = bond_valence_t(63, 34, 9, -2, 2.66, 0.37)
   bval_table(371) = bond_valence_t(63, 52, 9, -2, 2.85, 0.37)
   bval_table(372) = bond_valence_t(63, 7, 9, -3, 2.161, 0.37)
   bval_table(373) = bond_valence_t(63, 15, 9, -3, 2.62, 0.37)
   bval_table(374) = bond_valence_t(63, 33, 9, -3, 2.70, 0.37)
   bval_table(375) = bond_valence_t(63, 1, 9, -1, 1.95, 0.37)
   bval_table(376) = bond_valence_t(26, 8, 2, -2, 1.734, 0.37)
   bval_table(377) = bond_valence_t(26, 16, 2, -2, 2.12, 0.37)
   bval_table(378) = bond_valence_t(26, 9, 2, -1, 1.65, 0.37)
   bval_table(379) = bond_valence_t(26, 17, 2, -1, 2.06, 0.37)
   bval_table(380) = bond_valence_t(26, 35, 2, -1, 2.21, 0.35)
   bval_table(381) = bond_valence_t(26, 53, 2, -1, 2.47, 0.35)
   bval_table(382) = bond_valence_t(26, 7, 2, -3, 1.76, 0.37)
   bval_table(383) = bond_valence_t(26, 8, 3, -2, 1.759, 0.37)
   bval_table(384) = bond_valence_t(26, 16, 3, -2, 2.149, 0.37)
   bval_table(385) = bond_valence_t(26, 9, 3, -1, 1.679, 0.37)
   bval_table(386) = bond_valence_t(26, 17, 3, -1, 2.09, 0.37)
   bval_table(387) = bond_valence_t(26, 35, 3, -1, 2.22, 0.37)
   bval_table(388) = bond_valence_t(26, 7, 3, -3, 1.82, 0.37)
   bval_table(389) = bond_valence_t(26, 6, 3, 2, 1.689, 0.37)
   bval_table(390) = bond_valence_t(26, 16, 4, -2, 2.23, 0.35)
   bval_table(391) = bond_valence_t(26, 8, 6, -2, 1.76, 0.35)
   bval_table(392) = bond_valence_t(26, 8, 9, -2, 1.795, 0.30)
   bval_table(393) = bond_valence_t(26, 35, 9, -1, 2.26, 0.37)
   bval_table(394) = bond_valence_t(26, 53, 9, -1, 2.47, 0.37)
   bval_table(395) = bond_valence_t(26, 16, 9, -2, 2.16, 0.37)
   bval_table(396) = bond_valence_t(26, 34, 9, -2, 2.28, 0.37)
   bval_table(397) = bond_valence_t(26, 52, 9, -2, 2.53, 0.37)
   bval_table(398) = bond_valence_t(26, 7, 9, -3, 1.86, 0.37)
   bval_table(399) = bond_valence_t(26, 15, 9, -3, 2.27, 0.37)
   bval_table(400) = bond_valence_t(26, 33, 9, -3, 2.35, 0.37)
   bval_table(401) = bond_valence_t(26, 1, 9, -1, 1.53, 0.37)
   bval_table(402) = bond_valence_t(31, 34, 1, -1, 2.55, 0.37)
   bval_table(403) = bond_valence_t(31, 8, 3, -2, 1.730, 0.37)
   bval_table(404) = bond_valence_t(31, 16, 3, -2, 2.163, 0.37)
   bval_table(405) = bond_valence_t(31, 9, 3, -1, 1.62, 0.37)
   bval_table(406) = bond_valence_t(31, 17, 3, -1, 2.07, 0.37)
   bval_table(407) = bond_valence_t(31, 35, 3, -1, 2.20, 0.35)
   bval_table(408) = bond_valence_t(31, 53, 3, -1, 2.46, 0.37)
   bval_table(409) = bond_valence_t(31, 35, 9, -1, 2.24, 0.37)
   bval_table(410) = bond_valence_t(31, 53, 9, -1, 2.45, 0.37)
   bval_table(411) = bond_valence_t(31, 16, 9, -2, 2.17, 0.37)
   bval_table(412) = bond_valence_t(31, 34, 9, -2, 2.30, 0.37)
   bval_table(413) = bond_valence_t(31, 52, 9, -2, 2.54, 0.37)
   bval_table(414) = bond_valence_t(31, 7, 9, -3, 1.84, 0.37)
   bval_table(415) = bond_valence_t(31, 15, 9, -3, 2.26, 0.37)
   bval_table(416) = bond_valence_t(31, 33, 9, -3, 2.34, 0.37)
   bval_table(417) = bond_valence_t(31, 1, 9, -1, 1.51, 0.37)
   bval_table(418) = bond_valence_t(64, 8, 2, -2, 2.01, 0.37)
   bval_table(419) = bond_valence_t(64, 9, 2, -1, 2.40, 0.37)
   bval_table(420) = bond_valence_t(64, 8, 3, -2, 2.065, 0.37)
   bval_table(421) = bond_valence_t(64, 16, 3, -2, 2.53, 0.37)
   bval_table(422) = bond_valence_t(64, 9, 3, -1, 1.95, 0.37)
   bval_table(423) = bond_valence_t(64, 17, 3, -1, 2.445, 0.37)
   bval_table(424) = bond_valence_t(64, 35, 3, -1, 2.56, 0.40)
   bval_table(425) = bond_valence_t(64, 53, 3, -1, 2.78, 0.40)
   bval_table(426) = bond_valence_t(64, 35, 9, -1, 2.60, 0.37)
   bval_table(427) = bond_valence_t(64, 53, 9, -1, 2.82, 0.37)
   bval_table(428) = bond_valence_t(64, 16, 9, -2, 2.53, 0.37)
   bval_table(429) = bond_valence_t(64, 34, 9, -2, 2.65, 0.37)
   bval_table(430) = bond_valence_t(64, 52, 9, -2, 2.84, 0.37)
   bval_table(431) = bond_valence_t(64, 7, 9, -3, 2.146, 0.37)
   bval_table(432) = bond_valence_t(64, 15, 9, -3, 2.61, 0.37)
   bval_table(433) = bond_valence_t(64, 33, 9, -3, 2.68, 0.37)
   bval_table(434) = bond_valence_t(64, 1, 9, -1, 1.93, 0.37)
   bval_table(435) = bond_valence_t(32, 16, 2, -2, 2.15, 0.50)
   bval_table(436) = bond_valence_t(32, 8, 4, -2, 1.748, 0.37)
   bval_table(437) = bond_valence_t(32, 16, 4, -2, 2.217, 0.37)
   bval_table(438) = bond_valence_t(32, 34, 4, -2, 2.35, 0.37)
   bval_table(439) = bond_valence_t(32, 9, 4, -1, 1.66, 0.37)
   bval_table(440) = bond_valence_t(32, 17, 4, -1, 2.14, 0.37)
   bval_table(441) = bond_valence_t(32, 35, 9, -1, 2.30, 0.37)
   bval_table(442) = bond_valence_t(32, 53, 9, -1, 2.50, 0.37)
   bval_table(443) = bond_valence_t(32, 16, 9, -2, 2.23, 0.37)
   bval_table(444) = bond_valence_t(32, 34, 9, -2, 2.35, 0.37)
   bval_table(445) = bond_valence_t(32, 52, 9, -2, 2.56, 0.37)
   bval_table(446) = bond_valence_t(32, 7, 9, -3, 1.88, 0.37)
   bval_table(447) = bond_valence_t(32, 15, 9, -3, 2.32, 0.37)
   bval_table(448) = bond_valence_t(32, 33, 9, -3, 2.43, 0.37)
   bval_table(449) = bond_valence_t(32, 1, 9, -1, 1.55, 0.37)
   bval_table(450) = bond_valence_t(1, 8, 1, -2, 0.569, 0.94)
   bval_table(451) = bond_valence_t(1, 16, 1, -2, 1.192, 0.591)
   bval_table(452) = bond_valence_t(1, 9, 1, -1, 0.708, 0.558)
   bval_table(453) = bond_valence_t(1, 17, 1, -1, 1.336, 0.53)
   bval_table(454) = bond_valence_t(1, 7, 1, -3, 1.014, 0.413)
   bval_table(455) = bond_valence_t(72, 9, 3, -1, 2.62, 0.37)
   bval_table(456) = bond_valence_t(72, 8, 4, -2, 1.923, 0.37)
   bval_table(457) = bond_valence_t(72, 9, 4, -1, 1.85, 0.37)
   bval_table(458) = bond_valence_t(72, 17, 4, -1, 2.24, 0.37)
   bval_table(459) = bond_valence_t(72, 35, 9, -1, 2.47, 0.37)
   bval_table(460) = bond_valence_t(72, 16, 9, -2, 2.39, 0.37)
   bval_table(461) = bond_valence_t(72, 34, 9, -2, 2.52, 0.37)
   bval_table(462) = bond_valence_t(72, 52, 9, -2, 2.72, 0.37)
   bval_table(463) = bond_valence_t(72, 53, 9, -1, 2.68, 0.37)
   bval_table(464) = bond_valence_t(72, 7, 9, -3, 2.09, 0.37)
   bval_table(465) = bond_valence_t(72, 15, 9, -3, 2.48, 0.37)
   bval_table(466) = bond_valence_t(72, 33, 9, -3, 2.56, 0.37)
   bval_table(467) = bond_valence_t(72, 1, 9, -1, 1.78, 0.37)
   bval_table(468) = bond_valence_t(80, 8, 1, -2, 1.90, 0.37)
   bval_table(469) = bond_valence_t(80, 9, 1, -1, 1.81, 0.37)
   bval_table(470) = bond_valence_t(80, 17, 1, -1, 2.28, 0.37)
   bval_table(471) = bond_valence_t(80, 8, 2, -2, 1.924, 0.38)
   bval_table(472) = bond_valence_t(80, 16, 2, -2, 2.308, 0.37)
   bval_table(473) = bond_valence_t(80, 9, 2, -1, 2.17, 0.37)
   bval_table(474) = bond_valence_t(80, 17, 2, -1, 2.28, 0.37)
   bval_table(475) = bond_valence_t(80, 35, 2, -1, 2.38, 0.37)
   bval_table(476) = bond_valence_t(80, 53, 2, -1, 2.62, 0.37)
   bval_table(477) = bond_valence_t(80, 35, 9, -1, 2.40, 0.37)
   bval_table(478) = bond_valence_t(80, 53, 9, -1, 2.59, 0.37)
   bval_table(479) = bond_valence_t(80, 16, 9, -2, 2.32, 0.37)
   bval_table(480) = bond_valence_t(80, 34, 9, -2, 2.47, 0.37)
   bval_table(481) = bond_valence_t(80, 52, 9, -2, 2.61, 0.37)
   bval_table(482) = bond_valence_t(80, 7, 9, -3, 2.02, 0.37)
   bval_table(483) = bond_valence_t(80, 15, 9, -3, 2.42, 0.37)
   bval_table(484) = bond_valence_t(80, 33, 9, -3, 2.50, 0.37)
   bval_table(485) = bond_valence_t(80, 1, 9, -1, 1.71, 0.37)
   bval_table(486) = bond_valence_t(80, 80, 2, 2, 2.51, 0.35)
   bval_table(487) = bond_valence_t(67, 8, 3, -2, 2.025, 0.37)
   bval_table(488) = bond_valence_t(67, 16, 3, -2, 2.49, 0.37)
   bval_table(489) = bond_valence_t(67, 9, 3, -1, 1.908, 0.37)
   bval_table(490) = bond_valence_t(67, 17, 3, -1, 2.401, 0.37)
   bval_table(491) = bond_valence_t(67, 35, 3, -1, 2.52, 0.40)
   bval_table(492) = bond_valence_t(67, 53, 3, -1, 2.76, 0.40)
   bval_table(493) = bond_valence_t(67, 35, 9, -1, 2.55, 0.37)
   bval_table(494) = bond_valence_t(67, 53, 9, -1, 2.77, 0.37)
   bval_table(495) = bond_valence_t(67, 16, 9, -2, 2.48, 0.37)
   bval_table(496) = bond_valence_t(67, 34, 9, -2, 2.61, 0.37)
   bval_table(497) = bond_valence_t(67, 52, 9, -2, 2.80, 0.37)
   bval_table(498) = bond_valence_t(67, 7, 9, -3, 2.118, 0.37)
   bval_table(499) = bond_valence_t(67, 15, 9, -3, 2.56, 0.37)
   bval_table(500) = bond_valence_t(67, 33, 9, -3, 2.64, 0.37)
   bval_table(501) = bond_valence_t(67, 1, 9, -1, 1.88, 0.37)
   bval_table(502) = bond_valence_t(53, 53, 0, 0, 2.195, 0.35)
   bval_table(503) = bond_valence_t(53, 9, 1, -1, 2.32, 0.37)
   bval_table(504) = bond_valence_t(53, 17, 1, -1, 2.47, 0.37)
   bval_table(505) = bond_valence_t(53, 8, 3, -2, 2.02, 0.37)
   bval_table(506) = bond_valence_t(53, 9, 3, -1, 1.90, 0.37)
   bval_table(507) = bond_valence_t(53, 17, 3, -1, 2.39, 0.37)
   bval_table(508) = bond_valence_t(53, 8, 5, -2, 1.990, 0.44)
   bval_table(509) = bond_valence_t(53, 9, 5, -1, 1.84, 0.37)
   bval_table(510) = bond_valence_t(53, 17, 5, -1, 2.38, 0.37)
   bval_table(511) = bond_valence_t(53, 8, 7, -2, 1.93, 0.37)
   bval_table(512) = bond_valence_t(53, 9, 7, -1, 1.83, 0.37)
   bval_table(513) = bond_valence_t(53, 17, 7, -1, 2.31, 0.37)
   bval_table(514) = bond_valence_t(49, 17, 1, -1, 2.56, 0.37)
   bval_table(515) = bond_valence_t(49, 8, 3, -2, 1.902, 0.37)
   bval_table(516) = bond_valence_t(49, 16, 3, -2, 2.370, 0.37)
   bval_table(517) = bond_valence_t(49, 9, 3, -1, 1.792, 0.37)
   bval_table(518) = bond_valence_t(49, 17, 3, -1, 2.28, 0.37)
   bval_table(519) = bond_valence_t(49, 35, 3, -1, 2.51, 0.35)
   bval_table(520) = bond_valence_t(49, 53, 3, -1, 2.63, 0.37)
   bval_table(521) = bond_valence_t(49, 27, 3, -1, 2.593, 0.35)
   bval_table(522) = bond_valence_t(49, 25, 3, -2, 2.604, 0.35)
   bval_table(523) = bond_valence_t(49, 35, 9, -1, 2.41, 0.37)
   bval_table(524) = bond_valence_t(49, 53, 9, -1, 2.63, 0.37)
   bval_table(525) = bond_valence_t(49, 16, 9, -2, 2.36, 0.37)
   bval_table(526) = bond_valence_t(49, 34, 9, -2, 2.47, 0.37)
   bval_table(527) = bond_valence_t(49, 52, 9, -2, 2.69, 0.37)
   bval_table(528) = bond_valence_t(49, 7, 9, -3, 2.03, 0.37)
   bval_table(529) = bond_valence_t(49, 15, 9, -3, 2.43, 0.37)
   bval_table(530) = bond_valence_t(49, 33, 9, -3, 2.51, 0.37)
   bval_table(531) = bond_valence_t(49, 1, 9, -1, 1.72, 0.37)
   bval_table(532) = bond_valence_t(77, 8, 4, -2, 1.909, 0.258)
   bval_table(533) = bond_valence_t(77, 9, 4, -1, 1.80, 0.37)
   bval_table(534) = bond_valence_t(77, 8, 5, -2, 1.916, 0.37)
   bval_table(535) = bond_valence_t(77, 9, 5, -1, 1.82, 0.37)
   bval_table(536) = bond_valence_t(77, 17, 5, -1, 2.30, 0.37)
   bval_table(537) = bond_valence_t(77, 16, 9, -2, 2.38, 0.37)
   bval_table(538) = bond_valence_t(77, 34, 9, -2, 2.51, 0.37)
   bval_table(539) = bond_valence_t(77, 52, 9, -2, 2.71, 0.37)
   bval_table(540) = bond_valence_t(77, 35, 9, -1, 2.45, 0.37)
   bval_table(541) = bond_valence_t(77, 53, 9, -1, 2.66, 0.37)
   bval_table(542) = bond_valence_t(77, 7, 9, -3, 2.06, 0.37)
   bval_table(543) = bond_valence_t(77, 15, 9, -3, 2.46, 0.37)
   bval_table(544) = bond_valence_t(77, 33, 9, -3, 2.54, 0.37)
   bval_table(545) = bond_valence_t(77, 1, 9, -1, 1.76, 0.37)
   bval_table(546) = bond_valence_t(19, 8, 1, -2, 2.132, 0.37)
   bval_table(547) = bond_valence_t(19, 16, 1, -2, 2.59, 0.37)
   bval_table(548) = bond_valence_t(19, 34, 1, -2, 2.72, 0.37)
   bval_table(549) = bond_valence_t(19, 52, 1, -2, 2.93, 0.37)
   bval_table(550) = bond_valence_t(19, 9, 1, -1, 1.992, 0.37)
   bval_table(551) = bond_valence_t(19, 17, 1, -1, 2.519, 0.37)
   bval_table(552) = bond_valence_t(19, 35, 1, -1, 2.66, 0.37)
   bval_table(553) = bond_valence_t(19, 53, 1, -1, 2.88, 0.37)
   bval_table(554) = bond_valence_t(19, 7, 1, -3, 2.26, 0.37)
   bval_table(555) = bond_valence_t(19, 15, 1, -3, 2.64, 0.37)
   bval_table(556) = bond_valence_t(19, 33, 1, -3, 2.83, 0.37)
   bval_table(557) = bond_valence_t(19, 1, 1, -1, 2.10, 0.37)
   bval_table(558) = bond_valence_t(36, 9, 2, -1, 1.88, 0.37)
   bval_table(559) = bond_valence_t(57, 8, 3, -2, 2.086, 0.45)
   bval_table(560) = bond_valence_t(57, 16, 3, -2, 2.643, 0.37)
   bval_table(561) = bond_valence_t(57, 34, 3, -2, 2.74, 0.37)
   bval_table(562) = bond_valence_t(57, 52, 3, -2, 2.94, 0.37)
   bval_table(563) = bond_valence_t(57, 9, 3, -1, 2.02, 0.40)
   bval_table(564) = bond_valence_t(57, 17, 3, -1, 2.545, 0.37)
   bval_table(565) = bond_valence_t(57, 35, 3, -1, 2.72, 0.37)
   bval_table(566) = bond_valence_t(57, 53, 3, -1, 2.93, 0.37)
   bval_table(567) = bond_valence_t(57, 7, 3, -3, 2.261, 0.37)
   bval_table(568) = bond_valence_t(57, 15, 3, -3, 2.73, 0.37)
   bval_table(569) = bond_valence_t(57, 33, 3, -3, 2.80, 0.37)
   bval_table(570) = bond_valence_t(57, 1, 3, -1, 2.06, 0.37)
   bval_table(571) = bond_valence_t(3, 8, 1, -2, 1.466, 0.37)
   bval_table(572) = bond_valence_t(3, 16, 1, -2, 1.94, 0.37)
   bval_table(573) = bond_valence_t(3, 34, 1, -2, 2.09, 0.37)
   bval_table(574) = bond_valence_t(3, 52, 1, -2, 2.30, 0.37)
   bval_table(575) = bond_valence_t(3, 9, 1, -1, 1.360, 0.37)
   bval_table(576) = bond_valence_t(3, 17, 1, -1, 1.91, 0.37)
   bval_table(577) = bond_valence_t(3, 35, 1, -1, 2.02, 0.37)
   bval_table(578) = bond_valence_t(3, 53, 1, -1, 2.22, 0.37)
   bval_table(579) = bond_valence_t(3, 7, 1, -3, 1.61, 0.37)
   bval_table(580) = bond_valence_t(71, 8, 3, -2, 1.971, 0.37)
   bval_table(581) = bond_valence_t(71, 16, 3, -2, 2.43, 0.37)
   bval_table(582) = bond_valence_t(71, 34, 3, -2, 2.56, 0.37)
   bval_table(583) = bond_valence_t(71, 52, 3, -2, 2.75, 0.37)
   bval_table(584) = bond_valence_t(71, 9, 3, -1, 1.876, 0.37)
   bval_table(585) = bond_valence_t(71, 17, 3, -1, 2.361, 0.37)
   bval_table(586) = bond_valence_t(71, 35, 3, -1, 2.50, 0.37)
   bval_table(587) = bond_valence_t(71, 53, 3, -1, 2.73, 0.37)
   bval_table(588) = bond_valence_t(71, 7, 3, -3, 2.046, 0.37)
   bval_table(589) = bond_valence_t(71, 15, 3, -3, 2.51, 0.37)
   bval_table(590) = bond_valence_t(71, 33, 3, -3, 2.59, 0.37)
   bval_table(591) = bond_valence_t(71, 1, 3, -1, 1.82, 0.37)
   bval_table(592) = bond_valence_t(12, 8, 2, -2, 1.693, 0.37)
   bval_table(593) = bond_valence_t(12, 16, 2, -2, 2.18, 0.37)
   bval_table(594) = bond_valence_t(12, 34, 2, -2, 2.32, 0.37)
   bval_table(595) = bond_valence_t(12, 52, 2, -2, 2.53, 0.37)
   bval_table(596) = bond_valence_t(12, 9, 2, -1, 1.578, 0.37)
   bval_table(597) = bond_valence_t(12, 17, 2, -1, 2.08, 0.37)
   bval_table(598) = bond_valence_t(12, 35, 2, -1, 2.28, 0.37)
   bval_table(599) = bond_valence_t(12, 53, 2, -1, 2.46, 0.37)
   bval_table(600) = bond_valence_t(12, 7, 2, -3, 1.85, 0.37)
   bval_table(601) = bond_valence_t(12, 15, 2, -3, 2.29, 0.37)
   bval_table(602) = bond_valence_t(12, 33, 2, -3, 2.38, 0.37)
   bval_table(603) = bond_valence_t(12, 1, 2, -1, 1.53, 0.37)
   bval_table(604) = bond_valence_t(25, 8, 2, -2, 1.790, 0.37)
   bval_table(605) = bond_valence_t(25, 16, 2, -2, 2.22, 0.37)
   bval_table(606) = bond_valence_t(25, 9, 2, -1, 1.698, 0.37)
   bval_table(607) = bond_valence_t(25, 17, 2, -1, 2.133, 0.37)
   bval_table(608) = bond_valence_t(25, 35, 2, -1, 2.34, 0.37)
   bval_table(609) = bond_valence_t(25, 53, 2, -2, 2.52, 0.37)
   bval_table(610) = bond_valence_t(25, 7, 2, -3, 1.84, 0.37)
   bval_table(611) = bond_valence_t(25, 8, 3, -2, 1.760, 0.37)
   bval_table(612) = bond_valence_t(25, 9, 3, -1, 1.66, 0.37)
   bval_table(613) = bond_valence_t(25, 17, 3, -1, 2.14, 0.37)
   bval_table(614) = bond_valence_t(25, 7, 3, -3, 1.82, 0.37)
   bval_table(615) = bond_valence_t(25, 8, 4, -2, 1.753, 0.37)
   bval_table(616) = bond_valence_t(25, 9, 4, -1, 1.71, 0.37)
   bval_table(617) = bond_valence_t(25, 17, 4, -1, 2.13, 0.37)
   bval_table(618) = bond_valence_t(25, 7, 4, -3, 1.822, 0.37)
   bval_table(619) = bond_valence_t(25, 8, 5, -2, 1.781, 0.375)
   bval_table(620) = bond_valence_t(25, 8, 6, -2, 1.814, 0.375)
   bval_table(621) = bond_valence_t(25, 8, 7, -2, 1.819, 0.375)
   bval_table(622) = bond_valence_t(25, 9, 7, -1, 1.72, 0.37)
   bval_table(623) = bond_valence_t(25, 17, 7, -1, 2.17, 0.37)
   bval_table(624) = bond_valence_t(25, 8, 9, -2, 1.754, 0.37)
   bval_table(625) = bond_valence_t(25, 35, 9, -1, 2.26, 0.37)
   bval_table(626) = bond_valence_t(25, 53, 9, -1, 2.49, 0.37)
   bval_table(627) = bond_valence_t(25, 16, 9, -2, 2.20, 0.37)
   bval_table(628) = bond_valence_t(25, 34, 9, -1, 2.32, 0.37)
   bval_table(629) = bond_valence_t(25, 52, 9, -2, 2.55, 0.37)
   bval_table(630) = bond_valence_t(25, 7, 9, -3, 1.87, 0.37)
   bval_table(631) = bond_valence_t(25, 15, 9, -3, 2.24, 0.37)
   bval_table(632) = bond_valence_t(25, 33, 9, -3, 2.36, 0.37)
   bval_table(633) = bond_valence_t(25, 1, 9, -1, 1.55, 0.37)
   bval_table(634) = bond_valence_t(42, 16, 2, -2, 2.072, 0.422)
   bval_table(635) = bond_valence_t(42, 17, 2, -1, 2.052, 0.441)
   bval_table(636) = bond_valence_t(42, 8, 3, -2, 1.834, 0.37)
   bval_table(637) = bond_valence_t(42, 16, 3, -2, 2.062, 0.519)
   bval_table(638) = bond_valence_t(42, 9, 3, -1, 1.76, 0.35)
   bval_table(639) = bond_valence_t(42, 17, 3, -1, 2.22, 0.37)
   bval_table(640) = bond_valence_t(42, 35, 3, -1, 2.34, 0.37)
   bval_table(641) = bond_valence_t(42, 7, 3, -3, 1.96, 0.37)
   bval_table(642) = bond_valence_t(42, 8, 4, -2, 1.886, 0.37)
   bval_table(643) = bond_valence_t(42, 16, 4, -2, 2.235, 0.37)
   bval_table(644) = bond_valence_t(42, 9, 4, -1, 1.80, 0.37)
   bval_table(645) = bond_valence_t(42, 17, 4, -1, 2.128, 0.558)
   bval_table(646) = bond_valence_t(42, 7, 4, -3, 2.043, 0.37)
   bval_table(647) = bond_valence_t(42, 8, 5, -2, 1.907, 0.37)
   bval_table(648) = bond_valence_t(42, 16, 5, -2, 2.288, 0.37)
   bval_table(649) = bond_valence_t(42, 17, 5, -1, 2.26, 0.37)
   bval_table(650) = bond_valence_t(42, 7, 5, -3, 2.009, 0.37)
   bval_table(651) = bond_valence_t(42, 8, 6, -2, 1.907, 0.37)
   bval_table(652) = bond_valence_t(42, 16, 6, -2, 2.331, 0.37)
   bval_table(653) = bond_valence_t(42, 9, 6, -1, 1.81, 0.37)
   bval_table(654) = bond_valence_t(42, 17, 6, -1, 2.28, 0.37)
   bval_table(655) = bond_valence_t(42, 7, 6, -3, 2.009, 0.37)
   bval_table(656) = bond_valence_t(42, 8, 9, -2, 1.879, 0.30)
   bval_table(657) = bond_valence_t(42, 35, 9, -1, 2.43, 0.37)
   bval_table(658) = bond_valence_t(42, 53, 9, -1, 2.64, 0.37)
   bval_table(659) = bond_valence_t(42, 16, 9, -2, 2.35, 0.37)
   bval_table(660) = bond_valence_t(42, 34, 9, -2, 2.49, 0.37)
   bval_table(661) = bond_valence_t(42, 52, 9, -2, 2.69, 0.37)
   bval_table(662) = bond_valence_t(42, 7, 9, -3, 2.04, 0.37)
   bval_table(663) = bond_valence_t(42, 15, 9, -3, 2.44, 0.37)
   bval_table(664) = bond_valence_t(42, 33, 9, -3, 2.52, 0.37)
   bval_table(665) = bond_valence_t(42, 1, 9, -1, 1.73, 0.37)
   bval_table(666) = bond_valence_t(7, 8, 3, -2, 1.361, 0.37)
   bval_table(667) = bond_valence_t(7, 16, 3, -2, 1.73, 0.37)
   bval_table(668) = bond_valence_t(7, 9, 3, -1, 1.37, 0.37)
   bval_table(669) = bond_valence_t(7, 17, 3, -1, 1.75, 0.37)
   bval_table(670) = bond_valence_t(7, 7, -3, -3, 1.44, 0.35)
   bval_table(671) = bond_valence_t(7, 8, 5, -2, 1.432, 0.37)
   bval_table(672) = bond_valence_t(7, 9, 5, -1, 1.36, 0.37)
   bval_table(673) = bond_valence_t(7, 17, 5, -1, 1.80, 0.37)
   bval_table(674) = bond_valence_t(11, 8, 1, -2, 1.803, 0.37)
   bval_table(675) = bond_valence_t(11, 16, 1, -2, 2.300, 0.37)
   bval_table(676) = bond_valence_t(11, 34, 1, -2, 2.41, 0.37)
   bval_table(677) = bond_valence_t(11, 52, 1, -2, 2.64, 0.37)
   bval_table(678) = bond_valence_t(11, 9, 1, -1, 1.677, 0.37)
   bval_table(679) = bond_valence_t(11, 17, 1, -1, 2.15, 0.37)
   bval_table(680) = bond_valence_t(11, 35, 1, -1, 2.33, 0.37)
   bval_table(681) = bond_valence_t(11, 53, 1, -1, 2.56, 0.37)
   bval_table(682) = bond_valence_t(11, 7, 1, -3, 1.93, 0.37)
   bval_table(683) = bond_valence_t(11, 15, 1, -3, 2.36, 0.37)
   bval_table(684) = bond_valence_t(11, 33, 1, -3, 2.53, 0.37)
   bval_table(685) = bond_valence_t(11, 1, 1, -1, 1.68, 0.37)
   bval_table(686) = bond_valence_t(41, 8, 3, -2, 1.91, 0.35)
   bval_table(687) = bond_valence_t(41, 9, 3, -1, 1.71, 0.37)
   bval_table(688) = bond_valence_t(41, 17, 3, -1, 2.20, 0.37)
   bval_table(689) = bond_valence_t(41, 35, 3, -1, 2.35, 0.37)
   bval_table(690) = bond_valence_t(41, 8, 4, -2, 1.853, 0.479)
   bval_table(691) = bond_valence_t(41, 9, 4, -1, 1.90, 0.37)
   bval_table(692) = bond_valence_t(41, 17, 4, -1, 2.26, 0.35)
   bval_table(693) = bond_valence_t(41, 35, 4, -1, 2.62, 0.37)
   bval_table(694) = bond_valence_t(41, 7, 4, -3, 2.004, 0.37)
   bval_table(695) = bond_valence_t(41, 8, 5, -2, 1.911, 0.37)
   bval_table(696) = bond_valence_t(41, 9, 5, -1, 1.87, 0.37)
   bval_table(697) = bond_valence_t(41, 17, 5, -1, 2.27, 0.37)
   bval_table(698) = bond_valence_t(41, 53, 5, -1, 2.77, 0.37)
   bval_table(699) = bond_valence_t(41, 7, 5, -3, 2.01, 0.35)
   bval_table(700) = bond_valence_t(41, 35, 9, -1, 2.45, 0.37)
   bval_table(701) = bond_valence_t(41, 53, 9, -1, 2.68, 0.37)
   bval_table(702) = bond_valence_t(41, 16, 9, -2, 2.37, 0.37)
   bval_table(703) = bond_valence_t(41, 34, 9, -2, 2.51, 0.37)
   bval_table(704) = bond_valence_t(41, 52, 9, -2, 2.70, 0.37)
   bval_table(705) = bond_valence_t(41, 7, 9, -3, 2.06, 0.37)
   bval_table(706) = bond_valence_t(41, 15, 9, -3, 2.46, 0.37)
   bval_table(707) = bond_valence_t(41, 33, 9, -3, 2.54, 0.37)
   bval_table(708) = bond_valence_t(41, 1, 9, -1, 1.75, 0.37)
   bval_table(709) = bond_valence_t(60, 8, 2, -2, 1.95, 0.37)
   bval_table(710) = bond_valence_t(60, 16, 2, -2, 2.60, 0.35)
   bval_table(711) = bond_valence_t(60, 8, 3, -2, 2.021, 0.46)
   bval_table(712) = bond_valence_t(60, 16, 3, -2, 2.59, 0.37)
   bval_table(713) = bond_valence_t(60, 34, 3, -2, 2.71, 0.37)
   bval_table(714) = bond_valence_t(60, 52, 3, -2, 2.89, 0.37)
   bval_table(715) = bond_valence_t(60, 9, 3, -1, 2.008, 0.37)
   bval_table(716) = bond_valence_t(60, 17, 3, -1, 2.492, 0.37)
   bval_table(717) = bond_valence_t(60, 35, 3, -1, 2.66, 0.37)
   bval_table(718) = bond_valence_t(60, 53, 3, -1, 2.87, 0.37)
   bval_table(719) = bond_valence_t(60, 7, 3, -3, 2.201, 0.37)
   bval_table(720) = bond_valence_t(0, 8, 1, -2, 2.226, 0.37)
   bval_table(721) = bond_valence_t(0, 9, 1, -1, 2.129, 0.37)
   bval_table(722) = bond_valence_t(0, 17, 1, -1, 2.619, 0.37)
   bval_table(723) = bond_valence_t(28, 8, 2, -2, 1.675, 0.37)
   bval_table(724) = bond_valence_t(28, 16, 2, -2, 1.98, 0.37)
   bval_table(725) = bond_valence_t(28, 9, 2, -1, 1.596, 0.37)
   bval_table(726) = bond_valence_t(28, 17, 2, -1, 2.02, 0.37)
   bval_table(727) = bond_valence_t(28, 35, 2, -1, 2.20, 0.37)
   bval_table(728) = bond_valence_t(28, 53, 2, -1, 2.40, 0.37)
   bval_table(729) = bond_valence_t(28, 7, 2, -3, 1.70, 0.37)
   bval_table(730) = bond_valence_t(28, 8, 3, -2, 1.75, 0.37)
   bval_table(731) = bond_valence_t(28, 16, 3, -2, 2.040, 0.37)
   bval_table(732) = bond_valence_t(28, 9, 3, -1, 1.58, 0.37)
   bval_table(733) = bond_valence_t(28, 7, 3, -3, 1.731, 0.37)
   bval_table(734) = bond_valence_t(28, 8, 4, -2, 1.734, 0.335)
   bval_table(735) = bond_valence_t(28, 9, 4, -1, 1.61, 0.37)
   bval_table(736) = bond_valence_t(28, 35, 9, -1, 2.16, 0.37)
   bval_table(737) = bond_valence_t(28, 53, 9, -1, 2.34, 0.37)
   bval_table(738) = bond_valence_t(28, 16, 9, -2, 2.04, 0.37)
   bval_table(739) = bond_valence_t(28, 34, 9, -2, 2.14, 0.37)
   bval_table(740) = bond_valence_t(28, 52, 9, -2, 2.43, 0.37)
   bval_table(741) = bond_valence_t(28, 7, 9, -3, 1.75, 0.37)
   bval_table(742) = bond_valence_t(28, 15, 9, -3, 2.17, 0.37)
   bval_table(743) = bond_valence_t(28, 33, 9, -3, 2.24, 0.37)
   bval_table(744) = bond_valence_t(28, 1, 9, -1, 1.40, 0.37)
   bval_table(745) = bond_valence_t(93, 9, 3, -1, 2.00, 0.40)
   bval_table(746) = bond_valence_t(93, 17, 3, -1, 2.48, 0.40)
   bval_table(747) = bond_valence_t(93, 35, 3, -1, 2.62, 0.40)
   bval_table(748) = bond_valence_t(93, 53, 3, -1, 2.85, 0.40)
   bval_table(749) = bond_valence_t(93, 8, 4, -2, 2.18, 0.37)
   bval_table(750) = bond_valence_t(93, 9, 4, -1, 2.02, 0.37)
   bval_table(751) = bond_valence_t(93, 17, 4, -1, 2.46, 0.40)
   bval_table(752) = bond_valence_t(93, 8, 5, -2, 2.036, 0.411)
   bval_table(753) = bond_valence_t(93, 9, 5, -1, 1.97, 0.40)
   bval_table(754) = bond_valence_t(93, 17, 5, -1, 2.42, 0.40)
   bval_table(755) = bond_valence_t(93, 8, 6, -2, 2.022, 0.523)
   bval_table(756) = bond_valence_t(93, 9, 6, -1, 1.97, 0.40)
   bval_table(757) = bond_valence_t(93, 8, 7, -2, 2.076, 0.477)
   bval_table(758) = bond_valence_t(8, 8, -2, -2, 1.406, 0.37)
   bval_table(759) = bond_valence_t(76, 8, 4, -2, 1.811, 0.37)
   bval_table(760) = bond_valence_t(76, 16, 4, -2, 2.21, 0.37)
   bval_table(761) = bond_valence_t(76, 9, 4, -1, 1.72, 0.37)
   bval_table(762) = bond_valence_t(76, 17, 4, -1, 2.19, 0.37)
   bval_table(763) = bond_valence_t(76, 35, 4, -1, 2.37, 0.37)
   bval_table(764) = bond_valence_t(76, 8, 5, -2, 1.870, 0.485)
   bval_table(765) = bond_valence_t(76, 9, 5, -1, 1.81, 0.37)
   bval_table(766) = bond_valence_t(76, 8, 6, -2, 1.904, 0.375)
   bval_table(767) = bond_valence_t(76, 9, 6, -1, 1.80, 0.35)
   bval_table(768) = bond_valence_t(76, 8, 7, -2, 1.937, 0.349)
   bval_table(769) = bond_valence_t(76, 8, 8, -2, 1.966, 0.405)
   bval_table(770) = bond_valence_t(15, 8, 3, -2, 1.655, 0.399)
   bval_table(771) = bond_valence_t(15, 16, 3, -2, 2.12, 0.37)
   bval_table(772) = bond_valence_t(15, 34, 3, -2, 2.24, 0.37)
   bval_table(773) = bond_valence_t(15, 9, 3, -1, 1.53, 0.35)
   bval_table(774) = bond_valence_t(15, 8, 4, -2, 1.64, 0.37)
   bval_table(775) = bond_valence_t(15, 16, 4, -2, 2.13, 0.35)
   bval_table(776) = bond_valence_t(15, 9, 4, -1, 1.66, 0.37)
   bval_table(777) = bond_valence_t(15, 8, 5, -2, 1.617, 0.37)
   bval_table(778) = bond_valence_t(15, 16, 5, -2, 2.145, 0.37)
   bval_table(779) = bond_valence_t(15, 9, 5, -1, 1.54, 0.37)
   bval_table(780) = bond_valence_t(15, 17, 5, -1, 2.02, 0.37)
   bval_table(781) = bond_valence_t(15, 35, 5, -1, 2.17, 0.40)
   bval_table(782) = bond_valence_t(15, 7, 5, -3, 1.704, 0.37)
   bval_table(783) = bond_valence_t(15, 35, 9, -1, 2.15, 0.37)
   bval_table(784) = bond_valence_t(15, 53, 9, -1, 2.40, 0.37)
   bval_table(785) = bond_valence_t(15, 16, 9, -2, 2.11, 0.37)
   bval_table(786) = bond_valence_t(15, 34, 9, -2, 2.26, 0.37)
   bval_table(787) = bond_valence_t(15, 52, 9, -2, 2.44, 0.37)
   bval_table(788) = bond_valence_t(15, 7, 9, -3, 1.73, 0.37)
   bval_table(789) = bond_valence_t(15, 15, 9, -3, 2.19, 0.37)
   bval_table(790) = bond_valence_t(15, 33, 9, -3, 2.25, 0.37)
   bval_table(791) = bond_valence_t(15, 1, 9, -1, 1.41, 0.37)
   bval_table(792) = bond_valence_t(15, 15, 5, 5, 2.22, 0.35)
   bval_table(793) = bond_valence_t(91, 8, 4, -2, 2.15, 0.35)
   bval_table(794) = bond_valence_t(91, 9, 4, -1, 2.02, 0.40)
   bval_table(795) = bond_valence_t(91, 17, 4, -1, 2.49, 0.40)
   bval_table(796) = bond_valence_t(91, 35, 4, -1, 2.66, 0.40)
   bval_table(797) = bond_valence_t(91, 8, 5, -2, 2.09, 0.35)
   bval_table(798) = bond_valence_t(91, 9, 5, -1, 2.04, 0.37)
   bval_table(799) = bond_valence_t(91, 17, 5, -1, 2.45, 0.40)
   bval_table(800) = bond_valence_t(91, 35, 5, -1, 2.58, 0.40)
   bval_table(801) = bond_valence_t(82, 8, 2, -2, 1.963, 0.49)
   bval_table(802) = bond_valence_t(82, 16, 2, -2, 2.42, 0.50)
   bval_table(803) = bond_valence_t(82, 34, 2, -2, 2.69, 0.37)
   bval_table(804) = bond_valence_t(82, 9, 2, -1, 2.03, 0.37)
   bval_table(805) = bond_valence_t(82, 17, 2, -1, 2.53, 0.37)
   bval_table(806) = bond_valence_t(82, 35, 2, -1, 2.598, 0.40)
   bval_table(807) = bond_valence_t(82, 53, 2, -1, 2.83, 0.37)
   bval_table(808) = bond_valence_t(82, 7, 2, -3, 2.18, 0.40)
   bval_table(809) = bond_valence_t(82, 8, 4, -2, 2.042, 0.37)
   bval_table(810) = bond_valence_t(82, 9, 4, -1, 1.94, 0.37)
   bval_table(811) = bond_valence_t(82, 17, 4, -1, 2.43, 0.37)
   bval_table(812) = bond_valence_t(82, 35, 4, -1, 3.04, 0.35)
   bval_table(813) = bond_valence_t(82, 35, 9, -1, 2.64, 0.37)
   bval_table(814) = bond_valence_t(82, 53, 9, -1, 2.78, 0.37)
   bval_table(815) = bond_valence_t(82, 16, 9, -2, 2.55, 0.37)
   bval_table(816) = bond_valence_t(82, 34, 9, -2, 2.67, 0.37)
   bval_table(817) = bond_valence_t(82, 52, 9, -2, 2.84, 0.37)
   bval_table(818) = bond_valence_t(82, 7, 9, -3, 2.22, 0.37)
   bval_table(819) = bond_valence_t(82, 15, 9, -3, 2.64, 0.37)
   bval_table(820) = bond_valence_t(82, 33, 9, -3, 2.72, 0.37)
   bval_table(821) = bond_valence_t(82, 1, 9, -1, 1.97, 0.37)
   bval_table(822) = bond_valence_t(46, 8, 2, -2, 1.792, 0.37)
   bval_table(823) = bond_valence_t(46, 16, 2, -2, 2.09, 0.37)
   bval_table(824) = bond_valence_t(46, 9, 2, -1, 1.74, 0.37)
   bval_table(825) = bond_valence_t(46, 17, 2, -1, 2.05, 0.37)
   bval_table(826) = bond_valence_t(46, 35, 2, -1, 2.20, 0.37)
   bval_table(827) = bond_valence_t(46, 53, 2, -1, 2.36, 0.37)
   bval_table(828) = bond_valence_t(46, 7, 2, -3, 1.82, 0.35)
   bval_table(829) = bond_valence_t(46, 6, 2, -4, 1.73, 0.37)
   bval_table(830) = bond_valence_t(46, 8, 4, -2, 1.856, 0.352)
   bval_table(831) = bond_valence_t(46, 16, 4, -2, 2.30, 0.37)
   bval_table(832) = bond_valence_t(46, 9, 4, -1, 1.66, 0.37)
   bval_table(833) = bond_valence_t(46, 35, 9, -1, 2.19, 0.37)
   bval_table(834) = bond_valence_t(46, 53, 9, -1, 2.38, 0.37)
   bval_table(835) = bond_valence_t(46, 16, 9, -2, 2.10, 0.37)
   bval_table(836) = bond_valence_t(46, 34, 9, -2, 2.22, 0.37)
   bval_table(837) = bond_valence_t(46, 52, 9, -2, 2.48, 0.37)
   bval_table(838) = bond_valence_t(46, 7, 9, -3, 1.81, 0.37)
   bval_table(839) = bond_valence_t(46, 15, 9, -3, 2.22, 0.37)
   bval_table(840) = bond_valence_t(46, 33, 9, -3, 2.30, 0.37)
   bval_table(841) = bond_valence_t(46, 1, 9, -1, 1.47, 0.37)
   bval_table(842) = bond_valence_t(61, 9, 3, -1, 1.96, 0.40)
   bval_table(843) = bond_valence_t(61, 17, 3, -1, 2.45, 0.40)
   bval_table(844) = bond_valence_t(61, 35, 3, -1, 2.59, 0.40)
   bval_table(845) = bond_valence_t(61, 17, 3, -1, 2.82, 0.40)
   bval_table(846) = bond_valence_t(84, 8, 4, -2, 2.19, 0.37)
   bval_table(847) = bond_valence_t(84, 9, 4, -1, 2.38, 0.37)
   bval_table(848) = bond_valence_t(59, 8, 3, -2, 2.138, 0.37)
   bval_table(849) = bond_valence_t(59, 16, 3, -2, 2.60, 0.37)
   bval_table(850) = bond_valence_t(59, 34, 3, -1, 2.72, 0.37)
   bval_table(851) = bond_valence_t(59, 52, 3, -2, 2.90, 0.37)
   bval_table(852) = bond_valence_t(59, 9, 3, -1, 2.022, 0.37)
   bval_table(853) = bond_valence_t(59, 17, 3, -1, 2.50, 0.37)
   bval_table(854) = bond_valence_t(59, 35, 3, -1, 2.67, 0.37)
   bval_table(855) = bond_valence_t(59, 53, 3, -1, 2.89, 0.37)
   bval_table(856) = bond_valence_t(59, 7, 3, -3, 2.215, 0.37)
   bval_table(857) = bond_valence_t(59, 15, 3, -3, 2.68, 0.37)
   bval_table(858) = bond_valence_t(59, 33, 3, -3, 2.75, 0.37)
   bval_table(859) = bond_valence_t(59, 1, 3, -1, 2.02, 0.37)
   bval_table(860) = bond_valence_t(78, 8, 2, -2, 1.768, 0.37)
   bval_table(861) = bond_valence_t(78, 16, 2, -2, 2.16, 0.37)
   bval_table(862) = bond_valence_t(78, 9, 2, -1, 1.68, 0.37)
   bval_table(863) = bond_valence_t(78, 17, 2, -1, 2.05, 0.37)
   bval_table(864) = bond_valence_t(78, 35, 2, -1, 2.20, 0.37)
   bval_table(865) = bond_valence_t(78, 6, 2, 2, 1.760, 0.37)
   bval_table(866) = bond_valence_t(78, 7, 2, -3, 1.81, 0.37)
   bval_table(867) = bond_valence_t(78, 8, 3, -2, 1.856, 0.407)
   bval_table(868) = bond_valence_t(78, 17, 3, -1, 2.30, 0.37)
   bval_table(869) = bond_valence_t(78, 35, 3, -1, 2.47, 0.35)
   bval_table(870) = bond_valence_t(78, 8, 4, -2, 1.879, 0.37)
   bval_table(871) = bond_valence_t(78, 9, 4, -1, 1.759, 0.37)
   bval_table(872) = bond_valence_t(78, 17, 4, -1, 2.17, 0.37)
   bval_table(873) = bond_valence_t(78, 35, 4, -1, 2.6, 0.35)
   bval_table(874) = bond_valence_t(78, 35, 9, -1, 2.18, 0.37)
   bval_table(875) = bond_valence_t(78, 53, 9, -1, 2.37, 0.37)
   bval_table(876) = bond_valence_t(78, 16, 9, -2, 2.08, 0.37)
   bval_table(877) = bond_valence_t(78, 34, 9, -2, 2.19, 0.37)
   bval_table(878) = bond_valence_t(78, 52, 9, -2, 2.45, 0.37)
   bval_table(879) = bond_valence_t(78, 7, 9, -3, 1.77, 0.37)
   bval_table(880) = bond_valence_t(78, 15, 9, -3, 2.19, 0.37)
   bval_table(881) = bond_valence_t(78, 33, 9, -3, 2.26, 0.37)
   bval_table(882) = bond_valence_t(78, 1, 9, -1, 1.40, 0.37)
   bval_table(883) = bond_valence_t(94, 8, 3, -2, 2.11, 0.37)
   bval_table(884) = bond_valence_t(94, 9, 3, -1, 2.00, 0.37)
   bval_table(885) = bond_valence_t(94, 17, 3, -1, 2.48, 0.37)
   bval_table(886) = bond_valence_t(94, 35, 3, -1, 2.60, 0.40)
   bval_table(887) = bond_valence_t(94, 53, 3, -1, 2.84, 0.40)
   bval_table(888) = bond_valence_t(94, 8, 4, -2, 2.09, 0.35)
   bval_table(889) = bond_valence_t(94, 9, 4, -1, 1.97, 0.40)
   bval_table(890) = bond_valence_t(94, 17, 4, -1, 2.44, 0.40)
   bval_table(891) = bond_valence_t(94, 8, 5, -2, 2.11, 0.37)
   bval_table(892) = bond_valence_t(94, 9, 5, -1, 1.96, 0.40)
   bval_table(893) = bond_valence_t(94, 8, 6, -2, 2.06, 0.35)
   bval_table(894) = bond_valence_t(94, 9, 6, -1, 1.96, 0.40)
   bval_table(895) = bond_valence_t(94, 8, 7, -2, 2.05, 0.35)
   bval_table(896) = bond_valence_t(37, 8, 1, -2, 2.263, 0.37)
   bval_table(897) = bond_valence_t(37, 16, 1, -2, 2.70, 0.37)
   bval_table(898) = bond_valence_t(37, 34, 1, -2, 2.81, 0.37)
   bval_table(899) = bond_valence_t(37, 52, 1, -2, 3.00, 0.37)
   bval_table(900) = bond_valence_t(37, 9, 1, -1, 2.16, 0.37)
   bval_table(901) = bond_valence_t(37, 17, 1, -1, 2.652, 0.37)
   bval_table(902) = bond_valence_t(37, 35, 1, -1, 2.78, 0.37)
   bval_table(903) = bond_valence_t(37, 53, 1, -1, 3.01, 0.37)
   bval_table(904) = bond_valence_t(37, 7, 1, -3, 2.62, 0.37)
   bval_table(905) = bond_valence_t(37, 15, 1, -3, 2.76, 0.37)
   bval_table(906) = bond_valence_t(37, 33, 1, -3, 2.87, 0.37)
   bval_table(907) = bond_valence_t(37, 1, 1, -1, 2.26, 0.37)
   bval_table(908) = bond_valence_t(75, 17, 1, -1, 2.62, 0.35)
   bval_table(909) = bond_valence_t(75, 8, 3, -2, 1.9, 0.35)
   bval_table(910) = bond_valence_t(75, 17, 3, -1, 2.23, 0.37)
   bval_table(911) = bond_valence_t(75, 9, 4, -1, 1.81, 0.37)
   bval_table(912) = bond_valence_t(75, 17, 4, -1, 2.23, 0.37)
   bval_table(913) = bond_valence_t(75, 35, 4, -1, 2.35, 0.37)
   bval_table(914) = bond_valence_t(75, 8, 5, -2, 1.834, 0.557)
   bval_table(915) = bond_valence_t(75, 17, 5, -1, 2.24, 0.37)
   bval_table(916) = bond_valence_t(75, 9, 6, -1, 1.79, 0.37)
   bval_table(917) = bond_valence_t(75, 8, 7, -2, 1.943, 0.406)
   bval_table(918) = bond_valence_t(75, 9, 7, -1, 1.86, 0.37)
   bval_table(919) = bond_valence_t(75, 17, 7, -1, 2.23, 0.37)
   bval_table(920) = bond_valence_t(75, 35, 9, -1, 2.45, 0.37)
   bval_table(921) = bond_valence_t(75, 53, 9, -1, 2.61, 0.37)
   bval_table(922) = bond_valence_t(75, 16, 9, -2, 2.37, 0.37)
   bval_table(923) = bond_valence_t(75, 34, 9, -2, 2.50, 0.37)
   bval_table(924) = bond_valence_t(75, 52, 9, -2, 2.70, 0.37)
   bval_table(925) = bond_valence_t(75, 7, 9, -3, 2.06, 0.37)
   bval_table(926) = bond_valence_t(75, 15, 9, -3, 2.46, 0.37)
   bval_table(927) = bond_valence_t(75, 33, 9, -3, 2.54, 0.37)
   bval_table(928) = bond_valence_t(75, 1, 9, -1, 1.75, 0.37)
   bval_table(929) = bond_valence_t(45, 8, 3, -2, 1.793, 0.37)
   bval_table(930) = bond_valence_t(45, 9, 3, -1, 1.71, 0.37)
   bval_table(931) = bond_valence_t(45, 17, 3, -1, 2.08, 0.37)
   bval_table(932) = bond_valence_t(45, 35, 3, -1, 2.27, 0.35)
   bval_table(933) = bond_valence_t(45, 7, 3, -3, 1.82, 0.35)
   bval_table(934) = bond_valence_t(45, 8, 4, -2, 2.836, 0.422)
   bval_table(935) = bond_valence_t(45, 9, 4, -1, 1.59, 0.37)
   bval_table(936) = bond_valence_t(45, 9, 5, -1, 1.80, 0.37)
   bval_table(937) = bond_valence_t(45, 35, 9, -1, 2.25, 0.37)
   bval_table(938) = bond_valence_t(45, 53, 9, -1, 2.48, 0.37)
   bval_table(939) = bond_valence_t(45, 16, 9, -2, 2.15, 0.37)
   bval_table(940) = bond_valence_t(45, 34, 9, -1, 2.33, 0.37)
   bval_table(941) = bond_valence_t(45, 52, 9, -2, 2.55, 0.37)
   bval_table(942) = bond_valence_t(45, 7, 9, -3, 1.88, 0.37)
   bval_table(943) = bond_valence_t(45, 15, 9, -3, 2.29, 0.37)
   bval_table(944) = bond_valence_t(45, 33, 9, -3, 2.37, 0.37)
   bval_table(945) = bond_valence_t(45, 1, 9, -1, 1.55, 0.37)
   bval_table(946) = bond_valence_t(44, 34, 2, -2, 2.11, 0.35)
   bval_table(947) = bond_valence_t(44, 9, 2, -1, 1.84, 0.35)
   bval_table(948) = bond_valence_t(44, 8, 3, -2, 1.77, 0.37)
   bval_table(949) = bond_valence_t(44, 16, 3, -2, 2.20, 0.35)
   bval_table(950) = bond_valence_t(44, 9, 3, -1, 2.12, 0.37)
   bval_table(951) = bond_valence_t(44, 17, 3, -1, 2.25, 0.37)
   bval_table(952) = bond_valence_t(44, 7, 3, -3, 1.82, 0.35)
   bval_table(953) = bond_valence_t(44, 8, 4, -2, 1.833, 0.366)
   bval_table(954) = bond_valence_t(44, 16, 4, -2, 2.21, 0.37)
   bval_table(955) = bond_valence_t(44, 9, 4, -1, 1.74, 0.37)
   bval_table(956) = bond_valence_t(44, 17, 4, -1, 2.21, 0.37)
   bval_table(957) = bond_valence_t(44, 8, 5, -2, 1.90, 0.37)
   bval_table(958) = bond_valence_t(44, 9, 5, -1, 1.82, 0.37)
   bval_table(959) = bond_valence_t(44, 17, 5, -1, 2.23, 0.35)
   bval_table(960) = bond_valence_t(44, 8, 6, -2, 1.87, 0.35)
   bval_table(961) = bond_valence_t(44, 8, 7, -2, 1.99, 0.37)
   bval_table(962) = bond_valence_t(44, 35, 9, -1, 2.26, 0.37)
   bval_table(963) = bond_valence_t(44, 53, 9, -1, 2.48, 0.37)
   bval_table(964) = bond_valence_t(44, 16, 9, -2, 2.16, 0.37)
   bval_table(965) = bond_valence_t(44, 34, 9, -2, 2.33, 0.37)
   bval_table(966) = bond_valence_t(44, 52, 9, -2, 2.54, 0.37)
   bval_table(967) = bond_valence_t(44, 7, 9, -3, 1.88, 0.37)
   bval_table(968) = bond_valence_t(44, 15, 9, -3, 2.29, 0.37)
   bval_table(969) = bond_valence_t(44, 33, 9, -3, 2.36, 0.37)
   bval_table(970) = bond_valence_t(44, 1, 9, -1, 1.61, 0.37)
   bval_table(971) = bond_valence_t(16, 8, 2, -2, 1.74, 0.37)
   bval_table(972) = bond_valence_t(16, 16, 2, -2, 2.03, 0.37)
   bval_table(973) = bond_valence_t(16, 7, 2, -2, 1.597, 0.37)
   bval_table(974) = bond_valence_t(16, 7, 2, -3, 1.682, 0.37)
   bval_table(975) = bond_valence_t(16, 16, 2, 2, 2.10, 0.35)
   bval_table(976) = bond_valence_t(16, 8, 4, -2, 1.644, 0.37)
   bval_table(977) = bond_valence_t(16, 16, 4, -4, 2.35, 0.37)
   bval_table(978) = bond_valence_t(16, 9, 4, -1, 1.60, 0.37)
   bval_table(979) = bond_valence_t(16, 17, 4, -1, 2.02, 0.37)
   bval_table(980) = bond_valence_t(16, 7, 4, -3, 1.762, 0.37)
   bval_table(981) = bond_valence_t(16, 8, 6, -2, 1.624, 0.37)
   bval_table(982) = bond_valence_t(16, 9, 6, -1, 1.56, 0.37)
   bval_table(983) = bond_valence_t(16, 17, 6, -1, 2.03, 0.37)
   bval_table(984) = bond_valence_t(16, 7, 6, -3, 1.72, 0.37)
   bval_table(985) = bond_valence_t(16, 35, 9, -1, 2.17, 0.37)
   bval_table(986) = bond_valence_t(16, 53, 9, -1, 2.36, 0.37)
   bval_table(987) = bond_valence_t(16, 16, 9, -2, 2.07, 0.37)
   bval_table(988) = bond_valence_t(16, 34, 9, -2, 2.21, 0.37)
   bval_table(989) = bond_valence_t(16, 52, 9, -2, 2.45, 0.37)
   bval_table(990) = bond_valence_t(16, 7, 9, -3, 1.74, 0.37)
   bval_table(991) = bond_valence_t(16, 15, 9, -3, 2.15, 0.37)
   bval_table(992) = bond_valence_t(16, 33, 9, -3, 2.25, 0.37)
   bval_table(993) = bond_valence_t(16, 1, 9, -1, 1.38, 0.37)
   bval_table(994) = bond_valence_t(51, 8, 3, -2, 1.885, 0.53)
   bval_table(995) = bond_valence_t(51, 16, 3, -2, 2.38, 0.50)
   bval_table(996) = bond_valence_t(51, 34, 3, -2, 2.60, 0.37)
   bval_table(997) = bond_valence_t(51, 9, 3, -1, 1.883, 0.37)
   bval_table(998) = bond_valence_t(51, 17, 3, -1, 2.35, 0.37)
   bval_table(999) = bond_valence_t(51, 35, 3, -1, 2.51, 0.37)
   bval_table(1000) = bond_valence_t(51, 53, 3, -1, 2.76, 0.37)
   bval_table(1001) = bond_valence_t(51, 7, 3, -3, 2.108, 0.37)
   bval_table(1002) = bond_valence_t(51, 8, 5, -2, 1.904, 0.430)
   bval_table(1003) = bond_valence_t(51, 9, 5, -1, 1.797, 0.37)
   bval_table(1004) = bond_valence_t(51, 17, 5, -1, 2.30, 0.37)
   bval_table(1005) = bond_valence_t(51, 35, 5, -1, 2.48, 0.37)
   bval_table(1006) = bond_valence_t(51, 7, 5, -3, 1.99, 0.35)
   bval_table(1007) = bond_valence_t(51, 8, 9, -2, 1.934, 0.37)
   bval_table(1008) = bond_valence_t(51, 16, 9, -2, 2.45, 0.37)
   bval_table(1009) = bond_valence_t(51, 34, 9, -2, 2.57, 0.37)
   bval_table(1010) = bond_valence_t(51, 52, 9, -2, 2.78, 0.37)
   bval_table(1011) = bond_valence_t(51, 35, 9, -1, 2.50, 0.37)
   bval_table(1012) = bond_valence_t(51, 53, 9, -1, 2.72, 0.37)
   bval_table(1013) = bond_valence_t(51, 7, 9, -3, 2.12, 0.37)
   bval_table(1014) = bond_valence_t(51, 15, 9, -3, 2.52, 0.37)
   bval_table(1015) = bond_valence_t(51, 33, 9, -3, 2.60, 0.37)
   bval_table(1016) = bond_valence_t(51, 1, 9, -1, 2.77, 0.37)
   bval_table(1017) = bond_valence_t(21, 8, 3, -2, 1.849, 0.37)
   bval_table(1018) = bond_valence_t(21, 16, 3, -2, 2.321, 0.37)
   bval_table(1019) = bond_valence_t(21, 34, 3, -2, 2.44, 0.37)
   bval_table(1020) = bond_valence_t(21, 52, 3, -2, 2.64, 0.37)
   bval_table(1021) = bond_valence_t(21, 9, 3, -1, 1.76, 0.37)
   bval_table(1022) = bond_valence_t(21, 17, 3, -1, 2.36, 0.37)
   bval_table(1023) = bond_valence_t(21, 35, 3, -1, 2.38, 0.37)
   bval_table(1024) = bond_valence_t(21, 53, 3, -1, 2.59, 0.37)
   bval_table(1025) = bond_valence_t(21, 7, 3, -3, 1.98, 0.37)
   bval_table(1026) = bond_valence_t(21, 15, 3, -3, 2.40, 0.37)
   bval_table(1027) = bond_valence_t(21, 33, 3, -3, 2.48, 0.37)
   bval_table(1028) = bond_valence_t(21, 1, 3, -1, 1.68, 0.37)
   bval_table(1029) = bond_valence_t(34, 16, 2, -2, 2.21, 0.37)
   bval_table(1030) = bond_valence_t(34, 34, 2, -2, 2.33, 0.37)
   bval_table(1031) = bond_valence_t(34, 8, 4, -2, 1.811, 0.37)
   bval_table(1032) = bond_valence_t(34, 9, 4, -1, 1.73, 0.37)
   bval_table(1033) = bond_valence_t(34, 17, 4, -1, 2.22, 0.37)
   bval_table(1034) = bond_valence_t(34, 35, 4, -1, 2.43, 0.37)
   bval_table(1035) = bond_valence_t(34, 8, 6, -2, 1.788, 0.37)
   bval_table(1036) = bond_valence_t(34, 9, 6, -1, 1.69, 0.37)
   bval_table(1037) = bond_valence_t(34, 17, 6, -1, 2.16, 0.37)
   bval_table(1038) = bond_valence_t(34, 7, 6, -3, 1.90, 0.35)
   bval_table(1039) = bond_valence_t(34, 35, 9, -1, 2.33, 0.37)
   bval_table(1040) = bond_valence_t(34, 53, 9, -1, 2.54, 0.37)
   bval_table(1041) = bond_valence_t(34, 16, 9, -2, 2.25, 0.37)
   bval_table(1042) = bond_valence_t(34, 34, 9, -2, 2.36, 0.37)
   bval_table(1043) = bond_valence_t(34, 52, 9, -2, 2.55, 0.37)
   bval_table(1044) = bond_valence_t(34, 15, 9, -3, 2.34, 0.37)
   bval_table(1045) = bond_valence_t(34, 33, 9, -3, 2.42, 0.37)
   bval_table(1046) = bond_valence_t(34, 1, 9, -1, 1.54, 0.37)
   bval_table(1047) = bond_valence_t(14, 8, 4, -2, 1.624, 0.37)
   bval_table(1048) = bond_valence_t(14, 16, 4, -2, 2.126, 0.37)
   bval_table(1049) = bond_valence_t(14, 34, 4, -2, 2.26, 0.37)
   bval_table(1050) = bond_valence_t(14, 52, 4, -2, 2.49, 0.37)
   bval_table(1051) = bond_valence_t(14, 9, 4, -1, 1.58, 0.37)
   bval_table(1052) = bond_valence_t(14, 17, 4, -1, 2.03, 0.37)
   bval_table(1053) = bond_valence_t(14, 35, 4, -1, 2.20, 0.37)
   bval_table(1054) = bond_valence_t(14, 53, 4, -1, 2.41, 0.37)
   bval_table(1055) = bond_valence_t(14, 6, 4, -4, 1.883, 0.37)
   bval_table(1056) = bond_valence_t(14, 7, 4, -3, 1.724, 0.37)
   bval_table(1057) = bond_valence_t(14, 15, 4, -3, 2.23, 0.37)
   bval_table(1058) = bond_valence_t(14, 33, 4, -3, 2.31, 0.37)
   bval_table(1059) = bond_valence_t(14, 1, 4, -1, 1.47, 0.37)
   bval_table(1060) = bond_valence_t(62, 8, 2, -2, 2.116, 0.37)
   bval_table(1061) = bond_valence_t(62, 7, 2, -3, 2.267, 0.37)
   bval_table(1062) = bond_valence_t(62, 8, 3, -2, 2.088, 0.37)
   bval_table(1063) = bond_valence_t(62, 16, 3, -2, 2.55, 0.37)
   bval_table(1064) = bond_valence_t(62, 34, 3, -2, 2.67, 0.37)
   bval_table(1065) = bond_valence_t(62, 52, 3, -2, 2.86, 0.37)
   bval_table(1066) = bond_valence_t(62, 9, 3, -1, 1.94, 0.40)
   bval_table(1067) = bond_valence_t(62, 17, 3, -1, 2.466, 0.37)
   bval_table(1068) = bond_valence_t(62, 35, 3, -1, 2.66, 0.37)
   bval_table(1069) = bond_valence_t(62, 53, 3, -1, 2.84, 0.37)
   bval_table(1070) = bond_valence_t(62, 7, 3, -3, 2.171, 0.37)
   bval_table(1071) = bond_valence_t(62, 15, 3, -3, 2.63, 0.37)
   bval_table(1072) = bond_valence_t(62, 33, 3, -3, 2.70, 0.37)
   bval_table(1073) = bond_valence_t(62, 1, 3, -1, 1.96, 0.37)
   bval_table(1074) = bond_valence_t(50, 8, 2, -2, 1.849, 0.50)
   bval_table(1075) = bond_valence_t(50, 16, 2, -2, 2.35, 0.50)
   bval_table(1076) = bond_valence_t(50, 34, 2, -2, 2.476, 0.37)
   bval_table(1077) = bond_valence_t(50, 52, 2, -2, 2.747, 0.37)
   bval_table(1078) = bond_valence_t(50, 9, 2, -1, 1.925, 0.37)
   bval_table(1079) = bond_valence_t(50, 17, 2, -1, 2.335, 0.43)
   bval_table(1080) = bond_valence_t(50, 35, 2, -1, 2.500, 0.37)
   bval_table(1081) = bond_valence_t(50, 53, 2, -1, 2.752, 0.37)
   bval_table(1082) = bond_valence_t(50, 7, 2, -3, 2.046, 0.37)
   bval_table(1083) = bond_valence_t(50, 15, 2, -3, 2.488, 0.37)
   bval_table(1084) = bond_valence_t(50, 33, 2, -3, 2.585, 0.37)
   bval_table(1085) = bond_valence_t(50, 6, 2, -4, 2.077, 0.37)
   bval_table(1086) = bond_valence_t(50, 8, 4, -2, 1.905, 0.37)
   bval_table(1087) = bond_valence_t(50, 16, 4, -2, 2.399, 0.37)
   bval_table(1088) = bond_valence_t(50, 34, 4, -2, 2.524, 0.37)
   bval_table(1089) = bond_valence_t(50, 9, 4, -1, 1.843, 0.37)
   bval_table(1090) = bond_valence_t(50, 17, 4, -1, 2.276, 0.37)
   bval_table(1091) = bond_valence_t(50, 35, 4, -1, 2.444, 0.37)
   bval_table(1092) = bond_valence_t(50, 53, 4, -1, 2.700, 0.37)
   bval_table(1093) = bond_valence_t(50, 7, 4, -3, 2.024, 0.37)
   bval_table(1094) = bond_valence_t(50, 35, 9, -1, 2.55, 0.37)
   bval_table(1095) = bond_valence_t(50, 53, 9, -1, 2.76, 0.37)
   bval_table(1096) = bond_valence_t(50, 16, 9, -2, 2.39, 0.37)
   bval_table(1097) = bond_valence_t(50, 34, 9, -2, 2.59, 0.37)
   bval_table(1098) = bond_valence_t(50, 52, 9, -2, 2.76, 0.37)
   bval_table(1099) = bond_valence_t(50, 7, 9, -3, 2.06, 0.37)
   bval_table(1100) = bond_valence_t(50, 15, 9, -3, 2.45, 0.37)
   bval_table(1101) = bond_valence_t(50, 33, 9, -3, 2.62, 0.37)
   bval_table(1102) = bond_valence_t(50, 1, 9, -1, 1.85, 0.37)
   bval_table(1103) = bond_valence_t(38, 8, 2, -2, 2.118, 0.37)
   bval_table(1104) = bond_valence_t(38, 16, 2, -2, 2.59, 0.37)
   bval_table(1105) = bond_valence_t(38, 34, 2, -2, 2.72, 0.37)
   bval_table(1106) = bond_valence_t(38, 52, 2, -2, 2.87, 0.37)
   bval_table(1107) = bond_valence_t(38, 9, 2, -1, 2.019, 0.37)
   bval_table(1108) = bond_valence_t(38, 17, 2, -1, 2.51, 0.37)
   bval_table(1109) = bond_valence_t(38, 35, 2, -1, 2.68, 0.37)
   bval_table(1110) = bond_valence_t(38, 53, 2, -1, 2.88, 0.37)
   bval_table(1111) = bond_valence_t(38, 7, 2, -3, 2.23, 0.37)
   bval_table(1112) = bond_valence_t(38, 15, 2, -3, 2.67, 0.37)
   bval_table(1113) = bond_valence_t(38, 33, 2, -3, 2.76, 0.37)
   bval_table(1114) = bond_valence_t(38, 1, 2, -1, 2.01, 0.37)
   bval_table(1115) = bond_valence_t(73, 8, 4, -2, 2.29, 0.37)
   bval_table(1116) = bond_valence_t(73, 8, 5, -2, 1.920, 0.37)
   bval_table(1117) = bond_valence_t(73, 16, 5, -2, 2.47, 0.37)
   bval_table(1118) = bond_valence_t(73, 9, 5, -1, 1.88, 0.37)
   bval_table(1119) = bond_valence_t(73, 17, 5, -1, 2.30, 0.37)
   bval_table(1120) = bond_valence_t(73, 35, 9, -1, 2.45, 0.37)
   bval_table(1121) = bond_valence_t(73, 53, 9, -1, 2.66, 0.37)
   bval_table(1122) = bond_valence_t(73, 16, 9, -2, 2.39, 0.37)
   bval_table(1123) = bond_valence_t(73, 34, 9, -2, 2.51, 0.37)
   bval_table(1124) = bond_valence_t(73, 52, 9, -2, 2.70, 0.37)
   bval_table(1125) = bond_valence_t(73, 7, 9, -3, 2.01, 0.37)
   bval_table(1126) = bond_valence_t(73, 15, 9, -3, 2.47, 0.37)
   bval_table(1127) = bond_valence_t(73, 33, 9, -3, 2.55, 0.37)
   bval_table(1128) = bond_valence_t(73, 1, 9, -1, 1.76, 0.37)
   bval_table(1129) = bond_valence_t(65, 8, 3, -2, 2.032, 0.37)
   bval_table(1130) = bond_valence_t(65, 16, 3, -2, 2.51, 0.37)
   bval_table(1131) = bond_valence_t(65, 34, 3, -2, 2.63, 0.37)
   bval_table(1132) = bond_valence_t(65, 52, 3, -2, 2.82, 0.37)
   bval_table(1133) = bond_valence_t(65, 9, 3, -1, 1.936, 0.37)
   bval_table(1134) = bond_valence_t(65, 17, 3, -1, 2.427, 0.37)
   bval_table(1135) = bond_valence_t(65, 35, 3, -1, 2.58, 0.37)
   bval_table(1136) = bond_valence_t(65, 53, 3, -1, 2.80, 0.37)
   bval_table(1137) = bond_valence_t(65, 7, 3, -3, 2.130, 0.37)
   bval_table(1138) = bond_valence_t(65, 15, 3, -3, 2.59, 0.37)
   bval_table(1139) = bond_valence_t(65, 33, 3, -3, 2.66, 0.37)
   bval_table(1140) = bond_valence_t(65, 1, 3, -1, 1.91, 0.37)
   bval_table(1141) = bond_valence_t(65, 8, 4, -2, 2.018, 0.395)
   bval_table(1142) = bond_valence_t(43, 8, 3, -2, 1.768, 0.37)
   bval_table(1143) = bond_valence_t(43, 8, 4, -2, 1.841, 0.37)
   bval_table(1144) = bond_valence_t(43, 9, 4, -1, 1.88, 0.40)
   bval_table(1145) = bond_valence_t(43, 17, 4, -1, 2.21, 0.37)
   bval_table(1146) = bond_valence_t(43, 8, 5, -2, 1.859, 0.37)
   bval_table(1147) = bond_valence_t(43, 8, 6, -2, 1.955, 0.37)
   bval_table(1148) = bond_valence_t(43, 8, 7, -2, 1.909, 0.37)
   bval_table(1149) = bond_valence_t(52, 8, 4, -2, 1.955, 0.44)
   bval_table(1150) = bond_valence_t(52, 16, 4, -2, 2.44, 0.37)
   bval_table(1151) = bond_valence_t(52, 9, 4, -1, 1.87, 0.37)
   bval_table(1152) = bond_valence_t(52, 17, 4, -1, 2.312, 0.56)
   bval_table(1153) = bond_valence_t(52, 35, 4, -1, 2.55, 0.37)
   bval_table(1154) = bond_valence_t(52, 53, 4, -1, 2.782, 0.37)
   bval_table(1155) = bond_valence_t(52, 8, 6, -2, 1.917, 0.37)
   bval_table(1156) = bond_valence_t(52, 9, 6, -1, 1.82, 0.37)
   bval_table(1157) = bond_valence_t(52, 17, 6, -1, 2.30, 0.37)
   bval_table(1158) = bond_valence_t(52, 35, 9, -1, 2.53, 0.37)
   bval_table(1159) = bond_valence_t(52, 53, 9, -1, 2.76, 0.37)
   bval_table(1160) = bond_valence_t(52, 16, 9, -2, 2.45, 0.37)
   bval_table(1161) = bond_valence_t(52, 34, 9, -2, 2.53, 0.37)
   bval_table(1162) = bond_valence_t(52, 52, 9, -2, 2.76, 0.37)
   bval_table(1163) = bond_valence_t(52, 7, 9, -3, 2.12, 0.37)
   bval_table(1164) = bond_valence_t(52, 15, 9, -3, 2.52, 0.37)
   bval_table(1165) = bond_valence_t(52, 33, 9, -3, 2.60, 0.37)
   bval_table(1166) = bond_valence_t(52, 1, 9, -1, 1.83, 0.37)
   bval_table(1167) = bond_valence_t(90, 8, 4, -2, 2.167, 0.37)
   bval_table(1168) = bond_valence_t(90, 16, 4, -2, 2.64, 0.37)
   bval_table(1169) = bond_valence_t(90, 34, 4, -2, 2.76, 0.37)
   bval_table(1170) = bond_valence_t(90, 52, 4, -2, 2.94, 0.37)
   bval_table(1171) = bond_valence_t(90, 9, 4, -1, 2.068, 0.37)
   bval_table(1172) = bond_valence_t(90, 17, 4, -1, 2.55, 0.37)
   bval_table(1173) = bond_valence_t(90, 35, 4, -1, 2.71, 0.37)
   bval_table(1174) = bond_valence_t(90, 53, 4, -1, 2.93, 0.37)
   bval_table(1175) = bond_valence_t(90, 7, 4, -3, 2.34, 0.37)
   bval_table(1176) = bond_valence_t(90, 15, 4, -3, 2.73, 0.37)
   bval_table(1177) = bond_valence_t(90, 33, 4, -3, 2.80, 0.37)
   bval_table(1178) = bond_valence_t(90, 1, 4, -1, 2.07, 0.37)
   bval_table(1179) = bond_valence_t(22, 9, 2, -1, 2.15, 0.37)
   bval_table(1180) = bond_valence_t(22, 17, 2, -1, 2.31, 0.37)
   bval_table(1181) = bond_valence_t(22, 35, 2, -1, 2.49, 0.37)
   bval_table(1182) = bond_valence_t(22, 8, 3, -2, 1.654, 0.545)
   bval_table(1183) = bond_valence_t(22, 16, 3, -2, 2.11, 0.37)
   bval_table(1184) = bond_valence_t(22, 9, 3, -1, 1.723, 0.37)
   bval_table(1185) = bond_valence_t(22, 17, 3, -1, 2.22, 0.37)
   bval_table(1186) = bond_valence_t(22, 53, 3, -1, 2.52, 0.37)
   bval_table(1187) = bond_valence_t(22, 8, 4, -2, 1.815, 0.37)
   bval_table(1188) = bond_valence_t(22, 16, 4, -2, 2.29, 0.37)
   bval_table(1189) = bond_valence_t(22, 9, 4, -1, 1.76, 0.37)
   bval_table(1190) = bond_valence_t(22, 17, 4, -1, 2.19, 0.37)
   bval_table(1191) = bond_valence_t(22, 35, 4, -1, 2.36, 0.37)
   bval_table(1192) = bond_valence_t(22, 8, 9, -2, 1.790, 0.37)
   bval_table(1193) = bond_valence_t(22, 17, 9, -1, 2.184, 0.37)
   bval_table(1194) = bond_valence_t(22, 35, 9, -1, 2.32, 0.37)
   bval_table(1195) = bond_valence_t(22, 53, 9, -1, 2.54, 0.37)
   bval_table(1196) = bond_valence_t(22, 16, 9, -2, 2.24, 0.37)
   bval_table(1197) = bond_valence_t(22, 34, 9, -2, 2.38, 0.37)
   bval_table(1198) = bond_valence_t(22, 52, 9, -2, 2.60, 0.37)
   bval_table(1199) = bond_valence_t(22, 7, 9, -3, 1.93, 0.37)
   bval_table(1200) = bond_valence_t(22, 15, 9, -3, 2.36, 0.37)
   bval_table(1201) = bond_valence_t(22, 33, 9, -3, 2.42, 0.37)
   bval_table(1202) = bond_valence_t(22, 1, 9, -1, 1.61, 0.37)
   bval_table(1203) = bond_valence_t(81, 8, 1, -2, 2.124, 0.37)
   bval_table(1204) = bond_valence_t(81, 16, 1, -2, 2.545, 0.37)
   bval_table(1205) = bond_valence_t(81, 9, 1, -1, 2.15, 0.37)
   bval_table(1206) = bond_valence_t(81, 17, 1, -1, 2.56, 0.37)
   bval_table(1207) = bond_valence_t(81, 35, 1, -1, 2.69, 0.37)
   bval_table(1208) = bond_valence_t(81, 53, 1, -1, 2.822, 0.37)
   bval_table(1209) = bond_valence_t(81, 7, 1, -3, 2.286, 0.37)
   bval_table(1210) = bond_valence_t(81, 8, 3, -2, 2.003, 0.37)
   bval_table(1211) = bond_valence_t(81, 9, 3, -1, 1.88, 0.37)
   bval_table(1212) = bond_valence_t(81, 17, 3, -1, 2.32, 0.37)
   bval_table(1213) = bond_valence_t(81, 35, 3, -1, 2.65, 0.35)
   bval_table(1214) = bond_valence_t(81, 7, 3, -3, 2.014, 0.37)
   bval_table(1215) = bond_valence_t(81, 35, 9, -1, 2.70, 0.37)
   bval_table(1216) = bond_valence_t(81, 53, 9, -1, 2.91, 0.37)
   bval_table(1217) = bond_valence_t(81, 16, 9, -2, 2.63, 0.37)
   bval_table(1218) = bond_valence_t(81, 34, 9, -2, 2.70, 0.37)
   bval_table(1219) = bond_valence_t(81, 52, 9, -2, 2.93, 0.37)
   bval_table(1220) = bond_valence_t(81, 7, 9, -3, 2.29, 0.37)
   bval_table(1221) = bond_valence_t(81, 15, 9, -3, 2.71, 0.37)
   bval_table(1222) = bond_valence_t(81, 33, 9, -3, 2.79, 0.37)
   bval_table(1223) = bond_valence_t(81, 1, 9, -1, 2.05, 0.37)
   bval_table(1224) = bond_valence_t(69, 8, 3, -2, 2.000, 0.37)
   bval_table(1225) = bond_valence_t(69, 16, 3, -2, 2.45, 0.37)
   bval_table(1226) = bond_valence_t(69, 34, 3, -2, 2.58, 0.37)
   bval_table(1227) = bond_valence_t(69, 52, 3, -2, 2.77, 0.37)
   bval_table(1228) = bond_valence_t(69, 9, 3, -1, 1.842, 0.37)
   bval_table(1229) = bond_valence_t(69, 17, 3, -1, 2.38, 0.37)
   bval_table(1230) = bond_valence_t(69, 35, 3, -1, 2.53, 0.37)
   bval_table(1231) = bond_valence_t(69, 53, 3, -1, 2.74, 0.37)
   bval_table(1232) = bond_valence_t(69, 7, 3, -3, 2.14, 0.37)
   bval_table(1233) = bond_valence_t(69, 15, 3, -3, 2.53, 0.37)
   bval_table(1234) = bond_valence_t(69, 33, 3, -3, 2.62, 0.37)
   bval_table(1235) = bond_valence_t(69, 1, 3, -1, 1.85, 0.37)
   bval_table(1236) = bond_valence_t(92, 8, 2, -2, 2.08, 0.37)
   bval_table(1237) = bond_valence_t(92, 16, 3, -2, 2.54, 0.37)
   bval_table(1238) = bond_valence_t(92, 9, 3, -1, 2.02, 0.40)
   bval_table(1239) = bond_valence_t(92, 17, 3, -1, 2.49, 0.40)
   bval_table(1240) = bond_valence_t(92, 35, 3, -1, 2.64, 0.40)
   bval_table(1241) = bond_valence_t(92, 53, 3, -1, 2.87, 0.40)
   bval_table(1242) = bond_valence_t(92, 8, 4, -2, 2.112, 0.37)
   bval_table(1243) = bond_valence_t(92, 16, 4, -2, 2.55, 0.37)
   bval_table(1244) = bond_valence_t(92, 9, 4, -1, 2.038, 0.37)
   bval_table(1245) = bond_valence_t(92, 17, 4, -1, 2.47, 0.40)
   bval_table(1246) = bond_valence_t(92, 35, 4, -1, 2.60, 0.40)
   bval_table(1247) = bond_valence_t(92, 53, 4, -1, 2.88, 0.37)
   bval_table(1248) = bond_valence_t(92, 7, 4, -3, 2.18, 0.37)
   bval_table(1249) = bond_valence_t(92, 8, 5, -2, 2.075, 0.37)
   bval_table(1250) = bond_valence_t(92, 9, 5, -1, 1.966, 0.37)
   bval_table(1251) = bond_valence_t(92, 17, 5, -1, 2.46, 0.37)
   bval_table(1252) = bond_valence_t(92, 35, 5, -1, 2.7, 0.35)
   bval_table(1253) = bond_valence_t(92, 8, 6, -2, 2.051, 0.519)
   bval_table(1254) = bond_valence_t(92, 9, 6, -1, 1.98, 0.40)
   bval_table(1255) = bond_valence_t(92, 17, 6, -1, 2.42, 0.40)
   bval_table(1256) = bond_valence_t(92, 7, 6, -3, 1.93, 0.35)
   bval_table(1257) = bond_valence_t(92, 35, 9, -1, 2.63, 0.37)
   bval_table(1258) = bond_valence_t(92, 53, 9, -1, 2.84, 0.37)
   bval_table(1259) = bond_valence_t(92, 16, 9, -2, 2.56, 0.37)
   bval_table(1260) = bond_valence_t(92, 34, 9, -2, 2.70, 0.37)
   bval_table(1261) = bond_valence_t(92, 52, 9, -2, 2.86, 0.37)
   bval_table(1262) = bond_valence_t(92, 7, 9, -3, 2.24, 0.37)
   bval_table(1263) = bond_valence_t(92, 15, 9, -3, 2.64, 0.37)
   bval_table(1264) = bond_valence_t(92, 33, 9, -3, 2.72, 0.37)
   bval_table(1265) = bond_valence_t(92, 1, 9, -1, 1.97, 0.37)
   bval_table(1266) = bond_valence_t(23, 8, 1, -2, 1.88, 0.37)
   bval_table(1267) = bond_valence_t(23, 17, 1, -1, 2.00, 0.35)
   bval_table(1268) = bond_valence_t(23, 8, 2, -2, 1.70, 0.37)
   bval_table(1269) = bond_valence_t(23, 16, 2, -2, 2.11, 0.37)
   bval_table(1270) = bond_valence_t(23, 9, 2, -1, 2.16, 0.37)
   bval_table(1271) = bond_valence_t(23, 17, 2, -1, 2.44, 0.37)
   bval_table(1272) = bond_valence_t(23, 8, 3, -2, 1.743, 0.37)
   bval_table(1273) = bond_valence_t(23, 16, 3, -2, 2.17, 0.37)
   bval_table(1274) = bond_valence_t(23, 9, 3, -1, 1.702, 0.37)
   bval_table(1275) = bond_valence_t(23, 17, 3, -1, 2.19, 0.37)
   bval_table(1276) = bond_valence_t(23, 35, 3, -1, 2.33, 0.35)
   bval_table(1277) = bond_valence_t(23, 7, 3, -3, 1.813, 0.37)
   bval_table(1278) = bond_valence_t(23, 8, 4, -2, 1.784, 0.37)
   bval_table(1279) = bond_valence_t(23, 16, 4, -2, 2.226, 0.37)
   bval_table(1280) = bond_valence_t(23, 9, 4, -1, 1.70, 0.37)
   bval_table(1281) = bond_valence_t(23, 17, 4, -1, 2.16, 0.37)
   bval_table(1282) = bond_valence_t(23, 7, 4, -3, 1.875, 0.37)
   bval_table(1283) = bond_valence_t(23, 8, 5, -2, 1.803, 0.37)
   bval_table(1284) = bond_valence_t(23, 16, 5, -2, 2.25, 0.37)
   bval_table(1285) = bond_valence_t(23, 9, 5, -1, 1.70, 0.37)
   bval_table(1286) = bond_valence_t(23, 17, 5, -1, 2.16, 0.37)
   bval_table(1287) = bond_valence_t(23, 8, 9, -2, 1.788, 0.32)
   bval_table(1288) = bond_valence_t(23, 35, 9, -1, 2.30, 0.37)
   bval_table(1289) = bond_valence_t(23, 53, 9, -1, 2.51, 0.37)
   bval_table(1290) = bond_valence_t(23, 16, 9, -2, 2.23, 0.37)
   bval_table(1291) = bond_valence_t(23, 34, 9, -2, 2.33, 0.37)
   bval_table(1292) = bond_valence_t(23, 52, 9, -2, 2.57, 0.37)
   bval_table(1293) = bond_valence_t(23, 7, 9, -3, 1.86, 0.37)
   bval_table(1294) = bond_valence_t(23, 15, 9, -3, 2.31, 0.37)
   bval_table(1295) = bond_valence_t(23, 33, 9, -3, 2.39, 0.37)
   bval_table(1296) = bond_valence_t(23, 1, 9, -1, 1.58, 0.37)
   bval_table(1297) = bond_valence_t(74, 8, 4, -2, 1.851, 0.37)
   bval_table(1298) = bond_valence_t(74, 8, 5, -2, 1.881, 0.37)
   bval_table(1299) = bond_valence_t(74, 8, 6, -2, 1.917, 0.37)
   bval_table(1300) = bond_valence_t(74, 9, 6, -1, 1.83, 0.37)
   bval_table(1301) = bond_valence_t(74, 17, 6, -1, 2.27, 0.37)
   bval_table(1302) = bond_valence_t(74, 8, 9, -2, 1.896, 0.28)
   bval_table(1303) = bond_valence_t(74, 35, 9, -1, 2.45, 0.37)
   bval_table(1304) = bond_valence_t(74, 53, 9, -1, 2.66, 0.37)
   bval_table(1305) = bond_valence_t(74, 16, 9, -2, 2.39, 0.37)
   bval_table(1306) = bond_valence_t(74, 34, 9, -2, 2.51, 0.37)
   bval_table(1307) = bond_valence_t(74, 52, 9, -2, 2.71, 0.37)
   bval_table(1308) = bond_valence_t(74, 7, 9, -3, 2.06, 0.37)
   bval_table(1309) = bond_valence_t(74, 15, 9, -3, 2.46, 0.37)
   bval_table(1310) = bond_valence_t(74, 33, 9, -3, 2.54, 0.37)
   bval_table(1311) = bond_valence_t(74, 1, 9, -1, 1.76, 0.37)
   bval_table(1312) = bond_valence_t(54, 8, 2, -2, 2.05, 0.35)
   bval_table(1313) = bond_valence_t(54, 9, 2, -1, 2.02, 0.37)
   bval_table(1314) = bond_valence_t(54, 9, 4, -1, 1.93, 0.37)
   bval_table(1315) = bond_valence_t(54, 8, 6, -2, 2.00, 0.37)
   bval_table(1316) = bond_valence_t(54, 9, 6, -1, 1.89, 0.37)
   bval_table(1317) = bond_valence_t(54, 8, 8, -2, 1.94, 0.37)
   bval_table(1318) = bond_valence_t(39, 8, 3, -2, 2.028, 0.35)
   bval_table(1319) = bond_valence_t(39, 16, 3, -2, 2.48, 0.37)
   bval_table(1320) = bond_valence_t(39, 34, 3, -2, 2.61, 0.37)
   bval_table(1321) = bond_valence_t(39, 52, 3, -2, 2.80, 0.37)
   bval_table(1322) = bond_valence_t(39, 9, 3, -1, 1.904, 0.37)
   bval_table(1323) = bond_valence_t(39, 17, 3, -1, 2.40, 0.37)
   bval_table(1324) = bond_valence_t(39, 35, 3, -1, 2.55, 0.37)
   bval_table(1325) = bond_valence_t(39, 53, 3, -1, 2.77, 0.37)
   bval_table(1326) = bond_valence_t(39, 7, 3, -3, 2.17, 0.37)
   bval_table(1327) = bond_valence_t(39, 15, 3, -3, 2.57, 0.37)
   bval_table(1328) = bond_valence_t(39, 33, 3, -3, 2.64, 0.37)
   bval_table(1329) = bond_valence_t(39, 1, 3, -1, 1.86, 0.37)
   bval_table(1330) = bond_valence_t(70, 8, 2, -2, 1.989, 0.37)
   bval_table(1331) = bond_valence_t(70, 7, 2, -3, 2.092, 0.37)
   bval_table(1332) = bond_valence_t(70, 8, 3, -2, 1.965, 0.37)
   bval_table(1333) = bond_valence_t(70, 16, 3, -2, 2.43, 0.37)
   bval_table(1334) = bond_valence_t(70, 34, 3, -2, 2.56, 0.37)
   bval_table(1335) = bond_valence_t(70, 52, 3, -2, 2.76, 0.37)
   bval_table(1336) = bond_valence_t(70, 9, 3, -1, 1.875, 0.37)
   bval_table(1337) = bond_valence_t(70, 17, 3, -1, 2.371, 0.37)
   bval_table(1338) = bond_valence_t(70, 35, 3, -1, 2.451, 0.37)
   bval_table(1339) = bond_valence_t(70, 53, 3, -1, 2.72, 0.37)
   bval_table(1340) = bond_valence_t(70, 7, 3, -3, 2.064, 0.37)
   bval_table(1341) = bond_valence_t(70, 15, 3, -3, 2.53, 0.37)
   bval_table(1342) = bond_valence_t(70, 33, 3, -3, 2.59, 0.37)
   bval_table(1343) = bond_valence_t(70, 1, 3, -1, 1.82, 0.37)
   bval_table(1344) = bond_valence_t(30, 8, 2, -2, 1.704, 0.37)
   bval_table(1345) = bond_valence_t(30, 16, 2, -2, 2.09, 0.37)
   bval_table(1346) = bond_valence_t(30, 34, 2, -2, 2.22, 0.37)
   bval_table(1347) = bond_valence_t(30, 52, 2, -2, 2.45, 0.37)
   bval_table(1348) = bond_valence_t(30, 9, 2, -1, 1.62, 0.37)
   bval_table(1349) = bond_valence_t(30, 17, 2, -1, 2.01, 0.37)
   bval_table(1350) = bond_valence_t(30, 35, 2, -1, 2.15, 0.37)
   bval_table(1351) = bond_valence_t(30, 53, 2, -1, 2.36, 0.37)
   bval_table(1352) = bond_valence_t(30, 7, 2, -3, 1.77, 0.37)
   bval_table(1353) = bond_valence_t(30, 15, 2, -3, 2.15, 0.37)
   bval_table(1354) = bond_valence_t(30, 33, 2, -3, 2.24, 0.37)
   bval_table(1355) = bond_valence_t(30, 1, 2, -1, 1.42, 0.37)
   bval_table(1356) = bond_valence_t(40, 8, 2, -2, 2.34, 0.37)
   bval_table(1357) = bond_valence_t(40, 9, 2, -1, 2.24, 0.37)
   bval_table(1358) = bond_valence_t(40, 17, 2, -1, 2.58, 0.37)
   bval_table(1359) = bond_valence_t(40, 8, 4, -2, 1.928, 0.37)
   bval_table(1360) = bond_valence_t(40, 16, 4, -2, 2.41, 0.37)
   bval_table(1361) = bond_valence_t(40, 34, 4, -2, 2.53, 0.37)
   bval_table(1362) = bond_valence_t(40, 52, 4, -2, 2.67, 0.37)
   bval_table(1363) = bond_valence_t(40, 9, 4, -1, 1.846, 0.37)
   bval_table(1364) = bond_valence_t(40, 17, 4, -1, 2.33, 0.37)
   bval_table(1365) = bond_valence_t(40, 35, 4, -1, 2.48, 0.37)
   bval_table(1366) = bond_valence_t(40, 53, 4, -1, 2.69, 0.37)
   bval_table(1367) = bond_valence_t(40, 7, 4, -3, 2.11, 0.37)
   bval_table(1368) = bond_valence_t(40, 15, 4, -3, 2.52, 0.37)
   bval_table(1369) = bond_valence_t(40, 33, 4, -3, 2.57, 0.37)
   bval_table(1370) = bond_valence_t(40, 1, 4, -1, 1.79, 0.37)
!
   end subroutine load_bval_table

!---------------------------------------------------------------------------------------------------------------------

   subroutine read_bval_from_file(filebv,fileout)
!
!  Read parameters Ro,B from file 'bvparmxxxx.cif' and convert in fortran code
!  Last version from http://www.iucr.org/resources/data/datasets/bond-valence-parameters
!
   USE iso_fortran_env
   USE fileutil
   USE strutil
   USE elements
   character(len=*), intent(in)    :: filebv,fileout
   type(file_handle)               :: fileh,fout
   character(len=:), allocatable   :: line
   integer                         :: nline
   integer                         :: ier,jbv
   character(len=10), dimension(6) :: word
   integer                         :: nword
   integer                         :: nbval,z1,z2,z1last,z2last,val1,val2,val1last,val2last
!
   call fileh%fopen(filebv)
   if (fileh%good()) then
       call fout%fopen(fileout,'w')
       if (fout%good()) then
           jbv = fileh%handle()
           call find_key_file_a(jbv,'_valence_param_details',nline,line,ier)
           if (ignore_lines(jbv,1) /= 0) return
           nbval = 0
           z1last = 0
           z2last = 0
           val1last = 0
           val2last = 0
           do while(get_line(jbv,line,trimmed=.true.))
              if (line(1:1) == '#') exit
              call get_words(line,word,nword) 
              z1 = z_from_specie(word(1))
              z2 = z_from_specie(word(3))
              call s_to_i(word(2),val1,ier)
              call s_to_i(word(4),val2,ier)
!
!             For the same bond consider only the first occurancy
              if (z1 == z1last .and. z2 == z2last .and. val1 == val1last .and. val2 == val2last) then
                  cycle
              endif
!
              nbval = nbval + 1
!
!             write in fortran format
              write(fout%handle(),'(3x,a,4(i0,", "),a)')'bval_table('//trim(i_to_s(nbval))//') = bond_valence_t(', &
                                                        z1,z2,val1,val2,trim(word(5))//', '//trim(word(6))//')'
!
              z1last = z1
              z2last = z2
              val1last = val1
              val2last = val2
           enddo
           call fout%fclose()
       else
           write(ERROR_UNIT,'(a)') trim(fout%err_msg())
       endif
       call fileh%fclose()
   else
       write(ERROR_UNIT,'(a)') trim(fileh%err_msg())
   endif
!
   end subroutine read_bval_from_file

end module bond_valence
