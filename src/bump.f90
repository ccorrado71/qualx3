MODULE anti_bump

USE elements

implicit none

real, parameter, private :: SIGMADEF = 0.3

type bump_settings
  integer :: baction = 0     ! 0 = bump disabled, 1 = default strategy, 2 = user strategy
  real    :: weight  = 1000  ! weight for bump in the CF
  real    :: scaledist = 0.7 ! scale factor on distance
  !real    :: scaledist = 1.0 ! scale factor on distance
end type bump_settings

integer, dimension(4), parameter, private :: abelem = [C_at,N_at,O_at,S_at]

!S set_antibump_auto(bump, btable, atom, frag)                     !Generate automatically list of anti-bumping restraints
!F function bump_distance(z1,z2) result(dist)                      !Set target distances as sum of vdW radius
!S create_bump_table(atom, bumptab)                                !Create table of anti-bump restraints from the atomic model
!S activate_bump(bumptab,el1,el2,targ)                             !Activate bump restraint in the bump table and set target distance 
!S bump_from_table(bumptab,bump,atom,frag,bset)                    !Set restraints list from bump table
!S table_from_bump(bumptab,bump,atom,scaled)                       !Set bump table from existing restraint list. Used to align directive and graphic

!S bump_from_string(string, atom, bump, frag, scaled, ier)         !Get anti-bump restraints from a string
!S set_antibump_atoms(bump,atom,cell,frag,vat1,vat2,scaled,targ)   !Set anti-bump restraints between atoms vat
!S set_antibump_spec(bump,atom,frag,spec,scaled,targ)              !Set anti-bump restraints for for species in the array spec
!S set_antibump_spec1(bump,atom,frag,spec1,spec2,scaled,targ)      !Set all bump restraints between species spec1 spec2
!S print_antibump(kpr,bump,atom)                                   !Print list of anti-bumping restraints
!F antibump_function(bump,atom,doc,kpr) result(bfunc)              !Compute cost function for anti-bump restraints

CONTAINS

   subroutine set_antibump_auto(bump, atom, bond, cell, frag, rot, abset)
!
!  Generate automatically list of anti-bumping restraints
!
   use atom_type_util
   use fragmentmod
   use unit_cell
   use rrestr
   use rotationmod
   use connect_mod
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bump
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(bond_type), dimension(:), allocatable, intent(in)         :: bond
   type(cell_type), intent(in)                                    :: cell
   type(fragment_type), dimension(:), allocatable, intent(in)     :: frag
   type(rotation_type), dimension(:), allocatable, intent(in)     :: rot
   type(bump_settings), intent(in)                                :: abset
!
   call set_antibump_spec(bump, atom, bond, cell, frag, rot, eleminfo(abelem)%lab, abset)
!
   end subroutine set_antibump_auto

!--------------------------------------------------------------

   real function bump_distance(z1,z2) result(dist)
!
!  Set target distances as sum of vdW radius
!
   integer, intent(in) :: z1,z2
   real, parameter :: FACTH = 0.9
!
   if ((z1 == O_at .and. z2 == O_at) .or.      &   ! riduce distance for possible h-bond
       (z1 == N_at .and. z2 == O_at) .or.      &
       (z1 == O_at .and. z2 == N_at)) then     
       dist = FACTH*(eleminfo(z1)%w_radius + eleminfo(z2)%w_radius)
   else
       dist = eleminfo(z1)%w_radius + eleminfo(z2)%w_radius
   endif
!
   end function bump_distance

!--------------------------------------------------------------

   subroutine create_bump_table(atom, bumptab)
!
!  Create table of anti-bump restraints from the atomic model
!
   USE atom_type_util
   USE strutil
   USE arrayutil
   USE rrestr
   type(element_type), dimension(:), allocatable                :: elem
   type(atom_type), dimension(:), intent(in)                    :: atom
   type(restraint_type), dimension(:), allocatable, intent(out) :: bumptab
   integer                                                      :: nelem
   integer                                                      :: i,j
   integer                                                      :: nbump
!
   call elements_from_atom(atom,elem)
   nelem = numelem(elem)
   if (nelem > 0) then
       call resize_restraints(bumptab,(nelem*nelem + nelem)/2)
       nbump = 0
       do i=1,nelem
          do j=i,nelem
             nbump = nbump + 1
             call resize_array(bumptab(nbump)%na,2)
             bumptab(nbump)%code = -4  ! negative value for not-active anti-bump
             bumptab(nbump)%na(1) = elem(i)%ptab 
             bumptab(nbump)%na(2) = elem(j)%ptab 
             bumptab(nbump)%targ = bump_distance(elem(i)%z, elem(j)%z)
!             bumptab(nbump)%targ = SCALEDIST*(elem(i)%w_radius + elem(j)%w_radius)
!              write(0,*)trim(elem(i)%lab)//'-'//trim(elem(j)%lab),bumptab(nbump)%targ,bump_distance(elem(i)%ptab,elem(j)%ptab)
          enddo
       enddo
   endif
!
   end subroutine create_bump_table

!--------------------------------------------------------------

   subroutine activate_bump(bumptab,el1,el2,targ)
!
!  Activate bump restraint in the bump table and set target distance 
!
   USE rrestr
   type(restraint_type), dimension(:), intent(inout) :: bumptab  
   integer, intent(in)                               :: el1,el2 ! pointers to table of elements
   real, intent(in), optional                        :: targ    ! target distance
   integer                                           :: i
!
   do i=1,size(bumptab)
      if ((bumptab(i)%na(1) == el1 .and. bumptab(i)%na(2) == el2) .or.   &
          (bumptab(i)%na(1) == el2 .and. bumptab(i)%na(2) == el1)) then
           bumptab(i)%code = abs(bumptab(i)%code)
           if (present(targ)) bumptab(i)%targ = targ
      endif
   enddo
!
   end subroutine activate_bump
   
!--------------------------------------------------------------

   subroutine bump_from_table(bumptab,bump,atom,bond,cell,frag,rot,bset)
!
!  Set restraints list from bump table
!
   use fragmentmod
   use unit_cell
   use rrestr
   use rotationmod
   use connect_mod
   type(restraint_type), dimension(:), intent(in)                 :: bumptab
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bump
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(bond_type), dimension(:), allocatable, intent(in)         :: bond
   type(cell_type), intent(in)                                    :: cell
   type(fragment_type), dimension(:), allocatable, intent(in)     :: frag
   type(rotation_type), dimension(:), allocatable, intent(in)     :: rot
   type(bump_settings)                                            :: bset
   integer                                                        :: i
!
   do i=1,size(bumptab)
      if (bumptab(i)%code > 0) then
          call set_antibump_spec1(bump, atom, bond, cell, frag, rot,  &
          eleminfo(bumptab(i)%na(1))%lab, eleminfo(bumptab(i)%na(2))%lab, bset, bumptab(i)%targ)
      endif
   enddo
!
   end subroutine bump_from_table

!--------------------------------------------------------------

   subroutine table_from_bump(bumptab,bump,atom,bset)
!
!  Set bump table from existing restraint list. Used to align directive and graphic
!
   USE atom_type_util
   USE rrestr
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bumptab
   type(restraint_type), dimension(:), allocatable, intent(in)    :: bump
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(bump_settings), intent(inout)                             :: bset
   integer                                                        :: i,j,i1,i2,pos
   integer                                                        :: nat1,nat2
   integer, dimension(size(atom))                                 :: vat1,vat2
   logical                                                        :: ltarg
   real                                                           :: targ
!
   if (nrestraints(bump) == 0) return
!
   loop_bump: do i=1,size(bumptab)
!
!     Get atoms of specie na(1) and na(2)
      call get_atoms_of_specie(bumptab(i)%na(1),atom,vat1,nat1)
      if (nat1 == 0) cycle
      call get_atoms_of_specie(bumptab(i)%na(2),atom,vat2,nat2)
      if (nat2 == 0) cycle
!
!     Check if all atoms are involved in restraints
      ltarg = .false.
      do i1=1,nat1
         do i2=1,nat2
            pos = restraint_position(bump,[abs(vat1(i1)),vat2(i2)])
            if (pos == 0) cycle loop_bump
!corr            if (pos == 0) then
!corr                pos = restraint_position(bump,restraint_type(code=ABUMP,na=[-vat1(i1),vat2(i2)]))
!corr                if (pos == 0) cycle loop_bump
!corr            endif
!
!           Check target distance, should be the same
            if (.not.ltarg) then
                targ = bump(pos)%targ
                ltarg = .true.
            else
                if (targ /= bump(pos)%targ) cycle loop_bump
            endif
         enddo
      enddo
      call activate_bump(bumptab,bumptab(i)%na(1),bumptab(i)%na(2),targ/bset%scaledist)
   enddo loop_bump
!
!  Set type of action
   bset%baction = 2 
   loop_elem: do i=1,size(abelem)
      do j=i,size(abelem)
         if ((numatomspec(atom,abelem(i)) > 0) .and. (numatomspec(atom,abelem(j))) > 0) then ! elements are present in atom?
             pos = restraint_position(bumptab,[abelem(i),abelem(j)])
             if (pos == 0) exit loop_elem  ! this not generally possible
             if (bumptab(pos)%code < 0) then
                 bset%baction = 1 
                 exit loop_elem
             endif
         endif
      enddo
   enddo loop_elem
!
   end subroutine table_from_bump

!--------------------------------------------------------------

   subroutine bump_from_string(string,atom,bond,cell,bump,frag,rot,bset,ier)
!
!  Get anti-bump restraints from a string
!
   use strutil
   use atom_type_util
   use fragmentmod
   use unit_cell
   use rrestr
   use rotationmod
   use connect_mod
   character(len=*), intent(in)                                   :: string
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(bond_type), dimension(:), allocatable, intent(in)         :: bond
   type(cell_type), intent(in)                                    :: cell
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bump
   type(fragment_type), dimension(:), allocatable, intent(in)     :: frag
   type(rotation_type), dimension(:), allocatable, intent(in)     :: rot
!corr   real, intent(in)                                               :: scaled
   type(bump_settings), intent(in)                                :: bset
   integer, intent(out)                                           :: ier
   character(len=len_trim(string))                                :: line
   integer                                                        :: nspec
   character(len=20), dimension(4)                                :: wordv
   integer                                                        :: nword
   logical                                                        :: ok_bump, is_targ
   integer                                                        :: i
   real                                                           :: val, targ, wei
   integer                                                        :: lenv
   integer                                                        :: irep
   integer, dimension(10)                                         :: ivet
   real, dimension(10)                                            :: vet
   integer                                                        :: iv
   integer                                                        :: nfrag
   integer, dimension(size(atom))                                 :: vat1,vat2
   integer                                                        :: nat1,nat2
!
   ier = 0
   ok_bump = .true.
   line = trim(adjustl(string))
   call s_filter(line)
   if (len_trim(line) /= 0) then
       call s_rep(line,'frag',' ',irep)
       select case(irep) 
         case (0)    ! set bump as: bump specie1 specie2 target_distance weight
           call get_words(line,wordv,nword)
           if (nword >=2  .and. nword <= 4) then
               nspec = 0
               targ = -1
               wei = -1
               is_targ = .false.
               do i=1,nword
                  if (nspec == 2) then
                      if (.not.is_targ) then
                          ier = s_to_r(wordv(i),val,lenv)
                          if (ier == 1) exit
                          if (ier == 0) targ = val
                          is_targ = .true.
                      else
                          ier = s_to_r(wordv(i),val,lenv)
                          if (ier == 1) exit
                          if (ier == 0) wei = val
                      endif
                  else
                      nspec = nspec + 1
                  endif
               enddo
               if (nspec == 2) then
                   call get_atoms_of_string(wordv(1),atom,vat1,nat1)
                   if (nat1 > 0) then
                       call get_atoms_of_string(wordv(2),atom,vat2,nat2)
                       if (nat2 > 0) then
                           call set_antibump_atoms(bump,atom,bond,cell,frag,rot,vat1(:nat1),vat2(:nat2),bset,targ,wei)
                       endif
                   endif
                   if (nat1 == 0 .or. nat2 == 0) ier = 1
               endif
           else
               ier = 4
           endif

         case (1)  
           ier = 2

         case (2)   ! es. frag1 frag2 3.0
           nfrag = numfragments(frag)
           targ = -1
           wei = -1
           call getnum(line,vet,ivet,iv)
           select case(iv)       
             case (2,3,4)
               if (ivet(1) > nfrag .or. ivet(2) > nfrag .or. ivet(1) < 1 .or. ivet(2) < 1) then
                   ier = 3
               else
                   if (iv >= 3) targ = ivet(3)
                   if (iv == 4) wei = ivet(4)
                   call set_antibump_atoms(bump,atom,bond,cell,frag,rot,frag(ivet(1))%pos,frag(ivet(2))%pos,bset,targ,wei)
               endif
             case default
               ier = 3
           end select

      end select
   else
      call set_antibump_auto(bump, atom, bond, cell, frag, rot, bset)
   endif
!
   end subroutine bump_from_string

!--------------------------------------------------------------

   subroutine set_antibump_atoms(bump,atom,bond,cell,frag,rot,vat1,vat2,abset,targ,wei)
!
!  Set anti-bump restraints between atoms vat. If targ is not present or negative, his value is
!  calculated from radii and scale
!
   use atom_type_util
   use fragmentmod
   use cgeom
   use unit_cell
   use rrestr
   use rotationmod
   use connect_mod
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bump
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(bond_type), dimension(:), allocatable, intent(in)         :: bond
   type(cell_type), intent(in)                                    :: cell
   type(fragment_type), dimension(:), allocatable, intent(in)     :: frag
   type(rotation_type), dimension(:), allocatable, intent(in)     :: rot
   integer, dimension(:), intent(in)                              :: vat1,vat2
   type(bump_settings), intent(in)                                :: abset
   real, intent(in), optional                                     :: targ,wei
   integer                                                        :: nbump, nat
   integer                                                        :: i,j
   type(restraint_type)                                           :: bump_new
   integer                                                        :: respos
   integer                                                        :: a1,a2,nbumpold
   logical                                                        :: is_targ, is_wei
!
   nbump = nrestraints(bump)
   if (size(vat1)*size(vat2) == 0) return
!
   is_targ = .false.
   if (present(targ)) then
       is_targ =  targ > 0
   endif
   is_wei = .false.
   if (present(wei)) then
       is_wei = wei > 0
   endif
!
!  allocate bump to a reasonable dimension
   !call resize_restraints(bump,nbump + size(vat)*(size(vat) -1)/2)   ! n*(n-1)/2 is sum of serie an=1,2,3,..,n
   call resize_restraints(bump,nbump + size(vat1)*size(vat2))
!   
   nat = numatoms(atom)
   nbumpold = nbump
   bump_new%code = ABUMP
   allocate(bump_new%na(2))
!
   do i=1, size(vat1)
      do j=1, size(vat2)
         a1 = vat1(i)
         a2 = vat2(j)
!
!        Check existence of new restraint
         bump_new%na = [a1,a2]
         respos = restraint_position(bump,bump_new)
         if (respos > nbumpold) cycle ! a1-a2 duplicated in  arrays vat1-vat2

         bump_new%val = distanzaC(atom(a1)%xc,atom(a2)%xc,cell%get_g())
         if (is_targ) then
             bump_new%targ = targ
         else
             bump_new%targ = bump_distance(atom(a1)%z(), atom(a2)%z()) * abset%scaledist
             !bump_new%targ = bump_distance(atom(a1)%z(), atom(a2)%z()) - abset%scaledist
         endif
         if (is_wei) then
             bump_new%wei = wei
         else
             bump_new%wei = abset%weight
         endif
         bump_new%sigma = SIGMADEF

         if (respos == 0) then  ! add anti-bump in a list
             nbump = nbump + 1
             bump(nbump) = bump_new
         else                   ! replace existing anti-bump
!            Set only distance and wei, not n1 and n2. This don't change negative value of n1
             bump(respos)%targ = bump_new%targ
             bump(respos)%wei = bump_new%wei
         endif
      enddo
   enddo
!
   call resize_restraints(bump,nbump)
!
!  This is necessary only for new bump restraints
   if (nbump > nbumpold) then
       call set_bump_exclusions(atom,bond,frag,rot,bump(nbumpold+1:))
   endif
!
   end subroutine set_antibump_atoms

!--------------------------------------------------------------

   subroutine set_antibump_spec(bump,atom,bond,cell,frag,rot,spec,abset,targ)
!
!  Set anti-bump restraints for species in the array spec
!
   use atom_type_util
   use fragmentmod
   use unit_cell
   use rrestr
   use rotationmod
   use connect_mod
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bump
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(bond_type), dimension(:), allocatable, intent(in)         :: bond
   type(cell_type), intent(in)                                    :: cell
   type(fragment_type), dimension(:), allocatable, intent(in)     :: frag
   type(rotation_type), dimension(:), allocatable, intent(in)     :: rot
   character(len=*), dimension(:), intent(in)                     :: spec
   type(bump_settings), intent(in)                                :: abset
   real, intent(in), optional                                     :: targ
   integer                                                        :: i
   integer, dimension(size(atom))                                 :: vat1
   integer                                                        :: nb
!
   nb = 0
   do i=1,size(atom)
      if (is_atomic_specie(atom(i),spec)) then
          nb = nb + 1
          vat1(nb) = i
      endif
   enddo
   call set_antibump_atoms(bump,atom,bond,cell,frag,rot,vat1(:nb),vat1(:nb),abset,targ)
!
   end subroutine set_antibump_spec

!--------------------------------------------------------------

   subroutine set_antibump_spec1(bump,atom,bond,cell,frag,rot,spec1,spec2,abset,targ)
!
!  Set all bump restraints between species spec1 spec2
!
   use atom_type_util
   use fragmentmod
   use unit_cell
   use rrestr
   use rotationmod
   use connect_mod
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bump
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(bond_type), dimension(:), allocatable, intent(in)         :: bond
   type(cell_type), intent(in)                                    :: cell
   type(fragment_type), dimension(:), allocatable, intent(in)     :: frag
   type(rotation_type), dimension(:), allocatable, intent(in)     :: rot
   character(len=*), intent(in)                                   :: spec1, spec2
!corr   real, intent(in)                                               :: scaled
   type(bump_settings), intent(in)                                :: abset
   real, intent(in), optional                                     :: targ
   integer, dimension(size(atom))                                 :: vat1,vat2
   integer                                                        :: natom1,natom2
!
   call get_atoms_of_specie(pxen_from_specie(spec1),atom,vat1,natom1)
   if (natom1 == 0) return
   call get_atoms_of_specie(pxen_from_specie(spec2),atom,vat2,natom2)
   if (natom2 == 0) return
   call set_antibump_atoms(bump,atom,bond,cell,frag,rot,vat1(:natom1),vat2(:natom2),abset,targ)
!
   end subroutine set_antibump_spec1
   
!--------------------------------------------------------------

   subroutine set_bump_exclusions(atom,bond,frag,rot,bump)
!
!  Set exclusions for contacts if 
!  a) the distance between n1-n2 include more than NROTE bonds
!!!!!!  b) the 50% of bonds are rotable
!
   use molgraph
   use rrestr
   use connect_mod
   use fragmentmod
   use rotationmod
   use arrayutil
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(bond_type), dimension(:), allocatable, intent(in)         :: bond
   type(fragment_type), dimension(:), allocatable, intent(in)     :: frag
   type(rotation_type), dimension(:), allocatable, intent(in)     :: rot
   type(restraint_type), dimension(:), intent(inout)              :: bump
   integer                                                        :: i,j,n1,n2,nv
   integer, dimension(:), allocatable                             :: vpath
   integer                                                        :: nrote = 4
   integer                                                        :: nrotp
!
   allocate(vpath(size(atom)))
   call compute_dist_matrix(atom,bond,path=.true.)
   do i=1,size(bump)
      n1 = abs(bump(i)%na(1))
      n2 = bump(i)%na(2)
      if (fragment_pos(frag,n1) == fragment_pos(frag,n2)) then                  ! if in the same fragment
         !write(70,*)'LEN=',atom(n1)%glab()//'-'//atom(n2)%glab(),path_length(n1,n2)
          bump(i)%na(1) = -n1 ! only equivalent position are checked for atoms in the same molecular fragment
          if (path_length(n1,n2) >= nrote) then
              call get_path(n1,n2,vpath(2:),nv)
              if (nv + 1 >= nrote) then
                  nrotp = 0
                  vpath(1) = n1    !add terminal atoms to vpath
                  vpath(nv+2) = n2
                  nv = nv + 2
                  do j=1,numrotat(rot)
                     if (rot(j)%rcod == 1) then  ! only refined rotation
                         if (check_container(vpath(:nv),rot(j)%pax)) then
                             nrotp = nrotp + 1
                             if (nrotp == nrote) then
                             !if (nrotp == ceiling(nrote/2.)) then ! 50% of bonds, the largest integer 
                                 bump(i)%na(1) = n1
                                 !write(70,*)'vpath included=',vpath(:nv)
                                 exit
                             endif
                         endif
                     endif
                  enddo
!corr                  if (nrotp /= nrote) then
!corr                      write(70,*)'vpath excluded=',vpath(:nv),'NR=',nrotp
!corr                  endif
!corr              else
!corr                  !write(70,*)'path not considered'
              endif
          endif
      endif
   enddo
!
   end subroutine set_bump_exclusions

!--------------------------------------------------------------

   subroutine print_antibump(kpr,bump,atom,cell)
!
!  Print list of anti-bumping restraints
!
   USE atom_type_util
   USE connect_mod
   USE unit_cell
   USE rrestr
   integer, intent(in)                                         :: kpr
   type(restraint_type), dimension(:), allocatable, intent(in) :: bump
   type(atom_type), dimension(:), allocatable, intent(in)      :: atom
   type(cell_type), intent(in)                                 :: cell
   if (nrestraints(bump) > 0) then
       call print_restraints(kpr,bump,ABUMP,atom,cell,.false.)
   endif
   end subroutine print_antibump

!--------------------------------------------------------------

   real function antibump_function(bump,atom,spg,gmat,kpr,vres) result(bfunc)
!
!  Compute cost function for anti-bump restraints and update current value bump%val
!
   USE atom_type_util
   USE fragmentmod
   USE connect_mod
   USE cgeom
   USE rrestr
   USE spginfom
   use arrayutil
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bump
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(spaceg_type), intent(in)                                  :: spg
   real, dimension(3,3), intent(in)                               :: gmat
   logical, intent(in)                                            :: kpr
   integer, dimension(:), allocatable, intent(in), optional       :: vres
   integer                                                        :: nbump
   integer                                                        :: n1, n2
   real                                                           :: dd
   type(atom_type)                                                :: at
   integer                                                        :: i, k, nvres
   real, dimension(3)                                             :: ktra
   logical                                                        :: same_frag
   real                                                           :: distmin
   real, parameter                                                :: EPS = epsilon(1.0)
   real, parameter                                                :: DDOC = 1.0
!
   nvres = -1
   if (present(vres)) then
       nvres = size_array(vres)
   endif
   bfunc = 0
   nbump = nrestraints(bump)
   do i=1,nbump
      if (nvres == 0) cycle
      !if (nvres == 0) then
      !        write(70,*)'jump this restr: ',i,bump(i)%contrib
      !          contrib = bump(i)%contrib
      !endif
      if (nvres > 0) then
          if (all(vres /= i)) cycle  ! jump restraints non included in array vres
          !if (all(vres /= i)) then
          !    write(70,*)'jump this restr: ',i,bump(i)%contrib
          !      contrib = bump(i)%contrib
          !endif
      endif
      n1 = bump(i)%na(1)
      n2 = bump(i)%na(2)
      bump(i)%contrib = 0
      if (n1 < 0) then
          n1 = abs(n1)
          same_frag = .true.
      else
          same_frag = .false.
      endif
      distmin = huge(1.0) !!!!distanzaC(atom(n2)%xc,atom(n1)%xc,gmat)
      do k=1,spg%nsymop
!!!!!!test this code
         if (same_frag .and. k == 1) then
             call xdisteqs_intra(atom(n2)%xc,atom(n1)%xc,gmat,dd,ktra)
         else
             at = atom_symm(atom(n1),spg%symop(k))
             call xdisteqs(atom(n2)%xc,at%xc,gmat,dd,ktra)
         endif
!!!!!! end test this code
!old         at = atom_symm(atom(n1),spg%symop(k))
!old         call xdisteqs(atom(n2)%xc,at%xc,gmat,dd,ktra)
!old         if (same_frag .and. k == 1) then           ! n1 and n2 are in the same fragment when k==1
!old!
!old!            check if cell translation is 0: ktra - (at%xc-atom(n2)%xc) == 0
!old             if (atom(n2) + ktra == at) cycle  ! if true n1 is not an equivalent for translation
!old         endif
         if (dd < distmin) distmin = dd
         if (dd < bump(i)%targ) then
!
!            Check for doc: same elements, doc active, dist < DISTMIN
             if ((atom(n1)%doc) .and.  (atom(n2)%doc) .and.          &
                 (atom(n1)%ptab == atom(n2)%ptab) .and. (dd < DDOC)) then             
                  !write(0,*)'ocry=',trim(atom(n1)%lab)//'-'//trim(atom(n2)%lab),dd
                  cycle
             endif
!
             !bfunc = bfunc + ((1/bump(i)%sigma)*(dd - bump(i)%targ))**4 
             !bfunc = bfunc + bump(i)%wei*(dd - bump(i)%targ)**4 
             bump(i)%contrib = bump(i)%contrib + bump(i)%wei*(dd - bump(i)%targ)**4
             if(kpr) then
                !write(71,*)'activate bump:',atom(n1)%lab,atom(n2)%lab,dd,bump(i)%targ,(1/bump(i)%sigma)*(dd - bump(i)%targ)**2
                write(71,'(i5,a,a,3f8.4)')i,'activate bump:',atom(n1)%lab//' '//atom(n2)%lab,dd,   &
                      bump(i)%targ,bump(i)%wei*(dd - bump(i)%targ)**4
             endif
         endif
      enddo
      bump(i)%val = distmin
!!!!!!!!!!!!test
           ! if (nvres == 0) then
           !     write(70,*)'recomputed contrib: ',i,bump(i)%contrib,contrib,abs(bump(i)%contrib-contrib) > epsilon(1.0)
           ! endif
           ! if (nvres > 0) then
           !     !if (all(vres /= i)) cycle  ! jump restraints non included in array vres
           !     if (all(vres /= i)) then
           !         write(70,*)'recomputed contrib: ',i,bump(i)%contrib,contrib,abs(bump(i)%contrib-contrib) > epsilon(1.0)
           !     endif
           ! endif
!!!!!!!!!!!!test
   enddo
   bfunc = chi2tot(bump)
   if (kpr) then
       write(71,*)'fbump=',bfunc
   endif
!
   end function antibump_function

!--------------------------------------------------------------

   subroutine get_modified_abump(bump,vat,vmod)
!
!  Array vmod contains restraints modified by move of atom vat
!
   use arrayutil
   use rrestr
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bump
   integer, dimension(:), intent(in)                              :: vat
   integer, dimension(:), allocatable, intent(out)                :: vmod
   integer                                                        :: nbump,i

   nbump = nrestraints(bump)
   if (nbump == 0) return
   do i=1,nbump
      if (any(vat(:) == abs(bump(i)%na(1)))) then
          call push_back_array(vmod,i)
      else
          if (any(vat(:) == bump(i)%na(2))) then
              call push_back_array(vmod,i)
          endif
      endif
   enddo
   end subroutine get_modified_abump

END MODULE anti_bump

!corr   subroutine test_bump()
!corr   USE variables
!corr   USE fragmentmod
!corr   USE connect_mod
!corr   USE anti_bump
!corr   USE General, only: cell
!corr   USE unit_cell
!corr   type(fragment_type), dimension(:), allocatable :: frag
!corr   type(restraint_type), dimension(:), allocatable     :: bump
!corr!corr   type(bump_settings) :: bset
!corr   integer :: nfrag
!corr   real :: fbump
!corr   type(cell_type) :: cellt
!corr          write(0,*)'start test_bump',size(atm),size(lconn)
!corr   cellt = set_cell_type(cell)
!corr   call get_fragments(atm,cellt,lconn,nfrag,frag)
!corr   !call set_antibump_auto(bump, atm, frag)
!corr        !call set_antibump_spec1(bump, atm, cellt, frag, 'C', 'C',bset%scaledist)
!corr        !call set_antibump_spec1(bump, atm, cellt, frag, 'C', 'H',bset%scaledist)
!corr        !call set_antibump_spec1(bump, atm, cellt, frag, 'O', 'O',bset%scaledist)
!corr        !call set_antibump_spec1(bump, atm, frag, 'C', 'O', 2.4)
!corr        !call set_antibump_spec1(bump, atm, frag, 'O', 'O', 2.3)
!corr        !call set_antibump_spec1(bump, atm, frag, 'V', 'C')
!corr        !call set_antibump_spec1(bump, atm, frag, 'V', 'C')
!corr        !call set_antibump_spec1(bump, atm, frag, 'O', 'O',2.0)
!corr   !call set_antibump_spec1(bump, atm, frag, 'C', 'H')
!corr   !call set_antibump_spec1(bump, atm, frag, 'H', 'H')
!corr   !call set_antibump_spec1(bump, atm, frag, 'V', 'C')
!corr   !call set_antibump_spec1(bump, atm, frag, 'V', 'V')
!corr   !call set_antibump_spec1(bump, atm, frag, 'O', 'H')  !-- prosssimo test
!corr   fbump = antibump_function(bump, atm, cellt%get_g(),.false.,.false.)
!corr   call print_antibump(0,bump,atm,cellt)
!corr   fbump = antibump_function(bump, atm, cellt%get_g(),.false., .true.)
!corr   end subroutine test_bump
