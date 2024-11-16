module atom_basic

  implicit none

  type op_type
    integer               :: op = 1     ! symmetry operator
    integer, dimension(3) :: tra = 0    ! translation
  end type op_type

  !integer, parameter :: SIZELAB = 16
  integer, parameter :: SIZELAB = 25
  type atom_type
     real, dimension(3)     :: xc = 0.0       ! x,y,z
     real                   :: biso = 0.0     ! fattore termico
     real, dimension(6)     :: bij = 0.0      ! fattori termici anisotropi: b11,b22,b33,b12,b13,b23
     real                   :: och = 1.0      ! occupanza chimica
     real                   :: ocry = 1.0     ! occupanza cristall.
     real                   :: inte           ! intensita di picco
     integer, private       :: nz = 0         ! nz
     character(len=SIZELAB) :: lab  = ' '     ! label
     integer, dimension(5)  :: rcod = 0       ! posizione nella matrice jac. del parametro se affinato; 0 se non affinato
     integer                :: ptab = 0       ! pointer to table of elements
     real, dimension(3)     :: xsd = 0.0      ! standard deviation on x,y,z
     real                   :: bsd = 0.0      ! standard deviation on biso
     real                   :: osd = 0.0      ! standard deviation on och
     integer                :: asym = 0       ! corrisponding atom in the a.u.
     type(op_type)          :: op             ! symmetry operator
     logical                :: doc = .false.  ! dynamical occupancy correction
     character(len=14)      :: chain = ' '    ! info about protein chain 

  contains

     procedure          :: get_nz
     procedure          :: glab => get_lab
     procedure          :: kscatt => scattering_pointer
     procedure          :: c_radius => covalent_radius
     procedure          :: vdw_radius => van_der_waals_radius
     procedure          :: z => atomic_number
     procedure          :: charge => atomic_charge
     procedure          :: specie => atomic_specie
     procedure          :: spec => atomic_specie1 ! similar to previous but without charge
     procedure          :: type_at                ! pointer to array elem
     procedure          :: symm_code
     procedure          :: get_bmat
 
     procedure, private :: set_specie_nz
     generic, public    :: set_specie => set_specie_nz
     procedure          :: set_specie_from_z
     procedure          :: set_specie_from_ptab
     procedure          :: set_specie_from_el
     procedure          :: set_as_deleted
     procedure          :: set_bmat

     procedure          :: prn => write_atom

  end type atom_type

  interface operator(==)
    module procedure operator_equal
  end interface

  interface operator(/=)
    module procedure operator_not_equal
  end interface

integer, parameter :: DELETED_ATOM = -9999
integer, parameter, private :: NZCONST = 100

CONTAINS 

   logical function operator_equal(op1,op2)
   type(op_type), intent(in) :: op1,op2
   if (op1%op /= op2%op) then
       operator_equal = .false.
       return
   endif
   operator_equal = all(op1%tra == op2%tra)
   end function operator_equal

 !----------------------------------------------------------------------------------

   logical function operator_not_equal(op1,op2)
   type(op_type), intent(in) :: op1,op2
   operator_not_equal = .not.(op1 == op2)
   end function operator_not_equal

!----------------------------------------------------------------------

   integer function numatoms(atom)
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
!   
   if (allocated(atom)) then
       numatoms = size(atom)
   else
       numatoms = 0
   endif
!
   end function numatoms

 !----------------------------------------------------------------------------------

   integer elemental function get_nz(atom)
   class(atom_type), intent(in) :: atom
   get_nz = atom%nz
   end function get_nz

 !----------------------------------------------------------------------------------

   function get_lab(atom,num)
!
!  Concatena label con numero d'ordine dell'atomo
!
   USE strutil
   class(atom_type), intent(in)  :: atom
   character(len=:), allocatable :: get_lab
   integer, intent(in),optional  :: num
!
   if (present(num)) then
       get_lab = trim(atom%lab)//'('//i_to_s(num)//')'
   else
       get_lab = trim(atom%lab)
   endif
!
   end function get_lab

 !----------------------------------------------------------------------------------

   subroutine set_specie_nz(atom,nz,elem)
   USE elements
   class(atom_type), intent(inout)               :: atom
   integer, intent(in)                           :: nz
   type(element_type), dimension(:), allocatable :: elem
   integer :: inz
!
   atom%nz = nz
   inz = atom%kscatt()
!
   if (inz == 0 .or. inz > numelem(elem)) then
       atom%ptab = 0
       return
   endif
   atom%ptab = elem(inz)%ptab
!
   end subroutine set_specie_nz

 !----------------------------------------------------------------------------------

   subroutine set_specie_from_z(atom,z,elem)
   USE elements
   class(atom_type), intent(inout)               :: atom
   integer, intent(in)                           :: z
   type(element_type), dimension(:), allocatable :: elem
   integer :: i
!
   do i=1,numelem(elem)
      if (elem(i)%z == z) then
          call atom%set_specie_nz(i*NZCONST,elem)
          return
      endif
   enddo
   atom%nz = 0
   atom%ptab = 0
!
   end subroutine set_specie_from_z

 !----------------------------------------------------------------------------------

   subroutine set_specie_from_ptab(atom,elem,ptab)
   USE elements
   class(atom_type), intent(inout)               :: atom
   type(element_type), dimension(:), allocatable :: elem
   integer, intent(in), optional                 :: ptab
   integer                                       :: i
!
   if (present(ptab)) then
!
!      set ptab and nz
       do i=1,numelem(elem)
          if (elem(i)%ptab == ptab) then
              atom%nz = i*NZCONST
              atom%ptab = ptab
              return
          endif
       enddo
   else
!
!      set nz from ptab, ptab should be previosly defined
       do i=1,numelem(elem)
          if (elem(i)%ptab == atom%ptab) then
              atom%nz = i*NZCONST
              return
          endif
       enddo
   endif
   atom%nz = 0
   atom%ptab = 0 ! set 0 ptab if not present in array elem
!
   end subroutine set_specie_from_ptab

 !----------------------------------------------------------------------------------

   subroutine set_specie_from_el(atom,pos,elem)
   USE elements
   class(atom_type), intent(inout)               :: atom
   integer, intent(in)                           :: pos
   type(element_type), dimension(:), allocatable :: elem
!
   call atom%set_specie_nz(pos*NZCONST,elem)
!
   end subroutine set_specie_from_el

 !----------------------------------------------------------------------------------

   elemental subroutine set_as_deleted(atom)
   class(atom_type), intent(inout) :: atom
   atom%nz = DELETED_ATOM
   end subroutine set_as_deleted

 !----------------------------------------------------------------------------------

   elemental integer function scattering_pointer(atom)
   class(atom_type), intent(in) :: atom
   scattering_pointer = abs(atom%nz/NZCONST)
   end function scattering_pointer

 !----------------------------------------------------------------------------------

   elemental integer function atomic_number(atom) result(z)
   USE elements
   class(atom_type), intent(in) :: atom
   z = eleminfo(atom%ptab)%z
   end function atomic_number

 !----------------------------------------------------------------------------------

   elemental integer function atomic_charge(atom)
   USE elements
   class(atom_type), intent(in) :: atom
   atomic_charge = eleminfo(atom%ptab)%charge
   end function atomic_charge

 !----------------------------------------------------------------------------------

   elemental function atomic_specie(atom) result(str)
!
!  Get element with charge
!
   USE elements
   class(atom_type), intent(in) :: atom
   character(len=NLEN_LAB)      :: str
   str = eleminfo(atom%ptab)%lab
   end function atomic_specie

 !----------------------------------------------------------------------------------

   elemental function atomic_specie1(atom) result(str)
!
!  Get element without charge
!
   USE elements
   class(atom_type), intent(in)  :: atom
   character(len=NLEN_LAB) :: str
   str = trim(eleminfo(atom%z())%lab)
   end function atomic_specie1

 !----------------------------------------------------------------------------------

   elemental real function covalent_radius(atom) result(crad)
   USE elements
   class(atom_type), intent(in) :: atom
   crad = eleminfo(atom%ptab)%c_radius
   end function covalent_radius

 !----------------------------------------------------------------------------------

   elemental real function van_der_waals_radius(atom) result(crad)
   USE elements
   class(atom_type), intent(in) :: atom
   crad = eleminfo(atom%ptab)%w_radius
   end function van_der_waals_radius

!---------------------------------------------------------------------------------------

   elemental subroutine set_intensity(atom)
!
!  Assegna intensita' ragionevoli agli atomi 
!
   type(atom_type), intent(inout) :: atom
!
   atom%inte = nzConst*atom%z()
!
   end subroutine set_intensity

 !----------------------------------------------------------------------------------

   integer function type_at(atom,elem)
!
!  Find pointer to array o species elem
!
   USE elements
   class(atom_type), intent(in)                  :: atom
   type(element_type), dimension(:), allocatable :: elem
   integer                                       :: i
!
   do i=1,numelem(elem)
      if (elem(i)%ptab == atom%ptab) then
          type_at = i
          return 
      endif
   enddo
   type_at = 0
!
   end function type_at
   
 !----------------------------------------------------------------------------------

   function symm_code(atom)
   USE strutil
   class(atom_type), intent(in)  :: atom
   character(len=:), allocatable :: symm_code
   integer, dimension(3)         :: ktra
!
   if (atom%asym == 0 .or. atom%op == op_type()) then
       symm_code = '.'
   else
       ktra = atom%op%tra + 5
       if (any(ktra > 9)) then
           symm_code = i_to_s(atom%op%op)
       else
           symm_code = i_to_s(atom%op%op)//"_"//i_to_s(ktra(1))//i_to_s(ktra(2))//i_to_s(ktra(3))
       endif
   endif
!
   end function symm_code 

 !----------------------------------------------------------------------------------

   subroutine write_atom(atom,kpr)
   class(atom_type), intent(in) :: atom
   integer, intent(in)          :: kpr
   write(kpr,'(a,2i4,4f10.3,5i3)')trim(atom%lab),atom%nz,atom%ptab,atom%xc,atom%biso,atom%rcod
   end subroutine write_atom

 !----------------------------------------------------------------------------------

   subroutine write_atoms(atom,junit)
   type(atom_type), dimension(:), intent(in) :: atom
   integer, intent(in)                       :: junit
   write(junit) atom(:)
   end subroutine write_atoms

 !----------------------------------------------------------------------------------

   subroutine read_atoms(atom,junit,ier)
   type(atom_type), dimension(:), intent(out) :: atom
   integer, intent(in)                        :: junit
   integer, intent(out)                       :: ier
   read(junit,iostat=ier) atom(:)
   end subroutine read_atoms

 !----------------------------------------------------------------------------------

   function slabnum(slab,num)
!
!  Concatena label con numero d'ordine dell'atomo
!
   use strutil
   character(len=*), intent(in)     :: slab
   integer, intent(in)              :: num
   !character(len=len_trim(slab)+12) :: slabnum
   character(len=:), allocatable    :: slabnum
!
   slabnum = trim(slab)//'('//i_to_s(num)//')'
   !slabnum = ' '
   !write(slabnum,'(a,i0,a)')trim(slab)//'(',num,')'
!
   end function slabnum

 !----------------------------------------------------------------------------------

   function slabvet(vatm,llab,showord,maxlen)  result(str)
!
!  Genera nella stringa str una lista di atomi separati da virgola
!
   integer, dimension(:), intent(in)          :: vatm    ! puntatori alle labels
   character(len=*), dimension(:), intent(in) :: llab    ! labels
   logical, intent(in), optional              :: showord ! se vero mostra il numero d'ordine
   integer, intent(in), optional              :: maxlen  ! maximum length of string
   logical                                    :: showord1
   character(len=:), allocatable              :: str    
   integer                                    :: i
   integer                                    :: ipos
   integer                                    :: natlm
!
   if (present(showord)) then
       showord1 = showord
   else
       showord1 = .false. 
   endif
!
   str = ' '
   natlm = size(vatm) 
   if (natlm == 0) return
   ipos = vatm(1)
   if (showord1) then
       str = trim(slabnum(llab(ipos),ipos))
       do i=2,natlm
          ipos = vatm(i)
          if (present(maxlen)) then
              if (len(str) + len_trim(slabnum(llab(ipos),ipos)) + 1 > maxlen) then
                  str = str//' ...'
                  exit
              endif
          endif
          str = str//','//trim(slabnum(llab(ipos),ipos))
       enddo
   else
       str = trim(llab(ipos))
       do i=2,natlm
          ipos = vatm(i)
          if (present(maxlen)) then
              if (len(str) + len_trim(llab(ipos)) + 1 > maxlen) then
                  str = str//' ...'
                  exit
              endif
          endif
          str = str//','//trim(llab(ipos))
       enddo
   endif
!
   end function slabvet

 !----------------------------------------------------------------------------------

   subroutine get_from_label(label,specl,numb,ierror,letter)
!
!  Estrai dalla label la stringa e il seriale
!
   USE strutil
   character(len=*), intent(in)            :: label   !label atomo
   character(len=*), intent(out), optional :: specl   !porzione carattere
   integer, intent(out)                    :: numb    !seriale
   character(len=1), intent(out), optional :: letter  !lettera dopo il seriale
   integer, intent(out)                    :: ierror
   integer                                 :: kpos1,kpos2
   integer                                 :: lenl
   integer                                 :: lenn
   integer                                 :: i
!
   numb = 0
   lenl = len_trim(label)
!
!  Cerca una sequenza di numeri a partire dalla fine
   kpos1 = 0   ! posizione iniziale del numero
   kpos2 = 0   ! posizione finale del numero
   do i=lenl,1,-1
      if (kpos2 == 0 .and. ch_is_digit(label(i:i))) kpos2 = i
      if (kpos2 > 0 .and. .not.ch_is_digit(label(i:i))) then
          kpos1 = i+1
          exit
      endif
   enddo
!
!  Solo un carattere finale e' consentito (es. C23A ma non C23pippo)
   if (kpos1 > 1 .and. kpos2 >= lenl - 1) then
       call s_to_i(label(kpos1:kpos2),numb,ierror,lenn)
   else
       ierror = 1  ! sequenza di soli numeri o sole lettere
   endif
!
   if (present(specl)) then
       if (ierror == 0) then
           specl = label(1:kpos1-1)
       else
           specl = trim(label)
       endif
   endif
!
   if (present(letter)) then
       letter = ' '
       if (ierror == 0) then
           if (kpos2 == lenl-1 .and. ch_is_alpha(label(lenl:lenl))) letter = label(lenl:lenl)
       endif
   endif
   !write(0,*)'label=',trim(label),' numb=',numb,' lab=',trim(specl),' ier=',ierror
!
   end subroutine get_from_label

!----------------------------------------------------------------------------------------------------

   integer function numop(op)
   type(op_type), dimension(:), allocatable, intent(in) :: op
!   
   if (allocated(op)) then
       numop = size(op)
   else
       numop = 0
   endif
!
   end function numop
   
!----------------------------------------------------------------------------------------------------

   subroutine new_op(vetr,n)
!
!  Create new operator
!
   type(op_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                             :: n

   if (n < 0) return
   if (numop(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_op

!----------------------------------------------------------------------------------------------------

   subroutine resize_op(vetr,n,savevet)
!
!  Resize array of op_type
!
   type(op_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                       :: n
   logical, optional, intent(in)             :: savevet
   logical                                   :: savev
   integer                                   :: nv
   type(op_type), allocatable                :: vsav(:)
   integer                                         :: nsav
!
!  se n = 0 (riallocazione a 0): dealloca ed esci
   if (n == 0) then
       if (allocated(vetr)) deallocate(vetr)
       return
   endif
!
   if (.not.allocated(vetr)) then
       allocate(vetr(n))
   else
!
       nv = size(vetr)
       if (present(savevet)) then
           savev = savevet
       else
           savev = .true.
       endif
!
       if (savev) then
!
!          nsav contiene qual e' la porzione di vetr da salvare
           select case(nv-n)
             case (1:)       ! compatta x ad n
               nsav = n
             case (:-1)      ! espandi x ad n
               nsav = nv
             case (0)
               return        ! n=nv non fare niente
           end select
           allocate(vsav(n))
           vsav(:nsav) = vetr(:nsav)
           call move_alloc(vsav,vetr)
       else
           if (nv /= n) then
               deallocate(vetr)
               allocate(vetr(n))
           endif
       endif
   endif
!
   end subroutine resize_op

!----------------------------------------------------------------------------------------------------

   function get_bmat(atom) result(bmat)
   class(atom_type), intent(in) :: atom
   real, dimension(3,3)         :: bmat
!
   bmat(1,1) = atom%bij(1)
   bmat(2,2) = atom%bij(2)
   bmat(3,3) = atom%bij(3)
   bmat(1,2) = atom%bij(4)
   bmat(2,1) = atom%bij(4)
   bmat(1,3) = atom%bij(5)
   bmat(3,1) = atom%bij(5)
   bmat(2,3) = atom%bij(6)
   bmat(3,2) = atom%bij(6)
!
   end function get_bmat

!----------------------------------------------------------------------------------------------------

   subroutine set_bmat(atom,bmat)
   class(atom_type), intent(inout)  :: atom
   real, dimension(3,3), intent(in) :: bmat
!
   atom%bij(1) = bmat(1,1)
   atom%bij(2) = bmat(2,2)
   atom%bij(3) = bmat(3,3)
   atom%bij(4) = bmat(1,2)
   atom%bij(5) = bmat(1,3)
   atom%bij(6) = bmat(2,3)
!
   end subroutine set_bmat

end module atom_basic
