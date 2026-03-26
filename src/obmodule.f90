module OBmodule

USE atom_basic
USE connect_mod
USE iso_c_binding
implicit none
type(atom_type), dimension(:), allocatable :: atom_from_c
type(bond_type), dimension(:), allocatable :: bond_from_c
integer                                    :: spgnum
character  (len=16)                        :: spgname
real                                       :: MyCell(6)
logical                                    :: has_cell = .false. 
logical                                    :: has_spg  = .false. 

integer, parameter :: FF_MMFF94=1, FF_GHEMICAL=2, FF_UFF=3, FF_AUTO=4
integer, parameter :: CG_ALGO=1, SD_ALGO=2

type force_field_type 
  integer        :: id = FF_MMFF94         ! forcefield method
  integer        :: algo = CG_ALGO         ! optimization algorithm
  integer        :: steps = 500            ! maximum number of iterations
  integer        :: constr_atoms = 0       ! if 1 constraints on positions of atoms is enabled
  integer        :: logf = 1               ! set log file
  real(c_double) :: conv = 1.e-7           ! energy convergence criteria
end type force_field_type

contains

   subroutine OBConversionWriteFile(filename,atom_for_c,bond_for_c,cell,spg)
   USE unit_cell
   USE spginfom
   USE strutil, only: lung
   character(len=*), intent(in)                           :: filename
   type(atom_type), dimension(:), intent(in)              :: atom_for_c
   type(bond_type), dimension(:), allocatable, intent(in) :: bond_for_c
   type(cell_type), intent(in)                            :: cell
   type(spaceg_type), intent(in)                          :: spg
   real, dimension(6)                                     :: cella
   integer                                                :: numb_at, nb
   integer                                                :: OBWriteFileC, ier
   integer                                                :: num, ids
   
   numb_at = size(atom_for_c)
   nb = numbonds(bond_for_c)
   cella = cell%get_par()
   num = spg%num
   ids = spg%id_setting()
   ier = 1
!corr   if (nb > 0) then
!corr       ier = OBWriteFileC(filename(1:lung(filename)) // char(0), numb_at, nb,  &
!corr             atom_for_c, bond_for_c, cella(1:6), trim(spg%symbol_xhm)//char(0), num, ids)
!corr   else
!corr       ier = OBWriteFileC(filename(1:lung(filename)) // char(0), numb_at, nb,  &
!corr           atom_for_c, [bond_type()], cella(1:6), trim(spg%symbol_xhm)//char(0), num, ids)
!corr   endif
   
   end subroutine OBConversionWriteFile

!---------------------------------------------------------------------------

   subroutine OBConversionReadFile(filename,atom,bond,err)
   USE atom_type_util
   USE errormod
   USE strutil
   USE connect_mod
   USE fileutil
   character(len=*), intent(in)                            :: filename
   type(atom_type), dimension(:), allocatable, intent(out) :: atom
   type(bond_type), dimension(:), allocatable, intent(out) :: bond
   type(error_type), intent(out)                           :: err
   character(len=500)                                      :: message
   integer                                                 :: OBReadFileC, ier
!
   if (.not.file_exist(filename)) then
       ier = 1
       call err%set('File '//trim(filename)//' does not exist.')
       return
   endif
!
   ier = 1
!corr   ier = OBReadFileC(trim(filename) // char(0), message)
   if (ier /= 0) then
       message(len_noctrl(message)+1:) = ' '    ! remove all control character
       call err%set(message)
       return
   endif
!    
!corr #if 0
!corr    write(0, *) 'Number of Atoms ', size(atom_from_c)
!corr    write(0, *) 'Number of Bonds ', size(bond_from_c)
!corr    if(has_cell) then
!corr       write(0, *) 'Cell parameters ', MyCell
!corr    endif
!corr    if(has_spg) then
!corr       write(0, *) 'Spacegroup number ', spgnum
!corr       write(0, *) 'Spacegroup name ', spgname
!corr    endif
!corr    !call stampa_struttura(atom_from_c,kpr=0)
!corr #endif
   call copy_atoms(atom, atom_from_c)
   call copy_bonds(bond, bond_from_c)
   call clear_atoms(atom_from_c)
   call clear_bonds(bond_from_c)
!
   end subroutine OBConversionReadFile

!---------------------------------------------------------------------------

   subroutine OBConversionFromSmiles(smiles_string,atom,bond,err)
   USE atom_type_util
   USE errormod
   USE strutil
   USE connect_mod
   character(len=*), intent(in) :: smiles_string
   type(atom_type), dimension(:), allocatable, intent(out) :: atom
   type(bond_type), dimension(:), allocatable, intent(out) :: bond
   type(error_type), intent(out)                           :: err
   character(len=500)                                      :: message
   integer                                                 :: OBReadSmilesC, ier
!
   ier = 1
!corr   ier = OBReadSmilesC(trim(smiles_string) // char(0), message)
   if (ier /= 0) then
       message(len_noctrl(message)+1:) = ' '    ! remove all control character
       call err%set(message)
       return
   endif
   call copy_atoms(atom, atom_from_c)
   call copy_bonds(bond, bond_from_c)
!corr   call reallocate(atom_from_c,0)
   call clear_atoms(atom_from_c)
   call clear_bonds(bond_from_c)
!
   end subroutine OBConversionFromSmiles

!---------------------------------------------------------------------------

   subroutine OBOptimizeGeometry(atom, bond, cell, err, vconstr)
   USE atom_type_util
   USE strutil
   USE unit_cell
   USE errormod
   USE connect_mod
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: bond
   type(cell_type), intent(in)                               :: cell
   type(error_type), intent(out)                             :: err
   character(len=500)                                        :: message
   integer, dimension(:), intent(in), optional               :: vconstr !array containing the fixed atoms
   integer, dimension(:), allocatable                        :: vcon
   integer                                                   :: numb_at, nb
   integer                                                   :: OBOptimizeGeometryC, ier
   type(force_field_type)                                    :: ffopt
!!!!FIXME: gestire modelli con ghost
   numb_at = size(atom)
   nb = numbonds(bond)
   if (present(vconstr)) then
       allocate(vcon(size(vconstr)), source=vconstr)
       ffopt%constr_atoms = size(vconstr)
   else
       allocate(vcon(1), source=0)
   endif
!
   call frac_to_cart(atom,cell)
   ier = 1
!corr   if (nb == 0) then
!corr       ier = OBOptimizeGeometryC(ffopt, numb_at, nb, atom, [bond_type()], vcon, message)
!corr   else
!corr       ier = OBOptimizeGeometryC(ffopt, numb_at, nb, atom, bond, vcon, message)
!corr   endif
!corr   if (ier == -5 .and. ffopt%id == FF_MMFF94) then
!corr       ffopt%id = FF_UFF
!corr       if (nb == 0) then
!corr           ier = OBOptimizeGeometryC(ffopt, numb_at, nb, atom, [bond_type()], vcon, message)
!corr       else
!corr           ier = OBOptimizeGeometryC(ffopt, numb_at, nb, atom, bond, vcon, message)
!corr       endif
!corr   endif
   call cart_to_frac(atom,cell)
   if (ier /= 0) then
       message(len_noctrl(message)+1:) = ' '    ! remove all control character
       call err%set(message)
   endif
!
   end subroutine OBOptimizeGeometry

end module OBmodule

   subroutine passa_atomo_al_fortran(i,nat,atom)
   USE OBmodule
   USE atom_type_util
   integer, intent(in)         :: i
   integer, intent(in)         :: nat
   type(atom_type), intent(in) :: atom
   
   if (i == 1) then
!      allocate atom_from_c to nat at the first atom
       call new_atoms(atom_from_c,nat)
   endif
   atom_from_c(i) = atom
   
   end subroutine passa_atomo_al_fortran
!---------------------------------------------------------------------------
   subroutine passa_legame_al_fortran(i,nb,bond)
   USE OBmodule
   integer, intent(in)         :: i
   integer, intent(in)         :: nb
   type(bond_type), intent(in) :: bond
   
   if (i == 1) then
!      allocate bond_from_c to nat at the first bond
       call new_bonds(bond_from_c,nb)
   endif
   bond_from_c(i) = bond
   
   end subroutine passa_legame_al_fortran
!---------------------------------------------------------------------------
   subroutine passa_cella_al_fortran(cella)
   USE OBmodule
   real, dimension(6),intent(in):: cella
   
   MyCell = cella
   has_cell = .true.
   end subroutine passa_cella_al_fortran
!---------------------------------------------------------------------------
   subroutine passa_spg_al_fortran(spgn)
   USE OBmodule
   integer, intent(in)      :: spgn
   
   spgnum = spgn
   has_spg = .true.
   
   end subroutine passa_spg_al_fortran
!---------------------------------------------------------------------------
   subroutine passa_spgnam_al_fortran(spgnam)
   USE OBmodule
   USE strutil, only: Lung
   character  (len=16), intent(in) :: spgnam
   
   spgname = spgnam(1:Lung(spgnam))
   
   end subroutine passa_spgnam_al_fortran
!---------------------------------------------------------------------------
   integer function GetAtomicNumber(atomo)
   USE atom_basic
   type(atom_type), intent(in) :: atomo
!
   GetAtomicNumber = atomo%z()
!
   end function GetAtomicNumber
!---------------------------------------------------------------------------
