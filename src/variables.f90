MODULE variables

!   USE connect_mod, only: bond_type
!   USE atom_basic, only: atom_type
!   USE reflection_type_util, only: reflection_type
   USE datasetmod, only: dataset_type
   USE crystal_phase, only: crystal_phase_t
!
!   type(reflection_type), allocatable               :: ref(:) ! riflessi
!   type(atom_type), dimension(:), allocatable       :: atm    ! gli atomi
!   type(bond_type), dimension(:), allocatable       :: lconn  ! i legami
   type(dataset_type), dimension(:), allocatable    :: dataset
   type(crystal_phase_t), dimension(:), allocatable :: cryst

END MODULE variables
! -------------------------------------------------------------------------

MODULE peak_mod

   USE peak_util

   type(peak_type), dimension(:), allocatable :: pkind,pkindtot
   type(peaks_condition_type)                 :: pkcond

END MODULE peak_mod

