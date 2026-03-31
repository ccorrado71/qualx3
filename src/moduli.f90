MODULE General
   integer, parameter :: lo = 70
   integer            :: ifAutomatic = 0
   logical            :: is_anomalous = .false.
END MODULE General

MODULE Molcom
   integer :: jscreen = 0
   integer :: ifBrekke
END MODULE Molcom

MODULE PatternRef
   integer :: kprofile
   integer, parameter :: PROFILE_EXTRA=1, PROFILE_RIETVELD=2
   integer :: kprofile_type = PROFILE_RIETVELD
END MODULE PatternRef

MODULE PhaseModule
   real               :: erore
   integer            :: isumal
   real, dimension(7) :: shiftexam
END MODULE PhaseModule

MODULE TestModel
   USE atom_basic, only: atom_type
   type(atom_type), dimension(:), allocatable :: atmpub
   real, allocatable                          :: distv(:)
   integer, allocatable                       :: npunt(:)
   real                                       :: vett_test(4)
   real                                       :: distCoord
   real                                       :: rmsd
END MODULE TestModel

