 MODULE progtype
  use type_constants, only: DP

  type refine_info_type
     logical :: refine_profile
     real    :: dw                 ! Durbin Watson
     real    :: rp,rwp             ! Rp,Rwp
     real    :: rp1,rpw1           ! Rp e Rwp con background sottratto
     real    :: rexp               ! Re convenzionale
     real    :: rexp1              ! Re con background sottratto
     real    :: chi2               ! GOF 
     real    :: sall               ! S = chi including restraints
     real    :: rbragg             ! R-Bragg
     integer :: npartot            ! Total number of refined parameters
     integer :: npar               ! Parameter refined at last cycle 
     integer :: nref                 
     integer :: nres
     logical :: riding
  end type refine_info_type

  type rparam_type
     real                  :: val  = 0  ! valore del parametro
     real                  :: sd = 0    ! sd
     integer               :: rcod = 0  ! posizione nella matrice jac. del parametro se affinato; 0 se non affinato
     character(len=10)     :: str = ' ' ! stringa associata al parametro
  end type rparam_type

  type rparam_type_dp
     real(DP)              :: val  = 0  ! valore del parametro
     real                  :: sd = 0    ! sd
     integer               :: rcod = 0  ! posizione nella matrice jac. del parametro se affinato; 0 se non affinato
     character(len=10)     :: str = ' ' ! stringa associata al parametro
  end type rparam_type_dp

  type deratom
     real, dimension(3)    :: co
     real                  :: b
     real                  :: occ
  end type deratom

 END MODULE progtype
