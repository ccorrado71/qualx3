 MODULE progtype
  use type_constants, only: DP

  integer, parameter  :: ALLOCA   = 1
  integer, parameter  :: DEALLOCA = 0

  type refine_condition_type
     integer              :: algo           ! tipo di algoritmo di l.sq.
     logical              :: auto_coefb     ! selezione automatica del numero di coeff. di backgr.?
     logical              :: auto_prof      ! affinamento automatico profilo
     logical              :: auto_stru      ! affinamento automatico struttura
     integer              :: ncauto         ! num. di cicli di affinamento automatico
     integer              :: typewei        ! schema di pesaggio
     real                 :: eps            ! criterio di convergenza
     integer              :: maxcy          ! numero massimo di cicli di affinamento
     integer              :: raction = 0    ! 0=Rietveld 1=Le Bail 2=Pawley
     character(len=6)     :: sprint         ! V=verbose,S=small,C=correlation,E=st.dev.,M=lsq matrices,W=window per chi,B=statusbar
  end type refine_condition_type
  
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
