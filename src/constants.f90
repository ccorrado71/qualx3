module prog_constants

!   Message Window codes [1-6]
    enum, bind(c) 
      enumerator :: INFO_WINDOW = 1, QUEST_WINDOW, QUEST_WINDOW_R,          &
      QUEST_WINDOW_YES_NO, QUEST_WINDOW_NO_YES, THREE_BUTTONS, WARN_WINDOW, &
      ERR_WINDOW, SEVERE_ERR_WINDOW
    endenum
!
!  Message error code
   enum, bind(c)
     enumerator :: ERR_MISSING_SPG = 1001, ERR_UNKNOWN_SPG, ERR_CELL_PARAM, ERR_STRUCTURE
   endenum

   real, parameter :: CU_WAVE = 1.540560, MO_WAVE = 0.71073       ! frequently used wavelengths
   real, parameter :: DEF_WAVE = CU_WAVE, DEF_WAVE2 = 1.54442740  ! Wavelengths reported on ITC Vol C 2004, pag.206: 
                                                                  ! Ka1=1.54059290 Ka2=1.54442740
   real, parameter :: AU_TO_ANG = 0.529177249
!
!  wavelengths from ITC volume C (table 4.2.2.1). Third Edition 2004
!                                              kalpha1       kalpha2    kbeta
   real, parameter, dimension(3) :: CUwave = [1.54059290, 1.54442740, 1.3922340]
   real, parameter, dimension(3) :: MOwave = [0.70931715, 0.713607,   0.632303 ]
   real, parameter, dimension(3) :: CRwave = [2.2897260,  2.2936510,  2.0848810]
   real, parameter, dimension(3) :: COwave = [1.7889960,  1.7928350,  1.6208260]
   real, parameter, dimension(3) :: Fewave = [1.9360410,  1.9399730,  1.7566040]
end module prog_constants

module trig_constants
!  Trigonometric constants
   real, parameter :: pi     = 4.0*atan(1.0)     ! pi
   real, parameter :: twopi  = 2.0*pi            ! 2*pi
   real, parameter :: twopis = 2.0*pi*pi         ! 2*pi*pi
   real, parameter :: rtod   = 180.0/pi          ! radiant to degree
   real, parameter :: dtor   = 1.0/rtod          ! degree to radiant
end module trig_constants

module type_constants
   integer, parameter  :: I4B = selected_int_kind(9)   ! long integer
   integer, parameter  :: SP  = kind(1.0)              ! single precision
   integer, parameter  :: DP  = kind(1.0d0)            ! double precision

   real, parameter     :: epsmch  = epsilon(epsmch)
   real(DP), parameter :: epsmchd = epsilon(epsmchd)
end module type_constants
