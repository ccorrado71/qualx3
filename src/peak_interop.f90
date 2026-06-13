 module peak_search_interop

 use iso_c_binding

 implicit none

 private

   type, bind(C) :: peak_search_settings
     real(c_double)  :: minRange, maxRange     ! total amplitude of search range
     real(c_double)  :: minSearch, maxSearch   ! search restricted to this range
     real(c_double)  :: threshold              ! threshold on intensity
     integer(c_int)  :: sensitivity            ! sensistivity
     integer(c_int)  :: numPeaks               ! number of peaks found during the search
     integer(c_int)  :: numPeaksTot            ! total number of accesible peaks
     logical(c_bool) :: append                 ! new peak are appended to the previous search
   end type peak_search_settings

   type, bind(C) :: c_peak_type
     real(c_float) ::  x
     real(c_float) ::  xd
     real(c_float) ::  y
     real(c_float) ::  fwhm
   end type c_peak_type

   integer :: MAXNPSMOOTH = 50, MINSENS = 0, MAXSENS = 100
 
   integer :: peak_pos_message = 2
  contains

   subroutine peak_search_action(iAction, pSettings) bind(C,name="peak_search_action")
   use iso_c_binding
   USE VIEW, only: vedinew
   USE peak_mod
!FIX LATER   USE messagemod
   USE variables, only: dataset
   USE datamod
!corr   implicit none
!corr   interface
!corr     subroutine run_peaksearch(gui)
!corr     logical, intent(in), optional :: gui
!corr     end subroutine run_peaksearch
!corr   end interface
!
   integer(c_int), intent(in), value                :: iAction
   type(peak_search_settings)                       :: pSettings
   type(peak_type), dimension(:), allocatable, save :: pksav
   type(peak_type), dimension(:), allocatable       :: pktemp
   type(peaks_condition_type), save                 :: pkconds
   type(peaks_condition_type)                       :: pkcond_def
   real                                             :: scaln
   integer                                          :: npeak

   select case (iAction)
      case (0)    ! Cancel (reset the conditions)
        pkcond = pkconds
        call peak_copy(pkind,pksav)
!FIX LATER        call write_message('Number of peaks:',inum=numpeaks(pkind),pos=peak_pos_message)
        call vedinew()

     case (1)    ! Open window, pass variables to graphic, save settings
        if (.not.allocated(pkindtot)) then
            if (.not.allocated(pkind)) then ! Peak search has been not performed
                call run_peaksearch()
            else                            ! Peaks from external file: create only array pkindtot
                pkcond%srange = init_peak_range(dataset(1)%x,dataset(1)%y,dataset(1)%wave(1))
                call peak_create(dataset(1)%x,dataset(1)%y(:) - dataset(1)%yb(:),  &
                                 pksav,pkcond,dataset(1)%wave(1),pkindtot)
            endif
        endif
        pkconds = pkcond
        call peak_copy(pksav,pkind)
        pSettings%minRange = dataset(1)%xminc()
        pSettings%maxRange = dataset(1)%xmaxc()
        if (pkcond%minp > 0) then
            pSettings%minSearch = pkcond%minp
        else
            pSettings%minSearch = dataset(1)%xminc()
        endif
        if (pkcond%maxp > 0) then
            pSettings%maxSearch = pkcond%maxp
        else
            pSettings%maxSearch = dataset(1)%xmaxc()
        endif
        pSettings%threshold = pkcond%amp
        pSettings%numPeaks = numpeaks(pkind)
        pSettings%numPeaksTot = numpeaks(pkindtot)
        pSettings%sensitivity = sensitivity(pkcond%smcond(2),pkcond%smcond(1))
        pSettings%append = .false.
!FIX LATER        peak_pos_message = rowFindMesg(c_char_"Number of peaks:"//c_null_char)
!!        peak_pos_message = getRowCountMesg() + 1

     case (3,4)     ! Threshold(4), Range(3)
        if (numpeaks(pkindtot) == 0) return
        pkcond%minp = real(pSettings%minSearch)
        pkcond%maxp = real(pSettings%maxSearch)
        pkcond%amp = real(pSettings%threshold)

        call peak_copy(pktemp,pkindtot)
        call filtrasog(pktemp,pkcond%amp/100,npeak,code=1)
        call peak_select(pktemp,pkcond%minp,pkcond%maxp,DELETE_PEAK,outside=.true.)
        call peak_delete_selected(pktemp)
        if (pSettings%append) then
!           remove peaks in the search interval from the previous list (pkind)
            call peak_select(pkind,pkcond%minp,pkcond%maxp,DELETE_PEAK)
            call peak_delete_selected(pkind)
!           add new peaks
            if (numpeaks(pktemp) > 0) call peak_add(pkind,pktemp)
        else
            call peak_copy(pkind,pktemp)
        endif
        call peak_sort(pkind,ORD_BY_X)
        pSettings%numPeaks = numpeaks(pkind)   ! force update of num. of peaks
        call update_peak_graph1(peak_pos_message)

     case (6)     ! Default
        pSettings%minSearch = max(pkcond_def%minp,dataset(1)%xminc())
        pSettings%maxSearch = max(pkcond_def%maxp,dataset(1)%xmaxc())
        pSettings%threshold = pkcond_def%amp
        pSettings%sensitivity = sensitivity(pkcond_def%smcond(2),pkcond_def%smcond(1))
        pSettings%append = .false.
        pkcond = pkcond_def
        call run_peaksearch(gui=.false.)
        pSettings%numPeaks = numpeaks(pkind)
        call update_peak_graph1(peak_pos_message)

     case (7)     ! Sensitivity
        pkcond%smcond(1) = smoothing_points(pkcond%smcond(2),pSettings%sensitivity)
        pkcond%minp = real(pSettings%minSearch)
        pkcond%maxp = real(pSettings%maxSearch)
        pkcond%amp = real(pSettings%threshold)
        pkcond%autos = .false.
        if (pSettings%append) call peak_copy(pktemp,pkind)
        call run_peaksearch(gui=.false.)
        if (pSettings%append) then
            call peak_select(pktemp,pkcond%minp,pkcond%maxp,DELETE_PEAK)
            call peak_delete_selected(pktemp)
            if (numpeaks(pktemp) > 0) call peak_add(pkind,pktemp)
            call peak_sort(pkind,ORD_BY_X)
        endif
        pSettings%numPeaks = numpeaks(pkind)
        pkcond%autos = .true.
        call update_peak_graph1(peak_pos_message)

     case (5)     ! Number of peaks was changed
        if (numpeaks(pkindtot) == 0) return
        pkcond%minp = real(pSettings%minSearch)
        pkcond%maxp = real(pSettings%maxSearch)
        pkcond%maxpk = pSettings%numPeaks
        if (pkcond%maxpk == numpeaks(pkind) .and. .not.pSettings%append) return
        pkcond%amp = 0
!
!       Recover peaks and intensities for compute new amp (necessary also if maxpk < numpeaks)
        call peak_copy(pktemp,pkindtot)
        call filtrasog(pktemp,pkcond%amp/100,npeak,code=1)
        call peak_select(pktemp,pkcond%minp,pkcond%maxp,DELETE_PEAK,outside=.true.)
        call peak_delete_selected(pktemp)
!
!       Select nmaxpk peaks for intensities
        call peak_sort(pktemp,-ORD_BY_Y)
        if (numpeaks(pktemp) > pkcond%maxpk) then
            call resize_peaks(pktemp,pkcond%maxpk)
        else   ! too peaks required!
            if (.not. pSettings%append) then
                pSettings%numPeaks = numpeaks(pktemp)               ! force update of num. of peaks
            endif
        endif
        if (pSettings%append) then
!           remove peaks in the search interval from the previous list (pkind)
            call peak_select(pkind,pkcond%minp,pkcond%maxp,DELETE_PEAK)
            call peak_delete_selected(pkind)
!           add new peaks
            if (numpeaks(pktemp) > 0) call peak_add(pkind,pktemp)
        else
            call peak_copy(pkind,pktemp)
            if (numpeaks(pkind) > 0) then
                scaln = 100 / maxval(pkindtot%gety())              ! recompute scale on all peaks
                pkcond%amp = pkind(numpeaks(pkind))%gety()*scaln   ! define new threshold
            else
                pkcond%amp = 100
            endif
            pSettings%threshold = pkcond%amp
        endif
        call peak_sort(pkind,ORD_BY_X)
        call update_peak_graph1(peak_pos_message)

   end select

   end subroutine peak_search_action 

! ---------------------------------------------------------------------

   subroutine run_peaksearchwin() bind(C,name="run_peaksearchwin")
!
!  When the user press the button 'Pattern' > 'Peak search'
!
   USE peak_mod
   USE variables, only: dataset
   USE datamod
!corr   interface
!corr     subroutine run_peaksearch(gui)
!corr     logical, intent(in), optional :: gui
!corr     end subroutine run_peaksearch
!corr   end interface
!
!  Reset the range to create the array pkindtot
   pkcond%minp = dataset(1)%xminc()
   pkcond%maxp = dataset(1)%xmaxc()
!
   call run_peaksearch()
   end subroutine run_peaksearchwin

!-----------------------------------------------------------------------

   integer function sensitivity(poldeg,npoints)
   integer, intent(in) :: poldeg
   integer, intent(in) :: npoints
   integer             :: max_npoints, min_npoints
   real                :: p,q
!
   max_npoints = (poldeg - 1) / 2 + 1  ! -> sens. = MAXSENS
   min_npoints = MAXNPSMOOTH           ! -> sens. = MINSENS
!
   p = (MINSENS - MAXSENS) / real(min_npoints - max_npoints)
   q = MINSENS - p * min_npoints
   sensitivity = nint(npoints*p + q)
!
   end function sensitivity

!-----------------------------------------------------------------------

   integer function smoothing_points(poldeg,sens)  
   integer, intent(in) :: poldeg
   integer, intent(in) :: sens
   integer             :: max_npoints, min_npoints
   real                :: p,q
!
   max_npoints = (poldeg - 1) / 2 + 1  ! -> sens. = MAXSENS
   min_npoints = MAXNPSMOOTH           ! -> sens. = MINSENS
!
   p = (max_npoints - min_npoints) / real(MAXSENS - MINSENS) 
   q = min_npoints - p * MINSENS
   smoothing_points = nint(sens*p + q)
!
   end function smoothing_points

!-----------------------------------------------------------------------

   integer(c_int) function peak_number() bind(C,name="peak_number")
   USE peak_mod
   peak_number = numpeaks(pkind)
   end function peak_number

!-----------------------------------------------------------------------

   elemental function cast_peak_to_c(f_peak) result(c_peak)
   use peak_mod
   type(peak_type), intent(in) :: f_peak
   type(c_peak_type)           :: c_peak

   c_peak%x    = f_peak%getx()  
   c_peak%xd   = f_peak%getd()
   c_peak%y    = f_peak%gety()
   c_peak%fwhm = f_peak%fwhm
  
   end function cast_peak_to_c

!-----------------------------------------------------------------------

   function cast_peak_to_f(c_peak) result(f_peak)
   use peak_mod
   type(c_peak_type), intent(in) :: c_peak
   type(peak_type)               :: f_peak

   call f_peak%setx(c_peak%x)
   call f_peak%setd(c_peak%xd)
   call f_peak%sety(c_peak%y)
   f_peak%fwhm = c_peak%fwhm
  
   end function cast_peak_to_f

!-----------------------------------------------------------------------

   subroutine get_peak_list(c_pkind) bind(C,name="get_peak_list")
   use peak_mod
   type(c_peak_type), dimension(*) :: c_pkind

   c_pkind(:numpeaks(pkind)) = cast_peak_to_c(pkind(:))

   end subroutine get_peak_list

!-----------------------------------------------------------------------

   subroutine delete_peaksC(pkvet,npeak) bind(C,name="delete_peaksC")
   use peak_mod
!FIX LATER   use messagemod
   integer(c_int), dimension(*), intent(in) :: pkvet
   integer(c_int), value, intent(in)        :: npeak

   if (npeak == 0) return
   call peak_delete(pkind,pkvet(:npeak)+1)
!FIX LATER   peak_pos_message = rowFindMesg(c_char_"Number of peaks:"//c_null_char)
!FIX LATER   call write_message('Number of peaks:',inum=numpeaks(pkind),pos=peak_pos_message)

   end subroutine delete_peaksC

!-----------------------------------------------------------------------

   subroutine peak_list_change(c_peak,irow,icol,ier) bind(C,name="peak_list_change")
!
!  Edit peak or add and edit peak
!
   USE peak_mod
   USE counts
!FIX LATER   USE patternref, only: thmin,thmax
   USE arrayutil
!FIX LATER   USE conteggi, only:theta_int
   USE view
!FIX LATER   USE messagemod
   !USE variables, only: dataset
!FIX LATER   USE General, only: alambda
   implicit none
   type(c_peak_type), intent(inout)  :: c_peak ! modified peak
   integer(c_int), intent(in), value :: irow  ! row 
   integer(c_int), intent(in), value :: icol  ! 1 = 2theta was changed, 2 d was changed
   integer(c_int), intent(out)       :: ier
   !real                              :: tthval
   integer                           :: npeak
   type(peak_type)                   :: peak
   logical                           :: addpeak
!
   ier = 0
   npeak = numpeaks(pkind)
   addpeak = irow > npeak
!
!  Return if existing peak was not modified
   if (irow <= npeak) then
       peak = pkind(irow)
       if (icol == 1) call peak%setx(c_peak%x)
       if (icol == 2) call peak%setd(c_peak%xd)
       if (icol == 1 .and. equal_peak(peak,pkind(irow))) return
       if (icol == 2 .and. equal_peak_d(peak,pkind(irow))) return
   else
       peak = cast_peak_to_f(c_peak)
   endif
!
!FIX LATER
!   if (icol == 1) then      ! 2theta was changed
!       if (peak%getx() <= thmin .or. peak%getx() >= thmax) then ! 2theta out of range
!           if (addpeak) then
!               ier = 2
!           else
!               ier = 1
!               c_peak = cast_peak_to_c(pkind(irow))
!           endif
!           return
!       endif
!       call peak%calcd(alambda)
!   elseif (icol == 2) then  ! d was changed
!       tthval = thvalue(peak%getd(),alambda)
!       if (tthval <= thmin .or. tthval >= thmax) then ! 2theta out of range
!           if (addpeak) then
!               ier = 2
!           else
!               ier = 1
!               c_peak = cast_peak_to_c(pkind(irow))
!           endif
!           return
!       endif
!       call peak%setx(tthval)
!   endif
   if (addpeak) then   ! add on peak list
!      pay attention: this peak was added
!      set y and fwhm
!FIX LATER       call peak%get_int(theta_int(:,1),theta_int(:,2),dataset(1)%yb)
       peak%fwhm = peak_fwhm(pkind,peak%getx())
!FIX LATER       call peak%integrated_intensity(theta_int(:,1),theta_int(:,2),dataset(1)%yb)
!
!      add peak to list 
       call peak_add(pkind,peak,INSERT_PEAK)
!FIX LATER       call write_message('Number of peaks:',inum=numpeaks(pkind),pos=0)
   else
!FIX LATER       call peak%get_int(theta_int(:,1),theta_int(:,2),dataset(1)%yb)  ! set y
!FIX LATER       call peak%integrated_intensity(theta_int(:,1),theta_int(:,2),dataset(1)%yb)
       call peak_update_position(pkind,peak,irow)
   endif
   c_peak = cast_peak_to_c(peak)
   !call peak_print(pkind,kpr=0)
!
   end subroutine peak_list_change

!-------------------------------------------------------------------------------------------------------  

   subroutine LoadPeaksC(filename, length, tipo, ier) bind(C,name="LoadPeaksC")
   use strutil
   use iso_c_binding, only: c_char, c_int
   character(c_char), intent(in)      :: filename(*)
   integer(c_int), intent(in), value  :: length
   integer(c_int), intent(in), value  :: tipo
   integer(c_int), intent(out)        :: ier
   character(len=:), allocatable      :: filenam

   filenam = toFortranString(filename,length)
   call LoadPeaks(filenam,tipo,ier)

   end subroutine LoadPeaksC

!-------------------------------------------------------------------------------------------------------  

   subroutine SavePeaksC(filename, length, tipo) bind(C,name="SavePeaksC")
   use strutil
   use iso_c_binding, only: c_char, c_int
   character(c_char), intent(in)      :: filename(*)
   integer(c_int), intent(in), value  :: length
   integer(c_int), intent(in), value  :: tipo
   character(len=:), allocatable      :: filenam

   filenam = toFortranString(filename,length)

   call SavePeaks(filenam,tipo)
 
   end subroutine SavePeaksC 

!-------------------------------------------------------------------------------------------------------  

   function delta2thetaPeaks() bind(C,name="delta2thetaPeaks")
   use peak_mod
   real(c_double)  :: delta2thetaPeaks
   real, parameter :: deltatemp = 1.3
!
   if (numpeaks(pkind) > 0) then
      delta2thetaPeaks = min(0.5,deltatemp*(sum(pkind%fwhm)/numpeaks(pkind)))
   else
      delta2thetaPeaks = 0.5
   endif
!
   end function delta2thetaPeaks

 end module peak_search_interop 
