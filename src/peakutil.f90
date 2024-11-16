MODULE peak_util

implicit none

   type peak_type
     real, private :: x = 0.0
     real, private :: xd = 0.0
     real, private :: y = 0.0
     real          :: yint = 0.0
     real          :: fwhm = 0.2
     real          :: asym = 0.0
     integer       :: sel = 0    
     integer       :: rcod = 1          ! Refinement code 
     integer       :: info = 0

   contains 
     procedure :: width_points
     procedure :: get_int
     procedure :: integrated_intensity
     procedure :: getx
     procedure :: getd
     procedure :: gety
     procedure :: calcd
     procedure :: setx
     procedure :: sety
     procedure :: setd
     procedure :: setxd

   end type peak_type

   real, parameter, private :: DEF_AMP = 1.5

   type peaks_condition_type
      real                  :: slope = 0.1       ! threshold on slope of derivative
      real                  :: amp   = DEF_AMP   ! % threshold on height
      real                  :: srange = 0.15
      integer, dimension(2) :: smcond = [11,9]   ! smoothing parameters = [smoothing points,polynomial degree]
      integer               :: maxpk = -1        ! > 0 maximum number of peaks; < 0 no limit
      integer               :: minpk = -1        ! > 0 minimum number of peaks; < 0 no limit
      real                  :: minp = -1         ! min value of data for peak search
      real                  :: maxp = -1         ! min value of data for peak search
      logical               :: refine = .false.  ! Refine peaks
      logical               :: autos = .true.    ! automatic computing of the smoothing points
   end type peaks_condition_type

   integer, parameter :: ORD_BY_X=1, ORD_BY_Y=2, ORD_BY_FW=3
   integer, parameter :: APPEND_PEAK=1, INSERT_PEAK=2, DELETE_PEAK=-999, SELECT_PEAK = 10

!F numpeaks(peak)                             !Number of peaks
!S peak_set(peak,npk,x,y,fwhm,asym,sel)       !Set peak list
!S peak_get(peak,npk,x,y,fwhm,asym,sel)       !Get peak list
!S peak_print(peak,kpr)                       !Print peak list
!S peak_add(peak,padd)                        !Add peak to list
!F equal_peak(peak1,peak2)                    !Check if 2 peaks have the same x
!F equal_peak_d(peak1,peak2)                  !Check if 2 peaks have the same d
!F peak_fwhm(peak,xvalue)                     !Assegna al picco ipos una fwhm a partire dai picchi adiacenti
!S peak_sort(peak)                            !Sort peaks
!S peak_unselect(peak)                        !Deseleziona i picchi
!S peak_select(peak, xmin, xmax)              !Seleziona i picchi tra xmin e xmax
!S peak_copy(peak1,peak2)                     !Copia peak2 in peak1
!S peak_delete_selected(peak,npeak)           !Cancella i picchi selezionati
!S peak_delete                                !Delete peak from number or x-position
!S peak_filterd(peak,dmin)                    !Filtra in a sorted list for d <= dmin

private peak_add_s, peak_add_v
interface peak_add
  module procedure peak_add_s, peak_add_v
end interface

private peak_delete_ip, peak_delete_x, peak_delete_vet
interface peak_delete
  module procedure peak_delete_ip, peak_delete_x, peak_delete_vet
end interface 

CONTAINS

   integer function numpeaks(peak)
!
!  Number of peaks
!
   type(peak_type), dimension(:), allocatable, intent(in) :: peak
!   
   if (allocated(peak)) then
       numpeaks = size(peak)
   else
       numpeaks = 0
   endif
!
   end function numpeaks

!--------------------------------------------------------------------------------------------

   function width_points(pk,x)  result(xp)
   USE arrayutil
   class(peak_type), intent(in)   :: pk
   real, dimension(:), intent(in) :: x
   integer, dimension(2)          :: xp
   integer, parameter             :: NW = 2
!
   xp(1) = clocate(x,pk%x - NW*pk%fwhm)
   xp(2) = clocate(x,pk%x + NW*pk%fwhm)
!
   end function width_points

!--------------------------------------------------------------------------------------------
   elemental real function getx(pk)
   class(peak_type), intent(in)   :: pk
   getX = pk%x
   end function getx
!--------------------------------------------------------------------------------------------
   elemental real function getd(pk)
   class(peak_type), intent(in)   :: pk
   getd = pk%xd
   end function getd
!--------------------------------------------------------------------------------------------
   elemental real function gety(pk)
   class(peak_type), intent(in)   :: pk
   gety = pk%y
   end function gety
!--------------------------------------------------------------------------------------------

   elemental subroutine calcd(pk,wave)
   USE counts, only: dvalue
   class(peak_type), intent(inout) :: pk
   real, intent(in) :: wave
   pk%xd = dvalue(pk%x,wave)
   end subroutine calcd
!--------------------------------------------------------------------------------------------
   subroutine setx(pk,xvalue)
   class(peak_type), intent(inout) :: pk
   real, intent(in)                :: xvalue
   pk%x = xvalue
   end subroutine setx
!--------------------------------------------------------------------------------------------
   subroutine sety(pk,yvalue)
   class(peak_type), intent(inout) :: pk
   real, intent(in)                :: yvalue
   pk%y = yvalue
   end subroutine sety
!--------------------------------------------------------------------------------------------
   subroutine setd(pk,dvalue)
   class(peak_type), intent(inout) :: pk
   real, intent(in)                :: dvalue
   pk%xd = dvalue
   end subroutine setd
!--------------------------------------------------------------------------------------------
   subroutine setxd(pk,xvalue,wave)
   class(peak_type), intent(inout) :: pk
   real, intent(in)                :: xvalue
   real, intent(in)                :: wave
   pk%x = xvalue
   call pk%calcd(wave)
   end subroutine setxd
!--------------------------------------------------------------------------------------------

   subroutine peak_set(peak,npk,x,y,fwhm,asym,sel,code,wave)
!
!  Set peak list
!
   USE counts
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak        
   integer, intent(in), optional                             :: npk
   real, dimension(:), intent(in),optional                   :: x,y,fwhm,asym ! must be at least of size npk
   integer, dimension(:), intent(in), optional               :: sel
   integer, intent(in), optional                             :: code
   real, intent(in), optional                                :: wave
   integer :: npeak
!
   if (present(npk)) then
       !call reallocate_peaks(peak,npk,.false.)
       call new_peaks(peak,npk)
   endif
   npeak = numpeaks(peak)
   if (npeak == 0) return
!
   if (present(x) .and. present(wave)) then
       peak%x = x(:npeak)
       peak%xd = dvalue(x(:npeak),wave)
   endif
   if (present(y)) peak%y = y(:npeak)
   if (present(fwhm)) peak%fwhm = fwhm(:npeak)
   if (present(asym)) peak%asym = asym(:npeak)
   if (present(sel)) peak%sel = sel(:npeak)
   if (present(code)) peak%rcod = code
!   
   end subroutine peak_set

!--------------------------------------------------------------------------------------------

   subroutine peak_get(peak,npk,x,y,fwhm,asym,sel)
!
!  Get peak list
!
   type(peak_type), dimension(:), allocatable, intent(in) :: peak
   integer, intent(out)                                   :: npk
   real, dimension(:), intent(out),optional               :: x,y,fwhm,asym ! must be at least of size npk
   integer, dimension(:), intent(out), optional           :: sel
!
   npk = min(numpeaks(peak),size(x))
   if (npk > 0) then
       if (present(x)) then
           x(:npk) = peak(:npk)%x
       endif
       if (present(y)) then
           y(:npk) = peak(:npk)%y
       endif
       if (present(fwhm)) then
           fwhm(:npk) = peak(:npk)%fwhm
       endif
       if (present(asym)) then
           asym(:npk) = peak(:npk)%asym
       endif
       if (present(sel)) then
           sel(:npk) = peak(:npk)%sel
       endif
   endif
!   
   end subroutine peak_get

!--------------------------------------------------------------------------------------------

   subroutine peak_print(peak,kpr,filename,ini,fin,title)
!
!  Print peak list
!
   USE fileutil
   USE strutil, only: centra_str
   type(peak_type), dimension(:), allocatable, intent(in) :: peak
   integer, intent(in), optional                          :: kpr
   character(len=*), intent(in), optional                 :: filename
   integer, intent(in), optional                          :: ini,fin
   character(len=*), intent(in), optional                 :: title
   integer                                                :: i
   real                                                   :: scaly
   integer, dimension(1)                                  :: pini,pfin
   integer                                                :: prunit
   type(file_handle)                                      :: filep
!
   if (numpeaks(peak) > 0) then
       if (present(kpr)) then
           prunit = kpr
       else
           prunit = 0
       endif
       if (present(filename)) then
           call filep%fopen(filename,ios='w')
           if (filep%good()) prunit = filep%handle()
       endif
       if (present(ini)) then
           pini = max(lbound(peak),ini)
       else
           pini = lbound(peak)
       endif
       if (present(fin)) then
           pfin = min(ubound(peak),fin)
       else
           pfin = ubound(peak)
       endif
       write(prunit,'(2x,63("-"))')
       if (present(title)) then
           write(prunit,'(2x,a)') centra_str(trim(title),63)
       else
           write(prunit,'(a)') '                          Peak positions'
       endif
       write(prunit,'(2x,63("-"))')
       write(prunit,'(a)') '             2theta           d            FWHM     100.*I/Imax'
       write(prunit,'(2x,63("-"))')
       scaly = 100/maxval(peak%y)
       do i=pini(1),pfin(1)
          write(prunit,'(i5,")",4x,f10.4,2x,f12.6,4x,f10.4,4x,f8.2)')i,peak(i)%x,peak(i)%xd,peak(i)%fwhm,scaly*peak(i)%y
       enddo
       if (present(filename) .and. filep%good()) call filep%fclose()
   endif
!
   end subroutine peak_print

!--------------------------------------------------------------------------------------------

   subroutine peak_add_s(peak,padd,code)
!
!  Add peak to list
!   
   USE arrayutil
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak        
   type(peak_type), intent(in)                               :: padd
   integer, optional                                         :: code
   integer                                                   :: npeak
   integer                                                   :: pcode,pos
!
   if (present(code)) then
       pcode = code
   else
       pcode = APPEND_PEAK
   endif
!
   npeak = numpeaks(peak)
!
   if (npeak == 0) then
       allocate(peak(1),source=padd)
       return
   endif
!
   select case(code)
   case (APPEND_PEAK)
      call resize_peaks(peak,npeak+1)
      peak(npeak+1) = padd

   case (INSERT_PEAK)
!
!     Find position to insert peak, peaks must be sorted by x
      pos = clocate(peak%x,padd%x)
      if (padd%x > peak(pos)%x) then
          pos = pos + 1
      endif
!
!     insert peak
      call resize_peaks(peak,npeak+1)
      peak(pos:) = [padd,peak(pos:npeak)]
      !call peak_print(peak,0,pos-2,pos+2)
   end select
!   
   end subroutine peak_add_s   
   
!--------------------------------------------------------------------------------------------

   subroutine peak_add_v(peak,padd)
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak        
   type(peak_type), intent(in), dimension(:)                 :: padd
   integer                                                   :: npkcurr,npkadd,npknew
!
   npkcurr = numpeaks(peak)
   npkadd = size(padd)
   npknew = npkcurr + npkadd
   call resize_peaks(peak,npknew)
   peak(npkcurr+1:) = padd
!
   end subroutine peak_add_v

!--------------------------------------------------------------------------------------------

   logical function equal_peak(peak1,peak2)
   type(peak_type), intent(in) :: peak1,peak2
   equal_peak = (abs(peak1%x - peak2%x) <= epsilon(1.0))
   end function equal_peak

!--------------------------------------------------------------------------------------------

   logical function equal_peak_d(peak1,peak2)
   type(peak_type), intent(in) :: peak1,peak2
   equal_peak_d = (abs(peak1%xd - peak2%xd) <= epsilon(1.0))
   end function equal_peak_d

!--------------------------------------------------------------------------------------------

   logical function is_peak_position(peaks,xpos) result(is_peak)
!
!  Verify if xpos is a peak position
!
   type(peak_type), dimension(:), intent(in) :: peaks
   real, intent(in)                          :: xpos
!cor   real                                      :: eps = epsilon(1.0)
!
   if (size(peaks) == 0) then
       is_peak = .false.
       return
   endif
   is_peak = any(abs(peaks%x - xpos) <= epsilon(1.)) 
!
   end function is_peak_position

!--------------------------------------------------------------------------------------------

   subroutine peak_update_position(peaks,peaknew,oldpos)
!
!  Update the peak list if the position oldpos was changed with new value peaknew
!
   USE arrayutil
   type(peak_type), dimension(:), allocatable, intent(inout) :: peaks   ! list of peaks sorted by x
   type(peak_type), intent(in)                               :: peaknew ! new peak 
   integer, intent(in)                                       :: oldpos  ! old position
   integer :: newpos
!
   newpos = clocate(peaks%x,peaknew%x)
   if (peaknew%x > peaks(newpos)%x) newpos = newpos + 1
   if (newpos == oldpos .or. newpos == oldpos + 1) then
       peaks(oldpos) = peaknew
   else
       peaks(oldpos) = peaknew
       call peak_move_position(peaks,oldpos,newpos)
   endif
!
   end subroutine peak_update_position

!--------------------------------------------------------------------------------------------

   subroutine peak_move_position(peaks,ini,fin)
!
!  Move peak from a old position ini to a new position fin
!
   type(peak_type), dimension(:), allocatable, intent(inout) :: peaks   ! list of peaks sorted by x
   integer, intent(in) :: ini,fin
!
   if (ini < fin) then
       peaks(:) =[peaks(1:ini-1),peaks(ini+1:fin-1),peaks(ini),peaks(fin:)]
   elseif (ini > fin) then
       peaks(:) =[peaks(1:fin-1),peaks(ini),peaks(fin:ini-1),peaks(ini+1:)]
   endif
!
   end subroutine peak_move_position

!--------------------------------------------------------------------------------------------

   real function peak_fwhm(peak,xvalue)   result(fwhm)
!
!  Assegna al picco ipos una fwhm a partire dai picchi adiacenti
!
   USE arrayutil
   type(peak_type), dimension(:), allocatable, intent(in) :: peak              
   real, intent(in)                                       :: xvalue
   integer                                                :: npeak
   integer                                                :: ipos
!
   npeak = numpeaks(peak)
   select case (npeak)
       case (0)   ! nessun picco
         fwhm = 0.2
         
       case (1)   ! solo un picco
         fwhm = peak(1)%fwhm
         
       case (2:)   ! due soli picchi
         ipos = clocate(peak%x,xvalue)        
         if (ipos == 1) then
             if (xvalue < peak(ipos)%x) then
                 fwhm = peak(ipos)%fwhm
             else
                 fwhm = (peak(ipos)%fwhm + peak(ipos+1)%fwhm)/2
             endif
         elseif (ipos == npeak) then
             if (xvalue < peak(npeak)%x) then
                 fwhm = (peak(ipos)%fwhm + peak(ipos-1)%fwhm)/2
             else
                 fwhm = peak(ipos)%fwhm
             endif
         else
             if (xvalue < peak(ipos)%x) then
                 fwhm = (peak(ipos)%fwhm + peak(ipos-1)%fwhm)/2
             else
                 fwhm = (peak(ipos)%fwhm + peak(ipos+1)%fwhm)/2
             endif
         endif
         
   end select  
!
   end function peak_fwhm
   
!--------------------------------------------------------------------------------------------

   subroutine get_int(xp,xvet,yvet,back)
!
!  Set intensity of peak from his position in xvet
!
   USE arrayutil
   class(peak_type), intent(inout)   :: xp
   real, dimension(:), intent(in) :: xvet,yvet
   real, dimension(:), intent(in), optional :: back
!
   if (present(back)) then
       xp%y = peak_intensity(xp%x,xvet,yvet,back)
   else
       xp%y = peak_intensity(xp%x,xvet,yvet)
   endif
!
   end subroutine get_int

!--------------------------------------------------------------------------------------------

   subroutine integrated_intensity(pk,xvet,yvet,back)
!
!  Compute integrete intensity of peak by using fwhm
!
   USE arrayutil
   USE math_util
   class(peak_type), intent(inout)          :: pk
   real, dimension(:), intent(in)           :: xvet,yvet
   real, dimension(:), intent(in), optional :: back
   integer, dimension(2)                    :: xp
!
   xp(:) = pk%width_points(xvet)

   if (present(back)) then
       pk%yint = integrate(xvet(xp(1):xp(2)),yvet(xp(1):xp(2)) - back(xp(1):xp(2)),1,xp(2)-xp(1)+1)
   else
       pk%yint = integrate(xvet(xp(1):xp(2)),yvet(xp(1):xp(2)),1,xp(2)-xp(1)+1)
   endif
!
   end subroutine integrated_intensity

!--------------------------------------------------------------------------------------------

   real function peak_intensity(x,xvet,yvet,back)
!
!  Set intensity of peak from his position in xvet
!
   USE arrayutil
   real, intent(in)               :: x
   real, dimension(:), intent(in) :: xvet,yvet
   real, dimension(:), intent(in), optional :: back
   integer                        :: xpos
!
   xpos = clocate(xvet,x)
   if (present(back)) then
       peak_intensity = yvet(xpos) - back(xpos)
   else
       peak_intensity = yvet(xpos)
   endif
!
   end function peak_intensity

!--------------------------------------------------------------------------------------------
   
   subroutine peak_sort(peak,code)
!
!  Ordina i modelli della lista dal più' piccolo al piu' grande
!  l'ordine e' invertito se keycode < 0
!
   USE nr
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak        
   integer                                                   :: npeak
   integer, dimension(:), allocatable                        :: iord
   integer, intent(in) :: code
!
   npeak = numpeaks(peak)
   if (npeak > 0) then
       allocate(iord(npeak))
       select case(abs(code))
         case (ORD_BY_X)
           call indexx(peak%x,iord)
           if (code < 0) iord = iord(npeak:1:-1)
           peak = peak(iord)
         case (ORD_BY_Y)
           call indexx(peak%y,iord)
           if (code < 0) iord = iord(npeak:1:-1)
           peak = peak(iord)
         case (ORD_BY_FW)
           call indexx(peak%y/peak%fwhm,iord)
           if (code < 0) iord = iord(npeak:1:-1)
           peak = peak(iord)
       end select
   endif
!   
   end subroutine peak_sort
   
!----------------------------------------------------------------------------------------------

   subroutine peak_unselect(peak)
!
!  Deseleziona i picchi
!   
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak        
   integer                                                   :: i
!   
   do i=1,numpeaks(peak)
      peak(i)%sel = 0
   enddo
!   
   end subroutine peak_unselect
   
!----------------------------------------------------------------------------------------------

   subroutine peak_select(peak, xmin, xmax, code, outside)
!
!  Seleziona i picchi tra xmin e xmax
!
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak        
   real, intent(in)                                          :: xmin,xmax   
   integer, intent(in), optional                             :: code
   logical, intent(in), optional                             :: outside
   integer                                                   :: i, codep
   logical                                                   :: outsidep
!   
   if (xmin > xmax) return
   if (present(code)) then
       codep = code
   else
       codep = SELECT_PEAK
   endif
   if (present(outside)) then
       outsidep = outside
   else
       outsidep = .false.
   endif
!   
   if (outsidep) then
       do i=1,numpeaks(peak)
          if (peak(i)%x <=xmin .or. peak(i)%x >=xmax) then
              peak(i)%sel = codep
          endif
       enddo   
   else
       do i=1,numpeaks(peak)
          if (peak(i)%x >=xmin .and. peak(i)%x <=xmax) then
              peak(i)%sel = codep
          endif
       enddo   
   endif
!
   end subroutine peak_select
   
!----------------------------------------------------------------------------------------------

   subroutine peak_copy(peak1,peak2)
!
!  Copia peak2 in peak1
!    
   type(peak_type), dimension(:), allocatable, intent(out) :: peak1                 
   type(peak_type), dimension(:), allocatable, intent(in)  :: peak2
   integer                                                 :: npeak1, npeak2
!
   npeak2 = numpeaks(peak2)
   npeak1 = numpeaks(peak1)
   if (npeak1 == npeak2) then
       if (npeak1 /= 0) peak1 = peak2
   else
       if (allocated(peak1)) deallocate(peak1)
       if (npeak2 > 0) then
           allocate(peak1(npeak2),source=peak2)
       endif
   endif
!   
   end subroutine peak_copy
   
!----------------------------------------------------------------------------------------------

   subroutine peak_delete_selected(peak)
!
!  Cancella i picchi selezionati
!
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak              
   integer                                                   :: npeak
!
   if (numpeaks(peak) > 0) then
       npeak = count(peak(:)%sel /= DELETE_PEAK)
       peak(:npeak) = pack(peak, mask=peak%sel /= DELETE_PEAK)
       call resize_peaks(peak,npeak)
   endif  
!
   end subroutine peak_delete_selected
   
!----------------------------------------------------------------------------------------------

   subroutine peak_delete_ip(peak,ip)
!
!  Delete peak from order number
!
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak              
   integer, intent(in)                                       :: ip
   integer                                                   :: npeak
!
   npeak = numpeaks(peak)
   if (npeak > 0 .and. (ip <= npeak .and. ip > 0)) then
       peak(ip:npeak-1) = peak(ip+1:npeak)
       call resize_peaks(peak,npeak-1)
   endif
!
   end subroutine peak_delete_ip

!----------------------------------------------------------------------------------------------

   subroutine peak_delete_x(peak,xpos)
!
!  Delete peak from x-position
!
   USE arrayutil
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak
   real, intent(in)                                          :: xpos
   integer                                                   :: npeak
   integer, dimension(1) :: pos
!
   npeak = numpeaks(peak)
   if (npeak > 0) then
       pos = clocate(peak%x,xpos)
       if (abs(peak(pos(1))%x - xpos) <= epsilon(1.0)) then
           peak(pos(1):npeak-1) = peak(pos(1)+1:npeak)
           call resize_peaks(peak,npeak-1)
       endif
   endif
!
   end subroutine peak_delete_x

!----------------------------------------------------------------------------------------------
   
   subroutine peak_delete_vet(peak,vindex)
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak
   integer, dimension(:), intent(in)                         :: vindex
   integer                                                   :: npeak, vsize, i
!
   npeak = numpeaks(peak)
   vsize = size(vindex)
   if (npeak > 0 .and. vsize > 0) then
       do i=1,vsize
          peak(vindex(i))%sel = DELETE_PEAK
       enddo
       call peak_delete_selected(peak)
   endif
!
   end subroutine  peak_delete_vet 

!----------------------------------------------------------------------------------------------

   subroutine peak_create(x,y,peak,pcond,wave,pklist)
   USE pointmod, only:point_type
   USE arrayutil
   USE counts
   real, dimension(:), intent(in)             :: x,y
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak
   type(peaks_condition_type), intent(inout) :: pcond
   real, intent(in)                                      :: wave
   type(peak_type), allocatable, intent(inout), optional :: pklist(:)
   integer                                   :: npoint
   integer                                   :: np  
   integer                                   :: inid, find
   real :: scalep
!
   npoint = size(x)
   if (pcond%minp < 0) then
       inid = 1
   else
       inid = clocate(x,pcond%minp)
   endif
   if (pcond%maxp < 0) then
       find = size(x)
   else
       find = clocate(x,pcond%maxp)
   endif
   if ((find - inid) <= 0) return
!
   scalep = 1000 / maxval(y(:))
   if (pcond%autos) then
       pcond%smcond(1) = get_smooth_points(x(inid:find),pcond,nderiv=3)
   endif
   call findmaxd3(pcond,x(inid:find),y(inid:find),peak,np,scalep,pklist)
   if (np < pcond%minpk) then
       pcond%amp = DEF_AMP*0.4
       call findmaxd3(pcond,x(inid:find),y(inid:find),peak,np,scalep,pklist)
   endif
!
   if (np > 0) peak%xd = dvalue(peak%x,wave)
   if (present(pklist)) then
       if (numpeaks(pklist) > 0) pklist%xd = dvalue(pklist%x,wave)
   endif
!
   end subroutine peak_create

!----------------------------------------------------------------------------------------------

   integer function get_smooth_points(xdata,pcond,nderiv)  result(npoints)
   real, dimension(:), intent(in)            :: xdata
   type(peaks_condition_type), intent(inout) :: pcond
   integer, intent(in)                       :: nderiv
   integer                                   :: ncc
   integer, parameter                        :: MAXNP=100
   real                                      :: tstep
!
   ncc = size(xdata)
   tstep = sum(xdata(2:ncc) - xdata(1:ncc-1))/(ncc-1)
   npoints = nint(pcond%srange/tstep)
   if (npoints > MAXNP) npoints = MAXNP  ! too coefficients for polynomial function
   if (npoints == 0) npoints = 2
!
!  check if npoints is compatible with NDERIV
   if (2*npoints < nderiv) npoints = ceiling(real(npoints)/2) + 1
!
   end function get_smooth_points

!----------------------------------------------------------------------------------------------

   subroutine findmaxd3(pcond,xdata,ydata,pk,np,scaln,pklist)
   USE ssmoothing
   USE genlsq
   USE math_util
   USE arrayutil
!
   implicit none
   type(peaks_condition_type), intent(inout) :: pcond
   real, dimension(:), intent(in) :: xdata, ydata
   type(peak_type), allocatable, intent(inout) :: pk(:)
   integer, intent(out)               :: np
   real, intent(in) :: scaln
   type(peak_type), allocatable, intent(inout), optional :: pklist(:)
   real                   :: slope_threshold
   real                :: amp_threshold
   integer                :: nmaxpeak
   real, dimension(size(xdata))       :: ders    ,ders1
   real, dimension(size(xdata))       :: ydatan
   integer                            :: ndat
   integer                            :: i
   integer                            :: ini,fin
   integer, dimension(1)              :: locm
   integer, parameter                 :: nplus = 100
   integer                            :: nsizep
   integer                            :: pos,posnew
   integer                            :: nprange
   integer                            :: ngroup
!corr   real :: c1,c2,c3,peakx,peaky,fw
   real :: peakx,peaky,fw
   real :: amptest
!corr   real, dimension(2) :: mu
   logical :: outrange
!corr   real, dimension(3) :: pcoef
   integer :: npoints, poldeg
   real :: sogfw 
   real :: sogIfw
!corr   integer :: ncc   !!!, pos1
!corr   real :: tstep
   logical :: kpr = .false.
       integer :: nan_peak 
   real :: pkx, pky   !!!!, scaleamp
   integer :: ier
   integer :: irange
!corr   integer :: ir
   real :: fwdef
   integer, parameter :: NDERIV=3, MAXNP=100
   !integer, dimension(2) :: xp
   !real, dimension(2) :: line,xpi
   !integer :: ierl
!
       nan_peak = 0
   slope_threshold = pcond%slope
   amp_threshold = pcond%amp
   nmaxpeak = pcond%maxpk
!
!  nsizep è un ragionevale valore iniziale di allocazione per xp e yp
   nsizep = nplus
!
!  Rialloca su npp senza salvare il suo contenuto
   !call reallocate_peaks(pk,nsizep,.false.)
   call new_peaks(pk,nsizep)
!
!  normalize ydata
   !scaln = 1000 / maxval(ydata(:))
    !sogfw = 0.4
   sogfw = pcond%srange * 3
   fwdef = pcond%srange * 1.1
   !sogfw = pcond%srange * 4
   ydatan(:) = scaln*ydata(:)
!   amptest = amp_threshold*10
   amptest = amp_threshold/100
   sogIfw = amptest / sogfw
!
!   ncc = size(xdata)
!   tstep = sum(xdata(2:ncc) - xdata(1:ncc-1))/(ncc-1)
!   npoints = nint(pcond%srange/tstep)
!   if (npoints > MAXNP) npoints = MAXNP  ! too coefficients for polynomial function
!   if (npoints == 0) npoints = 2
       !npoints = min(20,npoints)
      !TEST    npoints = pcond%smcond(1)
!
!  check if npoints is compatible with NDERIV
!   if (2*npoints < NDERIV) npoints = ceiling(real(npoints)/2) + 1
   !npoints = get_smooth_points(xdata,pcond,NDERIV)
   npoints = pcond%smcond(1)
!
!  calcola smoothing della derivata prima
   poldeg = min(pcond%smcond(2),npoints*2)   ! degree of polynomial
   ders = savgol_smooth(ydatan,npoints,npoints,NDERIV,poldeg)   ! 2th derivative
      !write(0,*)'DERS=',ders(1:30)
   !ders = -savgol_smooth(ydatan,npoints,npoints,1,poldeg) ! 1th derivative
   ders = 100*ders / maxval(abs(ders))
   !ders1 = savgol_smooth(ydatan,npoints,npoints,4,poldeg) 
   !ders1 = 1000*ders1/maxval(ders1)
      ders1 = 0
   !ders = 100*ders
   !ders = savgol_smooth(ydatan,npoints,npoints,1,10)  ! pol=5 prima
   !ders = averagesmooth(derivative(ydatan),4)
   !slope_threshold = 0.0
   !ders = derivative(ydatan)
   !   do i=1,size(xdata)
   !      write(71,*)i,xdata(i),ders(i)
   !   enddo
!!!!!test this FIXME
   !ders1 = savgol_smooth(ydatan,npoints,npoints,1,3)  ! pol=5 prima
   !where(ydatan > 100) ders = ders1
!
   ndat = size(xdata)     ! num. di dati
!
!  selezione i punti in cui la derivata da positiva diventa negativa
   np = 0
!corr   if (npoints/2 <= 4) then
!corr       nprange = 3
!corr   else
!corr       nprange = 0.7*npoints
!corr   endif
   nprange = floor(max(0.7*npoints,2.0)) ! FIXME: test this
   !nprange = max(0.7*npoints,2.0)
       !write(0,*)'NPRANGE=',nprange
   !nprange = 8   !test this!
   pos = -ndat     ! inizializza pos 
   do i=1,ndat-1
      !if (ders(i) > 0 .and. ders(i+1) < 0) then             !Detect zero-crossing 2th derivative
      if (ders(i) < 0 .and. ders(i+1) > 0) then             !Detect zero-crossing  1th derivative
          !if (ders(i) - ders(i+1) > slope_threshold) then   !if slope on derivative is larger than threshold
          if (ders(i+1) - ders(i) > slope_threshold) then   !if slope on derivative is larger than threshold
              ! test if (ydatan(i) > amptest) then           !if height of peak is larger than amp_threshold
              if (ydatan(i) > 0.0) then           !if height of peak is larger than amp_threshold
!
!                 create sub-group of points near peak
                  ini = max(1,i-nprange)
                  fin = min(ndat,i+nprange)
                  ngroup = fin - ini + 1
                     !if (i == 1895) then
                     !  write(70,*)i,'peak=',xdata(i),'slope=',ders(i) - ders(i+1)  !!!,ders1(i)
                     !endif
                  if (ngroup > 3) then
                      !locm = maxloc(ydatan(ini:fin))
                      !posnew = locm(1)+ini-1
                      !    peakx = xdata(posnew)
                      !    peaky = ydata(posnew)
                      !    if (posnew == ini .or. posnew == fin) cycle
!
!                     Try to fit the peak with gaussian using different range
                         !call peak_gaussian_fit(xdata,ydata,i,nprange,pkx,pky,fw,ier)
!!!!!!-test fw1
                      !do irange=nprange+5,max(3,nprange-5),-1
                      !do irange=max(3,nprange-5),nprange+5
                      !do irange=max(3,nprange-6),nprange+6
                      do irange=max(3,nprange-6),nprange+6
                         call peak_gaussian_fit(xdata,ydata,i,irange,pkx,pky,fw,ier)
                         if (.not.is_nan(fw)) exit
                      enddo
                      !if (is_nan(fw)) then  ! tested: lost some cells
                      !    fw = fwdef
                      !endif
!!!!!!-test fw2
                      !do irange=nprange,nprange+5
                      !     write(0,*)'+irange=',irange
                      !   call peak_gaussian_fit(xdata,ydata,i,irange,pkx,pky,fw,ier)
                      !   if (.not.is_nan(fw)) exit
                      !enddo
                      !if (.not.is_nan(fw)) then
                      !    do irange=nprange-1,max(3,nprange-5),-1
                      !     write(0,*)'-irange=',irange
                      !       call peak_gaussian_fit(xdata,ydata,i,irange,pkx,pky,fw,ier)
                      !       if (.not.is_nan(fw)) exit
                      !    enddo
                      !endif
!!!!!!-test fw3
                      !do irange=nprange,max(3,nprange-5),-1
                      !   write(0,*)'-irange=',irange
                      !   call peak_gaussian_fit(xdata,ydata,i,irange,pkx,pky,fw,ier)
                      !   if (.not.is_nan(fw)) exit
                      !enddo
                      !if (.not.is_nan(fw)) then
                      !    do irange=nprange+1,nprange+5
                      !       write(0,*)'+irange=',irange
                      !       call peak_gaussian_fit(xdata,ydata,i,irange,pkx,pky,fw,ier)
                      !       if (.not.is_nan(fw)) exit
                      !    enddo
                      !endif
!!!!!!-test fw4
                      !write(0,*)'0irange=',nprange
                      !call peak_gaussian_fit(xdata,ydata,i,nprange,pkx,pky,fw,ier)
                      !if (is_nan(fw)) then
                      !    do ir=1,5
                      !       irange = nprange + ir
                      !       !write(0,*)'+irange=',irange
                      !       call peak_gaussian_fit(xdata,ydata,i,irange,pkx,pky,fw,ier)
                      !       if (.not.is_nan(fw)) exit
                      !       irange = nprange - ir
                      !       if (irange < 3) cycle
                      !       !write(0,*)'-irange=',irange
                      !       call peak_gaussian_fit(xdata,ydata,i,irange,pkx,pky,fw,ier)
                      !       if (.not.is_nan(fw)) exit
                      !    enddo
                      !endif
!
                      !locm = maxloc(ydatan(ini:fin))
                      !posnew = locm(1)+ini-1
                      posnew = locate_max(ydatan, ini, fin)
                          peakx = xdata(posnew)
                          peaky = ydata(posnew)
                     !if (i == 285) then
                     ! write(70,*)i,'peak=',peakx,peaky,fw,pkx,pky
                     !endif
                        !if (posnew == ini .or. posnew == fin) cycle  
                        !if (posnew == ini .or. posnew == fin) then  !!!FIXME - evitare
                        !    pos1 = clocate(xdata,pkx)
                        !    ini = max(1,pos1-nprange)
                        !    fin = min(ndat,pos1+nprange)
                        !    locm = maxloc(ydatan(ini:fin))
                        !    posnew = locm(1)+ini-1
                        !    peakx = xdata(posnew)
                        !    peaky = ydata(posnew)
                        !    if (posnew == ini .or. posnew == fin) cycle
                        !endif
!
                      !if ((posnew - pos) < nprange) cycle
                      !if ((posnew - pos) < nprange/2) cycle
                      !if (posnew == pos) cycle     ! skip this select
                      if (is_peak_position(pk(:np),peakx)) cycle
                      !if ((posnew - pos) < nprange/2) then ! select the most intense peak
                      !if (abs(posnew - pos) < nprange) then ! select the most intense peak
                      if (np > 0) then   !!!FIXME - write function; define method overlap
                      if (abs(pk(np)%x - xdata(posnew)) < (pk(np)%fwhm + fw)*0.15) then
                           if (pk(np)%y >= peaky) cycle
                           pk(np)%sel = DELETE_PEAK
                      endif
                      endif
                      !pos = posnew
                      if (kpr) write(0,'(a,5f10.3)')'peak=',peakx,peaky,fw,ydatan(posnew)/fw,ders1(posnew)
                      outrange = peakx > xdata(fin) .or. peakx < xdata(ini)
                               if(ydatan(posnew)/fw < sogIfw) cycle   !!! megli prenderli tutti e tagliare alla fine
                      !if (is_nan(fw) .or. fw > 2.0 .or. outrange) then ! if the fw is unreasonable use the max value
                      if (fw > 2.0 .or. outrange) then ! if the fw is unreasonable use the max value
                          locm = maxloc(ydatan(ini:fin))
                          posnew = locm(1)+ini-1
                          peakx = xdata(posnew)
                          peaky = ydata(posnew)
                          !fw = 0.1
                         if (np > 0) then  !!!FIXME-improve this part
                             fw =  get_fwhm(np,pk(:np),5)
                         else
                             fw =  fwdef !!!!!0.2
                         endif
                          !corr write(0,*)'peak recomupeted=',peakx,peaky,fw,outrange
                              !cycle 
                      endif
                      !pos = 0
                  else
                      locm = maxloc(ydatan(ini:fin))
                      posnew = locm(1)+ini-1
                      if (posnew /= pos) then  ! potrebbe selezionare un punto già preso prima
                          pos = posnew
                          peakx = xdata(pos)
                          peaky = ydata(pos)
                          !fw = 0.1   !!!FIXME
                         if (np > 0) then  !!!FIXME-improve this part
                             fw =  get_fwhm(np,pk(:np),5)
                         else
                             fw =  fwdef !!!!0.2
                         endif
                      endif
                  endif
!!!FIXME - set max number of peaks and update sog (outside this sub)
!
!                 If peak measurements fails and result is NaN, skip this peak
!
                  !if (is_nan(fw) .or. is_nan(peakx) .or. is_nan(peaky) .or. scaln*peaky < amptest) then
                  !if (is_nan(fw) .or. scaln*peaky < amptest) then
                  if (is_nan(fw)) then
                      nan_peak = nan_peak + 1
                 !    write(0,*)'skip this peak:',peakx,peaky,is_nan(fw),is_nan(peakx),is_nan(peaky),scaln*peaky
                  else
                      np=np+1
                      if (nsizep < np) then
                          nsizep = nsizep + nplus
                          call resize_peaks(pk,nsizep)
                      endif          
                      pk(np) = peak_type(x=peakx,y=peaky,fwhm=fw)
                      pos = posnew
!!!!!test
                      !xp(:) = pk(np)%width_points(xdata)
                      !line(:) = straight_line(xdata(xp(1)),ydata(xp(1)),xdata(xp(2)),ydata(xp(2)))
                      !xpi(:) = line_line_intersection(line,[1.0,-pk(np)%x],ierl)
                      !write(0,'(a,2i5,4f10.3)')'xp=',xp,xdata(xp(1)),ydata(xp(1)),xdata(xp(2)),ydata(xp(2))
                      !!write(0,*)'np=',pk(np)%x,pk(np)%y,xpi,pk(np)%y - xpi(2)
                      !write(0,*)'np=',pk(np)%x,pk(np)%y,line(1)*pk(np)%x+line(2)
!!!!!test
                  endif
              endif
           endif
      endif
   enddo
  !write(0,*)'Number of peaks = ',np
!
   call resize_peaks(pk,np)
   call peak_delete_selected(pk)
          ! filtra ratio int/fwhm
          !         do i=1,size(pk)
          !            write(0,'(a,5f10.3)')'peak=',pk(i)%x,pk(i)%y,pk(i)%fwhm,pk(i)%y/pk(i)%fwhm
          !         enddo
          !call filtrasog(pk,2000.,np)
   call peak_sort(pk,ORD_BY_X)
   call get_intensities(pk,xdata,ydata)
   if (present(pklist)) call peak_copy(pklist,pk)
       call filtrasog(pk,amptest,np,code=1)
   if (np > nmaxpeak .and. nmaxpeak > 0) then
       call peak_sort(pk,-ORD_BY_Y)
       !call peak_sort(pk,ORD_BY_X)
       np = nmaxpeak   
       !scaleamp = 100 / maxval(pk%y)
       !pcond%amp = pk(np+1)%y*scaleamp/10   ! define new threshold
       pcond%amp = 100*pk(np+1)%y/maxval(pk%y)   ! define new threshold
       call resize_peaks(pk,np)
   endif
   call peak_sort(pk,ORD_BY_X)
!corr   if (nan_peak > 0) then
!corr       write(6,*)'NAN_peaks=',nan_peak,100*nan_peak/real(np)
!corr   endif
       !write(0,*)'npoints=',npoints,tstep,poldeg
      if (kpr) write(0,*)'NPOINTS=',npoints,poldeg
   !call filtrafwhm(pk,2.0)
   !call peak_print(pk,0)
!
   end subroutine findmaxd3

!----------------------------------------------------------------------------------------------

   subroutine findmaxd4(pcond,xdata,ydata,pk,np,scaln,pklist)
   USE ssmoothing
   USE genlsq
   USE math_util
   USE arrayutil
!
   implicit none
   type(peaks_condition_type), intent(inout) :: pcond
   real, dimension(:), intent(in) :: xdata, ydata
   type(peak_type), allocatable, intent(inout) :: pk(:)
   integer, intent(out)               :: np
   real, intent(in) :: scaln
   type(peak_type), allocatable, intent(inout), optional :: pklist(:)
   real                   :: slope_threshold
   real                :: amp_threshold
   integer                :: nmaxpeak
   real, dimension(size(xdata))       :: ders !   ,ders1
   real, dimension(size(xdata))       :: ydatan
   integer                            :: ndat
   integer                            :: i
   integer                            :: ini  !,fin
   integer, dimension(1)              :: locm
   integer, parameter                 :: nplus = 100
   integer                            :: nsizep
   integer                            :: pos,posnew
   integer                            :: nprange
!corr   integer                            :: ngroup
!corr   real :: peakx,peaky,fw
   real :: amptest
!corr   logical :: outrange
   integer :: npoints, poldeg
   real :: sogfw 
   real :: sogIfw
   integer :: ncc   !!!, pos1
   real :: tstep
   logical :: kpr = .false.
       integer :: nan_peak 
!corr   real :: pkx, pky   !!!!, scaleamp
!corr   integer :: ier
!corr   integer :: irange
!corr   integer :: ir
   real :: fwdef
   integer, parameter :: NDERIV=2, MAXNP=100
   !integer, dimension(2) :: xp
   !real, dimension(2) :: line,xpi
   !integer :: ierl
!
       nan_peak = 0
   slope_threshold = pcond%slope
   amp_threshold = pcond%amp
   nmaxpeak = pcond%maxpk
!
!  nsizep è un ragionevale valore iniziale di allocazione per xp e yp
   nsizep = nplus
!
!  Rialloca su npp senza salvare il suo contenuto
   !call reallocate_peaks(pk,nsizep,.false.)
   call new_peaks(pk,nsizep)
!
!  normalize ydata
   !scaln = 1000 / maxval(ydata(:))
    !sogfw = 0.4
   sogfw = pcond%srange * 3
   fwdef = pcond%srange * 1.1
   !sogfw = pcond%srange * 4
   ydatan(:) = scaln*ydata(:)
!   amptest = amp_threshold*10
   amptest = amp_threshold/100
   sogIfw = amptest / sogfw
!
   ncc = size(xdata)
   tstep = sum(xdata(2:ncc) - xdata(1:ncc-1))/(ncc-1)
   npoints = nint(pcond%srange/tstep)
   if (npoints > MAXNP) npoints = MAXNP  ! too coefficients for polynomial function
       !npoints = min(20,npoints)
!
!  check if npoints is compatible with NDERIV
   if (2*npoints < NDERIV) npoints = ceiling(real(npoints)/2) + 1
!
!  calcola smoothing della derivata prima
           !npoints = 10
        poldeg = min(pcond%smcond(2),npoints*2)   ! degree of polynomial
     !write(0,*)'npoints=',npoints,poldeg
     !write(0,*)'npoints=',pcond%srange,tstep
   ders = savgol_smooth(ydatan,npoints,npoints,NDERIV,poldeg)   ! 2th derivative
   !ders = -savgol_smooth(ydatan,npoints,npoints,1,poldeg) ! 1th derivative
   !ders = 100*ders / maxval(abs(ders))
   !write(0,*)'DERS=',maxval(ders),minval(ders)
   !ders1 = savgol_smooth(ydatan,npoints,npoints,4,poldeg) 
   !ders1 = 1000*ders1/maxval(ders1)
   !   ders1 = 0
   !ders = 100*ders
   !ders = savgol_smooth(ydatan,npoints,npoints,1,10)  ! pol=5 prima
   !ders = averagesmooth(derivative(ydatan),4)
   !slope_threshold = 0.0
   !ders = derivative(ydatan)
   !   do i=1,size(xdata)
   !      write(71,*)i,xdata(i),ders(i)
   !   enddo
!!!!!test this FIXME
   !ders1 = savgol_smooth(ydatan,npoints,npoints,1,3)  ! pol=5 prima
   !where(ydatan > 100) ders = ders1
!
!  selezione i punti in cui la derivata da positiva diventa negativa
   np = 0
!corr   if (npoints/2 <= 4) then
!corr       nprange = 3
!corr   else
!corr       nprange = 0.7*npoints
!corr   endif
   nprange = floor(max(0.7*npoints,2.0)) ! FIXME: test this
   !nprange = max(0.7*npoints,2.0)
       !write(0,*)'NPRANGE=',nprange
   !nprange = 8   !test this!

!
   ndat = size(xdata)     ! num. di dati
   np = 0
   pos = -1   ! inizializza pos
   i = 2
   outer: do
      i = i + 1
      if (i == ndat - 2) exit outer
      if (ders(i) < 0) then
          ini = i
          inter: do
            i = i + 1
            if (i == ndat - 2) exit outer
            if (ders(i) > 0) then

!
!               Posizione del massimo di dati            
!               (1) +1 punto per ogni estremo
                locm = maxloc(ydata(ini-1:i+1))
                posnew = locm(1)+ini-2
                if (posnew /= pos) then
                    pos = posnew
                        !if (abs(ders(pos)) < slope_threshold) exit inter
                        if (abs(ders(pos)) < 0.01) exit inter
                        !if (abs(ders(pos)) < 1.5) exit inter
                    np = np + 1
                    if (np > nsizep) then
                        nsizep = nsizep + nplus
                        call resize_peaks(pk,nsizep)
                        !!!!call reallocate(fw,nsizep)
                        !call reallocate(icoda,nsizep)
                    endif
!
!                   Memorizza la fwhm e la posizione del picco                
                    pk(np)%fwhm = xdata(i) - xdata(ini-1)
                    pk(np)%x = xdata(pos); pk(np)%y = ydata(pos)
                    !write(0,'(a,3f10.4,a,f10.4,a,f10.4,a,f10.4,i0)')'DERS=',ders(ini),ders(pos),ders(i),'I=',ydata(pos),'F=',pk(np)%fwhm,' t=',xdata(pos),ini-1-i
!
!                   Banale controllo per individuare picchi sulla code 
!                   (2) +1 punto per ogni estremo
                    !if(ydata(pos-1) > ydata(pos) .or. ydata(pos+1) > ydata(pos)) then
                    !   icoda(np) = -1
                    !else
                    !   icoda(np) = 1
                    !endif
                endif
!                
                exit inter
            endif
          enddo inter
      endif
   enddo outer
!
   call resize_peaks(pk,np)
   call peak_delete_selected(pk)
          ! filtra ratio int/fwhm
          !         do i=1,size(pk)
          !            write(0,'(a,5f10.3)')'peak=',pk(i)%x,pk(i)%y,pk(i)%fwhm,pk(i)%y/pk(i)%fwhm
          !         enddo
          !call filtrasog(pk,2000.,np)
   call peak_sort(pk,ORD_BY_X)
   call get_intensities(pk,xdata,ydata)
   if (present(pklist)) call peak_copy(pklist,pk)
       call filtrasog(pk,amptest,np,code=1)
   if (np > nmaxpeak .and. nmaxpeak > 0) then
       call peak_sort(pk,-ORD_BY_Y)
       !call peak_sort(pk,ORD_BY_X)
       np = nmaxpeak   
       !scaleamp = 100 / maxval(pk%y)
       !pcond%amp = pk(np+1)%y*scaleamp/10   ! define new threshold
       pcond%amp = 100*pk(np+1)%y/maxval(pk%y)   ! define new threshold
       call resize_peaks(pk,np)
   endif
   call peak_sort(pk,ORD_BY_X)
!corr   if (nan_peak > 0) then
!corr       write(6,*)'NAN_peaks=',nan_peak,100*nan_peak/real(np)
!corr   endif
       !write(0,*)'npoints=',npoints,tstep,poldeg
      if (kpr) write(0,*)'NPOINTS=',npoints,poldeg
   !call filtrafwhm(pk,2.0)
   !call peak_print(pk,0)
!
   end subroutine findmaxd4

!----------------------------------------------------------------------------------------------

   integer function locate_max(x,ini,fin)
!
!  Find max in array x 
!
   real, dimension(:), intent(in) :: x
   integer, intent(in) :: ini,fin
   integer, dimension(1) :: loc
   integer :: ini1,fin1
   ini1 = ini
   fin1 = fin
   do 
      loc = maxloc(x(ini1:fin1))
      locate_max = loc(1) + ini1 - 1
      if (ini1 == fin1) exit
!
!     Riduce range if min is equal to border
      if (locate_max == ini1) then 
          ini1 = ini1 + 1
          cycle
      endif
      if (locate_max == fin1) then
          fin1 = fin1 - 1
          cycle
      endif
      exit
   enddo
   end function locate_max

!----------------------------------------------------------------------------------------------

   subroutine get_intensities(pk,xdata,ydata)
   USE arrayutil
   USE math_util
   type(peak_type), dimension(:), allocatable, intent(inout) :: pk(:)
   real, dimension(:), intent(in) :: xdata,ydata
   integer :: i, j, np
   integer, dimension(2) :: xp
   integer, dimension(size(pk)) :: p1,p2,pmax1
   integer, dimension(1) :: lmin
   integer :: pmax
   real, dimension(2) :: line
!
   np = numpeaks(pk)
!
!  Starting estimation of range p1-p2 for each peak
   do i=1,np
      xp(:) = pk(i)%width_points(xdata)
      p1(i) = xp(1)
      p2(i) = xp(2)
      pmax1(i) = clocate(xdata,pk(i)%x)
   enddo
!
   do i=1,np
      pmax = pmax1(i)
      lmin = minloc(ydata(p1(i):pmax))
      p1(i) = lmin(1) + p1(i) - 1
!
!     Check for more intense peak in overlap at left
      do j=i-1,1,-1
         if (p1(i) > pmax1(j)) exit
          if (pk(j)%y > pk(i)%y) then
              lmin = minloc(ydata(pmax1(j):pmax))
              p1(i) = lmin(1) + pmax1(j) - 1
              exit
          endif
      enddo
      lmin = minloc(ydata(pmax:p2(i)))
      p2(i) = lmin(1) + pmax - 1
!
!     Check for more intense peak in overlap at right 
      do j=i+1,np
         if (p2(i) < pmax1(j)) exit
         if (pk(j)%y > pk(i)%y) then
             lmin = minloc(ydata(pmax:pmax1(j)))
             p2(i) = lmin(1) + pmax - 1
             exit
         endif
      enddo

      line(:) = straight_line(xdata(p1(i)),ydata(p1(i)),xdata(p2(i)),ydata(p2(i)))
      !write(0,*)'POS=',p1(i),pmax,p2(i)
      !write(0,'(a,5f10.3,3i7)')'np=',pk(i)%x,pk(i)%y,(line(1)*pk(i)%x+line(2)),pk(i)%y - (line(1)*pk(i)%x+line(2)),pk(i)%fwhm,p1(i),pmax,p2(i)
      pk(i)%y = pk(i)%y - (line(1)*pk(i)%x+line(2))   !peak intensity
      pk(i)%yint = integrate(xdata,ydata,p1(i),p2(i)) !integrated intensity
      !write(0,'(a,i4,4f10.3)')'np=',i,pk(i)%x,pk(i)%y,pk(i)%yint,abs(pk(i)%y-sum(ydata(p1(i):p2(i)))/(xp(2)-xp(1)+1))
      !write(0,'(a,i4,4f10.3)')'np=',i,pk(i)%x,pk(i)%y,pk(i)%yint,abs(pk(i)%y-(sum(ydata(p1(i):p2(i)))-ydata(pmax))/(xp(2)-xp(1)))
   enddo
!
   end subroutine get_intensities

!----------------------------------------------------------------------------------------------

   subroutine set_intensities(pk,x,y,yb)   ! unused sub
!
!  Compute intensities simply located the position of peak in the (x,y) pattern
!
   USE arrayutil
   type(peak_type), dimension(:), intent(inout) :: pk
   real, dimension(:), intent(in)               :: x,y
   real, dimension(:), intent(in), optional     :: yb
   integer                                      :: i,ipos
!
   if (present(yb)) then
       do i=1,size(pk)
          ipos = clocate(x,pk(i)%x)
          pk(i)%y = y(ipos) - yb(ipos)
       enddo
   else
       do i=1,size(pk)
          ipos = clocate(x,pk(i)%x)
          pk(i)%y = y(ipos)
       enddo
   endif
!
   end subroutine set_intensities

!----------------------------------------------------------------------------------------------

   subroutine findmaxd1(xdata,ydata,npoints,pk,np)
   USE ssmoothing
   USE pointmod
!
   implicit none
   real, dimension(:), intent(in) :: xdata, ydata
   integer, intent(in)                :: npoints
   type(point_type), allocatable, intent(inout) :: pk(:)
   integer, intent(out)               :: np
   real, dimension(size(xdata))       :: ders
   integer                            :: ndat
   integer                            :: i
   integer                            :: ini,fin
   integer, dimension(1)              :: locm
   integer, parameter                 :: nplus = 100
   integer                            :: nsizep
   integer                            :: pos,posnew
   integer                            :: nprange
!
!  nsizep � un ragionevale valore iniziale di allocazione per xp e yp
   nsizep = nplus
!
!  Rialloca su npp senza salvare il suo contenuto
   call new_points(pk,nsizep)
!
!  calcola smoothing della derivata prima
   ders = savgol_smooth(ydata,npoints,npoints,1,2)  ! pol=5 prima
!
   ndat = size(xdata)     ! num. di dati
!
!  selezione i punti in cui la derivata da positiva diventa negativa
   np = 0
   nprange = nint(npoints*0.5)
   pos = -1     ! inizializza pos 
   do i=2,ndat
      if (ders(i) < 0 .and. ders(i-1) > 0) then          
!
!         seleziona il + grande nell'intorno definito da npoints         
          ini = max(1,i-nprange)
          fin = min(ndat,i+nprange)
          locm = maxloc(ydata(ini:fin))
          posnew = locm(1)+ini-1
          if (posnew /= pos) then  ! potrebbe selezionare un punto gi� preso prima
              pos = posnew
              np=np+1
              if (nsizep < np) then
                  nsizep = nsizep + nplus
                  call resize_points(pk,nsizep)
              endif          
              pk(np) = point_type(xdata(pos), ydata(pos))
          endif
      endif
   enddo
!
   call resize_points(pk,np)
!
   end subroutine findmaxd1

!----------------------------------------------------------------------------------------------

   subroutine peak_gaussian_fit(xdata,ydata,xpos,nprange,pkx,pky,fw,ier)
   USE genlsq
   USE math_util
   real, dimension(:), intent(in) :: xdata,ydata
   integer, intent(in)            :: xpos    ! location of peak in array xdata/ydata
   integer, intent(in)            :: nprange
   real, intent(out)              :: pkx,pky,fw
   integer, intent(out)           :: ier
   integer                        :: ndat,ini,fin
   real, dimension(3)             :: pcoef
   real, dimension(2)             :: mu
   real                           :: c1,c2,c3
   !real :: sig
   !integer :: i
   !real :: xs
   !real, dimension(2) :: line
!
   ier = 0
   ndat = size(xdata)
   ini = max(1,xpos-nprange)
   fin = min(ndat,xpos+nprange)
      !line(:) = straight_line(xdata(ini),ydata(ini),xdata(fin),ydata(fin))
!
!  gaussian: y = pky * exp(-0.5*((x(i)-pkx)/sig)**2)
   !call polyfit(xdata(ini:fin),log(ydata(ini:fin)-minval(ydata(ini:fin))+1),pcoef,mu)
   call polyfit(xdata(ini:fin),log(ydata(ini:fin)),pcoef,mu)
   !call polyfit(xdata(ini:fin),log(ydata(ini:fin) - line(1)*xdata(ini:fin) + line(2) + 1),pcoef,mu)
   c1 = pcoef(1); c2 = pcoef(2); c3 = pcoef(3)
   pkx = -((mu(2)*c2/(2*c3))-mu(1)) ! = mu
   pky = exp(c1-c3*(c2/(2*c3))**2)  
   fw = mu(2)*2.35482 / sqrt(-2*c3) ! sig = mu(2)/sqrt(-2*c3); fw = sig*2*sqrt(2*ln(2))
   !sig = mu(2)/sqrt(-2*c3)
   !do i=ini,fin
   !   write(0,'(a,4f15.3)')'y=',ydata(i),pky*exp(-0.5),pky*exp(-0.5*((xdata(i)-pkx)/sig)**2)
   !enddo
   !write(0,*)xpos,'ini-fin',ini,fin,sum(xdata(ini:fin)),sum(ydata(ini:fin))
   !write(0,*)xpos,'pcoef=',pcoef,' mu=',mu
   !write(0,*)xpos,'x,y,fw=',pkx,pky,fw
   !  stop
!
   if (is_nan(fw) .or. is_nan(pkx) .or. is_nan(pky)) then
       ier = 1
   endif
!
   end subroutine peak_gaussian_fit

!----------------------------------------------------------------------------------------------

   subroutine filtrasog(pp,sog,nd,code)
!
!  Filtra dei punti applicando una soglia alla loro intensità
!
   type(peak_type), allocatable, intent(inout) :: pp(:)
   real, intent(in)                            :: sog
   integer, intent(out)                        :: nd
   integer, intent(in)                         :: code
   type(peak_type), dimension(size(pp))        :: ppv
   real                                        :: maxy
   integer                                     :: i
   integer                                     :: ns
!
   ns = numpeaks(pp)
   if (ns == 0) return
   select case (code)
     case (1) 
       maxy = maxval(pp%y)
       nd = 0
       do i=1,ns
          if (pp(i)%y/maxy >= sog) then
          !if (pp(i)%y/pp(i)%fwhm >= sog) then
              nd = nd + 1
              ppv(nd) = pp(i)
          endif
       enddo

     case (2) 
       maxy = maxval(pp%y/pp%fwhm)
       nd = 0
       do i=1,ns
          if (pp(i)%y/pp(i)%fwhm/maxy >= sog) then
              nd = nd + 1
              ppv(nd) = pp(i)
          endif
       enddo

   end select
!
   if (nd /= ns) then
       deallocate(pp)
       if (nd > 0) allocate(pp(nd),source=ppv(:nd))
   endif
!
   end subroutine filtrasog

!----------------------------------------------------------------------------------------------

   real function get_fwhm(ipos,peak,np) result(fw)
   integer, intent(in)                       :: ipos
   type(peak_type), dimension(:), intent(in) :: peak
   integer, intent(in)                       :: np
   integer :: n1,n2
   integer :: npeak
!
   npeak = size(peak)
   if (npeak == 1) then
       fw = 0.2
       return
   endif
   if (ipos == 1) then
       n1 = 2
       n2 = min(ipos + np,npeak)
       if (n1 == n2) then
           fw = peak(n1)%fwhm
       else
           fw = sum(peak(n1:n2)%fwhm) / (n2-n1)
       endif
       return
   endif
   if (ipos == npeak) then
       n1 = max(ipos - np,1)
       n2 = npeak-1
       if (n1 == n2) then
           fw = peak(n1)%fwhm
       else
           fw = sum(peak(n1:n2)%fwhm) / (n2-n1)
       endif
       return
   endif
!
   n1 = max(ipos - np,1)
   n2 = min(ipos + np,npeak)
   fw = sum([peak(n1:ipos-1)%fwhm,peak(ipos+1:n2)%fwhm]) / (n2-n1)
!
   end function get_fwhm

!----------------------------------------------------------------------------------------------

   subroutine filtrafwhm(pp,fact)
   type(peak_type), allocatable, intent(inout) :: pp(:)
   real, intent(in) :: fact
   integer :: npeak,i,nstep
   real :: fwmed
!!
   npeak = numpeaks(pp)
   if (npeak == 0) return
   nstep = 1
   do i=1,npeak
      fwmed = get_fwhm(i,pp,nstep)  
          write(0,*)'FW :       ',i,fwmed,pp(i)%fwhm,pp(i)%x
      if (pp(i)%fwhm > fact*fwmed) then
          write(0,*)'FW ANOMALA:',i,fwmed,pp(i)%fwhm,pp(i)%x
      endif
   enddo

   end subroutine filtrafwhm

!----------------------------------------------------------------------------------------------

   subroutine save_peaks_bin(unitbin,peaks)
!
!  write peaks on binary file
!
   integer, intent(in)                                    :: unitbin
   type(peak_type), dimension(:), allocatable, intent(in) :: peaks
   integer                                                :: npeak
   npeak = numpeaks(peaks)
   write(unitbin)npeak
   if (npeak > 0) then
       write(unitbin)peaks(:)
   endif
   end subroutine save_peaks_bin

!----------------------------------------------------------------------------------------------

   subroutine read_peaks_bin(unitbin,peaks,err)
!
!  Read peaks from binary file
!
   USE errormod
   integer, intent(in)                                       :: unitbin
   type(peak_type), dimension(:), allocatable, intent(inout) :: peaks
   integer                                                   :: npeak
   type(error_type), intent(out)                             :: err
   integer                                                   :: ier
!
   read(unitbin, iostat=ier, err=10) npeak
   if (npeak > 0) then
       call resize_peaks(peaks,npeak,.false.)
       read(unitbin, iostat=ier, err=10)peaks
   endif
10 continue
   if (ier /= 0) then
       call resize_peaks(peaks,0)
       call err%set('Error on reading peak positions')
   endif
   end subroutine read_peaks_bin

!----------------------------------------------------------------------------------------------

   subroutine check_pkcond(pkcond,thmin,thmax)
!
!  Check peak search conditions
!
   type(peaks_condition_type), intent(inout) :: pkcond
   real, intent(in)                          :: thmin,thmax
!
   if (pkcond%minp > 0 .and. pkcond%minp < thmin) pkcond%minp = thmin
   if (pkcond%maxp > 0 .and. pkcond%maxp > thmax) pkcond%maxp = thmax
   if (pkcond%maxp > 0 .and. pkcond%minp > 0) then
       if (pkcond%minp > pkcond%maxp) then
           pkcond%minp = -1
           pkcond%maxp = -1
       endif
   endif
   if (pkcond%amp > 100) pkcond%amp = DEF_AMP
!
   end subroutine check_pkcond

!----------------------------------------------------------------------------------------------

   subroutine resize_peaks(vetr,n,savevet)
!
!  Rialloca ad n un array di tipo peak_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(peak_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n
   logical, optional, intent(in)               :: savevet
   logical                                     :: savev
   integer                                     :: nv
   type(peak_type), allocatable                :: vsav(:)
   integer                                     :: nsav
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
!          nsav contiene qual è la porzione di vetr da salvare
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
           deallocate(vetr)
           allocate(vetr(n))
       endif
   endif
!   
   end subroutine resize_peaks

!----------------------------------------------------------------------------------------------

   subroutine new_peaks(vetr,n)
!
!  Create new peaks
!
   type(peak_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n

   if (n < 0) return
   if (numpeaks(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_peaks

!----------------------------------------------------------------------------------------------------

   subroutine clear_peaks(vetr)
!
!  Delete all peaks
!
   type(peak_type), allocatable, intent(inout) :: vetr(:)

   if (allocated(vetr)) deallocate(vetr)

   end subroutine clear_peaks

!----------------------------------------------------------------------------------------------

   subroutine findmaxd2(pcond,xdata,ydata,pk,np,scaln)
   USE ssmoothing
   USE genlsq
   USE math_util
!
   implicit none
   type(peaks_condition_type), intent(inout) :: pcond
   real, dimension(:), intent(in) :: xdata, ydata
!corr   integer, intent(in)                :: npoints
   type(peak_type), allocatable, intent(inout) :: pk(:)
   integer, intent(out)               :: np
   real, intent(in) :: scaln
   real                   :: slope_threshold
   real                :: amp_threshold
   integer                :: nmaxpeak
   real, dimension(size(xdata))       :: ders    ,ders1
   real, dimension(size(xdata))       :: ydatan
   integer                            :: ndat
   integer                            :: i
   integer                            :: ini,fin
   integer, dimension(1)              :: locm
   integer, parameter                 :: nplus = 100
   integer                            :: nsizep
   integer                            :: pos,posnew
   integer                            :: nprange
   integer                            :: ngroup
   !real :: c1,c2,c3,peakx,peaky,fw
   real :: peakx,peaky,fw
   real :: amptest
   !real, dimension(2) :: mu
   logical :: outrange
   !real, dimension(3) :: pcoef
   integer :: npoints, poldeg
   real :: sogfw 
   real :: sogIfw
   integer :: ncc
   real :: tstep
   logical :: kpr = .false.
       integer :: nan_peak 
   real :: pkx, pky
   integer :: ier
   integer :: irange
   !integer, dimension(2) :: xp
   !real, dimension(2) :: line,xpi
   !integer :: ierl
!
       nan_peak = 0
   slope_threshold = pcond%slope
   amp_threshold = pcond%amp
   nmaxpeak = pcond%maxpk
!
!  nsizep è un ragionevale valore iniziale di allocazione per xp e yp
   nsizep = nplus
!
!  Rialloca su npp senza salvare il suo contenuto
   !call reallocate_peaks(pk,nsizep,.false.)
   call new_peaks(pk,nsizep)
!
!  normalize ydata
   !scaln = 1000 / maxval(ydata(:))
    !sogfw = 0.4
   sogfw = pcond%srange * 3
   !sogfw = pcond%srange * 4
   ydatan(:) = scaln*ydata(:)
   amptest = amp_threshold*10
   sogIfw = amptest / sogfw
!
   ncc = size(xdata)
   tstep = sum(xdata(2:ncc) - xdata(1:ncc-1))/(ncc-1)
   npoints = nint(pcond%srange/tstep)
       !npoints = min(20,npoints)
!
!  calcola smoothing della derivata prima
        poldeg = min(pcond%smcond(2),npoints*2)   ! degree of polynomial
       write(0,*)'npoints=',npoints,tstep,poldeg
      if (kpr) write(0,*)'NPOINTS=',npoints,poldeg
   !ders = savgol_smooth(ydatan,npoints,npoints,3,poldeg) 
   ders = -savgol_smooth(ydatan,npoints,npoints,1,poldeg) 
   ders = 100*ders / maxval(abs(ders))
   !ders1 = savgol_smooth(ydatan,npoints,npoints,4,poldeg) 
   !ders1 = 1000*ders1/maxval(ders1)
      ders1 = 0
   !ders = 100*ders
   !ders = savgol_smooth(ydatan,npoints,npoints,1,10)  ! pol=5 prima
   !ders = averagesmooth(derivative(ydatan),4)
   !slope_threshold = 0.0
   !ders = derivative(ydatan)
   !   do i=1,size(xdata)
   !      write(71,*)i,xdata(i),ders(i)
   !   enddo
!!!!!test this FIXME
   !ders1 = savgol_smooth(ydatan,npoints,npoints,1,3)  ! pol=5 prima
   !where(ydatan > 100) ders = ders1
!
   ndat = size(xdata)     ! num. di dati
!
!  selezione i punti in cui la derivata da positiva diventa negativa
   np = 0
   !nprange = 3  
   !nprange = min(5,npoints/2)   !!test this
   if (npoints/2 <= 4) then
       nprange = 3
   else
       nprange = nint(0.7*npoints)
   endif
       write(0,*)'NPRANGE=',nprange
   !nprange = 8   !test this!
   pos = -1     ! inizializza pos 
   do i=1,ndat-1
      !if (ders(i) > 0 .and. ders(i+1) < 0) then             !Detect zero-crossing
      if (ders(i) < 0 .and. ders(i+1) > 0) then             !Detect zero-crossing
                      !write(0,*)'peak=',xdata(i),'slope=',ders(i) - ders(i+1),ders1(i)
          !if (ders(i) - ders(i+1) > slope_threshold) then   !if slope on derivative is larger than threshold
          if (ders(i+1) - ders(i) > slope_threshold) then   !if slope on derivative is larger than threshold
              if (ydatan(i) > amptest) then           !if height of peak is larger than amp_threshold
!
!                 create sub-group of points near peak
                  ini = max(1,i-nprange)
                  fin = min(ndat,i+nprange)
                  ngroup = fin - ini + 1
                  if (ngroup > 3) then
!
!                     Try to fit the peak with gaussian using different range
                         !call peak_gaussian_fit(xdata,ydata,i,nprange,pkx,pky,fw,ier)
                         !!!FIXME - improve this loop
                      !do irange=max(3,nprange-5), nprange+5
                      do irange=nprange+5,max(3,nprange-5),-1
                         call peak_gaussian_fit(xdata,ydata,i,irange,pkx,pky,fw,ier)
                         if (.not.is_nan(fw)) exit
                      enddo
!
                      locm = maxloc(ydatan(ini:fin))
                      posnew = locm(1)+ini-1
                          peakx = xdata(posnew)
                          peaky = ydata(posnew)
                        if (posnew == ini .or. posnew == fin) cycle
!
                      if ((posnew - pos) < nprange) cycle
                      pos = posnew
                      if (kpr) write(0,'(a,5f10.3)')'peak=',peakx,peaky,fw,ydatan(posnew)/fw,ders1(posnew)
                      outrange = peakx > xdata(fin) .or. peakx < xdata(ini)
                               !if(ydatan(posnew)/fw < sogIfw) cycle   !!! megli prenderli tutti e tagliare alla fine
                      !if (is_nan(fw) .or. fw > 2.0 .or. outrange) then ! if the fw is unreasonable use the max value
                      if (fw > 2.0 .or. outrange) then ! if the fw is unreasonable use the max value
                          locm = maxloc(ydatan(ini:fin))
                          posnew = locm(1)+ini-1
                          peakx = xdata(posnew)
                          peaky = ydata(posnew)
                          fw = 0.1
                          write(0,*)'peak recomupeted=',peakx,peaky,fw,outrange
                              !cycle 
                      endif
                      !pos = 0
                  else
                      locm = maxloc(ydatan(ini:fin))
                      posnew = locm(1)+ini-1
                      if (posnew /= pos) then  ! potrebbe selezionare un punto già preso prima
                          pos = posnew
                          peakx = xdata(pos)
                          peaky = ydata(pos)
                          fw = 0.1   !!!FIXME
                      endif
                  endif
!!!FIXME - set max number of peaks and update sog (outside this sub)
!
!                 If peak measurements fails and result is NaN, skip this peak
!
                  !if (is_nan(fw) .or. is_nan(peakx) .or. is_nan(peaky) .or. scaln*peaky < amptest) then
                  if (is_nan(fw) .or. scaln*peaky < amptest) then
                      nan_peak = nan_peak + 1
                     write(0,*)'skip this peak:',peakx,peaky,is_nan(fw),is_nan(peakx),is_nan(peaky),scaln*peaky
                  else
                      np=np+1
                      if (nsizep < np) then
                          nsizep = nsizep + nplus
                          call resize_peaks(pk,nsizep)
                      endif          
                      pk(np) = peak_type(x=peakx,y=peaky,fwhm=fw)
!!!!!test
                      !xp(:) = pk(np)%width_points(xdata)
                      !line(:) = straight_line(xdata(xp(1)),ydata(xp(1)),xdata(xp(2)),ydata(xp(2)))
                      !xpi(:) = line_line_intersection(line,[1.0,-pk(np)%x],ierl)
                      !write(0,'(a,2i5,4f10.3)')'xp=',xp,xdata(xp(1)),ydata(xp(1)),xdata(xp(2)),ydata(xp(2))
                      !!write(0,*)'np=',pk(np)%x,pk(np)%y,xpi,pk(np)%y - xpi(2)
                      !write(0,*)'np=',pk(np)%x,pk(np)%y,line(1)*pk(np)%x+line(2)
!!!!!test
                  endif
              endif
          endif
      endif
   enddo
  !write(0,*)'Number of peaks = ',np
!
   call resize_peaks(pk,np)
          ! filtra ratio int/fwhm
          !         do i=1,size(pk)
          !            write(0,'(a,5f10.3)')'peak=',pk(i)%x,pk(i)%y,pk(i)%fwhm,pk(i)%y/pk(i)%fwhm
          !         enddo
          !call filtrasog(pk,2000.,np)
   if (np > nmaxpeak .and. nmaxpeak > 0) then
       call peak_sort(pk,-ORD_BY_Y)
       np = nmaxpeak   
       pcond%amp = pk(np+1)%y*scaln/10   ! define new threshold
       call resize_peaks(pk,np)
   endif
   call peak_sort(pk,ORD_BY_X)
   if (nan_peak > 0) then
       write(6,*)'NAN_peaks=',nan_peak,100*nan_peak/real(np)
   endif
   !call filtrafwhm(pk,2.0)
   !call peak_print(pk,0)
!
   end subroutine findmaxd2

!----------------------------------------------------------------------------------------------

   subroutine peak_filterd(peak,dmin)
!
!  Filtra in a sorted list for d <= dmin
!
   type(peak_type), dimension(:), allocatable, intent(inout) :: peak
   real, intent(in)                                          :: dmin
   integer                                                   :: i
!
   do i=2,numpeaks(peak)
      if (abs(peak(i)%x - peak(i-1)%x) <= dmin) then
          peak(i)%sel = DELETE_PEAK
      endif
   enddo
   call peak_delete_selected(peak)
!
   end subroutine peak_filterd

!----------------------------------------------------------------------------------------------

!corr   real function init_peak_range(x,y,alambda)  result(srange)
!corr!
!corr!  Compute reasonable value for srange from value SRANGE_CU for Cu radiation
!corr!
!corr   USE counts, only: delta_from_lambda
!corr   real, dimension(:), intent(in) :: x,y
!corr   real, intent(in)               :: alambda
!corr   integer, dimension(1)          :: loc
!corr   real                           :: pkx,pky,fwp
!corr   integer                        :: ierf
!corr   real, parameter                :: SRANGE_CU = 0.15 ! 2theta-range for peak_search at 1.54056
!corr!
!corr   loc = maxloc(y)
!corr   call peak_gaussian_fit(x,y,loc(1),4,pkx,pky,fwp,ierf)
!corr   if(ierf == 0) then
!corr      srange = fwp * 0.9
!corr      !pkcond%srange = fwp * 1.7
!corr   else
!corr      srange = delta_from_lambda(SRANGE_CU,1.54056,alambda,4.0)
!corr   endif
!corr!
!corr   end function init_peak_range

!----------------------------------------------------------------------------------------------

   real function init_peak_range(x,y,wave)  result(srange)
!
!  Compute reasonable value for srange from value SRANGE_CU for Cu radiation
!
   USE counts, only: delta_from_lambda
   real, dimension(:), intent(in)             :: x,y
   real, intent(in)                           :: wave
!corr   integer, dimension(1)                      :: loc
!corr   real                                       :: pkx,pky,fwp
   real                                       :: fwp
   real, parameter                            :: SRANGE_CU = 0.15 ! 2theta-range for peak_search at 1.54056
   type(peak_type), dimension(:), allocatable :: pk
   type(peaks_condition_type)                 :: pcond
   integer                                    :: np
   integer, parameter                         :: MAXFW_CALC = 3
!
   pcond%srange = delta_from_lambda(SRANGE_CU,1.54056,wave,4.0)
   call peak_create(x,y,pk,pcond,wave)
   np = numpeaks(pk)
   if(np > 0) then
      call peak_sort(pk,-ORD_BY_Y)
      !call peak_print(pk,0)
!
!     compute FWHM as average on MAXFW_CALC peaks
      fwp = sum(pk(:min(MAXFW_CALC,np))%fwhm) / min(MAXFW_CALC,np)
      !fwp = sum(pk(:np)%fwhm) / np
      srange = fwp * 0.9
      !srange = fwp * 0.5
      !srange = fwp * 0.6
   else
      srange = pcond%srange
   endif
!
   end function init_peak_range

END MODULE peak_util
