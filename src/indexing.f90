   subroutine SavePeaks(filnam, tipo)
   USE peak_mod, only: pkind,peak_print,numpeaks
   USE filereading
   !USE indexing
   !USE General, only:StructureName, alambda
   implicit none
   character(len=*), intent(in) :: filnam
   integer, intent(in)          :: tipo
!
   if (numpeaks(pkind) == 0) return
!
   select case (tipo)
      case (1)    ! 2theta values
        call write_column(trim(filnam),xcol1=pkind%getx(),metad='#2theta values')

      case (2)    ! d values
        call write_column(trim(filnam), xcol1=pkind%getd(),metad='#d values')

      !case (3)    ! input file for dicvol
      !  call export_dicvol(trim(filnam), pkind%getx(),StructureName, alambda)

      !case (4)    ! input file for mcmaille
      !  call export_mcmaille(trim(filnam), pkind%getx(), pkind%gety(),StructureName, alambda)

      case (5)    ! export list of peaks in txt file with fwhm and intensities
        call peak_print(pkind,filename=trim(filnam))

   end select
!
   end subroutine SavePeaks

!-------------------------------------------------------------------------------------------------------

   subroutine LoadPeaks(filnam, tipo, ier)
   USE peak_mod, only: pkind
   USE peak_util
   USE errormod
   USE General, only:lo
   USE view
   USE filereading
   !USE Conteggi
   USE arrayutil
   USE Counts
   !USE patternref, only:thmin,thmax
   USE Molcom, only:jscreen
!FIX LATER   USE messagemod
   USE fileutil
   !USE proginterface
   USE variables, only: dataset
   implicit none
   interface
      subroutine update_peak_list() bind(C,name="update_peak_list")
      end subroutine update_peak_list
   end interface
   character(len=*), intent(in)       :: filnam
   integer, intent(in)                :: tipo
   integer, intent(out)               :: ier
   type(error_type)                   :: error
   real, dimension(:), allocatable    :: xcol
   integer                            :: ncol
   integer                            :: i
   real                               :: xpeak0
   integer                            :: npeaks
   integer :: ierfw
   real :: fwp,pkx,pky
!
   ier = 0
!
!  Leggi d dei picchi dal file esterno
   call read_column(trim(filnam),xcol,ncol,error)
   if (error%signal) then
       call error%print()
       ier = 1
       return
   endif
!
   if (ncol > 0) then
       call new_peaks(pkind,ncol)
!
!      Estrai i 2theta e controlla la loro validita'
       npeaks = 0
       select case (tipo)
          case (1)   ! 2theta in input
            do i=1,ncol
               if (xcol(i) > dataset(1)%tmin .and. xcol(i) < dataset(1)%tmax) then
                   npeaks = npeaks + 1
                   call pkind(npeaks)%setxd(xcol(i),dataset(1)%wave(1))
               endif
            enddo

          case (2)   ! d values in input
            do i=1,ncol
               xpeak0 = thvalue(xcol(i),dataset(1)%wave(1))
               if (xpeak0 == -999) cycle ! picco non valido
               if (xpeak0 > dataset(1)%tmin .and. xpeak0 < dataset(1)%tmax) then
                   npeaks = npeaks + 1
                   call pkind(npeaks)%setx(xpeak0)
                   call pkind(npeaks)%setd(xcol(i))
               endif
            enddo

       end select
       call resize_peaks(pkind,npeaks)
       call peak_sort(pkind,ORD_BY_X)
       call peak_filterd(pkind,epsilon(1.0)) ! remove duplicate peaks
       npeaks = numpeaks(pkind)
!
       if (npeaks > 0) then
!
!          Set background is not available
           if (.not.allocated(dataset(1)%yb)) then
               !call fillcounts()
               !call back_for_peaksearch(.true.)
               call dataset(1)%make_background()
               style(STYLE_BACK)%vis = 1
               style(STYLE_BACKP)%vis = 1
           endif
!
!          Set y for peaks
           do i=1,npeaks
              !pkind(i)%y = peak_intensity(pkind(i)%x,theta_int(:,1),theta_int(:,2))
              call pkind(i)%get_int(dataset(1)%x,dataset(1)%y,dataset(1)%yb)
              call peak_gaussian_fit(dataset(1)%x,dataset(1)%y,clocate(dataset(1)%x,pkind(i)%getx()),4,pkx,pky,fwp,ierfw)
              if (ierfw /= 0) then
                  pkind(i)%fwhm = 0.2   !FIXME - NOT CORRECT - recompute from Cu to current wave
              else
                  pkind(i)%fwhm = fwp
              endif
              call pkind(i)%integrated_intensity(dataset(1)%x,dataset(1)%y,dataset(1)%yb)
           enddo
           !call peak_sort(pkind,ORD_BY_X)
!
           call peak_print(pkind,kpr=lo,title='Peak positions from external file '//trim(file_get_name(filnam))//':')
           if (jscreen > 0) then
!FIX LATER               call update_peak_list()
               call vedinew(5,1,dataset(1)%npoints())  ! disegna
!FIX LATER               call clear_messages(ipos=3)
!FIX LATER               call write_message('Number of peaks: ',inum=npeaks,pos=2)
           endif
       else
           call error%set('No valid peaks found!')
       endif
   else
       call error%set('No peaks found!')
   endif
   if (error%signal) then
       ier = 2
       call error%print()
   endif
!
   return
   end
