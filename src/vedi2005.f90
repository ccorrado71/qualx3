MODULE plot2

use :: iso_c_binding

interface

   subroutine init_plot2(xLabel, yLabel) bind(c)
   use, intrinsic :: iso_c_binding, only: c_char
   character(kind=c_char), dimension(*) :: xLabel, yLabel
   end subroutine init_plot2

   subroutine enable_rescale(rescale) bind(C, name="enable_rescale")
   use, intrinsic :: iso_c_binding, only: c_int
   integer(c_int), intent(in), value :: rescale
   end subroutine enable_rescale

   subroutine add_plot(x,y,num,ktype,visible,wave,pname) bind(C,name="add_plot")
   use, intrinsic :: iso_c_binding, only: c_int, c_float, c_char
   real(c_float), dimension(*)          :: x,y
   integer(c_int), value                :: num,ktype,visible
   real(c_float), value                 :: wave
   character(kind=c_char), dimension(*) :: pname
   end subroutine add_plot

   subroutine add_plot2(x,num,ktype,visible,wave) bind(C,name="add_plot2")
   use, intrinsic :: iso_c_binding, only: c_int, c_float
   real(c_float), dimension(*) :: x
   integer(c_int), value       :: num,ktype,visible
   real(c_float), value        :: wave
   end subroutine add_plot2

   subroutine add_reflections(x,h,k,l,num,visible,wave) bind(C,name="add_reflections")
   use, intrinsic :: iso_c_binding, only: c_int, c_float
   real(c_float), dimension(*)  :: x
   integer(c_int), dimension(*) :: h,k,l
   integer(c_int), value        :: num,visible
   real(c_float), value         :: wave
   end subroutine add_reflections

   subroutine draw_graphic() bind(C,name="draw_graphic")
   end subroutine draw_graphic

   subroutine set_graphic_area() bind(C,name="set_graphic_area")
   end subroutine set_graphic_area
end interface

END MODULE plot2

MODULE view

   use plotstyle

   implicit none

   integer, public :: xzleft,xzright

   integer, public                                     :: pd_action 
   integer, public                                     :: pinioss_sav,pfinoss_sav
   integer, public                                     :: pinicalc_sav,pfincalc_sav
   integer, public                                     :: iAct_sav = 1

   integer, dimension(:), allocatable, public :: idph

   enum, bind(c)
     enumerator :: Observed,Calculated,BBackground,Background_Points,         &
                   Difference,Cumulative,Reflections,Peaks,Smoothing,         &
                   Unindexed_Peaks,Systematic_Absences,Selected_Reflections,  &
                   Selected_Peaks,Intervals,Profile_Curves
   endenum

   CONTAINS

     subroutine vedinew(iAct,pinioss,pfinoss,pinicalc,pfincalc,lsetstyle,rescale)
     use, intrinsic :: iso_c_binding, only: c_int
     !USE PatternRef, only: jjj
     !USE Conteggi, only : theta_int,back_int
     !USE CommonExpo, only : backp,scala,nback
     !USE Riflessi, only : TThet
     !USE General, only : nrefl 
     USE plotstyle
     USE variables, only: dataset
     USE datasetmod, only: ndataset
     USE arrayutil
     use plot2, only: enable_rescale
!
     integer, intent(in), optional                 :: iAct
     integer, intent(in), optional                 :: pinioss,pfinoss   !limiti per l'osservato
     integer, intent(in), optional                 :: pinicalc,pfincalc !limiti per il calcolato
     logical, intent(in), optional                 :: lsetstyle         !se vero setta lo stile all'interno
     integer, intent(in), optional                 :: rescale
     integer                                       :: pinicalc1,pfincalc1
     !real, dimension(:,:), allocatable             :: backpur
     integer                                       :: i
     integer, dimension(:), allocatable :: inid,ifid
!
     if (present(iAct)) then
         pd_action = iAct
         if (present(lsetstyle)) then
             if (lsetstyle) call set_plot_style(pd_action)
         else
             call set_plot_style(pd_action)
         endif
     endif
     if (present(pinioss))then         
         xzleft = pinioss
         xzright = pfinoss
     endif
     if (present(pinicalc)) then
         pinicalc1 = pinicalc
         pfincalc1 = pfincalc
     else
         pinicalc1 = xzleft
         pfincalc1 = xzright
     endif
     if (present(rescale)) then
         call enable_rescale(rescale)
     else
         call enable_rescale(1)
     endif
!     
!c    select case (pd_action)
!c       case (1)
!c         if (allocated(dataset(1)%yb) .and. dataset(1)%npoints_back() > 0) then
!c             call dataset(1)%get_points(backpur)
!c             call plottanew(xzleft,xzright,pinicalc1,pfincalc1,theta_int(:,7),theta_int(:,2),tthr=TThet,   &
!c                            ybac=dataset(1)%yb(pinicalc1:pfincalc1),bacp=backpur)
!c         else
!c             call plottanew(xzleft,xzright,pinicalc1,pfincalc1,theta_int(:,7),theta_int(:,2),tthr=TThet)
!c         endif
!c
!c       case (2)
!c         !call plottanew(xzleft,xzright,pinicalc1,pfincalc1,theta_int(:,7),theta_int(:,2),tthr=TThet(:nrefl))
!c         call plottanew(xzleft,xzright,pinicalc1,pfincalc1,theta_int(:,7),theta_int(:,2),tthr=TThet)
!c
!c       case (3)
!c         call plottanew(xzleft,xzright,pinicalc1,pfincalc1,theta_int(:,7),theta_int(:,2),tthr=TThet,  &
!c         ybac=back_int(pinicalc1:pfincalc1,1),bacp=backp(:nback,:))
!c
!c       case (4)
!c         call plottanew(xzleft,xzright,pinicalc1,pfincalc1,theta_int(:,7),theta_int(:,2),     & 
!c         scala(jjj)*theta_int(pinicalc1:pfincalc1,3),TThet,back_int(pinicalc1:pfincalc1,1), &
!c         bacp=backp(:nback,:))
!c
!c       case (5)
!c         call dataset(1)%get_points(backpur)
!c         if (nrefl == 0) then
!c             call plottanew(xzleft,xzright,pinicalc1,pfincalc1,dataset(1)%x0,dataset(1)%y,ybac=dataset(1)%yb(pinicalc1:pfincalc1), &
!c             bacp=backpur)
!c         else
!c             call plottanew(xzleft,xzright,pinicalc1,pfincalc1,dataset(1)%x0,dataset(1)%y,ybac=dataset(1)%yb(pinicalc1:pfincalc1), &
!c             tthr=TThet,bacp=backpur)
!c         endif
!c
!c       case (6)
!c         if (nrefl == 0) then
!c             call plottanew(xzleft,xzright,pinicalc1,pfincalc1,theta_int(:,7),theta_int(:,2),      &
!c             scala(1)*theta_int(pinicalc1:pfincalc1,3),ybac=back_int(pinicalc1:pfincalc1,1),bacp=backp(:nback,:))
!c         else
!c             call plottanew(xzleft,xzright,pinicalc1,pfincalc1,theta_int(:,7),theta_int(:,2),      &
!c             scala(1)*theta_int(pinicalc1:pfincalc1,3),ybac=back_int(pinicalc1:pfincalc1,1),tthr=TThet,  & 
!c             bacp=backp(:nback,:))
!c         endif
!c  
!c       case (7)
!c         call plottanew(xzleft,xzright,pinicalc1,pfincalc1,theta_int(:,7),theta_int(:,2),      &
!c         scala(1)*theta_int(pinicalc1:pfincalc1,3),ybac=back_int(pinicalc1:pfincalc1,1),tthr=TThet,  &   
!c         bacp=backp(:nback,:))
!c
!c       case (8)
         allocate(inid(ndataset(dataset)))
         allocate(ifid(ndataset(dataset)))
         do i=1,ndataset(dataset)
            inid(i) = 1
            ifid(i) = size(dataset(i)%x)
         enddo
         call plottanew2(inid,ifid)   !!!!,pinicalc1,pfincalc1)

!c     end select
!
!    salva limiti per redraw
     pinioss_sav = xzleft
     pfinoss_sav = xzright
     pinicalc_sav = pinicalc1
     pfincalc_sav = pfincalc1
     iAct_sav = pd_action
!
     end subroutine vedinew

  !-----------------------------------------------------------------------------------------------

!c     subroutine plottanew(inic,ific,inic1,ific1,ttet,yoss,ycal,tthr,ybac,bacp)
!c     USE peak_mod
!c     USE plotstyle
!c     USE variables, only: cryst,dataset
!c     USE General, only: wavel
!c     USE arrayutil, only: clocate
!c     USE CommonExpo, only : nnt,ncoun1,ncoun2
!c     USE fileutil
!c     USE plot2
!c     USE strutil, only: toCString
!c     USE PatternRef, only: kprofile,kprofile_type,PROFILE_EXTRA
!c!
!c     integer, intent(in)                                   :: inic  ! conteggio iniziale per l'osservato
!c     integer, intent(in)                                   :: ific  ! conteggio finale per l'osservato
!c     integer, intent(in)                                   :: inic1 ! conteggio iniziale per background e calcolato
!c     integer, intent(in)                                   :: ific1 ! conteggio finale per background e calcolato
!c     real, dimension(:), intent(in)                        :: ttet  ! 2theta del conteggio
!c     real, dimension(:), intent(in)                        :: yoss  ! conteggio osservato
!c     real, dimension(:), intent(in), optional              :: ycal  ! conteggio calcolato
!c     real, dimension(:), intent(in), allocatable, optional :: tthr  ! 2theta del riflesso 
!c     real, dimension(:), intent(in), optional              :: ybac  ! background
!c     real, dimension(:,:), intent(in), optional            :: bacp  ! punti di background
!c     real, dimension(ific1-inic1+1)                        :: diffc 
!c     real, dimension(ific1-inic1+1)                        :: ycalb,ydiff
!c     real                                                  :: maxv
!c     integer                                               :: i,j
!c     real                                                  :: maxdiff,mindiff
!c     real                                                  :: adiff
!c     integer                                               :: ncalcc,startp,endp,npk
!c     logical                                               :: drawref, drawcalc, drawdiff
!c     real, dimension(:), allocatable                       :: intpeak
!c     integer                                               :: ninf, nsup
!c     real, allocatable, dimension(:)                       :: intervalx
!c     integer                                               :: npinter
!c!
!c     call set_graphic_area()
!c     ncalcc = ific1 - inic1 + 1
!c!
!c!    Calcola il massimo e minimo valore lungo y
!c     drawcalc = style(STYLE_CALC)%vis == 1 .and. present(ycal) .and. present(ybac)
!c     if (drawcalc) then
!c         ycalb = ycal(:ncalcc)+ybac(:ncalcc)
!c     endif
!c     drawdiff = style(STYLE_DIFF)%vis == 1 .and. present(ycal) .and. present(ybac)
!c     if (drawdiff) then
!c         ydiff = yoss(inic1:ific1)-ycal(:ncalcc)-ybac(:ncalcc)
!c     endif
!c!
!c     drawref = .false.
!c     if (cryst(1)%style%vis == 1 .and. present(tthr)) then
!c         if (allocated(tthr)) then
!c             drawref = .true.
!c         endif
!c     endif
!c!
!c!    Disegna il profilo osservato
!c     if (dataset(1)%style%vis == 1) then
!c         call add_plot(ttet(inic:ific),yoss(inic:ific),ific-inic+1,Observed,1,  &
!c                 dataset(1)%wave(1),toCString(file_basename(dataset(1)%fname)))
!c     endif
!c!
!c!    Disegna il profilo calcolato
!c     if (drawcalc) then
!c         call add_plot(ttet(inic1:ific1),ycalb,ific1-inic1+1,Calculated,1,wavel,c_null_char)
!c     endif
!c!
!c!    Disegna il background
!c     if (style(STYLE_BACK)%vis == 1 .and. present(ybac)) then
!c         call add_plot(ttet(inic1:ific1),ybac(:ncalcc),ific1-inic1+1,BBackground,1,wavel,c_null_char)
!c         if (style(STYLE_BACKP)%vis == 1 .and. present(bacp)) then    ! disegna i punti di background
!c             call add_plot(bacp(:,1),bacp(:,2),size(bacp,1),Background_Points,1,wavel,c_null_char)
!c         endif
!c     endif
!c!
!c!    Disegna differenza
!c     if (drawdiff) then
!c         call add_plot(ttet(inic1:ific1),ydiff,ific1-inic1+1,Difference,1,wavel,c_null_char)
!c     endif
!c!
!c!    calcola e disegna differrenza cumulativa
!c     if (style(STYLE_CUMUL)%vis == 1) then
!c         diffc(1) = abs(ydiff(inic1))
!c         do i=inic1+1,ific1
!c            j = i-inic1+1
!c            diffc(j) = diffc(j-1) + abs(ydiff(i))
!c         enddo
!c!
!c!        riscala diffc tra 0 e il massimo
!c         maxdiff = diffc(j)
!c         mindiff = diffc(1)
!c         adiff = diffc(j) - diffc(1)
!c         maxv = MAXVAL(ycalb)
!c         diffc = diffc*(maxv)/adiff - maxv*diffc(1)/adiff
!c         call add_plot(ttet(inic1:ific1),diffc,ific1-inic1+1,Cumulative,1,wavel,c_null_char)
!c     endif
!c!
!c!    Disegna i marcatori di picco 
!c     if (style(STYLE_PEAKS)%vis == 1) then
!c         if (numpeaks(pkind) > 0) then
!c             startp = clocate(pkind(:)%getx(),ttet(inic))
!c             if (pkind(startp)%getx() < ttet(inic)) startp = startp +1 
!c             endp = clocate(pkind(:)%getx(),ttet(ific))
!c             if (pkind(endp)%getx() > ttet(ific)) endp = endp -1 
!c             npk = endp - startp + 1
!c             if (npk > 0) then
!c                 allocate(intpeak(npk))
!c                 do i=1,npk
!c                    intpeak(i) = peak_intensity(pkind(startp+i-1)%getx(),dataset(1)%x0,dataset(1)%y)
!c                 enddo
!c                 call add_plot(pkind(startp:endp)%getx(),intpeak,npk,Peaks,1,wavel,c_null_char)
!c             endif
!c         endif
!c     endif
!c!
!c!    Disegna i riflessi
!c     if (drawref) then
!c         !call add_plot2(tthr,size(tthr),Reflections,1,wavel)
!c         call add_reflections(tthr,cryst(1)%ref(:)%hkl(1),cryst(1)%ref(:)%hkl(2),  &
!c                              cryst(1)%ref(:)%hkl(3),size(tthr),1,wavel)
!c     endif
!c!
!c!    Disegna gli intervalli
!c     if (style(STYLE_INTERVALS)%vis == 1) then
!c         npinter = (nnt-1)*2
!c         allocate(intervalx(npinter)) 
!c         npinter = 0
!c         do i=1,nnt-1
!c            ninf = ncoun1(i)
!c            nsup = ncoun2(i)
!c            npinter = npinter + 1
!c            intervalx(npinter) = ttet(ninf)
!c            npinter = npinter + 1
!c            intervalx(npinter) = ttet(nsup)
!c         enddo
!c         call add_plot2(intervalx,npinter,Intervals,1,wavel)
!c     endif
!c!
!c     if (kprofile == 1) then
!c         if (kprofile_type == PROFILE_EXTRA) then
!c             call profrefnew(ttet(inic),ttet(ific))  
!c         else
!c             call prof_curves_computation(ttet(inic),ttet(ific))
!c         endif
!c     endif
!c!
!c     call draw_graphic()
!c!
!c     end subroutine plottanew

  !-----------------------------------------------------------------------------------------------

     subroutine plottanew2(inic,ific)
     USE peak_mod
     USE plotstyle
     USE variables, only: cryst,dataset
     USE crystal_phase
     USE datasetmod
     USE arrayutil
     USE fileutil
     USE plot2
     USE strutil, only: toCString
     USE PatternRef, only: kprofile,kprofile_type,PROFILE_EXTRA
     USE background, only: BK_NONE
!
     integer, intent(in), dimension(:) :: inic  ! conteggio iniziale per l'osservato
     integer, intent(in), dimension(:) :: ific  ! conteggio finale per l'osservato
     integer                           :: inic1,ific1
     real, dimension(:), allocatable   :: ycalb,ydiff,diffc
     real                              :: maxv
     integer                           :: i,j
     real                              :: maxdiff,mindiff
     real                              :: adiff
     integer                           :: ncalc
     logical                           :: drawref, drawcalc, drawdiff
     integer                           :: nset_ref
     integer                           :: ids
     real, dimension(:), allocatable   :: intpeak
     integer                           :: nund
     real, dimension(:), allocatable   :: xund,yund
!
     call set_graphic_area()
!
     do ids=1,ndataset(dataset)
        drawcalc = style(STYLE_CALC)%vis == 1 .and. allocated(dataset(ids)%yb)  &
                   .and. allocated(dataset(ids)%yc)
        inic1 = max(dataset(ids)%nc1,inic(ids))
        ific1 = min(dataset(ids)%nc2,ific(ids))
        ncalc = ific1 - inic1 + 1
        if (drawcalc) then
            call new_array(ycalb,ncalc)
            ycalb = dataset(ids)%yc(inic1:ific1)+dataset(ids)%yb(inic1:ific1)
        endif
        drawdiff = (style(STYLE_DIFF)%vis == 1 .or. style(STYLE_CUMUL)%vis == 1) &
                    .and. allocated(dataset(ids)%yb) .and. allocated(dataset(ids)%yc)
        if (drawdiff) then
            call new_array(ydiff,ncalc)
            if (drawcalc) then
                ydiff = dataset(ids)%y(inic1:ific1)-ycalb
            else
                ydiff = dataset(ids)%y(inic1:ific1)-dataset(ids)%yc(inic1:ific1)-dataset(ids)%yb(inic1:ific1)
            endif
        endif
     enddo
!
     drawref = .false.
     if (cryst(1)%style%vis == 1) then
         !space_ref = diff*0.04
         nset_ref = count(cryst%numref() > 0)
         if (nset_ref > 0) then
             !minv = minv - nset_ref * space_ref
             drawref = .true.
         endif
     endif
!
!    Disegna il profilo osservato
     do ids=1,ndataset(dataset)
        if (dataset(ids)%style%vis == 1) then
            call add_plot(dataset(ids)%x0(inic(ids):ific(ids)),  &
                 dataset(ids)%y(inic(ids):ific(ids)),ific(ids)-inic(ids)+1,Observed,1,   &
                 dataset(ids)%wave(1),toCString(file_basename(dataset(ids)%fname)))
        endif
     enddo
!
!    Disegna il profilo calcolato
     if (drawcalc) then
         call add_plot(dataset(1)%x0(inic1:ific1),ycalb,ific1-inic1+1,Calculated,1,dataset(1)%wave(1),c_null_char)
     endif
!
!    Disegna il background
     if (style(STYLE_BACK)%vis == 1 .and.      &
         allocated(dataset(1)%yb)   .and.      &
         dataset(1)%cond%btype /= BK_NONE) then
         call add_plot(dataset(1)%x0(inic1:ific1),  &
              dataset(1)%yb(inic1:ific1),ific1-inic1+1,BBackground,1,dataset(1)%wave(1),c_null_char)
         if (style(STYLE_BACKP)%vis == 1 .and. dataset(1)%npoints_back() > 0) then
             call add_plot(dataset(1)%points%x,dataset(1)%points%y,   &
                  dataset(1)%npoints_back(),Background_Points,1,dataset(1)%wave(1),c_null_char)
         endif
     endif
!
!    Disegna differenza
     if (drawdiff) then
         call add_plot(dataset(1)%x0(inic1:ific1),ydiff,ific1-inic1+1,Difference,1,dataset(1)%wave(1),c_null_char)
     endif
!
!    Draw smoothing curve
     if (style(STYLE_SMOOTH)%vis == 1) then
         if (allocated(dataset(1)%smoothvec)) then
             call add_plot(dataset(1)%x0(inic1:ific1),dataset(1)%smoothvec,ific1-inic1+1,Smoothing,1,dataset(1)%wave(1),c_null_char)
         endif
     endif
!
!    calcola e disegna differrenza cumulativa
     if (style(STYLE_CUMUL)%vis == 1) then
         call new_array(diffc,ncalc)
         diffc(1) = abs(ydiff(inic1))
         do i=inic1+1,ific1
            j = i-inic1+1
            diffc(j) = diffc(j-1) + abs(ydiff(i))
         enddo
!
!        riscala diffc tra 0 e il massimo
         maxdiff = diffc(j)
         mindiff = diffc(1)
         adiff = diffc(j) - diffc(1)
         maxv = MAXVAL(ycalb)
         diffc = diffc*(maxv)/adiff - maxv*diffc(1)/adiff
         call add_plot(dataset(1)%x0(inic1:ific1),diffc,ific1-inic1+1,Cumulative,1,dataset(1)%wave(1),c_null_char)
     endif
!
!    Disegna i marcatori di picco 
     if (style(STYLE_PEAKS)%vis == 1) then
         if (numpeaks(pkind) > 0) then
             allocate(intpeak(numpeaks(pkind)))
             do i=1,numpeaks(pkind)
                intpeak(i) = peak_intensity(pkind(i)%getx(),dataset(1)%x0,dataset(1)%y)
             enddo
             call add_plot(pkind%getx(),intpeak,numpeaks(pkind),Peaks,1,dataset(1)%wave(1),c_null_char)

             if (style(STYLE_UND_PEAKS)%vis == 1) then
                 nund = 0
                 do i=1,numpeaks(pkind)
                    if (pkind(i)%info > 0) then
                        if (nund == 0) allocate(xund(pkind(i)%info),yund(pkind(i)%info))
                        nund = nund + 1
                        xund(nund) = pkind(i)%getx()
                        yund(nund) = intpeak(i)
                    endif
                 enddo
                 if (nund > 0) then
                     call add_plot(xund,yund,nund,Unindexed_Peaks,1,dataset(1)%wave(1),c_null_char)
                 endif
             endif
         endif
     endif
!
!    Disegna i riflessi
     if (drawref) then
         do i=1,numphase(cryst)
            if (cryst(i)%numref() > 0) then
                !call add_plot2(cryst(i)%ref%tthd(1),cryst(i)%numref(),Reflections,1,dataset(1)%wave(1))
                call add_reflections(cryst(i)%ref%tthd(1),cryst(1)%ref(:)%hkl(1),cryst(1)%ref(:)%hkl(2),  &
                                     cryst(1)%ref(:)%hkl(3),cryst(i)%numref(),1,dataset(1)%wave(1))
            endif
         enddo
     endif
!
!    kprofile =  1 --> calcola e disegna profili 
     !if (kprofile == 1) then
     !    if (kprofile_type == PROFILE_EXTRA) then
     !        call profrefnew(dataset(1)%x0(inic(1)),dataset(1)%x0(ific(1)))  
     !    else
     !        call prof_curves_computation(dataset(1)%x0(inic(1)),dataset(1)%x0(ific(1)))
     !    endif
     !endif
!
     call draw_graphic()
!
     end subroutine plottanew2

!-----------------------------------------------------------------------

     function get_plot_size(plot_type) bind(C,name="get_plot_size") result(psize)
     use iso_c_binding
     use peak_mod
     use variables, only: dataset
     integer(c_int), intent(in), value :: plot_type
     integer(c_int)                    :: psize
!
     select case (plot_type)
       case (Peaks)
         psize = numpeaks(pkind)

       case (Background_Points)
         psize = dataset(1)%npoints_back()
         
       case default
         psize = 0       
     end select
!
     end function get_plot_size

!-----------------------------------------------------------------------

     subroutine get_plot_xy(x,y,wave,plot_type) bind(C,name="get_plot_xy")
     use iso_c_binding
     use peak_mod
     use variables, only: dataset
     real(c_float), dimension(*) :: x,y
     real(c_float)               :: wave
     integer(c_int), value       :: plot_type
     integer                     :: i
!
     wave = dataset(1)%wave(1)
     select case (plot_type)
       case (Peaks)
         do i=1,numpeaks(pkind)
            x(i) = pkind(i)%getx()
            y(i) = peak_intensity(x(i),dataset(1)%x0,dataset(1)%y)
         enddo

       case (Background_Points)
          do i=1,dataset(1)%npoints_back()
             x(i) = dataset(1)%points(i)%x
             y(i) = dataset(1)%points(i)%y
          enddo

       case default
     end select
!
     end subroutine get_plot_xy

  !-----------------------------------------------------------------------------------------------

     subroutine set_plot_style(kaction)
     USE plotstyle
     USE variables, only: cryst,dataset
     USE crystal_phase
     USE datasetmod
     integer, intent(in) :: kaction
     integer             :: i
!
     select case (kaction)
       case (1)
         do i=1,ndataset(dataset)
            dataset(i)%style%vis = 1
         enddo
         style(STYLE_BACK)%vis = 0
         style(STYLE_BACKP)%vis = 0
         style(STYLE_CALC)%vis = -1
         style(STYLE_DIFF)%vis = -1
         style(STYLE_CUMUL)%vis = -1
         do i=1,numphase(cryst)
            cryst(i)%style%vis = 1
         enddo
         style(STYLE_PEAKS)%vis = 0  !!!!!-1
         style(STYLE_INTERVALS)%vis = -1
         style(STYLE_SMOOTH)%vis = 0 

       case (2)
         !style(STYLE_OBS)%vis = 1
         do i=1,ndataset(dataset)
            dataset(i)%style%vis = 1
         enddo
         style(STYLE_BACK)%vis = -1
         style(STYLE_BACKP)%vis = -1
         style(STYLE_CALC)%vis = -1
         style(STYLE_DIFF)%vis = -1
         style(STYLE_CUMUL)%vis = -1
         do i=1,numphase(cryst)
            cryst(i)%style%vis = 1
         enddo
         style(STYLE_PEAKS)%vis = 0  !!!!!-1
         style(STYLE_INTERVALS)%vis = 1
         style(STYLE_SMOOTH)%vis = 0 

       case (3)
         do i=1,ndataset(dataset)
            dataset(i)%style%vis = 1
         enddo
         style(STYLE_BACK)%vis = 1
         style(STYLE_BACKP)%vis = 1
         style(STYLE_CALC)%vis = -1
         style(STYLE_DIFF)%vis = -1
         style(STYLE_CUMUL)%vis = -1
         do i=1,numphase(cryst)
            cryst(i)%style%vis = 1
         enddo
         style(STYLE_PEAKS)%vis = 0  !!!!!-1
         style(STYLE_INTERVALS)%vis = 1
         style(STYLE_SMOOTH)%vis = 0 

       case (4)
         !style(STYLE_OBS)%vis = 1
         do i=1,ndataset(dataset)
            dataset(i)%style%vis = 1
         enddo
         style(STYLE_BACK)%vis = 1
         style(STYLE_BACKP)%vis = 0
         style(STYLE_CALC)%vis = 1
         style(STYLE_DIFF)%vis = 1
         style(STYLE_CUMUL)%vis = 0
         do i=1,numphase(cryst)
            cryst(i)%style%vis = 1
         enddo
         style(STYLE_PEAKS)%vis = 0  !!!!!-1
         style(STYLE_INTERVALS)%vis = -1
         style(STYLE_SMOOTH)%vis = 0 

       case (5)      ! background per peak-search
         do i=1,ndataset(dataset)
            dataset(i)%style%vis = 1
         enddo
         style(STYLE_BACK)%vis = 1
         style(STYLE_BACKP)%vis = 1
         style(STYLE_CALC)%vis = -1
         style(STYLE_DIFF)%vis = -1
         style(STYLE_CUMUL)%vis = -1
         do i=1,numphase(cryst)
            cryst(i)%style%vis = 1
         enddo
         style(STYLE_PEAKS)%vis = 1
         style(STYLE_INTERVALS)%vis = -1
         style(STYLE_SMOOTH)%vis = 0 

       case (6)
         do i=1,ndataset(dataset)
            dataset(i)%style%vis = 1
         enddo
         style(STYLE_BACK)%vis = 1
         style(STYLE_BACKP)%vis = 0
         style(STYLE_CALC)%vis = 1
         style(STYLE_DIFF)%vis = 1
         style(STYLE_CUMUL)%vis = 0
         do i=1,numphase(cryst)
            cryst(i)%style%vis = 1
         enddo
         style(STYLE_PEAKS)%vis = 1
         style(STYLE_INTERVALS)%vis = -1
         style(STYLE_SMOOTH)%vis = 0 
  
       case (7)
         do i=1,ndataset(dataset)
            dataset(i)%style%vis = 1
         enddo
         style(STYLE_BACK)%vis = 1
         style(STYLE_BACKP)%vis = 0
         style(STYLE_CALC)%vis = 1
         style(STYLE_DIFF)%vis = 1
         style(STYLE_CUMUL)%vis = 0
         do i=1,numphase(cryst)
            cryst(i)%style%vis = 1
         enddo
         style(STYLE_PEAKS)%vis = 0 
         style(STYLE_INTERVALS)%vis = -1
         style(STYLE_SMOOTH)%vis = 0 

       case (8)    ! Rietveld
         do i=1,ndataset(dataset)
            dataset(i)%style%vis = 1
         enddo
         style(STYLE_BACK)%vis = 1
         style(STYLE_BACKP)%vis = 0
         style(STYLE_CALC)%vis = 1
         style(STYLE_DIFF)%vis = 1
         style(STYLE_CUMUL)%vis = 0
         do i=1,numphase(cryst)
            cryst(i)%style%vis = 1
         enddo
         style(STYLE_PEAKS)%vis = 0 
         style(STYLE_INTERVALS)%vis = -1
         style(STYLE_SMOOTH)%vis = 0 

     end select
!
     end subroutine set_plot_style

  !-----------------------------------------------------------------------------------------------

!corr   subroutine process_action_points(kaction,xp,yp,ier) bind(C,name="process_action_points")   
!corr   use iso_c_binding
!corr   use peak_mod
!corr   use variables, only: dataset
!corr   use conteggi, only: theta_int
!corr   use arrayutil, only: clocate
!corr   use General, only:alambda, is_extra_active
!corr   use messagemod
!corr   use proginterface, only: fillcounts
!corr   integer(c_int), intent(in), value :: kaction
!corr   real(c_double), intent(in), value :: xp,yp
!corr   integer(c_int), intent(out)       :: ier
!corr   real                              :: thp
!corr   real                              :: xc,yc
!corr   integer                           :: iok
!corr   type(peak_type)                   :: padd
!corr   real                              :: pkx,pky,fwp
!corr   integer                           :: ierfw,ipos
!corr
!corr   enum, bind(c)
!corr     enumerator :: AddBackgroundPoint=4, DeleteBackgroundPoint, AddPeak, DeletePeak
!corr   endenum
!corr
!corr   ier = 0
!corr!corr   write(0,*)'ACTION:',is_extra_active,xp,yp
!corr   select case (kaction)
!corr     case (AddBackgroundPoint)
!corr        if (is_extra_active) then
!corr            call update_background_extra(1, real(xp), real(yp))
!corr        else
!corr            call update_background(1, real(xp), real(yp))
!corr        endif
!corr
!corr     case (DeleteBackgroundPoint)
!corr        !write(0,*)'Action: ',kaction,xp,yp
!corr        if (is_extra_active) then
!corr            call update_background_extra(2, real(xp), real(yp))
!corr        else
!corr            call update_background(2, real(xp), real(yp))
!corr        endif
!corr
!corr     case (AddPeak)
!corr        thp = real(xp)
!corr        call check_peak(pkind,thp,xc,yc,iok) 
!corr        if (iok == 1) then
!corr!
!corr!           Set background is not available
!corr            if (.not.allocated(dataset(1)%yb)) then
!corr                call fillcounts()
!corr                call back_for_peaksearch(.false.)
!corr            endif
!corr!
!corr!           Compute fwhm
!corr            call peak_gaussian_fit(theta_int(:,1),theta_int(:,2),clocate(theta_int(:,1),xc),4,pkx,pky,fwp,ierfw)
!corr            if (ierfw /= 0) fwp = peak_fwhm(pkind,xc)
!corr!
!corr            padd = peak_type(fwhm=fwp)
!corr            call padd%sety(yc)
!corr            call padd%setxd(xc,alambda)
!corr            call padd%get_int(theta_int(:,1),theta_int(:,2),dataset(1)%yb(:))
!corr!
!corr            call padd%integrated_intensity(theta_int(:,1),theta_int(:,2),dataset(1)%yb)
!corr!
!corr!           add peak to list
!corr            call peak_add(pkind,padd,INSERT_PEAK)
!corr            if (numpeaks(pkindtot) > 0) then
!corr                if (.not.is_peak_position(pkindtot,padd%getx()))  &
!corr                    call peak_add(pkindtot,padd,INSERT_PEAK)  ! add to array pkindtot if absent
!corr            else
!corr                call peak_add(pkindtot,padd,INSERT_PEAK)
!corr            endif
!corr!
!corr            style(STYLE_PEAKS)%vis = 1  ! force visualization
!corr            !call vedinew(rescale=0)
!corr            !call write_message('Number of peaks: ',inum=numpeaks(pkind),pos=2)
!corr            call update_peak_graph()
!corr         endif
!corr
!corr     case (DeletePeak)
!corr        ipos = clocate(pkind(:)%getx(),real(xp))
!corr        call peak_delete(pkindtot,pkind(ipos)%getx())
!corr        call peak_delete(pkind,ipos)
!corr        !call vedinew(rescale=0)
!corr        !call write_message('Number of peaks: ',inum=numpeaks(pkind),pos=2)
!corr        call update_peak_graph()
!corr
!corr   end select
!corr
!corr   end subroutine process_action_points

! ----------------------------------------------------------------------

!corr   subroutine check_peak(peaks,tt,xc,yc,iok)
!corr!
!corr   USE Conteggi
!corr   USE arrayutil
!corr   USE peak_util
!corr!
!corr   type(peak_type), dimension(:), allocatable, intent(in) :: peaks
!corr   real, intent(in)     :: tt
!corr   real, intent(out)    :: xc,yc
!corr   integer, intent(out) :: iok
!corr   integer              :: jmin
!corr   integer              :: iniz,ifin
!corr   real                 :: yjmin
!corr   real                 :: yjmaxr,yjmaxl
!corr   integer              :: jmaxr,jmaxl
!corr   integer              :: i
!corr   integer              :: iadd
!corr   logical              :: is_peak
!corr!
!corr   iok = 1
!corr   xc = tt
!corr   iadd = 3
!corr   jmin = clocate(theta_int(:,1),tt)   ! localizza conteggio piu' vicino
!corr   if (numpeaks(peaks) == 0) then
!corr       yc = theta_int(jmin,2)
!corr       return
!corr   endif
!corr!
!corr!  Controlla subito se nella posizione cliccata non esiste gia' un picco
!corr   !is_peak = any(abs(xmax(:npk) - theta_int(jmin,1)) < 1.e-07)
!corr!co     is_peak = any(abs(peaks%x - theta_int(jmin,1)) < 1.e-07)
!corr   is_peak = is_peak_position(peaks,theta_int(jmin,1))
!corr!
!corr   if (jmin > 0 .and. is_peak .eqv. .false.) then
!corr       iniz = jmin - iadd
!corr       if (iniz.lt.1) iniz = 1
!corr       ifin = jmin + iadd
!corr       if (ifin.gt.npunti) ifin = npunti
!corr       if (iniz.gt.ifin) iniz = ifin
!corr       yjmin = theta_int(jmin,2)
!corr!
!corr!      Cerca il massimo + vicino a destra
!corr       jmaxr = 0
!corr       yjmaxr = yjmin
!corr       do i=jmin+1,ifin
!corr          if (theta_int(i,2) > yjmaxr) then
!corr              yjmaxr = theta_int(i,2)
!corr              jmaxr = i
!corr          elseif (theta_int(i,2) < yjmaxr .and. jmaxr > 0) then
!corr              exit
!corr          endif
!corr       enddo
!corr       if (jmaxr == ifin) jmaxr = 0  ! il massimo non puo' coincidere col limite
!corr       if (jmaxr > 0) then
!corr       !if (any(abs(xmax(:npk) - theta_int(jmaxr,1)) < 1.e-07))jmaxr=-1  ! annulla il punto se esiste gia'
!corr!co         if (any(abs(peaks%x - theta_int(jmaxr,1)) < 1.e-07))jmaxr=-1  ! annulla il punto se esiste gia'
!corr       if(is_peak_position(peaks,theta_int(jmaxr,1)))jmaxr=-1   ! annulla il punto se esiste gia'
!corr       !if (jmaxr > 0)write(0,*)'massimo a destra=',jmaxr,theta_int(jmaxr,1),yjmaxr
!corr       endif
!corr!
!corr!      Cerca il massimo + vicino a sinistra
!corr       jmaxl = 0
!corr       yjmaxl = yjmin
!corr       do i=jmin-1,iniz,-1
!corr          if (theta_int(i,2) > yjmaxl) then
!corr              yjmaxl = theta_int(i,2)
!corr              jmaxl = i
!corr          elseif (theta_int(i,2) < yjmaxl .and. jmaxl > 0) then
!corr              exit
!corr          endif
!corr       enddo
!corr       if (jmaxl == iniz) jmaxl = 0  ! il massimo non puo' coincidere col limite
!corr       if (jmaxl > 0) then
!corr       !if (any(abs(peaks%x - theta_int(jmaxl,1)) < 1.e-07))jmaxl=-1  ! annulla il punto se esiste gia'
!corr       if(is_peak_position(peaks,theta_int(jmaxl,1)))jmaxl=-1  ! annulla il punto se esiste gia'
!corr       !if (any(abs(xmax(:npk) - theta_int(jmaxl,1)) < 1.e-07))jmaxl=-1  ! annulla il punto se esiste gia'
!corr       !if (jmaxl > 0)write(0,*)'massimo a sinistra=',jmaxl,theta_int(jmaxl,1),yjmaxl
!corr       endif
!corr!
!corr       if ((jmaxl + jmaxr == 0) .or. (jmaxl + jmaxr == -1)) then       ! non trova nessun massimo
!corr           yc = yjmin                              ! accetta selezione dell'utente
!corr           xc = theta_int(jmin,1)
!corr       elseif (jmaxl > 0 .and. jmaxr <= 0) then    ! massimo a sinistra
!corr           yc = yjmaxl
!corr           xc = theta_int(jmaxl,1)
!corr       elseif (jmaxr > 0 .and. jmaxl <= 0) then    ! massimo a destra
!corr           yc = yjmaxr
!corr           xc = theta_int(jmaxr,1)
!corr       elseif (jmaxr > 0 .and. jmaxl > 0) then     ! massimo a destra e sinistra: prendi quello pi� vicino
!corr           if (jmaxr - jmin  > jmin - jmaxl) then
!corr               yc = yjmaxl               ! a sinistra e' piu' vicino
!corr               xc = theta_int(jmaxl,1)
!corr           else
!corr               yc = yjmaxr               ! piu' vicino a destra o uguali
!corr               xc = theta_int(jmaxr,1)
!corr           endif
!corr       elseif (jmaxr < 0 .and. jmaxl < 0) then     ! il picco trovato esiste gia'
!corr           !write(0,*)"picco annullato perche' esiste gia'"
!corr           iok = 0
!corr       endif
!corr       !write(0,*)'scelgo massimo=',yc,xc,jmaxl,jmaxr,jmin
!corr    else
!corr       iok = 0
!corr    endif
!corr
!corr    end subroutine check_peak

  !-----------------------------------------------------------------------------------------------
                                                                                                                  
!corr     subroutine update_background_extra(paction,xp,yp)
!corr     USE PatternRef, only:jjj
!corr     USE Commonexpo, only:nnt,ntyp,ncoun1,ncoun2,nback,backp
!corr     USE conteggi
!corr     USE arrayutil, only: clocate
!corr     integer, intent(in) :: paction
!corr     real, intent(in)    :: xp,yp
!corr     integer             :: i,j
!corr     integer             :: jjjsav
!corr     integer             :: ipos
!corr!
!corr     jjjsav = jjj  
!corr     if (paction == 1) then      ! aggiungo punti col tasto sinistro
!corr         !ier = 0
!corr         nback=nback+1
!corr         backp(nback,1) = xp
!corr         backp(nback,2) = yp
!corr!
!corr!        localizza l'intervallo jjj a cui appartiene il punto aggiunto
!corr         do i=2,nnt-1
!corr            if (backp(nback,1) >= theta_int(ncoun1(i),7) .and.    &
!corr                backp(nback,1) <= theta_int(ncoun2(i),7)) then
!corr                jjj = i
!corr            endif
!corr         enddo
!corr         ntyp(nback)=jjj
!corr         call CalcoBacExt(1)
!corr
!corr     elseif (paction == 2) then  ! elimino punti col tasto destro
!corr!        locate and remove point
!corr         ipos = clocate(backp(:nback,1),xp)
!corr         backp(ipos:nback-1,:) = backp(ipos+1:nback,:)
!corr         ntyp(ipos:nback-1) = ntyp(ipos+1:nback)
!corr         nback = nback - 1
!corr!        localizza l'intervallo jjj a cui appartiene il punto da eliminare
!corr         do j=2,nnt-1
!corr            if (backp(ipos,1) >= theta_int(ncoun1(j),7) .and.    &
!corr                backp(ipos,1) <= theta_int(ncoun2(j),7)) then
!corr                jjj = j
!corr            endif
!corr         enddo
!corr         call CalcoBacExt(1)
!corr     endif
!corr     jjj = jjjsav
!corr     call vedinew(rescale=0)
!corr!
!corr     end subroutine update_background_extra

!----------------------------------------------------------------------------------------------------
!corr
!corr   subroutine get_reflection_info(phaseIndex,refIndex,hkl,tth,dval,err) bind(C,name="get_reflection_info")
!corr   use variables, only: cryst
!corr   use crystal_phase
!corr   use counts
!corr   use general, only: wavel
!corr   use iso_c_binding
!corr   integer(c_int), value, intent(in)         :: phaseIndex, refIndex
!corr   integer(c_int), dimension(3), intent(out) :: hkl
!corr   real(c_float), intent(out)                :: tth, dval
!corr   integer(c_int), intent(out)               :: err
!corr   err = 1
!corr   if (numphase(cryst) < phaseIndex) return
!corr   if (cryst(phaseIndex)%numref() < refIndex) return
!corr   hkl = cryst(phaseIndex)%ref(refIndex)%hkl
!corr   tth = cryst(phaseIndex)%ref(refIndex)%tthd(1)
!corr   dval = dvalue(tth,wavel)
!corr   err = 0
!corr   end subroutine get_reflection_info
!corr
!----------------------------------------------------------------------------------------------------

!c   subroutine prof_curves_computation(tmin, tmax)
!c   use variables, only: dataset,cryst
!c   use profile_function
!c   use crystal_phase
!c   use pearsonf
!c   use psvoigtf
!c   use tchzf
!c   use plot2, only: add_plot
!c   use iso_c_binding, only: c_null_char
!c   use refinecomref, only: rfcond
!c   use calculate_pdp, only: RIET_REFINE
!c   use arrayutil, only: clocate
!c   real, intent(in)                :: tmin, tmax
!c   integer                         :: nph, jref, startr, endr
!c   real, dimension(:), allocatable :: xp,yp
!c   integer                         :: ier
!c
!c   do nph=1,numphase(cryst)
!c      startr = clocate(cryst(nph)%ref%tthd(1),tmin)
!c      endr = clocate(cryst(nph)%ref%tthd(1),tmax)
!c      select case (dataset(1)%typefun(nph))
!c        case (PEARSON7)
!c              do jref=startr,endr
!c                 call profile_curve_pearson(dataset(1)%pear(nph),cryst(nph)%ref(jref),         &
!c                                            jref,rfcond%raction == RIET_REFINE,                &
!c                                            dataset(1)%x0,dataset(1)%yb,cryst(nph)%scal,       &
!c                                            dataset(1)%nwave,dataset(1)%ratio,nph,xp,yp,ier)
!c                 if (ier == 0) then
!c                     call add_plot(xp,yp,size(xp),Profile_Curves,1,dataset(1)%wave(1),c_null_char)
!c                 endif
!c              enddo
!c        case (PVOIG)
!c              do jref=startr,endr
!c                 call profile_curve_psvoigt(dataset(1)%pear(nph),cryst(nph)%ref(jref),         &
!c                                            jref,rfcond%raction == RIET_REFINE,                &
!c                                            dataset(1)%x0,dataset(1)%yb,cryst(nph)%scal,       &
!c                                            dataset(1)%nwave,dataset(1)%ratio,nph,xp,yp,ier)
!c                 if (ier == 0) then
!c                     call add_plot(xp,yp,size(xp),Profile_Curves,1,dataset(1)%wave(1),c_null_char)
!c                 endif
!c              enddo
!c
!c        case (TCHZ)
!c              do jref=startr,endr
!c                 call profile_curve_tchz(dataset(1)%pear(nph),cryst(nph)%ref(jref),         &
!c                                         jref,rfcond%raction == RIET_REFINE,                &
!c                                         dataset(1)%x0,dataset(1)%yb,cryst(nph)%scal,       &
!c                                         dataset(1)%nwave,dataset(1)%ratio,nph,xp,yp,ier)
!c                 if (ier == 0) then
!c                     call add_plot(xp,yp,size(xp),Profile_Curves,1,dataset(1)%wave(1),c_null_char)
!c                 endif
!c              enddo
!c
!c      end select
!c   enddo
!c
!c   end subroutine prof_curves_computation

   END MODULE view
