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
     if (style(STYLE_BACK)%vis == 1 .and.        &
         allocated(dataset(1)%yb)   .and.        &
         dataset(1)%cond%btype /= BK_NONE .and.  &
         .not.dataset(1)%back_subtracted) then
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

   subroutine process_action_points(kaction,xp,yp,ier) bind(C,name="process_action_points")   
   use iso_c_binding
   use peak_mod
   use variables, only: dataset
   use arrayutil, only: clocate
   integer(c_int), intent(in), value :: kaction
   real(c_double), intent(in), value :: xp,yp
   integer(c_int), intent(out)       :: ier
   real                              :: thp
   real                              :: xc,yc
   integer                           :: iok
   type(peak_type)                   :: padd
   real                              :: pkx,pky,fwp
   integer                           :: ierfw,ipos

   enum, bind(c)
     enumerator :: AddBackgroundPoint=4, DeleteBackgroundPoint, AddPeak, DeletePeak
   endenum

   ier = 0
!corr   write(0,*)'ACTION:',is_extra_active,xp,yp
   select case (kaction)
     case (AddBackgroundPoint)
        call update_background(1, real(xp), real(yp))

     case (DeleteBackgroundPoint)
        call update_background(2, real(xp), real(yp))

     case (AddPeak)
        thp = real(xp)
        call check_peak(pkind,thp,xc,yc,iok) 
        if (iok == 1) then
!
!           Set background is not available
            if (.not.allocated(dataset(1)%yb)) then
                call back_for_peaksearch(.false.)
            endif
!
!           Compute fwhm
            call peak_gaussian_fit(dataset(1)%x,dataset(1)%y,clocate(dataset(1)%x,xc),4,pkx,pky,fwp,ierfw)
            if (ierfw /= 0) fwp = peak_fwhm(pkind,xc)
!
            padd = peak_type(fwhm=fwp)
            call padd%sety(yc)
            call padd%setxd(xc,dataset(1)%wave(1))
            call padd%get_int(dataset(1)%x,dataset(1)%y,dataset(1)%yb(:))
!
            call padd%integrated_intensity(dataset(1)%x,dataset(1)%y,dataset(1)%yb)
!
!           add peak to list
            call peak_add(pkind,padd,INSERT_PEAK)
            if (numpeaks(pkindtot) > 0) then
                if (.not.is_peak_position(pkindtot,padd%getx()))  &
                    call peak_add(pkindtot,padd,INSERT_PEAK)  ! add to array pkindtot if absent
            else
                call peak_add(pkindtot,padd,INSERT_PEAK)
            endif
!
            style(STYLE_PEAKS)%vis = 1  ! force visualization
            !call vedinew(rescale=0)
            !call write_message('Number of peaks: ',inum=numpeaks(pkind),pos=2)
            call update_peak_graph()
         endif

     case (DeletePeak)
        ipos = clocate(pkind(:)%getx(),real(xp))
        call peak_delete(pkindtot,pkind(ipos)%getx())
        call peak_delete(pkind,ipos)
        !call vedinew(rescale=0)
        !call write_message('Number of peaks: ',inum=numpeaks(pkind),pos=2)
        call update_peak_graph()

   end select

   end subroutine process_action_points

! ----------------------------------------------------------------------

   subroutine check_peak(peaks,tt,xc,yc,iok)
!
   USE arrayutil
   USE peak_util
   USE variables, only: dataset
!
   type(peak_type), dimension(:), allocatable, intent(in) :: peaks
   real, intent(in)     :: tt
   real, intent(out)    :: xc,yc
   integer, intent(out) :: iok
   integer              :: jmin
   integer              :: iniz,ifin
   real                 :: yjmin
   real                 :: yjmaxr,yjmaxl
   integer              :: jmaxr,jmaxl
   integer              :: i
   integer              :: iadd
   logical              :: is_peak
!
   iok = 1
   xc = tt
   iadd = 3
   jmin = clocate(dataset(1)%x(:),tt)   ! localizza conteggio piu' vicino
   if (numpeaks(peaks) == 0) then
       yc = dataset(1)%y(jmin)
       return
   endif
!
!  Controlla subito se nella posizione cliccata non esiste gia' un picco
   !is_peak = any(abs(xmax(:npk) - theta_int(jmin,1)) < 1.e-07)
!co     is_peak = any(abs(peaks%x - theta_int(jmin,1)) < 1.e-07)
   is_peak = is_peak_position(peaks,dataset(1)%x(jmin))
!
   if (jmin > 0 .and. is_peak .eqv. .false.) then
       iniz = jmin - iadd
       if (iniz.lt.1) iniz = 1
       ifin = jmin + iadd
       if (ifin.gt.dataset(1)%npoints()) ifin = dataset(1)%npoints()
       if (iniz.gt.ifin) iniz = ifin
       yjmin = dataset(1)%y(jmin)
!
!      Cerca il massimo + vicino a destra
       jmaxr = 0
       yjmaxr = yjmin
       do i=jmin+1,ifin
          if (dataset(1)%y(i) > yjmaxr) then
              yjmaxr = dataset(1)%y(i)
              jmaxr = i
          elseif (dataset(1)%y(i) < yjmaxr .and. jmaxr > 0) then
              exit
          endif
       enddo
       if (jmaxr == ifin) jmaxr = 0  ! il massimo non puo' coincidere col limite
       if (jmaxr > 0) then
           if(is_peak_position(peaks,dataset(1)%x(jmaxr)))jmaxr=-1   ! annulla il punto se esiste gia'
       endif
!
!      Cerca il massimo + vicino a sinistra
       jmaxl = 0
       yjmaxl = yjmin
       do i=jmin-1,iniz,-1
          if (dataset(1)%y(i) > yjmaxl) then
              yjmaxl = dataset(1)%y(i)
              jmaxl = i
          elseif (dataset(1)%y(i) < yjmaxl .and. jmaxl > 0) then
              exit
          endif
       enddo
       if (jmaxl == iniz) jmaxl = 0  ! il massimo non puo' coincidere col limite
       if (jmaxl > 0) then
           if(is_peak_position(peaks,dataset(1)%x(jmaxl)))jmaxl=-1  ! annulla il punto se esiste gia'
       endif
!
       if ((jmaxl + jmaxr == 0) .or. (jmaxl + jmaxr == -1)) then       ! non trova nessun massimo
           yc = yjmin                              ! accetta selezione dell'utente
           xc = dataset(1)%x(jmin)
       elseif (jmaxl > 0 .and. jmaxr <= 0) then    ! massimo a sinistra
           yc = yjmaxl
           xc = dataset(1)%x(jmaxl)
       elseif (jmaxr > 0 .and. jmaxl <= 0) then    ! massimo a destra
           yc = yjmaxr
           xc = dataset(1)%x(jmaxr)
       elseif (jmaxr > 0 .and. jmaxl > 0) then     ! massimo a destra e sinistra: prendi quello pi� vicino
           if (jmaxr - jmin  > jmin - jmaxl) then
               yc = yjmaxl               ! a sinistra e' piu' vicino
               xc = dataset(1)%x(jmaxl)
           else
               yc = yjmaxr               ! piu' vicino a destra o uguali
               xc = dataset(1)%x(jmaxr)
           endif
       elseif (jmaxr < 0 .and. jmaxl < 0) then     ! il picco trovato esiste gia'
           !write(0,*)"picco annullato perche' esiste gia'"
           iok = 0
       endif
       !write(0,*)'scelgo massimo=',yc,xc,jmaxl,jmaxr,jmin
    else
       iok = 0
    endif

    end subroutine check_peak

  !-----------------------------------------------------------------------------------------------
                                                                                                                  
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
