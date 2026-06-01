MODULE datamod

   implicit none

CONTAINS

   subroutine open_diffraction_patt(fileIn,lengthIn,fileOut,lengthOut,add_data,ier) bind(C,name="open_diffraction_patt")
   USE iso_c_binding, only: c_char, c_int
   USE strutil
   character(kind=c_char), intent(in) :: fileIn(*)
   integer(c_int), intent(in), value  :: lengthIn
   character(kind=c_char), intent(in) :: fileOut(*)
   integer(c_int), intent(in), value  :: lengthOut
   integer(c_int), intent(in), value  :: add_data
   integer(c_int), intent(out)        :: ier
   character(len=:), allocatable      :: filnam,filout
!
   ier = 0
   filnam = toFortranString(fileIn,lengthIn)
   filout = toFortranString(fileOut,lengthOut)
   call open_pattern(filnam,add_data,ier)
!     
   end subroutine open_diffraction_patt

!---------------------------------------------------------------------------

   subroutine open_pattern(input_file,add_data,ier)
   USE fileutil
!   USE General, only: StructureName, fname, ext, INP_FILE, OUT_FILE
   USE datasetmod
   USE datautil
!   USE molcom, only:ifProject
   USE strutil
!   USE General, only:lo
   USE variables, only: dataset   !,cryst
   character(len=*), intent(in) :: input_file
   integer, intent(in), value   :: add_data
   integer, intent(out)         :: ier
   integer                      :: iflag
   integer                      :: itype, iform, nlenn, jop, jopen, ier2
   integer                      :: i,ierc
   type(dataset_type)           :: datas
!
   ier = 0
   call load_datafile(datas,input_file,.true.,ier)
   if (ier == 0) then
       if (add_data == 0) then
!           if (ifProject.ne.0) then   ! importante se esegui run successivi
!               iflag = 2
!!              comment the next line to add pattern
!               call chiudi_bin(iflag) ! call InitExpo
!           endif
!           StructureName = file_rem_ext(file_get_name(input_file))
!           call cryst(1)%set_name(StructureName)
!!
!           do i=1,size(fname)
!              if (i /= INP_FILE .and. i /= OUT_FILE) then
!                  fname(i) = s_blank_delete(trim(StructureName)//ext(i))
!              endif
!           enddo
!           fname(INP_FILE) = input_file
!           fname(OUT_FILE) = output_file
!!
!           close(lo,iostat=ierc)
!           itype = 0
!           iform = 1
!           nlenn = 80
!           jop = jopen(lo,fname(OUT_FILE),itype,iform,ier2)
!!
           call clear_dataset(dataset)
       endif
       call push_back_dataset(dataset,datas)
       call dataset(1)%scale_data()
       call work_on_pattern(dataset(1),add_data)
   endif
!
   end subroutine open_pattern

END MODULE datamod

!---------------------------------------------------------------------------

   subroutine work_on_pattern(datas,add_data)
!   USE Conteggi, only: npunti
   USE view, only: vedinew
!   USE messagemod
!   USE enable_amb
   USE datasetmod, only: dataset_type
   USE molcom, only: jscreen
!   USE datamod, only: abilita_tasti2
   implicit none
   type(dataset_type), intent(in) :: datas
   integer, intent(in)            :: add_data
!
!   if(add_data == 0) call dataset_to_expo(datas)

   if (jscreen > 0) then
!       call init_messages()
!       call write_message_pattern(datas%fname)
       call vedinew(8)   !,1,npunti)
!       call abilita_tasti2()
   endif

!   if (add_data == 0) call init_dati()
!
   end subroutine work_on_pattern

!---------------------------------------------------------------------------

   subroutine run_peaksearch(gui)
   USE peak_mod
   USE variables, only: dataset
   USE arrayutil
   USE view, only: vedinew
   implicit none
   logical, intent(in), optional   :: gui
   real, dimension(:), allocatable :: yc
   real, parameter                 :: SRANGE = 0.15 ! 2theta-range for peak_search at 1.54056
   logical                         :: guivar
!
   if (present(gui)) then
       guivar = gui
   else
       guivar = .true.
   endif
!
!  Subtract background
   if (.not.dataset(1)%has_back()) then
       call dataset(1)%make_background()
   endif

   if (guivar) call update_peak_graph()
   allocate(yc(dataset(1)%npoints()))
   yc(:) = dataset(1)%y(:) - dataset(1)%yb(:)
!
!  compute srange
   pkcond%srange = init_peak_range(dataset(1)%x,yc,dataset(1)%wave(1))
   !write(0,*)'SRANGE =',pkcond%srange
!
   call peak_create(dataset(1)%x,yc,pkind,pkcond,dataset(1)%wave(1),pkindtot)
   pkcond%maxpk = -1 ! unset max number of peaks for GUI
   pkcond%minpk = -1 ! unset min number of peaks for GUI
!
   if (guivar) call update_peak_graph()
   !call write_column('picchi.txt',xcol1=pkind%getx(),metad='#2theta values')
!
   end subroutine run_peaksearch

! ---------------------------------------------------------------------

   subroutine update_peak_graph()
   USE molcom, only: jscreen
   !USE enable_amb
   !USE messagemod
   !USE peak_mod
   USE view
   implicit none
   interface
      subroutine update_peak_list() bind(C,name="update_peak_list")
      end subroutine update_peak_list
   end interface
!
   if (jscreen > 0) then
       !call write_message('Number of peaks: ',inum=numpeaks(pkind),pos=2)
!FIX LATER       call write_message_peak()
       call update_peak_list()
!qt       call abilita_tasti('peaks',state=1)
       call vedinew(5, rescale=0)
   endif
!
   end subroutine update_peak_graph

! ---------------------------------------------------------------------

   subroutine update_peak_graph1(pos_message)
   USE molcom, only: jscreen
!FIX LATER   USE messagemod
   USE peak_mod
   USE view
   implicit none
!
   integer, intent(in) :: pos_message
!
!!!TOFIX: move all functions in GUI, except for vedinew
   if (jscreen > 0) then
!FIX LATER       call write_message('Number of peaks:',inum=numpeaks(pkind),pos=pos_message)
       call vedinew(5,rescale=0)   !!!!,1,npunti)
   endif
!
   end subroutine update_peak_graph1

! ---------------------------------------------------------------------

   subroutine back_for_peaksearch(vis)
   USE counts
   !USE arrayutil
   USE plotstyle
   USE pointmod
   USE variables, only: dataset
   implicit none
   logical, intent(in) :: vis   ! if true force visualization
   !integer             :: pfin
!
   !pfin = clocate(theta_int(:,7),thmax)
   !call dataset(1)%set_limit(1,pfin)
   call dataset(1)%make_background()
!
!  set background as visible
   if (vis) then
       style(STYLE_BACK)%vis = 1
       style(STYLE_BACKP)%vis = 1
   endif
!
   end subroutine back_for_peaksearch

!--------------------------------------------------------------------------------

   subroutine UserBack()
   USE background
   USE variables, only: dataset
!  
   implicit none
   real :: thmin0,thmax0
!
   select case (dataset(1)%cond%btype)

   case (POLY,CHEBY,FOUR_SERIES)
     if (dataset(1)%cond%auto) then
         call autobackground(dataset(1)%x0,dataset(1)%y,dataset(1)%yb,dataset(1)%cond%btype,    &
              dataset(1)%points,dataset(1)%thzerob,dataset(1)%coef,dataset(1)%cond%ncoef)
     else
         thmin0 = dataset(1)%xminc0()
         thmax0 = dataset(1)%xmaxc0()
         call param_back(dataset(1)%cond%btype,dataset(1)%points,dataset(1)%cond%ncoef,dataset(1)%coef,   &
                         thmin0,thmax0,dataset(1)%thzerob)
         call getbackground(dataset(1)%cond%btype,dataset(1)%yb(:dataset(1)%nc2),  &
              dataset(1)%x0,dataset(1)%coef(:dataset(1)%cond%ncoef),dataset(1)%thzerob)
     endif

   case (CSPLINE, BSPLINE, BK_FILTER)
     call compute_background(dataset(1)%x0,dataset(1)%y,dataset(1)%yb,dataset(1)%points,  &
                             dataset(1)%coef,dataset(1)%thzerob,dataset(1)%cond,dataset(1)%wave(1))

   end select
!
   end subroutine Userback

! ---------------------------------------------------------------------

   subroutine update_background(paction,xp,yp)
   USE VIEW
   USE pointmod
   USE plotstyle
   USE arrayutil
   USE variables, only: dataset
!
   implicit none
   interface
     subroutine run_peaksearch(gui)
     logical, intent(in), optional :: gui
     end subroutine run_peaksearch
   end interface
   integer, intent(in) :: paction
   real, intent(in)    :: xp,yp
   type(point_type)    :: padd
   integer             :: ipos
!
   if (paction == 1) then     ! Add point
!
!      Set background is not available
       if (.not.allocated(dataset(1)%yb)) then
           call back_for_peaksearch(.true.)
       endif
       padd%x = xp
       padd%y = yp
       ipos = clocate(dataset(1)%points(:)%x,padd%x)
       if (padd%x > dataset(1)%points(ipos)%x) ipos = ipos + 1
       call add_point(dataset(1)%points,padd,ipos)
   elseif (paction == 2) then ! Delete point
       padd%x = xp
       padd%y = yp
       ipos = clocate(dataset(1)%points(:)%x,padd%x)
       call del_point(dataset(1)%points,ipos)
   endif
!
   call UserBack()      ! ricalcola il background
!
   if (style(STYLE_PEAKS)%vis == 1) then  ! peaksearch
       call run_peaksearch()
   else
       call vedinew(rescale=0)
   endif
!
   end subroutine update_background
