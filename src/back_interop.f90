module background_interop

   use iso_c_binding
   use background, only: CHEBY

   implicit none

   private
   
   type, bind(C) :: c_back_condition_type
     integer(c_int)  :: btype = CHEBY                     ! background type
     logical(c_bool) :: auto  = logical(.true.,c_bool)    ! automatic selection of number of coefficient
     integer(c_int)  :: ncoef = 6                         ! number of coefficients
     integer(c_int)  :: niterf = 5                        ! number of iterations for filter
     integer(c_int)  :: nwinf = 50                        ! filter window
     real(c_double)  :: minf=0.0, maxf=0.0                ! range of application of the filter
   end type c_back_condition_type

   contains

   elemental function cast_to_c(f_type) result(c_type)
   use background, only: back_condition_type
   type(back_condition_type), intent(in) :: f_type
   type(c_back_condition_type)           :: c_type
!
   c_type%btype = int(f_type%btype,c_int)
   c_type%auto  = logical(f_type%auto,c_bool)
   c_type%ncoef = int(f_type%ncoef,c_int)
   c_type%niterf = int(f_type%niterf,c_int)
   c_type%nwinf = int(f_type%nwinf,c_int)
   c_type%minf = real(f_type%minf,c_double)
   c_type%maxf = real(f_type%maxf,c_double)
!
   end function cast_to_c

! -----------------------------------------------------------------------------------------------------

   elemental function cast_to_f(c_type) result(f_type)
   use background, only: back_condition_type
   type(c_back_condition_type), intent(in) :: c_type
   type(back_condition_type)               :: f_type
!
   f_type%btype = int(c_type%btype)
   f_type%auto  = logical(c_type%auto)
   f_type%ncoef = int(c_type%ncoef)
   f_type%niterf = int(c_type%niterf)
   f_type%nwinf = int(c_type%nwinf)
   f_type%minf = real(c_type%minf)
   f_type%maxf = real(c_type%maxf)
!
   end function cast_to_f

! -----------------------------------------------------------------------------------------------------

   subroutine get_back_settings(backc) bind(C,name="get_back_settings")
   USE variables, only: dataset
   type (c_back_condition_type), intent(out) :: backc
!
   dataset(1)%cond%minf = dataset(1)%xminc0()
   dataset(1)%cond%maxf = dataset(1)%xmaxc0()
   backc = cast_to_c(dataset(1)%cond)
!
   end subroutine get_back_settings

! -----------------------------------------------------------------------------------------------------

   subroutine modify_background(backc, kaction) bind(C,name="modify_background")
   USE background
   !USE RefinecomRef, only: rinfo
   USE VIEW
   !USE genlsq, only:funcls
   !USE proginterface
   USE variables, only: dataset
   USE arrayutil
   !interface
   !  subroutine run_peaksearch(gui)
   !  logical, intent(in), optional :: gui
   !  end subroutine run_peaksearch
   !end interface
!
   type (c_back_condition_type) :: backc
   integer(c_int), value        :: kaction
   !character(len=250)           :: strmess
   integer                      :: ncmin,ncmax
!
   select case (kaction)
      case (0)    ! Open
        !!!!!FIX THIS FOR QUALX
        !if (.not.allocated(dataset(1)%yb)) then
        !    call fillcounts()
        !    call back_for_peaksearch(.true.)
        !    call vedinew()
        !endif
        dataset(1)%cond%minf = dataset(1)%xminc0()
        dataset(1)%cond%maxf = dataset(1)%xmaxc0()
        backc = cast_to_c(dataset(1)%cond)
        call dataset(1)%make_background()
        style(STYLE_BACK)%vis = 1
        style(STYLE_BACKP)%vis = 1
        call vedinew()

      case (1,2)    ! Apply / Cancel
        dataset(1)%cond = cast_to_f(backc)
        call dataset(1)%make_background()
        backc = cast_to_c(dataset(1)%cond)
!
!       Peaksearch attivo: rigenera i picchi
        !if (style(STYLE_PEAKS)%vis == 1) then 
        !    call run_peaksearch()
        !endif
        call vedinew()

      case (3)    ! Apply filter
        dataset(1)%cond = cast_to_f(backc)
        ncmin = clocate(dataset(1)%x0,dataset(1)%cond%minf)
        ncmax = clocate(dataset(1)%x0,dataset(1)%cond%maxf)
        call smoothback(dataset(1)%yb(ncmin:ncmax),dataset(1)%cond%niterf,dataset(1)%cond%nwinf)
        call vedinew()

   end select
!
   end subroutine modify_background

! -----------------------------------------------------------------------------------------------------

   subroutine apply_background_subtraction() bind(C,name="apply_background_subtraction")
   USE variables, only: dataset
   USE VIEW
   USE molcom, only: jscreen
!
   if (.not.dataset(1)%back_subtracted) then
       if (.not.dataset(1)%has_back()) then
           call dataset(1)%make_background()
       endif
       call dataset(1)%subtract_background()
       if (jscreen > 0) call vedinew()
   endif
!
   end subroutine apply_background_subtraction

end module background_interop
