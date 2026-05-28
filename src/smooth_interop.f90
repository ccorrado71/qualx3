module interop_smooth
   use ssmoothing
   use iso_c_binding

   implicit none

   private

   type, bind(C) :: c_smooth_condition_type
     integer(c_int) :: method      = SAVGOL
     integer(c_int) :: npoints_sg  = 25
     integer(c_int) :: npoints_ave = 12
     integer(c_int) :: pol_order   = 5
   end type c_smooth_condition_type
  
   contains

   elemental function cast_to_c(f_type) result(c_type)
   type(smooth_condition_type), intent(in) :: f_type
   type(c_smooth_condition_type)           :: c_type
!
   c_type%method = int(f_type%method,c_int)
   c_type%npoints_sg = int(f_type%npoints_sg,c_int)
   c_type%npoints_ave = int(f_type%npoints_ave,c_int)
   c_type%pol_order = int(f_type%pol_order,c_int)
!
   end function cast_to_c

! -----------------------------------------------------------------------------------------------------

   elemental function cast_to_f(c_type) result(f_type)
   use background, only: back_condition_type
   type(c_smooth_condition_type), intent(in) :: c_type
   type(smooth_condition_type)               :: f_type
!
   f_type%method = int(c_type%method)
   f_type%npoints_sg = int(c_type%npoints_sg)
   f_type%npoints_ave = int(c_type%npoints_ave)
   f_type%pol_order = int(c_type%pol_order)
!
   end function cast_to_f

! -----------------------------------------------------------------------------------------------------

   subroutine set_smooth(kaction,scond) bind(C,name="set_smooth")
   use variables, only: dataset
   use view
   use plotstyle
!corr   use conteggi, only: theta_int
   integer(c_int), value, intent(in) :: kaction
   type(c_smooth_condition_type)     :: scond
!
   select case(kaction)
     case (0)       ! Open Dialog
       scond = cast_to_c(dataset(1)%scond)
       call dataset(1)%smooth_calculate()
       style(STYLE_SMOOTH)%vis = 1
       call vedinew(8,lsetstyle=.false.)

     case (1,2)     ! Apply change: 1 = points, 2 = polynomial order
       dataset(1)%scond = cast_to_f(scond)
       call dataset(1)%smooth_calculate()
       call vedinew(8,lsetstyle=.false.)

     case (3)     ! Cancel 
       dataset(1)%scond = cast_to_f(scond)
       call dataset(1)%smooth_calculate()
       style(STYLE_SMOOTH)%vis = 0
       call vedinew(8,lsetstyle=.false.)

     case (4)     ! Ok
       call dataset(1)%smooth_apply()
!corr       theta_int(:,2) = dataset(1)%y
       style(STYLE_SMOOTH)%vis = 0
       call vedinew(8,lsetstyle=.false.)

   end select
!
   end subroutine set_smooth

end module interop_smooth
