module qualx_main
   use iso_c_binding

   implicit none

   type, bind(C) :: param_options_type
        integer(c_int) :: nogui = 0
        integer(c_int) :: auto = 0
        integer(c_int) :: indexing = 0
        real(c_float)  :: wavel = 0.0
   end type param_options_type

contains

   subroutine qualxmain(param_opt,fileinc,len_in,fileoutc,len_out,ier) bind(C,name="qualxmain")
   use strutil
   use general, only: ifAutomatic
   use molcom, only: jscreen
   type(param_options_type)      :: param_opt
   character(c_char), intent(in) :: fileinc(*),fileoutc(*)
   integer(c_int), value         :: len_in, len_out
   integer(c_int)                :: ier
   character(len=:), allocatable :: filein,fileout
!
   filein = toFortranString(fileinc,len_in)
   fileout = toFortranString(fileoutc,len_out)

   call init_qualx()
   ifAutomatic = param_opt%auto          !Set automatic mode by command line
   if (param_opt%nogui == 1) jscreen = 0 !Disable GUI if required by command line

   if (len_trim(filein) /= 0) then
       call mainloop(filein,ier)
   endif

   end subroutine qualxmain

!-------------------------------------------------------------------------------

   subroutine mainloop(file_input,ier)
   use datamod
   character(len=*), intent(in)         :: file_input
   integer, intent(out)                 :: ier

   call open_pattern(file_input,0,ier)

   end subroutine mainloop

!-------------------------------------------------------------------------------

   subroutine init_qualx()
   use crystal_phase, only: new_phases, clear_phases
   use datasetmod, only: clear_dataset
   use variables, only: cryst, dataset
   use molcom, only: jscreen
!
   jscreen = 1
   call clear_dataset(dataset)
!
   call clear_phases(cryst)
   call new_phases(cryst,1)

   end subroutine init_qualx
end module qualx_main
