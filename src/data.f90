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
   !call open_pattern(filnam,filout,add_data,ier)
!     
   end subroutine open_diffraction_patt

END MODULE datamod
