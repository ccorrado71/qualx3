module qualx_interop

use iso_c_binding

implicit none

contains

   subroutine get_d_delta_values(dval, delta_d, tthval, intval, fwhm, wave, delta2theta) bind(C, name='get_d_delta_values')
   use iso_c_binding
   use peak_mod
   use counts
   use variables, only: dataset
   implicit none
   real(c_float), intent(out), dimension(*) :: dval
   real(c_float), intent(out), dimension(*) :: delta_d
   real(c_float), intent(out), dimension(*) :: tthval
   real(c_float), intent(out), dimension(*) :: intval
   real(c_float), intent(out), dimension(*) :: fwhm
   real(c_double), intent(out)              :: wave
   real(c_double), value, intent(in)        :: delta2theta
   integer :: npk

   npk = numpeaks(pkind)
   dval(:npk)    = pkind(:)%getd()
   tthval(:npk)  = pkind(:)%getx()
   intval(:npk)  = pkind(:)%gety()
   delta_d(:npk) = deltadval(dval(:npk), real(delta2theta), dataset(1)%wave(1))
   fwhm(:npk)    = pkind(:)%fwhm
   wave = real(dataset(1)%wave(1),c_double)

   end subroutine get_d_delta_values
   
end module qualx_interop
