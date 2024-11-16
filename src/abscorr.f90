module absmod

implicit none

!Bragg-Brentano x-ray geometry - Suortti model (Suortti, 1972; https://doi.org/10.1107/S0021889872009707)
integer, parameter :: BB_SUOR = 3  

!Bragg-Brentano x-ray geometry - Pitschke model (Pitschke, 1993; https://doi.org/10.1017/S0885715600019412)
integer, parameter :: BB_PITS = 4 

contains

   function abs_correction(tthd,iabsr,a) result(absr)
!
!  Calculate absorption function
!
   use trig_constants, only: dtor
   real, dimension(:), intent(in) :: tthd
   integer, intent(in)            :: iabsr
   real, dimension(2), intent(in) :: a
   real, dimension(size(tthd))    :: absr
   real                           :: sin_1,sin_12,den
   integer                        :: i
!
   select case(iabsr)
     !case (1)    ! Combination of Sparks and Suortti models (Young in DBWS user's guide)
     !case (2)    ! Sparks model (Sparks et al., 1992)
     case (BB_SUOR)    
      den = a(1) + (1-a(1))*exp(-a(2))
      do i=1,size(tthd)
         absr(i) = (a(1) + (1-a(1))*exp(-a(2)/(sin(0.5 * tthd(i) * dtor))))/den
      enddo
      
     case (BB_PITS)    
      den = 1 - a(1) + a(1)*a(2)
      do i=1,size(tthd)
         sin_1 = 1 / sin(0.5 * tthd(i) * dtor)
         sin_12 = sin_1*sin_1
         absr(i) = (1 - a(1)*(sin_1 -a(2)*sin_12)) / den
      enddo

     case default 
      absr = 1.0
   end select
!
   end function abs_correction

!------------------------------------------------------------------------------------------------------

   subroutine absr_der(tthd,iabsr,a,dera,dertth)
!
!  Compute derivative respect variables a (dera) and 2theta (dertth)
!
   use trig_constants, only: dtor
   real, dimension(:), intent(in) :: tthd
   integer, intent(in)            :: iabsr
   real, dimension(2), intent(in) :: a
   real, dimension(2,size(tthd))  :: dera
   real, dimension(size(tthd))    :: dertth
   real                           :: den,csc,tthr,sin1,dterm,den2,thr
   real                           :: der_csc,dersin1,sin_1,sin_12,cot
   integer                        :: i
!
   select case (iabsr)
     case (BB_SUOR)    
      den = a(1) + (1-a(1))*exp(-a(2))
      do i=1,size(tthd)
         tthr = tthd(i) * dtor
         csc = 1 / sin(0.5 * tthr)
         dertth(i) = -((-1 + a(1))*a(2)*dtor*csc**3*sin(tthr))/(4.*den*exp(a(2)*csc))
      enddo
!
      do i=1,size(tthd)
         sin1 = sin(0.5 * tthd(i) * dtor)
         dterm = (1 + a(1)*(exp(a(2)) - 1))**2
         dera(1,i) = (exp(a(2)) - exp(a(2)*(2 - 1/sin1))) / dterm
         dera(2,i) = -(((a(1)-1)*exp(a(2)-a(2)/sin1)*(-1+sin1-a(1)*(-1+exp(a(2))+sin1-exp(a(2)/sin1)*sin1)))/(dterm*sin1))       
      enddo

     case (BB_PITS)    
      den = 1 - a(1) + a(1)*a(2)
      do i=1,size(tthd)
         thr = 0.5 * tthd(i) * dtor
         csc = 1 / sin(thr)
         cot = 1 / tan(thr)
         dersin1 = -(dtor*cot*csc)/2.            ! derivative of 1/sin(theta) respect 2theta
         der_csc = (a(1)*(-1 + 2*a(2)*csc))/den  ! derivative respect 1/sin(theta)
         dertth(i) = der_csc * dersin1 
      enddo
!
      den2 = den*den
      do i=1,size(tthd)
         sin_1 = 1 / sin(0.5 * tthd(i) * dtor)
         sin_12 = sin_1*sin_1
         dera(1,i) = (1 - sin_1 + a(2)*(-1 + sin_12))/den2
         dera(2,i) = (a(1)*(-1 + a(1)*(sin_1 - sin_12) + sin_12))/den2
      enddo
   end select
!
   end subroutine absr_der

!------------------------------------------------------------------------------------------------------

   subroutine print_abscorr(kpr,absf,abspar)
   integer, intent(in)            :: kpr
   integer, intent(in)            :: absf
   real, dimension(:), intent(in) :: abspar
   integer                        :: i
!
   if (absf > 0) then
       select case(absf)
         case (BB_SUOR)
           write(kpr,'(a)')'Absorption correction, Suortti formula'
         case (BB_PITS)
           write(kpr,'(a)')'Absorption correction, Pitschke formula'
       end select
       do i=1,2
          write(kpr,'(a,i0,10x,f8.3)')'a',i,abspar(i)
       enddo
   endif
!
   end subroutine print_abscorr

end module absmod
