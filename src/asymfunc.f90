module asymfunc

implicit none

integer, parameter :: RIETVELD_TYPE = 1
integer, parameter :: BERAR_BALDINOZZI_TYPE = 2
integer, parameter :: PEARSON_TYPE = 3

contains

   subroutine asym_berar_baldinozzi(asym,dt,th,fwhm,a0,a1,b0,b1,der)
   USE trig_constants
   real, dimension(:), intent(out)             :: asym
   real, dimension(:), intent(in)              :: dt          ! 2theta_i corrected - 2theta reflection
   real, intent(in)                            :: th          ! 2theta of reflection
   real, intent(in)                            :: fwhm        ! fwhm of reflection
   real, dimension(:,:), intent(out), optional :: der         ! der th ref.,th count,fwhm,a0,a1,b0,b1
   real                                        :: a0,a1,b0,b1 ! independent parameters
   real                                        :: a,b,tant,tan2t,sdt,dtw
   integer                                     :: i
   real :: esdt,dtant_th,dtan2t_th,da_th,db_th,ddtw_dt,dasym_a,dasym_b,dasym_dtw,ddtw_fwhm
!
   tant = 1/tan(dtor*th/2)
   tan2t = 1/tan(dtor*th)
   a = a0*tant + a1*tan2t
   b = b0*tant + b1*tan2t
   if (present(der)) then
       dtant_th =  -0.5*dtor*tant**2          ! d(tant)/d(th)
       dtan2t_th = -dtor*tan2t**2             ! d(tan2t)/d(th)
       da_th = a0*dtant_th + a1*dtan2t_th     ! d(a)/d(th)
       db_th = b0*dtant_th + b1*dtan2t_th     ! d(b)/d(th)
       ddtw_dt = 1/fwhm                       ! d(dtw)/d(dt)
       do i=1,size(dt)
          dtw = dt(i)/fwhm
          sdt = dtw * dtw
          esdt = exp(-sdt)

          asym(i) = 1 + dtw * esdt * (2*a + b*(8*sdt-12))
          
          dasym_a = 2*dtw*esdt                 ! d(asym)/d(a)
          dasym_b = dtw*(8*sdt-12)*esdt        ! d(asym)/d(b)

          der(i,4) = dasym_a*tant              ! d(asym)/d(a0)
          der(i,5) = dasym_a*tan2t             ! d(asym)/d(a1)
          der(i,6) = dasym_b*tant              ! d(asym)/d(b0)
          der(i,7) = dasym_b*tan2t             ! d(asym)/d(b1)

          dasym_dtw = esdt*(2*(a - 2*a*dtw**2 - 2*b*(3 + 4*dtw**2*(-3 + dtw**2))))   ! d(asym)/d(dtw)
          ddtw_fwhm = -dt(i)/fwhm**2                                                 ! d(dtw)/dfwhm
          der(i,3) = dasym_dtw * ddtw_fwhm                                           ! d(asym)/d(fwhm)

          der(i,1) = dasym_dtw*(-ddtw_dt) + dasym_a*da_th + dasym_b*db_th            ! d(asym)/d(th reflection) 
          der(i,2) = dasym_dtw*ddtw_dt                                               ! d(asym)/d(th count)
       enddo
   else
       do i=1,size(dt)
          dtw = dt(i)/fwhm
          sdt = dtw * dtw
          asym(i) = 1 + dtw * exp(-sdt) * (2*a + b*(8*sdt-12))
       enddo
   endif
!
   end subroutine asym_berar_baldinozzi

!----------------------------------------------------------------------------------------------

   subroutine asym_rietveld(asym,dt,th,apar,der)
   USE trig_constants
   real, dimension(:), intent(out)             :: asym
   real, dimension(:), intent(in)              :: dt
   real, intent(in)                            :: th
   real, intent(in)                            :: apar
   real, dimension(:,:), intent(out), optional :: der  ! der th ref., th count, fwhm, a
   real                                        :: rdeg,tdeg_1
   integer                                     :: i
!
   tdeg_1 = 1/tan(0.5 * th * dtor)
   rdeg = 0.5 * th * dtor
   if (present(der)) then
       do i=1,size(dt)
          asym(i) = 1-apar*sign(1.0,dt(i))*dt(i)*dt(i)*tdeg_1
          der(i,1) =  apar*sign(1.0,dt(i))*dt(i)*(rtod*4.0*tdeg_1 + dt(i)/(sin(rdeg)**2))   ! d(asym)/d(th reflection)
          der(i,2) = -2.0*apar*sign(1.0,dt(i))*dt(i)*tdeg_1                                 ! d(asym)/d(th count)
          der(i,3) = 0.0                                                                    ! d(asym)/d(fwhm)
          der(i,4) = -sign(1.0,dt(i))*dt(i)*dt(i)*tdeg_1                                    ! d(asym)/d(a)
       enddo
   else
       asym(:size(dt)) = 1-apar*sign(1.0,dt)*dt*dt*tdeg_1
   endif
!
   end subroutine asym_rietveld

!----------------------------------------------------------------------------------------------

   subroutine asym_pearson(asym,dt,fwhm,beta,apar,der)
   USE trig_constants
   real, dimension(:), intent(out)             :: asym
   real, dimension(:), intent(in)              :: dt          ! 2theta_i corrected - 2theta reflection
   real, intent(in)                            :: fwhm        ! fwhm of reflection
   real, intent(in)                            :: beta        ! parameter of pearson function
   real, intent(in)                            :: apar
   real, dimension(:,:), intent(out), optional :: der         ! der th ref.,th count,fwhm,apar
   real :: pconst,cpear2,ccas,dt2
   integer :: i
!
   pconst = 2.0**(1.0/beta) - 1.0
   cpear2 = (4.0 * pconst) / (fwhm*fwhm)
   if (present(der)) then
       der(:,:) = 0
       do i=1,size(dt)
          dt2 = dt(i)*dt(i)
          ccas=1.0/cpear2+dt2
          asym(i)= 1.0 + apar*dt2*dt(i)/(ccas**1.5)
          der(i,2) = -3.0*apar*dt2*(dt2-ccas)/ccas**2.5   ! der(asym)/d(th count)
          der(i,4) = dt2*dt(i)/(ccas**1.5)                ! d(asym)/d(a)
       enddo
   else
       do i=1,size(dt)
          ccas=1.0/cpear2+dt(i)*dt(i)
          asym(i)= 1.0 + apar*dt(i)**3/(ccas**1.5)
       enddo
   endif
!
   end subroutine asym_pearson
   
!----------------------------------------------------------------------------------------------

   subroutine compute_asymmetry(atype,asym,dt,th,fwhm,apar,beta,der)
   integer, intent(in)                         :: atype
   real, dimension(:), intent(out)             :: asym
   real, dimension(:), intent(in)              :: dt
   real, intent(in)                            :: th
   real, intent(in)                            :: fwhm
   real, dimension(4), intent(in)              :: apar
   real, intent(in), optional                  :: beta
   real, dimension(:,:), intent(out), optional :: der
!
   select case (atype)
     case (RIETVELD_TYPE)
       if (present(der)) then
           call asym_rietveld(asym,dt,th,apar(1),der)
       else
           call asym_rietveld(asym,dt,th,apar(1))
       endif

     case (BERAR_BALDINOZZI_TYPE)
       if (present(der)) then
           call asym_berar_baldinozzi(asym,dt,th,fwhm,apar(1),apar(2),apar(3),apar(4),der)
       else
           call asym_berar_baldinozzi(asym,dt,th,fwhm,apar(1),apar(2),apar(3),apar(4))
       endif

     case (PEARSON_TYPE)
       if (present(der)) then
           call asym_pearson(asym,dt,fwhm,beta,apar(1),der)
       else
           call asym_pearson(asym,dt,fwhm,beta,apar(1))
       endif
   end select
!
   end subroutine compute_asymmetry

end module asymfunc
