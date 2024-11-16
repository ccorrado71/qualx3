MODULE Counts

implicit none

private thvalue_pass_l, thvalue_count
interface thvalue
  module procedure thvalue_pass_l, thvalue_count
end interface

private dvalue_pass_l
interface dvalue
  module procedure dvalue_pass_l
end interface

   CONTAINS

   real elemental function dvalue_pass_l(thval,lambda) result(dval)
!
!  Convert 2theta in d
!
   USE trig_constants, only:dtor
   real, intent(in) :: thval
   real, intent(in) :: lambda
!
   dval = lambda/(2.0*sin(dtor*thval*0.5))
!
   end function dvalue_pass_l

  !----------------------------------------------------------------------

   real elemental function thvalue_pass_l(dval,lambda)  result(thval)
!
!  Converte da d a 2theta un valore
!
   USE trig_constants, only:rtod
   real, intent(in) :: dval,lambda
   real             :: arg
!
   arg = 0.5*lambda/dval
   if (arg <= 1) then
       thval = 2.0*asin(arg)*rtod
   else
       thval = -999
   endif
!
   end function thvalue_pass_l

  !----------------------------------------------------------------------

   function thvalue_count(counts,lambda)
   USE pointmod
   type(point_type), dimension(:), intent(in) :: counts
   real, intent(in)                           :: lambda
   type(point_type), dimension(size(counts))  :: thvalue_count
   integer                                    :: nc
   thvalue_count%x = thvalue_pass_l(counts%x,lambda)
   thvalue_count%y = counts%y
   nc = size(counts)
   if (thvalue_count(1)%x > thvalue_count(nc)%x) then
       thvalue_count(:) = thvalue_count(nc:1:-1)
   endif
   end function thvalue_count

  !----------------------------------------------------------------------

   real elemental function deltadval(dval,deltatt,lambda)
!
!  Converte intervallo in 2theta in intervallo in d
!
   real, intent(in) :: dval
   real, intent(in) :: deltatt
   real, intent(in) :: lambda
!
   deltadval = (3.14159265/180.0)*deltatt*dval**2*sqrt(1-lambda**2/(4*dval**2))/lambda
!
   end function deltadval

  !----------------------------------------------------------------------

   real elemental function deltatval(dval,deltadval,lambda)
!
!  Converte intervallo in 2theta in intervallo in d
!
   real, intent(in) :: dval
   real, intent(in) :: deltadval
   real, intent(in) :: lambda
!
   !deltadval = (3.14159265/180.0)*deltatt*dval**2*sqrt(1-lambda**2/(4*dval**2))/lambda
   deltatval = deltadval/((3.14159265/180.0)*dval**2*sqrt(1-lambda**2/(4*dval**2))/lambda)
!
   end function deltatval

  !----------------------------------------------------------------------

   real elemental function delta_from_lambda(delta2theta, lambda1, lambda2, dval)
!
!  Covert delta2theta from lambda1 to lambda2
!
   real, intent(in) :: delta2theta, lambda1, lambda2, dval
   delta_from_lambda = delta2theta * sqrt((4*dval**2-lambda1**2)/(4*dval**2-lambda2**2)) * (lambda2/lambda1)
   end function delta_from_lambda

END  MODULE Counts

