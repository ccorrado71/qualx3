 MODULE math_util

 real, parameter, private :: EPSS = 1.0E-05

 private :: equal_vector_r, equal_vector_i
 interface equal_vector
     module procedure equal_vector_r, equal_vector_i
 end interface

 private :: equal_matrix_r, equal_matrix_i
 interface equal_matrix
     module procedure equal_matrix_r, equal_matrix_i
 end interface

 private :: is_integer_noeps, is_integer_eps
 interface is_integer
     module procedure is_integer_noeps, is_integer_eps
 end interface

 private exprep_dp, exprep_sp
 interface exprep
    module procedure exprep_dp, exprep_sp
 end interface exprep

 contains

   logical function is_nan(x)
#ifdef __INTEL_COMPILER
   USE, intrinsic :: ieee_arithmetic
   real, intent(in) :: x
   is_nan = ieee_is_nan(x) 
#else
   USE strutil
   real, intent(in) :: x
   is_nan = (trim(r_to_s(x)) == 'NaN')
#endif
   end function is_nan

!---------------------------------------------------------------------------------------------------------

   logical function is_inf(x)
#ifdef __INTEL_COMPILER
   USE, intrinsic :: ieee_arithmetic
   real, intent(in) :: x
   is_inf = .not.ieee_is_finite(x)
#else
!!!TODO
#endif
   end function is_inf

!---------------------------------------------------------------------------------------------------------
  
   function derivative(a)  result(der)
!
!  First derivative of vector using 2-point central difference
!
   real, dimension(:), intent(in) :: a
   real, dimension(size(a))       :: der
   integer                        :: n
!
   n = size(a)
   der(1) = a(2) - a(1)
   der(n) = a(n) - a(n-1)
!
   der(2:n-1) = a(3:n) - a(1:n-2) / 2
!
   end function derivative

!---------------------------------------------------------------------------------------------------------

   function straight_line(x1,y1,x2,y2) result(line)
!
!  Calculate straigt line between 2 points: y=line(1)*x + line(2)
!
   real, intent(in)   :: x1,y1,x2,y2
   real, dimension(2) :: line
!
   line(1) = (y2 -y1) / (x2 -x1)
   line(2) = -line(1)*x1 + y1
!
   end function straight_line

!---------------------------------------------------------------------------------------------------------

   function line_line_intersection(l1,l2,ier)  result(xp)
!
!  Intersection of two lines l1(1)x+l1(2), l2(1)x+l2(2)
!
   real, dimension(2), intent(in) :: l1,l2
   integer, intent(out)           :: ier
   real, dimension(2)             :: xp
!
   if (abs(l1(1) - l2(1)) <= epsilon(1.0)) then  ! parallel lines
       ier = 1
       xp(:) = 0
   else
       ier = 0
       xp(1) = (l2(2) - l1(2)) / (l1(1) - l2(1))
       xp(2) = (l1(1)*l2(2) - l2(1)*l1(2)) / (l1(1) - l2(1))
   endif
!
   end function line_line_intersection

!---------------------------------------------------------------------------------------------------------

   real function integrate(x,y,n1,n2)  result(area)
!
!  Integrate array in range n1-n2 with n2 >= n1
!
   real, dimension(:), intent(in) :: x,y
   integer, intent(in)            :: n1,n2
!
   if (n1 == n2) then
       area = y(n1)
   else
       area = 0.5 * sum((x(n1+1:n2) - x(n1:n2-1)) * (y(n1+1:n2) + y(n1:n2-1)))
   endif
!
   end function integrate

!---------------------------------------------------------------------------

   logical function equal_matrix_r(mat1,mat2)
!
!  mat1 is equal to mat2 ?
!
   real, dimension(:,:), intent(in) :: mat1,mat2
   equal_matrix_r = all (abs(mat1 - mat2) < EPSS)
   end function equal_matrix_r

!---------------------------------------------------------------------------

   logical function equal_matrix_i(mat1,mat2)
!
!  mat1 is equal to mat2 ?
!
   integer, dimension(:,:), intent(in) :: mat1,mat2
   equal_matrix_i = all (abs(mat1 - mat2) == 0)
   end function equal_matrix_i

!---------------------------------------------------------------------------

   logical function equal_vector_r(vet1,vet2)
!
!  vet1 is equal to vet2 ?
!
   real, dimension(:), intent(in) :: vet1,vet2
   equal_vector_r = all (abs(vet1 - vet2) < EPSS)
   end function equal_vector_r

!---------------------------------------------------------------------------

   logical function equal_vector_i(vet1,vet2)
   integer, dimension(:), intent(in) :: vet1,vet2
   equal_vector_i = all (abs(vet1 - vet2) == 0)
   end function equal_vector_i

!------------------------------------------------------------------------------------------

   elemental logical function is_integer_noeps(rnum)
   real, intent(in) :: rnum
   is_integer_noeps = ceiling(rnum) == rnum
   end function is_integer_noeps

!------------------------------------------------------------------------------------------

   elemental logical function is_integer_eps(rnum,eps)
   real, intent(in) :: rnum,eps
   is_integer_eps = abs(nint(rnum) - rnum) <= eps
   end function is_integer_eps

!------------------------------------------------------------------------------------------

   real function determinante_22(A)
!
   real, dimension(2,2) :: A
!
   determinante_22 = A(1,1)*A(2,2) - A(1,2)*A(2,1) 
!
   end function determinante_22

!------------------------------------------------------------------------------------------

   real function determinante(mat)
!
   real, dimension(3,3) :: mat
!
   determinante = mat(1,1)*mat(2,2)*mat(3,3) + mat(1,2)*mat(2,3)*mat(3,1) + mat(1,3)*mat(2,1)*mat(3,2) &
                - mat(3,1)*mat(2,2)*mat(1,3) - mat(3,2)*mat(2,3)*mat(1,1) - mat(3,3)*mat(2,1)*mat(1,2)
!
   end function determinante

!------------------------------------------------------------------------------------------

   real function determinante_44(A)
!
   real, dimension(4,4) :: A
!
   determinante_44 =   &
     A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
   - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
   + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
   - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))) 
!
   end function determinante_44

!------------------------------------------------------------------------------------------

   function matinv2(A,ok_flag) result(B)
   !
   !Performs a direct calculation of the inverse of a 3×3 matrix.
   real, intent(in)               :: A(2,2)   ! Matrix
   logical, intent(out), optional :: ok_flag  ! .TRUE. if the input matrix could be inverted, and .FALSE. if the input matrix is singular.
   real                           :: B(2,2)   ! Inverse matrix
   real                           :: det, detinv
   real, parameter                :: EPS = epsilon(1.0)

   ! Calculate the inverse determinant of the matrix
   det = determinante_22(A)
   if (abs(det) < EPS) then
       if (present(ok_flag)) ok_flag = .false.
       B(:,:) = 0
       return
   endif
   detinv =  1 / det

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * A(2,2)
   B(2,1) = -detinv * A(2,1)
   B(1,2) = -detinv * A(1,2)
   B(2,2) = +detinv * A(1,1)

   if (present(ok_flag)) ok_flag = .true.

   end function matinv2

!------------------------------------------------------------------------------------------

   function matinv3(A,ok_flag) result(B)
   !
   !Performs a direct calculation of the inverse of a 3×3 matrix.
   real, intent(in)               :: A(3,3)   ! Matrix
   logical, intent(out), optional :: ok_flag  ! .TRUE. if the input matrix could be inverted, and .FALSE. if the input matrix is singular.
   real                           :: B(3,3)   ! Inverse matrix
   real                           :: det, detinv
   real, parameter                :: EPS = epsilon(1.0)

   ! Calculate the inverse determinant of the matrix
   det = determinante(A)
   if (abs(det) < EPS) then
       if (present(ok_flag)) ok_flag = .false.
       B(:,:) = 0
       return
   endif
   detinv =  1 / det

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
   B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
   B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
   B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
   B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
   B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
   B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
   B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
   B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))

   if (present(ok_flag)) ok_flag = .true.

   end function matinv3

!------------------------------------------------------------------------------------------

   function matinv4(A,ok_flag) result(B)
   !
   !Performs a direct calculation of the inverse of a 3×3 matrix.
   real, intent(in)               :: A(4,4)   ! Matrix
   logical, intent(out), optional :: ok_flag  ! .TRUE. if the input matrix could be inverted, and .FALSE. if the input matrix is singular.
   real                           :: B(4,4)   ! Inverse matrix
   real                           :: det, detinv
   real, parameter                :: EPS = epsilon(1.0)

   ! Calculate the inverse determinant of the matrix
   det = determinante_44(A)
   if (abs(det) < EPS) then
       if (present(ok_flag)) ok_flag = .false.
       B(:,:) = 0
       return
   endif
   detinv =  1 / det

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)   &
                    -A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)   &
                    -A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)   &
                    -A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)   &
                    -A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)   &
                    -A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)   &
                    -A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)   &
                    -A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)   &
                    -A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)   &
                    -A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)   &
                    -A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)   &
                    -A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)   &
                    -A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)   &
                    -A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)   &
                    -A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)   &
                    -A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)   &
                    -A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))

   if (present(ok_flag)) ok_flag = .true.

   end function matinv4

!------------------------------------------------------------------------------------------

   subroutine real_to_complex(x, c)
   real, intent(in)     :: x(:)
   complex, intent(out) :: c(:,:,:)
   integer              :: i, j, k, idx

   idx = 1
   do i = 1, size(c,3)
     do j = 1, size(c,2)
       do k = 1, size(c,1)
         c(k,j,i) = cmplx(x(idx), x(idx+1))
         idx = idx + 2
       end do
     end do
   end do

   end subroutine real_to_complex

!------------------------------------------------------------------------------------------

   subroutine complex_to_real(c, x)
   complex, intent(in) :: c(:,:,:)
   real, intent(out)   :: x(:)
   integer             :: i, j, k, idx

   idx = 1
   do i = 1, size(c,3)
     do j = 1, size(c,2)
       do k = 1, size(c,1)
         x(idx) = real(c(k,j,i))
         x(idx+1) = aimag(c(k,j,i))
         idx = idx + 2
       end do
     end do
   end do

   end subroutine complex_to_real

!------------------------------------------------------------------------------------------

   pure function exprep_dp(x) result(f)
   
   use, intrinsic :: ieee_exceptions
   use type_constants, only: dp
   
   implicit none
   
   real(dp), intent(in) :: x
   real(dp) :: f
   
   logical,dimension(2) :: flags
   type(ieee_flag_type),parameter,dimension(2) :: out_of_range = [ieee_overflow,ieee_underflow]
   
   call ieee_set_halting_mode(out_of_range,.false.)
   
   f = exp(x)
   
   call ieee_get_flag(out_of_range,flags)
   if (any(flags)) then
     call ieee_set_flag(out_of_range,.false.)
     if (flags(1)) then
       f = huge(1.0_dp)
     else
       f = 0.0_dp
     end if
   end if
   
   end function exprep_dp

!-------------------------------------------------------------------------------------------------------

   pure function exprep_sp(x) result(f)
   
   use, intrinsic :: ieee_exceptions
   
   implicit none
   
   real, intent(in) :: x
   real :: f
   
   logical,dimension(2) :: flags
   type(ieee_flag_type),parameter,dimension(2) :: out_of_range = [ieee_overflow,ieee_underflow]
   
   call ieee_set_halting_mode(out_of_range,.false.)
   
   f = exp(x)
   
   call ieee_get_flag(out_of_range,flags)
   if (any(flags)) then
     call ieee_set_flag(out_of_range,.false.)
     if (flags(1)) then
       f = huge(1.0)
     else
       f = 0.0
     end if
   end if
   
   end function exprep_sp

!-------------------------------------------------------------------------------------------------------

   function exprep1(rval) result(exp_val)
!  This function replaces exp to avoid under- and overflows.
!  Note that the maximum and minimum values of
!  EXPREP are such that they has no effect on the algorithm.
                  
   real ,intent(in) :: rval
   real             :: exp_val
   real             :: maxexp= huge(1.0)
   real             :: maxarg=log(huge(1.0))
   real             :: minarg=log(tiny(1.0))
!     
   if (rval > maxarg) then
       exp_val = maxexp
   elseif (rval < minarg ) then 
       exp_val = 0.0
   else
       exp_val = exp(rval)
   endif
!
   end function exprep1

 END MODULE math_util

module Fattoriale_Bezier
   type punto
    real(8) :: x
    real(8) :: y
   end type punto

    !
    interface operator(.Fac.)
        module procedure factorial_r,factorial_i
    end interface
    !
    interface operator (.Bnm.)
        module procedure binomiale_i
    end interface
    !
    interface operator(.Bezier.)
        module procedure make_Bezier
    end interface
    !
    private :: factorial_r,factorial_i, &
               binomiale_i !!!!,make_Bezier
    !
contains
    !
    !
    function make_Bezier(cc,s)Result(b)
        !
        !   Calcola il polinomio di Bezier nel punto
        !   s compreso tra 0 ed 1.
        !   Il vettore Cc contiene i coefficienti e quindi
        !   determina il grado del polinomio di Bezier che
        !   e' il numero di coefficienti meno 1.
        !
        implicit none

        type(punto) :: b
        real(8),intent(in)::s
    !!!    real(8),dimension(0:,:),intent(in)::Cc
        type(punto), dimension(0:), intent(in)::Cc
        real(8), dimension(size(cc,1)) :: bern
        integer::k,N
        N=Size(Cc,1)-1
        bern = (/( (N.Bnm.k)*s**k*(1-s)**(N-k), k=0,N )/)
        b%x = dot_product(Cc%x,bern)
        b%y = dot_product(Cc%y,bern)
        return
    end function make_Bezier

    !
    recursive function Factorial_i(k)result(Ff)
        implicit none
        integer,intent(in)::k
        real(8)::Ff
        if(k.le.0) then
            Ff=1.D0
        else
            Ff=Factorial_r(k-1.D0)*k
        end if
        return
    end function Factorial_i
    !
    recursive function Factorial_r(k)result(Ff)
        implicit none
        real(8),intent(in)::k
        real(8)::Ff
        if(k.le.0.D0) then
            Ff=1.D0
        else
            Ff=Factorial_r(k-1.D0)*k
        end if
        return
    end function Factorial_r
    !
    recursive function Binomiale_i(N,p)result(Bn)
        implicit none
        integer,intent(in)::N,p
        real(8)::Bn
        if(p.lt.0.or.p.gt.N.or.N.lt.0) Then
            Bn=0.d0
        else if(p.eq.0.or.p.eq.N) then
            Bn=1.d0
        else if(p.gt.N/2) then
            Bn=Binomiale_i(N,N-p)
        else
            Bn=Binomiale_i(N,p-1)*(N-p+1)/p
        end if
        return
    end function Binomiale_i
    !
end module Fattoriale_Bezier
