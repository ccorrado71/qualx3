MODULE fractionm

   implicit none 

   type fract_type
     integer :: num,den
 
   contains
     procedure, private :: init
     procedure, private :: initr
     procedure :: string

   end type fract_type

   interface fractional
     module procedure constructor_int, constructor_real, constructor_realv
   end interface fractional

CONTAINS

   subroutine init(this,num,den)
   class(fract_type) :: this
   integer, intent(in), optional :: num
   integer, intent(in), optional :: den
   integer :: den1,num1
!
   if (present(num)) then
       num1 = num
   else
       num1 = 0
   endif
   if (present(den)) then
       den1 = den
   else
       den1 = 1
   endif
   this%num = num1
   this%den = den1
!
   end subroutine init

!---------------------------------------------------------------------------

   subroutine initr(this,rnum)
   class(fract_type)   :: this
   real, intent(in)    :: rnum
   integer             :: num,den
!
   if (rnum == 0.0) then
       call this%init(0,1)
   else
       call dec2frac(rnum,num,den)  
       call this%init(num,den)
   endif
!
   end subroutine initr

!---------------------------------------------------------------------------

   function constructor_int(num,den)
   type(fract_type) :: constructor_int
   integer, intent(in) :: num,den
   call constructor_int%init(num,den)
   end function constructor_int

!---------------------------------------------------------------------------

   function constructor_real(rnum)
   type(fract_type) :: constructor_real
   real, intent(in) :: rnum
   call constructor_real%initr(rnum)
   end function constructor_real

!---------------------------------------------------------------------------

   function constructor_realv(rnum)
   real, dimension(:), intent(in) :: rnum
   type(fract_type), dimension(size(rnum)) :: constructor_realv
   integer                        :: i
   do i=1,size(rnum)
      call constructor_realv(i)%initr(rnum(i))
   enddo
   end function constructor_realv

!---------------------------------------------------------------------------

   function simplify(this)
   type(fract_type) :: this,simplify
   integer          :: gcdnum
!
   gcdnum = gcd(this%num,this%den)
   simplify%num = this%num/gcdnum
   simplify%den = this%den/gcdnum
!
   end function simplify

!---------------------------------------------------------------------------

   function string(this) 
   class(fract_type) :: this
   character(len=10) :: string
!
   if (this%num == 0) then
       string ='0'
   elseif (this%num == this%den) then
       string ='1'
   else
       write(string,'(i0,"/",i0)') this%num,this%den
   endif
!
   end function string

!---------------------------------------------------------------------------

   function gcd(v, t)
!
!  Compute Greatest common divisor
!
   integer :: gcd
   integer, intent(in) :: v, t
   integer :: c, b, a
 
   b = t
   a = v
   do
      c = mod(a, b)
      if ( c == 0) exit
      a = b
      b = c
   end do
   gcd = b ! abs(b)
   end function gcd

!---------------------------------------------------------------------------

   integer function gcdv(vet)
!
!  Compute greatest common divisor of array of integers
!
   integer, dimension(:), intent(in) :: vet
   integer                           :: i
!
   gcdv = vet(1)
   do i=2,size(vet)
      gcdv = gcd(vet(i),gcdv)
   enddo
!
   end function gcdv
   
!---------------------------------------------------------------------------

   integer function lcm(a,b)
!
!  Compute least common multiple of 2 integers
!
   integer, intent(in) :: a,b
   lcm = a*b / gcd(a,b)
   end function lcm

!---------------------------------------------------------------------------

   integer function lcmv(vet)
!
!  Compute least common multiple of array of integers
!
   integer, dimension(:) :: vet
   integer               :: i,nv
   nv = size(vet)
   if (nv > 1) then
       lcmv = lcm(vet(1),vet(2))
       do i=3,size(vet)
          lcmv = lcm(lcmv,vet(i))
       enddo
   else
       lcmv = vet(1)
   endif
   end function lcmv

!---------------------------------------------------------------------------

   subroutine dec2frac(xnum,num,den)
!
!  Programmer:   David G. Simpson
!                NASA Goddard Space Flight Center
!                Greenbelt, Maryland  20771
!
!  Description:  This program will convert a decimal number to a fraction (i.e. rational number) with increasing
!                degrees of accuracy.  In other words, in computes a series of rational number approximations to
!                any given decimal number.
!
!
!  Note:         The algorithm is taken from "An Atlas of Functions" by Spanier and Oldham, Springer-Verlag, 1987, pp. 665-667.
!

   IMPLICIT NONE

   real, intent(in) :: xnum
   integer, intent(out) :: num,den
      
   integer, parameter :: dp = kind(1.d0)
   !real(dp), PARAMETER :: TOL = 1.0D-10       ! default value of tolerance se xnum fosse in doppia
   real(dp), PARAMETER :: TOL = 1.0D-5        ! default value of tolerance
   real(dp) :: X, NU, R, T, EPS, M
   INTEGER :: N1, N2, D1, D2
   LOGICAL :: SGN

      x = xnum
!
!     Save the sign of X, and make it positive.
      NU = X                                ! make a local copy of X
      SGN = NU .LT. 0.0D0                   ! save sign
      NU = ABS(NU)                          ! remove sign from X
!
!     Compute the rational equivalent of X.
      D1 = 1
      D2 = 1
      N1 = INT(NU)
      N2 = N1 + 1
      GO TO 300
  100 IF (R .GT. 1.0D0) GO TO 200
      R = 1.0D0/R
  200 N2 = N2 + N1*INT(R)
      D2 = D2 + D1*INT(R)
      N1 = N1 + N2
      D1 = D1 + D2
  300 R = 0.0D0
      IF (NU*D1 .EQ. DBLE(N1)) GO TO 400
      R = (N2-NU*D2)/(NU*D1-N1)
      IF (R .GT. 1.0D0) GO TO 400
      T = N2
      N2 = N1
      !N1 = T   !correction by C.Cuocci
      N1 = INT(T)
      T = D2
      D2 = D1
      !D1 = T   !correction by C.Cuocci
      D1 = INT(T)
  400 continue
!!  400 IF (SGN) THEN
!!         WRITE (UNIT=*, FMT='(1X,1H-,I0,1H/,I0)', ADVANCE='NO') N1, D1
!!      ELSE
!!         WRITE (UNIT=*, FMT='(1X,I0,1H/,I0)', ADVANCE='NO') N1, D1
!!      END IF
!!      WRITE (UNIT=*, FMT='(5X,1H=,ES24.15)', ADVANCE='NO') DBLE(N1)/DBLE(D1)
      EPS = ABS(1.0D0 - (N1/(NU*D1)))
      IF (EPS .LE. TOL) GO TO 600
      M = 1.0D0
  500 M = 10*M
      IF (M*EPS .LT. 1.0D0) GO TO 500
      EPS = (1.0D0/M)*INT(0.5D0+M*EPS)
  600 continue
!!  600 WRITE (UNIT=*, FMT='(5X,A,ES10.3)') 'error = ', EPS
!
      num = n1
      den = d1
!
      IF (EPS .LE. TOL) return
      IF (R .NE. 0.0D0) GO TO 100

   return

   end subroutine dec2frac

END MODULE fractionm
