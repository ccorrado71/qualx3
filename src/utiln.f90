  MODULE CCRYST

  USE trig_constants

  implicit none

  CONTAINS


   function beta_from_uij(uij,gr,cellr)   result(bij)
!
!  Calcola i termini bij del tensore beta da uij
!
   real, dimension(6), intent(in)   :: uij    ! u11, u22, u33, u12, u13, u23
   real, dimension(3,3), intent(in) :: gr     ! matrice metrica reciproca
   real, dimension(6), intent(in)   :: cellr  ! cella reciproca
   real, dimension(6)               :: bij
!
   bij(1) = twopis * uij(1) * gr(1,1)                  ! b11
   bij(2) = twopis * uij(2) * gr(2,2)                  ! b22
   bij(3) = twopis * uij(3) * gr(3,3)                  ! b33
   bij(4) = twopis * uij(4) * cellr(1) * cellr(2)      ! b12
   bij(5) = twopis * uij(5) * cellr(1) * cellr(3)      ! b13
   bij(6) = twopis * uij(6) * cellr(2) * cellr(3)      ! b23
!
   end function beta_from_uij

!--------------------------------------------------------------------

   function u_from_bij(bij,gr,cellr)   result(uij)
!
!  Calcola i termini uij  da bij
!
   real, dimension(6), intent(in)   :: bij    ! u11, u22, u33, u12, u13, u23
   real, dimension(3,3), intent(in) :: gr     ! matrice metrica reciproca
   real, dimension(6), intent(in)   :: cellr  ! cella reciproca
   real, dimension(6)               :: uij
!
   uij(1) = bij(1) / (twopis * gr(1,1))                  ! u11
   uij(2) = bij(2) / (twopis * gr(2,2))                  ! u22
   uij(3) = bij(3) / (twopis * gr(3,3))                  ! u33
   uij(4) = bij(4) / (twopis * cellr(1) * cellr(2))      ! u12
   uij(5) = bij(5) / (twopis * cellr(1) * cellr(3))      ! u13
   uij(6) = bij(6) / (twopis * cellr(2) * cellr(3))      ! u23
!
   end function u_from_bij

!--------------------------------------------------------------------

   function b_from_bij(bij,gr,cellr)   result(uij)
!
!  Convert bij in B
!
   real, dimension(6), intent(in)   :: bij    ! u11, u22, u33, u12, u13, u23
   real, dimension(3,3), intent(in) :: gr     ! matrice metrica reciproca
   real, dimension(6), intent(in)   :: cellr  ! cella reciproca
   real, dimension(6)               :: uij
!
   uij(1) = 4.0 * bij(1) / (gr(1,1))                  ! u11
   uij(2) = 4.0 * bij(2) / (gr(2,2))                  ! u22
   uij(3) = 4.0 * bij(3) / (gr(3,3))                  ! u33
   uij(4) = 4.0 * bij(4) / (cellr(1) * cellr(2))      ! u12
   uij(5) = 4.0 * bij(5) / (cellr(1) * cellr(3))      ! u13
   uij(6) = 4.0 * bij(6) / (cellr(2) * cellr(3))      ! u23
!
   end function b_from_bij

!--------------------------------------------------------------------

   real function bequiv_from_beta(bij,gg) result(bequiv)
!
!  Calcola b equivalente da beta Bequiv = (4/3)*Tr(beta*G)
!
   real, dimension(6), intent(in)   :: bij  ! tensore beta =  b11,b22,b33,b12,b13,b23
   real, dimension(3,3), intent(in) :: gg   ! matrice metrica
!
   bequiv = (4.0 / 3.0) * (bij(1)*gg(1,1) + bij(2)*gg(2,2) + bij(3)*gg(3,3) +  &
            2*bij(4)*gg(1,2) + 2*bij(5)*gg(1,3) + 2*bij(6)*gg(2,3))
!
   end function bequiv_from_beta

!--------------------------------------------------------------------

   logical function aniso_adp_is_ok(bij,gr,cellr)  result(ok)
!
!  Check relationship between adp    
!
!corr             USE nr
!corr             USE nrtype
             USE math_util
   real, dimension(6), intent(in) :: bij  ! tensore beta =  b11,b22,b33,b12,b13,b23
   real, dimension(3,3), intent(in) :: gr     ! matrice metrica reciproca
   real, dimension(6), intent(in)   :: cellr  ! cella reciproca
   real, dimension(3,3)           :: bmat !,bmat2
   real, dimension(6) :: bval
!corr   integer                        :: i,j
!corr   !real, parameter                :: EPS = epsilon(1.0)
!corr       real(dp), dimension(3) :: indx
!corr       integer :: ier
!corr       real(dp), dimension(3,3) :: bmatc
!corr       real :: EPS=epsilon(0.0)
!
   ok = .true.
!
!  Test on determinant > 0
!  If det > 0, matrix is positive definite and eigenvalues are positive 
   bval = b_from_bij(bij,gr,cellr)   ! convert betaij in Bij to have large values
   bmat = reshape([bval(1),bval(4),bval(5),bval(4),bval(2),bval(6),bval(5),bval(6),bval(3)],(/3,3/))
   ok = determinante(bmat) > 0
!corr      if(abs(determinante(bmat)) <= EPS) write(0,*)'DET=',determinante(bmat),bij(1)
!corr      if(determinante(bmat) <= 0) write(0,*)'DET NEG=',determinante(bmat),bij(1)
!corr      bmatc = bmat
!corr      call choldc(bmatc,indx,ier)
!corr      if (ier == -1)then
!corr          write(0,*)'Singular matrix',bij(1),determinante(bmat)
!corr      endif
!corr         return
!corr      !ok = ier == 0
!corr      
!corr   bmat2 = bmat*bmat
!corr!
!corr!  Bii > 0
!corr   do i=1,3
!corr      ok = bmat(i,i) > 0
!corr      !if (.not.ok) then
!corr      !        write(0,*)'RETURN WITH 1=',bmat(i,i)
!corr      !    return
!corr      !endif
!corr      if (.not.ok) return
!corr   enddo
!corr!
!corr!  BiiBij > Bij^2
!corr   do i=1,3
!corr      do j=1,3
!corr         if (i==j) cycle
!corr         ok = bmat(i,i)*bmat(i,j) > bmat2(i,j)
!corr         !if (.not.ok) then
!corr         !     write(0,*)'RETURN WITH 2=',bmat(i,i)*bmat(i,j),' < ',bmat2(i,j)
!corr         !     return
!corr         !endif
!corr         if (.not.ok) return
!corr      enddo
!corr   enddo
!corr!
!corr   ok = bmat(1,1)*bmat(2,2)*bmat(3,3) + bmat2(1,2)*bmat2(1,3)*bmat2(2,3) >   &
!corr        bmat(1,1)*bmat2(2,3) + bmat(2,2)*bmat2(1,3) + bmat(3,3)*bmat2(1,2)
!corr       !  if (.not.ok) then
!corr       !       write(0,*)'RETURN WITH 3=',bmat(1,1)*bmat(2,2)*bmat(3,3) + bmat2(1,2)*bmat2(1,3)*bmat2(2,3), ' < ',   &
!corr       ! bmat(1,1)*bmat2(2,3) + bmat(2,2)*bmat2(1,3) + bmat(3,3)*bmat2(1,2)
!corr       !       return
!corr       !  endif
!corr!
   end function aniso_adp_is_ok

!--------------------------------------------------------------------

   real elemental function b_from_u(u) result(b)
!
!  Calcola b da u. b=8*pi^2*u
!
   real, intent(in) :: u
!
   b = 4.0*twopis*u   
!
   end function b_from_u

!--------------------------------------------------------------------

   real elemental function u_from_b(b) result(u)
!
!  Calcola b da u. b=8*pi^2*u
!
   real, intent(in) :: b
   real, parameter :: const=1/(4.0*twopis)
!
   u = const*b
!
   end function u_from_b
  END MODULE CCRYST

MODULE PRNUTIL

 implicit none

 CONTAINS

   subroutine print_matrix(mat,kpr)
   integer, dimension(:,:), intent(in) :: mat
   integer, intent(in)                 :: kpr
   integer, dimension(2)               :: lb,ub
   integer                             :: i
   lb = lbound(mat)
   ub = ubound(mat)
   do i=lb(1),ub(1)
      write(kpr,'(*(i6))')mat(i,:)
   enddo
   end subroutine print_matrix

!----------------------------------------------------------------------------------

   subroutine stampa_matrice(amatr,stringpar,ndigit,kpr,title,numcolo)
   USE type_constants, only:DP
   USE strutil
!
   real(DP), dimension(:,:), intent(in)           :: amatr     ! matrice da stampare
   character(len=*), dimension(:), intent(in)        :: stringpar ! stringa associata alla riga
   integer, intent(in)                               :: ndigit    ! numero di cifre da stampare per numero
   integer,                          intent(in)      :: kpr       ! unita' di stampa
   character(len=*), intent(in), optional            :: title     ! eventuale titolo 
   integer,optional                                  :: numcolo   ! numero di colonne da usare per la stampa
   integer                                           :: numcol 
   integer                                           :: ndim    
   integer                                           :: num_elem    
   integer                                           :: i,j,is,iniz,ifin
   character(len=ndigit+7), dimension(:),allocatable :: stringv,stringp
   integer                                           :: ns
   character(len=20)                                 :: sform
   integer                                           :: wfield
!
   ndim = size(amatr,1)
   if(present(numcolo)) then
     numcol=numcolo
   else
     numcol = 70
   endif
   wfield  = ndigit + 7  !+3 perche:1 spazio di separazione+virgola+punto decimale+4 spazi per exp
!
!  Calcola il numero di elementi da stampare per blocco
   num_elem  = numcol / wfield
   allocate(stringv(num_elem),stringp(ndim))
!
!  centra tutta le stringhe nel campo wfield
   do i=1,ndim
      stringp(i) = centra_str(stringpar(i),wfield)
   enddo
!
!  Stampa
   if (present(title)) then
       write(6,'(/a)')centra_str(title,numcol+16) !perche 1x,t15
   endif
   i=0
   do 
      i=i+1
      iniz = 1 + (i - 1) * num_elem
      if (iniz > ndim) exit
      ifin = min (num_elem * i,ndim)
      ns = ifin-iniz+1
      write(sform,'(a,i0,a)')'(1x,t15,',ns,'a)'
      write(kpr,sform) stringp(iniz:ifin)(1:wfield)
      do j=1,ndim
         ns = 0
         do is=iniz,ifin
            ns = ns + 1
            stringv(ns) = string_sig(amatr(is,j),ndigit)
         enddo
         write(sform,'(a,i0,a)')'(1x,a,t15,',ns,'a)'
         write(kpr,sform)trim(stringpar(j)),stringv(1:ns)
      enddo
   enddo
!
   end subroutine stampa_matrice

!-------------------------------------------------------------------------------

   subroutine stampa_vettore(amatr,stringpar,ndigit,kpr,title)
   USE type_constants, only:DP
   USE strutil
!
   real(DP), dimension(:),      intent(in) :: amatr    ! matrice da stampare
   character(len=*), dimension(:),       intent(in) :: stringpar! stringa associata alla riga
   integer, intent(in)                               :: ndigit    ! numero di cifre da stampare per numero
   integer,                          intent(in)      :: kpr       ! unita' di stampa
   character(len=*), intent(in), optional            :: title     ! eventuale titolo 
   integer :: numcol                                        ! larghezza massima della pagina di stampa
   integer :: ndim                                           ! dimensione della matrice
   integer :: num_elem                                      ! numero di elementi da stampare per riga
   integer :: i,is,iniz,ifin
   character(len=ndigit+7), dimension(:),allocatable :: stringv,stringp
   integer                                           :: ns
   character(len=20)                                 :: sform
   integer                                           :: wfield
!
   ndim = size(amatr,1)
   numcol = 70
   wfield  = ndigit + 7  !+3 perche:1 spazio di separazione+virgola+punto decimale+4 spazi per exp
!
!  Calcola il numero di elementi da stampare per blocco
   num_elem  = numcol / wfield
   allocate(stringv(num_elem),stringp(ndim))
!
!  centra tutta le stringhe nel campo wfield
   do i=1,ndim
      stringp(i) = centra_str(stringpar(i),wfield)
   enddo
!
!  Stampa
   if (present(title)) then
       write(6,'(/a)')centra_str(title,numcol+16) !perche 1x,t15
   endif
   i = 0
   do 
      i = i + 1
      iniz = 1 + (i - 1) * num_elem
      if (iniz > ndim) exit
      ifin = min (num_elem * i,ndim)
      ns = ifin-iniz+1
      write(sform,'(a,i0,a)')'(1x,t15,',ns,'a)'
      write(kpr,sform) stringp(iniz:ifin)(1:wfield)
      !write(kpr,'(9x,100(a8,2x))') stringpar(iniz:ifin)
      ns = 0
      do is=iniz,ifin
         ns = ns + 1
         stringv(ns) = string_sig(amatr(is),ndigit)
      enddo
      !write(kpr,'(8x,100e10.3)') amatr(iniz:ifin)
         !write(sform,'(a,i0,a)')'(1x,t15,',ns,'a)'
      write(kpr,sform)stringv(1:ns)
   enddo
!
   end subroutine stampa_vettore

END MODULE PRNUTIL

 MODULE rand_mod
 
 implicit none

 private :: randvalue_real,randvalue_int, randvaluev_real, randvaluev_int
 interface randvalue
   module procedure randvalue_real,randvalue_int, randvaluev_real, randvaluev_int
 end interface randvalue

 REAL, PRIVATE      :: half = 0.5

 CONTAINS
 
   Subroutine Init_Ran(Seed,jump)
   integer, intent (in)               :: seed
   integer, intent (in), optional     :: jump
   integer, dimension(:), allocatable :: seedv
   integer                            :: isize
   real                               :: xrand
   integer                            :: i
   call random_seed(size=isize)  ! isize is compiler dipendent
   allocate(seedv(isize))
   if (Seed == 0) then      ! seed selected by the system clock
       seedv(:) = int(seedgen(pid=1))
   else                     ! seed selected by the user
       seedv(:) = Seed
   endif
   call random_seed(put=seedv)   
   if (present(jump)) then
       do i=1,jump
          call random_number(xrand)
       enddo
   endif
   End Subroutine Init_Ran                                      

!--------------------------------------------------------------------

   function seedgen(pid)
!corr   use iso_fortran_env
!corr   implicit none
!! a Windows 32 bit NON PIACE
!!mr   integer(kind=int64) :: seedgen
   integer(kind=8) :: seedgen
   integer, intent(IN) :: pid
   integer :: s

   call system_clock(s)
   seedgen = abs( mod((s*181)*((pid-83)*359), 104729) ) 
   end function seedgen

!--------------------------------------------------------------------

   real function randvalue_real(minv,maxv) result(value)
   real, intent(in) :: minv,maxv
   real             :: xrand
!
   call random_number(xrand)
   value = xrand*(maxv-minv)+minv
!
   end function randvalue_real

!--------------------------------------------------------------------

   integer function randvalue_int(minv,maxv) result(value)
   integer, intent(in) :: minv,maxv
   real                :: xrand
!
   call random_number(xrand)
   value = int(xrand*(maxv-minv)+minv + 0.5)
!
   end function randvalue_int

!--------------------------------------------------------------------

   function randvaluev_real(n,minv,maxv)  result(xrand)
   integer, intent(in) :: n
   real, dimension(n)  :: xrand
   real, intent(in)    :: minv,maxv
!
   call random_number(xrand)
   xrand(:) = xrand(:)*(maxv-minv)+minv
!
   end function randvaluev_real

!--------------------------------------------------------------------

   function randvaluev_int(n,minv,maxv,unique)  result(xrand)
   integer, intent(in)           :: n
   integer, intent(in)           :: minv,maxv
   logical, intent(in), optional :: unique  
   integer, dimension(n)         :: xrand
   real, dimension(n)            :: xrandr
   integer                       :: xri, nrand, nmax
!
   if (present(unique)) then
       if (unique) then

           xrand(1) = randvalue(minv,maxv)
           if (n == 1) return
           nmax = min(n,maxv - minv + 1)  
           nrand = 1
           do 
              xri = randvalue(minv,maxv)
              if (any(xrand(:nrand) == xri)) cycle
              nrand = nrand + 1
              xrand(nrand) = xri
              if (nrand == nmax) exit
           enddo

           return
       endif
   endif
!
   call random_number(xrandr)
   xrand(:) = nint(xrandr(:)*(maxv-minv)+minv)
!
   end function randvaluev_int

!--------------------------------------------------------------------

   FUNCTION random_normal() RESULT(fn_val)

! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.

!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.

!
!  For specific mean (m) and standard deviation (std): random_normal(a)*std + m
!
   REAL :: fn_val

!     Local variables
   REAL     :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,           &
            r1 = 0.27597, r2 = 0.27846, u, v, x, y, q

!     Generate P = (u,v) uniform in rectangle enclosing acceptance region

   DO
     CALL RANDOM_NUMBER(u)
     CALL RANDOM_NUMBER(v)
     v = 1.7156 * (v - half)

!     Evaluate the quadratic form
     x = u - s
     y = ABS(v) - t
     q = x**2 + y*(a*y - b*x)

!     Accept P if inside inner ellipse
     IF (q < r1) EXIT
!     Reject P if outside outer ellipse
     IF (q > r2) CYCLE
!     Reject P if outside acceptance region
     IF (v**2 < -4.0*LOG(u)*u**2) EXIT
   END DO

!     Return ratio of P's coordinates as the normal deviate
   fn_val = v/u
   RETURN

   END FUNCTION random_normal

!--------------------------------------------------------------------

   function random_vector_sphere()  result(vctr)
!  The following is taken from Allen & Tildesley, p. 349
!  Generate a random vector towards a point in the unit sphere
!  Daniel Duque 2004
!
!  To generate point on a sphere with radius r => vctr*r
!
   implicit none
   
   real, dimension(3) :: vctr
   real :: ran1,ran2,ransq,ranh
   real, dimension(2) :: ran
   
   do
      call random_number(ran)
      ran1=1.0-2.0*ran(1)
      ran2=1.0-2.0*ran(2)
      ransq=ran1**2+ran2**2
      if(ransq.le.1.0) exit
   enddo
   
   ranh=2.0*sqrt(1.0-ransq)
   
   vctr(1)=ran1*ranh
   vctr(2)=ran2*ranh
   vctr(3)=(1.0-2.0*ransq)
   
   end function random_vector_sphere

!--------------------------------------------------------------------

   function getPoint(xr)
!
!  Generate random point in a unit radius sphere.
!  getPoint(xr) * rad for sphere of radius rad
!
   use trig_constants
   real, dimension(3), intent(in) :: xr ! 3 random points in 0-1 range
   real, dimension(3)             :: getPoint
   real                           :: theta,phi,r
   real                           :: sinTheta,cosTheta,sinPhi,cosPhi
!
   theta = xr(1)*2.0*pi
   phi = acos(2.0*xr(2)-1.0)
   r = xr(3)**(1.0/3.0)

   sinTheta = sin(theta)
   cosTheta = cos(theta)
   sinPhi = sin(phi)
   cosPhi = cos(phi)

   getPoint(1) = r * sinPhi * cosTheta
   getPoint(2) = r * sinPhi * sinTheta
   getPoint(3) = r * cosPhi
!
   end function getPoint

 END MODULE rand_mod
