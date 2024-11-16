!  TOMS algorithm 573
!
!  nl2sol -- an adaptive nonlinear least-squares algorithm
!
!  authors = john e. dennis, jr., david m. gay, and roy e. welsch
!
!  acm transactions on mathematical software, september, 1981.

! Translation from Fortran 66/77 format to compatibility with ELF90
! by Alan Miller, 5 June 1997
! CSIRO Mathematical & Information Sciences
! Private Bag 10, Clayton South MDC
! Victoria 3169, Australia
! Fax: (+61) 3-9545-8080   Phone: (+61) 3-9545-8036
! Alan.Miller @ vic.cmis.csiro.au
! http://www.mel.dms.csiro.au/~alan
! http://www.ozemail.com.au/~milleraj

! Latest revision - 10 July 1997

MODULE machine_constants
IMPLICIT NONE
!!!!!!!!!!!!!!!!INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(15, 60)
INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(15,307)

CONTAINS

FUNCTION imdcon(k) RESULT(ival)

IMPLICIT NONE
INTEGER, INTENT(IN) :: k
INTEGER             :: ival

!  ***  return integer machine-dependent constants  ***

!     ***  k = 1 means return standard output unit number.   ***
!     ***  k = 2 means return alternate output unit number.  ***
!     ***  k = 3 means return  input unit number.            ***
!          (note -- k = 2, 3 are used only by test programs.)

INTEGER :: mdcon(3) = (/ 6, 0, 5 /)

ival = mdcon(k)

RETURN
!  ***  last card of imdcon follows  ***
END FUNCTION imdcon


FUNCTION rmdcon(k) RESULT(fn_val)

!  ***  return machine dependent constants used by nl2sol  ***

IMPLICIT NONE
INTEGER, INTENT(IN) :: k
REAL (dp)           :: fn_val

!  ***  the constant returned depends upon k...

!  ***        k = 1... smallest pos. eta such that -eta exists.
!  ***        k = 2... square root of eta.
!  ***        k = 3... unit roundoff = smallest pos. no. machep such
!  ***                 that 1 + machep .gt. 1 .and. 1 - machep .lt. 1.
!  ***        k = 4... square root of machep.
!  ***        k = 5... square root of big (see k = 6).
!  ***        k = 6... largest machine no. big such that -big exists.

SELECT CASE (k)
  CASE(1)
    fn_val = TINY(1._dp)
  CASE(2)
    fn_val = SQRT( TINY(1._dp) )
  CASE(3)
    fn_val = EPSILON(1._dp)
  CASE(4)
    fn_val = SQRT( EPSILON(1._dp) )
  CASE(5)
    fn_val = SQRT( HUGE(1._dp) )
  CASE(6)
    fn_val = HUGE(1._dp)
  CASE DEFAULT
    WRITE(0, *) '** Illegal argument, k = ', k, ' to FUNCTION rmdcon **'
END SELECT

RETURN
!  ***  last card of rmdcon follows  ***
END FUNCTION rmdcon

END MODULE machine_constants



MODULE toms573
USE machine_constants
IMPLICIT NONE

CONTAINS


SUBROUTINE nl2sol (n, p, x, calcr, calcj, calcp, iv, v, uiparm, urparm, ufparm)

!  ***  Minimize nonlinear sum of squares using analytic Jacobian  ***
!  ***  (nl2sol version 2.2)  ***

use enable_amb, only: event_from_gui
INTEGER, INTENT(IN)                 :: n, p
INTEGER, INTENT(IN OUT)             :: iv(:)
INTEGER, INTENT(IN OUT), OPTIONAL   :: uiparm(:)
REAL (dp), INTENT(IN OUT)           :: x(:), v(:)
REAL (dp), INTENT(IN OUT), OPTIONAL :: urparm(:), ufparm
!     dimension iv(60+p),  v(93 + n*p + 3*n + p*(3*p+33)/2)
!     dimension uiparm(:), urparm(:)
! EXTERNAL calcr,calcj,ufparm
EXTERNAL calcr,calcj,calcp

!INTERFACE
!  SUBROUTINE calcr(n, p, x, nf, r, uiparm)     !!!!!!!!!!!!!, urparm, ufparm)
!    USE machine_constants
!    IMPLICIT NONE
!    INTEGER, INTENT(IN)                 :: n, p
!    INTEGER, INTENT(IN OUT)             :: nf
!    !INTEGER, INTENT(IN OUT), OPTIONAL   :: uiparm(:)
!    INTEGER, INTENT(IN OUT)   :: uiparm(:)
!    REAL (dp), INTENT(IN)               :: x(:)
!    REAL (dp), INTENT(IN OUT), OPTIONAL :: urparm(:), ufparm
!    REAL (dp), INTENT(OUT)              :: r(:)
!  END SUBROUTINE calcr
!
!  SUBROUTINE calcj(n, p, x, nf, j, uiparm)     !!!!!!!!!!!!!, urparm, ufparm)
!    USE machine_constants
!    IMPLICIT NONE
!    INTEGER, INTENT(IN)                 :: n, p
!    INTEGER, INTENT(IN OUT)             :: nf
!    !INTEGER, INTENT(IN OUT), OPTIONAL   :: uiparm(:)
!    INTEGER, INTENT(IN OUT)   :: uiparm(:)
!    REAL (dp), INTENT(IN)               :: x(:)
!!    REAL (dp), INTENT(IN OUT), OPTIONAL :: urparm(:), ufparm
!    REAL (dp), INTENT(OUT)              :: j(:)
!  END SUBROUTINE calcj
!END INTERFACE

!  ***  purpose  ***

!        given a p-vector x of parameters, calcr computes an n-vector
!     r = r(x) of residuals corresponding to x.  (r(x) probably arises
!     from a nonlinear model involving p parameters and n observations.)
!     this routine interacts with nl2itr to seek a parameter vector x
!     that minimizes the sum of the squares of (the components of) r(x),
!     i.e., that minimizes the sum-of-squares function
!     f(x) = (r(x)**t) * r(x) / 2.  r(x) is assumed to be a twice con-
!     tinuously differentiable function of x.

!--------------------------  parameter usage  --------------------------

! n........ (input) the number of observations, i.e., the number of
!                  components in r(x).  n must be .ge. p.
! p........ (input) the number of parameters (components in x).  p must
!                  be positive.
! x........ (input/output).  on input, x is an initial guess at the
!                  desired parameter estimate.  on output, x contains
!                  the best parameter estimate found.
! calcr.... (input) a subroutine which, given x, computes r(x).  calcr
!                  must be declared external in the calling program.
!                  it is invoked by
!                       call calcr(n,p,x,nf,r,uiparm,urparm,ufparm)
!                  when calcr is called, nf is the invocation count
!                  for calcr.  it is included for possible use with
!                  calcj.  if x is out of bounds (e.g. if it would
!                  cause overflow in computing r(x)), then calcr should
!                  set nf to 0.  this will cause a shorter step to be
!                  attempted.  the other parameters are as described
!                  above and below.  calcr should not change n, p, or x.
! calcj.... (input) a subroutine which, given x, computes the jacobian
!                  matrix j of r at x, i.e., the n by p matrix whose
!                  (i,k) entry is the partial derivative of the i-th
!                  component of r with respect to x(k).  calcj must be
!                  declared external in the calling program.  it is
!                  invoked by
!                       call calcj(n,p,x,nf,j,uiparm,urparm,ufparm)
!                  nf is the invocation count for calcr at the time
!                  r(x) was evaluated.  the x passed to calcj is
!                  usually the one passed to calcr on either its most
!                  recent invocation or the one prior to it.  if calcr
!                  saves intermediate results for use by calcj, then it
!                  is possible to tell from nf whether they are valid
!                  for the current x (or which copy is valid if two
!                  copies are kept).  if j cannot be computed at x,
!                  then calcj should set nf to 0.  in this case, nl2sol
!                  will return with iv(1) = 15.  the other parameters
!                  to calcj are as described above and below.  calcj
!                  should not change n, p, or x.
! iv....... (input/output) an integer value array of length at least
!                  60 + p that helps control the nl2sol algorithm and
!                  that is used to store various intermediate quanti-
!                  ties.  of particular interest are the initialization/
!                  return code iv(1) and the entries in iv that control
!                  printing and limit the number of iterations and func-
!                  tion evaluations.  see the section on iv input
!                  values below.
! v........ (input/output) a floating-point value array of length at
!                  least 93 + n*p + 3*n + p*(3*p+33)/2 that helps con-
!                  trol the nl2sol algorithm and that is used to store
!                  various intermediate quantities.  of particular in-
!                  terest are the entries in v that limit the length of
!                  the first step attempted (lmax0), specify conver-
!                  gence tolerances (afctol, rfctol, xctol, xftol),
!                  and help choose the step size used in computing the
!                  covariance matrix (delta0).  see the section on
!                  (selected) v input values below.
! uiparm... (input) user integer parameter array passed without change
!                  to calcr and calcj.
! urparm... (input) user floating-point parameter array passed without
!                  change to calcr and calcj.
! ufparm... (input) user external subroutine or function passed without
!                  change to calcr and calcj.

!  ***  iv input values (from subroutine dfault)  ***

! iv(1)...  on input, iv(1) should have a value between 0 and 12......
!             0 and 12 mean this is a fresh start.  0 means that
!             dfault(iv, v) is to be called to provide all default
!             values to iv and v.  12 (the value that dfault assigns to
!             iv(1)) means the caller has already called dfault(iv, v)
!             and has possibly changed some iv and/or v entries to non-
!             default values.  default = 12.
! iv(covprt)... iv(14) = 1 means print a covariance matrix at the solu-
!             tion.  (this matrix is computed just before a return with
!             iv(1) = 3, 4, 5, 6.)
!             iv(covprt) = 0 means skip this printing.  default = 1.
! iv(covreq)... iv(15) = nonzero means compute a covariance matrix
!             just before a return with iv(1) = 3, 4, 5, 6.  in
!             this case, an approximate covariance matrix is obtained
!             in one of several ways.  let k = abs(iv(covreq)) and let
!             scale = 2*f(x)/max(1,n-p),  where 2*f(x) is the residual
!             sum of squares.  if k = 1 or 2, then a finite-difference
!             hessian approximation h is obtained.  if h is positive
!             definite (or, for k = 3, if the jacobian matrix j at x
!             is nonsingular), then one of the following is computed...
!                  k = 1....  scale * h**-1 * (j**t * j) * h**-1.
!                  k = 2....  scale * h**-1.
!                  k = 3....  scale * (j**t * j)**-1.
!             (j**t is the transpose of j, while **-1 means inverse.)
!             if iv(covreq) is positive, then both function and grad-
!             ient values (calls on calcr and calcj) are used in com-
!             puting h (with step sizes determined using v(delta0) --
!             see below), while if iv(covreq) is negative, then only
!             function values (calls on calcr) are used (with step
!             sizes determined using v(dltfdc) -- see below).  if
!             iv(covreq) = 0, then no attempt is made to compute a co-
!             variance matrix (unless iv(covprt) = 1, in which case
!             iv(covreq) = 1 is assumed).  see iv(covmat) below.
!             default = 1.
! iv(dtype).... iv(16) tells how the scale vector d (see ref. 1) should
!             be chosen.  iv(dtype) .ge. 1 means choose d as described
!             below with v(dfac).  iv(dtype) .le. 0 means the caller
!             has chosen d and has stored it in v starting at
!             v(94 + 2*n + p*(3*p + 31)/2).  default = 1.
! iv(inits).... iv(25) tells how the s matrix (see ref. 1) should be
!             initialized.  0 means initialize s to 0 (and start with
!             the gauss-newton model).  1 and 2 mean that the caller
!             has stored the lower triangle of the initial s rowwise in
!             v starting at v(87+2*p).  iv(inits) = 1 means start with
!             the gauss-newton model, while iv(inits) = 2 means start
!             with the augmented model (see ref. 1).  default = 0.
! iv(mxfcal)... iv(17) gives the maximum number of function evaluations
!             (calls on calcr, excluding those used to compute the co-
!             variance matrix) allowed.  if this number does not suf-
!             fice, then nl2sol returns with iv(1) = 9.  default = 200.
! iv(mxiter)... iv(18) gives the maximum number of iterations allowed.
!             it also indirectly limits the number of gradient evalua-
!             tions (calls on calcj, excluding those used to compute
!             the covariance matrix) to iv(mxiter) + 1.  if iv(mxiter)
!             iterations do not suffice, then nl2sol returns with
!             iv(1) = 10.  default = 150.
! iv(outlev)... iv(19) controls the number and length of iteration sum-
!             mary lines printed (by itsmry).  iv(outlev) = 0 means do
!             not print any summary lines.  otherwise, print a summary
!             line after each abs(iv(outlev)) iterations.  if iv(outlev)
!             is positive, then summary lines of length 117 (plus carri-
!             age control) are printed, including the following...  the
!             iteration and function evaluation counts, current func-
!             tion value (v(f) = half the sum of squares), relative
!             difference in function values achieved by the latest step
!             (i.e., reldf = (f0-v(f))/f0, where f0 is the function
!             value from the previous iteration), the relative function
!             reduction predicted for the step just taken (i.e.,
!             preldf = v(preduc) / f0, where v(preduc) is described
!             below), the scaled relative change in x (see v(reldx)
!             below), the models used in the current iteration (g =
!             gauss-newton, s=augmented), the marquardt parameter
!             stppar used in computing the last step, the sizing factor
!             used in updating s, the 2-norm of the scale vector d
!             times the step just taken (see ref. 1), and npreldf, i.e.,
!             v(nreduc)/f0, where v(nreduc) is described below -- if
!             npreldf is positive, then it is the relative function
!             reduction predicted for a newton step (one with
!             stppar = 0).  if npreldf is zero, either the gradient
!             vanishes (as does preldf) or else the augmented model
!             is being used and its hessian is indefinite (with preldf
!             positive).  if npreldf is negative, then it is the nega-
!             of the relative function reduction predicted for a step
!             computed with step bound v(lmax0) for use in testing for
!             singular convergence.
!                  if iv(outlev) is negative, then lines of maximum
!             length 79 (or 55 is iv(covprt) = 0) are printed, includ-
!             ing only the first 6 items listed above (through reldx).
!             default = 1.
! iv(parprt)... iv(20) = 1 means print any nondefault v values on a
!             fresh start or any changed v values on a restart.
!             iv(parprt) = 0 means skip this printing.  default = 1.
! iv(prunit)... iv(21) is the output unit number on which all printing
!             is done.  iv(prunit) = 0 means suppress all printing.
!             (setting iv(prunit) to 0 is the only way to suppress the
!             one-line termination reason message printed by itsmry.)
!             default = standard output unit (unit 6 on most systems).
! iv(solprt)... iv(22) = 1 means print out the value of x returned (as
!             well as the corresponding gradient and scale vector d).
!             iv(solprt) = 0 means skip this printing.  default = 1.
! iv(statpr)... iv(23) = 1 means print summary statistics upon return-
!             ing.  these consist of the function value (half the sum
!             of squares) at x, v(reldx) (see below), the number of
!             function and gradient evaluations (calls on calcr and
!             calcj respectively, excluding any calls used to compute
!             the covariance), the relative function reductions predict-
!             ed for the last step taken and for a newton step (or per-
!             haps a step bounded by v(lmax0) -- see the descriptions
!             of preldf and npreldf under iv(outlev) above), and (if an
!             attempt was made to compute the covariance) the number of
!             calls on calcr and calcj used in trying to compute the
!             covariance.  iv(statpr) = 0 means skip this printing.
!             default = 1.
! iv(x0prt).... iv(24) = 1 means print the initial x and scale vector d
!             (on a fresh start only).  iv(x0prt) = 0 means skip this
!             printing.  default = 1.

!  ***  (selected) iv output values  ***

! iv(1)........ on output, iv(1) is a return code....
!             3 = x-convergence.  the scaled relative difference be-
!                  tween the current parameter vector x and a locally
!                  optimal parameter vector is very likely at most
!                  v(xctol).
!             4 = relative function convergence.  the relative differ-
!                  ence between the current function value and its lo-
!                  cally optimal value is very likely at most v(rfctol).
!             5 = both x- and relative function convergence (i.e., the
!                  conditions for iv(1) = 3 and iv(1) = 4 both hold).
!             6 = absolute function convergence.  the current function
!                  value is at most v(afctol) in absolute value.
!             7 = singular convergence.  the hessian near the current
!                  iterate appears to be singular or nearly so, and a
!                  step of length at most v(lmax0) is unlikely to yield
!                  a relative function decrease of more than v(rfctol).
!             8 = false convergence.  the iterates appear to be converg-
!                  ing to a noncritical point.  this may mean that the
!                  convergence tolerances (v(afctol), v(rfctol),
!                  v(xctol)) are too small for the accuracy to which
!                  the function and gradient are being computed, that
!                  there is an error in computing the gradient, or that
!                  the function or gradient is discontinuous near x.
!             9 = function evaluation limit reached without other con-
!                  vergence (see iv(mxfcal)).
!            10 = iteration limit reached without other convergence
!                  (see iv(mxiter)).
!            11 = stopx returned .true. (external interrupt).  see the
!                  usage notes below.
!            13 = f(x) cannot be computed at the initial x.
!            14 = bad parameters passed to assess (which should not
!                  occur).
!            15 = the jacobian could not be computed at x (see calcj
!                  above).
!            16 = n or p (or parameter nn to nl2itr) out of range --
!                  p .le. 0 or n .lt. p or nn .lt. n.
!            17 = restart attempted with n or p (or par. nn to nl2itr)
!                  changed.
!            18 = iv(inits) is out of range.
!            19...45 = v(iv(1)) is out of range.
!            50 = iv(1) was out of range.
!            87...(86+p) = jtol(iv(1)-86) (i.e., v(iv(1)) is not
!                  positive (see v(dfac) below).
! iv(covmat)... iv(26) tells whether a covariance matrix was computed.
!             if (iv(covmat) is positive, then the lower triangle of
!             the covariance matrix is stored rowwise in v starting at
!             v(iv(covmat)).  if iv(covmat) = 0, then no attempt was
!             made to compute the covariance.  if iv(covmat) = -1,
!             then the finite-difference hessian was indefinite.  and
!             and if iv(covmat) = -2, then a successful finite-differ-
!             encing step could not be found for some component of x
!             (i.e., calcr set nf to 0 for each of two trial steps).
!             note that iv(covmat) is reset to 0 after each successful
!             step, so if such a step is taken after a restart, then
!             the covariance matrix will be recomputed.
! iv(d)........ iv(27) is the starting subscript in v of the current
!             scale vector d.
! iv(g)........ iv(28) is the starting subscript in v of the current
!             least-squares gradient vector (j**t)*r.
! iv(nfcall)... iv(6) is the number of calls so far made on calcr (i.e.,
!             function evaluations, including those used in computing
!             the covariance).
! iv(nfcov).... iv(40) is the number of calls made on calcr when
!             trying to compute covariance matrices.
! iv(ngcall)... iv(30) is the number of gradient evaluations (calls on
!             calcj) so far done (including those used for computing
!             the covariance).
! iv(ngcov).... iv(41) is the number of calls made on calcj when
!             trying to compute covariance matrices.
! iv(niter).... iv(31) is the number of iterations performed.
! iv(r)........ iv(50) is the starting subscript in v of the residual
!             vector r corresponding to x.

!  ***  (selected) v input values (from subroutine dfault)  ***

! v(afctol)... v(31) is the absolute function convergence tolerance.
!             if nl2sol finds a point where the function value (half
!             the sum of squares) is less than v(afctol), and if nl2sol
!             does not return with iv(1) = 3, 4, or 5, then it returns
!             with iv(1) = 6.  default = max(10**-20, machep**2), where
!             machep is the unit roundoff.
! v(delta0)... v(44) is a factor used in choosing the finite-difference
!             step size used in computing the covariance matrix when
!             iv(covreq) = 1 or 2.  for component i, step size
!                  v(delta0) * max(abs(x(i)), 1/d(i)) * sign(x(i))
!             is used, where d is the current scale vector (see ref. 1).
!             (if this step results in calcr setting nf to 0, then -0.5
!             times this step is also tried.)  default = machep**0.5,
!             where machep is the unit roundoff.
! v(dfac)..... v(41) and the d0 and jtol arrays (see v(d0init) and
!             v(jtinit)) are used in updating the scale vector d when
!             iv(dtype) .gt. 0.  (d is initialized according to
!             v(dinit).)  let d1(i) =
!               max(sqrt(jcnorm(i)**2 + max(s(i,i),0)), v(dfac)*d(i)),
!             where jcnorm(i) is the 2-norm of the i-th column of the
!             current jacobian matrix and s is the s matrix of ref. 1.
!             if iv(dtype) = 1, then d(i) is set to d1(i) unless
!             d1(i) .lt. jtol(i), in which case d(i) is set to
!                                max(d0(i), jtol(i)).
!             if iv(dtype) .ge. 2, then d is updated during the first
!             iteration as for iv(dtype) = 1 (after any initialization
!             due to v(dinit)) and is left unchanged thereafter.
!             default = 0.6.
! v(dinit).... v(38), if nonnegative, is the value to which the scale
!             vector d is initialized.  default = 0.
! v(dltfdc)... v(40) helps choose the step size used when computing the
!             covariance matrix when iv(covreq) = -1 or -2.  for
!             differences involving x(i), the step size first tried is
!                       v(dltfdc) * max(abs(x(i)), 1/d(i)),
!             where d is the current scale vector (see ref. 1).  (if
!             this step is too big the first time it is tried, i.e., if
!             calcr sets nf to 0, then -0.5 times this step is also
!             tried.)  default = machep**(1/3), where machep is the
!             unit roundoff.
! v(d0init)... v(37), if positive, is the value to which all components
!             of the d0 vector (see v(dfac)) are initialized.  if
!             v(dfac) = 0, then it is assumed that the caller has
!             stored d0 in v starting at v(p+87).  default = 1.0.
! v(jtinit)... v(39), if positive, is the value to which all components
!             of the jtol array (see v(dfac)) are initialized.  if
!             v(jtinit) = 0, then it is assumed that the caller has
!             stored jtol in v starting at v(87).  default = 10**-6.
! v(lmax0).... v(35) gives the maximum 2-norm allowed for d times the
!             very first step that nl2sol attempts.  it is also used
!             in testing for singular convergence -- if the function
!             reduction predicted for a step of length bounded by
!             v(lmax0) is at most v(rfctol) * abs(f0), where  f0 is
!             the function value at the start of the current iteration,
!             and if nl2sol does not return with iv(1) = 3, 4, 5, or 6,
!             then it returns with iv(1) = 7.    default = 100.
! v(rfctol)... v(32) is the relative function convergence tolerance.
!             if the current model predicts a maximum possible function
!             reduction (see v(nreduc)) of at most v(rfctol)*abs(f0) at
!             the start of the current iteration, where  f0 is the
!             then current function value, and if the last step attempt-
!             ed achieved no more than twice the predicted function
!             decrease, then nl2sol returns with iv(1) = 4 (or 5).
!             default = max(10**-10, machep**(2/3)), where machep is
!             the unit roundoff.
! v(tuner1)... v(26) helps decide when to check for false convergence
!             and to consider switching models.  this is done if the
!             actual function decrease from the current step is no more
!             than v(tuner1) times its predicted value.  default = 0.1.
! v(xctol).... v(33) is the x-convergence tolerance.  if a newton step
!             (see v(nreduc)) is tried that has v(reldx) .le. v(xctol)
!             and if this step yields at most twice the predicted func-
!             tion decrease, then nl2sol returns with iv(1) = 3 (or 5).
!             (see the description of v(reldx) below.)
!             default = machep**0.5, where machep is the unit roundoff.
! v(xftol).... v(34) is the false convergence tolerance.  if a step is
!             tried that gives no more than v(tuner1) times the predict-
!             ed function decrease and that has v(reldx) .le. v(xftol),
!             and if nl2sol does not return with iv(1) = 3, 4, 5, 6, or
!             7, then it returns with iv(1) = 8.  (see the description
!             of v(reldx) below.)  default = 100*machep, where
!             machep is the unit roundoff.
! v(*)........ dfault supplies to v a number of tuning constants, with
!             which it should ordinarily be unnecessary to tinker.  see
!             version 2.2 of the nl2sol usage summary (which is an
!             appendix to ref. 1).

!  ***  (selected) v output values  ***

! v(dgnorm)... v(1) is the 2-norm of (d**-1)*g, where g is the most re-
!             cently computed gradient and d is the corresponding scale
!             vector.
! v(dstnrm)... v(2) is the 2-norm of d*step, where step is the most re-
!             cently computed step and d is the current scale vector.
! v(f)........ v(10) is the current function value (half the sum of
!             squares).
! v(f0)....... v(13) is the function value at the start of the current
!             iteration.
! v(nreduc)... v(6), if positive, is the maximum function reduction
!             possible according to the current model, i.e., the func-
!             tion reduction predicted for a newton step (i.e.,
!             step = -h**-1 * g,  where  g = (j**t) * r  is the current
!             gradient and h is the current hessian approximation --
!             h = (j**t)*j  for the gauss-newton model and
!             h = (j**t)*j + s  for the augmented model).
!                  v(nreduc) = zero means h is not positive definite.
!                  if v(nreduc) is negative, then it is the negative of
!             the function reduction predicted for a step computed with
!             a step bound of v(lmax0) for use in testing for singular
!             convergence.
! v(preduc)... v(7) is the function reduction predicted (by the current
!             quadratic model) for the current step.  this (divided by
!             v(f0)) is used in testing for relative function
!             convergence.
! v(reldx).... v(17) is the scaled relative change in x caused by the
!             current step, computed as
!                  max(abs(d(i)*(x(i)-x0(i)), 1 .le. i .le. p) /
!                     max(d(i)*(abs(x(i))+abs(x0(i))), 1 .le. i .le. p),
!             where x = x0 + step.

!-------------------------------  notes  -------------------------------

!  ***  algorithm notes  ***

!        see ref. 1 for a description of the algorithm used.
!        on problems which are naturally well scaled, better perform-
!     ance may be obtained by setting v(d0init) = 1.0 and iv(dtype) = 0,
!     which will cause the scale vector d to be set to all ones.

!  ***  usage notes  ***

!        after a return with iv(1) .le. 11, it is possible to restart,
!     i.e., to change some of the iv and v input values described above
!     and continue the algorithm from the point where it was interrupt-
!     ed.  iv(1) should not be changed, nor should any entries of iv
!     and v other than the input values (those supplied by dfault).
!        those who do not wish to write a calcj which computes the ja-
!     cobian matrix analytically should call nl2sno rather than nl2sol.
!     nl2sno uses finite differences to compute an approximate jacobian.
!        those who would prefer to provide r and j (the residual and
!     jacobian) by reverse communication rather than by writing subrou-
!     tines calcr and calcj may call on nl2itr directly.  see the com-
!     ments at the beginning of nl2itr.
!        those who use nl2sol interactively may wish to supply their
!     own stopx function, which should return .true. if the break key
!     has been pressed since stopx was last invoked.  this makes it pos-
!     sible to externally interrupt nl2sol (which will return with
!     iv(1) = 11 if stopx returns .true.).
!        storage for j is allocated at the end of v.  thus the caller
!     may make v longer than specified above and may allow calcj to use
!     elements of j beyond the first n*p as scratch storage.

!  ***  portability notes  ***

!        the nl2sol distribution tape contains both single- and double-
!     precision versions of the nl2sol source code, so it should be un-
!     necessary to change precisions.
!        only the functions imdcon and rmdcon contain machine-dependent
!     constants.  to change from one machine to another, it should
!     suffice to change the (few) relevant lines in these functions.
!        intrinsic functions are explicitly declared.  on certain com-
!     puters (e.g. univac), it may be necessary to comment out these
!     declarations.  so that this may be done automatically by a simple
!     program, such declarations are preceded by a comment having c/+
!     in columns 1-3 and blanks in columns 4-72 and are followed by
!     a comment having c/ in columns 1 and 2 and blanks in columns 3-72.
!        the nl2sol source code is expressed in 1966 ansi standard
!     fortran.  it may be converted to fortran 77 by
!     commenting out all lines that fall between a line having c/6 in
!     columns 1-3 and a line having c/7 in columns 1-3 and by removing
!     (i.e., replacing by a blank) the c in column 1 of the lines that
!     follow the c/7 line and preceed a line having c/ in columns 1-2
!     and blanks in columns 3-72.  these changes convert some data
!     statements into parameter statements, convert some variables from
!     real to character*4, and make the data statements that initialize
!     these variables use character strings delimited by primes instead
!     of hollerith constants.  (such variables and data statements
!     appear only in modules itsmry and parchk.  parameter statements
!     appear nearly everywhere.)

!  ***  references  ***

! 1.  dennis, j.e., gay, d.m., and welsch, r.e. (1981), an adaptive
!             nonlinear least-squares algorithm, acm trans. math.
!             software, vol. 7, no. 3.

!  ***  general  ***

!     coded by david m. gay (winter 1979 - winter 1980).
!     this subroutine was written in connection with research
!     supported by the national science foundation under grants
!     mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989, and
!     mcs-7906671.

!----------------------------  declarations  ---------------------------

! EXTERNAL itsmry,nl2itr
! itsmry... prints iteration summary and info about initial and final x.
! nl2itr... reverse-communication routine that carries out nl2sol algorithm.
LOGICAL :: strted
INTEGER :: d1,j1,nf,r1

!  ***  iv subscript values  ***

INTEGER, PARAMETER :: nfcall=6, nfgcal=7, toobig=2

!  ***  v subscript values  ***

INTEGER, PARAMETER :: d=27, j=33, r=50

!qt integer :: kevent

!+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++

d1=94 + 2*n + p*(3*p+31)/2
iv(d)=d1
r1=d1+p
iv(r)=r1
j1=r1+n
iv(j)=j1
strted=.true.
IF (iv(1) /= 0.AND.iv(1) /= 12) GO TO 40
strted=.false.
iv(nfcall)=1
iv(nfgcal)=1

10 nf=iv(nfcall)
CALL calcr (n, p, x, nf, v(r1:), uiparm(1))          !!!!!!!!!!!!!!!, urparm, ufparm)
!corrado
        if (uiparm(1) > 0) return
!qt        call gestione_eventi(kevent)
        uiparm(2) = event_from_gui()
        if (uiparm(2) == 1 .or. uiparm(2) == 2) return   ! stop da grafica
!corrado
IF (strted) GO TO 20
IF (nf > 0) GO TO 30
iv(1)=13
GO TO 60

20 IF (nf <= 0) iv(toobig)=1
GO TO 40

30 CALL calcj (n, p, x, iv(nfgcal), v(j1:), uiparm(1))      !!!!!!!!!!, urparm, ufparm)
IF (iv(nfgcal) == 0) GO TO 50
strted=.true.

40 CALL nl2itr (v(d1:), iv, v(j1:), n, n, p, v(r1:), v, x)

IF (iv(1)-2 < 0) THEN
  GO TO 10
ELSE IF (iv(1)-2 == 0) THEN
  GO TO 30
ELSE
  GO TO 70
END IF

50 iv(1)=15
!corrado 60 CALL itsmry (v(d1:), iv, p, v, x)
60 CONTINUE 
!corrado

70 RETURN
!  ***  last card of nl2sol follows  ***
END SUBROUTINE nl2sol


SUBROUTINE nl2sno (n, p, x, calcr, iv, v, uiparm, urparm, ufparm)

!  ***  like nl2sol, but without calcj -- minimize nonlinear sum of  ***
!  ***  squares using finite-difference jacobian approximations      ***
!  ***  (nl2sol version 2.2)  ***

INTEGER, INTENT(IN)                 :: n,p
INTEGER, INTENT(IN OUT)             :: iv(:)
INTEGER, INTENT(IN OUT), OPTIONAL   :: uiparm(:)
REAL (dp), INTENT(IN OUT)           :: x(:), v(:)
REAL (dp), INTENT(IN OUT), OPTIONAL :: urparm(:), ufparm
!     dimension iv(60+p),  v(93 + n*p + 3*n + p*(3*p+33)/2)
! EXTERNAL calcr

INTERFACE
  SUBROUTINE calcr(n, p, x, nf, r, uiparm, urparm, ufparm)
    USE machine_constants
    IMPLICIT NONE
    INTEGER, INTENT(IN)                 :: n, p
    INTEGER, INTENT(IN OUT)             :: nf
    INTEGER, INTENT(IN OUT), OPTIONAL   :: uiparm(:)
    REAL (dp), INTENT(IN)               :: x(:)
    REAL (dp), INTENT(IN OUT), OPTIONAL :: urparm(:), ufparm
    REAL (dp), INTENT(OUT)              :: r(:)
  END SUBROUTINE calcr
END INTERFACE

!-----------------------------  discussion  ----------------------------

!        the parameters for nl2sno are the same as those for nl2sol
!     (which see), except that calcj is omitted.  instead of calling
!     calcj to obtain the jacobian matrix of r at x, nl2sno computes
!     an approximation to it by finite (forward) differences -- see
!     v(dltfdj) below.  nl2sno uses function values only when comput-
!     the covariance matrix (rather than the functions and gradients
!     that nl2sol may use).  to do so, nl2sno sets iv(covreq) to -1 if
!     iv(covprt) = 1 with iv(covreq) = 0 and to minus its absolute
!     value otherwise.  thus v(delta0) is never referenced and only
!     v(dltfdc) matters -- see nl2sol for a description of v(dltfdc).
!        the number of extra calls on calcr used in computing the jaco-
!     bian approximation are not included in the function evaluation
!     count iv(nfcall) and are not otherwise reported.

! v(dltfdj)... v(36) helps choose the step size used when computing the
!             finite-difference jacobian matrix.  for differences in-
!             volving x(i), the step size first tried is
!                       v(dltfdj) * max(abs(x(i)), 1/d(i)),
!             where d is the current scale vector (see ref. 1).  (if
!             this step is too big, i.e., if calcr sets nf to 0, then
!             smaller steps are tried until the step size is shrunk be-
!             low 1000 * machep, where machep is the unit roundoff.
!             default = machep**0.5.

!  ***  references  ***

! 1.  dennis, j.e., gay, d.m., and welsch, r.e. (1981), an adaptive
!             nonlinear least-squares algorithm, acm trans. math.
!             software, vol. 7, no. 3.

!  ***  general  ***

!     coded by david m. gay.
!     this subroutine was written in connection with research
!     supported by the national science foundation under grants
!     mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989, and
!     mcs-7906671.

!+++++++++++++++++++++++++++  declarations  ++++++++++++++++++++++++++++

! dfault... supplies default parameter values.
! itsmry... prints iteration summary and info about initial and final x.
! nl2itr... reverse-communication routine that carries out nl2sol algo-
!             rithm.
! rmdcon... returns machine-dependent constants.
! vscopy... sets all elements of a vector to a scalar.

LOGICAL   :: strted
INTEGER   :: dk,d1,i,j1,j1k,k,nf,rn,r1
REAL (dp) :: h,xk

REAL (dp), PARAMETER :: hfac=1.d+3, negpt5=-0.5d+0, one=1.d+0,  &
                               zero=0.d+0

!  ***  iv subscript values  ***

INTEGER, PARAMETER :: covprt=14, covreq=15, d=27, dtype=16, j=33,   &
                      nfcall=6, nfgcal=7, r=50, toobig=2

!  ***  v subscript values  ***

INTEGER, PARAMETER     :: dltfdj=36, dinit = 38
!     save hlim
REAL (dp), SAVE :: hlim = 0.d+0

!+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++

d1=94+2*n+p*(3*p+31)/2
iv(d)=d1
r1=d1+p
iv(r)=r1
j1=r1+n
iv(j)=j1
rn=j1-1
IF (iv(1) == 0) CALL dfault (iv, v)
iv(covreq)=-ABS(iv(covreq))
IF (iv(covprt) /= 0.AND.iv(covreq) == 0) iv(covreq)=-1
strted=.true.
IF (iv(1) /= 12) GO TO 80
strted=.false.
iv(nfcall)=1
iv(nfgcal)=1
!        ***  initialize scale vector d to ones for computing
!        ***  initial jacobian.
IF (iv(dtype) > 0) CALL vscopy (p, v(d1:), one)
IF (v(dinit) > zero) CALL vscopy (p, v(d1:), v(dinit))

10 nf=iv(nfcall)
CALL calcr (n, p, x, nf, v(r1:), uiparm)        !!!!!!!!!!!!, urparm, ufparm)
IF (strted) GO TO 20
IF (nf > 0) GO TO 30
iv(1)=13
GO TO 90

20 IF (nf <= 0) iv(toobig)=1
GO TO 80

!  ***  compute finite-difference jacobian  ***

30 j1k=j1
dk=d1
DO k=1,p
  xk=x(k)
  h=v(dltfdj)*MAX(ABS(xk),one/v(dk))
  dk=dk+1
  40 x(k)=xk+h
  nf=iv(nfgcal)
  CALL calcr (n, p, x, nf, v(j1k:), uiparm)       !!!!!!!!!!!!!!, urparm, ufparm)
  IF (nf > 0) GO TO 50
  IF (hlim == zero) hlim=hfac*rmdcon(3)
!             ***  hlim = hfac times the unit roundoff  ***
  h=negpt5*h
  IF (ABS(h) >= hlim) GO TO 40
  iv(1)=15
  GO TO 90
  50 x(k)=xk
  DO i=r1,rn
    v(j1k)=(v(j1k)-v(i))/h
    j1k=j1k+1
  END DO
END DO

strted=.true.

80 CALL nl2itr (v(d1:), iv, v(j1:), n, n, p, v(r1:), v, x)
IF (iv(1)-2 < 0) THEN
  GO TO 10
ELSE IF (iv(1)-2 == 0) THEN
  GO TO 30
ELSE
  GO TO 100
END IF

90 CALL itsmry (v(d1:), iv, p, v, x)

100 RETURN
!  ***  last card of nl2sno follows  ***
END SUBROUTINE nl2sno


SUBROUTINE nl2itr (d, iv, jac, n, nn, p, r, v, x)

!  ***  carry out nl2sol (nonlinear least-squares) iterations  ***
!  ***  (nl2sol version 2.2)  ***

!  ***  parameter declarations  ***

INTEGER, INTENT(IN)       :: n, nn, p
INTEGER, INTENT(IN OUT)   :: iv(:)
REAL (dp), INTENT(IN OUT) :: d(:), jac(:), r(:), v(:), x(:)
!     dimension iv(60+p), v(93 + 2*n + p*(3*p+31)/2), j(nn,p), r(n)

!--------------------------  parameter usage  --------------------------

! d.... scale vector.
! iv... integer value array.
! j.... n by p jacobian matrix (lead dimension nn).
! n.... number of observations (components in r).
! nn... lead dimension of j.
! p.... number of parameters (components in x).
! r.... residual vector.
! v.... floating-point value array.
! x.... parameter vector.

!  ***  discussion  ***

!        parameters iv, n, p, v, and x are the same as the correspond-
!     ing ones to nl2sol (which see), except that v can be shorter
!     (since the part of v that nl2sol uses for storing d, j, and r is
!     not needed).  moreover, compared with nl2sol, iv(1) may have the
!     two additional output values 1 and 2, which are explained below,
!     as is the use of iv(toobig) and iv(nfgcal).  the values iv(d),
!     iv(j), and iv(r), which are output values from nl2sol (and
!     nl2sno), are not referenced by nl2itr or the subroutines it calls.
!        on a fresh start, i.e., a call on nl2itr with iv(1) = 0 or 12,
!     nl2itr assumes that r = r(x), the residual at x, and j = j(x),
!     the corresponding jacobian matrix of r at x.

! iv(1) = 1 means the caller should set r to r(x), the residual at x,
!             and call nl2itr again, having changed none of the other
!             parameters.  an exception occurs if r cannot be evaluated
!             at x (e.g. if r would overflow), which may happen because
!             of an oversized step.  in this case the caller should set
!             iv(toobig) = iv(2) to 1, which will cause nl2itr to ig-
!             nore r and try a smaller step.  the parameter nf that
!             nl2sol passes to calcr (for possible use by calcj) is a
!             copy of iv(nfcall) = iv(6).
! iv(1) = 2 means the caller should set j to j(x), the jacobian matrix
!             of r at x, and call nl2itr again.  the caller may change
!             d at this time, but should not change any of the other
!             parameters.  the parameter nf that nl2sol passes to
!             calcj is iv(nfgcal) = iv(7).  if j cannot be evaluated
!             at x, then the caller may set iv(nfgcal) to 0, in which
!             case nl2itr will return with iv(1) = 15.

!  ***  general  ***

!     coded by david m. gay.
!     this subroutine was written in connection with research
!     supported by the national science foundation under grants

!     mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989, and
!     mcs-7906671.
!        (see nl2sol for references.)

!+++++++++++++++++++++++++++  declarations  ++++++++++++++++++++++++++++

!  ***  local variables  ***

INTEGER   :: dig1,g1,g01,h0,h1,i,im1,ipivi,ipivk,ipiv1,ipk,k,km1,  &
             l,lky1,lmat1,lstgst,m,pp1o2,qtr1,rdk,rd0,rd1,rsave1,smh,sstep,  &
             step1,stpmod,s1,temp1,temp2,w1,x01
REAL (dp) :: e,rdof1,sttsst,t,t1
REAL (dp) :: j(nn,p)
!        integer :: jj

!  ***  external functions and subroutines  ***

! assess... assesses candidate step.
! covclc... computes covariance matrix.
! dotprd... returns inner product of two vectors.
! dupdat... updates scale vector d.
! gqtstp... computes goldfeld-quandt-trotter step (augmented model).
! itsmry... prints iteration summary and info about initial and final x.
! lmstep... computes levenberg-marquardt step (gauss-newton model).
! parchk... checks validity of input iv and v values.
! qapply... applies orthogonal matrix q from qrfact to a vector.
! qrfact... computes qr decomposition of a matrix via householder trans.
! rptmul... multiplies vector by the r matrix (and/or its transpose)
!             stored by qrfact.
! slupdt... performs quasi-newton update on compactly stored lower tri-
!             angle of a symmetric matrix.
! stopx.... returns .true. if the break key has been pressed.
! vaxpy.... computes scalar times one vector plus another.
! vcopy.... copies one vector to another.
! vscopy... sets all elements of a vector to a scalar.
! v2norm... returns the 2-norm of a vector.

!  ***  iv subscript values  ***

INTEGER, PARAMETER :: cnvcod=34, covmat=26, covprt=14, covreq=15, dig=43, &
                      dtype=16, g=28, h=44, ierr=32, inits=25, ipivot=61, &
                      ipiv0=60, irc=3, kagqt=35, kalm=36, lky=37, lmat=58, &
                      mode=38, model=5, mxfcal=17, mxiter=18, nfcall=6,   &
                      nfgcal=7, nfcov=40, ngcov=41, ngcall=30, niter=31,  &
                      qtr=49, radinc=8, rd=51, restor=9, rsave=52, s=53,  &
                      step=55, stglim=11, stlstg=56, sused=57, switch=12, &
                      toobig=2, w=59, xirc=13, x0=60

!  ***  v subscript values  ***

INTEGER, PARAMETER :: cosmin=43, dgnorm=1, dinit=38, dstnrm=2, d0init=37,  &
                      f=10, fdif=11, fuzz=45, f0=13, gtstep=4, incfac=23,  &
                      jtinit=39, jtol1=87, lmax0=35, nvsave=9, phmxfc=21,  &
                      preduc=7, radfac=16, radius=8, rad0=9, rlimit=42,    &
                      size=47, stppar=5, tuner4=29, tuner5=30, vsave1=78,  &
                      wscale=48

REAL (dp), PARAMETER :: half=0.5d+0, negone=-1.d+0, one=1.d+0, zero=0.d+0

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

j = RESHAPE(jac, (/ nn, p /))
i=iv(1)
IF (i == 1) GO TO 20
IF (i == 2) GO TO 50

!  ***  check validity of iv and v input values  ***

!     ***  note -- if iv(1) = 0, then parchk calls dfault(iv, v)  ***
CALL parchk (iv, n, nn, p, v)
i=iv(1)-2

SELECT CASE (i)
  CASE (1:6)
    GO TO 300
  CASE (7,9)
    GO TO 150
  CASE (8)
    GO TO 100
  CASE (10)
    GO TO 10
  CASE DEFAULT
    GO TO 590
END SELECT

!  ***  initialization and storage allocation  ***

10 iv(niter)=0
iv(nfcall)=1
iv(ngcall)=1
iv(nfgcal)=1
iv(mode)=-1
iv(stglim)=2
iv(toobig)=0
iv(cnvcod)=0
iv(covmat)=0
iv(nfcov)=0
iv(ngcov)=0
iv(kalm)=-1
iv(radinc)=0
iv(s)=jtol1+2*p
pp1o2=p*(p+1)/2
iv(x0)=iv(s)+pp1o2
iv(step)=iv(x0)+p
iv(stlstg)=iv(step)+p
iv(dig)=iv(stlstg)+p
iv(g)=iv(dig)+p
iv(lky)=iv(g)+p
iv(rd)=iv(lky)+p
iv(rsave)=iv(rd)+p
iv(qtr)=iv(rsave)+n
iv(h)=iv(qtr)+n
iv(w)=iv(h)+pp1o2
iv(lmat)=iv(w)+4*p+7
!     +++ length of w = p*(p+9)/2 + 7.  lmat is contained in w.
IF (v(dinit) >= zero) CALL vscopy (p, d, v(dinit))
IF (v(jtinit) > zero) CALL vscopy (p, v(jtol1:), v(jtinit))
i=jtol1+p
IF (v(d0init) > zero) CALL vscopy (p, v(i:), v(d0init))
v(rad0)=zero
v(stppar)=zero
v(radius)=v(lmax0)/(one + v(phmxfc))

!  ***  set initial model and s matrix  ***

iv(model)=1
IF (iv(inits) == 2) iv(model)=2
s1=iv(s)
IF (iv(inits) == 0) CALL vscopy (pp1o2, v(s1:), zero)

!  ***  compute function value (half the sum of squares)  ***

20 t=v2norm(n,r)
IF (t > v(rlimit)) iv(toobig)=1
IF (iv(toobig) /= 0) GO TO 30
v(f)=half*t**2
30 IF (iv(mode) < 0) THEN
  GO TO  40
ELSE IF (iv(mode) == 0) THEN
  GO TO 300
ELSE
  GO TO 540
END IF

40 IF (iv(toobig) == 0) GO TO 60
iv(1)=13
GO TO 580

!  ***  make sure jacobian could be computed  ***

50 IF (iv(nfgcal) /= 0) GO TO 60
iv(1)=15
GO TO 580

!  ***  compute gradient  ***

60 iv(kalm)=-1
g1=iv(g)
DO i=1,p
  v(g1)=dotprd(n,r,j(:,i))
  g1=g1+1
END DO
IF (iv(mode) > 0) GO TO 520

!  ***  update d and make copies of r for possible use later  ***

IF (iv(dtype) > 0) CALL dupdat (d, iv, j, n, p, v)
rsave1=iv(rsave)
CALL vcopy (n, v(rsave1:), r)
qtr1=iv(qtr)
CALL vcopy (n, v(qtr1:), r)

!  ***  compute  d**-1 * gradient  ***

g1=iv(g)
dig1=iv(dig)
k=dig1
DO i=1,p
  v(k)=v(g1)/d(i)
  k=k+1
  g1=g1+1
END DO
v(dgnorm)=v2norm(p,v(dig1:))

IF (iv(cnvcod) /= 0) GO TO 510
IF (iv(mode) == 0) GO TO 460
iv(mode)=0

!-----------------------------  main loop  -----------------------------

!  ***  print iteration summary, check iteration limit  ***

90 CALL itsmry (d, iv, p, v, x)
100 k=iv(niter)
IF (k < iv(mxiter)) GO TO 110
iv(1)=10
GO TO 580
110 iv(niter)=k+1

!  ***  update radius  ***

IF (k == 0) GO TO 130
step1=iv(step)
DO i=1,p
  v(step1)=d(i)*v(step1)
  step1=step1+1
END DO
step1=iv(step)
v(radius)=v(radfac)*v2norm(p,v(step1:))

!  ***  initialize for start of next iteration  ***

130 x01=iv(x0)
v(f0)=v(f)
iv(kagqt)=-1
iv(irc)=4
iv(h)=-ABS(iv(h))
iv(sused)=iv(model)

!     ***  copy x to x0 ***

CALL vcopy (p, v(x01:), x)

!  ***  check stopx and function evaluation limit  ***

140 IF (.NOT.stopx()) GO TO 160
iv(1)=11
GO TO 170

!     ***  come here when restarting after func. eval. limit or stopx.

150 IF (v(f) >= v(f0)) GO TO 160
v(radfac)=one
k=iv(niter)
GO TO 110

160 IF (iv(nfcall) < iv(mxfcal)+iv(nfcov)) GO TO 180
iv(1)=9
170 IF (v(f) >= v(f0)) GO TO 580

!        ***  in case of stopx or function evaluation limit with
!        ***  improved v(f), evaluate the gradient at x.

iv(cnvcod)=iv(1)
GO TO 450

!. . . . . . . . . . . . .  compute candidate step  . . . . . . . . . .

180 step1=iv(step)
w1=iv(w)
IF (iv(model) == 2) GO TO 220

!  ***  compute levenberg-marquardt step  ***

qtr1=iv(qtr)
IF (iv(kalm) >= 0) GO TO 190
rd1=iv(rd)
IF (-1 == iv(kalm)) CALL qrfact (n, p, j, v(rd1:), iv(ipivot:),  &
                                 iv(ierr), 0, v(w1:))
CALL qapply (n, p, j, v(qtr1:), iv(ierr))
190 h1=iv(h)
IF (h1 > 0) GO TO 210

!        ***  copy r matrix to h  ***

h1=-h1
iv(h)=h1
k=h1
rd1=iv(rd)
v(k)=v(rd1)
IF (p == 1) GO TO 210
DO i=2,p
  CALL vcopy (i-1, v(k+1:), j(:,i))
  k=k+i
  rd1=rd1+1
  v(k)=v(rd1)
END DO

210 g1=iv(g)
CALL lmstep (d, v(g1:), iv(ierr), iv(ipivot:), iv(kalm), p, v(qtr1:),  &
             v(h1:), v(step1:), v, v(w1:))
GO TO 290

!  ***  compute goldfeld-quandt-trotter step (augmented model)  ***

220 IF (iv(h) > 0) GO TO 280

!     ***  set h to  d**-1 * ( (j**t)*j + s) ) * d**-1.  ***

h1=-iv(h)
iv(h)=h1
s1=iv(s)
IF (-1 /= iv(kalm)) GO TO 250

!        ***  j is in its original form  ***

DO i=1,p
  t=one/d(i)
  DO k=1,i
    v(h1)=t*(dotprd(n,j(:,i),j(:,k)) + v(s1))/d(k)
    h1=h1+1
    s1=s1+1
  END DO
END DO
GO TO 280

!  ***  lmstep has applied qrfact to j  ***

250 smh=s1-h1
h0=h1-1
ipiv1=iv(ipivot)
t1=one/d(ipiv1)
rd0=iv(rd)-1
rdof1=v(rd0+1)
DO i=1,p
  l=ipiv0+i
  ipivi=iv(l)
  h1=h0 + ipivi*(ipivi-1)/2
  l=h1+ipivi
  m=l+smh
!             ***  v(l) = h(ipivot(i), ipivot(i))  ***
!             ***  v(m) = s(ipivot(i), ipivot(i))  ***
  t=one/d(ipivi)
  rdk=rd0+i
  e=v(rdk)**2
  IF (i > 1) e=e + dotprd(i-1,j(:,i),j(:,i))
  v(l)=(e+v(m))*t**2
  IF (i == 1) CYCLE
  l=h1+ipiv1
  IF (ipivi < ipiv1) l=l+((ipiv1-ipivi)*(ipiv1+ipivi-3))/2
  m=l+smh
!             ***  v(l) = h(ipivot(i), ipivot(1))  ***
!             ***  v(m) = s(ipivot(i), ipivot(1))  ***
  v(l)=t*(rdof1*j(1,i)+v(m))*t1
  IF (i == 2) CYCLE
  im1=i-1
  DO k=2,im1
    ipk=ipiv0+k
    ipivk=iv(ipk)
    l=h1+ipivk
    IF (ipivi < ipivk) l=l+((ipivk-ipivi)*(ipivk+ipivi-3))/2
    m=l+smh
!                  ***  v(l) = h(ipivot(i), ipivot(k))  ***
!                  ***  v(m) = s(ipivot(i), ipivot(k))  ***
    km1=k-1
    rdk=rd0+k
    v(l)=t*(dotprd(km1,j(:,i),j(:,k)) + v(rdk)*j(k,i) + v(m))/d(ipivk)
  END DO
END DO

!  ***  compute actual goldfeld-quandt-trotter step  ***

280 h1=iv(h)
dig1=iv(dig)
lmat1=iv(lmat)
CALL gqtstp (d, v(dig1:), v(h1:), iv(kagqt), v(lmat1:), p, v(step1:), v,  &
             v(w1:))

!  ***  compute r(x0 + step)  ***

290 IF (iv(irc) == 6) GO TO 300
x01=iv(x0)
step1=iv(step)
CALL vaxpy (p, x, one, v(step1:), v(x01:))
iv(nfcall)=iv(nfcall)+1
iv(1)=1
iv(toobig)=0
GO TO 590

!. . . . . . . . . . . . .  assess candidate step  . . . . . . . . . . .

300 step1=iv(step)
lstgst=iv(stlstg)
x01=iv(x0)
CALL assess (d, iv, p, v(step1:), v(lstgst:), v, x, v(x01:))

!  ***  if necessary, switch models and/or restore r  ***

IF (iv(switch) == 0) GO TO 310
iv(h)=-ABS(iv(h))
iv(sused)=iv(sused)+2
CALL vcopy (nvsave, v, v(vsave1:))
310 IF (iv(restor) == 0) GO TO 320
rsave1=iv(rsave)
CALL vcopy (n, r, v(rsave1:))
320 l=iv(irc)-4
stpmod=iv(model)
IF (l > 0) THEN
  SELECT CASE (l)
    CASE (1)
      GO TO 340
    CASE (2)
      GO TO 360
    CASE (3:8)
      GO TO 370
    CASE (9)
      GO TO 500
    CASE (10)
      GO TO 460
  END SELECT
END IF

!  ***  decide whether to change models  ***

e=v(preduc)-v(fdif)
sstep=iv(lky)
s1=iv(s)
CALL slvmul (p, v(sstep:), v(s1:), v(step1:))
sttsst=half*dotprd(p,v(step1:),v(sstep:))
IF (iv(model) == 1) sttsst=-sttsst
IF (ABS(e+sttsst)*v(fuzz) >= ABS(e)) GO TO 330

!     ***  switch models  ***

iv(model)=3-iv(model)
IF (iv(model) == 1) iv(kagqt)=-1
IF (iv(model) == 2.AND.iv(kalm) > 0) iv(kalm)=0
IF (-2 < l) GO TO 380
iv(h)=-ABS(iv(h))
iv(sused)=iv(sused)+2
CALL vcopy (nvsave, v(vsave1:), v)
GO TO 350

330 IF (-3 < l) GO TO 380

!     ***  recompute step with decreased radius  ***

v(radius)=v(radfac)*v(dstnrm)
GO TO 140

!  ***  recompute step, saving v values and r if necessary  ***

340 v(radius)=v(radfac)*v(dstnrm)
350 IF (v(f) >= v(f0)) GO TO 140
rsave1=iv(rsave)
CALL vcopy (n, v(rsave1:), r)
GO TO 140

!  ***  compute step of length v(lmax0) for singular convergence test

360 v(radius)=v(lmax0)
GO TO 180

!  ***  convergence or false convergence  ***

370 iv(cnvcod)=l
IF (v(f) >= v(f0)) GO TO 510
IF (iv(xirc) == 14) GO TO 510
iv(xirc)=14

!. . . . . . . . . . . .  process acceptable step  . . . . . . . . . . .

380 iv(covmat)=0

!  ***  set  lky = (j(x0)**t) * r(x)  ***

lky1=iv(lky)
IF (iv(kalm) >= 0) GO TO 400

!     ***  jacobian has not been modified  ***

DO i=1,p
  v(lky1)=dotprd(n,j(:,i),r)
  lky1=lky1+1
END DO
GO TO 410

!  ***  qrfact has been applied to j.  store copy of r in qtr and  ***
!  ***  apply q to it.                                             ***

400 qtr1=iv(qtr)
CALL vcopy (n, v(qtr1:), r)
CALL qapply (n, p, j, v(qtr1:), iv(ierr))

!  ***  multiply top p-vector in qtr by permuted upper triangle    ***
!  ***  stored by qrfact in j and rd.                              ***

rd1=iv(rd)
temp1=iv(stlstg)
CALL rptmul (3, iv(ipivot:), j, p, v(rd1:), v(qtr1:), v(lky1:), v(temp1:))

!  ***  see whether to set v(radfac) by gradient tests  ***

410 IF (iv(irc) /= 3) GO TO 450
step1=iv(step)
temp1=iv(stlstg)
temp2=iv(x0)

!     ***  set  temp1 = hessian * step  for use in gradient tests  ***

IF (stpmod == 2) GO TO 420

!        ***  step computed using gauss-newton model  ***
!        ***  -- qrfact has been applied to j         ***

rd1=iv(rd)
CALL rptmul (2, iv(ipivot:), j, p, v(rd1:), v(step1:), v(temp1:), v(temp2:))
GO TO 450

!     ***  step computed using augmented model  ***

420 h1=iv(h)
k=temp2
DO i=1,p
  v(k)=d(i)*v(step1)
  k=k+1
  step1=step1+1
END DO
CALL slvmul (p, v(temp1:), v(h1:), v(temp2:))
DO i=1,p
  v(temp1)=d(i)*v(temp1)
  temp1=temp1+1
END DO

!  ***  save old gradient and compute new one  ***

450 iv(ngcall)=iv(ngcall)+1
g1=iv(g)
g01=iv(w)
CALL vcopy (p, v(g01:), v(g1:))
iv(1)=2
GO TO 590

!  ***  initializations -- g0 = g - g0, etc.  ***

460 g01=iv(w)
g1=iv(g)
CALL vaxpy (p, v(g01:), negone, v(g01:), v(g1:))
step1=iv(step)
temp1=iv(stlstg)
temp2=iv(x0)
IF (iv(irc) /= 3) GO TO 490

!  ***  set v(radfac) by gradient tests  ***

!     ***  set  temp1 = d**-1 * (hessian * step  +  (g(x0) - g(x)))  ***

k=temp1
l=g01
DO i=1,p
  v(k)=(v(k)-v(l))/d(i)
  k=k+1
  l=l+1
END DO

!        ***  do gradient tests  ***

IF (v2norm(p,v(temp1:)) <= v(dgnorm)*v(tuner4)) GO TO 480
IF (dotprd(p,v(g1:),v(step1:)) >= v(gtstep)*v(tuner5)) GO TO 490
480 v(radfac)=v(incfac)

!  ***  finish computing lky = ((j(x) - j(x0))**t) * r  ***

!     ***  currently lky = (j(x0)**t) * r  ***

490 lky1=iv(lky)
CALL vaxpy (p, v(lky1:), negone, v(lky1:), v(g1:))

!  ***  determine sizing factor v(size)  ***

!     ***  set temp1 = s * step  ***
s1=iv(s)
CALL slvmul (p, v(temp1:), v(s1:), v(step1:))

t1=ABS(dotprd(p,v(step1:),v(temp1:)))
t=ABS(dotprd(p,v(step1:),v(lky1:)))
v(size)=one
IF (t < t1) v(size)=t/t1

!  ***  update s  ***

CALL slupdt (v(s1:), v(cosmin), p, v(size), v(step1:), v(temp1:),  &
             v(temp2:), v(g01:), v(wscale), v(lky1:))
iv(1)=2
GO TO 90

!. . . . . . . . . . . . . .  misc. details  . . . . . . . . . . . . . .

!  ***  bad parameters to assess  ***

500 iv(1)=14
GO TO 580

!  ***  convergence obtained -- compute covariance matrix if desired ***

510 IF (iv(covreq) == 0.AND.iv(covprt) == 0) GO TO 570
IF (iv(covmat) /= 0) GO TO 570
IF (iv(cnvcod) >= 7) GO TO 570
iv(mode)=0
520 CALL covclc (i, d, iv, j, n, p, r, v, x)

SELECT CASE (i)
  CASE (1:2)
    iv(nfcov)=iv(nfcov)+1
    iv(nfcall)=iv(nfcall)+1
    iv(restor)=i
    iv(1)=1
    GO TO 590
  CASE (3)
    GO TO 550
  CASE (4)
    iv(mode)=0
    IF (iv(niter) == 0) iv(mode)=-1
    GO TO 570
END SELECT

540 IF (iv(restor) == 1.OR.iv(toobig) /= 0) GO TO 520
iv(nfgcal)=iv(nfcall)
550 iv(ngcov)=iv(ngcov)+1
iv(ngcall)=iv(ngcall)+1
iv(1)=2
GO TO 590

570 iv(1)=iv(cnvcod)
iv(cnvcod)=0

!  ***  print summary of final iteration and other requested items  ***

580 CALL itsmry (d, iv, p, v, x)

590 jac(:nn*p) = RESHAPE(j, (/ nn*p /))
RETURN

!  ***  last card of nl2itr follows  ***
END SUBROUTINE nl2itr


SUBROUTINE assess (d, iv, p, step, stlstg, v, x, x0)

!  ***  assess candidate step (nl2sol version 2.2)  ***

INTEGER, INTENT(IN)       :: p
INTEGER, INTENT(IN OUT)   :: iv(:)
REAL (dp), INTENT(IN)     :: d(:), x0(:)
REAL (dp), INTENT(IN OUT) :: step(:), stlstg(:), v(:), x(:)

!  ***  purpose  ***

!        this subroutine is called by an unconstrained minimization
!     routine to assess the next candidate step.  it may recommend one
!     of several courses of action, such as accepting the step, recom-
!     puting it using the same or a new quadratic model, or halting due
!     to convergence or false convergence.  see the return code listing
!     below.

!--------------------------  parameter usage  --------------------------

!     iv (i/o) integer parameter and scratch vector -- see description
!             below of iv values referenced.
!      d (in)  scale vector used in computing v(reldx) -- see below.
!      p (in)  number of parameters being optimized.
!   step (i/o) on input, step is the step to be assessed.  it is un-
!             changed on output unless a previous step achieved a
!             better objective function reduction, in which case stlstg
!             will have been copied to step.
! stlstg (i/o) when assess recommends recomputing step even though the
!             current (or a previous) step yields an objective func-
!             tion decrease, it saves in stlstg the step that gave the
!             best function reduction seen so far (in the current itera-
!             tion).  if the recomputed step yields a larger function
!             value, then step is restored from stlstg and
!             x = x0 + step is recomputed.
!      v (i/o) real parameter and scratch vector -- see description
!             below of v values referenced.
!      x (i/o) on input, x = x0 + step is the point at which the objec-
!             tive function has just been evaluated.  if an earlier
!             step yielded a bigger function decrease, then x is
!             restored to the corresponding earlier value.  otherwise,
!             if the current step does not give any function decrease,
!             then x is restored to x0.
!     x0 (in)  initial objective function parameter vector (at the
!             start of the current iteration).

!  ***  iv values referenced  ***

!    iv(irc) (i/o) on input for the first step tried in a new iteration,
!             iv(irc) should be set to 3 or 4 (the value to which it is
!             set when step is definitely to be accepted).  on input
!             after step has been recomputed, iv(irc) should be
!             unchanged since the previous return of assess.
!                on output, iv(irc) is a return code having one of the
!             following values...
!                  1 = switch models or try smaller step.
!                  2 = switch models or accept step.
!                  3 = accept step and determine v(radfac) by gradient
!                       tests.
!                  4 = accept step, v(radfac) has been determined.
!                  5 = recompute step (using the same model).
!                  6 = recompute step with radius = v(lmax0) but do not
!                       evaulate the objective function.
!                  7 = x-convergence (see v(xctol)).
!                  8 = relative function convergence (see v(rfctol)).
!                  9 = both x- and relative function convergence.
!                 10 = absolute function convergence (see v(afctol)).
!                 11 = singular convergence (see v(lmax0)).
!                 12 = false convergence (see v(xftol)).
!                 13 = iv(irc) was out of range on input.
!             return code i has precdence over i+1 for i = 9, 10, 11.
! iv(mlstgd) (i/o) saved value of iv(model).
!  iv(model) (i/o) on input, iv(model) should be an integer identifying
!             the current quadratic model of the objective function.
!             if a previous step yielded a better function reduction,
!             then iv(model) will be set to iv(mlstgd) on output.
! iv(nfcall) (in)  invocation count for the objective function.
! iv(nfgcal) (i/o) value of iv(nfcall) at step that gave the biggest
!             function reduction this iteration.  iv(nfgcal) remains
!             unchanged until a function reduction is obtained.
! iv(radinc) (i/o) the number of radius increases (or minus the number
!             of decreases) so far this iteration.
! iv(restor) (out) set to 0 unless x and v(f) have been restored, in
!             which case assess sets iv(restor) = 1.
!  iv(stage) (i/o) count of the number of models tried so far in the
!             current iteration.
! iv(stglim) (in)  maximum number of models to consider.
! iv(switch) (out) set to 0 unless a new model is being tried and it
!             gives a smaller function value than the previous model,
!             in which case assess sets iv(switch) = 1.
! iv(toobig) (in)  is nonzero if step was too big (e.g. if it caused
!             overflow).
!   iv(xirc) (i/o) value that iv(irc) would have in the absence of
!             convergence, false convergence, and oversized steps.

!  ***  v values referenced  ***

! v(afctol) (in)  absolute function convergence tolerance.  if the
!             absolute value of the current function value v(f) is less
!             than v(afctol), then assess returns with iv(irc) = 10.
! v(decfac) (in)  factor by which to decrease radius when iv(toobig) is
!             nonzero.
! v(dstnrm) (in)  the 2-norm of d*step.
! v(dstsav) (i/o) value of v(dstnrm) on saved step.
!   v(dst0) (in)  the 2-norm of d times the newton step (when defined,
!             i.e., for v(nreduc) .ge. 0).
!      v(f) (i/o) on both input and output, v(f) is the objective func-
!             tion value at x.  if x is restored to a previous value,
!             then v(f) is restored to the corresponding value.
!   v(fdif) (out) the function reduction v(f0) - v(f) (for the output
!             value of v(f) if an earlier step gave a bigger function
!             decrease, and for the input value of v(f) otherwise).
! v(flstgd) (i/o) saved value of v(f).
!     v(f0) (in)  objective function value at start of iteration.
! v(gtslst) (i/o) value of v(gtstep) on saved step.
! v(gtstep) (in)  inner product between step and gradient.
! v(incfac) (in)  minimum factor by which to increase radius.
!  v(lmax0) (in)  maximum reasonable step size (and initial step bound).
!             if the actual function decrease is no more than twice
!             what was predicted, if a return with iv(irc) = 7, 8, 9,
!             or 10 does not occur, if v(dstnrm) .gt. v(lmax0), and if
!             v(preduc) .le. v(rfctol) * abs(v(f0)), then assess re-
!             turns with iv(irc) = 11.  if so doing appears worthwhile,
!             then assess repeats this test with v(preduc) computed for
!             a step of length v(lmax0) (by a return with iv(irc) = 6).
! v(nreduc) (i/o)  function reduction predicted by quadratic model for
!             newton step.  if assess is called with iv(irc) = 6, i.e.,
!             if v(preduc) has been computed with radius = v(lmax0) for
!             use in the singular convervence test, then v(nreduc) is
!             set to -v(preduc) before the latter is restored.
! v(plstgd) (i/o) value of v(preduc) on saved step.
! v(preduc) (i/o) function reduction predicted by quadratic model for
!             current step.
! v(radfac) (out) factor to be used in determining the new radius,
!             which should be v(radfac)*dst, where  dst  is either the
!             output value of v(dstnrm) or the 2-norm of
!             diag(newd)*step  for the output value of step and the
!             updated version, newd, of the scale vector d.  for
!             iv(irc) = 3, v(radfac) = 1.0 is returned.
! v(rdfcmn) (in)  minimum value for v(radfac) in terms of the input
!             value of v(dstnrm) -- suggested value = 0.1.
! v(rdfcmx) (in)  maximum value for v(radfac) -- suggested value = 4.0.
!  v(reldx) (out) scaled relative change in x caused by step, computed
!             by function  reldst  as
!                 max (d(i)*abs(x(i)-x0(i)), 1 .le. i .le. p) /
!                    max (d(i)*(abs(x(i))+abs(x0(i))), 1 .le. i .le. p).
!             if an acceptable step is returned, then v(reldx) is com-
!             puted using the output (possibly restored) values of x
!             and step.  otherwise it is computed using the input
!             values.
! v(rfctol) (in)  relative function convergence tolerance.  if the
!             actual function reduction is at most twice what was pre-
!             dicted and  v(nreduc) .le. v(rfctol)*abs(v(f0)),  then
!             assess returns with iv(irc) = 8 or 9.  see also v(lmax0).
! v(stppar) (in)  marquardt parameter -- 0 means full newton step.
! v(tuner1) (in)  tuning constant used to decide if the function
!             reduction was much less than expected.  suggested
!             value = 0.1.
! v(tuner2) (in)  tuning constant used to decide if the function
!             reduction was large enough to accept step.  suggested
!             value = 10**-4.
! v(tuner3) (in)  tuning constant used to decide if the radius
!             should be increased.  suggested value = 0.75.
!  v(xctol) (in)  x-convergence criterion.  if step is a newton step
!             (v(stppar) = 0) having v(reldx) .le. v(xctol) and giving
!             at most twice the predicted function decrease, then
!             assess returns iv(irc) = 7 or 9.
!  v(xftol) (in)  false convergence tolerance.  if step gave no or only
!             a small function decrease and v(reldx) .le. v(xftol),
!             then assess returns with iv(irc) = 12.

!-------------------------------  notes  -------------------------------

!  ***  application and usage restrictions  ***

!        this routine is called as part of the nl2sol (nonlinear
!     least-squares) package.  it may be used in any unconstrained
!     minimization solver that uses dogleg, goldfeld-quandt-trotter,
!     or levenberg-marquardt steps.

!  ***  algorithm notes  ***

!        see (1) for further discussion of the assessing and model
!     switching strategies.  while nl2sol considers only two models,
!     assess is designed to handle any number of models.

!  ***  usage notes  ***

!        on the first call of an iteration, only the i/o variables
!     step, x, iv(irc), iv(model), v(f), v(dstnrm), v(gtstep), and
!     v(preduc) need have been initialized.  between calls, no i/o
!     values execpt step, x, iv(model), v(f) and the stopping toler-
!     ances should be changed.
!        after a return for convergence or false convergence, one can
!     change the stopping tolerances and call assess again, in which
!     case the stopping tests will be repeated.

!  ***  references  ***

!     (1) dennis, j.e., jr., gay, d.m., and welsch, r.e. (1981),
!        an adaptive nonlinear least-squares algorithm,
!        acm trans. math. software, vol. 7, no. 3.

!     (2) powell, m.j.d. (1970)  a fortran subroutine for solving
!        systems of nonlinear algebraic equations, in numerical
!        methods for nonlinear algebraic equations, edited by
!        p. rabinowitz, gordon and breach, london.

!  ***  history  ***

!        john dennis designed much of this routine, starting with
!     ideas in (2). roy welsch suggested the model switching strategy.
!        david gay and stephen peters cast this subroutine into a more
!     portable form (winter 1977), and david gay cast it into its
!     present form (fall 1978).

!  ***  general  ***

!     this subroutine was written in connection with research
!     supported by the national science foundation under grants
!     mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989, and
!     mcs-7906671.

!------------------------  external quantities  ------------------------

!  ***  no common blocks  ***

!--------------------------  local variables  --------------------------

LOGICAL   :: goodx
INTEGER   :: i,nfc
REAL (dp) :: emax,gts,reldx1,rfac1,xmax

!  ***  data initializations  ***

REAL (dp), PARAMETER :: half=0.5d+0, one=1.d+0, two=2.d+0, zero=0.d+0

INTEGER, PARAMETER :: irc=3, mlstgd=4, model=5, nfcall=6, nfgcal=7, radinc=8, &
                      restor=9, stage=10, stglim=11, switch=12, toobig=2,   &
                      xirc=13

INTEGER, PARAMETER :: afctol=31, decfac=22, dstnrm=2, dst0=3, dstsav=18,   &
                      f=10, fdif=11, flstgd=12, f0=13, gtslst=14, gtstep=4, &
                      incfac=23, lmax0=35, nreduc=6, plstgd=15, preduc=7,  &
                      radfac=16, rdfcmn=24, rdfcmx=25, reldx=17, rfctol=32, &
                      stppar=5, tuner1=26, tuner2=27, tuner3=28, xctol=33,  &
                      xftol=34

!+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++

nfc=iv(nfcall)
iv(switch)=0
iv(restor)=0
rfac1=one
goodx=.true.
i=iv(irc)
SELECT CASE (i)
  CASE (1)
    GO TO 20
  CASE (2)
    GO TO 30
  CASE (3:4)
    GO TO 10
  CASE (5)
    GO TO 40
  CASE (6)
    GO TO 280
  CASE (7:11)
    GO TO 220
  CASE (12)
    GO TO 170
  CASE DEFAULT
    iv(irc)=13
    GO TO 300
END SELECT

!  ***  initialize for new iteration  ***

10 iv(stage)=1
iv(radinc)=0
v(flstgd)=v(f0)
IF (iv(toobig) == 0) GO TO 90
iv(stage)=-1
iv(xirc)=i
GO TO 60

!  ***  step was recomputed with new model or smaller radius  ***
!  ***  first decide which  ***

20 IF (iv(model) /= iv(mlstgd)) GO TO 30
!        ***  old model retained, smaller radius tried  ***
!        ***  do not consider any more new models this iteration  ***
iv(stage)=iv(stglim)
iv(radinc)=-1
GO TO 90

!  ***  a new model is being tried.  decide whether to keep it.  ***

30 iv(stage)=iv(stage)+1

!     ***  now we add the possibiltiy that step was recomputed with  ***
!     ***  the same model, perhaps because of an oversized step.     ***

40 IF (iv(stage) > 0) GO TO 50

!        ***  step was recomputed because it was too big.  ***

IF (iv(toobig) /= 0) GO TO 60

!        ***  restore iv(stage) and pick up where we left off.  ***

iv(stage)=-iv(stage)
i=iv(xirc)
SELECT CASE (i)
  CASE (1)
    GO TO 20
  CASE (2)
    GO TO 30
  CASE (3:4)
    GO TO 90
  CASE (5)
    GO TO 70
END SELECT

50 IF (iv(toobig) == 0) GO TO 70

!  ***  handle oversize step  ***

IF (iv(radinc) > 0) GO TO 80
iv(stage)=-iv(stage)
iv(xirc)=iv(irc)

60 v(radfac)=v(decfac)
iv(radinc)=iv(radinc)-1
iv(irc)=5
GO TO 300

70 IF (v(f) < v(flstgd)) GO TO 90

!     *** the new step is a loser.  restore old model.  ***

IF (iv(model) == iv(mlstgd)) GO TO 80
iv(model)=iv(mlstgd)
iv(switch)=1

!     ***  restore step, etc. only if a previous step decreased v(f).

80 IF (v(flstgd) >= v(f0)) GO TO 90
iv(restor)=1
v(f)=v(flstgd)
v(preduc)=v(plstgd)
v(gtstep)=v(gtslst)
IF (iv(switch) == 0) rfac1=v(dstnrm)/v(dstsav)
v(dstnrm)=v(dstsav)
nfc=iv(nfgcal)
goodx=.false.


!  ***  compute relative change in x by current step  ***

90 reldx1=reldst(p,d,x,x0)

!  ***  restore x and step if necessary  ***

IF (goodx) GO TO 110
DO i=1,p
  step(i)=stlstg(i)
  x(i)=x0(i)+stlstg(i)
END DO

110 v(fdif)=v(f0)-v(f)
IF (v(fdif) > v(tuner2)*v(preduc)) GO TO 140

!        ***  no (or only a trivial) function decrease
!        ***  -- so try new model or smaller radius

v(reldx)=reldx1
IF (v(f) < v(f0)) GO TO 120
iv(mlstgd)=iv(model)
v(flstgd)=v(f)
v(f)=v(f0)
CALL vcopy (p, x, x0)
iv(restor)=1
GO TO 130
120 iv(nfgcal)=nfc
130 iv(irc)=1
IF (iv(stage) < iv(stglim)) GO TO 160
iv(irc)=5
iv(radinc)=iv(radinc)-1
GO TO 160

!  ***  nontrivial function decrease achieved  ***

140 iv(nfgcal)=nfc
rfac1=one
IF (goodx) v(reldx)=reldx1
v(dstsav)=v(dstnrm)
IF (v(fdif) > v(preduc)*v(tuner1)) GO TO 190

!  ***  decrease was much less than predicted -- either change models
!  ***  or accept step with decreased radius.

IF (iv(stage) >= iv(stglim)) GO TO 150
!        ***  consider switching models  ***
iv(irc)=2
GO TO 160

!     ***  accept step with decreased radius  ***

150 iv(irc)=4

!  ***  set v(radfac) to fletcher*s decrease factor  ***

160 iv(xirc)=iv(irc)
emax=v(gtstep)+v(fdif)
v(radfac)=half*rfac1
IF (emax < v(gtstep)) v(radfac)=rfac1*MAX(v(rdfcmn),half* v(gtstep)/emax)

!  ***  do false convergence test  ***

170 IF (v(reldx) <= v(xftol)) GO TO 180
iv(irc)=iv(xirc)
IF (v(f) < v(f0)) GO TO 200
GO TO 230

180 iv(irc)=12
GO TO 240

!  ***  handle good function decrease  ***

190 IF (v(fdif) < (-v(tuner3)*v(gtstep))) GO TO 210

!     ***  increasing radius looks worthwhile.  see if we just
!     ***  recomputed step with a decreased radius or restored step
!     ***  after recomputing it with a larger radius.

IF (iv(radinc) < 0) GO TO 210
IF (iv(restor) == 1) GO TO 210

!        ***  we did not.  try a longer step unless this was a newton
!        ***  step.

v(radfac)=v(rdfcmx)
gts=v(gtstep)
IF (v(fdif) < (half/v(radfac)-one)*gts) v(radfac)=MAX(v(incfac)  &
,half*gts/(gts+v(fdif)))
iv(irc)=4
IF (v(stppar) == zero) GO TO 230
!             ***  step was not a newton step.  recompute it with
!             ***  a larger radius.
iv(irc)=5
iv(radinc)=iv(radinc)+1

!  ***  save values corresponding to good step  ***

200 v(flstgd)=v(f)
iv(mlstgd)=iv(model)
CALL vcopy (p, stlstg, step)
v(dstsav)=v(dstnrm)
iv(nfgcal)=nfc
v(plstgd)=v(preduc)
v(gtslst)=v(gtstep)
GO TO 230

!  ***  accept step with radius unchanged  ***

210 v(radfac)=one
iv(irc)=3
GO TO 230

!  ***  come here for a restart after convergence  ***

220 iv(irc)=iv(xirc)
IF (v(dstsav) >= zero) GO TO 240
iv(irc)=12
GO TO 240

!  ***  perform convergence tests  ***

230 iv(xirc)=iv(irc)
240 IF (ABS(v(f)) < v(afctol)) iv(irc)=10
IF (half*v(fdif) > v(preduc)) GO TO 300
emax=v(rfctol)*ABS(v(f0))
IF (v(dstnrm) > v(lmax0).AND.v(preduc) <= emax) iv(irc)=11
IF (v(dst0) < zero) GO TO 250
i=0
IF ((v(nreduc) > zero.AND.v(nreduc) <= emax) .OR.   &
   (v(nreduc) == zero .AND. v(preduc) == zero)) i=2
IF (v(stppar) == zero.AND.v(reldx) <= v(xctol).AND.goodx) i=i+1
IF (i > 0) iv(irc)=i+6

!  ***  consider recomputing step of length v(lmax0) for singular
!  ***  convergence test.

250 IF (ABS(iv(irc)-3) > 2.AND.iv(irc) /= 12) GO TO 300
IF (v(dstnrm) > v(lmax0)) GO TO 260
IF (v(preduc) >= emax) GO TO 300
IF (v(dst0) <= zero) GO TO 270
IF (half*v(dst0) <= v(lmax0)) GO TO 300
GO TO 270
260 IF (half*v(dstnrm) <= v(lmax0)) GO TO 300
xmax=v(lmax0)/v(dstnrm)
IF (xmax*(two-xmax)*v(preduc) >= emax) GO TO 300
270 IF (v(nreduc) < zero) GO TO 290

!  ***  recompute v(preduc) for use in singular convergence test  ***

v(gtslst)=v(gtstep)
v(dstsav)=v(dstnrm)
IF (iv(irc) == 12) v(dstsav)=-v(dstsav)
v(plstgd)=v(preduc)
iv(irc)=6
CALL vcopy (p, stlstg, step)
GO TO 300

!  ***  perform singular convergence test with recomputed v(preduc)  ***

280 v(gtstep)=v(gtslst)
v(dstnrm)=ABS(v(dstsav))
CALL vcopy (p, step, stlstg)
iv(irc)=iv(xirc)
IF (v(dstsav) <= zero) iv(irc)=12
v(nreduc)=-v(preduc)
v(preduc)=v(plstgd)
290 IF (-v(nreduc) <= v(rfctol)*ABS(v(f0))) iv(irc)=11

300 RETURN

!  ***  last card of assess follows  ***
END SUBROUTINE assess


SUBROUTINE covclc (covirc, d, iv, j, n, p, r, v, x)

!  ***  compute covariance matrix for nl2itr (nl2sol version 2.2)  ***

!  ***  let k = ABS(iv(covreq).  for k <= 2, a finite-difference
!  ***  hessian h is computed (using func. and grad. values if
!  ***  iv(covreq) is nonnegative, and using only func. values if
!  ***  iv(covreq) is negative).  for scale = 2*f(x) / max(1, n-p),
!  ***  where 2*f(x) is the residual sum of squares, covclc computes...
!  ***             k = 0 or 1...  scale * h**-1 * (j**t * j) * h**-1.
!  ***             k = 2...  scale * h**-1.
!  ***             k >= 3...  scale * (j**t * j)**-1.

!  ***  parameter declarations  ***

INTEGER, INTENT(IN)       :: n, p
INTEGER, INTENT(IN OUT)   :: covirc, iv(:)
REAL (dp), INTENT(IN)     :: d(:)
REAL (dp), INTENT(IN OUT) :: v(:), x(:), j(:,:)
REAL (dp), INTENT(OUT)    :: r(:)
!     dimension iv(*), v(*), j(nn,p), r(n)

!  ***  local variables  ***

LOGICAL       :: havej
INTEGER       :: gp,gsave1,g1,hc,hmi,hpi,hpm,i,ipivi,ipivk,ip1,irc,k,  &
                 kind,kl,l,m,mm1,mm1o2,pp1o2,qtr1,rd1,stpi,stpm,stp0,wl,w0,w1
REAL (dp)     :: del, t, wk
INTEGER, SAVE :: cov

! linvrt... invert lower triangular matrix.
! litvmu... apply inverse-transpose of compact lower triang. matrix.
! livmul... apply inverse of compact lower triang. matrix.
! lsqrt.... compute cholesky factor of (lower trinag. of) a sym. matrix.
! ltsqar... given lower triang. matrix l, compute (l**t)*l.
! qrfact... compute qr decomposition of a matrix.
! vcopy.... copy one vector to another.
! vscopy... set all elements of a vector to a scalar.

!  ***  subscripts for iv and v  ***

REAL (dp), PARAMETER :: half=0.5d+0, negpt5=-0.5d+0, one=1.d+0,  &
                               two=2.d+0, zero=0.d+0

INTEGER, PARAMETER :: covmat=26, covreq=15, delta=50, delta0=44, dltfdc=40,  &
                      f=10, fx=46, g=28, h=44, ierr=32, ipivot=61, ipiv0=60, &
                      kagqt=35, kalm=36, lmat=58, mode=38, nfgcal=7, qtr=49, &
                      rd=51, rsave=52, savei=54, switch=12, toobig=2, w=59,  &
                      xmsave=49

!+++++++++++++++++++++++++++++++  body  ++++++++++++++++++++++++++++++++

covirc=4
kind=iv(covreq)
m=iv(mode)
IF (m > 0) GO TO 10
iv(kagqt)=-1
IF (iv(kalm) > 0) iv(kalm)=0
IF (ABS(kind) >= 3) GO TO 310
v(fx)=v(f)
k=iv(rsave)
CALL vcopy (n, v(k:), r)
10 IF (m > p) GO TO 220
IF (kind < 0) GO TO 110

!  ***  compute finite-difference hessian using both function and
!  ***  gradient values.

gsave1=iv(w)+p
g1=iv(g)
IF (m > 0) GO TO 20
!        ***  first call on covclc.  set gsave = g, take first step  ***
CALL vcopy (p, v(gsave1:), v(g1:))
iv(switch)=iv(nfgcal)
GO TO 90

20 del=v(delta)
x(m)=v(xmsave)
IF (iv(toobig) == 0) GO TO 40

!     ***  handle oversize v(delta)  ***

IF (del*x(m) > zero) GO TO 30
!             ***  we already tried shrinking v(delta), so quit  ***
iv(covmat)=-2
GO TO 210

!        ***  try shrinking v(delta)  ***
30 del=negpt5*del
GO TO 100

40 cov=iv(lmat)
gp=g1+p-1

!  ***  set  g = (g - gsave)/del  ***

DO i=g1,gp
  v(i)=(v(i)-v(gsave1))/del
  gsave1=gsave1+1
END DO

!  ***  add g as new col. to finite-diff. hessian matrix  ***

k=cov+m*(m-1)/2
l=k+m-2
IF (m == 1) GO TO 70

!  ***  set  h(i,m) = 0.5 * (h(i,m) + g(i))  for i = 1 to m-1  ***

DO i=k,l
  v(i)=half*(v(i)+v(g1))
  g1=g1+1
END DO

!  ***  add  h(i,m) = g(i)  for i = m to p  ***

70 l=l+1
DO i=m,p
  v(l)=v(g1)
  l=l+i
  g1=g1+1
END DO

90 m=m+1
iv(mode)=m
IF (m > p) GO TO 210

!  ***  choose next finite-difference step, return to get g there  ***

del=v(delta0)*MAX(one/d(m),ABS(x(m)))
IF (x(m) < zero) del=-del
v(xmsave)=x(m)
100 x(m)=x(m)+del
v(delta)=del
covirc=2
GO TO 390

!  ***  compute finite-difference hessian using function values only.

110 stp0=iv(w)+p-1
mm1=m-1
mm1o2=m*mm1/2
IF (m > 0) GO TO 120
!        ***  first call on covclc.  ***
iv(savei)=0
GO TO 200

120 i=iv(savei)
IF (i > 0) GO TO 180
IF (iv(toobig) == 0) GO TO 140

!     ***  handle oversize step  ***

stpm=stp0+m
del=v(stpm)
IF (del*x(xmsave) > zero) GO TO 130
!             ***  we already tried shrinking the step, so quit  ***
iv(covmat)=-2
GO TO 390

!        ***  try shrinking the step  ***
130 del=negpt5*del
x(m)=x(xmsave)+del
v(stpm)=del
covirc=1
GO TO 390

!  ***  save f(x + stp(m)*e(m)) in h(p,m)  ***

140 pp1o2=p*(p-1)/2
cov=iv(lmat)
hpm=cov+pp1o2+mm1
v(hpm)=v(f)

!  ***  start computing row m of the finite-difference hessian h.  ***

hmi=cov+mm1o2
IF (mm1 == 0) GO TO 160
hpi=cov+pp1o2
DO i=1,mm1
  v(hmi)=v(fx)-(v(f)+v(hpi))
  hmi=hmi+1
  hpi=hpi+1
END DO
160 v(hmi)=v(f)-two*v(fx)

!  ***  compute function values needed to complete row m of h.  ***

i=1

170 iv(savei)=i
stpi=stp0+i
v(delta)=x(i)
x(i)=x(i)+v(stpi)
IF (i == m) x(i)=v(xmsave)-v(stpi)
covirc=1
GO TO 390

180 x(i)=v(delta)
IF (iv(toobig) == 0) GO TO 190
!        ***  punt in the event of an oversize step  ***
iv(covmat)=-2
GO TO 390

!  ***  finish computing h(m,i)  ***

190 stpi=stp0+i
hmi=cov+mm1o2+i-1
stpm=stp0+m
v(hmi)=(v(hmi)+v(f))/(v(stpi)*v(stpm))
i=i+1
IF (i <= m) GO TO 170
iv(savei)=0
x(m)=v(xmsave)

200 m=m+1
iv(mode)=m
IF (m > p) GO TO 210

!  ***  prepare to compute row m of the finite-difference hessian h.
!  ***  compute m-th step size stp(m), then return to obtain
!  ***  f(x + stp(m)*e(m)), where e(m) = m-th std. unit vector.

del=v(dltfdc)*MAX(one/d(m),ABS(x(m)))
IF (x(m) < zero) del=-del
v(xmsave)=x(m)
x(m)=x(m)+del
stpm=stp0+m
v(stpm)=del
covirc=1
GO TO 390

!  ***  restore r, v(f), etc.  ***

210 k=iv(rsave)
CALL vcopy (n, r, v(k:))
v(f)=v(fx)
IF (kind < 0) GO TO 220
iv(nfgcal)=iv(switch)
qtr1=iv(qtr)
CALL vcopy (n, v(qtr1:), r)
IF (iv(covmat) < 0) GO TO 390
covirc=3
GO TO 390

220 cov=iv(lmat)

!  ***  the complete finite-diff. hessian is now stored at v(cov).   ***
!  ***  use it to compute the requested covariance matrix.           ***

!     ***  compute cholesky factor c of h = c*(c**t)  ***
!     ***  and store it at v(hc).  ***

hc=cov
IF (ABS(kind) == 2) GO TO 230
hc=ABS(iv(h))
iv(h)=-hc
230 CALL lsqrt (1, p, v(hc:), v(cov:), irc)
iv(covmat)=-1
IF (irc /= 0) GO TO 390

w1=iv(w)+p
IF (ABS(kind) > 1) GO TO 340

!  ***  covariance = scale * h**-1 * (j**t * j) * h**-1  ***

CALL vscopy (p*(p+1)/2, v(cov:), zero)
havej=iv(kalm) == (-1)
!     ***  havej = .true. means j is in its original form, while
!     ***  havej = .false. means qrfact has been applied to j.

m=p
IF (havej) m=n
w0=w1-1
rd1=iv(rd)
DO i=1,m
  IF (havej) GO TO 250
  
!        ***  set w = ipivot * (row i of r matrix from qrfact).  ***
  
  CALL vscopy (p, v(w1:), zero)
  ipivi=ipiv0+i
  l=w0+iv(ipivi)
  v(l)=v(rd1)
  rd1=rd1+1
  IF (i == p) GO TO 270
  ip1=i+1
  DO k=ip1,p
    ipivk=ipiv0+k
    l=w0+iv(ipivk)
    v(l)=j(i,k)
  END DO
  GO TO 270
  
!        ***  set w = (row i of j).  ***
  
  250 l=w0
  DO k=1,p
    l=l+1
    v(l)=j(i,k)
  END DO
  
!        ***  set w = h**-1 * w.  ***
  
  270 CALL livmul (p, v(w1:), v(hc:), v(w1:))
  CALL litvmu (p, v(w1:), v(hc:), v(w1:))
  
!        ***  add  w * w**t  to covariance matrix.  ***
  
  kl=cov
  DO k=1,p
    l=w0+k
    wk=v(l)
    DO l=1,k
      wl=w0+l
      v(kl)=v(kl) + wk*v(wl)
      kl=kl+1
    END DO
  END DO
END DO
GO TO 370

!  ***  covariance = scale * (j**t * j)**-1.  ***

310 rd1=iv(rd)
IF (iv(kalm) /= (-1)) GO TO 320

!        ***  apply qrfact to j  ***

qtr1=iv(qtr)
CALL vcopy (n, v(qtr1:), r)
w1=iv(w)+p
CALL qrfact (n, p, j, v(rd1:), iv(ipivot:), iv(ierr), 0, v(w1:))
iv(kalm)=-2
320 iv(covmat)=-1
IF (iv(ierr) /= 0) GO TO 390
cov=iv(lmat)
hc=ABS(iv(h))
iv(h)=-hc

!     ***  set hc = (r matrix from qrfact).  ***

l=hc
DO i=1,p
  IF (i > 1) CALL vcopy (i-1, v(l:), j(:,i))
  l=l+i-1
  v(l)=v(rd1)
  l=l+1
  rd1=rd1+1
END DO

!  ***  the cholesky factor c of the unscaled inverse covariance matrix
!  ***  (or permutation thereof) is stored at v(hc).

!  ***  set c = c**-1.

340 CALL linvrt (p, v(hc:), v(hc:))

!  ***  set c = c**t * c.

CALL ltsqar (p, v(hc:), v(hc:))

IF (hc == cov) GO TO 370

!     ***  c = permuted, unscaled covariance.
!     ***  set cov = ipivot * c * ipivot**t.

DO i=1,p
  m=ipiv0+i
  ipivi=iv(m)
  kl=cov-1+ipivi*(ipivi-1)/2
  DO k=1,i
    m=ipiv0+k
    ipivk=iv(m)
    l=kl+ipivk
    IF (ipivk > ipivi) l=l+(ipivk-ipivi)*(ipivk+ipivi-3)/2
    v(l)=v(hc)
    hc=hc+1
  END DO
END DO

370 iv(covmat)=cov

!  ***  apply scale factor = (resid. sum of squares) / max(1,n-p).

t=v(f)/(half*REAL(MAX(1,n-p)))
k=cov-1+p*(p+1)/2
v(cov:k)=t*v(cov:k)

390 RETURN
!  ***  last card of covclc follows  ***
END SUBROUTINE covclc


SUBROUTINE dfault (iv, v)

!  ***  supply nl2sol (version 2.2) default values to iv and v  ***

INTEGER, INTENT(OUT)   :: iv(:)
REAL (dp), INTENT(OUT) :: v(:)

REAL (dp) :: machep, mepcrt, sqteps

REAL (dp), PARAMETER :: one=1.d+0, three=3.d+0

!  ***  iv subscript values  ***

INTEGER, PARAMETER :: covprt=14, covreq=15, dtype=16, inits=25, mxfcal=17,   &
                      mxiter=18, outlev=19, parprt=20, prunit=21, solprt=22, &
                      statpr=23, x0prt=24

!  ***  v subscript values  ***

INTEGER, PARAMETER :: afctol=31, cosmin=43, decfac=22, delta0=44, dfac=41,  &
                      dinit=38, dltfdc=40, dltfdj=36, d0init=37, epslon=19, &
                      fuzz=45, incfac=23, jtinit=39, lmax0=35, phmnfc=20,   &
                      phmxfc=21, rdfcmn=24, rdfcmx=25, rfctol=32, rlimit=42, &
                      tuner1=26, tuner2=27, tuner3=28, tuner4=29, tuner5=30, &
                      xctol=33, xftol=34

!-----------------------------------------------------------------------

iv(1)=12
iv(covprt)=0     !def=1
iv(covreq)=0     !def=1
iv(dtype)=1
iv(inits)=0
iv(mxfcal)=200
!!!!!!corrado   iv(mxiter)=150
!angelaiv(mxiter)=150
iv(mxiter)=50
iv(outlev)=1
iv(parprt)=1
iv(prunit)=0     !def=imdcon(1)   
iv(solprt)=0     !def=1
iv(statpr)=0     !def=1
iv(x0prt)=0      !def=1

machep=rmdcon(3)
v(afctol)=1.d-20
IF (machep > 1.d-10) v(afctol)=machep**2
v(cosmin)=MAX(1.d-6,1.d+2*machep)
v(decfac)=0.5D+0
sqteps=rmdcon(4)
v(delta0)=sqteps
v(dfac)=0.6D+0
v(dinit)=0.d+0
mepcrt=machep**(one/three)
v(dltfdc)=mepcrt
v(dltfdj)=sqteps
v(d0init)=1.d+0
v(epslon)=0.1D+0
v(fuzz)=1.5D+0
v(incfac)=2.d+0
v(jtinit)=1.d-6
v(lmax0)=500.d+0       !def=100.d+0
v(phmnfc)=-0.1D+0
v(phmxfc)=0.1D+0
v(rdfcmn)=0.1D+0
v(rdfcmx)=4.d+0
v(rfctol)=MAX(1.d-10,mepcrt**2)
v(rlimit)=rmdcon(5)
v(tuner1)=0.1D+0
v(tuner2)=1.d-4
v(tuner3)=0.75D+0
v(tuner4)=0.5D+0
v(tuner5)=0.75D+0
!v(xctol)=sqteps
!angelav(xctol)=1.d-5  ! corrado ha modificato il default
!v(xctol)=1.d-3  ! corrado ha modificato il default
v(xctol)=1.d-5  ! corrado ha modificato il default
v(xftol)=1.d+2*machep

RETURN
!  ***  last card of dfault follows  ***
END SUBROUTINE dfault


FUNCTION dotprd (p, x, y) RESULT(fn_val)

!  ***  return the inner product of the p-vectors x and y.  ***

INTEGER, INTENT(IN)   :: p
REAL (dp), INTENT(IN) :: x(:),y(:)
REAL (dp)             :: fn_val

INTEGER   :: i
REAL (dp) :: t

!  ***  rmdcon(2) returns a machine-dependent constant, sqteta, which
!  ***  is slightly larger than the smallest positive number that
!  ***  can be squared without underflowing.

REAL (dp), PARAMETER :: one=1.d+0, zero=0.d+0
REAL (dp), SAVE      :: sqteta=0.d+0

fn_val=zero
IF (p <= 0) GO TO 30
IF (sqteta == zero) sqteta=rmdcon(2)
DO i=1,p
  t=MAX(ABS(x(i)),ABS(y(i)))
  IF (t > one) GO TO 10
  IF (t < sqteta) CYCLE
  t=(x(i)/sqteta)*y(i)
  IF (ABS(t) < sqteta) CYCLE
  10 fn_val=fn_val + x(i)*y(i)
END DO

30 RETURN
!  ***  last card of dotprd follows  ***
END FUNCTION dotprd


SUBROUTINE dupdat (d, iv, j, n, p, v)

!  ***  update scale vector d for nl2itr (nl2sol version 2.2)  ***

!  ***  parameter declarations  ***

INTEGER, INTENT(IN)    :: iv(:), n, p
REAL (dp), INTENT(IN)  :: j(:,:), v(:)
REAL (dp), INTENT(OUT) :: d(:)
!     dimension iv(*), v(*), j(nn,p)

!  ***  local variables  ***

INTEGER   :: d0,i,jtoli,s1
REAL (dp) :: sii,t,vdfac

REAL (dp) :: zero=0.d+0

!  ***  subscripts for iv and v  ***

INTEGER, PARAMETER :: dfac=41, dtype=16, jtol0=86, niter=31, s=53

!-----------------------------------------------------------------------

i=iv(dtype)
IF (i == 1) GO TO 10
IF (iv(niter) > 0) GO TO 30

10 vdfac=v(dfac)
d0=jtol0+p
s1=iv(s)-1
DO i=1,p
  s1=s1+i
  sii=v(s1)
  t=v2norm(n,j(:,i))
  IF (sii > zero) t=SQRT(t*t+sii)
  jtoli=jtol0+i
  d0=d0+1
  IF (t < v(jtoli)) t=MAX(v(d0),v(jtoli))
  d(i)=MAX(vdfac*d(i),t)
END DO

30 RETURN
!  ***  last card of dupdat follows  ***
END SUBROUTINE dupdat


SUBROUTINE gqtstp (d, dig, dihdi, ka, l, p, step, v, w)

!  *** compute goldfeld-quandt-trotter step by more-hebden technique ***
!  ***  (nl2sol version 2.2)  ***

!  ***  parameter declarations  ***

INTEGER, INTENT(IN)       :: p
INTEGER, INTENT(IN OUT)   :: ka
REAL (dp), INTENT(IN)     :: d(:), dig(:)
REAL (dp), INTENT(IN OUT) :: l(:), v(:), step(:), w(:), dihdi(:)
!     dimension dihdi(p*(p+1)/2), l(p*(p+1)/2), w(4*p+7)

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!  ***  purpose  ***

!        given the (compactly stored) lower triangle of a scaled
!     hessian (approximation) and a nonzero scaled gradient vector,
!     this subroutine computes a goldfeld-quandt-trotter step of
!     approximate length v(radius) by the more-hebden technique.  in
!     other words, step is computed to (approximately) minimize
!     psi(step) = (g**t)*step + 0.5*(step**t)*h*step  such that the
!     2-norm of d*step is at most (approximately) v(radius), where
!     g  is the gradient,  h  is the hessian, and  d  is a diagonal
!     scale matrix whose diagonal is stored in the parameter d.
!     (gqtstp assumes  dig = d**-1 * g  and  dihdi = d**-1 * h * d**-1.)
!     if g = 0, however, step = 0 is returned (even at a saddle point).

!  ***  parameter description  ***

!     d (in)  = the scale vector, i.e. the diagonal of the scale
!              matrix  d  mentioned above under purpose.
!   dig (in)  = the scaled gradient vector, d**-1 * g.  if g = 0, then
!              step = 0 and  v(stppar) = 0 are returned.
! dihdi (in)  = lower triangle of the scaled hessian (approximation),
!              i.e., d**-1 * h * d**-1, stored compactly by rows., i.e.,
!              in the order (1,1), (2,1), (2,2), (3,1), (3,2), etc.
!    ka (i/o) = the number of hebden iterations (so far) taken to deter-
!              mine step.  ka .lt. 0 on input means this is the first
!              attempt to determine step (for the present dig and dihdi)
!              -- ka is initialized to 0 in this case.  output with
!              ka = 0 (or v(stppar) = 0)  means  step = -(h**-1)*g.
!     l (i/o) = workspace of length p*(p+1)/2 for cholesky factors.
!     p (in)  = number of parameters -- the hessian is a  p x p  matrix.
!  step (i/o) = the step computed.
!     v (i/o) contains various constants and variables described below.
!     w (i/o) = workspace of length 4*p + 6.

!  ***  entries in v  ***

! v(dgnorm) (i/o) = 2-norm of (d**-1)*g.
! v(dstnrm) (output) = 2-norm of d*step.
! v(dst0)   (i/o) = 2-norm of d*(h**-1)*g (for pos. def. h only), or
!             overestimate of smallest eigenvalue of (d**-1)*h*(d**-1).
! v(epslon) (in)  = max. rel. error allowed for psi(step).  for the
!             step returned, psi(step) will exceed its optimal value
!             by less than -v(epslon)*psi(step).  suggested value = 0.1.
! v(gtstep) (out) = inner product between g and step.
! v(nreduc) (out) = psi(-(h**-1)*g) = psi(newton step)  (for pos. def.
!             h only -- v(nreduc) is set to zero otherwise).
! v(phmnfc) (in)  = tol. (together with v(phmxfc)) for accepting step
!             (more*s sigma).  the error v(dstnrm) - v(radius) must lie
!             between v(phmnfc)*v(radius) and v(phmxfc)*v(radius).
! v(phmxfc) (in)  (see v(phmnfc).)
!             suggested values -- v(phmnfc) = -0.25, v(phmxfc) = 0.5.
! v(preduc) (out) = psi(step) = predicted obj. func. reduction for step.
! v(radius) (in)  = radius of current (scaled) trust region.
! v(rad0)   (i/o) = value of v(radius) from previous call.
! v(stppar) (i/o) is normally the marquardt parameter, i.e. the alpha
!             described below under algorithm notes.  if h + alpha*d**2
!             (see algorithm notes) is (nearly) singular, however,
!             then v(stppar) = -alpha.

!  ***  usage notes  ***

!     if it is desired to recompute step using a different value of
!     v(radius), then this routine may be restarted by calling it
!     with all parameters unchanged except v(radius).  (this explains
!     why step and w are listed as i/o).  on an intiial call (one with
!     ka .lt. 0), step and w need not be initialized and only compo-
!     nents v(epslon), v(stppar), v(phmnfc), v(phmxfc), v(radius), and
!     v(rad0) of v must be initialized.  to compute step from a saddle
!     point (where the true gradient vanishes and h has a negative
!     eigenvalue), a nonzero g with small components should be passed.

!  ***  application and usage restrictions  ***

!     this routine is called as part of the nl2sol (nonlinear least-
!     squares) package (ref. 1), but it could be used in solving any
!     unconstrained minimization problem.

!  ***  algorithm notes  ***

!        the desired g-q-t step (ref. 2, 3, 4) satisfies
!     (h + alpha*d**2)*step = -g  for some nonnegative alpha such that
!     h + alpha*d**2 is positive semidefinite.  alpha and step are
!     computed by a scheme analogous to the one described in ref. 5.
!     estimates of the smallest and largest eigenvalues of the hessian
!     are obtained from the gerschgorin circle theorem enhanced by a
!     simple form of the scaling described in ref. 6.  cases in which
!     h + alpha*d**2 is nearly (or exactly) singular are handled by
!     the technique discussed in ref. 2.  in these cases, a step of
!     (exact) length v(radius) is returned for which psi(step) exceeds
!     its optimal value by less than -v(epslon)*psi(step).

!  ***  functions and subroutines called  ***

! dotprd - returns inner product of two vectors.
! litvmu - applies inverse-transpose of compact lower triang. matrix.
! livmul - applies inverse of compact lower triang. matrix.
! lsqrt  - finds cholesky factor (of compactly stored lower triang.).
! lsvmin - returns approx. to min. sing. value of lower triang. matrix.
! rmdcon - returns machine-dependent constants.
! v2norm - returns 2-norm of a vector.

!  ***  references  ***

! 1.  dennis, j.e., gay, d.m., and welsch, r.e. (1981), an adaptive
!             nonlinear least-squares algorithm, acm trans. math.
!             software, vol. 7, no. 3.
! 2.  gay, d.m. (1981), computing optimal locally constrained steps,
!             siam j. sci. statist. computing, vol. 2, no. 2, pp.
!             186-197.
! 3.  goldfeld, s.m., quandt, r.e., and trotter, h.f. (1966),
!             maximization by quadratic hill-climbing, econometrica 34,
!             pp. 541-551.
! 4.  hebden, m.d. (1973), an algorithm for minimization using exact
!             second derivatives, report t.p. 515, theoretical physics
!             div., a.e.r.e. harwell, oxon., england.
! 5.  more, j.j. (1978), the levenberg-marquardt algorithm, implemen-
!             tation and theory, pp.105-116 of springer lecture notes
!             in mathematics no. 630, edited by g.a. watson, springer-
!             verlag, berlin and new york.
! 6.  varga, r.s. (1965), minimal gerschgorin sets, pacific j. math. 15,
!             pp. 719-729.

!  ***  general  ***

!     coded by david m. gay.
!     this subroutine was written in connection with research
!     supported by the national science foundation under grants
!     mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989, and
!     mcs-7906671.

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!  ***  local variables  ***

LOGICAL   :: restrt
INTEGER   :: dggdmx,diag,diag0,dstsav,emax,emin,i,im1,inc,irc,j,k,  &
             kalim,k1,lk0,phipin,q,q0,uk0,x,x0
REAL (dp) :: alphak,aki,akk,delta,dst,epso6,lk,oldphi,phi,  &
                    phimax,phimin,psifac,rad,root,si,sk,sw,t,twopsi,t1,uk,wi

!  ***  subscripts for v  ***

INTEGER, PARAMETER :: dgnorm=1, dstnrm=2, dst0=3, epslon=19, gtstep=4,   &
                      nreduc=6, phmnfc=20, phmxfc=21, preduc=7, radius=8, &
                      rad0=9, stppar=5

REAL (dp), PARAMETER :: epsfac=50.0d+0, four=4.0d+0, half=0.5d+0,  &
                               kappa=2.0d+0, negone=-1.0d+0, one=1.0d+0,  &
                               p001=1.0d-3, six=6.0d+0, three=3.0d+0,  &
                               two=2.0d+0, zero=0.0d+0
!     save dgxfac
REAL (dp), SAVE :: dgxfac = 0.d+0

!  ***  body  ***

!     ***  store largest abs. entry in (d**-1)*h*(d**-1) at w(dggdmx).
dggdmx=p+1
!     ***  store gerschgorin over- and underestimates of the largest
!     ***  and smallest eigenvalues of (d**-1)*h*(d**-1) at w(emax)
!     ***  and w(emin) respectively.
emax=dggdmx+1
emin=emax+1
!     ***  for use in recomputing step, the final values of lk, uk, dst,
!     ***  and the inverse derivative of more*s phi at 0 (for pos. def.
!     ***  h) are stored in w(lk0), w(uk0), w(dstsav), and w(phipin)
!     ***  respectively.
lk0=emin+1
phipin=lk0+1
uk0=phipin+1
dstsav=uk0+1
!     ***  store diag of (d**-1)*h*(d**-1) in w(diag),...,w(diag0+p).
diag0=dstsav
diag=diag0+1
!     ***  store -d*step in w(q),...,w(q0+p).
q0=diag0+p
q=q0+1
rad=v(radius)
!     ***  phitol = max. error allowed in dst = v(dstnrm) = 2-norm of
!     ***  d*step.
phimax=v(phmxfc)*rad
phimin=v(phmnfc)*rad
!     ***  epso6 and psifac are used in checking for the special case
!     ***  of (nearly) singular h + alpha*d**2 (see ref. 2).
psifac=two*v(epslon)/(three*(four*(v(phmnfc)+one)*(kappa+one)+  &
kappa+two)*rad**2)
!     ***  oldphi is used to detect limits of numerical accuracy.  if
!     ***  we recompute step and it does not change, then we accept it.
oldphi=zero
epso6=v(epslon)/six
irc=0
restrt=.false.
kalim=ka+50

!  ***  start or restart, depending on ka  ***

IF (ka >= 0) GO TO 290

!  ***  fresh start  ***

k=0
uk=negone
ka=0
kalim=50

!     ***  store diag(dihdi) in w(diag0+1),...,w(diag0+p)  ***

j=0
DO i=1,p
  j=j+i
  k1=diag0+i
  w(k1)=dihdi(j)
END DO

!     ***  determine w(dggdmx), the largest element of dihdi  ***

t1=zero
j=p*(p+1)/2
DO i=1,j
  t=ABS(dihdi(i))
  IF (t1 < t) t1=t
END DO
w(dggdmx)=t1

!  ***  try alpha = 0 ***

30 CALL lsqrt (1, p, l, dihdi, irc)
IF (irc == 0) GO TO 50
!        ***  indef. h -- underestimate smallest eigenvalue, use this
!        ***  estimate to initialize lower bound lk on alpha.
j=irc*(irc+1)/2
t=l(j)
l(j)=one
w(1:irc) = zero
w(irc)=one
CALL litvmu (irc, w, l, w)
t1=v2norm(irc,w)
lk=-t/t1/t1
v(dst0)=-lk
IF (restrt) GO TO 200
v(nreduc)=zero
GO TO 60

!     ***  positive definite h -- compute unmodified newton step.  ***
50 lk=zero
CALL livmul (p, w(q:), l, dig)
v(nreduc)=half*dotprd(p,w(q:),w(q:))
CALL litvmu (p, w(q:), l, w(q:))
dst=v2norm(p,w(q:))
v(dst0)=dst
phi=dst-rad
IF (phi <= phimax) GO TO 260
IF (restrt) GO TO 200

!  ***  prepare to compute gerschgorin estimates of largest (and
!  ***  smallest) eigenvalues.  ***

60 v(dgnorm)=v2norm(p,dig)
IF (v(dgnorm) == zero) GO TO 430
k=0
DO i=1,p
  wi=zero
  IF (i == 1) GO TO 80
  im1=i-1
  DO j=1,im1
    k=k+1
    t=ABS(dihdi(k))
    wi=wi+t
    w(j)=w(j)+t
  END DO
  80 w(i)=wi
  k=k+1
END DO

!  ***  (under-)estimate smallest eigenvalue of (d**-1)*h*(d**-1)  ***

k=1
t1=w(diag)-w(1)
IF (p <= 1) GO TO 110
DO i=2,p
  j=diag0+i
  t=w(j)-w(i)
  IF (t >= t1) CYCLE
  t1=t
  k=i
END DO

110 sk=w(k)
j=diag0+k
akk=w(j)
k1=k*(k-1)/2+1
inc=1
t=zero
DO i=1,p
  IF (i == k) GO TO 120
  aki=ABS(dihdi(k1))
  si=w(i)
  j=diag0+i
  t1=half*(akk-w(j)+si-aki)
  t1=t1+SQRT(t1*t1+sk*aki)
  IF (t < t1) t=t1
  IF (i < k) GO TO 130
  120 inc=i
  130 k1=k1+inc
END DO

w(emin)=akk-t
uk=v(dgnorm)/rad-w(emin)

!  ***  compute gerschgorin (over-)estimate of largest eigenvalue  ***

k=1
t1=w(diag)+w(1)
IF (p <= 1) GO TO 160
DO i=2,p
  j=diag0+i
  t=w(j)+w(i)
  IF (t <= t1) CYCLE
  t1=t
  k=i
END DO

160 sk=w(k)
j=diag0+k
akk=w(j)
k1=k*(k-1)/2+1
inc=1
t=zero
DO i=1,p
  IF (i == k) GO TO 170
  aki=ABS(dihdi(k1))
  si=w(i)
  j=diag0+i
  t1=half*(w(j)+si-aki-akk)
  t1=t1+SQRT(t1*t1+sk*aki)
  IF (t < t1) t=t1
  IF (i < k) GO TO 180
  170 inc=i
  180 k1=k1+inc
END DO

w(emax)=akk+t
lk=MAX(lk,v(dgnorm)/rad-w(emax))

!     ***  alphak = current value of alpha (see alg. notes above).  we
!     ***  use more*s scheme for initializing it.
alphak=ABS(v(stppar))*v(rad0)/rad

IF (irc /= 0) GO TO 200

!  ***  compute l0 for positive definite h  ***

CALL livmul (p, w, l, w(q:))
t=v2norm(p,w)
w(phipin)=dst/t/t
lk=MAX(lk,phi*w(phipin))

!  ***  safeguard alphak and add alphak*i to (d**-1)*h*(d**-1)  ***

200 ka=ka+1
IF (-v(dst0) >= alphak.OR.alphak < lk.OR.alphak >= uk) alphak=uk*  &
MAX(p001,SQRT(lk/uk))
k=0
DO i=1,p
  k=k+i
  j=diag0+i
  dihdi(k)=w(j)+alphak
END DO

!  ***  try computing cholesky decomposition  ***

CALL lsqrt (1, p, l, dihdi, irc)
IF (irc == 0) GO TO 230

!  ***  (d**-1)*h*(d**-1) + alphak*i  is indefinite -- overestimate
!  ***  smallest eigenvalue for use in updating lk  ***

j=(irc*(irc+1))/2
t=l(j)
l(j)=one
DO i=1,irc
  w(i)=zero
END DO
w(irc)=one
CALL litvmu (irc, w, l, w)
t1=v2norm(irc,w)
lk=alphak-t/t1/t1
v(dst0)=-lk
GO TO 200

!  ***  alphak makes (d**-1)*h*(d**-1) positive definite.
!  ***  compute q = -d*step, check for convergence.  ***

230 CALL livmul (p, w(q:), l, dig)
CALL litvmu (p, w(q:), l, w(q:))
dst=v2norm(p,w(q:))
phi=dst - rad
IF (phi <= phimax.AND.phi >= phimin) GO TO 270
IF (phi == oldphi) GO TO 270
oldphi=phi
IF (phi > zero) GO TO 240
!        ***  check for the special case of  h + alpha*d**2  (nearly)
!        ***  singular.  delta is .ge. the smallest eigenvalue of
!        ***  (d**-1)*h*(d**-1) + alphak*i.
IF (v(dst0) > zero) GO TO 240
delta=alphak+v(dst0)
twopsi=alphak*dst*dst + dotprd(p,dig,w(q:))
IF (delta < psifac*twopsi) GO TO 250

!  ***  unacceptable alphak -- update lk, uk, alphak  ***

240 IF (ka >= kalim) GO TO 270
CALL livmul (p, w, l, w(q:))
t1=v2norm(p,w)
!     ***  the following MIN is necessary because of restarts  ***
IF (phi < zero) uk=MIN(uk,alphak)
alphak=alphak + (phi/t1)*(dst/t1)*(dst/rad)
lk=MAX(lk,alphak)
GO TO 200

!  ***  decide how to handle (nearly) singular h + alpha*d**2  ***

!     ***  if not yet available, obtain machine dependent value dgxfac.
250 IF (dgxfac == zero) dgxfac=epsfac*rmdcon(3)

!     ***  now decide.  ***
IF (delta > dgxfac*w(dggdmx)) GO TO 330
!        ***  delta is so small we cannot handle the special case in
!        ***  the available arithmetic.  accept step as it is.
GO TO 270

!  ***  acceptable step on first try  ***

260 alphak=zero

!  ***  successful step in general.  compute step = -(d**-1)*q  ***

270 step(1:p) = - w(q0+1:q0+p) / d(1:p)
v(gtstep)=-dotprd(p,dig,w(q:))
v(preduc)=half*(ABS(alphak)*dst*dst - v(gtstep))
GO TO 410

!  ***  restart with new radius  ***

290 IF (v(dst0) <= zero.OR.v(dst0)-rad > phimax) GO TO 310

!     ***  prepare to return newton step  ***

restrt=.true.
ka=ka+1
k=0
DO i=1,p
  k=k+i
  j=diag0+i
  dihdi(k)=w(j)
END DO
uk=negone
GO TO 30

310 IF (ka == 0) GO TO 50

dst=w(dstsav)
alphak=ABS(v(stppar))
phi=dst-rad
t=v(dgnorm)/rad
IF (rad > v(rad0)) GO TO 320

!        ***  smaller radius  ***
uk=t-w(emin)
lk=zero
IF (alphak > zero) lk=w(lk0)
lk=MAX(lk,t-w(emax))
IF (v(dst0) > zero) lk=MAX(lk,(v(dst0)-rad)*w(phipin))
GO TO 240

!     ***  bigger radius  ***
320 uk=t-w(emin)
IF (alphak > zero) uk=MIN(uk,w(uk0))
lk=MAX(zero,-v(dst0),t-w(emax))
IF (v(dst0) > zero) lk=MAX(lk,(v(dst0)-rad)*w(phipin))
GO TO 240

!  ***  handle (nearly) singular h + alpha*d**2  ***

!     ***  negate alphak to indicate special case  ***
330 alphak=-alphak
!     ***  allocate storage for scratch vector x  ***
x0=q0+p
x=x0+1

!  ***  use inverse power method with start from lsvmin to obtain
!  ***  approximate eigenvector corresponding to smallest eigenvalue
!  ***  of (d**-1)*h*(d**-1).

delta=kappa*delta
CALL lsvmin(p, l, w(x:), w, t)

k=0
!     ***  normalize w  ***
340 w(1:p)=t*w(1:p)
!     ***  complete current inv. power iter. -- replace w by (l**-t)*w.
CALL litvmu (p, w, l, w)
t1=one/v2norm(p,w)
t=t1*t
IF (t <= delta) GO TO 370
IF (k > 30) GO TO 270
k=k+1

!     ***  start next inv. power iter. by storing normalized w in x.
w(x0+1:x0+p) = t1 * w(1:p)
!     ***  compute w = (l**-1)*x.
CALL livmul (p, w, l, w(x:))
t=one/v2norm(p,w)
GO TO 340

370 w(1:p)=t1*w(1:p)

!  ***  now w is the desired approximate (unit) eigenvector and
!  ***  t*x = ((d**-1)*h*(d**-1) + alphak*i)*w.

sw=dotprd(p,w(q:),w)
t1=(rad+dst)*(rad-dst)
root=SQRT(sw*sw+t1)
IF (sw < zero) root=-root
si=t1/(sw+root)
!     ***  accept current step if adding si*w would lead to a
!     ***  further relative reduction in psi of less than v(epslon)/3.
v(preduc)=half*twopsi
t1=zero
t=si*(alphak*sw - half*si*(alphak + t*dotprd(p,w(x:),w)))
IF (t < epso6*twopsi) GO TO 390
v(preduc)=v(preduc)+t
dst=rad
t1=-si
390 DO i=1,p
  j=q0+i
  w(j)=t1*w(i) - w(j)
  step(i)=w(j)/d(i)
END DO
v(gtstep)=dotprd(p,dig,w(q:))

!  ***  save values for use in a possible restart  ***

410 v(dstnrm)=dst
v(stppar)=alphak
w(lk0)=lk
w(uk0)=uk
v(rad0)=rad
w(dstsav)=dst

!     ***  restore diagonal of dihdi  ***

j=0
DO i=1,p
  j=j+i
  k=diag0+i
  dihdi(j)=w(k)
END DO
GO TO 450

!  ***  special case -- g = 0 ***

430 v(stppar)=zero
v(preduc)=zero
v(dstnrm)=zero
v(gtstep)=zero
step(1:p)=zero

450 RETURN

!  ***  last card of gqtstp follows  ***
END SUBROUTINE gqtstp


SUBROUTINE itsmry (d, iv, p, v, x)

!  ***  print nl2sol (version 2.2) iteration summary  ***

!  ***  parameter declarations  ***

INTEGER, INTENT(IN)     :: p
INTEGER, INTENT(IN OUT) :: iv(:)
REAL (dp), INTENT(IN)   :: d(:), v(:), x(:)

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!  ***  local variables  ***

INTEGER           :: cov1, g1, i, ii, iv1, i1, m, nf, ng, ol, pu
CHARACTER (LEN=4) :: model1(6) = (/ '    ', '    ', '    ', '    ', '  g ',  &
                                    '  s ' /), &
                     model2(6) = (/ ' g  ', ' s  ', 'g-s ', 's-g ', '-s-g',  &
                                    '-g-s' /)
REAL (dp)  :: nreldf, oldf, preldf, reldf

!  ***  no external functions or subroutines  ***

!  ***  iv subscript values  ***

INTEGER, PARAMETER :: covmat=26, covprt=14, g=28, covreq=15, needhd=39,  &
                       nfcall=6, nfcov=40, ngcov=41, ngcall=30, niter=31, &
                       outlev=19, prntit=48, prunit=21, solprt=22,     &
                       statpr=23, sused=57, x0prt=24

!  ***  v subscript values  ***

INTEGER, PARAMETER :: dstnrm=2, f=10, f0=13, fdif=11, nreduc=6, preduc=7,  &
                      reldx=17, size=47, stppar=5

REAL (dp), PARAMETER :: zero=0.d+0

!-----------------------------------------------------------------------

!-righe aggiunte da Corrado

      !call printlsq2(iv(niter),p,x)

!-fine righe aggiunte da Corrado

pu=iv(prunit)
IF (pu == 0) GO TO 610
iv1=iv(1)
ol=iv(outlev)
IF (iv1 < 2.OR.iv1 > 15) GO TO 320
IF (ol == 0) GO TO 70
IF (iv1 >= 12) GO TO 70
IF (iv1 >= 10.AND.iv(prntit) == 0) GO TO 70
IF (iv1 > 2) GO TO 10
iv(prntit)=iv(prntit)+1
IF (iv(prntit) < ABS(ol)) GO TO 610
10 nf=iv(nfcall)-ABS(iv(nfcov))
iv(prntit)=0
reldf=zero
preldf=zero
oldf=v(f0)
IF (oldf <= zero) GO TO 20
reldf=v(fdif)/oldf
preldf=v(preduc)/oldf
20 IF (ol > 0) GO TO 40

!        ***  print short summary line  ***

IF (iv(needhd) == 1) WRITE (pu,30)
30 FORMAT ('0  it    nf', '      f', '        reldf', '      preldf',  &
           '     reldx')
iv(needhd)=0
WRITE (pu,60) iv(niter),nf,v(f),reldf,preldf,v(reldx)
GO TO 70

!     ***  print long summary line  ***

40 IF (iv(needhd) == 1) WRITE (pu,50)
50 FORMAT ('0  it    nf', '      f', '        reldf', '      preldf',  &
           '     reldx', '    model    stppar', '      size',  &
           '      d*step', '     npreldf')
iv(needhd)=0
m=iv(sused)
nreldf=zero
IF (oldf > zero) nreldf=v(nreduc)/oldf
WRITE (pu,60) iv(niter),nf,v(f),reldf,preldf,v(reldx),model1(m),  &
              model2(m),v(stppar),v(size),v(dstnrm),nreldf
60 FORMAT (' ', i5, i6, 4F11.3, a3, a4, 4F11.3)

! 70 GO TO (610,610,80,100,120,140,160,180,200,220,240,340,260,280,300),iv1
70 SELECT CASE (iv1)
  CASE (1:2)
    GO TO 610
  CASE (3)
    WRITE (pu,90)
    90 FORMAT ('0***** x-convergence *****')
    GO TO 370
  CASE (4)
    WRITE (pu,110)
    110 FORMAT ('0***** relative function convergence *****')
    GO TO 370
  CASE (5)
    WRITE (pu,130)
    130 FORMAT ('0***** x- and relative function convergence *****')
    GO TO 370
  CASE (6)
    WRITE (pu,150)
    150 FORMAT ('0***** absolute function convergence *****')
    GO TO 370
  CASE (7)
    WRITE (pu,170)
    170 FORMAT ('0***** singular convergence *****')
    GO TO 370
  CASE (8)
    WRITE (pu,190)
    190 FORMAT ('0***** false convergence *****')
    GO TO 370
  CASE (9)
    WRITE (pu,210)
    210 FORMAT ('0***** function evaluation limit *****')
    GO TO 370
  CASE (10)
    WRITE (pu,230)
    230 FORMAT ('0***** iteration limit *****')
    GO TO 370
  CASE (11)
    WRITE (pu,250)
    250 FORMAT ('0***** stopx *****')
    GO TO 370
  CASE (12)
    GO TO 340
  CASE (13)
    WRITE (pu,270)
    270 FORMAT ('0***** initial sum of squares overflows *****')
    GO TO 340
  CASE (14)
    WRITE (pu,290)
    290 FORMAT ('0***** bad parameters to assess *****')
    GO TO 610
  CASE (15)
    WRITE (pu,310)
    310 FORMAT ('0***** j could not be computed *****')
    IF (iv(niter) > 0) GO TO 420
    GO TO 340
END SELECT

320 WRITE (pu,330) iv1
330 FORMAT ('0***** iv(1) =', i5, ' *****')
GO TO 610

!  ***  initial call on itsmry  ***

340 IF (iv(x0prt) /= 0) WRITE (pu,350) (i,x(i),d(i),i=1,p)
350 FORMAT ('0 i     initial x(i)', '       ', 'd(i)'//  &
            (' ', i5, f17.6, f14.3))
IF (iv1 >= 13) GO TO 610
iv(needhd)=0
iv(prntit)=0
IF (ol == 0) GO TO 610
IF (ol < 0) WRITE (pu,30)
IF (ol > 0) WRITE (pu,50)
WRITE (pu,360) v(f)
360 FORMAT ('0 0  1', f11.3, '           ', f11.3)
GO TO 610

!  ***  print various information requested on solution  ***

370 iv(needhd)=1
IF (iv(statpr) == 0) GO TO 420
oldf=v(f0)
preldf=zero
nreldf=zero
IF (oldf <= zero) GO TO 380
preldf=v(preduc)/oldf
nreldf=v(nreduc)/oldf
380 nf=iv(nfcall)-iv(nfcov)
ng=iv(ngcall)-iv(ngcov)
WRITE (pu,390) v(f),v(reldx),nf,ng,preldf,nreldf
390 FORMAT ('0function', f17.6, '   reldx', f20.6/ ' func. evals', i8,  &
            '         grad. evals', I8/ ' preldf', F19.6, '   ',  &
            'npreldf', F18.6)

IF (iv(nfcov) > 0) WRITE (pu,400) iv(nfcov)
400 FORMAT ('0', i4, ' extra func. evals for covariance.')
IF (iv(ngcov) > 0) WRITE (pu,410) iv(ngcov)
410 FORMAT (' ', i4, ' extra grad. evals for covariance.')

420 IF (iv(solprt) == 0) GO TO 460
iv(needhd)=1
g1=iv(g)
WRITE (pu,430)
430 FORMAT ('0 i      final x(i)', '        ', 'd(i)', '          ', 'g(i)'/)
DO i=1,p
  WRITE (pu,450) i,x(i),d(i),v(g1)
  g1=g1+1
END DO
450 FORMAT (' ', i5, f17.6, 2F14.3)

460 IF (iv(covprt) == 0) GO TO 610
cov1=iv(covmat)
iv(needhd)=1
IF (cov1 < 0.0) THEN
  GO TO 470
ELSE IF (cov1 == 0.0) THEN
  GO TO 500
ELSE
  GO TO 520
END IF
470 IF (-1 == cov1) WRITE (pu,480)
480 FORMAT ('0++++++ indefinite covariance matrix ++++++')
IF (-2 == cov1) WRITE (pu,490)
490 FORMAT ('0++++++ oversize steps in computing covariance +++++')
GO TO 610

500 WRITE (pu,510)
510 FORMAT ('0++++++ covariance matrix not computed ++++++')
GO TO 610

520 i=ABS(iv(covreq))
IF (i <= 1) WRITE (pu,530)
530 FORMAT ('0covariance = scale * h**-1 * (j**t * j) * h**-1'/)
IF (i == 2) WRITE (pu,540)
540 FORMAT ('0covariance = scale * h**-1'/)
IF (i >= 3) WRITE (pu,550)
550 FORMAT ('0covariance = scale * (j**t * j)**-1'/)
ii=cov1-1
IF (ol <= 0) GO TO 580
DO i=1,p
  i1=ii+1
  ii=ii+i
  WRITE (pu,570) i, v(i1:ii)
END DO
570 FORMAT (' row', i3, '  ', 9F12.4/ ('         ', 9F12.4))
GO TO 610

580 DO i=1,p
  i1=ii+1
  ii=ii+i
  WRITE (pu,600) i, v(i1:ii)
END DO
600 FORMAT (' row', i3, '  ', 5F12.4/ ('         ', 5F12.4))

610 RETURN
!  ***  last card of itsmry follows  ***
END SUBROUTINE itsmry


SUBROUTINE linvrt (n, lin, l)

!  ***  compute  lin = l**-1,  both  n x n  lower triang. stored   ***
!  ***  compactly by rows.  lin and l may share the same storage.  ***

!  ***  parameters  ***

INTEGER, INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: l(:)
REAL (dp), INTENT(OUT) :: lin(:)
!     dimension l(n*(n+1)/2), lin(n*(n+1)/2)

!  ***  local variables  ***

INTEGER   :: i,ii,im1,jj,j0,j1,k,k0,np1
REAL (dp) :: t
REAL (dp), PARAMETER :: one=1.d+0, zero=0.d+0

!  ***  body  ***

np1=n+1
j0=n*(np1)/2
DO ii=1,n
  i=np1-ii
  lin(j0)=one/l(j0)
  IF (i <= 1) GO TO 40
  j1=j0
  im1=i-1
  DO jj=1,im1
    t=zero
    j0=j1
    k0=j1-jj
    DO k=1,jj
      t=t-l(k0)*lin(j0)
      j0=j0-1
      k0=k0+k-i
    END DO
    lin(j0)=t/l(k0)
  END DO
  j0=j0-1
END DO
40 RETURN
!  ***  last card of linvrt follows  ***
END SUBROUTINE linvrt


SUBROUTINE litvmu (n, x, l, y)

!  ***  solve  (l**t)*x = y,  where  l  is an  n x n  lower triangular
!  ***  matrix stored compactly by rows.  x and y may occupy the same
!  ***  storage.  ***

INTEGER, INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: l(:), y(:)
REAL (dp), INTENT(OUT) :: x(:)

! Local variables
INTEGER              :: i, ii, ij, im1, i0, j, np1
REAL (dp)            :: xi
REAL (dp), PARAMETER :: zero=0.d+0

x(1:n) = y(1:n)
np1=n+1
i0=n*(n+1)/2
DO ii=1,n
  i=np1-ii
  xi=x(i)/l(i0)
  x(i)=xi
  IF (i <= 1) GO TO 40
  i0=i0-i
  IF (xi == zero) CYCLE
  im1=i-1
  DO j=1,im1
    ij=i0+j
    x(j)=x(j)-xi*l(ij)
  END DO
END DO
40 RETURN
!  ***  last card of litvmu follows  ***
END SUBROUTINE litvmu


SUBROUTINE livmul (n, x, l, y)

!  ***  solve  l*x = y, where  l  is an  n x n  lower triangular
!  ***  matrix stored compactly by rows.  x and y may occupy the same
!  ***  storage.  ***

INTEGER, INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: l(:), y(:)
REAL (dp), INTENT(OUT) :: x(:)

! Local variables
INTEGER   :: i,j,k
REAL (dp) :: t
REAL (dp), PARAMETER :: zero=0.d+0
!/

DO k=1,n
  IF (y(k) /= zero) GO TO 20
  x(k)=zero
END DO
GO TO 40
20 j=k*(k+1)/2
x(k)=y(k)/l(j)
IF (k >= n) GO TO 40
k=k+1
DO i=k,n
  t=dotprd(i-1,l(j+1:),x)
  j=j+i
  x(i)=(y(i)-t)/l(j)
END DO
40 RETURN
!  ***  last card of livmul follows  ***
END SUBROUTINE livmul


SUBROUTINE lmstep (d, g, ierr, ipivot, ka, p, qtr, r, step, v, w)

!  ***  compute levenberg-marquardt step using more-hebden technique  **
!  ***  nl2sol version 2.2.  ***

!  ***  parameter declarations  ***

INTEGER, INTENT(IN)       :: p
INTEGER, INTENT(IN OUT)   :: ierr, ipivot(:), ka
REAL (dp), INTENT(IN)     :: d(:), g(:), qtr(:), r(:)
REAL (dp), INTENT(IN OUT) :: v(:), w(:)
REAL (dp), INTENT(OUT)    :: step(:)
!     dimension w(p*(p+5)/2 + 4)

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!  ***  purpose  ***

!        given the r matrix from the qr decomposition of a jacobian
!     matrix, j, as well as q-transpose times the corresponding
!     residual vector, resid, this subroutine computes a levenberg-
!     marquardt step of approximate length v(radius) by the more-
!     technique.

!  ***  parameter description  ***

!      d (in)  = the scale vector.
!      g (in)  = the gradient vector (j**t)*r.
!   ierr (i/o) = return code from qrfact or qrfgs -- 0 means r has
!             full rank.
! ipivot (i/o) = permutation array from qrfact or qrfgs, which compute
!             qr decompositions with column pivoting.
!     ka (i/o).  ka .lt. 0 on input means this is the first call on
!             lmstep for the current r and qtr.  on output ka con-
!             tains the number of hebden iterations needed to determine
!             step.  ka = 0 means a gauss-newton step.
!      p (in)  = number of parameters.
!    qtr (in)  = (q**t)*resid = q-transpose times the residual vector.
!      r (in)  = the r matrix, stored compactly by columns.
!   step (out) = the levenberg-marquardt step computed.
!      v (i/o) contains various constants and variables described below.
!      w (i/o) = workspace of length p*(p+5)/2 + 4.

!  ***  entries in v  ***

! v(dgnorm) (i/o) = 2-norm of (d**-1)*g.
! v(dstnrm) (i/o) = 2-norm of d*step.
! v(dst0)   (i/o) = 2-norm of gauss-newton step (for nonsing. j).
! v(epslon) (in) = max. rel. error allowed in twonorm(r)**2 minus
!             twonorm(r - j*step)**2.  (see algorithm notes below.)
! v(gtstep) (out) = inner product between g and step.
! v(nreduc) (out) = half the reduction in the sum of squares predicted
!             for a gauss-newton step.
! v(phmnfc) (in)  = tol. (together with v(phmxfc)) for accepting step
!             (more*s sigma).  the error v(dstnrm) - v(radius) must lie
!             between v(phmnfc)*v(radius) and v(phmxfc)*v(radius).
! v(phmxfc) (in)  (see v(phmnfc).)
! v(preduc) (out) = half the reduction in the sum of squares predicted
!             by the step returned.
! v(radius) (in)  = radius of current (scaled) trust region.
! v(rad0)   (i/o) = value of v(radius) from previous call.
! v(stppar) (i/o) = marquardt parameter (or its negative if the special
!             case mentioned below in the algorithm notes occurs).

! note -- see data statement below for values of above subscripts.

!  ***  usage notes  ***

!     if it is desired to recompute step using a different value of
!     v(radius), then this routine may be restarted by calling it
!     with all parameters unchanged except v(radius).  (this explains
!     why many parameters are listed as i/o).  on an intiial call (one
!     with ka = -1), the caller need only have initialized d, g, ka, p,
!     qtr, r, v(epslon), v(phmnfc), v(phmxfc), v(radius), and v(rad0).

!  ***  application and usage restrictions  ***

!     this routine is called as part of the nl2sol (nonlinear least-
!     squares) package (ref. 1).

!  ***  algorithm notes  ***

!     this code implements the step computation scheme described in
!     refs. 2 and 4.  fast givens transformations (see ref. 3, pp. 60-
!     62) are used to compute step with a nonzero marquardt parameter.
!        a special case occurs if j is (nearly) singular and v(radius)
!     is sufficiently large.  in this case the step returned is such
!     that  twonorm(r)**2 - twonorm(r - j*step)**2  differs from its
!     optimal value by less than v(epslon) times this optimal value,
!     where j and r denote the original jacobian and residual.  (see
!     ref. 2 for more details.)

!  ***  functions and subroutines called  ***

! dotprd - returns inner product of two vectors.
! litvmu - apply inverse-transpose of compact lower triang. matrix.
! livmul - apply inverse of compact lower triang. matrix.
! vcopy  - copies one vector to another.
! v2norm - returns 2-norm of a vector.

!  ***  references  ***

! 1.  dennis, j.e., gay, d.m., and welsch, r.e. (1981), an adaptive
!             nonlinear least-squares algorithm, acm trans. math.
!             software, vol. 7, no. 3.
! 2.  gay, d.m. (1981), computing optimal locally constrained steps,
!             siam j. sci. statist. computing, vol. 2, no. 2, pp.
!             186-197.
! 3.  lawson, c.l., and hanson, r.j. (1974), solving least squares
!             problems, prentice-hall, englewood cliffs, n.j.
! 4.  more, j.j. (1978), the levenberg-marquardt algorithm, implemen-
!             tation and theory, pp.105-116 of springer lecture notes
!             in mathematics no. 630, edited by g.a. watson, springer-
!             verlag, berlin and new york.

!  ***  general  ***

!     coded by david m. gay.
!     this subroutine was written in connection with research
!     supported by the national science foundation under grants
!     mcs-7600324, dcr75-10143, 76-14311dss, mcs76-11989, and
!     mcs-7906671.

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!  ***  local variables  ***

INTEGER   :: dstsav,i,ip1,i1,j1,k,kalim,l,lk0,phipin,pp1o2,res,res0,  &
             rmat,rmat0,uk0
REAL (dp) :: a,adi,alphak,b,dfacsq,dst,dtol,d1,d2,lk,oldphi,  &
                    phi,phimax,phimin,psifac,rad,si,sj,sqrtak,t,twopsi,uk,wl

!  ***  subscripts for v  ***

INTEGER, PARAMETER :: dgnorm=1, dstnrm=2, dst0=3, epslon=19, gtstep=4,  &
                      nreduc=6, phmnfc=20, phmxfc=21, preduc=7, radius=8, &
                      rad0=9, stppar=5

REAL (dp), PARAMETER :: dfac=256.d+0, eight=8.d+0, half=0.5d+0,  &
                               negone=-1.d+0, one=1.d+0, p001=1.d-3,   &
                               three=3.d+0, ttol=2.5d+0, zero=0.d+0

!  ***  body  ***

!     ***  for use in recomputing step, the final values of lk and uk,
!     ***  the inverse derivative of more*s phi at 0 (for nonsing. j)
!     ***  and the value returned as v(dstnrm) are stored at w(lk0),
!     ***  w(uk0), w(phipin), and w(dstsav) respectively.
lk0=p+1
phipin=lk0+1
uk0=phipin+1
dstsav=uk0+1
rmat0=dstsav
!     ***  a copy of the r-matrix from the qr decomposition of j is
!     ***  stored in w starting at w(rmat), and a copy of the residual
!     ***  vector is stored in w starting at w(res).  the loops below
!     ***  that update the qr decomp. for a nonzero marquardt parameter
!     ***  work on these copies.
rmat=rmat0+1
pp1o2=p*(p+1)/2
res0=pp1o2+rmat0
res=res0+1
rad=v(radius)
IF (rad > zero) psifac=v(epslon)/((eight*(v(phmnfc)+one)+three)* rad**2)
phimax=v(phmxfc)*rad
phimin=v(phmnfc)*rad
!     ***  dtol, dfac, and dfacsq are used in rescaling the fast givens
!     ***  representation of the updated qr decomposition.
dtol=one/dfac
dfacsq=dfac*dfac
!     ***  oldphi is used to detect limits of numerical accuracy.  if
!     ***  we recompute step and it does not change, then we accept it.
oldphi=zero
lk=zero
uk=zero
kalim=ka+12

!  ***  start or restart, depending on ka  ***

IF (ka < 0) THEN
  GO TO  10
ELSE IF (ka == 0) THEN
  GO TO  20
ELSE
  GO TO 310
END IF

!  ***  fresh start -- compute v(nreduc)  ***

10 ka=0
kalim=12
k=p
IF (ierr /= 0) k=ABS(ierr)-1
v(nreduc)=half*dotprd(k,qtr,qtr)

!  ***  set up to try initial gauss-newton step  ***

20 v(dst0)=negone
IF (ierr /= 0) GO TO 50

!  ***  compute gauss-newton step  ***

!     ***  note -- the r-matrix is stored compactly by columns in
!     ***  r(1), r(2), r(3), ...  it is the transpose of a
!     ***  lower triangular matrix stored compactly by rows, and we
!     ***  treat it as such when using litvmu and livmul.
CALL litvmu (p, w, r, qtr)
!     ***  temporarily store permuted -d*step in step.
DO i=1,p
  j1=ipivot(i)
  step(i)=d(j1)*w(i)
END DO
dst=v2norm(p,step)
v(dst0)=dst
phi=dst-rad
IF (phi <= phimax) GO TO 350
!     ***  if this is a restart, go to 110 ***
IF (ka > 0) GO TO 70

!  ***  gauss-newton step was unacceptable.  compute l0 ***

DO i=1,p
  j1=ipivot(i)
  step(i)=d(j1)*(step(i)/dst)
END DO
CALL livmul (p, step, r, step)
t=one/v2norm(p,step)
w(phipin)=(t/dst)*t
lk=phi*w(phipin)

!  ***  compute u0 ***

50 w(1:p)=g(1:p)/d(1:p)
v(dgnorm)=v2norm(p,w)
uk=v(dgnorm)/rad
IF (uk <= zero) GO TO 330

!     ***  alphak will be used as the current marquardt parameter.  we
!     ***  use more*s scheme for initializing it.
alphak=ABS(v(stppar))*v(rad0)/rad


!  ***  top of loop -- increment ka, copy r to rmat, qtr to res  ***

70 ka=ka+1
CALL vcopy (pp1o2, w(rmat:), r)
CALL vcopy (p, w(res:), qtr)

!  ***  safeguard alphak and initialize fast givens scale vector.  ***

IF (alphak <= zero.OR.alphak < lk.OR.alphak >= uk) alphak=uk*  &
                                                   MAX(p001,SQRT(lk/uk))
sqrtak=SQRT(alphak)
w(1:p)=one

!  ***  add alphak*d and update qr decomp. using fast givens trans.  ***

DO i=1,p
!        ***  generate, apply 1st givens trans. for row i of alphak*d.
!        ***  (use step to store temporary row)  ***
  l=i*(i+1)/2+rmat0
  wl=w(l)
  d2=one
  d1=w(i)
  j1=ipivot(i)
  adi=sqrtak*d(j1)
  IF (adi >= ABS(wl)) GO TO 110
  90 a=adi/wl
  b=d2*a/d1
  t=a*b+one
  IF (t > ttol) GO TO 110
  w(i)=d1/t
  d2=d2/t
  w(l)=t*wl
  a=-a
  DO j1=i,p
    l=l+j1
    step(j1)=a*w(l)
  END DO
  GO TO 130
  
  110 b=wl/adi
  a=d1*b/d2
  t=a*b+one
  IF (t > ttol) GO TO 90
  w(i)=d2/t
  d2=d1/t
  w(l)=t*adi
  DO j1=i,p
    l=l+j1
    wl=w(l)
    step(j1)=-wl
    w(l)=a*wl
  END DO
  
  130 IF (i == p) GO TO 240
  
!        ***  now use givens trans. to zero elements of temp. row  ***
  
  ip1=i+1
  DO i1=ip1,p
    l=i1*(i1+1)/2+rmat0
    wl=w(l)
    si=step(i1-1)
    d1=w(i1)
    
!             ***  rescale row i1 if necessary  ***
    
    IF (d1 >= dtol) GO TO 150
    d1=d1*dfacsq
    wl=wl/dfac
    k=l
    DO j1=i1,p
      k=k+j1
      w(k)=w(k)/dfac
    END DO
    
!             ***  use givens trans. to zero next element of temp. row
    
    150 IF (ABS(si) > ABS(wl)) GO TO 180
    IF (si == zero) CYCLE
    160 a=si/wl
    b=d2*a/d1
    t=a*b+one
    IF (t > ttol) GO TO 180
    w(l)=t*wl
    w(i1)=d1/t
    d2=d2/t
    DO j1=i1,p
      l=l+j1
      wl=w(l)
      sj=step(j1)
      w(l)=wl+b*sj
      step(j1)=sj-a*wl
    END DO
    GO TO 200
    
    180 b=wl/si
    a=d1*b/d2
    t=a*b+one
    IF (t > ttol) GO TO 160
    w(i1)=d2/t
    d2=d1/t
    w(l)=t*si
    DO j1=i1,p
      l=l+j1
      wl=w(l)
      sj=step(j1)
      w(l)=a*wl+sj
      step(j1)=b*sj-wl
    END DO
    
!             ***  rescale temp. row if necessary  ***
    
    200 IF (d2 >= dtol) CYCLE
    d2=d2*dfacsq
    step(i1:p)=step(i1:p)/dfac
  END DO
END DO

!  ***  compute step  ***

240 CALL litvmu (p, w(res:), w(rmat:), w(res:))
!     ***  recover step and store permuted -d*step at w(res)  ***
DO i=1,p
  j1=ipivot(i)
  k=res0+i
  t=w(k)
  step(j1)=-t
  w(k)=t*d(j1)
END DO
dst=v2norm(p,w(res:))
phi=dst-rad
IF (phi <= phimax.AND.phi >= phimin) GO TO 370
IF (oldphi == phi) GO TO 370
oldphi=phi

!  ***  check for (and handle) special case  ***

IF (phi > zero) GO TO 270
IF (ka >= kalim) GO TO 370
twopsi=alphak*dst*dst-dotprd(p,step,g)
IF (alphak >= twopsi*psifac) GO TO 270
v(stppar)=-alphak
GO TO 380

!  ***  unacceptable step -- update lk, uk, alphak, and try again  ***

260 IF (phi < zero) uk=MIN(uk,alphak)
GO TO 280
270 IF (phi < zero) uk=alphak
280 DO i=1,p
  j1=ipivot(i)
  k=res0+i
  step(i)=d(j1)*(w(k)/dst)
END DO
CALL livmul (p, step, w(rmat:), step)
step(1:p)=step(1:p)/SQRT(w(1:p))
t=one/v2norm(p,step)
alphak=alphak+t*phi*t/rad
lk=MAX(lk,alphak)
GO TO 70

!  ***  restart  ***

310 lk=w(lk0)
uk=w(uk0)
IF (v(dst0) > zero.AND.v(dst0)-rad <= phimax) GO TO 20
alphak=ABS(v(stppar))
dst=w(dstsav)
phi=dst-rad
t=v(dgnorm)/rad
IF (rad > v(rad0)) GO TO 320

!        ***  smaller radius  ***
uk=t
IF (alphak <= zero) lk=zero
IF (v(dst0) > zero) lk=MAX(lk,(v(dst0)-rad)*w(phipin))
GO TO 260

!     ***  bigger radius  ***
320 IF (alphak <= zero.OR.uk > t) uk=t
lk=zero
IF (v(dst0) > zero) lk=MAX(lk,(v(dst0)-rad)*w(phipin))
GO TO 260

!  ***  special case -- rad .le. 0 or (g = 0 and j is singular)  ***

330 v(stppar)=zero
dst=zero
lk=zero
uk=zero
v(gtstep)=zero
v(preduc)=zero
step(1:p)=zero
GO TO 390

!  ***  acceptable gauss-newton step -- recover step from w  ***

350 alphak=zero
DO i=1,p
  j1=ipivot(i)
  step(j1)=-w(i)
END DO

!  ***  save values for use in a possible restart  ***

370 v(stppar)=alphak
380 v(gtstep)=dotprd(p,step,g)
v(preduc)=half*(alphak*dst*dst-v(gtstep))
390 v(dstnrm)=dst
w(dstsav)=dst
w(lk0)=lk
w(uk0)=uk
v(rad0)=rad

RETURN

!  ***  last card of lmstep follows  ***
END SUBROUTINE lmstep


SUBROUTINE lsqrt (n1, n, l, a, irc)

!  ***  compute rows n1 through n of the cholesky factor  l  of
!  ***  a = l*(l**t),  where  l  and the lower triangle of  a  are both
!  ***  stored compactly by rows (and may occupy the same storage).
!  ***  irc = 0 means all went well.  irc = j means the leading
!  ***  principal  j x j  submatrix of  a  is not positive definite --
!  ***  and  l(j*(j+1)/2)  contains the (nonpos.) reduced j-th diagonal.

!  ***  parameters  ***

INTEGER, INTENT(IN)    :: n1, n
INTEGER, INTENT(OUT)   :: irc
REAL (dp), INTENT(IN)  :: a(:)
REAL (dp), INTENT(OUT) :: l(:)
!     dimension l(n*(n+1)/2), a(n*(n+1)/2)

!  ***  local variables  ***

INTEGER   :: i, ij, ik, im1, i0, j, jk, jm1, j0, k
REAL (dp) :: t, td, zero=0.d+0

!  ***  body  ***

i0=n1*(n1-1)/2
DO i=n1,n
  td=zero
  IF (i == 1) GO TO 40
  j0=0
  im1=i-1
  DO j=1,im1
    t=zero
    IF (j == 1) GO TO 20
    jm1=j-1
    DO k=1,jm1
      ik=i0+k
      jk=j0+k
      t=t+l(ik)*l(jk)
    END DO
    20 ij=i0+j
    j0=j0+j
    t=(a(ij)-t)/l(j0)
    l(ij)=t
    td=td+t*t
  END DO
  40 i0=i0+i
  t=a(i0)-td
  IF (t <= zero) GO TO 60
  l(i0)=SQRT(t)
END DO

irc=0
GO TO 70

60 l(i0)=t
irc=i

70 RETURN

!  ***  last card of lsqrt  ***
END SUBROUTINE lsqrt


SUBROUTINE lsvmin (p, l, x, y, fn_val)

!  ***  estimate smallest sing. value of packed lower triang. matrix l

!  ***  parameter declarations  ***

INTEGER, INTENT(IN)       :: p
REAL (dp), INTENT(IN)     :: l(:)
REAL (dp), INTENT(IN OUT) :: x(:), y(:)
REAL (dp), INTENT(OUT)    :: fn_val
!     dimension l(p*(p+1)/2)

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!  ***  purpose  ***

!     this function returns a good over-estimate of the smallest
!     singular value of the packed lower triangular matrix l.

!  ***  parameter description  ***

!  p (in)  = the order of l.  l is a  p x p  lower triangular matrix.
!  l (in)  = array holding the elements of  l  in row order, i.e.
!             l(1,1), l(2,1), l(2,2), l(3,1), l(3,2), l(3,3), etc.
!  x (out) if lsvmin returns a positive value, then x is a normalized
!             approximate left singular vector corresponding to the
!             smallest singular value.  this approximation may be very
!             crude.  if lsvmin returns zero, then some components of x
!             are zero and the rest retain their input values.
!  y (out) if lsvmin returns a positive value, then y = (l**-1)*x is an
!             unnormalized approximate right singular vector correspond-
!             ing to the smallest singular value.  this approximation
!             may be crude.  if lsvmin returns zero, then y retains its
!             input value.  the caller may pass the same vector for x
!             and y (nonstandard fortran usage), in which case y over-
!             writes x (for nonzero lsvmin returns).

!  ***  application and usage restrictions  ***

!     there are no usage restrictions.

!  ***  algorithm notes  ***

!     the algorithm is based on (1), with the additional provision that
!     lsvmin = 0 is returned if the smallest diagonal element of l
!     (in magnitude) is not more than the unit roundoff times the
!     largest.  the algorithm uses a random number generator proposed
!     in (4), which passes the spectral test with flying colors -- see
!     (2) and (3).

!  ***  subroutines and functions called  ***

!        v2norm - function, returns the 2-norm of a vector.

!  ***  references  ***

!     (1) cline, a., moler, c., stewart, g., and wilkinson, j.h.(1977),
!         an estimate for the condition number of a matrix, report
!         tm-310, applied math. div., argonne national laboratory.

!     (2) hoaglin, d.c. (1976), theoretical properties of congruential
!         random-number generators --  an empirical view,
!         memorandum ns-340, dept. of statistics, harvard univ.

!     (3) knuth, d.e. (1969), the art of computer programming, vol. 2
!         (seminumerical algorithms), addison-wesley, reading, mass.

!     (4) smith, c.s. (1971), multiplicative pseudo-random number
!         generators with prime modulus, j. assoc. comput. mach. 18,
!         pp. 586-593.

!  ***  history  ***

!     designed and coded by david m. gay (winter 1977/summer 1978).

!  ***  general  ***

!     this subroutine was written in connection with research
!     supported by the national science foundation under grants
!     mcs-7600324, dcr75-10143, 76-14311dss, and mcs76-11989.

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!  ***  local variables  ***

INTEGER   :: i,ii,j,ji,jj,jjj,jm1,j0,pplus1
REAL (dp) :: b,psj,sminus,splus,t,xminus,xplus

REAL (dp), PARAMETER :: half=0.5d+0, one=1.d+0, r9973=9973.d+0,   &
                               zero=0.d+0
!     save ix
INTEGER, SAVE :: ix = 2

!  ***  body  ***

!  ***  first check whether to return lsvmin = 0 and initialize x  ***

ii=0
DO i=1,p
  x(i)=zero
  ii=ii+i
  IF (l(ii) == zero) GO TO 100
END DO
IF (MOD(ix,9973) == 0) ix=2
pplus1=p+1

!  ***  solve (l**t)*x = b, where the components of b have randomly
!  ***  chosen magnitudes in (.5,1) with signs chosen to make x large.

!     do j = p to 1 by -1...
DO jjj=1,p
  j=pplus1-jjj
!       ***  determine x(j) in this iteration. note for i = 1,2,...,j
!       ***  that x(i) holds the current partial sum for row i.
  ix=MOD(3432*ix,9973)
  b=half*(one+REAL(ix)/r9973)
  xplus=(b-x(j))
  xminus=(-b-x(j))
  splus=ABS(xplus)
  sminus=ABS(xminus)
  jm1=j-1
  j0=j*jm1/2
  jj=j0+j
  xplus=xplus/l(jj)
  xminus=xminus/l(jj)
  IF (jm1 == 0) GO TO 30
  DO i=1,jm1
    ji=j0+i
    splus=splus+ABS(x(i)+l(ji)*xplus)
    sminus=sminus+ABS(x(i)+l(ji)*xminus)
  END DO
  30 IF (sminus > splus) xplus=xminus
  x(j)=xplus
!       ***  update partial sums  ***
  IF (jm1 == 0) CYCLE
  DO i=1,jm1
    ji=j0+i
    x(i)=x(i) + l(ji)*xplus
  END DO
END DO

!  ***  normalize x  ***

t=one/v2norm(p,x)
x(1:p)=t*x(1:p)

!  ***  solve l*y = x and return svmin = 1/twonorm(y)  ***

DO j=1,p
  psj=zero
  jm1=j-1
  j0=j*jm1/2
  IF (jm1 == 0) GO TO 80
  DO i=1,jm1
    ji=j0+i
    psj=psj + l(ji)*y(i)
  END DO
  80 jj=j0+j
  y(j)=(x(j)-psj)/l(jj)
END DO

fn_val=one/v2norm(p,y)
GO TO 110

100 fn_val=zero
110 RETURN
!  ***  last card of lsvmin follows  ***
END SUBROUTINE lsvmin


SUBROUTINE ltsqar (n, a, l)

!  ***  set a to lower triangle of (l**t) * l  ***

!  ***  l = n x n lower triang. matrix stored rowwise.  ***
!  ***  a is also stored rowwise and may share storage with l.  ***

INTEGER, INTENT(IN)    :: n
REAL (dp), INTENT(IN)  :: l(:)
REAL (dp), INTENT(OUT) :: a(:)
!     dimension a(n*(n+1)/2), l(n*(n+1)/2)

INTEGER   :: i, ii, iim1, i1, j, k, m
REAL (dp) :: lii, lj

ii=0
DO i=1,n
  i1=ii+1
  ii=ii+i
  m=1
  IF (i == 1) GO TO 30
  iim1=ii-1
  DO j=i1,iim1
    lj=l(j)
    DO k=i1,j
      a(m)=a(m) + lj*l(k)
      m=m+1
    END DO
  END DO
  30 lii=l(ii)
  DO j=i1,ii
    a(j)=lii*l(j)
  END DO
END DO

RETURN
!  ***  last card of ltsqar follows  ***
END SUBROUTINE ltsqar


SUBROUTINE parchk (iv, n, nn, p, v)

!  ***  check nl2sol (version 2.2) parameters, print changed values  ***

INTEGER, INTENT(IN)       :: n, nn, p
INTEGER, INTENT(IN OUT)   :: iv(:)
REAL (dp), INTENT(IN OUT) :: v(:)

! dfault -- supplies dfault parameter values.
! rmdcon -- returns machine-dependent constants.
! vcopy  -- copies one vector to another.

!  ***  local variables  ***

INTEGER           :: i, iv1, jtolp, k, l, m, pu
CHARACTER (LEN=4) :: which(3)
REAL (dp)         :: machep, vk

!  ***  iv and v subscripts  ***

INTEGER, PARAMETER   :: nvdflt=27
REAL (dp), PARAMETER :: zero=0.d+0

INTEGER, PARAMETER :: dtype=16, dtype0=29, d0init=37, epslon=19, inits=25, &
                      jtinit=39, jtol0=86, jtol1=87, oldn=45, oldnn=46,   &
                      oldp=47, parprt=20, parsv1=51, prunit=21
!     save big, tiny

REAL (dp), SAVE :: big=0.d+0, tiny=1.d+0
CHARACTER (LEN=4), PARAMETER :: vn(2,27) = RESHAPE( (/  &
                                           'epsl','on..', 'phmn','fc..',  &
                                           'phmx','fc..', 'decf','ac..',  &
                                           'incf','ac..', 'rdfc','mn..',  &
                                           'rdfc','mx..', 'tune','r1..',  &
                                           'tune','r2..', 'tune','r3..',  &
                                           'tune','r4..', 'tune','r5..',  &
                                           'afct','ol..', 'rfct','ol..',  &
                                           'xcto','l...', 'xfto','l...',  &
                                           'lmax','0...', 'dltf','dj..',  &
                                           'd0in','it..', 'dini','t...',  &
                                           'jtin','it..', 'dltf','dc..',  &
                                           'dfac','....', 'rlim','it..',  &
                                           'cosm','in..', 'delt','a0..',  &
                                           'fuzz','....' /), (/ 2, 27 /) )

REAL (dp) :: vm(27) = (/ 1.0D-3, -0.99D+0, 1.0D-3, 1.0D-2, &
              1.2D+0, 1.d-2, 1.2D+0, 0.d+0, 0.d+0, 1.d-3, -1.d+0, &
              0.d+0, 0.d+0, 0.d+0, 0.d+0, 0.d+0, 0.d+0, 0.d+0,    &
              0.d+0, -10.d+0, 0.d+0, 0.d+0, 0.d+0, 1.d+10, 0.d+0, &
              0.d+0, 1.01D+0 /)

REAL (dp) :: vx(27) = (/ 0.9D+0, -1.d-3, 1.d+1, 0.8D+0,    &
              1.d+2, 0.8D+0, 1.d+2, 0.5D+0, 0.5D+0, 1.d+0, 1.d+0, &
              1.d+0, 1.d+0, 0.1D+0, 1.d+0, 1.d+0, 1.d+0, 1.d+0,   &
              1.d+0, 1.d+0, 1.d+0, 1.d+0, 1.d+0, 1.d+0, 1.d+0,    &
              1.d+0, 1.d+2 /)

CHARACTER (LEN=4), PARAMETER :: cngd(3) = (/ '---c', 'hang', 'ed v' /), &
                                dflt(3) = (/ 'nond', 'efau', 'lt v' /)

!.......................................................................

IF (iv(1) == 0) CALL dfault (iv, v)
pu=iv(prunit)
iv1=iv(1)
IF (iv1 /= 12) GO TO 30
IF (nn >= n.AND.n >= p.AND.p >= 1) GO TO 20
iv(1)=16
IF (pu /= 0) WRITE (pu,10) nn,n,p
10 FORMAT ('0///// bad nn, n, or p... nn =', i5, ', n =', i5, ', p =', i5)
GO TO 300
20 k=iv(21)
CALL dfault (iv(21:), v(33:))
iv(21)=k
iv(dtype0)=iv(dtype+20)
iv(oldn)=n
iv(oldnn)=nn
iv(oldp)=p
which(1)=dflt(1)
which(2)=dflt(2)
which(3)=dflt(3)
GO TO 80
30 IF (n == iv(oldn).AND.nn == iv(oldnn).AND.p == iv(oldp)) GO TO 50
iv(1)=17
IF (pu /= 0) WRITE (pu,40) iv(oldnn),iv(oldn),iv(oldp),nn,n,p
40 FORMAT ('0///// (nn,n,p) changed from (', i5, ',', i5, ',', i3,  &
            ') to (' , i5, ',', i5, ',', i3, ').')
GO TO 300

50 IF (iv1 <= 11.AND.iv1 >= 1) GO TO 70
iv(1)=50
IF (pu /= 0) WRITE (pu,60) iv1
60 FORMAT ('0/////  iv(1) =', i5, ' should be between 0 and 12.')
GO TO 300

70 which(1)=cngd(1)
which(2)=cngd(2)
which(3)=cngd(3)

80 IF (big > tiny) GO TO 90
tiny=rmdcon(1)
machep=rmdcon(3)
big=rmdcon(6)
vm(12)=machep
vx(12)=big
vm(13)=tiny
vx(13)=big
vm(14)=machep
vm(17)=tiny
vx(17)=big
vm(18)=machep
vx(19)=big
vx(20)=big
vx(21)=big
vm(22)=machep
vx(24)=rmdcon(5)
vm(25)=machep
vm(26)=machep
90 m=0
IF (iv(inits) >= 0.AND.iv(inits) <= 2) GO TO 110
m=18
IF (pu /= 0) WRITE (pu,100) iv(inits)
100 FORMAT ('0/////  inits... iv(25) =', i4, ' should be between 0 and 2.')
110 k=epslon
DO i=1,nvdflt
  vk=v(k)
  IF (vk >= vm(i).AND.vk <= vx(i)) GO TO 130
  m=k
  IF (pu /= 0) WRITE (pu,120) vn(1,i),vn(2,i),k,vk,vm(i),vx(i)
  120 FORMAT ('0/////  ', 2A4, '.. v(', i2, ') =', f11.3,  &
              ' should be between', F11.3, ' AND', F11.3)
  130 k=k+1
END DO

IF (iv1 == 12.AND.v(jtinit) > zero) GO TO 170

!  ***  check jtol values  ***

jtolp=jtol0+p
DO i=jtol1,jtolp
  IF (v(i) > zero) CYCLE
  k=i-jtol0
  IF (pu /= 0) WRITE (pu,150) k,i,v(i)
  150 FORMAT ('0///// jtol(', i3, ') = v(', i3, ') =', f11.3,  &
              ' should be positive.')
  m=i
END DO

170 IF (m == 0) GO TO 180
iv(1)=m
GO TO 300

180 IF (pu == 0.OR.iv(parprt) == 0) GO TO 300
IF (iv1 /= 12.OR.iv(inits) == 0) GO TO 200
m=1
WRITE (pu,190) iv(inits)
190 FORMAT ('0nondefault values....'/' inits..... iv(25) =',i3)
200 IF (iv(dtype) == iv(dtype0)) GO TO 220
IF (m == 0) WRITE (pu,230) which
m=1
WRITE (pu,210) iv(dtype)
210 FORMAT (' dtype..... iv(16) =', i3)
220 k=epslon
l=parsv1
DO i=1,nvdflt
  IF (v(k) == v(l)) GO TO 250
  IF (m == 0) WRITE (pu,230) which
  230 FORMAT ('0', 3A4, 'alues....'/)
  m=1
  WRITE (pu,240) vn(1,i),vn(2,i),k,v(k)
  240 FORMAT (' ', 2A4, '.. v(', i2, ') =', f15.7)
  250 k=k+1
  l=l+1
END DO
iv(dtype0)=iv(dtype)
CALL vcopy (nvdflt, v(parsv1:), v(epslon:))
IF (iv1 /= 12) GO TO 300
IF (v(jtinit) > zero) GO TO 280
jtolp=jtol0+p
WRITE (pu,270) v(jtol1:jtolp)
270 FORMAT ('0(initial) jtol array...'/ (' ', 6F12.3))
280 IF (v(d0init) > zero) GO TO 300
k=jtol1+p
l=k+p-1
WRITE (pu,290) v(k:l)
290 FORMAT ('0(initial) d0 array...'/' ', 6F12.3)

300 RETURN
!  ***  last card of parchk follows  ***
END SUBROUTINE parchk


SUBROUTINE qapply (n, p, j, r, ierr)
!     *****parameters.
INTEGER, INTENT(IN)       :: n, p, ierr
REAL (dp), INTENT(IN)     :: j(:,:)
REAL (dp), INTENT(IN OUT) :: r(:)
! dimension j(nn,p), r(n)

!     ..................................................................

!     *****purpose.
!     this subroutine applies to r the orthogonal transformations
!     stored in j by qrfact

!     *****parameter description.
!     on input.

!        nn is the row dimension of the matrix j as declared in
!             the calling program dimension statement

!        n is the number of rows of j and the size of the vector r

!        p is the number of columns of j and the size of sigma

!        j contains on and below its diagonal the column vectors
!             u which determine the householder transformations
!             ident - u*u.transpose

!        r is the right hand side vector to which the orthogonal
!             transformations will be applied

!        ierr if non-zero indicates that not all the transformations
!             were successfully determined and only the first
!             abs(ierr) - 1 transformations will be used

!     on output.

!        r has been overwritten by its transformed image

!     *****application and usage restrictions.
!     none

!     *****algorithm notes.
!     the vectors u which determine the householder transformations
!     are normalized so that their 2-norm squared is 2.  the use of
!     these transformations here is in the spirit of (1).

!     *****subroutines and functions called.

!     dotprd - function, returns the inner product of vectors

!     *****references.
!     (1) businger, p. a., and golub, g. h. (1965), linear least squares
!        solutions by householder transformations, numer. math. 7,
!        pp. 269-276.

!     *****history.
!     designed by david m. gay, coded by stephen c. peters (winter 1977)

!     *****general.

!     this subroutine was written in connection with research
!     supported by the national science foundation under grants
!     mcs-7600324, dcr75-10143, 76-14311dss, and mcs76-11989.

!     ..................................................................

!     *****local variables.
INTEGER   :: k, l, nl1
REAL (dp) :: t

k=p
IF (ierr /= 0) k=ABS(ierr)-1

DO l=1,k
  nl1=n-l+1
  t=-dotprd(nl1,j(l:,l),r(l:))
  r(l:n)=r(l:n) + t*j(l:n,l)
END DO

RETURN
!     .... last card of qapply .........................................
END SUBROUTINE qapply


SUBROUTINE qrfact (m, n, qr, alpha, ipivot, ierr, nopivk, sum)

!  ***  compute the qr decomposition of the matrix stored in qr  ***

!     *****parameters.
INTEGER, INTENT(IN)       :: m, n, nopivk
INTEGER, INTENT(OUT)      :: ierr, ipivot(:)
REAL (dp), INTENT(IN OUT) :: qr(:,:)
REAL (dp), INTENT(OUT)    :: alpha(:), sum(:)
! dimension  ipivot(n), qr(nm,n), alpha(n), sum(n)

!     *****local variables.
INTEGER         :: i, j, jbar, k, k1, minum, mk1
REAL (dp)       :: alphak, beta, qrkk, qrkmax, sigma, temp, rktol1, sumj
REAL (dp), SAVE :: rktol=0._dp, ufeta=0._dp

!     *****functions.
! dotprd... returns inner product of two vectors.
! rmdcon... returns machine-dependent constants.
! vaxpy... computes scalar times one vector plus another.
! vscopy... sets all elements of a vector to a scalar.
! v2norm... returns the 2-norm of a vector.

!     *****constants.
REAL (dp), PARAMETER :: one=1.0d+0, p01=0.01d+0, p99=0.99d+0, zero=0.0d+0

!     ..................................................................

!     *****purpose.

!     this subroutine does a qr-decomposition on the m x n matrix qr,
!        with an optionally modified column pivoting, and returns the
!        upper triangular r-matrix, as well as the orthogonal vectors
!        used in the transformations.

!     *****parameter description.
!     on input.

!        nm must be set to the row dimension of the two dimensional
!             array parameters as declared in the calling program
!             dimension statement.

!        m must be set to the number of rows in the matrix.

!        n must be set to the number of columns in the matrix.

!        qr contains the real rectangular matrix to be decomposed.

!     nopivk is used to control pivotting.  columns 1 through
!        nopivk will remain fixed in position.

!        sum is used for temporary storage for the subroutine.

!     on output.

!        qr contains the non-diagonal elements of the r-matrix
!             in the strict upper triangle. the vectors u, which
!             define the householder transformations   i - u*u-transp,
!             are in the columns of the lower triangle. these vectors u
!             are scaled so that the square of their 2-norm is 2.0.

!        alpha contains the diagonal elements of the r-matrix.

!        ipivot reflects the column pivoting performed on the input
!             matrix to accomplish the decomposition. the j-th
!             element of ipivot gives the column of the original
!             matrix which was pivoted into column j during the
!             decomposition.

!        ierr is set to.
!             0 for normal return,
!             k if no non-zero pivot could be found for the k-th
!                  transformation, or
!             -k for an error exit on the k-th thansformation.
!             if an error exit was taken, the first (k - 1)
!             transformations are correct.


!     *****applications and usage restrictions.
!     this may be used when solving linear least-squares problems --
!     see subroutine qr1 of rosepack.  it is called for this purpose
!     by llsqst in the nl2sol (nonlinear least-squares) package.

!     *****algorithm notes.
!     this version of qrfact tries to eliminate the occurrence of
!     underflows during the accumulation of inner products.  rktol1
!     is chosen below so as to insure that discarded terms have no
!     effect on the computed two-norms.

!     adapted from the algol routine solve (1).

!     *****references.
!     (1)     businger,p. and golub,g.h., linear least squares
!     solutions by housholder transformations, in wilkinson,j.h.
!     and reinsch,c.(eds.), handbook for automatic computation,
!     volume ii. linear algebra, springer-verlag, 111-118 (1971).
!     prepublished in numer.math. 7, 269-276 (1965).

!     *****history.
!     this amounts to the subroutine qr1 of rosepack with rktol1 used
!     in place of rktol below, with v2norm used to initialize (and
!     sometimes update) the sum array, and with calls on dotprd and
!     vaxpy in place of some loops.

!     *****general.

!     development of this program supported in part by
!     national science foundation grant gj-1154x3 and
!     national science foundation grant dcr75-08802
!     to national bureau of economic research, inc.


!     ..................................................................
!     ..................................................................


!     ..........  ufeta is the smallest positive floating point number
!        s.t. ufeta and -ufeta can both be represented.

!     ..........  rktol is the square root of the relative precision
!        of floating point arithmetic (machep).

!     *****body of program.
IF (ufeta > zero) GO TO 10
ufeta=rmdcon(1)
rktol=rmdcon(4)
10 ierr=0
rktol1=p01*rktol

DO j=1,n
  sum(j)=v2norm(m,qr(:,j))
  ipivot(j)=j
END DO

minum=MIN(m,n)

DO k=1,minum
  mk1=m-k+1
!        ..........k-th householder transformation..........
  sigma=zero
  jbar=0
!        ..........find largest column sum..........
  IF (k <= nopivk) GO TO 50
  DO j=k,n
    IF (sigma >= sum(j)) CYCLE
    sigma=sum(j)
    jbar=j
  END DO
  
  IF (jbar == 0) GO TO 120
  IF (jbar == k) GO TO 50
!        ..........column interchange..........
  i=ipivot(k)
  ipivot(k)=ipivot(jbar)
  ipivot(jbar)=i
  sum(jbar)=sum(k)
  sum(k)=sigma
  
  DO i=1,m
    sigma=qr(i,k)
    qr(i,k)=qr(i,jbar)
    qr(i,jbar)=sigma
  END DO
!        ..........end of column interchange..........
!        ..........  second inner product  ..........
  50 qrkmax=zero
  
  DO i=k,m
    IF (ABS(qr(i,k)) > qrkmax) qrkmax=ABS(qr(i,k))
  END DO
  
  IF (qrkmax < ufeta) GO TO 110
  alphak=v2norm(mk1,qr(k:,k))/qrkmax
  sigma=alphak**2
  
!        ..........  end second inner product  ..........
  qrkk=qr(k,k)
  IF (qrkk >= zero) alphak=-alphak
  alpha(k)=alphak*qrkmax
  beta=qrkmax*SQRT(sigma-(qrkk*alphak/qrkmax))
  qr(k,k)=qrkk-alpha(k)
  qr(k:m,k)=qr(k:m,k)/beta
  k1=k+1
  IF (k1 > n) CYCLE
  
  DO j=k1,n
    temp=-dotprd(mk1,qr(k:,k),qr(k:,j))
    
!             ***  set qr(i,j) = qr(i,j) + temp*qr(i,k), i = k,...,m.
    
    CALL vaxpy (mk1, qr(k:,j), temp, qr(k:,k), qr(k:,j))
    
    IF (k1 > m) CYCLE
    sumj=sum(j)
    IF (sumj < ufeta) CYCLE
    temp=ABS(qr(k,j)/sumj)
    IF (temp < rktol1) CYCLE
    IF (temp >= p99) GO TO 80
    sum(j)=sumj*SQRT(one-temp**2)
    CYCLE
    80 sum(j)=v2norm(m-k,qr(k1:,j))
  END DO
!        ..........end of k-th householder transformation..........
END DO

GO TO 150
!     ..........error exit on k-th transformation..........
110 ierr=-k
GO TO 130
!     ..........no non-zero acceptable pivot found..........
120 ierr=k
130 DO i=k,n
  alpha(i)=zero
  IF (i > k) CALL vscopy (i-k, qr(k:,i), zero)
END DO
!     ..........return to caller..........
150 RETURN
!     ..........last card of qrfact..........
END SUBROUTINE qrfact


FUNCTION reldst (p, d, x, x0) RESULT(fn_val)

!  ***  compute and return relative difference between x and x0 ***
!  ***  nl2sol version 2.2  ***

INTEGER, INTENT(IN)   :: p
REAL (dp), INTENT(IN) :: d(:),x(:),x0(:)
REAL (dp)             :: fn_val

INTEGER   :: i
REAL (dp) :: emax,t,xmax,zero=0.d+0

emax=zero
xmax=zero
DO i=1,p
  t=ABS(d(i)*(x(i)-x0(i)))
  IF (emax < t) emax=t
  t=d(i)*(ABS(x(i))+ABS(x0(i)))
  IF (xmax < t) xmax=t
END DO
fn_val=zero
IF (xmax > zero) fn_val=emax/xmax
RETURN
!  ***  last card of reldst follows  ***
END FUNCTION reldst


SUBROUTINE rptmul (func, ipivot, j, p, rd, x, y, z)

!  ***  func = 1... set  y = rmat * (perm**t) * x.
!  ***  func = 2... set  y = perm * (rmat**t) * rmat * (perm**t) * x.
!  ***  func = 3... set  y = perm * (rmat**t) x.

!  ***  perm = matrix whose i-th col. is the ipivot(i)-th unit vector.
!  ***  rmat is the upper triangular matrix whose strict upper triangle
!  ***       is stored in  j  and whose diagonal is stored in rd.
!  ***  z is a scratch vector.
!  ***  x and y may share storage.

INTEGER, INTENT(IN)       :: func, p, ipivot(:)
REAL (dp), INTENT(IN)     :: j(:,:), rd(:)
REAL (dp), INTENT(IN OUT) :: x(:), y(:), z(:)
! dimension j(nn,p)

!  ***  local variables  ***

INTEGER   :: i, im1, k, km1
REAL (dp) :: zk

!-----------------------------------------------------------------------

IF (func > 2) GO TO 50

!  ***  first set  z = (perm**t) * x  ***

DO i=1,p
  k=ipivot(i)
  z(i)=x(k)
END DO

!  ***  now set  y = rmat * z  ***

y(1)=z(1)*rd(1)
IF (p <= 1) GO TO 40
DO k=2,p
  km1=k-1
  zk=z(k)
  y(1:km1)=y(1:km1) + j(1:km1,k)*zk
  y(k)=zk*rd(k)
END DO

40 IF (func <= 1) GO TO 110
GO TO 70

50 y(1:p)=x(1:p)

!  ***  set  z = (rmat**t) * y  ***

70 z(1)=y(1)*rd(1)
IF (p == 1) GO TO 90
DO i=2,p
  im1=i-1
  z(i)=y(i)*rd(i) + dotprd(im1,j(1:,i),y)
END DO

!  ***  now set  y = perm * z  ***

90 DO i=1,p
  k=ipivot(i)
  y(k)=z(i)
END DO

110 RETURN
!  ***  last card of rptmul follows  ***
END SUBROUTINE rptmul


SUBROUTINE slupdt (a, cosmin, p, size, step, u, w, wchmtd, wscale,y)

!  ***  update symmetric  a  so that  a * step = y  ***
!  ***  (lower triangle of  a  stored rowwise       ***

!  ***  parameter declarations  ***

INTEGER, INTENT(IN)       :: p
REAL (dp), INTENT(IN)     :: cosmin, size, step(:), wchmtd(:), y(:)
REAL (dp), INTENT(IN OUT) :: a(:), u(:), w(:)
REAL (dp), INTENT(OUT)    :: wscale
!     dimension a(p*(p+1)/2)

!  ***  local variables  ***

INTEGER   :: i, j, k
REAL (dp) :: denmin, sdotwm, t, ui, wi

REAL (dp), PARAMETER :: half=0.5d+0, one=1.d+0, zero=0.d+0

!-----------------------------------------------------------------------

sdotwm=dotprd(p,step,wchmtd)
denmin=cosmin*v2norm(p,step)*v2norm(p,wchmtd)
wscale=one
IF (denmin /= zero) wscale=MIN(one,ABS(sdotwm/denmin))
t=zero
IF (sdotwm /= zero) t=wscale/sdotwm
w(1:p)=t*wchmtd(1:p)
CALL slvmul (p, u, a, step)
t=half*(size*dotprd(p,step,u) - dotprd(p,step,y))
u(1:p)=t*w(1:p) + y(1:p) - size*u(1:p)

!  ***  set  a = a + u*(w**t) + w*(u**t)  ***

k=1
DO i=1,p
  ui=u(i)
  wi=w(i)
  DO j=1,i
    a(k)=size*a(k) + ui*w(j) + wi*u(j)
    k=k+1
  END DO
END DO

RETURN
!  ***  last card of slupdt follows  ***
END SUBROUTINE slupdt


SUBROUTINE slvmul (p, y, s, x)

!  ***  set  y = s * x,  s = p x p symmetric matrix.  ***
!  ***  lower triangle of  s  stored rowwise.         ***

!  ***  parameter declarations  ***

INTEGER, INTENT(IN)    :: p
REAL (dp), INTENT(IN)  :: s(:), x(:)
REAL (dp), INTENT(OUT) :: y(:)
!     dimension s(p*(p+1)/2)

!  ***  local variables  ***

INTEGER   :: i, im1, j, k
REAL (dp) :: xi

!  ***  no intrinsic functions  ***

!-----------------------------------------------------------------------

j=1
DO i=1,p
  y(i)=dotprd(i,s(j:),x)
  j=j+i
END DO

IF (p <= 1) GO TO 40
j=1
DO i=2,p
  xi=x(i)
  im1=i-1
  j=j+1
  DO k=1,im1
    y(k)=y(k) + s(j)*xi
    j=j+1
  END DO
END DO

40 RETURN
!  ***  last card of slvmul follows  ***
END SUBROUTINE slvmul


FUNCTION stopx() RESULT(fn_val)
!     *****parameters...
LOGICAL      :: fn_val

!     ..................................................................

!     *****purpose...
!     this function may serve as the stopx (asynchronous interruption)
!     function for the nl2sol (nonlinear least-squares) package at
!     those installations which do not wish to implement a
!     dynamic stopx.

!     *****algorithm notes...
!     at installations where the nl2sol system is used
!     interactively, this dummy stopx should be replaced by a
!     function that returns .true. if and only if the interrupt
!     (break) key has been pressed since the last call on stopx.

!     ..................................................................

fn_val=.false.
RETURN
END FUNCTION stopx


SUBROUTINE vaxpy (p, w, a, x, y)

!  ***  set w = a*x + y  --  w, x, y = p-vectors, a = scalar  ***

INTEGER, INTENT(IN)    :: p
REAL (dp), INTENT(IN)  :: a, x(:), y(:)
REAL (dp), INTENT(OUT) :: w(:)

w(1:p) = a*x(1:p) + y(1:p)
RETURN
END SUBROUTINE vaxpy


SUBROUTINE vcopy (p, y, x)

!  ***  set y = x, where x and y are p-vectors  ***

INTEGER, INTENT(IN)    :: p
REAL (dp), INTENT(IN)  :: x(:)
REAL (dp), INTENT(OUT) :: y(:)

y(1:p) = x(1:p)
RETURN
END SUBROUTINE vcopy


SUBROUTINE vscopy (p, y, s)

!  ***  set p-vector y to scalar s  ***

INTEGER, INTENT(IN)    :: p
REAL (dp), INTENT(IN)  :: s
REAL (dp), INTENT(OUT) :: y(:)

y(1:p)=s
RETURN
END SUBROUTINE vscopy


FUNCTION v2norm (p, x) RESULT(fn_val)

!  ***  return the 2-norm of the p-vector x, taking  ***
!  ***  care to avoid the most likely underflows.    ***

INTEGER, INTENT(IN)   :: p
REAL (dp), INTENT(IN) :: x(:)
REAL (dp)             :: fn_val

INTEGER   :: i,j
REAL (dp) :: r,scale,t,xi

REAL (dp), PARAMETER :: one=1.d+0, zero=0.d+0
!     save sqteta
REAL (dp), SAVE :: sqteta=0.d+0

IF (p > 0) GO TO 10
fn_val=zero
GO TO 70
10 DO i=1,p
  IF (x(i) /= zero) GO TO 30
END DO
fn_val=zero
GO TO 70

30 scale=ABS(x(i))
IF (i < p) GO TO 40
fn_val=scale
GO TO 70
40 t=one
IF (sqteta == zero) sqteta=rmdcon(2)

!     ***  sqteta is (slightly larger than) the square root of the
!     ***  smallest positive floating point number on the machine.
!     ***  the tests involving sqteta are done to prevent underflows.

j=i+1
DO i=j,p
  xi=ABS(x(i))
  IF (xi > scale) GO TO 50
  r=xi/scale
  IF (r > sqteta) t=t+r*r
  CYCLE
  50 r=scale/xi
  IF (r <= sqteta) r=zero
  t=one+t*r*r
  scale=xi
END DO

fn_val=scale*SQRT(t)
70 RETURN
!  ***  last card of v2norm follows  ***
END FUNCTION v2norm


END MODULE toms573
