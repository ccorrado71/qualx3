!--------------------------------------------------------------------------------------------
        MODULE nrtype
        USE type_constants
        REAL(DP), PARAMETER :: PI_D=3.141592653589793238462643383279502884197_dp
        INTEGER, PARAMETER  :: SPC = KIND((1.0,1.0))
        INTEGER, PARAMETER  :: DPC = KIND((1.0D0,1.0D0))
        INTEGER, PARAMETER  :: LGT = KIND(.true.)
        REAL(SP), PARAMETER :: TWOPI=6.283185307179586476925286766559005768394_sp
        REAL(DP), PARAMETER :: TWOPI_D=6.283185307179586476925286766559005768394_dp
        TYPE sprs2_sp
              INTEGER(I4B) :: n,len
              REAL(SP), DIMENSION(:), POINTER :: val
              INTEGER(I4B), DIMENSION(:), POINTER :: irow
              INTEGER(I4B), DIMENSION(:), POINTER :: jcol
        END TYPE sprs2_sp
        TYPE sprs2_dp
              INTEGER(I4B) :: n,len
              REAL(DP), DIMENSION(:), POINTER :: val
              INTEGER(I4B), DIMENSION(:), POINTER :: irow
              INTEGER(I4B), DIMENSION(:), POINTER :: jcol
        END TYPE sprs2_dp
        END MODULE nrtype
!--------------------------------------------------------------------------------------------
        MODULE nr

        INTERFACE
            SUBROUTINE asolve(b,x,itrnsp,err)
            USE nrtype
            REAL(DP), DIMENSION(:), INTENT(IN) :: b
            REAL(DP), DIMENSION(:), INTENT(OUT) :: x
            INTEGER(I4B), INTENT(IN) :: itrnsp
            integer, intent(out)     :: err
            END SUBROUTINE asolve
        END INTERFACE
        INTERFACE
            SUBROUTINE atimes(x,r,itrnsp)
            USE nrtype
            REAL(DP), DIMENSION(:), INTENT(IN) :: x
            REAL(DP), DIMENSION(:), INTENT(OUT) :: r
            INTEGER(I4B), INTENT(IN) :: itrnsp
            END SUBROUTINE atimes
        END INTERFACE
        INTERFACE
            FUNCTION chebev(a,b,c,x)
            USE nrtype
            REAL(SP), INTENT(IN) :: a,b,x
            REAL(SP), DIMENSION(:), INTENT(IN) :: c
            REAL(SP) :: chebev
            END FUNCTION chebev
        END INTERFACE
        INTERFACE
            FUNCTION chebft(a,b,n,func)
            USE nrtype
            REAL(SP), INTENT(IN) :: a,b
            INTEGER(I4B), INTENT(IN) :: n
            REAL(SP), DIMENSION(n) :: chebft
            INTERFACE
            FUNCTION func(x)
            USE nrtype
            implicit none
            real(sp), dimension(:), intent(in) :: x
            real(sp), dimension(size(x)) :: func
            END FUNCTION func
            END INTERFACE
            END FUNCTION chebft
        END INTERFACE
        INTERFACE
            FUNCTION chder(a,b,c)
            USE nrtype
            REAL(SP), INTENT(IN) :: a,b
            REAL(SP), DIMENSION(:), INTENT(IN) :: c
            REAL(SP), DIMENSION(size(c)) :: chder
            END FUNCTION chder
        END INTERFACE
        INTERFACE
            SUBROUTINE choldc(a,p,ier)
            USE nrtype
            REAL(DP), DIMENSION(:,:), INTENT(INOUT) :: a
            REAL(DP), DIMENSION(:), INTENT(OUT) :: p
            INTEGER, INTENT(OUT) :: ier
            END SUBROUTINE choldc
        END INTERFACE
        INTERFACE
            SUBROUTINE cholsl(a,p,b,x)
            USE nrtype
            REAL(DP), DIMENSION(:,:), INTENT(IN) :: a
            REAL(DP), DIMENSION(:), INTENT(IN) :: p,b
            REAL(DP), DIMENSION(:), INTENT(INOUT) :: x
            END SUBROUTINE cholsl
        END INTERFACE
        INTERFACE
            FUNCTION convlv(data,respns,isign)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(INOUT) :: data
            REAL(SP), DIMENSION(:), INTENT(IN) :: respns
            INTEGER(I4B), INTENT(IN) :: isign
            REAL(SP), DIMENSION(size(data)) :: convlv
            END FUNCTION convlv
        END INTERFACE
        INTERFACE
            SUBROUTINE covsrt(covar,maska)
            USE nrtype
            REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: covar
            LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: maska
            END SUBROUTINE covsrt
        END INTERFACE
        INTERFACE
            FUNCTION dfridr(func,x,h,err)
            USE nrtype
            REAL(SP), INTENT(IN) :: x,h
            REAL(SP), INTENT(OUT) :: err
            REAL(SP) :: dfridr
            INTERFACE
            FUNCTION func(x)
            USE nrtype
            REAL(SP), INTENT(IN) :: x
            REAL(SP) :: func
            END FUNCTION func
            END INTERFACE
            END FUNCTION dfridr
        END INTERFACE
        INTERFACE indexx
            SUBROUTINE indexx_sp(arr,index)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(IN) :: arr
            INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: index
            END SUBROUTINE indexx_sp
            SUBROUTINE indexx_i4b(iarr,index)
            USE nrtype
            INTEGER(I4B), DIMENSION(:), INTENT(IN) :: iarr
            INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: index
            END SUBROUTINE indexx_i4b
        END INTERFACE
        INTERFACE
                SUBROUTINE fit(x,y,a,b,siga,sigb,chi2,q,sig)
                USE nrtype
                REAL(SP), DIMENSION(:), INTENT(IN) :: x,y
                REAL(SP), INTENT(OUT) :: a,b,siga,sigb,chi2,q
                REAL(SP), DIMENSION(:), OPTIONAL, INTENT(IN) :: sig
                END SUBROUTINE fit
        END INTERFACE
        INTERFACE
            FUNCTION fpoly(x,n)
            USE nrtype
            REAL(SP), INTENT(IN) :: x
            INTEGER(I4B), INTENT(IN) :: n
            REAL(SP), DIMENSION(n) :: fpoly
            END FUNCTION fpoly
        END INTERFACE
        INTERFACE four1
            SUBROUTINE four1_dp(data,isign)
            USE nrtype
            COMPLEX(DPC), DIMENSION(:), INTENT(INOUT) :: data
            INTEGER(I4B), INTENT(IN) :: isign
            END SUBROUTINE four1_dp
!BL
            SUBROUTINE four1_sp(data,isign)
            USE nrtype
            COMPLEX(SPC), DIMENSION(:), INTENT(INOUT) :: data
            INTEGER(I4B), INTENT(IN) :: isign
            END SUBROUTINE four1_sp
        END INTERFACE
        INTERFACE fourrow
            SUBROUTINE fourrow_dp(data,isign)
            USE nrtype
            COMPLEX(DPC), DIMENSION(:,:), INTENT(INOUT) :: data
            INTEGER(I4B), INTENT(IN) :: isign
            END SUBROUTINE fourrow_dp
!BL
            SUBROUTINE fourrow_sp(data,isign)
            USE nrtype
            COMPLEX(SPC), DIMENSION(:,:), INTENT(INOUT) :: data
            INTEGER(I4B), INTENT(IN) :: isign
            END SUBROUTINE fourrow_sp
        END INTERFACE
        INTERFACE gser
                FUNCTION gser_s(a,x,gln)
                USE nrtype
                REAL(SP), INTENT(IN) :: a,x
                REAL(SP), OPTIONAL, INTENT(OUT) :: gln
                REAL(SP) :: gser_s
                END FUNCTION gser_s
!BL
                FUNCTION gser_v(a,x,gln)
                USE nrtype
                REAL(SP), DIMENSION(:), INTENT(IN) :: a,x
                REAL(SP), DIMENSION(:), OPTIONAL, INTENT(OUT) :: gln
                REAL(SP), DIMENSION(size(a)) :: gser_v
                END FUNCTION gser_v
        END INTERFACE
        INTERFACE gammln
            FUNCTION gammln_s(xx)
            USE nrtype
            REAL(SP), INTENT(IN) :: xx
            REAL(SP) :: gammln_s
            END FUNCTION gammln_s
!BL
            FUNCTION gammln_v(xx)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(IN) :: xx
            REAL(SP), DIMENSION(size(xx)) :: gammln_v
            END FUNCTION gammln_v
        END INTERFACE
        INTERFACE gammq
                FUNCTION gammq_s(a,x)
                USE nrtype
                REAL(SP), INTENT(IN) :: a,x
                REAL(SP) :: gammq_s
                END FUNCTION gammq_s
!BL
                FUNCTION gammq_v(a,x)
                USE nrtype
                REAL(SP), DIMENSION(:), INTENT(IN) :: a,x
                REAL(SP), DIMENSION(size(a)) :: gammq_v
                END FUNCTION gammq_v
        END INTERFACE
        INTERFACE
            SUBROUTINE gaussj(a,b)
            USE nrtype
            REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: a,b
            END SUBROUTINE gaussj
        END INTERFACE
        INTERFACE gcf
                FUNCTION gcf_s(a,x,gln)
                USE nrtype
                REAL(SP), INTENT(IN) :: a,x
                REAL(SP), OPTIONAL, INTENT(OUT) :: gln
                REAL(SP) :: gcf_s
                END FUNCTION gcf_s
!BL
                FUNCTION gcf_v(a,x,gln)
                USE nrtype
                REAL(SP), DIMENSION(:), INTENT(IN) :: a,x
                REAL(SP), DIMENSION(:), OPTIONAL, INTENT(OUT) :: gln
                REAL(SP), DIMENSION(size(a)) :: gcf_v
                END FUNCTION gcf_v
        END INTERFACE
        INTERFACE
            SUBROUTINE lfit(x,y,sig,a,maska,covar,chisq,funcs)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(IN) :: x,y,sig
            REAL(SP), DIMENSION(:), INTENT(INOUT) :: a
            LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: maska
            REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: covar
            REAL(SP), INTENT(OUT) :: chisq
            INTERFACE
                    SUBROUTINE funcs(x,arr)
                    USE nrtype
                    REAL(SP),INTENT(IN) :: x
                    REAL(SP), DIMENSION(:), INTENT(OUT) :: arr
                    END SUBROUTINE funcs
            END INTERFACE
            END SUBROUTINE lfit
        END INTERFACE
        INTERFACE
            FUNCTION locate(xx,x)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(IN) :: xx
            REAL(SP), INTENT(IN) :: x
            INTEGER(I4B) :: locate
            END FUNCTION locate
        END INTERFACE
        INTERFACE
            SUBROUTINE polint(xa,ya,x,y,dy)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(IN) :: xa,ya
            REAL(SP), INTENT(IN) :: x
            REAL(SP), INTENT(OUT) :: y,dy
            END SUBROUTINE polint
        END INTERFACE
        INTERFACE realft
            SUBROUTINE realft_dp(data,isign,zdata)
            USE nrtype
            REAL(DP), DIMENSION(:), INTENT(INOUT) :: data
            INTEGER(I4B), INTENT(IN) :: isign
            COMPLEX(DPC), DIMENSION(:), OPTIONAL, TARGET :: zdata
            END SUBROUTINE realft_dp
!BL
            SUBROUTINE realft_sp(data,isign,zdata)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(INOUT) :: data
            INTEGER(I4B), INTENT(IN) :: isign
            COMPLEX(SPC), DIMENSION(:), OPTIONAL, TARGET :: zdata
            END SUBROUTINE realft_sp
        END INTERFACE
        INTERFACE svbksb
            SUBROUTINE svbksb_dp(u,w,v,b,x)
            USE nrtype
            REAL(DP), DIMENSION(:,:), INTENT(IN) :: u,v
            REAL(DP), DIMENSION(:), INTENT(IN) :: w,b
            REAL(DP), DIMENSION(:), INTENT(OUT) :: x
            END SUBROUTINE svbksb_dp
!BL
            SUBROUTINE svbksb_sp(u,w,v,b,x)
            USE nrtype
            REAL(SP), DIMENSION(:,:), INTENT(IN) :: u,v
            REAL(SP), DIMENSION(:), INTENT(IN) :: w,b
            REAL(SP), DIMENSION(:), INTENT(OUT) :: x
            END SUBROUTINE svbksb_sp
        END INTERFACE
        INTERFACE svdcmp
            SUBROUTINE svdcmp_dp(a,w,v)
            USE nrtype
            REAL(DP), DIMENSION(:,:), INTENT(INOUT) :: a
            REAL(DP), DIMENSION(:), INTENT(OUT) :: w
            REAL(DP), DIMENSION(:,:), INTENT(OUT) :: v
            END SUBROUTINE svdcmp_dp
!BL
            SUBROUTINE svdcmp_sp(a,w,v)
            USE nrtype
            REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: a
            REAL(SP), DIMENSION(:), INTENT(OUT) :: w
            REAL(SP), DIMENSION(:,:), INTENT(OUT) :: v
            END SUBROUTINE svdcmp_sp
        END INTERFACE
        INTERFACE
            SUBROUTINE svdfit(x,y,sig,a,v,w,chisq,funcs)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(IN) :: x,y,sig
            REAL(SP), DIMENSION(:), INTENT(OUT) :: a,w
            REAL(SP), DIMENSION(:,:), INTENT(OUT) :: v
            REAL(SP), INTENT(OUT) :: chisq
            INTERFACE
                    FUNCTION funcs(x,n)
                    USE nrtype
                    REAL(SP), INTENT(IN) :: x
                    INTEGER(I4B), INTENT(IN) :: n
                    REAL(SP), DIMENSION(n) :: funcs
                    END FUNCTION funcs
            END INTERFACE
            END SUBROUTINE svdfit
        END INTERFACE
        INTERFACE
            SUBROUTINE linbcg(b,x,itol,tol,itmax,iter,err,errd)
            USE nrtype
            REAL(DP), DIMENSION(:), INTENT(IN) :: b
            REAL(DP), DIMENSION(:), INTENT(INOUT) :: x
            INTEGER(I4B), INTENT(IN) :: itol,itmax
            REAL(DP), INTENT(IN) :: tol
            INTEGER(I4B), INTENT(OUT) :: iter
            REAL(DP), INTENT(OUT) :: err
            integer, intent(out)  :: errd
            END SUBROUTINE linbcg
        END INTERFACE
        INTERFACE
            SUBROUTINE lubksb_new(a,indx,b)
            USE nrtype
            REAL(SP), DIMENSION(:,:), INTENT(IN) :: a
            INTEGER(I4B), DIMENSION(:), INTENT(IN) :: indx
            REAL(SP), DIMENSION(:), INTENT(INOUT) :: b
            END SUBROUTINE lubksb_new
        END INTERFACE
        INTERFACE
            SUBROUTINE ludcmp_new(a,indx,d,ier)
            USE nrtype
            REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: a
            INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: indx
            REAL(SP), INTENT(OUT) :: d
            INTEGER, INTENT(OUT), optional :: ier
            END SUBROUTINE ludcmp_new
        END INTERFACE
        INTERFACE pythag
            FUNCTION pythag_dp(a,b)
            USE nrtype
            REAL(DP), INTENT(IN) :: a,b
            REAL(DP) :: pythag_dp
            END FUNCTION pythag_dp
!BL
            FUNCTION pythag_sp(a,b)
            USE nrtype
            REAL(SP), INTENT(IN) :: a,b
            REAL(SP) :: pythag_sp
            END FUNCTION pythag_sp
        END INTERFACE
        INTERFACE
            SUBROUTINE sortnr(arr)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(INOUT) :: arr
            END SUBROUTINE sortnr
        END INTERFACE
        INTERFACE
            FUNCTION savgol(nl,nrr,ld,m)
            USE nrtype
            INTEGER(I4B), INTENT(IN) :: nl,nrr,ld,m
            REAL(SP), DIMENSION(nl+nrr+1) :: savgol
            END FUNCTION savgol
        END INTERFACE
        INTERFACE
            SUBROUTINE spline(x,y,yp1,ypn,y2)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(IN) :: x,y
            REAL(SP), INTENT(IN) :: yp1,ypn
            REAL(SP), DIMENSION(:), INTENT(OUT) :: y2
            END SUBROUTINE spline
        END INTERFACE
        INTERFACE
            FUNCTION splint(xa,ya,y2a,x)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(IN) :: xa,ya,y2a
            REAL(SP), INTENT(IN) :: x
            REAL(SP) :: splint
            END FUNCTION splint
        END INTERFACE
        INTERFACE
            FUNCTION snrm(sx,itol)
            USE nrtype
            REAL(DP), DIMENSION(:), INTENT(IN) :: sx
            INTEGER(I4B), INTENT(IN) :: itol
            REAL(DP) :: snrm
            END FUNCTION snrm
        END INTERFACE
        INTERFACE sprsax
            SUBROUTINE sprsax_dp(sa,x,b)
            USE nrtype
            TYPE(sprs2_dp), INTENT(IN) :: sa
            REAL(DP), DIMENSION (:), INTENT(IN) :: x
            REAL(DP), DIMENSION (:), INTENT(OUT) :: b
            END SUBROUTINE sprsax_dp
!BL
            SUBROUTINE sprsax_sp(sa,x,b)
            USE nrtype
            TYPE(sprs2_sp), INTENT(IN) :: sa
            REAL(SP), DIMENSION (:), INTENT(IN) :: x
            REAL(SP), DIMENSION (:), INTENT(OUT) :: b
            END SUBROUTINE sprsax_sp
        END INTERFACE
        INTERFACE sprsin
            SUBROUTINE sprsin_sp(a,thresh,sa)
            USE nrtype
            REAL(SP), DIMENSION(:,:), INTENT(IN) :: a
            REAL(SP), INTENT(IN) :: thresh
            TYPE(sprs2_sp), INTENT(OUT) :: sa
            END SUBROUTINE sprsin_sp
!BL
            SUBROUTINE sprsin_dp(a,thresh,sa)
            USE nrtype
            REAL(DP), DIMENSION(:,:), INTENT(IN) :: a
            REAL(DP), INTENT(IN) :: thresh
            TYPE(sprs2_dp), INTENT(OUT) :: sa
            END SUBROUTINE sprsin_dp
        END INTERFACE
        INTERFACE sprsdiag
            SUBROUTINE sprsdiag_dp(sa,b)
            USE nrtype
            TYPE(sprs2_dp), INTENT(IN) :: sa
            REAL(DP), DIMENSION(:), INTENT(OUT) :: b
            END SUBROUTINE sprsdiag_dp
!BL
            SUBROUTINE sprsdiag_sp(sa,b)
            USE nrtype
            TYPE(sprs2_sp), INTENT(IN) :: sa
            REAL(SP), DIMENSION(:), INTENT(OUT) :: b
            END SUBROUTINE sprsdiag_sp
        END INTERFACE
        INTERFACE sprstx
            SUBROUTINE sprstx_dp(sa,x,b)
            USE nrtype
            TYPE(sprs2_dp), INTENT(IN) :: sa
            REAL(DP), DIMENSION (:), INTENT(IN) :: x
            REAL(DP), DIMENSION (:), INTENT(OUT) :: b
            END SUBROUTINE sprstx_dp
!BL
            SUBROUTINE sprstx_sp(sa,x,b)
            USE nrtype
            TYPE(sprs2_sp), INTENT(IN) :: sa
            REAL(SP), DIMENSION (:), INTENT(IN) :: x
            REAL(SP), DIMENSION (:), INTENT(OUT) :: b
            END SUBROUTINE sprstx_sp
        END INTERFACE
        INTERFACE tridag
            SUBROUTINE tridag_ser(a,b,c,r,u)
            USE nrtype
            REAL(SP), DIMENSION(:), INTENT(IN) :: a,b,c,r
            REAL(SP), DIMENSION(:), INTENT(OUT) :: u
            END SUBROUTINE tridag_ser
        END INTERFACE

        END MODULE nr
!--------------------------------------------------------------------------------------------
        MODULE nrutil

        USE nrtype
        IMPLICIT NONE
        integer(I4B), parameter :: NPAR_ARTH=16,NPAR2_ARTH=8
        INTEGER(I4B), PARAMETER :: NPAR_GEOP=4,NPAR2_GEOP=2
        INTEGER(I4B), PARAMETER :: NPAR_CUMSUM=16
        INTEGER(I4B), PARAMETER :: NPAR_POLY=8
        INTERFACE array_copy
            MODULE PROCEDURE array_copy_r, array_copy_d, array_copy_i
        END INTERFACE
        INTERFACE arth
            MODULE PROCEDURE arth_r, arth_d, arth_i
        END INTERFACE
        INTERFACE assert
                MODULE PROCEDURE assert1,assert2,assert3,assert4,assert_v
        END INTERFACE
        INTERFACE assert_eq
            MODULE PROCEDURE assert_eq2,assert_eq3,assert_eq4,assert_eqn
        END INTERFACE
        INTERFACE cumsum
            MODULE PROCEDURE cumsum_r,cumsum_i
        END INTERFACE
        INTERFACE diagmult
            MODULE PROCEDURE diagmult_rv,diagmult_r
        END INTERFACE
        INTERFACE imaxloc
            MODULE PROCEDURE imaxloc_r
        END INTERFACE
        INTERFACE swap
            MODULE PROCEDURE swap_i,swap_r,swap_rv,swap_c, &
                             swap_cv,swap_cm,swap_z,swap_zv,swap_zm, &
                             masked_swap_rs,masked_swap_rv,masked_swap_rm
        END INTERFACE
        INTERFACE geop
            MODULE PROCEDURE geop_r
        END INTERFACE
        INTERFACE outerprod
            MODULE PROCEDURE outerprod_r, outerprod_d
        END INTERFACE
        INTERFACE poly
            MODULE PROCEDURE poly_rr,poly_rrv
        END INTERFACE
        INTERFACE scatter_add
            MODULE PROCEDURE scatter_add_r,scatter_add_d
        END INTERFACE

        CONTAINS

        SUBROUTINE array_copy_r(src,dest,n_copied,n_not_copied)
        REAL(SP), DIMENSION(:), INTENT(IN) :: src
        REAL(SP), DIMENSION(:), INTENT(OUT) :: dest
        INTEGER(I4B), INTENT(OUT) :: n_copied, n_not_copied
        n_copied=min(size(src),size(dest))
        n_not_copied=size(src)-n_copied
        dest(1:n_copied)=src(1:n_copied)
        END SUBROUTINE array_copy_r
!BL
        SUBROUTINE array_copy_d(src,dest,n_copied,n_not_copied)
        REAL(DP), DIMENSION(:), INTENT(IN) :: src
        REAL(DP), DIMENSION(:), INTENT(OUT) :: dest
        INTEGER(I4B), INTENT(OUT) :: n_copied, n_not_copied
        n_copied=min(size(src),size(dest))
        n_not_copied=size(src)-n_copied
        dest(1:n_copied)=src(1:n_copied)
        END SUBROUTINE array_copy_d
!BL
        SUBROUTINE array_copy_i(src,dest,n_copied,n_not_copied)
        INTEGER(I4B), DIMENSION(:), INTENT(IN) :: src
        INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: dest
        INTEGER(I4B), INTENT(OUT) :: n_copied, n_not_copied
        n_copied=min(size(src),size(dest))
        n_not_copied=size(src)-n_copied
        dest(1:n_copied)=src(1:n_copied)
        END SUBROUTINE array_copy_i

        FUNCTION iminloc(arr)
        REAL(SP), DIMENSION(:), INTENT(IN) :: arr
        INTEGER(I4B), DIMENSION(1) :: imin
        INTEGER(I4B) :: iminloc
        imin=minloc(arr(:))
        iminloc=imin(1)
        END FUNCTION iminloc

        SUBROUTINE assert1(n1,string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        LOGICAL, INTENT(IN) :: n1
        if (.not. n1) then
            write (*,*) 'nrerror: an assertion failed with this tag:',string
            STOP 'program terminated by assert1'
        end if
        END SUBROUTINE assert1
!BL
        SUBROUTINE assert2(n1,n2,string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        LOGICAL, INTENT(IN) :: n1,n2
        if (.not. (n1 .and. n2)) then
                write (*,*) 'nrerror: an assertion failed with this tag:', &
                        string
                STOP 'program terminated by assert2'
        end if
        END SUBROUTINE assert2
!BL
        SUBROUTINE assert3(n1,n2,n3,string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        LOGICAL, INTENT(IN) :: n1,n2,n3
        if (.not. (n1 .and. n2 .and. n3)) then
                write (*,*) 'nrerror: an assertion failed with this tag:', &
                        string
                STOP 'program terminated by assert3'
        end if
        END SUBROUTINE assert3
!BL
        SUBROUTINE assert4(n1,n2,n3,n4,string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        LOGICAL, INTENT(IN) :: n1,n2,n3,n4
        if (.not. (n1 .and. n2 .and. n3 .and. n4)) then
                write (*,*) 'nrerror: an assertion failed with this tag:', &
                        string
                STOP 'program terminated by assert4'
        end if
        END SUBROUTINE assert4
!BL
        SUBROUTINE assert_v(n,string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        LOGICAL, DIMENSION(:), INTENT(IN) :: n
        if (.not. all(n)) then
                write (*,*) 'nrerror: an assertion failed with this tag:', &
                        string
                STOP 'program terminated by assert_v'
        end if
        END SUBROUTINE assert_v
!BL
        FUNCTION assert_eq2(n1,n2,string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        INTEGER, INTENT(IN) :: n1,n2
        INTEGER :: assert_eq2
        if (n1 == n2) then
            assert_eq2=n1
        else
            write (*,*) 'nrerror: an assert_eq failed with this tag:',string
            STOP 'program terminated by assert_eq2'
        end if
        END FUNCTION assert_eq2

        FUNCTION assert_eq3(n1,n2,n3,string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        INTEGER, INTENT(IN) :: n1,n2,n3
        INTEGER :: assert_eq3
        if (n1 == n2 .and. n2 == n3) then
            assert_eq3=n1
        else
            write (*,*) 'nrerror: an assert_eq failed with this tag:',string
            STOP 'program terminated by assert_eq3'
        end if
        END FUNCTION assert_eq3

        FUNCTION assert_eq4(n1,n2,n3,n4,string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        INTEGER, INTENT(IN) :: n1,n2,n3,n4
        INTEGER :: assert_eq4
        if (n1 == n2 .and. n2 == n3 .and. n3 == n4) then
                assert_eq4=n1
        else
                write (*,*) 'nrerror: an assert_eq failed with this tag:', &
                        string
                STOP 'program terminated by assert_eq4'
        end if
        END FUNCTION assert_eq4

        FUNCTION assert_eqn(nn,string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        INTEGER, DIMENSION(:), INTENT(IN) :: nn
        INTEGER :: assert_eqn
        if (all(nn(2:) == nn(1))) then
            assert_eqn=nn(1)
        else
            write (*,*) 'nrerror: an assert_eq failed with this tag:',string
            STOP 'program terminated by assert_eqn'
        end if
        END FUNCTION assert_eqn

        SUBROUTINE diagmult_rv(mat,diag)
        REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: mat
        REAL(SP), DIMENSION(:), INTENT(IN) :: diag
        INTEGER(I4B) :: j,n
        n = assert_eq2(size(diag),min(size(mat,1),size(mat,2)),'diagmult_rv')
        do j=1,n
            mat(j,j)=mat(j,j)*diag(j)
        end do
        END SUBROUTINE diagmult_rv

        SUBROUTINE diagmult_r(mat,diag)
        REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: mat
        REAL(SP), INTENT(IN) :: diag
        INTEGER(I4B) :: j,n
        n = min(size(mat,1),size(mat,2))
        do j=1,n
            mat(j,j)=mat(j,j)*diag
        end do
        END SUBROUTINE diagmult_r

        FUNCTION geop_r(first,factor,n)
        REAL(SP), INTENT(IN) :: first,factor
        INTEGER(I4B), INTENT(IN) :: n
        REAL(SP), DIMENSION(n) :: geop_r
        INTEGER(I4B) :: k,k2
        REAL(SP) :: temp
        if (n > 0) geop_r(1)=first
        if (n <= NPAR_GEOP) then
            do k=2,n
               geop_r(k)=geop_r(k-1)*factor
            end do
        else
            do k=2,NPAR2_GEOP
               geop_r(k)=geop_r(k-1)*factor
            end do
            temp=factor**NPAR2_GEOP
            k=NPAR2_GEOP
            do
              if (k >= n) exit
              k2=k+k
              geop_r(k+1:min(k2,n))=temp*geop_r(1:min(k,n-k))
              temp=temp*temp
              k=k2
            end do
        end if
        END FUNCTION geop_r

        FUNCTION imaxloc_r(arr)
        REAL(SP), DIMENSION(:), INTENT(IN) :: arr
        INTEGER(I4B) :: imaxloc_r
        INTEGER(I4B), DIMENSION(1) :: imax
        imax=maxloc(arr(:))
        imaxloc_r=imax(1)
        END FUNCTION imaxloc_r
    
        SUBROUTINE swap_i(a,b)
        INTEGER(I4B), INTENT(INOUT) :: a,b
        INTEGER(I4B) :: dum
        dum=a
        a=b
        b=dum
        END SUBROUTINE swap_i
!BL
        SUBROUTINE swap_r(a,b)
        REAL(SP), INTENT(INOUT) :: a,b
        REAL(SP) :: dum
        dum=a
        a=b
        b=dum
        END SUBROUTINE swap_r
!BL
        SUBROUTINE swap_rv(a,b)
        REAL(SP), DIMENSION(:), INTENT(INOUT) :: a,b
        REAL(SP), DIMENSION(SIZE(a)) :: dum
        dum=a
        a=b
        b=dum
        END SUBROUTINE swap_rv
!BL
        SUBROUTINE swap_c(a,b)
        COMPLEX(SPC), INTENT(INOUT) :: a,b
        COMPLEX(SPC) :: dum
        dum=a
        a=b
        b=dum
        END SUBROUTINE swap_c
!BL
        SUBROUTINE swap_cv(a,b)
        COMPLEX(SPC), DIMENSION(:), INTENT(INOUT) :: a,b
        COMPLEX(SPC), DIMENSION(SIZE(a)) :: dum
        dum=a
        a=b
        b=dum
        END SUBROUTINE swap_cv
!BL
        SUBROUTINE swap_cm(a,b)
        COMPLEX(SPC), DIMENSION(:,:), INTENT(INOUT) :: a,b
        COMPLEX(SPC), DIMENSION(size(a,1),size(a,2)) :: dum
        dum=a
        a=b
        b=dum
        END SUBROUTINE swap_cm
!BL
        SUBROUTINE swap_z(a,b)
        COMPLEX(DPC), INTENT(INOUT) :: a,b
        COMPLEX(DPC) :: dum
        dum=a
        a=b
        b=dum
        END SUBROUTINE swap_z
!BL
        SUBROUTINE swap_zv(a,b)
        COMPLEX(DPC), DIMENSION(:), INTENT(INOUT) :: a,b
        COMPLEX(DPC), DIMENSION(SIZE(a)) :: dum
        dum=a
        a=b
        b=dum
        END SUBROUTINE swap_zv
!BL
        SUBROUTINE swap_zm(a,b)
        COMPLEX(DPC), DIMENSION(:,:), INTENT(INOUT) :: a,b
        COMPLEX(DPC), DIMENSION(size(a,1),size(a,2)) :: dum
        dum=a
        a=b
        b=dum
        END SUBROUTINE swap_zm
!BL
        SUBROUTINE masked_swap_rs(a,b,mask)
        REAL(SP), INTENT(INOUT) :: a,b
        LOGICAL(LGT), INTENT(IN) :: mask
        REAL(SP) :: swp
        if (mask) then
                swp=a
                a=b
                b=swp
        end if
        END SUBROUTINE masked_swap_rs
!BL
        SUBROUTINE masked_swap_rv(a,b,mask)
        REAL(SP), DIMENSION(:), INTENT(INOUT) :: a,b
        LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: mask
        REAL(SP), DIMENSION(size(a)) :: swp
        where (mask)
                swp=a
                a=b
                b=swp
        end where
        END SUBROUTINE masked_swap_rv
!BL
        SUBROUTINE masked_swap_rm(a,b,mask)
        REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: a,b
        LOGICAL(LGT), DIMENSION(:,:), INTENT(IN) :: mask
        REAL(SP), DIMENSION(size(a,1),size(a,2)) :: swp
        where (mask)
                swp=a
                a=b
                b=swp
        end where
        END SUBROUTINE masked_swap_rm
!BL
!BL

        FUNCTION zroots_unity(n,nn)
        INTEGER(I4B), INTENT(IN) :: n,nn
        COMPLEX(SPC), DIMENSION(nn) :: zroots_unity
        INTEGER(I4B) :: k
        REAL(SP) :: theta
        zroots_unity(1)=1.0
        theta=TWOPI/n
        k=1
        do
                if (k >= nn) exit
                zroots_unity(k+1)=cmplx(cos(k*theta),sin(k*theta),SPC)
                zroots_unity(k+2:min(2*k,nn))=zroots_unity(k+1)*&
                        zroots_unity(2:min(k,nn-k))
                k=2*k
        end do
        END FUNCTION zroots_unity
!BL
        FUNCTION outerprod_r(a,b)
        REAL(SP), DIMENSION(:), INTENT(IN) :: a,b
        REAL(SP), DIMENSION(size(a),size(b)) :: outerprod_r
        outerprod_r = spread(a,dim=2,ncopies=size(b)) * &
        spread(b,dim=1,ncopies=size(a))
        END FUNCTION outerprod_r 
    
        FUNCTION outerprod_d(a,b)
        REAL(DP), DIMENSION(:), INTENT(IN) :: a,b
        REAL(DP), DIMENSION(size(a),size(b)) :: outerprod_d
        outerprod_d = spread(a,dim=2,ncopies=size(b)) * &
        spread(b,dim=1,ncopies=size(a))
        END FUNCTION outerprod_d

        FUNCTION outerand(a,b)
        LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: a,b
        LOGICAL(LGT), DIMENSION(size(a),size(b)) :: outerand
        outerand = spread(a,dim=2,ncopies=size(b)) .and. &
                spread(b,dim=1,ncopies=size(a))
        END FUNCTION outerand
!BL
        FUNCTION poly_rr(x,coeffs)
        REAL(SP), INTENT(IN) :: x
        REAL(SP), DIMENSION(:), INTENT(IN) :: coeffs
        REAL(SP) :: poly_rr
        REAL(SP) :: pow
        REAL(SP), DIMENSION(:), ALLOCATABLE :: vec
        INTEGER(I4B) :: i,n,nn
        n=size(coeffs)
        if (n <= 0) then
        poly_rr=0.0_sp
        else if (n < NPAR_POLY) then
        poly_rr=coeffs(n)
        do i=n-1,1,-1
        poly_rr=x*poly_rr+coeffs(i)
        end do
        else
        allocate(vec(n+1))
        pow=x
        vec(1:n)=coeffs
        do
        vec(n+1)=0.0_sp
        nn=ishft(n+1,-1)
        vec(1:nn)=vec(1:n:2)+pow*vec(2:n+1:2)
        if (nn == 1) exit
        pow=pow*pow
        n=nn
        end do
        poly_rr=vec(1)
        deallocate(vec)
        end if
        END FUNCTION poly_rr

        FUNCTION poly_rrv(x,coeffs)
        REAL(SP), DIMENSION(:), INTENT(IN) :: coeffs,x
        REAL(SP), DIMENSION(size(x)) :: poly_rrv
        INTEGER(I4B) :: i,n,m
        m=size(coeffs)
        n=size(x)
        if (m <= 0) then
        poly_rrv=0.0_sp
        else if (m < n .or. m < NPAR_POLY) then
        poly_rrv=coeffs(m)
        do i=m-1,1,-1
        poly_rrv=x*poly_rrv+coeffs(i)
        end do
        else
        do i=1,n
        poly_rrv(i)=poly_rr(x(i),coeffs)
        end do
        end if
        END FUNCTION poly_rrv

        FUNCTION arth_r(first,increment,n)
        REAL(SP), INTENT(IN) :: first,increment
        INTEGER(I4B), INTENT(IN) :: n
        REAL(SP), DIMENSION(n) :: arth_r
        INTEGER(I4B) :: k,k2
        REAL(SP) :: temp
        if (n > 0) arth_r(1)=first
        if (n <= NPAR_ARTH) then
                 do k=2,n
                    arth_r(k)=arth_r(k-1)+increment
                 end do
        else
                 do k=2,NPAR2_ARTH
                    arth_r(k)=arth_r(k-1)+increment
                 end do
                 temp=increment*NPAR2_ARTH
                 k=NPAR2_ARTH
                 do
                    if (k >= n) exit
                    k2=k+k
                    arth_r(k+1:min(k2,n))=temp+arth_r(1:min(k,n-k))
                    temp=temp+temp
                    k=k2
                 end do
        end if
        END FUNCTION arth_r

        FUNCTION arth_d(first,increment,n)
        REAL(DP), INTENT(IN) :: first,increment
        INTEGER(I4B), INTENT(IN) :: n
        REAL(DP), DIMENSION(n) :: arth_d
        INTEGER(I4B) :: k,k2
        REAL(DP) :: temp
        if (n > 0) arth_d(1)=first
        if (n <= NPAR_ARTH) then
                 do k=2,n
                    arth_d(k)=arth_d(k-1)+increment
                 end do
        else
                 do k=2,NPAR2_ARTH
                    arth_d(k)=arth_d(k-1)+increment
                 end do
                 temp=increment*NPAR2_ARTH
                 k=NPAR2_ARTH
                 do
                   if (k >= n) exit
                   k2=k+k
                   arth_d(k+1:min(k2,n))=temp+arth_d(1:min(k,n-k))
                   temp=temp+temp
                   k=k2
                 end do
        end if
        END FUNCTION arth_d

        FUNCTION arth_i(first,increment,n)
        INTEGER(I4B), INTENT(IN) :: first,increment,n
        INTEGER(I4B), DIMENSION(n) :: arth_i
        INTEGER(I4B) :: k,k2,temp
        if (n > 0) arth_i(1)=first
        if (n <= NPAR_ARTH) then
                 do k=2,n
                    arth_i(k)=arth_i(k-1)+increment
                 end do
        else
                 do k=2,NPAR2_ARTH
                    arth_i(k)=arth_i(k-1)+increment
                 end do
                 temp=increment*NPAR2_ARTH
                 k=NPAR2_ARTH
                 do
                    if (k >= n) exit
                    k2=k+k
                    arth_i(k+1:min(k2,n))=temp+arth_i(1:min(k,n-k))
                    temp=temp+temp
                    k=k2
                 end do
        end if
        END FUNCTION arth_i
               
        RECURSIVE FUNCTION cumsum_r(arr,seed) RESULT(ans)
        REAL(SP), DIMENSION(:), INTENT(IN) :: arr
        REAL(SP), OPTIONAL, INTENT(IN) :: seed
        REAL(SP), DIMENSION(size(arr)) :: ans
        INTEGER(I4B) :: n,j
        REAL(SP) :: sd
        n=size(arr)
        if (n == 0_i4b) RETURN
        sd=0.0_sp
        if (present(seed)) sd=seed
        ans(1)=arr(1)+sd
        if (n < NPAR_CUMSUM) then
                do j=2,n
                   ans(j)=ans(j-1)+arr(j)
                end do
        else
                ans(2:n:2)=cumsum_r(arr(2:n:2)+arr(1:n-1:2),sd)
                ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
        end if
        END FUNCTION cumsum_r

        RECURSIVE FUNCTION cumsum_i(arr,seed) RESULT(ans)
        INTEGER(I4B), DIMENSION(:), INTENT(IN) :: arr
        INTEGER(I4B), OPTIONAL, INTENT(IN) :: seed
        INTEGER(I4B), DIMENSION(size(arr)) :: ans
        INTEGER(I4B) :: n,j,sd
        n=size(arr)
        if (n == 0_i4b) RETURN
        sd=0_i4b
        if (present(seed)) sd=seed
        ans(1)=arr(1)+sd
        if (n < NPAR_CUMSUM) then
                do j=2,n
                   ans(j)=ans(j-1)+arr(j)
                end do
        else
                ans(2:n:2)=cumsum_i(arr(2:n:2)+arr(1:n-1:2),sd)
                ans(3:n:2)=ans(2:n-1:2)+arr(3:n:2)
        end if
        END FUNCTION cumsum_i

        SUBROUTINE nrerror(string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        write (*,*) 'nrerror: ',string
        STOP 'program terminated by nrerror'
        END SUBROUTINE nrerror

        FUNCTION vabs(v)
        REAL(SP), DIMENSION(:), INTENT(IN) :: v
        REAL(SP) :: vabs
        vabs=sqrt(dot_product(v,v))
        END FUNCTION vabs

        SUBROUTINE scatter_add_r(dest,source,dest_index)
        REAL(SP), DIMENSION(:), INTENT(OUT) :: dest
        REAL(SP), DIMENSION(:), INTENT(IN) :: source
        INTEGER(I4B), DIMENSION(:), INTENT(IN) :: dest_index
        INTEGER(I4B) :: m,n,j,i
        n=assert_eq2(size(source),size(dest_index),'scatter_add_r')
        m=size(dest)
        do j=1,n
                i=dest_index(j)
                if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
        end do
        END SUBROUTINE scatter_add_r
        SUBROUTINE scatter_add_d(dest,source,dest_index)
        REAL(DP), DIMENSION(:), INTENT(OUT) :: dest
        REAL(DP), DIMENSION(:), INTENT(IN) :: source
        INTEGER(I4B), DIMENSION(:), INTENT(IN) :: dest_index
        INTEGER(I4B) :: m,n,j,i
        n=assert_eq2(size(source),size(dest_index),'scatter_add_d')
        m=size(dest)
        do j=1,n
                i=dest_index(j)
                if (i > 0 .and. i <= m) dest(i)=dest(i)+source(j)
        end do
        END SUBROUTINE scatter_add_d

        END MODULE nrutil

!--------------------------------------------------------------------------------------------

        FUNCTION savgol(nl,nrr,ld,m)
        USE nrtype; USE nrutil, ONLY : arth,assert,poly
        USE nr, ONLY : lubksb_new,ludcmp_new
        IMPLICIT NONE
        INTEGER(I4B), INTENT(IN) :: nl,nrr,ld,m
        REAL(SP), DIMENSION(nl+nrr+1) :: savgol
        INTEGER(I4B) :: imj,ipj,mm,np
        INTEGER(I4B), DIMENSION(m+1) :: indx
        REAL(SP) :: d,sm
        REAL(SP), DIMENSION(m+1) :: b
        REAL(SP), DIMENSION(m+1,m+1) :: a
        INTEGER(I4B) :: irng(nl+nrr+1)
        call assert(nl >= 0, nrr >= 0, ld <= m, nl+nrr >= m, 'savgol args')
        do ipj=0,2*m
           sm=sum(arth(1.0_sp,1.0_sp,nrr)**ipj)+&
              sum(arth(-1.0_sp,-1.0_sp,nl)**ipj)
           if (ipj == 0) sm=sm+1.0_sp
           mm=min(ipj,2*m-ipj)
           do imj=-mm,mm,2
              a(1+(ipj+imj)/2,1+(ipj-imj)/2)=sm
           end do
        end do
        call ludcmp_new(a(:,:),indx(:),d)
        b(:)=0.0
        b(ld+1)=1.0
        call lubksb_new(a(:,:),indx(:),b(:))
        savgol(:)=0.0
        irng(:)=arth(-nl,1,nrr+nl+1)
        np=nl+nrr+1
        savgol(mod(np-irng(:),np)+1)=poly(real(irng(:),sp),b(:))
        END FUNCTION savgol

        SUBROUTINE lubksb_new(a,indx,b)
        USE nrtype; USE nrutil, ONLY : assert_eq
        IMPLICIT NONE
        REAL(SP), DIMENSION(:,:), INTENT(IN) :: a
        INTEGER(I4B), DIMENSION(:), INTENT(IN) :: indx
        REAL(SP), DIMENSION(:), INTENT(INOUT) :: b
        INTEGER(I4B) :: i,n,ii,ll
        REAL(SP) :: summ
        n=assert_eq(size(a,1),size(a,2),size(indx),'lubksb')
        ii=0
        do i=1,n
                ll=indx(i)
                summ=b(ll)
                b(ll)=b(i)
                if (ii /= 0) then
                        summ=summ-dot_product(a(i,ii:i-1),b(ii:i-1))
                else if (summ /= 0.0) then
                        ii=i
                end if
                b(i)=summ
        end do
        do i=n,1,-1
                b(i) = (b(i)-dot_product(a(i,i+1:n),b(i+1:n)))/a(i,i)
        end do
        END SUBROUTINE lubksb_new

        SUBROUTINE ludcmp_new(a,indx,d,ier)
        USE nrtype; USE nrutil, ONLY : assert_eq,imaxloc,outerprod,swap
        IMPLICIT NONE
        REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: a
        INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: indx
        REAL(SP), INTENT(OUT) :: d
        INTEGER, INTENT(OUT), optional :: ier
        REAL(SP), DIMENSION(size(a,1)) :: vv
        REAL(SP), PARAMETER :: TINY=1.0e-20_sp
        INTEGER(I4B) :: j,n,imax
        if (present(ier)) ier = 0
        n=assert_eq(size(a,1),size(a,2),size(indx),'ludcmp')
        d=1.0
        vv=maxval(abs(a),dim=2)
        if (any(vv == 0.0)) then
            ier = -1           ! Singular matrix
            return
        endif
        vv=1.0_dp/vv
        do j=1,n
                imax=(j-1)+imaxloc(vv(j:n)*abs(a(j:n,j)))
                if (j /= imax) then
                        call swap(a(imax,:),a(j,:))
                        d=-d
                        vv(imax)=vv(j)
                end if
                indx(j)=imax
                if (a(j,j) == 0.0) a(j,j)=TINY
                a(j+1:n,j)=a(j+1:n,j)/a(j,j)
                a(j+1:n,j+1:n)=a(j+1:n,j+1:n)-outerprod(a(j+1:n,j),a(j,j+1:n))
        end do
        END SUBROUTINE ludcmp_new

!--------------------------------------------------------------------------------------------

        FUNCTION chebev(a,b,c,x)
        USE nrtype;             !USE nrutil, ONLY : nrerror
        IMPLICIT NONE
        REAL(SP), INTENT(IN) :: a,b,x
        REAL(SP), DIMENSION(:), INTENT(IN) :: c
        REAL(SP) :: chebev
        INTEGER(I4B) :: j,m
        REAL(SP) :: d,dd,sv,y,y2
      !  if ((x-a)*(x-b) > 0.0) call nrerror('x not in range in chebev_s')
        m=size(c)
        d=0.0
        dd=0.0
        y=(2.0_sp*x-a-b)/(b-a)
        y2=2.0_sp*y
        do j=m,2,-1
                sv=d
                d=y2*d-dd+c(j)
                dd=sv
        end do
        chebev=y*d-dd+0.5_sp*c(1)
        END FUNCTION chebev

        FUNCTION chebft(a,b,n,func)
        USE nrtype; USE nrutil, ONLY : arth,outerprod
        IMPLICIT NONE
        REAL(SP), INTENT(IN) :: a,b
        INTEGER(I4B), INTENT(IN) :: n
        REAL(SP), DIMENSION(n) :: chebft
        INTERFACE
            FUNCTION func(x)
            USE nrtype
            implicit none
            real(sp), dimension(:), intent(in) :: x
            real(sp), dimension(size(x)) :: func
            END FUNCTION func
        END INTERFACE
        REAL(DP) :: bma,bpa
        REAL(DP), DIMENSION(n) :: theta
        bma=0.5_dp*(b-a)
        bpa=0.5_dp*(b+a)
        theta(:)=PI_D*arth(0.5_dp,1.0_dp,n)/n
        chebft(:)=matmul(cos(outerprod(arth(0.0_dp,1.0_dp,n),theta)), &
                func(real(cos(theta)*bma+bpa,sp)))*2.0_dp/n
        END FUNCTION chebft

        FUNCTION chder(a,b,c)
        USE nrtype; USE nrutil, ONLY : arth,cumsum
        IMPLICIT NONE
        REAL(SP), INTENT(IN) :: a,b
        REAL(SP), DIMENSION(:), INTENT(IN) :: c
        REAL(SP), DIMENSION(size(c)) :: chder
        INTEGER(I4B) :: n
        REAL(SP) :: con
        REAL(SP), DIMENSION(size(c)) :: temp
        n=size(c)
        temp(1)=0.0
        temp(2:n)=2.0_sp*arth(n-1,-1,n-1)*c(n:2:-1)
        chder(n:1:-2)=cumsum(temp(1:n:2))
        chder(n-1:1:-2)=cumsum(temp(2:n:2))
        con=2.0_sp/(b-a)
        chder=chder*con
        END FUNCTION chder
!--------------------------------------------------------------------------------------------
        SUBROUTINE indexx_sp(arr,index)
        USE nrtype; USE nrutil, ONLY : arth,assert_eq,nrerror,swap
        IMPLICIT NONE
        REAL(SP), DIMENSION(:), INTENT(IN) :: arr
        INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: index
        INTEGER(I4B), PARAMETER :: NN=15, NSTACK=50
        REAL(SP) :: a
        INTEGER(I4B) :: n,k,i,j,indext,jstack,l,r
        INTEGER(I4B), DIMENSION(NSTACK) :: istack
        n=assert_eq(size(index),size(arr),'indexx_sp')
        index=arth(1,1,n)
        jstack=0
        l=1
        r=n
        do
                if (r-l < NN) then
                        do j=l+1,r
                                indext=index(j)
                                a=arr(indext)
                                do i=j-1,l,-1
                                        if (arr(index(i)) <= a) exit
                                        index(i+1)=index(i)
                                end do
                                index(i+1)=indext
                        end do
                        if (jstack == 0) RETURN
                        r=istack(jstack)
                        l=istack(jstack-1)
                        jstack=jstack-2
                else
                        k=(l+r)/2
                        call swap(index(k),index(l+1))
                        call icomp_xchg(index(l),index(r))
                        call icomp_xchg(index(l+1),index(r))
                        call icomp_xchg(index(l),index(l+1))
                        i=l+1
                        j=r
                        indext=index(l+1)
                        a=arr(indext)
                        do
                                do
                                        i=i+1
                                        if (arr(index(i)) >= a) exit
                                end do
                                do
                                        j=j-1
                                        if (arr(index(j)) <= a) exit
                                end do
                                if (j < i) exit
                                call swap(index(i),index(j))
                        end do
                        index(l+1)=index(j)
                        index(j)=indext
                        jstack=jstack+2
                        if (jstack > NSTACK) call nrerror('indexx: NSTACK too small')
                        if (r-i+1 >= j-l) then
                                istack(jstack)=r
                                istack(jstack-1)=i
                                r=j-1
                        else
                                istack(jstack)=j-1
                                istack(jstack-1)=l
                                l=i
                        end if
                end if
        end do
        CONTAINS

        SUBROUTINE icomp_xchg(i,j)
        INTEGER(I4B), INTENT(INOUT) :: i,j
        INTEGER(I4B) :: swp
        if (arr(j) < arr(i)) then
                swp=i
                i=j
                j=swp
        end if
        END SUBROUTINE icomp_xchg
        END SUBROUTINE indexx_sp

        SUBROUTINE indexx_i4b(iarr,index)
        USE nrtype; USE nrutil, ONLY : arth,assert_eq,nrerror,swap
        IMPLICIT NONE
        INTEGER(I4B), DIMENSION(:), INTENT(IN) :: iarr
        INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: index
        INTEGER(I4B), PARAMETER :: NN=15, NSTACK=50
        INTEGER(I4B) :: a
        INTEGER(I4B) :: n,k,i,j,indext,jstack,l,r
        INTEGER(I4B), DIMENSION(NSTACK) :: istack
        n=assert_eq(size(index),size(iarr),'indexx_sp')
        index=arth(1,1,n)
        jstack=0
        l=1
        r=n
        do
                if (r-l < NN) then
                        do j=l+1,r
                                indext=index(j)
                                a=iarr(indext)
                                do i=j-1,1,-1
                                        if (iarr(index(i)) <= a) exit
                                        index(i+1)=index(i)
                                end do
                                index(i+1)=indext
                        end do
                        if (jstack == 0) RETURN
                        r=istack(jstack)
                        l=istack(jstack-1)
                        jstack=jstack-2
                else
                        k=(l+r)/2
                        call swap(index(k),index(l+1))
                        call icomp_xchg(index(l),index(r))
                        call icomp_xchg(index(l+1),index(r))
                        call icomp_xchg(index(l),index(l+1))
                        i=l+1
                        j=r
                        indext=index(l+1)
                        a=iarr(indext)
                        do
                                do
                                        i=i+1
                                        if (iarr(index(i)) >= a) exit
                                end do
                                do
                                        j=j-1
                                        if (iarr(index(j)) <= a) exit
                                end do
                                if (j < i) exit
                                call swap(index(i),index(j))
                        end do
                        index(l+1)=index(j)
                        index(j)=indext
                        jstack=jstack+2
                        if (jstack > NSTACK) call nrerror('indexx: NSTACK too small')
                        if (r-i+1 >= j-l) then
                                istack(jstack)=r
                                istack(jstack-1)=i
                                r=j-1
                        else
                                istack(jstack)=j-1
                                istack(jstack-1)=l
                                l=i
                        end if
                end if
        end do
        CONTAINS

        SUBROUTINE icomp_xchg(i,j)
        INTEGER(I4B), INTENT(INOUT) :: i,j
        INTEGER(I4B) :: swp
        if (iarr(j) < iarr(i)) then
                swp=i
                i=j
                j=swp
        end if
        END SUBROUTINE icomp_xchg
        END SUBROUTINE indexx_i4b
!--------------------------------------------------------------------------------------------
        FUNCTION dfridr(func,x,h,err)
        USE nrtype; USE nrutil, ONLY : assert,geop,iminloc
        IMPLICIT NONE
        REAL(SP), INTENT(IN) :: x,h
        REAL(SP), INTENT(OUT) :: err
        REAL(SP) :: dfridr
        INTERFACE
                FUNCTION func(x)
                USE nrtype
                IMPLICIT NONE
                REAL(SP), INTENT(IN) :: x
                REAL(SP) :: func
                END FUNCTION func
        END INTERFACE
        INTEGER(I4B),PARAMETER :: NTAB=10
        REAL(SP), PARAMETER :: CON=1.4_sp,CON2=CON*CON,BIG=huge(x),SAFE=2.0
        INTEGER(I4B) :: ierrmin,i,j
        REAL(SP) :: hh
        REAL(SP), DIMENSION(NTAB-1) :: errt,fac
        REAL(SP), DIMENSION(NTAB,NTAB) :: a
        call assert(h /= 0.0, 'dfridr arg')
        hh=h
        a(1,1)=(func(x+hh)-func(x-hh))/(2.0_sp*hh)
        err=BIG
        fac(1:NTAB-1)=geop(CON2,CON2,NTAB-1)
        do i=2,NTAB
                hh=hh/CON
                a(1,i)=(func(x+hh)-func(x-hh))/(2.0_sp*hh)
                do j=2,i
                        a(j,i)=(a(j-1,i)*fac(j-1)-a(j-1,i-1))/(fac(j-1)-1.0_sp)
                end do
                errt(1:i-1)=max(abs(a(2:i,i)-a(1:i-1,i)),abs(a(2:i,i)-a(1:i-1,i-1)))
                ierrmin=iminloc(errt(1:i-1))
                if (errt(ierrmin) <= err) then
                        err=errt(ierrmin)
                        dfridr=a(1+ierrmin,i)
                end if
                if (abs(a(i,i)-a(i-1,i-1)) >= SAFE*err) RETURN
        end do
        END FUNCTION dfridr

      !--------------------------------------------------------------------------------------------

        SUBROUTINE choldc(a,p,ier)
        USE nrtype; USE nrutil, ONLY : assert_eq,nrerror
        IMPLICIT NONE
        REAL(DP), DIMENSION(:,:), INTENT(INOUT) :: a
        REAL(DP), DIMENSION(:), INTENT(OUT) :: p
        INTEGER(I4B) :: i,n
        REAL(DP) :: summ
        integer, intent(out) :: ier
        ier = 0
        n=assert_eq(size(a,1),size(a,2),size(p),'choldc')
        do i=1,n
                summ=a(i,i)-dot_product(a(i,1:i-1),a(i,1:i-1))
                if (summ <= 0.0) then
                    ier = -1
                    return
                endif
                p(i)=sqrt(summ)
                a(i+1:n,i)=(a(i,i+1:n)-matmul(a(i+1:n,1:i-1),a(i,1:i-1)))/p(i)
        end do
        END SUBROUTINE choldc

      !--------------------------------------------------------------------------------------------

        SUBROUTINE cholsl(a,p,b,x)
        USE nrtype; USE nrutil, ONLY : assert_eq
        IMPLICIT NONE
        REAL(DP), DIMENSION(:,:), INTENT(IN) :: a
        REAL(DP), DIMENSION(:), INTENT(IN) :: p,b
        REAL(DP), DIMENSION(:), INTENT(INOUT) :: x
        INTEGER(I4B) :: i,n
        n=assert_eq((/size(a,1),size(a,2),size(p),size(b),size(x)/),'cholsl')
        do i=1,n
                x(i)=(b(i)-dot_product(a(i,1:i-1),x(1:i-1)))/p(i)
        end do
        do i=n,1,-1
                x(i)=(x(i)-dot_product(a(i+1:n,i),x(i+1:n)))/p(i)
        end do
        END SUBROUTINE cholsl

!corr ! original code with problem on ifort compilers > 14
!corr  SUBROUTINE sprsin_sp(a,thresh,sa)
!corr  USE nrtype; USE nrutil, ONLY : arth,assert_eq
!corr  IMPLICIT NONE
!corr  REAL(SP), DIMENSION(:,:), INTENT(IN) :: a
!corr  REAL(SP), INTENT(IN) :: thresh
!corr  TYPE(sprs2_sp), INTENT(OUT) :: sa
!corr  INTEGER(I4B) :: n,len
!corr  LOGICAL(LGT), DIMENSION(size(a,1),size(a,2)) :: mask
!corr  n=assert_eq(size(a,1),size(a,2),'sprsin_sp')
!corr  mask=abs(a)>thresh
!corr  len=count(mask)
!corr  allocate(sa%val(len),sa%irow(len),sa%jcol(len))
!corr  sa%n=n
!corr  sa%len=len
!corr  sa%val=pack(a,mask)
!corr  sa%irow=pack(spread(arth(1,1,n),2,n),mask)
!corr  sa%jcol=pack(spread(arth(1,1,n),1,n),mask)
!corr  END SUBROUTINE sprsin_sp
!corr 
!corr  SUBROUTINE sprsin_dp(a,thresh,sa)
!corr  USE nrtype; USE nrutil, ONLY : arth,assert_eq
!corr  IMPLICIT NONE
!corr  REAL(DP), DIMENSION(:,:), INTENT(IN) :: a
!corr  REAL(DP), INTENT(IN) :: thresh
!corr  TYPE(sprs2_dp), INTENT(OUT) :: sa
!corr  INTEGER(I4B) :: n,len
!corr  LOGICAL(LGT), DIMENSION(size(a,1),size(a,2)) :: mask
!corr  n=assert_eq(size(a,1),size(a,2),'sprsin_dp')
!corr  mask=abs(a)>thresh
!corr  len=count(mask)
!corr  allocate(sa%val(len),sa%irow(len),sa%jcol(len))
!corr  sa%n=n
!corr  sa%len=len
!corr  sa%val=pack(a,mask)
!corr  sa%irow=pack(spread(arth(1,1,n),2,n),mask)
!corr  sa%jcol=pack(spread(arth(1,1,n),1,n),mask)
!corr  END SUBROUTINE sprsin_dp
!corr 
      !--------------------------------------------------------------------------------------------
        SUBROUTINE sprsin_sp(a,thresh,sa)
        USE nrtype; USE nrutil, ONLY : arth,assert_eq
        IMPLICIT NONE
        REAL(SP), DIMENSION(:,:), INTENT(IN) :: a
        REAL(SP), INTENT(IN) :: thresh
        TYPE(sprs2_sp), INTENT(OUT) :: sa
        INTEGER(I4B) :: n,len
        LOGICAL(LGT), DIMENSION(size(a,1),size(a,2)) :: mask
        integer(i4b), dimension(:), allocatable :: temp
        n=assert_eq(size(a,1),size(a,2),'sprsin_sp')
        mask=abs(a)>thresh
        len=count(mask)
        allocate(sa%val(len),sa%irow(len),sa%jcol(len))
        allocate(temp(len))
        sa%n=n
        sa%len=len
        sa%val=pack(a,mask)
        temp=pack(spread(arth(1,1,n),2,n),mask)
        sa%irow=temp
        temp=pack(spread(arth(1,1,n),1,n),mask) 
        sa%jcol=temp
        !sa%jcol=pack(spread(arth(1,1,n),1,n),mask) - error on ifort
        END SUBROUTINE sprsin_sp

        SUBROUTINE sprsin_dp(a,thresh,sa)
        USE nrtype; USE nrutil, ONLY : arth,assert_eq
        IMPLICIT NONE
        REAL(DP), DIMENSION(:,:), INTENT(IN) :: a
        REAL(DP), INTENT(IN) :: thresh
        TYPE(sprs2_dp), INTENT(OUT) :: sa
        INTEGER(I4B) :: n,len
        integer(i4b), dimension(:), allocatable :: temp
        LOGICAL(LGT), DIMENSION(size(a,1),size(a,2)) :: mask
        n=assert_eq(size(a,1),size(a,2),'sprsin_dp')
        mask=abs(a)>thresh
        len=count(mask)
        allocate(sa%val(len),sa%irow(len),sa%jcol(len))
        allocate(temp(len))
        sa%n=n
        sa%len=len
        sa%val=pack(a,mask)
        temp=pack(spread(arth(1,1,n),2,n),mask)
        sa%irow=temp
        temp=pack(spread(arth(1,1,n),1,n),mask)
        sa%jcol=temp
        !sa%jcol=pack(spread(arth(1,1,n),1,n),mask) - error on ifort
        END SUBROUTINE sprsin_dp
      !--------------------------------------------------------------------------------------------

        MODULE xlinbcg_data
        USE nrtype
        TYPE(sprs2_dp) :: sa
        END MODULE xlinbcg_data

      !--------------------------------------------------------------------------------------------

        SUBROUTINE asolve(b,x,itrnsp,err)
        USE nrtype; USE nrutil, ONLY : assert_eq,nrerror
        USE nr, ONLY : sprsdiag
        USE xlinbcg_data
        REAL(DP), DIMENSION(:), INTENT(IN) :: b
        REAL(DP), DIMENSION(:), INTENT(OUT) :: x
        INTEGER(I4B), INTENT(IN) :: itrnsp
        INTEGER(I4B) :: ndum
        integer, intent(out)      :: err
        err = 0
        ndum=assert_eq(size(b),size(x),'asolve')
        call sprsdiag(sa,x)
        if (any(x == 0.0)) then
            !call nrerror('asolve: singular diagonal matrix')
             err = -1
             return
        endif
        x=b/x
        END SUBROUTINE asolve

      !--------------------------------------------------------------------------------------------

        SUBROUTINE atimes(x,r,itrnsp)
        USE nrtype; USE nrutil, ONLY : assert_eq
        USE nr, ONLY : sprsax,sprstx
        USE xlinbcg_data
        REAL(DP), DIMENSION(:), INTENT(IN) :: x
        REAL(DP), DIMENSION(:), INTENT(OUT) :: r
        INTEGER(I4B), INTENT(IN) :: itrnsp
        INTEGER(I4B) :: n
        n=assert_eq(size(x),size(r),'atimes')
        if (itrnsp == 0) then
            call sprsax(sa,x,r)
        else
            call sprstx(sa,x,r)
        end if
        END SUBROUTINE atimes
      !--------------------------------------------------------------------------------------------

        FUNCTION snrm(sx,itol)
        USE nrtype
        IMPLICIT NONE
        REAL(DP), DIMENSION(:), INTENT(IN) :: sx
        INTEGER(I4B), INTENT(IN) :: itol
        REAL(DP) :: snrm
        if (itol <= 3) then
            snrm=sqrt(dot_product(sx,sx))
        else
            snrm=maxval(abs(sx))
        end if
        END FUNCTION snrm

      !--------------------------------------------------------------------------------------------

        SUBROUTINE sprsax_sp(sa,x,b)
        USE nrtype; USE nrutil, ONLY : assert_eq,scatter_add
        IMPLICIT NONE
        TYPE(sprs2_sp), INTENT(IN) :: sa
        REAL(SP), DIMENSION (:), INTENT(IN) :: x
        REAL(SP), DIMENSION (:), INTENT(OUT) :: b
        INTEGER(I4B) :: ndum
        ndum=assert_eq(sa%n,size(x),size(b),'sprsax_sp')
        b=0.0_sp
        call scatter_add(b,sa%val*x(sa%jcol),sa%irow)
        END SUBROUTINE sprsax_sp

        SUBROUTINE sprsax_dp(sa,x,b)
        USE nrtype; USE nrutil, ONLY : assert_eq,scatter_add
        IMPLICIT NONE
        TYPE(sprs2_dp), INTENT(IN) :: sa
        REAL(DP), DIMENSION (:), INTENT(IN) :: x
        REAL(DP), DIMENSION (:), INTENT(OUT) :: b
        INTEGER(I4B) :: ndum
        ndum=assert_eq(sa%n,size(x),size(b),'sprsax_dp')
        b=0.0_dp
        call scatter_add(b,sa%val*x(sa%jcol),sa%irow)
        END SUBROUTINE sprsax_dp

      !--------------------------------------------------------------------------------------------

        SUBROUTINE sprstx_sp(sa,x,b)
        USE nrtype; USE nrutil, ONLY : assert_eq,scatter_add
        IMPLICIT NONE
        TYPE(sprs2_sp), INTENT(IN) :: sa
        REAL(SP), DIMENSION (:), INTENT(IN) :: x
        REAL(SP), DIMENSION (:), INTENT(OUT) :: b
        INTEGER(I4B) :: ndum
        ndum=assert_eq(sa%n,size(x),size(b),'sprstx_sp')
        b=0.0_sp
        call scatter_add(b,sa%val*x(sa%irow),sa%jcol)
        END SUBROUTINE sprstx_sp

        SUBROUTINE sprstx_dp(sa,x,b)
        USE nrtype; USE nrutil, ONLY : assert_eq,scatter_add
        IMPLICIT NONE
        TYPE(sprs2_dp), INTENT(IN) :: sa
        REAL(DP), DIMENSION (:), INTENT(IN) :: x
        REAL(DP), DIMENSION (:), INTENT(OUT) :: b
        INTEGER(I4B) :: ndum
        ndum=assert_eq(sa%n,size(x),size(b),'sprstx_dp')
        b=0.0_dp
        call scatter_add(b,sa%val*x(sa%irow),sa%jcol)
        END SUBROUTINE sprstx_dp

      !--------------------------------------------------------------------------------------------

        SUBROUTINE sprsdiag_sp(sa,b)
        USE nrtype; USE nrutil, ONLY : array_copy,assert_eq
        IMPLICIT NONE
        TYPE(sprs2_sp), INTENT(IN) :: sa
        REAL(SP), DIMENSION(:), INTENT(OUT) :: b
        REAL(SP), DIMENSION(size(b)) :: val
        INTEGER(I4B) :: k,l,ndum,nerr
        INTEGER(I4B), DIMENSION(size(b)) :: i
        LOGICAL(LGT), DIMENSION(:), ALLOCATABLE :: mask
        ndum=assert_eq(sa%n,size(b),'sprsdiag_sp')
        l=sa%len
        allocate(mask(l))
        mask = (sa%irow(1:l) == sa%jcol(1:l))
        call array_copy(pack(sa%val(1:l),mask),val,k,nerr)
        i(1:k)=pack(sa%irow(1:l),mask)
        deallocate(mask)
        b=0.0
        b(i(1:k))=val(1:k)
        END SUBROUTINE sprsdiag_sp

        SUBROUTINE sprsdiag_dp(sa,b)
        USE nrtype; USE nrutil, ONLY : array_copy,assert_eq
        IMPLICIT NONE
        TYPE(sprs2_dp), INTENT(IN) :: sa
        REAL(DP), DIMENSION(:), INTENT(OUT) :: b
        REAL(DP), DIMENSION(size(b)) :: val
        INTEGER(I4B) :: k,l,ndum,nerr
        INTEGER(I4B), DIMENSION(size(b)) :: i
        LOGICAL(LGT), DIMENSION(:), ALLOCATABLE :: mask
        ndum=assert_eq(sa%n,size(b),'sprsdiag_dp')
        l=sa%len
        allocate(mask(l))
        mask = (sa%irow(1:l) == sa%jcol(1:l))
        call array_copy(pack(sa%val(1:l),mask),val,k,nerr)
        i(1:k)=pack(sa%irow(1:l),mask)
        deallocate(mask)
        b=0.0
        b(i(1:k))=val(1:k)
        END SUBROUTINE sprsdiag_dp

      !--------------------------------------------------------------------------------------------

        SUBROUTINE linbcg(b,x,itol,tol,itmax,iter,err,errd)
        USE nrtype; USE nrutil, ONLY : assert_eq,nrerror
        USE nr, ONLY : atimes,asolve,snrm
        IMPLICIT NONE
        REAL(DP), DIMENSION(:), INTENT(IN) :: b
        REAL(DP), DIMENSION(:), INTENT(INOUT) :: x
        INTEGER(I4B), INTENT(IN) :: itol,itmax
        REAL(DP), INTENT(IN) :: tol
        INTEGER(I4B), INTENT(OUT) :: iter
        REAL(DP), INTENT(OUT) :: err
        REAL(DP), PARAMETER :: EPS=1.0e-14_dp
        INTEGER(I4B) :: n
        REAL(DP) :: ak,akden,bk,bkden,bknum,bnrm,dxnrm,xnrm,zm1nrm,znrm
        REAL(DP), DIMENSION(size(b)) :: p,pp,r,rr,z,zz
        integer, intent(out)         :: errd
        n=assert_eq(size(b),size(x),'linbcg')
        errd = 0
        iter=0
        call atimes(x,r,0)
        r=b-r
        rr=r
!       call atimes(r,rr,0)
        select case(itol)
                case(1)
                        bnrm=snrm(b,itol)
                        call asolve(r,z,0,errd)
                        if (errd < 0) return
                case(2)
                        call asolve(b,z,0,errd)
                        if (errd < 0) return
                        bnrm=snrm(z,itol)
                        call asolve(r,z,0,errd)
                        if (errd < 0) return
                case(3:4)
                        call asolve(b,z,0,errd)
                        if (errd > 0) return
                        bnrm=snrm(z,itol)
                        call asolve(r,z,0,errd)
                        if (errd < 0) return
                        znrm=snrm(z,itol)
                case default
                        call nrerror('illegal itol in linbcg')
        end select
        do
                if (iter > itmax) exit
                iter=iter+1
                call asolve(rr,zz,1,errd)
                if (errd < 0) return
                bknum=dot_product(z,rr)
                if (iter == 1) then
                        p=z
                        pp=zz
                else
                        bk=bknum/bkden
                        p=bk*p+z
                        pp=bk*pp+zz
                end if
                bkden=bknum
                call atimes(p,z,0)
                akden=dot_product(z,pp)
                ak=bknum/akden
                call atimes(pp,zz,1)
                x=x+ak*p
                r=r-ak*z
                rr=rr-ak*zz
                call asolve(r,z,0,errd)
                if (errd < 0) return
                select case(itol)
                        case(1)
                                err=snrm(r,itol)/bnrm
                        case(2)
                                err=snrm(z,itol)/bnrm
                        case(3:4)
                                zm1nrm=znrm
                                znrm=snrm(z,itol)
                                if (abs(zm1nrm-znrm) > EPS*znrm) then
                                        dxnrm=abs(ak)*snrm(p,itol)
                                        err=znrm/abs(zm1nrm-znrm)*dxnrm
                                else
                                        err=znrm/bnrm
                                        cycle
                                end if
                                xnrm=snrm(x,itol)
                                if (err <= 0.5_dp*xnrm) then
                                        err=err/xnrm
                                else
                                        err=znrm/bnrm
                                        cycle
                                end if
                end select
                !write (*,*) ' iter=',iter,' err=',err
                if (err <= tol) exit
        end do
        END SUBROUTINE linbcg
  !--------------------------------------------------------------------------------------------

    SUBROUTINE lfit(x,y,sig,a,maska,covar,chisq,funcs)
    USE nrtype; USE nrutil, ONLY : assert_eq,diagmult,nrerror
    USE nr, ONLY :covsrt,gaussj
    IMPLICIT NONE
    REAL(SP), DIMENSION(:), INTENT(IN) :: x,y,sig
    REAL(SP), DIMENSION(:), INTENT(INOUT) :: a
    LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: maska
    REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: covar
    REAL(SP), INTENT(OUT) :: chisq
    INTERFACE
        SUBROUTINE funcs(x,arr)
        USE nrtype
        IMPLICIT NONE
        REAL(SP),INTENT(IN) :: x
        REAL(SP), DIMENSION(:), INTENT(OUT) :: arr
        END SUBROUTINE funcs
    END INTERFACE
    INTEGER(I4B) :: i,j,k,l,ma,mfit,n
    REAL(SP) :: sig2i,wt,ym
    REAL(SP), DIMENSION(size(maska)) :: afunc
    REAL(SP), DIMENSION(size(maska),1) :: beta
    n=assert_eq(size(x),size(y),size(sig),'lfit: n')
    ma=assert_eq(size(maska),size(a),size(covar,1),size(covar,2),'lfit: ma')
    mfit=count(maska)
    if (mfit == 0) call nrerror('lfit: no parameters to be fitted')
    covar(1:mfit,1:mfit)=0.0
    beta(1:mfit,1)=0.0
    do i=1,n
        call funcs(x(i),afunc)
        ym=y(i)
        if (mfit < ma) ym=ym-sum(a(1:ma)*afunc(1:ma), mask=.not. maska)
        sig2i=1.0_sp/sig(i)**2
        j=0
        do l=1,ma
            if (maska(l)) then
                j=j+1
                wt=afunc(l)*sig2i
                k=count(maska(1:l))
                covar(j,1:k)=covar(j,1:k)+wt*pack(afunc(1:l),maska(1:l))
                beta(j,1)=beta(j,1)+ym*wt
            end if
        end do
    end do
    call diagmult(covar(1:mfit,1:mfit),0.5_sp)
    covar(1:mfit,1:mfit)= &
        covar(1:mfit,1:mfit)+transpose(covar(1:mfit,1:mfit))
    call gaussj(covar(1:mfit,1:mfit),beta(1:mfit,1:1))
    a(1:ma)=unpack(beta(1:ma,1),maska,a(1:ma))
    chisq=0.0
    do i=1,n
        call funcs(x(i),afunc)
        chisq=chisq+((y(i)-dot_product(a(1:ma),afunc(1:ma)))/sig(i))**2
    end do
    call covsrt(covar,maska)
    END SUBROUTINE lfit

  !--------------------------------------------------------------------------------------------

    SUBROUTINE covsrt(covar,maska)
    USE nrtype; USE nrutil, ONLY : assert_eq,swap
    IMPLICIT NONE
    REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: covar
    LOGICAL(LGT), DIMENSION(:), INTENT(IN) :: maska
    INTEGER(I4B) :: ma,mfit,j,k
    ma=assert_eq(size(covar,1),size(covar,2),size(maska),'covsrt')
    mfit=count(maska)
    covar(mfit+1:ma,1:ma)=0.0
    covar(1:ma,mfit+1:ma)=0.0
    k=mfit
    do j=ma,1,-1
        if (maska(j)) then
            call swap(covar(1:ma,k),covar(1:ma,j))
            call swap(covar(k,1:ma),covar(j,1:ma))
            k=k-1
        end if
    end do
    END SUBROUTINE covsrt

 !--------------------------------------------------------------------------------------------

    SUBROUTINE svbksb_sp(u,w,v,b,x)
    USE nrtype; USE nrutil, ONLY : assert_eq
    REAL(SP), DIMENSION(:,:), INTENT(IN) :: u,v
    REAL(SP), DIMENSION(:), INTENT(IN) :: w,b
    REAL(SP), DIMENSION(:), INTENT(OUT) :: x
    INTEGER(I4B) :: mdum,ndum
    REAL(SP), DIMENSION(size(x)) :: tmp
    mdum=assert_eq(size(u,1),size(b),'svbksb_sp: mdum')
    ndum=assert_eq((/size(u,2),size(v,1),size(v,2),size(w),size(x)/),&
        'svbksb_sp: ndum')
    where (w /= 0.0)
        tmp=matmul(b,u)/w
    elsewhere
        tmp=0.0
    end where
    x=matmul(v,tmp)
    END SUBROUTINE svbksb_sp

    SUBROUTINE svbksb_dp(u,w,v,b,x)
    USE nrtype; USE nrutil, ONLY : assert_eq
    REAL(DP), DIMENSION(:,:), INTENT(IN) :: u,v
    REAL(DP), DIMENSION(:), INTENT(IN) :: w,b
    REAL(DP), DIMENSION(:), INTENT(OUT) :: x
    INTEGER(I4B) :: mdum,ndum
    REAL(DP), DIMENSION(size(x)) :: tmp
    mdum=assert_eq(size(u,1),size(b),'svbksb_dp: mdum')
    ndum=assert_eq((/size(u,2),size(v,1),size(v,2),size(w),size(x)/),&
        'svbksb_dp: ndum')
    where (w /= 0.0)
        tmp=matmul(b,u)/w
    elsewhere
        tmp=0.0
    end where
    x=matmul(v,tmp)
    END SUBROUTINE svbksb_dp

 !--------------------------------------------------------------------------------------------

    SUBROUTINE svdcmp_sp(a,w,v)
    USE nrtype; USE nrutil, ONLY : assert_eq,nrerror,outerprod
    USE nr, ONLY : pythag
    IMPLICIT NONE
    REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: a
    REAL(SP), DIMENSION(:), INTENT(OUT) :: w
    REAL(SP), DIMENSION(:,:), INTENT(OUT) :: v
    INTEGER(I4B) :: i,its,j,k,l,m,n,nm
    REAL(SP) :: anorm,c,f,g,h,s,scale,x,y,z
    REAL(SP), DIMENSION(size(a,1)) :: tempm
    REAL(SP), DIMENSION(size(a,2)) :: rv1,tempn
    m=size(a,1)
    n=assert_eq(size(a,2),size(v,1),size(v,2),size(w),'svdcmp_sp')
    g=0.0
    scale=0.0
    do i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0
        scale=0.0
        if (i <= m) then
            scale=sum(abs(a(i:m,i)))
            if (scale /= 0.0) then
                a(i:m,i)=a(i:m,i)/scale
                s=dot_product(a(i:m,i),a(i:m,i))
                f=a(i,i)
                g=-sign(sqrt(s),f)
                h=f*g-s
                a(i,i)=f-g
                tempn(l:n)=matmul(a(i:m,i),a(i:m,l:n))/h
                a(i:m,l:n)=a(i:m,l:n)+outerprod(a(i:m,i),tempn(l:n))
                a(i:m,i)=scale*a(i:m,i)
            end if
        end if
        w(i)=scale*g
        g=0.0
        scale=0.0
        if ((i <= m) .and. (i /= n)) then
            scale=sum(abs(a(i,l:n)))
            if (scale /= 0.0) then
                a(i,l:n)=a(i,l:n)/scale
                s=dot_product(a(i,l:n),a(i,l:n))
                f=a(i,l)
                g=-sign(sqrt(s),f)
                h=f*g-s
                a(i,l)=f-g
                rv1(l:n)=a(i,l:n)/h
                tempm(l:m)=matmul(a(l:m,l:n),a(i,l:n))
                a(l:m,l:n)=a(l:m,l:n)+outerprod(tempm(l:m),rv1(l:n))
                a(i,l:n)=scale*a(i,l:n)
            end if
        end if
    end do
    anorm=maxval(abs(w)+abs(rv1))
    do i=n,1,-1
        if (i < n) then
            if (g /= 0.0) then
                v(l:n,i)=(a(i,l:n)/a(i,l))/g
                tempn(l:n)=matmul(a(i,l:n),v(l:n,l:n))
                v(l:n,l:n)=v(l:n,l:n)+outerprod(v(l:n,i),tempn(l:n))
            end if
            v(i,l:n)=0.0
            v(l:n,i)=0.0
        end if
        v(i,i)=1.0
        g=rv1(i)
        l=i
    end do
    do i=min(m,n),1,-1
        l=i+1
        g=w(i)
        a(i,l:n)=0.0
        if (g /= 0.0) then
            g=1.0_sp/g
            tempn(l:n)=(matmul(a(l:m,i),a(l:m,l:n))/a(i,i))*g
            a(i:m,l:n)=a(i:m,l:n)+outerprod(a(i:m,i),tempn(l:n))
            a(i:m,i)=a(i:m,i)*g
        else
            a(i:m,i)=0.0
        end if
        a(i,i)=a(i,i)+1.0_sp
    end do
    do k=n,1,-1
        do its=1,30
            do l=k,1,-1
                nm=l-1
                if ((abs(rv1(l))+anorm) == anorm) exit
                if ((abs(w(nm))+anorm) == anorm) then
                    c=0.0
                    s=1.0
                    do i=l,k
                        f=s*rv1(i)
                        rv1(i)=c*rv1(i)
                        if ((abs(f)+anorm) == anorm) exit
                        g=w(i)
                        h=pythag(f,g)
                        w(i)=h
                        h=1.0_sp/h
                        c= (g*h)
                        s=-(f*h)
                        tempm(1:m)=a(1:m,nm)
                        a(1:m,nm)=a(1:m,nm)*c+a(1:m,i)*s
                        a(1:m,i)=-tempm(1:m)*s+a(1:m,i)*c
                    end do
                    exit
                end if
            end do
            z=w(k)
            if (l == k) then
                if (z < 0.0) then
                    w(k)=-z
                    v(1:n,k)=-v(1:n,k)
                end if
                exit
            end if
            if (its == 30) call nrerror('svdcmp_sp: no convergence in svdcmp')
            x=w(l)
            nm=k-1
            y=w(nm)
            g=rv1(nm)
            h=rv1(k)
            f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0_sp*h*y)
            g=pythag(f,1.0_sp)
            f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
            c=1.0
            s=1.0
            do j=l,nm
                i=j+1
                g=rv1(i)
                y=w(i)
                h=s*g
                g=c*g
                z=pythag(f,h)
                rv1(j)=z
                c=f/z
                s=h/z
                f= (x*c)+(g*s)
                g=-(x*s)+(g*c)
                h=y*s
                y=y*c
                tempn(1:n)=v(1:n,j)
                v(1:n,j)=v(1:n,j)*c+v(1:n,i)*s
                v(1:n,i)=-tempn(1:n)*s+v(1:n,i)*c
                z=pythag(f,h)
                w(j)=z
                if (z /= 0.0) then
                    z=1.0_sp/z
                    c=f*z
                    s=h*z
                end if
                f= (c*g)+(s*y)
                x=-(s*g)+(c*y)
                tempm(1:m)=a(1:m,j)
                a(1:m,j)=a(1:m,j)*c+a(1:m,i)*s
                a(1:m,i)=-tempm(1:m)*s+a(1:m,i)*c
            end do
            rv1(l)=0.0
            rv1(k)=f
            w(k)=x
        end do
    end do
    END SUBROUTINE svdcmp_sp

    SUBROUTINE svdcmp_dp(a,w,v)
    USE nrtype; USE nrutil, ONLY : assert_eq,nrerror,outerprod
    USE nr, ONLY : pythag
    IMPLICIT NONE
    REAL(DP), DIMENSION(:,:), INTENT(INOUT) :: a
    REAL(DP), DIMENSION(:), INTENT(OUT) :: w
    REAL(DP), DIMENSION(:,:), INTENT(OUT) :: v
    INTEGER(I4B) :: i,its,j,k,l,m,n,nm
    REAL(DP) :: anorm,c,f,g,h,s,scale,x,y,z
    REAL(DP), DIMENSION(size(a,1)) :: tempm
    REAL(DP), DIMENSION(size(a,2)) :: rv1,tempn
    m=size(a,1)
    n=assert_eq(size(a,2),size(v,1),size(v,2),size(w),'svdcmp_dp')
    g=0.0
    scale=0.0
    do i=1,n
        l=i+1
        rv1(i)=scale*g
        g=0.0
        scale=0.0
        if (i <= m) then
            scale=sum(abs(a(i:m,i)))
            if (scale /= 0.0) then
                a(i:m,i)=a(i:m,i)/scale
                s=dot_product(a(i:m,i),a(i:m,i))
                f=a(i,i)
                g=-sign(sqrt(s),f)
                h=f*g-s
                a(i,i)=f-g
                tempn(l:n)=matmul(a(i:m,i),a(i:m,l:n))/h
                a(i:m,l:n)=a(i:m,l:n)+outerprod(a(i:m,i),tempn(l:n))
                a(i:m,i)=scale*a(i:m,i)
            end if
        end if
        w(i)=scale*g
        g=0.0
        scale=0.0
        if ((i <= m) .and. (i /= n)) then
            scale=sum(abs(a(i,l:n)))
            if (scale /= 0.0) then
                a(i,l:n)=a(i,l:n)/scale
                s=dot_product(a(i,l:n),a(i,l:n))
                f=a(i,l)
                g=-sign(sqrt(s),f)
                h=f*g-s
                a(i,l)=f-g
                rv1(l:n)=a(i,l:n)/h
                tempm(l:m)=matmul(a(l:m,l:n),a(i,l:n))
                a(l:m,l:n)=a(l:m,l:n)+outerprod(tempm(l:m),rv1(l:n))
                a(i,l:n)=scale*a(i,l:n)
            end if
        end if
    end do
    anorm=maxval(abs(w)+abs(rv1))
    do i=n,1,-1
        if (i < n) then
            if (g /= 0.0) then
                v(l:n,i)=(a(i,l:n)/a(i,l))/g
                tempn(l:n)=matmul(a(i,l:n),v(l:n,l:n))
                v(l:n,l:n)=v(l:n,l:n)+outerprod(v(l:n,i),tempn(l:n))
            end if
            v(i,l:n)=0.0
            v(l:n,i)=0.0
        end if
        v(i,i)=1.0
        g=rv1(i)
        l=i
    end do
    do i=min(m,n),1,-1
        l=i+1
        g=w(i)
        a(i,l:n)=0.0
        if (g /= 0.0) then
            g=1.0_dp/g
            tempn(l:n)=(matmul(a(l:m,i),a(l:m,l:n))/a(i,i))*g
            a(i:m,l:n)=a(i:m,l:n)+outerprod(a(i:m,i),tempn(l:n))
            a(i:m,i)=a(i:m,i)*g
        else
            a(i:m,i)=0.0
        end if
        a(i,i)=a(i,i)+1.0_dp
    end do
    do k=n,1,-1
        do its=1,30
            do l=k,1,-1
                nm=l-1
                if ((abs(rv1(l))+anorm) == anorm) exit
                if ((abs(w(nm))+anorm) == anorm) then
                    c=0.0
                    s=1.0
                    do i=l,k
                        f=s*rv1(i)
                        rv1(i)=c*rv1(i)
                        if ((abs(f)+anorm) == anorm) exit
                        g=w(i)
                        h=pythag(f,g)
                        w(i)=h
                        h=1.0_dp/h
                        c= (g*h)
                        s=-(f*h)
                        tempm(1:m)=a(1:m,nm)
                        a(1:m,nm)=a(1:m,nm)*c+a(1:m,i)*s
                        a(1:m,i)=-tempm(1:m)*s+a(1:m,i)*c
                    end do
                    exit
                end if
            end do
            z=w(k)
            if (l == k) then
                if (z < 0.0) then
                    w(k)=-z
                    v(1:n,k)=-v(1:n,k)
                end if
                exit
            end if
            if (its == 30) call nrerror('svdcmp_dp: no convergence in svdcmp')
            x=w(l)
            nm=k-1
            y=w(nm)
            g=rv1(nm)
            h=rv1(k)
            f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0_dp*h*y)
            g=pythag(f,1.0_dp)
            f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
            c=1.0
            s=1.0
            do j=l,nm
                i=j+1
                g=rv1(i)
                y=w(i)
                h=s*g
                g=c*g
                z=pythag(f,h)
                rv1(j)=z
                c=f/z
                s=h/z
                f= (x*c)+(g*s)
                g=-(x*s)+(g*c)
                h=y*s
                y=y*c
                tempn(1:n)=v(1:n,j)
                v(1:n,j)=v(1:n,j)*c+v(1:n,i)*s
                v(1:n,i)=-tempn(1:n)*s+v(1:n,i)*c
                z=pythag(f,h)
                w(j)=z
                if (z /= 0.0) then
                    z=1.0_dp/z
                    c=f*z
                    s=h*z
                end if
                f= (c*g)+(s*y)
                x=-(s*g)+(c*y)
                tempm(1:m)=a(1:m,j)
                a(1:m,j)=a(1:m,j)*c+a(1:m,i)*s
                a(1:m,i)=-tempm(1:m)*s+a(1:m,i)*c
            end do
            rv1(l)=0.0
            rv1(k)=f
            w(k)=x
        end do
    end do
    END SUBROUTINE svdcmp_dp

 !--------------------------------------------------------------------------------------------

    FUNCTION pythag_sp(a,b)
    USE nrtype
    IMPLICIT NONE
    REAL(SP), INTENT(IN) :: a,b
    REAL(SP) :: pythag_sp
    REAL(SP) :: absa,absb
    absa=abs(a)
    absb=abs(b)
    if (absa > absb) then
        pythag_sp=absa*sqrt(1.0_sp+(absb/absa)**2)
    else
        if (absb == 0.0) then
            pythag_sp=0.0
        else
            pythag_sp=absb*sqrt(1.0_sp+(absa/absb)**2)
        end if
    end if
    END FUNCTION pythag_sp

    FUNCTION pythag_dp(a,b)
    USE nrtype
    IMPLICIT NONE
    REAL(DP), INTENT(IN) :: a,b
    REAL(DP) :: pythag_dp
    REAL(DP) :: absa,absb
    absa=abs(a)
    absb=abs(b)
    if (absa > absb) then
        pythag_dp=absa*sqrt(1.0_dp+(absb/absa)**2)
    else
        if (absb == 0.0) then
            pythag_dp=0.0
        else
            pythag_dp=absb*sqrt(1.0_dp+(absa/absb)**2)
        end if
    end if
    END FUNCTION pythag_dp

 !--------------------------------------------------------------------------------------------

    SUBROUTINE svdfit(x,y,sig,a,v,w,chisq,funcs)
    USE nrtype; USE nrutil, ONLY : assert_eq,vabs
    USE nr, ONLY : svbksb,svdcmp
    IMPLICIT NONE
    REAL(SP), DIMENSION(:), INTENT(IN) :: x,y,sig
    REAL(SP), DIMENSION(:), INTENT(OUT) :: a,w
    REAL(SP), DIMENSION(:,:), INTENT(OUT) :: v
    REAL(SP), INTENT(OUT) :: chisq
    INTERFACE
        FUNCTION funcs(x,n)
        USE nrtype
        IMPLICIT NONE
        REAL(SP), INTENT(IN) :: x
        INTEGER(I4B), INTENT(IN) :: n
        REAL(SP), DIMENSION(n) :: funcs
        END FUNCTION funcs
    END INTERFACE
    REAL(SP), PARAMETER :: TOL=1.0e-5_sp
    INTEGER(I4B) :: i,ma,n
    REAL(SP), DIMENSION(size(x)) :: b,sigi
    REAL(SP), DIMENSION(size(x),size(a)) :: u,usav
    n=assert_eq(size(x),size(y),size(sig),'svdfit: n')
    ma=assert_eq(size(a),size(v,1),size(v,2),size(w),'svdfit: ma')
    sigi=1.0_sp/sig
    b=y*sigi
    do i=1,n
        usav(i,:)=funcs(x(i),ma)
    end do
    u=usav*spread(sigi,dim=2,ncopies=ma)
    usav=u
    call svdcmp(u,w,v)
    where (w < TOL*maxval(w)) w=0.0
    call svbksb(u,w,v,b,a)
    chisq=vabs(matmul(usav,a)-b)**2
    END SUBROUTINE svdfit

 !--------------------------------------------------------------------------------------------

    FUNCTION fpoly(x,n)
    USE nrtype; USE nrutil, ONLY : geop
    IMPLICIT NONE
    REAL(SP), INTENT(IN) :: x
    INTEGER(I4B), INTENT(IN) :: n
    REAL(SP), DIMENSION(n) :: fpoly
    fpoly=geop(1.0_sp,x,n)
    END FUNCTION fpoly

  !--------------------------------------------------------------------------------------------

    SUBROUTINE gaussj(a,b)
    USE nrtype; USE nrutil, ONLY : assert_eq,nrerror,outerand,outerprod,swap
    IMPLICIT NONE
    REAL(SP), DIMENSION(:,:), INTENT(INOUT) :: a,b
    INTEGER(I4B), DIMENSION(size(a,1)) :: ipiv,indxr,indxc
    LOGICAL(LGT), DIMENSION(size(a,1)) :: lpiv
    REAL(SP) :: pivinv
    REAL(SP), DIMENSION(size(a,1)) :: dumc
    INTEGER(I4B), TARGET :: irc(2)
    INTEGER(I4B) :: i,l,n
    INTEGER(I4B), POINTER :: irow,icol
    n=assert_eq(size(a,1),size(a,2),size(b,1),'gaussj')
    irow => irc(1)
    icol => irc(2)
    ipiv=0
    do i=1,n
        lpiv = (ipiv == 0)
        irc=maxloc(abs(a),outerand(lpiv,lpiv))
        ipiv(icol)=ipiv(icol)+1
        if (ipiv(icol) > 1) call nrerror('gaussj: singular matrix (1)')
        if (irow /= icol) then
            call swap(a(irow,:),a(icol,:))
            call swap(b(irow,:),b(icol,:))
        end if
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol) == 0.0) &
            call nrerror('gaussj: singular matrix (2)')
        pivinv=1.0_sp/a(icol,icol)
        a(icol,icol)=1.0
        a(icol,:)=a(icol,:)*pivinv
        b(icol,:)=b(icol,:)*pivinv
        dumc=a(:,icol)
        a(:,icol)=0.0
        a(icol,icol)=pivinv
        a(1:icol-1,:)=a(1:icol-1,:)-outerprod(dumc(1:icol-1),a(icol,:))
        b(1:icol-1,:)=b(1:icol-1,:)-outerprod(dumc(1:icol-1),b(icol,:))
        a(icol+1:,:)=a(icol+1:,:)-outerprod(dumc(icol+1:),a(icol,:))
        b(icol+1:,:)=b(icol+1:,:)-outerprod(dumc(icol+1:),b(icol,:))
    end do
    do l=n,1,-1
        call swap(a(:,indxr(l)),a(:,indxc(l)))
    end do
    END SUBROUTINE gaussj

      !--------------------------------------------------------------------------------------------

        FUNCTION gammln_s(xx)
        USE nrtype; USE nrutil, ONLY : arth,assert
        IMPLICIT NONE
        REAL(SP), INTENT(IN) :: xx
        REAL(SP) :: gammln_s
        REAL(DP) :: tmp,x
        REAL(DP) :: stp = 2.5066282746310005_dp
        REAL(DP), DIMENSION(6) :: coef = (/76.18009172947146_dp,&
                -86.50532032941677_dp,24.01409824083091_dp,&
                -1.231739572450155_dp,0.1208650973866179e-2_dp,&
                -0.5395239384953e-5_dp/)
             write(77,*)'xx=',xx
        call assert(xx > 0.0, 'gammln_s arg')
        x=xx
        tmp=x+5.5_dp
        tmp=(x+0.5_dp)*log(tmp)-tmp
        gammln_s=tmp+log(stp*(1.000000000190015_dp+&
                sum(coef(:)/arth(x+1.0_dp,1.0_dp,size(coef))))/x)
        END FUNCTION gammln_s

      !--------------------------------------------------------------------------------------------

        FUNCTION gammln_v(xx)
        USE nrtype; USE nrutil, ONLY: assert
        IMPLICIT NONE
        INTEGER(I4B) :: i
        REAL(SP), DIMENSION(:), INTENT(IN) :: xx
        REAL(SP), DIMENSION(size(xx)) :: gammln_v
        REAL(DP), DIMENSION(size(xx)) :: ser,tmp,x,y
        REAL(DP) :: stp = 2.5066282746310005_dp
        REAL(DP), DIMENSION(6) :: coef = (/76.18009172947146_dp,&
                -86.50532032941677_dp,24.01409824083091_dp,&
                -1.231739572450155_dp,0.1208650973866179e-2_dp,&
                -0.5395239384953e-5_dp/)
        if (size(xx) == 0) RETURN
        call assert(all(xx > 0.0), 'gammln_v arg')
        x=xx
        tmp=x+5.5_dp
        tmp=(x+0.5_dp)*log(tmp)-tmp
        ser=1.000000000190015_dp
        y=x
        do i=1,size(coef)
                y=y+1.0_dp
                ser=ser+coef(i)/y
        end do
        gammln_v=tmp+log(stp*ser/x)
        END FUNCTION gammln_v

	FUNCTION gcf_s(a,x,gln)
	USE nrtype; USE nrutil, ONLY : nrerror
	USE nr, ONLY : gammln
	IMPLICIT NONE
	REAL(SP), INTENT(IN) :: a,x
	REAL(SP), OPTIONAL, INTENT(OUT) :: gln
	REAL(SP) :: gcf_s
	INTEGER(I4B), PARAMETER :: ITMAX=100
	REAL(SP), PARAMETER :: EPS=epsilon(x),FPMIN=tiny(x)/EPS
	INTEGER(I4B) :: i
	REAL(SP) :: an,b,c,d,del,h
	if (x == 0.0) then
		gcf_s=1.0
		RETURN
	end if
	b=x+1.0_sp-a
	c=1.0_sp/FPMIN
	d=1.0_sp/b
	h=d
	do i=1,ITMAX
		an=-i*(i-a)
		b=b+2.0_sp
		d=an*d+b
		if (abs(d) < FPMIN) d=FPMIN
		c=b+an/c
		if (abs(c) < FPMIN) c=FPMIN
		d=1.0_sp/d
		del=d*c
		h=h*del
		if (abs(del-1.0_sp) <= EPS) exit
	end do
	if (i > ITMAX) call nrerror('a too large, ITMAX too small in gcf_s')
	if (present(gln)) then
		gln=gammln(a)
		gcf_s=exp(-x+a*log(x)-gln)*h
	else
		gcf_s=exp(-x+a*log(x)-gammln(a))*h
	end if
	END FUNCTION gcf_s

	FUNCTION gcf_v(a,x,gln)
	USE nrtype; USE nrutil, ONLY : assert_eq,nrerror
	USE nr, ONLY : gammln
	IMPLICIT NONE
	REAL(SP), DIMENSION(:), INTENT(IN) :: a,x
	REAL(SP), DIMENSION(:), OPTIONAL, INTENT(OUT) :: gln
	REAL(SP), DIMENSION(size(a)) :: gcf_v
	INTEGER(I4B), PARAMETER :: ITMAX=100
	REAL(SP), PARAMETER :: EPS=epsilon(x),FPMIN=tiny(x)/EPS
	INTEGER(I4B) :: i
	REAL(SP), DIMENSION(size(a)) :: an,b,c,d,del,h
	LOGICAL(LGT), DIMENSION(size(a)) :: converged,zero
	i=assert_eq(size(a),size(x),'gcf_v')
	zero=(x == 0.0)
	where (zero)
		gcf_v=1.0
	elsewhere
		b=x+1.0_sp-a
		c=1.0_sp/FPMIN
		d=1.0_sp/b
		h=d
	end where
	converged=zero
	do i=1,ITMAX
		where (.not. converged)
			an=-i*(i-a)
			b=b+2.0_sp
			d=an*d+b
			d=merge(FPMIN,d, abs(d)<FPMIN )
			c=b+an/c
			c=merge(FPMIN,c, abs(c)<FPMIN )
			d=1.0_sp/d
			del=d*c
			h=h*del
			converged = (abs(del-1.0_sp)<=EPS)
		end where
		if (all(converged)) exit
	end do
	if (i > ITMAX) call nrerror('a too large, ITMAX too small in gcf_v')
	if (present(gln)) then
		if (size(gln) < size(a)) call &
			nrerror('gser: Not enough space for gln')
		gln=gammln(a)
		where (.not. zero) gcf_v=exp(-x+a*log(x)-gln)*h
	else
		where (.not. zero) gcf_v=exp(-x+a*log(x)-gammln(a))*h
	end if
	END FUNCTION gcf_v

	FUNCTION gser_s(a,x,gln)
	USE nrtype; USE nrutil, ONLY : nrerror
	USE nr, ONLY : gammln
	IMPLICIT NONE
	REAL(SP), INTENT(IN) :: a,x
	REAL(SP), OPTIONAL, INTENT(OUT) :: gln
	REAL(SP) :: gser_s
	INTEGER(I4B), PARAMETER :: ITMAX=100
	REAL(SP), PARAMETER :: EPS=epsilon(x)
	INTEGER(I4B) :: n
	REAL(SP) :: ap,del,summ
	if (x == 0.0) then
		gser_s=0.0
		RETURN
	end if
	ap=a
	summ=1.0_sp/a
	del=summ
	do n=1,ITMAX
		ap=ap+1.0_sp
		del=del*x/ap
		summ=summ+del
		if (abs(del) < abs(summ)*EPS) exit
	end do
	if (n > ITMAX) call nrerror('a too large, ITMAX too small in gser_s')
	if (present(gln)) then
		gln=gammln(a)
		gser_s=summ*exp(-x+a*log(x)-gln)
	else
		gser_s=summ*exp(-x+a*log(x)-gammln(a))
	end if
	END FUNCTION gser_s


	FUNCTION gser_v(a,x,gln)
	USE nrtype; USE nrutil, ONLY : assert_eq,nrerror
	USE nr, ONLY : gammln
	IMPLICIT NONE
	REAL(SP), DIMENSION(:), INTENT(IN) :: a,x
	REAL(SP), DIMENSION(:), OPTIONAL, INTENT(OUT) :: gln
	REAL(SP), DIMENSION(size(a)) :: gser_v
	INTEGER(I4B), PARAMETER :: ITMAX=100
	REAL(SP), PARAMETER :: EPS=epsilon(x)
	INTEGER(I4B) :: n
	REAL(SP), DIMENSION(size(a)) :: ap,del,summ
	LOGICAL(LGT), DIMENSION(size(a)) :: converged,zero
	n=assert_eq(size(a),size(x),'gser_v')
	zero=(x == 0.0)
	where (zero) gser_v=0.0
	ap=a
	summ=1.0_sp/a
	del=summ
	converged=zero
	do n=1,ITMAX
		where (.not. converged)
			ap=ap+1.0_sp
			del=del*x/ap
			summ=summ+del
			converged = (abs(del) < abs(summ)*EPS)
		end where
		if (all(converged)) exit
	end do
	if (n > ITMAX) call nrerror('a too large, ITMAX too small in gser_v')
	if (present(gln)) then
		if (size(gln) < size(a)) call &
			nrerror('gser: Not enough space for gln')
		gln=gammln(a)
		where (.not. zero) gser_v=summ*exp(-x+a*log(x)-gln)
	else
		where (.not. zero) gser_v=summ*exp(-x+a*log(x)-gammln(a))
	end if
	END FUNCTION gser_v

	FUNCTION gammq_s(a,x)
	USE nrtype; USE nrutil, ONLY : assert
	USE nr, ONLY : gcf,gser
	IMPLICIT NONE
	REAL(SP), INTENT(IN) :: a,x
	REAL(SP) :: gammq_s
	call assert( x >= 0.0,  a > 0.0, 'gammq_s args')
	if (x<a+1.0_sp) then
		gammq_s=1.0_sp-gser(a,x)
	else
		gammq_s=gcf(a,x)
	end if
	END FUNCTION gammq_s


	FUNCTION gammq_v(a,x)
	USE nrtype; USE nrutil, ONLY : assert,assert_eq
	USE nr, ONLY : gcf,gser
	IMPLICIT NONE
	REAL(SP), DIMENSION(:), INTENT(IN) :: a,x
	REAL(SP), DIMENSION(size(a)) :: gammq_v
	LOGICAL(LGT), DIMENSION(size(x)) :: mask
	INTEGER(I4B) :: ndum
	ndum=assert_eq(size(a),size(x),'gammq_v')
	call assert( all(x >= 0.0),  all(a > 0.0), 'gammq_v args')
	mask = (x<a+1.0_sp)
	gammq_v=merge(1.0_sp-gser(a,merge(x,0.0_sp,mask)), &
		gcf(a,merge(x,0.0_sp,.not. mask)),mask)
	END FUNCTION gammq_v

 SUBROUTINE sortnr(arr)
 USE nrtype; USE nrutil, ONLY : swap,nrerror
 IMPLICIT NONE
 REAL(SP), DIMENSION(:), INTENT(INOUT) :: arr
 INTEGER(I4B), PARAMETER :: NN=15, NSTACK=50
 REAL(SP) :: a
 INTEGER(I4B) :: n,k,i,j,jstack,l,r
 INTEGER(I4B), DIMENSION(NSTACK) :: istack
 n=size(arr)
 jstack=0
 l=1
 r=n
 do
  if (r-l < NN) then
   do j=l+1,r
    a=arr(j)
    do i=j-1,l,-1
     if (arr(i) <= a) exit
     arr(i+1)=arr(i)
    end do
    arr(i+1)=a
   end do
   if (jstack == 0) RETURN
   r=istack(jstack)
   l=istack(jstack-1)
   jstack=jstack-2
  else
   k=(l+r)/2
   call swap(arr(k),arr(l+1))
   call swap(arr(l),arr(r),arr(l)>arr(r))
   call swap(arr(l+1),arr(r),arr(l+1)>arr(r))
   call swap(arr(l),arr(l+1),arr(l)>arr(l+1))
   i=l+1
   j=r
   a=arr(l+1)
   do
    do
     i=i+1
     if (arr(i) >= a) exit
    end do
    do
     j=j-1
     if (arr(j) <= a) exit
    end do
    if (j < i) exit
    call swap(arr(i),arr(j))
   end do
   arr(l+1)=arr(j)
   arr(j)=a
   jstack=jstack+2
   if (jstack > NSTACK) call nrerror('sort: NSTACK too small')
   if (r-i+1 >= j-l) then
    istack(jstack)=r
    istack(jstack-1)=i
    r=j-1
   else
    istack(jstack)=j-1
    istack(jstack-1)=l
    l=i
   end if
  end if
 end do
 END SUBROUTINE sortnr

   SUBROUTINE fourrow_sp(data,isign)
   USE nrtype; USE nrutil, ONLY : assert,swap
   IMPLICIT NONE
   COMPLEX(SPC), DIMENSION(:,:), INTENT(INOUT) :: data
   INTEGER(I4B), INTENT(IN) :: isign
   INTEGER(I4B) :: n,i,istep,j,m,mmax,n2
   REAL(DP) :: theta
   COMPLEX(SPC), DIMENSION(size(data,1)) :: temp
   COMPLEX(DPC) :: w,wp
   COMPLEX(SPC) :: ws
   n=size(data,2)
   call assert(iand(n,n-1)==0, 'n must be a power of 2 in fourrow_sp')
   n2=n/2
   j=n2
   do i=1,n-2
        if (j > i) call swap(data(:,j+1),data(:,i+1))
        m=n2
        do
                if (m < 2 .or. j < m) exit
                j=j-m
                m=m/2
        end do
        j=j+m
   end do
   mmax=1
   do
        if (n <= mmax) exit
        istep=2*mmax
        theta=PI_D/(isign*mmax)
        wp=cmplx(-2.0_dp*sin(0.5_dp*theta)**2,sin(theta),kind=dpc)
        w=cmplx(1.0_dp,0.0_dp,kind=dpc)
        do m=1,mmax
                ws=w
                do i=m,n,istep
                        j=i+mmax
                        temp=ws*data(:,j)
                        data(:,j)=data(:,i)-temp
                        data(:,i)=data(:,i)+temp
                end do
                w=w*wp+w
        end do
        mmax=istep
   end do
   END SUBROUTINE fourrow_sp
   
   SUBROUTINE fourrow_dp(data,isign)
   USE nrtype; USE nrutil, ONLY : assert,swap
   IMPLICIT NONE
   COMPLEX(DPC), DIMENSION(:,:), INTENT(INOUT) :: data
   INTEGER(I4B), INTENT(IN) :: isign
   INTEGER(I4B) :: n,i,istep,j,m,mmax,n2
   REAL(DP) :: theta
   COMPLEX(DPC), DIMENSION(size(data,1)) :: temp
   COMPLEX(DPC) :: w,wp
   COMPLEX(DPC) :: ws
   n=size(data,2)
   call assert(iand(n,n-1)==0, 'n must be a power of 2 in fourrow_dp')
   n2=n/2
   j=n2
   do i=1,n-2
        if (j > i) call swap(data(:,j+1),data(:,i+1))
        m=n2
        do
                if (m < 2 .or. j < m) exit
                j=j-m
                m=m/2
        end do
        j=j+m
   end do
   mmax=1
   do
        if (n <= mmax) exit
        istep=2*mmax
        theta=PI_D/(isign*mmax)
        wp=cmplx(-2.0_dp*sin(0.5_dp*theta)**2,sin(theta),kind=dpc)
        w=cmplx(1.0_dp,0.0_dp,kind=dpc)
        do m=1,mmax
             ws=w
             do i=m,n,istep
                   j=i+mmax
                   temp=ws*data(:,j)
                   data(:,j)=data(:,i)-temp
                   data(:,i)=data(:,i)+temp
             end do
             w=w*wp+w
        end do
        mmax=istep
   end do
   END SUBROUTINE fourrow_dp

   SUBROUTINE four1_sp(data,isign)
   USE nrtype; USE nrutil, ONLY : arth,assert
   USE nr, ONLY : fourrow
   IMPLICIT NONE
   COMPLEX(SPC), DIMENSION(:), INTENT(INOUT) :: data
   INTEGER(I4B), INTENT(IN) :: isign
   COMPLEX(SPC), DIMENSION(:,:), ALLOCATABLE :: dat,temp
   COMPLEX(DPC), DIMENSION(:), ALLOCATABLE :: w,wp
   REAL(DP), DIMENSION(:), ALLOCATABLE :: theta
   INTEGER(I4B) :: n,m1,m2,j
   n=size(data)
   call assert(iand(n,n-1)==0, 'n must be a power of 2 in four1_sp')
   m1=2**ceiling(0.5_sp*log(real(n,sp))/0.693147_sp)
   m2=n/m1
   allocate(dat(m1,m2),theta(m1),w(m1),wp(m1),temp(m2,m1))
   dat=reshape(data,shape(dat))
   call fourrow(dat,isign)
   theta=arth(0,isign,m1)*TWOPI_D/n
   wp=cmplx(-2.0_dp*sin(0.5_dp*theta)**2,sin(theta),kind=dpc)
   w=cmplx(1.0_dp,0.0_dp,kind=dpc)
   do j=2,m2
        w=w*wp+w
        dat(:,j)=dat(:,j)*w
   end do
   temp=transpose(dat)
   call fourrow(temp,isign)
   data=reshape(temp,shape(data))
   deallocate(dat,w,wp,theta,temp)
   END SUBROUTINE four1_sp
   
   SUBROUTINE four1_dp(data,isign)
   USE nrtype; USE nrutil, ONLY : arth,assert
   USE nr, ONLY : fourrow
   IMPLICIT NONE
   COMPLEX(DPC), DIMENSION(:), INTENT(INOUT) :: data
   INTEGER(I4B), INTENT(IN) :: isign
   COMPLEX(DPC), DIMENSION(:,:), ALLOCATABLE :: dat,temp
   COMPLEX(DPC), DIMENSION(:), ALLOCATABLE :: w,wp
   REAL(DP), DIMENSION(:), ALLOCATABLE :: theta
   INTEGER(I4B) :: n,m1,m2,j
   n=size(data)
   call assert(iand(n,n-1)==0, 'n must be a power of 2 in four1_dp')
   m1=2**ceiling(0.5_sp*log(real(n,sp))/0.693147_sp)
   m2=n/m1
   allocate(dat(m1,m2),theta(m1),w(m1),wp(m1),temp(m2,m1))
   dat=reshape(data,shape(dat))
   call fourrow(dat,isign)
   theta=arth(0,isign,m1)*TWOPI_D/n
   wp=cmplx(-2.0_dp*sin(0.5_dp*theta)**2,sin(theta),kind=dpc)
   w=cmplx(1.0_dp,0.0_dp,kind=dpc)
   do j=2,m2
        w=w*wp+w
        dat(:,j)=dat(:,j)*w
   end do
   temp=transpose(dat)
   call fourrow(temp,isign)
   data=reshape(temp,shape(data))
   deallocate(dat,w,wp,theta,temp)
   END SUBROUTINE four1_dp

   SUBROUTINE realft_sp(data,isign,zdata)
   USE nrtype; USE nrutil, ONLY : assert,assert_eq,zroots_unity
   USE nr, ONLY : four1
   IMPLICIT NONE
   REAL(SP), DIMENSION(:), INTENT(INOUT) :: data
   INTEGER(I4B), INTENT(IN) :: isign
   COMPLEX(SPC), DIMENSION(:), OPTIONAL, TARGET :: zdata
   INTEGER(I4B) :: n,ndum,nh,nq
   COMPLEX(SPC), DIMENSION(size(data)/4) :: w
   COMPLEX(SPC), DIMENSION(size(data)/4-1) :: h1,h2
   COMPLEX(SPC), DIMENSION(:), POINTER :: cdata
   COMPLEX(SPC) :: z
   REAL(SP) :: c1=0.5_sp,c2
   n=size(data)
   call assert(iand(n,n-1)==0, 'n must be a power of 2 in realft_sp')
   nh=n/2
   nq=n/4
   if (present(zdata)) then
     ndum=assert_eq(n/2,size(zdata),'realft_sp')
     cdata=>zdata
     if (isign == 1) cdata=cmplx(data(1:n-1:2),data(2:n:2),kind=spc)
   else
     allocate(cdata(n/2))
     cdata=cmplx(data(1:n-1:2),data(2:n:2),kind=spc)
   end if
   if (isign == 1) then
     c2=-0.5_sp
     call four1(cdata,+1)
   else
     c2=0.5_sp
   end if
   w=zroots_unity(sign(n,isign),n/4)
   w=cmplx(-aimag(w),real(w),kind=spc)
   h1=c1*(cdata(2:nq)+conjg(cdata(nh:nq+2:-1)))
   h2=c2*(cdata(2:nq)-conjg(cdata(nh:nq+2:-1)))
   cdata(2:nq)=h1+w(2:nq)*h2
   cdata(nh:nq+2:-1)=conjg(h1-w(2:nq)*h2)
   z=cdata(1)
   if (isign == 1) then
     cdata(1)=cmplx(real(z)+aimag(z),real(z)-aimag(z),kind=spc)
   else
     cdata(1)=cmplx(c1*(real(z)+aimag(z)),c1*(real(z)-aimag(z)),kind=spc)
     call four1(cdata,-1)
   end if
   if (present(zdata)) then
     if (isign /= 1) then
       data(1:n-1:2)=real(cdata)
       data(2:n:2)=aimag(cdata)
     end if
   else
     data(1:n-1:2)=real(cdata)
     data(2:n:2)=aimag(cdata)
     deallocate(cdata)
   end if
   END SUBROUTINE realft_sp
   
   
   SUBROUTINE realft_dp(data,isign,zdata)
   USE nrtype; USE nrutil, ONLY : assert,assert_eq,zroots_unity
   USE nr, ONLY : four1
   IMPLICIT NONE
   REAL(DP), DIMENSION(:), INTENT(INOUT) :: data
   INTEGER(I4B), INTENT(IN) :: isign
   COMPLEX(DPC), DIMENSION(:), OPTIONAL, TARGET :: zdata
   INTEGER(I4B) :: n,ndum,nh,nq
   COMPLEX(DPC), DIMENSION(size(data)/4) :: w
   COMPLEX(DPC), DIMENSION(size(data)/4-1) :: h1,h2
   COMPLEX(DPC), DIMENSION(:), POINTER :: cdata
   COMPLEX(DPC) :: z
   REAL(DP) :: c1=0.5_dp,c2
   n=size(data)
   call assert(iand(n,n-1)==0, 'n must be a power of 2 in realft_dp')
   nh=n/2
   nq=n/4
   if (present(zdata)) then
     ndum=assert_eq(n/2,size(zdata),'realft_dp')
     cdata=>zdata
     if (isign == 1) cdata=cmplx(data(1:n-1:2),data(2:n:2),kind=spc)
   else
     allocate(cdata(n/2))
     cdata=cmplx(data(1:n-1:2),data(2:n:2),kind=spc)
   end if
   if (isign == 1) then
     c2=-0.5_dp
     call four1(cdata,+1)
   else
     c2=0.5_dp
   end if
   w=zroots_unity(sign(n,isign),n/4)
   w=cmplx(-aimag(w),real(w),kind=dpc)
   h1=c1*(cdata(2:nq)+conjg(cdata(nh:nq+2:-1)))
   h2=c2*(cdata(2:nq)-conjg(cdata(nh:nq+2:-1)))
   cdata(2:nq)=h1+w(2:nq)*h2
   cdata(nh:nq+2:-1)=conjg(h1-w(2:nq)*h2)
   z=cdata(1)
   if (isign == 1) then
     cdata(1)=cmplx(real(z)+aimag(z),real(z)-aimag(z),kind=dpc)
   else
     cdata(1)=cmplx(c1*(real(z)+aimag(z)),c1*(real(z)-aimag(z)),kind=dpc)
     call four1(cdata,-1)
   end if
   if (present(zdata)) then
     if (isign /= 1) then
       data(1:n-1:2)=real(cdata)
       data(2:n:2)=aimag(cdata)
     end if
   else
     data(1:n-1:2)=real(cdata)
     data(2:n:2)=aimag(cdata)
     deallocate(cdata)
   end if
   END SUBROUTINE realft_dp

   FUNCTION convlv(data,respns,isign)
   USE nrtype; USE nrutil, ONLY : assert,nrerror
   USE nr, ONLY : realft
   IMPLICIT NONE
   REAL(SP), DIMENSION(:), INTENT(INOUT) :: data
   REAL(SP), DIMENSION(:), INTENT(IN) :: respns
   INTEGER(I4B), INTENT(IN) :: isign
   REAL(SP), DIMENSION(size(data)) :: convlv
   INTEGER(I4B) :: no2,n,m
   COMPLEX(SPC), DIMENSION(size(data)/2) :: tmpd,tmpr
   n=size(data)
   m=size(respns)
   call assert(iand(n,n-1)==0, 'n must be a power of 2 in convlv')
   call assert(mod(m,2)==1, 'm must be odd in convlv')
   convlv(1:m)=respns(:)
   convlv(n-(m-3)/2:n)=convlv((m+3)/2:m)
   convlv((m+3)/2:n-(m-1)/2)=0.0
   no2=n/2
   call realft(data,1,tmpd)
   call realft(convlv,1,tmpr)
   if (isign == 1) then
     tmpr(1)=cmplx(real(tmpd(1))*real(tmpr(1))/no2, &
       aimag(tmpd(1))*aimag(tmpr(1))/no2, kind=spc)
     tmpr(2:)=tmpd(2:)*tmpr(2:)/no2
   else if (isign == -1) then
     if (any(abs(tmpr(2:)) == 0.0) .or. real(tmpr(1)) == 0.0 &
       .or. aimag(tmpr(1)) == 0.0) call nrerror &
       ('deconvolving at response zero in convlv')
     tmpr(1)=cmplx(real(tmpd(1))/real(tmpr(1))/no2, &
       aimag(tmpd(1))/aimag(tmpr(1))/no2, kind=spc)
     tmpr(2:)=tmpd(2:)/tmpr(2:)/no2
   else
     call nrerror('no meaning for isign in convlv')
   end if
   call realft(convlv,-1,tmpr)
   END FUNCTION convlv

   SUBROUTINE polint(xa,ya,x,y,dy)
   USE nrtype; USE nrutil, ONLY : assert_eq,iminloc,nrerror
   IMPLICIT NONE
   REAL(SP), DIMENSION(:), INTENT(IN) :: xa,ya
   REAL(SP), INTENT(IN) :: x
   REAL(SP), INTENT(OUT) :: y,dy
   INTEGER(I4B) :: m,n,ns
   REAL(SP), DIMENSION(size(xa)) :: c,d,den,ho
   n=assert_eq(size(xa),size(ya),'polint')
   c=ya
   d=ya
   ho=xa-x
   ns=iminloc(abs(x-xa))
   y=ya(ns)
   ns=ns-1
   do m=1,n-1
      den(1:n-m)=ho(1:n-m)-ho(1+m:n)
      if (any(den(1:n-m) == 0.0)) &
       call nrerror('polint: calculation failure')
      den(1:n-m)=(c(2:n-m+1)-d(1:n-m))/den(1:n-m)
      d(1:n-m)=ho(1+m:n)*den(1:n-m)
      c(1:n-m)=ho(1:n-m)*den(1:n-m)
      if (2*ns < n-m) then
       dy=c(ns+1)
      else
       dy=d(ns)
       ns=ns-1
      end if
      y=y+dy
   end do
   END SUBROUTINE polint

   SUBROUTINE spline(x,y,yp1,ypn,y2)
   USE nrtype; USE nrutil, ONLY : assert_eq
   USE nr, ONLY : tridag
   IMPLICIT NONE
   REAL(SP), DIMENSION(:), INTENT(IN) :: x,y
   REAL(SP), INTENT(IN) :: yp1,ypn
   REAL(SP), DIMENSION(:), INTENT(OUT) :: y2
   INTEGER(I4B) :: n
   REAL(SP), DIMENSION(size(x)) :: a,b,c,r
   n=assert_eq(size(x),size(y),size(y2),'spline')
   c(1:n-1)=x(2:n)-x(1:n-1)
   r(1:n-1)=6.0_sp*((y(2:n)-y(1:n-1))/c(1:n-1))
   r(2:n-1)=r(2:n-1)-r(1:n-2)
   a(2:n-1)=c(1:n-2)
   b(2:n-1)=2.0_sp*(c(2:n-1)+a(2:n-1))
   b(1)=1.0
   b(n)=1.0
   if (yp1 > 0.99e30_sp) then
     r(1)=0.0
     c(1)=0.0
   else
     r(1)=(3.0_sp/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
     c(1)=0.5
   end if
   if (ypn > 0.99e30_sp) then
     r(n)=0.0
     a(n)=0.0
   else
     r(n)=(-3.0_sp/(x(n)-x(n-1)))*((y(n)-y(n-1))/(x(n)-x(n-1))-ypn)
     a(n)=0.5
   end if
   call tridag(a(2:n),b(1:n),c(1:n-1),r(1:n),y2(1:n))
   END SUBROUTINE spline

   FUNCTION splint(xa,ya,y2a,x)
   USE nrtype; USE nrutil, ONLY : assert_eq,nrerror
   USE nr, ONLY: locate
   IMPLICIT NONE
   REAL(SP), DIMENSION(:), INTENT(IN) :: xa,ya,y2a
   REAL(SP), INTENT(IN) :: x
   REAL(SP) :: splint
   INTEGER(I4B) :: khi,klo,n
   REAL(SP) :: a,b,h
   n=assert_eq(size(xa),size(ya),size(y2a),'splint')
   klo=max(min(locate(xa,x),n-1),1)
   khi=klo+1
   h=xa(khi)-xa(klo)
   if (h == 0.0) then
    call nrerror('bad xa input in splint')
   endif
   a=(xa(khi)-x)/h
   b=(x-xa(klo))/h
   splint=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.0_sp
   END FUNCTION splint

   FUNCTION locate(xx,x)
   USE nrtype
   IMPLICIT NONE
   REAL(SP), DIMENSION(:), INTENT(IN) :: xx
   REAL(SP), INTENT(IN) :: x
   INTEGER(I4B) :: locate
   INTEGER(I4B) :: n,jl,jm,ju
   LOGICAL :: ascnd
   n=size(xx)
   ascnd = (xx(n) >= xx(1))
   jl=0
   ju=n+1
   do
     if (ju-jl <= 1) exit
     jm=(ju+jl)/2
     if (ascnd .eqv. (x >= xx(jm))) then
         jl=jm
     else
         ju=jm
     end if
   end do
   if (x == xx(1)) then
       locate=1
   else if (x == xx(n)) then
       locate=n-1
   else
       locate=jl
   end if
   END FUNCTION locate

   SUBROUTINE tridag_ser(a,b,c,r,u)
   USE nrtype; USE nrutil, ONLY : assert_eq,nrerror
   IMPLICIT NONE
   REAL(SP), DIMENSION(:), INTENT(IN) :: a,b,c,r
   REAL(SP), DIMENSION(:), INTENT(OUT) :: u
   REAL(SP), DIMENSION(size(b)) :: gam
   INTEGER(I4B) :: n,j
   REAL(SP) :: bet
   n=assert_eq((/size(a)+1,size(b),size(c)+1,size(r),size(u)/),'tridag_ser')
   bet=b(1)
   if (bet == 0.0) call nrerror('tridag_ser: Error at code stage 1')
   u(1)=r(1)/bet
   do j=2,n
      gam(j)=c(j-1)/bet
      bet=b(j)-a(j-1)*gam(j)
      if (bet == 0.0) &
          call nrerror('tridag_ser: Error at code stage 2')
      u(j)=(r(j)-a(j-1)*u(j-1))/bet
   end do
   do j=n-1,1,-1
      u(j)=u(j)-gam(j+1)*u(j+1)
   end do
   END SUBROUTINE tridag_ser

	SUBROUTINE fit(x,y,a,b,siga,sigb,chi2,q,sig)
	USE nrtype; USE nrutil, ONLY : assert_eq
	USE nr, ONLY : gammq
	IMPLICIT NONE
	REAL(SP), DIMENSION(:), INTENT(IN) :: x,y
	REAL(SP), INTENT(OUT) :: a,b,siga,sigb,chi2,q
	REAL(SP), DIMENSION(:), OPTIONAL, INTENT(IN) :: sig
	INTEGER(I4B) :: ndata
	REAL(SP) :: sigdat,ss,sx,sxoss,sy,st2
	REAL(SP), DIMENSION(size(x)), TARGET :: t
	REAL(SP), DIMENSION(:), POINTER :: wt
	if (present(sig)) then
		ndata=assert_eq(size(x),size(y),size(sig),'fit')
		wt=>t
		wt(:)=1.0_sp/(sig(:)**2)
		ss=sum(wt(:))
		sx=dot_product(wt,x)
		sy=dot_product(wt,y)
	else
		ndata=assert_eq(size(x),size(y),'fit')
		ss=real(size(x),sp)
		sx=sum(x)
		sy=sum(y)
	end if
	sxoss=sx/ss
	t(:)=x(:)-sxoss
	if (present(sig)) then
		t(:)=t(:)/sig(:)
		b=dot_product(t/sig,y)
	else
		b=dot_product(t,y)
	end if
	st2=dot_product(t,t)
	b=b/st2
	a=(sy-sx*b)/ss
	siga=sqrt((1.0_sp+sx*sx/(ss*st2))/ss)
	sigb=sqrt(1.0_sp/st2)
	t(:)=y(:)-a-b*x(:)
	q=1.0
	if (present(sig)) then
		t(:)=t(:)/sig(:)
		chi2=dot_product(t,t)
		if (ndata > 2) q=gammq(0.5_sp*(size(x)-2),0.5_sp*chi2)
	else
		chi2=dot_product(t,t)
		sigdat=sqrt(chi2/(size(x)-2))
		siga=siga*sigdat
		sigb=sigb*sigdat
	end if
	END SUBROUTINE fit

Module m_mrgrnk
Integer, Parameter :: kdp = selected_real_kind(15)
public :: mrgrnk
private :: kdp
private :: R_mrgrnk, I_mrgrnk, D_mrgrnk
interface mrgrnk
  module procedure D_mrgrnk, R_mrgrnk, I_mrgrnk
end interface mrgrnk
contains

Subroutine D_mrgrnk (XDONT, IRNGT)
! __________________________________________________________
!   MRGRNK = Merge-sort ranking of an array
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      Real (kind=kdp), Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
      Real (kind=kdp) :: XVALA, XVALB
!
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XDONT), SIZE(IRNGT))
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XDONT(IIND-1) <= XDONT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 2) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B. This line may be activated when the
!   initial set is already close to sorted.
!
!          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
!
!  One steps in the C subset, that we build in the final rank array
!
!  Make a copy of the rank array for the merge iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
!
            XVALA = XDONT (JWRKT(IINDA))
            XVALB = XDONT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XDONT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XDONT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
      Return
!
End Subroutine D_mrgrnk

Subroutine R_mrgrnk (XDONT, IRNGT)
! __________________________________________________________
!   MRGRNK = Merge-sort ranking of an array
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! _________________________________________________________
      Real, Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
      Real :: XVALA, XVALB
!
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XDONT), SIZE(IRNGT))
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XDONT(IIND-1) <= XDONT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 2) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B. This line may be activated when the
!   initial set is already close to sorted.
!
!          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
!
!  One steps in the C subset, that we build in the final rank array
!
!  Make a copy of the rank array for the merge iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
!
            XVALA = XDONT (JWRKT(IINDA))
            XVALB = XDONT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XDONT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XDONT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
      Return
!
End Subroutine R_mrgrnk
Subroutine I_mrgrnk (XDONT, IRNGT)
! __________________________________________________________
!   MRGRNK = Merge-sort ranking of an array
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      Integer, Dimension (:), Intent (In)  :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
      Integer :: XVALA, XVALB
!
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XDONT), SIZE(IRNGT))
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XDONT(IIND-1) <= XDONT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 2) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B. This line may be activated when the
!   initial set is already close to sorted.
!
!          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
!
!  One steps in the C subset, that we build in the final rank array
!
!  Make a copy of the rank array for the merge iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
!
            XVALA = XDONT (JWRKT(IINDA))
            XVALB = XDONT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XDONT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XDONT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
      Return
!
End Subroutine I_mrgrnk
end module m_mrgrnk
