  module unit_cell

  USE trig_constants

  implicit none

  type cell_type
    private
    real, dimension(6)    :: par   = [10., 10., 10., 90., 90., 90.]
    real, dimension(6)    :: sd    = 0.
    real, dimension(3,3)  :: ortom = reshape([10., 0., 0., 0., 10., 0., 0., 0., 10.],[3,3])     ! transformation matrix cryst > cart
    real, dimension(3,3)  :: ortoi = reshape([0.1, 0., 0., 0., 0.1, 0., 0., 0., 0.1],[3,3])     ! transformation matrix cart > cryst
    real, dimension(3,3)  :: g     = reshape([100., 0., 0., 0., 100., 0., 0., 0., 100.],[3,3])  ! direct metric tensor
    real, dimension(3,3)  :: r     = reshape([0.01, 0., 0., 0., 0.01, 0., 0., 0., 0.01],[3,3])  ! reciprocal metric tensor
    real                  :: vol = 1000.
    integer, dimension(6) :: cod = 0

  contains
    procedure, private :: get_par_v
    procedure, private :: get_par_s
    generic            :: get_par => get_par_s, get_par_v
    procedure          :: get_a
    procedure          :: get_b
    procedure          :: get_c
    procedure          :: get_abc
    procedure          :: get_alpha
    procedure          :: get_beta
    procedure          :: get_gamma
    procedure ,private :: get_sd_v
    procedure ,private :: get_sd_s
    generic            :: get_sd => get_sd_s, get_sd_v
    procedure, private :: get_cod_s
    procedure, private :: get_cod_v
    generic            :: get_cod => get_cod_s, get_cod_v
    procedure          :: get_ortom
    procedure          :: get_ortoi
    procedure          :: get_g
    procedure          :: get_r
    procedure          :: volume => get_cell_volume
    !procedure          :: read_bin => read_cell_bin

    procedure          :: set 
    procedure          :: set_cod
    procedure          :: set_refine
    procedure          :: set_sd
    procedure          :: set_sdref
    procedure          :: set_std
    procedure          :: norm => normalize_cell

    procedure          :: define_kmesh
    procedure          :: write => write_cell
  end type cell_type

  CONTAINS

   function cell_init() result(cell)
   type(cell_type) :: cell
   type(cell_type) :: celldef
   cell = celldef
   end function cell_init

!-----------------------------------------------------------------------

   function get_par_v(cell)
   class(cell_type), intent(in) :: cell
   real, dimension(6)           :: get_par_v
   get_par_v = cell%par
   end function get_par_v

!-----------------------------------------------------------------------

   real function get_par_s(cell,i)
   class(cell_type), intent(in) :: cell
   integer, intent(in)          :: i
   get_par_s = cell%par(i)
   end function get_par_s

!-----------------------------------------------------------------------

   real function get_a(cell)
   class(cell_type), intent(in) :: cell
   get_a = cell%par(1)
   end function get_a

!-----------------------------------------------------------------------

   real function get_b(cell)
   class(cell_type), intent(in) :: cell
   get_b = cell%par(2)
   end function get_b

!-----------------------------------------------------------------------

   real function get_c(cell)
   class(cell_type), intent(in) :: cell
   get_c = cell%par(3)
   end function get_c

!-----------------------------------------------------------------------

   function get_abc(cell)
   class(cell_type), intent(in) :: cell
   real, dimension(3)           :: get_abc
   get_abc = cell%par(:3)
   end function get_abc

!-----------------------------------------------------------------------

   real function get_alpha(cell)
   class(cell_type), intent(in) :: cell
   get_alpha = cell%par(4)
   end function get_alpha

!-----------------------------------------------------------------------

   real function get_beta(cell)
   class(cell_type), intent(in) :: cell
   get_beta = cell%par(5)
   end function get_beta

!-----------------------------------------------------------------------

   real function get_gamma(cell)
   class(cell_type), intent(in) :: cell
   get_gamma = cell%par(6)
   end function get_gamma

!-----------------------------------------------------------------------

   function get_sd_v(cell)
   class(cell_type), intent(in) :: cell
   real, dimension(6)           :: get_sd_v
   get_sd_v = cell%sd
   end function get_sd_v

!-----------------------------------------------------------------------

   real function get_sd_s(cell,i)
   class(cell_type), intent(in) :: cell
   integer, intent(in)          :: i
   get_sd_s = cell%sd(i)
   end function get_sd_s

!-----------------------------------------------------------------------

   integer function get_cod_s(cell,i)
   class(cell_type), intent(in) :: cell
   integer, intent(in)          :: i
   get_cod_s = cell%cod(i)
   end function get_cod_s

!-----------------------------------------------------------------------

   function get_cod_v(cell)
   class(cell_type), intent(in) :: cell
   integer, dimension(6)        :: get_cod_v
   get_cod_v = cell%cod
   end function get_cod_v

!-----------------------------------------------------------------------

   function get_ortom(cell)
   class(cell_type), intent(in) :: cell
   real, dimension(3,3)         :: get_ortom
   get_ortom = cell%ortom
   end function get_ortom

!-----------------------------------------------------------------------

   function get_ortoi(cell)
   class(cell_type), intent(in) :: cell
   real, dimension(3,3)         :: get_ortoi
   get_ortoi = cell%ortoi
   end function get_ortoi

!-----------------------------------------------------------------------

   function get_g(cell)
   class(cell_type), intent(in) :: cell
   real, dimension(3,3)         :: get_g
   get_g = cell%g
   end function get_g

!-----------------------------------------------------------------------

   function get_r(cell)
   class(cell_type), intent(in) :: cell
   real, dimension(3,3)         :: get_r
   get_r = cell%r
   end function get_r

!-----------------------------------------------------------------------

   function set_cell_type(par) result(cell)
   real, dimension(6), intent(in) :: par
   type(cell_type)                :: cell               
   call cell%set(par)
   end function set_cell_type

!-----------------------------------------------------------------------

   subroutine set(cell,par)
   class(cell_type), intent(inout) :: cell
   real, dimension(6), intent(in)  :: par
!
   cell%par = par
   cell%g = matrice_metrica(par)
   cell%r = matrice_mreciproca(par)
   cell%ortom = orthomatrix(par)
   cell%ortoi = orthomatrixi(par)
   cell%vol = cell_volume(cell%par)
!
   end subroutine set

!-----------------------------------------------------------------------

   subroutine set_std(cell)
!
!  Conversion in standard frame
!
   class(cell_type), intent(inout) :: cell
   cell%ortom = orthomatrix_std(cell%par)
   cell%ortoi = orthomatrixi_std(cell%par)
   end subroutine set_std

!-----------------------------------------------------------------------

   subroutine normalize_cell(cell,spg)
!
!  Normalize cell parameter according to symmetry
!
   USE spginfom
   class(cell_type), intent(inout) :: cell
   type(spaceg_type), intent(in)   :: spg
   real, dimension(6)              :: par
!
   par = cell%par
   select case (spg%csys_code)
      case (CS_Tetragonal, CS_Hexagonal)  ! a=b/=c
       par(2) = par(1)

      case (CS_Trigonal)
       if (index(spg%symbol_xhm,':R') > 0) then   ! Rhombohedral setting: a=b=c,al=bet=gam/=90
           par(2:3) = par(1)
           par(5:6) = par(4)
       else
           par(2) = par(1)
       endif

      case (CS_Cubic)                     ! a=b=c
       par(2:3) = par(1)

   end select
   call cell%set(par)
!
   end subroutine normalize_cell

!-----------------------------------------------------------------------

   subroutine set_sdref(cell,sd,spg)
!
!  Set sd for refined parameters 
!
   USE spginfom
   class(cell_type), intent(inout) :: cell
   real, dimension(6), intent(in)  :: sd
   type(spaceg_type), intent(in)   :: spg
   integer                         :: i

   do i=1,6
      if (cell%cod(i) > 0) then
          cell%sd(i) = sd(i)
      endif
   enddo

   select case (spg%csys_code)
      case (CS_Tetragonal, CS_Hexagonal)  ! a=b/=c
       cell%sd(2) = cell%sd(1)

      case (CS_Trigonal)
       if (index(spg%symbol_xhm,':R') > 0) then   ! Rhombohedral setting: a=b=c,al=bet=gam/=90
           cell%sd(2:3) = cell%sd(1)
           cell%sd(5:6) = cell%sd(4)
       else
           cell%sd(2) = cell%sd(1)
       endif

      case (CS_Cubic)                     ! a=b=c
       cell%sd(2:3) = cell%sd(1)

   end select

   end subroutine set_sdref

!-----------------------------------------------------------------------

   real function get_cell_volume(cell) result(volume)
   class(cell_type), intent(in) :: cell
   volume = cell%vol
   end function get_cell_volume

!-----------------------------------------------------------------------

   function matrice_metrica(cell)  result(gg)
   real, dimension(6), intent(in) :: cell
   real, dimension(3,3)           :: gg
!
   gg(1,1) = cell(1)**2
   gg(2,2) = cell(2)**2
   gg(3,3) = cell(3)**2
   gg(1,2) = cell(1) * cell(2) * cos(cell(6) * dtor)
   gg(1,3) = cell(1) * cell(3) * cos(cell(5) * dtor)
   gg(2,3) = cell(2) * cell(3) * cos(cell(4) * dtor)
   gg(3,1) = gg(1,3)
   gg(2,1) = gg(1,2)
   gg(3,2) = gg(2,3)
!
   end function matrice_metrica
!------------------------------------------------------------------------
   function cella_reciproca(cell)  result(cells)
   use math_util
   real, dimension(6), intent(in) :: cell
   real, dimension(6)             :: cells
   real                           :: vol
   real                           :: s1,s2,s3
   real                           :: c1,c2,c3
!
   vol = sqrt(determinante(matrice_metrica(cell)))  ! volume cella
   s1 = sin(cell(4)*dtor);        c1 = cos(cell(4)*dtor)
   s2 = sin(cell(5)*dtor);        c2 = cos(cell(5)*dtor)
   s3 = sin(cell(6)*dtor);        c3 = cos(cell(6)*dtor)
   cells(1) = cell(2)*cell(3)*s1 / vol         ! a*
   cells(2) = cell(1)*cell(3)*s2 / vol         ! b*
   cells(3) = cell(1)*cell(2)*s3 / vol         ! c*
   cells(4) = (c2*c3-c1)/(s2*s3)               ! cos(alpha*)
   cells(5) = (c1*c3-c2)/(s1*s3)               ! cos(beta*)
   cells(6) = (c1*c2-c3)/(s1*s2)               ! cos(gamma*)
!
   end function cella_reciproca

!------------------------------------------------------------------------------------------

   function orthomatrix(cell)  result(om)
!
!  Generate the standard cartesian frame (x//a). use this reference frame for pdb file
! 
!  [ a     b*cos(gamma)   c*cos(beta)              ]
!  [ 0     b*sin(gamma)   -c*sin(beta)*cos(alpha*) ]
!  [ 0         0               1/c*                ]
!
   real, dimension(6), intent(in) :: cell
   real, dimension(6)             :: cells
   real, dimension(3,3)           :: om
!
   cells = cella_reciproca(cell)
!
   om(1,1)=cell(1)                              ! a
   om(1,2)=cell(2)*cos(cell(6)*dtor)            ! b*cos(gamma)
   om(1,3)=cell(3)*cos(cell(5)*dtor)            ! c*cos(beta)
   om(2,1)=0             
   om(2,2)=cell(2)*sin(cell(6)*dtor)            ! b*sin(gamma)
   om(2,3)=-cell(3)*sin(cell(5)*dtor)*cells(4)  ! -c*sin(beta)*cos(alpha*)
   om(3,1)=0
   om(3,2)=0
   om(3,3)=1/cells(3)                           ! 1/c* = c*sin(beta)*sin(gamma*); sin(gamma*) = sqrt(1-cos(gamma*)**2)
                                                ! om(3,3) = V / (a * b * sin(gamma))
!
   end function orthomatrix

!------------------------------------------------------------------------------------------

   function orthomatrixi(cell)  result(om)
!  
!  Inverse matrix of the standard frame (x//a) for conversion fractional to cartesian
!
!  [ 1/a    -cos(gamma)/a*sin(gamma)   a**beta*  ]
!  [  0       1/b*sin(gamma)           b*alpha*  ]
!  [  0            0                   c*        ]
   real, dimension(6), intent(in) :: cell
   real, dimension(6)             :: cells
   real, dimension(3,3)           :: om
!
   cells = cella_reciproca(cell)
!
   om(1,1)=1/cell(1)                                             ! 1/a
   om(1,2)=-cos(cell(6)*dtor)/(cell(1)*sin(cell(6)*dtor))        ! -cos(gamma)
   om(1,3)=cells(1)*cells(5)                                     ! a**beta*
   om(2,1)=0                    
   om(2,2)=1.0/(cell(2)*sin(cell(6)*dtor))                       ! 1/b*sin(gamma)
   om(2,3)=cells(2)*cells(4)                                     ! b*alpha*
   om(3,1)=0
   om(3,2)=0
   om(3,3)=cells(3)                                              ! c*
!
   end function orthomatrixi

!--------------------------------------------------------------------

   function orthomatrix1(cell)  result(om)
!
!  Generate matrix to convert fractional coordinates in cartesian coordinates
!   
   real, dimension(6), intent(in) :: cell
   real, dimension(6)             :: cells
   real, dimension(3,3)           :: om
!
   cells = cella_reciproca(cell)
!
   om(1,1)=cell(1)*sin(cell(5)*dtor)                     ! a*sen(beta)
   om(1,2)=-cell(2)*sin(cell(4)*dtor)*cells(6)           ! -b*sen(alfa)*cos(gammas)
   om(1,3)=0
   om(2,1)=0
   om(2,2)=1.0/cells(2)                                  !b*sen(alfa)*sen(gammas) = 1.0/cells(2)
   om(2,3)=0
   om(3,1)=cell(1)*cos(cell(5)*dtor)                     !a*cos(beta)
   om(3,2)=cell(2)*cos(cell(4)*dtor)                     !b*cos(alfa)
   om(3,3)=cell(3)                                       !c
!
   end function orthomatrix1
   
!--------------------------------------------------------------------

   function orthomatrixi1(cell)  result(om)
!
!  Generate matrix to convert cartesian coordinates in fractionale coordinates
! 
   real, dimension(6), intent(in) :: cell
   real, dimension(6)             :: cells
   real, dimension(3,3)           :: om
   !real, dimension(3,3)           :: om1

   cells = cella_reciproca(cell)
!
   om(1,1)=cells(1)*sin(acos(cells(6)))
   om(1,2)=cell(2)*cells(2)*cells(6)*sin(cell(4)*dtor)/(cell(1)*sin(cell(5)*dtor))
   om(1,3)=0
   om(2,1)=0                                              
   om(2,2)=cells(2)                                       
   om(2,3)=0
   om(3,1)=-1.0/(cell(3)*tan(cell(5)*dtor))
   om(3,2) = -cell(2)*cells(2)*(cos(cell(4)*dtor) +       &
              cells(6)*sin(cell(4)*dtor)*cos(cell(5)*dtor)/sin(cell(5)*dtor))/cell(3)
   om(3,3)=1.0/cell(3)

!   
   end function orthomatrixi1
   
!------------------------------------------------------------------------------------------

   function orthomatrix_std(cell)  result(om)
!
!  Generate the standard cartesian frame (x//a). use this reference frame for pdb file
! 
!  [ a     b*cos(gamma)   c*cos(beta)              ]
!  [ 0     b*sin(gamma)   -c*sin(beta)*cos(alpha*) ]
!  [ 0         0               1/c*                ]
!
   real, dimension(6), intent(in) :: cell
   real, dimension(6)             :: cells
   real, dimension(3,3)           :: om
!
   cells = cella_reciproca(cell)
!
   om(1,1)=cell(1)                              ! a
   om(1,2)=cell(2)*cos(cell(6)*dtor)            ! b*cos(gamma)
   om(1,3)=cell(3)*cos(cell(5)*dtor)            ! c*cos(beta)
   om(2,1)=0             
   om(2,2)=cell(2)*sin(cell(6)*dtor)            ! b*sin(gamma)
   om(2,3)=-cell(3)*sin(cell(5)*dtor)*cells(4)  ! -c*sin(beta)*cos(alpha*)
   om(3,1)=0
   om(3,2)=0
   om(3,3)=1/cells(3)                           ! 1/c* = c*sin(beta)*sin(gamma*); sin(gamma*) = sqrt(1-cos(gamma*)**2)
!
   end function orthomatrix_std

!------------------------------------------------------------------------------------------

   function orthomatrixi_std(cell)  result(om)
!  
!  Inverse matrix of the standard frame (x//a) for conversion fractional to cartesian
!
!  [ 1/a    -cos(gamma)/a*sin(gamma)   a**beta*  ]
!  [  0       1/b*sin(gamma)           b*alpha*  ]
!  [  0            0                   c*        ]
   real, dimension(6), intent(in) :: cell
   real, dimension(6)             :: cells
   real, dimension(3,3)           :: om
!
   cells = cella_reciproca(cell)
!
   om(1,1)=1/cell(1)                                             ! 1/a
   om(1,2)=-cos(cell(6)*dtor)/(cell(1)*sin(cell(6)*dtor))        ! -cos(gamma)
   om(1,3)=cells(1)*cells(5)                                     ! a**beta*
   om(2,1)=0                    
   om(2,2)=1.0/(cell(2)*sin(cell(6)*dtor))                       ! 1/b*sin(gamma)
   om(2,3)=cells(2)*cells(4)                                     ! b*alpha*
   om(3,1)=0
   om(3,2)=0
   om(3,3)=cells(3)                                              ! c*
!
   end function orthomatrixi_std

!------------------------------------------------------------------------------------------

   function deriv_orthomatrix_std(cell)   result(der)
!
!  Compute derivatve matrix to cartesian frame. Standard cartesian frame (x//a) is used.
!  This function is applied to compute the sd on distance and angle
!
   real, dimension(6), intent(in) :: cell
   real, dimension(3,3,6)         :: der
   real                           :: cosa,cosb,cosg,sina,sinb,sing
   real                           :: fval,gval
   real                           :: dfa,dfb,dfg
   real                           :: dga,dgb,dgg
                       !real, dimension(6)             :: cells
!
   der = 0
                       !            cells = cella_reciproca(cell)
!
!  Standard frame x//a
!  [ a     b*cos(gamma)   c*cos(beta)                    ]     [ a      b*cosg    c*cosb ]
!  [ 0     b*sin(gamma)   -c*sin(beta)*cos(alpha*)       ]  =  [ 0      b*sing    c*fval ]
!  [ 0         0         1/c* = c*sin(beta)*sin(gamma*)  ]     [ 0        0       c*gval ]
!  
   cosa = cos(cell(4)*dtor)
   cosb = cos(cell(5)*dtor)
   cosg = cos(cell(6)*dtor)
   sina = sin(cell(4)*dtor)
   sinb = sin(cell(5)*dtor)
   sing = sin(cell(6)*dtor)
!
!  Compute fval and his derivative
   fval = (cosa - cosb*cosg) / sing      ! sin(beta)*cos(alpha*)
   dfa = -sina/sing                      ! dfval/dalpha
   dfb = sinb*cosg/sing                  ! dfval/dbeta
   dfg =  (cosb - cosa*cosg) / sing**2   ! dfval/dgamma
    !write(0,*)'fval=',fval,cell(3)*fval,-cell(3)*sin(cell(5)*dtor)*cells(4)
!
!  Compute gval and his derivative
   gval = sqrt(sinb*sinb - fval*fval)    ! sin(beta)*sin(gamma*)
   dga = -fval*dfa/gval                  ! dgval/dalpha
   dgb = (cosb*sinb - fval*dfb) / gval   ! dgval/dbeta
   dgg = -fval*dfg/gval                  ! dgval/dgamma
    !write(0,*)'gval=',gval,cell(3)*gval,1/cells(3)
!
!  der(a) =      [ 1    0      0    0     0      0     ]
   der(1,1,1) = 1
!
!  der(b*cosg) = [ 0  cosg     0    0     0     b*sing ]
   der(1,2,2) = cosg
   der(1,2,6) = -cell(2)*sing
!
!  der(c*cosb) = [ 0     0   cosb   0   -c*sinb   0    ]  
   der(1,3,3) = cosb 
   der(1,3,5) = -cell(3)*sinb
!
!  der(b*sing) = [ 0   sing   0     0     0     b*cosg ]
   der(2,2,2) = sing
   der(2,2,6) = cell(2)*cosg
!
!  der(c*fval) = [ 0    0    fval   c*dfa  c*dfb c*dfg ]
   der(2,3,3) = fval
   der(2,3,4) = cell(3)*dfa
   der(2,3,5) = cell(3)*dfb
   der(2,3,6) = cell(3)*dfg
!
!  der(c*gval) = [ 0    0    gval   c*dga  c*dgb  c*dgg ]
   der(3,3,3) = gval
   der(3,3,4) = cell(3)*dga
   der(3,3,5) = cell(3)*dgb
   der(3,3,6) = cell(3)*dgg
!
   end function deriv_orthomatrix_std

!------------------------------------------------------------------------------------------

   subroutine distance_and_sd(sdcell,ortho,dermat,x1,x2,s1,s2,dis,sd)
   real, dimension(6), intent(in)     :: sdcell   ! standard deviations
   real, dimension(3,3), intent(in)   :: ortho    ! direct structure matrix
   real, dimension(3,3,6), intent(in) :: dermat   ! derivative of direct structure matrix 
   real, dimension(3), intent(in)     :: x1,x2    ! fractional coordinates of two points
   real, dimension(3), intent(in)     :: s1,s2    ! standard deviation on fractional coordinates
   real, intent(out)                  :: dis,sd   ! distance and sd
   real, dimension(3)                 :: xdiff
   real, dimension(3)                 :: xmat
   real, dimension(6)                 :: dxyz
   real, dimension(6)                 :: dcell
   integer                            :: i
!
   xdiff = x2 - x1
   xmat = matmul(ortho,xdiff) 
   dis = sqrt(dot_product(xmat,xmat))
!   
!  derivate of d^2 with rispect x,y,z, factor 2 is ignored
   dxyz(:3) = matmul(xmat,ortho)
   dxyz(4:6) = -dxyz(1:3)
!
!  derivate d^2 with rispect a,b,c,alpha,beta,gamma, factor 2 is ignored
   do i=1,6
      dcell(i) = dot_product(xmat,matmul(dermat(:,:,i),xdiff))
   enddo
!
!  compute sigma(d^2)**2 = sum(der**2)
   sd = 0
   do i=1,3
      sd = sd + (dxyz(i)*s2(i))**2 + (dxyz(i+3)*s1(i))**2    &
              + (dcell(i)*sdcell(i))**2                      &
              + (dcell(i+3)*sdcell(i+3)*dtor)**2    
   enddo
!
   sd = sqrt(sd)/dis
!
   end subroutine distance_and_sd

!------------------------------------------------------------------------------------------

   function matrice_mreciproca(cell)  result(ggr)
   real, dimension(6), intent(in) :: cell
   real, dimension(6)             :: cells
   real, dimension(3,3)           :: ggr
!
   cells = cella_reciproca(cell)
!
   ggr(1,1) = cells(1)**2
   ggr(2,2) = cells(2)**2
   ggr(3,3) = cells(3)**2
   ggr(1,2) = cells(1) * cells(2) * cells(6)
   ggr(1,3) = cells(1) * cells(3) * cells(5)
   ggr(2,3) = cells(2) * cells(3) * cells(4)
   ggr(3,1) = ggr(1,3)
   ggr(2,1) = ggr(1,2)
   ggr(3,2) = ggr(2,3)
!
   end function matrice_mreciproca
  
!--------------------------------------------------------------------

   real function cell_volume(cell)
   use math_util
   real, dimension(:), intent(in) :: cell
!
   cell_volume = sqrt(determinante(matrice_metrica(cell)))
!
   end function cell_volume

!--------------------------------------------------------------------

   real function cell_volume_std(vol,cell,cellstd) result(std)
   real, intent(in)               :: vol
   real, dimension(:), intent(in) :: cell
   real, dimension(:), intent(in) :: cellstd
   real, dimension(3)             :: a,b
   real, dimension(3)             :: cellrad
   real                           :: vol2
!
   a(:) = cellstd(:3) / cell(:3)
   cellrad(:) = cell(4:)*dtor
   b(1) = sin(cellrad(1))*(cos(cellrad(1)) - cos(cellrad(2))*cos(cellrad(3)))*cellstd(4)*dtor
   b(2) = sin(cellrad(2))*(cos(cellrad(2)) - cos(cellrad(1))*cos(cellrad(3)))*cellstd(5)*dtor
   b(3) = sin(cellrad(3))*(cos(cellrad(3)) - cos(cellrad(1))*cos(cellrad(2)))*cellstd(6)*dtor
   vol2 = vol**2
   std = vol2*sum(a(:)**2) + ((cell(1)*cell(2)*cell(3))**4)*sum(b(:)**2)/vol2
   std = sqrt(std)
!
   end function cell_volume_std

!--------------------------------------------------------------------

   subroutine print_cell(cell,sdcell,sys,kpr)
   USE strutil
   real, dimension(6), intent(in)            :: cell
   real, dimension(6), intent(in), optional  :: sdcell
   character(len=2), intent(in), optional    :: sys
   integer, intent(in)                       :: kpr
   character(len=*), dimension(6), parameter :: scell= ['a    ','b    ','c    ','alpha','beta ','gamma']
   integer                                   :: i
   character(len=2)                          :: csys
   real                                      :: vol, stdvol
!
   if (present(sys)) then
       csys = sys
   else
       csys = ' '
   endif
   if (present(sdcell)) then
       vol = cell_volume(cell)
       stdvol = cell_volume_std(vol,cell,sdcell)
       select case (csys)
         case ('CU')         ! Cubic
           write(kpr,10)trim(scell(1))//'= ',string_esd(cell(1),sdcell(1)),'Volume= ',string_esd(vol,stdvol)

         case ('TE','HE')    ! Tetragonal, hexagonal
           write(kpr,10)trim(scell(1))//'= ',string_esd(cell(1),sdcell(1)),    &
                        trim(scell(3))//'= ',string_esd(cell(3),sdcell(3)),'Volume= ',string_esd(vol,stdvol)

         case ('OR')         ! Orthorombic
           write(kpr,10)(trim(scell(i))//'= ',string_esd(cell(i),sdcell(i)), i=1,3)
           write(kpr,10)'Volume= ',string_esd(vol,stdvol)

         case ('MO')         ! Monoclinic
           write(kpr,10)(trim(scell(i))//'= ',string_esd(cell(i),sdcell(i)), i=1,3)
           write(kpr,10)trim(scell(5))//'= ',string_esd(cell(5),sdcell(5)),'Volume= ',string_esd(vol,stdvol)

         case default       
           write(kpr,10)(trim(scell(i))//'= ',string_esd(cell(i),sdcell(i),5), i=1,3)
           write(kpr,10)(trim(scell(i))//'= ',string_esd(cell(i),sdcell(i),3), i=4,6)
           write(kpr,10)'Volume= ',string_esd(vol,stdvol)

       end select
10     format(t5,a8,a,t30,a8,a,t50,a8,a,t70,a,a)
   else
       write(kpr,'(1x,a)')'Cell parameters'
       write(kpr,'(1x,a8,f0.5,t25,a8,f0.5,t43,a8,f0.5)')(trim(scell(i))//' = ',cell(i),i=1,3)
       write(kpr,'(1x,a8,f0.3,t25,a8,f0.3,t43,a8,f0.3)')(trim(scell(i))//' = ',cell(i),i=4,6)
       write(kpr,'(" Cell volume: ",f0.2)') cell_volume(cell)
   endif
!
   end subroutine print_cell

!--------------------------------------------------------------------

   subroutine write_cell(cell,kpr)
   class(cell_type), intent(in) :: cell
   integer, intent(in)         :: kpr
   call print_cell(cell%par,cell%sd,kpr=kpr)
   end subroutine write_cell

!--------------------------------------------------------------------

   function define_kmesh(cell,dval)  result(km)
!
!  Define reasonable value for k-mesh using cell parameters
!  dval: grid resolution = 0.15 (very fine), 0.2 (fine), 0.3 (normal)
!  Reference: https://www.materialscloud.org/work/tools/qeinputgenerator
!
   use trig_constants, only: twopi
   class(cell_type), intent(in) :: cell
   real, intent(in)             :: dval 
   integer, dimension(3)        :: km
   real, dimension(6)           :: cells
!
   cells = cella_reciproca(cell%get_par())
   km(:) = ceiling(twopi*cells(:3)/dval)
!
   end function define_kmesh

! ----------------------------------------------------------------------

   function cell_ref_code(spg)  result(code)
!  Genera un codice utile nell'affinamento della cella
!  code = 0           -> parametro affinabile
!  code = -90 o -120  -> parametro non affinabile 
!  code = -1          -> parametro legato da constraint
   USE spginfom
!
   type(spaceg_type), intent(in) :: spg
   integer, dimension(6)         :: code
!
   code(:) = 0
   select case (spg%csys_code)
      case (CS_Monoclinic)                          ! Monoclin
          select case(spg%axis_direction())
            case ('a')
               code(5) = -90
               code(6) = -90
            case ('b')
               code(4) = -90
               code(6) = -90
            case ('c')
               code(4) = -90
               code(5) = -90
          end select
      case (CS_orthorhombic)            ! Orthorhombic
          code(4)=-90
          code(5)=-90
          code(6)=-90
      case (CS_Tetragonal)              ! Tetragonal
          code(2)=-1
          code(4)=-90
          code(5)=-90
          code(6)=-90
      case (CS_Trigonal)                ! Trigonal
          if (index(spg%symbol_xhm,':R') > 0) then   ! Rhombohedral setting: a=b=c,al=bet=gam/=90
              code(2)=-1
              code(3)=-1
              code(5)=-1
              code(6)=-1
          else                                       ! Hexagonal setting: a=b/=c,al=bet=90,gam=120
              code(2) = -1
              code(4)=-90
              code(5)=-90
              code(6)=-120
          endif
      case (CS_Hexagonal)               ! Hexagonal
          code(2)=-1
          code(4)=-90
          code(5)=-90
          code(6)=-120
      case (CS_Cubic)                   ! Cubic
          code(2)=-1
          code(3)=-1
          code(4)=-90
          code(5)=-90
          code(6)=-90
   end select
!
   end function cell_ref_code

! ----------------------------------------------------------------------

   subroutine set_rcode_cell(coderef,spg,value)  
   USE spginfom
   integer, dimension(6), intent(out) :: coderef
   type(spaceg_type), intent(in)      :: spg
   integer, intent(in)                :: value
!
!  Generate refinement code by symmetry
   coderef =  cell_ref_code(spg)
!
!  Update refinement code only if refinement is required (value/=0)
   if (value /= 0) then
       where (coderef == 0) coderef = 1
   endif
!
   end subroutine set_rcode_cell

! ----------------------------------------------------------------------

   subroutine set_cod(cell,i,val)
   class(cell_type), intent(inout) :: cell
   integer, intent(in)            :: i, val
   cell%cod(i) = val
   end subroutine set_cod

! ----------------------------------------------------------------------

   subroutine set_refine(cell,spg,val)
   USE spginfom
   class(cell_type), intent(inout) :: cell
   type(spaceg_type), intent(in)   :: spg
   integer, intent(in)             :: val
   call set_rcode_cell(cell%cod,spg,val)
   end subroutine set_refine

! ----------------------------------------------------------------------

   subroutine set_sd(cell,i,val)
   class(cell_type), intent(inout) :: cell
   integer, intent(in)             :: i
   real, intent(in)                :: val
   cell%sd(i) = val
   end subroutine set_sd

! ----------------------------------------------------------------------

   subroutine read_cell_bin(unitbin,cell,err)
!
!  Read cell from binary file
!
   USE errormod
   integer, intent(in)           :: unitbin
   type(cell_type), intent(out)  :: cell
   type(error_type), intent(out) :: err
   integer                       :: ier
!
   !read(unitbin,iostat=ier) cell%par,cell%sd,cell%ortom,cell%ortoi,cell%g,cell%cod
   read(unitbin,iostat=ier) cell
   if (ier /= 0) then
       call err%set('Error on reading cell parameters')
   endif
!
   end subroutine read_cell_bin

! ----------------------------------------------------------------------

   subroutine save_cell_bin(unitbin,cell)
!
!  Write cell on binary file
!
   integer, intent(in)         :: unitbin
   type(cell_type), intent(in) :: cell
   write(unitbin)cell
   end subroutine save_cell_bin

!----------------------------------------------------------------------------------------------------

   subroutine trafo(hkl,cell,gmat)
   real, dimension(3), intent(in)    :: hkl
   type(cell_type), intent(in)       :: cell
   real, dimension(3,3), intent(out) :: gmat
   real, dimension(3) :: xc,yc,zc
   real               :: zcc
   real, dimension(3,3) :: rten
   real, dimension(3,3,3) :: eps
   integer :: i,j,k,l,m,inull
   integer, dimension(2) :: inull2
!
!  Determine axes for transformation                               
!  Transform reciprocal vector HKL to real space vector ZC           
   zc = matmul(cell%get_r(),hkl)
!
   zcc = sqrt(dot_product(zc,matmul(cell%get_g(),zc)))
   write(0,*)'ZCC=',zcc
   if (zcc == 0) return
   inull = 0
   inull2 (:) = 0
   do i = 1, 3
      zc (i) = zc (i) / zcc
      if (hkl (i) .eq.0.0) then
         inull = inull + 1
         inull2 (inull) = i
      endif
      xc (i) = 0.0
      yc (i) = 0.0
   enddo
!                                                                    
!  Determine a direction as fundamental as possible for XC           
   if (inull.eq.2.or.inull.eq.1) then
      xc (inull2 (1) ) = 1.0
   else
      if (hkl (1) .eq.hkl (2) ) then
         xc (1) = hkl (1)
         xc (2) = - hkl (2)
      elseif (hkl (1) .eq.hkl (3) ) then
         xc (1) = hkl (1)
         xc (3) = - hkl (3)
      elseif (hkl (2) .eq.hkl (3) ) then
         xc (2) = hkl (2)
         xc (3) = - hkl (3)
      else
         xc (1) = hkl (2)
         xc (2) = - hkl (1)
      endif
   endif
!                                                                    
!  normalize XC                                                      
   xc(:) = xc(:) / sqrt(dot_product(xc,matmul(cell%get_g(),xc)))
!
!  calculate yc as the vector product of zc and xc                 
   rten = cell%get_r()
   eps = 0.0
   eps(1,2,3) = cell%vol
   eps(2,3,1) = cell%vol
   eps(3,1,2) = cell%vol
   eps(1,3,2) = -cell%vol
   eps(3,2,1) = -cell%vol
   eps(2,1,3) = -cell%vol
   do m = 1, 3
      yc (m) = 0.0
      do j = 1, 3
         do k = 1, 3
            do l = 1, 3
               yc (m) = yc (m) + eps (j, k, l) * zc (k) * xc (l) * rten (j, m)
            enddo
         enddo
      enddo
   enddo
!                                                                       
!  calculate indices of gmat                                         
   do i = 1, 3
      gmat (i, 1) = xc (i)
      gmat (i, 2) = yc (i)
      gmat (i, 3) = zc (i)
   enddo
   write(0,*)'ZC=',zc
   write(0,*)'XC=',xc
   write(0,*)'YC=',yc
!
   end subroutine trafo

!----------------------------------------------------------------------------------------------------

   subroutine cell_transform(cell,pmat,newcell)
!
!  Transform cell in newcell by pmat
!
   use trig_constants
   type(cell_type), intent(in)      :: cell
   real, dimension(3,3), intent(in) :: pmat
   type(cell_type), intent(out)     :: newcell
   real, dimension(3,3)             :: gnew
!                      T
!  Metric tensor: G = P G P from page 85 of ITC volume A
   gnew = matmul(transpose(pmat),matmul(cell%get_g(),pmat))
!
   !call newcell%set([sqrt(gnew(1,1)),sqrt(gnew(2,2)),sqrt(gnew(3,3)), &
   !                 rtod*acos(gnew(2,3)/sqrt(gnew(2,2)*gnew(3,3))),  &
   !                 rtod*acos(gnew(1,3)/sqrt(gnew(1,1)*gnew(3,3))),  &
   !                 rtod*acos(gnew(1,2)/sqrt(gnew(1,1)*gnew(2,2)))])
   call newcell%set(par_from_g(gnew))
!
   end subroutine cell_transform

!----------------------------------------------------------------------------------------------------

   function par_from_g(gmat) result(par)
!
!  Calculate cell parameters from the metric tensor
!
   real, dimension(3,3), intent(in) :: gmat
   real, dimension(6)               :: par
   par = [sqrt(gmat(1,1)),sqrt(gmat(2,2)),sqrt(gmat(3,3)), &
          rtod*acos(gmat(2,3)/sqrt(gmat(2,2)*gmat(3,3))),  &
          rtod*acos(gmat(1,3)/sqrt(gmat(1,1)*gmat(3,3))),  &
          rtod*acos(gmat(1,2)/sqrt(gmat(1,1)*gmat(2,2)))]
   end function par_from_g

!----------------------------------------------------------------------------------------------------

   subroutine standard_symmetry(spg,cell,kpr,ier,mat,vet)
   use spginfom
   type(spaceg_type), intent(inout) :: spg
   type(cell_type), intent(inout)   :: cell
   integer, intent(in)              :: kpr
   integer, intent(out)             :: ier
   real, dimension(3,3)             :: pmat
   real, dimension(3)               :: pvet
   type(cell_type)                  :: cellnew
   real, dimension(3,3), optional   :: mat  ! inverse matrix
   real, dimension(3), optional     :: vet
!
   ier = 1
   if (.not.is_standardize(spg)) return
   ier = 0
   if (kpr > 0) then 
       write(kpr,'(a)')' Transformation to standard setting'
       write(kpr,'(a)')' Input cell in '//trim(spg%symbol_xhm)
       call cell%write(kpr)
       write(kpr,'(a)')
   endif
   call spg%get_pmat1(pmat,pvet)
   if (present(mat) .and. present(vet)) then
       call spg%get_pmat(mat,vet)
   endif
   if (kpr > 0) then
       call print_transform_matrix(kpr,pmat,pvet,spg%pmat1)
       write(kpr,'(a)')
   endif
   call cell_transform(cell,pmat,cellnew)
   spg = standard_spg(spg)
   cell = cellnew
   if (kpr > 0) then
       write(kpr,'(a)')' Output cell in '//trim(spg%symbol_xhm)
       call cell%write(kpr)
   endif
!
   end subroutine standard_symmetry

!----------------------------------------------------------------------------------------------------

   subroutine transform_symmetry(spgin,spgout,cell,kpr,ier,mat,vet)
   use spginfom
   type(spaceg_type), intent(in)  :: spgin,spgout
   type(cell_type), intent(inout) :: cell
   integer, intent(in)            :: kpr
   integer, intent(out)           :: ier
   real, dimension(3,3)           :: pmat,pmat1,pmat2
   real, dimension(3)             :: pvet,pvet1,pvet2
   type(cell_type)                :: cellnew
   type(spaceg_type)              :: spgtemp
   real, dimension(3,3), optional :: mat  ! inverse matrix
   real, dimension(3), optional   :: vet
   logical                        :: is_mat
! 
   ier = 1
   if (spgin%num  /= spgout%num) return
   if (spg_equal(spgin,spgout)) return
   is_mat = present(mat) .and. present(vet)
!
   if (spgout%standard) then
       spgtemp = spgin
       if (is_mat) then
           call standard_symmetry(spgtemp,cell,kpr,ier,mat,vet)
       else
           call standard_symmetry(spgtemp,cell,kpr,ier)
       endif
       return
   endif
!
   ier = 0
   if (spgin%standard) then
       call spgout%get_pmat(pmat2,pvet2)
       if (is_mat) call spgout%get_pmat1(mat,vet)
   else
       call spgin%get_pmat1(pmat1,pvet1)
       call spgout%get_pmat(pmat,pvet)
       pmat2 = matmul(pmat1,pmat)
       pvet2 = pvet + pvet1
       if (is_mat) then
           ! Theorem: (AB)-1 = A-1 B-1
           call spgin%get_pmat(pmat1,pvet1)
           call spgout%get_pmat1(pmat,pvet)
           mat = matmul(pmat1,pmat)
           vet = pvet + pvet1
       endif
   endif
   if (kpr > 0) then
       write(kpr,'(a)')' Transformation to setting '//trim(spgout%symbol_xhm)
       write(kpr,'(a)')' Input cell in '//trim(spgin%symbol_xhm)
       call cell%write(kpr)
       write(kpr,'(a)')
       call print_transform_matrix(kpr,pmat2,pvet2)
       write(kpr,'(a)')
   endif
!
   call cell_transform(cell,pmat2,cellnew)
   cell = cellnew
   if (kpr > 0) then
       write(kpr,'(a)')'Output cell in '//trim(spgout%symbol_xhm)
       call cell%write(kpr)
   endif
!
   end subroutine transform_symmetry

!----------------------------------------------------------------------------------------------------

   subroutine print_transform_matrix(kpr,mat,vet,strmat)
   use math_util
   integer, intent(in)                    :: kpr
   real, dimension(3,3), intent(in)       :: mat
   real, dimension(3)                     :: vet
   character(len=*), intent(in), optional :: strmat
   integer                                :: i
!
   if (all(is_integer(vet)) .and. all(is_integer(mat))) then
       if (present(strmat)) then
           write(kpr,'(a,2(i0,", "),i0)')' Transformation matrix (P,p): '//trim(strmat)//'; ',int(vet(:))
       else
           write(kpr,'(a)')' Transformation matrix (P,p)'
       endif
       do i=1,3
          if (i == 2) then
              write(kpr,'(a,"[",3i3,"]",2x,"[",i3,"]")')' (P,p): ',int(mat(i,:)),int(vet(i))
          else
              write(kpr,'(a,"[",3i3,"]",2x,"[",i3,"]")')'        ',int(mat(i,:)),int(vet(i))
          endif
       enddo
   else
       if (present(strmat)) then
           write(kpr,'(a,2(f10.3,", "),f10.3)')' Transformation matrix (P,p): '//trim(strmat)//'; ',vet(:)
       else
           write(kpr,'(a)')' Transformation matrix (P,p)'
       endif
       do i=1,3
          if (i == 2) then
              write(kpr,'(a,"[",3f10.3,"]",2x,"[",f10.3,"]")')' (P,p): ',mat(i,:),vet(i)
          else
              write(kpr,'(a,"[",3f10.3,"]",2x,"[",f10.3,"]")')'        ',mat(i,:),vet(i)
          endif
       enddo
   endif
!
   end subroutine print_transform_matrix 

   end module unit_cell

