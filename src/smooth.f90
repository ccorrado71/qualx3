MODULE ssmoothing  

   implicit none  

   enum, bind(c)
     enumerator :: SAVGOL, AVERAGING
   endenum
   
   type smooth_condition_type
     integer :: method      = SAVGOL
     integer :: npoints_sg  = 4
     integer :: npoints_ave = 4
     integer :: pol_order   = 5
   end type smooth_condition_type

CONTAINS

   function smooth_calc(xdata, scond) result(xs)
   real, dimension(:), intent(in)          :: xdata  
   type(smooth_condition_type), intent(in) :: scond
   real, dimension(size(xdata))            :: xs
!
   select case (scond%method)
     case (SAVGOL)
       xs = savgol_smooth(xdata,scond%npoints_sg,scond%npoints_sg,0,scond%pol_order)

     case (AVERAGING)
       xs = averagesmooth(xdata,scond%npoints_ave)

   end select
!   
   end function smooth_calc

  !-----------------------------------------------------------------------------------

   function savgol_smooth(xdata,nleft,nright,oder,opol)  result(xs)
   USE nr
!
   real, dimension(:)              :: xdata  ! dati su cui applicare lo smoothing
   integer                         :: nleft  ! numero di punti a sinistra
   integer                         :: nright ! numero di punti a destra
   integer                         :: oder   ! ordine della derivata (=0 per smoothing della funzione)
   integer                         :: opol   ! ordine della polinomiale
   real, dimension(size(xdata))    :: xs     ! smoothing
   real, dimension(nleft+nright+1) :: coef   ! coefficienti di Savinsky-Golay
   real, dimension(-nleft:nright)  :: cout
   integer                         :: npunti   
   integer                         :: inip,finp
   integer                         :: i,j
   integer                         :: nleft0,nright0
   real, dimension(:), allocatable :: cout0
   integer                         :: boundary_condition 
!
   if (oder == 0) then
       boundary_condition = 2
   else
       boundary_condition = 1
   endif
   if (boundary_condition == 2) then
!
!      Starting points in case of smoothing
!
       !write(0,*)'SG=',nleft,nright,oder,opol
       xs(1) = xdata(1)
       do i = 2,nleft
          nleft0 = i - 1
          if (nleft0+nright < opol) then
              xs(i) = xdata(i)
          else
              coef(:nleft0+nright+1) = savgol(nleft0,nright,oder,opol)
              allocate(cout0(-nleft0:nright))
              cout0(-nleft0:0) = coef(nleft0+1:1:-1)
              cout0(1:nright) = coef(nleft0+nright+1:nleft0+2:-1)
              xs(i) = sum(cout0(-nleft0:nright)*xdata(i-nleft0:i+nright))
              deallocate(cout0)
          endif
       enddo
!
!      calcola i coefficienti
       coef = savgol(nleft,nright,oder,opol)
!
!      riordina i coefficienti
       cout(-nleft:0) = coef(nleft+1:1:-1)
       cout(1:nright) = coef(nleft+nright+1:nleft+2:-1)

   elseif (boundary_condition == 1) then
!
!      calcola i coefficienti
       coef = savgol(nleft,nright,oder,opol)
!
!      riordina i coefficienti
       cout(-nleft:0) = coef(nleft+1:1:-1)
       cout(1:nright) = coef(nleft+nright+1:nleft+2:-1)
!
!      Starting points. Andamento costante prima del punto n.1
       do i=1,nleft
          xs(i) = xdata(1)*sum(cout(-nleft:-i)) + sum(cout(-i+1:nright)*xdata(1:i+nright))
       enddo
   endif
!
   npunti = size(xdata)   ! num. di punti
!
!  Punti centrali
   inip = nleft + 1
   finp = npunti - nright
   do i=inip,finp
      xs(i) = sum(cout(-nleft:nright)*xdata(i-nleft:i+nright))
   enddo
!
!  Final points
   if (boundary_condition == 1) then
       do i=finp+1,npunti
          j = npunti - i + 1
          xs(i) = xdata(npunti)*sum(cout(j:nright)) + sum(cout(-nleft:j-1)*xdata(i-nleft:npunti))
       enddo
   elseif (boundary_condition == 2) then
       do i = npunti - nright + 1, npunti - 1 
          nright0 = npunti - i
          !write(0,*)'SGR=',nleft,nright0,oder,opol
          if (nleft+nright0 < opol) then
              xs(i) = xdata(i)
          else
              coef(:nleft+nright0+1) = savgol(nleft,nright0,oder,opol)
              allocate(cout0(-nleft:nright0))
              cout0(-nleft:0) = coef(nleft+1:1:-1)
              cout0(1:nright0) = coef(nleft+nright0+1:nleft+2:-1)
              xs(i) = sum(cout0(-nleft:nright0)*xdata(i-nleft:i+nright0))
              deallocate(cout0)
          endif
       enddo
       xs(npunti) = xdata(npunti)
   endif
!
   end function savgol_smooth
   
  !-----------------------------------------------------------------------------------

   function averagesmooth(datavet,npoints)
!
!  Esegue uno smoothing su datavet usando la tecnica della media
!
   implicit none
   real, dimension(:), intent(in) :: datavet ! vettore in ingresso
   integer, intent(in)            :: npoints ! 2*npoints + 1 e' il numero di punti mediati
   real, dimension(size(datavet)) :: averagesmooth
   integer                        :: punti
   integer                        :: i
   real                           :: const
!
   punti = size(datavet)
   if (punti < 2*npoints+1) then
       averagesmooth(:) = datavet(:)
       return
   endif
!
!  Use progressively smalling smoother for starting points to solve the edge effect
!
   averagesmooth(1) = datavet(1)
   do i=2,npoints
      averagesmooth(i) = sum(datavet(1:2*i-1))/(2*i-1)
   enddo
!
!  Central points
!
   const = 1./(2.*npoints+1)
   do i=npoints+1,punti-npoints
      averagesmooth(i) = const*(sum(datavet(i-npoints:i+npoints)))
   enddo
!
!  Use progressively smalling smoother for ending points to solve the edge effect
!
   do i=punti-npoints+1,punti - 1
      averagesmooth(i) = sum(datavet(2*i-punti:punti))/(2*punti - 2*i + 1)
   enddo
   averagesmooth(punti) = datavet(punti)
!
   end function averagesmooth

END MODULE SSMOOTHING
