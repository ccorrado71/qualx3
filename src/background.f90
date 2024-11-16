  MODULE Background

  USE pointmod, only : point_type

  implicit none

  enum, bind(c)
    enumerator :: CHEBY, POLY, FOUR_SERIES, CSPLINE, BSPLINE, BK_FILTER, BK_NONE
  endenum

  integer, parameter :: NMAXCOEFB = 36 

  type back_condition_type
    integer :: btype = CHEBY              ! background type
    logical :: auto  = .true.             ! automatic selection of number of coefficient
    integer :: ncoef = 6                  ! number of coefficients
    integer :: niterf = 5                 ! number of iterations for filter
    integer :: nwinf = 50                 ! filter window
    real    :: minf = 0.0, maxf = 0.0     ! range of application of the filter
  end type back_condition_type

  CONTAINS

!----------------------------------------------------------------------------------------

   subroutine compute_background(xcount,ycount,bcount,pointb,coefb,thzerob,backt,wave)
   use fattoriale_bezier, only: punto
   real, dimension(:), intent(in)                           :: xcount    ! x
   real, dimension(:), intent(in)                           :: ycount    ! y
   real, dimension(:), intent(out)                          :: bcount    ! background
   real, dimension(:), intent(out)                          :: coefb
   real, intent(out)                                        :: thzerob
   type(point_type), dimension(:), allocatable, intent(out) :: pointb
   type(back_condition_type), intent(inout)                 :: backt
   real, intent(in)                                         :: wave
   type(punto),dimension(:), allocatable                    :: bkpoint8
   integer                                                  :: npointb
   real                                                     :: thmin0,thmax0
!
!  Seleziona punti di background sull'osservato
   if (backt%btype /= BK_NONE .or. backt%btype /= BK_FILTER) then
       call piazza_punti(xcount,ycount,npointb,pointb,wave)
   endif
!
!  Definisci origine (thminbkg) del polinomio
   thmin0 = xcount(1)
   thmax0 = xcount(size(xcount))
   thzerob = (thmin0 + thmax0) / 2.0
!
   select case(backt%btype)
      case (POLY,CHEBY,FOUR_SERIES)
        if (backt%auto) then
           call autobackground(xcount,ycount,bcount,backt%btype,pointb,thzerob,coefb,backt%ncoef)
        else
           call param_back(backt%btype,pointb,size(coefb),coefb,thmin0,thmax0,thzerob)
           call getbackground(backt%btype,bcount,xcount,coefb,thzerob)
        endif

      case (CSPLINE)
        call genspline(xcount,pointb%x,pointb%y,bcount)

      case (BSPLINE)
        allocate(bkpoint8(npointb))
        bkpoint8%x = pointb%x
        bkpoint8%y = pointb%y
        call genbezier(xcount,bkpoint8,bcount)

      case (BK_FILTER)
        call bruckner_back(xcount,ycount,bcount,backt%niterf,backt%nwinf,backt%minf,backt%maxf)

      case (BK_NONE)
        bcount(:) = 0
!
   end select
   end subroutine compute_background

!----------------------------------------------------------------------------------------

   subroutine autobackground(xcount,ycount,bcount,typeback,backp,thzerob,coefb,ncoef)
             use arrayutil
   real, dimension(:), intent(in)   :: xcount,ycount
   real, dimension(:), intent(out)  :: bcount
   integer, intent(in)              :: typeback
   type(point_type), dimension(:), intent(in) :: backp
   real, intent(in)                 :: thzerob
   real, dimension(:), intent(out)  :: coefb
   integer, intent(out)             :: ncoef
   integer                          :: npointb
   integer                          :: nbk,ncoefmax
   real                             :: rpbmin,rpback
   real, allocatable                :: bcalc(:)
   real, allocatable                :: coefbkg0(:)
   real                             :: thmin0,thmax0
!!!!!!
     real, dimension(:), allocatable :: yb
     integer, dimension(:), allocatable :: vetp
     integer :: npp,pos,i,j
     integer, parameter :: NADDP=3
!corr     real :: rpback1
!!!!!!
!
   npointb = size(backp,1)                         ! migliorare introducendo controllo
   select case (typeback)                          ! su background troppo alti rispetto all'osservato
       case (POLY,FOUR_SERIES)                     ! magari rimuovendo il punto piu' vicino
           ncoefmax = 8
       case (CHEBY)
           ncoefmax = 20  !!!10
   end select
!
   thmin0 = xcount(1)              
   thmax0 = xcount(size(xcount))  
   rpbmin = huge(1.0)
   !ncoefmax = min(npointb-2,ncoefmax)   ! il num. coeff. non può essere piu' grande dei punti
   ncoefmax = min(nint(npointb*0.70),ncoefmax)   ! il num. coeff. non può essere piu' grande dei punti
   allocate(bcalc(npointb),coefbkg0(ncoefmax))
!
!  Save in vetp backgrounds points + neighbours
   allocate(vetp(size(xcount)))
   npp = 0
   vetp(:) = 0
   do i=1,npointb
      !npp = npp + 1
      !x0(npp) = backp(i)%x
      !y0(npp) = backp(i)%y
      pos = clocate(xcount,backp(i)%x)
      !write(0,*)'POINT:',i,npp,pos,x0(npp),y0(npp)
      do j=pos-NADDP,pos+NADDP
         if (j < 1 .or. j > size(xcount)) exit
         vetp(j) = j
         !npp = npp + 1
         !x0(npp) = xcount(j)
         !y0(npp) = ycount(j)
         !write(0,*)'POINT:',i,npp,j,x0(npp),y0(npp)
         !write(0,*)'POINT:',i,j,backp(i)%x,xcount(vetp(j)),ycount(vetp(j))
      enddo
   enddo
   npp = count(vetp > 0)
   vetp(:npp) = pack(vetp,mask=vetp>0)
   call resize_array(vetp,npp)
   !do j=1,npp
   !   write(0,*)'POINT:',j,xcount(vetp(j)),ycount(vetp(j))
   !enddo
   allocate(yb(npp))
!
   do nbk=1,ncoefmax
!
!     Calcola coefficienti
      call param_back(typeback,backp(:npointb),nbk,coefbkg0,thmin0,thmax0,thzerob)
!
!     Compute figure of merit
!corr      call getbackground(typeback,bcalc(:npointb),backp(:npointb)%x,coefbkg0(:nbk),thzerob)   ! back on selected points
!corr      rpback1 = ( SUM(abs(backp(:npointb)%y-bcalc(:))) / SUM(backp(:npointb)%y) ) * 100.
               !write(0,*)'NCOEF1=',nbk,rpback
      call getbackground(typeback,yb,xcount(vetp),coefbkg0(:nbk),thzerob)   ! back on selected points
               rpback = ( SUM(abs(ycount(vetp)-yb)) / SUM(ycount(vetp)) ) * 100.
!corr               write(0,*)'NCOEF2=',nbk, rpback,rpback1
!corr               rpback = rpback1
!
!     Seleziona errep minimo
      if (rpback < rpbmin) then
          rpbmin = rpback
          ncoef = nbk
          coefb(:nbk) = coefbkg0(:nbk)
      endif
   enddo
!
!  Calcola il background
   call getbackground(typeback,bcount,xcount,coefb(:ncoef),thzerob)
!
   end subroutine autobackground

  !---------------------------------------------------------------------------------------

   subroutine piazza_punti(xval,yval,npoints,points,wave)
!
!  Cerca i punti i background
!
   USE pointmod
   USE Counts
   real, dimension(:), intent(in)                             :: xval,yval ! dove piazzare i punti di background
   integer, intent(out)                                       :: npoints   ! num. di punti
   type(point_type), dimension(:), allocatable, intent(inout) :: points    ! punti di background
   real, intent(in)                                           :: wave
   real                                                       :: size_window
   integer                                                    :: minpoint
   integer                                                    :: nval
   type(point_type), allocatable, dimension(:)                :: dati
   integer                                                    :: nplim
   real, parameter                                            :: SWIN_CU = 2.0   ! default window size for Cu wavelength
   real                                                       :: tstep
   integer                                                    :: sizep
!
!   yval(:) = yvali(:)  ! prova-elimina poi yvali
     !call bruckner_back(yvali,yval,100,20)
!
   nval = size(xval)
   !minpoint = min(40,nval)
   minpoint = min(10,nval)
!
!  Definisci l'ampiezza di una finestra (in gradi) nella quale piazzare il punto
!
   !size_window = 2.0
   !size_window = deltatval(SWIN_CU,0.085,wave)
   !  write(0,*)'SIZE=',size_window
   size_window = delta_from_lambda(SWIN_CU,1.54056,wave,4.0)
     !write(0,*)'SIZE=',size_window
   !size_window = deltatval(2.0,0.040,wave)
      !write(0,*)'size=',size_window
      !write(0,*)'size=',deltadval(4.0,0.15,1.54056)
      !write(0,*)'size=',deltatval(4.0,deltadval(4.0,0.15,1.54056),wave)
!
!!!!!!!!!!!!test
!
!  Calculate amplitude of window in terms of number of points
   tstep = sum(xval(2:nval) - xval(1:nval-1))/(nval-1)
   sizep = nint(size_window/tstep)
   !write(0,*)'npoints=',npoints
!!!!!!!!!!!!test
   !xpmin = minval(xval)
   !xpmax = maxval(xval)
   !npoints = max(nint((xpmax - xpmin) / size_window),minpoint)
   !npoints = min (npoints,nval)  ! npoints non puo' essere piu grande di nval
             !write(0,*)'lim=',xpmin,xpmax,npoints,minpoint,nval
!
!  Piazza i punti
   allocate(dati(size(xval)))
   dati%x = xval
   dati%y = yval
   call getminpoints_old(xval,yval,npoints,points,sizep)
   !call filtra_punti(points,4,0.1); npoints = size(points)
   !call getminpoints(dati,npoints,points)
   !write(0,*)'punti prima=',npoints
   !do i=1,min(npoints,10)
   !   write(0,*)i,points(i)
   !enddo
!
!  Primo e ultimo punto del pattern
   if (points(1)%x /= dati(1)%x) then           ! aggiungi solo se non esiste gia'
       call add_point(points,dati(1),1)
       npoints = npoints + 1                  ! incrementa npointback
   endif
   if (points(npoints)%x /= dati(nval)%x) then  ! aggiungi solo se non esiste gia'
       call add_point(points,dati(nval))
       npoints = npoints + 1                  ! incrementa npointback
   endif
!
!  Aggiungi punti agli estremi per stabilizzare il polinomio
   nplim = 3
   if (npoints >= nplim) then
       call insert_points(points,1,nplim,dati,0.5)
       npoints = size(points)
   endif
   !write(0,*)'punti dopo=',npoints
   !do i=1,min(npoints,10)
   !   write(0,*)i,points(i)
   !enddo
   nplim = 3
   if (npoints >= nplim*3) then
       call insert_points(points,npoints-nplim,npoints,dati,0.5)
       npoints = size(points)
   endif
!
   npoints = size(points)
!
   end subroutine piazza_punti

 !------------------------------------------------------------------------------------

   subroutine insert_points(points,ipos,fpos,dati,freq)
!
!  Inserisci punti fra ipos e fpos
!
   USE pointmod
   USE nr
   USE math_util
   USE arrayutil
   type(point_type), dimension(:), allocatable, intent(inout) :: points
   integer, intent(in)                                        :: ipos,fpos
   type(point_type), dimension(:), intent(in)                 :: dati
   real, intent(in)                                           :: freq
   integer, parameter                                         :: NPMAX = 5
   type(point_type), dimension(:), allocatable                :: pdat
   type(point_type), dimension(:), allocatable                :: padd
   integer, dimension(:), allocatable                         :: iorder
   integer                                                    :: nadd,npadd
   integer                                                    :: i,j
   integer                                                    :: loc
   real                                                       :: diff
   real                                                       :: rangey
!
   allocate(padd(NPMAX*(fpos-ipos)))
   nadd = 0
   rangey = abs(points(fpos)%y - points(ipos)%y)
!!!equazione della retta per fpos e ipos come y=mx+c
   !mretta = (points(fpos)%y - points(ipos)%y) / (points(fpos)%x - points(ipos)%x)
   !cretta = -mretta
!corr   retta = straight_line(points(fpos)%x,points(fpos)%y,points(ipos)%x,points(ipos)%y)
     !write(0,*)'retta=',points(ipos)
     !write(0,*)'retta=',points(ipos)%x*retta(1) + retta(2)
!!!
!corr       write(0,*)'rangey=',rangey
   do i=ipos,fpos-1
!
!     valuta il numero di punti da aggiungere
      npadd = nint(min((points(i+1)%x - points(i)%x) / freq,real(NPMAX)))
      if (npadd == 0) npadd = 1
      call new_points(pdat,npadd)
!
!     genera npadd punti
      call create_point(points(i),points(i+1),pdat(:npadd))
!
!     Controllo per i punti al di sopra dell'osservato (dati)
      do j=1,npadd
         loc = clocate(dati%x,pdat(j)%x)
         diff = pdat(j)%y - dati(loc)%y
         if (diff > 0) then           ! punto in alto
             pdat(j)%y = dati(loc)%y 
               !write(0,*)'alto ',j,pdat(j)
         else                         ! punto troppo in basso
             if ((dati(loc)%y <= max(points(ipos)%y,points(fpos)%y)) .or.  &   !point is below the range ipos-fpos
              (abs(diff) < 0.4*rangey)) then
                 pdat(j)%y = dati(loc)%y ! la soluzione migliore e' un controllo
             else
                 cycle
             endif
       !old      if (abs(diff) < 0.4*rangey) pdat(j)%y = dati(loc)%y ! la soluzione migliore e' un controllo
               !write(0,*)'diff',diff,'punto ',j,pdat(j)          ! sull'osservato
     !write(0,*)'retta=',pdat(j)%x*retta(1) + retta(2)
         endif
         nadd = nadd + 1
         padd(nadd) = pdat(j)
      enddo
   enddo
   if (nadd > 0) then
      call add_points(points,padd(:nadd),ipos)
      allocate(iorder(size(points)))
      call indexx(points%x,iorder)
      points(:) = points(iorder)
   endif
!
   end subroutine insert_points

 !------------------------------------------------------------------------------------

   subroutine create_point(point1,point2,pointn)
!
!  Crea n punti tra point1 e point2
!
   type(point_type), intent(in)                :: point1,point2
   type(point_type), dimension(:), intent(out) :: pointn
   real                                        :: stepx,stepy
   integer                                     :: i
   integer                                     :: np
!
   np = size(pointn)     ! numero di punti da creare
   stepx = (point2%x - point1%x) / (np+1)
   stepy = (point2%y - point1%y) / (np+1)
   do i=1,np
      pointn(i)%x = point1%x + i*stepx
      pointn(i)%y = point1%y + i*stepy
   enddo
!
   end subroutine create_point

 !------------------------------------------------------------------------------------

   subroutine getminpoints_old(xvet,yvet,np,punti,wind)
!
!  Cerca i minimi di un vettore
!
   USE pointmod
   integer, intent(out)            :: np
   real, dimension(:), intent(in)    :: xvet, yvet
   type(point_type), dimension(:), allocatable, intent(inout) :: punti
   integer, intent(in)               :: wind
   integer                           :: i   !!!,wind
   integer, dimension(1)             :: loc
   integer                           :: minp,maxp,posm,posmold
   integer                           :: npoint
   integer                           :: nsizep, nplus
!
   npoint = size(xvet)
   !corrwind = size(xvet) / np   ! ampiezza della finestra
!
!  Rialloca su npp senza salvare il suo contenuto
   nplus = max(2,npoint/10)
   nsizep = nplus
   call new_points(punti,nsizep)
!
   posmold = 1
   maxp = 0
   i=0
   do
      i=i+1
      if (nsizep < i) then
          nsizep = nsizep + nplus
          call resize_points(punti,nsizep)
      endif          
      minp = 1 + maxp
      maxp = min(minp + wind - 1,npoint)
      punti(i)%y = MINVAL( yvet(minp : maxp) )
      loc = MINLOC( yvet(minp : maxp) )
      posm = loc(1) + minp - 1
      !write(25,*)'punto n.',i,minp,maxp,posm
      !if ((posm == minp .or. posm == maxp) .and. (minp /= 1)) then
      if ((posm == minp .or. posm == maxp)) then
          !write(25,*)'       punto rigettato'
          minp = max(posm - wind/2,posmold+1)
          maxp = min(minp + wind - 1,npoint)
          punti(i)%y = MINVAL( yvet(minp : maxp) )
          loc = MINLOC( yvet(minp : maxp) )
          posm = loc(1) + minp - 1
          !write(25,*)'punto n.',i,minp,maxp,posm
          !if (posm == minp)write(25,*)'punto forse non valido'
      endif
      posmold = posm
      punti(i)%x = xvet(posm)
      if (maxp == npoint) exit
   enddo
   np = i
!
   call resize_points(punti,np)
!
   end subroutine getminpoints_old

 !------------------------------------------------------------------------------------

   subroutine getminpoints(dati,np,punti)
!
!  Cerca i minimi nel vettore dati
!   
!corr   USE smoothing
   USE stat
   USE peak_util
!
   type(point_type), dimension(:), intent(in) :: dati
   integer, intent(out)                  :: np
   type(point_type), allocatable, intent(out) :: punti(:)
   integer                               :: npoint
!corr   integer                               :: i
!
   npoint = size(dati)
!
!  Cerca i minimi (ovvero i massimi di -point) dallo studio della derivata prima
!corr   call findmaxd1((/(point_type(dati(i)%x,-dati(i)%y),i=1,npoint)/),20,punti,np)
   call findmaxd1(dati(:)%x,-dati(:)%y,20,punti,np)
   punti%y = -punti%y
!   
!  Filtra i punti
   call filtra_punti(punti,4,0.1)
   np = size(punti)
!
   end subroutine getminpoints

  !-----------------------------------------------------------------------------

   subroutine findmaxd1_old(dati,npoints,pk,np)
   USE ssmoothing
   USE pointmod
!
   type(point_type), dimension(:), intent(in)     :: dati
   integer, intent(in)                :: npoints
   type(point_type), allocatable, intent(inout) :: pk(:)
   integer, intent(out)               :: np
   real, dimension(size(dati))       :: ders
   integer                            :: ndat
   integer                            :: i
   integer                            :: ini,fin
   integer, dimension(1)              :: locm
   integer, parameter                 :: nplus = 100
   integer                            :: nsizep
   integer                            :: pos,posnew
   integer                            :: nprange
!
!  nsizep è un ragionevale valore iniziale di allocazione per xp e yp
   nsizep = nplus
!
!  Rialloca su npp senza salvare il suo contenuto
   call new_points(pk,nsizep)
!
!  calcola smoothing della derivata prima
   ders = savgol_smooth(dati%y,npoints,npoints,1,2)  ! pol=5 prima
!
   ndat = size(dati)     ! num. di dati
!
!  selezione i punti in cui la derivata da positiva diventa negativa
   np = 0
   nprange = nint(npoints*0.5)
   pos = -1     ! inizializza pos 
   do i=2,ndat
      if (ders(i) < 0 .and. ders(i-1) > 0) then          
!
!         seleziona il + grande nell'intorno definito da npoints         
          ini = max(1,i-nprange)
          fin = min(ndat,i+nprange)
          locm = maxloc(dati(ini:fin)%y)
          posnew = locm(1)+ini-1
          if (posnew /= pos) then  ! potrebbe selezionare un punto già preso prima
              pos = posnew
              np=np+1
              if (nsizep < np) then
                  nsizep = nsizep + nplus
                  call resize_points(pk,nsizep)
              endif          
              pk(np) = dati(pos)              
          endif
      endif
   enddo
!
   call resize_points(pk,np)
!
   end subroutine findmaxd1_old

 !------------------------------------------------------------------------------------

   subroutine filtra_punti(punti,nad,sog)
   USE stat
   USE pointmod
!
!  Filtra punti rimuovendo quelli che hanno un valore superiore alla media dei punti adiacenti
!
   type(point_type), allocatable, intent(inout) :: punti(:) ! i punti da filtrare
   integer, intent(in)                     :: nad      ! num. di punti adiacenti
   real, intent(in)                        :: sog      ! soglia di rimozione punti
   integer                                 :: i
   !type(point_type), dimension(size(punti)) :: puntir ! i punti da filtrare
   real, dimension(size(punti)) :: yvalr ! i punti da filtrare
   real                                    :: yave,yavel,yaver
   integer :: npunti
!corr   real :: ymax
!   
!-------- sostituire puntir con real yvalr
   npunti = size(punti) 
!
!  normalizza le intensita'
   yvalr(:) = punti(:)%y - minval(punti(:)%y)
   yvalr(:) = yvalr(:)/maxval(yvalr)
   i = nad
   do 
      i = i + 1
      if (i > npunti - nad) exit
      yavel = sum(yvalr(i-nad:i-1))/nad                    ! media a sinistra
      yaver = sum(yvalr(i+1:i+nad))/nad                    ! media a destra
      if (yvalr(i) > yavel .and. yvalr(i) > yaver) then  ! Il punto è piu' in alto rispetto alle medie 
          yave = (yavel + yaver)/2
              write(0,*)'punty',punti(i)%x,yvalr(i),i,yavel,yaver
              write(0,*)'punty',punti(i)%x,yvalr(i),i,(yvalr(i) - yave),sog
!
!         Rimuovi il punto i se la differenza relativa rispetto alla media e > di sog          
          if (yvalr(i) - yave > sog) then 
              write(0,*)'eliminato punto ',yvalr(i),punti(i)%x,i
              yvalr(i:npunti-1) = yvalr(i+1:npunti)
              punti(i:npunti-1) = punti(i+1:npunti)
              npunti = npunti - 1
              i = i - 1
          endif
      else
              write(0,*)'puntn',punti(i)%x,yvalr(i),i,yavel,yaver
      endif
   enddo
!
!  Ricompatta i punti a npunti
   call resize_points(punti,npunti)
!
   end subroutine

  !-----------------------------------------------------------------------------

   subroutine param_back(itype,bobs,ncoef,coef,tmin,tmax,th0)
!
!  Calcola coefficienti di background della curva passante per i punti bobs
!
   USE nr; USE nrtype
   USE GENLSQ
   USE trig_constants
   USE type_constants, only: DP
!
   integer, intent(in)                        :: itype
   type(point_type), dimension(:), intent(in) :: bobs
   integer, intent(in)                        :: ncoef
   real, dimension(ncoef), intent(out)        :: coef
   real, intent(in)                           :: tmin,tmax
   real, intent(in), optional                 :: th0
   integer                                    :: i,j,kk
   real, dimension(36)                        :: tt
   real                                       :: xche
   real(DP), dimension(size(bobs),ncoef)      :: am
   real(DP), dimension(size(bobs))            :: bm
   real(DP), dimension(ncoef)                 :: x
   integer                                    :: np
   integer                                    :: ier
!
   np = size(bobs)
   bm = bobs(:np)%y
!
   select case (itype)

    case (POLY)
         am(:,1) = 1.0
         do i=2,ncoef
            am(:,i) = (bobs(:)%x/th0 - 1.0)**(i-1)
         enddo

    case (CHEBY)
         am(:,1) = 0.5
         tt(1) = 1.0
         do i=1,np
            xche = 2.0*(bobs(i)%x - tmin)/(tmax-tmin) - 1.0
            tt(2) = xche
            do j=2,ncoef-1
               tt(j+1) = 2.*xche*tt(j) - tt(j-1)
            enddo
            do kk=2,ncoef
                am(i,kk) = tt(kk)
            enddo
         enddo

    case (FOUR_SERIES)
         do i=1,ncoef
            am(:,i) = cos((i-1)*bobs(:)%x*dtor)
         enddo

   end select

   call solve_linlsq(am,bm,x,ier)
   coef(:ncoef) = real(x)
     !write(0,*)'IER=',ier,itype,FOUR_SERIES
     !write(0,*)'coef=',x
!
   end subroutine param_back

  !-------------------------------------------------------------------------------------

   subroutine getbackground(itype,backg,xback,coef,th0)
!
!  Calcola il background nei punti xback noti i coefficienti
!
   USE nr
   USE trig_constants
!
   integer, intent(in)             :: itype      ! tipo di background
   real, dimension(:), intent(out) :: backg      ! background
   real, dimension(:), intent(in)  :: xback      ! x del punto di background
   real, dimension(:), intent(in)  :: coef       ! coefficienti di background
   integer                         :: np         ! num. di punti in cui calcolare il background
   integer                         :: ncoef      ! num. di coefficienti
   real                :: tmin,tmax
   real, intent(in), optional      :: th0
   integer :: i,j,k
   real    :: thy
   real    :: aa,bb,xche
!
   np = size(backg)
   ncoef = size(coef)
!
   select case (itype)

      case (POLY)
       do j=1,np
          thY=xback(j)/th0 - 1.0
          backg(j)= coef(1)
          do k=2,ncoef
             backg(j)= backg(j)+coef(k)*thY**(k-1)
          enddo
       enddo

     case (CHEBY)
       tmin = xback(1)
       tmax = xback(np)
       aa = -1.0
       bb = 1.0
       do i=1,np
          xche = 2.0*(xback(i) - tmin)/(tmax-tmin) - 1.0
          backg(i) = chebev(aa,bb,coef(:ncoef),xche)
       enddo

     case (FOUR_SERIES)
       backg = 0.0
       do i=1,np
         do j=1,ncoef
            backg(i) = backg(i) + coef(j)*cos((j-1)*xback(i)*dtor)
         enddo
       enddo

   end select
!
   end subroutine getbackground

  !-------------------------------------------------------------------------------------

   subroutine bruckner_back(xcount,ycount,bcount,ncicli,sizew,tmin,tmax)
   use arrayutil
!
   real, dimension(:), intent(in)  :: xcount,ycount    ! x,y
   real, dimension(:), intent(out) :: bcount           ! background
   integer, intent(in)             :: ncicli
   integer, intent(in)             :: sizew
   real, intent(in), optional      :: tmin,tmax
   !real                            :: media_conteggi
   real                            :: soglia
   integer                         :: ncmin,ncmax !,nctot
!
   if (present(tmin) .and. present(tmax)) then
       ncmin = clocate(xcount,tmin)   
       ncmax = clocate(xcount,tmax)  
   else
       ncmin = 1
       ncmax = size(xcount)
   endif
   !nctot = ncmax - ncmin + 1
   bcount(ncmin:ncmax) = ycount(ncmin:ncmax)             ! inizializza background
!
!  Poni il background uguale a soglia per tutti i punti > di soglia
   !media_conteggi = sum(bcount(ncmin:ncmax)) / nctot
   !soglia = media_conteggi  + 2*(media_conteggi - minval(bcount(ncmin:ncmax)))
   soglia = (maxval(bcount(ncmin:ncmax)) + minval(bcount(ncmin:ncmax))) / 2.0
   where (bcount(ncmin:ncmax) > soglia) bcount(ncmin:ncmax) = soglia
!
   call smoothback(bcount(ncmin:ncmax),ncicli,sizew)
!
   end subroutine bruckner_back

  !-------------------------------------------------------------------------------------

   subroutine smoothback(bcount,ncicli,sizew)
   USE ssmoothing
!
   real, dimension(:), intent(inout) :: bcount    ! background
   integer, intent(in)               :: ncicli
   integer, intent(in)               :: sizew
   real ,dimension(size(bcount))     :: back_smooth
   integer                           :: i
!
   do i=1,ncicli
      back_smooth(:) = averagesmooth(bcount,sizew)                     ! esegue smoothing
      where( bcount(:) > back_smooth(:) ) bcount(:) = back_smooth(:)   ! prende la minore delle 2 intensita'
   enddo
!
   end subroutine smoothback

  !-----------------------------------------------------------------------------------

   subroutine genspline(xpc,pointx,pointy,ypc)
   USE nr
!
   real, dimension(:), intent(in)  :: xpc
   real, dimension(:), intent(in)  :: pointx,pointy
   real, dimension(:), intent(out) :: ypc
   real, dimension(size(pointx))   :: yd2
   integer                         :: npunti
   integer             :: i
   real                            :: yp1,ypn
!
   npunti = size(pointx)
   yp1 = (pointy(2) - pointy(1)) / (pointx(2) - pointx(1))
   ypn = (pointy(npunti) - pointy(npunti-1)) / (pointx(npunti) - pointx(npunti-1))
   call spline(pointx,pointy,0.0,0.0,yd2)
   do i=1,size(xpc)
      ypc(i) = splint(pointx,pointy,yd2,xpc(i))
   enddo
!
   end subroutine genspline

  !-----------------------------------------------------------------------------------

   subroutine genbezier(xpc,point,ypc)
   USE fattoriale_bezier
   USE nr
   USE type_constants, only: DP
!
   real, dimension(:), intent(in)        :: xpc      ! punti in cui calcolare la curva di Bezier
   type(punto), dimension(:), intent(in) :: point    ! punti di controllo della curva
   real, dimension(:), intent(out)       :: ypc      ! valore del polinomio di Bezier in xpc
   integer                               :: nsample
   real(DP)                              :: uval
   integer                               :: i
   integer                               :: j,klo,khi
   real(DP)                              :: h,a,b
   type(punto), allocatable              :: bez(:)
!
!  Variando uval da 0 a 1 campiono la curva in nsample punti
!
   nsample = size(point)*50               ! num. di campionamenti sulla curva
   allocate(bez(nsample))
   do i=1,nsample
      uval = real(i-1,8)/real(nsample-1,8)
      bez(i) = point.bezier.uval
   enddo
!
!  Utilizzo la 'bisezione' per calcolare i valori di ypc
!
   do i=1,size(xpc)
      j = locate(real(bez%x),xpc(i))
      if (j == 0) j = 1                 ! j = 0  se ho eliminato il punto di controllo nel minimo
      if (j == nsample) j = nsample - 1 ! in questo caso ho eliminato il punto di controllo nel massimo
      klo = j
      khi = j+1
      h = bez(khi)%x - bez(klo)%x
      a = (bez(khi)%x - xpc(i)) / h
      b = (xpc(i) - bez(klo)%x) / h
      ypc(i) = real(a*bez(klo)%y + b*bez(khi)%y)
   enddo
!
   end subroutine genbezier

!-------------------------------------------------------------------------------------------------

   subroutine set_background_from_string(bcond,string,ier) 
!
!  Extract from string: type ncoef; auto is true if ncoef is 0
!
   use strutil
   character(len=*), intent(in)             :: string
   type(back_condition_type), intent(inout) :: bcond
   integer, intent(out)                     :: ier
   integer, dimension(:), allocatable       :: ivet
   integer                                  :: iv
!
   ier = 1
   call Getnum1(string,ivet=ivet,iv=iv)
   if (err_string) return
   if (iv /= 2) return
   if (ivet(1) < 1 .and. ivet(1) > 3) return
   
   bcond%btype = ivet(1)
   if (ivet(2) >= 1 .and. ivet(2) <= NMAXCOEFB) then
       bcond%auto = .false.
       bcond%ncoef = ivet(2)
   else
       bcond%auto = .true.
   endif

   ier = 0
   return
!
   end subroutine set_background_from_string


 END MODULE Background
