 MODULE GENLSQ

 USE type_constants, only:DP

 implicit none

 private

 public  :: Allocafuncls,AllocaLSQ,lsqsolve,levmarq_algo,gaussn_algo,solve_linlsq, &   !!!,kptime,    &
            provainv,stampa_matrici_lsq,solve_linlsq_bcg,scalefit,polyfit,calculate_sd

 private :: applica_line_search,fullnew_algo,invltrimatrix

 real(DP), allocatable, public      :: funcls(:)         ! funzione costo = transpose(funcls)*funcls
 real(DP), allocatable, public      :: weilsq(:)         ! pesi
 real(DP), allocatable, public      :: jacob   (:,:)     ! jacobiano di funcls
 real(DP), allocatable, public      :: xpar(:)           ! paramatri correnti
 real(DP), allocatable, public      :: xparold(:)        ! parametri al ciclo precedente
 real(DP), public                   :: chisquarednow     ! chiquadro corrente
 real(DP), public                   :: chisquared_old    ! chiquadro al ciclo precedente
 integer, public                    :: termcode          ! codice di terminazione dell' algoritmo
 integer, public                    :: nclev             ! numero di cicli
 real(DP), allocatable, public      :: typex(:)          ! scala sui parametri
 integer, public                    :: ncytot            ! num. cumulativo di cicli di l.sq.
 real(DP), allocatable, public      :: grad(:)           ! gradiente
 real(DP), allocatable, public      :: hess(:,:)         ! hessiano
 real(DP), allocatable, public      :: matinv(:,:)       ! 
 real(DP), allocatable, public      :: shiftx(:)         ! shift sui parametri
 real(DP), public                   :: step_length       ! step length
 character(40), allocatable, public :: par_string(:)     ! stringa associata al parametro
 logical, public                    :: sdavail = .false. ! Are sd available?
!
!Algoritmi di ottimizzazione utilizzabili   
 integer, parameter, public         :: NL2SOL = 1     ! file: nl2sol.f90    (quasi-newton)
 integer, parameter, public         :: LMDER  = 2     ! file: marquardt.f90 (levenberg-marquardt)
 integer, parameter, public         :: GAUNEW = 3     ! file: genlsq.f90    (gauss-newton modified)

 CONTAINS

   subroutine Allocafuncls(alloca,np,savev)
   USE arrayutil
!
   integer, intent(in)           :: alloca
   integer, intent(in), optional :: np
   integer, dimension(2)         :: ierv
   logical, intent(in), optional :: savev
   logical                       :: savevet
!
   ierv = 0
   if (present(savev)) then
       savevet = savev
   else
   endif
   savevet = .false.
   if (alloca == 1) then
     call resize_array(funcls,np,savevet)
     call resize_array(weilsq,np,savevet)
   else
     if (allocated(funcls)) then
         deallocate (funcls,weilsq, stat=ierv(1))
         if (ierv(1) /= 0) stop 'funcls deallocation error'
     endif
   endif
!
   end subroutine Allocafuncls

!----------------------------------------------------------------------------------

   subroutine AllocaLSQ(alloca,num_param,noss)
!
   implicit none
   integer, intent(in)           :: alloca    ! azione 
   integer, intent(in), optional :: num_param ! numero di parametri
   integer, intent(in), optional :: noss      ! numero di osservazioni (include anche restraints)
   integer, dimension(9)         :: ierv
!
   ierv = 0
   if (alloca == 1) then
     if (.not. allocated(grad))       allocate (grad(1:num_param),               stat=ierv(1))
     if (.not. allocated(hess))       allocate (hess(1:num_param,1:num_param),   stat=ierv(2))
     if (.not. allocated(matinv))     allocate (matinv(1:num_param,1:num_param), stat=ierv(3))
     if (.not. allocated(shiftx))     allocate (shiftx(1:num_param),             stat=ierv(4))
     if (.not. allocated(par_string)) allocate (par_string(1:num_param),         stat=ierv(5))
     if (.not. allocated(jacob))      allocate (jacob(1:noss,1:num_param),       stat=ierv(6))
     if (.not. allocated(xpar))       allocate (xpar(1:num_param),               stat=ierv(7))
     if (.not. allocated(xparold))    allocate (xparold(1:num_param),            stat=ierv(8))
     if (.not. allocated(typex))      allocate (typex(1:num_param),              stat=ierv(9))
     if (sum(iabs(ierv)) /= 0) stop 'LSQ allocation error'
   else
     if (allocated(grad)) then
         deallocate (grad,hess,matinv,shiftx,par_string,jacob,xpar,xparold,typex, stat=ierv(1))
         if (ierv(1).ne.0) stop 'LSQ deallocation error'
     endif
   endif
!
   end subroutine AllocaLSQ

!-------------------------------------------------------------------------------

   subroutine lsqsolve(kalgo,nf,np,nlsmax,eps,funcg,funcl,funcn1,funcn2,wina,kevent,stoplsq,sprint)
!
   integer, intent(in)  :: kalgo         ! tipo di algoritmo
   integer, intent(in)  :: nf            ! numero di funzioni
   integer, intent(in)  :: np            ! numero di parametri
   integer, intent(in)  :: nlsmax        ! numero massimo di cicli
   real, intent(in)     :: eps           ! parametro di convergenza
   logical, intent(in)  :: wina          ! se .false. la messaggistica risulta disattivata
   integer, intent(out) :: stoplsq       ! se diverso da 0 l'algoritmo si � fermato per parametri fuori range
   character(len=*), intent(in), optional :: sprint ! opzioni di stampa
   integer, intent(out) :: kevent        ! eventi da grafica  
   external funcg,funcl,funcn1,funcn2
   integer              :: ier
!
   ier = 0
   stoplsq = 0
   kevent = 0
   step_length = 1.0  !FIXME-only for GAUNEW
   select case(kalgo)

        case (LMDER)
             call levmarq_algo(funcl,np,nf,2,wina)

        case (GAUNEW)
             call gaussn_algo(funcg,np,nlsmax,eps,sprint,kevent,stoplsq)

        case (NL2SOL)
             call fullnew_algo(nf,np,funcn1,funcn2,kevent,stoplsq)

   end select
!
   end subroutine lsqsolve

   !---------------------------------------------------------------------------

   subroutine levmarq_algo(fcn,Nactot,nfun,mode,kmess)
!
   external fcn
   integer, intent(in)          :: Nactot
   integer, intent(in)          :: mode
   logical, intent(in)          :: kmess
   integer, intent(in)          :: nfun
   real(DP)                  :: tol
   integer                      :: info
!
   sdavail = .false.
   nclev = -1
   tol = 0.000001D+00
   call lmder1 ( fcn, nfun, Nactot, xpar, funcls, jacob, nfun, tol, 1.0/typex, mode, info )
!
   termcode = 0
!   if (kmess) then
!       write(Inte,'(a20,1x,i2)')'Termination criteria',info
       select case (info)

         case (1:4)
           termcode = 1

         case (-1,5:8)
           termcode = 0

       end select
!   endif
!
   end subroutine levmarq_algo

   !---------------------------------------------------------------------------

   subroutine applica_line_search(fcn,Nactot,eps,kpr,errline)
!
!corr   USE General, only:lo
   USE math_util
!   USE, intrinsic :: ieee_arithmetic
   USE strutil, only:r_to_s,d_to_s
   interface 
     subroutine fcn(iflag,npar,nls,err)
     integer, intent(in)            :: iflag
     integer, intent(in)            :: npar
     integer, intent(in), optional  :: nls
     integer, intent(out), optional :: err
     end subroutine fcn
   end interface
   integer, intent(in)    :: Nactot
   real, intent(in)       :: eps      ! parametro di convergenza
   integer, intent(in)    :: kpr      ! se > 0 stampa informazioni
   integer, intent(out)   :: errline
!corr   integer                :: kpr
   integer                :: ifirstcycle
   real                   :: alfastep,step_min,step_max
   real                   :: initslope
   real                   :: step_length_temp
   real                   :: step_length_pre,chisquared_pre
   real(DP)               :: disc
   real(DP), dimension(2) :: cubic(2)
   real, dimension(2,2)   :: vetcubic1
   real, dimension(2)     :: vetcubic2
   integer                :: niter,nmaxiter
   integer                :: err
!
!  Inizializzazioni
!corr   if (present(kpr0)) then
!corr        kpr = kpr0
!corr   else
!corr        kpr = 0
!corr   endif
   errline = 0
   termcode = 0
   alfastep = 0.0001
   step_min = eps / MAXVAL(abs(shiftx)/max(abs(xpar),typex))   ! valore minimo dello steplenght
   step_max = 1.0                                              ! valore massimo dello steplenght
   step_length = step_max                                      ! inizializza step_length al valore massimo
   ifirstcycle = 1
!
   if (kpr > 0) then
       write(kpr,'(/,12x,a,/)')'----------------------------------------------------------'
       write(kpr,'(3x,a39,i4)')'Least squares informations for cycle n. ',ncytot
       write(kpr,*)
       write(kpr,*)'  Line search'
       write(kpr,'(a33,g12.4)')'   Minimum allowable steplenght: ',step_min
       write(kpr,*)
       write(kpr,*)'    Steplenght          Chisquared         Difference       Info'
       write(kpr,*)
   endif
!
!  Calcola derivata del chiquadro per step_length = 0
   initslope = dot_product (shiftx,grad)
   if (initslope > 0.0) then
       if (kpr > 0) write(kpr,*)'WARNING, initslope= ',initslope
       !step_length = 0.0    ! ripristino dei vecchi parametri
       call fcn(Nactot,-2)   
       termcode = -1
       return
   endif
!
   nmaxiter = 20  ! numero massimo di iterazioni
   niter = 0
   do 
       niter = niter + 1
!
!      Applica shifts e calcola chiquadro
       call fcn(Nactot,1,err=err)
       !err = 0
         !if (ierprof /= 0) write(0,*)'IERPROF_L=',ierprof
!
!      Controllo per numero massimo di iterazioni e NaN, generati da B grandi ad es.
       if (niter > nmaxiter .or. is_nan(real(chisquarednow)) .or. err /= 0) then
           if (kpr > 0) write(kpr,'(a,g14.6)')'WARNING, cannot find step length. step_length=0.0, chi2=',chisquarednow
           !step_length = 0   ! ripristino dei vecchi parametri
           !shiftx = 0.0
           !call fcn(Nactot,1)
           termcode = -1
           errline = 1
           exit
       endif
!
       if (chisquarednow <= chisquared_old + alfastep * step_length * initslope) then
!
!         Se il nuovo chiquadro (chisquarednow) e' minore del vecchio (chisquared_old)
!         ho trovato lo step_length ed esco
          if (kpr > 0) write(kpr,10)step_length,chisquarednow,chisquarednow-chisquared_old,'success'
          exit
       else if ( step_length < step_min ) then
!
!         Lo step_length e' troppo piccolo, ripristino ed esco.
          if (kpr > 0) write(kpr,10)step_length,chisquarednow,chisquarednow-chisquared_old,'too small'
          !step_length = 0   ! ripristino dei vecchi parametri
          call fcn(Nactot,-2)
          termcode = -1
          shiftx = 0.0
          exit
       else
          if ( ifirstcycle == 1) then
               if (kpr > 0) write(kpr,10)step_length,chisquarednow,chisquarednow-chisquared_old
!
!              La prima volta eseguo un fit con una quadrica
               step_length_temp = -initslope / ( 2 * ( chisquarednow - chisquared_old - initslope))
               ifirstcycle = 0
               if (kpr > 0) write(kpr,10)step_length_temp,chisquarednow,chisquarednow-chisquared_old,'quadric'
          else
!
!              Le volte successive eseguo un fit con una cubica
               vetcubic1(1,1) =   1.0 / step_length ** 2
               vetcubic1(1,2) = - 1.0 / step_length_pre ** 2
               vetcubic1(2,1) = - step_length_pre / step_length ** 2
               vetcubic1(2,2) =   step_length / step_length_pre ** 2
               vetcubic2(1)   =   chisquarednow - chisquared_old - step_length * initslope
               vetcubic2(2)   =   chisquared_pre - chisquared_old - step_length_pre * initslope
               cubic = (1.0/(step_length - step_length_pre)) * matmul(vetcubic1,vetcubic2)
               disc = cubic(2)**2 - 3.0*cubic(1)*initslope
               if (abs(cubic(1)) <= epsilon(cubic(1))) then
                   step_length_temp = - initslope / (2*cubic(2))   ! la cubica e' una quadrica
               else
                   step_length_temp = (-cubic(2) + sqrt(disc)) / (3.0*cubic(1))
               endif
               if (kpr > 0) write(kpr,10)step_length_temp,chisquarednow,chisquarednow-chisquared_old,'cubic'
               if (is_nan(real(step_length_temp))) then
                   termcode = -1
                   errline = 1
                   exit
               endif
          endif
          step_length_pre = step_length
          chisquared_pre  = chisquarednow
          if ( step_length_temp <= 0.1 * step_length ) then
               step_length = 0.1 * step_length
               if (kpr > 0) write(kpr,10)step_length,chisquarednow,chisquarednow-chisquared_old,'modified'
          else
               step_length = step_length_temp
          endif

       endif

   enddo
!
10 format(5x,g12.4,2x,es16.4,2x,g16.4,8x,a)
!
   end subroutine applica_line_search

   !---------------------------------------------------------------------------

   subroutine gaussn_algo(fgauss,Nactot,nlsmax,eps,strp,kevent,stoplsq)
   USE lsqalgo
   USE enable_amb, only: event_from_gui, STOP_EVENT, SKIP_EVENT
   interface 
     subroutine fgauss(iflag,npar,nls,err)
     integer, intent(in)            :: iflag
     integer, intent(in)            :: npar
     integer, intent(in), optional  :: nls
     integer, intent(out), optional :: err
     end subroutine fgauss
   end interface
!
   integer, intent(in)          :: Nactot       ! numero di parametri
   integer, intent(in)          :: nlsmax       ! numero massimo di cicli
   real, intent(in)             :: eps          ! eps di convergenza
   character(len=*), intent(in) :: strp         ! codici per la stampa
   integer, intent(inout)       :: kevent     ! codice di terminazione
   integer, intent(out)         :: stoplsq
   integer                      :: ier   
   integer                      :: i
   integer                      :: nls
   logical                      :: linv
   integer                      :: errline
   integer                      :: error
   integer                      :: errt
   real(DP)                     :: stepl_save
!
   ier = 0
   stoplsq = 0
   !linv = scan(strp,'CE') /= 0
   linv = .false.
   !sdavail = .false.
   sdavail = .true.
!
   call fgauss(Nactot,-1)  ! calcolo iniziale delle funzioni
!
   do nls=1,nlsmax
      ncytot = ncytot + 1
      call fgauss(Nactot,5) ! salvataggio parametri
      call fgauss(Nactot,0) ! calcolo jacobiano
!
      if (Nactot < 100) then
          call solve_linlsq(jacob,-funcls,shiftx,ier,grad,hess,linv,matinv)
      else
          shiftx = xpar
          call solve_linlsq_bcg(jacob,-funcls,shiftx,ier,grad,hess)
      endif

      grad = -2.0*grad     ! gradiente: 2.0*J**T * F 
      hess =  2.0*hess     ! hessiano: 2.0*J**T * J
!
      if (ier < 0) then
          termcode = -2    ! matrix is not positive definite
          !write(6,'(a)')'Warning: hessian matrix is not positive definite'
          !call stampa_matrici_lsq(kpr=6)
          sdavail = .false.
          return
      endif
!
      chisquared_old=chisquarednow
!
      !call calcgradn(Nactot,xpar,0.00000001_DP)
      !call calcgradn(Nactot,xpar,0.00001_DP)
!
      do i=1,5
         stepl_save = step_length
         call applica_line_search(fgauss,Nactot,eps,0,errline)
         call fgauss(Nactot,3,err=error)   ! esegui controlli sui parametri
         !errt = ierprof + error + errline
         errt = error + errline
               !write(0,*)'ERR=',errt,ierprof,error,errline
         if (errt == 0) then
            exit
         else
            if (error == 2) exit
            !write(0,*)i,'riduzione dello shift',chisquarednow,error
            !shiftx = shiftx*0.01   ! 
            shiftx = shiftx*0.1   !fare dei test
         endif
      enddo
!
      if (error > 0) then
          step_length = stepl_save
!
!         Fix parameters out of range, compute sd if required
          call fgauss(Nactot,4,err=error)   ! blocca il parametro fuori range
          !sdavail = .true.
          !write(6,*)"ripristina e riavvia l'algoritmo",chisquarednow
          stoplsq = 1
!qt          call gestione_eventi(kevent)
          kevent = event_from_gui()
          exit
      endif
!
      if (errt > 0) then
          !step_length = 0   ! ripristino dei vecchi parametri
          step_length = stepl_save
          shiftx = 0.0
          call fgauss(Nactot,-2)
      endif
!
      if (termcode == 0) call stopconditions(eps,real(chisquarednow),real(xpar),real(grad),real(shiftx),termcode,0)
!
      call fgauss(Nactot,2,nls)   ! stampa e aggiorna la grafica
!
!qt      call gestione_eventi(kevent)
      kevent = event_from_gui()
      if (termcode /= 0) exit
!
      if (kevent == STOP_EVENT .or. kevent == SKIP_EVENT) exit
   enddo
!
   end subroutine gaussn_algo

   !---------------------------------------------------------------------------

   subroutine calculate_sd(jac,fc,npari,sdvet,ier)
!
!  Calculate standard deviation (sd) from jacobian matrix and function values
!
   USE nr; USE nrtype
   real(dp), dimension(:,:), intent(in)          :: jac    ! jacobian matrix
   real(dp), dimension(:), intent(in)            :: fc     ! function values
   integer, intent(in)                           :: npari  ! number of indipendent parameters
   real(dp), dimension(size(jac,2)), intent(out) :: sdvet  ! sd vet
   real(dp), dimension(size(jac,2))              :: shift
   integer, intent(out)                          :: ier
   real(dp), dimension(size(jac,2),size(jac,2))  :: minv
   real(dp)                                      :: esdd
   integer                                       :: i, noss
!
   call solve_linlsq(jac,-fc,shift,ier,linv=.true.,ainv=minv)
   if (ier == 0) then
       noss = size(fc)    ! number of observations
       esdd = chisquarednow/(noss-npari)
       do i=1,npari
          sdvet(i) = sqrt(minv(i,i)*esdd) !* step_length
       enddo
   else
       sdvet(:) = 0
   endif
!
   end subroutine calculate_sd

   !---------------------------------------------------------------------------
!corr
!corr   subroutine calculate_sd(jac,fc,npari,sdvet,ier)
!corr!
!corr!  Calculate standard deviation (sd) from jacobian matrix and function values
!corr!
!corr   USE nr; USE nrtype
!corr   real(dp), dimension(:,:), intent(in)          :: jac    ! jacobian matrix
!corr   real(dp), dimension(:), intent(in)            :: fc     ! function values
!corr   integer, intent(in)                           :: npari  ! number of indipendent parameters
!corr   real(dp), dimension(size(jac,2)), intent(out) :: sdvet  ! sd vet
!corr   real(dp), dimension(size(jac,2))              :: shift
!corr   integer, intent(out)                          :: ier
!corr   real(dp), dimension(size(jac,2),size(jac,2))  :: minv
!corr   integer                                       :: i
!corr!
!corr   call solve_linlsq(jac,-fc,shift,ier,linv=.true.,ainv=minv)
!corr   if (ier == 0) then
!corr          write(0,*)'ESD:', step_length
!corr       do i=1,npari
!corr          sdvet(i) = sqrt(minv(i,i)*step_length)
!corr       enddo
!corr   else
!corr       sdvet(:) = 0
!corr   endif
!corr!
!corr   end subroutine calculate_sd
!corr
   !---------------------------------------------------------------------------

   subroutine stampa_matrici_lsq(kpr)
   USE PRNUTIL
   USE strutil
!
   integer, intent(in) :: kpr
!
   write(kpr,'(/a)')centra_str('Least squares matrices',80)
   call stampa_matrice(hess,par_string,7,kpr,'Hessian matrix')
   call stampa_vettore(grad,par_string,7,kpr,'Gradient vector')
!
   end subroutine stampa_matrici_lsq

   !---------------------------------------------------------------------------
   subroutine fullnew_algo(nf,np,calcr,calcj,kevent,stoplsq)
!corr   USE progtype
   USE toms573
!                                                   
   integer, intent(in)       :: nf ! num. di funzioni (osservazioni)
   integer, intent(in)       :: np ! num. parametri
   integer, allocatable      :: iv(:) 
   real(DP), allocatable :: v(:) 
   integer                   :: stoplsq
   integer                   :: kevent
   integer                   :: ierror
   integer                   :: np1,np2,np3
   external calcr, calcj
   integer, dimension(2)     :: pstop
!   
   sdavail = .false.
   np1 = 60 + np
   np2 = 93 + np*nf + 3*nf + np*(3*np+33)/2
   np3 = 94 + 2*nf + np*(3*np + 31)/2
   allocate(iv(1:np1),v(1:np2),stat=ierror) 
   v(np3:np3+np-1) = 1/typex
   xparold = xpar
   iv(1) = 0 
   pstop(1) = stoplsq
   !pstop(2) = 0
   call nl2sol(nf,np,xpar,calcr,calcj,calcj,iv,v,pstop)
   !call nl2sol(nf,np,xpar,calcr,calcj,iv,v,pstop) 
   stoplsq = pstop(1)
   kevent = pstop(2)
   termcode = iv(1)
!   write(6,*)'return code=',iv(1)
!
   end subroutine fullnew_algo

   !---------------------------------------------------------------------------

   subroutine solve_linlsq(a,b,x,ier,bbc,atac,linv,ainv)
!                                   T        T
!  Solve min( || ax - b ||2 ) ==> (a a) x = a b
!
   USE nr; USE nrtype
   real(dp), dimension(:,:), intent(in)                            :: a
   real(dp), dimension(:), intent(in)                              :: b
   real(dp), dimension(size(a,2)), intent(out)                     :: x
   integer, intent(out)                                            :: ier
   real(dp), dimension(size(a,2)), intent(out), optional           :: bbc
   real(dp), dimension(size(a,2),size(a,2)), intent(out), optional :: atac
   logical, intent(in), optional                                   :: linv                                
   real(dp), dimension(size(a,2),size(a,2)), intent(out), optional :: ainv
   real(dp), dimension(size(a,2),size(a,2))                        :: ata
   real(dp), dimension(size(a,2))                                  :: bb,p
!
   bb = matmul(transpose(a),b)
   ata = matmul(transpose(a),a)
   if (present(atac)) then ! salva ata e bb   
       bbc = bb
       atac = ata
   endif
   call choldc(ata,p,ier)  ! ier < 0: ata non e' definita positiva
   if (ier == 0) then
       call cholsl(ata,p,bb,x)
       if (present(linv)) then   
           if (linv) then                           ! calcola inversa di atac
               ainv = invltrimatrix(size(x),ata,p)  ! inversa del Cholesky Factor L
               ainv = matmul(transpose(ainv),ainv)  ! inversa di ata = (L-1)**T * L-1 
           endif
       endif
   endif
!
   end subroutine solve_linlsq

   !---------------------------------------------------------------------------

   subroutine solve_linlsq_bcg(a,b,x,ier,bbc,atac)
!                                   T        T
!  Solve min( || ax - b ||2 ) ==> (a a) x = a b
!
   USE nr; USE nrtype
   USE xlinbcg_data
   real(dp), dimension(:,:), intent(in)                            :: a
   real(dp), dimension(:), intent(in)                              :: b
   real(dp), dimension(size(a,2)), intent(inout)                     :: x
   real(dp), dimension(size(a,2)), intent(out), optional           :: bbc
   real(dp), dimension(size(a,2),size(a,2)), intent(out), optional :: atac
   integer, intent(out)                                            :: ier
   real(dp), dimension(size(a,2),size(a,2))                        :: ata
   real(dp), dimension(size(a,2))                                  :: bb   !!!!!,p
   integer                                                         :: itol,itmax
   real(dp)                                                        :: tol,err
   integer                                                         :: iter
   real(dp), allocatable, dimension(:,:)                           :: atrans
!
   allocate(atrans(size(a,2),size(a,1)))
   atrans(:,:) = transpose(a)
   bb = matmul(atrans,b)
   ata = matmul(atrans,a)
   if (present(atac)) then ! salva ata e bb   
       bbc = bb
       atac = ata
   endif
!
      !write(0,*)'ATA=',sum(ata)
   call sprsin(ata,1.e-6_dp,sa)
!
   !x(:) = 0
   itol = 1
   !tol = 1.e-3_dp
   tol = 1.e-2_dp
   !itmax = 70
   itmax = 40
   call linbcg(bb,x,itol,tol,itmax,iter,err,ier)
          !write(0,*)'IER=',ier
!
   end subroutine solve_linlsq_bcg

   !-------------------------------------------------------------------------------

   function invltrimatrix(n,a,p)       result(matr)
!
!  Inverte una matrice triangolare inferiore
!  La matrice da invertire e' contenuta nel triangolo inferiore di a 
!  eccetto gli elementi diagonali che sono contenuti in p
!
   USE nrtype
!
   implicit none
   integer, intent(in)                  :: n
   real(dp), dimension(n,n)             :: matr
   real(dp), dimension(n,n), intent(in) :: a
   real(dp), dimension(n), intent(in)   :: p
   integer                              :: i,j
!
   matr = a
   do i=1,n
      matr(i,i) = 1.0/p(i)
      do j=i+1,n
         matr(j,i) = -dot_product(matr(j,i:j-1),matr(i:j-1,i))/p(j)
      enddo
   enddo
   forall(i=1:n,j=1:n,j>i)matr(i,j) = 0.0  ! azzera triangolo superiore
!
   end function invltrimatrix

   !-------------------------------------------------------------------------------

   subroutine inverti_chol(mat,ier)
   USE nrtype
   USE nr
   real(dp), dimension(:,:), intent(inout) :: mat
   real(dp), dimension(size(mat,2))        :: p
   integer                                 :: ier
!
   call choldc(mat,p,ier)  ! ier < 0: ata non e' definita positiva
   if (ier == 0) then
       mat = invltrimatrix(size(p),mat,p)  ! inversa del Cholesky Factor L
       mat = matmul(transpose(mat),mat)    ! inversa di ata = (L-1)**T * L-1 
   endif
!
   end subroutine inverti_chol

   !-------------------------------------------------------------------------------

   subroutine inverti_lu(mato,matinv,ier)
   USE nrtype
   USE nr
   real(sp), dimension(:,:), intent(in)                        :: mato
   real(sp), dimension(size(mato,2),size(mato,2)), intent(out) :: matinv
   real(sp), dimension(size(mato,2),size(mato,2))              :: mat
   integer(i4b), dimension(size(mato,2))                       :: indx
   real                                                        :: d
   integer                                                     :: i,j
   integer                                                     :: ier
   integer                                                     :: n
!
   mat(:,:) = mato(:,:)
   call ludcmp_new(mat,indx,d,ier)
   if (ier == 0) then
       n = size(mat,2)
       do i=1,n
          do j=1,n
             matinv(i,j) = 0.0
          enddo
          matinv(i,i) = 1.0
       enddo
       do j=1,size(indx)
          call lubksb_new(mat,indx,matinv(:,j))
       enddo
   endif
!
   end subroutine inverti_lu

  !-------------------------------------------------------------------------------

   subroutine polyfit(x,y,pcoef,mu)
!
!  Polynomial curve fitting: y = p1 + p2*x + p3*x^2 + p4*x^3 + ..
!
   USE nr
      !USE nrtype, only:sp
   USE stat
   real, dimension(:), intent(in)  :: x,y   !fit poly(x(i)) to y(i)
!   integer, intent(in) :: n
   real, dimension(:), intent(out) :: pcoef !n+1 polynomial coefficients in ascending power
   real, dimension(2), intent(out) :: mu
   real, dimension(size(x))        :: sig, xs
!corr   real, dimension(size(pcoef),size(pcoef)) :: covar
   real, dimension(size(pcoef),size(pcoef)) :: v
!corr   logical, dimension(size(pcoef)) :: mask
   real, dimension(size(pcoef)) :: w
   real                            :: chisq
   !integer :: i
   real :: ave,stdvar
!
!  Scaling transformation improves numerical stability
   call avevar(x,ave,stdvar)
   stdvar = sqrt(stdvar)
   xs = (x - ave) / stdvar
!
   sig(:) = 1.0
   call svdfit(xs,y,sig,pcoef,v,w,chisq,fpoly)  !!FIXME - problem with ifort 15
!!!alternative in case of problem with svdfit
   !mask(:)=.true.
   !call lfit(xs,y,sig,pcoef,mask,covar,chisq,funcpoly)
    !do i=1,size(y)
    !  write(0,*)'y=',y(i),pcoef(1)+pcoef(2)*xs(i)+pcoef(3)*xs(i)**2
    !enddo
!!!end alternative
   mu(1) = ave
   mu(2) = stdvar
   !call svdvar(v,w,covar)
   !do i=1,size(pcoef)
   !write(0,*)'STD=',sqrt(covar(i,i))
   !enddo
!
   end subroutine polyfit

  !-------------------------------------------------------------------------------

   subroutine funcpoly(x,afunc)
   USE nr
   real, intent(in)                :: x
   real, dimension(:), intent(out) :: afunc
!
   afunc(:) = fpoly(x,size(afunc))
!
   end subroutine funcpoly

  !-------------------------------------------------------------------------------

   subroutine scalefit(x,y,b,chi2,sig)
   real, dimension(:), intent(in) :: x,y
   real, intent(out)              :: b,chi2
   real, dimension(:), optional, intent(in) :: sig
   real :: ss,sx,sxoss,sy,st2
   real, dimension(size(x)), target :: t
   real, dimension(:), pointer :: wt

   if (present(sig)) then
       wt=>t
       wt(:)=1.0/(sig(:)**2)
       ss=sum(wt(:))
       sx=dot_product(wt,x)
       sy=dot_product(wt,y)
   else
       ss=real(size(x))
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
   t(:)=y(:)-b*x(:)
   if (present(sig)) then
       t(:)=t(:)/sig(:)
       chi2=dot_product(t,t)
   else
       chi2=dot_product(t,t)
   end if
   end subroutine scalefit

 !-------------------------------------------------------------------------------
     
   subroutine provainv()
   USE nrtype
   real(dp), dimension(6,6) :: mat
   real(dp), dimension(6,6) :: matsav
   real(sp), dimension(6,6) :: matr,matri
!   real(sp), dimension(6,6) :: matsavr
   integer                  :: i
   integer                  :: ndim
   character(len=10)        :: strf
   integer                  :: ier
!
   mat(1,1) = 1.953
   mat(2,2) = 7.465
   mat(3,3) = 19.420
   mat(4,4) = 191.431
   mat(5,5) = 202.516
   mat(6,6) = 0.209
   mat(1,2) = -0.096
   mat(1,3) = 2.109
   mat(1,4) = 19.333
   mat(1,5) = -5.631
   mat(1,6) = -0.313
   mat(2,3) = 7.906
   mat(2,4) = -0.956
   mat(2,5) = 17.526
   mat(2,6) = 0.696
   mat(3,4) = 20.869
   mat(3,5) = -22.348
   mat(3,6) = -1.408
   mat(4,5) = -55.763
   mat(4,6) = -3.095
   mat(5,6) = 0.148
   mat(6,1:5) = mat(1:5,6)
   mat(5,1:4) = mat(1:4,5)
   mat(4,1:3) = mat(1:3,4)
   mat(3,1:2) = mat(1:2,3)
   mat(2,1) = mat(1,2)
!
   ndim = 6
   write(strf,'("(",i0,"f10.4)")')ndim
   write(0,*)'matrice da invertire'
   do i=1,ndim
      write(0,strf) mat(i,:ndim)
   enddo
   matsav(:,:) = mat(:,:)
   call inverti_chol(mat(:ndim,:ndim),ier)
   if (ier == 0) then
       write(0,*)'matrice inversa con choldc'
       do i=1,ndim
          write(0,strf) mat(i,:ndim)
       enddo
       mat(:ndim,:ndim) = matmul(mat(:ndim,:ndim),matsav(:ndim,:ndim))
       write(0,*)'verifica di choldc'
       do i=1,ndim
          write(0,strf) mat(i,:ndim)
       enddo
   else
       write(0,*)'matrice non invertibile con chol'
   endif
   matr(:,:) = matsav(:,:)
   call inverti_lu(matr(:ndim,:ndim),matri(:ndim,:ndim),ier)
   mat(:,:) = matri(:,:)
   if (ier == 0) then
       write(0,*)'matrice inversa con lu'
       do i=1,ndim
          write(0,strf) mat(i,:ndim)
       enddo
       mat(:ndim,:ndim) = matmul(mat(:ndim,:ndim),matsav(:ndim,:ndim))
       write(0,*)'verifica di lu'
       do i=1,ndim
          write(0,strf) mat(i,:ndim)
       enddo
   else
       write(0,*)'matrice non invertibile con lu'
   endif
!
   end subroutine provainv

END MODULE GENLSQ
