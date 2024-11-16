   MODULE lsqalgo
!
   implicit none
!
   CONTAINS

     subroutine calc_gradient(xgrad,x,fx,func,h,icod)
     USE strutil
     interface 
       real function func(x)
       real, dimension(:), intent(in) :: x
       end function func
     end interface
     real, dimension(:), intent(out) :: xgrad   ! gradiente
     real, dimension(:), intent(in)  :: x       ! parametri
     real, intent(in)                :: fx      ! valore della funzione in x
     real, optional, intent(in)      :: h       ! step
     integer, intent(in)             :: icod    ! modalita di calcolo
     real, dimension(size(x))        :: hv      
     integer                         :: npar
     real, dimension(size(x))        :: xtemp,xtemp2,xtemp_2
     real, dimension(size(x))        :: xtemp4,xtemp_4
     real                            :: eps
     integer                         :: i
!
!    Definizione dello step
     eps = epsilon(1.0)
     if (present(h)) then
         hv(:) = h
     else
         hv(:) = eps**(1.0/3)*abs(x(:))
     endif
     where(hv < eps) hv(:) = eps**(1.0/3)
!
     npar = size(x)

     select case(icod)    

       case (1)      

         xtemp(:) = x(:)
         do i=1,npar
            xtemp(i) = x(i) + hv(i)
            xgrad(i) = (func(xtemp) - fx) / hv(i)
            xtemp(i) = x(i)
         enddo

       case (2)

         xtemp2(:) = x(:)
         xtemp_2(:) = x(:)
         xtemp4(:) = x(:)
         xtemp_4(:) = x(:)
         do i=1,npar
            xtemp2(i) = x(i) + hv(i)/2.0
            xtemp_2(i) = x(i) - hv(i)/2.0
            xtemp4(i) = x(i) + hv(i)/4.0
            xtemp_4(i) = x(i) - hv(i)/4.0
           !if (xtemp_2(6) < 0.0) write(0,*)'warning',xtemp_2(6)
            xgrad(i) = (8*(func(xtemp4)-func(xtemp_4)) - (func(xtemp2)-func(xtemp_2)))/ (3.0*hv(i)) 
            xtemp2(i) = x(i)
            xtemp_2(i) = x(i)
            xtemp4(i) = x(i)
            xtemp_4(i) = x(i)
            !if (trim(r_to_s(xgrad(i))) == 'NaN') then
            !    write(0,*)'warning on par.',i !!!xgrad(i) = 1000.0
            !endif
            !write(0,*)i,'h=',hv(i),' grad=',xgrad(i)
         enddo
     end select
!
     end subroutine calc_gradient

!-------------------------------------------------------------------------------

     subroutine steepest_descent(func,xparam,fval,nmaxc,tol)
     interface
        subroutine func(xp,fval,grad,icod)
!
!       Funzione che per  icod = 1  valuta la funzione costo
!                         icod = 2  valuta il gradiente
        real, dimension(:), intent(in)            :: xp
        real, intent(out)                         :: fval
        real, dimension(:), intent(out), optional :: grad
        integer, intent(in)                       :: icod  
        end subroutine func
     end interface
     real, dimension(:), intent(inout) :: xparam
     real, intent(out)                 :: fval
     integer, intent(in)               :: nmaxc
     real, intent(in)                  :: tol
     real, dimension(size(xparam))     :: gradt
     real, dimension(size(xparam))     :: xparamnew,typep,shi
     real                              :: fvalnew
     integer                           :: i,j
     logical                           :: stepfound
     integer                           :: error
     integer                           :: term
     integer                           :: itype
     real                              :: stepl
     integer                           :: kpr=0
!
!    No scaling!
     typep(:) = 1.0
!
!    Calcola valore della funzione     
     call func(xparam,fval,gradt,icod=1)
!
     do i=1,nmaxc
!
!       Calcola gradiente
        call func(xparam,fval,gradt,icod=2)
!        
!       Valutazione dello step_length
            itype = 0
          if (itype == 0) then
        stepfound = .false.
        do j=3,6
           stepl = 0.1**j
           shi(:) = - stepl * gradt(:)
          xparamnew(:) = xparam(:) + shi(:)
           call func(xparamnew,fvalnew,gradt,icod=1)
           if (fvalnew < fval) then
               stepfound = .true.
               exit
           endif
        enddo
!
!       Aggiorna i parametri 
        if (stepfound) then
            xparam(:) = xparamnew(:)
            fval = fvalnew
        else
            exit
        endif
              else
        shi(:) = -gradt
        call line_search(func,stepl,fval,xparam,shi,gradt,tol,typep,error)
        if (error > 0) exit
             endif
        if(kpr==1)write(6,'(a,i0,a,f0.4,a,f0.6)')'ciclo n.',i,' f=',fval,' step=',stepl
!
!       Controllo per convergenza
        call stopconditions(tol,fval,xparam,gradt,shi,term,0)
        if (term > 0) exit
            
     enddo

!     contains
!
!     real function funcl(x)
!     real, dimension(:) :: x
!     call func(x,funcl,gradt,icod=1)
!     end function funcl
!
     end subroutine steepest_descent

!-------------------------------------------------------------------------------

     subroutine line_search(func,stepl,fval,xparam,dx,gradt,eps,typep,error)
!
!    Trova stepl tale favl(stel) = min, aggiorna xparam e fval se error = 0
!
     interface
        subroutine func(xp,fval,grad,icod)
!
!       Funzione che per  icod = 1  valuta la funzione costo
!                         icod = 2  valuta il gradiente
        real, dimension(:), intent(in)            :: xp
        real, intent(out)                         :: fval
        real, dimension(:), intent(out), optional :: grad
        integer, intent(in)                       :: icod
        end subroutine func
     end interface
     real, intent(out)                            :: stepl  ! step calcolato
     real, intent(out)                            :: fval   ! fval aggiornato
     real, dimension(:), intent(inout)            :: xparam ! parametro aggiornato
     real, dimension(size(xparam)), intent(inout) :: dx     ! xnew = x + step*dx
     real, dimension(size(xparam)), intent(in)    :: gradt  ! gradiente
     real, intent(in)                             :: eps    ! parametro di convergenza
     real, dimension(size(xparam)), intent(in)    :: typep  ! vettore per scaling
     integer                                      :: error  ! > 0 se non trova lo step
     real                                         :: alfastep
     integer                                      :: nmaxiter
     integer                                      :: niter
     real                                         :: fval_new
     real, dimension(size(xparam))                :: xparam_new,dx_new
     real                                         :: initslope
     integer                                      :: kpr = 6
     integer                                      :: ifirstcycle
     real                                         :: step_min,step_max
     real                                         :: step_length_temp,step_length_pre
     real                                         :: fval_pre
     real                                         :: disc
     real, dimension(2)                           :: cubic
     real, dimension(2,2)                         :: vetcubic1
     real, dimension(2)                           :: vetcubic2
!
     error = 0
     alfastep = 0.00001
     stepl = 1.0
!
     step_min = eps / MAXVAL(abs(dx)/max(abs(xparam),typep))   ! valore minimo dello steplenght
     step_max = 1.0                                            ! valore massimo dello steplenght
!
     if (kpr >= 0) then
         write(kpr,*)
         write(kpr,*)'  Line search'
         write(kpr,'(a33,g12.4)')'   Minimum allowable steplenght: ',step_min
         write(kpr,*)
         write(kpr,*)'    Steplenght          Chisquared         Difference       Info'
         write(kpr,*)
     endif

     initslope = dot_product(dx,gradt)
     stepl = step_max                                          ! inizializza step al valore massimo
     ifirstcycle = 1
     nmaxiter = 10
     niter = 0
     do
        niter = niter + 1
        if (niter > nmaxiter) then
            error = 1
            exit
        endif
!
        dx_new(:) = stepl*dx(:)
        xparam_new(:) = xparam(:) + dx_new(:)
        call func(xparam_new,fval_new,icod=1)
        !fval_new = func(xparam_new)
!
        if (fval_new <=  fval + alfastep*stepl*initslope) then 
!
!           Lo step soddisfa la regola di Armijo
            if (kpr >= 0) write(kpr,10)stepl,fval_new,fval_new-fval,'success'
            exit
        elseif (stepl < step_min) then
!
!           Lo step_length e' troppo piccolo
            if (kpr >= 0) write(kpr,10)stepl,fval_new,fval_new-fval,'too small'
            error = 2
            exit
        else
            if (ifirstcycle == 1) then
                if (kpr > 0) write(kpr,10)stepl,fval_new,fval_new-fval
!
!               La prima volta eseguo un fit con una quadrica
                step_length_temp = -initslope / ( 2 * ( fval_new - fval - initslope))
                ifirstcycle = 0
                if (kpr > 0) write(kpr,10)step_length_temp,fval_new,fval_new-fval,'quadric'
            else
!
!                Le volte successive eseguo un fit con una cubica
                 vetcubic1(1,1) =   1.0 / stepl ** 2
                 vetcubic1(1,2) = - 1.0 / step_length_pre ** 2
                 vetcubic1(2,1) = - step_length_pre / stepl ** 2
                 vetcubic1(2,2) =   stepl / step_length_pre ** 2
                 vetcubic2(1)   =   fval_new - fval - stepl * initslope
                 vetcubic2(2)   =   fval_pre - fval - step_length_pre * initslope
                 cubic = (1.0/(stepl - step_length_pre)) * matmul(vetcubic1,vetcubic2)
                 disc = cubic(2)**2 - 3.0*cubic(1)*initslope
                 if (abs(cubic(1)) <= epsilon(cubic(1))) then
                     step_length_temp = - initslope / (2*cubic(2))   ! la cubica e' una quadrica
                 else
                     step_length_temp = (-cubic(2) + sqrt(disc)) / (3.0*cubic(1))
                 endif
                 if (kpr > 0) write(kpr,10)step_length_temp,fval_new,fval_new-fval,'cubic'
            endif
            step_length_pre = stepl
            fval_pre  = fval_new
            if ( step_length_temp <= 0.1 * stepl ) then
                 stepl = 0.1 * stepl
                 if (kpr > 0) write(kpr,10)stepl,fval_new,fval_new-fval,'modified'
            else
                 stepl = step_length_temp
            endif
        endif
     enddo
10   format(5x,f12.4,2x,f16.4,2x,f16.4,8x,a)
!
!    Aggiorna i parametri se lo steplength e' stato trovato
     if (error == 0) then
         fval = fval_new
         xparam(:) = xparam_new(:)
         dx(:) = dx_new(:)
     endif
!
     end subroutine line_search

   !---------------------------------------------------------------------------

     subroutine stopconditions(eps,fval,xparam,gradt,shi,term,kpr0)
     USE General, only:lo
!
     real, intent(in)               :: eps
     real, intent(in)               :: fval
     real, dimension(:), intent(in) :: xparam,gradt,shi
     integer, intent(out)           :: term
     integer, intent(in), optional  :: kpr0
     integer                        :: kpr
     real                           :: gradval,shiftval
!
     if (present(kpr0)) then
         kpr = kpr0
     else
         kpr = 0
     endif
!
     term = 0
!
!    term = 1 -> criterio di stop sul gradiente verificato
     gradval = MAXVAL(abs(gradt)*max(xparam,1.0)/max(fval,1.0))
     if( gradval < eps**0.5) term = 1
!
!    term = 2 -> criterio di stop sugli shift verificato
!    term = 3 -> entrambi i criteri precedenti sono verificati
     shiftval = MAXVAL(abs(shi)/max(abs(xparam),1.0))
     if( shiftval < eps) term = term + 2
!
!    Stampa se richiesto
     if (kpr > 0) then
         write(lo,'(a21,2x,i3)')'   Stopping condition ',term
         write(lo,'(a21,e16.4,a7,e16.4)')'   Norm of gradient: ',gradval, ' Tol = ',eps**0.5
         write(lo,'(a21,e16.4,a7,e16.4)')'   Norm of shifts  : ',shiftval,' Tol = ',eps
     endif
!
     end subroutine stopconditions
   END MODULE lsqalgo
