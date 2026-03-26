MODULE pearsonf
   USE progtype, only: rparam_type
   implicit none

   integer, parameter :: P_UPAR=1,P_VPAR=2,P_WPAR=3,P_B0PAR=4,P_B1PAR=5,P_B2PAR=6,    &
                         P_ASYM1PAR=7,P_ASYM2PAR=8,P_ASYM3PAR=9,P_ASYM4PAR=10
   real, parameter :: BETAMIN=0.51, BETAMAX=20.0

   integer, parameter :: GAMRATIO=1,PEARPAR1=2,PEARPAR2=3,BETAPAR=4

   real, dimension(:), allocatable :: asymval
   real, private, dimension(:,:), allocatable :: der_asym

CONTAINS

   function init_pearson_param()  result(param)
   USE progtype, only: rparam_type
   USE profile_function
   type(profile_function_t) :: param
   
   param%par(P_UPAR)     = rparam_type(val=0.0,str='U')
   param%par(P_VPAR)     = rparam_type(val=0.0,str='V')
   param%par(P_WPAR)     = rparam_type(val=0.01,str='W')
   param%par(P_B0PAR)    = rparam_type(val=2.0,str='beta0')
   param%par(P_B1PAR)    = rparam_type(val=0.0,str='beta1')
   param%par(P_B2PAR)    = rparam_type(val=0.0,str='beta2')
   param%par(P_ASYM1PAR) = rparam_type(val=0.0,str='asym1')
   param%par(P_ASYM2PAR) = rparam_type(val=0.0,str='asym2')
   param%par(P_ASYM3PAR) = rparam_type(val=0.0,str='asym3')
   param%par(P_ASYM4PAR) = rparam_type(val=0.0,str='asym4')

   end function init_pearson_param

!------------------------------------------------------------------------------------

   subroutine pearson(pear,ref,xcount,fs,ycount,nwave,ratio,nph,ier)
   USE reflection_type_util, only:reflection_type   
   USE trig_constants, only:dtor,pi
   USE arrayutil
   USE profile_function
   USE asymfunc
!
   type(profile_function_t), intent(in)               :: pear
   type(reflection_type), dimension(:), intent(inout) :: ref
   real, dimension(:), intent(in)                     :: xcount
   real, dimension(:), intent(in)                     :: fs    
   real, dimension(:), intent(out)                    :: ycount
   integer, intent(in)                                :: nwave
   real, dimension(:), intent(in)                     :: ratio
   integer, intent(in)                                :: nph
   integer, intent(out)                               :: ier
   real                                               :: b0,b1,b2
   real                                               :: fu,fv,fw
   real                                               :: asym
   integer                                            :: j,kal,nnn
   integer                                            :: iniz,ifin
   real                                               :: tthdeg
   real                                               :: tdeg
   real                                               :: cpear,cpear2
   real                                               :: uvwg,uvw2,uvwqg
   real                                               :: ffx
   real                                               :: deltath,deltath2
   real                                               :: argbeta
   real                                               :: pears
   real                                               :: beta,pconst   !!!!!,betad,gambeta,gambetad
   real                                               :: gamra
   integer                                            :: negfw1,negfw2
   real                                               :: epsmch = epsilon(1.0)
   real :: tmax
   intrinsic :: gamma
!
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
!
   fu = pear%par(P_UPAR)%val    
   fv = pear%par(P_VPAR)%val    
   fw = pear%par(P_WPAR)%val    
   b0 = pear%par(P_B0PAR)%val   
   b1 = pear%par(P_B1PAR)%val   
   b2 = pear%par(P_B2PAR)%val   
   asym = pear%par(P_ASYM1PAR)%val
   negfw1 = 0
   negfw2 = 0
   ier = 0
   ycount(:) = 0.0
   tmax = xcount(size(xcount))
!
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=1,size(ref)
         tthdeg = ref(j)%tthd(kal)
!
         beta = b0 + b1/tthdeg + b2/(tthdeg**2)
         if (beta < betamin .or. beta > betamax) then
             !write(6,*)'WARN BETA=',beta,j
             negfw1 = negfw1 + 1
             !ier = -1
             !return
         endif
         if (beta < betamin) beta = betamin
         if (beta > betamax) beta = betamax
!
         gamra = gamma(beta) / gamma(beta-0.5)
!
         tdeg = tan (0.5 * tthdeg * dtor)
         uvwqg = fU*tdeg**2 + fV*tdeg + fW
         if (uvwqg < epsmch) then
             !write(6,*)'WARN FWHM=',uvwqg,j
             uvwqg = 0.0001
             negfw2 = negfw2 + 1
             !ier = -1
         endif
         uvwg = sqrt(uvwqg)
         pconst = 2.0**(1.0/beta) - 1.0
         cpear = (2.0 * sqrt(pconst/pi)) / uvwg
         cpear2 = (4.0 * pconst) / uvwqg
!!!new
         pdph(nph)%pvet(j,kal,BETAPAR) = beta
         pdph(nph)%pvet(j,kal,GAMRATIO) = gamra
         pdph(nph)%pvet(j,kal,PEARPAR1) = cpear
         pdph(nph)%pvet(j,kal,PEARPAR2) = cpear2
!!!new
!
         ref(j)%fwhm(kal) = uvwg
         uvw2 =  uvwg * ref(j)%pk * ref(j)%rapI
!TODO: test this!         if (tthdeg-uvw2 > tmax) cycle
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         ffx = ref(j)%m*fs(j)**2*ref(j)%po*ref(j)%lp(kal)*ref(j)%ab(kal)*ratio(kal)
!
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                                ref(j)%fwhm(kal),pear%par(P_ASYM1PAR:P_ASYM4PAR)%val,beta)
!
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            argbeta=(1.0+cpear2*deltath2)
            !ccas=1.0/cpear2+deltath2
            pears = gamra * cpear * argbeta**(-beta)
            !asterm= 1.0 + asym*deltath**3/(ccas**1.5)
            !  if (abs(asterm -asymval(nnn-iniz+1)) > 0.0001) write(70,*)'ASYM=',asterm,asymval(nnn-iniz+1),ccas
            !ycount(nnn)=ycount(nnn) + ffx*pears*asterm
            ycount(nnn)=ycount(nnn) + ffx*pears*asymval(nnn-iniz+1)
                  !write(71,*)'YC=',nnn,cpear2,xcount(nnn),tthdeg,asymval(nnn-iniz+1)
         enddo LOOP_CONTEGGI
      enddo LOOP_RIFLESSI
   enddo LOOP_KALPHA
   if (negfw1 > 0) ier = 1
   if (negfw2 > 0) ier = 1
   !if (real(negfw1)/size(ref) > 0.4) ier = 1
   !if (real(negfw2)/size(ref) > 0.4) ier = 1
   !if(negfw1>0)write(6,*)'NWGFW=',negfw1,real(negfw1)/size(ref),ier
   !if(negfw2>0)write(6,*)'NWGFW=',negfw2,real(negfw2)/size(ref),ier
!
   end subroutine pearson

!------------------------------------------------------------------------------------

   subroutine decomponi_pearson(pear,ref,xcount,scal,jac,nwave,rapporto_int,nph,ier)
   USE reflection_type_util, only:reflection_type
   USE trig_constants, only:dtor,pi
   USE arrayutil
   USE profile_function
   USE asymfunc
   USE type_constants, only: DP
!
   type(profile_function_t), intent(in)            :: pear
   type(reflection_type), dimension(:), intent(inout) :: ref
   real, dimension(:), intent(in)                     :: xcount
   real, intent(in)                                   :: scal
   real(DP), dimension(:,:), intent(inout)            :: jac
   integer, intent(in)                                :: nwave
   real, intent(in)                                   :: rapporto_int
   integer, intent(in)                                :: nph
   integer, intent(out)                               :: ier
   real                                               :: b0,b1,b2
   real                                               :: fu,fv,fw
   real                                               :: asym
   integer                                            :: j,kal,nnn
   integer                                            :: iniz,ifin
   real                                               :: tthdeg
   real                                               :: cpear,cpear2,uvw2
   real                                               :: kffx
   real                                               :: deltath,deltath2
   real                                               :: argbeta
   real                                               :: pears
   real                                               :: beta   
   real                                               :: gamra
   real, dimension(2)                                 :: rap
   integer                                            :: negfw
!
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
   fu = pear%par(P_UPAR)%val    
   fv = pear%par(P_VPAR)%val    
   fw = pear%par(P_WPAR)%val    
   b0 = pear%par(P_B0PAR)%val   
   b1 = pear%par(P_B1PAR)%val   
   b2 = pear%par(P_B2PAR)%val   
   asym = pear%par(P_ASYM1PAR)%val
   negfw = 0
   ier = 0
!corr   dycount(:,:) = 0.0
!corr   call init_calc(size(ref),nwave)
!
   rap = (/1.0,rapporto_int/)
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=1,size(ref)

         if (ref(j)%rcod == 0) cycle

         tthdeg = ref(j)%tthd(kal)
!
         beta = b0 + b1/tthdeg + b2/(tthdeg**2)
         if (beta < betamin .or. beta > betamax) then
             !write(6,*)'WARN BETA=',beta
             negfw = negfw + 1
             !ier = -1
             !return
         endif
         if (beta < betamin) beta = betamin
         if (beta > betamax) beta = betamax
         pdph(nph)%pvet(j,kal,BETAPAR) = beta
!
!!!new
         gamra  = pdph(nph)%pvet(j,kal,GAMRATIO)
         cpear  = pdph(nph)%pvet(j,kal,PEARPAR1)
         cpear2 = pdph(nph)%pvet(j,kal,PEARPAR2)
!!!new
         uvw2 =  ref(j)%fwhm(kal) * ref(j)%pk * ref(j)%rapI
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         kffx = ref(j)%m*ref(j)%po*ref(j)%lp(kal)*ref(j)%ab(kal)*rap(kal)
!
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                                ref(j)%fwhm(kal),pear%par(P_ASYM1PAR:P_ASYM4PAR)%val,beta)
!
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            argbeta=(1.0+cpear2*deltath2)
            !ccas=1.0/cpear2+deltath2
            pears = gamra * cpear * argbeta**(-beta)
            !asterm= 1.0 + asym*deltath**3/(ccas**1.5)
            !dycount(j,nnn)=scal*kffx*pears*asterm
            !dycount(j,nnn)=scal*kffx*pears*asymval(nnn-iniz+1)
            jac(nnn,ref(j)%rcod) = scal*kffx*pears*asymval(nnn-iniz+1)
         enddo LOOP_CONTEGGI
      enddo LOOP_RIFLESSI
   enddo LOOP_KALPHA
   !if(negfw>0)write(6,*)'NWGFW=',negfw,real(negfw)/size(ref)
   !if(negfw>0)write(0,*)'NWGFW=',negfw,real(negfw)/size(ref)
   if (real(negfw)/size(ref) > 0.4) ier = 1
!
   end subroutine decomponi_pearson


!--------------------------------------------------------------------

   subroutine derpears(fstr,pear,inizr,ifinr,xcount,ref,cell,derFct,derOP,deryz,dfw,derp,dercc,jac,  &
                       ratio,scal,refine_cell,refine_profile,nwave,gpocode,absf,abscode,abspar,radtype,sync,wave,spg,nph)
   USE trig_constants
   USE arrayutil
   USE profile_function
   USE reflection_type_util
   USE type_constants, only: DP
   USE elements, only: NEUTRON_SOURCE
   USE spginfom, only: spaceg_type
   USE asymfunc
   USE absmod
!
   implicit none
   real, dimension(:), intent(in)                  :: fstr
   type(profile_function_t), intent(in)         :: pear
   integer, intent(in)                             :: inizr,ifinr
   real, dimension(:), intent(in)                  :: xcount
   type(reflection_type), dimension(:), intent(in) :: ref
   real, dimension(6), intent(in)                  :: cell
   real, dimension(:), intent(in)                  :: derFct
   real, dimension(:), intent(in), allocatable     :: derOP
   real, dimension(:), intent(inout)               :: deryz
   real, dimension(:,:), intent(inout)             :: dfw
   real, dimension(:,:), intent(inout)             :: derp
   real, dimension(:,:), intent(out)               :: dercc
   real(DP), dimension(:,:), intent(inout)         :: jac
!corr   real, dimension(:), intent(out)                 :: derG
   real, dimension(:), intent(in)                  :: ratio
   real, intent(in)                                :: scal
   logical, intent(in)                             :: refine_cell,refine_profile
   integer, intent(in)                             :: nwave
   integer, intent(in)                             :: gpocode
   integer, intent(in)                             :: absf
   integer, dimension(:), intent(in)               :: abscode
   real, dimension(:), intent(in)                  :: abspar
   logical, intent(in)                             :: sync
   integer, intent(in)                             :: radtype, nph
   type(spaceg_type), intent(in)                   :: spg
   real, intent(in)                                :: wave
   integer                                         :: j,k,kal,nnn,iab
   integer                                         :: iniz,ifin
   real                                            :: asym
   real                                            :: tthdeg
   real                                            :: beta,beta0,beta1,beta2
   real                                            :: rdeg,tdeg,tdeg2,cdeg
   real                                            :: uvwg,uvwqg,uvw2
   real                                            :: derft,dlorp
   real                                            :: deltath,deltath2,deltath3
   real                                            :: argbeta,ccas,pears,asterm
   real                                            :: ffb1,ffnop,ffx,ffbx,ffnabs,lmf2
   real                                            :: absoc,pesolp
   real                                            :: deryop,derpth,derasth
   real, dimension(6)                              :: dercells
   real, dimension(3)                              :: gradasym,vetder
   real, dimension(4)                              :: gradprof,vetder1
   real                                            :: fwU,fwV,fwW
   real                                            :: ln2
   real                                            :: pconst,rapgamma
   real                                            :: deruvw,derbeta
   real                                            :: deraszero,derfunczero
   real                                            :: betad,gambeta,gambetad
   real                                            :: dergbeta,gamder,dergbetad
   real(DP)                                        :: mmpsi
   integer                                         :: ier
   real                                            :: kpol
   intrinsic                                       :: gamma
   integer, parameter                              :: ASYMTYPE = BERAR_BALDINOZZI_TYPE
   real, dimension(:,:), allocatable               :: der_abspar
   real, dimension(:), allocatable                 :: der_abstth
!
   if (size_array(der_asym,1) < size(xcount)) call new_array(der_asym,[1,1],[size(xcount),7])
   if (radtype == NEUTRON_SOURCE .or. sync) then
       kpol = 0.0
   else
       kpol = 0.8
   endif
   dercc = 0.0
   !derG(:)  = 0.0
!
   ln2 = alog(2.0)
   vetder(1) = 1.0
   vetder1(1) = 1.0
   fwU = pear%par(P_UPAR)%val    
   fwV = pear%par(P_VPAR)%val    
   fwW = pear%par(P_WPAR)%val    
   beta0 = pear%par(P_B0PAR)%val   
   beta1 = pear%par(P_B1PAR)%val   
   beta2 = pear%par(P_B2PAR)%val   
   asym = pear%par(P_ASYM1PAR)%val
   LOOP_KALPHA : do kal=1,nwave
      if (absf > 0) then
          if (any(abscode > 0)) then
              call new_array(der_abspar,[1,1],[size(abspar),ifinr-inizr+1])
              call new_array(der_abstth,ifinr-inizr+1)
              call absr_der(ref%tthd(kal),absf,abspar,der_abspar,der_abstth)
          endif
      endif
      LOOP_RIFLESSI : do j=inizr,ifinr
         tthdeg = ref(j)%tthd(kal)
         derft = derFct(j)   
         beta = pdph(nph)%pvet(j,kal,BETAPAR)
!
         gambeta   = gamma(beta)
         betad = beta-0.5
         gambetad  = gamma(betad)
!
!        mmpsi provide the logaritmic derivative of GF = GF' / GF
         dergbeta  = gambeta*mmpsi(dble(beta),ier)
         dergbetad = gambetad*mmpsi(dble(betad),ier)
         gamder    = (dergbeta*gambetad - gambeta*dergbetad)/gambetad**2   ! ! derivata del rapporto tra le funzioni gamma
!
         pconst = 2.0**(1.0/beta)-1
         rdeg = 0.5 * tthdeg * dtor
         tdeg = sin(rdeg) / cos(rdeg)
         tdeg2 = tdeg * tdeg
         cdeg = cos(rdeg)
         uvwg = ref(j)%fwhm(kal)
         uvwqg = uvwg**2
         uvw2 = uvwg * ref(j)%pk * ref(j)%rapI                    ! ampiezza di picco
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                             ref(j)%fwhm(kal),pear%par(P_ASYM1PAR:P_ASYM4PAR)%val,beta,der_asym)
!
         rapgamma = pdph(nph)%pvet(j,kal,GAMRATIO)
         if (refine_cell) then
             dlorp = derivataLP(ref(j)%tthd(kal),kpol)
             dercells = scal*derteta_risp_celldir(spg,cell,ref(j)%hkl,ref(j)%tthd(kal),wave)        ! derivate di theta rispetto alla cella * scala
!
             vetder(2)=(-(2.0*beta1)/(tthdeg**2)-(4.0*beta2)/(tthdeg**3))   ! der. di beta rispetto theta in rad.
             vetder(3) = (1.0 / cos(rdeg)**2)*(fwV + 2.0*fwU*tdeg)          ! der. quadrato di FWHM rispetto theta in rad.
             vetder1(2) = vetder(2)
             vetder1(3) = 0.5*vetder(3)/uvwg                                ! der. FWHM rispetto theta
             vetder1(4) = gamder * vetder(2)                         ! der. rapporto fun. gamma rispetto theta
         endif
         pesolp = ref(j)%lp(kal) * ratio(kal)
         absoc = ref(j)%ab(kal)
         lmf2 = pesolp*ref(j)%m*(fstr(j)**2)
         ffnop = lmf2*absoc
         ffnabs = lmf2*ref(j)%po
         !ffnop = pesolp*ref(j)%m*(fstr(j)**2)*absoc
         ffx = ffnop*ref(j)%po
         ffbx = pesolp*ref(j)%m*2.0*fstr(j)*ref(j)%po*absoc
         ffb1 = pesolp*ref(j)%m*ref(j)%po*absoc
!
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            deltath3=deltath**3
            argbeta=(1.0+pdph(nph)%pvet(j,kal,PEARPAR2)*deltath2)
            ccas=1.0/pdph(nph)%pvet(j,kal,PEARPAR2)+deltath2
            pears = pdph(nph)%pvet(j,kal,GAMRATIO) * pdph(nph)%pvet(j,kal,PEARPAR1) * argbeta**(-beta)
            !asterm= 1.0 + asym*deltath3/(ccas**1.5)
            asterm = asymval(nnn-iniz+1)
!
!           Compute derivative of 2theta corrected
            !deraszero = 6.0*asym*deltath2*(deltath2-ccas)/ccas**2.5                    ! der. asym ris. zero
            !derfunczero = pears*deltath*4.0*cpear2v(j)*beta/argbeta                 ! der. pearson ris. zero
            !deraszero = -3.0*asym*deltath2*(deltath2-ccas)/ccas**2.5                 ! der. asym ris. 2theta corrected
            deraszero = der_asym(nnn-iniz+1,2)                ! der. asym ris. 2theta corrected
            derfunczero = -pears*deltath*2.0*pdph(nph)%pvet(j,kal,PEARPAR2)*beta/argbeta  ! der. pearson ris. 2theta corrected
            deryz(nnn) = deryz(nnn) + scal * ffx * (derfunczero*asterm + deraszero*pears)
!
            if (ASYMTYPE == PEARSON_TYPE) then
                gradasym(3) = (-0.375*asym*deltath**3)/((-1 + 2**(1/beta))*ccas**2.5)      ! der. asym ris. quadr. di FWHM
                gradasym(2) = (-1.5*ln2*2**(-2 + 1/beta)*asym*deltath3*uvwqg)/     &
                                ((-1 + 2**(1/beta))**2*beta**2*ccas**2.5)                  ! der. asym ris. beta
            elseif (ASYMTYPE == BERAR_BALDINOZZI_TYPE) then
                gradasym(3) = der_asym(nnn-iniz+1,3) / (2*uvwg)
                gradasym(2) = 0
            endif
!
!           Derivata Pearson rispetto beta
!
            gradprof(2)=(-argbeta**(-1 - beta)*rapgamma*(                               &
                        ln2*2**(1/beta)*(argbeta* uvwg**2 - 8.*beta*deltath**2*pconst)+ &
                        2.*argbeta*beta**2*uvwg**2*pconst*aLog(argbeta) ))/              &
                        (beta**2*uvwg**3*(pconst/pi)**0.5*pi)
!
!           Derivata Pearson rispetto FWHM
!
            gradprof(3) = pears * (8.0*pconst*beta*deltath2/(uvwqg*argbeta) - 1.0) / uvwg
!
!           Derivata Pearson rispetto al rapporto tra funzioni gamma
!corr            gradprof(4)=cpearv(j,kal) * argbeta**(-beta)
            gradprof(4) = pdph(nph)%pvet(j,kal,PEARPAR1) * argbeta**(-beta)
!
            if(refine_profile) then
               deruvw = scal*ffx*(gradprof(3)*asterm*0.5/uvwg+gradasym(3)*pears) !gradprof(3)*0.5/uvwg=der.rispetto FWHM**2
               dfw(nnn,1)=dfw(nnn,1)+deruvw*tdeg2                          !der. rispetto U
               dfw(nnn,2)=dfw(nnn,2)+deruvw*tdeg                           !der. rispetto V
               dfw(nnn,3)=dfw(nnn,3)+deruvw                                !der. rispetto W
!
               !derp(nnn,1)=derp(nnn,1) + scal*ffx*pears*deltath3/(ccas**1.5)  !der. rispetto asym
               derp(nnn,4)=derp(nnn,4) + scal*ffx*pears*der_asym(nnn-iniz+1,4) !der. rispetto asym
               derp(nnn,5)=derp(nnn,5) + scal*ffx*pears*der_asym(nnn-iniz+1,5) !der. rispetto asym
               derp(nnn,6)=derp(nnn,6) + scal*ffx*pears*der_asym(nnn-iniz+1,6) !der. rispetto asym
               derp(nnn,7)=derp(nnn,7) + scal*ffx*pears*der_asym(nnn-iniz+1,7) !der. rispetto asym
!
!corr               derbeta = scal * ffx * (gradprof(2)*asterm+gradasym(2)*pears+gradprof(4)*phkl(j,2)*asterm)
               derbeta = scal * ffx * (gradprof(2)*asterm+gradasym(2)*pears+gradprof(4)*gamder*asterm)
               derp(nnn,1)=derp(nnn,1) + derbeta                             !der. rispetto beta0
               derp(nnn,2)=derp(nnn,2) + derbeta/tthdeg                      !der. rispetto beta1
               derp(nnn,3)=derp(nnn,3) + derbeta/tthdeg**2                   !der. rispetto beta2
            endif
            if(refine_cell) then
               if (ASYMTYPE == PEARSON_TYPE) then
                   gradasym(1) = rtod*((6.0*asym*deltath**4)/ccas**2.5 - (6.0*asym*deltath**2)/ccas**1.5)
                   derasth = DOT_PRODUCT(gradasym,vetder)                     !der. asym rispetto theta
               elseif (ASYMTYPE == BERAR_BALDINOZZI_TYPE) then
                   derasth = der_asym(nnn-iniz+1,1) + der_asym(nnn-iniz+1,3)*vetder(3)   !der.asym risp. theta
               endif
!
!              Derivata Pearson rispetto theta
               gradprof(1)=rtod*(32.*argbeta**(-1-beta)*beta*deltath*pconst*(pconst/pi)**0.5*rapgamma)/uvwg**3 !der. rispetto theta
               derpth = DOT_PRODUCT(gradprof,vetder1)     ! derivata Pearson rispetto theta
               do k=1,6                                   ! der. rispetto parametri di cella
                  dercc(nnn,k)=dercc(nnn,k)+ dercells(k)*(ffx * (derpth*asterm+derasth*pears) &
                          + (ffb1*derft+ffx/pesolp*dlorp) * pears*asterm)
               enddo
            endif
            if (gpocode > 0) then
                deryop = scal * ffnop * pears * asterm
                !derG(nnn)=derG(nnn) + deryop*derOP(j)
                jac(nnn,gpocode) = jac(nnn,gpocode) + deryop*derOP(j)
            endif
            do iab = 1,2
               if (abscode(iab) > 0) then
                   jac(nnn,abscode(iab)) = jac(nnn,abscode(iab)) + scal*pears*asterm*ffnabs*der_abspar(iab,j-inizr+1)
               endif
            enddo
         enddo LOOP_CONTEGGI
      enddo LOOP_RIFLESSI
   enddo LOOP_KALPHA
!
   end subroutine derpears

!----------------------------------------------------------------------------------------------

   subroutine derpearsa(ref,pear,inizr,ifinr,xcount,der,derc,nwave,ratio,scal,nph)
   USE progtype, only: deratom
   USE reflection_type_util, only: reflection_type
   USE arrayutil
   USE profile_function
   USE asymfunc
!
   type(reflection_type), dimension(:), intent(in) :: ref
   type(profile_function_t), intent(in)         :: pear
   integer, intent(in)                             :: inizr,ifinr
   real, dimension(:), intent(in)                  :: xcount
   type(deratom), dimension(:,:), intent(in)       :: der
   type(deratom), dimension(:,:), intent(out)      :: derc
   integer, intent(in)                             :: nwave
   real, dimension(:), intent(in)                  :: ratio
   real, intent(in)                                :: scal
   integer, intent(in)                             :: nph
   integer                                         :: j,jj,kal,nnn
   integer                                         :: iniz,ifin
   real                                            :: tthdeg
   real                                            :: asym,beta,uvw2
   real                                            :: deltath,deltath2,deltath3
   real                                            :: pears,cc
   real                                            :: ffb1,dydI
   integer                                         :: nat
!
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
   derc = deratom(0.0,0.0,0.0)
   nat = size(derc,2)
!
   asym = pear%par(P_ASYM1PAR)%val
   LOOP_KALPHA : do kal=1,nwave   !!!!!kalphar
      LOOP_RIFLESSI : do j=inizr,ifinr
         tthdeg = ref(j)%tthd(kal)
         beta = pdph(nph)%pvet(j,kal,BETAPAR)
         uvw2 = ref(j)%fwhm(kal) * ref(j)%pk * ref(j)%rapI
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         ffb1 = ratio(kal)*ref(j)%lp(kal)*ref(j)%m*ref(j)%po*ref(j)%ab(kal)
!
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                                ref(j)%fwhm(kal),pear%par(P_ASYM1PAR:P_ASYM4PAR)%val,beta)
!
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            deltath3=deltath*deltath2
            cc=1.0+pdph(nph)%pvet(j,kal,PEARPAR2)*deltath2
            !ccas=1.0/pdph(nph)%pvet(j,kal,PEARPAR2)+deltath2
            pears = pdph(nph)%pvet(j,kal,GAMRATIO) * pdph(nph)%pvet(j,kal,PEARPAR1) * cc**(-beta)
            !asterm= 1.0 + asym*deltath3/(ccas**1.5)
!
!           calcolo derivate param. strutt. per atomo
            !dydI = scal * ffb1 * pears * asterm
            dydI = scal * ffb1 * pears * asymval(nnn-iniz+1)
            do jj=1,nat
               derc(nnn,jj)%co = derc(nnn,jj)%co + dydI * der(j,jj)%co
               derc(nnn,jj)%b = derc(nnn,jj)%b + dydI * der(j,jj)%b
               derc(nnn,jj)%occ = derc(nnn,jj)%occ + dydI * der(j,jj)%occ
            enddo
!
         enddo LOOP_CONTEGGI
      enddo LOOP_RIFLESSI
   enddo LOOP_KALPHA
!
   end subroutine derpearsa

  !--------------------------------------------------------------------
  
   subroutine foleb_pears(ref,inizr,ifinr,Icalc,Iobs,pear,scal,yoss,ytot,ttetaczero,back,nph)
   USE arrayutil
   USE profile_function
   USE reflection_type_util, only: reflection_type
   USE asymfunc
!
   type(reflection_type), dimension(:), intent(in) :: ref
   integer, intent(in)                             :: inizr,ifinr
   real, dimension(:), intent(in)                  :: Icalc
   real, dimension(:), intent(out)                 :: Iobs
   type(profile_function_t), intent(in)            :: pear
   real, intent(in)                                :: scal
   real, dimension(:), intent(in)                  :: yoss,ytot,ttetaczero,back
   integer, intent(in)                             :: nph
   integer                                         :: iniz,ifin,kal
   real                                            :: asym,beta
   integer                                         :: j,nnn
   real                                            :: tthdeg,deltath,deltath2
   real                                            :: uvw2
   real                                            :: cc,pears,yci,diffo,ycalc
   real                                            :: sumy
   integer                                         :: nneg
   real                                            :: epsmch = epsilon(1.0)
!
   if (size_array(asymval) < size(ttetaczero)) call new_array(asymval,size(ttetaczero))
   nneg = 0
   asym = pear%par(P_ASYM1PAR)%val
   kal = 1
   LOOP_RIFLESSI : do j=inizr,ifinr
      sumy = 0.0
      tthdeg = ref(j)%tthd(kal)
!
      beta = pdph(nph)%pvet(j,kal,BETAPAR)
      uvw2 = ref(j)%fwhm(kal) * ref(j)%pk * ref(j)%rapI
!
      iniz = clocate(ttetaczero,tthdeg-uvw2)
!!!!TODO: use dataset%nc2
      ifin = min(clocate(ttetaczero,tthdeg+uvw2),size(ytot))
!
      call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,ttetaczero(iniz:ifin)-tthdeg,tthdeg,   &
                             ref(j)%fwhm(kal),pear%par(P_ASYM1PAR:P_ASYM4PAR)%val,beta)
!
      LOOP_CONTEGGI : do nnn=iniz,ifin
         deltath=ttetaczero(nnn)-tthdeg
         deltath2=deltath**2
         !cc=(1.0+cpear2v(j,kal)*deltath2)
         !ccas=1.0/cpear2v(j,kal)+deltath2
         cc=1.0+pdph(nph)%pvet(j,kal,PEARPAR2)*deltath2
         !ccas=1.0/pdph(nph)%pvet(j,kal,PEARPAR2)+deltath2
         pears = pdph(nph)%pvet(j,kal,GAMRATIO) * pdph(nph)%pvet(j,kal,PEARPAR1) * cc**(-beta)
         !asterm= 1.0 + asym*deltath**3/(ccas**1.5)
         !yci = scal * pears * asterm
         yci = scal * pears * asymval(nnn-iniz+1)
         if (ytot(nnn) > epsmch) then
             diffo = yoss(nnn)-back(nnn)
             if (diffo < 0.0) then
                 cycle
             endif
             ycalc = diffo*yci/ytot(nnn)
         else
             cycle
         endif
         sumy = sumy + ycalc
      enddo LOOP_CONTEGGI
!
      Iobs(j) = Icalc(j) * sumy
      if (Iobs(j) < epsmch) then
          Iobs(j) = 0.0
                !Iobs(j) = epsmch  ! avoid problem when all are negative
          nneg = nneg + 1
      endif
!
   enddo LOOP_RIFLESSI
   !if (nneg > 0) write(0,*)'IOBS NEGATIVI n=',nneg,sum(Iobs)
!
   end subroutine foleb_pears

!------------------------------------------------------------------------------------

   subroutine profile_curve_pearson(pear,ref,jref,fctype,xcount,yb,scal,nwave,ratio,nph,xp,yp,ier)
   USE reflection_type_util, only:reflection_type
   USE arrayutil
   USE profile_function
   USE asymfunc
!
   type(profile_function_t), intent(in)         :: pear
   type(reflection_type), intent(in)            :: ref
   integer, intent(in)                          :: jref
   logical, intent(in)                          :: fctype
   real, dimension(:), intent(in)               :: xcount,yb
   real, intent(in)                             :: scal
   integer, intent(in)                          :: nwave
   real, dimension(:)                           :: ratio
   integer, intent(in)                          :: nph
   real, dimension(:), allocatable, intent(out) :: xp,yp
   integer, intent(out)                         :: ier
   integer                                      :: kal,nnn
   integer                                      :: iniz,ifin
   real                                         :: tthdeg
   real                                         :: cpear,cpear2,uvw2
   real                                         :: kffx
   real                                         :: deltath,deltath2
   real                                         :: argbeta
   real                                         :: pears
   real                                         :: beta   
   real                                         :: gamra
   integer                                      :: np,ip
   real                                         :: fval
!
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
   ier = 0
   if (fctype) then
       fval = ref%fc
   else
       fval = ref%fo
   endif
!
   LOOP_KALPHA : do kal=1,nwave
     tthdeg = ref%tthd(kal)
!
     beta   = pdph(nph)%pvet(jref,kal,BETAPAR)
     gamra  = pdph(nph)%pvet(jref,kal,GAMRATIO)
     cpear  = pdph(nph)%pvet(jref,kal,PEARPAR1)
     cpear2 = pdph(nph)%pvet(jref,kal,PEARPAR2)
!
     uvw2 =  ref%fwhm(kal) * ref%pk * ref%rapI
!
     iniz = clocate(xcount,tthdeg-uvw2)
     ifin = clocate(xcount,tthdeg+uvw2)
!
     fval = ref%fc
     kffx = fval**2*ref%m*ref%po*ref%lp(kal)*ref%ab(kal)*ratio(kal)
!
     call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                            ref%fwhm(kal),pear%par(P_ASYM1PAR:P_ASYM4PAR)%val,beta)
!
     np = ifin - iniz + 1        
     allocate(xp(np), yp(np))
     LOOP_CONTEGGI : do nnn=iniz,ifin
        deltath=xcount(nnn)-tthdeg
        deltath2=deltath**2
        argbeta=(1.0+cpear2*deltath2)
        pears = gamra * cpear * argbeta**(-beta)
        ip = nnn-iniz+1
        xp(ip) = xcount(nnn)
        yp(ip) = scal*kffx*pears*asymval(ip) + yb(nnn)
     enddo LOOP_CONTEGGI
   enddo LOOP_KALPHA
!
   end subroutine profile_curve_pearson

!--------------------------------------------------------------------
!corr    subroutine defrange_pears(pear,ref,inizr,ifinr,kvolte)
!corr    USE Counts
!corr !corr   USE RefinecomRef
!corr !corr   USE variables, only:ref
!corr    USE arrayutil
!corr !corr   USE profile_function, only: rapIcalc
!corr    USE reflection_type_util
!corr !
!corr    implicit none
!corr    type(profile_function_t), intent(in) :: pear
!corr    type(reflection_type), dimension(:), intent(inout) :: ref
!corr    integer, intent(in)    :: inizr,ifinr
!corr    integer, intent(in)    :: kvolte
!corr    integer                :: j,nnn,kal
!corr    real , dimension(5000) :: drpf
!corr    integer                :: nrp,nrpe 
!corr    real                   :: beta,asym
!corr    real                   :: tthdeg
!corr    real                   :: uvw2,ungrad
!corr    real                   :: deltath,deltath2,deltath3,cc,ccas
!corr    real                   :: asterm,pears
!corr    real                   :: summ
!corr    integer                :: iniz,ifin
!corr !
!corr    asym = param%par(P_ASYM1PAR)
!corr    kal=1
!corr    LOOP_RIFLESSI : do j=inizr,ifinr
!corr      tthdeg = ref(j)%tthd
!corr      beta = pdph(nph)%pvet(j,kal,BETAPAR)
!corr      uvw2 =  ref(j)%fwhm(kal) * ref(j)%pk * ref(j)%rapI
!corr !
!corr          iniz = clocate(ttetaczero,tthdeg-uvw2)
!corr          ifin = clocate(ttetaczero,tthdeg+uvw2)
!corr !
!corr      nrp = 0
!corr      nrpe = 0
!corr      LOOP_CONTEGGI : do nnn=iniz,ifin
!corr         if (nrp.lt.5000) nrp = nrp + 1
!corr         deltath=ttetaczero(nnn)-tthdeg
!corr         deltath2=deltath**2
!corr         deltath3=deltath*deltath2
!corr !corr        cc=(1.0+phkl(j,4)*deltath2)
!corr !corr        ccas=1.0/phkl(j,4)+deltath2
!corr !corr        pears = phkl(j,3) * cc**(-beta)
!corr !corr         cc=(1.0+cpear2v(j,kal)*deltath2)
!corr !corr         ccas=1.0/cpear2v(j,kal)+deltath2
!corr          cc=1.0+pdph(nph)%pvet(j,kal,PEARPAR2)*deltath2
!corr          ccas=1.0/pdph(nph)%pvet(j,kal,PEARPAR2)+deltath2
!corr !corr         pears = cpearv(j,kal) * cc**(-beta)    ! gamrav(j) !!!
!corr          pears = pdph(nph)%pvet(j,kal,PEARPAR1) * cc**(-beta)    ! gamrav(j) !!!
!corr         asterm= 1.0 + asym*deltath3/(ccas**1.5)
!corr         drpf(nrp) = pears*asterm
!corr      enddo LOOP_CONTEGGI
!corr !
!corr      ungrad = 1.0
!corr        summ = 0.5*SUM((ttetaczero(2:nrp)-ttetaczero(1:nrp-1))*(drpf(2:nrp) + drpf(1:nrp-1)))*100.0
!corr      if (kvolte.le.4) then
!corr          if (summ.lt.99.5) then
!corr              ref(j)%pk = ref(j)%pk + (100. - summ)
!corr              if (ref(j)%pk > 20.0) ref(j)%pk = 20.0
!corr          endif
!corr          if (summ.gt.99.99) then
!corr              ref(j)%pk = ref(j)%pk - 1.0
!corr              if (ref(j)%pk < 3.0) ref(j)%pk = 3.0
!corr          endif
!corr      endif
!corr    enddo LOOP_RIFLESSI
!corr !
!corr    end subroutine defrange_pears
END MODULE pearsonf
