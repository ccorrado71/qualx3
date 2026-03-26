module psvoigtf
  USE progtype, only: rparam_type
  USE trig_constants, only: pi,dtor,rtod

  implicit none 

  integer, parameter :: PV_UPAR=1,PV_VPAR=2,PV_WPAR=3,PV_E0PAR=4,PV_E1PAR=5,PV_E2PAR=6,    &
                        PV_ASYM1PAR=7,PV_ASYM2PAR=8,PV_ASYM3PAR=9,PV_ASYM4PAR=10

  real, parameter :: GAUSCONST2 = 4.0*alog(2.0)
  real, parameter :: GAUSCONST = sqrt(GAUSCONST2/pi)
  real, parameter :: CONSTLOREN=2.0/pi

  integer, parameter :: ETAPAR=1,UVWPAR=2

  real, dimension(:), allocatable :: asymval
  real, private, dimension(:,:), allocatable :: der_asym

contains 

   function init_psvoigt_param()  result(param)
   USE progtype, only: rparam_type
   USE profile_function
   type(profile_function_t) :: param

   param%par(PV_UPAR)     = rparam_type(val=0.0,str='U')
   param%par(PV_VPAR)     = rparam_type(val=0.0,str='V')
   param%par(PV_WPAR)     = rparam_type(val=0.01,str='W')
   !param%par(PV_WPAR)     = rparam_type(val=0.0001,str='W')
   param%par(PV_E0PAR)    = rparam_type(val=0.5,str='eta0')
   param%par(PV_E1PAR)    = rparam_type(val=0.0,str='eta1')
   param%par(PV_E2PAR)    = rparam_type(val=0.0,str='eta2')
   param%par(PV_ASYM1PAR) = rparam_type(val=0.0,str='asym1')
   param%par(PV_ASYM2PAR) = rparam_type(val=0.0,str='asym2')
   param%par(PV_ASYM3PAR) = rparam_type(val=0.0,str='asym3')
   param%par(PV_ASYM4PAR) = rparam_type(val=0.0,str='asym4')

   end function init_psvoigt_param

!------------------------------------------------------------------------------------

   subroutine psvoigt(pvoi,ref,xcount,fs,ycount,nwave,ratio,nph,ier)
   USE arrayutil
   USE profile_function
   USE reflection_type_util
   USE asymfunc
!
   type(profile_function_t), intent(in)             :: pvoi
   type(reflection_type), dimension(:), intent(inout) :: ref
   real, dimension(:), intent(in)                     :: xcount
   real, dimension(:), intent(in)                     :: fs    
   real, dimension(:), intent(out)                    :: ycount
   real, dimension(:), intent(in)                     :: ratio
   integer, intent(in)                                :: nph
   integer, intent(in)                                :: nwave
   integer, intent(out)                               :: ier
   integer                                            :: j,nnn
   integer                                            :: iniz,ifin
   real                                               :: eta,eta0,eta1,eta2
   real                                               :: fu,fv,fw
   real                                               :: uvw2,uvwqg
!corr   real                                               :: asym
   real                                               :: tthdeg
   real                                               :: rdeg,tdeg,tdeg2
   real                                               :: cgaus,cgaus2,cloren,cloren2
   real                                               :: ffx
   real                                               :: deltath,deltath2
   real                                               :: pvoigt
   integer                                            :: negfw
   integer                                            :: kal
!
   ycount(:) = 0.0
!
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
!
   fu   =   pvoi%par(PV_UPAR)%val    
   fv   =   pvoi%par(PV_VPAR)%val    
   fw   =   pvoi%par(PV_WPAR)%val    
   eta0 =   pvoi%par(PV_E0PAR)%val   
   eta1 =   pvoi%par(PV_E1PAR)%val   
   eta2 =   pvoi%par(PV_E2PAR)%val   
!corr   asym =   pvoi%par(PV_ASYM1PAR)%val
   negfw = 0
   ier = 0
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=1,size(ref)
         tthdeg = ref(j)%tthd(kal)
         rdeg = 0.5 * tthdeg * dtor
         !tdeg = sin(rdeg) / cos(rdeg)
         tdeg = tan(rdeg)
         tdeg2 = tdeg * tdeg
!
         eta = eta0 + eta1*tdeg + eta2*tdeg2
         if (eta < 0 .or. eta > 1) then
             negfw = negfw + 1
         endif
         uvwqg = fu*tdeg2 + fv*tdeg + fw
         if (uvwqg < 0.0001) then
             uvwqg = 0.0001
             negfw = negfw + 1
         endif
         pdph(nph)%pvet(j,kal,ETAPAR) = eta
         pdph(nph)%pvet(j,kal,UVWPAR) = 1./uvwqg
         ref(j)%fwhm(kal) = sqrt(uvwqg)
         cgaus =   GAUSCONST / ref(j)%fwhm(kal)
         cloren = CONSTLOREN / ref(j)%fwhm(kal)
         cgaus2 =   GAUSCONST2 * pdph(nph)%pvet(j,kal,UVWPAR)
         cloren2 = 4.0 * pdph(nph)%pvet(j,kal,UVWPAR)
!
         uvw2 =  ref(j)%fwhm(kal) * ref(j)%pk * ref(j)%rapI
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         ffx = ref(j)%m*fs(j)**2*ref(j)%po*ref(j)%lp(kal)*ref(j)%ab(kal)*ratio(kal)
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                                ref(j)%fwhm(kal),pvoi%par(PV_ASYM1PAR:PV_ASYM4PAR)%val)
!
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            pvoigt = eta*cloren/(1.0+cloren2*deltath2) + (1.0-eta)*cgaus*exp(-cgaus2*deltath2)
            !ycount(nnn)=ycount(nnn)+scal*ffx*pvoigt*asymval(nnn-iniz+1)
            ycount(nnn)=ycount(nnn)+ffx*pvoigt*asymval(nnn-iniz+1)
         enddo LOOP_CONTEGGI
      enddo LOOP_RIFLESSI
   enddo LOOP_KALPHA
   !if(negfw>0)write(0,*)'NWGFW=',negfw,real(negfw)/size(ref)
   if (real(negfw)/size(ref) > 0.4) ier = 1
!
   end subroutine psvoigt

!-------------------------------------------------------------------------------------
  
   subroutine derpsvoi(pvoi,inizr,ifinr,xcount,ref,cell,derFct,derOP,deryz,dfw,derp,dercc,derG,  &
                       ratio,scal,refine_cell,refine_profile,nwave,gpocode,radtype,sync,wave,spg,nph)
   USE arrayutil
   USE profile_function
   USE reflection_type_util
   USE asymfunc
   USE elements, only: NEUTRON_SOURCE
   USE spginfom, only: spaceg_type
!
   type(profile_function_t), intent(in)          :: pvoi
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
   real, dimension(:), intent(out)                 :: derG
   real, dimension(:), intent(in)                  :: ratio
   real, intent(in)                                :: scal
   logical, intent(in)                             :: refine_cell, refine_profile
   integer, intent(in)                             :: nwave
   integer, intent(in)                             :: gpocode
   integer, intent(in)                             :: radtype, nph
   type(spaceg_type), intent(in)                   :: spg
   logical, intent(in)                             :: sync
   real, intent(in)                                :: wave
   integer                                         :: j,k,kal,nnn
   integer                                         :: iniz,ifin
!corr   real                                            :: asym
   real                                            :: eta0,eta1,eta2,eta
   real                                            :: tthdeg
   real                                            :: rdeg,tdeg,tdeg2
   real                                            :: uvwg,uvwqg,uvw2
   real                                            :: pesolp,absoc
   real                                            :: derft,dlorp
   real                                            :: ffnop,ffx,ffb1
   real                                            :: deltath,deltath2,cc
   real                                            :: asterm,pvoigt,pgau,plor
   real                                            :: deruvw,dereta,derasth
   real                                            :: derpth,deryop
   real, dimension(6)                              :: dercells
   real                                            :: derfunczero,deraszero
   real, dimension(3)                              :: vetder,gradprof
   real                                            :: constg
   real                                            :: fu,fv    !!!!!,fw
   real                                            :: secth2
   real                                            :: kpol
   real                                            :: cgaus,cgaus2,cloren,cloren2
!
   if (size_array(der_asym,1) < size(xcount)) call new_array(der_asym,[1,1],[size(xcount),7])
   if (radtype == NEUTRON_SOURCE .or. sync) then
       kpol = 0.0
   else
       kpol = 0.8
   endif
   dercc = 0.0
   derG(:)  = 0.0
!
   vetder(1) = 1.0
   fu   =   pvoi%par(PV_UPAR)%val    
   fv   =   pvoi%par(PV_VPAR)%val    
   eta0 =   pvoi%par(PV_E0PAR)%val   
   eta1 =   pvoi%par(PV_E1PAR)%val   
   eta2 =   pvoi%par(PV_E2PAR)%val   
!corr   asym =   pvoi%par(PV_ASYM1PAR)%val
   constg = 4.0*alog(2.0)
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=inizr,ifinr
         tthdeg = ref(j)%tthd(kal)
         derft = derFct(j)  
         eta = pdph(nph)%pvet(j,kal,ETAPAR)
         rdeg = 0.5 * tthdeg * dtor
         tdeg = tan(rdeg)
         tdeg2 = tdeg * tdeg
         secth2 = 1.0 / cos(rdeg)**2
         uvwg = ref(j)%fwhm(kal)
         uvwqg = uvwg*uvwg
         cgaus =   GAUSCONST / ref(j)%fwhm(kal)
         cloren = CONSTLOREN / ref(j)%fwhm(kal)
         cgaus2 =   GAUSCONST2 * pdph(nph)%pvet(j,kal,UVWPAR)
         cloren2 = 4.0 * pdph(nph)%pvet(j,kal,UVWPAR)

         uvw2 = ref(j)%fwhm(kal) * ref(j)%pk * ref(j)%rapI
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         if(refine_cell)then
            dlorp = derivataLP(ref(j)%tthd(kal),kpol)
            !dlorp = 0.0 
            dercells = scal*derteta_risp_celldir(spg,cell,ref(j)%hkl,ref(j)%tthd(kal),wave)   !(der. di theta risp. alla cella) * scala
            vetder(2) = secth2 * (eta1 + 2.0*eta2*tdeg)               !der. eta rispetto theta del riflesso
            vetder(3) = secth2 * (fv + 2.0*fu*tdeg)*0.5/uvwg          !der. FWHM rispetto theta del riflesso
         endif
         pesolp = ref(j)%lp(kal)
         absoc = ref(j)%ab(kal)
         ffnop = pesolp*ref(j)%m*(ref(j)%fc**2)*absoc
         ffx = ffnop*ref(j)%po
         ffb1 = ratio(kal)*pesolp*ref(j)%m*ref(j)%po*absoc
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin) - tthdeg,tthdeg,   &
                                ref(j)%fwhm(kal),pvoi%par(PV_ASYM1PAR:PV_ASYM4PAR)%val,der=der_asym)
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            cc=(1.0+cloren2*deltath2)
            plor= cloren/cc
            pgau= cgaus*exp(-cgaus2*deltath2)
            pvoigt = eta*plor + (1.0-eta)*pgau
            !asterm=1.0-(asym*sign(1.0,deltath)*deltath2)/tdeg
            asterm=asymval(nnn-iniz+1)
!
!           Derivative of 2theta corrected
            !deraszero = -2.0*asym*sign(1.0,deltath)*deltath/tdeg                     ! der. asym. rispetto 2theta corrected
            deraszero = der_asym(nnn-iniz+1,2)        ! der. asym. rispetto 2theta corrected
            derfunczero = -2.0*deltath*(cloren2*plor*eta/cc + cgaus2*(1.0-eta)*pgau) ! der. pvoigt rispetto 2theta corrected
            deryz(nnn) = deryz(nnn) + scal * ffx * (derfunczero*asterm + deraszero*pvoigt)
!
!           Derivata Pseudo-Voigt rispetto eta
            gradprof(2) = plor - pgau
!
!           Derivata Pseudo-Voigt rispetto FWHM
            gradprof(3) = ( eta*plor*(8.0*deltath2-cc*uvwqg)/cc + (eta-1.0)*pgau*(uvwqg-2.0*constg*deltath2) ) / uvwg**3
!
            if (refine_profile) then
              deruvw = scal * ffx * (asterm * gradprof(3) + der_asym(nnn-iniz+1,3)*pvoigt) / (2.0*uvwg)   !gradprof(3)/(2.0*uvwg) = der. rispetto FWHM**2
              dfw(nnn,1)=dfw(nnn,1) + deruvw * tdeg2    !der. rispetto U
              dfw(nnn,2)=dfw(nnn,2) + deruvw * tdeg     !der. rispetto V
              dfw(nnn,3)=dfw(nnn,3) + deruvw            !der. rispetto W
!
              !derp(nnn,1)=derp(nnn,1) - scal*ffx*pvoigt*sign(1.0,deltath)*deltath2/tdeg !der. rispetto asym
              !derp(nnn,1)=derp(nnn,1) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,4) !der. rispetto asym
              !derp(nnn,5)=derp(nnn,5) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,5) !der. rispetto asym
              !derp(nnn,6)=derp(nnn,6) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,6) !der. rispetto asym
              !derp(nnn,7)=derp(nnn,7) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,7) !der. rispetto asym
              derp(nnn,4)=derp(nnn,4) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,4) !der. rispetto asym
              derp(nnn,5)=derp(nnn,5) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,5) !der. rispetto asym
              derp(nnn,6)=derp(nnn,6) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,6) !der. rispetto asym
              derp(nnn,7)=derp(nnn,7) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,7) !der. rispetto asym
!
              dereta = scal * ffx * asterm * gradprof(2)
              derp(nnn,1)=derp(nnn,1) + dereta            !der. rispetto eta0
              derp(nnn,2)=derp(nnn,2) + dereta * tdeg     !der. rispetto eta1
              derp(nnn,3)=derp(nnn,3) + dereta * tdeg2    !der. rispetto eta2
            endif
            if (refine_cell) then
               !derasth=asym*sign(1.0,deltath)*deltath*(rtod*4.0/tdeg + deltath/(sin(rdeg)**2)) !der.asym risp. theta
!
!              d(asym(theta,H(theta)) = d(asym(theta))/d(theta) + d(asym(H))/d(H) * d(H)/d(theta)
               derasth=der_asym(nnn-iniz+1,1) + der_asym(nnn-iniz+1,3)*vetder(3)   !der.asym risp. theta
!
!              Derivata Psvoig rispetto theta del riflesso
               gradprof(1) = rtod*4.0*deltath*(eta*4.0*plor/cc + (1-eta)*constg*pgau)/uvwqg
!
               derpth = DOT_PRODUCT(gradprof,vetder)       !derivata rispetto theta del riflesso
               do k=1,6
                   dercc(nnn,k)=dercc(nnn,k)+dercells(k)*            &
                        (ffx*(derpth*asterm+derasth*pvoigt)+(ffb1*derft+ffx/pesolp*dlorp)*pvoigt*asterm)
               enddo
            endif
!
            if (gpocode > 0) then
                deryop = scal * ffnop * pvoigt * asterm
                derG(nnn)=derG(nnn) + deryop*derOP(j)
            endif
         enddo LOOP_CONTEGGI
      enddo LOOP_RIFLESSI
   enddo LOOP_KALPHA
!
   end subroutine derpsvoi

!-----------------------------------------------------------------------

   subroutine derpsvoia(ref,pvoi,inizr,ifinr,xcount,der,derc,nwave,ratio,scal,nph)
   USE progtype, only : deratom
   USE reflection_type_util, only : reflection_type
   USE arrayutil
   USE profile_function
   USE asymfunc
!
   type(reflection_type), dimension(:), intent(in) :: ref
   type(profile_function_t), intent(in)          :: pvoi
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
!corr   real                                            :: rdeg,tdeg
   real                                            :: eta,uvw2
   real                                            :: deltath,deltath2
   real                                            :: pvoigt
   real                                            :: ffb1,dydI
   real                                            :: cgaus1,cgaus2,cloren1,cloren2
   integer                                         :: nat
!
   derc = deratom(0.0,0.0,0.0)
   nat = size(derc,2)
!
!corr   asym = pvoi%asym(1)%val
!corr   asym = pvoi%par(PV_ASYM1PAR)%val
!corr   scal = scalr%val
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=inizr,ifinr
         tthdeg = ref(j)%tthd(kal)
!corr         rdeg = 0.5 * tthdeg * dtor
!corr         tdeg = tan(rdeg)
!
         cgaus2 =   GAUSCONST/ref(j)%fwhm(kal)
         cloren1 = CONSTLOREN / ref(j)%fwhm(kal)
!corr         cloren2 = 4.0 * ruvw(j,kal)
!corr         cgaus1 =   GAUSCONST2 * ruvw(j,kal)
         cloren2 = 4.0 * pdph(nph)%pvet(j,kal,UVWPAR)
         cgaus1 =   GAUSCONST2 * pdph(nph)%pvet(j,kal,UVWPAR)
!
         eta = pdph(nph)%pvet(j,kal,ETAPAR)
         uvw2 = ref(j)%fwhm(kal) * ref(j)%pk * ref(j)%rapI
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         !!!!!opT = sum(frac*oprcorr(j,:))
         !!!!!opT = oprcorr(j,1)
         ffb1 = ratio(kal)*ref(j)%lp(kal)*ref(j)%m*ref(j)%po*ref(j)%ab(kal)
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin) - tthdeg,tthdeg,ref(j)%fwhm(kal),    &
                                pvoi%par(PV_ASYM1PAR:PV_ASYM4PAR)%val)
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            pvoigt = eta*cloren1/(1.0+cloren2*deltath2) + (1.0-eta)*cgaus2*exp(-cgaus1*deltath2)
!
!           calcolo derivate param. strutt. per atomo
            dydI = scal * ffb1 * pvoigt * asymval(nnn-iniz+1)
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
   end subroutine derpsvoia

!--------------------------------------------------------------------

   subroutine foleb_psvoi(ref,inizr,ifinr,Icalc,Iobs,pvoi,scal,yoss,ytot,ttetaczero,back,nph)
   USE arrayutil
   USE reflection_type_util, only: reflection_type
   USE asymfunc
   USE profile_function
!
   type(reflection_type), dimension(:), intent(in) :: ref
   integer, intent(in)                             :: inizr,ifinr
   real, dimension(:), intent(in)                  :: Icalc
   real, dimension(:), intent(out)                 :: Iobs
   type(profile_function_t), intent(in)          :: pvoi
   real, intent(in)                                :: scal
   real, dimension(:), intent(in)                  :: yoss,ytot,ttetaczero,back
   integer, intent(in)                             :: nph
   integer                                         :: iniz,ifin
   real                                            :: eta
   integer                                         :: j,nnn
   real                                            :: tthdeg,deltath,deltath2
   real                                            :: uvw2
   real                                            :: yci,diffo,ycalc
   real                                            :: plor,pgau,pvoigt
   real                                            :: sumy
   integer                                         :: nneg
   real                                            :: epsmch = epsilon(1.0)
   real                                            :: cgaus,cgaus2,cloren,cloren2
   integer                                         :: kal
!
   nneg = 0
   kal = 1
   LOOP_RIFLESSI : do j=inizr,ifinr
      sumy = 0.0
      tthdeg = ref(j)%tthd(kal)
!
      eta = pdph(nph)%pvet(j,kal,ETAPAR)
      uvw2 = ref(j)%fwhm(kal) * ref(j)%pk * ref(j)%rapI
      cgaus =   GAUSCONST / ref(j)%fwhm(kal)
      cgaus2 =   GAUSCONST2 * pdph(nph)%pvet(j,kal,UVWPAR)
      cloren = CONSTLOREN / ref(j)%fwhm(kal)
      cloren2 = 4.0 * pdph(nph)%pvet(j,kal,UVWPAR)
!
      iniz = clocate(ttetaczero,tthdeg-uvw2)
      ifin = min(clocate(ttetaczero,tthdeg+uvw2),size(ytot))
!
      call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,ttetaczero(iniz:ifin)-tthdeg,tthdeg,   &
                             ref(j)%fwhm(kal),pvoi%par(PV_ASYM1PAR:PV_ASYM4PAR)%val)
      LOOP_CONTEGGI : do nnn=iniz,ifin
         deltath=ttetaczero(nnn)-tthdeg
         deltath2=deltath**2
         plor= cloren/(1.0+cloren2*deltath2)
         pgau= cgaus*exp(-cgaus2*deltath2)
         pvoigt = eta*plor + (1.0-eta)*pgau
         yci = scal * pvoigt * asymval(nnn-iniz+1)
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
          nneg = nneg + 1
      endif
!
   enddo LOOP_RIFLESSI
   if (nneg > 0) write(6,*)'IOBS NEGATIVI n=',nneg
!
   end subroutine foleb_psvoi

!------------------------------------------------------------------------------------

   subroutine profile_curve_psvoigt(pvoi,ref,jref,fctype,xcount,yb,scal,nwave,ratio,nph,xp,yp,ier)
   USE reflection_type_util, only:reflection_type
   USE arrayutil
   USE profile_function
   USE asymfunc
!
   type(profile_function_t), intent(in)         :: pvoi
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
   real                                         :: uvw2,eta
   integer                                      :: kal,nnn
   integer                                      :: iniz,ifin
   real                                         :: tthdeg
   real                                         :: cgaus,cgaus2,cloren,cloren2
   real                                         :: kffx
   real                                         :: deltath,deltath2
   real                                         :: pvoigt
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
     eta = pdph(nph)%pvet(jref,kal,ETAPAR)
     uvw2 = ref%fwhm(kal) * ref%pk * ref%rapI
     cgaus =   GAUSCONST / ref%fwhm(kal)
     cgaus2 =   GAUSCONST2 * pdph(nph)%pvet(jref,kal,UVWPAR)
     cloren = CONSTLOREN / ref%fwhm(kal)
     cloren2 = 4.0 * pdph(nph)%pvet(jref,kal,UVWPAR)
!
     iniz = clocate(xcount,tthdeg-uvw2)
     ifin = clocate(xcount,tthdeg+uvw2)
!
     fval = ref%fc
     kffx = fval**2*ref%m*ref%po*ref%lp(kal)*ref%ab(kal)*ratio(kal)
!
     call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                            ref%fwhm(kal),pvoi%par(PV_ASYM1PAR:PV_ASYM4PAR)%val)
!
     np = ifin - iniz + 1        
     allocate(xp(np), yp(np))
     LOOP_CONTEGGI : do nnn=iniz,ifin
        deltath=xcount(nnn)-tthdeg
        deltath2=deltath**2
        pvoigt = eta*cloren/(1.0+cloren2*deltath2) + (1.0-eta)*cgaus*exp(-cgaus2*deltath2)
        ip = nnn-iniz+1
        xp(ip) = xcount(nnn)
        yp(ip) = scal*kffx*pvoigt*asymval(ip) + yb(nnn)
     enddo LOOP_CONTEGGI
   enddo LOOP_KALPHA
!
   end subroutine profile_curve_psvoigt

end module psvoigtf
