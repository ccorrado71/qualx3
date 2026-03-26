 MODULE tchzf
   USE trig_constants
   USE progtype, only: rparam_type
   implicit none
  
   type tch_function_type
      type(rparam_type) :: u,v,w,z
      type(rparam_type) :: x,y
      type(rparam_type), dimension(4) :: asym
   end type tch_function_type

   integer, parameter :: T_UPAR=1,T_VPAR=2,T_WPAR=3,T_ZPAR=4,T_XPAR=5,T_YPAR=6,    &
                         T_ASYM1PAR=7,T_ASYM2PAR=8,T_ASYM3PAR=9,T_ASYM4PAR=10

   real, parameter, private :: CONSTG1 = 4.0*alog(2.0)
   real, parameter, private :: CONSTG2 = sqrt(CONSTG1/pi)
   real, parameter, private :: CONSTL  = 2.0/pi

   integer, parameter, private :: ETAPAR=1

!corr   private :: init_calc

   real, private, dimension(:), allocatable :: asymval
   real, private, dimension(:,:), allocatable :: der_asym


 CONTAINS 
   
!corr   subroutine init_calc(n)
!corr   USE arrayutil
!corr   integer, intent(in)          :: n
!corr!
!corr!corr   if (allocated(etav) .and. n == size(etav)) return
!corr!corr   call new_array(etav,n)
!corr!
!corr   end subroutine init_calc

!----------------------------------------------------------------------------------------------------

   function init_tchz_param()  result(param)
   USE progtype, only: rparam_type
   USE profile_function
   type(profile_function_t) :: param

   param%par(T_UPAR)     = rparam_type(val=0.0,str='U')
   param%par(T_VPAR)     = rparam_type(val=0.0,str='V')
   param%par(T_WPAR)     = rparam_type(val=0.01,str='W')
   param%par(T_ZPAR)     = rparam_type(val=0.0,str='Z')
   param%par(T_XPAR)     = rparam_type(val=0.01,str='X')
   param%par(T_YPAR)     = rparam_type(val=0.0,str='Y')
   param%par(T_ASYM1PAR) = rparam_type(val=0.0,str='asym1')
   param%par(T_ASYM2PAR) = rparam_type(val=0.0,str='asym2')
   param%par(T_ASYM3PAR) = rparam_type(val=0.0,str='asym3')
   param%par(T_ASYM4PAR) = rparam_type(val=0.0,str='asym4')

   end function init_tchz_param

!------------------------------------------------------------------------------------

   subroutine psvoigt_tchz(tch,ref,xcount,fs,ycount,nwave,ratio,nph,ier)
   USE reflection_type_util, only:reflection_type
   USE arrayutil
   USE profile_function
   USE asymfunc
!
   implicit none
   type(profile_function_t), intent(in)                :: tch
   type(reflection_type), dimension(:), intent(inout) :: ref
   real, dimension(:), intent(in)                     :: xcount
   real, dimension(:), intent(in)                     :: fs    
   real, dimension(:), intent(out)                    :: ycount
   integer, intent(in)                                :: nwave
   real, dimension(:), intent(in)                     :: ratio
   integer, intent(in)                                :: nph
   integer, intent(out)                               :: ier
   integer                                            :: iniz,ifin
   integer                                            :: j,kal,nnn
   real                                               :: eta
   real                                               :: fU,fV,fW,fZ,fX,fY
   real                                               :: rdeg,tanth,costh
   real                                               :: uvw2
   real                                               :: hg,hl,fwhm,qh,argfwhm
   real                                               :: tthdeg
   real                                               :: cgaus2,cgaus1,cloren1,cloren2
   real                                               :: ffx
   real                                               :: deltath,deltath2
   real                                               :: pvoigt   !,asterm
   real                                               :: arghg
   integer                                            :: neg1,neg2,negfw
!
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
!
   neg1=0
   neg2=0
   ier = 0
   ycount(:) = 0.0
!
   fU = tch%par(T_UPAR)%val
   fV = tch%par(T_VPAR)%val
   fW = tch%par(T_WPAR)%val
   fZ = tch%par(T_ZPAR)%val
   fX = tch%par(T_XPAR)%val
   fY = tch%par(T_YPAR)%val
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=1,size(ref)
         tthdeg = ref(j)%tthd(kal)
         rdeg = 0.5 * tthdeg * dtor
         costh = cos(rdeg)
         tanth = sin(rdeg) / costh
!
         arghg = fU*tanth**2 + fV*tanth + fW + fZ/costh**2
         if (arghg < epsilon(0.1)) then
           ! write(6,*)'NEGATIV'
             arghg = 0.0000001
             neg1 = neg1+1
           ! ierprof = 1
           ! return
         endif
         hg = sqrt (arghg)                                        !componente gaussiana della FWHM
         hl = fX*tanth + fY/costh                                 !componente lorentziana della FWHM
         argfwhm = hg**5 + 2.69269*hg**4*hl + 2.42843*hg**3*hl**2 + 4.47163*hg**2*hl**3 + 0.07842*hg*hl**4 + hl**5
         if (argfwhm < epsilon(0.1)) then
         !!!!    write(0,*)'WARNING argfwhm = ',argfwhm,j
             argfwhm = 0.0000001
             neg2 = neg2+1
         !   write(0,*)'WARNING argfwhm'
         !   write(6,*)'WARNING argfwhm'
         !   ierprof = 1
         !   return
         endif
         fwhm = argfwhm**0.2
         qh = hl / fwhm
         eta = 1.36603*qh - 0.47719*qh**2 + 0.11116*qh**3         !PV mixing parameter
!
         cgaus1 =   CONSTG1/fwhm**2
         cgaus2 =   CONSTG2/fwhm
         cloren1 = CONSTL / fwhm
         cloren2 = 4.0 / fwhm**2
!
         ref(j)%fwhm(kal) = fwhm
         pdph(nph)%pvet(j,kal,ETAPAR) = eta
         uvw2 =  fwhm * ref(j)%pk * ref(j)%rapI                  !ampiezza del picco
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         ffx = ref(j)%m*fs(j)**2*ref(j)%po*ref(j)%lp(kal)*ref(j)%ab(kal)*ratio(kal)
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                                ref(j)%fwhm(kal),tch%par(T_ASYM1PAR:T_ASYM4PAR)%val)
!
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            pvoigt = eta*cloren1/(1.0+cloren2*deltath2) + (1.0-eta)*cgaus2*exp(-cgaus1*deltath2)
            !asterm=1.0-(asym*sign(1.0,deltath)*deltath2)/tanth
            !ycount(nnn)=ycount(nnn)+ffx*pvoigt*asterm
            !    if (abs(asterm -asymval(nnn-iniz+1)) > 0.001) write(0,*)'ASYM=',asterm,asymval(nnn-iniz+1)
            ycount(nnn)=ycount(nnn)+ffx*pvoigt*asymval(nnn-iniz+1)
         enddo LOOP_CONTEGGI
      enddo LOOP_RIFLESSI
   enddo LOOP_KALPHA
!
   !negfw = neg1+neg2
   negfw = neg1
   if (negfw>0)then
!!!       write(0,*)'VALORI FUNZIONE NEGATIVI ',neg1,neg2
       !if (real(negfw)/size(ref) > 0.4) ierprof = 1
   endif
!
   end subroutine psvoigt_tchz

!--------------------------------------------------------------------

   subroutine derpsvoi_tchz(tch,inizr,ifinr,xcount,ref,cell,derFct,derOP,deryz,dfw,derp,dercc,derG,    &
                            ratio,scal,refine_cell,refine_profile,nwave,gpocode,radtype,sync,wave,spg)
   USE arrayutil
   USE profile_function
   USE reflection_type_util
   USE elements, only: NEUTRON_SOURCE
   USE spginfom, only: spaceg_type
   USE asymfunc
!
   implicit none
   type(profile_function_t), intent(in)            :: tch
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
   logical, intent(in)                             :: refine_cell,refine_profile
   integer, intent(in)                             :: nwave
   integer, intent(in)                             :: gpocode
   integer, intent(in)                             :: radtype
   type(spaceg_type), intent(in)                   :: spg
   logical, intent(in)                             :: sync
   real, intent(in)                                :: wave
   integer                                         :: j,k,kal,nnn
   integer                                         :: iniz,ifin
!corr   logical                                         :: iderc
   real                                            :: asym    !,scal
   real                                            :: cgaus2,cgaus1,cloren1,cloren2
   real                                            :: qh,eta,detaqh,dqhfwhm
   real                                            :: detah
   real                                            :: tthdeg
   real                                            :: uvw2
   real                                            :: pesolp,absoc
   real                                            :: derft,dlorp
   real                                            :: ffnop,ffx,ffb1
   real                                            :: deltath,deltath2,cc
   real                                            :: asterm,pvoigt,pgau,plor
   real                                            :: deruvw,derasth
   real                                            :: derpth,deryop
   real, dimension(6)                              :: dercells
   real                                            :: derfunczero,deraszero
!corr   real, dimension(2)                              :: rap
   real, dimension(3)                              :: vetder,gradprof
   real                                            :: fU,fV,fW,fZ,fX,fY
   real                                            :: hg,hl,fwhm,argfwhm,pfw
   real                                            :: derfg,derfl
   real                                            :: dfwhmg,dfwhml
   real                                            :: derhgt,derhlt
   real                                            :: dconst
   real                                            :: rdeg,tanth,tanth2,senth,costh
   real                                            :: secth2
   real, dimension(6)                              :: ct
   real, dimension(6)                              :: hgv,hlv
   integer, dimension(6)                           :: ci
   real                                            :: arghg
   real                                            :: kpol
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
   ct = (/1.0,2.69269,2.42843,4.47163,0.07842,1.0/)
   ci = (/0,1,2,3,4,5/)
   vetder(1) = 1.0
   fU = tch%par(T_UPAR)%val
   fV = tch%par(T_VPAR)%val
   fW = tch%par(T_WPAR)%val
   fZ = tch%par(T_ZPAR)%val
   fX = tch%par(T_XPAR)%val
   fY = tch%par(T_YPAR)%val
   asym = tch%par(T_ASYM1PAR)%val
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=inizr,ifinr
         tthdeg = ref(j)%tthd(kal)
         rdeg = 0.5 * tthdeg * dtor
         costh = cos(rdeg)
         senth = sin(rdeg)
         tanth = senth / costh
         tanth2 = tanth**2
!
         arghg = fU*tanth2 + fV*tanth + fW + fZ/costh**2
         if (arghg < epsilon(0.1)) then
         !   write(0,*)'NEGATIV'
             arghg = 0.0000001
         endif
         hg = sqrt ( arghg )    !componente gaussiana della FWHM
         hl = fX*tanth + fY/costh                                 !componente lorentziana della FWHM
         hgv = hg**ci(6:1:-1); hlv = hl**ci                       !potenze di hg e hv
         argfwhm = sum(ct*hgv*hlv)
         if (argfwhm < 0.0) then
             argfwhm = 0.0000001
             write(6,*)'WARN DERIVATE'
         endif
         fwhm = argfwhm**0.2                                      !FWHM
         pfw = 0.2*argfwhm**(-0.8)
         dfwhmg = pfw*sum(ct(:5)*ci(6:2:-1)*hgv(2:)*hlv(:5))      !der. di FWHM risp. hg
         dfwhml = pfw*sum(ct(2:6)*ci(2:6)*hgv(2:6)*hlv(:5))       !der. di FWHM risp. hl
!
         cgaus1 =  CONSTG1 / fwhm**2
         cgaus2 =  CONSTG2 / fwhm
         cloren1 = CONSTL / fwhm
         cloren2 = 4.0 / fwhm**2
!
         qh = hl / fwhm
         eta = 1.36603*qh - 0.47719*qh**2 + 0.11116*qh**3         !PV mixing parameter
         detaqh = 1.36603 - 2.0*0.47719*qh + 3.0*0.11116*qh**2    !der. eta risp. qh
         dqhfwhm = -hl/fwhm**2                                    !der. qh risp. fwhm
         detah  = detaqh*dqhfwhm                                  !der. eta risp. h
!
         derft = derFct(j) 
         secth2 = 1.0 / costh**2
!corr         uvw2 = fwhm * ref(j)%pk * rapIcalc(j)
         uvw2 = fwhm * ref(j)%pk * ref(j)%rapI
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         if (refine_cell) then
            dlorp = derivataLP(ref(j)%tthd(kal),kpol)
            derhgt = secth2*(fV + 2.0*(fU+fZ)*tanth)*0.5/hg         !der. hg risp. theta
            derhlt = secth2*(fX + fY*senth)                         !der. hl risp. theta
            vetder(3) = dfwhmg*derhgt + dfwhml*derhlt               !der. FWHM risp. theta
            vetder(2) = detaqh*(derhlt/fwhm+dqhfwhm*vetder(3))      !der. eta risp. theta
            dercells = scal*derteta_risp_celldir(spg,cell,ref(j)%hkl,ref(j)%tthd(kal),wave) !(der. di theta risp. alla cella) * scala
         endif
         pesolp = ref(j)%lp(kal)
         absoc = ref(j)%ab(kal)
         ffnop = pesolp*ref(j)%m*(ref(j)%fc**2)*absoc
         !!!!op = oprcorr(j,:3)
         !!!!opT = sum(frac*op)
         !!!!opT = op(1)
         ffx = ffnop*ref(j)%po
         ffb1 = ratio(kal)*pesolp*ref(j)%m*ref(j)%po*absoc
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                                ref(j)%fwhm(kal),tch%par(T_ASYM1PAR:T_ASYM4PAR)%val,der=der_asym)
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            cc=(1.0+cloren2*deltath2)
            plor= cloren1/cc
            pgau= cgaus2*exp(-cgaus1*deltath2)
            pvoigt = eta*plor + (1.0-eta)*pgau
            !asterm=1.0-(asym*sign(1.0,deltath)*deltath2)/tanth
            asterm = asymval(nnn-iniz+1)
!
!           Derivative of 2theta corrected
            !deraszero = 4.0*asym*sign(1.0,deltath)*deltath/tanth                       ! der. asym. rispetto zero
            !derfunczero = 4.0*deltath*(cloren2*plor*eta/cc + cgaus1*(1.0-eta)*pgau)    ! der. pvoigt rispetto zero
            !deraszero = -2.0*asym*sign(1.0,deltath)*deltath/tanth                       ! der. asym. rispetto 2theta corrected
            deraszero = der_asym(nnn-iniz+1,2)                 ! der. asym. rispetto 2theta corrected
            derfunczero = -2.0*deltath*(cloren2*plor*eta/cc + cgaus1*(1.0-eta)*pgau)    ! der. pvoigt rispetto 2theta corrected
            deryz(nnn) = deryz(nnn) + scal * ffx * (derfunczero*asterm + deraszero*pvoigt)
!
!           Derivata Pseudo-Voigt rispetto eta
!
            gradprof(2) = plor - pgau
!
!           Derivata Pseudo-Voigt rispetto FWHM
            gradprof(3) = ( eta*plor*(8.0*deltath2-cc*fwhm**2)/cc + (eta-1)*pgau*(fwhm**2-2.0*CONSTG1*deltath2) ) / fwhm**3
!
            if (refine_profile) then
                deruvw = (gradprof(2)*detah+gradprof(3))                       !der. TCH rispetto FWHM
                deruvw = asterm*deruvw + der_asym(nnn-iniz+1,3)*pvoigt         !der. (TCH*asterm) rispect FWHM
!
                dconst = scal*ffx  !*asterm
                derfg  = dconst*deruvw*dfwhmg/(2.0*hg)
                dfw(nnn,1)=dfw(nnn,1) + derfg * tanth2                         !der. rispetto U
                dfw(nnn,2)=dfw(nnn,2) + derfg * tanth                          !der. rispetto V
                dfw(nnn,3)=dfw(nnn,3) + derfg                                  !der. rispetto W
                dfw(nnn,4)=dfw(nnn,4) + derfg / costh**2                       !der. rispetto Z
!
                derfl = dconst*(deruvw*dfwhml+gradprof(2)*detaqh/fwhm)
                dfw(nnn,5)=dfw(nnn,5) + derfl * tanth                          !der. rispetto X
                dfw(nnn,6)=dfw(nnn,6) + derfl / costh                          !der. rispetto Y
!
                !derp(nnn,1)=derp(nnn,1) - scal*ffx*pvoigt*sign(1.0,deltath)*deltath2/tanth !der. rispetto asym
                derp(nnn,1)=derp(nnn,1) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,4) !der. rispetto asym
                derp(nnn,2)=derp(nnn,2) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,5) !der. rispetto asym
                derp(nnn,3)=derp(nnn,3) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,6) !der. rispetto asym
                derp(nnn,4)=derp(nnn,4) + scal*ffx*pvoigt*der_asym(nnn-iniz+1,7) !der. rispetto asym
            endif
            if (refine_cell) then
               !derasth=asym*sign(1.0,deltath)*deltath*(rtod*4.0/tanth + deltath/(sin(rdeg)**2)) !der.asym risp. rdeg
!
!              d(asym(theta,H(theta)) = d(asym(theta))/d(theta) + d(asym(H))/d(H) * d(H)/d(theta)
               derasth=der_asym(nnn-iniz+1,1) + der_asym(nnn-iniz+1,3)*vetder(3)   !der.asym risp. theta
!
!              Derivata Psvoig rispetto theta in radianti del riflesso
               gradprof(1) = rtod*4.0*deltath*(eta*cloren2*plor/cc + (1-eta)*cgaus1*pgau)
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
   end subroutine derpsvoi_tchz

!--------------------------------------------------------------------

   subroutine dertchza(ref,tch,inizr,ifinr,xcount,der,derc,nwave,ratio,scal,nph)
   USE progtype, only: deratom
   USE reflection_type_util, only: reflection_type
   USE arrayutil
   USE profile_function
   USE asymfunc
!
   type(reflection_type), dimension(:), intent(in) :: ref
   type(profile_function_t), intent(in)             :: tch
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
   real                                            :: rdeg,tdeg
   real                                            :: eta,uvw2
!corr   real                                            :: asym
   real                                            :: deltath,deltath2
   real                                            :: pvoigt   !,asterm
   real                                            :: ffb1,dydI
   real                                            :: cgaus1,cgaus2,cloren1,cloren2
   real                                            :: fwhm
   integer                                         :: nat
!
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
!
   derc = deratom(0.0,0.0,0.0)
   nat = size(derc,2)
!
!corr   asym = tch%par(T_ASYM1PAR)%val
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=inizr,ifinr
         tthdeg = ref(j)%tthd(kal)
         rdeg = 0.5 * tthdeg * dtor
         tdeg = sin(rdeg) / cos(rdeg)
!
         fwhm = ref(j)%fwhm(kal)
         cgaus1 =   CONSTG1/fwhm**2
         cgaus2 =   constg2/fwhm
         cloren1 = CONSTL / fwhm
         cloren2 = 4.0 / fwhm**2
!
         eta = pdph(nph)%pvet(j,kal,ETAPAR)
         uvw2 = fwhm * ref(j)%pk * ref(j)%rapI
!
         iniz = clocate(xcount,tthdeg-uvw2)
         ifin = clocate(xcount,tthdeg+uvw2)
!
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                                ref(j)%fwhm(kal),tch%par(T_ASYM1PAR:T_ASYM4PAR)%val)
!
         !!!!opT = sum(frac*oprcorr(j,:))
         !!!!opT = oprcorr(j,1)
         ffb1 = ratio(kal)*ref(j)%lp(kal)*ref(j)%m*ref(j)%po*ref(j)%ab(kal)
         LOOP_CONTEGGI : do nnn=iniz,ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            pvoigt = eta*cloren1/(1.0+cloren2*deltath2) + (1.0-eta)*cgaus2*exp(-cgaus1*deltath2)
!corr            asterm=1.0-(asym*sign(1.0,deltath)*deltath2)/tdeg
!
!           calcolo derivate param. strutt. per atomo
!
!corr            dydI = scal * ffb1 * pvoigt * asterm
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
   end subroutine dertchza

  !------------------------------------------------------------------------------------------

   subroutine foleb_tchz(ref,inizr,ifinr,Icalc,Iobs,tch,scal,yoss,ytot,ttetaczero,back,nph)
   USE arrayutil
   USE profile_function
   USE reflection_type_util, only: reflection_type
   USE asymfunc
!
   type(reflection_type), dimension(:), intent(in) :: ref
   integer, intent(in)                             :: inizr,ifinr
   real, dimension(:), intent(in)                  :: Icalc
   real, dimension(:), intent(out)                 :: Iobs
   type(profile_function_t), intent(in)            :: tch
   real, intent(in)                                :: scal
   real, dimension(:), intent(in)                  :: yoss,ytot,ttetaczero,back
   integer, intent(in)                             :: nph
   integer                                         :: iniz,ifin
   real                                            :: eta  !,asym
   integer                                         :: j,nnn,kal
   real                                            :: tthdeg,deltath,deltath2
   real                                            :: uvw2
   real                                            :: yci,diffo,ycalc   !,asterm
   real                                            :: pvoigt
   real                                            :: sumy
   real                                            :: cgaus1,cgaus2
   real                                            :: cloren1,cloren2
   real                                            :: fwhm
   real                                            :: rdeg,tanth
   real                                            :: epsmch = epsilon(1.0)
!
   kal = 1
   LOOP_RIFLESSI : do j=inizr,ifinr
      sumy = 0.0
      tthdeg = ref(j)%tthd(kal)
      rdeg = 0.5 * tthdeg * dtor
      tanth = sin(rdeg) / cos(rdeg)
!
      eta = pdph(nph)%pvet(j,kal,ETAPAR)
      uvw2 = ref(j)%fwhm(kal) * ref(j)%pk * ref(j)%rapI
!
      fwhm = ref(j)%fwhm(kal)
      cgaus1 =   CONSTG1/fwhm**2
      cgaus2 =   CONSTG2/fwhm
      cloren1 = CONSTL / fwhm
      cloren2 = 4.0 / fwhm**2
!
      iniz = clocate(ttetaczero,tthdeg-uvw2)
      ifin = min(clocate(ttetaczero,tthdeg+uvw2),size(ytot))
!
      call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,ttetaczero(iniz:ifin)-tthdeg,tthdeg,   &
                             ref(j)%fwhm(kal),tch%par(T_ASYM1PAR:T_ASYM4PAR)%val)
!
      LOOP_CONTEGGI : do nnn=iniz,ifin
         deltath=ttetaczero(nnn)-tthdeg
         deltath2=deltath**2
         pvoigt = eta*cloren1/(1.0+cloren2*deltath2) + (1.0-eta)*cgaus2*exp(-cgaus1*deltath2)
!corr         asterm=1.0-(asym*sign(1.0,deltath)*deltath2)/tanth
!corr         yci = scal * pvoigt * asterm
         yci = scal * pvoigt * asymval(nnn-iniz+1)
         if (ytot(nnn) > epsmch) then
            diffo = yoss(nnn)-back(nnn)
            if (diffo.lt.0.0) then
                !diffo = 0.0
                cycle
            endif
            ycalc = diffo*yci/ytot(nnn)
         else
            !ycalc = 0.0
            cycle
         endif
         sumy = sumy + ycalc
      enddo LOOP_CONTEGGI
!
      Iobs(j) = Icalc(j) * sumy
!!!!      if (Iobs(j) < 0.0) Iobs(j) = 0.0
      if (Iobs(j) < epsmch) Iobs(j) = 0.0
!
   enddo LOOP_RIFLESSI
!
   end subroutine foleb_tchz

 !------------------------------------------------------------------------------------------

   subroutine profile_curve_tchz(tch,ref,jref,fctype,xcount,yb,scal,nwave,ratio,nph,xp,yp,ier)
   USE reflection_type_util, only:reflection_type
   USE arrayutil
   USE profile_function
   USE asymfunc
!
   type(profile_function_t), intent(in)         :: tch
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
   real                                         :: eta
   real                                         :: fwhm,uvw2
   integer                                      :: kal,nnn
   integer                                      :: iniz,ifin
   real                                         :: tthdeg
   real                                         :: cgaus1,cgaus2,cloren1,cloren2
   real                                         :: kffx
   real                                         :: deltath,deltath2
   real                                         :: pvoigt
   integer                                      :: np,ip
   real                                         :: fval
!
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
!
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
!
     fwhm = ref%fwhm(kal)
     cgaus1 =   CONSTG1/fwhm**2
     cgaus2 =   CONSTG2/fwhm
     cloren1 = CONSTL / fwhm
     cloren2 = 4.0 / fwhm**2
!
     iniz = clocate(xcount,tthdeg-uvw2)
     ifin = clocate(xcount,tthdeg+uvw2)
!
     fval = ref%fc
     kffx = fval**2*ref%m*ref%po*ref%lp(kal)*ref%ab(kal)*ratio(kal)
!
     call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(iniz:ifin)-tthdeg,tthdeg,   &
                            ref%fwhm(kal),tch%par(T_ASYM1PAR:T_ASYM4PAR)%val)
!
     np = ifin - iniz + 1        
     allocate(xp(np), yp(np))
     LOOP_CONTEGGI : do nnn=iniz,ifin
        deltath=xcount(nnn)-tthdeg
        deltath2=deltath**2
        pvoigt = eta*cloren1/(1.0+cloren2*deltath2) + (1.0-eta)*cgaus2*exp(-cgaus1*deltath2)
        ip = nnn-iniz+1
        xp(ip) = xcount(nnn)
        yp(ip) = scal*kffx*pvoigt*asymval(ip) + yb(nnn)
     enddo LOOP_CONTEGGI
   enddo LOOP_KALPHA
!
   end subroutine profile_curve_tchz

 END MODULE tchzf
