!--------------------------------------------------------------
!--------------------------------------------------------------
!
!  SUBROUTINES SUI RESTRAINTS
!
!--------------------------------------------------------------
!--------------------------------------------------------------
 MODULE RRESTR

! F nrestraints(reslist, code)                               Restituisce il numero di restraints
! S resapp(reslist,numj,kcalcder,coderwi)                    Calcola jacobiano e contributo al chiquadro dei restraints
! S weigthrestr(reslist,coderw,numj,kcalcder)                Gestione pesi 
! S aresd(nrj,rest,kcalcder)                                 Jacobiano dei restraints su distanza
! S aresa(nrj,rest,kcalcder)                                 Jacobiano dei restraints su angoli
! S arespnew(nrj,rest,kcalcder)                              Jacobiano dei restraints su piani
! F plane_condition(at)                                      Controlla planarita degli atomi
! F mdistance_lsqplane(ati)                                  Calcola la distanza media degli atomi from the least sq. plane
! S reset_restraints(reslist)                                Azzera restraints
! S save_restraints(unitbin,res)                             Write restraints on binary file
! S read_restraints(unitbin,res,err)                         Read restraints from binary file
! S resize_restraints(vetr,n,savevet)                        Rialloca ad n un vettore di tipo restraint
! S new_restraints(vetr,n)                                   Create new restraints
! S delete_restraints(vetr,i)                                Delete all restraints
! S delete_restraints_code(resvr)                            Delete restraints with code = DELETED_RES
! S add_restraint_to_list(reslist,res)                       Aggiungi un restraint alla lista
! F restraint_position(reslist,res or na)                    Cerca la posizione di un restraint all'interno della lista
! S stampa_restraints(kpr,reslist)                           Stampa i restraints
! S read_restraints_from_file(filename,reslist)              Legge restraints da file esterno
! S write_restraints_on_file(filename,reslist,atom)          Scrive i restraints su un file esterno
! S get_restraint_from_string(string,atom,res,ier)           Legge il restraint dalla stringa del tipo:
! S read_restraints_from_directives(sdir,...)                Read restraints from array of directives
! S set_value_for_restraint(res,atom)                        Calcola valore corrente per il restraint
! S set_equivalent_for_angle(resv,legm)                      For angle n1-n2-n3 assign negative value to atom for which is not necessary compute equivalent
! S set_weight(res,code,wei)                                 Assign weight to all restraints of type code
! F conn_to_restraints(iconn,atom,code,reslist)              Converte tutte le distanze e/o angoli in restraints
! S update_restraints(reslist,rmvet)                         Aggiorna la lista dei restraints se ho eliminato degli atomi
! S push_back_restr(reslist,code,atr,targ,sig)               Definisce il restraint e lo aggiunge alla lista 
! S add_constraint(code,vpar,vat,atom,coefi)                 Aggiunge un nuovo constraint alla lista
! F constr_aveb(constr,atom,bmin,bmax)  result(bmed)         Compute an average b for atom in constr
! S add_constraint_on_biso()                                 Imponi che atomi della stessa specia abbiano lo stesso fattore termico
! F par_in_constraint(kat,npar) result(kconstr)              Controlla se un atomo e' implicato in un constraint
! F coef_of_constraint(kconstr,nat,npar) result(coef)        Prende il coefficente del parametro npar del constraint
! S refine_constraint(constr,atom,code)                      Chiedo di affinare (code=1) o non affinare (code=0) un constraint
! S reallocate_constr(vetr,n,savevet)                        Rialloca ad n un vettore di tipo restraint
! S calc_chi2res(reslist,npf)                                Chi2 dei restraints
! S ordina_res(reslist)                                      Ordina i restraints per classe
! S set_constr_on_position()                                 Cerca atomi in posizione speciale generando nconstr constraints
! S print_lsqcond()                                          Stampa informazioni sulle posizioni speciali dai constraint
! F chi2res_value(rest)                                      Compute contribution of single restraint

 USE type_constants, only:DP
 USE atom_basic, only: atom_type

 implicit none

 enum, bind(c)
   enumerator :: RESDIST=1, RESANGLE, RESPLANE, ABUMP, RESBV
 end enum
 integer, parameter :: DELETED_RES = -9999
 integer, parameter :: TARG_BOND_TAB=1,TARG_CURRENT=2,TARG_MOL_MEC=3

 type restraint_type
   integer                            :: code = 0    ! indica il tipo di restraint: 1=distanza,2=angolo,3=piano,4=anti-bump
   integer, dimension(:), allocatable :: na          ! puntatori agli atomi coinvolti nel restraint
   real                               :: val = 0.0   ! valore corrente del restraint
   real                               :: targ = 0.0  ! valore target del restraint
   real                               :: sigma = 0.0 ! deviazione standard
   real                               :: wei = 1.0   ! weight
   integer, dimension(3)              :: sym = 0     ! if /= 0 consider symmetry equivalent atoms -- new: 0 all atoms, 1 = intra, 2 = inter
   integer                            :: active = 0
   real :: contrib = 0
   real :: contrib_copy = 0
 end type 

 integer, dimension(3) :: ktargettype      ! tipo di target: 1 da table, 2 uguale al target 
 integer               :: autowei = 0      ! pesi in automatico se vale 1
 real, parameter       :: WRES_DEF = 1000  ! default value for weight
 
private :: nrestraints_all, nrestraints_code
interface nrestraints
  module procedure nrestraints_all, nrestraints_code
end interface

private :: delete_restraints_all, delete_restraints_pos
interface delete_restraints
  module procedure delete_restraints_all, delete_restraints_pos
end interface 

private :: restraint_position_res, restraint_position_at
interface restraint_position
  module procedure restraint_position_res, restraint_position_at
end interface 

 CONTAINS


   pure integer function nrestraints_all(reslist)
!
!  Restituisce il numero di restraints
!
   type(restraint_type), dimension(:), allocatable, intent(in) :: reslist
!   
   if(allocated(reslist)) then 
      nrestraints_all = size(reslist)
   else
      nrestraints_all = 0
   endif
!
   end function nrestraints_all

 !--------------------------------------------------------------------------

   integer function nrestraints_code(reslist,code)
   type(restraint_type), dimension(:), allocatable, intent(in) :: reslist
   integer, intent(in)                                         :: code
!
   if (allocated(reslist)) then
       nrestraints_code = count(reslist%code == code)
   else
       nrestraints_code = 0
   endif
!
   end function nrestraints_code 

 !--------------------------------------------------------------------------

   subroutine resapp(atom,cell,reslist,wres,numj,kcalcder,coderwi)
!
!  Calcola jacobiano e contributo al chiquadro dei restraints
!
   USE unit_cell
   USE type_constants, only:DP
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(cell_type), intent(in)                                    :: cell
   type(restraint_type), dimension(:), allocatable, intent(inout) :: reslist
   real(DP), dimension(3), intent(inout)                          :: wres
   !real, dimension(3), intent(inout)                          :: wres

   integer, intent(in)                                            :: numj     ! puntatore allo jacobiano
   integer, intent(in)                                            :: kcalcder ! = 0/1 -> non calcolo / calcolo anche le derivate dei restraints
   integer, intent(in), optional                                  :: coderwi  ! codici per differente gestione dei pesi
   integer                                                        :: coderw
   integer                                                        :: i
   integer                                                        :: numres
!
   numres = nrestraints(reslist)  !!!!size(reslist)
   if(numres==0) return
!
   do i=1,numres

      select case(reslist(i)%code)

         case(1)                                !distances
          call aresd(atom,i+numj,reslist(i),kcalcder,cell%get_g())
          
         case(2)                                !angles
          call aresa(atom,i+numj,reslist(i),kcalcder,cell%get_g())

         case(3)                                !planes
          call arespnew(atom,i+numj,reslist(i),kcalcder)

      end select
   enddo
!   
!  Integra funcls e jacob con il contributo dei pesi
   if (present(coderwi)) then
       coderw = coderwi
   else
       coderw = 0
   endif
   call weigthrestr(reslist,wres,coderw,numj,kcalcder)
!
   end subroutine resapp

!-----------------------------------------------------------------------

   subroutine weigthrestr(reslist,wres,coderw,numj,kcalcder)
!
!  Questa subroutine richiede che i restraints siano ordinati per classi (usa ordina_res)
!
   USE genlsq, only:funcls,jacob
   type(restraint_type), dimension(:), intent(in) :: reslist
   real(DP), dimension(3), intent(inout)          :: wres
   integer, intent(in)                            :: coderw
   integer, intent(in)                            :: numj
   integer, intent(in)                            :: kcalcder
   integer                                        :: posi,posf
   real(DP), dimension(3), save                   :: weires
   integer                                        :: i
   integer, dimension(3)                          :: nrr
   logical                                        :: lpr = .false.
   real(DP)                                       :: chi2f
   real(DP), dimension(3)                         :: chi2r
!
!  Calcola num. di restraints per classe
   do i=1,3
      nrr(i) = count(reslist(:)%code==i)
   enddo
!  
!  Calcola in weires la scala tra restraints e funzioni non restraints
   if (coderw >  0) then
       call calc_chi2res(reslist,numj,chi2f,chi2r)
       do i=1,3
          if (nrr(i) == 0) then
              weires(i) = 0.0_dp
          else
              if (chi2r(i) <= epsilon(1.0_dp)) then
                  weires(i) = 1.0_dp
              else
                  weires(i) = chi2f / chi2r(i)
              endif
              if(lpr) write(6,'(a,i5,g20.6)')'weilog=',int(log10(weires(i))),weires(i),chi2f,chi2r(i)
          endif
       enddo
   endif
!
   select case (coderw)
       case (0)
!
!        usa il peso fornito esternamente 
         weires = wres

       case (1)
!
!        wres diventa il fattore di scala 
!        Viene aggiornato per un'eventuale stampa
         wres = weires

       case (2)
!
!        wres diventa un peso dei restraints dopo la scala
!        Non viene aggiornato perchè va mantenuto il valore fornito esternamente
         weires = wres*weires
   end select
   if (lpr) then
       write(6,*)'WEIRES=',weires
       write(6,*)'WQQ   =',wres
       write(6,*)'CHI2  =',chi2f,chi2r(1:3)
       write(6,*)'CHI2*W=',chi2f,weires(1:3)*chi2r(1:3)
   endif
!
!  Applica il peso calcolato ai restraints
   posf = numj
   do i=1,3
      if (nrr(i) == 0) cycle
      posi = posf + 1
      posf = posf + nrr(i)
      funcls(posi:posf) = sqrt(weires(i))*funcls(posi:posf)
      if(kcalcder == 1) jacob(posi:posf,:) = sqrt(weires(i))*jacob(posi:posf,:)
   enddo
!
   end subroutine weigthrestr

!-----------------------------------------------------------------------

   subroutine aresd(atom,nrj,rest,kcalcder,gmat)
!
!  jacobiano dei restraints su distanza
!
   USE GENLSQ,       only:jacob,funcls,weilsq
   USE CGEOM,        only:distanzaC
!
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   integer, intent(in)       :: nrj         !puntatore del restraints nello jacobiano
   type(restraint_type), intent(inout) :: rest
   integer, intent(in)       :: kcalcder    !se =1 calcolo il contributo allo jacobiano
   real, dimension(3,3), intent(in) :: gmat
   type(atom_type), dimension(2) :: ra          !gli atomi coinvolti nel restraints
   real                      :: diste,sigma !distanza attesa e sigma
   integer                   :: i
   real                      :: coef,dista,dd
   real,  dimension(2,3)     :: de
   integer                   :: kpar
!
   ra(1) = atom(rest%na(1))
   ra(2) = atom(rest%na(2))
   diste = rest%targ
   sigma = rest%sigma
   coef = 1.0 / sigma**2
!
!  Calcola distanza tra gli atomi
   dista  = distanzaC(ra(1)%xc,ra(2)%xc,gmat)
   rest%val = dista
   dd = diste - dista
!
   weilsq(nrj) = sqrt(coef)
   funcls(nrj) = weilsq(nrj)*dd
!
!  calcolo derivate della distanza rispetto ad x,y,z
!  de(1,:) = derivate della distanza ristetto alle coord x1,y1,z1 di at1
!  de(2,:) = derivate della distanza ristetto alle coord x2,y2,z2 di at2
   if (kcalcder == 1) then
       de(1,:) = MATMUL (gmat,ra(1)%xc - ra(2)%xc) / dista
       de(2,:) = -de(1,:)
!
       jacob(nrj,:) = 0.0  !azzero su tutti i parametri
       do i=1,2
          do kpar=1,3
             if (ra(i)%rcod(kpar) > 0) then
                 !jacob(nrj,ra(i)%rcod(kpar)) = -sqrt(wqq(1)*coef) * de(i,kpar)
                 jacob(nrj,ra(i)%rcod(kpar)) = -weilsq(nrj) * de(i,kpar)
             endif
          enddo
       enddo
   endif
!
   end subroutine aresd
   
!-----------------------------------------------------------------------

   subroutine aresa(atom,nrj,rest,kcalcder,gmat)
!
!  Jacobiano dei restraints su angoli
!
   USE GENLSQ,       only:jacob,funcls,weilsq
   USE CGEOM,        only:distanzaC
   USE trig_constants
!
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(atom_type), dimension(3) :: ra           !info sui parametri coinvolti nel restraints
   real                      :: angE,sigma   !distanza attesa e sigma
   integer, intent(in)       :: nrj          !puntatore del restraints nello jacobiano
   type(restraint_type), intent(inout) :: rest
   integer, intent(in)       :: kcalcder     !se=1 calcolo il contributo allo jacobiano
   real, dimension(3,3), intent(in) :: gmat
   real,     dimension(3)    :: XGX1
   real, dimension(3,3)      :: de
   integer                   :: i
   real                      :: coef
   real                      :: dis1,dis2,dis11,dis22,dis12,dd,XGX
   real                      :: costhA,angA,costhE
   integer                   :: kpar
!
   ra(1) = atom(rest%na(1))
   ra(2) = atom(rest%na(2))
   ra(3) = atom(rest%na(3))
   angE = rest%targ
   sigma = rest%sigma
   coef = 1.0 / sigma**2
   dis1 = distanzaC(ra(1)%xc,ra(2)%xc,gmat)
   dis2 = distanzaC(ra(2)%xc,ra(3)%xc,gmat)
   dis11 = dis1 * dis1
   dis22 = dis2 * dis2
   dis12 = dis1 * dis2
   XGX = DOT_PRODUCT(ra(1)%xc-ra(2)%xc,MATMUL(gmat,ra(3)%xc-ra(2)%xc))
   costhA = XGX / dis12
   angA = acos ( costhA ) * rtod
   rest%val = angA
   costhE = cos ( angE*dtor )
!
   dd = costhE - costhA
   weilsq(nrj) = sqrt(coef)
   funcls(nrj) = weilsq(nrj)*dd
   if (kcalcder == 1) then
!
!      derivate del coseno dell'angolo rispetto ad x,y,z di n1
       de(1,1:3) = MATMUL(gmat,((ra(3)%xc-ra(2)%xc) - XGX * (ra(1)%xc-ra(2)%xc)/dis11)/dis12)
!
!      derivate del coseno dell'angolo rispetto ad x,y,z di n3
       de(3,1:3) = MATMUL(gmat,((ra(1)%xc-ra(2)%xc) - XGX * (ra(3)%xc-ra(2)%xc)/dis22)/dis12)
!
!      derivate del coseno dell'angolo rispetto ad x,y,z di n2
       XGX1 = 2*ra(2)%xc - ra(3)%xc - ra(1)%xc + XGX * ((ra(1)%xc-ra(2)%xc)/dis11 + (ra(3)%xc-ra(2)%xc)/dis22)
       de(2,1:3) = MATMUL(gmat,XGX1) / dis12
!
       jacob(nrj,:) = 0.0
       do i=1,3
          do kpar=1,3
             if (ra(i)%rcod(kpar) > 0) then
                 !jacob(nrj,ra(i)%rcod(kpar)) = -sqrt(wqq(2)*coef) * de(i,kpar)
                 jacob(nrj,ra(i)%rcod(kpar)) = -weilsq(nrj) * de(i,kpar)
             endif
          enddo
       enddo
   endif
!
   end subroutine aresa

!-----------------------------------------------------------------------

   subroutine arespnew(atom,nrj,rest,kcalcder)
!
!  Jacobiano dei restraints su piani
!
   USE GENLSQ, only:jacob,funcls,weilsq
!
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   real                                                   :: sigma         !sigma
   integer, intent(in)                                    :: nrj           !puntatore del restraints nello jacobiano
   type(restraint_type), intent(in)                       :: rest
   integer, intent(in)                                    :: kcalcder      !se=1 calcolo il contributo allo jacobiano
   type(atom_type), dimension(size(rest%na))              :: rat           !gli atomi coinvolti nel restraint
   integer                                                :: i,k,l,m
   real(DP)                                               :: detV,dd,coef
   real, dimension(3)                                     :: xc
   real(DP), dimension(3)                                 :: ddetV
   real,     dimension(3)                                 :: dbard
   real(DP), dimension(3,3)                               :: s,s2
   integer                                                :: kpar
   integer                                                :: numat
!
!  calcolo piano degli atomi
   sigma = rest%sigma
   numat = size(rest%na)
    do i=1,numat
       rat(i) = atom(rest%na(i))
    enddo
!
!  trova il baricentro degli atomi da planarizzare
   do i=1,3
      dbard(i) = sum(rat%xc(i)) / numat
   enddo
!
!  calcolo elementi della matrice V 
   s = 0.0
   do i = 1 , numat
      xc = rat(i)%xc - dbard   ! shift rispetto al baricentro
      do k = 1 , 3
         do l = 1 , 3
            s(k,l) =s(k,l)+xc(l)*xc(k)
         enddo
      enddo
   enddo
!
!  calcolo determinante della matrice V
   s2(1,3) = s(1,3)**2
   s2(1,2) = s(1,2)**2
   s2(2,3) = s(2,3)**2
   detV = -s2(1,3)*s(2,2)+2*s(1,2)*s(1,3)*s(2,3)-s(1,1)*s2(2,3)      &
          -s2(1,2)*s(3,3)+s(1,1)*s(2,2)*s(3,3)
!
   dd = detV
   coef = 1.0 / sigma**2
   weilsq(nrj) = sqrt(coef)
   funcls(nrj) = weilsq(nrj)*dd
   if (kcalcder == 1) then
!
       jacob(nrj,:) = 0.0
       do m=1,numat
!
!         calcolo derivate di V
!         ddetV(1) = derivata di detV rispetto a x
!         ddetV(2) = derivata di detV rispetto a y
!         ddetV(3) = derivata di detV rispetto a z
          xc = rat(m)%xc
          ddetV(1) = 2.0*xc(1)*(s(2,2)*s(3,3) - s2(2,3))         &
                   + 2.0*xc(2)*(s(1,3)*s(2,3) - s(1,2)*s(3,3))   &
                   + 2.0*xc(3)*(s(1,2)*s(2,3) - s(1,3)*s(2,2))

          ddetV(2) = 2.0*xc(2)*(s(1,1)*s(3,3) - s2(1,3))         &
                   + 2.0*xc(1)*(s(1,3)*s(2,3) - s(1,2)*s(3,3))   &
                   + 2.0*xc(3)*(s(1,2)*s(1,3) - s(1,1)*s(2,3))

          ddetV(3) = 2.0*xc(3)*(s(1,1)*s(2,2) - s2(1,2))         &
                   + 2.0*xc(1)*(s(1,2)*s(2,3) - s(1,3)*s(2,2))   &
                   + 2.0*xc(2)*(s(1,2)*s(1,3) - s(1,1)*s(2,3))
!
          do kpar=1,3
             if (rat(m)%rcod(kpar) > 0) then
                 jacob(nrj,rat(m)%rcod(kpar)) = weilsq(nrj) * ddetV(kpar)
             endif
          enddo
       enddo
   endif
!
   end subroutine arespnew 
   
!--------------------------------------------------------------------------

   real function plane_condition(at)   result(detV)
!
!  Calcola a partire da atomi in coord. cartesiane detV.
!  detV = 0 se gli atomi sono su un piano 
!   
   type(atom_type), dimension(:), intent(in) :: at      !atomi in coordinate cartesiane  
   integer                               :: numat
   integer                               :: i,k,l
   real, dimension(3)                    :: dbard
   real, dimension(3,3)                  :: s,s2
   real, dimension(3)                    :: xc
!   
!  trova il baricentro degli atomi da planarizzare
   numat = size(at)
   do i=1,3
      dbard(i) = sum(at%xc(i)) / numat
   enddo
!
!  calcolo elementi della matrice V 
   s = 0.0
   do i = 1 , numat
      xc = at(i)%xc - dbard   ! shift rispetto al baricentro
      do k = 1 , 3
         do l = 1 , 3
            s(k,l) =s(k,l)+xc(l)*xc(k)
         enddo
      enddo
   enddo
!
!  calcolo determinante della matrice V
   s2(1,3) = s(1,3)**2
   s2(1,2) = s(1,2)**2
   s2(2,3) = s(2,3)**2   
   detV = -s2(1,3)*s(2,2)+2*s(1,2)*s(1,3)*s(2,3)-s(1,1)*s2(2,3)      &
          -s2(1,2)*s(3,3)+s(1,1)*s(2,2)*s(3,3)
!   
   end function plane_condition
      
!--------------------------------------------------------------------------

   real function mdistance_lsqplane(ati,cell)  result(distam)
!
!  Calcola la distanza media degli atomi at dal piano dei minimi quadrati
!
   USE CGEOM, only:lsqplane
   USE unit_cell
   type(atom_type), dimension(:), intent(in) :: ati
   type(cell_type), intent(in)               :: cell
   real, dimension(3,size(ati))   :: cat
   real, dimension(4)             :: pvet
   integer                        :: nap
   integer                        :: i
!
   nap = size(ati)
!
!  Converti in coordinate cartesiane
   do i=1,nap
      cat(:,i) = MATMUL(cell%get_ortom(),ati(i)%xc)
   enddo
!
!  Calcola normale al piano lsq
   call lsqplane(cat,pvet)
!
!  Calcolo distanza media dal piano
   distam = 0.0
   do i=1,nap
      distam = distam + abs(SUM(pvet(:3)*cat(:,i)))
   enddo
   distam = distam/nap
!
   end function mdistance_lsqplane


!--------------------------------------------------------------------------

   subroutine reset_restraints(reslist,wres)
!
!  Azzera restraints
!
   type(restraint_type), dimension(:), allocatable, intent(inout) :: reslist
   real(DP), dimension(3), intent(out)                            :: wres
   autowei = 0
   !wres = 10000
   wres = WRES_DEF
!
!  Cancella restraints già esistenti
   if (allocated(reslist)) deallocate(reslist)
!
   end subroutine reset_restraints

!--------------------------------------------------------------------------

   subroutine add_restraint_to_list(reslist,res)
!
!  Aggiungi un restraint alla lista 
!
   type(restraint_type), dimension(:), allocatable, intent(inout) :: reslist
   type(restraint_type), intent(in)                               :: res                                           
   integer                                                        :: n
   integer                                                        :: pos
   integer                                                        :: code
   integer                                                        :: i
!
   n = nrestraints(reslist)
!
!  Calcola la posizione in cui inserire il restrain
   pos = 1
   if (n /= 0) then
       code = res%code
       do i=1,code
          pos = pos + count(reslist(:)%code == i)
       enddo
   endif
!
   call resize_restraints(reslist,n+1) 
   reslist(pos+1:) = reslist(pos:n)   ! libera la posizione
   reslist(pos) = res
!
   end subroutine add_restraint_to_list

!--------------------------------------------------------------------------
!corr
!corr   subroutine remove_restraint_from_list(reslist,pos)
!corr!
!corr!  Rimuovi dalla lista il restraint in posizione pos 
!corr!
!corr   type(restraint_type), dimension(:), allocatable, intent(inout) :: reslist
!corr   integer, intent(in)                                            :: pos
!corr   integer                                                        :: n
!corr!
!corr   n = nrestraints(reslist)
!corr   reslist(pos:n-1) = reslist(pos+1:)
!corr   call resize_restraints(reslist,n-1)
!corr!
!corr   end subroutine remove_restraint_from_list
!corr
!--------------------------------------------------------------------------

   integer function restraint_position_res(reslist,res)  result(pos)
!
!  Find restraint res in the array
!
   USE nr
   USE arrayutil
   type(restraint_type), dimension(:), allocatable, intent(in) :: reslist
   type(restraint_type), intent(in)                            :: res
   integer                                                     :: i
   integer                                                     :: nrs
   integer, allocatable, dimension(:)                          :: iord1,iord2
   integer                                                     :: np1,np2
   integer, dimension(2)                                       :: nal,na
!
   pos = 0
   nrs = nrestraints(reslist)
   if (nrs == 0) return
!
   select case (res%code)
      case (ABUMP)
         do i=1,nrs
           if (res%code /= reslist(i)%code) cycle
           !if (reslist(i)%na(1) * res%na(1) > 0) then   ! verify same sign
               nal(1) = abs(reslist(i)%na(1))
               nal(2) = reslist(i)%na(2)
               na(1) = abs(res%na(1))
               na(2) = res%na(2)
               if ((nal(1) == na(1) .and. nal(2) == na(2)) .or.  &
                    (nal(1) == na(2) .and. nal(2) == na(1)))then
                   pos = i
                   exit
               endif
           !endif
        enddo

      case (RESDIST)
        do i=1,nrs
           if (res%code /= reslist(i)%code) cycle
           if ((reslist(i)%na(1) == res%na(1) .and. reslist(i)%na(2) == res%na(2)) .or.  &
                (reslist(i)%na(1) == res%na(2) .and. reslist(i)%na(2) == res%na(1)))then
               pos = i
               exit
           endif
        enddo

      case (RESANGLE)
        do i=1,nrs
           if (res%code /= reslist(i)%code) cycle
           if (reslist(i)%na(2) == res%na(2)) then
               if ((reslist(i)%na(1) == res%na(1) .and. reslist(i)%na(3) == res%na(3)) .or.  &
                    (reslist(i)%na(1) == res%na(3) .and. reslist(i)%na(3) == res%na(1)))then
                   pos = i
                   exit
               endif
            endif
        enddo

      case (RESPLANE)
        np1 = size(res%na)
        do i=1,nrs
           if (res%code /= reslist(i)%code) cycle
           np2 = size(reslist(i)%na)
           if (np1 == np2) then     ! se hanno lo stesso numero di atomi
               !call reallocate(iord1,np1)
               call resize_array(iord1,np1)
               call indexx(res%na,iord1)
               !call reallocate(iord2,np2)
               call resize_array(iord2,np2)
               call indexx(reslist(i)%na,iord2)
               if (all(reslist(i)%na(iord2) == res%na(iord1))) then  ! se hanno gli stessi atomi
                   pos = i
                   exit
               endif
           endif
        enddo

   end select
!
   end function restraint_position_res

!--------------------------------------------------------------------------

   integer function restraint_position_at(reslist,at)  result(pos)
!
!  Find restraint with atoms at in the array
!
   type(restraint_type), dimension(:), allocatable, intent(in) :: reslist
   integer, dimension(:), intent(in)                           :: at
!
   select case (size(at))
      case (2)
        pos = restraint_position_res(reslist,restraint_type(code=RESDIST,na=at))
      case (3)
        pos = restraint_position_res(reslist,restraint_type(code=RESANGLE,na=at))
      case (4:)
        pos = restraint_position_res(reslist,restraint_type(code=RESPLANE,na=at))
      case default
        pos = 0
   end  select
!
   end function restraint_position_at

!--------------------------------------------------------------------------

   subroutine print_restraints_all(kpr,restr,atom,cell,print_type)
   USE atom_type_util
   USE unit_cell
   integer, intent(in)                                         :: kpr
   type(restraint_type), allocatable, dimension(:), intent(in) :: restr
   type(atom_type), dimension(:), intent(in)                   :: atom
   type(cell_type), intent(in)                                 :: cell
   logical, intent(in)                                         :: print_type
!
   if (nrestraints(restr) > 0) then
       call print_restraints(kpr,restr,RESDIST,atom,cell,print_type)
       call print_restraints(kpr,restr,RESANGLE,atom,cell,print_type)
       call print_restraints(kpr,restr,RESPLANE,atom,cell,print_type)
       call print_restraints(kpr,restr,RESBV,atom,cell,print_type)
   endif
!
   end subroutine print_restraints_all

!--------------------------------------------------------------------------

   subroutine print_restraints(kpr,restr,code,atom,cell,print_type)
   USE atom_type_util
   USE strutil
   USE unit_cell
   integer, intent(in)                                         :: kpr
   type(restraint_type), allocatable, dimension(:), intent(in) :: restr
   integer, intent(in)                                         :: code
   type(atom_type), dimension(:), intent(in)                   :: atom
   type(cell_type), intent(in)                                 :: cell
   logical, intent(in)                                         :: print_type
   real                                                        :: diff,distadif,angledif,dista
   integer                                                     :: i
   character(len=200)                                          :: string
   integer                                                     :: n1,n2,n3,nrescode
   logical                                                     :: any_bump_neg
   character(len=6), dimension(0:2) :: typeres = ['All   ','Intra ','Inter ']
   character(len=3) :: adv
!
   if (nrestraints(restr) == 0) return
   nrescode = count(restr(:)%code==code)
   if (nrescode == 0) return
   adv = 'yes'
!   
!  Title
   select case (code)
      case (RESDIST) 
       distadif = 0
       if (print_type) adv = 'no'
       write(kpr,'(/a)')centra_str('Restraint(s) on distance',80)
       write(kpr,'(a)',advance=adv)'   Num.    Atoms              Observed  Expected       Weight'
       if (print_type) write(kpr,'(a)')'   Type'
      case (RESANGLE)
       angledif = 0
       write(kpr,'(/a)')centra_str('Restraint(s) on angle',80)
       write(kpr,'(a)')'   Num.    Atoms              Observed  Expected Angle            Weight'
      case (RESPLANE)
       write(kpr,'(/a)')centra_str('Restraint(s) on plane',80)
       write(kpr,'(a)')'   Num.  Distance      Weight              Atoms'
      case (ABUMP)
       any_bump_neg = .false.
       write(kpr,'(/a)')centra_str('Anti-bump restraint(s)',80)
       write(kpr,'(a)')'   Num.    Atoms               Observed       dmin        Weight'
      case (RESBV)
       write(kpr,'(/a)')centra_str('Bond valence restraint(s)',80)
       write(kpr,'(a)')'   Num.   Atom               Observed   Expected   Sigma     Weight'
   end select
!
   do i=1,nrestraints(restr)
      if (restr(i)%code == code) then
          select case (restr(i)%code)
            case (RESDIST) 
              n1 = restr(i)%na(1)
              n2 = restr(i)%na(2)
              diff = restr(i)%val - restr(i)%targ
              write(kpr,10,advance=adv) i,atom(n1)%lab,atom(n2)%lab,restr(i)%val,restr(i)%targ,restr(i)%wei
              10 format(2x,i3,')',2x,a8,2x,a8,2x,f8.3,2x,f8.3,2x,es15.3)
              if (print_type) write(kpr,'(3x,a)')typeres(restr(i)%sym(1))
              distadif = distadif + abs(diff)

            case (RESANGLE)
              n1 = restr(i)%na(1)
              n2 = restr(i)%na(2)
              n3 = restr(i)%na(3)
              diff = restr(i)%val - restr(i)%targ
              write(kpr,20) i,atom(n1)%lab,atom(n2)%lab,atom(n3)%lab,restr(i)%val,restr(i)%targ,restr(i)%wei  !!!!!diff,wqq(2)
              !20 format(2x,i3,')',1x,a8,1x,a8,1x,a8,1x,f8.3,2x,f8.3,2x,f8.3,2x,es15.3)
              20 format(2x,i3,')',1x,a8,1x,a8,1x,a8,1x,f8.3,2x,f8.3,2x,es15.3)
              angledif = angledif + abs(diff)

            case (RESPLANE)
              dista = mdistance_lsqplane(atom(restr(i)%na),cell)
              write(string,'(2x,i3,")",1x,f8.3,2x,f13.3)') i,dista,restr(i)%wei  !!!!!wqq(3)
              call write_svet(atom(restr(i)%na)%lab,size(restr(i)%na),kpr,string(:len_trim(string)+1))

            case (ABUMP)
              n1 = abs(restr(i)%na(1))
              n2 = restr(i)%na(2)
              if (restr(i)%val < restr(i)%targ) then
                  write(kpr,'(2x,i5,")",2x,a8,2x,a8,2x,f8.3,a,4x,f8.3,10x,g12.5)',advance='no')i,atom(n1)%lab,atom(n2)%lab,   &
                  restr(i)%val,'<',restr(i)%targ,restr(i)%wei
              else
                  write(kpr,'(2x,i5,")",2x,a8,2x,a8,2x,f8.3,5x,f8.3,10x,g12.5)',advance='no')i,atom(n1)%lab,atom(n2)%lab,   &
                  restr(i)%val,restr(i)%targ,restr(i)%wei
              endif
              if (restr(i)%na(1) < 0) then
                  any_bump_neg = .true.
                  write(kpr,'(a)')' (*)'
              else
                  write(kpr,*)
              endif

            case (RESBV)
              n1 = restr(i)%na(1)
              write(kpr,'(2x,i5,")",2x,a8,10x,f8.3,3(2x,f8.3))') i,atom(n1)%lab,restr(i)%val,restr(i)%targ, &
                    restr(i)%sigma,restr(i)%wei
          end select
      endif
   enddo
!
   select case (code)
      case (RESDIST) 
          write(kpr,'(2x,a,f0.4)')'Average of differences on distances = ',distadif/nrescode
      case (RESANGLE)
          write(kpr,'(2x,a,f0.4)')'Average of differences on angles = ',angledif/nrescode
      case (RESPLANE)
      case (ABUMP)
          if (any_bump_neg) write(kpr,'(2x,a)')'(*) Pair of atoms separated by chain shorter than 4 bonds'
   end select
!
   end subroutine print_restraints

!--------------------------------------------------------------------------

   subroutine read_restraints_from_file(atom,cell,filename,reslist,wres,err)
!
!  Legge restraints da file esterno
!
   USE prog_constants
   USE strutil
   USE fileutil
   USE unit_cell
   USE errormod
   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
   type(cell_type), intent(in)                                    :: cell
   character(len=*), intent(in)                                   :: filename
   type(restraint_type), dimension(:), allocatable, intent(inout) :: reslist
   real(DP), dimension(:), intent(inout)                          :: wres
   type(restraint_type)                                           :: res
!corr   integer                                                        :: nExitCode
   type(error_type), intent(out)                                  :: err
   character(len=500)                                             :: line,line1,word
   integer                                                        :: nlongl,nlongw
   real, dimension(100)                                           :: vet
   integer, dimension(100)                                        :: ivet
   integer                                                        :: iv,ier
   type(file_handle)                                              :: fres
!
   call fres%fopen(filename)
   if (fres%good()) then
       call reset_restraints(reslist,wres)
       do  
         read(fres%handle(),'(a)',iostat=ier)line
         if (ier < 0) exit
         call s_filter(line)
         line1 = line
         call Cutst(line1,nlongl,word,nlongw)
         nlongw = min(4,nlongw)     ! bastano 4 caratteri per individuare la direttiva
         word = upper(word)
         select case(word(:nlongw))

            case ('WEID')       ! peso sulle distanze
              call Getnum(line1(:nlongl),vet,ivet,iv)
              if (iv > 0) wres(1) = vet(1)

            case ('WEIA')       ! peso sugli angoli
              call Getnum(line1(:nlongl),vet,ivet,iv)
              if (iv > 0) wres(2) = vet(1)

            case ('WEIP')       ! peso sui piani
              call Getnum(line1(:nlongl),vet,ivet,iv)
              if (iv > 0) wres(3) = vet(1)

            case default        ! leggi restraints
              call get_restraint_from_string(line,atom,res,err)
              if (ier == 0) then
                  call set_value_for_restraint(res,atom,cell)    ! assegna valore corrente
                  call add_restraint_to_list(reslist,res)
              endif

         end select
       enddo
       call fres%fclose()
   else
       call err%set('Open error for file '//trim(filename))
       !call MsgWinErr('ERROR','Open error for file '//trim(filename),  &
       !ERR_WINDOW,nExitCode)
   endif
!
   end subroutine read_restraints_from_file

!--------------------------------------------------------------------------

   subroutine write_restraints_on_file(filename,reslist,atom,weir)
!
!  Scrive i restraints su un file esterno
!
   USE progtype
   USE strutil
   USE fileutil
   character(len=*), intent(in)                                :: filename
   type(restraint_type), dimension(:), allocatable, intent(in) :: reslist
   type(atom_type), dimension(:), intent(in)                   :: atom
   real, dimension(3), intent(in), optional                    :: weir
   integer                                                     :: i
   !character(len=100)                                          :: formt
   integer                                                     :: nap
   integer                                                     :: nrs
   type(file_handle) :: fres
!
   call fres%fopen(filename,'w')
!
   nrs = nrestraints(reslist)
   if (nrs > 0) then
       do i=1,nrs
         select case(reslist(i)%code)
             case (RESDIST)
               write(fres%handle(),'(1x,2(a,2x),2f12.3)')trim(atom(reslist(i)%na(1))%lab), &
                    trim(atom(reslist(i)%na(2))%lab),reslist(i)%targ,reslist(i)%sigma !!!, ' ;1'

             case (RESANGLE)
               write(fres%handle(),'(1x,3(a,2x),2f12.3)')trim(atom(reslist(i)%na(1))%lab), &
                    trim(atom(reslist(i)%na(2))%lab),trim(atom(reslist(i)%na(3))%lab),  &
                    reslist(i)%targ,reslist(i)%sigma  !!!!, ' ;2'

             case (RESPLANE)
               nap = size(reslist(i)%na)
               write(fres%handle(),'(1x,a,2x,f12.3)')cat_svet(atom(reslist(i)%na)%lab,nap,sep=' '),reslist(i)%sigma !!!!, ' ;3'

         end select
       enddo
       if (present(weir)) then 
          ! write(fres%handle(),'(a,3f20.6)')'weight  ',weir(:)
           if (any(reslist(:)%code==1))write(fres%handle(),'(a,f20.5)')'weid ',weir(1)
           if (any(reslist(:)%code==2))write(fres%handle(),'(a,f20.5)')'weia ',weir(2)
           if (any(reslist(:)%code==3))write(fres%handle(),'(a,f20.5)')'weip ',weir(3)
       endif
   else
       write(fres%handle(),*)   !svuota il file
   endif
!
   call fres%fclose()
!
   end subroutine write_restraints_on_file

!--------------------------------------------------------------------------

   subroutine get_restraint_from_string(string,atom,res,err,sigd,weid,inter)
!
!  Legge il restraint dalla stringa del tipo:
!    'atom1 atom2       target   esd  wei'   for distance  (target, esd, wei, optional)
!    'atom1 atom2 atom3 target   esd  wei'   for angle     (target, esd, wei, optional)
!    'atom1 atom2 atom3 atom4 .. esd  wei'   for plane   
!
   USE strutil
   USE atom_type_util
   USE errormod
   character(len=*), intent(in)                :: string
   type(atom_type), dimension(:), intent(in)   :: atom
   type(restraint_type), intent(out)           :: res
   type(error_type), intent(out)               :: err
   real, optional, intent(in)                  :: sigd, weid
   logical, intent(out), optional              :: inter
   character(len=len_trim(string))             :: line     !,word     
!corr   integer                                     :: nlongl,nlongw
   integer                                     :: natom
   logical                                     :: lval
   real                                        :: value1     !!!!!,value2
   integer, dimension(100)                     :: vpos
   real, parameter                             :: SIGDIST=0.01, SIGANG=2.0, SIGPLANE=0.1
   logical                                     :: is_esd, is_wei
!corr   real, dimension(20)                         :: vet
   integer                                     :: iv,ier,nword,nw
   real                                        :: sigdis
   character(len=:), allocatable, dimension(:) :: wordv
!
   if (present(sigd)) then
       sigdis = sigd
   else
       sigdis = SIGDIST
   endif
   if (present(inter)) inter = .false.
!corr   ier = 0
   line = trim(adjustl(string))
   call s_filter(line)
   if (len_trim(line) /= 0) then
       natom = 0
       lval = .false.
       is_esd = .false.
       is_wei = .false.
       call get_words1(line,wordv,nword)
!test       do iv=1,nword
!test          write(0,*)'WORD=',wordv(iv)
!test       enddo
       if (nword > 0) then
!
!          Set symmetry type for restraint from last word
           if (s_eqi(wordv(nword),'inter')) then
               inter = .true.
               nword = nword - 1
           endif
       endif
       if (nword > 0) then
!
!          read atoms
!corr           nw = 0
           do nw=1,nword
!corr               nw = nw + 1
               !call Cutst(line,nlongl,word,nlongw)
               !if (nlongw == 0) exit
               !call s_is_r(word,value1,lval)  ! controlla se si tratta di numero e leggi target
               !write(0,*)'WORD=',wordv(nw)
               call s_is_r(wordv(nw),value1,lval)  ! controlla se si tratta di numero e leggi target
               if (lval) exit
               natom = natom + 1
               !vpos(natom) = string_locate(word,atom(:)%lab)
               vpos(natom) = string_locate(wordv(nw),atom(:)%lab)
               if (vpos(natom) == 0) then
                   call err%set('Error reading '//wordv(nw))
!corr                   ier = 1
                   exit
               endif
           enddo
       else
               call err%set('Error')
!corr               ier = 1
       endif
!corr       if (natom > 1 .and. ier == 0) then
       if (natom > 1 .and. .not.err%signal) then
           if (lval) then
!
!              leggi esd and weight
               if (natom <= 3) then  ! se distanza o angolo
                   iv = nword - nw
                   !call Getnum(line,vet,iv=iv)
                   select case (iv)
                     case (1)
                      nw = nw + 1
                      ier = s_to_r(wordv(nw),res%sigma)
                      if (ier /= 0) then
                          call err%set('Error reading '//wordv(nw))
                          return
                      endif
                      is_esd = .true.
                      !res%sigma = vet(1)
                      !res%sigma = value1
                     case (2)
                      nw = nw + 1
                      ier = s_to_r(wordv(nw),res%sigma)
                      if (ier /= 0) then
                          call err%set('Error reading '//wordv(nw))
                          return
                      endif
                      is_esd = .true.
                      !res%sigma = vet(1)
                      nw = nw + 1
                      ier = s_to_r(wordv(nw),res%wei)
                      if (ier /= 0) then
                          call err%set('Error reading '//wordv(nw))
                          return
                      endif
                      is_wei = .true.
                      !res%wei = vet(2)
                   end select
               endif
           else
!
!              If distance is absent look up in the table and set default for esd
               if (natom == 2) then 
                   value1 = bond_distance(atom(vpos(1)),atom(vpos(2)))
               elseif (natom == 3) then
                   value1 = 110.0
               else
                   value1 = SIGPLANE
               endif
           endif
       else
           !ier = 1
           call err%set('Error')
       endif
   else
       call err%set('Error')
       !ier = 1
   endif
!corr   if (ier == 0) then
   if (.not.err%signal) then
       allocate(res%na(natom))
       res%na(:) = vpos(:natom)
       select case (natom)
         case (2)
            res%code = natom - 1
            res%targ = value1
            if (.not.is_esd)  res%sigma = sigdis
            if (present(weid) .and. .not.is_wei) res%wei = weid
         case (3)
            res%code = natom - 1
            res%targ = value1
            if (.not.is_esd)  res%sigma = SIGANG
         case (4:)
            res%code = 3
            res%sigma = value1
       end select
   endif
!corr        if (ier == 0) then
!corr            write(0,*)'RES=',res%na(1),res%na(2),res%targ,res%sigma,res%code,res%wei
!corr        else
!corr            write(0,*)'RES= error '
!corr        endif
!
   end subroutine get_restraint_from_string

!--------------------------------------------------------------------------
!corr
!corr   subroutine read_restraints_from_directives(sdir,ndir,atom,resv,sigd,weid)
!corr!
!corr!  Read restraints from array of directives
!corr!
!corr   USE errormod
!corr   USE strutil
!corr   USE commandsmod
!corr   character(len=*), dimension(:), intent(in)                     :: sdir
!corr   integer, intent(in)                                            :: ndir
!corr   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
!corr   type(restraint_type), dimension(:), allocatable, intent(inout) :: resv
!corr   real, intent(in)                                               :: sigd,weid
!corr   type(error_type)                                               :: err
!corr   character(len=:), allocatable                                  :: str0
!corr   character(len=NLENDIR)                                         :: word
!corr   integer                                                        :: i,nlenw,nlens
!corr   type(restraint_type) :: restemp
!corr   integer :: ier
!corr!
!corr   do i=1,ndir
!corr      str0 = trim(sdir(i))
!corr      call cutst(str0,nlens,word,nlenw)
!corr      if (match_word(word,'REST')) then
!corr      !if (s_eqi(word,'RES')) then
!corr          !write(0,*)'SDIR=',str0,trim(word)
!corr          if (nlens == 0) then
!corr              call err%set("Error in directive "//trim(sdir(i)))
!corr              cycle
!corr          endif
!corr          call get_restraint_from_string(str0,atom,restemp,ier,sigd,weid)
!corr          if (ier == 0) then
!corr              call add_restraint_to_list(resv,restemp)
!corr          else
!corr              call err%set("Error in directive "//trim(sdir(i)))
!corr              cycle
!corr          endif
!corr      endif
!corr   enddo
!corr   if (err%signal) call err%print()
!corr!
!corr   end subroutine read_restraints_from_directives 
!corr
!--------------------------------------------------------------------------

   subroutine set_res_symmetry(res,bond,frag,inter)
!
!  SYM = 0 all symmetry equivalent distances are considered (default for not bonded atoms); 
!  SYM = 1 symmetry equivalent distances are not considered (default for atoms in the same fragment)
!  SYM = 2 only symmetry equivalent atoms are considered
!  if inter is true the restraint is forced to be intermolecolar 
!  if inter is false the subroutine evalueate if the restraint is intermolecular
!
   use fragmentmod
   use connect_mod
   type(restraint_type), intent(inout)                        :: res
   type(bond_type), dimension(:), allocatable, intent(in)     :: bond
   type(fragment_type), dimension(:), allocatable, intent(in) :: frag
   logical, intent(in)                                        :: inter
!
   select case (res%code)
      case (RESDIST)
        if (inter) then
            res%sym(1) = 2
        else
            res%sym(1) = 0
            if ((res%na(1) == res%na(2))) then
                res%sym(1) = 2
                return
            endif
            if (numbonds(bond) > 0) then
                if (bond_position(bond,res%na(1),res%na(2)) > 0) then
                    res%sym(1) = 2
                    return
                endif
            endif
            if (numfragments(frag) == 0) return
            if (fragment_pos(frag,[res%na(1),res%na(2)]) /= 0) res%sym(1)=1  ! atoms in the same fragment
        endif
        !write(0,*)'res n.',i,'SYM=',resv(i)%sym(1)

      case (RESANGLE)
!
!       check if all atoms are in the same fragment
        res%sym(:) = [1,1,1]
        if (fragment_pos(frag,[res%na(1),res%na(2),res%na(3)]) == 0 .or. inter) then ! if intermolecular angle
            if (bond_position(bond,res%na(1),res%na(2)) > 0) then
                if (inter) then
                    res%sym(3) = 2
                else
                    res%sym(3) = 0
                endif
            endif
            if (res%na(1) /= res%na(3)) then  ! avoid check if angle is N1-N2-N1
                if (bond_position(bond,res%na(2),res%na(3)) > 0) then
                    if (inter) then
                        res%sym(1) = 2
                    else
                        res%sym(1) = 0
                    endif
                endif
            endif
!
!           if all atoms in restraints are not bound, fix the central atoms
            if (all(res%sym(:) == 1)) res%sym(2) = 0
             !write(0,*)'ANG SYM=',res%sym(:)
        endif

       
   end select
!
   end subroutine set_res_symmetry

!--------------------------------------------------------------------------
!corr
!corr   subroutine set_equivalent_for_restraints(resv,legm)
!corr!
!corr!  For distance n1-n2 assign value 0 to atom for which identity operator must be not considered
!corr!  For angle n1-n2-n3 assign value 0 to atom for which is not necessary compute equivalent
!corr!
!corr   USE connect_mod
!corr   type(restraint_type), dimension(:), allocatable, intent(inout) :: resv
!corr   type(bond_type), dimension(:), allocatable, intent(in)         :: legm
!corr   integer                                                        :: i
!corr   integer                                                        :: numrestr
!corr!
!corr   numrestr = nrestraints(resv)
!corr   if (numrestr == 0) return
!corr   do i=1,numrestr
!corr      select case (resv(i)%code)
!corr         case (RESDIST)
!corr           if ((resv(i)%na(1) == resv(i)%na(2)) .or. bond_position(legm,resv(i)%na(1),resv(i)%na(2)) > 0) then
!corr               resv(i)%sym(1) = 0
!corr           else 
!corr               resv(i)%sym(1) = 1
!corr           endif
!corr           !write(0,*)'res n.',i,'SYM=',resv(i)%sym(1)
!corr
!corr         case (RESANGLE)
!corr           resv(i)%sym(:) = [1,0,1]
!corr           if (bond_position(legm,resv(i)%na(1),resv(i)%na(2)) > 0) then
!corr               resv(i)%sym(1) = 0
!corr     !          resv(i)%sym(2) = 0
!corr           endif
!corr           if (bond_position(legm,resv(i)%na(2),resv(i)%na(3)) > 0) then
!corr     !          resv(i)%sym(2) = 0
!corr               resv(i)%sym(3) = 0
!corr           endif
!corr!
!corr!          if all atoms in restraints are not bound, fix the central atoms
!corr     !      if (all(resv(i)%sym(:) == 1)) resv(i)%sym(2) = 0
!corr           !write(0,*)'res n.',i,'SYM=',resv(i)%sym(:)
!corr      end select
!corr   enddo
!corr!
!corr   end subroutine set_equivalent_for_restraints
!corr
!--------------------------------------------------------------------------

   subroutine set_value_for_restraint(res,atom,cell)
!
!  Calcola valore corrente per il restraint
!
   USE progtype
   USE cgeom
   USE unit_cell
   USE trig_constants
   type(restraint_type), intent(inout)       :: res
   type(atom_type), dimension(:), intent(in) :: atom
   type(cell_type), intent(in)               :: cell
!
   select case (res%code)
      case (1)
        res%val = distanzaC(atom(res%na(1))%xc,atom(res%na(2))%xc,cell%get_g())

      case (2)
        res%val = rtod*angleC(atom(res%na(1))%xc,atom(res%na(2))%xc,atom(res%na(3))%xc,cell%get_g())

      case (3)
        res%val = mdistance_lsqplane(atom(res%na),cell)

   end select
!
   end subroutine set_value_for_restraint

!--------------------------------------------------------------------------

   subroutine set_weight(res,code,wei)
!
!  Assign weight to all restraints of type code
!
   type(restraint_type), dimension(:), allocatable, intent(inout)  :: res
   integer, intent(in)                                             :: code
   real(DP), intent(in)                                            :: wei
   integer                                                         :: i
!
   do i=1,nrestraints(res)
      if (res(i)%code == code) res(i)%wei = wei
   enddo 
!
   end subroutine set_weight

!------------------------------------------------------------------------

   recursive subroutine conn_to_restraints(bond,atom,cell,code,reslist,mode)
!
!  Converte tutte le distanze e/o angoli in restraints
!  code = 1 (distanze), code = 2 (angoli), code = 3 (angoli+distanze)
!
   USE progtype
   USE connect_mod
   USE atom_type_util
   USE bondtmod
   USE unit_cell
   USE centroids
   USE arrayutil
   type(bond_type), dimension(:), allocatable, intent(in) :: bond
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(cell_type), intent(in)                            :: cell
   type(atom_type), dimension(size(atom))                 :: atomcart
   integer, intent(in)                                    :: code
   type(restraint_type), dimension(:), allocatable        :: reslist
   integer, intent(in)                                    :: mode
   type(container_type), dimension(:), allocatable        :: connt
   type(angle_type), dimension(:), allocatable            :: angle
   integer                                                :: natom
   integer                                                :: nleg,nang
   integer                                                :: i
   real                                                   :: rtarget
   real                                                   :: diffa,diffmina
   type(centroid_type), dimension(:), allocatable         :: centr
   integer                                                :: ncentr
   real                                                   :: dista
!
   nleg = numbonds(bond)
   if (nleg == 0) return
   natom = size(atom)
   select case(code)
       case (RESDIST)
         atomcart(:) = atom
         call frac_to_cart(atomcart,cell%get_ortom())
         do i=1,nleg
            if (is_hydrogen(atomcart(bond(i)%n1)) .or. is_hydrogen(atomcart(bond(i)%n2))) cycle
            if (mode == TARG_CURRENT .or. mode == TARG_MOL_MEC) then
                rtarget = bond(i)%dist
            else
!
!               Ipotizza una distanza target
                call get_info_distance(atomcart(bond(i)%n1),atomcart(bond(i)%n2),dist=rtarget)
            endif
            call push_back_restr(reslist,RESDIST,(/bond(i)%n1,bond(i)%n2/),rtarget,0.01,bond(i)%dist)
         enddo

       case (RESANGLE)
         call bond_to_connect(natom,bond,connt)
         call connect_to_ang(connt,atom,cell%get_g(),angle,nang,.true.)
         do i=1,nang
            if (is_hydrogen(atom(angle(i)%n1)) .or. is_hydrogen(atom(angle(i)%n2)) .or. is_hydrogen(atom(angle(i)%n3))) cycle
            if (mode == TARG_CURRENT .or. mode == TARG_MOL_MEC) then
                rtarget = angle(i)%val
            else
                rtarget = 110.0
                diffmina = abs (angle(i)%val - 110.0) 
                diffa = abs (angle(i)%val - 120.0) 
                if (diffa < diffmina) rtarget = 120.0
            endif
            call push_back_restr(reslist,RESANGLE,(/angle(i)%n1,angle(i)%n2,angle(i)%n3/),rtarget,angle(i)%sigma,angle(i)%val)
         enddo

       case (RESPLANE)
         call find_centroids(atom,bond,centr,ncentr)
         do i=1,ncentr
            dista = mdistance_lsqplane(atom(centr(i)%at),cell)
            call push_back_restr(reslist,RESPLANE,centr(i)%at,0.1,0.01,dista)
         enddo

   end select
!
   end subroutine conn_to_restraints

!corr!--------------------------------------------------------------------------
!corr#if 0
!corr   subroutine make_restraints(atom,legm,cell,rtype,mode,resv)
!corr   type(atom_type), dimension(:), allocatable, intent(in)         :: atom
!corr   type(bond_type), dimension(:), allocatable, intent(in)         :: legm
!corr   type(cell_type), intent(in)                                    :: cell
!corr   integer, intent(in)                                            :: rtype
!corr   integer, intent(in)                                            :: mode
!corr   type(restraint_type), dimension(:), allocatable, intent(inout) :: reslist
!corr   end subroutine make_restraints
!corr#endif
!corr!--------------------------------------------------------------------------
!corr#if 0 
!corr   subroutine update_restraints(reslist,rmvet)
!corr!
!corr!  Aggiorna la lista dei restraints se ho eliminato degli atomi
!corr!  rmvet = nuovo numero d'ordine degli atomi, vale 0 per atomi eliminati
!corr!
!corr   type(restraint_type), dimension(:), allocatable, intent(inout) :: reslist
!corr   integer, dimension(:), intent(in)                              :: rmvet     
!corr   integer                                                        :: i
!corr   integer                                                        :: nat
!corr!
!corr   nat = size(rmvet)
!corr!
!corr!  Elimina i restraints nei quali sono coinvolti atomi eliminati
!corr   do i=1,nat
!corr      if (rmvet(i) == 0) then
!corr      endif
!corr   enddo
!corr!
!corr   end subroutine update_restraints
!corr#endif
!--------------------------------------------------------------------------

   subroutine push_back_restr(reslist,code,atr,targ,sig,val)
!
!  Definisce il restraint e lo aggiunge alla lista 
!
   type(restraint_type), dimension(:), allocatable, intent(inout) :: reslist
   integer, intent(in)               :: code  ! codice restraints
   integer, dimension(:), intent(in) :: atr   ! n. d'ordine degli atomi coinvolti
   real, intent(in)                  :: targ  ! valore target
   real, intent(in)                  :: sig   ! sigma
   real, intent(in), optional        :: val   ! valore corrente
   integer                           :: numres
!
   numres = nrestraints(reslist) + 1
   call resize_restraints(reslist,numres)    
   if (present(val)) then
       reslist(numres) = restraint_type(code,atr,val,targ,sig)
   else
       reslist(numres) = restraint_type(code,atr,0.0,targ,sig)
   endif
!
   end subroutine push_back_restr

!--------------------------------------------------------------------------

   subroutine save_restraints(unitbin,res)
!
!  Write restraints on binary file
!
   integer, intent(in)                                         :: unitbin
   type(restraint_type), dimension(:), allocatable, intent(in) :: res
   integer                                                     :: nres
   integer                                                     :: i
   nres = nrestraints(res)
   write(unitbin) nres
   do i=1,nres
      write(unitbin)size(res(i)%na)
      write(unitbin)res(i)%code,res(i)%na,res(i)%val,res(i)%targ,res(i)%sigma
   enddo
   end subroutine save_restraints

!--------------------------------------------------------------------------

   subroutine read_restraints(unitbin,res,err)
!
!  Read restraints from binary file
!
   USE errormod
   USE arrayutil
   integer, intent(in)                                            :: unitbin
   type(restraint_type), dimension(:), allocatable, intent(inout) :: res
   type(error_type), intent(out)                                  :: err
   integer                                                        :: nres
   integer                                                        :: ier
   integer                                                        :: natres
   integer                                                        :: i
!
   read(unitbin,iostat=ier,err=10)nres
   if (nres > 0 .and. ier == 0) then
       call new_restraints(res,nres)
       do i=1,nres
          read(unitbin,iostat=ier,err=10)natres
          !call reallocate(res(i)%na,natres)
          call new_array(res(i)%na,natres)
          read(unitbin,iostat=ier,err=10)res(i)%code,res(i)%na,res(i)%val,res(i)%targ,res(i)%sigma
       enddo
   endif
!
10 continue
   if (ier /= 0) then
       call err%set('Error on reading restraints information')
   endif
!
   end subroutine read_restraints

!--------------------------------------------------------------------------
 
   subroutine resize_restraints(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo restraint
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(restraint_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n
   logical, optional, intent(in)               :: savevet
   logical                                     :: savev
   integer                                     :: nv
   type(restraint_type), allocatable           :: vsav(:)
   integer                                     :: nsav
!
!  se n = 0 (riallocazione a 0): dealloca ed esci
   if (n == 0) then
       if (allocated(vetr)) deallocate(vetr)
       return
   endif
!
   if (.not.allocated(vetr)) then
       allocate(vetr(n))
   else
!
       nv = size(vetr)
       if (present(savevet)) then
           savev = savevet
       else
           savev = .true.
       endif
!
       if (savev) then
!
!          nsav contiene qual è la porzione di vetr da salvare
           select case(nv-n)
             case (1:)       ! compatta x ad n
               nsav = n
             case (:-1)      ! espandi x ad n
               nsav = nv
             case (0)
               return        ! n=nv non fare niente
           end select
!
!          salva vetr fino a nsav
           allocate(vsav(n))
           vsav(:nsav) = vetr(:nsav)
           call move_alloc(vsav,vetr)
       else
           if (nv /= n) then
               deallocate(vetr)
               allocate(vetr(n))
           endif
       endif
   endif
!
   end subroutine resize_restraints

!--------------------------------------------------------------------------------------------------

   subroutine new_restraints(vetr,n)
!
!  Create new restraints
!
   type(restraint_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                              :: n

   if (n < 0) return
   if (nrestraints(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_restraints

!--------------------------------------------------------------------------------------------------

   subroutine delete_restraints_all(vetr)
!
!  Delete all restraints
!
   type(restraint_type), allocatable, intent(inout) :: vetr(:)

   if (allocated(vetr)) deallocate(vetr)

   end subroutine delete_restraints_all

!--------------------------------------------------------------------------------------------------

   subroutine delete_restraints_pos(vetr,pos)
!
!  Delete pos in the array restraint_type
!
   type(restraint_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                              :: pos
   integer                                          :: nres
!
   nres = nrestraints(vetr)
   if (nres == 0) return
   if (pos < lbound(vetr,dim=1) .or. pos > ubound(vetr,dim=1)) return
   !vetr(:nres-1) = [vetr(:pos-1),vetr(pos+1:)]
   vetr(pos:nres-1) = vetr(pos+1:)
   call resize_restraints(vetr,nres-1)
!
   end subroutine delete_restraints_pos

!--------------------------------------------------------------------------------------------------

   subroutine delete_restraints_atom(vetr,vat,code)
!
!  Delete restraints containing all atoms vat
!
   type(restraint_type), allocatable, intent(inout) :: vetr(:)
   integer, dimension(:), intent(in)                :: vat
   integer, intent(in)                              :: code
   integer                                          :: pos
!
   !res = restraint_type(na=vat)
   pos = restraint_position(vetr,restraint_type(code=code,na=vat))
   if (pos == 0) then
       if (code == ABUMP) then  ! na(1) could be negative for ABUMP
           pos = restraint_position(vetr,restraint_type(code=code,na=[-vat(1),vat(2)]))
       endif
       if (pos == 0) return
   endif
   call delete_restraints_pos(vetr,pos)
!
   end subroutine delete_restraints_atom

!--------------------------------------------------------------------------------------------------

   subroutine delete_restraints_code(resvr)
!
!  Delete restraints with code = DELETED_RES
!
   type(restraint_type), allocatable, intent(inout) :: resvr(:)
   integer                                          :: nres
!
   if (nrestraints(resvr) == 0) return
   nres = count(resvr%code > 0)
   resvr(:nres) = pack(resvr, mask=resvr%code /= DELETED_RES)
   call resize_restraints(resvr,nres)
!
   end subroutine delete_restraints_code 

!--------------------------------------------------------------------------------------------------

   subroutine delete_restraints_from_string(str,vetr,atom,code,ier)
!
!  Delete restraints from string. e.g., C1 C2 or C1 *
!
   USE strutil
   USE atom_type_util
   character(len=*), intent(in)                     :: str
   type(restraint_type), allocatable, intent(inout) :: vetr(:)
   type(atom_type), allocatable, intent(in)         :: atom(:)
   integer, intent(in)                              :: code
   integer, intent(out)                             :: ier
   character(len=20), dimension(3)                  :: wordv
   integer                                          :: nword
   integer :: nat1,nat2,i,j
   integer, dimension(size(atom)) :: vat1,vat2
!
   ier = 0
   if (len_trim(str) == 0) return
   call get_words(str,wordv,nword)
   if (nword /= 2) then
       ier = 1
       return
   endif
!
   call get_atoms_of_string(wordv(1),atom,vat1,nat1)
   if (nat1 == 0) return
   call get_atoms_of_string(wordv(2),atom,vat2,nat2)
   if (nat2 == 0) return
!
   do i=1,nat1
      do j=1, nat2
         call delete_restraints_atom(vetr,[vat1(i),vat2(j)],code)
      enddo
   enddo
!
   end subroutine delete_restraints_from_string

!--------------------------------------------------------------------------------------------------

   subroutine calc_chi2res(reslist,npf,chi2f,chi2r)
!
!  Questa subroutine richiede che i restraints siano ordinati per classi (usa
!  ordina_res)
!
   USE GENLSQ,only:funcls
   type(restraint_type), dimension(:), intent(in) :: reslist
   integer, intent(in) :: npf          ! n di funzioni non di restraints 
   real(DP), intent(out) :: chi2f
   real(DP), dimension(3), intent(out) :: chi2r
   integer             :: i
   integer             :: posi,posf
   integer             :: nr
!
!  Calcola il contributi al chiquadro delle funzioni non di restraints 
   posi = 1
   posf = npf
   chi2f = dot_product(funcls(posi:posf),funcls(posi:posf))
!
!  Calcola il contributo al chiquadro per ogni classe di restraints
   do i=1,3
      nr = count(reslist(:)%code==i)
      if (nr == 0) then
          chi2r(i) = 0
      else
          posi = posf + 1
          posf = posf + nr
          chi2r(i) = dot_product(funcls(posi:posf),funcls(posi:posf))
      endif
   enddo
!
   end subroutine calc_chi2res

!--------------------------------------------------------------------------------------------------

   subroutine ordina_res(reslist)
!
!  Ordina i restraints per classe
!
   USE nr
   type(restraint_type), dimension(:), allocatable, intent(inout) :: reslist
   integer, dimension(size(reslist))                 :: iord
   integer                                           :: n
!
   n = nrestraints(reslist)  !!!!!size(reslist)
   if (n > 0) then
       call indexx(reslist%code,iord)
       reslist(:) = reslist(iord)
   endif
!
   end subroutine ordina_res

!----------------------------------------------------------------------------------------------------        

   real function chi2res_value(rest)
!
!  Compute contribution of single restraint
!
   type(restraint_type), intent(in) :: rest
   real                             :: rplus,rless
!
   rplus = rest%targ + rest%sigma
   rless = rest%targ - rest%sigma
   if (rest%val <= rless) then
       chi2res_value = rest%wei*((rest%val-rless))**2
   elseif (rest%val >= rplus) then
       chi2res_value = rest%wei*((rest%val-rplus))**2
   else
       chi2res_value = 0
   endif
!
   end function chi2res_value

!----------------------------------------------------------------------------------------------------        

   real function chi2tot(resv)
!
!  Sum contribution of all restraints
!
   type(restraint_type), dimension(:), intent(in) :: resv
!
   chi2tot = sum(resv%contrib)
!
   end function chi2tot

!----------------------------------------------------------------------------------------------------        

   subroutine res_save_contrib(resv)
   type(restraint_type), dimension(:), intent(inout) :: resv
!
   resv%contrib_copy = resv%contrib
!
   end subroutine res_save_contrib

!----------------------------------------------------------------------------------------------------        

   subroutine res_restore_contrib(resv)
   type(restraint_type), dimension(:), intent(inout) :: resv
!
   resv%contrib = resv%contrib_copy
!
   end subroutine res_restore_contrib

!----------------------------------------------------------------------------------------------------        

   subroutine generate_restraints(at, bond, cell, rescode, target_type, reslist)
   use atom_type_util
   use connect_mod
   use unit_cell
   use spginfom
   type(atom_type), allocatable, intent(in)         :: at(:)
   type(bond_type), allocatable, intent(in)         :: bond(:)
   type(cell_type), intent(in)                      :: cell 
   integer, intent(in)                              :: rescode
   integer, intent(in)                              :: target_type
   type(restraint_type), allocatable, intent(inout) :: reslist(:)

   call conn_to_restraints(bond,at,cell,rescode,reslist,target_type)

   end subroutine generate_restraints

!----------------------------------------------------------------------------------------------------        

   subroutine update_restraints_target(at, bond, cell, rescode, target_type, reslist)
   use atom_type_util
   use connect_mod
   use unit_cell
   use spginfom
   type(atom_type), allocatable, intent(in)         :: at(:)
   type(bond_type), allocatable, intent(in)         :: bond(:)
   type(cell_type), intent(in)                      :: cell 
   integer, intent(in)                              :: rescode
   integer, intent(in)                              :: target_type
   type(restraint_type), allocatable, intent(inout) :: reslist(:)
   type(restraint_type), allocatable                :: new_res(:)
   integer                                          :: i,pos

   if (nrestraints(reslist) == 0) return

   call conn_to_restraints(bond,at,cell,rescode,new_res,target_type)

   do i=1,nrestraints(new_res)
      pos = restraint_position(reslist, new_res(i))
      if (pos > 0) then
          reslist(pos)%targ =  new_res(i)%targ
      endif
   enddo

   end subroutine update_restraints_target 

 END MODULE RRESTR
