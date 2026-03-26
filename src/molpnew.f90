MODULE molpnew

!corr USE progtype

implicit none

real, dimension(:,:,:), allocatable    :: asymtab
integer, dimension(:,:,:), allocatable :: cr_index_start,cr_index_end

CONTAINS

   subroutine create_connectivity(atom,legm,cellt,spg,check,tolmin,tolmax,angl,move,kpr,cart,vet,code,usecov)
!
!  Create connectivity
!  Se vet e' presente i legami vengono aggiunti a legm
!
   USE connect_mod
   USE fragmentmod
   USE atom_type_util
   USE unit_cell
   USE spginfom
   type(atom_type), dimension(:), intent(inout)                        :: atom          ! atomi modificati solo se move=.true.
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legm          ! legami in forma legm
   type(cell_type), intent(in)                                         :: cellt
   type(spaceg_type), intent(in)                                       :: spg
   logical, intent(in), optional                                       :: check         ! se true attiva il controllo sugli angoli
   real, intent(in), optional                                          :: tolmin,tolmax ! tolleranza sulla distanza
   real, intent(in), optional                                          :: angl          ! angolo minimo consentito
   logical, intent(in), optional                                       :: move          ! regroup dei frammenti attivo
   integer, intent(in), optional                                       :: kpr           ! se 1 stampa qualcosa
   type(atom_type), dimension(:), allocatable                          :: atomcart      ! atomi modificati solo se move=.true.
   logical, intent(in), optional                                       :: cart          ! le coordinate sono in formato cartesiano?
   integer, dimension(:), intent(in), optional                         :: vet           ! code = 1, only atoms in the vector vet are connected to each other and with all other atoms
                                                                                        ! code = 2, only atoms in the vector vet are connected with all other atoms but not to each other
                                                                                        ! code = 3, exclude from connectivity bond i-j with vet > 0 and vet(i) /= vet(j)
   integer, intent(in), optional                                       :: code          ! specify how to use the vector vet
   logical, intent(in), optional                                       :: usecov        ! if true the covalent radii are used
   logical                                                             :: usecovr 
   logical                                                             :: checkb 
   integer                                                             :: kprb
   logical                                                             :: movem
   integer                                                             :: nleg
   type(bond_type), dimension(:), allocatable                          :: legmi
   real                                                                :: angmin
   real                                                                :: tlmin,tlmax
   logical                                                             :: cartm
   integer                                                             :: codevet
!
   if (present(kpr)) then
       kprb = kpr
   else
       kprb = 0
   endif
   if (present(check)) then
       checkb = check
   else
       checkb = .false.
   endif
   if (present(tolmin)) then
       tlmin = tolmin
   else
       tlmin = DEF_BONDSET%tolmin
   endif
   if (present(tolmax)) then
       tlmax = tolmax
   else
       tlmax = DEF_BONDSET%tolmax
   endif
   if (present(angl)) then
       angmin = angl
   else
       angmin = DEF_BONDSET%angle
   endif
   if (present(move)) then
       movem = move
   else
       movem = .false.
   endif
   if (present(cart)) then
       cartm = cart
   else
       cartm = .false.
   endif
   if (present(usecov)) then
       usecovr = usecov
   else
       usecovr = .false.
   endif
   if (kprb > 0) then
       write(6,'(1x,79("="),/30x,"Creating Connectivity")')
       write(6,'(2x,a)')           'Bond distance range'
       if (tlmin >= 0) then
           write(6,'(4x,a,f0.3)')      'distance >= mindist'//' + ',tlmin
       else
           write(6,'(4x,a,f0.3)')      'distance >= mindist'//' - ',abs(tlmin)
       endif
       if (tlmax >= 0) then
           write(6,'(4x,a,f0.3)')      'distance <= maxdist'//' + ',tlmax
       else
           write(6,'(4x,a,f0.3)')      'distance <= maxdist'//' - ',abs(tlmax)
       endif
       if (checkb) then
           write(6,'(2x,a,f0.2,a)')'Angle checking            : angle < ',angmin,' will be rejected'
       else
           write(6,'(2x,a)')       'Angle checking            : disabled'
       endif
       if (movem) then
           write(6,'(2x,a)')       'Regroup                   : enabled'
       else
           write(6,'(2x,a)')       'Regroup                   : disabled'
       endif
   endif
!
!  Converti in cartesiane
   allocate(atomcart(size(atom)),source=atom)
   if (.not.cartm) call frac_to_cart(atomcart,cellt%get_ortom())
!
!  Crea connettivita'
   codevet = 1
   if (present(vet)) then
       if (present(code)) codevet = code
       call create_connectivity_fix(atomcart,cellt,legmi,checkb,tlmin,tlmax,angmin,kprb,vet,codevet,usecovr)
   else
       call create_connectivity_fix(atomcart,cellt,legmi,checkb,tlmin,tlmax,angmin,kprb,usecov=usecovr)
   endif
!
!  Se richiesto raggruppa i frammenti
   if (movem) then
       call assemble_fragments(atom,legmi,cellt,spg,checkb,tlmin,tlmax,angmin,usecovr)
   endif
!
   if (kprb > 0) then
       nleg = numbonds(legmi)
       write(6,'(/2x,i0,a)')nleg,' bonds were found'
       if (nleg > 0) then
           call print_connect(atom(:)%lab,bond=legmi)
       endif
       write(6,'(/1x,79("="))')
   endif
!
   if (present(legm)) then
       if (present(vet) .and. codevet /= 3) then
           call combine_legm(legm,legmi)
       else
           call copy_bonds(legm,legmi)
       endif
   endif
!
   end subroutine create_connectivity

!---------------------------------------------------------------------      

   subroutine assemble_fragments(atom,legm,cell,spg,check,tolmin,tolmax,angl,usecov)
   USE fragmentmod
   USE atom_type_util
   USE unit_cell
   USE spginfom
   USE connect_mod
   type(atom_type), dimension(:), intent(inout)              :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   type(cell_type), intent(in)                               :: cell
   type(spaceg_type), intent(in)                             :: spg
   logical, intent(in), optional                             :: check ! se true attiva il controllo sugli angoli
   real, intent(in), optional                                :: tolmin,tolmax   ! tolleranza sulla distanza
   real, intent(in), optional                                :: angl  ! angolo minimo consentito
   logical, intent(in)                                       :: usecov
   integer                                                   :: natom, nfrag
   type(fragment_type), dimension(:), allocatable            :: fragm
   type(atom_type), dimension(size(atom))                    :: atomcart  
!
!  Generate fragments
   natom = size(atom)
   call get_fragments(atom,cell,legm,nfrag,fragm)
!
!  Regroup fragments and create neu connectivity
   if (nfrag > 1) then
       call regroup_fragment(fragm,atom,cell,spg)
       atomcart(:) = atom 
       call frac_to_cart(atomcart,cell%get_ortom())
       call create_connectivity_fix(atomcart,cell,legm,check,tolmin,tolmax,angl,0,usecov=usecov)
   endif
!
!  Riporta tutto in cella se baricentro fuori cella
   call translate_in_cell(atom)

   end subroutine assemble_fragments

!---------------------------------------------------------------------      

   subroutine complete_atoms(atom,cell,spg,elem,veta,dist,legm)
   USE unit_cell
   USE cgeom
   USE connect_mod
   USE atom_type_util
   USE spginfom
   USE elements
   USE arrayutil
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom   ! tutti gli atomi
   type(cell_type), intent(in)                               :: cell
   type(spaceg_type), intent(in)                             :: spg
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   integer, dimension(:), intent(in)                         :: veta   ! atomi su cui applicare il completamento
   real, intent(in)                                          :: dist
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm  ! legami in forma legm
   type(atom_type), dimension(size(veta))                    :: atomc
   real, dimension(size(veta))                               :: dista
   integer                                                   :: i,j
   integer                                                   :: nat
   real                                                      :: dij
   real, dimension(3)                                        :: ci
   type(atom_type), dimension(:), allocatable                :: atnew
   integer, dimension(1)                                     :: loc
   integer, dimension(size(veta))                            :: vetd
   integer                                                   :: n1,n2,n3
   type(bond_type), dimension(:), allocatable                :: legmc
   integer                                                   :: nbond
   integer                                                   :: ke,ka1,ka2,ka3
   integer                                                   :: natnew
   type(container_type), dimension(:), allocatable          :: conn
   integer                                                   :: kisol
   real                                                      :: d1,d2
   integer                                                   :: natold
!
!  Isola gli atomi su cui applicare il completamento
   nat = size(veta)  
   atomc(:) = atom(veta)
   call frac_to_cart(atomc,cell%get_ortom())
!
!  Crea connettivita' degli atomi coinvolti
   call create_connectivity(atomc,legmc,cell,spg,cart=.true.)
   nbond = numbonds(legmc)
   call bond_to_connect(nat,legmc,conn)
   
   select case (nat)

     case (5)     ! aggiungi un atomo
!
!      Valuta dove aggiungere l'atomo cercando l'atomo piu' isolato
!      cioe' quello con la somma delle distanze più grande
       call bond_to_connect(nat,legmc,conn)
       dista(:) = 0
       do i=1,nat-1
          do j=i+1,nat
             dij = distanzaC(atomc(i)%xc,atomc(j)%xc)
             dista(i) = dista(i) + dij
             dista(j) = dista(j) + dij
          enddo
       enddo
       dista = dista/conn(:)%nat   ! weigth on distance based on number of bonds
       loc = maxloc(dista)
       n1 = loc(1) ! atomo piu' isolato
!
!      Cerca l'atomo n2 piu' vicino a n1
       call get_atoms_distance(atomc(n1),atomc,vetd,vexcl=(/n1/))
       n2 = vetd(1)
!
!      Cerca l'atomo n3 piu' vicino a n2
       call get_atoms_distance(atomc(n2),atomc,vetd,vexcl=(/n1,n2/))
       n3 = vetd(1)
!
!      Genera l'atomo atnew
       allocate(atnew(1))
       atnew(1) = atomc(n1)  ! inizializza atnew
       ci(:) = (/dist,120.,0./)
       call Get_Cartesian_from_Z(ci,atnew(1)%xc,atomc(n1)%xc,atomc(n2)%xc,atomc(n3)%xc)

     case (4)    ! aggiungi 2 atomi

!corr!
!corr!      Crea connettivita' degli atomi coinvolti
!corr       call create_connectivity(atomc,legmc,cell,spg,cart=.true.)
!corr       nbond = numbonds(legmc)
!corr       call bond_to_connect(nat,legmc,conn)
!
       select case (nbond)
           case (2)   ! 2 legami: 2 diverse situazioni sono possibili
!
!            Cerca un atomo isolato
             kisol = 0
             do i=1,nat
                if (conn(i)%nat == 0) then
                    kisol = i
                endif
             enddo
!
             if (kisol > 1) then     ! situazione 1: 3 atomi connessi tra loro e uno isolato
                 ka2 = 0
                 do i=1,nat
                    if (conn(i)%nat == 2) then
                        ka2 = i
                    endif
                 enddo
                 ka1 = conn(ka2)%pos(1)
                 ka3 = conn(ka2)%pos(2)
                 allocate(atnew(2))
                 atnew(1) = atomc(ka1)
                 atnew(2) = atomc(ka1)
                 ci(:) = (/dist,120.,0./)
                 call Get_Cartesian_from_Z(ci,atnew(1)%xc,atomc(ka1)%xc,atomc(ka2)%xc,atomc(ka3)%xc)
                 call Get_Cartesian_from_Z(ci,atnew(2)%xc,atomc(ka3)%xc,atomc(ka2)%xc,atomc(ka1)%xc)
             else                    ! situazione 2: 2 atomi connessi a coppie e nessun isolato
                 ka1 = legmc(1)%n1
                 ka2 = legmc(1)%n2
                 ka3 = legmc(2)%n1
                 allocate(atnew(2))
                 atnew(:) = atomc(ka1)
                 ci(:) = (/dist,120.,0./)
                 call Get_Cartesian_from_Z(ci,atnew(1)%xc,atomc(ka1)%xc,atomc(ka2)%xc,atomc(ka3)%xc)
                 call Get_Cartesian_from_Z(ci,atnew(2)%xc,atomc(ka2)%xc,atomc(ka1)%xc,atomc(ka3)%xc)

             endif

           case (3)   ! 3 legami: aggiungi 2 atomi tra gli atomi estremi della catena
!
!            Cerca l'atomo estremo ke 
             do i=1,nat
                if (conn(i)%nat == 1) then
                    ke=i
                    exit
                endif
             enddo
!
             ka1 = conn(ke)%pos(1) ! atomo connesso a ke
!
!            Cerca l'atomo ka2 connesso a ka1
             ka2 = 0
             do i=1,conn(ka1)%nat
                if (conn(ka1)%pos(i) /= ke) then
                    ka2 = conn(ka1)%pos(i)
                endif
             enddo
!
             if (ka2 > 0) then
                 allocate(atnew(2))
                 atnew(:) = atomc(ke)  ! inizializza atnew
                 ci(:) = (/dist,120.,0./)
                 call Get_Cartesian_from_Z(ci,atnew(1)%xc,atomc(ke)%xc,atomc(ka1)%xc,atomc(ka2)%xc)
                 call Get_Cartesian_from_Z(ci,atnew(2)%xc,atnew(1)%xc,atomc(ke)%xc,atomc(ka1)%xc)
             endif
       end select

     case (3)     ! aggiungi 3 atomi
!corr!
!corr!      Crea connettivita' degli atomi coninvolti
!corr       call create_connectivity(atomc,legmc,cell,spg,cart=.true.)
!corr       nbond = numbonds(legmc)
!corr       call bond_to_connect(nat,legmc,conn)
!       
       select case(nbond)
          case (0)   ! 0 legami
            allocate(atnew(3))
            atnew(:) = atomc(1)
            ci(:) = (/dist,90.,0./)
            call Get_Cartesian_from_Z(ci,atnew(1)%xc,atomc(1)%xc,atomc(2)%xc,atomc(3)%xc)
            call Get_Cartesian_from_Z(ci,atnew(2)%xc,atomc(1)%xc,atomc(3)%xc,atomc(2)%xc)
            call Get_Cartesian_from_Z(ci,atnew(3)%xc,atomc(2)%xc,atomc(1)%xc,atomc(3)%xc)

          case (1)   ! 1 legame 
            ka1 = legmc(1)%n1
            ka2 = legmc(1)%n2
            do i=1,nat
               if (conn(i)%nat == 0) then
                   ka3 = i      ! ka3 e' l'atomo isolato
               endif
            enddo
            allocate(atnew(3))
            atnew(:) = atomc(ka1)
            ci(:) = (/dist,120.,0./)
            call Get_Cartesian_from_Z(ci,atnew(1)%xc,atomc(ka1)%xc,atomc(ka2)%xc,atomc(ka3)%xc)
            call Get_Cartesian_from_Z(ci,atnew(2)%xc,atomc(ka2)%xc,atomc(ka1)%xc,atnew(1)%xc)
!
!           Decidi dove va il terzo atomo
            d1 = distanzaC(atnew(1)%xc,atomc(ka3)%xc)
            d2 = distanzaC(atnew(2)%xc,atomc(ka3)%xc)
            if (d1 > d2) then  ! inserisci il terzo atomo tra atnew(1) e ka3
                call Get_Cartesian_from_Z(ci,atnew(3)%xc,atnew(1)%xc,atomc(ka1)%xc,atomc(ka2)%xc)
            else               ! inserisci il terzo atomo tra atnew(2) e ka3
                call Get_Cartesian_from_Z(ci,atnew(3)%xc,atnew(2)%xc,atomc(ka2)%xc,atomc(ka1)%xc)
            endif

          case (2)   ! 2 legami, i 3 atomi sono connessi tra loro
!
!           Prendi l'atomo centrale
            do i=1,nat
               if (conn(i)%nat == 2) then
                   ka2 = i
                   exit
               endif
            enddo
!
            ka1 = conn(ka2)%pos(1)
            ka3 = conn(ka2)%pos(2)
            allocate(atnew(3))
            atnew(:) = atomc(ka1)
            ci(:) = (/dist,120.,0./)
            call Get_Cartesian_from_Z(ci,atnew(1)%xc,atomc(ka1)%xc,atomc(ka2)%xc,atomc(ka3)%xc)
            call Get_Cartesian_from_Z(ci,atnew(2)%xc,atnew(1)%xc,atomc(ka1)%xc,atomc(ka2)%xc)
            call Get_Cartesian_from_Z(ci,atnew(3)%xc,atnew(2)%xc,atnew(1)%xc,atomc(ka1)%xc)
       end select

   end select 
!
!  Aggiungi i nuovi atomi
   if (allocated(atnew)) then
       call cart_to_frac(atnew,cell%get_ortoi())
       natold = size(atom)
       call add_atoms_to_list(atom,atnew,natnew)
       call atom_string(atom,elem,vet=(/(i,i=natold+1,natnew)/))   ! labella gli atomi aggiunti                                 
       call create_connectivity(atom,legm,cell,spg,vet=(/(i,i=natold+1,natnew)/))  ! connetti gli atomi aggiunti
   endif
!
   end subroutine complete_atoms

!---------------------------------------------------------------------      

   subroutine regularize_atoms(atom,veta,dist,legm,spg,cell,ier)
!
!  Regolarizza gli atomi in veta a poligono regolare di lato dist
!
   USE connect_mod
   USE unit_cell
   USE cgeom
   USE atom_type_util
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom   ! tutti gli atomi
   integer, dimension(:), intent(in)                         :: veta   ! atomi su cui applicare la regolarizzazione
   real, intent(in)                                          :: dist
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm  ! legami 
   type(spaceg_type), intent(in)                             :: spg
   type(cell_type), intent(in)                               :: cell
   integer, intent(out)                                      :: ier
   type(atom_type), dimension(size(veta))                    :: atomc
   integer                                                   :: natr
   real, dimension(3)                                        :: bar,p1,p2
   real, dimension(3,size(veta))                             :: xyz
   real, dimension(4)                                        :: vp
   integer, dimension(size(veta))                            :: vex
   integer                                                   :: i,j
   real                                                      :: distpp,dista,distmin
   real                                                      :: rad
   integer                                                   :: kat,kmin
   real                                                      :: angr
   type(atom_type), dimension(1)                             :: atr
   real, dimension(3)                                        :: dircos
!
   natr = size(veta)
   ier = 0
   if (natr >= 3) then
!
!      conversione in cartesiane
       atomc(:) = atom(veta)
       !call frac_to_cart(atomc,orthomatrix(cell))
       call frac_to_cart(atomc,cell%get_ortom())
!
!      Calcola piano dei minimi quadrati (piano lsq)
       do i=1,natr
          xyz(:,i) = atomc(i)%xc
       enddo
       call lsqplane(xyz(:,:natr),vp)
!
!      Calcola proiezione dei punti nel piano lsq
       do i=1,natr
          distpp = dot_product(vp(:3),atomc(i)%xc) + vp(4) ! distanza punto-piano
          atomc(i)%xc = atomc(i)%xc(:) - vp(:3)*distpp     ! proiezione nel piano
       enddo
!
       bar = baricentro(atomc(:natr))   
!
!      calcola raggio circonferenza circoscitta al poligono regolare di lato dist
       rad = (dist/2) / sin(pi/natr)
!
!      Cerca l'atomo con la distanza dal bar. piu' vicina a rad
       distmin = distanzaC(atomc(1)%xc,bar)
       kat = 1
       do i=2,natr
          dista = distanzaC(atomc(i)%xc,bar)
          if (dista < distmin) then
              distmin = dista
              kat = i
          endif
       enddo
!
!      Sposta l'atomo kat a distanza rad lungo la retta bar-atomo(kat)
       atomc(kat)%xc = bar(:) - (bar(:) - atomc(kat)%xc)*rad/distmin
!
!      Genera per rotazione a partire da kat gli altri atomi
       angr = twopi/natr            ! angolo di rotazione
       p1(:) = bar(:) + vp(:3)*0.5  ! genero p1 e p2 lungo la normale al piano lsq per bar
       p2(:) = bar(:) - vp(:3)*0.5
       dircos = direction_cos(p1,p2)
       vex(:) = 0
       vex(kat) = 1
       do i=1,natr-1
          atr(1)%xc = atomc(kat)%xc
          call rotate_atoms(atr,p1,dircos,i*angr,cell,cart=.true.) ! ruota atr intorno a p1-p2
!
!         cerca l'atomo più vicino ad atr e aggiorna le sue coordinate
          distmin = huge(1.0)
          do j=1,natr
             if (vex(j) == 1) cycle
             dista = distanzaC(atr(1)%xc,atomc(j)%xc)
             if (dista < distmin) then
                 distmin = dista
                 kmin = j
             endif
          enddo
          atomc(kmin)%xc = atr(1)%xc  ! aggiorna le coord.
          vex(kmin) = 1               ! segnala che l'atomo e' stato gia' considerato
       enddo
!
!      Ripristina coord. cristallografiche
       !corr call cart_to_frac(atomc,orthomatrixi(cell))
       call cart_to_frac(atomc,cell%get_ortoi())
       atom(veta) = atomc(:)    ! aggiorna modello in uscita
!
!      Aggiorna la connettivita'      
       call remove_bond_from_atom(legm,veta)              ! rimuovi prima tutti i legami formati dai veta
       call create_connectivity(atom,legm,cell,spg,vet=veta)  ! ricrea la connettivita' solo di veta
   else
       ier = 1
   endif
!
   end subroutine regularize_atoms

!---------------------------------------------------------------------      

   subroutine move_atoms(atom,legm,spg,vat,kat,cell,ier)
!
!  Avvicina gli atomi in vat all'atomo in kat
!
   USE connect_mod
   USE fragmentmod
   USE atom_type_util
   USE unit_cell
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   type(spaceg_type), intent(in)                             :: spg
   integer, dimension(:), intent(in)                         :: vat
   integer, intent(in)                                       :: kat
   type(cell_type), intent(in)                               :: cell
   integer, intent(out)                                      :: ier
   integer                                                   :: nat
   integer                                                   :: nfrag
   type(fragment_type), dimension(:), allocatable            :: fragment
   integer, dimension(:), allocatable                        :: vetf
   type(atom_type), dimension(size(atom))                    :: atomf
   integer                                                   :: natf
   integer                                                   :: pos
   integer                                                   :: i
   integer                                                   :: nv
   real                                                      :: distb
   integer                                                   :: koper
   real, dimension(3)                                        :: ktra
   integer                                                   :: nmove
   type(bond_type), dimension(:), allocatable                :: legmc
!
   nat = numatoms(atom)
   if (nat > 0) then
       nv = size(vat)
       call copy_bonds(legmc,legm)
!
!      disconnetti vat
       call disconnect_atoms(atom,legmc,vat)
!
       call get_fragments(atom,cell,legmc,nfrag,fragment)
       allocate(vetf(nfrag))
                 !call print_fragment(fragment,atom)
!
!      cerca i frammenti che contengono vat
       vetf(:) = 0
       do i=1,nv
          pos = fragment_pos(fragment,vat(i))
          if (pos > 0) then
!
!             questo controllo serve nel caso il frammento pos contenga kat
!             (utente clicca su un atomo contenuto nel gruppo di atomi da avvicinare)
              if (all(fragment(pos)%pos /= kat)) then
                  vetf(pos) = 1
              endif
          endif
       enddo
!
!      adesso disconnetti solo gli atomi per i quali vetf=1
       call copy_bonds(legmc,legm)
       do i=1,nfrag
          if (vetf(i) == 1) then
              call disconnect_atoms(atom,legmc,fragment(i)%pos)
          endif
       enddo
!
!      metti kat in un ulteriore frammento a parte
       call add_fragment(fragment,atom,veta=(/kat/))
       nfrag = nfrag + 1
!
!      I frammenti con vetf = 1 vanno avvicinati al frammento con kat, cioe' fragment(nfrag)
       nmove = 0     ! conta il numero di movimenti
       do i=1,nfrag-1
          if (vetf(i) == 1) then
!
!             Cerca distanza minima tra kat e il frammento i
              call fragment_distance_sym(fragment(nfrag),fragment(i),atom(fragment(nfrag)%pos),  &
                                         atom(fragment(i)%pos),distb,koper,ktra,.true.,cell,spg)
!
!             Applica operatore k e traslazione ktra per avvicinare il frammento se non sono entrambi identita'
              if (.not. (koper == 1 .and. all(ktra == 0))) then
                    !write(0,*)'kop=',koper,ktra
                  nmove = nmove + 1
                  natf = fragment(i)%nat      
                  atomf(:natf) = atom(fragment(i)%pos)
                  call apply_sym_oper(atomf(:natf),spg%symop(koper))
                  call translate_atoms(atomf(:natf),ktra) 
                  atom(fragment(i)%pos) = atomf(:natf)
!
!                 Riconnetti solo gli atomi spostati
                  call create_connectivity(atom,legmc,cell,spg,vet=fragment(i)%pos,code=2)
              endif
          endif
       enddo
   endif
!
   if (nmove > 0) then
       ier = 0
       call copy_bonds(legm,legmc)    ! aggiorna i legami
   else
       ier = 1                        ! segnale che non ci sono stati movimenti
   endif
!
   end subroutine move_atoms

!---------------------------------------------------------------------      

   subroutine apply_symmetry(atom,atoms,legm,legms,level,fitopt,cell,spg)
   USE connect_mod
   USE cgeom
   USE unit_cell
   USE atom_type_util
   USE fragmentmod
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(in)        :: atom    ! unita' asimmetrica
   type(atom_type), dimension(:), allocatable, intent(out)       :: atoms   ! atomi nella cella
   type(bond_type), dimension(:), allocatable, intent(in)        :: legm    ! legami nell'u.a.
   type(bond_type), dimension(:), allocatable, intent(out)       :: legms   ! legami nella cella
   real, dimension(3), intent(in)                                :: level
   integer, intent(in)                                           :: fitopt
   type(cell_type), intent(in)                                   :: cell
   type(spaceg_type), intent(in)                                 :: spg
   type(atom_type), dimension(:), allocatable                    :: atomc,atomco
   type(atom_type), dimension(:), allocatable                    :: atomscart, atomcart
   type(bond_type), dimension(:), allocatable                    :: legmc, legmf
   integer                                                       :: nat
   integer                                                       :: natsym
   integer                                                       :: i,j,k
   type(bond_type), dimension(:), allocatable                    :: legmau  
   real, dimension(3)                                            :: xtra
   integer                                                       :: k1,k2,k3
   integer, dimension(size(atom))                                :: vrem
   integer                                                       :: nrem
   integer                                                       :: nleg
   real, dimension(3)                                            :: xtra0
   real, dimension(3)                                            :: bar1,bar2
   type(op_type)                                                 :: op
   type(fragment_type), dimension(:), allocatable                :: fragm
   integer                                                       :: nfrag,nf,natf
   integer, dimension(:), allocatable                            :: vatmol
   integer, dimension(3)                                         :: klim
   real, dimension(3)                                            :: prange
   logical                                                       :: baropt
   integer                                                       :: nummol
   logical                                                       :: addatoms
   real, dimension(:), allocatable                               :: radfrag
   integer, dimension(2)                                         :: posi,posj
   integer                                                       :: nfi,nfj
   integer, parameter                                            :: MAXATOMS_PACK = 300
   integer, parameter                                            :: MAXATOMS_CHECK = 600
   real, dimension(3,3)                                          :: gmat
   real                                                          :: distb
   !real                                                          :: init_time, end_time

   !call cpu_time(init_time)
   nat = numatoms(atom)
   if (nat == 0) return
   nleg = numbonds(legm)
   gmat = cell%get_g()
!
   natsym = 0
   where(level > 0) 
         klim = ceiling(1+level)   
   else where
         klim = 1                 
   end where 
   prange(:) = level(:)          
   baropt = .false.
   nummol = 0
!
!  estrai frammenti da atom sole se < di MAXATOMS_PACK
   if (nat < MAXATOMS_PACK) then
       call get_fragments(atom,cell,legm,nfrag,fragm)
   else   
       call add_fragment(fragm,atom,veta=(/(i,i=1,nat)/))  ! define only one molecule
       nfrag = 1
   endif
!
   allocate(vatmol(0:nfrag*product(klim(:)*2)*spg%nsymop))   ! prod(klim*2) is the number of explored cells
   vatmol(nummol) = 0
!
   do nf=1,nfrag   ! loop on molecules in asym. unit
      call get_legm_from_fragment(fragm(nf),legm,legmf)
      natf = fragm(nf)%nat
      call new_atoms(atomc,natf)
      call new_atoms(atomco,natf)
      do i=1,spg%nsymop 
         atomco(:) = atom(fragm(nf)%pos)
         call apply_sym_oper(atomco,spg%symop(i))
         call translate_in_cell(atomco,xtra0)
         do k1=-klim(1),klim(1)
            do k2=-klim(2),klim(2)
               do k3=-klim(3),klim(3)
                  addatoms = .false.
                  call copy_atoms(atomc,atomco)
                  call copy_bonds(legmc,legmf)
                  xtra = (/k1,k2,k3/)
                  call translate_atoms(atomc,xtra)
                  op = op_type(i,nint(xtra+xtra0))
                  do j=1,natf
                     atomc(j)%asym = fragm(nf)%pos(j)
                     atomc(j)%op = op
                  enddo
                  call check_new_molecule()   ! manage special position: hot spot of algo
                  if (nrem == natf) cycle
                  if (fitopt == 1) then    ! remove atoms out of range
                      do j=1,natf
                         if (vrem(j) == 0) cycle !already removed by check_new_molecule
                         if (is_out_of_range(atomc(j:j),-prange,1+prange,2)) then
                             vrem(j) = 0
                             nrem = nrem + 1
                         endif
                      enddo
                      if (nrem == natf) cycle
                      if (nrem > 0) call remove_atoms_from_list(atomc,vrem(:natf),0,legm=legmc)
                      addatoms = .true.
                  else
                      if (nrem > 0) call remove_atoms_from_list(atomc,vrem(:natf),0,legm=legmc)
                      if (.not.is_out_of_range(atomc,-prange,1+prange,fitopt-1)) then
                          addatoms = .true.
                      endif
                  endif
                  if (addatoms) then
                      call add_atoms_to_list(atoms,atomc,natsym,legm1=legms,legm2=legmc)
                      nummol = nummol + 1
                      vatmol(nummol) = natsym
                  endif
               enddo
            enddo
         enddo
      enddo
   enddo
!
   if (natsym == 0) return
!
!  Cerca legami tra le u.a.. Escludi questo controllo per le macromolecole
   if (nat < MAXATOMS_PACK) then
       allocate(radfrag(nfrag))
       do nf=1,nfrag
          call frac_to_cart_copy(atom(fragm(nf)%pos),atomcart,cell%get_ortom())
          call get_radius_molecule(atomcart,radfrag(nf))
       enddo
       call frac_to_cart_copy(atoms,atomscart,cell%get_ortom()) ! converti in cartesiane
       do i=1,nummol-1
          posi(1) = vatmol(i-1) + 1
          posi(2) = vatmol(i)
          nfi = fragment_pos(fragm, atomscart(posi(1))%asym)
          do j=i+1,nummol
             posj(1) = vatmol(j-1) + 1
             posj(2) = vatmol(j)
             bar1 = baricentro(atomscart(posi(1) : posi(2)))
             bar2 = baricentro(atomscart(posj(1) : posj(2)))
             nfj = fragment_pos(fragm, atomscart(posj(2))%asym)
             distb = distanzaC(bar1,bar2)
             ! avoid unreasonable bonds in case of disordered structure (e.g. 4107507.cif)
             ! but you could have problems with inorganic compounds
             !if (distb < 1.0) cycle 
             if (distb > radfrag(nfi) + radfrag(nfj) + 4) cycle   ! se verificato non puo' esserci legame
             call connect_groups(atomscart(posi(1):posi(2)),atomscart(posj(1):posj(2)),legmau,.false.)
             do k=1,numbonds(legmau)
                legmau(k)%n1 = legmau(k)%n1 + posi(1) - 1
                legmau(k)%n2 = legmau(k)%n2 + posj(1) - 1
             enddo
             call combine_legm(legms,legmau)
          enddo
       enddo
   endif
!
   call AppSymmBij(atoms, spg)
   !call cpu_time(end_time)
   !write(0,*)'TIME:',end_time-init_time
!
   CONTAINS

   subroutine check_new_molecule()
!
!  Controlla che la nuova unita' asimmetrica non contenga atomi gia' considerati
!
   real               :: djk
   integer            :: j1,ks
   real, dimension(3) :: dx
   real, parameter    :: D2MIN = 0.3*0.3   ! square of minimum distance
!  se D2MIN e' troppo grande rispetto alla tolleranza di legami qualche atomo potrebbe restare isolato
!
   nrem = 0
   if (nat > MAXATOMS_CHECK) then
       vrem(:natf) = 1
       return
   endif
   loop_atom: do j1=1,natf
      vrem(j1) = j1
      do ks=1,natsym
         if(atoms(ks)%asym == atomc(j1)%asym) then  ! Is it the same atom?
            dx = atomc(j1)%xc - atoms(ks)%xc
            djk = DOT_PRODUCT(dx,MATMUL(gmat,dx))
            if (djk < D2MIN) then
                nrem = nrem + 1
                vrem(j1) = 0
                cycle loop_atom
            endif
         endif
      enddo
   enddo loop_atom
!
   end subroutine check_new_molecule
!
   end subroutine apply_symmetry

!----------------------------------------------------------------------------------------------------

   subroutine transform_atoms(atom,cell,spg,symop,tcell,na,vet,legm)
!
!  Transform atoms applying the operator symop and the cell translation tcell
!
   USE connect_mod
   USE spginfom
   USE atom_type_util
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   type(cell_type), intent(in)                               :: cell
   type(spaceg_type), intent(in)                             :: spg
   type(symop_type), intent(in)                              :: symop
   integer, dimension(3), intent(in)                         :: tcell
   integer, intent(in), optional                             :: na
   integer, dimension(:), intent(in), optional               :: vet
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legm
   integer :: i
!
   if (present(na)) then  ! trasform only the na atoms in the array vet
       do i=1,na
           call apply_sym_oper(atom(vet(i)),symop_type(symop%rot,symop%trn+tcell))
       enddo
!
!      rebuild bonds only for kptra atoms
       call disconnect_atoms(atom,legm,vet)
       call create_connectivity(atom,legm,cell,spg,vet=vet)
   else
       call apply_sym_oper(atom,symop_type(symop%rot,symop%trn+tcell))
   endif
!
   end subroutine transform_atoms

!----------------------------------------------------------------------------------------------------

   subroutine make_crystal(atom,cell,spg,np,atomcr)
   USE atom_type_util
   USE spginfom
   USE unit_cell
   USE arrayutil
   type(atom_type), dimension(:), allocatable, intent(in)  :: atom
   type(cell_type), intent(in)                             :: cell
   type(spaceg_type), intent(in)                           :: spg
   integer, dimension(3), intent(in)                       :: np
   type(atom_type), dimension(:), allocatable, intent(out) :: atomcr
   type(atom_type)                                         :: atmp
   integer                                                 :: nat,natcr
   integer                                                 :: i,j,ia
   integer                                                 :: k1,k2,k3,pos
   integer, dimension(3)                                   :: ncr_i,ncr_f
   logical :: lprint = .false.
! 
   nat = numatoms(atom)
   if (nat == 0) return
! 
   do i=1,3
      if (mod(np(i),2) == 0) then
          ncr_f(i) = np(i)/2
          ncr_i(i) = -ncr_f(i) + 1
      else
          ncr_f(i) = np(i)/2
          ncr_i(i) = -ncr_f(i)
      endif
      !write(0,*)'MAKE CRYSTAL:',ncr_i(i),ncr_f(i)
   enddo
! 
   if (allocated(asymtab)) deallocate(asymtab)  !!!TOFIX
   allocate(asymtab(nat,3,spg%nsymop))
   call make_symmetry_table(atom,spg,asymtab)
   !call new_atoms(atomcr,spg%nsymop*nat*((2*np(1)+1)*(2*np(2)+1)*(2*np(3)+1)))
   call new_atoms(atomcr,spg%nsymop*nat*np(1)*np(2)*np(3))
   call new_array(cr_index_start,ncr_i,ncr_f)
   call new_array(cr_index_end,ncr_i,ncr_f)
! 
!  Copy a.u.
   atomcr(:nat) = atom(:)
   do i=1,nat
      call translate_in_cell(atomcr(i))
      atomcr(i)%asym = i
      atomcr(i)%op = op_type()
   enddo
   natcr = nat
! 
!  Apply symmetry operators
   do i=1,nat
      do j=2,spg%nsymop
         atmp = atomcr(i)
         atmp%xc = asymtab(i,:3,j)
         atmp%op%op = j
         call translate_in_cell(atmp)
         if (check_position(atmp,atomcr,natcr,cell) == 0) then
             natcr = natcr + 1
             atomcr(natcr) = atmp
         endif
      enddo
   enddo
   pos = natcr
   cr_index_start(0,0,0) = 1
   cr_index_end(0,0,0) = natcr
! 
!  Apply translation np to group of atoms
   do k1=ncr_i(1),ncr_f(1)
      do k2=ncr_i(2),ncr_f(2)
         do k3=ncr_i(3),ncr_f(3)
            if (all([k1,k2,k3] == 0)) cycle
            atomcr(natcr+1:natcr+pos) = atomcr(:pos)
            call translate_atoms(atomcr(natcr+1:natcr+pos),real([k1,k2,k3]))
!
!           set cell index, ex. -2,-1,0,1,2 for 5x5
            do ia=natcr+1,natcr+pos
               atomcr(ia)%op%tra = [k1,k2,k3]
            enddo
            cr_index_start(k1,k2,k3) = natcr+1
            natcr = natcr + pos
            cr_index_end(k1,k2,k3) = natcr
         enddo
      enddo
   enddo
   if (lprint) write(0,*)'NATCR=',natcr,numatoms(atomcr)
   call resize_atoms(atomcr,natcr)
!TODO !!!temporary additional check
   do i=1,natcr
      pos = check_position(atomcr(i),atomcr,natcr,cell)
      if (pos /= i) then
          if (lprint) write(0,*)'ATOM=',i,pos
      endif
   enddo
! 
   end subroutine make_crystal

!-----------------------------------------------------------------------------------------

   integer function check_position(atom,atoms,nats,cell) result(pos)
! 
!  Check for duplicate atoms
! 
   USE unit_cell
   USE atom_basic
   type(atom_type), intent(in)               :: atom
   type(atom_type), dimension(:), intent(in) :: atoms
   integer, intent(in)                       :: nats
   type(cell_type), intent(in)               :: cell
   real, dimension(3)                        :: dx
   real                                      :: djk
   real, parameter                           :: D2MIN = 0.3*0.3   ! square of minimum distance
   integer                                   :: i
! 
   pos = 0
   do i=1,nats
      if (atom%asym == atoms(i)%asym) then
          dx = atom%xc - atoms(i)%xc
          djk = DOT_PRODUCT(dx,MATMUL(cell%get_g(),dx))
          if (djk < D2MIN) then
              pos = i
              return
          endif
      endif
   enddo
! 
   end function check_position
END MODULE molpnew
