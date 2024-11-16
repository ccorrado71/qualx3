MODULE contacts
!
!  Module to finding hydrogen bonds and other nonbonded contacts
!
   USE elements

   implicit none

   type contact_type
     integer :: zval    ! numero atomico
     integer :: active  ! 1 = contatto attivo, 0 = contatto non attivo
   end type contact_type

   type contact_option_type
     real    :: tolmin     ! tolleranza sulla distanza minima
     real    :: tolmax     ! tolleranza sulla distanza massima
     integer :: hpresent   ! Require hydrogen atom to be present
     integer :: inter      ! Intermolecular
     integer :: intra      ! Intramolecular H bonds with donor and acceptor separated by niminbond
     integer :: nminbond   ! numero minimo di legami che separa accettore dal donatore
     integer :: intrash    ! Intramolecular short contacts
     integer :: nminbondsh ! numero minimo di legami for short contacts
     integer :: hangat     ! 1 if hanging atoms are visibile 
     real    :: cutoff     ! cutoff for contacts
   end type contact_option_type
!
!  Definition of hydrogen bond: D-H...A where D=donor and A=acceptor
   integer, parameter                        :: NDONORS = 4, NACCEPTORS = 7
!
!  Donor atom types
   type(contact_type), dimension(NDONORS), parameter :: donor0 = (/contact_type(7,1),  & ! azoto
                                                                   contact_type(8,1),  & ! ossigeno
                                                                   contact_type(16,1), & ! zolfo
                                                                   contact_type(6,0)/)   ! carbonio
   type(contact_type), dimension(NDONORS) :: donor = donor0
!
!  Acceptor atom types
   type(contact_type), dimension(NACCEPTORS), parameter :: acceptor0 = (/contact_type(7,1),  & ! azoto
                                                             contact_type(8,1),  & ! ossigeno
                                                             contact_type(16,1), & ! zolfo
                                                             contact_type(9,0),  & ! fluoro
                                                             contact_type(17,1), & ! cloro
                                                             contact_type(35,1), & ! bromo
                                                             contact_type(53,1)/)  ! iodio
   type(contact_type), dimension(NACCEPTORS) :: acceptor = acceptor0

   type(contact_option_type), parameter :: hbtopt0 = contact_option_type(-5.0,0.0,0,1,1,3,0,3,1,0.)
   type(contact_option_type)            :: hbtopt = hbtopt0;

   integer, dimension(0:MAXELEMENTS), private  :: vdon,vacc

   integer, parameter, private :: HANGCODE = 1

!FIXME --- forse non ha senso l'expand_mol
   integer, parameter :: EXPAND_ALL=1, EXPAND_ATOM=2, EXPAND_MOL=3

   CONTAINS


   subroutine find_contacts(atom,legm,cell,spg,legmH,legmsh,atoms,legms,lsym,expand,hbond,shbond,start)
!
!  Find contacts
!
   USE atom_type_util
   USE connect_mod
   USE unit_cell
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom   ! atomo nell'u.a.
   type(bond_type), dimension(:), allocatable, intent(in)    :: legm   ! legami nell'u.a.
   type(cell_type), intent(in)                               :: cell
   type(spaceg_type), intent(in)                             :: spg
   type(bond_type), dimension(:), allocatable, intent(inout) :: legmH  ! legami H
   type(bond_type), dimension(:), allocatable, intent(inout) :: legmsh ! short contacts
   type(atom_type), dimension(:), allocatable, intent(inout) :: atoms  ! atomi con simmetria
   type(bond_type), dimension(:), allocatable, intent(inout) :: legms  ! legami con simmetria
   integer, intent(in)                                       :: start
   logical, intent(in)                                       :: lsym   ! packing attivo
   integer, dimension(2), intent(in)                         :: expand ! espandi i contatti
   logical, intent(in)                                       :: hbond  ! cerca i legami idrogeno
   logical, intent(in)                                       :: shbond ! cerca gli short contacts
!corr   type(bond_type), dimension(:), allocatable                :: legmHi,legmHis
   integer                                                   :: kpr = -1
! 
   if (numatoms(atom) == 0) then
       call clear_atoms(atoms)
       call clear_bonds(legms)
       call clear_bonds(legmH)
       call clear_bonds(legmsh)
       return
   endif
!
   !if (hbond .and. shbond) then        ! cerca tutti i contatti
       call find_short_contacts(atom,legm,legmH,legmsh,atoms,legms,lsym,expand,hbond,shbond,cell,spg,start)
!
!      !Cerca legami intramolecolari
       !if (hbtopt%intra == 1) then
       !    call find_intra_hydrogen_bonds(atom,legm,cell,legmHi)
       !    call apply_symmetry_legm(atoms,legmHi,legmHis,spg)
       !    call combine_legm(legmH,legmHis)
       !endif
   !else
   !    if (hbond) then                 ! cerca solo i legami H
   !        call find_hydrogen_bonds(atom,legm,legmH,atoms,legms,lsym,expand,cell,spg)
   !    endif
   !    if (shbond) then                ! cerca solo gli short contacts
   !        call find_short_contacts(atom,legm,legmH,legmsh,atoms,legms,lsym,expand,hbond,shbond,cell,spg,start)
   !    endif
   !endif
   if (kpr >= 0) then
       if (hbond) then
           write(kpr,*)'Hydrogen bonds'
           call print_connect(atoms%lab,bond=legmH,kpri=kpr)
       endif
       if (shbond) then
           write(kpr,*)'Short contacts'
           call print_connect(atoms%lab,bond=legmsh,kpri=kpr)
       endif
   endif
!
   end subroutine find_contacts

!-------------------------------------------------------------------------------------------------
#if 0
   subroutine find_hydrogen_bonds(atom,legm,legmH,atoms,legms,lsym,expand,cell,spg)
!
!  Cerca legami idrogeno.  
!
   USE atom_type_util
   USE connect_mod
   USE unit_cell           !!!!!, only: set_cell_type
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(in)        :: atom   ! atomo nell'u.a.
   type(bond_type), dimension(:), allocatable, intent(in)        :: legm   ! legami nell'u.a.
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legmH  ! legami H
   type(atom_type), dimension(:), allocatable, intent(inout)     :: atoms  ! atomi con simmetria
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legms  ! legami con simmetria
   logical, intent(in)                                           :: lsym   ! packing attivo
   integer, dimension(2),intent(in)                              :: expand ! espandi i contatti
   type(cell_type), intent(in)                                   :: cell
   type(spaceg_type), intent(in)                                 :: spg
   type(bond_type), dimension(:), allocatable                    :: legmHi,legmHis
   logical                                                       :: expand_active
   integer                                                       :: i
   integer                                                       :: nat0
   integer, dimension(:), allocatable                            :: veth
!
!  Cerca legami intermolecolari
   expand_active = .false.
   if (hbtopt%inter == 1) then
       if (expand(1) == EXPAND_ATOM) then 
           expand_active = .true.
           nat0 = numatoms(atoms)
           call grow_fragments(atom,atoms,legm,legms,cell,spg,lhbondi=.true.,legmH=legmH,  &
                               lsym=nat0 > 0,kexp=expand,reset=.false.)
                               !lsym=.true.,kexp=expand,reset=.false.)
           if (nat0 /= numatoms(atoms)) then 
               allocate(veth(numatoms(atoms) - nat0 + 1))
               veth(1) = expand(2)
               veth(2:) = (/(i,i=nat0+1,numatoms(atoms))/)
               call find_hydrogen_bonds_sym(atom,atoms,legm,legms,legmH,.true.,cell,spg,usevet=veth)
               call check_hang_atom(legmH, legms)  ! l'hang atom non e' piu' tale se su di esso avviene un expand
           endif
       elseif (expand(1) == EXPAND_ALL) then 
           expand_active = .true.
           call grow_fragments(atom,atoms,legm,legms,cell,spg,lhbondi=.false.,legmH=legmH,lsym=lsym,kexp=expand)
           call find_hydrogen_bonds_sym(atom,atoms,legm,legms,legmH,.true.,cell,spg)
       else 
           call find_hydrogen_bonds_sym(atom,atoms,legm,legms,legmH,lsym,cell,spg)
       endif
   else
       call copy_atoms(atoms,atom)
       call copy_bonds(legms,legm)
       call clear_bonds(legmH)
   endif
!
!  Cerca legami intramolecolari
   if (hbtopt%intra == 1) then
       call find_intra_hydrogen_bonds(atom,legm,cell,legmHi)
       call apply_symmetry_legm(atoms,legmHi,legmHis,spg)
       if (expand_active) then
           call combine_legm(legmH,legmHis,mergeb = .true.)
       else
           call combine_legm(legmH,legmHis)
       endif
   endif
!
   end subroutine find_hydrogen_bonds
#endif
!-------------------------------------------------------------------------------------------------

   subroutine find_short_contacts(atom,legm,legmH,legmsh,atoms,legms,lsym,expand,hbond,shbond,cell,spg,start_atom)
!
!  Cerca legami idrogeno.  
!
   USE atom_type_util
   USE connect_mod
   USE unit_cell
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(in)        :: atom   ! atomo nell'u.a.
   type(bond_type), dimension(:), allocatable, intent(in)        :: legm   ! legami nell'u.a.
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legmH  ! legami H
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legmsh ! short contacts
   type(atom_type), dimension(:), allocatable, intent(inout)     :: atoms  ! atomi con simmetria
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legms  ! legami con simmetria
   logical, intent(in)                                           :: lsym   ! packing attivo
   integer, dimension(2), intent(in)                             :: expand ! espandi i contatti
   logical                                                       :: hbond  ! cerca anche i legami H
   logical                                                       :: shbond ! cerca short contacts
   type(cell_type), intent(in)                                   :: cell
   type(spaceg_type), intent(in)                                 :: spg
   integer, intent(in)                                           :: start_atom
!corr   logical                                                       :: sym
!corr   integer                                                       :: i
!corr   integer                                                       :: nat0
!corr   integer, dimension(:), allocatable                            :: veth
!
!  Cerca legami intermolecolari
   if (expand(1) == EXPAND_ATOM) then 
       call find_short_contacts_sym_new(atom,atoms,legm,legms,legmsh,legmH,hbond,shbond,lsym,cell=cell,spg=spg, &
                                        section=start_atom)
   !    nat0 = numatoms(atoms)
   !    !sym = .true.
   !    sym = nat0 > 0
   !    call grow_fragments(atom,atoms,legm,legms,cell,spg,hbond,legmH,.true.,legmsh,sym,kexp=expand,reset=.false.)
   !    if (nat0 /= numatoms(atoms)) then 
   !        allocate(veth(numatoms(atoms) - nat0 + 1))
   !        veth(1) = expand(2)
   !        veth(2:) = (/(i,i=nat0+1,numatoms(atoms))/)
   !        call find_short_contacts_sym(atom,atoms,legm,legms,legmsh,legmH,hbond,.true.,cell,spg,usevet=veth)
   !        if (hbond) call check_hang_atom(legmH, legms)  ! l'hang atom non e' piu' tale se su di esso avviene un expand
   !        call check_hang_atom(legmsh, legms) 
   !    endif
   !elseif (expand(1) == EXPAND_ALL) then 
   !    !call grow_fragments(atom,atoms,legm,legms,cell,spg,hbond,legmH,.true.,legmsh,lsym=lsym,kexp=expand)
   !    !call grow_fragments(atom,atoms,legm,legms,cell,spg,lsym=lsym,kexp=expand)
   !    !call find_short_contacts_sym(atom,atoms,legm,legms,legmsh,legmH,hbond,.true.,cell,spg)
   !    call find_short_contacts_sym_new(atom,atoms,legm,legms,legmsh,legmH,hbond,lsym=.true.,cell=cell,spg=spg)
   else 
       !call grow_fragments(atom,atoms,legm,legms,cell,spg,lsym=.false.,kexp=(/EXPAND_ALL,0/))
       call find_short_contacts_sym_new(atom,atoms,legm,legms,legmsh,legmH,hbond,shbond,lsym,cell=cell,spg=spg)
   endif
!
   end subroutine find_short_contacts

!-------------------------------------------------------------------------------------------------

   subroutine find_intra_hydrogen_bonds(atom,legm,cell,legmH) 
!
!  Cerca legami idrogeno intramolecolari
!
   USE atom_type_util
   USE connect_mod
   USE cgeom
   USE unit_cell
   USE fragmentmod
   USE arrayutil
   USE molgraph
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(bond_type), dimension(:), allocatable, intent(in)    :: legm
   type(cell_type), intent(in)                               :: cell
   type(bond_type), dimension(:), allocatable, intent(inout) :: legmH
   integer                                                   :: nat
   integer                                                   :: i,j
   integer                                                   :: zi,zj
   type(atom_type), dimension(:), allocatable                :: atomc
   real                                                      :: dist
   integer                                                   :: nb
   integer                                                   :: hbfound
   integer, dimension(:), allocatable                        :: vet1,vet2
   integer                                                   :: nbondH
   logical                                                   :: distok
   type(container_type), dimension(:), allocatable          :: connH
   integer                                                   :: ndonH
!
   nat = numatoms(atom)
   if (nat == 0) return
!
   !call th%set_type('Contacts')
   !call th%tic()
   call compute_dist_matrix(atom,legm,path=.true.)
!
   allocate(vet1(2*nat),vet2(2*nat)) 
!
   call set_donor_acceptor() 
!
!  converti in cartesiane
   call copy_atoms(atomc,atom)
   call frac_to_cart(atomc,cell%get_ortom())
!
   if (hbtopt%hpresent == 1) then                  ! ho richiesto che l'atomo sia legato a H
       call get_conn_hydrogens(connH,atom,nat,legm)  ! estrai la connettivita' degli H
   else
       allocate(connH(nat))
       connH(:)%nat = 1    ! e' come assumere che tutti gli atomi siano legati a H
   endif
!
!  Loop su tutte le coppie di atomi alla ricerca di coppie donatore-accettore
   nbondH = 0
   loop_atoms: do i=1,nat-1
      zi = atomc(i)%z()
      if (vdon(zi) == 1 .or. vacc(zi) == 1) then  ! l'atomo i e' un donatore/accettore?
          do j=i+1,nat
             zj = atomc(j)%z()
             hbfound = hydrogen_bond_code(zj,zi)
             if (hbtopt%hpresent == 1) then
!
!                controlla che il donatore sia legato ad un H
                 ndonH = 0
                 if (vdon(zj) == 1 .and. connH(j)%nat > 0) ndonH = ndonH + 1
                 if (vdon(zi) == 1 .and. connH(i)%nat > 0) ndonH = ndonH + 1
                 if (ndonH == 0) hbfound = 0
             endif
             if (hbfound > 0) then      ! trovato possibile legame idrogeno
!
!                controlla la distanza
                 call check_hbond_distance(atomc(i)%xc,atomc(j)%xc,zi,zj,distok,dist)
                 if (distok) then
!
!                    controlla il numero di legami che separono il donatore dall'accettore
                     !nb = number_of_bonds_between(i,j,conn,nconnbr,connbr)   !!!!!,hbtopt%nminbond)
                     nb = path_length(i,j)
                     if (nb > hbtopt%nminbond .or. nb == 0) then  ! if nb=0 i and j are in separeted molecules
!
!                        salva in vet1 i donatori e in vet2 gli accettori
                         nbondH = nbondH + 1
                         if (hbfound == 1) then
                             vet1(nbondH) = i
                             vet2(nbondH) = j
                         else
                             vet1(nbondH) = j
                             vet2(nbondH) = i
                         endif
                         if (nbondH == size(vet1)) exit loop_atoms ! too bonds, the structure has problem!
                     endif
                 endif
             endif
          enddo
      endif
   enddo loop_atoms
!
   call free_dist_matrix()
!
!  Crea alla fine il vettore legmH
   if (nbondH > 0) then
       call add_bonds(legmH,atom,vet1(:nbondH),vet2(:nbondH),cell)
       legmH(ubound(legmH,dim=1)-nbondH+1:)%ord = 0 ! intramolecular bonds don't have hanging atoms
   endif
   !call th%toc()
   !call th%tprint(0)
!
   end subroutine find_intra_hydrogen_bonds

!-------------------------------------------------------------------------------------------------------------------

   integer function hydrogen_bond_code(z1,z2)  result(hcode)
!
!  Controlla se tra due atomi di numeri atomi di numeri atomi z1 e z2 e' possibile
!  un legameH. vdon e vacc vanno inizializzati con la set_donor_acceptor
!
   integer, intent(in) :: z1,z2
!
   hcode = 0
   if (vacc(z1) == 1 .and. vdon(z2) == 1) then      ! 1 accettore, 2 donatore
       hcode = 1
   elseif (vdon(z1) == 1 .and. vacc(z2) == 1) then  ! 1 donatore, 2 accettore
       hcode = 2
   endif
!
   end function hydrogen_bond_code

!-------------------------------------------------------------------------------------------------------------------

   subroutine check_hbond_distance(x1,x2,z1,z2,distok,dist)
!
!  Controlla che la distanza tra due atomi sia compatibile con quella di un legame H
!
   USE cgeom
   real, dimension(3), intent(in) :: x1,x2    ! coordinate cartesiane degli atomi
   integer, intent(in)            :: z1,z2    ! numeri atomici corrispondenti
   logical, intent(out)           :: distok   ! e' la distanza buona per un legame H
   real, intent(out)              :: dist     ! la distanza
   real                           :: sumvdw
   real                           :: distmin,distmax
!
   sumvdw = vdw_radius(z1) + vdw_radius(z2)
   distmin = sumvdw + hbtopt%tolmin
   distmax = sumvdw + hbtopt%tolmax
   dist = distanzaC(x1,x2)
   distok = dist < distmax .and. dist > distmin .and. dist >= 0.001
!
   end subroutine check_hbond_distance

!-------------------------------------------------------------------------------------------------------------------

   subroutine check_hbond(k1,k2,x1,x2,z1,z2,conn1,conn2,legH)
   USE connect_mod
   USE bondtmod
   USE arrayutil
   integer, intent(in)              :: k1,k2    ! atom numbers
   real, dimension(3), intent(in)   :: x1,x2    ! coordinate cartesiane degli atomi
   integer, intent(in)              :: z1,z2    ! numeri atomici corrispondenti
   type(container_type), intent(in) :: conn1,conn2
   type(bond_type), intent(out)     :: legH     ! legame H eventualmente trovato
   integer                          :: hbfound
   logical                          :: distok
   real                             :: dist
   real                             :: tol
   real                             :: bondt
   integer                          :: k
   integer                          :: ndonH
!
   tol = 0.3
   legH%n1 = 0
   hbfound = hydrogen_bond_code(z1,z2)
!
   if (hbtopt%hpresent == 1) then
!
!      controlla che il donatore sia legato ad un H
       ndonH = 0
       if (vdon(z1) == 1 .and. conn1%nat > 0) ndonH = ndonH + 1
       if (vdon(z2) == 1 .and. conn2%nat > 0) ndonH = ndonH + 1
       if (ndonH == 0) hbfound = 0
   endif
!
   if (hbfound > 0) then      ! trovato possibile legame idrogeno
!
!      controlla la distanza
       call check_hbond_distance(x1,x2,z1,z2,distok,dist)
       if (distok) then
!
!          Controlla anche che i 2 atomi non siano a distanza di legame chimico
           do k=1,3
              bondt = bond_table(k,z1,z2)
              if (bondt > 0) then
                  if (abs(bondt - dist) <= tol) then
                      distok = .false.
                      exit
                  endif
              else
                  exit
              endif
           enddo
!
           if (distok) then
               legH%n1 = k1
               legH%n2 = k2
               legH%dist = dist
               legH%sigma = 0.3
               legH%ord = 0
           endif
       endif
   endif
!
   end subroutine check_hbond

!-------------------------------------------------------------------------------------------------------------------

   subroutine check_shortc(k1,k2,x1,x2,z1,z2,legs)
   USE connect_mod
   USE bondtmod
   USE cgeom
   integer, intent(in)            :: k1,k2    ! atom numbers
   real, dimension(3), intent(in) :: x1,x2    ! coordinate cartesiane degli atomi
   integer, intent(in)            :: z1,z2    ! numeri atomici corrispondenti
   type(bond_type), intent(out)   :: legs     ! legame H eventualmente trovato
   logical                        :: distok
   real                           :: dist
   real                           :: tol
   real                           :: bondt,over
   integer                        :: k
!
   tol = 0.3
   legs%n1 = 0
!
!  controlla la distanza
   dist = distanzaC(x1,x2)
   over = vdw_radius(z1) + vdw_radius(z2) - dist
   !distok = ((vdw_radius(z1) + vdw_radius(z2) - hbtopt%cutoff) > dist) .and. dist > 0.001
   distok = over > hbtopt%cutoff  .and. dist > 0.001
   if (distok) then
!
!      Controlla anche che i 2 atomi non siano a distanza di legame chimico
       do k=1,3
          bondt = bond_table(k,z1,z2)
          if (bondt > 0) then
              if (abs(bondt - dist) <= tol) then
                  distok = .false.
                  exit
              endif
          else
              exit
          endif
       enddo
!
       !if (distok) legs = bond_type(k1,k2,dist,0.3,0)
       if (distok) legs = bond_type(k1,k2,dist,over,0)
   endif
!
   end subroutine check_shortc

!-------------------------------------------------------------------------------------------------------------------

   subroutine find_inter_hydrogen_bonds(atomc1,atomc2,connH1,connH2,legmH,lhbond,legmsh,shbond)
!
!  Cerca legami idrogeno tra atomc1 e atomc2
!
   USE atom_type_util
   USE connect_mod
   USE cgeom
   USE ccryst
   USE arrayutil
   type(atom_type), dimension(:), intent(in)                    :: atomc1,atomc2   ! in coordinate cartesiane
   type(container_type), dimension(:), allocatable, intent(in) :: connH1,connH2
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legmH
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legmsh
   logical, optional                                         :: lhbond,shbond
   integer                                                   :: nat1,nat2
   integer, dimension(size(atomc1))                          :: zval1
   integer, dimension(size(atomc2))                          :: zval2
   integer                                                   :: i,j
   integer                                                   :: nbondH,nbondsh
   type(bond_type)                                           :: leg
   logical                                                   :: lhbondi,shbondi
!
   nat1 = size(atomc1)
   nat2 = size(atomc2)
   if (nat1 == 0 .or. nat2 == 0) return
!
!  genera Z di tutti gli atomi
   zval1(:) = atomc1%z()
   zval2(:) = atomc2%z()
!       
   if (present(lhbond)) then
       lhbondi = lhbond
   else
       lhbondi = .false.
   endif
   if (present(shbond)) then
       shbondi = shbond
   else
       shbondi = .false.
   endif
!
   if (lhbondi .and. shbondi) then
       call set_donor_acceptor() 
!
       nbondH = 0
       call new_bonds(legmH,nat1*nat2)
       nbondsh = 0
       call new_bonds(legmsh,nat1*nat2)
!
       do i=1,nat1
          do j=1,nat2
             call check_hbond(i,j,atomc1(i)%xc,atomc2(j)%xc,zval1(i),zval2(j),connH1(i),connH2(j),leg)
             if (leg%n1 > 0) then
                 nbondH = nbondH + 1
                 legmH(nbondH) = leg
             else
                 call check_shortc(i,j,atomc1(i)%xc,atomc2(j)%xc,zval1(i),zval2(j),leg)
                 if (leg%n1 > 0) then
                     nbondsh = nbondsh + 1
                     legmsh(nbondsh) = leg
                 endif
             endif
          enddo
       enddo
       call resize_bonds(legmH,nbondH)
       call resize_bonds(legmsh,nbondsh)
   else
       if (lhbondi) then
           call set_donor_acceptor() 
!
           nbondH = 0
           call new_bonds(legmH,nat1*nat2)
!
           do i=1,nat1
              if (vdon(zval1(i)) == 1 .or. vacc(zval1(i)) == 1) then  ! l'atomo i e' un donatore/accettore?
                  do j=1,nat2
                     call check_hbond(i,j,atomc1(i)%xc,atomc2(j)%xc,zval1(i),zval2(j),connH1(i),connH2(j),leg)
                     if (leg%n1 > 0) then
                         nbondH = nbondH + 1
                         legmH(nbondH) = leg
                     endif
                  enddo
              endif
           enddo
!
           call resize_bonds(legmH,nbondH)
       endif
!
       if (shbondi) then
           nbondsh = 0
           call new_bonds(legmsh,nat1*nat2)
!
           do i=1,nat1
              do j=1,nat2
                 call check_shortc(i,j,atomc1(i)%xc,atomc2(j)%xc,zval1(i),zval2(j),leg)
                 if (leg%n1 > 0) then
                     nbondsh = nbondsh + 1
                     legmsh(nbondsh) = leg
                 endif
              enddo
           enddo
!
           call resize_bonds(legmsh,nbondsh)
       endif
   endif
!
   end subroutine find_inter_hydrogen_bonds

!-------------------------------------------------------------------------------------------------------------------

#if 0
   subroutine find_hydrogen_bonds_sym(atom,atoms,legm,legms,legmH,lsym,cell,spg,usevet)
!
!  Cerca legami idrogeno in atoms.
!  atoms contiene l'unita' asimmetrica piu' il risultato dell'applicazione di operatori di simmetria
!
   USE atom_type_util
   USE connect_mod
   USE unit_cell
   USE spginfom
   USE arrayutil
   type(atom_type), dimension(:), allocatable, intent(in)        :: atom   ! atomi nell'u.a.
   type(atom_type), dimension(:), allocatable, intent(inout)     :: atoms   ! atomi nella cella
   type(bond_type), dimension(:), allocatable, intent(in)        :: legm  ! legami 
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legms  ! legami nella cella
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legmH
   logical, intent(in)                                           :: lsym
   type(cell_type), intent(in)                                   :: cell
   type(spaceg_type), intent(in)                                 :: spg
   integer, optional, dimension(:), intent(in)                   :: usevet
   type(atom_type), dimension(:), allocatable                    :: atomcart
   integer                                                       :: natsym
   integer, dimension(:), allocatable                            :: zval
   integer                                                       :: i,j,k
   integer                                                       :: nbondH,nmaxbondH
   type(bond_type)                                               :: legH
   integer                                                       :: nat
   type(atom_type), dimension(1)                                 :: atom2
   type(atom_type)                                               :: atomc2,atomsav
   integer, dimension(3)                                         :: ktra
   integer                                                       :: k1,k2,k3
   integer                                                       :: natsym0
   logical                                                       :: ladd
   type(container_type), dimension(:), allocatable              :: connH
   integer, dimension(3)                                         :: diff
   integer, dimension(:), allocatable                            :: useat
   logical                                                       :: use_all
!
   if (.not.lsym) call init_for_symm(atom,legm,atoms,legms)
   natsym = numatoms(atoms)
   if (natsym == 0) then
       call clear_bonds(legmH)
       return
   endif
   allocate(zval(natsym))
!
!  converti in cartesiane
   call frac_to_cart_copy(atoms,atomcart,cell%get_ortom())
!
   zval(:) = atoms%z()
   call set_donor_acceptor() 
!
   if (hbtopt%hpresent == 1) then                  ! ho richiesto che l'atomo sia legato a H
       call get_conn_hydrogens(connH,atoms,natsym,legms)  ! estrai la connettivita' degli H
   else
       allocate(connH(natsym))
       connH(:)%nat = 1    ! e' come assumere che tutti gli atomi siano legati a H
   endif
!
   nmaxbondH = 4*natsym
   if (present(usevet)) then
       allocate(useat(size(usevet)))
       useat = usevet
       use_all = .false.
   else
       allocate(useat(1))
       useat(1) = 0
       use_all = .true.
   endif
   if (use_all) then  ! ricalcola tutti gli H-bonds
       nbondH = 0
       call new_bonds(legmH,nmaxbondH)
   else               ! calcola H-bonds solo per gli atomi in usevet
       nbondH = numbonds(legmH)
       call resize_bonds(legmH,nmaxbondH)
   endif
!
!  Cerca legami H all'interno di atoms ma in unita' diverse ma solo a simmetria attiva
   if (lsym .and. use_all) then
       do i=1,natsym-1
          if (vdon(zval(i)) == 1 .or. vacc(zval(i)) == 1) then  ! l'atomo i e' un donatore/accettore?
              do j=i+1,natsym
                 !if (infos(i)%opcode /= infos(j)%opcode) then   ! unita' diverse
                 if (atoms(i)%op /= atoms(j)%op) then
                     call check_hbond(i,j,atomcart(i)%xc,atomcart(j)%xc,zval(i),zval(j),connH(i),connH(j),legH)
                     if (legH%n1 > 0) then
                         nbondH = nbondH + 1
                         legmH(nbondH) = legH
                     endif
                 endif
              enddo
          endif
       enddo
   endif
!
!  Ora cerca altri legami H con atomi non inclusi in atoms
   nat = size(atom)
   natsym0 = natsym
   call resize_atoms(atoms,natsym0+nmaxbondH,.true.)
   ladd = .false.
   loop_nat: do j=1,nat
      if (vdon(zval(j)) == 1 .or. vacc(zval(j)) == 1) then  ! l'atomo i e' un donatore/accettore?
          do k=1,spg%nsymop
             atom2(1) = atom(j)
!corr             call apply_sym_oper(k,atom2(1:1))
             call apply_sym_oper(atom2(1:1),spg%symop(k))
             do i=1,natsym
                if (vdon(zval(i)) == 1 .or. vacc(zval(i)) == 1) then  ! l'atomo i e' un donatore/accettore?
                    if (use_all .or. any(useat(:) == i)) then         ! i contenuto in useat?
                        diff(:) = nint(atoms(i)%xc - atom2(1)%xc)
                        do k1=-1,1
                           do k2=-1,1
                              do k3=-1,1
                                 ktra = (/k1,k2,k3/) + diff  
                                 !opcode = operator_code(k,ktra)
                                 !corr if (checkeq_infos(symminfo_type(j,k,opcode),infos(:natsym))) cycle
                                 if (checkeq_symm(atoms(:natsym),j,op_type(k,ktra)) > 0) cycle
                                 atomsav = atom2(1)
                                 atomsav%xc = atomsav%xc + ktra
                                 atomc2 = cartesian_coord(atomsav,cell%get_ortom())
                                 call check_hbond(i,natsym0+1,atomcart(i)%xc,atomc2%xc,zval(i),zval(j),connH(i),connH(j),legH)
                                 if (legH%n1 > 0) then
                                     if (nbondH == nmaxbondH) exit loop_nat ! too short contacts and bad structure!
                                     nbondH = nbondH + 1
                                     !write(0,*)'hang atom:',legH%n2
                                     !corr legH%n2 = -legH%n2 ! mark bond with hang atom
                                     legH%ord = HANGCODE
                                     legmH(nbondH) = legH
                                     ladd = .true.
                                 endif
                                 if (ladd) then
                                     natsym0 = natsym0 + 1
                                     atoms(natsym0) = atomsav
                                     atoms(natsym0)%asym = j
                                     atoms(natsym0)%op = op_type(k,ktra)
                                      !write(0,*)'HB1=',trim(atoms(legH%n1)%lab)//'-'//trim(atoms(legH%n2)%lab)
                                     ladd = .false.
                                 endif
                              enddo
                           enddo
                        enddo
                    endif
                endif
             enddo
          enddo
      endif
   enddo loop_nat
!!!!!!   endif

!!!!   if (ier == 0) then
       call resize_bonds(legmH,nbondH)
       call resize_atoms(atoms,natsym0)
!corr       call reallocate_infos(infos,natsym0,.true.)
!
!!! FIXME
!!! mercury non lo fa -- se commenti questi pezzo potresti avere problema con la connettivita' dopo expand
!!! ti conviene marcare gli hanging atoms
!!!corr       if (natsym0 - natsym > 0) then   ! se ho aggiunto nuovi atomi ...
!!!corr           call complete_contacts(atomcart,atoms,legms,legmH)
!!!corr       endif
!!!!   else   ! ripristina ma mostra i legami intramolecolari
!!!!       call reallocate_legm(legmH,0)
!!!!       call reallocate(atoms,natsym,.true.)
!!!!       call reallocate_infos(infos,natsym,.true.)
!!!!   endif
!
   end subroutine find_hydrogen_bonds_sym

!---------------------------------------------------------------------------------------      

   subroutine find_short_contacts_sym(atom,atoms,legm,legms,legmsh,legmH,hbond,lsym,cell,spg,usevet,nodupl)
!
!  Find short contacts in atom. 
!  Atoms will contain a.u. and the result of the application of symmetry operators
!  nodupl = true if you want to display symmetrical contacts: A..B and B..A. Default choice.
!         = false only A..B, this choice is suggested if you want generate a simple list of contacs
!
   USE atom_type_util
   USE connect_mod
   USE unit_cell
   USE spginfom
   USE fragmentmod
   USE arrayutil
   USE molgraph
   type(atom_type), dimension(:), allocatable, intent(in)        :: atom   ! atomi nell'u.a.
   type(atom_type), dimension(:), allocatable, intent(inout)     :: atoms  ! atomi nella cella
   type(bond_type), dimension(:), allocatable, intent(in)        :: legm   ! legami 
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legms  ! legami nella cella
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legmH,legmsh
   logical                                                       :: hbond
   logical, intent(in)                                           :: lsym
   type(cell_type), intent(in)                                   :: cell
   type(spaceg_type), intent(in)                                 :: spg
   integer, optional, dimension(:), intent(in)                   :: usevet
   logical, intent(in), optional                                 :: nodupl
   type(atom_type), dimension(:), allocatable                    :: atomcart
   integer                                                       :: natsym
   integer, dimension(:), allocatable                            :: zval
   integer                                                       :: i,j,k
   integer                                                       :: nbondH,nmaxbondH
   integer                                                       :: nbondsh,nmaxbondsh
   type(bond_type)                                               :: leg
   integer                                                       :: nat
   type(atom_type), dimension(1)                                 :: atom2
   type(atom_type)                                               :: atomc2,atomsav
   integer, dimension(3)                                         :: ktra
   integer                                                       :: k1,k2,k3
   integer                                                       :: natsym0
   logical                                                       :: ladd
   logical                                                       :: jump
   type(container_type), dimension(:), allocatable               :: connH
   integer, dimension(3)                                         :: diff
   integer                                                       :: ier
   integer, dimension(:), allocatable                            :: useat
   logical                                                       :: use_all
   integer                                                       :: nmaxatoms
   logical                                                       :: saves
   type(fragment_type), dimension(:), allocatable                :: frag
   integer                                                       :: nfrag,iasym,jpos
   integer, dimension(size(atom))                                :: vfrag
   type(bond_type), dimension(:), allocatable                    :: legmshi,legmshii
   integer                                                       :: nbondshi,nbondshimax
   type(atom_type)                                               :: atomi,atomj
   logical                                                       :: nodupli
     integer :: nb
!
   ier = 0
   if (.not.lsym) call init_for_symm(atom,legm,atoms,legms)
   natsym = numatoms(atoms)
   nat = numatoms(atom)
   if (nat == 0) then
       call clear_bonds(legmH)
       call clear_bonds(legmsh)
       return
   endif
!
   if (present(nodupl)) then
       nodupli = nodupl
   else
       nodupli = .false.
   endif
!
!  Make array vfrag containing info about fragment
   call frac_to_cart_copy(atoms,atomcart,cell%get_ortom())
   call get_fragments(atom,cell,legm,nfrag,frag)
   do i=1,nat
      vfrag(i) = fragment_pos(frag,i)
   enddo
!
   if (present(usevet)) then
       allocate(useat(size(usevet)))
       useat = usevet
       use_all = .false.
   else
       allocate(useat(1))
       useat(1) = 0
       use_all = .true.
   endif
!
   allocate(zval(natsym))
   zval(:) = atoms%z()
   call set_donor_acceptor() 
!
   if (use_all) then  ! ricalcola tutti gli short contacts
       nbondH = 0
       nbondsh = 0
       saves = .false.
   else               ! calcola contacts solo per gli atomi in usevet
       nbondH = numbonds(legmH)
       nbondsh = numbonds(legmsh)
       saves = .true.
   endif
   if (hbond) then
       nmaxbondH = 4*natsym
       call resize_bonds(legmH,nmaxbondH,saves)
   endif
   nmaxbondsh = 4*natsym
   call resize_bonds(legmsh,nmaxbondsh,saves)
!
   if (hbond) then
       if (hbtopt%hpresent == 1) then                  ! ho richiesto che l'atomo sia legato a H
           call get_conn_hydrogens(connH,atoms,natsym,legms)  ! estrai la connettivita' degli H
       else
           allocate(connH(natsym))
           connH(:)%nat = 1    ! e' come assumere che tutti gli atomi siano legati a H
       endif
   endif
!
!  Cerca legami H all'interno di atoms ma in unita' diverse
   if (lsym .and. use_all) then
       call compute_dist_matrix(atoms,legms,path=.true.)
       do i=1,natsym-1
          do j=i+1,natsym
             if (atoms(i)%op /= atoms(j)%op) then   ! unita' diverse
                 jump = .false.
!!!!!!!!!!!!!test
                     nb = path_length(i,j)
                     !if (nb > 3 .or. nb == 0) then  ! if nb=0 i and j are in separeted molecules
                     if (nb == 0) then  ! if nb=0 i and j are in separeted molecules
!!!!!!!!!!!!!test
!
!                Cerca prima i legami H se richiesti
                 if (hbond) then
                     call check_hbond(i,j,atomcart(i)%xc,atomcart(j)%xc,zval(i),zval(j),connH(i),connH(j),leg)
                     if (leg%n1 > 0) then
                         nbondH = nbondH + 1
                         legmH(nbondH) = leg
                         jump = .true.
                     endif
                 endif
!
!                Se esiste il legame H salta il controllo sullo short contact
                 if (.not.jump) then
                     call check_shortc(i,j,atomcart(i)%xc,atomcart(j)%xc,zval(i),zval(j),leg)
                     if (leg%n1 > 0) then
                         nbondsh = nbondsh + 1
                         legmsh(nbondsh) = leg
                     endif
                 endif
                     else
                       !write(0,*)'path:',nb,atoms(i)%glab()//"-"//atoms(j)%glab()
                     endif
             endif
          enddo
       enddo
   endif
!
!  Find short contacts between fragments
   if (nfrag > 0) then  ! .and. .not.lsym) then
       nbondshimax = 4*nat
       call new_bonds(legmshi,nbondshimax)
       nbondshi = 0
       loop_ext: do i=1,nat
          atomi = cartesian_coord(atom(i),cell%get_ortom())
          do j=i+1,nat
             if (vfrag(i) /= vfrag(j)) then
                 if (lsym) then
                     atomj = cartesian_coord(atom(j),cell%get_ortom())
                     call check_shortc(i,j,atomi%xc,atomj%xc,atom(i)%z(),atom(j)%z(),leg)
                 else
                     call check_shortc(i,j,atomcart(i)%xc,atomcart(j)%xc,zval(i),zval(j),leg)
                 endif
                 if (leg%n1 > 0) then
                     nbondshi = nbondshi + 1
                     legmshi(nbondshi) = leg
                     if (nbondshi == nbondshimax) exit loop_ext
                 endif
             endif
          enddo
       enddo loop_ext
       if (nbondshi > 0) then
           if(lsym) then
              call apply_symmetry_legm(atoms,legmshi,legmshii,spg)
           else
              allocate(legmshii(nbondshi),source=legmshi(:nbondshi))
           endif
       endif
   endif
!
!  Ora cerca altri legami H con atomi non inclusi in atoms
   natsym0 = natsym
   nmaxatoms = natsym0+nmaxbondsh
   if (hbond) nmaxatoms = nmaxatoms + nmaxbondh
   call resize_atoms(atoms,nmaxatoms)
   ladd = .false.
   jpos = 1
   loop_atom: do j=1,nat
      if (nodupli) jpos = j
      do k=1,spg%nsymop
         atom2(1) = atom(j)
         call apply_sym_oper(atom2(1:1),spg%symop(k))
         do i=jpos,natsym
         !do i=1,natsym
            if (use_all .or. any(useat(:) == i)) then         ! i contenuto in useat?
                diff(:) = nint(atoms(i)%xc - atom2(1)%xc)
                do k1=-1,1
                   do k2=-1,1
                      do k3=-1,1
                         ktra = (/k1,k2,k3/) + diff(:)
!
!                        atom2 contained in atoms, same fragments
                         !same_frag = vfrag(j) == vfrag(atoms(i)%asym)
                         !if (checkeq_symm(atoms(:natsym),j,op_type(k,ktra)) .and. vfrag(j) == vfrag(atoms(i)%asym)) cycle
                         if (checkeq_symm(atoms(:natsym),j,op_type(k,ktra)) > 0) cycle
                         atomsav = atom2(1)
                         atomsav%xc = atomsav%xc + ktra
                         atomc2 = cartesian_coord(atomsav,cell%get_ortom())
                         jump = .false.
                         if (hbond) then
                             call check_hbond(i,natsym0+1,atomcart(i)%xc,atomc2%xc,zval(i),zval(j),connH(i),connH(j),leg)
                             if (leg%n1 > 0) then
                                 if (nbondH == nmaxbondH) exit loop_atom ! too short contacts and bad structure!
                                 nbondH = nbondH + 1
                                 !write(0,*)'hang atom:',leg%n2
                                 !leg%n2 = -leg%n2 ! mark bond with hang atom
                                 leg%ord = HANGCODE
                                 legmH(nbondH) = leg
                                 ladd = .true.
                                 jump = .true.
                             endif
                         endif
                         if (.not.jump) then
                             call check_shortc(i,natsym0+1,atomcart(i)%xc,atomc2%xc,zval(i),zval(j),leg)
                             if (leg%n1 > 0) then
                                 if (nbondsh == nmaxbondsh) exit loop_atom ! too short contacts and bad structure!
                                 leg%ord = HANGCODE
                                 ladd = .true.
                                 nbondsh = nbondsh + 1
                                 legmsh(nbondsh) = leg
!                                
!                                Check for atom already added. Possibile when an hanging atom has more then one contacts
                                 if (natsym0 > 0) then
                                     iasym = checkeq_symm(atoms(natsym+1:natsym0),j,op_type(k,ktra))
                                     if (iasym > 0) then
                                         ladd=.false.
                                         leg%ord = 0
                                         legmsh(nbondsh)%n2 = iasym+natsym
                                     endif
                                 endif
                             endif
                         endif
                         if (ladd) then
                             natsym0 = natsym0 + 1
                             atoms(natsym0) = atomsav
                             atoms(natsym0)%asym = j
                             atoms(natsym0)%op = op_type(k,ktra)
                             ladd = .false.
                             if (natsym0 == nmaxatoms) exit loop_atom ! too atoms and bad structure!
                         endif
                      enddo
                   enddo
                enddo
            endif
         enddo
      enddo
   enddo loop_atom
!
   call resize_bonds(legmH,nbondH)
   call resize_bonds(legmsh,nbondsh)
   call combine_legm(legmsh,legmshii)
   call resize_atoms(atoms,natsym0,.true.)
!corr   call reallocate_infos(infos,natsym0,.true.)
!
!!! FIXME
!!! mercury non lo fa -- se commenti questi pezzo potresti avere problema con la connettivita' dopo expand
!!! ti conviene marcare gli hanging atoms
!!corr   if (natsym0 - natsym > 0 .and. ier == 0) then   ! se ho aggiunto nuovi atomi ...
!!corr       call complete_contacts(atomcart,atoms,legms,legmsh)
!!corr   endif
!
   end subroutine find_short_contacts_sym
#endif
!---------------------------------------------------------------------------------------      

   subroutine set_donor_acceptor()  
!
!  Genera i vettori vdon e vacc che valgano 1 per i donatori/accettori attivi e 0 per quelli non attivi
!
   integer                             :: i
!
   vdon(:) = 0
   do i=1,NDONORS
      if (donor(i)%active == 1) then
          vdon(donor(i)%zval) = 1
      endif
   enddo
   vacc(:) = 0
   do i=1,NACCEPTORS
      if (acceptor(i)%active == 1) then
          vacc(acceptor(i)%zval) = 1
      endif
   enddo
!
   end subroutine set_donor_acceptor

!-------------------------------------------------------------------------------------------------------------------

   function number_of_bonds_between(ki,kj,connect,nconnbr,connbr)  result(nbmin)
!
!  Trova il numero di legami che separa ki da kj
!
   USE connect_mod
   USE arrayutil
   integer, intent(in)                                          :: ki,kj
   type(container_type), dimension(:), intent(in)              :: connect
   integer, intent(in)                                          :: nconnbr
   type(container_type), dimension(:), allocatable, intent(in) :: connbr
   type(container_type), dimension(:), allocatable             :: connc
   integer                                                      :: ncc
   integer                                                      :: i,j
   integer                                                      :: kpr = 0
   integer                                                      :: nbmin
   integer                                                      :: krot
   logical                                                      :: kfound2
   integer, dimension(:), allocatable                           :: minpath
!
   ncc = size(connect)
   allocate(connc(ncc),source=connect) 
!
!  Elimina tutte le ramificazioni che non contengono entrambi gli atomi
   kfound2 = .false.
   do i=1,nconnbr
      krot = 0
      do j=1,connbr(i)%nat
         if (connbr(i)%pos(j) == ki .or. connbr(i)%pos(j) == kj) then
             krot = krot + 1
         endif
      enddo
      if (krot == 0) then    ! now remove branch from connc
          call container_update_remove(connc,vrem = connbr(i)%pos)
      endif
      if (krot == 2) then
          kfound2 = .true.
      endif
   enddo
!
!  If kfound2 is false the 2 atoms are not connected
   if (.not.kfound2) then
        nbmin = 0
        return
   endif
!
   if (kpr > 0) write(6,*)'NB tra :',ki,kj
   allocate(minpath(size(connc)))
!
   call minpath_find(ki,kj,connc,minpath,nbmin)
   !if (nminstep > 0) write(0,*)'PATH=',minpath(:nbmin)
!
   end function number_of_bonds_between

!-------------------------------------------------------------------------------------------------------------------
!corr
!corr   subroutine check_intra_contacts(atom,legm,cell,legmc,nblimit)    !!!!!unused subroutine
!corr!
!corr!  Elimina quei contatti che sono diventat di tipo intramolecolare
!corr!
!corr   USE connect_mod
!corr   USE fragmentmod
!corr   USE unit_cell
!corr   USE arrayutil
!corr   USE atom_basic
!corr           use molgraph
!corr   type(atom_type), dimension(:), intent(in), allocatable    :: atom    ! atomi
!corr   type(bond_type), dimension(:), allocatable, intent(in)    :: legm    ! legami chimici
!corr   type(cell_type), intent(in)                               :: cell
!corr   type(bond_type), dimension(:), allocatable, intent(inout) :: legmc   ! contatti
!corr   integer, intent(in)                                       :: nblimit ! minimo numero di legami 
!corr   integer                                                   :: nat,nleg,nlegc
!corr   integer                                                   :: i
!corr   integer                                                   :: nconnbr
!corr   type(container_type), dimension(:), allocatable          :: conn,connbr
!corr   integer                                                   :: nb
!corr!
!corr   nlegc = numbonds(legmc)
!corr   if (nlegc > 0) then
!corr            call compute_distance_matrix(atom,legm)
!corr       nat = numatoms(atom) !size(atom)
!corr       nleg = numbonds(legm)
!corr!
!corr!      estrai ramificazioni per call alla number_of_bonds_between
!corr       call bond_to_connect(nat,legm,conn)       ! estrai prima conn da legm
!corr       call get_branch(atom,cell,conn,legm,nconnbr,connbr) 
!corr!
!corr!      Check per contatti intramolecolari
!corr       do i=1,nlegc
!corr          nb = number_of_bonds_between(legmc(i)%n1,legmc(i)%n2,conn,nconnbr,connbr)   !!!!!,nblimit)
!corr              !write(0,*)'nb=',nb,legmc(i)%n1,legmc(i)%n2
!corr              write(0,*)'nb=',legmc(i)%n1,legmc(i)%n2,nb,path_length(legmc(i)%n1,legmc(i)%n2)
!corr          if (nb <= nblimit) then
!corr              !write(0,*)'legame rimosso'
!corr              legmc(i)%n1 = 0   ! marca il legame per poi eliminarlo
!corr          endif
!corr       enddo
!corr       call remove_bonds(legmc)
!corr   endif
!corr!
!corr   end subroutine check_intra_contacts
!corr
!-------------------------------------------------------------------------------------------------------------------

   subroutine grow_fragments(atom,atoms,legm,legms,cellt,spg,lhbondi,legmH,shbondi,legmsh,lsym,kexp,reset,usecov)
!
!  Grow fragments in presenza della simmetria
!
   USE connect_mod
   USE cgeom
   USE unit_cell
   USE atom_type_util
   USE fragmentmod
   USE spginfom
   USE arrayutil
   USE connect_mod
         use elements
   type(atom_type), dimension(:), allocatable, intent(in)              :: atom    ! unita' asimmetrica
   type(atom_type), dimension(:), allocatable, intent(inout)           :: atoms   ! atomi nella cella
   type(bond_type), dimension(:), allocatable, intent(in)              :: legm    ! legami nell'u.a.
   type(bond_type), dimension(:), allocatable, intent(inout)           :: legms   ! legami nella cella
   type(cell_type), intent(in)                                         :: cellt
   type(spaceg_type), intent(in)                                       :: spg
   logical, intent(in), optional                                       :: lhbondi,shbondi
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legmH,legmsh
   type(atom_type), dimension(:), allocatable                          :: atomc,atomco
   type(atom_type), dimension(:), allocatable                          :: atomcart,atomscart
   type(bond_type), dimension(:), allocatable                          :: legmc,legmf,legmHs,legmshs
   integer, intent(in), dimension(2)                                   :: kexp
   logical, intent(in), optional                                       :: reset
   logical, intent(in), optional                                       :: usecov
   logical                                                             :: usecovr
   logical, intent(in)                                                 :: lsym
   integer                                                             :: nat
   integer                                                             :: natsym
   integer                                                             :: i,j,k,nfi,nfj
   type(bond_type), dimension(:), allocatable                          :: legmau, legm_intra
   integer, dimension(3)                                               :: xtra
   integer                                                             :: k1,k2,k3
   integer                                                             :: numau
   integer, dimension(size(atom))                                      :: vrem
   integer                                                             :: nrem
   integer                                                             :: nleg
   type(op_type)                                                       :: opcode
   type(fragment_type), dimension(:), allocatable                      :: fragm
   integer                                                             :: nfrag,nf,natf
   logical                                                             :: lhbond,shbond
   integer                                                             :: nlegH,nlegsh
   real                                                                :: rad0
   real, dimension(:), allocatable                                     :: radfrag
   real, dimension(3)                                                  :: bar0,bar1,bar2
   integer                                                             :: nlegnew
   integer, dimension(:), allocatable                                  :: iordat
   integer                                                             :: nl
   integer                                                             :: natsym0
   type(container_type), dimension(:), allocatable                     :: connHs,connH
   integer, dimension(3)                                               :: diff
   integer                                                             :: katom
   logical                                                             :: reset_bond
   integer                                                             :: nummol
   integer, dimension(:), allocatable                                  :: vatmol
   logical, dimension(:), allocatable                                  :: inorg
   integer, dimension(:), allocatable                                  :: iordrem
   real, dimension(3,3)                                                :: gmat
   integer, dimension(2)                                               :: posi,posj
   real                                                                :: distb
!
   nat = numatoms(atom)
   if (nat == 0) then
       call clear_atoms(atoms)
       call clear_bonds(legms)
       if (present(legmH)) call clear_bonds(legmH)
       if (present(legmsh)) call clear_bonds(legmsh)
       return
   endif
   nleg = numbonds(legm)
   gmat = cellt%get_g()
   allocate(iordat(nat))
!
   if (kexp(1) == EXPAND_ATOM) then
       katom = kexp(2)
   else
       katom = 0
   endif
!
   if (present(reset)) then
       reset_bond = reset
   else
       reset_bond = .true.
   endif
   if (present(usecov)) then
       usecovr = usecov
   else
       usecovr = .false.
   endif
!
   if (present(lhbondi)) then
       lhbond = lhbondi
   else
       lhbond = .false.
   endif
   if (lhbond .and. reset_bond) call clear_bonds(legmH)
   if (present(shbondi)) then
       shbond = shbondi
   else
       shbond = .false.
   endif
   if (shbond .and. reset_bond) call clear_bonds(legmsh)
!
!  estrai frammenti da atom
   call get_fragments(atom,cellt,legm,nfrag,fragm)
!
!  search inorganic fragments
   allocate(inorg(nfrag))
   do nf=1,nfrag
      inorg(nf) = is_organic(atom(fragm(nf)%pos)) == 0
      !write(0,*)'frag n.',nf,inorg(nf)
   enddo
       !inorg(:) = .false.
!
   if (.not.lsym) call init_for_symm(atom,legm,atoms,legms)
!
!  Applica operatori rotazionali
   natsym = numatoms(atoms)
   natsym0 = natsym
   numau = 0
!
!  Calcola raggi dei frammenti
   call frac_to_cart_copy(atoms,atomscart,cellt%get_ortom())
   if (katom > 0) then
       call get_radius_molecule(atomscart(katom:katom),rad0,bar2)
   else
       call get_radius_molecule(atomscart,rad0,bar2)
   endif
   if (hbtopt%hpresent == 1) then                  ! ho richiesto che l'atomo sia legato a H
       call get_conn_hydrogens(connHs,atoms,natsym,legms)  ! estrai la connettivita' degli H
   else
       allocate(connHs(natsym))
       connHs(:)%nat = 1    ! e' come assumere che tutti gli atomi siano legati a H
   endif
!
   allocate(radfrag(nfrag))
   do nf=1,nfrag
      call frac_to_cart_copy(atom(fragm(nf)%pos),atomcart,cellt%get_ortom())
      call get_radius_molecule(atomcart,radfrag(nf))
   enddo
!
   if (katom > 0) then 
       bar0 = baricentro(atoms(katom:katom))
   else
       bar0 = baricentro(atoms)
   endif
   nummol = 0
   allocate(vatmol(0:nfrag*spg%nsymop*27))
   vatmol(nummol) = natsym0
   do nf=1,nfrag   ! loop su tutti i frammenti
      call get_legm_from_fragment(fragm(nf),legm,legmf)
      call copy_bonds(legmc,legmf)
      natf = fragm(nf)%nat
      call new_atoms(atomco,fragm(nf)%nat)
      call new_container(connH,natf)
      if (hbtopt%hpresent == 1) then                  ! ho richiesto che l'atomo sia legato a H
          do i=1,natf
             connH(i) = connHs(fragm(nf)%pos(i))
          enddo
      else
          connH(:)%nat = 1    ! e' come assumere che tutti gli atomi siano legati a H
      endif
      do i=1,spg%nsymop  
         atomco(:) = atom(fragm(nf)%pos)   
         call apply_sym_oper(atomco,spg%symop(i))
         diff(:) = nint(bar0(:) - baricentro(atomco))   ! trasla di diff per avvicinare
         do k1=-1,1
            do k2=-1,1
               do k3=-1,1
                  xtra = (/k1,k2,k3/) + diff
                  opcode = op_type(i,xtra)
                  if (opcode == op_type()) cycle   ! non applicare l'identita'
                  if (katom == 0) then
!
!                     Check in the original set of atoms if the operator opcode has been already applied.
                      if (any_op_equal(atoms(:natsym0),opcode,fragm(nf)%pos)) cycle    !!!!!!!!! (*) PROBLEMA SE katom e' un hang atom
                  endif
                  call copy_atoms(atomc,atomco)
                  call translate_atoms(atomc,real(xtra))
!
                  call frac_to_cart_copy(atomc,atomcart,cellt%get_ortom()) ! converti in cartesiane
                  bar1 = baricentro(atomcart)
                  if (distanzaC(bar1,bar2) > radfrag(nf) + rad0 + 4) cycle
!
                  if (katom > 0) then
!
!                     expansion required only on katom
                      if (inorg(nf)) then
!
!                         find bonds only with katom
                          call connect_groups(atomcart(:),atomscart(katom:katom),legmau,usecovr)
                          if(numbonds(legmau) == 0) cycle
                          legmau%n2 = katom
                      else
!
!                         find all bonds with molecule but exit from loop if katom is not connected
                          call connect_groups(atomcart(:),atomscart,legmau,usecovr)
                          !if (.not.is_connected(legmau,katom)) cycle
                          if (numbonds(legmau) > 0) then
                              if (all(legmau%n2 /= katom)) cycle
                          endif
                      endif
                  else
                      call connect_groups(atomcart(:),atomscart,legmau,usecovr)
                  endif
                 !!! call connect_groups(atomcart(:),atomscart,legmau,usecovr)
                 !!! if (katom > 0) then
                 !!!     if (.not.is_connected(legmau,katom)) cycle
                 !!! endif
                  nlegH = 0
                  nlegsh = 0
                  nlegnew = numbonds(legmau)
                  if (lhbond .and. shbond) then
                      call find_inter_hydrogen_bonds(atomcart,atomscart,connH,connHs,legmHs,lhbond,legmshs,shbond)
                      nlegH = numbonds(legmHs)
                      nlegsh = numbonds(legmshs)
                  else
                      if (lhbond) then
                          call find_inter_hydrogen_bonds(atomcart,atomscart,connH,connHs,legmHs,lhbond)
                          nlegH = numbonds(legmHs)
                      endif
                      if (shbond) then
                          call find_inter_hydrogen_bonds(atomcart,atomscart,connH,connHs,legmsh=legmshs,shbond=shbond)
                          nlegsh = numbonds(legmshs)
                      endif
                  endif
                  if (nlegnew > 0 .or. nlegH > 0 .or. nlegsh > 0) then
!
!                     set code before check_new_au
                      do j=1,natf
                         atomc(j)%asym = fragm(nf)%pos(j)
                         atomc(j)%op = opcode
                      enddo
!
                      if (inorg(nf)) then    
                          call copy_bonds(legmc,legmf)  !!! test
!
!                         for inorganic fragment keep only atoms directly connected
                          if (nlegnew > 0) then
!
!                             find only bonds between connected atoms in legmau%n1
!                             this part is important to include intramolecular bonds
                              call connect_groups(atomcart(legmau%n1),atomscart(:),legm_intra,usecovr)
                              legm_intra%n1 = legmau(legm_intra%n1)%n1
                              call copy_bonds(legmau,legm_intra)
                              nlegnew = numbonds(legmau)
!
!                             now remove not connected atoms = atoms not included in legmau%n1 
                              call remove_atoms_vet(atomc,legmc,legmau%n1,keep=.true.,iord=iordrem)
                              legmau%n1 = iordrem(legmau%n1)
                          endif
                          call check_new_au()
                          if (nrem == numatoms(atomc)) cycle
                      else
                          call check_new_au()
                          if (nrem == numatoms(atomc)) cycle
                          call copy_bonds(legmc,legmf)
                      endif
                      !natc = size(atomc)
!
                      !call check_new_au()
               !test       if (nrem == natf) cycle
                      !if (nrem == numatoms(atomc)) cycle
               !test       call copy_bonds(legmc,legmf)
                      if (nrem > 0) then                                                                 
                          !test call remove_atoms_from_list(atomc,vrem(:natf),0,legm=legmc,iord=iordat)
                          call remove_atoms_from_list(atomc,vrem(:size(atomc)),0,legm=legmc,iord=iordat)
                          do nl=1,nlegnew
                             if (vrem(legmau(nl)%n1) /= 0) then
                                 legmau(nl)%n1 = iordat(legmau(nl)%n1) + natsym
                             else
                                 legmau(nl)%n1 = 0
                             endif
                          enddo
                          call remove_bonds(legmau)
                          if (numbonds(legmau) == 0) cycle
                      else
                          do nl=1,nlegnew
                             legmau(nl)%n1 = legmau(nl)%n1 + natsym
                          enddo
                          if (nlegH > 0) then
                              do nl=1,nlegH
                                 legmHs(nl)%n1 = legmHs(nl)%n1 + natsym
                              enddo
                              call combine_legm(legmH,legmHs)
                          endif
                          if (nlegsh > 0) then
                              do nl=1,nlegsh
                                 legmshs(nl)%n1 = legmshs(nl)%n1 + natsym
                              enddo
                              call combine_legm(legmsh,legmshs)
                          endif
                      endif
!     
                      call add_atoms_to_list(atoms,atomc,natsym,legm1=legms,legm2=legmc)
                      call combine_legm(legms,legmau)
                      nummol = nummol + 1
                      vatmol(nummol) = natsym
                      !if (nummol == 1) go to 100
!     
                  endif
               enddo
            enddo
         enddo
      enddo   
   enddo
!100 continue
   if (nummol  > 0) then
!
!      Cerca legami tra le u.a. aggiunte
       call frac_to_cart_copy(atoms,atomscart,cellt%get_ortom()) ! converti in cartesiane
       do i=1,nummol-1
          posi(1) = vatmol(i-1) + 1
          posi(2) = vatmol(i)
          nfi = fragment_pos(fragm, atomscart(posi(1))%asym)
          !write(0,*)posi(1),'NFI=',nfi,atomscart(posi(1))%rcod(1)
          do j=i+1,nummol
             posj(1) = vatmol(j-1) + 1
             posj(2) = vatmol(j)
             bar1 = baricentro(atomscart(posi(1) : posi(2)))
             bar2 = baricentro(atomscart(posj(1) : posj(2)))
             nfj = fragment_pos(fragm, atomscart(posj(2))%asym)
             !write(0,*)'dist=',distanzaC(bar1,bar2),radfrag(nfi),radfrag(nfj)
             distb = distanzaC(bar1,bar2)
             if (distb < 1.0) cycle ! avoid unreasonable bonds in case of disordered structure (e.g. 4107507.cif)
             if (distanzaC(bar1,bar2) > radfrag(nfi) + radfrag(nfj) + 4) cycle 
             call connect_groups(atomscart(posi(1):posi(2)),atomscart(posj(1):posj(2)),legmau,usecovr)
             do k=1,numbonds(legmau)
                legmau(k)%n1 = legmau(k)%n1 + posi(1) - 1
                legmau(k)%n2 = legmau(k)%n2 + posj(1) - 1
             enddo
             call combine_legm(legms,legmau)
          enddo
       enddo
!
   endif
!
!  clear rcod
   !!!FIXME - update b aniso

   !call cpu_time(end_time)
   !write(0,*)'time=',end_time-init_time
!
   CONTAINS

   subroutine check_new_au()
!
!  Controlla che la nuova unita' asimmetrica non contenga atomi gia' considerati
!
   real               :: djk
   integer            :: j1,ks
   real, dimension(3) :: dx
   real, parameter    :: D2MIN = 0.6*0.6  ! square of minimum distance
!
   nrem = 0
   loop_atom: do j1=1,size(atomc)
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
   end subroutine check_new_au
   
   end subroutine grow_fragments

!-------------------------------------------------------------------------------      

   subroutine get_conn_hydrogens(connH,atom,nat,legm)
!
!  Metti in connH le connettivita ma solo degli atomi H
!  
   USE connect_mod
   USE atom_type_util
   USE elements
   USE arrayutil
   type(container_type), dimension(:), allocatable, intent(out) :: connH ! connettivita degli H
   type(atom_type), dimension(:), intent(in)                     :: atom  ! gli atomi
   type(bond_type), dimension(:), allocatable, intent(in)        :: legm  ! i legami
   integer, intent(in)                                           :: nat
   integer                                                       :: nleg
   integer, dimension(:), allocatable                            :: vleg
   integer, dimension(size(atom))                                :: zval
   integer                                                       :: i,j
   integer                                                       :: numH
   integer                                                       :: numleg
!
   !nat = numatoms(atom)
   nleg = numbonds(legm)
   call new_container(connH,nat)
   if (nleg > 0) then
       zval(:) = atom%z()
        !write(0,*)size(atom),nat,'Z=',zval(:)
       do i=1,nat
          call get_connected_atoms(legm,i,vleg,numleg)
          if (numleg > 0) then
              numH = 0
              !write(0,*)'VLEG=',vleg(:numleg)
!
!             Conta in numH quanti H sono legati all'atom i
              do j=1,numleg
                 if (zval(vleg(j)) == H_at) then
                     numH = numH+1
                     vleg(j) = -vleg(j)  ! marca l'atomo H
                 endif
              enddo
!
!             Ora puoi allocare e riemire connH(i)
              if (numH > 0) then
                  connH(i)%nat = numH
                  allocate(connH(i)%pos(numH))
                  numH = 0
                  do j=1,numleg
                     if (vleg(j) < 0) then
                         numH = numH+1
                         connH(i)%pos(numH) = -vleg(j)
                     endif
                  enddo
              else
                  connH(i)%nat = 0
              endif
          else
              connH(i)%nat = 0
          endif
       enddo
   else
       connH(:)%nat = 0
   endif
!
   end subroutine get_conn_hydrogens

!-------------------------------------------------------------------------------      

   subroutine find_Hacceptor(atom,bond,kat,cell,spg,legmH,atH,is_distmat)  !,conn,connbr,nconnbr)
!
!  Cerca accetori di H per l'atomo kat
!
   USE connect_mod
   USE atom_type_util
   USE unit_cell
   USE spginfom
   USE arrayutil
   USE nr
   USE molgraph
   type(atom_type), dimension(:), intent(in)        :: atom   ! tutti gli atomi
   type(bond_type), dimension(:), allocatable, intent(in) :: bond   ! legami
   integer, intent(in)                              :: kat
   type(cell_type), intent(in)                      :: cell
   type(spaceg_type), intent(in)                    :: spg
   type(bond_type), dimension(:), allocatable, intent(out) :: legmH      
   type(atom_type), dimension(:), allocatable, intent(out) :: atH       
   logical, intent(inout) :: is_distmat
!corr   type(container_type), dimension(:), allocatable  :: conn, connbr
!corr   integer, intent(in)                              :: nconnbr
   integer, dimension(size(atom))                   :: zval
   integer                                          :: i,k
   type(atom_type), dimension(1)                    :: atom2
   integer, dimension(3)                            :: ktra
   integer                                          :: natsym
   type(container_type), dimension(:), allocatable  :: connH
   type(bond_type)                                  :: legH
   integer                                          :: k1,k2,k3
   type(atom_type)                                  :: atomc2,atomsav,atomcart
   type(op_type)                                    :: opcode
   integer, dimension(3)                            :: diff
   integer, parameter                               :: nminH = 3
   integer                                          :: nb,nbasym
   integer                                          :: MAXHBOND = 4
   integer, dimension(:), allocatable               :: iord
   integer                                          :: iasym,natsym0
   type(atom_type), dimension(:), allocatable :: atasym
   logical :: ladd
   logical :: check_intra, okcheck
!
   natsym = size(atom)
   natsym0 = 0
   call resize_atoms(atasym,5)
   call set_donor_acceptor()
   zval(:) = atom%z()
   allocate(connH(natsym))
   connH(:)%nat = 1 
   call new_bonds(legmH,MAXHBOND)
   call new_atoms(atH,MAXHBOND)
   atomcart = cartesian_coord(atom(kat),cell%get_ortom())
   if (.not.is_distmat) then
       call compute_dist_matrix(atom,bond,path=.true.)
       is_distmat = .true.
   endif
!
   nbasym = 0
   loop_sym: do i=1,natsym
      if (vacc(zval(i)) == 1) then  ! l'atomo i e' un accettore?
          do k=1,spg%nsymop
             atom2(1) = atom(i)
             call apply_sym_oper(atom2(1:1),spg%symop(k))
             diff(:) = nint(atom(kat)%xc - atom2(1)%xc)   ! trasla di diff per avvicinare dopo la rotazione
             do k1=-1,1
                do k2=-1,1
                   do k3=-1,1
                      ktra = (/k1,k2,k3/) + diff(:)
                      opcode = op_type(k,ktra)
!
                      atomsav = atom2(1)
                      atomsav%xc = atomsav%xc + ktra

                      atomc2 = cartesian_coord(atomsav,cell%get_ortom())
!
!                     if next condition is verified the 2 atoms are in the same asymmetric unit
                      check_intra = .false.
                      if (checkeq_symm_new(atom,atomsav%xc,i,op_type(k,ktra),cell%get_g()) > 0) then
                          check_intra = .true.   ! possible intramolecular contact
                      endif
                      call check_hbond(kat,i,atomcart%xc,atomc2%xc,zval(i),zval(kat),connH(kat),connH(i),legH)
                      ladd = .false.

                      if (legH%n1 > 0) then
!
!                         Check for duplicate atom
                          if (natsym0 > 0) then
                              iasym = checkeq_symm_new(atasym(:natsym0),atomsav%xc,i,op_type(k,ktra),cell%get_g())
                              if (iasym > 0) cycle
                          endif

                          okcheck = .true.
                          if (check_intra) then
                              nb = path_length(i,kat)   ! if nb=0 i and kat are in separeted molecules
                              ! here check for i>j to avoid duplicate intra bonds
                              okcheck = ((nb > hbtopt%nminbond) .or. nb == 0) !.and. (i < kat)
                          endif
                          if (okcheck) then
                              nbasym = nbasym + 1
                              !if (legH%dist < legmH%dist) then  
                              legmH(nbasym) = legH
                              ladd = .true.
                              natsym0 = natsym0 + 1
                              atH(nbasym) = atomc2
                              atasym(natsym0) = atomsav
                              atasym(natsym0)%asym = i
                              atasym(natsym0)%op = op_type(k,ktra)
                              if (nbasym == MAXHBOND) exit loop_sym
                          endif
                      endif
                   enddo
                enddo
             enddo
          enddo
      endif
   enddo loop_sym
   call resize_bonds(legmH,nbasym)
   call resize_atoms(atH,nbasym)
   if (nbasym > 1) then
       allocate(iord(nbasym))
       call indexx(legmH%dist,iord)
       legmH(:) = legmH(iord)
       atH(:) = atH(iord)
   endif
!
   end subroutine find_Hacceptor

!-------------------------------------------------------------------------------      

   integer function is_hang_atom(kat,legm)  result(hang)
!
!  Check if kat is an hanging atom
!
   USE connect_mod
   integer, intent(in)                        :: kat
   type(bond_type), dimension(:), allocatable :: legm
   integer                                    :: i
   hang = 0
   do i=1,numbonds(legm)
      !if (legm(i)%n2 == -kat) then
      if (legm(i)%n2 == kat .and. legm(i)%ord == HANGCODE) then
          hang = i   
          exit
      endif
   enddo
   end function is_hang_atom

!-------------------------------------------------------------------------------      

   subroutine check_hang_atom(legc, legm)
!
!  Controlla gli hang atoms
!
   USE connect_mod
   type(bond_type), dimension(:), allocatable, intent(inout) :: legc
   type(bond_type), dimension(:), allocatable, intent(in   ) :: legm
   integer                                                   :: i
!
   do i=1,numbonds(legc)
!     verifica che gli hang atoms non sia legati
      if (legc(i)%ord == HANGCODE .and. ( is_connected(legm,legc(i)%n2) )) then
          legc(i)%ord = 0
      endif
   enddo
!
   end subroutine check_hang_atom

!-------------------------------------------------------------------------------      

   subroutine analyze_contacts(atom,bond,cell,spg,cutoff,nc,kpr)
   USE atom_basic
   USE connect_mod
   USE unit_cell
   USE spginfom
   USE strutil
   type(atom_type), dimension(:), allocatable, intent(in) :: atom 
   type(bond_type), dimension(:), allocatable, intent(in) :: bond
   type(cell_type), intent(in)                            :: cell
   type(spaceg_type), intent(in)                          :: spg
   real, intent(in)                                       :: cutoff
   integer, dimension(3), intent(out)                     :: nc
   integer, intent(in)                                    :: kpr
   type(bond_type), dimension(:), allocatable             :: bondH 
   type(bond_type), dimension(:), allocatable             :: bondsh
   type(atom_type), dimension(:), allocatable             :: atoms !, atomsg
   type(bond_type), dimension(:), allocatable             :: bonds !, bondsg
   real                                                   :: cutoff_old, over
   integer :: n1, n2, i
!
   cutoff_old = hbtopt%cutoff 
   hbtopt%cutoff = cutoff
   !call grow_fragments(atom,atoms,bond,bonds,cell,spg,lsym=.false.,kexp=(/EXPAND_ALL,0/))
   !call find_short_contacts_sym (atom,atoms,bond,bonds,bondsh,bondH,  &
   !     hbond=.true.,lsym=.true.,cell=cell,spg=spg,nodupl=.true.)
   call find_short_contacts_sym_new(atom,atoms,bond,bonds,bondsh,bondH,hbond=.true.,shbond=.true.,lsym=.false.,  &
                                          cell=cell,spg=spg,nodupl=.true.)
   hbtopt%cutoff = cutoff_old
   nc(:) = 0
   if (numbonds(bondsh) > 0) then
       do i=1,numbonds(bondsh)
          over = bondsh(i)%sigma
          if (over >= 0.4) then
              n1 = bondsh(i)%n1
              n2 = bondsh(i)%n2
              nc(1) = nc(1) + 1
              if (nc(1) == 1) then
                  write(kpr,'(a,a)',advance='no')  &
                  'Clash:          ',atoms(n1)%glab()//"-"//atoms(n2)%glab()//"("//r_to_s(over,3)//")"
              else
                  write(kpr,'(a)',advance='no')', '//atoms(n1)%glab()//"-"//atoms(n2)%glab()//"("//r_to_s(over,3)//")"
              endif
          endif
       enddo
       if (nc(1) > 0) write(kpr,'(a)',advance='yes')
       do i=1,numbonds(bondsh)
          over = bondsh(i)%sigma
          if (over >= 0.2 .and. over < 0.4) then
              n1 = bondsh(i)%n1
              n2 = bondsh(i)%n2
              nc(2) = nc(2) + 1
              if (nc(2) == 1) then
                  write(kpr,'(a,a)',advance='no')  &
                  'Short Contacts: ',atoms(n1)%glab()//"-"//atoms(n2)%glab()//"("//r_to_s(over,3)//")"
              else
                  write(kpr,'(a)',advance='no')', '//atoms(n1)%glab()//"-"//atoms(n2)%glab()//"("//r_to_s(over,3)//")"
              endif
          endif
       enddo
       if (nc(2) > 0) write(kpr,'(a)',advance='yes')
       do i=1,numbonds(bondsh)
          over = bondsh(i)%sigma
          if (over < 0.2) then
              n1 = bondsh(i)%n1
              n2 = bondsh(i)%n2
              nc(3) = nc(3) + 1
              if (nc(3) == 1) then
                  write(kpr,'(a,a)',advance='no')  &
                  'Contacts:       ',atoms(n1)%glab()//"-"//atoms(n2)%glab()//"("//r_to_s(over,3)//")"
              else
                  write(kpr,'(a)',advance='no')', '//atoms(n1)%glab()//"-"//atoms(n2)%glab()//"("//r_to_s(over,3)//")"
              endif
          endif
       enddo
       if (nc(3) > 0) write(kpr,'(a)',advance='yes')
       !if (kpr > 0) call print_connect(atoms%lab,bond=bondsh,kpri=kpr)
   endif
!
!
   end subroutine analyze_contacts

!---------------------------------------------------------------------------------------      

   subroutine find_short_contacts_sym_new(atom,atoms,legm,legms,legmsh,legmH,hbond,shbond,lsym,  &
                                          cell,spg,nodupl,section)
!
!  Find short contacts in atom. 
!  Atoms will contain a.u. and the result of the application of symmetry operators
!  nodupl = true if you want to display symmetrical contacts: A..B and B..A. Default choice.
!         = false only A..B, this choice is suggested if you want generate a simple list of contacs
!
   USE atom_type_util
   USE connect_mod
   USE unit_cell
   USE spginfom
   USE fragmentmod
   USE arrayutil
   USE molgraph
   type(atom_type), dimension(:), allocatable, intent(in)        :: atom   ! atomi nell'u.a.
   type(atom_type), dimension(:), allocatable, intent(inout)     :: atoms  ! atomi nella cella
   type(bond_type), dimension(:), allocatable, intent(in)        :: legm   ! legami 
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legms  ! legami nella cella
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legmH,legmsh
   logical                                                       :: hbond,shbond
   logical, intent(in)                                           :: lsym
   type(cell_type), intent(in)                                   :: cell
   type(spaceg_type), intent(in)                                 :: spg
   logical, intent(in), optional                                 :: nodupl
!corr   integer, optional, dimension(:), intent(in)                   :: usevet
!corr   logical, intent(in), optional                                 :: checkhang
   integer, intent(in), optional                                 :: section
   type(atom_type), dimension(:), allocatable                    :: atomcart
   integer                                                       :: natsym
   integer, dimension(:), allocatable                            :: zval
   integer                                                       :: i,j,k
   integer                                                       :: nbondH,nmaxbondH
   integer                                                       :: nbondsh,nmaxbondsh
   type(bond_type)                                               :: leg
   integer                                                       :: nat
   type(atom_type), dimension(1)                                 :: atom2
   type(atom_type)                                               :: atomc2,atomsav
   integer, dimension(3)                                         :: ktra
   integer                                                       :: k1,k2,k3
   integer                                                       :: natsym0
   logical                                                       :: ladd
   logical                                                       :: jump
   type(container_type), dimension(:), allocatable               :: connH
   integer, dimension(3)                                         :: diff
   integer                                                       :: ier
!corr   integer, dimension(:), allocatable                            :: useat
!corr   logical                                                       :: use_all
   integer                                                       :: nmaxatoms
   logical                                                       :: saves
!corr   type(fragment_type), dimension(:), allocatable                :: frag
   integer                                                       :: iasym,jpos
!corr   integer, dimension(size(atom))                                :: vfrag
!corr   type(bond_type), dimension(:), allocatable                    :: legmshi,legmshii
!corr   integer                                                       :: nbondshi,nbondshimax
   !type(atom_type)                                               :: atomi,atomj
   logical                                                       :: nodupli
     integer :: nb,n1,n2
   type(atom_type), dimension(:), allocatable :: atomsg 
   type(bond_type), dimension(:), allocatable :: bondsg 
   type(bond_type), dimension(:), allocatable :: basym, basymH
   type(atom_type), dimension(:), allocatable :: atasym
   type(atom_type) :: atop
   type(op_type) :: opt
   integer :: start_atom
   integer :: nbasym, nbasymH
   logical :: check_intra, okcheck
   logical :: intrahb, intrash !, is_path
   !logical :: bfound
!
   ier = 0
   if (.not.lsym) call init_for_symm(atom,legm,atoms,legms)
   natsym = numatoms(atoms)
   nat = numatoms(atom)
   if (nat == 0 .or. (lsym .and. natsym == 0)) then
       call clear_bonds(legmH)
       call clear_bonds(legmsh)
       return
   endif
!
   if (present(nodupl)) then
       nodupli = nodupl
   else
       nodupli = .false.
   endif
!
!corr   if (present(checkhang)) then
!corr       if (checkhang) then
!corr           do i=1,numbonds(legmsh)
!corr              if (legmsh(i)%ord == HANGCODE) atoms(legmsh(i)%n2)%rcod(1) = HANGCODE
!corr           enddo
!corr           do i=1,numatoms(atoms)
!corr              if (atoms(i)%rcod(1) == HANGCODE) write(0,*)atoms(i)%glab()//' is hang atom'
!corr           enddo
!corr       endif
!corr   endif

   start_atom = 1
   if (present(section)) then
       if (section > 0) start_atom = section
   endif
!!
!!  Make array vfrag containing info about fragment
!   call frac_to_cart_copy(atoms,atomcart,cell%get_ortom())
!   call get_fragments(atom,cell,legm,nfrag,frag)
!   do i=1,nat
!      vfrag(i) = fragment_pos(frag,i)
!   enddo
!!
!   if (present(usevet)) then
!       allocate(useat(size(usevet)))
!       useat = usevet
!       use_all = .false.
!   else
!       allocate(useat(1))
!       useat(1) = 0
!       use_all = .true.
!   endif
!
   allocate(zval(natsym))
   zval(:) = atoms%z()
   call set_donor_acceptor() 
!
   !if (use_all) then  ! ricalcola tutti gli short contacts
   if (hbond) then
       nmaxbondH = 4*natsym
   endif
   if (start_atom == 1) then
       nbondH = 0
       nbondsh = 0
       call new_bonds(legmH,nmaxbondH)
       !saves = .false.
   else               ! calcola contacts solo per gli atomi in usevet
       nbondH = numbonds(legmH)
       nbondsh = numbonds(legmsh)
       call resize_bonds(legmH,nmaxbondH)
       !saves = .true.
   endif
   !if (hbond) then
   !    nmaxbondH = 4*natsym
   !    call resize_bonds(legmH,nmaxbondH,saves)
   !endif
!
   if (hbond) then
       if (hbtopt%hpresent == 1) then                  ! ho richiesto che l'atomo sia legato a H
           call get_conn_hydrogens(connH,atoms,natsym,legms)  ! estrai la connettivita' degli H
       else
           allocate(connH(natsym))
           connH(:)%nat = 1    ! e' come assumere che tutti gli atomi siano legati a H
       endif
   endif
!
   intrahb = hbond .and. hbtopt%intra == 1
   intrash = shbond .and. hbtopt%intrash == 1
        intrash = .false.
!
!  Ora cerca altri legami H con atomi non inclusi in atoms
   nbasym = 0
   nbasymH = 0
   nmaxbondsh = 4*numatoms(atom)
   nmaxbondh = nmaxbondsh
   if (shbond) then
       call resize_bonds(basym,nmaxbondsh,saves)
   endif
   if (hbond) then
       call resize_bonds(basymH,nmaxbondh,saves)
   endif

   call grow_fragments(atom,atomsg,legm,bondsg,cell,spg,lsym=.false.,kexp=(/EXPAND_ALL,0/))
   call frac_to_cart_copy(atom,atomcart,cell%get_ortom())
   call compute_dist_matrix(atomsg,bondsg,path=.true.)

   natsym0 = nat
   nmaxatoms = natsym0
   if (shbond) nmaxatoms = natsym0+nmaxbondsh
   if (hbond) nmaxatoms = nmaxatoms + nmaxbondh
   call resize_atoms(atasym,nmaxatoms)
   atasym(:nat) = atom(:)  ! necessary only if search for intramolecular
   atasym(:nat)%asym = [(i,i=1,nat)]
   !ladd = .false.
   jpos = 1
         !write(70,*)'NAT:',size(atom),size(atomsg),natsym,use_all
         !call print_atoms(atom,title='asym unit',kpr=70)
         !call print_atoms(atomsg,title='grow fragment',kpr=70)
   loop_atom: do j=1,nat
      if (nodupli) jpos = j
      do k=1,spg%nsymop
         atom2(1) = atom(j)
         call apply_sym_oper(atom2(1:1),spg%symop(k))
         !do i=jpos,natsym
         do i=jpos,nat
            !if (use_all .or. any(useat(:) == i)) then         ! i contenuto in useat?
                !diff(:) = nint(atoms(i)%xc - atom2(1)%xc)
                diff(:) = nint(atom(i)%xc - atom2(1)%xc)
                do k1=-1,1
                   do k2=-1,1
                      do k3=-1,1
                         ktra = (/k1,k2,k3/) + diff(:)
!
                         atomsav = atom2(1)
                         atomsav%xc = atomsav%xc + ktra
!
!                        Check for duplicate atom and duplicate bond
                         iasym = 0
                         if (natsym0 > nat) then
                             !iasym = checkeq_symm(atasym(nat+1:natsym0),j,op_type(k,ktra))
                             iasym = checkeq_symm_new(atasym(nat+1:natsym0),atomsav%xc,j,op_type(k,ktra),cell%get_g())
                             if (iasym > 0) then
                                 if (hbond .and. nbasymH > 0) then
                                     !if (nbasymH > 0) write(0,*)'BONDH:',bond_position(basymH,i,iasym+nat)
                                     if (bond_position(basymH,i,iasym+nat) > 0) cycle
                                 endif
                                 if (shbond .and. nbasym > 0) then
                                     !if (nbasym > 0) write(0,*)'BONDS:',bond_position(basym,i,iasym+nat)
                                     if (bond_position(basym,i,iasym+nat) > 0) cycle
                                 endif
                             endif
                         endif
!
                         atomc2 = cartesian_coord(atomsav,cell%get_ortom())
!
!                        if next condition is verified the 2 atoms are in the same asymmetric unit
                         check_intra = .false.
                         !if (checkeq_symm_new(atomsg,j,op_type(k,ktra)) > 0) then
                         if (checkeq_symm_new(atomsg,atomsav%xc,j,op_type(k,ktra),cell%get_g()) > 0) then
                             !if (.not.intrahb .and. .not.intrash) cycle
                             check_intra = .true.   ! possible intramolecular contact
                         endif
                         jump = .false.
                         ladd = .false.
                         !is_path = .false.
                         if (hbond) then
                             call check_hbond(i,natsym0+1,atomcart(i)%xc,atomc2%xc,zval(i),zval(j),connH(i),connH(j),leg)
                             if (leg%n1 > 0) then
                                 if (nbasymH == nmaxbondH) exit loop_atom ! too short contacts and bad structure!
                                 okcheck = .true.
                                 if (check_intra) then
                                     nb = path_length(i,j)   ! if nb=0 i and j are in separeted molecules
                                     ! here check for i>j to avoid duplicate intra bonds
                                     okcheck = ((nb > hbtopt%nminbond .and. intrahb) .or. nb == 0) .and. (i < j)
                                 endif
                                 if (okcheck) then
                                     nbasymH = nbasymH + 1
                                     !!!!FIX HANGCODE
                                     leg%ord = HANGCODE
                                     basymH(nbasymH) = leg
                                     ladd = .true.
                                     jump = .true.
                                     if (iasym > 0) then
                                         ladd = .false.
                                         basymH(nbasymH)%n2 = iasym+nat
                                     endif
                                 endif
                             endif
                         endif
                         if (.not.jump .and. shbond) then
                             !if (check_intra .and. .not.intrash) cycle  
                             !write(70,'(a,i5,i5,1x,a,4i5)')'CHECK:',i,natsym0+1,atomcart(i)%glab()//'-'//atomc2%glab(),k,ktra
                             call check_shortc(i,natsym0+1,atomcart(i)%xc,atomc2%xc,zval(i),zval(j),leg)
                             if (leg%n1 > 0) then
                                 if (nbasym == nmaxbondsh) exit loop_atom ! too short contacts and bad structure!
                                 okcheck = .true.
                                 if (check_intra) then
                                     nb = path_length(i,j)   ! if nb=0 i and j are in separeted molecules
                                     okcheck = ((nb > hbtopt%nminbond .and. intrash) .or. nb == 0) .and. (i < j)
                                 endif
                                 if (okcheck) then
                                     leg%ord = HANGCODE ! FIX: must be 0 for intra bonds of a.u.
                                     ladd = .true.
                                     nbasym = nbasym + 1
                                     basym(nbasym) = leg
!                                    
!                                    Check for atom already added. Possibile when an hanging atom has more then one contacts
                                     if (natsym0 > nat) then
                                         !iasym = checkeq_symm(atasym(nat+1:natsym0),j,op_type(k,ktra))
                                         !iasym = checkeq_symm_new(atasym(nat+1:natsym0),atomsav%xc,j,op_type(k,ktra),cell%get_g())
                                         if (iasym > 0) then
                                            ladd=.false.
                                            !basym(nbasym)%ord = 0
                                            basym(nbasym)%n2 = iasym+nat
                                         endif
                                     endif
                                 endif
                             endif
                         endif
                         if (ladd) then
                             natsym0 = natsym0 + 1
                             atasym(natsym0) = atomsav
                             atasym(natsym0)%asym = j 
                             atasym(natsym0)%op = op_type(k,ktra)
                             ladd = .false.
                             if (natsym0 == nmaxatoms) exit loop_atom ! too atoms and bad structure!
                         endif
                 !if (leg%n1 > 0 .and. okcheck) then
                 !    write(0,*)'HANG: ',nbasymH,basymH(nbasymH)%ord,basymH(nbasymH)%n2
                 !endif
           !if (leg%n1 > 0 .and. okcheck) write(70,*)'Bond: ',nbasym,atasym(basym(nbasym)%n1)%glab()   &
           !                //'-'//atasym(basym(nbasym)%n2)%glab()
                      enddo
                   enddo
                enddo
            !endif
         enddo
      enddo
   enddo loop_atom
!
   call free_dist_matrix()
   if (nbasym == 0 .and. nbasymH == 0) return

   call resize_bonds(basym,nbasym)
   if (hbond) call resize_bonds(basymH,nbasymH)
   call resize_atoms(atasym,natsym0)

   if (lsym) then
       nmaxatoms = natsym
       if (shbond) then
           nmaxbondsh = 4*natsym
           call resize_bonds(legmsh,nmaxbondsh,saves)
           nmaxatoms = nmaxatoms+nmaxbondsh
       endif
       if (hbond) then
           nmaxbondh = 4*natsym
           call resize_bonds(legmH,nmaxbondH,saves)
           nmaxatoms = nmaxatoms + nmaxbondh
       endif
       call resize_atoms(atoms,nmaxatoms)
       
       natsym0 = natsym
       if (hbond) then
           do i=1,numbonds(basymH)
              n1 = basymH(i)%n1
              n2 = basymH(i)%n2
              do j=start_atom,natsym
                 if (atoms(j)%asym == atasym(n1)%asym) then
                     atop = atasym(n2)
                     call apply_sym_oper(atop,spg%symop(atoms(j)%op%op))  ! TODO: write new function
                     call translate_atoms(atop,real(atoms(j)%op%tra))
                     call find_oper(atasym(n2)%op,atoms(j)%op,opt,spg)
                     nbondh = nbondh + 1
                     legmH(nbondh) = basymH(i)
                     legmH(nbondh)%n1 = j
                     legmH(nbondh)%ord = HANGCODE
                     iasym = checkeq_symm(atoms(:natsym0),atasym(n2)%asym,opt)
                     if (iasym > 0) then
                         if (iasym <= natsym) legmH(nbondh)%ord = 0
                         legmH(nbondh)%n2 = iasym
                     else
                         natsym0 = natsym0+1
                         legmH(nbondh)%n2 = natsym0
                         atoms(natsym0) = atop
                         atoms(natsym0)%op = opt
                     endif
                 endif
              enddo
           enddo
           call resize_bonds(legmh,nbondh)
       endif

       if (shbond) then
           loop_atom2: do i=1,numbonds(basym)
              n1 = basym(i)%n1
              n2 = basym(i)%n2
              do j=start_atom,natsym
                 if (atoms(j)%asym == atasym(n1)%asym) then
                     if (nbondsh == nmaxbondsh) exit loop_atom2 ! too short contacts and bad structure!
                     atop = atasym(n2)
                     call apply_sym_oper(atop,spg%symop(atoms(j)%op%op))  ! TODO: write new function
                     call translate_atoms(atop,real(atoms(j)%op%tra))
                     call find_oper(atasym(n2)%op,atoms(j)%op,opt,spg)
                     nbondsh = nbondsh + 1
                     legmsh(nbondsh) = basym(i)
                     legmsh(nbondsh)%n1 = j
                     legmsh(nbondsh)%ord = HANGCODE
                     iasym = checkeq_symm(atoms(:natsym0),atasym(n2)%asym,opt)
                     if (iasym > 0) then
                         if (iasym <= natsym) legmsh(nbondsh)%ord = 0
                         legmsh(nbondsh)%n2 = iasym
                     else
                         natsym0 = natsym0+1
                         legmsh(nbondsh)%n2 = natsym0
                         atoms(natsym0) = atop
                         atoms(natsym0)%op = opt
!!!!!!t    est oper
                         !atop = atom(atasym(n2)%asym)
                         !write(70,*)'AT1:',atop%xc
                         !call apply_sym_oper(atop,spg%symop(opt%op))
                         !call translate_atoms(atop,real(opt%tra))
                         !write(70,*)'AT2:',atop%xc
                         !write(70,*)'ATO:',atoms(natsym0)%xc
!!!!!!t    est oper
                     endif
                 endif
              enddo
           enddo loop_atom2
           call resize_bonds(legmsh,nbondsh)
       endif
       call resize_atoms(atoms,natsym0)
   else
       if (shbond) then
           call move_alloc(basym,legmsh)
       endif
       if (hbond) then
           call move_alloc(basymH,legmH)
       endif
       call move_alloc(atasym,atoms)
   endif
!100 continue
!
!corr   call resize_bonds(legmH,nbondH)
!corr   call resize_bonds(legmsh,nbondsh)
!corr   call combine_legm(legmsh,legmshii)
!   call resize_atoms(atoms,natsym0,.true.)
!
   end subroutine find_short_contacts_sym_new

!---------------------------------------------------------------------------------------      

   subroutine find_oper(op1,op2,op3,spg)
   use atom_basic
   use spginfom
   type(op_type), intent(in)     :: op1,op2
   type(op_type), intent(out)    :: op3
   type(spaceg_type), intent(in) :: spg
   real, dimension(3) :: optra
   type(symop_type) :: symop
   !integer, dimension(3,3)          :: opmat
   !integer, parameter :: KPR=70
   integer :: i,kop
   !character(len=30) :: str
!
   !write(KPR,*)'========================================================'
   !write(KPR,*)'OP1=',op1
   !write(KPR,*)'OP2=',op2
   symop%rot = matmul(spg%symop(op1%op)%rot,spg%symop(op2%op)%rot)   
   optra = matmul(spg%symop(op2%op)%rot,spg%symop(op1%op)%trn) +  &
           matmul(spg%symop(op2%op)%rot,op1%tra) +  &
           spg%symop(op2%op)%trn + op2%tra
   !write(KPR,*)'TR tot symm:    ',optra
   symop%trn = mod(optra+10,1.0)
   !write(KPR,*)'TR tot symm mod:',symop%trn
   optra = optra - symop%trn !+ op1%tra + op2%tra
   !write(KPR,*)'TR finale:      ',symop%trn
   kop = 0
   do i=1,spg%nsymop
      if (symop_equal(spg%symop(i),symop)) then
          kop = i
          exit
      endif
   enddo
   
   if (kop > 0) then
       !write(KPR,*)'Trovato operatore:   ',kop,trim(spg%symopstr(kop))
       !write(KPR,*)'Trovata traslazione: ',optra
   else
       !write(KPR,*)'Operatore non trovato'
       kop = 1
   endif
   op3 = op_type(kop,nint(optra))
!
   end subroutine find_oper

END MODULE contacts
