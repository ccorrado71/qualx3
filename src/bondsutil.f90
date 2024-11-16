MODULE connect_mod
!
! Questo modulo contiene funzioni per gestire variabili di tipo:
! 1) bond_type
! 2) connect_type
! 3) angle_type
! 4) torsion_type
!
! +                                                          add integer to members n1 and n2
! S iconn_to_connect(natom,icon,connt)                       Subroutine prelimare che utilizza iconn per generare connect e numleg
! S iconn_to_legm(icon,legm,atom)                            Converti icon in legm
! S connect_to_leg(connt,atom,legm,numleg,lordi)             Riempie leg a partire da connect e numleg
! S connect_to_ang(connt,atom,angle,numang,lordi)            Riempie ang a partire da connect
! S bond_to_angle(legm,angle,angvali)                        Genera angoli da legm
! S bond_to_connect(natom,legm,conn)                         Converti legm in connect
! S get_connected_atoms(legm,nat,vat)                        Restituisce in vat gli atomi connessi con nat
! S get_connected_atoms_spec(legm,nat,zval,zspec,vat,nleg)   Restituisce in vat gli atomi connessi con nat e di specie spec
! S get_bonds_of_atom(legm,nat,vleg)                         Restituisce in vleg i legami formati dall'atomo nat
! S get_angles_of_bond(angle,n1,n2,vang,nang)                Get angles containing the bond n1-n2
! F is_in_angle(angle,n1,n2)                                 Check if angle contains the bond n1-n2
! F number_of_bonds(legm,nat) result(num)                    Calcola quanti legami forma l'atomo nat
! F is_connected(legm,nat)                                   Check on legm if nat is connected
! F numbonds(legm)                                           Restituisce il numero di legami
! F numangles(angle)                                         Restituisce il numero di angoli
! F bond_position(legm,n1,n2) result(pos)                    Restituisce la locazione del legame n1-n2 nel vettore legm
! S print_connect(code)                                      Stampa la connettivita' nelle sue varie forme
! S get_chain(pat,stopa,nat,veta)                            Trova la catena legata ad un atomo
! S remove_atoms_from_iconn(vat,icon)                        Aggiorna iconn rimuovendo gli atomi contenuti in vat
! S tabconn_to_iconn(tabconn,icon)                           Converti tabella di connettivita in iconn
! S tabconn_to_connect(tabconn,connt)                        Converti tabconn in connect
! S tabconn_to_legm(tabconn,atom,sd,legm)                    Converti tabconn in bond_type
! S iconn_to_tabconn(tabconn,icon)                           Converti iconn in tabconn
! S bond_to_iconn(legm,icon)                                 Converti legm in iconn
! S bond_to_tabconn(legm,tabconn)                            Converti legm in tabconn
! S resize_bonds(vetr,n,savevet)                             Riallocazione di bond_type
! S clear_bonds(vetr)                                        Delete all bonds
! S new_bonds(vetr,n)                                        Create new bonds
! S combine_legm(legm1,legm2,shifta,mergeb)                  Aggiungi legm2 a legm1, merge is possible
! S copy_bonds(legm1,legm2)                                  Copia legm2 in legm1
! F compare_legm(legm1,legm2)                                Controlla se ci sono differenze tra legm1 e legm2
! S remove_bond(legm,lpos)                                   Rimuovi il legame lpos dal vettore legm
! S remove_bonds(legm,vet)                                   Rimuovi i legami nel vettore vet
! S remove_bonds_sym(legms,legm,symm,vet)                    Rimuovi tutti i legami indicati nel vettore vet e gli equivalenti per simmetria
! S remove_bondsv(legm,vat,val)                              Rimuovi tutti i legami per i queli il vettore vat e' uguale a val
! S bond_delete_selected(bonds)                              Remove bonds selected as DELETE_BOND
! S remove_bond_from_atoms(legm,n1,n2)                       Rimuovi il legame n1-n2 dal vettore legm
! S remove_bond_from_atom(legm,na)                           Rimuovi tutti i legami formati dagli atomi nel vettore veta
! S connect_update_remove(conn,vrem)                         Update connect for removing atoms in vrem. conn%pos is not reallocated
! S connect_update_add(conn,leg)                             Update connect adding bonds in leg
! S bonds_update(legm,iord)                                  Aggiorna la numerazione dei legami. Se iord(i) e' zero il legame è rimosso.
! S bonds_shift(legm,shift,kpos)                             Applica shift al numero d'ordine per atomi maggiori di kpos
! S disconnect_atoms(atom,legm,vat)                          Rimuovi tutti i legami tra vat e gli altri atomi ma non all'interno di vat
! S change_atom_in_legm(atold,atnew,legm)                    Cambia in legm il puntatore all'atomo atold con il puntatore atnew
! S add_bonds(legm,atom,vet1,vet2)                           Aggiungi legami tra atomi in vet1 e vet2
! S extract_bonds(legm,vet,lege,nlege)                       Estrai i legami formati dagli atomi indicati nel vettore vet
! S cycle_search(kat,tpos,conn,found)                        Check if a closed path from kat to tpos exists
! S minpath_find(startat,endat,conn,path,nstep)              Find the smallest path from startat to endat
! S bond_info(z1,z2,dist12,btype,dist,delta)                 Get info about bond z1-z2 from bond table
! F bond_type_from_table(legm,z1,z2)                         Try to assign bond type using the connection table.
! F is_bond_equal(leg, vet/legz, zval)                       Verify if legm is the type specified in legz/vet
! F bond_distanceZ(z1,z2)  result(dist)                      Search distance between atoms with Z number z1 and z2
! S get_atoms_legm(k,legm,connt,vet1,nat1,vet2,nat2,in_ring) Extract the groups of atoms vet1 and vet2 connected by bond k, recognize bond in ring
! S bond_distance_update(kat,atom,legm,conn,xgg)             Update bond distances for atom kat
! S bond_angle_update(kat,atom,angle,xgg)                    Update bond angles for atom kat
! S sort_bonds(atom,legm)                                    Sort bonds according to the order number of atoms
! S order_type(atom,bond)                                    Order element types in bonds
! S find_duplicate_bonds_sym(atom,bond,nstartb,nb)           Find bonds equivalent for symmetry starting from nstartb, move them at the end of array
! S find_duplicate_angles_sym(atom,angle,nang)               Find angles equivalent for symmetry, move them at the end of array

USE atom_basic, only: atom_type

implicit none

integer, parameter :: SINGLE_BOND=1,DOUBLE_BOND=2,TRIPLE_BOND=3,AR_SINGLE=4,AR_DOUBLE=5
integer, parameter :: DELETE_BOND = -999

type bond_type
  integer :: n1 = 0, n2 = 0          ! atomi legati
  real    :: dist = 0.0              ! distanza corrente
  real    :: sigma = 0.3             ! sigma
  integer :: ord = SINGLE_BOND       ! bond order

contains
 
  procedure :: set_as_deleted

end type
 
private set_as_deleted

type angle_type
   integer :: n1,n2,n3 ! atomi legati
   real    :: val      ! valore dell'angolo
   real    :: sigma    ! sigma
end type

type torsion_type
   integer :: n1,n2,n3,n4
   real    :: val
end type

type bond_setting_type
     real :: tolmin, tolmax ! legame se dmin-tolmin < d < dmax+tolmax
     real :: angle          ! legame se l'angolo < angle
end type bond_setting_type
type(bond_setting_type), parameter :: DEF_BONDSET = bond_setting_type(0.0,0.0,38)

private :: bond_distance_update_at, bond_distance_update_all
interface bond_distance_update
   module procedure bond_distance_update_at, bond_distance_update_all
end interface

private :: is_bond_equal_l, is_bond_equal_v
interface is_bond_equal
   module procedure is_bond_equal_l, is_bond_equal_v
end interface

private :: order_type_bs, order_type_bv, order_type_av, order_type_tv
interface order_type
   module procedure :: order_type_bs, order_type_bv, order_type_av, order_type_tv
end interface

interface operator(+)
  module procedure bond_add_int
end interface

interface operator(==)
  module procedure bond_equal
end interface

CONTAINS
      
   elemental function bond_add_int(legm,num) result(leg)
   type(bond_type), intent(in) :: legm
   integer, intent(in)         :: num
   type(bond_type)             :: leg
   leg%n1 = legm%n1 + num
   leg%n2 = legm%n2 + num
   leg%dist = legm%dist
   leg%sigma = legm%sigma
   leg%ord = legm%ord
   end function bond_add_int

!----------------------------------------------------------------------------------------------------

   logical function bond_equal(leg1,leg2)
   type(bond_type), intent(in) :: leg1,leg2
   bond_equal = (leg1%n1 == leg2%n1 .and. leg1%n2 == leg2%n2) .or. (leg1%n1 == leg2%n2 .and. leg1%n2 == leg2%n1)
   end function bond_equal

!----------------------------------------------------------------------------------------------------

   elemental subroutine set_as_deleted(bond)
   class(bond_type), intent(inout) :: bond
   bond%n1 = DELETE_BOND
   end subroutine set_as_deleted

!----------------------------------------------------------------------------------------------------

   subroutine iconn_to_connect(natom,icon,connt)
!
!  Subroutine prelimare che utilizza iconn per generare connt
!   
   USE arrayutil
   integer, intent(in)                                             :: natom
   integer, dimension(:), intent(in)                               :: icon
   type(container_type), dimension(:), allocatable, intent(inout) :: connt
   integer                                                         :: ndimc
   integer                                                         :: i,j
   integer                                                         :: natleg
   integer                                                         :: n1,n2
   integer                                                         :: codel
!
   if (allocated(connt)) deallocate(connt)
   allocate(connt(natom))  ! alloca connt sul numero di atomi
   ndimc = icon(1)*3 + 1  ! individua la dimension utile di iconn

!  Alloca connt%pos sul numero di legami (natleg) formato da ogni atomo i   
   do i=1,natom
      natleg = 0
      do j=2,ndimc,3
         n1 = icon(j)
         n2 = icon(j+1)
         codel = icon(j+2)
         if (codel < 0) cycle
         if (n1 == i .or. n2 == i) then
             natleg = natleg + 1
         endif
      enddo
      if (natleg > 0)allocate(connt(i)%pos(natleg))
   enddo
!
!  Riempie connt   
   connt%nat = 0
   do i=2,ndimc,3
      n1 = icon(i)
      n2 = icon(i+1)
      codel = icon(i+2)
      if (codel < 0) cycle
      connt(n1)%nat = connt(n1)%nat + 1
      connt(n1)%pos(connt(n1)%nat) = n2
      connt(n2)%nat = connt(n2)%nat + 1
      connt(n2)%pos(connt(n2)%nat) = n1
   enddo
!
   end subroutine iconn_to_connect
   
!----------------------------------------------------------------------------------------------------

   subroutine iconn_to_legm(icon,legm,atom,gmat)
!
!  Converti icon in legm
!
   USE cgeom
   integer, dimension(:), intent(in)                         :: icon
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   type(atom_type), dimension(:), intent(in)                 :: atom
   real, dimension(3,3), intent(in)                          :: gmat
   integer                                                   :: nleg,nleg0
   integer                                                   :: nc
   integer                                                   :: i
!
   nleg = icon(1)
   call new_bonds(legm,nleg)
   nc = 1
   nleg0 = 0
   do i=1,nleg
      if (icon(nc+3) >= 0) then
          nleg0 = nleg0 + 1
          legm(nleg0)%n1 = icon(nc+1)
          legm(nleg0)%n2 = icon(nc+2)
          legm(nleg0)%dist = distanzaC(atom(icon(nc+1))%xc,atom(icon(nc+2))%xc,gmat)
          legm(nleg0)%sigma = 0.3
          legm(nleg0)%ord = 0
      endif
      nc = nc + 3
   enddo
   call resize_bonds(legm,nleg0)
!
   end subroutine iconn_to_legm

!----------------------------------------------------------------------------------------------------

   subroutine bond_to_connect(natom,legm,conn)
!
!  Converti legm in connect
!
   USE arrayutil
   integer, intent(in)                                          :: natom
   type(bond_type), dimension(:), allocatable, intent(in)       :: legm
   type(container_type), dimension(:), allocatable, intent(out) :: conn
   integer                                                      :: i
   integer, dimension(:), allocatable                           :: vleg
   integer                                                      :: nleg
!
   call new_container(conn,natom)
!
   do i=1,natom
      call get_connected_atoms(legm,i,vleg,nleg)       
      conn(i)%nat = nleg
      if (nleg > 0) then
          allocate(conn(i)%pos(conn(i)%nat))
          conn(i)%pos = vleg(:)
      endif
   enddo
!
   end subroutine bond_to_connect

!----------------------------------------------------------------------------------------------------

   subroutine bond_to_connect_site(natom,legm,conn,atom)
!
!  Converti legm in connect with only 1 atom for site
!
   use arrayutil
   use cgeom
   integer, intent(in)                                          :: natom
   type(bond_type), dimension(:), allocatable, intent(in)       :: legm
   type(container_type), dimension(:), allocatable, intent(out) :: conn
   type(atom_type), dimension(:), intent(in)                    :: atom  ! cartesian
   integer                                                      :: i,j,k
   integer, dimension(:), allocatable                           :: vleg
   integer                                                      :: nleg
!
   call new_container(conn,natom)
!
   do i=1,natom
      call get_connected_atoms(legm,i,vleg,nleg)       
      conn(i)%nat = 0
      if (nleg > 0) then
          if (nleg > 1) then
              do j=1,nleg
                 if (vleg(j) < 0) cycle
                 call container_set(conn(i),vleg(j))
                 if (atom(vleg(j))%och < 1.0) then
                     do k=j+1,nleg
                        if (vleg(k) < 0) cycle
                        if (distanzaC(atom(vleg(j))%xc,atom(vleg(k))%xc) < 0.01) then
                            vleg(k) = -vleg(k)
                        endif
                     enddo
                 endif
              enddo
          else
              conn(i)%nat = nleg
              allocate(conn(i)%pos(conn(i)%nat))
              conn(i)%pos = vleg(:)
          endif
      endif
   enddo
!
   end subroutine bond_to_connect_site

!----------------------------------------------------------------------------------------------------

   subroutine get_connected_atoms(legm,nat,vat,nleg)
!
!  Restituisce in vat gli atomi connessi con nat
!
   USE arrayutil
   type(bond_type), dimension(:), allocatable, intent(in) :: legm
   integer, intent(in)                                    :: nat
   integer, dimension(:), allocatable, intent(inout)      :: vat
   integer, intent(out)                                   :: nleg
   integer                                                :: i
!
!  Alloca vat al numero di atomi legati
   nleg = number_of_bonds(legm,nat)
   call new_array(vat,nleg)
!
   nleg = 0
   do i=1,numbonds(legm)
      if (legm(i)%n1 == nat) then
          nleg = nleg + 1
          vat(nleg) = legm(i)%n2
      elseif (legm(i)%n2 == nat) then
          nleg = nleg + 1
          vat(nleg) = legm(i)%n1
      endif
   enddo
!
   end subroutine get_connected_atoms

!----------------------------------------------------------------------------------------------------

   subroutine get_connected_atoms_spec(legm,nat,zval,zspec,vat,nleg)
!
!  Restituisce in vat gli atomi connessi con nat e di specie spec
!
   USE arrayutil
   type(bond_type), dimension(:), allocatable, intent(in) :: legm
   integer, intent(in)                                    :: nat
   integer, dimension(:), intent(in)                      :: zval
   integer, intent(in)                                    :: zspec
   integer, dimension(:), allocatable, intent(inout)      :: vat
   integer, intent(out)                                   :: nleg
   integer                                                :: i
!
!  Alloca vat al numero di atomi legati
   nleg = number_of_bonds(legm,nat)
   call new_array(vat,nleg)
!
   nleg = 0
   do i=1,numbonds(legm)
      if (legm(i)%n1 == nat) then
          if (zval(legm(i)%n2) == zspec) then
              nleg = nleg + 1
              vat(nleg) = legm(i)%n2
          endif
      elseif (legm(i)%n2 == nat) then
          if (zval(legm(i)%n1) == zspec) then
              nleg = nleg + 1
              vat(nleg) = legm(i)%n1
          endif
      endif
   enddo
   call resize_array(vat,nleg)
!
   end subroutine get_connected_atoms_spec

!----------------------------------------------------------------------------------------------------

   subroutine get_bonds_of_atom(legm,nat,vleg,nleg)
!
!  Restituisce in vleg i legami dell'atomo nat
!
   USE arrayutil
   type(bond_type), dimension(:), allocatable, intent(in) :: legm
   integer, intent(in)                                    :: nat
   integer, dimension(:), allocatable, intent(inout)      :: vleg
   integer, intent(out)                                   :: nleg
   integer                                                :: i
!
!  Alloca vleg al numero di legami formati
   nleg = number_of_bonds(legm,nat)
   if (nleg > 0) then
       call new_array(vleg,nleg)
!
       nleg = 0
       do i=1,numbonds(legm)
          if (legm(i)%n1 == nat) then
              nleg = nleg + 1
              vleg(nleg) = i
          elseif (legm(i)%n2 == nat) then
              nleg = nleg + 1
              vleg(nleg) = i
          endif
       enddo
   endif
!
   end subroutine get_bonds_of_atom

!----------------------------------------------------------------------------------------------------

   subroutine get_angles_of_bond(angle,n1,n2,vang,nang)
!
!  Get angles containing the bond n1-n2
!
   USE arrayutil
   type(angle_type), dimension(:), allocatable, intent(in) :: angle
   integer, intent(in)                                     :: n1,n2
   integer, dimension(:), allocatable, intent(inout)       :: vang
   integer, intent(out)                                    :: nang
   integer                                                 :: i,numang
!
   nang = 0
   numang = numangles(angle)
   if (numang == 0) return
   call new_array(vang,numang)
!    
   do i=1,numang
      if (is_in_angle(angle(i),n1,n2) /= 0) then
          nang = nang + 1
          vang(nang) = i
      endif
   enddo
!
   end subroutine get_angles_of_bond

!----------------------------------------------------------------------------------------------------

   integer function is_in_angle(angle,n1,n2)
!
!  Check if angle contains the bond n1-n2
!
   type(angle_type), intent(in) :: angle
   integer, intent(in)          :: n1,n2
!
   is_in_angle = 0
   if ((n1 == angle%n1 .and. n2 == angle%n2) .or. (n1 == angle%n2 .and. n2 == angle%n1))  then
       is_in_angle = 1
   elseif ((n1 == angle%n2 .and. n2 == angle%n3) .or. (n1 == angle%n3 .and. n2 == angle%n2))  then
       is_in_angle = 2
   endif
!
   end function is_in_angle

!----------------------------------------------------------------------------------------------------

   integer function number_of_bonds(legm,nat) result(num)
!
!  Calcola quanti legami forma l'atomo nat
!
   type(bond_type), dimension(:), allocatable, intent(in) :: legm
   integer, intent(in)                                    :: nat
   integer                                                :: nleg
!
   nleg = numbonds(legm)
   if (nleg > 0) then
       num = count(legm%n1 == nat .or. legm%n2 == nat)
   else
       num = 0
   endif
!
   end function number_of_bonds
 
!----------------------------------------------------------------------------------------------------

   logical function is_connected(legm,nat) 
!
!  Check on legm if nat is connected
!
   type(bond_type), dimension(:), allocatable, intent(in) :: legm
   integer, intent(in)                                    :: nat
   integer                                                :: nleg
   nleg = numbonds(legm)
   if (nleg > 0) then
       is_connected = any(legm(:)%n1 == nat .or. legm(:)%n2 == nat)
   else
       is_connected = .false.
   endif
!
   end function is_connected

!----------------------------------------------------------------------------------------------------
      
   subroutine connect_to_leg(connt,atom,gmat,legm,numleg,lordi,sizel)
!
!  Riempie leg e numleg a partire da connect
!   
   USE CGEOM, only:distanzaC
   USE strutil
   USE arrayutil
   type(container_type), dimension(:), intent(in)           :: connt
   type(atom_type), dimension(:), intent(in)                 :: atom
   real, dimension(3,3), intent(in)                          :: gmat
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   logical, intent(in), optional                             :: lordi
   integer, intent(in), optional                             :: sizel ! if conn doesn't contain all bonds
   logical                                                   :: lord
   integer, intent(out)                                      :: numleg
   integer, dimension(size(connt),size(connt))               :: vetleg
   integer                                                   :: i,nl
   integer                                                   :: n1,n2
   integer                                                   :: nn2
   character(16*2), dimension(:), allocatable                :: strleg
   integer, dimension(:), allocatable                        :: indx
   integer                                                   :: n1sav
!
   if (present(sizel)) then
       call new_bonds(legm,sizel)
   else
       numleg=sum(connt%nat)/2
       call new_bonds(legm,numleg)
   endif
!
!  Alloca legm sul numero di legami numleg      
   vetleg = 0
   numleg = 0
   do i=1,size(connt)
      n1 = i
      do nl=1,connt(i)%nat
         n2 = connt(i)%pos(nl)
         if ( vetleg(n1,n2) == 0) then
              numleg = numleg + 1   
              legm(numleg)%n1 = i 
              nn2 = connt(i)%pos(nl)
              legm(numleg)%n2 = nn2   !!!!connt(i)%pos(nl) 
              legm(numleg)%sigma = 0.03
! ifort64-opt va in errore (mah!) per cui ho introdotto nn2
!             legm(numleg)%dist = distanzaC(atom(i)%xc,atom(legm(numleg)%n2)%xc,xgg)
              legm(numleg)%dist = distanzaC(atom(i)%xc,atom(nn2)%xc,gmat)
               !write(*,*)'numleg=',numleg,legm(numleg)
               !write(*,*)'at1=',atom(i)%xc,atom(legm(numleg)%n2)%xc
              vetleg(n1,n2) = 1
              vetleg(n2,n1) = 1
         endif
      enddo
   enddo
   if (present(sizel)) call resize_bonds(legm,numleg)
!
   if (present(lordi)) then
       lord = lordi
   else
       lord = .false.
   endif
!
!  Ordina i legami in base alle label degli atomi
   if (numleg > 0 .and. lord) then
!
!      prima ordina le label nel singolo legame
       do i=1,numleg
          if (lgt(atom(legm(i)%n1)%lab,atom(legm(i)%n2)%lab))then
!
!             scambia gli atomi nel legame
              n1sav = legm(i)%n1
              legm(i)%n1 = legm(i)%n2
              legm(i)%n2 = n1sav
          endif
       enddo
!
!      poi crea lista di legami ordinati
       allocate(strleg(numleg),indx(numleg))
       do i=1,numleg
          strleg(i) = trim(atom(legm(i)%n1)%lab)//trim(atom(legm(i)%n2)%lab)
       enddo
       call svec_sort_heap_a_index ( numleg, strleg, indx )
       legm(:) = legm(indx(:))
   endif
!   
   end subroutine connect_to_leg
   
!----------------------------------------------------------------------------------------------------            
      
   subroutine connect_to_ang(connt,atom,gmat,angle,numang,lordi)
!
!  Riempie ang a partire da connect
!   
   USE CGEOM   
   USE strutil
   USE trig_constants
   USE arrayutil
   type(container_type), dimension(:), intent(in)            :: connt
   type(atom_type), dimension(:), intent(in)                  :: atom
   real, dimension(3,3)                                       :: gmat
   type(angle_type), dimension(:), allocatable, intent(inout) :: angle
   integer, intent(out)                                       :: numang
   logical, intent(in), optional                              :: lordi  ! angoli ordinati in base alle label
   logical                                                    :: lord
   integer                                                    :: i,j,k
   character(16*3), dimension(:), allocatable                 :: strang
   integer, dimension(:), allocatable                         :: indx
   integer                                                    :: n1sav
!
   if (allocated(angle)) deallocate(angle)
!
!  Calcola il numero di angoli con la seguente formula
   numang = sum(connt%nat*(connt%nat -1),mask=connt%nat>1)/2   
   allocate(angle(numang))
!   
   numang = 0
   do i=1,size(connt)
      if (connt(i)%nat > 1) then          
          do j=1,connt(i)%nat             
             do k=j+1,connt(i)%nat
                numang = numang + 1
                angle(numang)%n1 = connt(i)%pos(j)
                angle(numang)%n2 = i                
                angle(numang)%n3 = connt(i)%pos(k)
                angle(numang)%val = rtod*angleC(atom(connt(i)%pos(j))%xc,atom(i)%xc,atom(connt(i)%pos(k))%xc,gmat)
                angle(numang)%sigma = 3.0
             enddo
          enddo
      endif
   enddo
!
   if (present(lordi)) then
       lord = lordi
   else
       lord = .false.
   endif
!
!  Ordina gli angoli in base alle label degli atomi
   if (numang > 0 .and. lord) then
!
!      prima ordina le label nel singolo angolo
       do i=1,numang
          if (lgt(atom(angle(i)%n1)%lab,atom(angle(i)%n3)%lab))then
!
!             scambia gli atomi nel legame
              n1sav = angle(i)%n1
              angle(i)%n1 = angle(i)%n3
              angle(i)%n3 = n1sav
          endif
       enddo
!
!      poi crea lista di angoli ordinati
       if (numang > 1) then
           allocate(strang(numang),indx(numang))
           do i=1,numang
              strang(i) = trim(atom(angle(i)%n1)%lab)//trim(atom(angle(i)%n2)%lab)//trim(atom(angle(i)%n3)%lab)
           enddo
           call svec_sort_heap_a_index ( numang, strang, indx )
           angle(:) = angle(indx(:))
       endif
   endif
!   
   end subroutine connect_to_ang
      
!----------------------------------------------------------------------------------------------------

   subroutine bond_to_angle(legm,atom,gmat,angle,angvali,cart)
!
!  Genera angoli. Se angvali e' assegnato, solo gli angoli < di angvali vengono generati 
!
   USE cgeom
   USE trig_constants
   type(bond_type), dimension(:), allocatable, intent(in)     :: legm
   type(atom_type), dimension(:), intent(in)                  :: atom
   real, dimension(3,3), intent(in)                           :: gmat
   type(angle_type), dimension(:), allocatable, intent(inout) :: angle
   real, dimension(2), intent(in), optional                   :: angvali ! in gradi
   logical, intent(in), optional                              :: cart    ! se vero, atom e' in coord. cartesiane
   real, dimension(2)                                         :: angval
   real                                                       :: value
   integer                                                    :: nleg,numang
   integer                                                    :: i,j
   integer                                                    :: ndimang
   integer                                                    :: n1i,n2i,n1j,n2j,na,nb,nc
   logical                                                    :: okang
   logical                                                    :: carti
!
   nleg = numbonds(legm)
   if (nleg > 0) then
       if (present(angvali)) then
           angval = angvali
       else
           angval = [-1,1000]  ! all angles accepted!
       endif
       if (present(cart)) then
          carti = cart
       else
          carti = .false.
       endif
!
!      alloca inizialmente gli angoli al numero di legami
       ndimang = nleg
       call reallocate_angle(angle,ndimang)
!
!      cerca angoli in legm
       numang = 0
       do i=1,nleg-1
          n1i = legm(i)%n1
          n2i = legm(i)%n2
          do j=i+1,nleg
             if(j == i) cycle
             n1j = legm(j)%n1
             n2j = legm(j)%n2
             okang = .true.
             if (n1i == n1j) then
                 na = n2i
                 nb = n1i
                 nc = n2j
             elseif (n1i == n2j) then
                 na = n2i
                 nb = n1i
                 nc = n1j
             elseif (n2i == n1j) then
                 na = n1i
                 nb = n2i
                 nc = n2j
             elseif (n2i == n2j) then
                 na = n1i
                 nb = n2i
                 nc = n1j
             else
                 okang = .false.
             endif
             if (okang) then  ! angolo trovato
                 if (carti) then
                     value = rtod*angleC(atom(na)%xc,atom(nb)%xc,atom(nc)%xc)     ! coord. cartesiane
                 else
                     value = rtod*angleC(atom(na)%xc,atom(nb)%xc,atom(nc)%xc,gmat) ! coord. cristallogr.
                 endif
                 if(value > angval(1) .and. value < angval(2))then
                    numang = numang + 1
                    if (numang > ndimang) then  ! check sulla dimensione di angle
                        ndimang = ndimang + nleg
                        call reallocate_angle(angle,ndimang)
                    endif
                    angle(numang) = angle_type(na,nb,nc,value,3.0)
                    !write(0,*)'angle=',value,atom(na)%lab,atom(nb)%lab,atom(nc)%lab
                 endif
             endif
          enddo
       enddo
       call reallocate_angle(angle,numang)
   endif
!
   end subroutine bond_to_angle

!----------------------------------------------------------------------------------------------------

   integer function numbonds(legm)
   type(bond_type), dimension(:), allocatable, intent(in) :: legm
!
   if (allocated(legm)) then
       numbonds = size(legm)
   else
       numbonds = 0
   endif
!
   end function numbonds

!----------------------------------------------------------------------------------------------------

   integer function numangles(angle)
   type(angle_type), dimension(:), allocatable, intent(in) :: angle
!
   if (allocated(angle)) then
       numangles = size(angle)
   else
       numangles = 0
   endif
!
   end function numangles

!----------------------------------------------------------------------------------------------------

   integer function numtorsions(tors)
   type(torsion_type), dimension(:), allocatable, intent(in) :: tors
!
   if (allocated(tors)) then
       numtorsions = size(tors)
   else
       numtorsions = 0
   endif
!
   end function numtorsions

!----------------------------------------------------------------------------------------------------

   integer function bond_position(bond,n1,n2) result(pos)
!
!  Restituisce la locazione del legame n1-n2 nel vettore bond
   type(bond_type), dimension(:), intent(in) :: bond
   integer, intent(in)                       :: n1,n2
   integer                                   :: i
!   
   pos = 0
   do i=1,size(bond)
      if ((bond(i)%n1 == n1 .and. bond(i)%n2 == n2) .or. (bond(i)%n1 == n2 .and. bond(i)%n2 == n1)) then
         pos = i
         exit
      endif
   enddo
!
   end function bond_position

!----------------------------------------------------------------------------------------------------
      
   subroutine print_connect(lab,bond,connt,angle,tors,kpri,filename,vet,csv,csvstring,icon)
!   
!  Puo' stampare la connettivita' in tutte le sue forme
!
   USE atom_basic
   USE strutil
   USE fileutil
   USE arrayutil
   character(len=*), dimension(:), intent(in)                          :: lab
   type(bond_type), dimension(:), allocatable, intent(in), optional    :: bond
   type(container_type), dimension(:), intent(in), optional            :: connt
   type(angle_type), dimension(:), allocatable, intent(in), optional   :: angle
   type(torsion_type), dimension(:), allocatable, intent(in), optional :: tors
   integer, intent(in), optional                                       :: kpri
   character(len=*), intent(in), optional                              :: filename
   integer, dimension(:), intent(in), optional                         :: vet
   logical, intent(in), optional                                       :: csv
   character(len=*), intent(in), optional                              :: csvstring
   integer, dimension(:), intent(in), optional                         :: icon
   logical                                                             :: csvtype
   integer                                                             :: kpr
   integer                                                             :: i,j
   character(len=:), allocatable                                       :: stringl
   integer                                                             :: ipos
   integer                                                             :: i1,i2
   integer                                                             :: ndimc
   integer, dimension(:), allocatable                                  :: veti,ktype,ival
   real, dimension(:), allocatable                                     :: rval
   character(len=10), dimension(:),allocatable                         :: srow
   integer                                                             :: ncol,nat
   type(file_handle)                                                   :: fb
!
   if (present(filename)) then
       call fb%fopen(filename,'w')
       kpr = fb%handle()
   else
       if (present(kpri)) then
           kpr = kpri
       else
           kpr = 6
       endif
   endif
!
   if (present(csv)) then
       csvtype = csv
   else
       csvtype = .false.
   endif
!
!  Stampa iconn
   if (present(icon)) then
       write(kpr,'(/2x,50("-")/25x,"ICONN table"/)')
       write(kpr,'(2x,a/2x,50("-"))')'  bond                    ICONN(3)'
       ndimc = icon(1)*3 + 1
       do i=2,ndimc,3
          i1 = icon(i)
          i2 = icon(i+1)
          write(kpr,'(4x,a,t40,i0)')trim(slabnum(lab(i1),i1))//'-'//trim(slabnum(lab(i2),i2)),icon(i+2)
       enddo
   endif             
!
!  Stampa di connt
   if (present(connt)) then
       write(kpr,'(/2x,50("-")/15x,"Table of connectivity"/)')
       write(kpr,'(2x,a/2x,50("-"))')'  atom          bonds           with atom(s)'
       do i=1,size(connt)            
          stringl = ' '
          if (connt(i)%nat > 0) then
!
!             Genera in stringl una stringa contenente gli atomi legati all'atomo i            
              ipos = connt(i)%pos(1)
              stringl = slabnum(lab(ipos),ipos)
              do j=2,connt(i)%nat
                 ipos = connt(i)%pos(j)
                 stringl = trim(stringl)//','//slabnum(lab(ipos),ipos)
              enddo
          endif
          write(kpr,'(4x,a,t22,i0,10x,a)')slabnum(lab(i),i),connt(i)%nat,trim(stringl)
       enddo
   endif
!         
!  Stampa i legami
   if (present(bond)) then
       if (numbonds(bond) > 0)then
           if (csvtype) then
               do i=1,numbonds(bond)
                  write(kpr,'(a)')';'//trim(csvstring)//';'//trim(lab(bond(i)%n1))//';'   &
                                     //trim(lab(bond(i)%n2))//';'//r_to_s(bond(i)%dist)
               enddo
           else
               ncol = 4
               call init_table()
!
!              Scrivi titoli delle colonne
               ktype(:) = 3
               call set_row_list(veti,ktype,sval=(/'Number  ','Atom 1  ','Atom 2  ','Distance'/),kpr=kpr)
!
!              Scrivi la tabella dei legami
               ktype = (/1,3,3,23/)
               nat = size(lab)
               do i=1,numbonds(bond)
                  ival(1) = i
                  if (bond(i)%n1 > nat .or. bond(i)%n2 > nat) then
                      write(kpr,'(a,2x,a)')'Wrong bond between atom '//trim(i_to_s(bond(i)%n1))//   &
                      ' and '//trim(i_to_s(bond(i)%n2)),'N. of atoms: '//trim(i_to_s(nat))
                      cycle
                  endif
                  srow(2) = lab(bond(i)%n1)
                  srow(3) = lab(bond(i)%n2) 
                  rval(4) = bond(i)%dist
                  call set_row_list(veti,ktype,ival,rval,srow,.false.,kpr)
               enddo
           endif
       endif
    endif
!
!   Stampa gli angoli
    if (present(angle)) then
        ncol = 5
        call init_table()
!
!       Scrivi titoli delle colonne
        ktype(:) = 3
        call set_row_list(veti,ktype,sval=(/'Number','Atom 1','Atom 2','Atom 3','Angle '/),kpr=kpr)
!
!       Scrivi la tabella degli angoli
        ktype = (/1,3,3,3,22/)
        do i=1,numangles(angle)
           ival(1) = i
           srow(2) = lab(angle(i)%n1)
           srow(3) = lab(angle(i)%n2)
           srow(4) = lab(angle(i)%n3)
           rval(5) = angle(i)%val
           call set_row_list(veti,ktype,ival,rval,srow,.false.,kpr)
        enddo
    endif                      
!
!   Stampa torsioni
    if (present(tors)) then
        ncol = 6
        call init_table()
!
!       Scrivi titoli delle colonne
        ktype(:) = 3
        call set_row_list(veti,ktype,sval=(/'Number','Atom 1','Atom 2','Atom 3','Atom 4','Angle '/),kpr=kpr)
!
!       Scrivi la tabella degli angoli
        ktype = (/1,3,3,3,3,22/)
        do i=1,numtorsions(tors)
           ival(1) = i
           srow(2) = lab(tors(i)%n1)
           srow(3) = lab(tors(i)%n2)
           srow(4) = lab(tors(i)%n3)
           srow(5) = lab(tors(i)%n4)
           rval(6) = tors(i)%val
           call set_row_list(veti,ktype,ival,rval,srow,.false.,kpr)
        enddo
    endif
!
    if (present(filename)) call fb%fclose()
!
    CONTAINS

    subroutine init_table()
    if (allocated(ktype)) deallocate(ktype)
    if (allocated(veti)) deallocate(veti)
    if (allocated(ival)) deallocate(rval)
    if (allocated(rval)) deallocate(rval)
    if (allocated(srow)) deallocate(srow)
    allocate(ktype(ncol),veti(ncol),ival(ncol),rval(ncol),srow(ncol))
    if (present(vet)) then
        veti(:) = vet  ! stampa solo le colonne con vet=1
    else
        veti(:) = 1    ! stampa tutte le colonne
    endif
    end subroutine init_table
!
   end subroutine print_connect
         
!----------------------------------------------------------------------------------------------------
      
   recursive subroutine get_chain(connt,pat,stopa,nat,veta)
!
!  Restituisce nel vettore veta tutti i nat atomi legati in catena all'atomo pat
!  esclusi gli atomi della ramificazione che parte con l'atomo stopa
!  Inizializza esternamente la sub. con nat=1, vet(1)=pat, vet(2:)=0 
!  La sub. richiede la connettivit� in forma connect
!   
   USE arrayutil
   type(container_type), dimension(:), intent(in) :: connt
   integer, intent(in)                  :: pat
   integer, intent(in)                  :: stopa
   integer, intent(inout)               :: nat
   integer, dimension(:), intent(inout) :: veta
   integer                              :: nat0
   integer                              :: pa
   integer                              :: i
   integer                              :: ipa
!!!!   integer                              :: kpr = 0
!
   pa = pat
   outer: do 
      nat0 = 0
!
!     Loop interno sugli atomi legati all'atomo pa      
      inter: do i=1,connt(pa)%nat
         ipa = connt(pa)%pos(i)
!
!        Non considero l'atomo se � gi� stato selezionato (cio� � in veta) o se = a stopa         
         if (ipa == stopa .or. any(veta == ipa))  cycle
         nat0 = nat0 + 1
         nat = nat + 1
         veta(nat) = ipa
         if (nat0 > 0) then    ! l'atomo pa contiene una ramificazione
             call get_chain(connt,ipa,stopa,nat,veta)
         endif
      enddo inter
      if (nat0 == 0) exit      ! catena interrotta per atomo terminale o uguale a stopa
      pa = veta(nat)
   enddo outer
!!!!   if (kpr>0) write(6,'(i0,1x,100a)')nat,'ATOMO ',aLab(pat),' LEGA ',aLab(veta(:nat))
!   
   end subroutine get_chain   
            
!----------------------------------------------------------------------------------------------------

   subroutine remove_atoms_from_iconn(vat,icon)
!
!  Aggiorna iconn rimuovendo gli atomi contenuti in vat
!
   integer, dimension(:), intent(in)    :: vat  ! n.ord degli atomi da rimuovere
   integer, dimension(:), intent(inout) :: icon
   integer, dimension(size(icon))       :: icon_new
   integer                              :: i,inew
   integer                              :: n1,n2
   integer                              :: ndimc
!
   ndimc = icon(1)*3 + 1
   inew = 1
   icon_new(1) = 0
   do i=2,ndimc,3
      n1 = icon(i)
      n2 = icon(i+1)
      if (all(vat /= n1) .and. all(vat /= n2)) then
          !write(6,*)'accetto legame',n1,n2
!
!         incrementa i numeri di legami
          icon_new(1) = icon_new(1) + 1
!
!         correggi la numerazione 
          n1 = n1 - count(vat < n1)
          n2 = n2 - count(vat < n2)
!
          inew = inew + 1
          icon_new(inew) = n1
          inew = inew + 1
          icon_new(inew) = n2
          inew = inew + 1
          icon_new(inew) = icon(i+2)
      endif
   enddo
   icon(:) = icon_new(:)
!
   end subroutine remove_atoms_from_iconn

!-------------------------------------------------------------------------------------------------

   subroutine tabconn_to_iconn(tabconn,icon,add)
!
!  Converti tabconn in iconn
!
   integer, dimension(:,:), intent(in)    :: tabconn
   integer, dimension(:), intent(inout)   :: icon
   logical, intent(in), optional          :: add
   integer                                :: nat
   integer                                :: i,j
   integer                                :: icc
!   
   nat = size(tabconn,1)
   if (present(add)) then ! aggiungi all'iconn
       icc = icon(1)*3 + 1
   else                   ! riscrivi l'iconn
       icc = 1
       icon(1) = 0
   endif
   do j=1,nat-1
      do i=j+1,nat
         if (tabconn(i,j) == 1) then
             icon(1) = icon(1) + 1
             icc = icc + 1
             icon(icc) = i
             icc = icc + 1
             icon(icc) = j
             icc = icc + 1
             icon(icc) = 1
             !tabconn(j,i) = 0
         endif
      enddo
   enddo
!
   end subroutine tabconn_to_iconn

!-------------------------------------------------------------------------------------------------

   subroutine tabconn_to_connect(tabconn,connt)
!
!  Converti tabconn in connect
!
   USE arrayutil
   integer, dimension(:,:), intent(in)                             :: tabconn
   type(container_type), dimension(:), intent(inout), allocatable :: connt
   integer                                                         :: nat
   integer, dimension(size(tabconn,1))                             :: veta
   integer                                                         :: i,j
!
   nat = size(tabconn,1)
   call new_container(connt,nat)
   do i=1,nat
      veta(:) = 0
      connt(i)%nat = 0
      do j=1,nat
         if (tabconn(i,j) /= 0) then
             connt(i)%nat = connt(i)%nat + 1  
             veta(connt(i)%nat) = j
         endif
      enddo
      if (connt(i)%nat > 0) then
          allocate(connt(i)%pos(connt(i)%nat))
          connt(i)%pos = veta(:connt(i)%nat)
      endif
   enddo
!
   end subroutine tabconn_to_connect

!-------------------------------------------------------------------------------------------------
   subroutine tabconn_to_legm(tabconn,atom,gmat,sd,legm)
!
!  Converti tabconn in bond_type
!
   USE CGEOM, only:distanzaC
   integer, dimension(:,:), intent(in)                       :: tabconn
   type(atom_type), dimension(:), intent(in)                 :: atom
   real, dimension(3,3), intent(in)                          :: gmat
   real, intent(in)                                          :: sd
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer                                                   :: nat
   integer                                                   :: nconn
   integer                                                   :: i,j
!
   nat = size(tabconn(:,1))               ! num. di atomi
   nconn = count(tabconn(:,:) /= 0)/2     ! num. di legami
!
   call new_bonds(legm,nconn)
   legm(:)%sigma = sd
!
   nconn = 0
   do i=1,nat-1
      do j=i+1,nat
         if (tabconn(i,j) /= 0) then       
            nconn = nconn + 1
            legm(nconn) = bond_type(i,j,distanzaC(atom(i)%xc,atom(j)%xc,gmat),0.03,0)
         endif
      enddo
   enddo
!
   end subroutine tabconn_to_legm

!-------------------------------------------------------------------------------------------------

   subroutine iconn_to_tabconn(tabconn,icon)
!
!  Converti iconn in tabconn
!
   integer, dimension(:,:), intent(inout) :: tabconn
   integer, dimension(:), intent(in)      :: icon
   integer                                :: nat
   integer                                :: ic
   integer                                :: i1,i2
   integer                                :: i
!   
   nat = size(tabconn(:,1))
   tabconn(:,:) = 0
   ic = 1
   do i=1,icon(1)
      ic = ic + 1
      i1 = icon(ic)
      ic = ic + 1
      i2 = icon(ic)
      ic = ic + 1
      tabconn(i1,i2) = 1
      tabconn(i2,i1) = 1
   enddo
!
   end subroutine iconn_to_tabconn

!-------------------------------------------------------------------------------------------------

   subroutine bond_to_iconn(legm,icon)
   type(bond_type), dimension(:), allocatable, intent(in) :: legm
   integer, dimension(:), intent(inout)                   :: icon
   integer                                                :: nleg
   integer                                                :: i
   integer                                                :: nc
!
   nleg = numbonds(legm)
   icon(1) = nleg
   nc = 1
   do i=1,min(nleg,(size(icon) - 1) / 3)
      nc = nc + 1
      icon(nc) = legm(i)%n1
      nc = nc + 1
      icon(nc) = legm(i)%n2
      nc = nc + 1
      icon(nc) = 0
   enddo
!
   end subroutine bond_to_iconn

!---------------------------------------------------------------------------------------

   subroutine bond_to_tabconn(legm,tabconn)
   type(bond_type), dimension(:), intent(in) :: legm
   integer, dimension(:,:), intent(out)      :: tabconn
   integer                                   :: nleg
   integer                                   :: i
!
   nleg = size(legm)
   tabconn(:,:) = 0
   do i=1,nleg
      tabconn(legm(i)%n1,legm(i)%n2) = i
      tabconn(legm(i)%n2,legm(i)%n1) = i
   enddo
!
   end subroutine bond_to_tabconn

!---------------------------------------------------------------------------------------

   subroutine combine_legm(legm1,legm2,shifta,mergeb,nleg)
!
!  Aggiungi legm2 a legm1
!
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm1
   type(bond_type), dimension(:), allocatable, intent(in)    :: legm2
   integer, intent(in), optional                             :: shifta ! incremento sulla numerazione di legm2
   logical, intent(in), optional                             :: mergeb ! add only new bonds
   integer                                                   :: shift
   integer, intent(in), optional                             :: nleg   ! add only nleg bonds
   integer                                                   :: i
   integer                                                   :: nleg1,nleg2
   logical                                                   :: mergebonds
   integer :: nlegnew
!
   nleg2 = numbonds(legm2)
   if (present(nleg)) then
       if (nleg < nleg2) nleg2 = nleg
   endif
   if (nleg2 > 0) then
       if (present(shifta)) then
           shift = shifta
       else
           shift = 0
       endif
!
       nleg1 = numbonds(legm1)
       call resize_bonds(legm1,nleg1+nleg2)
       if (present(mergeb)) then
           mergebonds = mergeb
       else
           mergebonds = .false.
       endif
       if (mergebonds) then
           nlegnew = 0
           do i=1,nleg2
              if (bond_position(legm1(:nleg1),legm2(i)%n1,legm2(i)%n2) > 0) cycle
              nlegnew = nlegnew + 1
              legm1(nleg1+nlegnew)%n1 = legm2(i)%n1 + shift
              legm1(nleg1+nlegnew)%n2 = legm2(i)%n2 + shift
              legm1(nleg1+nlegnew)%dist = legm2(i)%dist
              legm1(nleg1+nlegnew)%sigma = legm2(i)%sigma
              legm1(nleg1+nlegnew)%ord = legm2(i)%ord
           enddo
           call resize_bonds(legm1,nleg1+nlegnew)
       else
           if (shift > 0) then
               legm1(nleg1+1:) = legm2(:nleg2) + shift
           else
               legm1(nleg1+1:) = legm2(:nleg2)
           endif
       endif
   endif
!
   end subroutine combine_legm

!-------------------------------------------------------------------------------------------------

   subroutine copy_bonds(legm1,legm2)
!
!  Copia legm2 in legm1
!
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm1
   type(bond_type), dimension(:), allocatable, intent(in)    :: legm2
   integer                                                   :: nleg2
!
   nleg2 = numbonds(legm2)
   call new_bonds(legm1,nleg2)
   if (nleg2 > 0) legm1(:) = legm2(:)
!
   end subroutine copy_bonds

!-------------------------------------------------------------------------------------------------

   integer function compare_legm(legm1,legm2)   result(diff)
!
!  Controlla se ci sono differenze tra legm1 e legm2
!
   type(bond_type), dimension(:), allocatable, intent(in) :: legm1
   type(bond_type), dimension(:), allocatable, intent(in) :: legm2
   integer                                                :: nleg1,nleg2
   integer                                                :: n1,n2
   integer                                                :: i
   integer                                                :: pos
!
   nleg1 = numbonds(legm1)
   nleg2 = numbonds(legm2)
   diff = abs(nleg1 - nleg2)
!
!  Se diff e' zero esegui un ulteriore controllo
   if (diff == 0) then   
       do i=1,nleg1
          n1 = legm1(i)%n1
          n2 = legm2(i)%n2
          pos = bond_position(legm2,n1,n2)
          if (pos == 0) then
              diff = 1
              exit
          endif
       enddo
   endif
!
   end function  compare_legm

!-------------------------------------------------------------------------------------------------

   subroutine remove_bond(legm,lpos)
!
!  Rimuovi il legame lpos dal vettore legm
!
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer, intent(in)                                       :: lpos
   integer                                                   :: nleg
!
   nleg = numbonds(legm)
   if (lpos > 0 .and. nleg > 0) then
       legm(lpos:nleg-1) = legm(lpos+1:nleg)
       call resize_bonds(legm,nleg-1)
   endif
!
   end subroutine remove_bond

!-------------------------------------------------------------------------------------------------
  
   subroutine remove_bonds(legm,vet)
!
!  Rimuovi tutti i legami indicati nel vettore vet
!  Se vet e' assente vengono rimossi i legami con legm(i)%n1 = 0
!
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer, dimension(:), intent(in), optional               :: vet
   integer                                                   :: nleg
   integer                                                   :: i
   integer, dimension(:), allocatable                        :: remvet
!
   nleg = numbonds(legm)
   if (nleg > 0) then
       allocate(remvet(nleg),source=legm%n1)
!
!      Marca con 0 i legami da eliminare   
       if (present(vet)) then
           do i=1,size(vet)
              remvet(vet(i)) = 0
           enddo
       endif
!
!      elimina i legami marcati
       call remove_bondsv(legm,remvet,0)
   endif
!
   end subroutine remove_bonds

!-------------------------------------------------------------------------------------------------

   subroutine remove_bonds_sym(legms,legm,atoms,vet)
!
!  Rimuovi tutti i legami indicati nel vettore vet e gli equivalenti per simmetria
!
   type(bond_type), dimension(:), allocatable, intent(inout) :: legms ! legami nella cella
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm  ! legami nell'u.a.
   type(atom_type), dimension(:), allocatable, intent(in)    :: atoms  ! info sulla simmetria
   integer, dimension(:), intent(in), optional               :: vet  
   integer                                                   :: nleg
   integer                                                   :: i,j
   integer                                                   :: n1,n2
   integer                                                   :: pos
   integer                                                   :: n1s,n2s
   logical                                                   :: delb,delbs
!
   nleg = numbonds(legms)
!
   delbs = .false.
   do i=1,size(vet)
      pos = vet(i)
      n1 = atoms(abs(legms(pos)%n1))%asym
      n2 = atoms(abs(legms(pos)%n2))%asym
      do j=1,nleg
         if (legms(j)%n1 < 0) cycle    ! legame gia' eliminato
         n1s = atoms(legms(j)%n1)%asym
         n2s = atoms(legms(j)%n2)%asym
         if ((n1 == n1s .and. n2 == n2s) .or. (n1 == n2s .and. n2 == n1s)) then
             delbs = .true.
             legms(j)%n1 = -legms(j)%n1   ! marca il legame per eliminarlo
         endif
      enddo
!corr!
!corr!     Cerca legame nell'u.a., se non esiste allora e' un legame tra u.a.
!corr      pos = bond_position(legm,n1,n2)
!corr      if (pos > 0) legm(pos)%n1 = 0  ! marca il legame che verra eliminato
   enddo
!
   if (numbonds(legm) > 0) then
!
!      Remove bonds in the asu
       delb = .false.
       do i=1,size(vet)
          pos = vet(i)
          n1 = atoms(abs(legms(pos)%n1))%asym
          n2 = atoms(abs(legms(pos)%n2))%asym
          pos = bond_position(legm,n1,n2)
          if (pos > 0) then
              delb = .true.
              legm(pos)%n1 = 0  ! marca il legame che verra eliminato
          endif
       enddo
       if(delb) call remove_bonds(legm)
   endif
!
!  remove bonds in the cell
   if (delbs) then
       where(legms(:)%n1 < 0) legms(:)%n1 = 0
       call remove_bonds(legms)
   endif
!
   end subroutine remove_bonds_sym
   
!-------------------------------------------------------------------------------------------------

   subroutine remove_bondsv(legm,vat,val)
!
!  Rimuovi tutti i legami per i queli il vettore vat e' uguale a val
!
   type(bond_type), dimension(:), intent(inout), allocatable  :: legm
   integer, dimension(:), intent(in)                          :: vat    ! size(vat) >= size(legm)
   integer, intent(in)                                        :: val
   integer                                                    :: nleg,nb
!
   nb = numbonds(legm)
   if (nb > 0) then
       nleg = count(vat(:nb) /= val)
       if (nleg == nb) return
       legm(:nleg) = pack(legm,mask=vat(:nb)/=val)
       call resize_bonds(legm,nleg)
   endif
!
   end subroutine remove_bondsv

!-------------------------------------------------------------------------------------------------

   subroutine bond_delete_selected(bonds)
!
!  Remove bonds selected as DELETE_BOND
!
   type(bond_type), dimension(:), intent(inout), allocatable :: bonds
   call remove_bondsv(bonds,bonds%n1,DELETE_BOND)
   end subroutine bond_delete_selected

!-------------------------------------------------------------------------------------------------

   subroutine remove_bond_from_atoms(legm,n1,n2)
!
!  Rimuovi il legame n1-n2 dal vettore legm
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer, intent(in)                                       :: n1,n2
   integer                                                   :: lpos
!
   if (numbonds(legm) > 0) then
       lpos = bond_position(legm,n1,n2)
       call remove_bond(legm,lpos)
   endif
!
   end subroutine remove_bond_from_atoms

!-------------------------------------------------------------------------------------------------

   subroutine remove_bond_from_atom(legm,veta,corr)
!
!  Rimuovi tutti i legami formati dagli atomi nel vettore veta ed eventualmente correggi la numerazione
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer, dimension(:), intent(in)                         :: veta
   logical, intent(in), optional                             :: corr
   logical                                                   :: lcorr
   integer                                                   :: nleg
   integer                                                   :: nleg_new
   type(bond_type), dimension(:), allocatable                :: legm_new
   integer                                                   :: i
   integer                                                   :: n1,n2
!
   if (present(corr)) then
       lcorr = corr
   else
       lcorr = .false.
   endif
!
   nleg = numbonds(legm)
   if (nleg > 0) then
!
!      Ricrea la nuova connettivita' in leg_new
       call new_bonds(legm_new,nleg)
       nleg_new = 0
       do i=1,nleg
          n1 = legm(i)%n1
          n2 = legm(i)%n2
          if (all(veta /= n1) .and. all(veta /= n2)) then
              nleg_new = nleg_new + 1  ! incrementa num. di legami
!
!             correggi la numerazione, funzione utile se hai eliminato gli atomi in veta
              if (lcorr) then
                  legm_new(nleg_new)%n1 = n1 - count(veta < n1)
                  legm_new(nleg_new)%n2 = n2 - count(veta < n2)
                  legm_new(nleg_new)%dist = legm(i)%dist
                  legm_new(nleg_new)%sigma = legm(i)%sigma
                  legm_new(nleg_new)%ord = legm(i)%ord
              else
                  legm_new(nleg_new) = legm(i)
              endif
          endif
       enddo
       call resize_bonds(legm_new,nleg_new)
       call copy_bonds(legm,legm_new)
   endif
!
   end subroutine remove_bond_from_atom

!-------------------------------------------------------------------------------------------------

   subroutine connect_update_add(conn,leg)
!
!  Update connect adding bonds in leg
!
   USE arrayutil
   type(container_type), dimension(:), allocatable, intent(inout) :: conn
   type(bond_type), dimension(:), intent(in)                       :: leg
   integer :: i,nbmax
!
   nbmax = max(maxval(leg%n1),maxval(leg%n2))
   if (nbmax > size_array(conn)) then
       call resize_container(conn,nbmax)
   endif
   do i=1,size(leg)
      conn(leg(i)%n1)%nat = conn(leg(i)%n1)%nat + 1
      conn(leg(i)%n2)%nat = conn(leg(i)%n2)%nat + 1
      call push_back_array(conn(leg(i)%n1)%pos,leg(i)%n2)
      call push_back_array(conn(leg(i)%n2)%pos,leg(i)%n1)
   enddo
!
   end subroutine connect_update_add

!-------------------------------------------------------------------------------------------------
 
   subroutine bonds_update(legm,iord)
!
!  Aggiorna la numerazione dei legami. Se iord(i) e' zero il legame è rimosso.
!  new order = iord (old order)
!
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer, intent(in), dimension(:)                         :: iord
   integer                                                   :: n1,n2
   integer                                                   :: nleg
   integer                                                   :: nrem
   integer                                                   :: i
!   
   nleg = numbonds(legm)
   if (nleg > 0) then
       nrem = 0
       do i=1,nleg
          n1 = iord(legm(i)%n1)
          n2 = iord(legm(i)%n2)
          if (n1 == 0 .or. n2 == 0) then
              nrem = nrem + 1
              legm(i)%n1 = -100  ! marca il legame da eliminare
          else
              legm(i)%n1 = n1
              legm(i)%n2 = n2
          endif
       enddo
!
!      Ora rimuovi i legami marcati
       if (nrem > 0) then
           call remove_bondsv(legm,legm%n1,-100)
       endif
   endif
!
   end subroutine bonds_update

!-------------------------------------------------------------------------------------------------

   subroutine bonds_shift(legm,shift,kpos)
!
!  Applica shift al numero d'ordine per atomi maggiori di kpos
!
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer, intent(in)                                       :: shift
   integer, intent(in)                                       :: kpos
   integer                                                   :: i
!
   do i=1,numbonds(legm)
      if (legm(i)%n1 > kpos) legm(i)%n1 = legm(i)%n1 + shift
      if (legm(i)%n2 > kpos) legm(i)%n2 = legm(i)%n2 + shift
   enddo
!
   end subroutine bonds_shift

!-------------------------------------------------------------------------------------------------

   subroutine disconnect_atoms(atom,legm,vat)
!
!  Rimuovi tutti i legami tra vat e gli altri atomi ma non all'interno di vat, equivale
!  a disconnettere il frammento vat dal resto
!
   type(atom_type), dimension(:),              intent(in)    :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer, dimension(:), intent(in)                         :: vat
   integer                                                   :: i
   integer, dimension(size(atom))                            :: vat1
   integer                                                   :: nat
   integer                                                   :: n1,n2
   integer                                                   :: nleg
   integer                                                   :: nv1
!
   nleg = numbonds(legm)
   if (nleg > 0) then
       nat = size(atom)
!
!      Copia in vat1 gli atomi diversi da vat
       nv1 = 0
       do i=1,nat
          if (any(vat(:) == i))cycle
          nv1 = nv1 + 1
          vat1(nv1) = i
       enddo
!
!      Marca i legami tra vat e vat1
       do i=1,nleg
          n1 = legm(i)%n1
          n2 = legm(i)%n2
          if ((any(vat == n1) .and. any(vat1(:nv1) == n2)) .or. (any(vat1(:nv1) == n1) .and. any(vat == n2))) then
              legm(i)%n1 = 0   ! marca il legame mettendo n1=0
          endif
       enddo
!
!      elimina i legami marcati
       call remove_bondsv(legm,legm%n1,0)
   endif
!
   end subroutine disconnect_atoms

!-------------------------------------------------------------------------------------------------

   subroutine add_bonds(legm,atom,vet1,vet2,cell)
!
!  Aggiungi legami tra atomi in vet1 e vet2
   USE cgeom
   USE unit_cell
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   type(atom_type), dimension(:), intent(in)                 :: atom
   integer, dimension(:), intent(in)                         :: vet1
   integer, dimension(:), intent(in)                         :: vet2
   type(cell_type), intent(in)                               :: cell
   integer, dimension(size(vet1))                            :: vet11
   integer                                                   :: numleg
   integer                                                   :: nlegadd
   integer                                                   :: i
   integer                                                   :: nadd_init
   integer                                                   :: numleg_new
!
   numleg = numbonds(legm)
   nlegadd = 0
   nadd_init = size(vet1)
   vet11(:) = 1
   if (numleg > 0) then
!
!      marca i legami gia' esistenti e conta i legami da aggiungere
       do i=1,nadd_init
          if (bond_position(legm,vet1(i),vet2(i)) > 0) then
              vet11(i) = 0  ! marca il legame gia' esistente
          else
              nlegadd = nlegadd + 1
          endif
       enddo
   else
       nlegadd = nadd_init
   endif
   if (nlegadd > 0) then
       numleg_new = nlegadd + numleg
       call resize_bonds(legm,numleg_new)   ! espandi legm
!
!      Aggiungi i nuovi legami
       numleg_new = numleg
       do i=1,nadd_init
          if (vet11(i) > 0) then
              numleg_new = numleg_new + 1
              legm(numleg_new)%n1 = vet1(i)
              legm(numleg_new)%n2 = vet2(i)
              legm(numleg_new)%dist = distanzaC(atom(vet1(i))%xc,atom(vet2(i))%xc,cell%get_g())
              legm(numleg_new)%sigma = 0.3
          endif
       enddo
   endif

   end subroutine add_bonds

!-------------------------------------------------------------------------------------------------

   subroutine extract_bonds(legm,vet,lege,nlege)
!
!  Estrai i legami formati dagli atomi indicati nel vettore vet
!
   type(bond_type), dimension(:), intent(in)               :: legm
   integer, dimension(:), intent(in)                       :: vet
   type(bond_type), allocatable, dimension(:), intent(out) :: lege
   integer, intent(out)                                    :: nlege
   integer                                                 :: i,j
   integer                                                 :: nleg, nvet, pos
!
   nlege = 0
   nleg = size(legm)
   nvet = size(vet)
   if (nleg > 0 .and. nvet > 0) then
       allocate(lege(nleg))
       do i=1,nvet-1
          do j=i+1,nvet
             pos = bond_position(legm,vet(i),vet(j))
             if (pos > 0) then
                 nlege = nlege + 1
                 lege(nlege)%n1 = i
                 lege(nlege)%n2 = j
             endif
          enddo
       enddo
       call resize_bonds(lege,nlege)
   endif
!
   end subroutine extract_bonds

!-------------------------------------------------------------------------------------------------

   subroutine resize_bonds(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo bond_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(bond_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n
   logical, optional, intent(in)               :: savevet
   logical                                     :: savev
   integer                                     :: nv
   type(bond_type), allocatable                :: vsav(:)
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
!          nsav contiene qual � la porzione di vetr da salvare
           select case(nv-n)
             case (1:)       ! compatta x ad n
               nsav = n
             case (:-1)      ! espandi x ad n
               nsav = nv
             case (0)
               return        ! n=nv non fare niente
           end select
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
   end subroutine resize_bonds

!----------------------------------------------------------------------------------------------------

   subroutine clear_bonds(vetr)
!
!  Delete all bonds
!
   type(bond_type), allocatable, intent(inout) :: vetr(:)

   if (allocated(vetr)) deallocate(vetr)

   end subroutine clear_bonds

!-------------------------------------------------------------------------------------------------

   subroutine new_bonds(vetr,n)
!
!  Create new connect
!
   type(bond_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                               :: n

   if (n < 0) return
   if (numbonds(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_bonds

!-------------------------------------------------------------------------------------------------

   subroutine reallocate_angle(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo container_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(angle_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n
   logical, optional, intent(in)               :: savevet
   logical                                     :: savev
   integer                                     :: nv
   type(angle_type), allocatable                :: vsav(:)
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
!          nsav contiene qual � la porzione di vetr da salvare
           select case(nv-n)
             case (1:)       ! compatta x ad n
               nsav = n
             case (:-1)      ! espandi x ad n
               nsav = nv
             case (0)
               return        ! n=nv non fare niente
           end select
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
   end subroutine reallocate_angle

!-------------------------------------------------------------------------------------------------

   subroutine new_torsions(vetr,n)
!
!  Create new connect
!
   type(torsion_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                            :: n

   if (n < 0) return
   if (numtorsions(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_torsions

!--------------------------------------------------------------------------------------------

   subroutine resize_torsions(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo container_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(torsion_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                            :: n
   logical, optional, intent(in)                  :: savevet
   logical                                        :: savev
   integer                                        :: nv
   type(torsion_type), allocatable                :: vsav(:)
   integer                                        :: nsav
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
!          nsav contiene qual � la porzione di vetr da salvare
           select case(nv-n)
             case (1:)       ! compatta x ad n
               nsav = n
             case (:-1)      ! espandi x ad n
               nsav = nv
             case (0)
               return        ! n=nv non fare niente
           end select
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
   end subroutine resize_torsions

!---------------------------------------------------------------------      

   subroutine cycle_search(kat,tpos,conn,found)
!
!  Check if a closed path from kat to tpos exists
!
   USE arrayutil
   integer, intent(in)                             :: kat,tpos
   type(container_type), dimension(:), intent(inout) :: conn
   logical, intent(out)                          :: found
   integer :: j
!
   found = .false.
   call cycle_search_rec(kat,tpos,conn,found)
   do j=1,size(conn)
      if (conn(j)%nat > 0) then
          conn(j)%pos(:) = abs(conn(j)%pos(:))
      endif
   enddo
!
   end subroutine cycle_search

!---------------------------------------------------------------------      

   recursive subroutine cycle_search_rec(kat,tpos,conn,found)
   USE arrayutil
   integer, intent(in)                                :: kat,tpos
   type(container_type), dimension(:), intent(inout) :: conn
   logical, intent(inout)                             :: found
   integer                                            :: i,j
   integer                                            :: ffpos
!
   if (found) return
   do i=1,conn(tpos)%nat
      if (conn(tpos)%pos(i) <= 0) cycle
      if (conn(tpos)%pos(i) == kat) then
          found = .true.
          exit
      else
          ffpos = conn(tpos)%pos(i)
          conn(tpos)%pos(i) = -conn(tpos)%pos(i)
          do j=1,conn(ffpos)%nat
             if (conn(ffpos)%pos(j) == tpos) then
                 conn(ffpos)%pos(j) = -conn(ffpos)%pos(j)
                 exit
             endif
          enddo
          call cycle_search_rec(kat,ffpos,conn,found)
      endif
   enddo
!
   end subroutine cycle_search_rec

!---------------------------------------------------------------------      

   subroutine minpath_find(startat,endat,conn,minpath,nminstep)
!
!  Find the smallest path from startat to endat
!
   USE arrayutil
   integer, intent(in)                                :: startat, endat
   type(container_type), dimension(:), intent(inout) :: conn
   integer, dimension(:), intent(out)                 :: minpath
   integer, intent(out)                               :: nminstep
   integer, dimension(size(minpath))                  :: path
   integer                                            :: nstep
!
   nminstep = 0
   minpath(:) = 0
   nstep = 0
   path(:) = 0
   call minpath_find_rec(startat,endat,conn,path,nstep,minpath,nminstep)
   minpath(2:nminstep+1) = minpath(:nminstep)
   minpath(1) = startat
   nminstep = nminstep + 1
!
   end subroutine minpath_find

!---------------------------------------------------------------------      

   recursive subroutine minpath_find_rec(startat,endat,conn,path,nstep,minpath,nminstep)
   USE arrayutil
   integer, intent(in)                                :: startat, endat
   type(container_type), dimension(:), intent(inout) :: conn
   integer, dimension(:), intent(inout)               :: path,minpath
   integer, intent(inout)                             :: nstep,nminstep
   integer                                            :: i, j, pos, jpos
   !integer                                            :: kpr = 70
   integer, dimension(size(path))                     :: path1
   integer                                            :: nstep1
   integer, parameter                                 :: MAX_MEMBERS = 10  ! limit to maximum path size
!
   !if (kpr > 0) write(kpr,*)'start=',startat,' path in=',path(:nstep)
   !if (conn(startat)%nat > 2) write(kpr,'(a,i0)')'ramificazione su ',startat
   do i=1,conn(startat)%nat
      pos = conn(startat)%pos(i)
      if (pos <= 0) cycle
      if (pos == endat) then
          if (nminstep == 0 .or. nstep < nminstep) then
              nminstep = nstep
              minpath(:nminstep) = path(:nstep)
              !write(kpr,'(a,*(i4))')'*******************ciclo terminato:',path(:nstep)
          endif
          nstep = 0
          exit
      else
!!!          if (any(path(:nstep) == pos)) then
          if ((nstep > MAX_MEMBERS) .or. (nminstep > 0 .and. nstep > nminstep) .or. any(path(:nstep) == pos)) then
              !write(kpr,'(a,i0)')'*****ciclo fallito on at.',pos
              !write(kpr,'(a,*(i4))')'*****ciclo fallito:',path(:nstep)
              nstep = 0
              exit
          else
              nstep = nstep + 1
              path(nstep) = pos
              do j=1,conn(pos)%nat
                 if (conn(pos)%pos(j) == startat) then
                     conn(pos)%pos(j) = -conn(pos)%pos(j)
                     jpos = j
                 endif
              enddo
              path1(:nstep) = path(:nstep)
              nstep1 = nstep
              !write(kpr,'(a,i4,a,i4,a,*(i4))')'loop on',startat,' path new: start:',pos,' path1=',path1(:nstep1)
              call minpath_find_rec(pos,endat,conn,path1,nstep1,minpath,nminstep)
              conn(pos)%pos(jpos) = -conn(pos)%pos(jpos)
              nstep = nstep - 1
              !write(kpr,'(a,i4,a,i4,a,*(i4))')'loop on', startat,' path out: start:',pos, 'path=',path(:nstep)
          endif
      endif
   enddo
!
   end subroutine minpath_find_rec

!---------------------------------------------------------------------      

   subroutine bond_info(z1,z2,dist12,btype,dist,delta)
!
!  Get info about bond z1-z2 from bond table
!
   USE bondtmod
   integer, intent(in)         :: z1,z2    ! atomic number
   real, intent(in)            :: dist12   ! distance z1-z2
   integer, intent(out)        :: btype    ! bond order from bond table. 0 if bond is absent 
   real, intent(out)           :: dist     ! distance from bond table
   real, intent(out), optional :: delta    ! difference with tabulate value
   real                        :: bondmax, bondmin, dist_table
   real                        :: diff, diffmin
   integer                     :: k
!
   btype = 0
   diffmin = 100
   dist = 0
   if (z1 /= 0 .and. z2 /= 0) then
       do k=1,3
          bondmax = bond_table_max(k,z1,z2)
          if (bondmax > 0.000) then
              bondmin = bond_table_min(k,z1,z2)
              if (dist12 >= bondmin .and. dist12 <= bondmax) then
                  dist_table = bond_table(k,z1,z2)
                  diff = abs(dist_table-dist12)
                  if (diff < diffmin) then
                      btype = k
                      dist = dist_table
                      diffmin = diff
                  endif
              endif
          else
              exit
          endif
       enddo
   endif
!
   if (present(delta)) then
       if (btype > 0) then
           delta = diffmin
       else
           delta = 0
       endif
   endif
!
   end subroutine bond_info

!---------------------------------------------------------------------      

   integer function bond_type_from_table(legm,z1,z2)   result(btype)
!
!  Try to assign bond type using the connection table. Valid only for organic compounds
!
   USE bondtmod
   type(bond_type), intent(in) :: legm
   integer, intent(in)         :: z1,z2
   real                        :: bondmax, bondmin
   real                        :: dist, diff, diffmin
   integer                     :: k
   dist = legm%dist
   btype = 1
   diffmin = 100
   do k=1,3
      bondmax = bond_table_max(k,z1,z2)
      if (bondmax > 0.000) then
          bondmin = bond_table_min(k,z1,z2)
          if (dist >= bondmin .and. dist <= bondmax) then
              diff = abs(dist-bond_table(k,z1,z2))
              if (diff < diffmin) then
                  btype = k
                  diffmin = diff
              endif
          endif
      else
          exit
      endif
   enddo
!
   end function bond_type_from_table

!---------------------------------------------------------------------      

   logical function is_bond_equal_l(leg, legz, zval) 
!
!  Verify if legm is the type specified in legz
!
   type(bond_type), intent(in) :: leg, legz
   integer, dimension(:)       :: zval
   integer                     :: z1,z2
   z1 = zval(leg%n1)
   z2 = zval(leg%n2)
   is_bond_equal_l = (((z1 == legz%n1 .and. z2 == legz%n2) .or. (z2 == legz%n1 .and. z1 == legz%n2)) &
         .and. (leg%ord == legz%ord))
   end function is_bond_equal_l

!---------------------------------------------------------------------      

   logical function is_bond_equal_v(leg, vet, zval)
!
!  Verify n1 and n2 are both present in the array vet
!
   type(bond_type), intent(in) :: leg
   integer, dimension(:)       :: vet,zval
   integer                     :: z1,z2,i
   logical                     :: is_bond1,is_bond2
!
   z1 = zval(leg%n1)
   z2 = zval(leg%n2)

   is_bond1 = .false.
   is_bond2 = .false.
   do i=1,size(vet)
      if (.not.is_bond1) then
          if (vet(i) == z1) then
              is_bond1 = .true.
          endif
      endif
      if (.not.is_bond2) then
          if (vet(i) == z2) then
              is_bond2 = .true.
          endif
      endif
      is_bond_equal_v = is_bond1 .and. is_bond2
      if (is_bond_equal_v) return
   enddo
!
   end function is_bond_equal_v

!---------------------------------------------------------------------      

   logical function is_bond_order(leg,ord,z1,z2,tol)
!
!  Check if leg can be bond with order ord with tolerance tol
!
   USE bondtmod
   type(bond_type), intent(in) :: leg
   integer, intent(in)         :: ord
   integer, intent(in)         :: z1,z2
   real, intent(in)            :: tol
   real                        :: dist
   is_bond_order = .false.
   if (ord <= 3) then
       dist = bond_table(ord,z1,z2)
       if (dist > 0.0) then
           is_bond_order = abs(leg%dist - dist) <= tol
       endif
   endif
   end function is_bond_order

!---------------------------------------------------------------------      

   real function bond_distanceZ(z1,z2)  result(dist)
!
!  Search distance between atoms with Z number z1 and z2
!
   USE elements
   USE bondtmod
   integer, intent(in) :: z1,z2
   if (z1 == 0 .or. z2 == 0 .or. z1 > N_ELEMENTS .or. z2 > N_ELEMENTS) then
       dist = 0.0
   else
       dist = bond_table(1,z1,z2)
       if (dist == 0.0) then   ! distance absent on table: use covelent radius
           dist = eleminfo(z1)%c_radius + eleminfo(z2)%c_radius
       endif
   endif
!
   end function bond_distanceZ

!---------------------------------------------------------------------      

   subroutine get_atoms_legm(k,legm,connt,vet1,nat1,vet2,nat2,in_ring)
!
!  Extract the groups of atoms vet1 and vet2 connected by bond k, recognize bond in ring
!
   USE arrayutil
   integer, intent(in)                                          :: k
   type(bond_type), dimension(:), allocatable, intent(in)       :: legm
   type(container_type), dimension(:), allocatable, intent(in) :: connt
   integer, dimension(:), intent(out)                           :: vet1,vet2
   integer, intent(out)                                         :: nat1,nat2
   logical, intent(out)                                         :: in_ring
   integer :: i
!
   vet1(:) = 0
   nat1 = 1
   vet1(1) = legm(k)%n1
   call get_chain(connt,legm(k)%n1,legm(k)%n2,nat1,vet1)  ! chain in one direction

   vet2(:) = 0
   nat2 = 1
   vet2(1) = legm(k)%n2
   call get_chain(connt,legm(k)%n2,legm(k)%n1,nat2,vet2)  ! chain in another direction
!
!  Check if bond k is in ring
   in_ring = .false.
   do i=1,nat1
      if (any(vet1(i) == vet2(:nat2))) then
          in_ring = .true.
          exit
      endif
   enddo
!
   end subroutine get_atoms_legm

!---------------------------------------------------------------------      

   subroutine bond_distance_update_all(atom,legm,gmat)
!
!  Update bond distances for all bonds
!
   USE cgeom
   type(atom_type), dimension(:), intent(in)    :: atom
   type(bond_type), dimension(:), intent(inout), allocatable :: legm
   real, dimension(3,3), intent(in)             :: gmat
   integer                                      :: i
!
   do i=1,numbonds(legm)
      legm(i)%dist = distanzaC(atom(legm(i)%n1)%xc,atom(legm(i)%n2)%xc,gmat)
   enddo
!
   end subroutine bond_distance_update_all

!---------------------------------------------------------------------      

   subroutine bond_distance_update_at(kat,atom,legm,conn,gmat)
!
!  Update bond distances for atom kat
!
   USE cgeom
   USE arrayutil
   integer, intent(in)                          :: kat
   type(atom_type), dimension(:), intent(in)    :: atom
   type(bond_type), dimension(:), intent(inout) :: legm
   type(container_type), intent(in)            :: conn
   real, dimension(3,3), intent(in)             :: gmat
   integer                                      :: i,pos
!
   do i=1,conn%nat
      pos = bond_position(legm,kat,conn%pos(i))
      legm(pos)%dist = distanzaC(atom(kat)%xc,atom(conn%pos(i))%xc,gmat)
   enddo
!
   end subroutine bond_distance_update_at

!---------------------------------------------------------------------      

   subroutine bond_angle_update(kat,atom,angle,gmat)
!
!  Update bond angles for atom kat
!
   USE cgeom
   USE trig_constants
   integer, intent(in)                           :: kat
   type(atom_type), dimension(:), intent(in)     :: atom
   type(angle_type), dimension(:), intent(inout) :: angle
   real, dimension(3,3), intent(in)              :: gmat
   integer                                       :: i
!
   do i=1,size(angle)
      if (angle(i)%n2 == kat) then
          angle(i)%val = rtod*angleC(atom(angle(i)%n1)%xc,atom(angle(i)%n2)%xc,atom(angle(i)%n3)%xc,gmat) 
      endif
   enddo
!
   end subroutine bond_angle_update

!---------------------------------------------------------------------      

   subroutine order_atoms_in_bond(atom,bond)
   USE atom_basic
   USE elements
   USE nrutil, only: swap
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: bond
   integer                                                   :: i
!
   if (numatoms(atom) == 0) return
!
   do i=1,numbonds(bond)
      if (.not.order_is_ok(atom(bond(i)%n1)%z(),atom(bond(i)%n2)%z())) then !Sort Z: es. C-N but not N-C
          call swap(bond(i)%n1,bond(i)%n2)
      endif
   enddo
!
   end subroutine order_atoms_in_bond

!---------------------------------------------------------------------      

   subroutine sort_bonds(atom,bond)
!
!  Sort bonds according to the order number of atoms
!
   USE atom_basic
   USE strutil
   USE nr
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: bond
   integer, dimension(:), allocatable                        :: indx,icode
   integer                                                   :: numleg,natom
!
   numleg = numbonds(bond)
   natom = numatoms(atom)
   if (natom == 0 .or. numleg == 0) return
!
   allocate(indx(numleg),icode(numleg))
   if (atom(1)%asym > 0) then
!
!      symmetry applied: use the asymetric atom for sort
       icode(:) = natom*atom(bond(:)%n1)%asym + atom(bond(:)%n2)%asym
   else
       icode(:) = natom*bond(:)%n1 + bond(:)%n2
   endif
   call indexx(icode,indx)
   bond(:) = bond(indx(:))

   end subroutine sort_bonds

!---------------------------------------------------------------------      

   logical function ring_is_aromatic(ring,bonds)  result(is_ar)
   USE arrayutil
   type(container_type), intent(in)           :: ring
   type(bond_type), dimension(:), allocatable :: bonds
   integer                                    :: i,pos
!
   is_ar = .false.
   if (numbonds(bonds) == 0) return
   do i=1,ring%nat-1
      pos = bond_position(bonds,ring%pos(i),ring%pos(i+1))
      if (bonds(pos)%ord /= AR_SINGLE .and. bonds(pos)%ord /= AR_DOUBLE) return
   enddo
   is_ar = .true.
!
   end function ring_is_aromatic

!---------------------------------------------------------------------      

   subroutine order_type_bs(atom,bond)
!
!  Order element types in bond
!
   USE atom_basic
   USE elements
   USE nrutil, only: swap
   type(atom_type), dimension(:), intent(in) :: atom
   type(bond_type), intent(inout)            :: bond
!
   if (.not.order_is_ok(atom(bond%n1)%z(),atom(bond%n2)%z())) then !Sort Z: es. C-N but not N-C
       call swap(bond%n1,bond%n2)
   endif
!
   end subroutine order_type_bs

!---------------------------------------------------------------------      

   subroutine order_type_bv(atom,bond)
!
!  Order element types in bonds
!
   USE atom_basic
   type(atom_type), dimension(:), intent(in)                 :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: bond
   integer                                                   :: i
!
   do i=1,numbonds(bond)
      call order_type_bs(atom,bond(i))
   enddo
!
   end subroutine order_type_bv

!---------------------------------------------------------------------      

   subroutine order_type_av(atom,ang)
!
!  Order element types in bond angles
!
   USE atom_basic
   USE elements
   USE nrutil, only: swap
   type(atom_type), dimension(:), intent(in)                  :: atom
   type(angle_type), dimension(:), allocatable, intent(inout) :: ang
   integer                                                    :: i
!
   do i=1,numangles(ang)
      if (.not.order_is_ok(atom(ang(i)%n1)%z(),atom(ang(i)%n3)%z())) then !Sort Z: es. C-N but not N-C
          call swap(ang(i)%n1,ang(i)%n3)
      endif
   enddo
!
   end subroutine order_type_av

!-------------------------------------------------------------------------------------------------------      

   subroutine order_type_tv(atom,tors)
!
!  Order element types in torsion angles
!
   USE atom_basic
   USE elements
   USE nrutil, only: swap
   type(atom_type), dimension(:), intent(in)                    :: atom
   type(torsion_type), dimension(:), allocatable, intent(inout) :: tors
   integer                                                      :: i
!
   do i=1,numtorsions(tors)
      if (atom(tors(i)%n2)%z() == atom(tors(i)%n3)%z()) then     ! case: A-B-B-C
          if (.not.order_is_ok(atom(tors(i)%n1)%z(),atom(tors(i)%n4)%z())) then
              call swap(tors(i)%n2,tors(i)%n3)
              call swap(tors(i)%n1,tors(i)%n4)
          endif
      else
          if (.not.order_is_ok(atom(tors(i)%n2)%z(),atom(tors(i)%n3)%z())) then
              call swap(tors(i)%n2,tors(i)%n3)
              call swap(tors(i)%n1,tors(i)%n4)
          endif
      endif
   enddo
!
   end subroutine order_type_tv

!-------------------------------------------------------------------------------------------------------      

   subroutine find_duplicate_bonds_sym(atom,bond,nstartb,nb)
!
!  Find bonds equivalent for symmetry starting from nstartb, move them at the end of array
!
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: bond
   integer, intent(in)                                       :: nstartb  ! starting bond for check
   integer, intent(out)                                      :: nb       ! remaining number of bonds
   real, parameter                                           :: EPS = 10e-03
   integer                                                   :: i,j,ndel
   type(bond_type), dimension(:), allocatable                :: bondd
!
   ndel = 0
   nb = numbonds(bond)
   do i=nstartb,numbonds(bond)
      do j=1,i-1
         !if (bond(j)%n1 == DELETE_BOND) cycle
         if (bond(j)%n1 < 0) cycle
         if ((atom(bond(i)%n1)%asym == atom(bond(j)%n1)%asym .and. atom(bond(i)%n2)%asym == atom(bond(j)%n2)%asym) .or.   &
             (atom(bond(i)%n1)%asym == atom(bond(j)%n2)%asym .and. atom(bond(i)%n2)%asym == atom(bond(j)%n1)%asym)) then
             if (abs(bond(i)%dist - bond(j)%dist) <= EPS) then
                 ndel = ndel + 1
                 !write(0,'(i5,2(1x,a,1x,f0.5))')i,atom(bond(i)%n1)%glab(bond(i)%n1)//'-'//atom(bond(i)%n2)%glab(bond(i)%n2),bond(i)%dist,  &
                 !                            atom(bond(j)%n1)%glab(bond(j)%n1)//'-'//atom(bond(j)%n2)%glab(bond(j)%n2),bond(j)%dist
                 bond(i)%n1 = -bond(i)%n1 !DELETE_BOND
                 exit
             endif
         endif
      enddo
   enddo
   if (ndel > 0) then
!
!      Now move all bond with n1 < 0 at the end of array
       !write(0,*)'NDEL=',ndel,numbonds(bond)-nstartb+1,numbonds(bond),nstartb
       allocate(bondd(ndel))
       bondd = pack(bond,mask=bond%n1 < 0)
       bondd%n1 = -bondd%n1
       nb = numbonds(bond) - ndel
       bond(:nb) = pack(bond,mask=bond%n1 > 0)
       bond(nb+1:) = bondd
      ! call bond_delete_selected(bond)
   endif
!
   end subroutine find_duplicate_bonds_sym

!-------------------------------------------------------------------------------------------------------      

   subroutine find_duplicate_angles_sym(atom,angle,nang)
!
!  Find angles equivalent for symmetry, move them at the end of array
!
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(angle_type), dimension(:), allocatable, intent(inout) :: angle
   integer, intent(out)                                      :: nang       ! remaining number of angles
   real, parameter                                           :: EPS = 10e-02
   integer                                                   :: i,j,ndel,nasym
   integer, dimension(3) :: na
   type(angle_type), dimension(:), allocatable :: ang
!
   ndel = 0
   nang = numangles(angle)
   nasym = maxval(atom%asym)
!corr   write(70,*)'NASYM=',nasym,nang
!corr   call print_connect(atom%lab,angle=angle,kpri=70)
   do i=1,nang
      na = [angle(i)%n1,angle(i)%n2,angle(i)%n3]
      if (any(na > nasym)) then
          do j=1,i-1
             if (angle(j)%n1 < 0) cycle
             if (atom(na(2))%asym == atom(angle(j)%n2)%asym .and. &
                 ((atom(na(1))%asym == atom(angle(j)%n1)%asym .and. atom(na(3))%asym == atom(angle(j)%n3)%asym) .or.  &
                  (atom(na(3))%asym == atom(angle(j)%n1)%asym .and. atom(na(1))%asym == atom(angle(j)%n3)%asym)).and. &
                   abs(angle(i)%val - angle(j)%val) <= EPS) then
!corr                 write(70,'(a,i3,1x,a,1x,a,1x,a,f10.3)')'A1:',i,atom(na(1))%glab(),atom(na(2))%glab(),atom(na(3))%glab(),angle(i)%val
!corr                 write(70,'(a,i3,1x,a,1x,a,1x,a,f10.3)')'A2:',j,atom(angle(j)%n1)%glab(),atom(angle(j)%n2)%glab(),atom(angle(j)%n3)%glab(),angle(j)%val
                 ndel = ndel + 1
                 angle(i)%n1 = -angle(i)%n1
!corr                 write(70,*)'angle n.',i,' was deleted'
                 exit
             endif
          enddo
      endif
   enddo
   if (ndel > 0) then
!
!      Now move all angle with n1 < 0 at the end of array
       allocate(ang(ndel))
       ang = pack(angle,mask=angle%n1 < 0)
       ang%n1 = -ang%n1
       nang = numangles(angle) - ndel
       angle(:nang) = pack(angle,mask=angle%n1 > 0)
       angle(nang+1:) = ang
   endif
!corr   call print_connect(atom%lab,angle=angle,kpri=70)
!
   end subroutine find_duplicate_angles_sym

!-------------------------------------------------------------------------------------------------------      

   logical function is_valid_torsion(tors,bond)  result(is_valid)
   integer, dimension(4), intent(in)                      :: tors
   type(bond_type), dimension(:), allocatable, intent(in) :: bond
!
   is_valid = .false.
   if (numbonds(bond) == 0) return
   if (bond_position(bond,tors(1),tors(2)) == 0) return
   if (bond_position(bond,tors(2),tors(3)) == 0) return
   if (bond_position(bond,tors(3),tors(4)) == 0) return
   is_valid = .true.
!
   end function is_valid_torsion

END MODULE connect_mod     
