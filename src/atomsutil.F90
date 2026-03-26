MODULE atom_type_util
!
! Questo modulo contiene un insieme di routine per gestire operazioni con variabili di tipo atom_type
!
! F function natom_cell                                           number of atoms in cell
! F numatomspec(atom,zval or spec or el)                          numero di atomi di una data specie
! F number_for_specie(z,zval,nzval) result(num)                   Numero di specie con Z=z
! S get_atoms_of_specie(pxen,atom,vat,natom)                      Extract atoms of the same specie in a array
! S get_atoms_of_z(zval,atom,vat,natom)                           Extract atoms of with the same z
! S get_atom_of_label(strlab,atom,kat)                            Extract the first atom with label equal to strlab
! S get_atoms_of_label(strlab,atom,vat,natom)                     Extract atoms with label equal to strlab
! S get_atoms_of_string(str,atom,vat,natom)                       Extract atoms with label compatible with string str (e.g., C1, C*, C**, *)
! F is_atomic_specie(atom,spec)                                   atom è di tipo spec?
! F is_hydrogen(atom)
! F number_of_hydrogens(atom)
! S elements_from_atom(atom)                                      Extract elements from atoms
! S compute_content(atom,elem,spg)                                Compute content from atoms
! F is_present_sd(atom)                                           Check if atom has sd on coordinates
! F Z_value(atom,nsym,chform)                                     Compute Z value and chemical formula from asu and symmetry
! S formula_from_atom(atom,elem)                                  Extract formula from atom
! S xyz_to_atom(atom,xco,bf,occr,occh,nz,label,inte)              carica modello strutturale in atom
! S frac_to_cart(at)                                              conversione di atomi atm da coordinate frazionarie a cartesiane
! S frac_to_cart_copy(atom,atomcart,orto)                         converti atom in coordinate cartesiane copiandolo in atomcart
! F cartesian_coord(atom) result(atomc)                           conversione da cristallografiche a cartesiane
! F xyz_cart(atom,cell) result(xc)                                compute xyz in cartesian
! S cart_to_frac(at)                                              conversione di atomi atm da coordinate cartesiane a frazionarie
! S translate_atoms(atmnew,xtra)                                  trasla atmold in atmnew
! S fillatom(atom)                                                trasferisci atom in variabili expo 
! S atom_to_xyzs(atom)                                            trasferisci atom e nats in variabili expo
! S bonds_to_tors(legm,atom,tors,cart)                             genera torsioni
! S add_atoms_to_list(atom,atomadd,natnew,icon1,icon2)            combina 2 modelli atomici
! S add_atom(atom,atomadd,legm,leg)                               aggiungi un atomo alla lista
! S combine_iconn(icontot,icon1,icon2,nat1)                       combina 2 connettivita
! S calcola_occ(atom,modcoor)                                     calcola occ. per atom
! S set_biso(atom)                                                assegna biso ad atom
! S set_specie(kat,atom,kel,elem)                                 Assign element kel of array at atom kat
! S print_atoms(at,height,title,write_occ)                        stampa atom
! S save_structure_bin(unitbin,atom,legm)                         save structure model on binary file
! S read_structure_bin(unitbin,atom,legm)                         read structure model from binary file
! S apply_sym_oper(k,atom)                                        applica operatore k ad atom 
! F atom_symm(k,atom)                                             applica operatore k ad atom
! S make_tetrahedron(atom,dist,cell,xpos,kcentri)                 crea un tetraedro in coordinate cartesiane
! S make_octahedron (atom,dist,cell,xpos,kcentri)                 crea un ottaedro in coordinate cartesiane
! S make_square (atom,dist,cell,xpos,kcentri)                     make square plane
! S make_triangle(atom, dist, cell, xpos, leg)                    make triangle
! S make_cube(atom, dist, cell, xpos)                             make cube
! S make_anti_prism_tetragonal(atom, dist, cell, xpos)            make anti prism tetragonal
! S make_prism_trigonal(atom, dist, cell, xpos)                   make prism tetragonal
! S make_icosahedron(atom, dist, cell, xpos)                      make anti prism tetragonal
! S create_connectivity_special(atom,dist,legm,esd,z1,z2)         cerca legami fra atomi di specie z1 e z2 a distanza dist
! S check_angle(atom,legm,angl,kpr)                               rimuove angoli minori di angl
! S translate_in_cell(atom)                                       trasla in cella in gruppo di atomi
! F is_bar_out_of_cell(atom)                                      controlla se il baricentro e' fuori cella
! F is_out_of_cell(atom)                                          controlla se tutti gli atomi sono fuori cella
! F is_out_of_range(atom,inf,sup,bar)                             Atoms fuori dal range?
! F baricentro(at)                                                calcola il baricentro di at
! S get_info_distance(atom1,atom2,pos,dist,errd)                  Info sulla distanza tra 2 atomi
! F bond_distance(...)  result(dist)                              Search distance between atoms with Z number z1 and z2
! F bond_distance_max(z1,z2)                                      Assign the maximum distance
! F bond_distance_min(z1,z2)                                      Assign the minimum distance
! S get_radius_molecule(atom,radmax,bar)                          Assimila la molecola ad una sfera e calcola il suo raggio
! S save_atoms(atoms)                                             genera copia degli atomi
! S restore_atoms(atoms)                                          ripristina copia degli atomi
! S clear_saved_atoms()                                           cancella la copia degli atomi
! S copy_atoms(atom1,atom2)                                       Copia atom2 in atom1
! S remove_atoms_from_list(atom,veta,val,legm,iord)               Cancella dalla lista tutti gli atomi per i quali vet e' uguale a val
! S remove_atoms_sym(atom,atoms,legm,legms,symm,veta,iord)        Rimuovi gli atomi in vet con gli equivalenti
! S remove_atoms_vet(atom,legm,veta,iorder,keep)                  Remove atoms in array veta. if keep is true only atoms in vet are kept
! S delete_ghosts(atom)                                           Delete ghost atoms (ptab=nz=0)
! F integer function is_organic(zval,nzval)                       Riconosce se un composto è organico
! S sort_by_specie(atom,legm)                                     Ordine atomi in base alla specie
! S force_connectivity(atom,legm,kat)                             Forza la connettivita' per l'atomo kat
! S connect_groups(atom1,atom2,legm)                              Cerca connettivita' tra 2 gruppi di atomi
! S renumber_atoms(atom,veta,string,iser)                         Renumber of atoms
! S get_minimum_distance(at,atom,kmin,dmin)                       Distanza minima di at dagli atomi in atom in cartesiane
! S get_minimum_distance_sym(at,atom,cell,spg,kmin,dmin,xeqmin)   Minimum distance of at from array atom considering symmetry
! S get_atoms_distance(at,atom,vetd,distv,vexcl)                  Calcola distanza di at dagli atomi atom (in coord. cartesiane)
! S get_distance_radius                                           Trova gli atomi a distanza dist da kat
! S atom_string(atom,vet)                                         Crea stringa degli atomi dalla specie chimica
! S add_to_content(zvs,nvs)                                       Aggiungi nuove specie al contenuto
! F is_in_content(zspec)                                          La specie chimica con numero atomico zspec e' nel contenuto?
! S apply_symmetry_legm(atoms,legm,legms)                         Applica la simmetria ai legami
! S expand_contacts(atomasym,atom,legm)                           Espandi i contatti di atom
! S duplicate_atoms(atom,legm,vet)                                Duplicate selected atoms with random traslation
! F operator_code(kop,tra) result(opcode)                         Genara codice per memorizzare la rotazione e la traslazione
! F tra_opcode(opcode)                                            Estrai la traslazione dal codice
! S get_limit_translation(opcode,ktramin,ktramax)                 Individua la min e la max traslazione
! S init_infos(atoms)                                             inizializza informazioni sulla simmetria
! F function checkeq_symm(atoms,asym,op) result(eq)               Check equality of op and asym
! F function any_op_equal(atoms,op)   result(eq)                  Check equality of op
! S init_for_symm(atom,legm,atoms,legms)                          Inizialize symmetry by copying asymmetric unit
! S find_sssr(atom,legm)                                          Find SSSR from a connection table. 
! F ring_planarity(atom,ring)                                     Determine wheter ring is planar or not.
! F ring_planarity_estimate(atom,ring) result(rps)                Estimate the planarity of ring
! S coord_in_newcell                                              Trasporta le coord. frazionarie in altra cella
! F Angle_Dihedral(ri,rj,rk,rn)                                   Calcola l'angolo di torsione definito dagli atomi ri,rj,rk,rl
! F rotation_matrix(l,m,n,theta)                                  Matrice di rotazione intorno ad un asse
! S rand_rotate_atoms(atom,xrot)                                  Ruota in modo random un insieme di atomi intorno al baricentro   
! S rotate_atoms(atmr,pp1,pp2,theta)                              Ruota di theta un insieme di atomi intorno ad un asse pp2-pp1
! S Get_Cartesian_from_Z(ci,ri,rj,rk,rn)                          Usata per conversione da Zmatrix a cartesiane
! F serial_number(atom) result(kser)                              Extract the serial number of an atom
! S remove_duplicate_labels(atom)                                 Remove duplicate labels
! F is_duplicated_label(atom,k)                                   For the label of atom k check if the label is duplicated
! S get_atoms_from_string(string,atom,vatom,val,defval,err)       Read string containing list of atoms and an optional numeric value
! F max_serial_number(atom,lab,prefix) result(maxserial)          Find the maximum serial number for a specified prefix of label
! F density_value(mw,vcell,nop)                                   Compute density
! F is_metal(atom)                                                true if the atom is metal
! F any_is_metal(atom)                                            true if some atom is metal
! F all_is_metal(atom)                                            true if all atoms are metal
! F volume_per_atom                                               Compute volume per atom
! F molecular_volume(atom)                                        Compute volume of molecule
! F number_of_molecule(atom,cell)  result(Z)                      Compute number of molecules from cell volume
! F linear_abs_coeff(atom,cell,nop)   result(mu)                  Compute the linear absorption coefficient
! S find_duplicate_atoms(atom,legm,cell,spg,dist,vet,nd,kpr)      Find duplicate atoms in a distance dist
! F is_duplicated_atoms(atom,legm,cell,spg,dist,kpr)              Check for duplicated atoms
! S change_bond_distance(atom,legm,k,dist,xgg)                    Change bond distance of bond k
! S change_bond_angle(k,angv,atom,legm,aval,cell)                 Change bond angle of angle k
! S change_torsion_angle(tors,atom,legm,tval,cell,changed)        Rotate torsion angle
! F norm_torsion(ang)                                             Normalize torsion angle between -180 and 180
! S copy_atoms_sym(atoms,legms,symm,kop,symop,tcell,...)          Copy atoms applying the sym operator symop and tcell. Duplicate atoms are not removed
! F lsq_conditions(atom,spg,cell,iser) result(code)               Atom in fixed position beacuse of special positions or polar axis
! S resize_atoms(vetr,n,savevet)                                  Resize array of atoms
! S new_atoms(vetr,n)                                             Create new atoms
! S push_back_atom(arr,val)                                       Adds a new atom at the end of the array
! S clear_atoms(vetr)                                             Delete all atoms
! S set_specie_atoms(atom,nz,elem,ini,fin)                        Set specie from nz
! S specie_from_el(atom,pos,elem,ini,fin)                         Set specie from element type
! S specie_from_ptab(atom,elem)                                   Ptab has assigned, set specie
! S Zmatrix_to_Cartesian(na,I_coor,conn)
! S cartesian_to_zmatrix(atom,legm,I_coor,conn)
! S make_random(atom)                                             Make random coordinates
! F function origin_for_rotation(conn,pos)  result(or)            Find origin for rotations for atoms specified in array pos
! S make_specie(atom,spg,elem,radtype)                            Assign specie from peak intensity
! F str_symop(op,spg)                                             Convert symmetry operator op_type in a string
! S get_atom_site(atom,site)                                      Extract site info from asymmetric unit
! F get_charge_el(atom,elem)                                      Get charge using array elem. See also atomic_charge 
! F s_get_atomlist(atom,vat,nat,sep,vmask) result(str)            Generate sorted list of atom types with separator and mask
!

USE atom_basic

implicit none

type chem_group_t
  integer               :: geom  ! number of atoms bound to center
  integer               :: zc    ! Z of center
  integer, dimension(4) :: za    ! Z of other atoms
  integer, dimension(4) :: btype ! bond type
  integer, dimension(4) :: cval  ! max number of bond for other atoms
end type chem_group_t

private formula_from_atom_elem

private is_organic_z, is_organic_atm

type(atom_type), dimension(:), allocatable :: atoms_copy

private natom_cell_el, natom_cell_at
interface natom_cell
  module procedure natom_cell_el, natom_cell_at
end interface

interface atom_symm
  module procedure atom_symm_scalar, atom_symm_scalar_xyz, atom_symm_vet
end interface

interface numatomspec
  module procedure numatomspec_s, numatomspec_z, numatomspec_el
end interface

interface is_atomic_specie
  module procedure is_atomic_specie_ss, is_atomic_specie_sv,     &
                   is_atomic_speciez_ss, is_atomic_speciez_vs, is_atomic_speciez_vv
end interface

interface operator(==)
  module procedure equal_atom
end interface

interface operator(+)
  module procedure add_xyz
end interface

interface bond_distance
  module procedure bond_distanceZ, bond_distanceAT
end interface

interface volume_per_atom
  module procedure volume_per_atom_cont, volume_per_atom_at, volume_per_atom_elem
end interface 

interface is_organic
  module procedure is_organic_z, is_organic_atm
end interface 

private :: is_hydrogen_s, is_hydrogen_v, is_hydrogen_el
interface is_hydrogen
  module procedure is_hydrogen_s, is_hydrogen_v, is_hydrogen_el
end interface 

private :: fractional_coord_a_o
interface fractional_coord
  module procedure fractional_coord_a_o
end interface

private :: cartesian_coord_a_o
interface cartesian_coord
  module procedure cartesian_coord_a_o
end interface

!corr private :: apply_sym_oper_kmat, apply_sym_oper_s, apply_sym_oper_s_scalar
private :: apply_sym_oper_s, apply_sym_oper_s_scalar
interface apply_sym_oper
  module procedure apply_sym_oper_s, apply_sym_oper_s_scalar
end interface

private :: get_atoms_from_string_vet
interface get_atoms_from_string
  module procedure get_atoms_from_string_vet
end interface

private :: compute_doc_all, compute_doc_vet
interface compute_doc
   module procedure compute_doc_all, compute_doc_vet
end interface

private :: frac_to_cart_orto, frac_to_cart_cell
interface frac_to_cart
   module procedure frac_to_cart_orto, frac_to_cart_cell
end interface

private :: cart_to_frac_orto, cart_to_frac_cell
interface cart_to_frac
   module procedure cart_to_frac_orto, cart_to_frac_cell
end interface

private :: translate_atoms_s, translate_atoms_v
interface translate_atoms
   module procedure translate_atoms_s, translate_atoms_v
end interface

private :: translate_in_cells, translate_in_cellv
interface translate_in_cell
   module procedure translate_in_cells, translate_in_cellv
end interface

private :: is_linear_x3, is_linear_x4
interface is_linear
   module procedure is_linear_x3, is_linear_x4
end interface

real, parameter :: ANGLIM_METAL = 30

CONTAINS

   logical function equal_atom(atom1,atom2)
   type(atom_type), intent(in) :: atom1,atom2
   real, parameter             :: EPS = epsilon(1.0)
   equal_atom = all(abs(atom1%xc - atom2%xc) <= EPS)
   end function equal_atom

!----------------------------------------------------------------------

   function add_xyz(atom,xyz)
   type(atom_type), intent(in)    :: atom
   real, dimension(3), intent(in) :: xyz
   type(atom_type)                :: add_xyz
   add_xyz%xc = atom%xc + xyz
   end function add_xyz

!----------------------------------------------------------------------

   real function natom_cell_el(elem,excludeH)  result(nat)
!
!  Number of atoms in cell
!
   USE elements
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   logical, intent(in), optional                             :: excludeH
   logical                                                   :: excludeHat
!
   if (numelem(elem) == 0) then
       nat = 0
       return
   endif
!
   if (present(excludeH)) then
       excludeHat = excludeH
   else
       excludeHat = .false.
   endif
   if (excludeHat) then
       nat = sum(elem%nw,mask=elem%z /= H_at)
   else
       nat = sum(elem%nw)
   endif
!
   end function natom_cell_el

!----------------------------------------------------------------------

   real function natom_cell_at(atom,nsymt,excludeH) result(natc)
!
!  Number of atoms in cell
!
   USE elements
   type(atom_type), dimension(:), allocatable :: atom
   integer, intent(in)                        :: nsymt
   logical, intent(in), optional              :: excludeH
   logical                                    :: excludeHat
!
   natc = 0
   if (numatoms(atom) == 0) return
!
   if (present(excludeH)) then
       excludeHat = excludeH
   else
       excludeHat = .false.
   endif
   if (excludeHat) then
       natc = nsymt*sum(atom%ocry*atom%och,mask=atom%z() /= H_at)
   else
       natc = nsymt*sum(atom%ocry*atom%och)
   endif
!
   end function natom_cell_at 

!----------------------------------------------------------------------

   integer function numatomspec_z(atom,zval)   result(nums)
!
!  Numero di atomi con Z = zval
!
   USE atom_basic
   type(atom_type), dimension(:), allocatable :: atom
   integer, intent(in)                        :: zval
   if (numatoms(atom) > 0) then
       nums = count(atom%z() == zval)
   else
       nums = 0
   endif
!
   end function numatomspec_z

!----------------------------------------------------------------------

   integer function numatomspec_s(atom,spec)   result(nums)
!
!  Numero di atomi di tipo spec
!
   type(atom_type), dimension(:), allocatable :: atom
   character(len=*), intent(in)               :: spec
   integer                                    :: nat
   integer                                    :: i
!
   nat = numatoms(atom)
   if (nat > 0) then
       nums = 0
       do i=1,nat
          if (is_atomic_specie(atom(i),spec)) then
              nums = nums + 1
          endif
       enddo
   else
       nums = 0
   endif
!
   end function numatomspec_s

!----------------------------------------------------------------------

   integer function numatomspec_el(atom,elem)   result(nums)
!
!  Numero di atomi con Z = zval
!
   USE elements
   type(atom_type), dimension(:), allocatable :: atom
   type(element_type), intent(in)             :: elem
!
   if (numatoms(atom) > 0) then
       nums = count(atom%ptab == elem%ptab)
   else
       nums = 0
   endif
!
   end function numatomspec_el

!----------------------------------------------------------------------

   integer function number_for_specie(z,zval,nzval) result(num)
!
!  Numero di specie con Z=z
!
   integer, intent(in)               :: z ! specie di cui si vuole conoscere il numero
   integer, dimension(:), intent(in) :: zval,nzval
   integer                           :: i
!
   num = 0
   do i=1,size(zval)
      if (zval(i) == z) then
          num = nzval(i)
          exit
      endif
   enddo
!
   end function number_for_specie

!---------------------------------------------------------------------      

   subroutine get_atoms_of_specie(pxen,atom,vat,natom)
!
!  Extract atoms of the same specie in a array
!
   integer, intent(in)                         :: pxen   ! specie
   type(atom_type), dimension(:), intent(in)   :: atom   ! atoms
   integer, dimension(size(atom)), intent(out) :: vat    ! array of atoms of pxen
   integer, intent(out)                        :: natom  ! number of atoms in vat
   integer :: i
!
   natom = count(atom%ptab == pxen)
   if (natom > 0) then
       vat(:natom) = pack((/(i,i=1,size(atom))/),mask = atom%ptab == pxen)
   endif
!
   end subroutine get_atoms_of_specie

!---------------------------------------------------------------------      

   subroutine get_atoms_of_z(zval,atom,vat,natom)
!
!  Extract atoms of with the same z
!
   integer, intent(in)                         :: zval   ! specie
   type(atom_type), dimension(:), intent(in)   :: atom   ! atoms
   integer, dimension(size(atom)), intent(out) :: vat    ! array of atoms of zval
   integer, intent(out)                        :: natom  ! number of atoms in vat
   integer :: i
!
   natom = count(atom%z() == zval)
   if (natom > 0) then
       vat(:natom) = pack((/(i,i=1,size(atom))/),mask = atom%z() == zval)
   endif
!
   end subroutine get_atoms_of_z

!---------------------------------------------------------------------      

   subroutine get_atom_of_label(strlab,atom,kat)
!
!  Extract the first atom with label equal to strlab
!
   USE strutil
   character(len=*), intent(in)                           :: strlab ! label
   type(atom_type), dimension(:), allocatable, intent(in) :: atom   ! atoms
   integer                                                :: i,kat
!
   kat = 0
   do i=1,numatoms(atom)
      if (s_eqi(atom(i)%lab,strlab)) then
          kat = i
          return
      endif
   enddo
!
   end subroutine get_atom_of_label

!---------------------------------------------------------------------      

   subroutine get_atoms_of_label(strlab,atom,vat,natom)
!
!  Extract atoms with label equal to strlab
!
   USE strutil
   character(len=*), intent(in)                           :: strlab ! label
   type(atom_type), dimension(:), allocatable, intent(in) :: atom   ! atoms
   integer, dimension(size(atom)), intent(out)            :: vat    ! array of atoms of pxen
   integer, intent(out)                                   :: natom  ! number of atoms in vat
   integer                                                :: i
!
   natom = 0
   do i=1,numatoms(atom)
      if (s_eqi(atom(i)%lab,strlab)) then
          natom = natom + 1
          vat(natom) = i
      endif
   enddo
!
   end subroutine get_atoms_of_label

!---------------------------------------------------------------------      

   subroutine get_atoms_of_string(str,atom,vat,natom)
!
!  Extract atoms with label compatible with string str (e.g., C1, C*, C**, *, **)
!
   USE strutil
   USE elements
   character(len=*), intent(in)                              :: str   ! specie
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom  ! atoms
   integer, dimension(size(atom)), intent(out)               :: vat   ! array of atoms of pxen
   integer, intent(out)                                      :: natom ! number of atoms in vat
   integer                                                   :: i,el
   character(len=len_trim(str))                              :: str1
!
   natom = 0
   if (numatoms(atom) == 0 .or. len_trim(str) == 0) return
!
!  Find str in label of atoms
   call get_atoms_of_label(str,atom,vat,natom)
   if (natom > 0) return
!
!  Find str in species
   str1 = str
   call s_trim_char(str1,'*')
   if (len_trim(str1) == 0) then ! case str='*' or '**', etc.
       natom = numatoms(atom)
       vat = [(i,i=1,natom)]
       return
   endif
!corr   el = is_element(elem,str1)    ! case ex. Ca*, C*
   el = pxen_from_specie(str1)    ! case ex. Ca*, C*
   if (el > 0) then
       !call get_atoms_of_specie(elem(el)%ptab,atom,vat,natom)
       call get_atoms_of_specie(el,atom,vat,natom)
       if (natom > 0) return
   endif
   el = z_from_specie(str1)      ! force get in case ex. O* but str1=O2-
   if (el > 0) then
       call get_atoms_of_z(el,atom,vat,natom)
   endif
!
   end subroutine get_atoms_of_string

!---------------------------------------------------------------------      

   subroutine get_param_of_string(str,atom,kpar,vat,natom,err)
!
!  Extract param from string. Format: par(label) or par(number)
!
   USE strutil
   USE errormod
   character(len=*), intent(in)                              :: str   ! specie
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom  ! atoms
   integer, intent(out)                                      :: kpar  ! array of parameters
   integer, dimension(size(atom)), intent(out)               :: vat   ! array of atoms of pxen
   integer, intent(out)                                      :: natom ! number of atoms in vat
   type(error_type), intent(out)                             :: err
   integer :: posb,posb2,lens,ival
!
   kpar = 0
   natom = 0
   lens = len_trim(str)
   if (lens < 4 .or. numatoms(atom) == 0) go to 10
   posb = index(str,'(')
   if (posb < 1 .or. posb > lens-2) go to 10
   posb2 = index(str,')')
   if (posb2 < posb) go to 10
!
!  Read parameter
   select case(lower(str(:posb-1)))
     case ('xyz')
      kpar = 1
     case ('b')
      kpar = 4
     case ('occ')
      kpar = 5
   end select
!
!  Read atom in brackets
   if (s_is_i(str(posb+1:lens-1),ival)) then     ! atom as number es. occ(1)
       if (ival > 0 .and. ival < numatoms(atom)) then
           natom = 1
           vat(natom) = ival
       else
           go to 10
       endif
   else                                          ! atom as label es. occ(Ca1), B(O*)
       call get_atoms_of_string(str(posb+1:lens-1),atom,vat,natom)
       if (natom == 0) go to 10
   endif
   return
!
10 call err%set('Error reading parameter '//trim(str))
!
   end subroutine get_param_of_string

!---------------------------------------------------------------------      

   logical function is_atomic_specie_ss(atom,spec) 
!
!  atom è di tipo spec?
!
   USE strutil
   type(atom_type), intent(in) :: atom
   character(len=*)            :: spec
!
   is_atomic_specie_ss = s_eqidb(spec,atom%specie())
!
   end function is_atomic_specie_ss

!----------------------------------------------------------------------

   logical function is_atomic_specie_sv(atom,spec)
!
!  The atomic number of atom is in the array spec
!
   USE strutil
   type(atom_type), intent(in)                :: atom
   character(len=*), dimension(:), intent(in) :: spec
   integer                                    :: i
!
   is_atomic_specie_sv = .false.
   do i=1,size(spec)
      is_atomic_specie_sv = s_eqidb(spec(i),atom%specie())
      if (is_atomic_specie_sv) return
   enddo
!
   end function is_atomic_specie_sv

!----------------------------------------------------------------------

   logical function is_atomic_speciez_ss(atom,zspec)
!
!  atomic number of atom is z?
!
   USE atom_basic
   type(atom_type), intent(in) :: atom
   integer, intent(in)         :: zspec
!
   is_atomic_speciez_ss = ( atom%z() == zspec )
!
   end function is_atomic_speciez_ss

!----------------------------------------------------------------------

   logical function is_atomic_speciez_vs(atom,zspec)
!
!  atomic number of atom is z?
!
   USE strutil
   USE atom_basic
   type(atom_type), intent(in), dimension(:) :: atom
   integer, intent(in)                       :: zspec
!
   is_atomic_speciez_vs =  any(atom%z() == zspec)
!
   end function is_atomic_speciez_vs

!----------------------------------------------------------------------

   logical function is_atomic_speciez_vv(atom,zspec)
!
!  Just one of the species in the array zspec is in the molecule
!
   USE strutil
   type(atom_type), intent(in), dimension(:) :: atom
   integer, dimension(:), intent(in)         :: zspec
   integer                                   :: i
!
   is_atomic_speciez_vv = .false.
   do i=1,size(zspec)
      is_atomic_speciez_vv = is_atomic_speciez_vs(atom,zspec(i))
      if (is_atomic_speciez_vv) return
   enddo
!
   end function is_atomic_speciez_vv

!----------------------------------------------------------------------

   logical function is_hydrogen_s(atom)
   USE elements
   type(atom_type), intent(in) :: atom
   is_hydrogen_s = is_atomic_specie(atom,H_at)
   end function is_hydrogen_s

!----------------------------------------------------------------------

   logical function is_hydrogen_v(atom)
   USE elements
   type(atom_type), dimension(:), intent(in) :: atom
   is_hydrogen_v = is_atomic_specie(atom,H_at)
   end function is_hydrogen_v

!----------------------------------------------------------------------

   logical function is_hydrogen_el(elem)
   USE elements
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   is_hydrogen_el = is_element(elem,H_at) > 0
   end function is_hydrogen_el

!----------------------------------------------------------------------

   integer function number_of_hydrogens(atom)
   USE elements
   USE atom_basic
   type(atom_type), dimension(:), intent(in) :: atom
   number_of_hydrogens = count(atom%z() == H_at)
   end function number_of_hydrogens

!----------------------------------------------------------------------

   subroutine elements_from_atom(atom,elem)
!
!  Extract elements from atoms
!
   USE elements  
   type(atom_type), dimension(:), intent(in) :: atom
   type(element_type), dimension(:), allocatable :: elem
!
   call elem_from_atoms(elem,atom(:)%ptab)
!
   end subroutine elements_from_atom

!----------------------------------------------------------------------

   subroutine compute_content(atom,elem,spg)
!
!  Compute content (variable nw) from atoms
!
   USE elements  
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(element_type), dimension(:), allocatable          :: elem
   type(spaceg_type), intent(in)                          :: spg
   integer                                                :: i,ks
!
   elem%nw = 0.
   do i=1,numatoms(atom)
      ks = atom(i)%kscatt()
      if(ks == 0) cycle
      elem(ks)%nw = elem(ks)%nw + atom(i)%och*atom(i)%ocry*spg%nsymop
   enddo
!
   end subroutine compute_content

!----------------------------------------------------------------------

   logical function is_present_sd(atom)
!
!  Check if atom has sd on coordinates
!
   type(atom_type), intent(in) :: atom
!
   is_present_sd = any(abs(atom%xsd) > 0.0)
!
   end function is_present_sd

!----------------------------------------------------------------------
!corr
!corr   subroutine formula_from_atom_old(atom,elem)
!corr!
!corr!  Extract formula from atom
!corr!
!corr   USE elements
!corr   USE fractionm
!corr   type(atom_type), dimension(:), intent(in) :: atom
!corr   type(element_type), dimension(:), allocatable :: elem
!corr   integer                                       :: i,j
!corr   type(fract_type), dimension(:), allocatable :: fract
!corr   integer                                       :: nel
!corr   integer                                       :: mcm
!corr      real, dimension(:), allocatable :: ochspec
!corr!
!corr   !call elem_from_atoms(elem,atom%ptab)   ! suppress ionic species from forumula
!corr   call elem_from_atoms(elem,eleminfo(atom%ptab)%z)
!corr   nel = numelem(elem)
!corr   if (nel > 0) then
!corr       allocate(ochspec(nel))
!corr       ochspec(:) = 1
!corr       !do i=1,nel   ! suppress ionic species 
!corr       !   elem(i)%nw = 0
!corr       !   do j=1,size(atom)
!corr       !      if (atom(j)%ptab == elem(i)%ptab) then
!corr       !          elem(i)%nw = atom(j)%ocry + elem(i)%nw
!corr       !          ochspec(i) = ochspec(i)*atom(j)%och
!corr       !      endif
!corr       !   enddo
!corr       !enddo
!corr       do i=1,nel
!corr          elem(i)%nw = 0
!corr          do j=1,size(atom)
!corr             if (eleminfo(atom(j)%ptab)%z == elem(i)%z) then
!corr                 elem(i)%nw = atom(j)%ocry + elem(i)%nw
!corr                 ochspec(i) = ochspec(i)*atom(j)%och
!corr             endif
!corr          enddo
!corr       enddo
!corr!
!corr       allocate(fract(nel))
!corr       do i=1,nel
!corr          fract(i) = fractional(elem(i)%nw)
!corr       enddo
!corr       mcm = lcmv(fract(:)%den)
!corr       elem(:)%nw = mcm * elem(:)%nw * ochspec(:)
!corr   endif
!corr!
!corr   end subroutine formula_from_atom_old
!corr
!!------------------------------------------------------------------------------------------

   integer function Z_value(atom,nsym,chform)
!
!  Compute Z value and chemical formula from asymmetric unit and symmetry
!
   use elements
   use arrayutil
   use fractionm
   use strutil
   use math_util
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   integer, intent(in)                           :: nsym
   type(element_type), dimension(:), allocatable, intent(out), optional :: chform
   integer                                       :: i,j,nel,nat
   type(element_type), dimension(:), allocatable :: elem
   integer, dimension(:), allocatable            :: nwv
   type(container_type), dimension(:), allocatable :: sitea
   integer :: nsite,kat,nat_site,nel1
!
   Z_value = 1
   nat = numatoms(atom)
   if (nat == 0) return
!
   call elements_from_atom(atom,elem)
   nel = numelem(elem)
   if (nel == 0) return
!
   call get_atom_site(atom,sitea)
   nsite = size(sitea)
   !write(0,*)'NSITE=',nsite,nat
   !do i=1,nsite
   !   write(0,'(a,i3,a,*(a10))')'SITE=',i,' at:',atom(sitea(i)%pos(:sitea(i)%nat))%lab
   !enddo
!
   do i=1,nel
      elem(i)%nw = 0
      do j=1,nsite
         nat_site = sitea(j)%nat
         !kat = maxloc(atom(sitea(j)%pos(:nat_site))%och,dim=1)
         !kat = sitea(j)%pos(kat)
         !write(0,*)'KAT=',kat,atom(sitea(j)%pos(:nat_site))%och,sum(atom(sitea(j)%pos(:nat_site))%och)
         kat = sitea(j)%pos(1)
         if (atom(kat)%ptab == elem(i)%ptab) then
             elem(i)%nw = elem(i)%nw + atom(kat)%ocry*sum(atom(sitea(j)%pos(:nat_site))%och)
         endif
      enddo
      elem(i)%nw = elem(i)%nw * nsym
      !write(0,*)'EL=',elem(i)%lab,elem(i)%nw
   enddo
!
   allocate(nwv(nel))
   nel1 = 0
   do i=1,nel
      if (nint(elem(i)%nw) > 0.0 .and. is_integer(elem(i)%nw,0.1)) then
          nel1 = nel1 + 1      
          nwv(nel1) = nint(elem(i)%nw)
          !write(0,*)'EL=',elem(i)%lab,elem(i)%nw
      endif
   enddo
   if (nel1 > 0) then
       !write(0,*)'MCM=',nwv(:nel1)
       Z_value = gcdv(nwv(:nel1))
   endif
!
   if (present(chform)) then
       call copy_elem(chform,elem)
       call formula_from_atom_elem(atom,z_value,nsym,chform)
   endif
!
   end function Z_value

!----------------------------------------------------------------------

   subroutine formula_from_atom(atom,z_value,nsym,chform)
!
!  Extract chemical formula from atom
!
   USE elements
   type(atom_type), dimension(:), intent(in)                  :: atom
   integer, intent(in)                                        :: z_value,nsym
   type(element_type), dimension(:), allocatable, intent(out) :: chform
   integer                                                    :: nel
!
   if (z_value == 0) return
!
   call elements_from_atom(atom,chform)
   nel = numelem(chform)
   if (nel == 0) return
!
   call formula_from_atom_elem(atom,z_value,nsym,chform)
!
   end subroutine formula_from_atom

!------------------------------------------------------------------------------------------
  
   subroutine formula_from_atom_elem(atom,z_value,nsym,chform)
   use elements
   type(atom_type), dimension(:), intent(in)                    :: atom
   integer, intent(in)                                          :: z_value,nsym
   type(element_type), dimension(:), allocatable, intent(inout) :: chform
   integer                                                      :: i,j
!
   do i=1,numelem(chform)
      chform(i)%nw = 0
      do j=1,size(atom)
         if (atom(j)%ptab == chform(i)%ptab) then
             chform(i)%nw = chform(i)%nw + atom(j)%och*atom(j)%ocry
         endif
      enddo
      chform(i)%nw = chform(i)%nw * nsym
   enddo
   chform%nw = chform%nw/Z_value
!
   end subroutine formula_from_atom_elem

!------------------------------------------------------------------------------------------

   subroutine xyz_to_atom(atom,xco,bf,bij,occr,occh,nz,lab,inte,elem)
!
!  Poni il modello strutturale in atom
!
   USE elements
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   real,dimension(:,:),intent(in)                            :: xco   ! coordinate e num. di atomi
   real, dimension(:), intent(in), optional                  :: bf    ! fattori termici
   real, dimension(:,:), intent(in), optional                :: bij   ! fattori termici aniso
   real, dimension(:), intent(in), optional                  :: occr  ! occupanza cristall.
   real, dimension(:), intent(in), optional                  :: occh  ! occupanza chimica 
   real, dimension(:), intent(in), optional                  :: inte  ! intensita di picco
   integer, dimension(:), intent(in), optional               :: nz    ! nz
   character(len=*), dimension(:), optional                  :: lab   ! label
   type(element_type), dimension(:), allocatable, optional   :: elem
   integer                                                   :: i
   integer                                                   :: num_atoms
!
   num_atoms = size(xco,2)
   call resize_atoms(atom,num_atoms)
   do i=1,num_atoms
!
!     x,y,z
      atom(i)%xc   = xco(:,i)
!
!     thermal factor
      if (present(bf)) atom(i)%biso = bf(i)
!
!     anisotropic thermal factor
      if (present(bij)) atom(i)%bij(:) = bij(:,i)
!
!     occupanza chimica
      if (present(occh)) atom(i)%och = occh(i)
!
!     occupanza cristallografica
      if (present(occr)) atom(i)%ocry = occr(i)
!
!     specie
      if (present(nz) .and. present(elem)) call atom(i)%set_specie(nz(i),elem)
!
!     intensita di picco
      if (present(inte)) atom(i)%inte = inte(i)
!
!     label
      if (present(lab)) atom(i)%lab  = lab(i)
   enddo
!
   end subroutine xyz_to_atom

!----------------------------------------------------------------------

   subroutine frac_to_cart_copy(atom,atomcart,orto)
!
!  Converti atom in coordinate cartesiane copiandolo in atomcart
!
   type(atom_type), dimension(:), intent(in)                 :: atom
   type(atom_type), dimension(:), allocatable, intent(inout) :: atomcart
   real, dimension(3,3), intent(in)                          :: orto
   integer                                                   :: nat
!   
   nat = size(atom)
   call new_atoms(atomcart,nat)
   atomcart(:) = atom(:)
   call frac_to_cart(atomcart,orto)
!
   end subroutine frac_to_cart_copy

!----------------------------------------------------------------------

   subroutine frac_to_cart_orto(at,orto)
!
!  Conversione di atomi atm da coordinate frazionarie a cartesiane
!
   type(atom_type), dimension(:), intent(inout) :: at
   real, dimension(3,3), intent(in)             :: orto
   integer                                      :: i
!
   do i=1,size(at)
      at(i)%xc = matmul(orto,at(i)%xc)
   enddo
!   
   end subroutine frac_to_cart_orto

!----------------------------------------------------------------------------------------------------

   subroutine frac_to_cart_cell(at,cell)
!
!  Conversione di atomi atm da coordinate frazionarie a cartesiane
!
   USE unit_cell
   type(atom_type), dimension(:), intent(inout) :: at
   type(cell_type), intent(in)                  :: cell
!
   call frac_to_cart_orto(at,cell%get_ortom())
!   
   end subroutine frac_to_cart_cell
   
!----------------------------------------------------------------------------------------------------

   function cartesian_coord_a_o(atom,ortom) result(atomc)
!  Conversione da cristallografiche a cartesiane
   type(atom_type), intent(in)      :: atom
   real, dimension(3,3), intent(in) :: ortom
   type(atom_type)                  :: atomc
!
   atomc = atom
   atomc%xc = matmul(ortom,atom%xc)
!
   end function cartesian_coord_a_o

!----------------------------------------------------------------------------------------------------

   function fractional_coord_a_o(atomc,ortoi) result(atomf)
!  Conversione da cristallografiche a cartesiane
   type(atom_type), intent(in)      :: atomc
   real, dimension(3,3), intent(in) :: ortoi
   type(atom_type)                  :: atomf
!
   atomf = atomc
   atomf%xc = matmul(ortoi,atomc%xc)
!
   end function fractional_coord_a_o

!----------------------------------------------------------------------------------------------------

   function xyz_cart(atom,cell) 
!
!  Compute xyz in cartesian
!
   USE unit_cell
   type(atom_type), intent(in) :: atom
   type(cell_type), intent(in) :: cell
   real, dimension(3)          :: xyz_cart
   xyz_cart = matmul(cell%get_ortom(),atom%xc)
   end function xyz_cart

!----------------------------------------------------------------------------------------------------

   subroutine cart_to_frac_orto(at,orto)
!
!  Conversion from cartesian to fractional
!
   type(atom_type), dimension(:), intent(inout) :: at
   real, dimension(3,3), intent(in)             :: orto
   integer                                      :: i
!
   do i=1,size(at)
      at(i)%xc = matmul(orto,at(i)%xc)
   enddo
!   
   end subroutine cart_to_frac_orto

!----------------------------------------------------------------------------------------------------

   subroutine cart_to_frac_cell(at,cell)
!
!  Conversion from cartesian to fractional
!
   USE unit_cell
   type(atom_type), dimension(:), intent(inout) :: at
   type(cell_type), intent(in)                  :: cell
!
   call frac_to_cart_orto(at,cell%get_ortoi())
!   
   end subroutine cart_to_frac_cell

!----------------------------------------------------------------------------------------------------

   subroutine translate_atoms_v(atom,xtra)
!
!  Translate array of atoms
!
   type(atom_type), dimension(:), intent(inout) :: atom
   real, dimension(:), intent(in)               :: xtra    ! traslazione da applicare
   integer                                      :: i
!
   do i=1,size(atom)
      atom(i)%xc = atom(i)%xc + xtra
   enddo
!
   end subroutine translate_atoms_v

!----------------------------------------------------------------------------------------------------

   subroutine translate_atoms_s(atom,xtra)
!
!  Translate an atom
!
   type(atom_type), intent(inout) :: atom
   real, dimension(:), intent(in) :: xtra    ! traslazione da applicare
!
   atom%xc = atom%xc + xtra
!
   end subroutine translate_atoms_s

!----------------------------------------------------------------------------------------------------

   subroutine bonds_to_torsions(legm,atom,cell,tors,cart)
!
!  Genera angoli. Se angvali e' assegnato, solo gli angoli < di angvali vengono generati 
!
   USE cgeom
   USE unit_cell
   USE connect_mod
   USE arrayutil
   type(bond_type), dimension(:), allocatable, intent(in)       :: legm
   type(atom_type), dimension(:), intent(in)                    :: atom
   type(cell_type), intent(in)                                  :: cell
   type(torsion_type), dimension(:), allocatable, intent(inout) :: tors
   logical, intent(in), optional                                :: cart    ! se vero, atom e' in coord. cartesiane
   integer                                                      :: nleg
   logical                                                      :: carti
   integer                                                      :: i,j,k
   integer                                                      :: nat
   type(container_type), dimension(:), allocatable              :: conn
   integer                                                      :: ndimtors
   integer                                                      :: ntors
   type(atom_type), dimension(size(atom))                       :: atomc
   integer                                                      :: n1,n2,n3,n4
   real                                                         :: val
!
   nleg = numbonds(legm)
   if (nleg > 0) then
       if (present(cart)) then
          carti = cart
       else
          carti = .false.
       endif
!
!      Converti in cartesiane
       if (.not.carti) then
           atomc(:) = atom(:)
           call frac_to_cart(atomc,cell%get_ortom())
       endif
!
!      alloca inizialmente gli angoli al numero di legami x 2
       ndimtors = 2*nleg
       call new_torsions(tors,ndimtors)
!
       nat = size(atomc)
       call bond_to_connect(nat,legm,conn)    
!
       ntors = 0
       do i=1,nleg     ! cerca le torsioni associate ad ogni legame
          n2 = legm(i)%n1
          n3 = legm(i)%n2
          if (conn(n2)%nat == 1 .or. conn(n3)%nat == 1) cycle  ! non puo' esserci torsione
          do j=1,conn(n2)%nat
             n1 = conn(n2)%pos(j)
             if (n1 == n3) cycle
             do k=1,conn(n3)%nat
                n4 = conn(n3)%pos(k)
                if (n4 == n2) cycle
                ntors = ntors + 1
                if (ntors > ndimtors) then   ! espandi la dimensione di tors
                    ndimtors = ndimtors + nleg
                    call resize_torsions(tors,ndimtors)
                endif
                val = Angle_Dihedral(atomc(n1)%xc,atomc(n2)%xc,atomc(n3)%xc,atomc(n4)%xc)
                tors(ntors) = torsion_type(n1,n2,n3,n4,val)
             enddo
          enddo
       enddo
!
       call resize_torsions(tors,ntors) ! compatta tors a ntors
   endif
!
   end subroutine bonds_to_torsions

!---------------------------------------------------------------------------------------

   subroutine add_atoms_to_list(atom,atomadd,natnew,legm1,legm2)
!
!  Combina 2 modelli in atom
!
   USE connect_mod
   type(atom_type), allocatable, dimension(:), intent(inout) :: atom
   type(atom_type), dimension(:), intent(in)                 :: atomadd
   integer, intent(out)                                      :: natnew
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legm1
   type(bond_type), dimension(:), allocatable, intent(in),    optional :: legm2
   integer                                                   :: natcurr
   integer                                                   :: natadd
!
!  Combina gli atomi
   natcurr = numatoms(atom)   !!!!size(atom)
   natadd = size(atomadd)
   natnew = natcurr + natadd
   call resize_atoms(atom,natnew)
   atom(natcurr+1:) = atomadd
!
!  Combina la connettivita' in leg
   if (present(legm1) .and. present(legm2)) then
       call combine_legm(legm1,legm2,natcurr)
   endif
!
   end subroutine add_atoms_to_list

!---------------------------------------------------------------------------------------

   subroutine add_atom(atom,atomadd,legm,leg)
   USE connect_mod
   type(atom_type), allocatable, dimension(:), intent(inout) :: atom
   type(atom_type)                                           :: atomadd
   type(bond_type), allocatable, dimension(:), intent(inout), optional :: legm
   type(bond_type), dimension(:), intent(in), optional                 :: leg
   integer                                                   :: natcurr
   type(bond_type), allocatable, dimension(:) :: leg1
!
   natcurr = numatoms(atom)
   call resize_atoms(atom,natcurr+1)
   atom(natcurr+1) = atomadd
   if (present(legm) .and. present(leg)) then
       allocate(leg1(size(leg)),source=leg)
       call combine_legm(legm,leg1,0)
   endif
!
   end subroutine add_atom

!---------------------------------------------------------------------------------------

   subroutine combine_iconn(icontot,icon1,icon2,nat1)
!
!  Combina in icontot la connettivita di due modelli 
!
   integer, dimension(:), allocatable, intent(inout) :: icontot
   integer, dimension(:), intent(in)                 :: icon1,icon2
   integer, intent(in)                               :: nat1
   integer                                           :: ncon1,ncon2,ncontot
   integer                                           :: i,nc2
!
   ncon1 = icon1(1)
   ncon2 = icon2(1)
   ncontot = ncon1+ncon2
   allocate(icontot(3*ncontot+1))
   icontot(1) = ncontot
   if (ncontot > 0) then
       icontot(2:3*ncon1+1) = icon1(2:3*ncon1+1)
       nc2 = 3*ncon1+1
       do i=2,ncon2*3+1,3
          nc2 = nc2 + 1
          icontot(nc2) = icon2(i)+nat1
          nc2 = nc2 + 1
          icontot(nc2) = icon2(i+1)+nat1
          nc2 = nc2 + 1
          icontot(nc2) = icon2(i+2)
       enddo
   endif
!
   end subroutine combine_iconn

!---------------------------------------------------------------------------------------

   subroutine calcola_occ(atom,spg,cell,modcoor,dist)
   USE kspec_mod
   USE unit_cell
   USE spginfom
   type(atom_type), dimension(:), intent(inout) :: atom   
   type(spaceg_type), intent(in)                :: spg
   type(cell_type), intent(in)                  :: cell
   logical, optional                            :: modcoor
   real, intent(in), optional                   :: dist
   logical                                      :: modco
   integer                                      :: kspecb
   real, dimension(11)                          :: xo,xn
   integer, dimension(10)                       :: key      
   integer                                      :: i
   integer                                      :: natc
   integer                                      :: iactn
   integer                                      :: js
   real                                         :: ddmin
!
!  modcoor = .true. -> modifica le coordinate
   if (present(modcoor)) then
       modco = modcoor
   else          
       modco = .true.
   endif        
!
!  eventualmente modifica distanza di default
   if (present(dist))then
       ddmin = dist
   else
       ddmin = DEF_DISTMIN
   endif
!
   natc = size(atom)
   js = lattice_system(spg,cell%get_par())
   !iactn = 2
   iactn = 1
   do i=1,natc
      xo(:3) = atom(i)%xc
      kspecb =  kspecb_new(xo,xn,key,spg,js,cell%get_g(),iactn,ddmin=ddmin)
      !write(6,*)'at=',i,'key=',key(:)
      atom(i)%ocry = 1.0/key(10)
      if (modco) atom(i)%xc = xn(:3)
   enddo
!   
   end subroutine calcola_occ

!---------------------------------------------------------------------------------------

   elemental subroutine set_biso_powcod(atom,zval)
!
!  Assegna dei fattori termici ragionevoli agli atomi
!
   USE elements
   USE atom_basic
   type(atom_type), intent(inout) :: atom
   integer, intent(in), optional  :: zval  ! numero atomico 
   integer                        :: zv
!
   if (present(zval)) then
       zv = zval
   else
       zv = atom%z()
   endif
!
   select case (zv)
      case (H_at)
        atom%biso = 4.0

      case default
        atom%biso = 1.0

   end select
!
   end subroutine set_biso_powcod

!---------------------------------------------------------------------------------------

   elemental subroutine set_biso(atom,zval)
!
!  Assegna dei fattori termici ragionevoli agli atomi (DASH approach)
!
   USE elements
   USE atom_basic
   type(atom_type), intent(inout) :: atom
   integer, intent(in), optional  :: zval  ! numero atomico 
   integer                        :: zv
!
   if (present(zval)) then
       zv = zval
   else
       zv = atom%z()
   endif
!
   select case (zv)
      case (H_at)
        atom%biso = 6.0   
        !atom%biso = 4.0   
        
      !old case (He_at,C_at,N_at,O_at,F_at,Ne_at,P_at,S_at,Cl_at,Ar_at,Br_at,Kr_at,I_at,Xe_at,Rn_at)
      case (C_at,B_at,N_at,O_at,P_at,S_at,F_at,Cl_at,Br_at,I_at) ! organic elements
        atom%biso = 3.0   

      case default
        atom%biso = 1.0   

   end select
!
   end subroutine set_biso

!---------------------------------------------------------------------------------------

   subroutine print_atoms(at,height,title,write_occ,kpr)
   USE strutil
   type(atom_type), dimension(:), intent(in)   :: at         ! gli atomi
   logical, optional                           :: height     ! le intensita'?
   character(len=*), intent(in), optional      :: title      ! titolo per intestazione
   logical, intent(in), optional               :: write_occ  ! occupanza cristallografica
   integer, intent(in), optional               :: kpr        ! stampa su kpr
   logical                                     :: wocc
   integer                                     :: n
   logical                                     :: heightl 
   integer                                     :: kpri
!
   if (present(kpr)) then
       kpri = kpr
   else
       kpri = 6
   endif
   if (present(height)) then
       heightl = height
   else
       heightl = .false.
   endif
   if (present(title)) then
       write(kpri,'(/a)')centra_str(title,80)
   else           
       write(kpri,*)
   endif        
   if (heightl) then
       write(kpri,'(a)')'                  Height         X           Y         Z             B       Site'
       do n=1,size(at)
          write(kpri,'(1x,i4,a1,2x,a5,2x,g10.3,3(2x,f10.4),2x,f10.3,2x,f8.3)') &
                n,')',at(n)%lab,at(n)%inte,at(n)%xc(:3),at(n)%biso,at(n)%och
       enddo
   else
       if (present(write_occ)) then
           wocc = write_occ
       else
           wocc = .false.
       endif
       if (wocc) then
           write(kpri,'(a)')'                     X           Y           Z           B       Site     Occ'
           do n=1,size(at)
              write(kpri,'(1x,i4,a1,2x,a5,3(2x,f10.4),2x,f10.3,2(2x,f8.3))') &
                    n,')',at(n)%lab,at(n)%xc(:3),at(n)%biso,at(n)%och,at(n)%ocry
           enddo
       else
           write(kpri,'(a)')'                     X           Y           Z           B       Site'
           do n=1,size(at)
              write(kpri,'(1x,i4,a1,2x,a5,3(2x,f10.4),2x,f10.3,2x,f8.3)') &
                    n,')',at(n)%lab,at(n)%xc(:3),at(n)%biso,at(n)%och
           enddo
       endif
   endif
   write(kpri,*)
!
   end subroutine print_atoms

!-------------------------------------------------------------------------------

   subroutine print_atoms_vet(atom,vet,title,kpr,adp_type,spg)
   USE strutil
   USE ccryst
   USE spginfom
   type(atom_type), intent(in), dimension(:), allocatable :: atom   ! atomi da stampare
   integer, dimension(:), intent(in), optional            :: vet    ! cosa stampare?
   character(len=*), intent(in), optional                 :: title  ! titolo per intestazione
   integer, intent(in), optional                          :: kpr    ! file pointer
   integer, intent(in), optional                          :: adp_type  ! adp type
!corr   type(symminfo_type), dimension(:), allocatable, optional :: infos ! symmetry info
   type(spaceg_type), intent(in), optional                  :: spg   ! space group
   integer, dimension(:), allocatable                     :: ival
   real, dimension(:), allocatable                        :: rval
   character(len=40), dimension(:), allocatable           :: sval
   integer, dimension(:), allocatable                     :: ktype
   integer                                                :: kpri
   integer, dimension(:), allocatable                     :: veti
   integer                                                :: ncol
   integer                                                :: i
   integer                                                :: adp_t
   character(len=9) :: adp_string
   logical :: lsym
!
   if (present(kpr)) then
       kpri = kpr
   else
       kpri = 6
   endif
!
   if (present(title)) then
       write(kpri,'(/a)')centra_str(title,80)
   else           
       write(kpri,*)
   endif        
!
   if (present(adp_type)) then
       adp_t = adp_type
   else
       adp_t = 1
   endif
!
   if (present(vet)) then
       ncol = size(vet)
       allocate(veti(ncol))
       veti(:) = vet(:)
   else
       ncol = 10
       allocate(veti(ncol))
       veti(:) = 1
!corr       veti(:9) = 1
!corr       veti(10) = 0
   endif
   lsym = present(spg)
   allocate(ktype(ncol),ival(ncol),rval(ncol),sval(ncol))
!
!  Titoli delle colonne
   ktype(:) = 3
   if (adp_t == 1) then
       adp_string = 'B'
   else
       adp_string = 'U'
   endif
   call set_row_list(veti,ktype,sval=(/'Number   ','Label    ','Type     ',    &
   'Intensity','Xfrac    ','Yfrac    ','Zfrac    ',adp_string,'s.o.f.   ','Symm.Op. '/),kpr=kpri)
!
!  Scrivi la tabella degli atomi
   ktype = [1,3,3,22,25,25,25,23,23,3]
   do i=1,numatoms(atom)
      ival(1) = i
      sval(2) = atom(i)%lab
      sval(3) = atom(i)%specie()
      rval(4) = atom(i)%inte
      rval(5) = atom(i)%xc(1)
      rval(6) = atom(i)%xc(2)
      rval(7) = atom(i)%xc(3)
      if (adp_t == 1) then
          rval(8) = atom(i)%biso
      else
          rval(8) = u_from_b(atom(i)%biso)
      endif
      rval(9) = atom(i)%ocry*atom(i)%och
      if (lsym) then
          sval(10) = spg%symopstr(atom(i)%op%op)
      else
          sval(10) = 'x, y, z'
      endif
      call set_row_list(veti,ktype,ival,rval,sval,.false.,kpri)
   enddo
!
   end subroutine print_atoms_vet

!-------------------------------------------------------------------------------

   subroutine save_structure_bin(unitbin,atom,bond)
!
!  Save structure model on binary file
!
   USE connect_mod
   integer, intent(in)                                              :: unitbin ! unit of binary file
   type(atom_type), intent(in), dimension(:), allocatable           :: atom    ! atomi da stampare
   type(bond_type), dimension(:), allocatable, intent(in), optional :: bond    ! legami
   integer                                                          :: nat, numb
!
!  write atom struct
   nat = numatoms(atom)
   write(unitbin) nat
   if (nat > 0) call write_atoms(atom,unitbin)
!   
!  write bond struct
   if (present(bond)) then
       numb = numbonds(bond)
   else
       numb = 0
   endif
   write(unitbin) numb
   if (numb > 0) write(unitbin) bond(:)
!
   end subroutine save_structure_bin

!-------------------------------------------------------------------------------

   subroutine read_structure_bin(unitbin,atom,bond,err)
!
!  Read structure model from binary file
!
   USE connect_mod
   USE errormod
   integer, intent(in)                                              :: unitbin ! unit of binary file
   type(atom_type), dimension(:), allocatable, intent(out)          :: atom    ! atomi da stampare
   type(bond_type), dimension(:), allocatable, intent(out),optional :: bond    ! legami
   type(error_type), intent(out)                                    :: err
   integer                                                          :: ier
   integer                                                          :: nat, numb
!
!  read atom struct
   read(unitbin,iostat=ier,err=10) nat
   if (nat > 0 .and. ier == 0) then
       call new_atoms(atom,nat)
       !read(unitbin,iostat=ier,err=10) atom(:)
       call read_atoms(atom,unitbin,ier)
       if (ier /= 0) go to 10
   endif
!   
!  read bond struct
   read(unitbin,iostat=ier,err=10) numb
   if (present(bond)) then
       if (numb > 0 .and. ier == 0) then
           call new_bonds(bond,numb)
           read(unitbin,iostat=ier,err=10) bond(:)
       endif
   endif
!
10 continue
   if (ier /= 0) then
       call err%set('Error on reading structure')
   endif
!
   end subroutine read_structure_bin
   
!-------------------------------------------------------------------------------

   function atom_symm_scalar(atom,symop) result(at)
!corr   USE symmetry, only:kmat, tmat
   USE spginfom
!corr   integer, intent(in)         :: k
   type(symop_type), intent(in)                 :: symop
   type(atom_type), intent(in) :: atom
   type(atom_type)             :: at
!corr   at%xc = matmul(kmat(k,:,:),atom%xc) + tmat(k,:)   ! Rot*X + Tra
   at%xc = matmul(symop%rot,atom%xc) + symop%trn   ! Rot*X + Tra
   end function atom_symm_scalar

!-------------------------------------------------------------------------------

   function atom_symm_scalar_xyz(atom,symop) result(xyz)
!corr   USE symmetry, only:kmat, tmat
   USE spginfom
!corr   integer, intent(in)            :: k
   real, dimension(3), intent(in) :: atom
   type(symop_type), intent(in)                 :: symop
   real, dimension(3)             :: xyz
!corr   xyz(:) = matmul(kmat(k,:,:),atom) + tmat(k,:)   ! Rot*X + Tra
   xyz(:) = matmul(symop%rot,atom) + symop%trn   ! Rot*X + Tra
   end function atom_symm_scalar_xyz

!-------------------------------------------------------------------------------

   function atom_symm_vet(atom,symop) result(at)
   USE spginfom
   type(atom_type), dimension(:), intent(in) :: atom
   type(symop_type), intent(in)                 :: symop
   type(atom_type), dimension(size(atom))    :: at
   integer                                   :: i
   do i=1,size(atom)
      at(i)%xc = matmul(symop%rot,atom(i)%xc) + symop%trn   ! Rot*X + Tra
   enddo
   end function atom_symm_vet

!-------------------------------------------------------------------------------

   subroutine apply_sym_oper_s(atom,symop)
   USE spginfom
   type(symop_type), intent(in)                 :: symop
   type(atom_type), dimension(:), intent(inout) :: atom
   integer                                      :: i
!
   do i=1,size(atom)
      atom(i)%xc = matmul(symop%rot,atom(i)%xc) + symop%trn   ! Rot*X + Tra
   enddo
!
   end subroutine apply_sym_oper_s

!-------------------------------------------------------------------------------

   subroutine apply_sym_oper_s_scalar(atom,symop)
   USE spginfom
   type(symop_type), intent(in)   :: symop
   type(atom_type), intent(inout) :: atom
!
   atom%xc = matmul(symop%rot,atom%xc) + symop%trn   ! Rot*X + Tra
!
   end subroutine apply_sym_oper_s_scalar

!---------------------------------------------------------------------      

   subroutine make_tetrahedron(atom,dist,cell,legm,xpos,kcentr,centro)
!
!  Crea un tetraedro 
!
   USE connect_mod
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom    ! gli atomi
   real, intent(in)                                          :: dist    ! la distanza tra atomo centrale e vertici
   type(cell_type), intent(in), optional                     :: cell
   type(bond_type), dimension(:), allocatable, optional      :: legm
   real, dimension(3), intent(in), optional                  :: xpos    ! posizione 
   integer, intent(in),optional                              :: kcentr  ! gestione atomo centrale
   logical, intent(in),optional                              :: centro  ! atomo al centro
   logical                                                   :: centr
   real                                                      :: xx,rr,dd,aa
   integer                                                   :: nat
!
!  atomo centrale nell'origine
   if (present(centro) .and. present(kcentr)) then
       centr = centro
   else
       centr = .true.
   endif
   if (centr) then  ! aggiungi atomo al centro
       allocate(atom(5))
       nat = 1
       atom(nat)%xc(:) = (/0.0,0.0,0.0/)
   else                   ! l'atomo kcentr deve essere al centro
       allocate(atom(4))
       nat = 0
   endif
!
!  atomi ai vertici
   xx = 0.942809041*dist   !  (4/(3*sqrt(2)))*dist
   rr = 0.333333333*dist   !  (1/3)*dist
   dd = 0.47140452*dist    !  (sqrt(2)/3)*dist
   aa = 1.632993162*dist   !  (4/sqrt(6))*dist
   atom(nat+1)%xc = (/xx,0.0,-rr/)
   atom(nat+2)%xc = (/-dd,0.5*aa,-rr/)
   atom(nat+3)%xc = (/-dd,-0.5*aa,-rr/)
   atom(nat+4)%xc = (/0.0,0.0,dist/)
!
!  Connettivita
   if (present(legm)) then
       call new_bonds(legm,4)
       if (centr) then
           legm(:)%n1 = (/1,1,1,1/)
           legm(:)%n2 = (/2,3,4,5/)
       else
           legm(:)%n1 = (/kcentr,kcentr,kcentr,kcentr/)
           legm(:)%n2 = (/1,2,3,4/)
       endif
   endif
!
   if (present(cell)) call cart_to_frac(atom,cell%get_ortoi())    !converti in coord. frazionarie
   if (present(xpos))  call translate_atoms(atom,xpos)  !sposta in xpos
!
   end subroutine make_tetrahedron

!---------------------------------------------------------------------      

   subroutine make_octahedron (atom,dist,cell,xpos,kcentr,centro)
!
!  Crea un ottaedro 
!
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom    ! gli atomi
   real, intent(in)                                          :: dist    ! la distanza tra atomo centrale e vertici
   type(cell_type), optional, intent(in)                     :: cell    ! matrice per conversione in coord. fraz.
   real, dimension(3), intent(in), optional                  :: xpos    ! posizione 
   integer, intent(in),optional                              :: kcentr  ! gestione atomo centrale
   logical, intent(in),optional                              :: centro  ! atomo al centro
   logical                                                   :: centr
   integer                                                   :: nat
!
!  atomo centrale nell'origine
   if (present(centro) .and. present(kcentr)) then
       centr = centro
   else
       centr = .true.
   endif
   if (centr) then  ! aggiungi atomo al centro
       allocate(atom(7))
       nat = 1
       atom(nat)%xc(:) = (/0.0,0.0,0.0/)
   else                   ! l'atomo kcentr deve essere al centro
       allocate(atom(6))
       nat = 0
   endif
!
!  atomi ai vertici
   atom(nat+1)%xc = (/dist,0.0,0.0/)
   atom(nat+2)%xc = (/0.0,dist,0.0/)
   atom(nat+3)%xc = (/0.0,0.0,dist/)
   atom(nat+4)%xc = (/-dist,0.0,0.0/)
   atom(nat+5)%xc = (/0.0,-dist,0.0/)
   atom(nat+6)%xc = (/0.0,0.0,-dist/)
!
   if (present(cell)) call cart_to_frac(atom,cell%get_ortoi())  !converti in coord. frazionarie
   if (present(xpos))  call translate_atoms(atom,xpos)  !sposta in xpos
!
   end subroutine make_octahedron

!---------------------------------------------------------------------      

   subroutine make_square (atom,dist,cell,xpos,kcentr,centro)
!
!  Mak sqaure plane
!
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom    ! gli atomi
   real, intent(in)                                          :: dist    ! la distanza tra atomo centrale e vertici
   type(cell_type), optional, intent(in)                     :: cell   ! unit cell
   real, dimension(3), intent(in), optional                  :: xpos    ! posizione 
   integer, intent(in),optional                              :: kcentr  ! gestione atomo centrale
   logical, intent(in),optional                              :: centro  ! atomo al centro
   logical                                                   :: centr
   integer                                                   :: nat
!
!  atomo centrale nell'origine
   if (present(centro) .and. present(kcentr)) then
       centr = centro
   else
       centr = .true.
   endif
   if (centr) then  ! aggiungi atomo al centro
       allocate(atom(5))
       nat = 1
       atom(nat)%xc(:) = (/0.0,0.0,0.0/)
   else                   ! l'atomo kcentr deve essere al centro
       allocate(atom(4))
       nat = 0
   endif
!
!  atomi ai vertici
   atom(nat+1)%xc = (/dist,0.0,0.0/)
   atom(nat+2)%xc = (/0.0,dist,0.0/)
   atom(nat+3)%xc = (/-dist,0.0,0.0/)
   atom(nat+4)%xc = (/0.0,-dist,0.0/)
!
   if (present(cell)) call cart_to_frac(atom,cell%get_ortoi())  !converti in coord. frazionarie
   if (present(xpos))  call translate_atoms(atom,xpos)  !sposta in xpos
!
   end subroutine make_square

!---------------------------------------------------------------------      

   subroutine make_trigonal(atom, dist, cell, xpos)
!
!  Make trigonal plane
!
   USE trig_constants
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   real, intent(in)                                          :: dist
   type(cell_type), intent(in)                               :: cell  
   real, dimension(3), intent(in), optional                  :: xpos 
!
   call new_atoms(atom,4)
   atom(1)%xc = (/0., 0., 0./)
   atom(2)%xc = (/dist, 0.0, 0.0/)
   atom(3)%xc = (/-dist/2., dist*sqrt(3.)/2., 0./)
   atom(4)%xc = (/-dist/2.,-dist*sqrt(3.)/2., 0./)
!
   call cart_to_frac(atom,cell%get_ortoi())   !converti in coord. frazionarie
   call translate_atoms(atom,xpos) !sposta in xpos
!
   end subroutine make_trigonal

!---------------------------------------------------------------------      

   subroutine make_cube(atom, dist, cell, xpos)
!
!  Make cube
!
   USE trig_constants
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   real, intent(in)                                          :: dist
   type(cell_type), intent(in)                               :: cell  
   real, dimension(3), intent(in), optional                  :: xpos 
   real                                                      :: dis
!
   call new_atoms(atom,9)
   dis = dist/sqrt(3.)
   atom(1)%xc =  [0.,0.,0.]   ! central atom
   atom(2)%xc =  [ dis,-dis, dis]
   atom(3)%xc =  [ dis, dis, dis]
   atom(4)%xc =  [-dis, dis, dis]
   atom(5)%xc =  [-dis,-dis, dis]
   atom(6)%xc =  [ dis,-dis,-dis]
   atom(7)%xc =  [ dis, dis,-dis]
   atom(8)%xc =  [-dis, dis,-dis]
   atom(9)%xc =  [-dis,-dis,-dis]
!
   call cart_to_frac(atom,cell%get_ortoi())   !converti in coord. frazionarie
   call translate_atoms(atom,xpos) !sposta in xpos
!
   end subroutine make_cube

!---------------------------------------------------------------------      

   subroutine make_anti_prism_tetragonal(atom, dist, cell, xpos)
!
!  Make anti prism tetragonal
!
   USE trig_constants
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   real, intent(in)                                          :: dist
   type(cell_type), intent(in)                               :: cell   ! matrice per conversione in coord. fraz.
   real, dimension(3), intent(in), optional                  :: xpos    ! posizione 
   real :: dis
!
   call new_atoms(atom,9)
   dis = dist/sqrt(3.)
   atom(1)%xc =  [0.,0.,0.]   ! central atom
   atom(2)%xc =  [ dis,-dis, dis]
   atom(3)%xc =  [ dis, dis, dis]
   atom(4)%xc =  [-dis, dis, dis]
   atom(5)%xc =  [-dis,-dis, dis]
   atom(6)%xc =  [ dis*sqrt(2.), 0.,-dis]
   atom(7)%xc =  [ 0., dis*sqrt(2.),-dis]
   atom(8)%xc =  [-dis*sqrt(2.), 0.,-dis]
   atom(9)%xc =  [ 0.,-dis*sqrt(2.),-dis]
!
   call cart_to_frac(atom,cell%get_ortoi())   !converti in coord. frazionarie
   call translate_atoms(atom,xpos) !sposta in xpos
!
   end subroutine make_anti_prism_tetragonal

!---------------------------------------------------------------------      

   subroutine make_prism_trigonal(atom, dist, cell, xpos)
!
!  Make prism tetragonal
!
   USE trig_constants
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   real, intent(in)                           :: dist
   type(cell_type), intent(in)           :: cell   ! matrice per conversione in coord. fraz.
   real, dimension(3), intent(in), optional   :: xpos    ! posizione 
   real :: dis,dis1
!
   call new_atoms(atom,7)
   dis = dist*sqrt(3./7.)
   dis1 = dis*2./sqrt(3.)
   atom(1)%xc =  [0.,0.,0.]   ! central atom
   atom(2)%xc =  [ dis1   ,0.               , dis]
   atom(3)%xc =  [-dis1/2., dis1*sqrt(3.)/2., dis]
   atom(4)%xc =  [-dis1/2.,-dis1*sqrt(3.)/2., dis]
   atom(5)%xc =  [ dis1   ,0.               ,-dis]
   atom(6)%xc =  [-dis1/2., dis1*sqrt(3.)/2.,-dis]
   atom(7)%xc =  [-dis1/2.,-dis1*sqrt(3.)/2.,-dis]
!
   call cart_to_frac(atom,cell%get_ortoi())   !converti in coord. frazionarie
   call translate_atoms(atom,xpos) !sposta in xpos
!
   end subroutine make_prism_trigonal

!---------------------------------------------------------------------      

   subroutine make_icosahedron(atom, dist, cell, xpos)
!
!  Make anti prism tetragonal
!
   USE trig_constants
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   real, intent(in)                                          :: dist
   type(cell_type), intent(in)                               :: cell
   real, dimension(3), intent(in), optional                  :: xpos 
   real :: g0,a,g
!
   call new_atoms(atom,13)
   g0=(1.+sqrt(5.))/2.
   a=dist/sqrt(1.+g0*g0)
   g=g0*a
   atom(1)%xc =  [0.,0.,0.] ! central atom

   atom(2)%xc  =  [0., g, a]
   atom(3)%xc  =  [0., g,-a]
   atom(4)%xc  =  [0.,-g, a]
   atom(5)%xc  =  [0.,-g,-a]

   atom(6)%xc  =  [ a,0., g]
   atom(7)%xc  =  [-a,0., g]
   atom(8)%xc  =  [ a,0.,-g]
   atom(9)%xc  =  [-a,0.,-g]

   atom(10)%xc =  [ g, a,0.] 
   atom(11)%xc =  [ g,-a,0.] 
   atom(12)%xc =  [-g, a,0.] 
   atom(13)%xc =  [-g,-a,0.] 
!
   call cart_to_frac(atom,cell%get_ortoi())   !converti in coord. frazionarie
   call translate_atoms(atom,xpos) !sposta in xpos
!
   end subroutine make_icosahedron

!---------------------------------------------------------------------      

   subroutine create_connectivity_fix(atom,cell,legm,check,tolmin,tolmax,angl,kpr,vet,code,usecov)
!
!  Crea la connettivita senza spostare gli atomi
!  Se vet e' presente si cercano solo i legami che coinvolgono gli atomi in vet
!
   USE cgeom
   USE bondtmod
   USE connect_mod
   USE unit_cell
!
   type(atom_type), dimension(:), intent(in)                           :: atom
   type(cell_type), intent(in)                                         :: cell
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legm
   logical, intent(in), optional                                       :: check
   real, intent(in), optional                                          :: tolmin,tolmax
   real, intent(in), optional                                          :: angl
   integer, intent(in), optional                                       :: kpr
   integer, dimension(:), intent(in), optional                         :: vet
   integer, intent(in), optional                                       :: code
   logical, intent(in), optional                                       :: usecov
   logical                                                             :: usecovr
   logical                                                             :: checkb
   integer                                                             :: kprb
   integer                                                             :: nat
   integer                                                             :: i,j 
   integer, dimension(size(atom))                                      :: zval
   integer                                                             :: nleg
   integer                                                             :: ndimleg
   real                                                                :: angmin
   type(bond_type)                                                     :: legnew
   real                                                                :: tlmin,tlmax
   integer                                                             :: codevet
       integer :: iv
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
       tlmin = 0.0 
   endif
   if (present(tolmax)) then
       tlmax = tolmax
   else
       tlmax = 0.0 
   endif
   if (present(angl)) then
       angmin = angl
   else
       angmin = DEF_BONDSET%angle
   endif
   if (present(usecov)) then
       usecovr = usecov
   else
       usecovr = .false.
   endif
!
   nat = size(atom)
   nleg = 0
!
   if (nat > 1) then  ! piu' di un atomo ...
!
!      genera Z di tutti gli atomi
       zval(:) = atom%z()
!
!      Crea tabella di connettivita'
       ndimleg = nat 
       call resize_bonds(legm,ndimleg)
       if (present(vet)) then
           if (present(code)) then
               codevet = code
           else
               codevet = 1
           endif
           select case (codevet)
        
             case (1)   ! only atoms in the vector vet are connected to each other and with all other atoms
               do iv=1,size(vet)
                  i = vet(iv)
                  if (zval(i) == 0) cycle
                  do j=1,nat
                     if (any(vet(1:iv) == j)) cycle ! trick: exclude i=j and all atoms connected before
                     if (zval(j) == 0) cycle
                     call create_bond(i,j,atom(i)%xc,atom(j)%xc,zval(i),zval(j),tlmin,tlmax,legnew,usecovr)
                     if (legnew%n1 > 0) then  ! se 0 non c'è legame tra i due atomi
                         nleg = nleg + 1
                         if (nleg > ndimleg) then   ! check sulla dimensione di legm
                             ndimleg = ndimleg + nat/2
                             call resize_bonds(legm,ndimleg)
                         endif
                         legm(nleg) = legnew
                     endif
                  enddo
               enddo

             case (2)   ! only atoms in the vector vet are connected with all other atoms but not to each other
               do iv=1,size(vet)
                  i = vet(iv)
                  if (zval(i) == 0) cycle
                  do j=1,nat
                     if (any(vet(:) == j)) cycle ! trick: exclude all atoms in vet
                     if (zval(j) == 0) cycle
                     call create_bond(i,j,atom(i)%xc,atom(j)%xc,zval(i),zval(j),tlmin,tlmax,legnew,usecovr)
                     if (legnew%n1 > 0) then  ! se 0 non c'è legame tra i due atomi
                         nleg = nleg + 1
                         if (nleg > ndimleg) then   ! check sulla dimensione di legm
                             ndimleg = ndimleg + nat/2
                             call resize_bonds(legm,ndimleg)
                         endif
                         legm(nleg) = legnew
                     endif
                  enddo
               enddo

             case (3)   ! exclude bonds i-j with vet > 0 and vet(i)/=vet(j)
               do i=1,nat-1
                  if (zval(i) == 0) cycle
                  do j=i+1,nat
                     if (zval(j) == 0) cycle
                     if ((vet(i) > 0 .and. vet(j) > 0) .and. (vet(i) /= vet(j))) cycle  ! test bond i-j
                     call create_bond(i,j,atom(i)%xc,atom(j)%xc,zval(i),zval(j),tlmin,tlmax,legnew,usecovr)
                     if (legnew%n1 > 0) then  ! se 0 non c'è legame tra i due atomi
                         nleg = nleg + 1
                         if (nleg > ndimleg) then   ! check sulla dimensione di legm
                             ndimleg = ndimleg + nat/2
                             call resize_bonds(legm,ndimleg)
                         endif
                         legm(nleg) = legnew
                     endif
                  enddo
               enddo

           end select
       else
           do i=1,nat-1
              if (zval(i) == 0) cycle
              do j=i+1,nat
                 if (zval(j) == 0) cycle
                 call create_bond(i,j,atom(i)%xc,atom(j)%xc,zval(i),zval(j),tlmin,tlmax,legnew,usecovr)
                 if (legnew%n1 > 0) then  ! se 0 non c'è legame tra i due atomi
                     nleg = nleg + 1
                     if (nleg > ndimleg) then   ! check sulla dimensione di legm
                         ndimleg = ndimleg + nat/2
                         call resize_bonds(legm,ndimleg)
                     endif
                     legm(nleg) = legnew
                 endif
              enddo
           enddo
       endif
       call resize_bonds(legm,nleg)
       if (nleg > 0) then
           if (checkb) then
               call check_angle(atom,cell,legm,angmin,kprb)
           endif 
       endif
   endif
!
   end subroutine create_connectivity_fix

!---------------------------------------------------------------------      

   subroutine create_bond(ka1,ka2,x1,x2,z1,z2,tolmin,tolmax,leg,usecovr)
!
!  Valuta se possibile legame tra ka1,ka2. Se leg%n1 = 0 allora non c'e' legame
!
   USE bondtmod
   USE connect_mod
   USE cgeom
   USE elements
   integer, intent(in)            :: ka1,ka2   ! atom numbers
   real, dimension(3), intent(in) :: x1,x2     ! coordinate cart
   integer, intent(in)            :: z1,z2     ! numeri atomici
   real, intent(in)               :: tolmin,tolmax        ! tolerance
   logical, intent(in)            :: usecovr
   type(bond_type), intent(out)   :: leg       ! legame
   integer                        :: k
   real                           :: dist,distc
   real                           :: bondmin,bondmax
!
   leg%n1 = 0
   bondmax = bond_table_max(1,z1,z2)
   if (bondmax > 0.000) then
!
!      valuta distanza x1-x2 solo se la prima distanza in tabella e' > 0
      !corr xdiff(:) = x1(:) - x2(:)
      !corr dist = sqrt(xdiff(1)**2+xdiff(2)**2+xdiff(3)**2)
       dist = distanzaC(x1,x2)
!
       bondmin = bond_table_min(1,z1,z2) + tolmin
       bondmax = bondmax + tolmax
       if (dist >= bondmin .and. dist <= bondmax) then 
           leg = bond_type(ka1,ka2,dist)
       else
           do k=2,3
              bondmax = bond_table_max(k,z1,z2)
              if (bondmax > 0.000) then
                  bondmin = bond_table_min(k,z1,z2) + tolmin
                  bondmax = bondmax + tolmax
                  if (dist >= bondmin .and. dist <= bondmax) then 
                      leg = bond_type(ka1,ka2,dist)
                  endif
              else
                  exit
              endif
           enddo
       endif
   endif
!
!  Try to force the connectivity using the covalent radii
   if (usecovr .and. leg%n1 == 0) then
   !if (leg%n1 == 0) then
       if (bondmax <= 0.000) dist = distanzaC(x1,x2)
       distc = eleminfo(z1)%c_radius + eleminfo(z2)%c_radius
!corr       if (dist > distc - 0.4 .and. dist <  distc + 0.4) then
!corr           leg = bond_type(ka1,ka2,dist)
!corr       endif
       if (dist > distc*0.82 .and. dist < distc*1.18) then
           leg = bond_type(ka1,ka2,dist)
       endif
   endif
!
   end subroutine create_bond
!---------------------------------------------------------------------      

   subroutine check_atoms(kat,vet,atomc,cell,code)
!
!  Controlla che gli angoli tra kat e gli atomi in vet siano plausibili
!
   USE connect_mod
   USE unit_cell
   integer, intent(in)                         :: kat
   integer, dimension(:), intent(in)           :: vet
   type(atom_type), dimension(:), intent(in)   :: atomc  ! coordinate cartesiane
   type(cell_type), intent(in)                 :: cell
   integer, intent(out)                        :: code
   integer                                     :: i
   type(bond_type), dimension(:), allocatable  :: legm
   type(angle_type), dimension(:), allocatable :: angle
   integer                                     :: nat
   real                                        :: angmin,angmax
   logical                                     :: is_angmax
!
   code = 0
   nat = size(vet)
   if (nat >= 2) then
       call resize_bonds(legm,nat)
       do i=1,nat
          legm(i)%n1 = kat
          legm(i)%n2 = vet(i)
       enddo
!
!      Decidi l'angolo limite sulla base del numero di coordinazione
       select case (nat)
          case (2)  
            angmin = 55
            is_angmax = .false.
          case (3)
            angmin = 100
            angmax = 140
            is_angmax = .true.
          case (4) 
            angmin = 60
            angmax = 150
            is_angmax = .true.
          case (5:)
            ! angmin = 60
            angmin = 50
            is_angmax = .false.
       end select
!
!      Estrai angoli da legm 
       call bond_to_angle(legm,atomc,cell%get_g(),angle,cart=.true.)
       do i=1,size(angle)
          if (angle(i)%val < angmin) then
              code = code + 1
          else
              if (is_angmax) then
                  if (angle(i)%val > angmax) code = code + 1
              endif
          endif
       enddo
   endif
!
   end subroutine check_atoms
!---------------------------------------------------------------------      

   subroutine check_angle(atom,cell,legm,angl,kpr)
!
!  Rimuove angoli minori di angl
!
   USE bondtmod
   USE connect_mod
   USE unit_cell
   type(atom_type), dimension(:), intent(in)                           :: atom ! in coordinate cartesiane
   type(cell_type), intent(in)                                         :: cell
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legm
   real, intent(in), optional                                          :: angl
   integer, intent(in), optional                                       :: kpr
   integer                                                             :: kpra
   type(angle_type), dimension(:), allocatable                         :: angle
   integer                                                             :: numang
   real                                                                :: anglim
   logical                                                             :: delang
   integer                                                             :: n1i,n2i,n3i
   integer                                                             :: n1j,n2j,n3j
   integer                                                             :: del12,del23
   integer                                                             :: nleg1,nleg3
   integer                                                             :: i,j,nang
   real                                                                :: err12,err32
   integer, dimension(:), allocatable :: vang
!
!  Genera gli angoli minori di anglim
   if (present(angl)) then
       anglim = angl
   else
       anglim = 55
   endif
   call bond_to_angle(legm,atom,cell%get_g(),angle,[-1.0,anglim],cart=.true.) ! generazione angoli da coordinate cartesiane
   if (present(kpr)) then
       kpra = kpr
   else
       kpra = 0
   endif
   if (kpra > 0) call print_connect(atom%lab,angle=angle,kpri=kpra)  
!
!  if n1 is 0 the angle is hidden
!
   if (anglim > ANGLIM_METAL) then
!      Metal atoms could have low bond angles
!      Hide angle > anglim_metal and containing metal atoms
       do i=1,numangles(angle)
             !write(0,*)'ZVAL=',atom([angle(i)%n1,angle(i)%n2,angle(i)%n3])%z(),size(atom)
             !write(0,*)'ANG=',angle(i)
          if (any_is_metal(atom([angle(i)%n1,angle(i)%n2,angle(i)%n3])) .and. angle(i)%val > ANGLIM_METAL) then
              angle(i)%n1 = 0
          endif
       enddo
   endif
!
   numang = numangles(angle)
   if (numang > 0) then
       if (kpra > 0) write(6,*)
!
!      Check for atoms in the same site and equally connected (e.g. site(Ca/Eu)--O)
       if (any(atom%och /= 1.0)) then
           do i=1,numang
              n1i = angle(i)%n1
!corr              if (n1i < 0) cycle
              if (n1i == 0) cycle
              n2i = angle(i)%n2
              n3i = angle(i)%n3
              if (angle(i)%val <= 0.05 .and. (atom(n2i)%och /= 1.0 .or. atom(n3i)%och /= 1.0)) then
                  del12 = 0
                  del23 = 0
                  if (atom(n1i)%och > atom(n3i)%och) then
                      del23 = 1
                  elseif (atom(n1i)%och < atom(n3i)%och) then
                      del12 = 1
                  elseif (atom(n1i)%och == atom(n3i)%och) then
                      if (n1i < n3i) then  ! selection considering the order number
                          del23 = 1
                      else
                          del12 = 1
                      endif
                  endif
                  if (del12 == 1) then
                      call get_angles_of_bond(angle,n1i,n2i,vang,nang)
                      !angle(i)%n1 = -angle(i)%n1
                      call remove_bond_from_atoms(legm,n1i,n2i)
                      if (kpra > 0) call print_del_bond(n1i,n2i,angle(i),kpra)
                      angle(vang(:nang))%n1 = 0 !hide angles with bond n1i-n2i
                  elseif (del23 == 1) then
                      call get_angles_of_bond(angle,n2i,n3i,vang,nang)
                      !angle(i)%n1 = -angle(i)%n1
                      call remove_bond_from_atoms(legm,n2i,n3i)
                      if(kpra > 0) call print_del_bond(n2i,n3i,angle(i),kpra)
                      angle(vang(:nang))%n1 = 0 !hide angles with bond n2i-n3i
                  endif
              endif
           enddo
       endif
!
!      Remove angle with duplicate bonds (angle < 0.05)
       do i=1,numang
          if (angle(i)%val < 0.05) then
              if (kpra > 0) write(kpra,*)'REMOVE ANGLE=',  &
                  atom(angle(i)%n1)%glab(),atom(angle(i)%n2)%glab(),atom(angle(i)%n3)%glab()
              if (angle(i)%n1 > angle(i)%n3) then
                  call remove_bond_from_atoms(legm,angle(i)%n1,angle(i)%n2)
              else
                  call remove_bond_from_atoms(legm,angle(i)%n2,angle(i)%n3)
              endif
              angle(i)%n1 = 0
          endif
       enddo
!
!      Rimuovi i legami in comune a piu' angoli
       do i=1,numang-1         
          n1i = angle(i)%n1
          if (n1i == 0) cycle ! angolo da non considerare
          n2i = angle(i)%n2
          n3i = angle(i)%n3
          del12 = 0
          del23 = 0
          do j=i+1,numang
             n1j = angle(j)%n1
             if (n1j == 0) cycle 
             n2j = angle(j)%n2
             n3j = angle(j)%n3
             delang = .false.
             if ((n1i == n1j .and. n2i == n2j) .or. (n1i == n2j .and. n2i == n1j)) then
                 del12 = 1
                 delang = .true.
             elseif ((n2i == n2j .and. n3i == n3j) .or. (n2i == n3j .and. n3i == n2j)) then
                 del23 = 1
                 delang = .true.
             endif
             if (delang) then
                 angle(j)%n1 = 0
             endif
          enddo
          if (del12 == 1 .or. del23 == 1) then
              if (del12 == 1) then
                  call remove_bond_from_atoms(legm,n1i,n2i)
                  if (kpra > 0) call print_del_bond(n1i,n2i,angle(i),kpra)
              endif
              if (del23 == 1) then
                  call remove_bond_from_atoms(legm,n2i,n3i)
                  if(kpra > 0) call print_del_bond(n2i,n3i,angle(i),kpra)
              endif
              angle(i)%n1 = 0
          endif
       enddo
!
!      Rimuovi legame a piu' bassa connettivita'
       do i=1,numang
          n1i = angle(i)%n1
          if (angle(i)%n1 == 0) cycle
          n2i = angle(i)%n2
          n3i = angle(i)%n3
          nleg1 = number_of_bonds(legm,n1i)
          nleg3 = number_of_bonds(legm,n3i)
          if (nleg1 > nleg3) then      ! atomo n1i piu' connesso
              call remove_bond_from_atoms(legm,n2i,n3i)
              if (kpra > 0) call print_del_bond(n2i,n3i,angle(i),kpra)
          elseif (nleg1 < nleg3) then  ! atomo n3i piu' connesso
              call remove_bond_from_atoms(legm,n2i,n1i)
              if (kpra > 0) call print_del_bond(n2i,n1i,angle(i),kpra)
          else                         ! stessa connettivita' su n1i e n3i
!
!             Elimino il legame più sbagliato
              call get_info_distance(atom(n1i),atom(n2i),errd=err12)
              call get_info_distance(atom(n3i),atom(n2i),errd=err32)
              if (err12 > err32) then
                  call remove_bond_from_atoms(legm,n2i,n1i)
                  if (kpra > 0) call print_del_bond(n2i,n1i,angle(i),kpra)
              else
                  call remove_bond_from_atoms(legm,n2i,n3i)
                  if (kpra > 0) call print_del_bond(n2i,n3i,angle(i),kpra)
              endif
          endif
       enddo
   endif
!
   CONTAINS
   
   subroutine print_del_bond(n1b,n2b,ang,kpr)
   integer, intent(in) :: n1b,n2b,kpr
   type(angle_type)    :: ang
   write(kpr,'(2x,a,f0.2)')'Bond '//trim(atom(n1b)%lab)//'-'//trim(atom(n2b)%lab)//' was deleted because the angle '//  &
              trim(atom(abs(ang%n1))%lab)//'-'//trim(atom(ang%n2)%lab)//'-'//trim(atom(ang%n3)%lab)//' is ',ang%val
   end subroutine print_del_bond
!
   end subroutine check_angle

!---------------------------------------------------------------------      

   subroutine create_connectivity_special(atom,cell,dist,legm,esd,z1,z2)
!
!  Cerca legami fra atomi di specie z1 e z2 a distanza dist
!
   USE cgeom
   USE connect_mod
   USE unit_cell
   type(atom_type), dimension(:), intent(in)                 :: atom
   type(cell_type), intent(in)                               :: cell
   real, intent(in)                                          :: dist
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer, intent(in), optional                             :: z1,z2
   real, intent(in), optional                                :: esd
   real                                                      :: esdval
   integer, dimension(size(atom))                            :: zval
   integer, dimension(size(atom),size(atom))                 :: tabconn
   integer                                                   :: i,j
   integer                                                   :: za1,za2
   real                                                      :: distij
   integer                                                   :: nat
!
   if (present(esd))then
       esdval = esd
   else
       esdval = 0.3
   endif
!
   nat = size(atom)
!
   tabconn(:,:) = 0
   if (nat > 1) then
!
!      genera Z di tutti gli atomi
       if (present(z1) .or. present(z2)) then
!corr           zval(:) = atomic_number(atom)
           zval(:) = atom%z()
           if (present(z1)) za1 = z1
           if (present(z2)) then
               za2 = z2
               if (.not.present(z1)) za1 = za2
           else
               za2 = za1
           endif
       else
!
!          nessun controllo controllo sulle specie
           zval(:) = 0
           za1 = 0
           za2 = 0
       endif
!
       do i=1,nat-1
          do j=i+1,nat
             if ((zval(i) == za1 .and. zval(j) == za2) .or. (zval(j) == za1 .and. zval(i) == za2)) then
                 distij = distanzaC(atom(i)%xc,atom(j)%xc,cell%get_g())
                 if (abs(distij - dist) <= esdval) then
                     tabconn(i,j) = 1
                     tabconn(j,i) = 1
                 endif
             endif
          enddo
       enddo
   endif
!
   call tabconn_to_legm(tabconn,atom,cell%get_g(),esdval,legm)
!
   end subroutine create_connectivity_special

!---------------------------------------------------------------------      

   subroutine AppSymmBij(atoms,spg)
   USE spginfom
   type(atom_type), dimension(:), intent(inout) :: atoms
   type(spaceg_type), intent(in)                :: spg
   integer                                      :: ii, j
   real                                         :: bijTmp(3,3) 
!
   do ii=1, size(atoms)
      if (atoms(ii)%bij(1) > 0.0) then
          !bijTmp(1,1) = atoms(ii)%bij(1)
          !bijTmp(2,2) = atoms(ii)%bij(2)
          !bijTmp(3,3) = atoms(ii)%bij(3)
          !bijTmp(1,2) = atoms(ii)%bij(4)
          !bijTmp(2,1) = atoms(ii)%bij(4)
          !bijTmp(1,3) = atoms(ii)%bij(5)
          !bijTmp(3,1) = atoms(ii)%bij(5)
          !bijTmp(2,3) = atoms(ii)%bij(6)
          !bijTmp(3,2) = atoms(ii)%bij(6)
          bijTmp(:,:) = atoms(ii)%get_bmat()
          j = atoms(ii)%op%op
          bijTmp(:,:) = matmul(matmul(transpose(spg%symop(j)%rot(:,:)),bijTmp(:,:)),spg%symop(j)%rot(:,:))
          !atoms(ii)%bij(1) = bijTmp(1,1)
          !atoms(ii)%bij(2) = bijTmp(2,2)
          !atoms(ii)%bij(3) = bijTmp(3,3)
          !atoms(ii)%bij(4) = bijTmp(1,2)
          !atoms(ii)%bij(5) = bijTmp(1,3)
          !atoms(ii)%bij(6) = bijTmp(2,3)
          call atoms(ii)%set_bmat(bijTmp)
      endif
   enddo
!
   end subroutine AppSymmBij

!---------------------------------------------------------------------      

   subroutine get_limit_translation(atoms,ktramin,ktramax)
!
!  Individua la min e la max traslazione
!
   type(atom_type), dimension(:), intent(in)  :: atoms
   integer, dimension(3), intent(out) :: ktramin,ktramax
   integer, dimension(3)              :: ktra
   integer                            :: i
!
   ktramin = atoms(1)%op%tra
   ktramax = atoms(1)%op%tra
   do i=2,size(atoms)
      ktra = atoms(i)%op%tra
      where (ktra < ktramin) ktramin = ktra
      where (ktra > ktramax) ktramax = ktra
   enddo
!
   end subroutine get_limit_translation

!---------------------------------------------------------------------      

   subroutine init_infos(atoms)
!
!  Inizializza informazioni sulla simmetria
!
   type(atom_type), dimension(:), allocatable, intent(inout) :: atoms
   integer                                                   :: i
!
   do i=1,numatoms(atoms)
      atoms(i)%asym = i
      atoms(i)%op = op_type()
   enddo
!
   end subroutine init_infos

!---------------------------------------------------------------------      

   integer function checkeq_symm(atoms,asym,op) result(eq)
!
!  Check equality of op and asym
!
   type(atom_type), dimension(:), intent(in) :: atoms
   integer, intent(in)                       :: asym
   type(op_type), intent(in)                 :: op
   integer                                   :: i
!   
   eq = 0 
   do i=1,size(atoms)
      if (atoms(i)%asym == asym .and. atoms(i)%op == op) then
          eq = i
          return
      endif
   enddo
!
   end function checkeq_symm

!---------------------------------------------------------------------      

   integer function checkeq_symm_new(atoms,xc,asym,op,gmat) result(eq)
!
!  Check equality of op and asym
!
   type(atom_type), dimension(:), intent(in) :: atoms
   real, dimension(3), intent(in) :: xc
   integer, intent(in)                       :: asym
   type(op_type), intent(in)                 :: op
   real, dimension(3,3), intent(in)          :: gmat
   integer                                   :: i
   real, dimension(3)                        :: dx
   real :: d
   real, parameter :: D2MIN = 0.6*0.6 
!   
   eq = 0 
   do i=1,size(atoms)
      if (atoms(i)%asym == asym) then
          if (atoms(i)%op == op) then
              eq = i
              return
          endif
          if (atoms(i)%ocry < 1.0) then
              dx = atoms(i)%xc - xc
              d = DOT_PRODUCT(dx,MATMUL(gmat,dx))
              if (d < D2MIN) then
                  eq = i
                  !write(0,*)'DMIN=',sqrt(d),asym,atoms(i)%ocry
                  return
              endif
          endif
      endif
   enddo
!
   end function checkeq_symm_new

!---------------------------------------------------------------------      

   logical function any_op_equal(atoms,op,vasym)   result(eq)
!
!  Check equality of op
!
   type(atom_type), dimension(:), intent(in) :: atoms
   type(op_type), intent(in)                 :: op
   integer, dimension(:), intent(in)         :: vasym
   integer                                   :: i,j
!
   eq = .false.
   do i=1,size(vasym)
      do j=1,size(atoms)
         if ((atoms(j)%asym == vasym(i)) .and. (atoms(j)%op == op)) then
             eq = .true.
             return
         endif
      enddo
   enddo
!
   end function any_op_equal

!---------------------------------------------------------------------      

   subroutine init_for_symm(atom,legm,atoms,legms)
!
!  Inizialize symmetry by copying asymmetric unit
!
   USE connect_mod
   type(atom_type), dimension(:), allocatable, intent(in)  :: atom
   type(bond_type), dimension(:), allocatable, intent(in)  :: legm
   type(atom_type), dimension(:), allocatable, intent(out) :: atoms
   type(bond_type), dimension(:), allocatable, intent(out) :: legms
   integer                                                 :: i
!
   call copy_atoms(atoms,atom)
   call copy_bonds(legms,legm)
   do i=1,numatoms(atoms)
      atoms(i)%asym = i
   enddo
!bug: seg. fault if natoms is 0   atoms%asym = [(i,i=1,numatoms(atoms))]
!
   end subroutine init_for_symm

!--------------------------------------------------------------------------------------------------

   subroutine connect_groups(atom1,atom2,legm,usecov)
!
!  Cerca connettivita' tra 2 gruppi di atomi in coordinate cartesiane
!
   USE bondtmod
   USE connect_mod
   USE cgeom
   type(atom_type), dimension(:), intent(in)                 :: atom1,atom2
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   logical, intent(in)                                       :: usecov
   integer                                                   :: nat1,nat2
   integer                                                   :: i,j  
   integer                                                   :: nleg
   integer, dimension(size(atom1))                           :: zval1
   integer, dimension(size(atom2))                           :: zval2
   type(bond_type)                                           :: legnew
   real                                                      :: tolmin,tolmax
!
   nat1 = size(atom1)
   nat2 = size(atom2)
   if (nat1 == 0 .or. nat2 == 0) return
!
!  genera Z di tutti gli atomi
   zval1(:) = atom1%z()
   zval2(:) = atom2%z()
!
   call new_bonds(legm,nat1*nat2)
!
   tolmin = 0.0
   tolmax = 0.0
   nleg = 0
   do i=1,nat1
      if (zval1(i) /= 0) then
          do j=1,nat2
             if (zval2(j) /= 0) then
                 call create_bond(i,j,atom1(i)%xc,atom2(j)%xc,zval1(i),zval2(j),tolmin,tolmax,legnew,usecov)
                 if (legnew%n1 > 0) then  ! se 0 non c'è legame tra i due atomi
                     nleg = nleg + 1
                     legm(nleg) = legnew
                 endif
             endif
          enddo
      endif
   enddo
!
   call resize_bonds(legm,nleg)
!
   end subroutine connect_groups

!---------------------------------------------------------------------      

   subroutine translate_in_cellv(atom,xtra)
!
!  Trasla in cella in gruppo di atomi
!
   type(atom_type), dimension(:), intent(inout) :: atom
   real, dimension(3), optional, intent(out)    :: xtra
   integer                                      :: nat
   real, dimension(3)                           :: barini,barfin
   real, dimension(3)                           :: xtrasl
!
   nat = size(atom)
!
   if (is_bar_out_of_cell(atom)) then
       barini(:) = baricentro(atom)
       barfin(:) = mod(barini(:)+10,1.0)
       xtrasl(:) = barfin(:) - barini(:)
       call translate_atoms(atom,xtrasl)
       if (present(xtra)) xtra(:) = xtrasl(:)
   else
       if (present(xtra)) xtra(:) = 0
   endif
!
   end subroutine translate_in_cellv

!---------------------------------------------------------------------      

   subroutine translate_in_cells(atom,xtra)
!
!  Trasla in cella gruppo di atomi
!
   type(atom_type), intent(inout)            :: atom
   real, dimension(3), optional, intent(out) :: xtra
   real, dimension(3)                        :: xtrasl
!
!corr   if (is_bar_out_of_cell([atom])) then
   if (any(atom%xc(:) >= 1.0 .or. atom%xc(:) < 0.0)) then
       xtrasl(:) = mod(atom%xc+10,1.0) - atom%xc
       call translate_atoms(atom,xtrasl)
       if (present(xtra)) xtra(:) = xtrasl(:)
   else
       if (present(xtra)) xtra(:) = 0
   endif
!
   end subroutine translate_in_cells

!---------------------------------------------------------------------      

   logical function is_bar_out_of_cell(atom)
!
!  Controlla se il baricentro e' fuori cella
!
   type(atom_type), dimension(:), intent(in) :: atom
   real, dimension(3) :: bar
!
   bar(:) = baricentro(atom)
   is_bar_out_of_cell = any(bar(:) >= 1.0 .or. bar(:) < 0.0) 
!
   end function is_bar_out_of_cell

!---------------------------------------------------------------------      

   logical function is_out_of_cell(atom)
!
!  Sono tutti gli atomi fuori cella?
!
   type(atom_type), dimension(:), intent(in) :: atom
   integer                                   :: nat
   integer                                   :: i
   integer                                   :: nin
   real                                      :: tol = 4.0*epsilon(1.0)
!
   nat = size(atom)
   nin = 0
   do i=1,nat
      if (any(atom(i)%xc < 0.0-tol .or. atom(i)%xc >= 1.0+tol)) then 
          nin = nin + 1
      endif
   enddo
   is_out_of_cell = nin == nat
!
   end function is_out_of_cell

!---------------------------------------------------------------------      

   logical function is_out_of_range(atom,inf,sup,code)
!
!  Atoms fuori dal range?
!
   type(atom_type), dimension(:), intent(in) :: atom
   real, dimension(3), intent(in)            :: inf,sup
   integer, intent(in)                       :: code
   real, dimension(3)                        :: xbar
   integer                                   :: nat
   integer                                   :: i
   integer                                   :: nin
!
   nat = size(atom)
   nin = 0
   select case (code)
      case (1) !  is centroid out of range?
        xbar = baricentro(atom)
        is_out_of_range = any(xbar < inf .or. xbar > sup)

      case (2)               ! all atoms out of range?
        do i=1,nat
           if (any(atom(i)%xc < inf .or. atom(i)%xc > sup)) then
               nin = nin + 1
           endif
        enddo
        is_out_of_range = nin == nat

      case (3)               ! just an atom out of range?
        is_out_of_range = .false.
        do i=1,nat
           if (any(atom(i)%xc < inf .or. atom(i)%xc > sup)) then
               is_out_of_range = .true.
               exit
           endif
        enddo
   end select
!
   end function is_out_of_range

!---------------------------------------------------------------------      

   function baricentro(at)
!
!  Calcola il baricentro di at
!   
   type(atom_type), dimension(:), intent(in) :: at
   real, dimension(3)                        :: baricentro
   integer                                   :: i
   integer                                   :: numat
!
   numat = size(at)
   do i=1,3
      baricentro(i) = sum(at%xc(i)) / numat
   enddo
!   
   end function baricentro

!---------------------------------------------------------------------      

   subroutine get_info_distance(atom1,atom2,pos,dist,errd)
!
!  Info sulla distanza tra 2 atomi in coordinate cartesiane
!
   USE connect_mod
   USE elements
   type(atom_type), intent(in)    :: atom1,atom2 ! atomi in coordinate cartesiane
   integer, optional, intent(out) :: pos         ! puntatore alla distanza sulla tabella dei legami
   real, optional, intent(out)    :: dist        ! distanza tra i 2 atomi
   real, optional, intent(out)    :: errd        ! deviazione dal valore in tabella
   integer                        :: z1,z2
   integer                        :: kpos
   real                           :: dist12, distt, diffmin
!
   z1 = atom1%z()
   z2 = atom2%z()
!   
   dist12 = sqrt(dot_product(atom1%xc-atom2%xc,atom1%xc-atom2%xc))
!
   call bond_info(z1,z2,dist12,kpos,distt,diffmin)
   if (present(pos)) pos = kpos
   if (present(errd)) errd = diffmin
   if (present(dist)) then
       if (kpos > 0) then
           dist = distt
       else
           !dist = dist12
!          suggest distance from radius
           dist = eleminfo(z1)%c_radius + eleminfo(z2)%c_radius
       endif
   endif
!
   end subroutine get_info_distance

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

   real function bond_distanceAT(at1,at2) result(dist)

!  Find distance from table, if absent compute from radius
!
   type(atom_type), intent(in) :: at1,at2
!
   dist = bond_distance(at1%z(),at2%z())
!
   end function bond_distanceAT

!---------------------------------------------------------------------      

   real function bond_distance_max(z1,z2)  result(distmax)
!
!  Assign the maximum distance
!
   USE elements
   USE bondtmod
   integer, intent(in) :: z1,z2
   real                :: dist
   integer             :: k
!
   if (z1 == 0 .or. z2 == 0 .or. z1 > N_ELEMENTS .or. z2 > N_ELEMENTS) then
       distmax = 0.0
   else
       distmax = bond_table_max(1,z1,z2)
       if (distmax > 0.0) then
           do k=2,3
              dist = bond_table_max(k,z1,z2)
              if (dist > 0.000) then
                  if (dist >  distmax) distmax = dist
              else
                  exit
              endif
           enddo
       else ! distance absent on table: use covelent radius
           distmax = eleminfo(z1)%c_radius + eleminfo(z2)%c_radius + 0.4
       endif
   endif
!
   end function bond_distance_max

!---------------------------------------------------------------------      

   real function bond_distance_min(z1,z2)  result(distmin)
!
!  Assign the minimum distance
!
   USE elements
   USE bondtmod
   integer, intent(in) :: z1,z2
   real                :: dist
   integer             :: k
!
   if (z1 == 0 .or. z2 == 0 .or. z1 > N_ELEMENTS .or. z2 > N_ELEMENTS) then
       distmin = 0.0
   else
       distmin = bond_table_min(1,z1,z2)
       if (distmin > 0.0) then
           do k=2,3
              dist = bond_table_min(k,z1,z2)
              if (dist > 0.000) then
                  if (dist <  distmin) distmin = dist
              else
                  exit
              endif
           enddo
       else ! distance absent on table: use covelent radius
           distmin = eleminfo(z1)%c_radius + eleminfo(z2)%c_radius - 0.4
       endif
   endif
!
   end function bond_distance_min

!---------------------------------------------------------------------      

   subroutine get_radius_molecule(atom,radmax,bar)
!
!  Assimila la molecola ad una sfera e calcola il suo raggio come massima distanza dal baricentro
!
   USE cgeom
   type(atom_type), dimension(:), intent(in) :: atom  ! in coordinate cartesiane
   real, intent(out)                         :: radmax
   real, dimension(3), intent(out), optional :: bar
   real, dimension(3)                        :: barc
   integer                                   :: i
   real                                      :: rad
!
   barc = baricentro(atom)
!
   radmax = distanzaC(barc,atom(1)%xc)
   do i=2,size(atom)
      rad = distanzaC(barc,atom(i)%xc)
      if (rad > radmax) radmax = rad
   enddo
!
   if (present(bar)) then
       bar(:) = barc(:)
   endif
!
   end subroutine get_radius_molecule

!---------------------------------------------------------------------      

   subroutine save_atoms(atoms)
!
!  Genera copia degli atomi
!
   type(atom_type), dimension(:), intent(in) :: atoms
   integer                       :: natoms
!
   natoms = size(atoms)  
   call resize_atoms(atoms_copy,natoms)
   atoms_copy(:) = atoms(:)
!
   end subroutine save_atoms

!---------------------------------------------------------------------      

   subroutine restore_atoms(atoms,natoms)
!
!  Ripristina copia degli atomi
!
   type(atom_type), dimension(:), allocatable, intent(inout) :: atoms
   integer, intent(out)                                      :: natoms
!
   if (allocated(atoms_copy)) then
       natoms = size(atoms_copy)
       call new_atoms(atoms,natoms)
       atoms(:) = atoms_copy(:)
   endif
!
   end subroutine restore_atoms

!---------------------------------------------------------------------      

   subroutine clear_saved_atoms()
!
!  Cancella la copia degli atomi
!
   call clear_atoms(atoms_copy)
   end subroutine clear_saved_atoms

!---------------------------------------------------------------------      

   subroutine copy_atoms(atom1,atom2)
!
!  Copia atom2 in atom1
!
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom1
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom2
   integer                                                   :: nat2,nat1
!
   nat2 = numatoms(atom2) 
   nat1 = numatoms(atom1) 
   if (nat1 == nat2) then
       if (nat1 /= 0) atom1 = atom2
   else
       if (allocated(atom1)) deallocate(atom1)
       if (nat2 > 0) then
           allocate(atom1(nat2),source=atom2)
       endif
   endif
!
   end subroutine copy_atoms

!---------------------------------------------------------------------      

   subroutine remove_atoms_from_list(atom,veta,val,legm,iord)
!
!  Cancella dalla lista tutti gli atomi per i quali vet e' uguale a val
!
   USE connect_mod
   type(atom_type), dimension(:), allocatable, intent(inout)           :: atom
   integer, dimension(:), intent(in)                                   :: veta
   integer                                                             :: val
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legm
   integer, dimension(:), optional                                     :: iord
   integer                                                             :: nat
   integer                                                             :: nrem
   integer                                                             :: nnat
   integer                                                             :: j1,j2
   integer                                                             :: i
   integer, dimension(size(atom))                                      :: vetnew
   integer, dimension(size(atom))                                      :: iorder
!
   nrem = count(veta(:) == val)
   if (nrem > 0) then
       nat = numatoms(atom)
       nnat = nat - nrem
       j1 = 0
       j2 = nnat
       do i=1,nat
          if (veta(i) /= val) then
              j1 = j1 + 1
              vetnew(j1) = i
              iorder(i) = j1
          else
              j2 = j2 + 1
              vetnew(j2) = i
              iorder(i) = 0
          endif
       enddo
       atom(:nat) = atom(vetnew)
       call resize_atoms(atom,nnat)
!
!      aggiorna la connettivita' se richiesto
       if (present(legm)) then   ! aggiorna la connettivita' se richiesto
           call remove_bond_from_atom(legm,vetnew(nnat+1:),corr=.true.)
       endif
!
!      iord e' tale che iord(vecchia posizione) = nuova posizione
       if (present(iord)) then
           do i=1,nat
              iord(i) = iorder(i)
           enddo
       endif
   else
       if (present(iord)) then
           iord(:) = [(i,i=1,size(iord))]
       endif
   endif
!
   end subroutine remove_atoms_from_list

!---------------------------------------------------------------------      

   subroutine remove_atoms_sym(atom,atoms,legm,legms,veta,iorder)
!
!  Rimuovi gli atomi in vet con gli equivalenti
!
   USE connect_mod
   USE atom_basic
   type(atom_type), dimension(:), allocatable, intent(inout)     :: atom,atoms
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legm,legms
   integer, dimension(:), intent(in)                             :: veta
   integer, dimension(:), allocatable, intent(out)               :: iorder
   integer, dimension(size(veta))                                :: vetac
   integer                                                       :: nptra
   integer                                                       :: i
!
   nptra = size(veta)
   do i=1,nptra
      vetac(i) = atoms(veta(i))%asym ! prendi l'asimmetrico corrispondente
      call atom(vetac(i))%set_as_deleted()
   enddo
!
!  Loop to delete atoms in the unit cell
   do i=1,size(atoms)
      if (any(vetac == atoms(i)%asym)) then
          call atoms(i)%set_as_deleted()
      endif
   enddo
!
!  Elimina atomi
   allocate(iorder(size(atoms)))
   call remove_deleted(atom,legm=legm,iord=iorder)   ! elimina dall'u.a.
   atoms%asym = iorder(atoms%asym)                   ! update asym for atoms in the unit cell
   call remove_deleted(atoms,legm=legms,iord=iorder) ! delete atoms in the unit cell
!
   end subroutine remove_atoms_sym

!---------------------------------------------------------------------      

   subroutine remove_atoms_vet(atom,legm,veta,iord,keep)
!
!  Remove atoms in array veta. If keep is true only atoms in vet are kept
!
   USE connect_mod
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer, dimension(:), intent(in)                         :: veta
   integer, dimension(:), allocatable, intent(out), optional :: iord
   logical, intent(in), optional                             :: keep
   integer                                                   :: i
   logical                                                   :: keep1
!
   if (present(keep)) then
       keep1 = keep
   else
       keep1 = .false.
   endif
!
   if (keep1) then
       do i=1,size(atom)
          if (any(veta(:) == i)) cycle
          call atom(i)%set_as_deleted()
       enddo
   else
       do i=1,size(veta)
          call atom(veta(i))%set_as_deleted()
       enddo
   endif
!
   if (present(iord)) then
       allocate(iord(size(atom)))
       call remove_deleted(atom,legm=legm,iord=iord)
   else
       call remove_deleted(atom,legm=legm)
   endif
!
   end subroutine remove_atoms_vet

!----------------------------------------------------------------------------------------------------

   subroutine delete_ghosts(atom,legm)
!
!  Delete ghost atoms (ptab=nz=0)
!
   USE connect_mod
   type(atom_type), dimension(:), allocatable, intent(inout)           :: atom
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legm
!
   if (numatoms(atom) == 0) return
   if (present(legm)) then
       call remove_atoms_from_list(atom,atom%ptab,0,legm)
   else
       call remove_atoms_from_list(atom,atom%ptab,0)
   endif
!
   end subroutine delete_ghosts

!---------------------------------------------------------------------      

   subroutine remove_deleted(atom,legm,iord,noresize)
!
!  Cancella dalla lista tutti gli atomi per i quali vet e' uguale a val
!
   USE connect_mod
   type(atom_type), dimension(:), allocatable, intent(inout)           :: atom
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legm
   integer, dimension(:), optional                                     :: iord
   logical, intent(in), optional                                       :: noresize
   integer                                                             :: nat
   integer                                                             :: nrem
   integer                                                             :: nnat
   integer                                                             :: j1,j2
   integer                                                             :: i
   integer, dimension(size(atom))                                      :: vetnew
   integer, dimension(size(atom))                                      :: iorder
!
   nrem = count(atom%get_nz() == DELETED_ATOM)
   if (nrem > 0) then
       nat = numatoms(atom)
       nnat = nat - nrem
       j1 = 0
       j2 = nnat
       do i=1,nat
          if (atom(i)%get_nz() /= DELETED_ATOM) then
              j1 = j1 + 1
              vetnew(j1) = i
              iorder(i) = j1
          else
              j2 = j2 + 1
              vetnew(j2) = i
              iorder(i) = 0
          endif
       enddo
       atom(:nat) = atom(vetnew)
       if (.not. present(noresize))then
           call resize_atoms(atom,nnat)
       else
           if (.not.noresize) call resize_atoms(atom,nnat)
       endif
!
!      aggiorna la connettivita' se richiesto
       if (present(legm)) then   ! aggiorna la connettivita' se richiesto
           call remove_bond_from_atom(legm,vetnew(nnat+1:),corr=.true.)
       endif
!
!      iord e' tale che iord(vecchia posizione) = nuova posizione
       if (present(iord)) then
           do i=1,nat
              iord(i) = iorder(i)
           enddo
       endif
   else
       if (present(iord)) then
           iord(:) = [(i,i=1,size(iord))]
       endif
   endif
!
   end subroutine remove_deleted

!---------------------------------------------------------------------      

   integer function is_organic_z(zval,nzval)
!
!  is_organic = 1 -> organic
!             = 2 -> organometallic                
!             = 0 -> inorganic
!
   USE elements
   integer, dimension(:), intent(in) :: zval   ! Z of atomic species
   integer, dimension(:), intent(in) :: nzval  ! number of atomic species
   integer, dimension(size(zval))    :: inorg
   integer                           :: i
   integer                           :: nsp
   real                              :: perci
   integer                           :: sinorg
   integer                           :: numO,numOi
!
!  A species, organic or organometallic, must contain C (Z=6)
   if (any(zval(:) == C_at)) then
       nsp = size(zval)
!
!      the other species must be H(1),N(7),O(8),P(15),S(16),F(9),Cl(17),Br(35),I(53)
!      store in the inorg vector the inorganic species, i.e. different from vetorg
       do i=1,nsp
          !if (any(org_elements(:) == zval(i))) then
          if (is_organic_el(zval(i))) then
              inorg(i) = 0   
          else
              inorg(i) = nzval(i)
          endif
       enddo
!
       sinorg = sum(inorg(:)) ! sum of inorganic species
       if (sinorg == 0) then
!           
!          there are no inorganic species
           is_organic_z = 1
       else
!
!          But O could also belong to the inorganic part (e.g. zeolites)
           numO = number_for_specie(O_at,zval,nzval)  ! number of O
           if (numO > 0) then
               numOi = 0
               do i=1,nsp
                  if (inorg(i) > 0 .or. zval(i) == P_at) then   ! P is also inorganic if O are present
!                     oxygen for inorganic atom = 0.7*oxidation number 
                      numOi = numOi + nint(0.7*oxidation_number(zval(i))) * nzval(i)
                  endif
               enddo
               if (numOi > numO) numOi = numO
               sinorg = sinorg + numOi
           endif
           perci=100*sinorg/sum(nzval(:))
!
           if (perci <= 20) then
               is_organic_z = 2    ! organometallic
           else
               is_organic_z = 0    ! inorganic
           endif
       endif
   else  
!
!      if it doesn't contain C it's inorganic
       is_organic_z = 0
   endif
!
   end function is_organic_z

!---------------------------------------------------------------------      

   integer function is_organic_atm(atom)
   USE elements
   type(atom_type), dimension(:), intent(in)     :: atom
   type(element_type), dimension(:), allocatable :: elem
!   
   call elem_from_atoms(elem,atom%ptab)
   if (numelem(elem) > 0) then
       is_organic_atm = is_organic(elem%z,nint(elem%nw))
   else
       is_organic_atm = 0
   endif
!
   end function is_organic_atm

!---------------------------------------------------------------------      

   subroutine sort_by_specie(atom,legm,cell,iorder)
!
!  Ordine atomi in base alla specie e al seriale della label
!
   USE nr
   USE connect_mod
   USE atom_basic
   USE cgeom
   USE unit_cell
   type(atom_type), dimension(:), intent(inout)                        :: atom
   type(bond_type), dimension(:), allocatable, intent(inout), optional :: legm
   type(cell_type), intent(in)                                         :: cell
   integer, dimension(size(atom)), optional                            :: iorder
   integer, dimension(size(atom))                                      :: iord
   integer                                                             :: nat
   integer                                                             :: i
   integer, dimension(size(atom))                                      :: iser
   type(atom_type), dimension(size(atom))                              :: atomc
   character(len=100)                                                  :: sprefix
   integer                                                             :: ierror
   integer                                                             :: nleg
!
!  Estrai dalla label il seriale. (es. C1 prima di C2, C3)
   do i=1,size(atom)
      call get_from_label(atom(i)%lab,sprefix,iser(i),ierror)
   enddo
!
!  Genera nuovo indice degli atomi in base al numero atomico
!corr   call indexx(atomic_number(atom)*1000-iser,iord)
   call indexx(atom%z()*1000-iser,iord)
   nat = size(atom)
   iord = iord(nat:1:-1)
!
!  Ordina gli atomi
   atom(:) = atom(iord)
!
!  Ordina i legami
   if (present(legm)) then
       nleg = numbonds(legm)
       if (nleg > 0) then
           iord(iord) = (/(i,i=1,nat)/)  ! per i legami serve un indice inverso
           legm(:)%n1 = iord(legm(:)%n1)
           legm(:)%n2 = iord(legm(:)%n2)
!
!          ricalcola le distanze
           atomc(:) = atom(:)
           call frac_to_cart(atomc,cell%get_ortom())
           do i=1,nleg
              legm(i)%dist = distanzaC(atomc(legm(i)%n1)%xc,atomc(legm(i)%n2)%xc)
           enddo
       endif
   endif
!
   if (present(iorder)) then
       iorder(:) = iord(:)
   endif
!
   end subroutine sort_by_specie

!---------------------------------------------------------------------      

   subroutine force_connectivity(atom,legm,kat,cell)
!
!  Forza la connettivita' per l'atomo kat
!
   USE connect_mod
   USE cgeom
   USE unit_cell
   type(atom_type), dimension(:), intent(in)                 :: atom  
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   integer                                                   :: kat
   type(cell_type), intent(in)                               :: cell
   integer, dimension(1)                                     :: vet1,vet2
   integer                                                   :: kfind
   integer                                                   :: i
   real                                                      :: dist,distmin
!
!  Cerca l'atomo piu' vicino a kat
   distmin = huge(1.0)
   kfind = 0
   do i=1,size(atom)
      if (i == kat) cycle
      dist = distanzac(atom(i)%xc,atom(kat)%xc,cell%get_g())
      if (dist < distmin) then
          kfind = i
          distmin = dist
      endif
   enddo
!
!  Aggiungi il legame
   if (kfind > 0) then
       vet1(1) = kat
       vet2(1) = kfind
       call add_bonds(legm,atom,vet1,vet2,cell)
   endif
!
   end subroutine force_connectivity

!---------------------------------------------------------------------      

   subroutine renumber_atoms(atom,veta,string,iser,vetd,nd)
!
!  Renumber of atoms
!
   USE strutil
   type(atom_type), dimension(:), intent(inout)          :: atom
   integer, dimension(:), intent(in)                     :: veta   ! atomi da rinumerare
   character(len=*), intent(in)                          :: string ! prefisso della label; se vuota si usa la specie
   integer, intent(in)                                   :: iser   ! inizio della numerazione
   integer, dimension(size(veta)), intent(out), optional :: vetd   ! restituisce le stringhe che si duplicano
   integer, optional                                     :: nd     ! numero di duplicazioni
   integer                                               :: i
   integer                                               :: kat
   logical                                               :: is_string
   integer, dimension(size(veta))                        :: vetdd
   integer                                               :: ndouble
   integer                                               :: lens
!
   ndouble = 0    ! conta le stringa che verranno eventualmente duplicate
   lens = len_trim(string)
   do i=1,size(veta)
      kat = veta(i)
      if (atom(kat)%z() > 0) then   ! ghost atom?
          if (lens == 0) then    ! renumber usando la specie come prefisso
              write(atom(kat)%lab,'(a,i0)')trim(atom(kat)%specie()),iser+i-1
          else
              write(atom(kat)%lab,'(a,i0)')trim(string),iser+i-1
          endif
          is_string = string_locate(atom(kat)%lab,atom(:)%lab,vet=veta) > 0  ! controlla che la nuova stringa non esista gia'
          if (is_string) then         ! memorizza le duplicazioni di stringa
              ndouble = ndouble + 1
              vetdd(ndouble) = kat
          endif
      endif
   enddo
!
!  Salva in uscita le duplicazioni
   if (present(vetd) .and. present(nd)) then
       nd = ndouble
       vetd(:nd) = vetdd(:nd)
   endif
!
   end subroutine renumber_atoms

!---------------------------------------------------------------------      

   subroutine get_minimum_distance(at,atom,kmin,dmin)
!
!  Distanza minima di at dagli atomi in atom in cartesiane
!
   USE cgeom
   type(atom_type), intent(in)                 :: at    
   type(atom_type), dimension(:), intent(in)   :: atom 
   integer, intent(out)                        :: kmin ! atom(kmin) a distanza minima
   real, intent(out)                           :: dmin
   real                                        :: dist
   integer                                     :: i
!
   dmin = distanzaC(at%xc,atom(1)%xc)
   kmin = 1
   do i=2,size(atom)
      dist = distanzaC(at%xc,atom(i)%xc)
      if (dist < dmin) then
          dmin = dist
          kmin = i
      endif
   enddo
!
   end subroutine get_minimum_distance

!---------------------------------------------------------------------      

   subroutine get_minimum_distance_sym(at,atom,cell,spg,kmin,dmin,xeqmin)
!
!  Minimum distance of at from array atom considering symmetry
!
   USE cgeom
   USE unit_cell
   USE spginfom
   type(atom_type), intent(in)               :: at    
   type(atom_type), dimension(:), intent(in) :: atom 
   type(cell_type), intent(in)               :: cell
   type(spaceg_type), intent(in)             :: spg
   integer, intent(out)                      :: kmin ! atom(kmin) a distanza minima
   real, intent(out)                         :: dmin
   real, dimension(3), intent(out)           :: xeqmin
   real                                      :: dist
   integer                                   :: i
   real, dimension(3)                        :: xeq
!
   dmin = huge(0.0)
   kmin = 0
   do i=1,size(atom)
      call distance_equivalent(atom(i)%xc,at%xc,cell%get_g(),spg,dist,xeq,1)
      if (dist < dmin) then
          dmin = dist
          kmin = i
          xeqmin = xeq
      endif
   enddo
     !write(0,*)'DIST=',dmin,distanzaC(xeqmin,atom(kmin)%xc,cell%get_g())
!
   end subroutine get_minimum_distance_sym

!---------------------------------------------------------------------      

   subroutine get_atoms_distance(at,atom,vetd,distv,vexcl)
!
!  Calcola distanza di at dagli atomi in atom (in coord. cartesiane)
!
   USE nr
   USE cgeom
   type(atom_type), intent(in)                 :: at    
   type(atom_type), dimension(:), intent(in)   :: atom 
   integer, dimension(:), intent(out)          :: vetd  ! contine gli atomi ordinati dal piu' vicino
   real, dimension(:), intent(out), optional   :: distv ! le distanze corrispondenti
   integer, dimension(:), intent(in), optional :: vexcl ! atomi da escudere in atom
   integer                                     :: i
   integer                                     :: nd
   integer, dimension(size(atom))              :: vete,vetnd
   real, dimension(size(atom))                 :: distvi
!
   vete(:) = 0
   if (present(vexcl)) then
       vete(vexcl) = 1
   endif
!
   nd = 0
   do i=1,size(atom)
      if (vete(i) == 1) cycle   ! atomo da escludere
      nd = nd + 1
      vetnd(nd) = i   ! memorizza il puntatore ad atom
      distvi(nd) = distanzaC(at%xc,atom(i)%xc)
   enddo
!
!  Genera indece vetd
   call indexx(distvi(:nd),vetd(:nd))
!
!  Ordina le distanze
   if (present(distv)) then
       distv(:nd) = distvi(vetd(:nd))
   endif
!
   vetd(:nd) = vetnd(vetd(:nd))
!
   end subroutine get_atoms_distance

!---------------------------------------------------------------------      

   !subroutine get_groups_distance(atm1,atm2,dmin,dmed)
   !USE cgeom
   !type(atom_type), dimension(:), intent(in) :: atm1,atm2 
   !integer, dimension(size(atm1))            :: z1
   !integer, dimension(size(atm2))            :: z2
   !real, intent(out)                         :: dmin, dmed
   !integer                                   :: nat1, nat2
   !integer                                   :: i,j
   !real                                      :: dist
   !integer                                   :: btype
   !real                                      :: distab
!!
!   nat1 = size(atm1)
!   nat2 = size(atm2)
!   z1(:) = atomic_number(atm1)
!   z2(:) = atomic_number(atm2)
!   dmin = huge(1.0)
!   dmed = 0
!   do i=1,nat1
!      do j=1,nat2
!         dist = distanzaC(atm1(i)%xc,atm2(j)%xc)
!         call bond_info(z1(i),z2(j),dist,btype,distab)
!         if (dist < dmin) then
!             dmin = dist
!         endif
!         dmed = dmed + dist
!      enddo
!   enddo
!   dmed = dmed / (nat1*nat2)
!
!   end subroutine get_groups_distance

!---------------------------------------------------------------------      

   subroutine get_distance_radius(atom,cell,spg,kat,distm,mode,kfound,vat,vdist,vsym,atoms)
!
!  Trova gli atomi a distanza dist da kat
!
   USE cgeom
   USE nr
   USE unit_cell
   USE spginfom
   type(atom_type), dimension(:), intent(in)         :: atom       ! tutti gli atomi 
   type(cell_type), intent(in)                       :: cell
   type(spaceg_type), intent(in)                     :: spg
   integer, intent(in)                               :: kat        ! atomo su cui eseguire la ricerca
   real, intent(in)                                  :: distm      ! raggio della sfera entro cui cercare gli atomi
   integer, intent(in)                               :: mode       ! modalita' di ricerca (leggi sotto)
   integer, intent(out)                              :: kfound     ! numero di trovati
   integer, dimension(:), allocatable, intent(inout), optional :: vat   ! atomi trovati e operatore di simmetria corrispondente
   type(op_type), dimension(:), allocatable, intent(inout), optional :: vsym   ! atomi trovati e operatore di simmetria corrispondente
   type(atom_type), dimension(:), allocatable, intent(in), optional :: atoms  ! info sulla simmetria
   real, dimension(:), allocatable, intent(inout), optional    :: vdist      ! distanze
   type(atom_type)                                   :: atomk 
   real, dimension(3)                                :: ktra
   real                                              :: dd
   integer                                           :: ndist
   integer                                           :: i,k
   integer                                           :: nat
   real, dimension(spg%nsymop*27)         :: disteq
   type(op_type), dimension(spg%nsymop*27)      :: vds
   integer                                           :: ndisteq
   integer, dimension(:), allocatable                :: iord
   integer, dimension(1)                             :: loc
   integer                                           :: ndim
   type(atom_type)                                   :: atom1  
   integer                                           :: k1,k2,k3
   type(op_type)                                           :: opcode
   real, dimension(3) :: diff
!
   nat = size(atom)
   ndist = 0
   atomk = atom(kat)
   ndim = nat
   if (present(vat)) call reallocate_dist(ndim)
   kfound = 0
!
   select case (mode)   ! find atoms when symmetry is applyied
      case (1)
        do i=1,nat
           if (i > 1 .and. any(atoms(:i-1)%asym == atoms(i)%asym)) cycle  ! skip symmetry equivalent atoms
           do k=1,spg%nsymop
              atom1 = atom_symm(atom(i),spg%symop(k))
              diff(:) = nint(atomk%xc - atom1%xc)
              do k1=-1,1
                 do k2=-1,1
                    do k3=-1,1
                       ktra = (/k1,k2,k3/) + diff(:)
                       dd = distanzaC(atomk%xc,atom1%xc+ktra,cell%get_g())
                       if (atoms(i)%asym == atoms(kat)%asym .and. dd <= 0.001) cycle
                       if (dd <= distm) then
!
!                          Ora accetta il nuovo atomo
                           kfound = kfound + 1
                           if (present(vat)) then
                               if (ndim < kfound) then
                                   ndim = ndim + nat
                                   call reallocate_dist(ndim)
                               endif
                               vat(kfound) = i       
                               vdist(kfound) = dd
                               vsym(kfound) = op_type(k,nint(ktra))
                           endif
                       endif
                    enddo
                 enddo
              enddo
           enddo
        enddo

      case (2)          ! cerca considerando anche gli equivalenti
        do i=1,nat
           do k=1,spg%nsymop
              atom1 = atom_symm(atom(i),spg%symop(k))
              diff(:) = nint(atomk%xc - atom1%xc)
              do k1=-1,1
                 do k2=-1,1
                    do k3=-1,1
                       ktra = (/k1,k2,k3/) + diff(:)
                       opcode = op_type(k,nint(ktra))
                       if (opcode == op_type() .and. i==kat) cycle
                       dd = distanzaC(atomk%xc,atom1%xc+ktra,cell%get_g())
                       if (dd <= distm) then
!
!                          Ora accetta il nuovo atomo
                           kfound = kfound + 1
                           if (present(vat)) then
                               if (ndim < kfound) then
                                   ndim = ndim + nat
                                   call reallocate_dist(ndim)
                               endif
                               vat(kfound) = i       
                               vdist(kfound) = dd
                               vsym(kfound) = opcode
                           endif
                       endif
                    enddo
                 enddo
              enddo
           enddo
        enddo
!!!!!prova
        !do i=1,nat
        !   call distance_equivalent(atomk%xc,atom(i)%xc,xgg,dd)
        !   if (dd < distm) then
        !       write(0,*)'atom:'//trim(atom(i)%lab),dd
        !   endif
        !enddo
!!!!!prova

      case (3)     ! per ogni atomo fornisce solo l'equivalente piu' vicino
        do i=1,nat
           ndisteq = 0
           do k=1,spg%nsymop
              !atom1(1) = atom(i)
              !call applica_sym_oper(k,atom1)
!corr              atom1 = atom_symm(k,atom(i))
              atom1 = atom_symm(atom(i),spg%symop(k))
              do k1=-2,2
                 do k2=-2,2
                    do k3=-2,2
                       ktra = (/k1,k2,k3/)
                       opcode = op_type(k,nint(ktra))
                       if (opcode == op_type() .and. i==kat) cycle
                       dd = distanzaC(atomk%xc,atom1%xc+ktra,cell%get_g())
                       if (dd <= distm) then
                           ndisteq = ndisteq + 1
                           disteq(ndisteq) = dd
                           vds(ndisteq) = opcode
                       endif
                    enddo
                 enddo
              enddo
           enddo
           if (ndisteq > 0) then
               loc = minloc(disteq(:ndisteq))
               kfound = kfound + 1
               vat(kfound) = i       
               vdist(kfound) = disteq(loc(1))
               vsym(kfound) = vds(loc(1))
           endif
        enddo
   end select
!
   if (kfound > 0 .and. present(vat)) then
!
!      ricompatta i vettori vdist e vat
       call reallocate_dist(kfound)
!
!      Ordina le distanze dalla piu' piccola
       allocate(iord(kfound))
       call indexx(vdist,iord)
       vdist(:) = vdist(iord)
       vat(:) = vat(iord)
       vsym(:) = vsym(iord)
   endif

   contains
  
   subroutine reallocate_dist(nsize)
   USE arrayutil
   integer, intent(in) :: nsize
   call resize_array(vdist,nsize)
   call resize_array(vat,nsize)
   call resize_op(vsym,nsize)
   end subroutine reallocate_dist
!
   end subroutine get_distance_radius

!---------------------------------------------------------------------      

   integer function atom_hybridization(kat,legm,zval)   result(hyb)
!
!  Calcola il tipo di ibridazione di un atomo
!  0: ibridazione non identificabile
!  1: ibridazione sp
!  2: ibridazione sp2
!  3: ibridazionne sp3
!
   USE connect_mod
   integer, intent(in)                       :: kat
   type(bond_type), dimension(:), allocatable, intent(in) :: legm
   integer                                   :: zval
   integer                                   :: nbond
!
   hyb = 0
   nbond = number_of_bonds(legm,kat)
!
   select case(nbond)
     case (0)     
!
!       atomo isolato -> assegna ibridazione sp3 se B,C,N,O,S
        select case (zval)
           case (5,6,7,8,16)   ! B,C,N,O,S
             hyb = 3
           case default  
             hyb = 0
        end select

     case (1)

     case (2)
     case (3)
     case (4)
     case default
      hyb = 0
   end select
!
   end function atom_hybridization

!---------------------------------------------------------------------      

   subroutine atom_string(atom,elem,vet)
!
!  Crea stringa degli atomi dalla specie chimica
!
   USE strutil
   USE atom_basic
   USE elements
   type(atom_type), dimension(:), intent(inout) :: atom
   type(element_type), dimension(:), intent(in) :: elem
   integer, dimension(:), intent(in), optional  :: vet  ! labella solo gli atomi indicati in vet
   integer                                      :: nat
   integer, dimension(:), allocatable           :: nzz
   integer, dimension(:), allocatable           :: vnk
   integer                                      :: iz
   integer                                      :: i,j
   integer                                      :: nkk
   integer                                      :: numb
   integer                                      :: ierror
   integer                                      :: natl
   character(len=SIZELAB)                       :: spec
   integer                                      :: kat
   integer                                      :: kspec
!
   nat = size(atom)   ! numero totale di atomi
   if (present(vet)) then
!
!      Crea stringa solo per gli atomi in vet
       natl = size(vet)  ! numero di atomi da labellare
!
!      Calcola in vnk il numero di atomi per specie
       allocate(nzz(natl))
!corr       nzz(:) = iabs(atom(vet(:))%get_nz()/nzConst)
       nzz(:) = atom(vet(:))%kscatt()
       nkk = maxval(nzz)
       allocate(vnk(0:nkk))
       vnk(:) = 0
!
!      Per ognuna delle specie metti in vnk il massimo numero seriale
       do j=1,nat
          if (any(vet == j)) cycle  ! non considerare gli atomi da labellare
          call get_from_label(atom(j)%lab,spec,numb,ierror) ! estrai label e seriale 
          if (ierror == 0) then
              kspec = -1
              if  (s_eqi(spec,'Q')) then  ! e' un ghost
                   kspec = 0
              else
                   do i=1,nkk
                      if (s_eqi(spec,elem(i)%lab)) then
                          kspec = i; exit
                      endif
                   enddo
              endif
              if (kspec >= 0) then
                  if (numb > vnk(kspec)) vnk(kspec) = numb 
              endif
          endif
       enddo
!
!      Genera la stringa
       do i=1,natl
          kat = vet(i)
          iz = nzz(i)
          vnk(iz) = vnk(iz) + 1
          if (iz == 0) then
              write(atom(kat)%lab,'(a,i0)')'Q',vnk(iz)  ! ghost atom
          else
              write(atom(kat)%lab,'(a,i0)')trim(elem(iz)%lab),vnk(iz)
          endif
       enddo
   else
!
!      Crea stringa per tutti gli atomi
       allocate(nzz(nat))
!corr       nzz(:) = iabs(atom(:)%get_nz()/nzConst)
       nzz(:) = atom(:)%kscatt()
       nkk = maxval(nzz)
       allocate(vnk(0:nkk))
       vnk(:) = 0
       do i=1,nat
          iz = nzz(i)
          vnk(iz) = vnk(iz) + 1
          if (iz == 0) then
              write(atom(i)%lab,'(a,i0)')'Q',vnk(iz)  ! ghost atom
          else
              write(atom(i)%lab,'(a,i0)')trim(elem(iz)%lab),vnk(iz)
          endif
       enddo
   endif
!
   end subroutine atom_string

!---------------------------------------------------------------------      

   subroutine merge_model(atom1,atom2,spg,cell,elem,nmodif)
!
!  Combina atom1 con atom2. atom2 contiene in nz il valore di Z
!
   USE spginfom
   USE unit_cell
   USE elements
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom1
   type(atom_type), dimension(:), intent(in)                 :: atom2
   type(spaceg_type), intent(in)                             :: spg
   type(cell_type), intent(in)                               :: cell
   type(element_type), intent(in), dimension(:), allocatable :: elem
   integer, intent(out)                                      :: nmodif
   integer                                                   :: i,k
   integer                                                   :: nat1,nat2
   real                                                      :: epsx,epsb,epso
   integer                                                   :: natnew
!
   nat1 = numatoms(atom1)
   nat2 = size(atom2)
   epsx = 0.00001
   epsb = 0.00001
   epso = 0.001
   nmodif = 0   ! contatore delle modifiche
!
!  Controlla se atom2 contiene piu' atomi
   if (nat2 > nat1) then
!
!      Aggiungi gli atomi in piu' in fondo alla lista di atom1
       call add_atoms_to_list(atom1,atom2(nat1+1:),natnew)
       do i=nat1+1,natnew
          call atom1(i)%set_specie_from_z(atom2(i)%ptab,elem)
       enddo
       call set_intensity(atom1(nat1+1:))
       atom1(nat1+1:)%och = 1.0
       call calcola_occ(atom1(nat1+1:),spg,cell)
       nmodif = nmodif + nat2 - nat1
   endif
!
   do i=1,min(nat1,nat2)
!         
!     confronta coordinate 
      do k=1,3
         if (abs(atom1(i)%xc(k) - atom2(i)%xc(k)) >= epsx) then 
             atom1(i)%xc(k) = atom2(i)%xc(k)
             nmodif = nmodif + 1
         endif
      enddo
!
!     confronta b
      if (abs(atom1(i)%biso - atom2(i)%biso) >= epsb) then 
          atom1(i)%biso = atom2(i)%biso
          nmodif = nmodif + 1
      endif
!
!     confronta sof
      if (abs(atom1(i)%och - atom2(i)%och) >= epso) then 
          atom1(i)%och = atom2(i)%och
          nmodif = nmodif + 1
      endif
!
!     confronta specie chimica
      if (atom1(i)%ptab /= atom2(i)%ptab) then
          call atom1(i)%set_specie_from_z(atom2(i)%ptab,elem)
          nmodif = nmodif + 1
      endif
!
!     confronta label
      if (trim(atom1(i)%lab) /= trim(atom2(i)%lab)) then
          atom1(i)%lab = atom2(i)%lab
          nmodif = nmodif + 1
      endif
   enddo
!
   end subroutine merge_model

!---------------------------------------------------------------------      

   function plane_of_atoms(atom,cell,veta,cart) result(coef)
!
!  Piano passante per gli atomi di veta se veta e' presente
!
   USE unit_cell
   USE cgeom
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(cell_type), intent(in)                            :: cell
   integer, dimension(:), intent(in), optional            :: veta
   logical, intent(in)                                    :: cart
   real, dimension(4)                                     :: coef
   type(atom_type), dimension(:), allocatable             :: atomc
   integer                                                :: i
   integer                                                :: nvet
   real, dimension(:,:), allocatable                      :: xyza
!
!  atomi su cui calcolae il piano
   if (present(veta)) then
       nvet = size(veta)
   else
       nvet = numatoms(atom)
   endif
!
   if (nvet > 2) then
!
!      conversione in cartesiane
       call copy_atoms(atomc,atom)
       if (.not.cart) then
           call frac_to_cart(atomc,cell%get_ortom())
       endif
!
!      riempi xyza per calcolo del piano
       allocate(xyza(3,nvet))
       if (present(veta)) then
           do i=1,nvet
              xyza(:,i) = atomc(veta(i))%xc(:)
           enddo
       else
           do i=1,nvet
              xyza(:,i) = atomc(i)%xc(:)
           enddo
       endif
         !xyza(:3,:) = xyz(:3,:nvet)
!
       select case (nvet) 
          case (3)
           coef(:) = plane3points(xyza(:,1),xyza(:,2),xyza(:,3),1)
 
          case (4:)
            call lsqplane(xyza,coef)

       end select
   else
       coef(:) = 0
   endif
!
   end function plane_of_atoms

!-------------------------------------------------------------------------------------------------

   subroutine apply_symmetry_legm(atoms,legm,legms,spg)
!
!  Applica la simmetria ai legami
!
   USE connect_mod
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(in)     :: atoms
   type(bond_type), dimension(:), allocatable, intent(in)     :: legm   ! legm nella u.a.
   type(bond_type), dimension(:), allocatable, intent(inout)  :: legms  ! legm con simmetria
   type(spaceg_type), intent(in)                              :: spg
   integer                                                    :: numleg
   integer                                                    :: i,j,k  !!!!!,op1,op2
   integer                                                    :: n1,n2
   integer                                                    :: nat
   integer                                                    :: numaumax
   integer                                                    :: nleg_new
!
   numleg = numbonds(legm)
   nat = numatoms(atoms)
   if (nat > 0 .and. numleg > 0) then
       numaumax = spg%nsymop*numleg*27
       call new_bonds(legms,numaumax)
       nleg_new = 0
       loop_bonds: do i=1,numleg      ! loop sui legami nell'u.a.
          n1 = legm(i)%n1
          n2 = legm(i)%n2
          do j=1,nat-1    ! loop su tutti gli atomi con simmetria
             if (atoms(j)%asym == n1 .or. atoms(j)%asym == n2) then
                do k=j+1,nat
                   if (atoms(k)%asym == n1 .or. atoms(k)%asym == n2) then
                       if (atoms(j)%op /= atoms(k)%op) cycle
                       nleg_new = nleg_new + 1
                       legms(nleg_new)%n1 = j
                       legms(nleg_new)%n2 = k
                       legms(nleg_new)%dist = legm(i)%dist
                       legms(nleg_new)%sigma = 0.3
                       legms(nleg_new)%ord = legm(i)%ord
                       if (numaumax == nleg_new) exit loop_bonds
                   endif
                enddo
             endif
          enddo
       enddo loop_bonds
       call resize_bonds(legms,nleg_new)
   endif
!
   end subroutine  apply_symmetry_legm

!--------------------------------------------------------------------------------------------------

   subroutine expand_contacts(atomasym,cell,spg,atom,legm,usecov)
!
!  Espandi i contatti di atom
!
   USE connect_mod
   USE cgeom
   USE unit_cell
   USE spginfom
   type(atom_type), dimension(:), intent(in)                 :: atomasym  ! unita' asimmetrica
   type(cell_type), intent(in)                               :: cell
   type(spaceg_type), intent(in)                             :: spg
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom      ! atomi da espandere
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   logical, intent(in)                                       :: usecov
   type(atom_type), dimension(:), allocatable                :: atomadd
   type(bond_type), dimension(:), allocatable                :: legmadd
   type(atom_type), dimension(:), allocatable                :: atomcart,atomcart1
   type(atom_type), dimension(1)                             :: atomc
   type(atom_type)                                           :: atomtra,atomsav
   type(op_type)                                             :: opcode
   integer, dimension(size(atomasym))                        :: zasym
   integer, dimension(size(atom))                            :: zval
   type(bond_type)                                           :: legnew
   integer                                                   :: nat,natasym
   integer                                                   :: nlegmax
   integer                                                   :: nleg
   integer                                                   :: k1,k2,k3
   integer, dimension(3)                                     :: ktra
   integer                                                   :: ka
   integer                                                   :: i,k
   real                                                      :: tolmin=0.0,tolmax=0.0
   logical                                                   :: distp
   integer                                                   :: kk
   real, dimension(3)                                        :: bar
   real                                                      :: rad
   real, dimension(3)                                        :: dx
   real, parameter                                           :: D2MIN = 0.6*0.6  ! square of minimum distance
   integer                                                   :: naddat,nadd
   logical                                                   :: addat
   integer, dimension(3)                                     :: diff
!
   natasym = size(atomasym)
   nat = size(atom)
!
!  Alloca ad un numero sufficientemente grande
   nlegmax = 8*nat
   call new_bonds(legmadd,nlegmax)
   call new_atoms(atomadd,nlegmax)
!
!  converti in cartesiane
   call frac_to_cart_copy(atom,atomcart,cell%get_ortom())
   call frac_to_cart_copy(atomasym,atomcart1,cell%get_ortom())
!
   call get_radius_molecule(atomcart,rad,bar)  ! calcola raggio e baricentro
!
!  estrai numeri atomi
   zasym(:) = atomasym%z()
   zval(:) = atom%z()
   nleg = 0
   naddat = 0
!
!  Applica operatori all' u.a. (atomasym) e cerca legami con atom
   loop_asym: do i=1,natasym   
      if (zasym(i) == 0) cycle
      do k=1,spg%nsymop
         atomc = atomasym(i)
         call apply_sym_oper(atomc,spg%symop(k))
         do ka=1,nat
            if (zval(ka) == 0) cycle
            diff(:) = nint(atom(ka)%xc-atomc(1)%xc)
            do k1=-1,1
               do k2=-1,1
                  do k3=-1,1
                     ktra = (/k1,k2,k3/) + diff(:)
                     opcode = op_type(k,ktra)
                     if (checkeq_symm(atom(:nat),i,opcode) > 0) cycle
                     atomsav = atomc(1)
                     atomsav%xc = atomsav%xc + ktra  ! conserva in atomsav le coord. cryst.
                     atomtra = cartesian_coord(atomsav,cell%get_ortom())
!
!                    check per atomi in posizione speciale
                     distp = .true.
                     addat = .true.
                     do kk=1,nat
                        if (atom(kk)%asym == i) then
                            dx(:) = atomtra%xc-atomcart(kk)%xc
                            if (dot_product(dx,dx) < D2MIN) then
                                distp = .false.
                                exit
                            endif
                        endif
                     enddo
                     if (distp) then
                         do kk=1,naddat
                            if (atomadd(kk)%asym == i) then
                                dx(:) = atomsav%xc-atomadd(kk)%xc
                                if (dot_product(dx,matmul(cell%get_g(),dx)) < D2MIN) then
                                    !distp = .false.
                                    addat = .false.
                                    nadd = kk+nat
                                    if (bond_position(legmadd(:nleg),ka,nadd) > 0) distp = .false.
                                    exit
                                endif
                            endif
                         enddo
                     endif
!
                     if (distp) then
!
!                        Controlla se l'equivalente è legato ad un atomo
                         if (addat) nadd = nat+naddat+1
                         !addat = .false.
                            !call create_bond(nat+nleg+1,ka,atomtra%xc,atomcart(ka)%xc,zasym(i),zval(ka),tolmin,tolmax,legnew,usecov)
!
!                           Pay attention: in a cif is written only the symm. code of second atom (nat+nleg+1)
                            call create_bond(ka,nadd,atomcart(ka)%xc,atomtra%xc,zval(ka),zasym(i),tolmin,tolmax,legnew,usecov)
                            if (legnew%n1 > 0) then  ! se 0 non c'è legame tra i due atomi
                                !if (.not.addat)write(70,*)'CHECK: ',bond_position(legmadd(:nleg),ka,nadd)
                                nleg = nleg + 1
                                legmadd(nleg) = legnew
                                !write(70,*)'ADD BOND',nlegmax,nleg
                                !write(70,*)trim(atom(ka)%lab)//' '//trim(atomsav%lab)
                                !write(70,*)ka,nadd,'XC=',atomsav%xc
                                !write(70,*)'OP=',opcode
                                if (addat) then
                                    naddat = naddat + 1
                                    atomadd(naddat) = atomsav
                                    atomadd(naddat)%asym = i
                                    atomadd(naddat)%op = opcode
                                endif
                            endif
                            if (nleg == nlegmax) exit loop_asym  ! troppi legami! esci.
                         endif
                  enddo
               enddo
            enddo
         enddo
      enddo
   enddo loop_asym
!
   if (nleg > 0) then
       call add_atoms_to_list(atom,atomadd(:naddat),nat)
       call resize_bonds(legmadd,nleg)
       call combine_legm(legm,legmadd)
   endif
!
   end subroutine expand_contacts

!--------------------------------------------------------------------------------------------------

!corr   subroutine expand_contacts(atomasym,cell,spg,atom,legm,usecov)
!corr!
!corr!  Espandi i contatti di atom
!corr!
!corr   USE connect_mod
!corr   USE cgeom
!corr   USE unit_cell
!corr   USE spginfom
!corr   type(atom_type), dimension(:), intent(in)                     :: atomasym  ! unita' asimmetrica
!corr   type(cell_type), intent(in)                                   :: cell
!corr   type(spaceg_type), intent(in)                                 :: spg
!corr   type(atom_type), dimension(:), allocatable, intent(inout)     :: atom      ! atomi da espandere
!corr   type(bond_type), dimension(:), allocatable, intent(inout)     :: legm
!corr   logical, intent(in)                                           :: usecov
!corr   type(atom_type), dimension(:), allocatable                    :: atomadd
!corr   type(bond_type), dimension(:), allocatable                    :: legmadd
!corr   type(atom_type), dimension(:), allocatable                    :: atomcart,atomcart1
!corr   type(atom_type), dimension(1)                                 :: atomc
!corr   type(atom_type)                                               :: atomtra,atomsav
!corr   type(op_type)                                                 :: opcode
!corr   integer, dimension(size(atomasym))                            :: zasym
!corr   integer, dimension(size(atom))                                :: zval
!corr   type(bond_type)                                               :: legnew
!corr   integer                                                       :: nat,natasym
!corr   integer                                                       :: nlegmax
!corr   integer                                                       :: nleg
!corr   integer                                                       :: k1,k2,k3
!corr   integer, dimension(3)                                         :: ktra
!corr   integer                                                       :: ka
!corr   integer                                                       :: i,k
!corr   real                                                          :: tolmin=0.0,tolmax=0.0
!corr   logical                                                       :: distp
!corr   integer                                                       :: kk
!corr   integer, dimension(3)                                         :: ktramax, ktramin
!corr   real, dimension(3)                                            :: xtra
!corr   real, dimension(3)                                            :: bar
!corr   real                                                          :: rad
!corr   real, dimension(3) :: dx
!corr   real, parameter :: D2MIN = 0.6*0.6  ! square of minimum distance
!corr   integer :: naddat,nadd
!corr   logical :: add_atom
!corr!
!corr   natasym = size(atomasym)
!corr   nat = size(atom)
!corr!
!corr!  Prendi la traslazione minima e massima
!corr   call get_limit_translation(atom,ktramin,ktramax)
!corr!
!corr!  Allarga di 2 i limiti di traslazione
!corr   ktramin(:) = ktramin(:) - 1
!corr   ktramax(:) = ktramax(:) + 1
!corr!
!corr!  Alloca ad un numero sufficientemente grande
!corr   nlegmax = 4*nat
!corr   call new_bonds(legmadd,nlegmax)
!corr   call new_atoms(atomadd,nlegmax)
!corr!
!corr!  converti in cartesiane
!corr   call frac_to_cart_copy(atom,atomcart,cell%get_ortom())
!corr   call frac_to_cart_copy(atomasym,atomcart1,cell%get_ortom())
!corr!
!corr   call get_radius_molecule(atomcart,rad,bar)  ! calcola raggio e baricentro
!corr!
!corr!  estrai numeri atomi
!corr   zasym(:) = atomasym%z()
!corr   zval(:) = atom%z()
!corr   nleg = 0
!corr   naddat = 0
!corr!
!corr!  Applica operatori all' u.a. (atomasym) e cerca legami con atom
!corr   loop_asym: do i=1,natasym   
!corr      if (zasym(i) == 0) cycle
!corr      do k=1,spg%nsymop
!corr         atomc = atomasym(i)
!corr         call apply_sym_oper(atomc,spg%symop(k))
!corr         call translate_in_cell(atomc,xtra)
!corr         do k1=ktramin(1),ktramax(1)
!corr            do k2=ktramin(2),ktramax(2)
!corr               do k3=ktramin(3),ktramax(3)
!corr                  ktra = (/k1,k2,k3/)
!corr                  opcode = op_type(k,nint(ktra+xtra))
!corr                  if (checkeq_symm(atom(:nat),i,opcode) > 0) cycle
!corr                  atomsav = atomc(1)
!corr                  atomsav%xc = atomsav%xc + ktra  ! conserva in atomsav le coord. cryst.
!corr                  atomtra = cartesian_coord(atomsav,cell%get_ortom())
!corr!
!corr!                 check per atomi in posizione speciale
!corr                  distp = .true.
!corr                  do kk=1,nat
!corr                     if (atom(kk)%asym == i) then
!corr                         dx(:) = atomtra%xc-atomcart(kk)%xc
!corr                         if (dot_product(dx,dx) < D2MIN) then
!corr                             distp = .false.
!corr                             exit
!corr                         endif
!corr                     endif
!corr                  enddo
!corr                  if (distp) then
!corr                      do kk=1,naddat
!corr                         if (atomadd(kk)%asym == i) then
!corr                         !test if (atomadd(kk)%ptab == atom(i)%ptab) then  ! useful if you have equivalent atoms in au (doc)
!corr                             dx(:) = atomsav%xc-atomadd(kk)%xc
!corr                             !if (dist < 0.01) then
!corr                             if (dot_product(dx,matmul(cell%get_g(),dx)) < D2MIN) then
!corr                                 distp = .false.
!corr                                 exit
!corr                             endif
!corr                         endif
!corr                      enddo
!corr                  endif
!corr!
!corr                  if (distp) then
!corr!
!corr!                     Controlla se l'equivalente è legato ad un atomo
!corr                      if (distanzaC(bar,atomtra%xc) > rad + 4) cycle  
!corr                      nadd = nat+naddat+1
!corr                      add_atom = .false.
!corr                      do ka=1,nat
!corr                         if (zval(ka) == 0) cycle
!corr                         !call create_bond(nat+nleg+1,ka,atomtra%xc,atomcart(ka)%xc,zasym(i),zval(ka),tolmin,tolmax,legnew,usecov)
!corr!
!corr!                        Pay attention: in a cif is written only the symm. code of second atom (nat+nleg+1)
!corr                         call create_bond(ka,nadd,atomcart(ka)%xc,atomtra%xc,zval(ka),zasym(i),tolmin,tolmax,legnew,usecov)
!corr                         if (legnew%n1 > 0) then  ! se 0 non c'è legame tra i due atomi
!corr                             nleg = nleg + 1
!corr                             legmadd(nleg) = legnew
!corr                             add_atom = .true.
!corr                               !write(72,*)'ADD BOND',nlegmax,nleg
!corr                               !write(72,*)trim(atomasym(ka)%lab)//' '//trim(atomsav%lab)
!corr                               !write(72,*)ka,nadd,'XC=',atomsav%xc
!corr                               !write(72,*)'OP=',opcode
!corr                             if (nleg == nlegmax) exit  ! troppi legami! esci.
!corr                         endif
!corr                      enddo
!corr                      if (add_atom) then
!corr                          naddat = naddat + 1
!corr                          atomadd(naddat) = atomsav
!corr                          atomadd(naddat)%asym = i
!corr                          atomadd(naddat)%op = opcode
!corr                      endif
!corr                      if (nleg == nlegmax) exit loop_asym  ! troppi legami! esci.
!corr                  endif
!corr               enddo
!corr            enddo
!corr         enddo
!corr      enddo
!corr   enddo loop_asym
!corr!
!corr   if (nleg > 0) then
!corr       call add_atoms_to_list(atom,atomadd(:naddat),nat)
!corr       call resize_bonds(legmadd,nleg)
!corr       call combine_legm(legm,legmadd)
!corr   endif
!corr!
!corr   end subroutine expand_contacts

!------------------------------------------------------------------------------------------      

   subroutine duplicate_atoms(atom,legm,elem,vet,rand)
!
!  Duplicate selected atoms with random traslation
!
   USE connect_mod
   USE rand_mod
   USE elements
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom  
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   type(element_type), dimension(:), intent(in)              :: elem
   integer, dimension(:), optional                           :: vet  ! atoms to duplicate
   logical, intent(in), optional                             :: rand
   integer                                                   :: nat, natnew
   type(atom_type), dimension(:), allocatable                :: atomadd
   type(bond_type), dimension(:), allocatable                :: legmadd
   integer                                                   :: nlegadd
   integer                                                   :: i,nleg,natadd,nvet
   logical                                                   :: randtra
!
   nat = numatoms(atom)
   if (nat > 0) then
       if (present(vet)) then
!
!          extract selected atoms
           natadd = 0
           nvet = size(vet)
           allocate(atomadd(nvet))
           do i=1,nvet
              if (vet(i) > 0 .and. vet(i) <= nat) then
                  natadd = natadd + 1
                  atomadd(natadd) = atom(vet(i))
              endif
           enddo
           if (present(rand)) then
               randtra = rand
           else
               randtra = .true.
           endif
           if (randtra) then
               call translate_atoms(atomadd,randvalue(3,0.0,0.5))
               call translate_in_cell(atomadd)
           endif
!
!          extract selected bonds
           nleg = numbonds(legm)
           if (nleg > 0) then
               call extract_bonds(legm,vet,legmadd,nlegadd)
           endif
!
           call add_atoms_to_list(atom,atomadd,natnew,legm1=legm,legm2=legmadd)
           call atom_string(atom,elem,vet=(/(i,i=nat+1,natnew)/))
       else
!
!          duplicate atoms
           natnew =nat*2
           call resize_atoms(atom,natnew)
           atom(nat+1:) = atom(1:nat)
           call translate_atoms(atom(nat+1:),randvalue(3,0.0,0.5))
           call translate_in_cell(atom(nat+1:))
!
!          duplicate bonds
           nleg = numbonds(legm)
           if (nleg > 0) then
               call resize_bonds(legm,nleg*2)
               do i=1,nleg
                  legm(i+nleg) = legm(i) + nat
               enddo
           endif
           call atom_string(atom,elem,vet=(/(i,i=nat+1,natnew)/))
       endif
   endif
!
   end subroutine duplicate_atoms

!---------------------------------------------------------------------      

   subroutine find_sssr(atom,legm,ring,nring)
!
!  Find the smallest set of smallest rings (SSSR) from a connection table. 
!  (J. Chem. Inf. Comput. Sci. 1993, 33, 657-662)
!
   USE connect_mod
   USE arrayutil
   type(atom_type), dimension(:), allocatable, intent(in)        :: atom
   type(bond_type), dimension(:), allocatable, intent(in)        :: legm  
   type(container_type), dimension(:), allocatable, intent(out) :: ring
   integer, intent(out)                                          :: nring
   type(bond_type), dimension(:), allocatable                    :: legmc, legmb, legmh
   type(container_type), dimension(:), allocatable              :: connect
   integer                                                       :: nat, nleg
   logical                                                       :: found
   integer                                                       :: i,j,k, nl
   integer, dimension(:), allocatable                            :: vet2,vrem,vleg,vrem_min
   integer                                                       :: n1,n2, jpos1, jpos2
   integer                                                       :: nminstep
   integer, dimension(:), allocatable                            :: minpath 
   integer                                                       :: nrem, nrem_min, nlegh
   integer                                                       :: kat, rootat, kk, ka2
   integer                                                       :: nblock, nblock1, natblock, nlegb
   integer, dimension(:), allocatable                            :: block
   integer, dimension(1)                                         :: loc
   integer                                                       :: hide_atom 
   integer                                                       :: nring0
   integer, parameter                                            :: MAXAT_BLOCK = 100
   logical                                                       :: n2found
!#define PRINT_ENABLE 
#if defined(PRINT_ENABLE) 
   logical                                                       :: kpr = .false.
#endif
!   
   nring = 0
   nat = numatoms(atom)
   nleg = numbonds(legm)
   if (nat == 0 .or. nleg == 0) return
   call copy_bonds(legmc,legm)
   call bond_to_connect(nat,legm,connect)
   allocate(vrem(nat),vrem_min(nat))
!
!  Step 1: remove all acyclic atoms
   nrem = 0
   do i=1,nat
      call cycle_search(i,i,connect,found)
      if (.not.found) then
          nrem = nrem + 1
          vrem(nrem) = i
      endif
   enddo
   if (nrem == nat) return
   if (nrem > 0) then
#if defined(PRINT_ENABLE) 
       if (kpr) write(0,'(a,*(i4))')'Acycic atoms: ',vrem(:nrem)
#endif
       call remove_bond_from_atom(legmc,vrem(:nrem))
       call bond_to_connect(nat,legmc,connect)
   endif
!
!  Step 2: remove all open acyclic bonds from connection table
   nrem = 0
   allocate(vleg(nleg), source=0)
   do i=1,numbonds(legmc)
      n1 = legmc(i)%n1
      n2 = legmc(i)%n2
!
!     hide bond n1-n2
      do j=1,connect(n1)%nat    
         if (connect(n1)%pos(j) == n2) then
             connect(n1)%pos(j) = -connect(n1)%pos(j)
             jpos1 = j
             exit
         endif
      enddo
      do j=1,connect(n2)%nat    
         if (connect(n2)%pos(j) == n1) then
             connect(n2)%pos(j) = -connect(n2)%pos(j)
             jpos2 = j
             exit
         endif
      enddo
!     n1 and n2 are connected in cycle?
      call cycle_search(n1,n2,connect,found) 
      if (.not.found) then
#if defined(PRINT_ENABLE) 
          if (kpr) write(0,*)'Acyclic bonds:',n1,n2
#endif
          connect(n1)%pos(jpos1) = 0
          connect(n2)%pos(jpos2) = 0
          nrem = nrem + 1
          vleg(i) = 1
      endif
   enddo
   if (nrem > 0) then
       call remove_bondsv(legmc,vleg,1)
       call bond_to_connect(nat,legmc,connect)
   endif
!
!  Step 3: separate blocks saving info in vet2
   allocate(vet2(nat), source=0)
   nblock = 0
   nblock1 = -1
   do i=1,nat
      if (vet2(i) > 0) cycle                 
      if (connect(i)%nat > 0) then
          do j=1,nat
             if (vet2(j) > 0) cycle                 
             if (connect(j)%nat > 0) then
                 call cycle_search(i,j,connect,found)
                 if (found) then                 
                     if (i /= nblock1) nblock = nblock + 1
                     vet2(j) = nblock
                     nblock1 = i
                 endif
             endif
          enddo
      endif
   enddo
!
!  Number of rings (fail for 1501992.cif)
   nring = nblock + count(connect(:)%nat == 3)/2 + count(connect(:)%nat == 4) 
   call new_container(ring,nring)
!
!  Step 4: find rings
   allocate(block(nat))
   allocate(minpath(nat)) 
   nring0 = 0
   block_loop: do i=1,nblock
!
!     Get a block
      natblock = 0
      hide_atom = 0
      do j=1,nat
         if (vet2(j) == 0) cycle
         if (vet2(j) == i) then
             natblock = natblock + 1
             block(natblock) = j
         endif
      enddo
!
!     Get connectivity only for block i
      nrem = 0
      do j=1,nat
         if (vet2(j) /= i) then
             nrem = nrem + 1
             vrem(nrem) = j
         endif
      enddo
      call copy_bonds(legmb,legmc)
      call remove_bond_from_atom(legmb,vrem(:nrem))
      call bond_to_connect(nat,legmb,connect)
#if defined(PRINT_ENABLE) 
      if (kpr) write(0,'(a,i0,a,i0,a,i0,a,*(i4))')    &
              'block n. ',i,' (',natblock,' atoms, ',size(legmb),' bonds): ',block(:natblock)
#endif
!
!     Check for anomalies in the block: wrong connectivity, random atoms, ...
      if (natblock > MAXAT_BLOCK .and. size(legmb) > 6*natblock) cycle
!
      rootat_loop: do   ! loop on root atoms in the block i
!
!        Choice of the root atom with the smallest ring connectivity
         loc = minloc(connect(:)%nat,mask=connect(:)%nat > 1)
         rootat = loc(1)
         if (rootat == 0) exit rootat_loop  ! (*) see personal notes
         if (connect(rootat)%nat > 4) exit
!
!        Search the smallest ring containing the root atom
         call minpath_find(rootat,rootat,connect,minpath,nminstep)
#if defined(PRINT_ENABLE) 
         if (kpr) write(0,'(i4,a,*(i4))')nminstep, 'ring: ',minpath(:nminstep)
#endif
!corr-bug 17/1/2014         if (nring0 <= nring) then
         if (nring0 < nring) then
             if (nminstep >= 3) then   ! (*) possible if rootat is an N1/N2 atom not previusly removed
                 nring0 = nring0 + 1
                 ring(nring0)%nat = nminstep
                 allocate(ring(nring0)%pos(nminstep), source=minpath(:nminstep))
             endif
         else
             exit block_loop ! errore: nring0 > nring
         endif
!
         if (hide_atom > 0) then
!
!            Restore hidden atom
#if defined(PRINT_ENABLE) 
             if (kpr) write(0,*)'restore hidden atom: ',hide_atom
#endif
             do nl=1,nlegh
                if (legmh(nl)%n1 == hide_atom) then
                    ka2 = legmh(nl)%n2
                else
                    ka2 = legmh(nl)%n1
                endif
                if (ka2 == rootat) cycle
                if (connect(ka2)%nat > 0) then
                    nlegb = size(legmb)
                    call resize_bonds(legmb,nlegb+1)
                    legmb(nlegb+1) = legmh(nl)
                    !write(0,*)'ripristino :',legmh(nl)%n1,legmh(nl)%n2
                endif
             enddo
             call bond_to_connect(nat,legmb,connect)
             hide_atom = 0
             natblock = natblock + 1
         endif
!
!        Hiding the root atom with N3 or N4 connectivity
         if (connect(rootat)%nat == 3 .or. connect(rootat)%nat == 4) then
             call get_bonds_of_atom(legmb,rootat,vleg,nlegh)
             if (nlegh > 0) then
                 call resize_bonds(legmh,nlegh)
                 do nl=1,nlegh
                    legmh(nl) = legmb(vleg(nl))
                 enddo
             endif
             hide_atom = rootat
#if defined(PRINT_ENABLE) 
             if (kpr) write(0,*)'hide atom:',rootat
#endif
         endif
!
!        Elimination of reducible atoms
         nrem_min = 0
         if (hide_atom > 0) then
             nrem_min = nrem_min + 1
             vrem_min(nrem_min) = hide_atom
         else
!
!            Remove the large sequence of atoms with connectivity N2
!            This loop should be on all atom not removed recursively. The if (*) evoid this 
             do j=1,nminstep
                kat = minpath(j)
                if (connect(kat)%nat == 2) then
                    nrem = 1
                    vrem(nrem) = kat
                    do k=1,nminstep-1
                       kk = mod(j+k,nminstep)
                       if (kk == 0) kk = nminstep
                       kk = minpath(kk)
                       if (connect(kk)%nat == 2) then
                           nrem = nrem + 1
                           vrem(nrem) = kk
                       else
                           exit
                       endif
                    enddo
                    if (nrem > nrem_min) then
                        nrem_min = nrem
                        vrem_min(:nrem) = vrem(:nrem)
                    endif
                endif
             enddo
         endif
!
         if (nrem_min > 0 .and. nrem_min /= natblock) then
             !connect(vrem_min(:nrem_min))%nat = 0  bug for cycles (6,5,6)  17/1/2014
             call container_update_remove(connect,vrem_min(:nrem_min)) ! important to recognaze atoms N1
!
!            Test remaining N2 atoms
             n2found = .false.
             do j=1,nminstep
                kat = minpath(j)
                if (any(vrem_min(:nrem_min) == kat)) cycle
                if (connect(kat)%nat <= 2) then ! also N1 are possible, vedi appunti
                    n2found = .true.
!
!                   check if this atom belong to another cycle
                    call cycle_search(kat,kat,connect,found)
                    if (.not.found) then
#if defined(PRINT_ENABLE) 
                        if (kpr) write(0,*)'remove N2 atom:',kat
#endif
                        connect(kat)%nat = 0
                        nrem_min = nrem_min + 1
                        vrem_min(nrem_min) = kat
                    endif
                endif
             enddo
!
             if (hide_atom > 0 .and. .not.n2found) then
!
!                rare event: after hiding of rootat the path doesn't contain N2 atom
#if defined(PRINT_ENABLE) 
                 if (kpr) write(0,*)'Cannot find N2 atom after hiding ',hide_atom
#endif
                 exit
             endif
!
#if defined(PRINT_ENABLE) 
             if (kpr) write(0,'(a,*(i4))')'eliminate:',vrem_min(:nrem_min)
#endif
             natblock = natblock - nrem_min
             if (natblock == 0) exit rootat_loop
             call remove_bond_from_atom(legmb,vrem_min(:nrem_min))
             call bond_to_connect(nat,legmb,connect)
         else
             exit rootat_loop
         endif
      enddo rootat_loop
   enddo block_loop
!
!  update nring that could be different from nring 
   nring = nring0
!
#if defined(PRINT_ENABLE) 
   do i=1,nring
      write(0,'(a,i0,a,i0,a,*(1x,i0))')'ring n.',i,', ',ring(i)%nat,' atoms:',ring(i)%pos(:)
   enddo
#endif
!
   end subroutine find_sssr

!---------------------------------------------------------------------      

   subroutine bond_type_perception(atom,legm,cell,rings,nrings)
   USE connect_mod
   USE unit_cell
   USE cgeom
   USE elements
   USE arrayutil
   type(atom_type), dimension(:), allocatable, intent(in)                 :: atom
   type(bond_type), dimension(:), allocatable, intent(inout)              :: legm  
   type(cell_type), intent(in)                                            :: cell
   type(container_type), dimension(:), allocatable, intent(out), optional :: rings
   integer, intent(out), optional                                         :: nrings
   type(container_type), dimension(:), allocatable                        :: ring, conn
   type(atom_type), dimension(:), allocatable                             :: atomc
   integer                                                                :: nat, nleg, nring
   integer                                                                :: i,j,k
   logical                                                                :: plan
   integer, dimension(:), allocatable                                     :: varom
   integer, dimension(:), allocatable                                     :: zval
   integer                                                                :: z1,z2
   integer, allocatable, dimension(:,:)                                   :: tabconn
   integer                                                                :: n1,n2,a1,a2,a3,a4,c1,c2,jc
   integer                                                                :: posb, ord_dist, ord_geom
   integer                                                                :: apos
   integer                                                                :: nchin
   integer, dimension(6)                                                  :: vchin
   logical                                                                :: match
   logical                                                                :: kpr = .false.
   integer, dimension(10), parameter :: kind_at = [H_at,F_at,Cl_at,Br_at,I_at,C_at,N_at,O_at,P_at,S_at]
   type(chem_group_t) :: CO_group = chem_group_t(3,C_at,(/C_at,O_at,C_at,0/),(/1,2,1,0/),(/4,1,4,0/))      ! RR'C=0  (chetone)
!bug-28/05/2019   type(chem_group_t) :: COO_group = chem_group_t(3,C_at,(/O_at,O_at,C_at,0/),(/20,20,1,0/),(/1,2,4,0/))   ! -COOR
   type(chem_group_t) :: COO_group = chem_group_t(3,C_at,(/O_at,O_at,C_at,0/),(/2,1,1,0/),(/1,2,4,0/))   ! -COOR
   type(chem_group_t) :: amide_group = chem_group_t(3,C_at,(/O_at,N_at,C_at,0/),(/2,1,1,0/),(/1,3,4,0/))   ! -CONRR'
   type(chem_group_t) :: NO2_group = chem_group_t(3,N_at,(/C_at,O_at,O_at,0/),(/1,2,2,0/),(/4,1,1,0/))     ! -NO2
   type(chem_group_t) :: CN2_group = chem_group_t(3,C_at,(/N_at,N_at,C_at,0/),(/20,20,1,0/),(/2,3,4,0/))   ! -CN2 (es. 3ptb)
   !type(chem_group_t) :: CN3_group = chem_group_t(3,C_at,(/N_at,N_at,N_at,0/),(/20,20,20,0/),(/2,3,3,0/))  ! -CN3 (es. cime)
   type(chem_group_t) :: CN3_group = chem_group_t(3,C_at,(/N_at,N_at,N_at,0/),(/2,1,1,0/),(/2,3,3,0/))  ! -CN3 (es. cime)
   type(chem_group_t) :: PO4_group = chem_group_t(4,P_at,(/O_at,O_at,O_at,O_at/),(/20,20,20,1/),(/1,1,1,2/)) ! -PO4
   type(chem_group_t) :: PO3_group = chem_group_t(4,P_at,(/O_at,O_at,O_at,C_at/),(/20,20,20,1/),(/1,1,1,4/)) ! -RPO3 (fosfonate, es.8atc)
   type(chem_group_t) :: sulfoN_group = chem_group_t(4,S_at,(/O_at,O_at,N_at,C_at/),(/2,2,1,1/),(/1,1,3,4/)) ! -SO2NRR' (es. hydrochloro)
   type(chem_group_t) :: urea_group = chem_group_t(3,C_at,(/O_at,N_at,N_at,0/),(/2,1,1,0/),(/1,3,3,0/)) ! -CONN 
   type(chem_group_t) :: C3N_group = chem_group_t(3,C_at,(/N_at,C_at,C_at,0/),(/2,1,1,0/),(/2,3,3,0/)) ! >C=N- (es. py151)
   type(chem_group_t) :: CNO2_group = chem_group_t(3,C_at,(/O_at,N_at,O_at,0/),(/2,1,1,0/),(/1,3,2,0/)) ! -OC(=O)N (es. peptide4)
!
   nat = numatoms(atom)
   nleg = numbonds(legm)
   if (present(nrings)) nrings = 0
   if (nat == 0 .or. nleg == 0) return
   allocate(zval(nat))
   zval(:) = atom%z()
   legm(:)%ord = 0
!
!  The algorithm judges the bond only if some species are present
   if (.not.is_atomic_specie(atom,[C_at,N_at,P_at,S_at])) then
       legm(:)%ord = 1
       return
   endif
!
   call bond_to_connect(nat,legm,conn)
   allocate(tabconn(nat,nat))
   call bond_to_tabconn(legm,tabconn)
!
   call frac_to_cart_copy(atom,atomc,cell%get_ortom()) ! converti in cartesiane
!
!  All bond order with atoms different from H,C,N,O,F,P,S,Cl,Br,I are set to 1
   do i=1,nleg
      !z1 = zval(legm(i)%n1)
      !z2 = zval(legm(i)%n2)
      !if (any([H_at,C_at,N_at,O_at,F_at,P_at,S_at,Cl_at,Br_at,I_at] == z1)) cycle
      if (.not.is_bond_equal(legm(i),kind_at,zval)) then
          legm(i)%ord = 1
      endif
   enddo
!
!  Assign bond types for H and halogens (F,Cl,Br,I)
   do i=1,nleg
      z1 = zval(legm(i)%n1)
      z2 = zval(legm(i)%n2)
      if (any((/H_at,F_at,Cl_at,Br_at,I_at/) == z1) .or. any((/H_at,F_at,Cl_at,Br_at,I_at/) == z2)) then
          legm(i)%ord = 1
      endif
   enddo
!
!  Assign bond types for O (Z=8) and S (Z=16) where conn=2
   do i=1,nleg
      z1 = zval(legm(i)%n1)
      z2 = zval(legm(i)%n2)
      if ((z1 == O_at .or. z1 == S_at) .and. conn(legm(i)%n1)%nat == 2) then
          legm(i)%ord = 1
          cycle
      endif
      if ((z2 == O_at .or. z2 == S_at) .and. conn(legm(i)%n2)%nat == 2) then
          legm(i)%ord = 1
      endif
   enddo
!
!  Search for linear functional groups
   do i=1,nleg
      if (legm(i)%ord /= 0) cycle
      n1 = legm(i)%n1
      n2 = legm(i)%n2
      c1 = conn(n1)%nat
      c2 = conn(n2)%nat
      if (c1 == 2 .or. c2 == 2) then
          if (c1 + c2 == 4) then 
!
!             find sequence a1-a2-a3-a4
              a1 = conn(n1)%pos(1)
              if (a1 == n2) a1 = conn(n1)%pos(2)
              a2 = n1
              a3 = n2
              a4 = conn(n2)%pos(1)
              if (a4 == n1) a4 = conn(n2)%pos(2)
              if(is_linear(atomc(a1)%xc,atomc(a2)%xc,atomc(a3)%xc,atomc(a4)%xc))then
                 if (zval(a2) == C_at .and. zval(a3) == C_at) then          ! R-C#C-R
                     legm(tabconn(a1,a2))%ord = 1
                     legm(tabconn(a2,a3))%ord = 3
                     legm(tabconn(a3,a4))%ord = 1
                 elseif (zval(a2) == N_at .and. zval(a3) == N_at) then      ! R-N=N=N
                     if (zval(a4) == N_at .and. conn(a4)%nat == 1) then
                         legm(tabconn(a1,a2))%ord = 1
                         legm(tabconn(a2,a3))%ord = 2
                         legm(tabconn(a3,a4))%ord = 2
                     elseif(zval(a1) == N_at .and. conn(a1)%nat == 1) then
                         legm(tabconn(a1,a2))%ord = 2
                         legm(tabconn(a2,a3))%ord = 2
                         legm(tabconn(a3,a4))%ord = 1
                     endif
                 elseif ((zval(a2) == N_at .and. zval(a3) == C_at) .or.   & ! R-N=C=S
                         (zval(a2) == C_at .and. zval(a3) == N_at)) then
                     if (zval(a1) == S_at) then
                         legm(tabconn(a1,a2))%ord = 2
                         legm(tabconn(a2,a3))%ord = 2
                         legm(tabconn(a3,a4))%ord = 1
                     elseif (zval(a4) == S_at) then
                         legm(tabconn(a1,a2))%ord = 1
                         legm(tabconn(a2,a3))%ord = 2
                         legm(tabconn(a3,a4))%ord = 2
                     endif
                 endif
              endif
              !if (is_linear(a
              !write(0,*)'angle=',a1,n1,a2,angle
              !if (angle >= 175) then
              !    call get_info_distance(atomc(a1),atomc(n1),btype)
              ! !   if (btype
              !    write(0,*)'angle=',a1,n1,a2,angle,btype
              !    call get_info_distance(atomc(n1),atomc(a2),btype)
              !    write(0,*)'angle=',a1,n1,a2,angle,btype
              !endif
          elseif (c1 + c2 == 3) then ! case R-C#C(H), R-C#N, >=N=N
!
!             find sequence a1-a2-a3 with a3 terminal atom
              if (c1 == 1) then
                  a1 = conn(n2)%pos(1)
                  if (a1 == n1) a1 = conn(n2)%pos(2)
                  a2 = n2
                  a3 = n1
              else
                  a1 = conn(n1)%pos(1)
                  if (a1 == n2) a1 = conn(n1)%pos(2)
                  a2 = n1
                  a3 = n2
              endif
              if (is_linear(atomc(a1)%xc,atomc(a2)%xc,atomc(a3)%xc)) then
                  if (zval(a2) == N_at .and. zval(a3) == N_at .and. zval(a1) == C_at .and. conn(a1)%nat == 3) then ! match with >=N=N
                      legm(tabconn(a1,a2))%ord = 2
                      legm(tabconn(a2,a3))%ord = 2
                  else
                      legm(tabconn(a1,a2))%ord = 1
                      legm(tabconn(a2,a3))%ord = 3
                  endif
              endif
          elseif (c1 + c2 == 5) then  ! alleni: >=C=<
!!!!!!!!!!!!!!TODO
          endif
      endif
   enddo
!
!  Searching function groups with connectivity 4
   do i=1,nat
      if (conn(i)%nat == 4) then
          select case(zval(i))
            case (15) !Chemical groups of P
             call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,PO4_group,match)
             if (match) cycle
             call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,PO3_group,match)
             if (match) cycle

            case (16) !Chemical groups of S
             call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,sulfoN_group,match)
          end select
      endif
   enddo
!
   call find_sssr(atom,legm,ring,nring)
   if (present(rings) .and. present(nrings)) then
       call copy_container(rings,ring)
       nrings = nring
   endif
!
!  Rings are checked for aromaticity
   allocate(varom(nat), source=0)
   if (nring > 0) then
       do i=1,nring
          plan = all(conn(ring(i)%pos)%nat <= 3) ! in a aromatic ring the connectivity can't be more than 3
          if (plan) plan = ring_planarity(atomc,ring(i))
          if (plan) then
!
!             Init at -20 the bond type in ring
              do j=1,ring(i)%nat-1
                 posb = tabconn(ring(i)%pos(j),ring(i)%pos(j+1))
                 if (legm(posb)%ord == 0) legm(posb)%ord = -20   !!!FIXME - test on phuran
              enddo
              posb = tabconn(ring(i)%pos(ring(i)%nat),ring(i)%pos(1))
              if (legm(posb)%ord == 0) legm(posb)%ord = -20   !!!FIXME -test on phuran
!
              varom(ring(i)%pos) = 1
!
!             Additional check for trigonal planar geometry for 3 connected atoms
              do j=1,ring(i)%nat
                 jc = ring(i)%pos(j)
                 if (conn(jc)%nat == 3) then
                     if (.not.is_trigonal(atomc(jc)%xc,     &
                         atomc(conn(jc)%pos(1))%xc,atomc(conn(jc)%pos(2))%xc,atomc(conn(jc)%pos(3))%xc)) then
                         varom(jc) = 0
!
                         do k=1,conn(jc)%nat
                            posb = tabconn(jc,conn(jc)%pos(k))
                            legm(posb)%ord = SINGLE_BOND  ! jc should be an sp3 atom
                         enddo
                     endif
                 endif
              enddo
              if (kpr) write(0,'(a,*(i4))')'planar ring:',ring(i)%pos
          endif
       enddo
!
!      aromatic N with conn 3
       do i=1,nat
          if (varom(i) == 1 .and. zval(i) == N_at .and. conn(i)%nat == 3) then
              do j=1,conn(i)%nat
                 posb = tabconn(i,conn(i)%pos(j))
                 legm(posb)%ord = 10  !!!dopo prova con 1100765.cif
              enddo
          endif
       enddo
!
!      aromatic S
       do i=1,nat
          if (varom(i) == 1 .and. zval(i) == S_at) then
              do j=1,conn(i)%nat
                 posb = tabconn(i,conn(i)%pos(j))
                 legm(posb)%ord = 10 
              enddo
          endif
       enddo
   endif
!
!  Search for trigonal planar functional group
   do i=1,nat
      if (conn(i)%nat == 3) then  ! .and. varom(i) == 0) then
          if (is_trigonal(atomc(i)%xc,atomc(conn(i)%pos(1))%xc,atomc(conn(i)%pos(2))%xc,atomc(conn(i)%pos(3))%xc)) then
              select case (zval(i))
                case (C_at) !Chemical groups of C
!
!                 Check only amide for aromatic system because amide breaks aromaticity
                  call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,amide_group,match)
                  if (match .and. kpr) write(0,*)match,'match amide at ',i
                  if (match) cycle

                  !if (varom(i) > 0) cycle  ! after bug on cyclic ester  ! moved after
                  call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,CO_group,match)
                  if (match .and. kpr) write(0,*)match,'match chetone at ',i
                  if (match) cycle
                  call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,COO_group,match)
                  if (match .and. kpr) write(0,*)match,'match COOR at ',i
                  if (match) cycle

                  if (varom(i) > 0) cycle  ! C=N group should be tested for conjugation
                  call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,CN2_group,match)
                  if (match .and. kpr) write(0,*)match,'match CN2 at ',i
                  if (match) cycle
                  call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,CN3_group,match)
                  if (match .and. kpr) write(0,*)match,'match CN3 at ',i
                  if (match) cycle
                  call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,urea_group,match)
                  if (match .and. kpr) write(0,*)match,'match urea group at ',i
                  if (match) cycle
                  call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,C3N_group,match)
                  if (match .and. kpr) write(0,*)match,'match C3N group at ',i
                  if (match) cycle
                  call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,CNO2_group,match)
                  if (match .and. kpr) write(0,*)match,'match CNO2 group at ',i
                  if (match) cycle

                case (N_at) !Chemical groups of N
                  if (varom(i) > 0) cycle
                  call match_chemical_group(atomc,zval,legm,conn,tabconn,i,conn(i)%pos,NO2_group,match)
                  if (match .and. kpr) write(0,*)match,'match NO2 at ',i
                  if (match) cycle

                case (S_at) !Chemical groups of S
              end select
          endif
      endif
   enddo
!
   if (nring > 0) then
!
!      chinonic rule
       nchin = 0
       do i=1,nring
          if (ring(i)%nat == 6) then
              nchin = 0
              do j=1,ring(i)%nat
                 apos = ring(i)%pos(j)
                 if (zval(apos) == C_at .and. conn(apos)%nat == 3) then
                     do k=1,conn(apos)%nat
                        a1 = conn(apos)%pos(k)
                        if (zval(a1) == O_at .and. varom(a1) == 0 .and. conn(a1)%nat == 1) then
                            if (kpr) write(0,*)'chin. at. ',a1
                            nchin = nchin + 1
                            vchin(nchin) = apos
                            exit
                        endif
                     enddo
                 endif
              enddo
!!!! FIXME controlla legami semplici per i chinoni, sembra che non entrino nella coniugazione
              if (nchin == 2) then
                  do j=1,nchin
                     do k=1,conn(vchin(j))%nat
                        a1 = conn(vchin(j))%pos(k)
                        if (varom(a1) == 1) then
                            legm(tabconn(a1,vchin(j)))%ord = 1   !!!!-20
                            if (kpr) write(0,*)'leg=',a1,vchin(j),' single'
                        else
                            legm(tabconn(a1,vchin(j)))%ord = 2
                            if (kpr) write(0,*)'leg=',a1,vchin(j),' double'
                        endif
                     enddo
                  enddo
              endif
          endif
       enddo
   endif
!
!  Set order for remaining bonds
   do i=1,nleg
      if (legm(i)%ord == 0) then
          ord_dist = bond_type_from_table(legm(i),zval(legm(i)%n1),zval(legm(i)%n2))
          if (ord_dist >= 2) then
              call bond_type_from_geom(atomc,legm(i),conn,ord_geom)
              select case (ord_geom)
                 !case (0,2,3) 
                 case (0,DOUBLE_BOND) 
                   legm(i)%ord = -20  ! check for conjugation
                 case (TRIPLE_BOND)
                   legm(i)%ord = TRIPLE_BOND
                 case default
                   legm(i)%ord = SINGLE_BOND
              end select
          else
              legm(i)%ord = ord_dist
          endif
      endif
          !write(0,*)'legm=',legm(i)%n1,legm(i)%n2,legm(i)%ord
   enddo
!
   call set_conjugation(legm,zval,nat,nleg)
!
!  Set as single all bonds connected to rings
   !do i=1,nat
   !   if (varom(i) == 1 .and. conn(i)%nat > 2) then
   !       do j=1,conn(i)%nat
   !          if (varom(conn(i)%pos(j)) == 0) then
   !              legm(tabconn(i,conn(i)%pos(j)))%ord = 1
   !          endif
   !       enddo
   !   endif
   !enddo
!
   where (legm(:)%ord == 10) 
          !legm(:)%ord = AR_SINGLE
          legm(:)%ord = SINGLE_BOND
   elsewhere (legm(:)%ord == 20)
          !legm(:)%ord = 2 + 3
          !legm(:)%ord = AR_DOUBLE
          legm(:)%ord = DOUBLE_BOND
   elsewhere (legm(:)%ord == 0)
          legm(:)%ord = SINGLE_BOND
   endwhere
!
   if (kpr) then
       do i=1,nleg
          write(0,'(i4,1x,a,a,a,a,i0)')i,trim(atom(legm(i)%n1)%lab),'-',trim(atom(legm(i)%n2)%lab),' ord=',legm(i)%ord
       enddo
   endif
!
   end subroutine bond_type_perception

!---------------------------------------------------------------------      

   subroutine bond_type_from_geom(atom,leg,conn,order)
!
!  Extract bond order from geometry
!
!!!!FIXME - al momento non gestisce i tripli legami
   USE connect_mod
   USE cgeom
   USE trig_constants
   USE elements
   USE arrayutil
   type(atom_type), dimension(:), intent(in)       :: atom
   type(bond_type), intent(in)                     :: leg  
   type(container_type), dimension(:), intent(in) :: conn
   integer, intent(out)                            :: order
   integer                                         :: n1,n2,at1,at2
   real                                            :: angle
   integer                                         :: i,j
   real                                            :: anglemin
!
   n1 = leg%n1
   n2 = leg%n2
   order = 1
!
!  Don't check if atoms are connected with more then 2 bonds
   if (conn(n1)%nat <= 3 .and. conn(n2)%nat <= 3) then
       if (conn(n1)%nat == 1 .or. conn(n2)%nat == 1) then
           if (conn(n1)%nat == 1 .and. conn(n2)%nat == 1) then
!              orden can't be defined using geometrical consideration
               if (is_organic_el(atom(n1)%z()) .and. is_organic_el(atom(n2)%z()) ) then  ! if organic set from distance
                   order = bond_type_from_table(leg,atom(n1)%z(),atom(n2)%z())  !es. HC#CH
               else
                   order = 0  
               endif
           else
               if (conn(leg%n2)%nat == 1) then
                   n1 = leg%n2
                   n2 = leg%n1
               endif
               if (conn(n2)%nat == 3) then
                   if (is_trigonal(atom(n2)%xc,atom(conn(n2)%pos(1))%xc,   &
                              atom(conn(n2)%pos(2))%xc,atom(conn(n2)%pos(3))%xc)) order = 2
                                 !write(0,*)'trigonal=',n2,conn(n2)%pos,order
               else
                   angle = rtod*angleC(atom(conn(n2)%pos(1))%xc,atom(n2)%xc,atom(conn(n2)%pos(2))%xc)
                                 !write(0,*)'2angle=',conn(n2)%pos(1),n2,conn(n2)%pos(2),angle
                   if (abs(angle - 120) <= 3) then
                       order = 2
                   endif
                   !angle = 0
                   !do i=1,conn(n2)%nat
                   !   n3 = conn(n2)%pos(i)
                   !   if (n3 /= n1) then
                   !       angle = rtod*angleC(atom(n1)%xc,atom(n2)%xc,atom(n3)%xc)
                   !              write(0,*)'angle=',n1,n2,n3,angle
                   !   endif
                   !enddo
               endif
           endif
       else
           anglemin = huge(1.0)
           do i=1,conn(n1)%nat
              at1 = conn(n1)%pos(i)
              if (at1 /= n2) then
                  do j=1,conn(n2)%nat
                     at2 = conn(n2)%pos(j)
                     if (at2 /= n1) then
                         angle = angle_dihedral(atom(at1)%xc,atom(n1)%xc,atom(n2)%xc,atom(at2)%xc)
                         angle = min(abs(angle),abs(180-angle),abs(-180-angle))
                         if (angle < anglemin) anglemin = angle
                         !write(0,*)'angle=',at1,n1,n2,at2,angle,anglemin
                     endif
                  enddo
              endif
           enddo
           if (anglemin <= 15) then  ! find reason on Labute,2005 J.Chem.Inf.Mod
               order = 2
           endif
       endif
   endif
!
   end subroutine bond_type_from_geom

!---------------------------------------------------------------------      

   subroutine set_conjugation(legm,zval,nat,nleg)
!
!  Define bond type for conjugated system. This is a difficult task!
!
   USE connect_mod
   USE elements
   USE arrayutil
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm  
   integer, dimension(:), intent(in)                         :: zval
   integer, intent(in)                                       :: nat, nleg
   integer, dimension(nat)                                   :: veta
   integer, dimension(nleg)                                  :: vetb,vram,block,vordmax
   integer                                                   :: i,j,k,l
   type(bond_type), dimension(:), allocatable                :: legmc
   type(container_type), dimension(:), allocatable          :: connj
   integer                                                   :: nlc, nselb
   integer                                                   :: nram, numb, nlegc
   real                                                      :: costfmax, costf
   logical                                                   :: is_ram, is_nitrogen
   logical :: kpr = .false.
   integer :: lend
!
!  Isolate conjugated system in legmc
   call copy_bonds(legmc,legm)
   where(legmc%ord /= -20) legmc%ord = 0
   call remove_bondsv(legmc,legmc%ord,0)
   call bond_to_connect(nat,legmc,connj)
   nlegc = numbonds(legmc)
   nram = 0
   do i=1,nlegc
      if (connj(legmc(i)%n1)%nat == 3 .or. connj(legmc(i)%n2)%nat == 3) then
          nram = nram + 1
          vram(nram) = i
          if (kpr) write(0,*)'ramificazione at. ',legmc(i)%n1,legmc(i)%n2
      endif
   enddo
!
   do i=1,nlegc
      if (legmc(i)%ord == -20) then
          legmc(i)%ord = 10
          veta(:) = 1
          vetb(:) = 0
          vetb(i) = 1
          call set_conjugation_rec(legmc,connj,i,veta,vetb,.false.)
!
!         Save in block the system of conjugated bonds and check for N
          numb = 0
          is_nitrogen = .false.
          do j=1,nlegc
             if (vetb(j) == 1) then
                 numb = numb + 1
                 block(numb) = j
                 if (.not.is_nitrogen) is_nitrogen = zval(legmc(j)%n1) == N_at .or. zval(legmc(j)%n2) == N_at
             endif
          enddo
               if (kpr) write(0,*)'is_nitrogen=',is_nitrogen
!
!         Just one bond: set it as double
          if (numb == 1) then
              !legmc(block(numb))%ord = 20
              legmc(block(numb))%ord = DOUBLE_BOND
              cycle
          endif
!
!         Ci sono ramificazioni nel blocco?
          if (nram > 0) then
              do j=1,numb
                 is_ram  = any(vram(:nram) == block(j))
                 if (is_ram) exit
              enddo
          else
              is_ram = .false.
          endif
!
!FIXME ad if on nram and Nitrogen in loop
          !if (nram > 0 .or. is_nitrogen) then
          !if (is_ram .or. is_nitrogen) then
!
!             Ci sono ramificazioni nell blocco
              !do j=1,numb
              !   is_ram  = any(vram(:nram) == block(j))
              !   if (is_ram) exit
              !enddo
              !if (is_ram .or. is_nitrogen) then
                  if (kpr) write(0,*)'conj system:',block(:numb)
                  costfmax = bond_weigth(legmc(block(:numb)),zval)
                  if (kpr) write(0,*)'start from: ',legmc(i)%n1,legmc(i)%n2,' CF=',costfmax
                  vordmax(:numb) = legmc(block(:numb))%ord
                        lend = 2
                  loop_reverse:  do l=1,lend
                  do j=1,numb
                     if (.not.(block(j) == i .and. l == 1)) then
                         nselb = block(j)
                         legmc(block(:numb))%ord = -20
                         legmc(nselb)%ord = 10
                         veta(:) = 1
                         vetb(:) = 0
                         vetb(nselb) = legmc(nselb)%ord/10
                         veta(legmc(nselb)%n1) = legmc(nselb)%ord/10
                         veta(legmc(nselb)%n2) = legmc(nselb)%ord/10
                         call set_conjugation_rec(legmc,connj,nselb,veta,vetb,l==2)
!
!                        Compute cost function
                         costf = bond_weigth(legmc(block(:numb)),zval)
                         if (kpr) then
                             write(0,*)'start from: ',legmc(nselb)%n1,legmc(nselb)%n2,' CF=',costf
                             do k=1,numb
                                write(0,*)k,'leg=',legmc(k)%n1,legmc(k)%n2,legmc(k)%ord
                             enddo
                         endif
                         if (costf > costfmax) then
                             costfmax = costf
                             vordmax(:numb) = legmc(block(:numb))%ord
                             if (kpr) write(0,*)'new max:',costfmax
                         endif
                     endif
              if (j==2 .and. .not.(is_ram .or. is_nitrogen)) exit loop_reverse  ! stop testing without ram and N
                  enddo
                     enddo loop_reverse
                  legmc(block(:numb))%ord = vordmax(:numb)
              !endif
          !endif
      endif
   enddo
!
   nlc = 0
   do i=1,nleg
      if (legm(i)%ord == -20) then
          nlc = nlc + 1
          legm(i)%ord = legmc(nlc)%ord 
      endif
   enddo
!
!corr   CONTAINS
!corr
!corr   real function cost_function()    result(cf)
!corr   cf = 0
!
!  N con singolo legame e' favorito
!corr   do k=1,nat
!corr      !!!!!if(connj(k)%nat > 0) write(0,*)k,'veta=',veta(k),' Z=',zval(k)
!corr      if (connj(k)%nat > 0 .and. zval(k) == N_at .and. veta(k) == H_at) cf = cf + 1
!corr   enddo
!
!corr   cf = cf + sum(legmc(block(:numb))%ord) / 10
!corr   !!!!!write(0,*)'start from: ',legmc(nselb)%n1,legmc(nselb)%n2,' CF=',costf
!corr   end function cost_function
!
   end subroutine set_conjugation
   
!---------------------------------------------------------------------      
  
   real function bond_weigth(leg,zval)  result(wei)
   USE connect_mod
   type(bond_type), dimension(:), intent(in) :: leg  
   integer, dimension(:), intent(in)         :: zval
   integer                                   :: i
   type(bond_type) :: legC_C = bond_type(6,6,1.33,0.3,20)
   type(bond_type) :: legC_N = bond_type(6,7,1.33,0.3,20)
!
   wei = 0
   do i=1,size(leg)
      if (is_bond_equal(leg(i),legC_C,zval)) then
          !write(0,*)'contr. leg=',i,leg(i)%n1,leg(i)%n2,8
          wei = wei + 8
      elseif (is_bond_equal(leg(i),legC_N,zval)) then
          !write(0,*)'contr. leg=',i,leg(i)%n1,leg(i)%n2,6
          wei = wei + 6
      endif
   enddo
!
   end function bond_weigth

!---------------------------------------------------------------------      

   recursive subroutine set_conjugation_rec(legm,conn,start,veta,vetb,rev)
   USE connect_mod
   USE arrayutil
   type(bond_type), dimension(:), intent(inout)    :: legm  
   type(container_type), dimension(:), intent(in) :: conn
   integer, intent(in)                             :: start 
   integer, dimension(:), intent(inout)            :: veta   ! order for each atoms
   integer, dimension(:), intent(inout)            :: vetb   ! if 1 the order has been assigned for bond
   logical, intent(in)                             :: rev
   type(bond_type)                                 :: leg
   integer                                         :: i
   integer                                         :: ini,fin,step
   leg = legm(start)
   if (rev) then
       ini = size(legm)
       fin = 1
       step = -1
   else
       ini = 1
       fin = size(legm)
       step = 1
   endif
   do i=ini,fin,step
      if (conn(legm(i)%n1)%nat == 0) exit ! chain terminated
      if (legm(i)%ord < 10 .and. legm(i)%ord >= 0) cycle   ! 0 < ord < 10
      if (i == start) cycle
      if (legm(i)%ord == 20 .or. legm(i)%ord == 10) cycle
      if ((legm(i)%n1 == leg%n1 .or. legm(i)%n2 == leg%n1) .or.     &
          (legm(i)%n1 == leg%n2 .or. legm(i)%n2 == leg%n2)) then
          if (veta(legm(i)%n1) == 2 .or. veta(legm(i)%n2) == 2) then
              legm(i)%ord = 10
          else
              legm(i)%ord = 20
              veta(legm(i)%n1) = 2
              veta(legm(i)%n2) = 2
          endif
          vetb(i) = 1
          call set_conjugation_rec(legm,conn,i,veta,vetb,rev)
      endif
   enddo
   end subroutine set_conjugation_rec
   
!---------------------------------------------------------------------      

   logical function is_trigonal(xc,x1,x2,x3)
!
!  Check if x1,x2,x3 have trigonal, planar geometry around xc
!
   USE cgeom
   real, dimension(3), intent(in) :: xc,x1,x2,x3
   real                           :: triple_product
!
   triple_product = dot_product(x1 - xc, cross_product(x2 - xc, x3 - xc))
   is_trigonal = abs(triple_product) <= 0.9
!
   end function is_trigonal

!---------------------------------------------------------------------      

   logical function is_linear_x3(x1,x2,x3)
!
!  Check if x1,x2,x3 have linear geometry
!
   USE cgeom
   USE trig_constants, only:rtod
   real, dimension(3), intent(in) :: x1,x2,x3
!
   !is_linear = rtod*angleC(x1,x2,x3) >= 170
   is_linear_x3 = rtod*angleC(x1,x2,x3) >= 155
!
   end function is_linear_x3

!---------------------------------------------------------------------      

   logical function is_linear_x4(x1,x2,x3,x4)
!
!  Check if x1,x2,x3,x4 have linear geometry
!
   USE cgeom
   USE trig_constants, only:rtod
   real, dimension(3), intent(in) :: x1,x2,x3,x4
!
   is_linear_x4 = is_linear_x3(x1,x2,x3) .and. is_linear(x2,x3,x4)
!
   end function is_linear_x4

!---------------------------------------------------------------------      

   subroutine match_chemical_group(atom,zval,legm,conn,tab,xc,xa,group,match)
   USE connect_mod
   USE cgeom
   USE arrayutil
   USE elements
   type(atom_type), dimension(:), intent(in)      :: atom
   integer, dimension(:), intent(in)              :: zval
   type(bond_type), dimension(:), intent(inout)   :: legm  
   type(container_type), dimension(:), intent(in) :: conn
   integer, dimension(:,:)                        :: tab
   integer                                        :: xc
   integer, dimension(:)                          :: xa
   type(chem_group_t)                             :: group
   logical, intent(out)                           :: match
   integer, dimension(group%geom)                 :: xvet
   integer                                        :: i,j
   integer                                        :: ngeom
   integer                                        :: pos, bnat, cval !, at
   integer, dimension(4)                          :: vat, jump
   integer                                        :: btype
   integer                                        :: posmin
   real                                           :: dmin, dist
   integer, dimension(:), allocatable             :: ordsav
!
   match = .false.
!
!  Check on Z for xc
   match = zval(xc) == group%zc 
   if (match) then
!
!      Check on Z e connectivity on xa
       ngeom = group%geom
       xvet(:) = 0
       do i=1,ngeom
          do j=1,ngeom
             if (zval(xa(j)) == group%za(i) .and. conn(xa(j))%nat <= group%cval(i) .and. all(xvet(:) /= xa(j))) then
                 xvet(i) = xa(j)
                 exit
             endif
          enddo
          if (xvet(i) == 0) then
              match = .false.
              exit
          endif
       enddo
!
!      Check on connectivity
       if (match) then
           !write(0,*)'NEW: carbonilic at ',xc,xvet(:ngeom)
           jump(:) = 0
           allocate(ordsav(size(legm)),source=legm(:)%ord)
           do i=1,ngeom
              if (jump(i) == 1) cycle
              btype = group%btype(i)
              select case (btype)
                case (1,2,3)
                  pos = tab(xvet(i),xc)
                  if (btype == 2) then
                      if (.not.is_bond_order(legm(pos),btype,zval(xvet(i)),zval(xc),0.15)) then
!corr                          write(0,*)'rigettato:',bond_type_from_table(legm(pos),zval(xvet(i)),zval(xc))
                          legm%ord = ordsav    ! restore old array
                          match = .false.
                          return
                      endif
                  endif
                  legm(pos)%ord = group%btype(i)
!
!                 Set bond type also for the other bonds
!!!!!TEMP: after cif bug_perc.mol
                  !do j=1,conn(xvet(i))%nat
                  !   at = conn(xvet(i))%pos(j)
                  !   if (at /= xc .and. zval(at) /= C_at) then  ! not this for carbon
                  !       pos = tab(xvet(i),conn(xvet(i))%pos(j))
                  !       legm(pos)%ord = 1 
                  !       !if (legm(pos)%ord == 0) legm(pos)%ord = 1 ! avoid changing order if already assigned - BAD
                  !   endif
                  !enddo

                case (20)  ! might be  1 or 2
                  cval = group%cval(i)
                  bnat = 0
                  do j=1,ngeom
                     if (group%btype(j) == btype) then
                         if (conn(xvet(i))%nat <= cval) then
                             bnat = bnat + 1
                             vat(bnat) = xvet(j)
                         else
                             pos = tab(xvet(j),xc)
                             legm(pos)%ord = 1
                         endif
                         jump(j) = 1
                     endif
                  enddo
                  if (bnat == 1) then
                      pos = tab(vat(1),xc)
                      legm(pos)%ord = 1
                  else
!
!                     Assign order 2 to minimum distance
                      dmin = distanzaC(atom(vat(1))%xc,atom(xc)%xc)
                      posmin = 1
                      do j=2,bnat
                         dist = distanzaC(atom(vat(j))%xc,atom(xc)%xc)
                         if (dist < dmin) then
                             posmin = j
                             dist = dmin
                         endif
                      enddo
                      !write(0,*)'due possibilita: calcola distanze ', vat(:bnat)
                      do j=1,bnat
                         pos = tab(vat(j),xc)
                         if (j == posmin) then
                             legm(pos)%ord = 2
                         else
                             legm(pos)%ord = 1
                         endif
                      enddo
                  endif
              end select
           enddo
       endif
   endif
!
   end subroutine match_chemical_group

!---------------------------------------------------------------------      

   logical function ring_planarity(atom,ring)
!
!  Determine wheter ring is planar or not.
!  Ref: works of Zhao and Sayle about perception of molecules, J. Chem. Inf. Coumput. Sci.
!
   !USE connect_mod
   USE cgeom
   USE arrayutil
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(container_type), intent(in)                      :: ring
   integer                                                :: kat, rsize
   real                                                   :: ave, val, cutoff
   integer                                                :: i, n1, n2, n3, n4
!
   rsize = ring%nat
!
   ave = 0
   do i=1,rsize
      kat = mod(i-1,rsize)
      if (kat == 0) kat = rsize
      n1 = ring%pos(kat)
      n2 = ring%pos(i) 
      n3 = ring%pos(mod(i,rsize) + 1)
      n4 = ring%pos(mod(i+1,rsize) + 1)
      val = Angle_Dihedral(atom(n1)%xc,atom(n2)%xc,atom(n3)%xc,atom(n4)%xc)
      ave = ave + abs(val)
   enddo
   ave = ave / rsize
!
!  The same cutoff of fconv is used
   if (rsize <= 5) then
       cutoff = 10
   else
       cutoff = 15
   endif
   ring_planarity = ave <= cutoff
   !write(0,*)'ave=',ave,ring_planarity
!
   end function ring_planarity

!---------------------------------------------------------------------      

   real function ring_planarity_estimate(atom,ring) result(rps)
!
!  Estimate the planarity of ring
!
   !USE connect_mod
   USE cgeom
   USE arrayutil
   type(atom_type), dimension(:), allocatable, intent(in) :: atom 
   type(container_type), intent(in)                      :: ring 
   integer                                                :: kat, rsize
   real                                                   :: val
   integer                                                :: i, n1, n2, n3, n4
!
   rsize = ring%nat
!
   rps = 0
   do i=1,rsize
      kat = mod(i-1,rsize)
      if (kat == 0) kat = rsize
      n1 = ring%pos(kat)
      n2 = ring%pos(i) 
      n3 = ring%pos(mod(i,rsize) + 1) 
      n4 = ring%pos(mod(i+1,rsize) + 1) 
      val = Angle_Dihedral(atom(n1)%xc,atom(n2)%xc,atom(n3)%xc,atom(n4)%xc)
      rps = rps + abs(val)
   enddo
   rps = rps / rsize
!
   end function ring_planarity_estimate

!---------------------------------------------------------------------      

   subroutine coord_in_newcell(atom,cellold,cellnew)
!
!  Trasporta le coord. frazionarie da cellold in cellnew
!
   USE unit_cell
   type(atom_type), dimension(:), intent(inout) :: atom
   type(cell_type), intent(in)                  :: cellold,cellnew
!
!  Converti in cartesiane usando cellold
   call frac_to_cart(atom,cellold%get_ortom())
!
!  Riconverti in coordinate frazionarie con la nuova cella
   call cart_to_frac(atom,cellnew%get_ortoi())
!
   end subroutine coord_in_newcell

!----------------------------------------------------------------------------------------------------
   
   subroutine rand_rotate_atoms(atom,xrot,cell,xc)
!
!  Ruota in modo random un insieme di atomi intorno al baricentro   
!
   USE cgeom
   USE unit_cell
   type(atom_type), dimension(:), intent(inout) :: atom   !atomi prima della rotazione
   real, dimension(:), intent(in)               :: xrot   !3 random numbers tra 0-1 
   type(cell_type), intent(in)                  :: cell
   real, dimension(3), intent(in), optional     :: xc     !centre of rotation in fractional coordinates
   integer                                      :: i   
   real, dimension(3,3)                         :: rmat
   integer                                      :: nat
   real, dimension(3)                           :: xcc
!
   nat = size(atom) !numero di atomi
!
!  Converti il modello iniziale in coordinate cartesiane
   call frac_to_cart(atom,cell%get_ortom())
!
!  Set rotation centre xcc
   if (present(xc)) then
       xcc = matmul(cell%get_ortom(),xc)
   else
       xcc = baricentro(atom)
   endif
!
!  Translate xc in the origin
   do i=1,nat
      atom(i)%xc = atom(i)%xc - xcc
   enddo
!
!  Genera matrice di rotazione random  
   rmat = rand_rotation_matrix(xrot)   
!
!  Applica rotazione alle coordinate cartesiane   
   do i=1,nat
      atom(i)%xc = matmul(rmat,atom(i)%xc)
   enddo
!
!  Ritrasla xc nella posizione originaria   
   do i=1,nat
      atom(i)%xc = atom(i)%xc + xcc
   enddo
!
!  Ripristina in coordinate frazionarie      
   call cart_to_frac(atom,cell%get_ortoi())   
!   
   end subroutine rand_rotate_atoms
      
!----------------------------------------------------------------------------------------------------
   
   subroutine rotate_atoms(atmr,pp1,pp2,theta,cell,cart)
!
!  Ruota di theta un insieme di atomi intorno ad un asse per i punti p1 e p2
!
   USE cgeom
   USE unit_cell
   type(atom_type), dimension(:), intent(inout) :: atmr     ! atomi da routare
   real, dimension(3), intent(in)               :: pp1,pp2  ! 2 punti che individuano l'asse di rotazione
   real, intent(in)                             :: theta    ! angolo di rotazione in radianti
   type(cell_type)                              :: cell
   logical, intent(in), optional                :: cart
   logical                                      :: carte
   real                                         :: l,m,n
   real, dimension(3,3)                         :: rmat
   integer                                      :: nat
   integer                                      :: i
   real, dimension(3)                           :: p1,p2
!
   if (present(cart)) then
       carte = cart
   else
       carte = .false.
   endif
!
!  p1 e p2 in coord cartesiane
   if (.not. carte) then
       p1 = pp1
       p2 = pp2
   else
       p1 = pp1
       p2 = pp2
   endif
!
!  Coseni direttori l,m,n
   l = p2(1)
   m = p2(2)
   n = p2(3)
!
!  Calcola matrice di rotazione
   rmat = rotation_matrix(l,m,n,theta)
!
   nat = size(atmr) !numero di atomi
!
!  Converti il modello iniziale in coordinate cartesiane
   if (.not.carte) call frac_to_cart(atmr,cell%get_ortom())
!
!  Fissa l'origine sull'atomo p1
   do i=1,nat
      atmr(i)%xc = atmr(i)%xc - p1
   enddo   
!
!  Applica rotazione alle coordinate cartesiane   
   do i=1,nat
      atmr(i)%xc = matmul(rmat,atmr(i)%xc)
   enddo
!
!  Ripristina l'origine
   do i=1,nat
      atmr(i)%xc = atmr(i)%xc + p1
   enddo      
!
!  Ripristina in coordinate frazionarie      
   if (.not.carte) call cart_to_frac(atmr,cell%get_ortoi())   
!     
   end subroutine rotate_atoms      

!----------------------------------------------------------------------------------------------------

   integer function serial_number(atom) result(kser)
!
!  Extract the serial number of an atom
!
   USE atom_basic
   type(atom_type), intent(in) :: atom
   integer                     :: ier
!
   call get_from_label(atom%lab,numb=kser,ierror=ier)
   if (ier > 0) kser = -1  ! serial assente
!
   end function serial_number

!----------------------------------------------------------------------------------------------------

   subroutine remove_duplicate_labels(atom)
!
!  Remove duplicate labels
!
   USE strutil
   USE atom_basic
   type(atom_type), dimension(:), intent(inout) :: atom
   integer                                      :: i
   integer                                      :: maxs
   integer                                      :: numb,error
   character(len=100)                           :: prefix
!
   do i=1,size(atom)
      if (is_duplicated_label(atom,i)) then
          maxs = max_serial_number(atom,atom(i)%lab)
          call get_from_label(atom(i)%lab,prefix,numb,error)
          atom(i)%lab = trim(prefix)//i_to_s(maxs+1)
      endif
      !write(0,*)'Atom: ',trim(atom(i)%lab),max_serial_number(atom,atom(i)%lab),is_duplicated_label(atom,i)
   enddo
!
   !write(0,*)'NEW LABEL'
   !do i=1,size(atom)
   !   write(0,*)'new label:',trim(atom(i)%lab)
   !enddo
!
   end subroutine remove_duplicate_labels

!----------------------------------------------------------------------------------------------------

   logical function is_duplicated_label(atom,k)
!
!  For the label of atom k check if the label is duplicated
!
   USE strutil
   type(atom_type), dimension(:), intent(inout) :: atom
   integer, intent(in)                          :: k
   integer                                      :: i
!
   is_duplicated_label = .false.
   do i=1,k-1   ! check only up to k position
      is_duplicated_label = s_eqi(atom(i)%lab,atom(k)%lab)
      if (is_duplicated_label) exit
   enddo
!
   end function is_duplicated_label

!----------------------------------------------------------------------------------------------------

   integer function max_serial_number(atom,lab,prefix) result(maxserial)
!
!  Find the maximum serial number for a specified prefix of label
!
   USE strutil
   USE atom_basic
   type(atom_type), dimension(:), intent(inout) :: atom
   character(len=*), intent(in),optional        :: lab
   character(len=*), intent(in),optional        :: prefix
   integer                                      :: error
   character(len=SIZELAB)                       :: sprefix_lab
   integer                                      :: numb, numb_lab
   integer                                      :: i
   character(len=100)                           :: sprefix
!
   maxserial = 0
!
   if (present(lab)) then
       call get_from_label(lab,sprefix_lab,numb_lab,error)
   else
       sprefix_lab = trim(prefix)
   endif
!
   do i=1,size(atom)
      call get_from_label(atom(i)%lab,sprefix,numb,error)
      if (error == 0) then
          if (numb > maxserial .and. s_eqi(sprefix,sprefix_lab)) maxserial = numb
      endif
   enddo
!
   end function max_serial_number

!----------------------------------------------------------------------------------------------------
!corr
!corr   elemental logical function is_metal(atom)
!corr   USE elements
!corr   type(atom_type), intent(in) :: atom
!corr   integer, parameter :: notmetal(16) = [H_at,He_at,C_at,N_at,O_at,F_at,Ne_at,P_at,S_at,Cl_at,Ar_at,Br_at,Kr_at,I_at,Xe_at,Rn_at]
!corr!
!corr   is_metal = .not.any(notmetal(:) == atom%z())
!corr!
!corr   end function is_metal
!corr
!----------------------------------------------------------------------------------------------------

   logical function is_metal(atom)
!
!  true if the atom is metal
!
   USE elements
   type(atom_type), intent(in) :: atom
   integer, parameter :: notmetal(16) = [H_at,He_at,C_at,N_at,O_at,F_at,Ne_at,P_at,S_at,Cl_at,Ar_at,Br_at,Kr_at,I_at,Xe_at,Rn_at]
!
   is_metal = .not.any(notmetal(:) == atom%z())
!
   end function is_metal

!----------------------------------------------------------------------------------------------------

   logical function any_is_metal(atom)
!
!  true if some atom is metal
!
   type(atom_type), dimension(:), intent(in) :: atom
   integer                                   :: i
   do i=1,size(atom)
      any_is_metal = is_metal(atom(i))
      if (any_is_metal) return
   enddo
   end function any_is_metal

!----------------------------------------------------------------------------------------------------

   logical function all_is_metal(atom)
!
!  true if all atoms are metal
!
   type(atom_type), dimension(:), intent(in) :: atom
   integer                                   :: i
   all_is_metal = .true.
   do i=1,size(atom)
      all_is_metal = is_metal(atom(i))
      if (.not. all_is_metal) return
   enddo
   end function all_is_metal

!----------------------------------------------------------------------------------------------------

   real function density_value(mw,vcell,nop)
!
!  Compute density in g/cm3
!
   USE elements
   real, intent(in)     :: mw     ! molecular mass of asymmetric unit
   real, intent(in)     :: vcell  ! cell volume
   integer, intent(in)  :: nop    ! number of symmetry operators
   real, parameter      :: DENS_CONST = 1.660543780 !1/NA*10E-24
!
   density_value = DENS_CONST*nop*mw/vcell
!
   end function density_value

!--------------------------------------------------------------------

   real function volume_per_atom_cont(specv,nspecv,vcell) result(vol)
!
!  Compute volume per atom from cell content
!
   USE strutil
   character(len=*), dimension(:), intent(in) :: specv   ! specie coinvol
   integer, dimension(:), intent(in)          :: nspecv  ! num. di atomi per specie
   real, intent(in)                           :: vcell   ! volume della cella
   real                                       :: sumspec
   integer                                    :: i
!
!  Count non-hydrogen species
   sumspec = 0.
   do i=1,size(nspecv)
      if (.not.s_eqi(specv(i),'H ')) then
          sumspec = sumspec + nspecv(i)
      endif
   enddo
!
   if (sumspec > 0) then
       vol  = vcell  / sumspec
   else
       vol = 0.
   endif
!
   end function volume_per_atom_cont

!----------------------------------------------------------------------------------------------------

   real function volume_per_atom_elem(elem,vcell) result(vol)
   USE elements
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   real, intent(in)                           :: vcell   ! volume della cella
   real                                       :: sumspec
   integer                                    :: i
!
!  Count non-hydrogen species
   sumspec = 0.
   do i=1,numelem(elem)
      if (elem(i)%z /= H_at) then
          sumspec = sumspec + elem(i)%nw
      endif
   enddo
!
   if (sumspec > 0) then
       vol  = vcell  / sumspec
   else
       vol = 0.
   endif
!
   end function volume_per_atom_elem

!----------------------------------------------------------------------------------------------------

   real function volume_per_atom_at(atom,nsymt,vcell)  result(vol)
!
!  Compute volume per atom from asymmetric unit
!
   USE elements
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   integer, intent(in)                       :: nsymt
   real, intent(in)                          :: vcell
   real                                      :: natcell
!
!  Sum over non-H atoms
   natcell = natom_cell(atom,nsymt,.true.)
   if (natcell > 0) then
       vol  = vcell  / natcell
   else
       vol = 0.
   endif
!
   end function volume_per_atom_at

!----------------------------------------------------------------------------------------------------

   real function molecular_volume(atom) result(vol)
!
!  Compute volume of molecule in atom
!
   USE elements
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
!
   vol = 0.
   if (numatoms(atom) == 0) return
   vol = sum(average_volume(atom%z()))
!
   end function molecular_volume

!----------------------------------------------------------------------------------------------------

   real function number_of_molecules(atom,cell,spg)  result(Z)
!
!  Compute number of molecules from cell volume
!
   USE unit_cell
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(cell_type), intent(in)                            :: cell
   type(spaceg_type), intent(in)                          :: spg
   real, parameter                                        :: PACKING_FACTOR = 0.7
   real                                                   :: vol_per_atom
!
   Z = PACKING_FACTOR*cell%volume()/(spg%nsymop*molecular_volume(atom))
   vol_per_atom = volume_per_atom(atom, spg%nsymop, cell%volume())/nint(Z)
   if (vol_per_atom > 30) Z=Z+1
!
   end function number_of_molecules

!----------------------------------------------------------------------------------------------------

   real function linear_abs_coeff(atom,dens,wave)   result(mu)
!
!  Compute the linear absorption coefficient (cm-1) (mu)
!  Mass absorption coefficient (MAC) = mu/dens
!
   USE elements
   USE ccryst
   USE errormod
   type(atom_type), dimension(:), intent(in)     :: atom
   real, intent(in)                              :: dens
   real, intent(in)                              :: wave
   type(element_type), dimension(:), allocatable :: elem
   integer                                       :: nat,nel,i,j
   real                                          :: mw   !!!!, dens
   type(error_type)                              :: err
!
   call elements_from_atom(atom,elem)
   nel = numelem(elem)
   if (nel > 0) then
       nat = size(atom)
       do i=1,nel
          elem(i)%nw = 0
          do j=1,nat
             if (atom(j)%ptab == elem(i)%ptab) then
                 elem(i)%nw = atom(j)%och*atom(j)%ocry + elem(i)%nw
             endif
          enddo
       enddo
       elem(:)%nw = elem(:)%nw * elem(:)%weight
       mw = sum(elem(:)%nw) 
!OLD       call elem_set_mac(elem,wave) ! get mac at the selected wavelength
       call elem_set_nist_factors(elem,wave,RX_SOURCE,err)
       mu = (dens/mw) * sum(elem(:)%nw * elem(:)%mac)
   else
       mu = 0
   endif
!
   end function linear_abs_coeff

!----------------------------------------------------------------------------------------------------

   subroutine calcola_occ_new(atom,spg,gmat,distmin)
   USE cgeom
   USE spginfom
   type(atom_type), dimension(:), intent(inout) :: atom
   type(spaceg_type), intent(in)                :: spg
   real, dimension(3,3), intent(in)             :: gmat
   real, intent(in)                             :: distmin
   real, dimension(3,spg%nsymop)                   :: xeq
   integer                                      :: i
   integer                                      :: nat   !,neqa
   integer, dimension(spg%nsymop)                  :: vets
   real, dimension(3)                           :: ktra
   real                                         :: dd
   integer                                      :: k1,k2
               !real,dimension(3) :: s
!
   nat = size(atom)
   do i=1,nat
      call get_equivalent1(atom(i)%xc,spg,xeq)
      vets(:) = 1
      do k1=1,spg%nsymop
         if (vets(k1) == 1) then
             do k2=k1+1,spg%nsymop
                if (vets(k2) == 1) then
                    call xdisteqs(xeq(:,k1),xeq(:,k2),gmat,dd,ktra)
                    if (dd <= distmin) then
                        !write(0,*)'OP=',k1,k2,dd
                        vets(k2) = 0
                    endif
                endif
             enddo
         endif
      enddo
      atom(i)%ocry = sum(vets)/real(spg%nsymop)
   enddo
!corr   do i=1,nat
!corr      write(0,*)'OCC.'//trim(atom(i)%lab),atom(i)%ocry
!corr   enddo
!
   end subroutine calcola_occ_new

!----------------------------------------------------------------------------------------------------

   subroutine make_symmetry_table(atom,spg,atomsym,vet)
   USE cgeom
   USE spginfom
   type(atom_type), dimension(:), intent(in)               :: atom
   type(spaceg_type), intent(in)                           :: spg
   real, dimension(size(atom),3,spg%nsymop), intent(inout) :: atomsym
   integer, dimension(:), optional                         :: vet
   integer                                                 :: i
!
   if (present(vet)) then
       do i=1,size(vet)
          call get_equivalent1(atom(vet(i))%xc,spg,atomsym(vet(i),:,:))
       enddo
   else
       do i=1,size(atom)
          call get_equivalent1(atom(i)%xc,spg,atomsym(i,:,:))
       enddo
   endif
!
   end subroutine make_symmetry_table

!----------------------------------------------------------------------------------------------------

   subroutine compute_doc_st_all(atom,spg,gmat,distmin,atomsym)
   USE cgeom
   USE spginfom
   type(atom_type), dimension(:), intent(inout) :: atom
   type(spaceg_type), intent(in)                :: spg
   real, dimension(3,3), intent(in)             :: gmat
   real, intent(in)                             :: distmin
   real, dimension(size(atom),3,spg%nsymop), intent(in) :: atomsym
   real, dimension(3,spg%nsymop)                   :: xeq
   integer                                      :: nat
   integer                                      :: i,j,k
   real                                         :: dd
   real, dimension(3)                           :: ktra
!
   nat = size(atom)
   atom%ocry = 1.0
   do i=1,nat
      !call get_equivalent1(atom(i)%xc,spg,xeq)
      xeq = atomsym(i,:,:)
      !do j=1,nat      !!!old
      do j=i,nat     !!!new
         if (atom(i)%get_nz() == atom(j)%get_nz()) then
             do k=1,spg%nsymop
                if (k==1 .and. j == i) cycle
                call xdisteqs(atom(j)%xc,xeq(:,k),gmat,dd,ktra)
                if (dd < distmin) then
                    atom(i)%ocry = atom(i)%ocry + abs(dd-distmin)
                    if (i /= j) atom(j)%ocry = atom(j)%ocry + abs(dd-distmin)   !!!new occ
                    !write(0,*)trim(atom(i)%lab)//'-'//trim(atom(j)%lab),abs(dd-distmin)
                endif
             enddo
         endif
      enddo
   enddo
   atom%ocry = 1/atom%ocry
   !do i=1,nat
   !   write(0,*)'OCC.'//trim(atom(i)%lab),atom(i)%ocry
   !enddo
!
   end subroutine compute_doc_st_all

!----------------------------------------------------------------------------------------------------

   subroutine compute_doc_all(atom,spg,gmat,distmin)
   USE cgeom
   USE spginfom
   type(atom_type), dimension(:), intent(inout) :: atom
   type(spaceg_type), intent(in)                :: spg
   real, dimension(3,3), intent(in)             :: gmat
   real, intent(in)                             :: distmin
   real, dimension(3,spg%nsymop)                   :: xeq
   integer                                      :: nat
   integer                                      :: i,j,k
   real                                         :: dd
   real, dimension(3)                           :: ktra
!
   nat = size(atom)
   atom%ocry = 1.0
   do i=1,nat
      call get_equivalent1(atom(i)%xc,spg,xeq)
      !do j=1,nat      !!!old
      do j=i,nat     !!!new
         if (atom(i)%get_nz() == atom(j)%get_nz()) then
             do k=1,spg%nsymop
                if (k==1 .and. j == i) cycle
                call xdisteqs(atom(j)%xc,xeq(:,k),gmat,dd,ktra)
                if (dd < distmin) then
                    atom(i)%ocry = atom(i)%ocry + abs(dd-distmin)
                    if (i /= j) atom(j)%ocry = atom(j)%ocry + abs(dd-distmin)   !!!new occ
                    !write(0,*)trim(atom(i)%lab)//'-'//trim(atom(j)%lab),abs(dd-distmin)
                endif
             enddo
         endif
      enddo
   enddo
   atom%ocry = 1/atom%ocry
!corr   do i=1,nat
!corr      write(0,*)'OCC.'//trim(atom(i)%lab),atom(i)%ocry
!corr   enddo
!
   end subroutine compute_doc_all

!----------------------------------------------------------------------------------------------------

   subroutine compute_doc_atoms(atom,cell,spg,distmin)
   USE cgeom
   USE spginfom
   USE unit_cell
   type(atom_type), dimension(:), intent(inout) :: atom
   type(cell_type), intent(in)                  :: cell
   type(spaceg_type), intent(in)                :: spg
   real, intent(in)                             :: distmin
   real, dimension(3,spg%nsymop)                :: xeq
   integer                                      :: nat
   integer                                      :: i,j,k
   real                                         :: dd
   real, dimension(3)                           :: ktra
   real, dimension(3,3)                         :: gmat
!
   nat = size(atom)
   gmat = cell%get_g()
   atom%ocry = 1.0
   do i=1,nat
      call get_equivalent1(atom(i)%xc,spg,xeq)
      do j=i,nat
         if (atom(i)%get_nz() == atom(j)%get_nz()) then
             do k=1,spg%nsymop
                if (k==1 .and. j == i) cycle
                call xdisteqs(atom(j)%xc,xeq(:,k),gmat,dd,ktra)
                if (dd < distmin) then
                    atom(i)%ocry = atom(i)%ocry + abs(dd-distmin)
                    if (i /= j) atom(j)%ocry = atom(j)%ocry + abs(dd-distmin)
                    !write(0,*)trim(atom(i)%lab)//'-'//trim(atom(j)%lab),abs(dd-distmin)
                endif
             enddo
         endif
      enddo
   enddo
   atom%ocry = 1/atom%ocry
!corr   do i=1,nat
!corr      write(0,*)'OCC.'//trim(atom(i)%lab),atom(i)%ocry
!corr   enddo
!
   end subroutine compute_doc_atoms

!----------------------------------------------------------------------------------------------------

   subroutine compute_doc_vet(atom,spg,gmat,distmin,vet)
   USE cgeom
   USE atom_basic
   USE spginfom
   type(atom_type), dimension(:), intent(inout) :: atom
   type(spaceg_type), intent(in)                :: spg
   real, dimension(3,3), intent(in)             :: gmat
   real, intent(in)                             :: distmin
   integer, dimension(:), intent(in)            :: vet
   real, dimension(3,spg%nsymop)                   :: xeq
   integer                                      :: nat
   integer                                      :: i,j,k,ki,kj
   real                                         :: dd
   real, dimension(3)                           :: ktra
!
   nat = size(vet)
   atom(vet)%ocry = 1.0
   do ki=1,nat
      i = vet(ki)
      call get_equivalent1(atom(i)%xc,spg,xeq)
      do kj=ki,nat  
         j = vet(kj)
         if (atom(i)%get_nz() == atom(j)%get_nz()) then
             do k=1,spg%nsymop
                if (k==1 .and. j == i) cycle
                call xdisteqs(atom(j)%xc,xeq(:,k),gmat,dd,ktra)
                if (dd < distmin) then
                    atom(i)%ocry = atom(i)%ocry + abs(dd-distmin)
                    if (i /= j) atom(j)%ocry = atom(j)%ocry + abs(dd-distmin)   !!!new occ
                    !write(0,*)trim(atom(i)%lab)//'-'//trim(atom(j)%lab),abs(dd-distmin)
                endif
             enddo
         endif
      enddo
   enddo
   atom(vet)%ocry = 1/atom(vet)%ocry
   !do i=1,nat
   !   write(0,*)'OCC.'//trim(atom(i)%lab),atom(i)%ocry
   !enddo
!
   end subroutine compute_doc_vet

!----------------------------------------------------------------------------------------------------

   logical function is_duplicated_atoms(atom,dist,cell,spg)
!
!  Check for duplicated atoms
!
   USE unit_cell
   USE spginfom
   USE connect_mod
   USE cgeom
   type(atom_type), dimension(:), intent(in) :: atom
   type(cell_type), intent(in), optional     :: cell
   type(spaceg_type), intent(in), optional   :: spg
   real, intent(in)                          :: dist !minimum distance
   integer                                   :: i,j,k,nat
   real, dimension(3)                        :: ktra,xc
   real                                      :: dd
!
   is_duplicated_atoms = .true.
   nat = size(atom)
   if (present(spg)) then   ! symmetry check
       do i=1,nat-1
          do j=i+1,nat
             if (atom(i)%get_nz() == atom(j)%get_nz()) then
                 do k=1,spg%nsymop
                    xc = atom_symm(atom(i)%xc,spg%symop(k))
                    call xdisteqs(atom(j)%xc,xc,cell%get_g(),dd,ktra)
                    if (dd <= dist) then
                        return   
                    endif
                 enddo
             endif
          enddo
       enddo
   else
       do i=1,nat-1
          do j=i+1,nat
             if (atom(i)%get_nz() == atom(j)%get_nz()) then
                 if (distanzaC(atom(j)%xc,atom(i)%xc,cell%get_g()) <= dist) then
                     return   
                 endif
             endif
          enddo
       enddo
   endif
   is_duplicated_atoms = .false.
!
   end function is_duplicated_atoms

!----------------------------------------------------------------------------------------------------

   subroutine find_duplicate_atoms(atom,legm,cell,spg,dist,vet,nd,kpr)
!
!  Find duplicate atoms in a distance dist
!
   USE cgeom
   USE atom_basic
   USE connect_mod
   USE unit_cell
   USE spginfom
   USE arrayutil
   type(atom_type), dimension(:), intent(in)              :: atom
   type(bond_type), dimension(:), allocatable, intent(in) :: legm  
   type(cell_type), intent(in)                            :: cell
   type(spaceg_type), intent(in)                          :: spg
   real, intent(in)                                       :: dist !minimum distance
   integer, intent(in)                                    :: kpr  !if  >=0 print is enabled
   integer, dimension(size(atom)), intent(out)            :: vet
   integer, dimension(size(atom),size(atom))              :: tabat
   type(container_type), dimension(:), allocatable        :: conn
   integer                                                :: nd
   integer                                                :: i,j
   integer                                                :: nat
   integer, dimension(size(atom))                         :: groupat
   integer                                                :: natg
   real, dimension(3)                                     :: xc
   real, dimension(3)                                     :: ktra
   real                                                   :: dd
   integer                                                :: k, jat
   character(len=:), allocatable                          :: strl
   integer, dimension(1)                                  :: cmax
   integer                                                :: minb, minj
!
   nd = 0
   nat = size(atom)
!
!  Create table with 1 for couples (i,j) of atoms < dist
   tabat(:,:) = 0
   do i=1,nat-1
      loop_atom: do j=i+1,nat
         if (atom(i)%get_nz() == atom(j)%get_nz()) then
             do k=1,spg%nsymop
                xc = atom_symm(atom(i)%xc,spg%symop(k))
                call xdisteqs(atom(j)%xc,xc,cell%get_g(),dd,ktra)
                if (dd <= dist) then
                    tabat(i,j) = 1
                    cycle loop_atom
                endif
             enddo
         endif
      enddo loop_atom
   enddo 
!
   if (any(tabat(:,:) == 1)) then
       vet(:) = 0
       call bond_to_connect(nat,legm,conn)
       do i=1,nat-1
          if (any(vet(:nd) == i)) cycle
!
!         store in groupat atoms in the same position
          natg = 1
          groupat(natg) = i
          do j=i+1,nat
             if (tabat(i,j) > 0 .and. (all(vet(:nd) /= j)))then
                 natg = natg + 1
                 groupat(natg) = j
             endif
          enddo
          if (natg > 1) then
              if (kpr >= 0) then
                  strl = slabvet(groupat(:natg),atom(:)%lab)
                  write(kpr,'(a,a,a)')' WARNING: The atoms ',strl,' are in the same position'
              endif
              if (all(conn(groupat(2:natg))%nat == conn(groupat(1))%nat)) then 
!                 if atoms in group have the same number of bonds keep the atom bound to atom with lower connection
!                 so tetrahedra are preserved rispect to octahedra
                  cmax(1) = 1
                  minb = huge(1)
                  do j=1,natg
                     jat = groupat(j)
                         !write(0,*)'G=',groupat(:natg)
                     !write(0,*)jat,'VAL=',minval(conn(conn(jat)%pos)%nat)
                     if (conn(jat)%nat > 0) then  ! if jat is not an isolated atom
                         minj = minval(conn(conn(jat)%pos)%nat)
                         if (minj < minb) then
                             minb = minj
                             cmax(1) = j
                         endif
                     endif
                  enddo
                  !write(0,*)'non so cosa rimuovere, keep '//trim(atom(groupat(cmax(1)))%lab)
              else
!                 keep atom with higher connectivity
                  cmax = maxloc(conn(groupat(:natg))%nat)
                  !write(0,*)'rimuovo tutti tranne '//trim(atom(groupat(cmax(1)))%lab)
              endif
              do j=1,natg
                 if (j /= cmax(1)) then
                     nd = nd + 1
                     vet(nd) = groupat(j)
                 endif
              enddo
          endif
       enddo
            !write(0,*)'vet=',vet(:nd)
       if (nd > 0 .and. kpr >= 0) then
           write(kpr,'(/a,a)')' List of duplicate atoms: ',slabvet(vet(:nd),atom(:)%lab)
           write(kpr,'(a)')" Choose 'Delete Duplicate Atoms' in the menu Modify to delete these atoms"
       endif
   endif
!
   end subroutine find_duplicate_atoms

!----------------------------------------------------------------------------------------------------

   subroutine get_atoms_from_string_vet(string,atom,vatom,natom,val,defval,nval,dupl,jolly,err)
!
!  Read string containing list of atoms and an optional numeric value
!
   USE strutil
   USE errormod
   character(len=*), intent(in)                           :: string
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   integer, dimension(:), intent(out)                     :: vatom 
   integer, intent(out)                                   :: natom
   real, dimension(:), intent(out), optional              :: val
   real, dimension(:), intent(in), optional               :: defval
   integer, intent(out), optional                         :: nval
   logical, intent(in), optional                          :: dupl  ! check for duplicate atoms
   logical, intent(in), optional                          :: jolly ! jolly character * is allowed
   type(error_type), intent(out)                          :: err
   character(len=len_trim(string))                        :: line
   character(len=20), dimension(size(atom))               :: wordv
   integer                                                :: nword
   logical                                                :: lval,dupli
   real                                                   :: rval
   integer                                                :: i,j,nw,natstr,iw
   integer, dimension(size(atom))                         :: vatstr
   logical                                                :: jollyc
!
   natom = 0
   line = trim(adjustl(string))
   call s_filter(line)
   if (present(val) .and. present(defval)) val(:) = defval(:)
   if (present(dupl)) then
       dupli = dupl
   else
       dupli = .true.
   endif
   if (len_trim(line) > 0) then
       call get_words(line,wordv,nword)
!
       nw = nword
!
!      Check if the last word is numeric
       if (present(val)) then
           nval = 0
           do iw=1,size(val)
              call s_is_r(wordv(nword-iw+1),rval,lval)
              if (lval) then
                  !!!!nw = nw - 1
                  nval = nval + 1
                  val(nval) = rval
              else
                  exit
              endif
           enddo
           if (nval > 0) then
               nw = nw - nval
               val(:nval) = val(nval:1:-1)
           endif
       endif
!
       jollyc = .true.
       if (present(jolly)) then
           jollyc = jolly
       endif
!
!      Read labels
       do i=1,nw
          if (jollyc) then
              call get_atoms_of_string(wordv(i),atom,vatstr,natstr)
          else
              call get_atoms_of_label(wordv(i),atom,vatstr,natstr)
          endif
          if (natstr == 0) then
              if (err%signal) then
                  call err%add(', '//wordv(i))
              else
                  call err%set('Undefined atom(s) '//trim(wordv(i)))
              endif
          else
              do j=1,natstr
                 if (dupli) then
                     if (any(vatom(:natom) == vatstr(j))) then  ! check for duplicate atom
                         call err%set('Duplicate atom: '//trim(wordv(i)))
                         return
                     endif
                 endif
                 natom = natom + 1
                 if (natom > size(vatom)) then
                     call err%set('Too many atoms')
                     return
                 endif
                 vatom(natom) = vatstr(j)
              enddo
          endif
       enddo
   endif
!
   end subroutine get_atoms_from_string_vet

!----------------------------------------------------------------------------------------------------

   subroutine change_bond_distance(atom,legm,k,dist,gmat)
!
!  Change bond distance of the bond k 
!
   USE connect_mod
   USE arrayutil
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom   ! all atoms 
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm   ! all bonds
   integer, intent(in)                                       :: k      ! pointer to bond to change
   real, intent(in)                                          :: dist
   real, dimension(3,3), intent(in)                          :: gmat
   type(container_type), dimension(:), allocatable           :: connt
   integer, dimension(size(atom))                            :: vet1,vet2
   integer                                                   :: nat1,nat2
   integer                                                   :: nat
   logical                                                   :: in_ring
   real, dimension(3)                                        :: xtra
   integer                                                   :: i
   integer                                                   :: n1,n2
!
   n1 = legm(k)%n1
   n2 = legm(k)%n2
!corr   if (kpr > 0) write(0,*)'change bond ',k,trim(atom(n1)%lab)//'-'//trim(atom(n2)%lab)
   nat = numatoms(atom)
   call bond_to_connect(nat,legm,connt)
   call get_atoms_legm(k,legm,connt,vet1,nat1,vet2,nat2,in_ring)
!
!  Compute translation
   xtra = (atom(vet1(1))%xc - atom(vet2(1))%xc)*(dist - legm(k)%dist) / legm(k)%dist
   if (in_ring) then
!
!      Move the less number of atoms
       call get_atoms_outside_ring(n1,n2,legm,connt,vet1,nat1)
       vet1(nat1+1) = n1; nat1 = nat1 + 1
       call get_atoms_outside_ring(n2,n1,legm,connt,vet2,nat2)
       vet2(nat2+1) = n2; nat2 = nat2 + 1
   endif
   !else
!
!  Apply translation to group with less atoms
   if (nat1 < nat2) then
       forall(i=1:nat1) atom(vet1(i))%xc = atom(vet1(i))%xc + xtra
       call bond_distance_update(n1,atom,legm,connt(n1),gmat)
       !write(0,*)'TRANS nat1'
   else  ! if nat1 >= nat2
       forall(i=1:nat2) atom(vet2(i))%xc = atom(vet2(i))%xc - xtra
       call bond_distance_update(n2,atom,legm,connt(n2),gmat)
       !write(0,*)'TRANS nat2'
   endif
   !endif
   !write(0,'(a,*(i5))')'chain 1:',vet1(:nat1)
   !write(0,'(a,*(i5))')'chain 2:',vet2(:nat2)
   !write(0,*)'IN RING=',in_ring
!
   end subroutine change_bond_distance

!----------------------------------------------------------------------------------------------------

   subroutine get_atoms_outside_ring(n1,n2,legm,conn,vetbond,nb)
!
!  For the bond n1-n2 in ring provide in vetbond the atoms connected to n1 outside the ring
!
   USE connect_mod
   USE arrayutil
   integer, intent(in)                                          :: n1,n2
   type(bond_type), dimension(:), allocatable, intent(in)       :: legm
   type(container_type), dimension(:), allocatable, intent(in) :: conn
   integer, dimension(:), intent(out)                           :: vetbond
   integer, intent(out)                                         :: nb
   integer                                                      :: i
   integer, dimension(size(conn))                               :: veta,vetb
   integer                                                      :: jat, kleg
   logical                                                      :: in_ring
   integer                                                      :: nata, natb
!
   nb = 0
   vetbond(:) = 0
   do i=1,conn(n1)%nat
      jat = conn(n1)%pos(i)
      if (jat == n2) cycle
      kleg = bond_position(legm,n1,jat)
      call get_atoms_legm(kleg,legm,conn,veta,nata,vetb,natb,in_ring)
      if (in_ring) cycle
      if (any(veta(:nata) == n2)) then
          !write(0,*)n1,jat,'vetb=',vetb(:natb)
          vetbond(vetb(:natb)) = vetb(:natb)
      else
          !write(0,*)n1,jat,'veta=',veta(:nata)
          vetbond(veta(:nata)) = veta(:nata)
      endif
   enddo
   if (count(vetbond > 0) > 0) then
       nb = count(vetbond > 0)
       vetbond(:nb) = pack(vetbond,mask=vetbond > 0)
       !write(0,*)'vetb=',vetbond(:)
   endif
!
   end subroutine get_atoms_outside_ring

!----------------------------------------------------------------------------------------------------
   
   subroutine change_bond_angle(k,angv,atom,legm,aval,cell)
!
!  Change bond angle of angle k
!
   USE connect_mod
   USE cgeom
   USE unit_cell
   USE arrayutil
   integer, intent(in)                                        :: k    ! order number of angle to modify
   type(atom_type), dimension(:), allocatable, intent(inout)  :: atom ! atoms
   type(bond_type), dimension(:), allocatable, intent(in)     :: legm ! bonds
   type(angle_type), dimension(:), allocatable, intent(inout) :: angv ! angles
   real, intent(in)                                           :: aval ! final value of angle k
   type(cell_type), intent(in)                                :: cell ! cell parameters
   type(angle_type)                                           :: ang
   integer                                                    :: nat,k1,k2
   type(container_type), dimension(:), allocatable           :: connt
   integer, dimension(size(atom))                             :: vet12,vet21,vet23,vet32
   integer                                                    :: nat12,nat21,nat23,nat32
   logical                                                    :: in_ring1,in_ring2
   real, dimension(4)                                         :: xplan
   type(atom_type), dimension(size(atom))                     :: atmr
   type(atom_type), dimension(3)                              :: atang
   real                                                       :: diffa
   real                                                       :: dir
   real, dimension(3,3)                                       :: orm   !!!!!,ori
!
   nat = numatoms(atom)
   call bond_to_connect(nat,legm,connt)
   ang = angv(k)
   k1 = bond_position(legm,ang%n1,ang%n2)
   call get_atoms_legm(k1,legm,connt,vet12,nat12,vet21,nat21,in_ring1)
   k2 = bond_position(legm,ang%n2,ang%n3)
   call get_atoms_legm(k2,legm,connt,vet23,nat23,vet32,nat32,in_ring2)
   orm = cell%get_ortom()
   atang = [cartesian_coord(atom(ang%n1),orm),cartesian_coord(atom(ang%n2),orm),cartesian_coord(atom(ang%n3),orm)]
   xplan = plane3points(atang(1)%xc,atang(2)%xc,atang(3)%xc,ptype=1)
   if (in_ring1 .and. in_ring2) then
       atmr(1) = atom(ang%n1) 
       diffa = (ang%val-aval)*dtor/2
       call rotate_atoms(atmr(:1),atang(2)%xc,xplan(:3),diffa,cell)
       atom(ang%n1) = atmr(1)
       atmr(1) = atom(ang%n3) 
       call rotate_atoms(atmr(:1),atang(2)%xc,xplan(:3),-diffa,cell)
       atom(ang%n3) = atmr(1)
       angv(k)%val = aval
       call bond_angle_update(ang%n1,atom,angv,cell%get_g())
       call bond_angle_update(ang%n3,atom,angv,cell%get_g())
   else
       if (in_ring1) then
!corr          write(0,*)'bond 1 in ring',in_ring1+in_ring2     ! T
           if (legm(k2)%n1 == ang%n3) then
!corr               write(0,*)'1rotate ',vet23(:nat23)
               vet12(:nat23) = vet23(:nat23)
               nat12 = nat23
               dir = 1
           else
!corr               write(0,*)'2rotate ',vet32(:nat32)
               vet12(:nat32) = vet32(:nat32)
               nat12 = nat32
               dir = -1
           endif
       elseif(in_ring2) then
!
!          rotate n1 and connected atoms
!corr        write(0,*)'bond 2 in ring',in_ring1+in_ring2     ! T
           if (legm(k1)%n1 == ang%n1) then
               dir = 1
!corr               write(0,*)'1rotate ',vet12(:nat12)
           else
!corr               write(0,*)'2rotate ',vet21(:nat21)
               dir = -1
               vet12(:nat21) = vet21(:nat21)
               nat12 = nat21
           endif
      else
!
!          rotate the smallest number of atoms
           dir = 1
           if (legm(k1)%n1 /= ang%n1) then
               vet12(:nat21) = vet21(:nat21)
               nat12 = nat21
!corr               write(0,*)'1rotate ',vet12(:nat12)
           endif
           if (legm(k2)%n1 /= ang%n3) then
               vet23(:nat32) = vet32(:nat32)
               nat23 = nat32
!corr               write(0,*)'2rotate ',vet23(:nat23)
           endif
           if (nat12 > nat32) then
               vet12(:nat23) = vet23(:nat23)
               nat12 = nat23
               dir = -1
           endif
!corr           write(0,*)'rotate ',vet12(:nat12)
      endif
      atmr(:nat12) = atom(vet12(:nat12))
!corr      atang = [cartesian_coord(atom(ang%n1)),cartesian_coord(atom(ang%n2)),cartesian_coord(atom(ang%n3))]
!corr      xplan = plane3points(atang(1)%xc,atang(2)%xc,atang(3)%xc,ptype=1)
      call rotate_atoms(atmr(:nat12),atang(2)%xc,xplan(:3),dir*(ang%val-aval)*dtor,cell)
      atom(vet12(:nat12)) = atmr(:nat12)
      call bond_angle_update(ang%n2,atom,angv,cell%get_g())
   endif
!
   end subroutine change_bond_angle

!----------------------------------------------------------------------------------------------------

   subroutine change_torsion_angle(tors,atom,legm,tval,cell,changed)
!
!  Rotate torsion angle
!
   USE connect_mod
   USE cgeom
   USE unit_cell
   USE arrayutil
   type(torsion_type), intent(inout)                         :: tors    ! torsion angle to modify
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom    ! atoms
   type(bond_type), dimension(:), allocatable, intent(in)    :: legm    ! bonds
   real, intent(in)                                          :: tval    ! final value of angle k
   type(cell_type), intent(in)                               :: cell    ! cell parameters
   logical, intent(out)                                      :: changed ! torsion angle was changed?
   integer                                                   :: nat
   type(container_type), dimension(:), allocatable          :: connt
   integer                                                   :: kleg
   integer, dimension(size(atom))                            :: vet1,vet2
   integer                                                   :: nat1,nat2
   logical                                                   :: in_ring
   type(atom_type), dimension(size(atom))                    :: atmr
   real, dimension(3)                                        :: pa1,pa2
   real :: dir 
!
   nat = numatoms(atom)
   call bond_to_connect(nat,legm,connt)
   kleg = bond_position(legm,tors%n2,tors%n3)
   call get_atoms_legm(kleg,legm,connt,vet1,nat1,vet2,nat2,in_ring)
   if (.not.in_ring) then
!
!      rotate the smallest number of atoms
       dir = 1
       if (nat2 < nat1) then
           vet1(:nat2) = vet2(:nat2)
           nat1 = nat2
           dir = -1
!corr           write(0,*)'change dir'
       endif
!corr       write(0,*)'1rotate: ',atom(vet1(:nat1))%lab,' di ',tval-tors%val,' from ',tors%val,' to ',norm_torsion(tval)
!corr       ortom = cell%get_ortom()
!corr       ortoi = cell%get_ortoi()
       pa1 = matmul(cell%get_ortom(),atom(tors%n2)%xc)
       pa2 = matmul(cell%get_ortom(),atom(tors%n3)%xc)
       atmr(:nat1) = atom(vet1(:nat1))
       call rotate_atoms(atmr(:nat1),pa1,direction_cos(pa1,pa2),dir*(tval - tors%val)*dtor,cell)
       atom(vet1(:nat1)) = atmr(:nat1)
       tors%val = norm_torsion(tval)
       changed = .true.
   else
       changed = .false.
!corr       write(0,*)'bond in ring'
   endif
!
   end subroutine change_torsion_angle

!----------------------------------------------------------------------------------------------------
  
   real function norm_torsion(ang) 
!
!  Normalize torsion angle between -180 and 180
!
   real, intent(in) :: ang
   real :: mang
   !mang = mod(ang+36000,360.)
   mang = mod(ang,360.)
   if (mang > 180) then
       norm_torsion = mang - 360
   elseif (mang < -180) then
       norm_torsion = 360 + mang
   else
       norm_torsion = mang
   endif
!
   end function norm_torsion

!----------------------------------------------------------------------------------------------------

   subroutine copy_atoms_sym(atoms,legms,kop,symop,tcell,na,vet)
!
!  Copy atoms applying the sym operator symop and tcell. Duplicate atoms are not removed
!
   USE connect_mod
   USE spginfom
   type(atom_type), dimension(:), allocatable, intent(inout)     :: atoms
   type(bond_type), dimension(:), allocatable, intent(inout)     :: legms
!corr   type(symminfo_type), dimension(:), allocatable, intent(inout) :: symm
   integer, intent(in)                                           :: kop
   type(symop_type), intent(in)                                  :: symop
   integer, dimension(3), intent(in)                             :: tcell
   integer, intent(in), optional                                 :: na
   integer, dimension(:), intent(in), optional                   :: vet
   type(atom_type), dimension(:), allocatable                    :: atom
   type(bond_type), dimension(:), allocatable                    :: legm
   integer :: nat, natnew, i, nleg
!
   nat = numatoms(atoms)
!
   if (present(na)) then
       allocate(atom(na))
       atom = atoms(vet)
       if (numbonds(legms) > 0) call extract_bonds(legms,vet,legm,nleg)
   else
       call copy_atoms(atom,atoms)
       call copy_bonds(legm,legms)
   endif
   call apply_sym_oper(atom,symop)
   call translate_atoms(atom,real(tcell))
!
   call add_atoms_to_list(atoms,atom,natnew,legms,legm)
!corr   call reallocate_infos(symm,natnew)
   do i=nat+1,natnew
!corr      symm(i)%asym = symm(i-nat)%asym
!corr      symm(i)%oper = kop   ! FIXME: symm(i-nat)%oper + kop
!corr      symm(i)%opcode = operator_code(kop,tcell)
      atoms(i)%asym = atoms(i-nat)%asym
      atoms(i)%op = op_type(kop,tcell)   ! FIXME: symm(i-nat)%oper + kop
   enddo
!
   end subroutine copy_atoms_sym

!----------------------------------------------------------------------------------------------------

   function lsq_conditions(atom,spg,cell,iser) result(code)
!
!  Atom in fixed position because of special positions or polar axis
!
   USE unit_cell
   USE spginfom
   USE kspec_mod
   type(atom_type), intent(in)   :: atom
   type(spaceg_type), intent(in) :: spg
   type(cell_type), intent(in)   :: cell
   integer, intent(in)           :: iser
   integer, dimension(10)        :: code
   real, dimension(11)           :: xo,xn
   integer, dimension(10)        :: key
   integer                       :: js,khead=0
   integer                       :: k
!
   xo(:3) = atom%xc
   js = lattice_system(spg,cell%get_par())
   if (iser == 1) khead = 0
   k = kspecb_new(xo,xn,key,spg,js,cell%get_g(),2,iser,atom%lab,khead)
   code(:) = key(:)
  ! do i=1,3
  !    if (key(i) == 0) then
  !        code(i) = -1
  !    else
  !        code(i) = 0
  !    endif
  ! enddo
! 
!  Check for polar axis only for iser == 1
   if (iser == 1) then
       if (spg%polar()) then
           call spg%code_for_polar(code,-1)
       endif
   endif
!
   end function lsq_conditions

!----------------------------------------------------------------------------------------------------

   subroutine resize_atoms(vetr,n,savevet)
!
!  Resize array of atoms
!
   type(atom_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n
   logical, optional, intent(in)               :: savevet
   logical                                     :: savev
   integer                                     :: nv
   type(atom_type), allocatable                :: vsav(:)
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
!          nsav contiene qual e' la porzione di vetr da salvare
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
   end subroutine resize_atoms

!----------------------------------------------------------------------------------------------------

   subroutine new_atoms(vetr,n)
!
!  Create new atoms
!
   type(atom_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n

   if (n < 0) return
   if (numatoms(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_atoms

!----------------------------------------------------------------------------------------------------

   subroutine push_back_atom(arr,val)
!
!  Adds a new atom at the end of the array
!
   type(atom_type), dimension(:), allocatable, intent(inout) :: arr
   type(atom_type), intent(in)                               :: val
   integer                                                   :: ndim
   ndim = numatoms(arr)
   call resize_atoms(arr,ndim+1)
   arr(ndim+1) = val
   end subroutine push_back_atom

!----------------------------------------------------------------------------------------------------

   subroutine clear_atoms(vetr)
!
!  Delete all atoms
!
   type(atom_type), allocatable, intent(inout) :: vetr(:)

   if (allocated(vetr)) deallocate(vetr)

   end subroutine clear_atoms

!----------------------------------------------------------------------------------------------------

   subroutine set_specie_atoms(atom,nz,elem,ini,fin)
!
!  Set specie from nz
!
   USE elements
   USE atom_basic
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   integer, dimension(:), intent(in)                         :: nz
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   integer, intent(in), optional                             :: ini,fin
   integer                                                   :: inia,fina,nat
   integer                                                   :: i,k
!
   nat = numatoms(atom)
   if (nat == 0 ) return

   if (present(ini)) then
       if (ini > nat .or. ini < 0) return
       inia = ini
   else
       inia = 1
   endif

   if (present(fin)) then
       if (fin > nat .or. fin < 0) return
       fina = fin
   else
       fina = nat
   endif
!
   k = 0
   do i=inia,fina
      k = k + 1
      call atom(i)%set_specie(nz(k),elem)
   enddo
!
   end subroutine set_specie_atoms

 !----------------------------------------------------------------------------------------------------

   subroutine specie_from_el(atom,pos,elem,ini,fin)
!
!  Set specie from element type
!
   USE elements
   USE atom_basic
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   integer, intent(in)                                       :: pos
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   integer, intent(in), optional                             :: ini,fin
   integer                                                   :: inia,fina,nat
   integer                                                   :: i,k
!
   nat = numatoms(atom)
   if (nat == 0 ) return

   if (present(ini)) then
       if (ini > nat .or. ini < 0) return
       inia = ini
   else
       inia = 1
   endif

   if (present(fin)) then
       if (fin > nat .or. fin < 0) return
       fina = fin
   else
       fina = nat
   endif
!
   k = 0
   do i=inia,fina
      k = k + 1
      call atom(i)%set_specie_from_el(pos,elem)
   enddo
!
   end subroutine specie_from_el
   
!----------------------------------------------------------------------------------------------------

   subroutine specie_from_ptab(atom,elem)
!
!  Ptab has assigned, set specie
!
   USE atom_basic
   USE elements
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   integer                                                   :: i
   do i=1,numatoms(atom)
      call atom(i)%set_specie_from_ptab(elem)
   enddo
   end subroutine specie_from_ptab

!-------------------------------------------------------------------------------------------------

   Subroutine Zmatrix_to_Cartesian(na,I_coor,conn)
   USE cgeom
   USE trig_constants
   integer, intent(in)                    :: na
   real, dimension(:,:), intent(inout)    :: I_coor
   integer, dimension(:,:), intent(inout) :: conn
   integer                                :: i,j,k,n
   real                                   :: dist, ang
   real, dimension(3)                     :: ci,ri,rj,rk,rn
!
!  First atom is always at origin (Z-matrix)
   I_coor(:,1) = 0.0
   conn(:,1) = 0
!   
!  Second atom is always along "x"
   I_coor(2:3,2) = 0.0
   conn(2:3,2) = 0
   conn(1,2)   = 1
!
!  Third atom is always in the "xy" plane       !A(i) d_ij  ang_ijk   dang_ijkl  j k l
   if (conn(1,3) == 1) then
       conn(2,3) = 2
       conn(3,3) = 0
       dist= I_coor(1,3)
       ang = I_coor(2,3)*dtor
       I_coor(1,3) = dist * cos(ang)
       I_coor(2,3) = dist * sin(ang)
       I_coor(3,3) = 0.0
   else
       conn(1,3) = 2
       conn(2,3) = 1
       conn(3,3) = 0
       dist= I_coor(1,3)
       ang = I_coor(2,3)*dtor
       I_coor(1,3) = dist * cos(pi-ang) +  I_coor(1,2)
       I_coor(2,3) = dist * sin(pi-ang)
       I_coor(3,3) = 0.0
   end if

   do i=4,na
      ci(:) = I_coor(:,i)
      j     = conn(1,i)         !The connectivity is needed for the Z-matrix description
      k     = conn(2,i)         !If the connectivity is given it is possible to transform to
      n     = conn(3,i)         !Z-matrix if cartesian/spherical coordinates are given.
      if (j == 0 .or. k == 0 .or. n == 0) cycle
      rj(:) = I_coor(:,j)
      rk(:) = I_coor(:,k)
      rn(:) = I_coor(:,n)
      call get_cartesian_from_Z(ci,ri,rj,rk,rn)
      I_coor(:,i) = ri
   end do

   end subroutine Zmatrix_to_Cartesian

!------------------------------------------------------------------------------------------

   subroutine cartesian_to_zmatrix(atom,legm,I_coor,conn)
   USE connect_mod
   USE cgeom
   USE trig_constants
   type(atom_type), intent(inout), dimension(:)           :: atom
   type(bond_type), intent(in), dimension(:), allocatable :: legm
   real, dimension(:,:), intent(out)                      :: I_coor
   integer, dimension(:,:), intent(out)                   :: conn
   integer, dimension(size(atom),size(atom))              :: T_Conn
   real, dimension(size(atom),size(atom))                 :: T_Dist
   integer                                                :: i,j,k,l,m,n
   integer                                                :: nc1,nc2,nc3
   integer                                                :: na
   real, dimension(3)                                     :: ri,rj,rk,rn,ci
   real                                                   :: dist,ang
!
!  Genera da legm tabella delle distanze   
   T_Conn(:,:) = 0
   T_Dist(:,:) = 0.0
   !do i=1,size(legm)
   do i=1,numbonds(legm)
      nc1 = legm(i)%n1
      nc2 = legm(i)%n2
      if (nc1 < nc2) then
          T_Conn(nc1,nc2) = nc1
          T_Conn(nc2,nc1) = nc1
      else
          T_Conn(nc1,nc2) = nc2
          T_Conn(nc2,nc1) = nc2
      endif
      T_Dist(nc1,nc2) = legm(i)%dist
      T_Dist(nc2,nc1) = legm(i)%dist
   enddo
!
   na = size(atom)
   do i=1,na
!      
!     Distances: Fill N1
      j=minloc(T_Dist(i,1:i-1),dim=1,mask=(T_Dist(i,1:i-1) > 0.0))
      conn(1,i)=j
!
!     Angles: Fill N2
      if (i > 2) then
         nc1=count((T_Conn(j,1:i-1) > 0 .and. T_Conn(j,1:i-1) /=j),dim=1)
         nc2=count((T_Conn(i,1:i-1) > 0 .and. T_Conn(i,1:i-1) /=j),dim=1)
         k=0
         if (nc1 > 0) then
            do
               k=minloc(T_Dist(j,1:i-1),dim=1, mask=(T_Dist(j,1:i-1) > 0.0))
               if (k == j) then
                  T_Dist(j,k)=-T_Dist(j,k)
                  cycle
               else
                  exit
               end if
            end do
         elseif (nc2 > 0) then
            do
               k=minloc(T_Dist(i,1:i-1),dim=1, mask=(T_Dist(i,1:i-1) > 0.0))
               if (k == j) then
                  T_Dist(i,k)=-T_Dist(i,k)
                  cycle
               else
                  exit
               end if
            end do
         end if
         if (k == 0) then
            !scegli uno qualsiasi
            do l=1,i-1
               if (l == j) cycle
               k=l
               exit
            end do
         end if
         Conn(2,i)=k
      end if
      T_Dist=abs(T_Dist)
!
!     Torsion 
      if (i > 3) then
         nc1=count((T_Conn(k,1:i-1) > 0 .and. T_Conn(k,1:i-1) /=j .and. T_Conn(k,1:i-1) /=k),dim=1)
         nc2=count((T_Conn(j,1:i-1) > 0 .and. T_Conn(j,1:i-1) /=j .and. T_Conn(j,1:i-1) /=k),dim=1)
         nc3=count((T_Conn(i,1:i-1) > 0 .and. T_Conn(i,1:i-1) /=j .and. T_Conn(i,1:i-1) /=k),dim=1)

         l=0
         if (nc1 > 0) then
            do
               l=minloc(T_Dist(k,1:i-1),dim=1, mask=(T_Dist(k,1:i-1) > 0.0))
               if (l == j .or. l == k) then
                  T_Dist(k,l)=-T_Dist(k,l)
                  cycle
               else
                  exit
               end if
            end do
         elseif (nc2 > 0) then
            do
               l=minloc(T_Dist(j,1:i-1),dim=1, mask=(T_Dist(j,1:i-1) > 0.0))
               if (l == j .or. l == k) then
                  T_Dist(j,l)=-T_Dist(j,l)
                  cycle
               else
                  exit
               end if
            end do
         elseif (nc3 > 0) then
            do
               l=minloc(T_Dist(i,1:i-1),dim=1, mask=(T_Dist(i,1:i-1) > 0.0))
               if (l == j .or. l == k) then
                  T_Dist(i,l)=-T_Dist(i,l)
                  cycle
               else
                  exit
               end if
            end do
         end if
         if (l==0) then
            !scegli uno qualsiasi
            do m=1,i-1
               if (m == j .or. m == k) cycle
               l=m
               exit
            end do
         end if
         conn(3,i)=l
      end if
      T_Dist=abs(T_Dist)
   enddo
!
!  First atom is always at origin (Z-matrix)
   I_Coor(:,1) = 0.0
   conn(1,1) = 1
   conn(2:3,1) = 0
!
!  Second atom is always along "x"
   if (na > 1) then
       ri= atom(2)%xc - atom(1)%xc
       dist=sqrt(dot_product(ri,ri))
       I_Coor(1,2)   = dist
       I_Coor(2:3,2) = 0.0
       conn(2:3,2)   = 0
       conn(1,2)     = 1
!
!      Third atom is always in the "xy" plane ----!
!      A(i) d_ij  ang_ijk   dang_ijkl  j k l
       if (na > 2) then
           if (conn(1,3) == 1) then
              conn(2,3) = 2
              conn(3,3) = 0
              ri = atom(3)%xc - atom(1)%xc
              rj = atom(2)%xc - atom(1)%xc
              dist= sqrt(dot_product(ri,ri))
              !ang = acosd(dot_product(ri,rj)/dist/sqrt(dot_product(rj,rj)))
              ang = rtod*acos(dot_product(ri,rj)/dist/sqrt(dot_product(rj,rj)))
              I_coor(1,3) = dist
              I_coor(2,3) = ang
              I_coor(3,3) = 0.0
           else
              conn(1,3) = 2
              conn(2,3) = 1
              conn(3,3) = 0
              ri = atom(3)%xc - atom(2)%xc
              rj = atom(1)%xc - atom(2)%xc
              dist= sqrt(dot_product(ri,ri))
              !ang = acosd(dot_product(ri,rj)/dist/sqrt(dot_product(rj,rj)))
              ang = rtod*acos(dot_product(ri,rj)/dist/sqrt(dot_product(rj,rj)))
              I_coor(1,3) = dist
              I_coor(2,3) = ang
              I_coor(3,3) = 0.0
           end if
       endif
   endif
!
   do i=4,na       
      ri = atom(i)%xc
      j  = conn(1,i)  
      k  = conn(2,i) 
      n  = conn(3,i)
!
      rj = atom(j)%xc 
      rk = atom(k)%xc
      rn = atom(n)%xc
      call get_Z_from_cartesian(ci,ri,rj,rk,rn)
      I_coor(:,i) = ci
   end do
! 
   end subroutine cartesian_to_zmatrix

!------------------------------------------------------------------------------------------

   subroutine make_random(atom)
!
!  Make random coordinates
!
   USE rand_mod
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   integer                                                   :: i
   do i=1,numatoms(atom)
      atom(i)%xc = randvalue(3,0.,1.)
   enddo
   end subroutine make_random

!------------------------------------------------------------------------------------------

   integer function origin_for_rotation(conn,pos)  result(or)
!
!  Find origin for rotations for atoms specified in array pos
!
   !USE connect_mod
   USE arrayutil
   type(container_type), dimension(:), allocatable :: conn
   integer, dimension(:), intent(in)                :: pos
   integer                                          :: i,nac,jat,orp
   logical                                          :: nac1
!
   or = 0
   nac = size(pos)
   if (nac <= 2) return
!
!  Find an atom connectected to all other atoms
   nac1 = .false.
   do i=1,size(pos)
      jat = pos(i)
      if (conn(jat)%nat > 1) then
          if (nac1) return
          nac1 = .true.
          orp = jat
      endif
   enddo
   or = orp
!
   end function origin_for_rotation

!----------------------------------------------------------------------------------------------------

   subroutine make_specie(atom,spg,elem,radtype)
!
!  Assign specie from peak intensity and occupancy
!  occupancy atom%ocry must be already assigned, call make_occupancy before
!
   USE elements
   USE nr
   USE spginfom
   type(atom_type), dimension(:), intent(inout)              :: atom
   type(spaceg_type), intent(in)                             :: spg
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   integer, intent(in)                                       :: radtype
   integer                                                   :: nat
   real                                                      :: maxinte
   integer                                                   :: i,j
   integer                                                   :: nelem,nelmin
   type(element_type), dimension(:), allocatable             :: el
   real, dimension(:), allocatable                           :: elint,zdiff
   integer, dimension(:), allocatable                        :: ordiff
   real                                                      :: const
   integer                                                   :: jmin,kspec,kat
   integer, dimension(size(atom))                            :: ordat
   real, dimension(size(atom))                               :: atint
   real :: zcal,tol
!
   nat = size(atom)
   if (nat == 0) return
!
   nelem = numelem(elem)
   if (nelem == 0) then
       do i=1,nat
          call atom(i)%set_specie(0,elem)
       enddo
       return
   endif
!
!  Sort elements in array el from the most intense
   allocate(el(nelem),source=elem)
   call ordina_elements(el,radtype,sort=-1)
!
   select case (radtype)
      case (RX_SOURCE)          ! raggi X
        allocate(elint(nelem), source=real(el%z))
        atint(:) = atom(:)%inte
      case (NEUTRON_SOURCE)     ! neutroni
!corr        allocate(elint(nelem), source=abs(el%fact)) 
        allocate(elint(nelem), source=abs(at_scatt0(el,radtype))) 
        atint(:) = abs(atom(:)%inte)
      case (ELECTRON_SOURCE)    ! elettroni
        allocate(elint(nelem), source=el%zeff) 
        atint(:) = atom(:)%inte
   end select
!
!  Create index of atoms sorted for intensity
   call indexx(atint,ordat)
   ordat = ordat(nat:1:-1)  ! from the larger
   maxinte = atint(ordat(1))
!
   !const = maxinte / elint(1)
   const = elint(1) / maxinte 
!
!  Do not assign H for X-Ray
   nelmin = nelem
   if (el(nelem)%z == H_at .and. radtype == RX_SOURCE) then
       nelmin = nelem - 1
   endif
!
   allocate(zdiff(nelmin),ordiff(nelmin))
   do i=1,nat
      kat = ordat(i)
!
      !zdiff(:) = abs(const*elint(:nelmin) - atint(kat))
      zcal = const*atint(kat)  ! z calculated
      tol = zcal * 2.0
!
!     Find specie corresponding to minimum difference     
!corr      zdiff(:) = abs(zcal - elint(:))
!corr      call indexx(zdiff,ordiff)
!        write(0,'(a,i0,a,*(f10.3))')'AT=',i,' diff=',zdiff
!        write(0,'(a,i0,a,*(f10.3))')'AT=',i,' diff=',el(:nelmin)%nw
      jmin = 0
      kspec = nelmin
      do j=1,nelmin   
         if (el(j)%nw > 0) then   
                !write(0,*)'at=',i,zdiff(j),tol
!corr             if (zdiff(j) < tol) then
             if (abs(zcal - elint(j)) < tol) then
             !write(0,*)kspec,'NW=',el%nw
                 kspec = j 
                 exit
             endif
         endif
      enddo
      if (kspec == 0) kspec = nelmin ! assign lighter specie
      !atom(kat)%ptab = el(kspec)%ptab
      call atom(kat)%set_specie_from_ptab(elem,el(kspec)%ptab)
      el(kspec)%nw = el(kspec)%nw - spg%nsymop*atom(kat)%ocry
             !write(0,*)'NW=',el%nw
   enddo
!
!corr   atom(:)%nz = nz_from_pxen(atom%ptab,elem)
!
   end subroutine make_specie

!----------------------------------------------------------------------------------------------------

   function atoms_label(vatm,llab,showord,maxlen)  result(str)
!
!  Genera nella stringa str una lista di atomi separati da virgola
!
   integer, dimension(:), intent(in)          :: vatm    ! puntatori alle labels
   character(len=*), dimension(:), intent(in) :: llab    ! labels
   logical, intent(in), optional              :: showord ! se vero mostra il numero d'ordine
   integer, intent(in), optional              :: maxlen  ! maximum length of string
   logical                                    :: showord1
   character(len=:), allocatable              :: str
   integer                                    :: i
   integer                                    :: ipos
   integer                                    :: natlm
!
   if (present(showord)) then
       showord1 = showord
   else
       showord1 = .false.
   endif
!
   str = ' '
   natlm = size(vatm)
   if (natlm == 0) return
   ipos = vatm(1)
   if (showord1) then
       str = trim(slabnum(llab(ipos),ipos))
       do i=2,natlm
          ipos = vatm(i)
          if (present(maxlen)) then
              if (len(str) + len_trim(slabnum(llab(ipos),ipos)) + 1 > maxlen) then
                  str = str//' ...'
                  exit
              endif
          endif
          str = str//','//trim(slabnum(llab(ipos),ipos))
       enddo
   else
       str = trim(llab(ipos))
       do i=2,natlm
          ipos = vatm(i)
          if (present(maxlen)) then
              if (len(str) + len_trim(llab(ipos)) + 1 > maxlen) then
                  str = str//' ...'
                  exit
              endif
          endif
          str = str//','//trim(llab(ipos))
       enddo
   endif
!
   end function atoms_label

!----------------------------------------------------------------------------------------------------

   function str_symop(op,spg)
!
!  Convert symmetry operator op_type in a string
!
   USE atom_basic
   USE spginfom
   USE strutil
   type(op_type), intent(in)     :: op
   type(spaceg_type), intent(in) :: spg
   character(len=:), allocatable :: str_symop
!
   str_symop = trim(spg%symopstr(op%op))//' + ('//(i_to_s(op%tra(1)))//','//i_to_s(op%tra(2))//','//i_to_s(op%tra(3))//')'
!
   end function str_symop

!----------------------------------------------------------------------------------------------------

   subroutine get_atom_site(atom,site)
!
!  Extract site info from asymmetric unit
!
   use arrayutil
   use cgeom
   type(atom_type), dimension(:), allocatable, intent(in)       :: atom
   type(container_type), dimension(:), allocatable, intent(out) :: site
   integer                                                      :: natoms,i,nsite,j
   logical, dimension(:), allocatable                           :: siteok
! 
   natoms = numatoms(atom)
   if (natoms == 0) return
! 
   call new_container(site,numatoms(atom))
   nsite = 0
   allocate(siteok(natoms),source=.false.)
   do i=1,natoms
      if (siteok(i)) cycle
      nsite = nsite + 1
      call container_set(site(nsite),i)
      siteok(i) = .true.
      if (atom(i)%och >= 1.0) cycle
      do j=i+1,natoms
         if (siteok(j)) cycle
         if (distanzaC(atom(i)%xc,atom(j)%xc) < 0.01) then
             call container_set(site(nsite),j)
             siteok(j) = .true.
         endif
      enddo
   enddo
   call resize_container(site,nsite)
! 
   end subroutine get_atom_site

!corr   subroutine get_site_info(atom,site)
!corr!
!corr!  Get site info
!corr!
!corr   USE cgeom
!corr   type(atom_type), dimension(:), allocatable, intent(in) :: atom  ! cartesian coord.
!corr   integer, dimension(size(atom),size(atom)), intent(out) :: site
!corr   integer                                                :: nat,i,j
!corr!
!corr   nat = numatoms(atom)
!corr   site(:,:) = 0
!corr   do i=1,nat-1
!corr      if (atom(i)%och /= 1.0) then
!corr          do j=i+1,nat
!corr             if (distanzaC(atom(i)%xc,atom(j)%xc) < 0.01) then
!corr                 site(i,j) = 1
!corr                 site(j,i) = 1
!corr             endif
!corr          enddo
!corr      endif
!corr   enddo
!corr!
!corr   end subroutine get_site_info
!corr
!----------------------------------------------------------------------------------------------------

   integer function get_charge_el(atom,elem)  result(charge)
!
!  Get charge using array elem. See also atomic_charge 
!
   USE elements
   type(atom_type), intent(in)                               :: atom
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   integer                                                   :: ks
!
   ks = atom%kscatt()
   if (ks == 0) then
       charge = 0
   else
       charge = elem(ks)%charge
   endif
!
   end function get_charge_el

!--------------------------------------------------------------------------------------------------------

   function s_get_atomlist(atom,vat,nat,sep,vmask,ncopy) result(str)
!
!  Generate sorted list of atom types with separator and mask
!
   USE elements, only: NLEN_LAB
   USE atom_basic
   USE strutil
   type(atom_type), dimension(:), intent(in)          :: atom
   integer, dimension(:), intent(in)                  :: vat
   integer, intent(in)                                :: nat
   character(len=*), intent(in), optional             :: sep
   logical, dimension(:), intent(in), optional        :: vmask ! must have the same size of atom
   integer, intent(in), optional                      :: ncopy
   character(len=NLEN_LAB), dimension(:), allocatable :: svet
   character(len=:), allocatable                      :: str
   integer                                            :: na,i
   integer, dimension(:), allocatable                 :: ncp
!
   str = ' '
   if (nat == 0) return
   if (present(vmask)) then
       na = count(vmask)
       if (na == 0) return
       allocate(svet(na))
       svet = atom(pack(vat,mask=vmask))%spec()
   else
       na = nat
       allocate(svet(na))
       svet(:) = atom(vat)%spec()
   endif
   if (na > 1) then
       call svec_sort_heap_a(na,svet)
       if (present(ncopy)) then
           if (ncopy > 1 .and. na > 1) then
               allocate(ncp(na))
               call s_find_duplicate(svet,ncp)
               call s_delete_copies(svet,na,ncp,ncopy)
               do i=1,na
                  if (ncp(i) > ncopy) then
                      svet(i) = trim(svet(i))//i_to_s(ncp(i))
                  endif
               enddo
           endif
       endif
       if (na > 1) then
           if (present(sep)) then
               str = cat_svet(svet,na,sep)
           else
               str = cat_svet(svet,na)
           endif
       else
           str = trim(svet(1))
       endif
   else
       str = trim(svet(1))
   endif
!
   end function s_get_atomlist

!----------------------------------------------------------------------------------------------------

   subroutine detect_spg(atom,cell)
   USE unit_cell
   USE symm_table
   USE spginfom
   USE cgeom
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(cell_type), intent(in)                            :: cell
   type(spaceg_type)                                      :: spg
   integer                                                :: i,is,ia,iat,nat,nsela
   type(atom_type)                                        :: at
   real :: dmin = 0.5
   logical, dimension(:), allocatable :: at_selected
   integer :: nat_selected
   real :: dd
   real, dimension(3) :: ktra
   integer :: csys
   logical :: spfound
!
   nat = numatoms(atom)
   if (numatoms(atom) == 0) return
   allocate(at_selected(nat))
   nat_selected = 0
   csys = csys_from_cell(cell%get_par())
!
   loop_spg: do i=2,SG_NUMBER
   !loop_spg: do i=1,1
      at_selected = .false.
      nat_selected = 0
      spg = init_spaceg_type(sg_info(i)%hall)
      !spg = init_spaceg_type('-P 2ybc')
      !spg = init_spaceg_type('P 21/m')
! check if compatible with cell
      !if (csys /= spg%csys_code) cycle
      if (.not.spg_check_cell(spg,cell%get_par())) cycle
!          if (sg_info(i)%num /= 14) cycle
      write(71,*)'====================TEST spg:'//trim(spg%symbol_xhm)
      do ia=1,nat
         nsela = 0
         if (.not.at_selected(ia)) then
             do is=2,spg%nsymop
                at = atom(ia)
                call apply_sym_oper(at,spg%symop(is))
                do iat=1,nat
                   if (.not.at_selected(iat)) then
                       !dd = distanzaC(xc,atomc(iat)%xc,cell%get_g())
                       call xdisteqs(at%xc,atom(iat)%xc,cell%get_g(),dd,ktra)
                       if (dd < dmin) then
                           nat_selected = nat_selected + 1
                           at_selected(iat) = .true.
                           nsela = nsela + 1
                           write(71,'(i0,a,i0,1x,i0,a,i0,a,i0,a,f0.3)')ia,' MATCH WITH ', &
                           iat,nat_selected,'/',nat,' op=',is,' dd=',dd
                                 !write(71,*)'AT=',ia,at%xc
                                 !write(71,*)'AT=',ia,atom(iat)%xc
                       endif
                   endif
                enddo
             enddo  
         endif
         spfound = .false.
         if (nsela == spg%nsymop -1) then
             if (ia == nat) then
                 spfound = .true.
             else
                 spfound = all(at_selected(ia+1:))
             endif
         else
             if (nsela == 0) then
                 spfound = (nat == ia)
             else
                 cycle loop_spg
             endif
         endif
         if (spfound) then
             write(71,*)'Spg '//trim(spg%symbol_xhm)//' seems correct!'
             write(71,*)'AT IN asym. unit.',count(.not.at_selected),nat_selected,nat,ia
             write(0,*)'Spg '//trim(spg%symbol_xhm)//' seems correct!'
             write(0,*)'AT IN asym. unit.',count(.not.at_selected)
             cycle loop_spg
         endif
!corr         if (nsela /= spg%nsymop -1) then
!corr                 write(71,*)'FINIAA=',ia,nsela,spg%nsymop,nat_selected,nat,nat-ia
!corr             if (nsela == 0 .and. nat_selected == nat-ia+1) then
!corr                 write(71,*)'Spg '//trim(spg%symbol_xhm)//' seems correct!'
!corr                 write(71,*)'AT IN asym. unit.',count(.not.at_selected),nat_selected,nat,ia
!corr                 write(0,*)'Spg '//trim(spg%symbol_xhm)//' seems correct!'
!corr                 write(0,*)'AT IN asym. unit.',count(.not.at_selected)
!corr                 cycle loop_spg
!corr             elseif (nsela == 0 .and. at_selected(ia)) then
!corr                 cycle
!corr             else
!corr                 write(71,*)'NSELA=',nsela,spg%nsymop,nat_selected,nat,nat-ia
!corr                 cycle loop_spg
!corr             endif
!corr         endif
      enddo
   enddo loop_spg
!
   end subroutine detect_spg

!----------------------------------------------------------------------------------------------------

   subroutine transform_coordinates(atom,qmat,qvet)
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   real, dimension(3,3), intent(in)                          :: qmat
   real, dimension(3), intent(in)                            :: qvet
   integer                                                   :: i
   real, dimension(3,3)                                      :: bmat
!
   do i=1,numatoms(atom)
      atom(i)%xc = matmul(qmat,atom(i)%xc) + qvet(:)
      if (atom(i)%bij(1) > 0) then
          bmat = atom(i)%get_bmat()                         !                 T
          bmat = matmul(matmul(qmat,bmat),transpose(qmat))  ! bmat' = Q bmat Q
          call atom(i)%set_bmat(bmat)
      endif
   enddo
!
   end subroutine transform_coordinates 

!----------------------------------------------------------------------------------------------------

   integer function get_maxlen_labels(atom) result(maxl)
   type(atom_type), dimension(:), intent(in) :: atom
   integer                                   :: i,lent

   maxl = len_trim(atom(1)%lab)
   do i=2,size(atom)
      lent = len_trim(atom(i)%lab)
      if (lent > maxl) maxl = lent
   enddo
   
   end function get_maxlen_labels

END MODULE atom_type_util   
