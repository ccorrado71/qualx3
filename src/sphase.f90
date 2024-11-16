MODULE crystal_phase
   USE reflection_type_util, only: reflection_type, gref_type
   USE atom_basic, only: atom_type
   USE connect_mod, only: bond_type
   USE arrayutil, only: container_type
   USE constraints, only: constraint_type, CONSTR_ADP_NONE
   USE rigid_body, only: rigid_body_type
   USE rrestr, only: restraint_type, TARG_BOND_TAB, WRES_DEF
   USE fragmentmod, only: fragment_type
   USE rotationmod, only: rotation_type
   USE model_util, only: model_type
   USE bond_valence, only: bond_valence_t
   USE anti_bump, only: bump_settings
   USE profile_function, only: profinfo_type
   use fastfcal, only: sfactor_type
   use mapinfo, only: map_info_type
   USE spginfom
   USE unit_cell
   USE elements
   USE plotstyle
   USE type_constants, only: DP

   implicit none

   type crystal_phase_t
     type(atom_type), dimension(:), allocatable          :: at
     type(bond_type), dimension(:), allocatable          :: bond
     type(spaceg_type)                                   :: spg
     type(cell_type)                                     :: cell
     type(element_type), dimension(:), allocatable       :: elem
     character(len=300)                                  :: strelem = ' '
     type(restraint_type), dimension(:), allocatable     :: res
     type(restraint_type), dimension(:), allocatable     :: abres
     type(restraint_type), dimension(:), allocatable     :: res_list
     real(DP), dimension(3)                              :: wres = WRES_DEF
     real                                                :: chi2res=0.,chi2resa=0.,chi2resd=0.,chi2resbv=0.
     integer                                             :: res_target_type = TARG_BOND_TAB
     type(bump_settings)                                 :: abset
     type(constraint_type), dimension(:), allocatable    :: constr
     type(constraint_type), dimension(:), allocatable    :: sconstr
     type(rigid_body_type), dimension(:), allocatable    :: rigidb
     logical                                             :: riding_model = .true.
     real                                                :: adp_factor = 1.2
     integer                                             :: constr_adp_type = CONSTR_ADP_NONE
     type(reflection_type), dimension(:), allocatable    :: ref    ! used reflections
     type(reflection_type), dimension(:), allocatable    :: refc   ! entire set of reflections
     type(gref_type), dimension(:), allocatable          :: gref   ! reflection groups
     character(len=300)                                  :: cr_name = ' '
     real                                                :: scal = 1.0, ssd = 0.0
     integer                                             :: scode = 0
!corr     integer                                             :: kopcorr = 0
     integer, dimension(3)                               :: hklpo = [0,0,1]
     integer                                             :: gcode = 0
     real                                                :: gpar = 1.0, gsd = 0.0
     real, dimension(:), allocatable                     :: yc
     type(atom_type), dimension(:), allocatable, private :: atsym
     type(container_type), dimension(:,:,:), allocatable :: grid3d
     integer, dimension(:,:), allocatable                :: grid_bnd
     integer, dimension(3), private                      :: sizegrid

     real                                                :: wt = 1.0           ! weight fraction
     real                                                :: wtsd = 0.0         ! sd on weight fraction
     real                                                :: wtn = 1.0          ! normalized weight fraction
     real                                                :: wtnsd = 0.0        ! sd on normalized weight fraction
     real                                                :: dens               ! density
     logical                                             :: brindley = .false. ! apply Brindley model
     real                                                :: p_size = 0.0       ! particle size (micromiters)
     integer                                             :: br_class = 0       ! Brindley classification of powder
     real                                                :: tau = 1.0          ! Brindley correcttion factor
     logical, private                                    :: is_std = .false.   ! crystal phase is standard
     real                                                :: ws = 0.0           ! weight fraction of standard phase
     logical, private                                    :: is_wtpub = .false. ! is weight fraction published
     real                                                :: wtrue = 0.0        ! published weight fraction

     integer                                             :: absf = 0           ! absorption function
     real, dimension(2)                                  :: abspar = 0.0       ! refinable absorption coefficients
     real, dimension(2)                                  :: absesd = 0.0       ! esd on absorption coefficients
     integer, dimension(2)                               :: abscode = 0        ! refinement codes for coefficients

     logical                                             :: is_extraction = .true. ! is extraction enabled?

     type(atom_type), dimension(:), allocatable          :: atpub
     logical, private                                    :: knw = .false.
     real                                                :: dmean = 0.0
     real                                                :: rmsd = 0.0
     integer                                             :: natfound = 0

     integer, private                                    :: doc_info = 0
     integer, dimension(:), allocatable                  :: vetdoc

     type(container_type), dimension(:), allocatable     :: conn
     type(fragment_type), dimension(:), allocatable      :: frag
     type(rotation_type), dimension(:), allocatable      :: rot
     type(bond_valence_t), dimension(:,:), allocatable   :: bvpar
     type(map_info_type)                                 :: mapinfo

     type(model_type), dimension(:), allocatable         :: model
     type(atom_type), dimension(:), allocatable          :: atm0
     type(atom_type), dimension(:), allocatable          :: atmc

     type(profinfo_type), dimension(:,:), allocatable    :: profi

     type(sfactor_type), dimension(:), allocatable       :: sfact,sfactc
     integer, dimension(:), allocatable                  :: kpscat

     logical, private                                    :: has_cell = .false.
     logical, private                                    :: has_spg = .false.
     logical, private                                    :: conn_updated = .false.
     logical                                             :: at_refined = .false.
     logical                                             :: is_symm_user = .false.

     type(style_type)                                    :: style = style_type(1,7,0,1,1,1,3,8,1)
   contains

     ! Data retrieval methods
     procedure, private :: get_cellcod_s
     procedure, private :: get_cellcod_v
     generic            :: get_cellcod => get_cellcod_s, get_cellcod_v
     procedure, private :: get_cellpar_s
     procedure, private :: get_cellpar_v
     generic            :: get_cellpar => get_cellpar_s, get_cellpar_v
     procedure          :: get_cellsd
     procedure          :: get_reflections         
     procedure          :: get_atoms         
     procedure          :: get_bonds         
     procedure          :: get_scattering         
     procedure          :: get_strelem         
     procedure          :: natoms => get_number_of_atoms
     procedure          :: nbonds => get_number_of_bonds
     procedure          :: numelem => get_number_of_elements
     procedure          :: numres => get_number_of_restraints
     procedure          :: numresbv => get_number_of_restraints_bv
     procedure          :: numresab => get_number_of_restraints_ab
     procedure          :: numref => get_number_of_reflections
     procedure          :: numfrag => get_number_of_molecules
     procedure          :: numconstr => get_number_of_constraints
     procedure          :: nrigid => get_number_of_rigidb
     procedure          :: natpub => get_number_of_pub_atoms
     procedure          :: idof => number_of_internal_dof
     procedure          :: edof => number_of_external_dof
     procedure          :: numrot => get_number_of_rotations
     procedure          :: nmodels => get_number_of_models
     procedure          :: has_symmetry => is_symmetry_defined
     procedure          :: is_cell => is_cell_defined
     procedure          :: is_spg => is_spg_defined
     procedure          :: is_known
     procedure          :: doc_active
     procedure          :: doc_active_all
     procedure          :: refine_cell
     procedure          :: is_standard
     procedure          :: is_wt_known
     procedure          :: is_empty => is_empty_phase
     procedure          :: is_po_active

     ! Data modification methods
     procedure          :: set => set_crystal
     procedure          :: set_name
     procedure          :: set_atoms
     procedure          :: set_bonds
     procedure, private :: set_spg_t
     procedure, private :: set_spg_s
     generic            :: set_spg => set_spg_t, set_spg_s
     procedure          :: set_unit_cell
     procedure, private :: set_doc_all
     procedure, private :: set_doc_at
     procedure, private :: set_doc_vet
     generic            :: set_doc => set_doc_all, set_doc_at, set_doc_vet
     procedure          :: set_symmetry
     procedure          :: change_symmetry
     procedure          :: set_reflections
     procedure, private :: set_scattering_elem
     procedure, private :: set_scattering_formula
     generic            :: set_scattering => set_scattering_formula, set_scattering_elem
!corr     procedure          :: update_scatterers_wave
!corr     procedure, private :: set_radtype
     procedure          :: set_scatterers_radiation
     procedure          :: set_strelem
     procedure          :: set_cellcod
     procedure          :: set_cell
     procedure          :: set_cellsd
     procedure          :: set_cellsdref
     procedure          :: norm_cell
     procedure          :: set_refine_cell
     procedure          :: set_refine_sf
     procedure          :: add_atoms
     procedure          :: clear_restraints
     procedure          :: clear_constraints
     procedure          :: set_density
     procedure          :: set_standard
     procedure          :: set_wtpub

     ! Utilities
     procedure, private :: make_reflections_tthrange
     procedure, private :: make_reflections_d
     generic            :: make_reflections => make_reflections_tthrange, make_reflections_d
     procedure          :: save_ref
     procedure          :: restore_ref
     procedure          :: set_used_ref
     procedure          :: calc_2theta
     procedure          :: LPcorr
     procedure          :: ABScorr
     procedure          :: sfactor
     procedure          :: sfactor_anis
     procedure          :: init_conn                                 ! build conn from bonds
     procedure          :: make_fragments                            ! identify molecular fragments
     procedure          :: make_rotations                            ! identify free rotations
     procedure          :: set_as_known                              ! published structure
     procedure          :: load_bvparam                              ! load bond valence parameters
     procedure          :: make_symmetry_cell                        ! generate symmetry equivalent atoms in cell
     procedure          :: allocate_grid                             ! generate grid for atom searching
     procedure, private :: fill_grid_all
     procedure, private :: fill_grid_vet
     generic            :: fill_grid => fill_grid_all, fill_grid_vet ! fill grid
     procedure, private :: compute_doc_all
     procedure, private :: compute_doc_vet
     generic            :: compute_doc => compute_doc_all, compute_doc_vet ! compute doc
     procedure          :: compute_bvs                                     ! compute bond valence sum
     procedure          :: bvs_cutoff                                      ! cutoff for bvs computing
     procedure          :: coord_number                                    ! coordination number
     procedure          :: valence_atom_info                               ! info about valence
     procedure          :: gii_index                                       ! global instability index
     procedure          :: abump_function                                  ! compute anti-bump function
     procedure          :: density => compute_density
     procedure          :: make_occupancy
     procedure          :: standard_setting
     procedure          :: transform_setting

     ! Output
     procedure :: print => print_crystal
     procedure :: print_bonds
     procedure :: print_molecules
     procedure :: print_doc
     procedure :: print_bvparam
     procedure :: cr_save => save_crystal_bin_s
     procedure :: cr_read => read_crystal_bin_s
     procedure :: print_grid3d

     ! Private procedure
     procedure, private :: set_doc_info        ! set variable doc_info
     procedure, private :: get_doc_atoms       ! set vetdoc
   end type crystal_phase_t

   CONTAINS
  
   subroutine set_name(crystal,sname)
   USE atom_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   character(len=*), intent(in)          :: sname
   crystal%cr_name = sname
   end subroutine set_name

!----------------------------------------------------------------------------------------------

   subroutine set_atoms(crystal,at)
   USE atom_type_util
   class(crystal_phase_t), intent(inout)             :: crystal
   type(atom_type), dimension(:), allocatable, optional :: at
   call copy_atoms(crystal%at,at)
   end subroutine set_atoms

 !----------------------------------------------------------------------------------------------

   subroutine add_atoms(crystal,at,bond)
   USE atom_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   type(atom_type), dimension(:), allocatable :: at
   type(bond_type), dimension(:), allocatable :: bond
   integer                                    :: nat
!
   call add_atoms_to_list(crystal%at,at,nat,crystal%bond,bond)
!
   end subroutine add_atoms
  
!----------------------------------------------------------------------------------------------

   subroutine clear_restraints(crystal)
   USE rrestr
   class(crystal_phase_t), intent(inout) :: crystal
   call reset_restraints(crystal%res,crystal%wres)
   call reset_restraints(crystal%abres,crystal%wres)
   end subroutine clear_restraints

!----------------------------------------------------------------------------------------------

   subroutine clear_constraints(crystal)
   USE constraints
   class(crystal_phase_t), intent(inout) :: crystal
   call reset_constraints(crystal%constr)
   end subroutine clear_constraints

!----------------------------------------------------------------------------------------------

   subroutine set_bonds(crystal,bonds)
   USE connect_mod
   class(crystal_phase_t), intent(inout)   :: crystal
   type(bond_type), dimension(:), allocatable :: bonds
!
   call copy_bonds(crystal%bond,bonds)
   crystal%conn_updated = .false.
!
   end subroutine set_bonds
   
!----------------------------------------------------------------------------------------------

   subroutine set_spg_t(crystal,spg)
   class(crystal_phase_t), intent(inout) :: crystal
   type(spaceg_type), intent(in)            :: spg
!
   if (spg%undef() .and. spg%num == 0) return
   crystal%spg = spg
   crystal%has_spg = .true.
   if (crystal%has_cell) call crystal%cell%set_refine(crystal%spg,0)
!
   end subroutine set_spg_t

!----------------------------------------------------------------------------------------------

   subroutine set_spg_s(crystal,str,err)
   USE errormod
   class(crystal_phase_t), intent(inout) :: crystal
   character(len=*), intent(in)             :: str
   type(error_type)                         :: err
   type(spaceg_type)                        :: spg
!
   spg = init_spaceg_type(str)
   if (spg%undef()) then
       call err%set('Error reading space group: '//trim(str))
       return
   endif
   call crystal%set_spg_t(spg)
!
   end subroutine set_spg_s
  
!----------------------------------------------------------------------------------------------

   subroutine set_unit_cell(crystal,cell)
   class(crystal_phase_t), intent(inout) :: crystal
   type(cell_type), intent(in)              :: cell
!
   crystal%cell = cell
   crystal%has_cell = .true.
   if (crystal%has_spg) call crystal%cell%set_refine(crystal%spg,0)
!
   end subroutine set_unit_cell

!----------------------------------------------------------------------------------------------

   subroutine set_doc_all(crystal,val)
!
!  Set DOC for all atoms
!
   class(crystal_phase_t), intent(inout) :: crystal
   logical, intent(in), optional         :: val
   logical                               :: vald
   if (present(val)) then
       vald = val
   else
       vald = .true.
   endif
   crystal%at%doc = vald
   if (vald) then
       crystal%doc_info = 1
   else
       crystal%doc_info = 0
   endif
   call crystal%get_doc_atoms()
   
   end subroutine set_doc_all

!----------------------------------------------------------------------------------------------

   subroutine set_doc_at(crystal,kat,val)
!
!  Set DOC for a specific atom
!
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                      :: kat
   logical, intent(in), optional            :: val
   if (present(val)) then
       crystal%at(kat)%doc = val
   else
       crystal%at(kat)%doc = .true.
   endif
   call crystal%set_doc_info()
   end subroutine set_doc_at

!----------------------------------------------------------------------------------------------

   subroutine set_doc_vet(crystal,vet,val)
!
!  Set DOC for atoms in vet
!
   class(crystal_phase_t), intent(inout) :: crystal
   integer, dimension(:), intent(in)        :: vet
   logical, intent(in), optional            :: val
   integer                                  :: i
   if (present(val)) then
       do i=1,size(vet)
          call crystal%set_doc_at(vet(i),val)
       enddo
   else
       do i=1,size(vet)
          call crystal%set_doc_at(vet(i),.true.)
       enddo
   endif
   end subroutine set_doc_vet

!----------------------------------------------------------------------------------------------

   subroutine set_doc_info(crystal)
   USE arrayutil
   class(crystal_phase_t), intent(inout) :: crystal
   integer                               :: natdoc
!
   call crystal%get_doc_atoms()
   natdoc = size_array(crystal%vetdoc)
   if (natdoc == 0) then
       crystal%doc_info = 0
   elseif (natdoc == crystal%natoms()) then
       crystal%doc_info = 1
   else 
       crystal%doc_info = 2
   endif
!
   end subroutine set_doc_info

!----------------------------------------------------------------------------------------------

   subroutine get_doc_atoms(crystal)
   USE arrayutil
   class(crystal_phase_t), intent(inout) :: crystal
   integer                               :: natdoc
   integer                               :: i
   natdoc = count(crystal%at%doc)
   call new_array(crystal%vetdoc,natdoc)
   if (natdoc > 0) crystal%vetdoc(:) = pack([(i,i=1,crystal%natoms())],mask=crystal%at%doc)
   end subroutine get_doc_atoms

!----------------------------------------------------------------------------------------------

   logical function doc_active(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   doc_active = crystal%doc_info /= 0
   end function doc_active

!----------------------------------------------------------------------------------------------

   logical function doc_active_all(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   doc_active_all = crystal%doc_info == 1
   end function doc_active_all

!----------------------------------------------------------------------------------------------

   subroutine get_reflections(crystal,ref)
   USE reflection_type_util
   class(crystal_phase_t), intent(in)                           :: crystal
   type(reflection_type), dimension(:), allocatable, intent(inout) :: ref
!
   call copy_ref(ref,crystal%ref)
!
   end subroutine get_reflections

!----------------------------------------------------------------------------------------------

   subroutine get_atoms(crystal,at)
   USE atom_type_util
   class(crystal_phase_t), intent(in)                     :: crystal
   type(atom_type), dimension(:), allocatable, intent(inout) :: at
!
   call copy_atoms(at,crystal%at)
!
   end subroutine get_atoms

!----------------------------------------------------------------------------------------------

   subroutine get_bonds(crystal,bond)
   USE connect_mod
   class(crystal_phase_t), intent(in)                     :: crystal
   type(bond_type), dimension(:), allocatable, intent(inout) :: bond
!
   call copy_bonds(bond,crystal%bond)
!
   end subroutine get_bonds

!----------------------------------------------------------------------------------------------

   subroutine get_scattering(crystal,elem)
   class(crystal_phase_t), intent(in)                        :: crystal
   type(element_type), dimension(:), allocatable, intent(inout) :: elem
!
   call copy_elem(elem,crystal%elem)
!
   end subroutine get_scattering

!----------------------------------------------------------------------------------------------

   function get_strelem(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   character(len=:), allocatable      :: get_strelem
!
   get_strelem = trim(crystal%strelem)
!
   end function get_strelem 

!----------------------------------------------------------------------------------------------

   integer function get_number_of_atoms(crystal)
   USE atom_type_util
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_atoms = numatoms(crystal%at)
   end function get_number_of_atoms

!----------------------------------------------------------------------------------------------

   integer function get_number_of_pub_atoms(crystal)
   USE atom_type_util
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_pub_atoms = numatoms(crystal%atpub)
   end function get_number_of_pub_atoms

!----------------------------------------------------------------------------------------------

   integer function get_number_of_bonds(crystal)
   USE connect_mod
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_bonds = numbonds(crystal%bond)
   end function get_number_of_bonds

!----------------------------------------------------------------------------------------------

   integer function get_number_of_elements(crystal)
   USE elements
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_elements = numelem(crystal%elem)
   end function get_number_of_elements

!----------------------------------------------------------------------------------------------

   integer function get_number_of_restraints(crystal,code)
   USE rrestr
   class(crystal_phase_t), intent(in) :: crystal
   integer, intent(in), optional      :: code
   if (present(code)) then
       get_number_of_restraints = nrestraints(crystal%res,code)
   else
       get_number_of_restraints = nrestraints(crystal%res)
   endif
   end function get_number_of_restraints

!----------------------------------------------------------------------------------------------

   integer function get_number_of_restraints_bv(crystal)
   USE rrestr
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_restraints_bv = nrestraints(crystal%res,RESBV)
   end function get_number_of_restraints_bv

!----------------------------------------------------------------------------------------------

   elemental integer function get_number_of_restraints_ab(crystal)
   USE rrestr
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_restraints_ab = nrestraints(crystal%abres)
   end function get_number_of_restraints_ab

!----------------------------------------------------------------------------------------------

   elemental integer function get_number_of_reflections(crystal)
   USE reflection_type_util
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_reflections = numrefl(crystal%ref)
   end function get_number_of_reflections

!----------------------------------------------------------------------------------------------

   integer function get_number_of_molecules(crystal)
   USE fragmentmod
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_molecules = numfragments(crystal%frag)
   end function get_number_of_molecules 

!----------------------------------------------------------------------------------------------

   integer function get_number_of_constraints(crystal)
   USE constraints
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_constraints = nconstraints(crystal%constr)
   end function get_number_of_constraints

!----------------------------------------------------------------------------------------------

   integer function get_number_of_rigidb(crystal)
   USE rigid_body
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_rigidb = numrigidb(crystal%rigidb)
   end function get_number_of_rigidb

!----------------------------------------------------------------------------------------------

   integer function number_of_external_dof(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   integer                            :: i
   number_of_external_dof = 0
   do i=1,crystal%numfrag()
      number_of_external_dof = number_of_external_dof + count(crystal%frag(i)%rcod > 0)
   enddo
   end function number_of_external_dof 

!----------------------------------------------------------------------------------------------

   integer function number_of_internal_dof(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   if (crystal%numrot() > 0) then
       number_of_internal_dof = count(crystal%rot%rcod > 0)
   else
       number_of_internal_dof = 0
   endif
   end function number_of_internal_dof 

!----------------------------------------------------------------------------------------------

   integer function get_number_of_rotations(crystal)
   USE rotationmod
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_rotations = numrotat(crystal%rot)
   end function get_number_of_rotations 

!----------------------------------------------------------------------------------------------

   integer function get_number_of_models(crystal)
   USE model_util
   class(crystal_phase_t), intent(in) :: crystal
   get_number_of_models = nummodels(crystal%model)
   end function get_number_of_models 
 
!----------------------------------------------------------------------------------------------

   logical function is_symmetry_defined(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   is_symmetry_defined = crystal%has_cell .and. crystal%has_spg
   end function is_symmetry_defined

!----------------------------------------------------------------------------------------------

   logical function is_cell_defined(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   is_cell_defined = crystal%has_cell
   end function is_cell_defined

!----------------------------------------------------------------------------------------------

   logical function is_spg_defined(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   is_spg_defined = crystal%has_spg
   end function is_spg_defined

!----------------------------------------------------------------------------------------------

   integer function get_cellcod_s(crystal,i)
   USE unit_cell
   class(crystal_phase_t), intent(in) :: crystal
   integer, intent(in)                   :: i
!
   get_cellcod_s = crystal%cell%get_cod(i)
!
   end function get_cellcod_s

!----------------------------------------------------------------------------------------------

   function get_cellcod_v(crystal)
   USE unit_cell
   class(crystal_phase_t), intent(in) :: crystal
   integer, dimension(6)                 :: get_cellcod_v 
!
   get_cellcod_v = crystal%cell%get_cod()
!
   end function get_cellcod_v

!----------------------------------------------------------------------------------------------

   real function get_cellpar_s(crystal,i)
   USE unit_cell
   class(crystal_phase_t), intent(in) :: crystal
   integer, intent(in)                   :: i
!
   get_cellpar_s = crystal%cell%get_par(i)
!
   end function get_cellpar_s

!----------------------------------------------------------------------------------------------

   function get_cellpar_v(crystal)
   USE unit_cell
   class(crystal_phase_t), intent(in) :: crystal
   real, dimension(6)                 :: get_cellpar_v 
!
   get_cellpar_v = crystal%cell%get_par()
!
   end function get_cellpar_v

!----------------------------------------------------------------------------------------------

   function get_cellsd(crystal)
   USE unit_cell
   class(crystal_phase_t), intent(in) :: crystal
   real, dimension(6)                    :: get_cellsd 
!
   get_cellsd = crystal%cell%get_sd()
!
   end function get_cellsd

!----------------------------------------------------------------------------------------------

   subroutine set_symmetry(crystal,spg,cell)
   class(crystal_phase_t), intent(inout) :: crystal
   type(spaceg_type), intent(in)            :: spg
   type(cell_type), intent(in)              :: cell
   call crystal%set_spg(spg)
   call crystal%set_unit_cell(cell)
   end subroutine set_symmetry  

!----------------------------------------------------------------------------------------------

   subroutine change_symmetry(crystal,spg,cell)
!
!  Set symmetry and update the coordinates to preserve the internal geometry
!
   USE atom_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   type(spaceg_type), intent(in)            :: spg
   type(cell_type), intent(in)              :: cell
!
   call frac_to_cart(crystal%at,crystal%cell%get_ortom())
   call crystal%set_symmetry(spg,cell)
   call cart_to_frac(crystal%at,crystal%cell%get_ortoi())
!
   end subroutine change_symmetry  

!----------------------------------------------------------------------------------------------

   subroutine set_reflections(crystal,ref)
   USE reflection_type_util
   class(crystal_phase_t), intent(inout)                        :: crystal
   type(reflection_type), dimension(:), allocatable, intent(in) :: ref
!
   call copy_ref(crystal%ref,ref)
!
   end subroutine set_reflections

!----------------------------------------------------------------------------------------------

   subroutine set_scattering_elem(crystal,elem)
   USE elements
   class(crystal_phase_t), intent(inout)                  :: crystal
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   call copy_elem(crystal%elem,elem)
   end subroutine set_scattering_elem

!----------------------------------------------------------------------------------------------

   subroutine set_scattering_formula(crystal,strelem,wave,radtype,ier)
   USE elements
   class(crystal_phase_t), intent(inout)         :: crystal
   character(len=*), intent(in)                  :: strelem
   real, intent(in)                              :: wave
   integer, intent(in)                           :: radtype
   integer, intent(out)                          :: ier
   type(element_type), dimension(:), allocatable :: elem
   call build_scatterers(strelem,wave,radtype,elem,ier)
   if (ier /= 0) return
   call copy_elem(crystal%elem,elem)
   call crystal%set_strelem(strelem)
   !call crystal%set_radtype(radtype)
   call elem_set_radiation(crystal%elem,radtype)
   call ordina_elements(crystal%elem,radtype)
   end subroutine set_scattering_formula

!----------------------------------------------------------------------------------------------
!corr
!corr   subroutine set_radtype(crystal,radtype)
!corr   USE elements
!corr   class(crystal_phase_t), intent(inout)         :: crystal
!corr   integer, intent(in), optional                 :: radtype
!corr   call elem_set_radiation(crystal%elem,radtype)
!corr   call ordina_elements(crystal%elem,radtype)
!corr   end subroutine set_radtype
!corr
!----------------------------------------------------------------------------------------------

   subroutine set_scatterers_radiation(crystal,wave,radtype)
   use errormod
   use elements
   class(crystal_phase_t), intent(inout) :: crystal
   real, intent(in)                      :: wave
   integer, intent(in)                   :: radtype
   type(error_type)                      :: err
!
   call elem_set_nist_factors(crystal%elem,wave,radtype,err)
   call elem_set_radiation(crystal%elem,radtype)
   call ordina_elements(crystal%elem,radtype)
!
   end subroutine set_scatterers_radiation

!----------------------------------------------------------------------------------------------

   subroutine set_strelem(crystal,strelem)
   USE elements
   class(crystal_phase_t), intent(inout) :: crystal
   character(len=*), intent(in)          :: strelem
   crystal%strelem = strelem
   end subroutine set_strelem

!----------------------------------------------------------------------------------------------
!corr
!corr   subroutine update_scatterers_wave(crystal,wave,err)
!corr   use errormod
!corr   class(crystal_phase_t), intent(inout) :: crystal
!corr   real, intent(in)                      :: wave
!corr   type(error_type), intent(out)         :: err
!corr!
!corr   call elem_set_nist_factors(crystal%elem,wave,err)
!corr!
!corr   end subroutine update_scatterers_wave
!corr
!----------------------------------------------------------------------------------------------

   subroutine set_cellcod(crystal,i,val)
   USE unit_cell
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                      :: i,val
   call crystal%cell%set_cod(i,val)
   end subroutine set_cellcod

!----------------------------------------------------------------------------------------------

   subroutine set_cell(crystal,par)
   USE unit_cell
   class(crystal_phase_t), intent(inout) :: crystal
   real, dimension(6), intent(in)        :: par
   call crystal%cell%set(par)
   end subroutine set_cell

!----------------------------------------------------------------------------------------------

   subroutine set_cellsd(crystal,i,val)
   USE unit_cell
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                      :: i
   real, intent(in)                         :: val
   call crystal%cell%set_sd(i,val)
   end subroutine set_cellsd

!----------------------------------------------------------------------------------------------

   subroutine set_cellsdref(crystal,sd)
   USE unit_cell
   class(crystal_phase_t), intent(inout) :: crystal
   real, dimension(6), intent(in)           :: sd
   call crystal%cell%set_sdref(sd,crystal%spg)
   end subroutine set_cellsdref

!----------------------------------------------------------------------------------------------

   subroutine norm_cell(crystal)
   USE unit_cell
   class(crystal_phase_t), intent(inout) :: crystal
   call crystal%cell%norm(crystal%spg)
   end subroutine norm_cell

!----------------------------------------------------------------------------------------------

   subroutine set_refine_cell(crystal,val)
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                      :: val
!
   call crystal%cell%set_refine(crystal%spg,val)
!
   end subroutine set_refine_cell

!----------------------------------------------------------------------------------------------

   subroutine set_refine_sf(crystal,val)
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                   :: val
   integer                               :: i
!
   do i=1,crystal%numref()
      crystal%ref(i)%rcod = val
   enddo
!
   end subroutine set_refine_sf

!----------------------------------------------------------------------------------------------

   subroutine set_ycalc(crystal,ycalc)
   USE arrayutil
   class(crystal_phase_t), intent(inout) :: crystal
   real, dimension(:), allocatable          :: ycalc
   call copy_array(crystal%yc,ycalc)
   end subroutine set_ycalc

!----------------------------------------------------------------------------------------------

   subroutine make_reflections_tthrange(crystal,thmin,thmax,wave)
   USE reflection_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   real, intent(in)                      :: thmin,thmax
   real, dimension(:), intent(in)        :: wave
   integer                               :: nref
   integer, dimension(3)                 :: ihmx
!
   if (.not.crystal%has_cell) return
   if (.not.crystal%has_spg) return
   call create_reflections(thmin,thmax,crystal%cell%get_par(),crystal%spg,wave,crystal%ref,nref,ihmx)
   if (crystal%numref() > 0) call crystal%calc_2theta(wave)
!
   end subroutine make_reflections_tthrange

!----------------------------------------------------------------------------------------------

   subroutine make_reflections_d(crystal,dval)
   !use prog_constants, only: CU_WAVE
   !use counts
   USE reflection_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   real, intent(in)                      :: dval
   real, dimension(1)                    :: wave = -1
   integer                               :: nref
   integer, dimension(3)                 :: ihmx
!
   !call make_reflections_tthrange(crystal,0.0,thvalue(dval,wave(1)),wave)
   !call make_reflections_tthrange(crystal,0.001,thvalue(dval,wave(1)),wave)
   if (.not.crystal%has_cell) return
   if (.not.crystal%has_spg) return
   call create_reflections(dval,crystal%cell%get_par(),crystal%spg,wave,crystal%ref,nref,ihmx)
!
   end subroutine make_reflections_d 

!----------------------------------------------------------------------------------------------

   subroutine save_ref(crystal)
   USE reflection_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   call copy_ref(crystal%refc,crystal%ref)
   end subroutine save_ref

!----------------------------------------------------------------------------------------------

   subroutine restore_ref(crystal)
   USE reflection_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   call copy_ref(crystal%ref,crystal%refc)
   end subroutine restore_ref

!----------------------------------------------------------------------------------------------

   subroutine set_used_ref(crystal,nrefu)
   USE reflection_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                   :: nrefu
   integer                               :: oldsize
   if (crystal%numref() == nrefu) return
   oldsize = crystal%numref()
   call resize_reflections(crystal%ref,nrefu)
   if (nrefu > oldsize) then
       if (numrefl(crystal%refc) > 0) then
           crystal%ref(oldsize+1:nrefu) = crystal%refc(oldsize+1:nrefu)
       endif
   endif
   end subroutine set_used_ref

!----------------------------------------------------------------------------------------------

   subroutine calc_2theta(crystal,wave)
   USE reflection_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   real, dimension(:), intent(in)        :: wave
   call calculate_2theta(crystal%ref,crystal%cell%get_par(),wave)
   end subroutine calc_2theta

!----------------------------------------------------------------------------------------------

   subroutine LPcorr(crystal,radtype,sync,nwave)
   USE reflection_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                   :: radtype,nwave
   logical, intent(in)                   :: sync
   integer                               :: i
   do i=1,nwave
      crystal%ref%lp(i) = lp_correction(crystal%ref%tthd(i),radtype,sync)
   enddo
   end subroutine LPcorr

!----------------------------------------------------------------------------------------------

   subroutine ABScorr(crystal,nwave)
   USE absmod
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                   :: nwave
   integer                               :: i
   do i=1,nwave
      crystal%ref%ab(i) = abs_correction(crystal%ref%tthd(i),crystal%absf,crystal%abspar)
   enddo
   end subroutine ABScorr

!----------------------------------------------------------------------------------------------

   subroutine sfactor(crystal,radtype,anomal)
   USE atom_type_util
   USE reflection_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                   :: radtype
   logical, intent(in)                   :: anomal
!
   if (.not.crystal%has_cell) return
   if (numatoms(crystal%at) == 0) return
   if (numelem(crystal%elem) == 0) return
   if (numrefl(crystal%ref) == 0) return

   call sfcalc(crystal%ref,crystal%at,crystal%spg,crystal%elem,radtype,anomal)
!
   end subroutine sfactor

!----------------------------------------------------------------------------------------------

   subroutine sfactor_anis(crystal,radtype,anomal)
   USE atom_type_util
   USE reflection_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                   :: radtype
   logical, intent(in)                   :: anomal
!
   if (.not.crystal%has_cell) return
   if (numatoms(crystal%at) == 0) return
   if (numelem(crystal%elem) == 0) return
   if (numrefl(crystal%ref) == 0) return

   call sfcalc_anis(crystal%ref,crystal%at,crystal%spg,crystal%elem,radtype,anomal)
!
   end subroutine sfactor_anis

!----------------------------------------------------------------------------------------------

   subroutine init_conn(crystal)
   USE connect_mod
   class(crystal_phase_t), intent(inout) :: crystal
   call bond_to_connect(crystal%natoms(),crystal%bond,crystal%conn)
   crystal%conn_updated = .true.
   end subroutine init_conn 

!----------------------------------------------------------------------------------------------

   subroutine make_fragments(crystal,keep)
   USE fragmentmod
   USE arrayutil
   class(crystal_phase_t), intent(inout) :: crystal
   logical, intent(in), optional         :: keep    ! preserve refinement code
   integer                               :: numfrag,i
   integer, dimension(:,:), allocatable  :: rcod
   integer, dimension(:), allocatable    :: rat
!
   if (.not.crystal%conn_updated) call crystal%init_conn()
   if (present(keep)) then
       if (keep .and. crystal%numfrag() > 0) then
           call new_array(rcod,[1,1],[crystal%numfrag(),6])
           call new_array(rat,crystal%numfrag())
           do i=1,crystal%numfrag()
              rcod(i,:) = crystal%frag(i)%rcod
              rat(i) = crystal%frag(i)%rat
           enddo
       endif
   endif
   call get_fragments_from_leg(crystal%at,crystal%cell,crystal%bond,crystal%conn,numfrag,crystal%frag)
   call frag_set_centre(crystal%frag,crystal%conn)
   if (present(keep)) then
       if (allocated(rcod)) then
           if (size(rcod,1) == crystal%numfrag()) then
               do i=1,crystal%numfrag()
                  crystal%frag(i)%rcod = rcod(i,:)
                  crystal%frag(i)%rat = rat(i)
               enddo
           endif
       endif
   endif
!
   end subroutine make_fragments 

!----------------------------------------------------------------------------------------------

   subroutine make_rotations(crystal,keep)
   USE rotationmod
   USE arrayutil
   class(crystal_phase_t), intent(inout) :: crystal
   logical, intent(in), optional         :: keep
   integer                               :: numrot,i
   integer, dimension(:), allocatable    :: rcod
!
   if (.not.crystal%conn_updated) call crystal%init_conn()
   if (present(keep)) then
       if (keep .and. crystal%numrot() > 0) then
           call new_array(rcod,crystal%numrot())
           do i=1,crystal%numrot()
              rcod(i) = crystal%rot(i)%rcod
           enddo
       endif
   endif
   call get_rotation_from_leg(crystal%at,crystal%conn,crystal%bond,crystal%cell,numrot,crystal%rot,.true.)
   if (present(keep)) then
       if (allocated(rcod)) then
           if (size(rcod) == crystal%numrot()) then
               crystal%rot(:)%rcod = rcod(:)
           endif
       endif
   endif
!
   end subroutine make_rotations 

!----------------------------------------------------------------------------------------------

   subroutine set_as_known(crystal,atpub,radtype,anomal)
   USE atom_type_util
   USE elements
   USE reflection_type_util
   class(crystal_phase_t), intent(inout)            :: crystal
   type(atom_type), dimension(:), allocatable       :: atpub
   integer, intent(in)                              :: radtype
   logical, intent(in)                              :: anomal
   type(reflection_type), dimension(:), allocatable :: ref
   integer                                          :: fase
   integer                                          :: i
!
   if (numatoms(atpub) == 0) return
   call copy_atoms(crystal%atpub,atpub)
!
!  remove H atoms for X-rays
   if (radtype == RX_SOURCE) call remove_atoms_from_list(crystal%atpub,crystal%atpub%ptab,H_at)
!
!  compute structure factors from known structure
   if (crystal%numref() == 0) return
!
   call copy_ref(ref,crystal%ref)
   call fcalcang(ref,atpub,crystal%spg,crystal%elem,radtype,anomal)   ! use atpub that contains H atoms
   do i=1,crystal%numref()
      crystal%ref(i)%fv=ref(i)%fc
      fase=mod(ref(i)%ph,360)
      if(fase <= 0)fase=fase+360
      crystal%ref(i)%phv=fase
!corr      crystal%ref(i)%jcode=crystal%ref(i)%jcode + fase*32
   enddo
!
   crystal%knw = .true.
!
   end subroutine set_as_known

!----------------------------------------------------------------------------------------------

   subroutine load_bvparam(crystal)
   USE bond_valence
   class(crystal_phase_t), intent(inout) :: crystal
   if (crystal%numelem() == 0) return
   if (allocated(crystal%bvpar)) then
       if (size(crystal%bvpar,1) /= crystal%numelem()) then
           deallocate(crystal%bvpar)
           allocate(crystal%bvpar(crystal%numelem(),crystal%numelem()))
           call bv_compute_table(crystal%elem,crystal%bvpar)
           call bvtable_check(crystal%elem,crystal%bvpar)
       endif
   else
       allocate(crystal%bvpar(crystal%numelem(),crystal%numelem()))
       call bv_compute_table(crystal%elem,crystal%bvpar)
       call bvtable_check(crystal%elem,crystal%bvpar)
   endif
   end subroutine load_bvparam

!----------------------------------------------------------------------------------------------

   subroutine make_symmetry_cell(crystal)
   USE atom_type_util
   USE spginfom
   USE arrayutil
   class(crystal_phase_t), intent(inout)     :: crystal
   integer                                   :: i,j,natcr
   type(atom_type)                           :: atmp
   real, dimension(3)                        :: xtra
!
   call new_atoms(crystal%atsym,crystal%spg%nsymop*crystal%natoms())
! 
   natcr = 0
   do j=1,crystal%spg%nsymop
      do i=1,crystal%natoms()
         atmp = crystal%at(i)
         call apply_sym_oper(atmp,crystal%spg%symop(j))
         call translate_in_cell(atmp,xtra)
         atmp%op = op_type(j,nint(xtra))
         atmp%asym = i
         natcr = natcr + 1
         crystal%atsym(natcr) = atmp
         !write(70,'(a,1x,3f10.3,4i4,3f10.2)')atmp%specie(),atmp%xc
      enddo
   enddo
!
   end subroutine make_symmetry_cell

!----------------------------------------------------------------------------------------------

   subroutine allocate_grid(crystal,cutoff)
   USE symmgrid
   class(crystal_phase_t), intent(inout) :: crystal
   real, intent(in)                      :: cutoff
   call init_grid(crystal%grid3d,crystal%grid_bnd,crystal%cell,cutoff,1,crystal%sizegrid)
   end subroutine allocate_grid

!----------------------------------------------------------------------------------------------

   subroutine print_grid3d(crystal,kpr)
   USE symmgrid
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                   :: kpr
   call print_grid(crystal%atsym,crystal%grid3d,kpr)
   end subroutine print_grid3d

!----------------------------------------------------------------------------------------------

   subroutine fill_grid_all(crystal)
   USE symmgrid
   class(crystal_phase_t), intent(inout) :: crystal
   call fillgrid(crystal%atsym,crystal%grid3d,crystal%sizegrid)
   end subroutine fill_grid_all

!----------------------------------------------------------------------------------------------

   subroutine fill_grid_vet(crystal,vet)
   USE symmgrid
   class(crystal_phase_t), intent(inout) :: crystal
   integer, dimension(:), intent(in)     :: vet
   call fillgrid_vet(crystal%atsym,crystal%grid3d,crystal%sizegrid,vet)
   end subroutine fill_grid_vet

!----------------------------------------------------------------------------------------------

   subroutine compute_doc_all(crystal,distmin)
   USE symmgrid
   class(crystal_phase_t), intent(inout) :: crystal
   real, intent(in)                      :: distmin
   call compute_doc_grid_all(crystal%atsym,crystal%grid3d,crystal%grid_bnd,crystal%sizegrid,   &
                              crystal%at,crystal%cell,distmin)
   end subroutine compute_doc_all

!----------------------------------------------------------------------------------------------

   subroutine compute_doc_vet(crystal,distmin,vet)
   USE symmgrid
   class(crystal_phase_t), intent(inout) :: crystal
   real, intent(in)                      :: distmin
   integer, dimension(:), intent(in)     :: vet
   call compute_doc_grid_vet(crystal%atsym,crystal%grid3d,crystal%grid_bnd,crystal%sizegrid,   &
                              crystal%at,crystal%cell,distmin,vet)
   end subroutine compute_doc_vet

!----------------------------------------------------------------------------------------------

   subroutine compute_bvs(crystal,kat,val)
   USE symmgrid
   class(crystal_phase_t), intent(in) :: crystal
   integer, intent(in)                :: kat
   real, intent(out)                  :: val
   !call print_grid_bd(crystal%atsym,crystal%grid3d,crystal%grid_bnd,71)
   call compute_bvs_grid(crystal%atsym,crystal%grid3d,crystal%grid_bnd,crystal%sizegrid, &
                                   kat,crystal%at,crystal%cell,crystal%bvpar,val)
   end subroutine compute_bvs

!----------------------------------------------------------------------------------------------

   real function bvs_cutoff(crystal)
   use bond_valence
   class(crystal_phase_t), intent(in) :: crystal
   if (allocated(crystal%bvpar)) then
       bvs_cutoff = bv_cutoff(crystal%bvpar)
   else
       bvs_cutoff = 3.0
   endif
   end function bvs_cutoff

!----------------------------------------------------------------------------------------------

   integer function coord_number(crystal,kat)
   USE symmgrid
   class(crystal_phase_t), intent(in) :: crystal
   integer, intent(in)                :: kat
   coord_number = bvalence_atom(crystal%atsym,crystal%grid3d,crystal%grid_bnd,crystal%sizegrid, &
                                   kat,crystal%at,crystal%cell,crystal%bvpar)
   end function coord_number

!----------------------------------------------------------------------------------------------

   subroutine valence_atom_info(crystal,kat,dinfo,bvs,cn)
   USE symmgrid
   class(crystal_phase_t), intent(in)                          :: crystal
   integer, intent(in)                                         :: kat
   type(bond_info_t), dimension(:), allocatable, intent(inout) :: dinfo
   !type(bond_info_t), dimension(:), intent(out) :: dinfo
   real, intent(out)                                           :: bvs
   integer, intent(out)                                        :: cn
   call bvalence_atom_info(crystal%atsym,crystal%grid3d,crystal%grid_bnd,crystal%sizegrid, &
                                   kat,crystal%at,crystal%cell,crystal%bvpar,dinfo,bvs)
   !cn = count(dinfo%dupl == 0)
   cn = size(dinfo)
   end subroutine valence_atom_info

!----------------------------------------------------------------------------------------------

   real function gii_index(crystal,bvs) result(gii)
   USE nrutil, only: vabs
   USE elements
   USE atom_type_util
   class(crystal_phase_t), intent(in) :: crystal
   real, dimension(:), intent(in)     :: bvs
   integer, dimension(:), allocatable :: charge
   integer                            :: i
!   
   gii = 0
   if (crystal%natoms() == 0) return
!
   allocate(charge(crystal%natoms()))
   do i=1,crystal%natoms()
      charge(i) = get_charge_el(crystal%at(i),crystal%elem)
      if (charge(i) == 0 .and. crystal%at(i)%kscatt() /= 0) then ! atom is not a ghost
          charge(i) = oxidation_number(crystal%at(i)%z())        ! force charge when undefined
      endif
   enddo
   !gii = sqrt(vabs(abs(charge) - bvs) / crystal%natoms())
   gii = vabs(abs(charge) - bvs) / sqrt(real(crystal%natoms()))
!
   end function gii_index

!----------------------------------------------------------------------------------------------

   real function abump_function(crystal)
   USE symmgrid
   class(crystal_phase_t), intent(inout) :: crystal
   abump_function = antibump_function_grid(crystal%atsym,crystal%grid3d,crystal%grid_bnd,crystal%sizegrid,  &
                                            crystal%abres,crystal%at,crystal%cell,.false.)
   end function abump_function

!----------------------------------------------------------------------------------------------

   logical elemental function is_known(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   is_known = crystal%knw  
   end function  is_known

!----------------------------------------------------------------------------------------------

   subroutine set_crystal(crystal,at,bonds,pname,spg,cell,elem)
   USE atom_type_util
   USE connect_mod
   USE elements
   class(crystal_phase_t), intent(inout)                            :: crystal
   type(atom_type), dimension(:), allocatable, optional                :: at
   type(bond_type), dimension(:), allocatable, optional                :: bonds
   type(spaceg_type), intent(in), optional                             :: spg
   type(cell_type), intent(in), optional                               :: cell
   type(element_type), dimension(:), allocatable, intent(in), optional :: elem
   character(len=*), intent(in), optional                              :: pname
!
   if (present(at)) then
       call crystal%set_atoms(at)
   endif
!
   if (present(bonds)) then
       call crystal%set_bonds(bonds)
   endif
!
   if (present(pname)) then
       crystal%cr_name = trim(pname)
   endif
!
   if (present(spg) .and. present(cell)) then
       call crystal%set_symmetry(spg,cell)
   endif
!
   if (present(elem)) then
       call crystal%set_scattering(elem)
   endif
!
   end subroutine set_crystal

!----------------------------------------------------------------------------------------------

   subroutine print_crystal(crystal,kpr,opt,radtype,wave)
   USE atom_type_util
   USE spginfom
   USE ccryst
   USE reflection_type_util
   class(crystal_phase_t), intent(in)            :: crystal
   integer, intent(in)                           :: kpr
   character(len=*), intent(in), optional        :: opt
   integer, intent(in), optional                 :: radtype
   real, intent(in), optional                    :: wave
   character(len=:), allocatable                 :: sopt
   type(element_type), dimension(:), allocatable :: elem1
   integer                                       :: rad
   if (present(opt)) then
       sopt = opt
   else
       sopt = 'NSCA'
   endif

   if (index(sopt,'N') > 0) then
       write(kpr,'(1x,a)') 'Crystal name: '//trim(crystal%cr_name)
   endif

   if (index(sopt,'S') > 0) then
       call crystal%spg%prn(kpr,prlevel=1)
       write(kpr,'(/)')
   endif

   if (index(sopt,'C') > 0) then
       call print_cell(crystal%cell%get_par(),kpr=kpr)
   endif

   if (index(sopt,'E') > 0) then
       if(crystal%numelem() > 0) then
          if (present(radtype)) then
              rad = radtype
          else
              rad = RX_SOURCE
          endif
          if (crystal%natoms() > 0) then
              call copy_elem(elem1,crystal%elem)
              call compute_content(crystal%at,elem1,crystal%spg)
              call print_elements(elem1,kpr,rad,wave)
          else
              call print_elements(crystal%elem,kpr,rad,wave)
          endif
       endif
   endif

   if (index(sopt,'A') > 0) then
       call print_atoms(crystal%at,kpr=kpr)
   endif

   if (index(sopt,'R') > 0 .and. present(wave)) then
       call write_reflections(jfile=kpr,refl=crystal%ref,code=5,wave=wave)
   endif

   end subroutine print_crystal

!----------------------------------------------------------------------------------------------

   subroutine print_bonds(crystal,kpr,opt)
   USE connect_mod
   USE rotationmod
   class(crystal_phase_t), intent(in)  :: crystal
   integer, intent(in)                    :: kpr
   character(len=*), intent(in), optional :: opt
   character(len=:), allocatable          :: sopt
!
   if (present(opt)) then
       sopt = opt
   else
       sopt = 'B'
   endif
!
   if (index(sopt,'B') > 0) then
       call print_connect(crystal%at%lab,crystal%bond,kpri=kpr)
   endif
   
   if (index(sopt,'C') > 0) then
       call print_connect(crystal%at%lab,connt=crystal%conn,kpri=kpr)
   endif

   if (index(sopt,'R') > 0) then
       call print_rotation(crystal%at%lab,crystal%rot,kpr,1)
   endif
!
   end subroutine print_bonds

!----------------------------------------------------------------------------------------------

   subroutine print_molecules(crystal,kpr)
   USE fragmentmod
   class(crystal_phase_t), intent(in)  :: crystal
   integer, intent(in)                 :: kpr
!
   if (crystal%numfrag() > 0) then
       call print_fragment(crystal%frag,crystal%at,kpr)
   endif
!
   end subroutine print_molecules

!----------------------------------------------------------------------------------------------

   subroutine print_doc(crystal,kpr)
   USE atom_basic
   USE arrayutil
   class(crystal_phase_t), intent(in)  :: crystal
   integer, intent(in)                 :: kpr
   integer, parameter                  :: MAXLENDOC = 60
!
   if (.not.crystal%doc_active()) return
!
   if (crystal%doc_active_all()) then
       write(kpr,'(1x,a)')'Dynamical Occupancy Correction is active on all atoms'
   else
       if (size_array(crystal%vetdoc) == 1) then
           write(kpr,'(1x,a,i0,a)')'Dynamical Occupancy Correction is active on selected atom: '//  &
           slabvet(crystal%vetdoc,crystal%at%lab)
       else
           write(kpr,'(1x,a,i0,a)')'Dynamical Occupancy Correction is active on ',size_array(crystal%vetdoc),   &
                                   ' selected atoms: '//slabvet(crystal%vetdoc,crystal%at%lab,maxlen=MAXLENDOC)
       endif
   endif
!
   end subroutine print_doc

!----------------------------------------------------------------------------------------------

   subroutine print_bvparam(crystal,kpr)
   USE bond_valence
   class(crystal_phase_t), intent(in) :: crystal
   integer, intent(in)                :: kpr
   if (allocated(crystal%bvpar)) then
       call print_bv_table(crystal%bvpar,kpr)
   endif
   end subroutine print_bvparam

!----------------------------------------------------------------------------------------------

   integer function numphase(phase)
   type(crystal_phase_t), dimension(:), allocatable, intent(in) :: phase
   if (allocated(phase)) then
       numphase = size(phase)
   else
       numphase = 0
   endif
   end function numphase

!----------------------------------------------------------------------------------------------

   subroutine resize_phases(phase,n)
!
!  Rialloca ad n un vettore reale.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(crystal_phase_t), allocatable, intent(inout) :: phase(:)
   integer, intent(in)                                  :: n
   integer                                              :: nv
   type(crystal_phase_t), allocatable                :: vsav(:)
   integer                                              :: nsav
!
!  se n = 0 (riallocazione a 0): dealloca ed esci
   if (n == 0) then
       if (allocated(phase)) deallocate(phase)
       return
   endif
!
   if (.not.allocated(phase)) then
       allocate(phase(n))
   else
!
       nv = size(phase)
!
!      nsav contiene qual è la porzione di phase da salvare
       select case(nv-n)
         case (1:)       ! compatta x ad n
           nsav = n
         case (:-1)      ! espandi x ad n
           nsav = nv
         case (0)
           return        ! n=nv non fare niente
       end select
       allocate(vsav(n))
       vsav(:nsav) = phase(:nsav)
       call move_alloc(vsav,phase)
   endif
!
   end subroutine resize_phases

!----------------------------------------------------------------------------------------------------

   subroutine new_phases(vetr,n)
!
!  Create new atoms
!
   type(crystal_phase_t), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n

   if (n < 0) return
   if (numphase(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_phases

!----------------------------------------------------------------------------------------------------

   subroutine clear_phases(vetr)
!
!  Delete all phases
!
   type(crystal_phase_t), allocatable, intent(inout) :: vetr(:)

   if (allocated(vetr)) deallocate(vetr)

   end subroutine clear_phases

!----------------------------------------------------------------------------------------------------

   subroutine push_back_phase(vetr,val)
!
!  Adds a new phase at the end of the array
!
   type(crystal_phase_t), allocatable, intent(inout) :: vetr(:)
   type(crystal_phase_t), intent(in)                 :: val
   integer                                              :: ndim
   ndim = numphase(vetr)
   call resize_phases(vetr,ndim+1)
   vetr(ndim+1) = val
   end subroutine push_back_phase

!----------------------------------------------------------------------------------------------------

   logical function any_crystal_atoms(cryst)
   type(crystal_phase_t), dimension(:), allocatable, intent(in) :: cryst
   integer                                                      :: nph
   any_crystal_atoms = .false.
   do nph=1,numphase(cryst)
      any_crystal_atoms = cryst(nph)%natoms() > 0
      if (any_crystal_atoms) return
   enddo
   end function any_crystal_atoms

!----------------------------------------------------------------------------------------------------

   subroutine save_crystal_bin_s(crystal,unitbin)
   USE unit_cell
   USE spginfom
   USE atom_type_util
   USE reflection_type_util
   USE model_util
   class(crystal_phase_t), intent(in) :: crystal
   integer, intent(in)                  :: unitbin
!
   write(unitbin)crystal%cr_name
!
   call save_cell_bin(unitbin,crystal%cell)
!
   call save_space_bin(unitbin,crystal%spg)
!
   call save_elem_bin(unitbin,crystal%elem,crystal%get_strelem())
!
   call save_structure_bin(unitbin,crystal%at,crystal%bond)
   write(unitbin)crystal%knw
   if (crystal%knw) then
       call save_structure_bin(unitbin,crystal%atpub)
   endif
!
   call save_refl_bin(unitbin,crystal%ref)
!
   call save_models_bin(unitbin,crystal%model)
!
   write(unitbin)crystal%style
!
   end subroutine save_crystal_bin_s

!----------------------------------------------------------------------------------------------------

   subroutine save_crystal_bin(crystal,unitbin)
   type(crystal_phase_t), intent(in), dimension(:), allocatable :: crystal
   integer, intent(in)                                          :: unitbin
   integer                                                      :: i
! 
   write(unitbin)numphase(crystal)
   do i=1,numphase(crystal)
      call crystal(i)%cr_save(unitbin)
   enddo
! 
   end subroutine save_crystal_bin

!----------------------------------------------------------------------------------------------------

   subroutine read_crystal_bin_s(crystal,unitbin,err)
   USE errormod
   USE atom_type_util
   USE reflection_type_util
   USE model_util
   class(crystal_phase_t), intent(out) :: crystal
   integer, intent(in)                    :: unitbin
   type(error_type), intent(out)          :: err
   integer                                :: ier
   type(cell_type)                        :: cell
   type(spaceg_type)                      :: spg
   character(len=:), allocatable          :: strel
! 
   read(unitbin,iostat=ier) crystal%cr_name
   if (ier /= 0) then
       call err%set('Error on reading crystal')
       return
   endif

   call read_cell_bin(unitbin,cell,err)
   if (err%signal) return
   call crystal%set_unit_cell(cell)

   call read_space_bin(unitbin,spg,err)
   if (err%signal) return
   call crystal%set_spg(spg)

   call read_elem_bin(unitbin,crystal%elem,strel,err)
   if (err%signal) return
   if (allocated(strel)) call crystal%set_strelem(strel)

   call read_structure_bin(unitbin,crystal%at,crystal%bond,err)
   if (err%signal) return
   read(unitbin,iostat=ier) crystal%knw
   if (ier /= 0) then
       call err%set('Error on reading crystal')
       return
   endif
   if (crystal%knw) then
       call read_structure_bin(unitbin,crystal%atpub,err=err)
       if (err%signal) return
   endif

   call read_refl_bin(unitbin,crystal%ref,err)
   if (err%signal) return

   call read_models_bin(unitbin,crystal%model,err) 
   if(err%signal) return

   read(unitbin,iostat=ier)crystal%style
   if (ier /= 0) then
       call err%set('Error on reading crystal style')
       return
   endif
! 
   end subroutine read_crystal_bin_s

!----------------------------------------------------------------------------------------------------

   subroutine read_crystal_bin(crystal,unitbin,err)
   USE errormod
   type(crystal_phase_t), intent(out), dimension(:), allocatable :: crystal
   integer, intent(in)                                           :: unitbin
   type(error_type), intent(out)                                 :: err
   integer                                                       :: i,ier,numphas
   
   read(unitbin,iostat=ier) numphas
   if (ier /= 0) then
       call err%set("Error on reading crystal phases")
       return
   endif
! 
   call new_phases(crystal,numphas)
   do i=1,numphas
      call crystal(i)%cr_read(unitbin,err)
      if (err%signal) return
   enddo

   end subroutine read_crystal_bin

!----------------------------------------------------------------------------------------------------

   logical function refine_cell(crystal)
!
!  True if cell is set for the refinement
!
   class(crystal_phase_t), intent(in)           :: crystal
   refine_cell = any(crystal%get_cellcod() > 0)
   end function refine_cell

!----------------------------------------------------------------------------------------------------

   real function compute_density(crystal)
   use atom_type_util
   class(crystal_phase_t), intent(in) :: crystal
   if (crystal%natoms() > 0) then
       compute_density = density_value(molecular_weight(crystal%at%ptab,crystal%at%och*crystal%at%ocry),  &
                                   crystal%cell%volume(),crystal%spg%nsymop)
   else
       if (crystal%numelem() > 0) then
           compute_density = density_value(molecular_weight(crystal%elem%ptab,crystal%elem%nw),  &
                                   crystal%cell%volume(),crystal%spg%nsymop)
       else
           compute_density = 0
       endif
   endif
   end function compute_density

!----------------------------------------------------------------------------------------------------

   subroutine make_occupancy(crystal,modcoor)
   use atom_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   logical, optional, intent(in)         :: modcoor
!
   if (present(modcoor)) then
       call calcola_occ(crystal%at,crystal%spg,crystal%cell,modcoor)
   else
       call calcola_occ(crystal%at,crystal%spg,crystal%cell)
   endif
!
   end subroutine make_occupancy
!----------------------------------------------------------------------------------------------------

   subroutine set_density(crystal)
   class(crystal_phase_t), intent(inout) :: crystal
   crystal%dens = crystal%density()
   end subroutine set_density

!----------------------------------------------------------------------------------------------------
!corr
!corr   subroutine set_mac(crystal,wave)
!corr   use elements
!corr   class(crystal_phase_t), intent(in) :: crystal
!corr   real, intent(in)                   :: wave
!corr   call elem_set_mac(crystal%elem,wave)
!corr   end subroutine set_mac
!corr
!----------------------------------------------------------------------------------------------------

   subroutine set_standard(crystal,ws)
   class(crystal_phase_t), intent(inout) :: crystal
   real, intent(in)                      :: ws
   crystal%is_std = .true.
   crystal%ws = ws
   end subroutine set_standard

!----------------------------------------------------------------------------------------------------

   subroutine set_wtpub(crystal,wtrue)
   class(crystal_phase_t), intent(inout) :: crystal
   real, intent(in)                      :: wtrue
   crystal%is_wtpub = .true.
   crystal%wtrue = wtrue
   end subroutine set_wtpub

!----------------------------------------------------------------------------------------------------

   elemental logical function is_standard(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   is_standard = crystal%is_std
   end function is_standard

!----------------------------------------------------------------------------------------------------

   elemental logical function is_wt_known(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   is_wt_known = crystal%is_wtpub
   end function is_wt_known

!----------------------------------------------------------------------------------------------------

   logical function is_empty_phase(crystal)
   class(crystal_phase_t), intent(in) :: crystal
   is_empty_phase = crystal%natoms() == 0 .and. .not.crystal%has_symmetry()
   end function is_empty_phase

!----------------------------------------------------------------------------------------------------

   logical function is_po_active(crystal)
   class(crystal_phase_t), intent(in) :: crystal
!
   is_po_active = abs(crystal%gpar - 1.0) > epsilon(1.0)
!
   end function is_po_active

!----------------------------------------------------------------------------------------------------

   integer function numrestot(crystals)
   type(crystal_phase_t), dimension(:), allocatable, intent(in) :: crystals
   integer :: i
   numrestot = 0
   do i=1,numphase(crystals)
      numrestot = numrestot + crystals(i)%numres()
   enddo
   end function numrestot

!----------------------------------------------------------------------------------------------------

   subroutine standard_setting(crystal,ier_std,kpr)
   use atom_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   integer, intent(out)                  :: ier_std
   integer, optional                     :: kpr
   real, dimension(3,3)                  :: qmat
   real, dimension(3)                    :: qvet
   integer                               :: kpri
   ier_std = 1
   if (.not.crystal%has_symmetry()) return
   if (present(kpr)) then
       kpri = kpr
   else
       kpri = -1
   endif
   call standard_symmetry(crystal%spg,crystal%cell,kpri,ier_std,qmat,qvet)  ! Q = P-1
   if (ier_std == 0) then
       qvet = -matmul(qmat,qvet)         ! q = -P-1 p
       call transform_coordinates(crystal%at,qmat,qvet)
   endif
   end subroutine standard_setting

!----------------------------------------------------------------------------------------------------

   subroutine transform_setting(crystal,spgnew,kpr)
   use atom_type_util
   class(crystal_phase_t), intent(inout) :: crystal
   integer, optional                     :: kpr
   type(spaceg_type), intent(in)         :: spgnew
   integer                               :: ier_std
   real, dimension(3,3)                  :: qmat
   real, dimension(3)                    :: qvet
   integer                               :: kpri
   if (.not.crystal%has_symmetry()) return
   if (present(kpr)) then
       kpri = kpr
   else
       kpri = -1
   endif
   call transform_symmetry(crystal%spg,spgnew,crystal%cell,kpri,ier_std,qmat,qvet)
   if (ier_std == 0) then
       call crystal%set_spg(spgnew)
       qvet = -matmul(qmat,qvet)         ! q = -P-1 p
       call transform_coordinates(crystal%at,qmat,qvet)
   endif
   end subroutine transform_setting
  
END MODULE crystal_phase
