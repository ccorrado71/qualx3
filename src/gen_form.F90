module gen_frm

USE prog_constants, only: CU_WAVE, CUwave
USE profile_function, only: PEARSON7

implicit none

enum, bind(c)
  enumerator :: ALL_FILES, CIF_FILE, PDB_FILE, SHELX_FILE, FRA_FILE,   &
                XYZ_FILE, ZMT_FILE, MOL_FILE, MOL2_FILE, MOP_FILE,     &
                POV_FILE, GAMESS_OUT, NWCHEM_OUT, ABINIT_OUT, GAMESS_IN, NWCHEM_IN_DFT, &
                GAUSSIAN_CART_IN, GAUSSIAN_ZMAT_IN, NWCHEM_IN_DFTD, ABINIT_IN, CRYSTAL_IN, &
                CRYSTAL_OUT,QE_IN,QE_OUT
endenum

integer, parameter :: TETRA=1,OCTA=2,SQUARE=3,CUBE=4,TRIG=5, APRISM_TETRA=6,&
                      PRISM_TRIGONAL=7,ICOSAHEDRON=8,ISOLATED=9,SMILES=10

type xpd_option_type
  integer             :: nwave = 1
  real, dimension(2)  :: wave = [CU_WAVE, CUwave(2)]
  real, dimension(2)  :: ratio = [1.0,0.5]
  real                :: ttmin = 5
  real                :: ttmax = 70
  real                :: ttstep = 0.02
  real                :: fwhm = 0.1
  real                :: scal = 1000.
  real                :: back = 0.
  integer             :: ftype = PEARSON7
  integer             :: nshape_par = 0
  real, dimension(10) :: shape_par = 0.0
end type xpd_option_type

private :: make_powder_pattern_var, make_powder_pattern_xpd
interface make_powder_pattern
  module procedure make_powder_pattern_var, make_powder_pattern_xpd
end interface

private :: read_dir_frag_s, read_dir_frag_a
interface read_dir_frag
  module procedure read_dir_frag_s, read_dir_frag_a
end interface 

contains
   subroutine import_crystal(filename,ftype,atom,legm,elem,cell,spgin,has_symmetry,wave,radtype,   &
                             gui_enabled,nowarning,err_read,cancel_required,mergecont,cname,mname)
!
!  If has_symmetry is true structure will be imported in symmetry (cell and spgin) defined in input
!  If has_symmetry is false cell and spgin will contain the symmetry of the file or default if symmetry is absent in file
!
   USE fileutil
   USE errormod
   USE connect_mod
   USE molpnew
   USE atom_type_util
   USE strutil
   USE elements
   USE spginfom
   USE unit_cell
   USE cif_frm
   USE pdb_frm
   USE fra_frm
   USE xyz_frm
   USE zmt_frm
   USE shelx_frm
   USE mol2_frm
   USE mop_frm
   USE qe_frm
   USE wlist
   USE OBmodule
   USE crystal_phase
   USE crystal_frm
   USE arrayutil
   character(len=*), intent(in)                                 :: filename
   integer, intent(in)                                          :: ftype          ! if 0 the file type is set from filename
   type(atom_type), dimension(:), allocatable, intent(inout)    :: atom
   type(bond_type), dimension(:), allocatable, intent(inout)    :: legm
   type(element_type), dimension(:), allocatable, intent(inout) :: elem
   type(cell_type), intent(inout)                               :: cell
   type(spaceg_type), intent(inout)                             :: spgin
   logical, intent(in)                                          :: has_symmetry    ! if true structure will be imported in cell and spgin
   real, intent(in)                                             :: wave
   integer, intent(in)                                          :: radtype
   logical, intent(in)                                          :: gui_enabled     ! if true GUI is used to chose between more models
   logical, intent(in)                                          :: nowarning
   type(error_type), intent(out)                                :: err_read
   logical, intent(out)                                         :: cancel_required ! if true 'Cancel' button was pressed
   logical, intent(in), optional                                :: mergecont       ! action on content: true for merge, false for complete update
   character(len=:), allocatable, optional                      :: cname,mname
   type(bond_type), dimension(:), allocatable                   :: legmc
   type(atom_type), dimension(:), allocatable                   :: atomc
   type(spaceg_type)                                            :: spgfile
   logical                                                      :: is_bfac,is_occ
   logical                                                      :: make_conn
   logical                                                      :: trasl
   type(error_type), dimension(:), allocatable                  :: errcif
   integer                                                      :: i
   logical                                                      :: make_label
   logical                                                      :: make_occ
   logical                                                      :: is_conect
   character(len=16)                                            :: spacepdb
   integer                                                      :: nmodel
   logical, dimension(2)                                        :: optconn
   type(cell_type)                                              :: cellfile
   integer                                                      :: filetype
   integer, parameter                                           :: NMAXATOM = 500
   integer                                                      :: nblockcif
   type(crystal_phase_t), dimension(:), allocatable             :: phase_file
   integer, parameter                                           :: CODE_CIFLIST = 101
   logical                                                      :: mergec
   logical                                                      :: usecov = .false.
   character(len=WL_NMAXLEN), dimension(:,:), allocatable       :: sitem
   integer                                                      :: selected_row
   integer, dimension(:), allocatable                           :: vmodel_cif
   integer                                                      :: selected_cif
   !character(len=:), allocatable                      :: cname1,mname1
!
   make_conn = .false.     
   is_bfac = .false.
   trasl = .false.
   make_label = .false.
   make_occ = .false.
   optconn = (/.true.,.true./)
   cancel_required = .false.
   if (ftype == ALL_FILES) then
       filetype = code_fragfilen(filename)
   else
       filetype = ftype
   endif

   select case (filetype)

     case (FRA_FILE)    ! fra or frac file
       spgfile = init_spaceg_type()
       call read_fra_file(filename,atomc,is_bfac,is_occ,err_read)
!  
!      se si genera errore leggi il file come standard fractional file
       if (err_read%signal) then
           call clear_atoms(atomc)
           call read_fractional(filename,cell,has_symmetry,atomc,is_bfac,is_occ,err_read)
           !is_bfac = .false.
           !is_occ = .false.
       else
           if (.not.has_symmetry) cell =  cell_init()
       endif
       if (.not.err_read%signal) then
           make_conn = .true.
           make_occ = .not.is_occ
           make_label = .true.
       endif

     case (XYZ_FILE)    ! cartesian file
       if (.not.has_symmetry) cell =  cell_init()
       spgfile = init_spaceg_type()
       call read_xyzfile(filename,cell,atomc,err_read)
       if (.not.err_read%signal) then
           make_conn = .true.
           make_occ = .true.
           trasl = .true.
           make_label = .true.
       endif

     case (SHELX_FILE)    ! ShelX file (.ins,.res)
       call read_shelxfile(filename,atomc,cell,has_symmetry,spgfile,err_read)
       if (.not.err_read%signal) then
           make_conn = .true.
           is_bfac = .true.
       endif

     case (PDB_FILE)    ! pdb file
       call read_PDBfile(filename,cell,has_symmetry,atomc,legmc,is_conect,spacepdb,err_read)
       if (.not.err_read%signal) then
           spgfile = init_spaceg_type(spacepdb)
           if (is_conect)call copy_bonds(legm,legmc)
           make_conn = .not.is_conect
           is_bfac = .true.
           trasl = .true.
       endif

     case (CIF_FILE)    ! cif file
       if (present(cname) .and. present(mname)) then  ! used for powcod generation
           call read_CIFfile(filename,phase_file,nblockcif,has_symmetry,cell,errcif,  &
            etype='S',dummy=.true.,bisotype=2,checkb=.true.,cname=cname,mname=mname) 
       else
            call read_CIFfile(filename,phase_file,nblockcif,has_symmetry,cell,errcif)
       endif
       if (any(errcif(:nblockcif)%signal .eqv. .false.)) then
           nmodel = 0
           allocate(vmodel_cif(nblockcif))
           vmodel_cif(:) = 0
           do i=1,nblockcif
              if (phase_file(i)%natoms() > 0) then
                  nmodel = nmodel + 1
                  vmodel_cif(nmodel) = i
              endif
           enddo
           selected_row = 1
           if (nmodel > 0) then
               selected_cif = vmodel_cif(1)
               if (nmodel > 1 .and. gui_enabled) then
                   call new_array(sitem,nmodel,2,WL_NMAXLEN)
                   do i=1,nmodel
                      sitem(i,1) = trim(phase_file(vmodel_cif(i))%cr_name)
                      sitem(i,2) = trim(phase_file(vmodel_cif(i))%spg%symbol_xhm) 
                   enddo
                   call wlist_create('CIF File',           &
                                     trim(i_to_s(nmodel))//' crystal structures. Select one of them.', &
                                     nmodel, 2, 0,         &
                                     ['Data block name',   &
                                      'Space group    '],  &
                                      sitem,selected_row)
                   if (selected_row == 0) return ! cancel
                   selected_cif = vmodel_cif(selected_row)
               endif
           else
               selected_cif = 1
           endif
           if (selected_row == 0) return    ! cancel
           err_read = errcif(selected_cif)
!
!          controlla la cella 
           if (.not.has_symmetry) then
               cell = phase_file(selected_cif)%cell
           endif
           if (phase_file(selected_cif)%is_spg()) then
               spgfile = phase_file(selected_cif)%spg
           else
               spgfile = init_spaceg_type()   ! force 'P 1'
           endif
           if (phase_file(selected_cif)%natoms() > 0) then
               call copy_atoms(atomc,phase_file(selected_cif)%at)
               make_conn = .true.
               is_bfac = .true.
               trasl = .false.
               make_occ = .false.
           endif
       else
           err_read = errcif(1)
       endif

     case (ZMT_FILE)    ! z-matrix
       spgfile = init_spaceg_type() 
       if (.not.has_symmetry) cell = cell_init()
       call read_zmtfile(filename,cell,atomc,err_read)
       if (.not.err_read%signal) then
           make_conn = .true.
           make_label = .true.
           make_occ = .true.
       endif

     case (MOP_FILE)    ! mop file
       spgfile = init_spaceg_type() 
       if (.not.has_symmetry) cell = cell_init()
!
!      Prova a leggere il file come mop in formato coordinate interne
       call read_mopintfile(filename,cell,atomc,err_read)
!    
!      se si genera errore leggi il file come mop a coordinate cartesiane
       if (err_read%signal) then
           call clear_atoms(atomc)
           call read_mopcartfile(filename,cell,atomc,err_read)
       endif
       if (.not.err_read%signal) then
           make_conn = .true.
           make_occ = .true.
           make_label = .true.
           trasl = .true.
       endif

     case (MOL_FILE,GAMESS_OUT,NWCHEM_OUT,ABINIT_OUT,CRYSTAL_OUT,QE_OUT)    ! mol file, gamess output file
       if (is_espresso_file(filename)) then
           call read_qe_file(filename,atomc,cellfile,spgfile,err_read)
           if (.not.err_read%signal) then
               if (.not.has_symmetry) cell = cellfile
               make_conn = .true.
               make_label = .true.
               make_occ = .true.
           endif
       elseif (is_crystal_file(filename) > 0) then
           call read_crystal_file(filename,atomc,cellfile,spgfile,err_read)
           if (.not.err_read%signal) then
               if (.not.has_symmetry) cell = cellfile
               make_conn = .true.
               make_label = .true.
               make_occ = .true.
           endif
       else
           call OBConversionReadFile(filename,atomc,legmc,err_read)
           if (.not.err_read%signal) then
               if (.not.has_symmetry) then
                   if (has_cell) then
                       cell = set_cell_type(MyCell)
                   else
                       cell = cell_init()
                   endif
                   if(has_spg) then
                      spgfile = init_spaceg_type(spgname)
                   else
                      spgfile = init_spaceg_type() 
                   endif
               endif
               call copy_bonds(legm,legmc)
               call cart_to_frac(atomc,cell%get_ortoi())
               trasl = .true.
               make_occ = .true.
               make_label = .true.
           endif
       endif

     case (MOL2_FILE)    ! mol2 file
       call read_mol2file(filename,atomc,legmc,spgfile,cellfile,err_read)
       if (.not.err_read%signal) then
           if (.not.has_symmetry) then
               if (.not. has_cell) cell = cellfile
           endif
           call copy_bonds(legm,legmc)
           call cart_to_frac(atomc,cell%get_ortoi())
           trasl = .true.
           make_occ = .true.
       endif

     case default 
         call err_read%set('Unknown file format. File '//trim(filename))

   end select
!
   if (err_read%signal) then
       call err_read%print()
   else
!
!      Update space group in input if was not set by user
       if (.not.has_symmetry) spgin = spgfile
!
       if (numatoms(atomc) > 0) then  ! cif files can't contain atom coordinates
           call copy_atoms(atom,atomc)
!
!          Trasla il frammento in cella: azione eseguita solo se il file contiene coord. cart.
           if (trasl) call translate_in_cell(atom)
!
!          Calcola occupanza cristallog. senza modificare le coordinate     
           call calcola_occ(atom,spgin,cell,modcoor=.false.)
           if(make_occ) atom%och = 1.0  ! inizializza occ. chimica
!
!          Action on cell content
           if (present(mergecont)) then
               mergec = mergecont
           else
               mergec = .true. ! in default merge is performed
           endif
           call make_elements(elem,atom(:)%ptab,wave,radtype,mergec,spgin%nsymop,atom%ocry*atom%och)   ! merge elements
           call specie_from_ptab(atom,elem)
!
!          Creare le label
           if (make_label) call atom_string(atom,elem)  
!
!          Genera connettivita' se non esiste nel file importato
           if (make_conn) then
!              disable because coordinates could be wrong or for too atoms
               !if (err_read%warning .or. size(atom) > NMAXATOM) optconn(:) = .false.   !!FIXME - potresti usare un code error
                                                                     !optconn(:) = .false.
               if (size(atom) > NMAXATOM) optconn(:) = .false. 
               if (((filetype == PDB_FILE .or. filetype == CIF_FILE)) .and. any(atom%rcod(1) /= 0)) then  ! special connectivity for duplicate residues
                   optconn(:) = .false.
                   call create_connectivity(atom,legm,cell,spgin,check=optconn(1),move=optconn(2), &
                        vet=atom(:)%rcod(1),code=3,usecov=usecov)
                   atom%rcod(1) = 0
               else
                   call create_connectivity(atom,legm,cell,spgin,check=optconn(1),move=optconn(2),usecov=usecov)
               endif
           endif
!
!          Genera fattori termici se non esistono nel file
           if(.not.is_bfac)call set_biso(atom)
!
           call set_intensity(atom)
!
       else
           call clear_atoms(atom)
           call clear_bonds(legm)
       endif
!
       if (err_read%warning .and. .not.nowarning) call err_read%print()
   endif
!
   end subroutine import_crystal

!----------------------------------------------------------------------------------------------------------------      

   subroutine export_structure(filename,atom,cell,spg,elem,ftype,sname,wavel,legm,comm,iscell)
   USE prognames
   USE errormod
   USE connect_mod
   USE elements
   USE spginfom
   USE cif_frm
   USE pdb_frm
   USE shelx_frm
   USE OBmodule
   USE unit_cell
   USE atom_type_util
   USE nwchem_frm
   USE gamess_frm
   USE abinit_frm
   USE xyz_frm
   USE zmt_frm
   USE fra_frm
   USE mop_frm
   USE crystal_frm
   USE qe_frm
   USE povray_frm
   USE fileutil
!!!TOFIX
   interface
       subroutine jav_get_style(cols, colcell, vfact, bmat)
       implicit none
       real, dimension(:,:) :: cols
       real, dimension(3)   :: colcell
       real, dimension(2)   :: vfact
       real, dimension(3,3) :: bmat
       end subroutine jav_get_style
   end interface
!!!TOFIX
   character(len=*), intent(in)                                       :: filename
   type(atom_type), dimension(:), intent(in), allocatable             :: atom
   type(cell_type), intent(in)                                        :: cell
   type(spaceg_type), intent(in)                                      :: spg
   type(element_type), dimension(:), allocatable                      :: elem
   integer, intent(in)                                                :: ftype
   character(len=*), intent(in)                                       :: sname
   real, intent(in)                                                   :: wavel
   type(bond_type), dimension(:), allocatable, intent(inout),optional :: legm
   character(len=*), intent(in), optional                             :: comm
   logical, intent(in), optional                                      :: iscell
   type(error_type)                                                   :: err_exp
   real, dimension(:,:), allocatable                                  :: cols
   real, dimension(3)                                                 :: colcell
   real, dimension(2)                                                 :: vfact
   real, dimension(3,3)                                               :: bmat
   integer                                                            :: filetype
   type(atom_type), dimension(:), allocatable                         :: atomc
   integer                                                            :: ier
!
   if (ftype == ALL_FILES) then
       filetype = code_fragfilen(filename)
   else
       filetype = ftype
   endif
!
   select case (filetype)
      case (SHELX_FILE)    ! .res file
        call create_shelxfile(filename,atom,legm,cell,spg,wavel,elem,sname,package_alt_name)

      case (PDB_FILE)     ! pdb file
        call create_pdbfile(filename,atom,cell,legm,spg)

      case (FRA_FILE)     ! fra file
        call create_frafile(filename,atom,cell,'Structure '//trim(sname))

      case (XYZ_FILE)    ! cartesian xyz file
        call create_xyzfile(filename,atom,cell,'Structure '//trim(sname))

      case (POV_FILE)    ! POV file
        allocate(cols(numelem(elem),3))
!!!TOFIX
        !call jav_get_style(cols, colcell, vfact, bmat)
        call create_povrayfile(filename,atom,legm,cell,elem,cols,colcell,iscell,bmat)
#if _WIN32
!corr        call Csystem('C:\PROGRA~1\POV-RA~1.5\bin\pvengine.exe '//'+I'//trim(filename)//char(0))
        ier = run_system('C:\PROGRA~1\POV-RA~1.5\bin\pvengine.exe '//'+I'//trim(filename))
#else
!corr        call Csystem('povray +P +W800 +H600'//' +I'//trim(filename)//' &'//char(0))
        ier = run_system('povray +P +W800 +H600'//' +I'//trim(filename)//' &')
#endif

      case (CIF_FILE)    ! CIF file
        if (present(comm)) then
            if (present(legm)) then
                call create_ciffile(atom,cell,spg,elem,package_alt_name,    &
                comm,bond=legm,filename=filename)
            else
                call create_ciffile(atom,cell,spg,elem,package_alt_name,    &
                comm,filename=filename)
            endif
        else
            if (present(legm)) then
                call create_ciffile(atom,cell,spg,elem,package_alt_name,    &
                bond=legm,filename=filename)
                !call create_ciffile(atom,cell,spg,elem,package_alt_name,    &
                !bond=legm,filename=filename,std=.true.,symm=.true.)
            else
                call create_ciffile(atom,cell,spg,elem,package_alt_name,filename=filename)
            endif
        endif

      case (ZMT_FILE)    ! zmt file (Fenske-Hall Z-Matrix)
        call create_zmtfile(filename,atom,legm,cell)

      case (MOL_FILE,MOL2_FILE)    ! mol file
        call bond_type_perception(atom,legm,cell)
        !if (numbonds(legm) > 0) where(legm%ord == 4) legm%ord = 5   ! In OBabel bond order is 1, 2, 3, 5=aromatic
        call frac_to_cart_copy(atom,atomc,cell%get_ortom())
        call OBConversionWriteFile(filename,atomc,legm,cell,spg)

      case (NWCHEM_IN_DFT)
        call write_nw_file(filename,atom,cell,spg,sname,nwtheory=1)

      case (NWCHEM_IN_DFTD)
        call write_nw_file(filename,atom,cell,spg,sname,nwtheory=2)

      case (ABINIT_IN)
        call write_ab_file(filename,atom,cell,spg)

      case (CRYSTAL_IN)
        call write_crystal_file(filename,atom,cell,spg,sname)

      case (GAMESS_IN)
        call write_gmfile(filename,atom,cell,sname)

      case (GAUSSIAN_CART_IN, GAUSSIAN_ZMAT_IN)
        call frac_to_cart_copy(atom,atomc,cell%get_ortom())
        call OBConversionWriteFile(filename,atomc,legm,cell,spg)

      case (MOP_FILE)    ! cartesian mop file 
        call create_mopcrtfile(filename,atom,cell,sname)

      case (QE_IN)    
        call write_qe_file(filename,atom,cell,spg,sname,.false.)

      case default 
        call err_exp%set('Unknown file format. File '//trim(filename))
   end select
!
   if (err_exp%signal) then
       call err_exp%print()
   endif
!
   end subroutine export_structure

!----------------------------------------------------------------------------------------------------------------      
!corr
!corr   subroutine cif_func(code)
!corr   integer, intent(in) :: code
!corr   select case (code)
!corr      case (:-1)
!corr        selected_cif = vmodel_cif(-code)
!corr      case (0)  ! open list
!corr      case (1)  ! Ok on list
!corr      case (2)  ! Cancel on list
!corr   end select
!corr   end subroutine cif_func
!corr
!---------------------------------------------------------------------      

   integer function code_fragfilen(filedirname)   result(code)
!
!  Utilizza l'estensione del nome del file per individuare il tipo
!
   USE strutil
   USE fileutil
   character(len=*), intent(in)         :: filedirname
!
   select case (remc0(upper(get_extension(filedirname))))
     case ('FRA','FRAC')
       code = FRA_FILE

     case ('XYZ')
       code = XYZ_FILE

     case ('RES','INS')
       code = SHELX_FILE

     case ('PDB','ENT')
       code = PDB_FILE

     case ('CIF')
       code = CIF_FILE

     case ('ZMT','FH','FHZ')
       code = ZMT_FILE

     case ('MOP')
       code = MOP_FILE

     case ('MOL','MDL','SDF','SD')
       code = MOL_FILE

     case ('MOL2','ML2')
       code = MOL2_FILE

     case ('NW')
       code = NWCHEM_IN_DFT

     case ('POV')
       code = POV_FILE

     case ('OUT','LOG')
       code = GAMESS_OUT

     case ('INP','GAMIN')
       code = GAMESS_IN

     case ('COM','GAU')
       code = GAUSSIAN_CART_IN

     case ('GZMAT')
       code = GAUSSIAN_ZMAT_IN

     case ('IN')
       code = ABINIT_IN

     case ('D12')
       code = CRYSTAL_IN

     case default 
       code = 0

   end select
!
   end function code_fragfilen

!-------------------------------------------------------------------------------------------------------  

   subroutine crystal_file_import(crystal,filename,ftype,wave,radtype,has_symmetry,nowarning,err)
   USE crystal_phase
   USE spginfom
   USE unit_cell
   USE errormod
   USE fileutil
   type(crystal_phase_t), intent(inout) :: crystal
   character(len=*), intent(in)         :: filename
   integer, intent(in), optional        :: ftype
   real, intent(in)                     :: wave
   integer, intent(in), optional        :: radtype
   logical, intent(in), optional        :: has_symmetry
   logical, intent(in), optional        :: nowarning
   type(error_type), intent(out)        :: err
   integer                              :: ftypef,radtypef
   type(cell_type)                      :: cell
   type(spaceg_type)                    :: spg
   logical                              :: has_symmetryf,nowarningf,cancel_req
!
   if (present(ftype)) then
       ftypef = ftype
   else
       ftypef = ALL_FILES
   endif
   if (present(radtype)) then
       radtypef = radtype
   else
       radtypef = RX_SOURCE
   endif
   if (present(has_symmetry)) then
       has_symmetryf = has_symmetry
   else
       has_symmetryf = .false.
   endif
   if (present(nowarning)) then
       nowarningf = nowarning
   else
       nowarningf = .false.
   endif
   if (has_symmetryf) then
       call import_crystal(filename,ftypef,crystal%at,crystal%bond,crystal%elem,crystal%cell,     &
                           crystal%spg,has_symmetryf,wave,radtypef,.false.,nowarningf,err,cancel_req)
   else
       call import_crystal(filename,ftypef,crystal%at,crystal%bond,crystal%elem,                  &
                           cell,spg,has_symmetryf,wave,radtypef,.false.,nowarningf,err,cancel_req)
   endif
   if (.not.err%signal) then
       if (.not.has_symmetryf)call crystal%set_symmetry(spg,cell)
       call crystal%set_name(file_rem_ext(file_get_name(filename)))
   endif
!
   end subroutine crystal_file_import

!----------------------------------------------------------------------------------------------------------------      

   subroutine crystal_file_export(crystal,filename,ftype,wave,comm)
   USE crystal_phase
   USE prog_constants
   type(crystal_phase_t), intent(inout)   :: crystal
   character(len=*), intent(in)           :: filename
   integer, intent(in), optional          :: ftype
   real, intent(in), optional             :: wave
   character(len=*), intent(in), optional :: comm
   integer                                :: ftypef   !!!!,radtypef
   real                                   :: wavef
!
   if (present(ftype)) then
       ftypef = ftype
   else
       ftypef = ALL_FILES
   endif
   if (present(wave)) then
       wavef = wave
   else
       wavef = CU_WAVE
   endif
!
   if (present(comm)) then
       call export_structure(filename,crystal%at,crystal%cell,crystal%spg,crystal%elem,ftypef,  &
                             crystal%cr_name,wavef,crystal%bond,comm,.false.)
   else
       call export_structure(filename,crystal%at,crystal%cell,crystal%spg,crystal%elem,ftypef,  &
                             crystal%cr_name,wavef,crystal%bond,iscell=.false.)
   endif
!
   end subroutine crystal_file_export

!----------------------------------------------------------------------------------------------------------------      

   subroutine import_from_file(filename,atom,legm,elem,cell,spg,add,gui,has_symmetry,wave,radtype,err)
   USE connect_mod
   USE atom_type_util
   USE errormod
   USE spginfom
   USE elements
   USE unit_cell
   character(len=*), intent(in)                                 :: filename
   type(atom_type), dimension(:), allocatable, intent(inout)    :: atom
   type(bond_type), dimension(:), allocatable, intent(inout)    :: legm
   type(element_type), dimension(:), allocatable, intent(inout) :: elem
   type(cell_type), intent(inout)                               :: cell
   type(spaceg_type), intent(inout)                             :: spg
   logical, intent(in)                                          :: add
   logical, intent(in)                                          :: gui
   logical, intent(in)                                          :: has_symmetry
   real, intent(in)                                             :: wave
   integer, intent(in)                                          :: radtype
   type(error_type), intent(out)                                :: err
   type(atom_type), dimension(:), allocatable                   :: atom1
   type(bond_type), dimension(:), allocatable                   :: legm1
   integer                                                      :: natnew
   logical                                                      :: cancel_req
!
   call import_crystal(filename,ALL_FILES,atom1,legm1,elem,cell,spg,has_symmetry, &
                       wave,radtype,gui,.false.,err,cancel_req)
   if (err%signal) return
!
   if (add) then
       call add_atoms_to_list(atom,atom1,natnew,legm1=legm,legm2=legm1)
       call remove_duplicate_labels(atom)
   else
       call copy_atoms(atom,atom1)
       call copy_bonds(legm,legm1)
   endif
!
   !call action_after_import(filename,has_symmetry,elem,cell,spg)
!
   end subroutine import_from_file

!-------------------------------------------------------------------------------------------------

   subroutine import_fragment(strdir,atom,legm,cell,spg,elem,wave,radtype,err,kadd,gui,has_symmetry,file_name)
!
!  Import model parsing string from command '%fragment'
!
   USE errormod
   USE atom_type_util
   USE unit_cell
   USE elements
   USE spginfom
   USE connect_mod
   USE fileutil
   character(len=*), intent(in)                                 :: strdir
   type(atom_type), dimension(:), allocatable, intent(inout)    :: atom
   type(bond_type), dimension(:), allocatable, intent(inout)    :: legm
   type(cell_type), intent(inout)                               :: cell
   type(spaceg_type), intent(inout)                             :: spg
   type(element_type), dimension(:), allocatable, intent(inout) :: elem
   real, intent(in)                                             :: wave
   integer, intent(in)                                          :: radtype
   type(error_type), intent(out)                                :: err
   logical, intent(in)                                          :: kadd,gui,has_symmetry
   character(len=:), allocatable, optional                      :: file_name
   type(element_type), dimension(:), allocatable                :: cform
   integer                                                      :: codef
   integer                                                      :: i
   real                                                         :: dist
   logical                                                      :: kadd1
   character(len=len_trim(strdir))                              :: smiles_string
!
   if (present(file_name)) file_name = ' '
   call parse_fragment_string(strdir,codef,cform,dist,wave,radtype,smiles_string,err)
   if (.not.err%signal) then
       select case(codef)
         case (TETRA,OCTA,SQUARE,CUBE,TRIG,APRISM_TETRA,PRISM_TRIGONAL,ICOSAHEDRON)
           call make_fragment(atom,legm,cell,spg,elem,codef,0,-cform(1)%z,-cform(2)%z,kadd,dist,wave,radtype,1)
         case (ISOLATED)
           kadd1 = kadd
           do i=1,numelem(cform)
              if (i > 1) kadd1 = .true.  ! add next elements
              call make_fragment(atom,legm,cell,spg,elem,ISOLATED,0,-cform(i)%z,0,kadd1,dist,wave,radtype,nint(cform(i)%nw))
           enddo
         case (SMILES) 
           call get_smiles(smiles_string,atom,legm,elem,spg%nsymop,wave,radtype,cell,kadd,err)
           if (err%signal) call err%print()
         case (0) 
           call import_from_file(strdir,atom,legm,elem,cell,spg,kadd,gui,has_symmetry,wave,radtype,err)
           if (present(file_name)) then
               if (.not.err%signal) file_name = file_rem_ext(strdir)
           endif
       end select
   else
       call err%print()
   endif
!
   end subroutine import_fragment

!-------------------------------------------------------------------------------------------------------  

   subroutine crystal_fragment_import(crystal,str,wave,radtype,err)
   USE crystal_phase
   USE errormod
   USE fileutil
   character(len=*), intent(in)         :: str
   type(crystal_phase_t), intent(inout) :: crystal
   real, intent(in)                     :: wave
   integer, intent(in)                  :: radtype
   type(error_type), intent(out)        :: err
   type(cell_type)                      :: cell
   type(spaceg_type)                    :: spg
   logical                              :: kadd,gui,has_symmetry
   character(len=:), allocatable        :: file_name
!
   gui = .false.
   has_symmetry = .false.
   if (crystal%is_cell()) then
       has_symmetry=.true.
       cell = crystal%cell
       spg = crystal%spg
   endif
   kadd = crystal%natoms() > 0
   call import_fragment(str,crystal%at,crystal%bond,cell,spg,crystal%elem,  &
                        wave,radtype,err,kadd,gui,has_symmetry,file_name)
   if (.not.has_symmetry) call crystal%set_symmetry(spg,cell)
   if (len_trim(crystal%cr_name) == 0 .and. len_trim(file_name) > 0) call crystal%set_name(file_basename(file_name))
!
   end subroutine crystal_fragment_import

!-------------------------------------------------------------------------------------------------------  

   subroutine import_crystal_dir(crystal,dir,ndir,is_file,radtype,thmin,thmax,wave,anomal,err)
   USE crystal_phase
   USE spginfom
   USE unit_cell
   USE errormod
   USE strutil
   USE spginfom
   USE atom_type_util
   USE constraints
   USE fileutil
   USE fragmentmod
   type(crystal_phase_t), intent(out)               :: crystal
   character(len=*), dimension(:), intent(in)       :: dir
   integer, intent(in)                              :: ndir
   logical, intent(in)                              :: is_file
   integer, intent(in)                              :: radtype
   real, intent(in)                                 :: thmin,thmax
   real, dimension(:), intent(in)                   :: wave
   logical, intent(in)                              :: anomal
   type(error_type), intent(out)                    :: err
   integer                                          :: i,j,zval
   character(len=:), allocatable                    :: sdir,word,file_known,cell_cont
   real, dimension(:), allocatable                  :: vet
   integer, dimension(:), allocatable               :: ivet
   integer                                          :: iv
   type(cell_type)                                  :: cellnew,cellpub
   type(spaceg_type)                                :: spgpub
   type(atom_type), dimension(:), allocatable       :: atemp
   type(bond_type), dimension(:), allocatable       :: btemp
   type(constraint_type), dimension(:), allocatable :: constr
   integer                                          :: nfrag,ncopies,nc
   integer                                          :: ier,inum
   real                                             :: rnum
   logical                                          :: found,update
   character(len=:), allocatable                    :: file_name
   character(len=:), allocatable                    :: new_spg
   type(spaceg_type)                                :: spgnew
   logical                                          :: is_name
   integer                                          :: nfound,deldir
   integer, dimension(:), allocatable               :: nfoundv
   logical                                          :: is_transform,cancel_req
!
   file_known = ' '
   file_name = ' '
   cell_cont = ' '
   is_name = .false.
   nfrag = 0
   nfound = 0
   deldir = 0
   is_transform = .false.
   new_spg = ' '
   allocate(nfoundv(ndir))
   do i=1,ndir
      if (i == 1 .and. is_file) then
          call crystal_file_import(crystal,dir(i),ALL_FILES,wave(1),radtype,err=err)
          if (err%signal) exit  ! error already printed
          if (.not.is_name) file_name = file_rem_ext(file_get_name(dir(i)))
          !if (crystal%numfrag() > 0) call crystal%make_fragments(keep=.true.)
      else
          sdir = trim(dir(i))
          call cutsta(sdir,line2=word)
          if (word_is_contained(word,'autoz')) then
              zval = nint(number_of_molecules(crystal%at,crystal%cell,crystal%spg))
              if (zval > 1) then
                  call crystal%get_atoms(atemp)
                  call crystal%get_bonds(btemp)
                  do j=1,zval-1
                     call crystal%add_atoms(atemp,btemp)
                  enddo
              endif

          elseif (word_is_contained(word,'abs')) then
              call s_to_i(sdir,inum,ier)   
              if (ier == 0) then
                  crystal%absf = inum
              else
                  call err%set('Error reading directive '//trim(dir(i)))
              endif

          elseif (word_is_contained(word,'cell')) then
              call getnum1(sdir,vet,iv=iv)
              if (iv == 6) then
                  cellnew = set_cell_type(vet(:6))
                  if (crystal%natoms() > 0) then
                      call coord_in_newcell(crystal%at,crystal%cell,cellnew)
                  endif
                  call crystal%set_unit_cell(cellnew)
              else
                  call err%set('Error reading directive '//trim(dir(i)))
              endif

          elseif (word_is_contained(word,'noextraction')) then
              crystal%is_extraction = .false.

          elseif (word_is_contained(word,'file')) then
              call crystal_file_import(crystal,sdir,ALL_FILES,wave(1),radtype,err=err)
              if (err%signal) exit  ! error already printed

          elseif (word_is_contained(word,'fragment')) then
              nfrag = nfrag + 1
              call read_dir_numfrag(dir,ndir,nfrag,ncopies,err)
              if (.not.err%signal) then
                  do nc=1,ncopies
                     call crystal_fragment_import(crystal,sdir,wave(1),radtype,err)
                     if (err%signal) exit
                  enddo
              endif

          elseif (word_is_contained(word,'known')) then
              file_known = sdir

          elseif (word_is_contained(word,'name')) then
              call crystal%set_name(sdir)
              is_name = .true.

          elseif (word_is_contained(word,'spacegroup')) then
              call crystal%set_spg(sdir,err)

          elseif (word_is_contained(word,'numfrag')) then
              cycle

          elseif (word_is_contained(word,'radius')) then
              ier = s_to_r(sdir,rnum)
              if (ier == 0) then
                  crystal%p_size = rnum
                  crystal%brindley = .true.
              else
                  call err%set('Error reading directive '//trim(dir(i)))
              endif

          elseif (word_is_contained(word,'po')) then ! orientazione preferenziale
              call getnum1(sdir,ivet=ivet,iv=iv)
              if (iv == 3) then
!corr                  crystal%kopcorr = 1
                  crystal%gcode = 1
                  crystal%hklpo(:) = ivet(1:3)
              endif

          elseif (word_is_contained(word,'constr')) then
              call constraint_from_equation(sdir,crystal%at,constr,err)
              if (.not.err%signal) then
                  call push_back_constraint(crystal%constr,constr)
              endif

          elseif (word_is_contained(word,'standard')) then
              ier = s_to_r_perc(sdir,rnum)
              if (ier == 0) then
                  call crystal%set_standard(rnum)
              else
                  call err%set('Error reading directive '//trim(dir(i)))
              endif
             
          elseif (word_is_contained(word,'wtrue')) then
              ier = s_to_r_perc(sdir,rnum)
              if (ier == 0) then
                  call crystal%set_wtpub(rnum)
              else
                  call err%set('Error reading directive '//trim(dir(i)))
              endif

          elseif (word_is_contained(word,'content')) then
              cell_cont = sdir
          elseif (word_is_contained(word,'DELETEHYDRO',7)) then   ! restraints 
              deldir = i
          elseif (word_is_contained(word,'transform',7)) then     ! transform space group 
              is_transform = .true.
              new_spg = sdir
          else
!
!             Other directives must be processed after
              nfound = nfound + 1
              nfoundv(nfound) = i

          endif
      endif

      if (err%signal) go to 10
   enddo
!
!  Process directives related to fragments
   update = .false.
   if (deldir > 0 .and. crystal%natoms() > 0) then  ! process directives that delete atoms before make frag
       call read_dir_frag(dir(deldir),crystal,radtype,err,found,update)
       if (err%signal) go to 10
   endif
   if (nfound > 0) then
       if (crystal%natoms() > 0) then
           call crystal%make_fragments()
           call crystal%make_rotations()
           do i=1,nfound
              call read_dir_frag(dir(nfoundv(i)),crystal,radtype,err,found,update)
              if (.not.found) then
                  call err%set('Error reading directive '//trim(dir(nfoundv(i))))
              endif
              if (err%signal) go to 10
           enddo
       else
           call err%set('Error reading directive '//trim(dir(nfoundv(1))))
           go to 10
       endif
   endif
!
   if (is_transform) then
       if (len_trim(new_spg) > 0) then
           spgnew = init_spaceg_type(new_spg)
           call crystal%transform_setting(spgnew,kpr=6)
       else
           call crystal%standard_setting(ier,kpr=6)
       endif
   endif
!
   if (thmin < thmax) call crystal%make_reflections(thmin,thmax,wave)
!
!  Process directive content if the cell contenten has been not defined
   if (len_trim(cell_cont) > 0 .and. crystal%numelem() == 0) then
       call crystal%set_scattering(cell_cont,wave(1),radtype,ier)
       if (ier /= 0) then
           call err%set('Error reading directive content '//trim(cell_cont))
           go to 10
       endif
   endif
!
!  Process directive known
   if (len_trim(file_known) > 0) then
       call import_crystal(file_known,ALL_FILES,atemp,btemp,crystal%elem,cellpub, &
                           spgpub,.false.,wave(1),radtype,.false.,.false.,err,cancel_req)
       call crystal%set_as_known(atemp,radtype,anomal)
   endif
!
!  Assign name if directory name is absent
   if (.not.is_name .and. len_trim(file_name) > 0) call crystal%set_name(file_name)

   return

10 call err%print()
!
   end subroutine import_crystal_dir 

!----------------------------------------------------------------------------------------------------

   subroutine read_dir_frag_s(strdir0,crystal,radtype,errdir,found,update)
   USE strutil
   USE anti_bump
   USE errormod
   USE atom_type_util
   USE connect_mod
   USE constraints
   USE fragmentmod
   USE rotationmod
   USE crystal_phase
   USE bond_valence
   USE rrestr
   USE fileutil
   use arrayutil
   use rigid_body
   use commandsmod
   use cryutil
   use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
   character(len=*), intent(in)                     :: strdir0
   type(crystal_phase_t), intent(inout)             :: crystal
   integer, intent(in)                              :: radtype
   type(error_type), intent(out)                    :: errdir
   logical, intent(out)                             :: found
   logical, intent(inout)                           :: update  ! structure has been changed
   character(len=len(strdir0))                      :: strdir,word
   real, dimension(100)                             :: vet
   real, dimension(:), allocatable                  :: vet1
   integer, dimension(100)                          :: ivet
   integer, dimension(:), allocatable               :: ivet1
   integer                                          :: iv,iv1
   integer                                          :: nlongs
   integer                                          :: ier
   integer                                          :: i
   integer, dimension(size(crystal%at))             :: ratom
   integer                                          :: natr
   real                                             :: rshift
   logical                                          :: conncalc,inter
   type(container_type), allocatable                :: connect(:)
   integer                                          :: fpos,nrot,kfrag
   integer, dimension(:), allocatable               :: vatom
   integer                                          :: natom,kposf,nval,natold
   real, dimension(:), allocatable                  :: val
   real, parameter                                  :: SIGD = 0.1
   real, parameter                                  :: WEID = 100
   type(restraint_type)                             :: restemp
   character(len=:), allocatable                    :: satom,scoord
   integer                                          :: rot,lens,pos,kat,mode
   type(file_handle)                                :: fbv
!
   conncalc = .false.
   strdir = strdir0
   call Cutst(strdir,nlongs,word) 
!
   found = .true.
   if (word_is_contained(word,'CENTRE_OF_ROTATION')) then ! centre of rotation
       call set_centre_from_string(strdir,crystal%frag,crystal%at,errdir)

   elseif (word_is_contained(word,'CUT')) then ! cut bonds
       allocate(vatom(2))
       call get_atoms_from_string(strdir,crystal%at,vatom,natom,err=errdir)
       if (natom /= 2) then
           ier = 1
           go to 10
       endif
       if (errdir%signal) then
           call errdir%add(' in directive '//trim(strdir0))
           return
       endif
       pos = bond_position(crystal%bond,vatom(1),vatom(2))
       if (pos == 0) then
           call errdir%set('Error in directive '//trim(strdir0)//':'//char(10)// &
                           'Bond cannot be found!')
           return
       else
           call remove_bond(crystal%bond,pos)
           update = .true.
       endif

   elseif (word_is_contained(word,'DOC')) then ! dynamical occupancy correction
       if (len_trim(strdir) > 0) then
           allocate(vatom(crystal%natoms()))
           call get_atoms_from_string(strdir,crystal%at,vatom,natom,err=errdir)
           if (errdir%signal) then
               call errdir%add(' in directive DOC')
           else
               if (natom > 0) call crystal%set_doc(vatom(:natom))
           endif
       else
           call crystal%set_doc()
       endif

    elseif (word_is_contained(word,'NODOC')) then ! dynamical occupancy correction
       if (len_trim(strdir) > 0) then
           allocate(vatom(crystal%natoms()))
           call get_atoms_from_string(strdir,crystal%at,vatom,natom,err=errdir)
           if (errdir%signal) then
               call errdir%add(' in directive NODOC')
           else
               if (natom > 0) call crystal%set_doc(vatom(:natom),.false.)
           endif
       else
           call crystal%set_doc(.false.)
       endif
            
   elseif (word_is_contained(word,'FIXED')) then   ! no extraction, no optimization 
       crystal%is_extraction = .false.
       do i=1,crystal%numfrag()
          crystal%frag(i)%rcod = 0
       enddo
       do i=1,crystal%numrot()
          crystal%rot(i)%rcod = 0
       enddo

   elseif (word_is_contained(word,'FIXROTATION') .or. word_is_contained(word,'INTDOF')) then ! fix rotation
       allocate(vatom(crystal%natoms()))
       call new_array(val,1)
       call get_atoms_from_string(strdir,crystal%at,vatom,natom,val,[0.0],nval,dupl=.true.,jolly=.false.,err=errdir)
       if (.not.errdir%signal .and. (natom /= 2 .and. natom /= 1)) then
           call errdir%set('Error in directive '//trim(strdir0))
           return
       endif
       if (errdir%signal) then
           call errdir%add(' in directive '//trim(strdir0))
       else
           if (natom == 2) then      ! fix rotation atom1-atom2
               if (crystal%numrot() > 0) then
                   kposf = rotation_position(crystal%rot,vatom(:natom))
                   if (kposf > 0) then
                       if (val(1) > 0) then
                           crystal%rot(kposf)%rcod = 1
                       else
                           crystal%rot(kposf)%rcod = 0
                       endif
                   else
                       call errdir%set('Error in directive '//trim(strdir0)//':'//char(10)// &
                                       'Rotation cannot be found!')
                   endif
               endif
           elseif (natom == 1) then  ! fix all rotations in fragment containing atom1
               kfrag = fragment_pos(crystal%frag,vatom(1))
               if (kfrag > 0) then
                   do i=1,crystal%numrot()
                      if (fragment_pos(crystal%frag,crystal%rot(i)%pax(1)) == kfrag) then
                          if (val(1) > 0) then
                              crystal%rot(i)%rcod = 1
                          else
                              crystal%rot(i)%rcod = 0
                          endif
                      endif
                   enddo
               endif
           endif
       endif

   elseif (word_is_contained(word,'INTDOF_DISP')) then ! set displacement for rotation
       allocate(vatom(crystal%natoms()))
       call new_array(val,2)
       call get_atoms_from_string(strdir,crystal%at,vatom,natom,val,[0.0,1.0],nval,dupl=.true.,jolly=.false.,err=errdir)
       mode = int(val(2))
       if (.not.errdir%signal) then
           ier = 0
           if(natom /= 4) ier = ier + 1
           if (nval > 2) ier = ier + 1
           if (mode < 0 .or. mode > 3) ier = ier + 1
           if(ier > 0) go to 10
       endif
       if (errdir%signal) then
           call errdir%add(' in directive '//trim(strdir0))
       else
           if (is_valid_torsion(vatom(:natom),crystal%bond)) then
               kposf = rotation_position(crystal%rot,vatom(2:3))
               if (kposf > 0) then
                   call set_torsion_from_atoms(vatom(:natom),crystal%at,crystal%cell,crystal%rot(kposf))
                   crystal%rot(kposf)%mode = mode
                   call set_limit_for_rotat(crystal%rot(kposf),abs(val(1)))
               else
                   call errdir%set('Error in directive '//trim(strdir0)//':'//char(10)// &
                                   'Rotation cannot be found!')
               endif
           else
               call errdir%set('Invalid torsion in directive '//trim(strdir0))
           endif
       endif

   elseif (word_is_contained(word,'EXTDOF')) then ! fix translation in fragment
       allocate(vatom(crystal%natoms()))
       call new_array(val,2)
       call get_atoms_from_string(strdir,crystal%at,vatom,natom,val,[0.0,0.0],nval,dupl=.true.,jolly=.false.,err=errdir)
       if (errdir%signal) then
           call errdir%add(' in directive '//trim(strdir0))
       else
           do i=1,natom
              fpos = fragment_pos(crystal%frag,vatom(i))
              if (fpos > 0) then
                  if (val(1) > 0) then
                      crystal%frag(fpos)%rcod(1:3) = 1
                  else
                      crystal%frag(fpos)%rcod(1:3) = 0
                  endif
                  if (val(2) > 0) then
                      crystal%frag(fpos)%rcod(4:6) = 1
                  else
                      crystal%frag(fpos)%rcod(4:6) = 0
                  endif
              else
                  call errdir%set('Error in directive '//trim(strdir0)//':'//char(10)// &
                                  'Molecular fragment cannot be found!')
              endif
           enddo
       endif

   elseif (word_is_contained(word,'FFTRANSLATION')) then ! fix translation in fragment, obsolete!
       allocate(vatom(crystal%natoms()))
       call get_atoms_from_string(strdir,crystal%at,vatom,natom,dupl=.true.,err=errdir)
       if (errdir%signal) then
           call errdir%add(' in directive fftranslation')
       else
           do i=1,natom
              fpos = fragment_pos(crystal%frag,vatom(i))
              if (fpos > 0) crystal%frag(fpos)%rcod(1:3) = 0
           enddo
       endif

   elseif (word_is_contained(word,'FFROTATION')) then ! fix rotation in fragment, obsolete!
       allocate(vatom(crystal%natoms()))
       call get_atoms_from_string(strdir,crystal%at,vatom,natom,dupl=.true.,err=errdir)
       if (errdir%signal) then
           call errdir%add(' in directive ffrotation')
       else
           do i=1,natom
              fpos = fragment_pos(crystal%frag,vatom(i))
              if (fpos > 0) crystal%frag(fpos)%rcod(4:6) = 0
           enddo
       endif

   elseif (word_is_contained(word,'REFINETF')  &
      .or. word_is_contained(word,'OPTIMADP')) then ! directive refinetf: refine thermal factor
       call constr_tf_from_string(strdir,crystal%constr,crystal%at,crystal%bond,crystal%frag,crystal%elem,radtype,ier)
       !if (ier /= 0) call errdir%set('Error in directive "'//trim(strdir0)//'"')
       if (ier /= 0) go to 10

   elseif (word_is_contained(word,'SHIFT_ATOM')) then ! shift to atoms: At1 At2 ... shift
       allocate(vatom(crystal%natoms()))
       call new_array(val,1)
       call get_atoms_from_string(strdir,crystal%at,vatom,natom,val,[0.5],nval,dupl=.true.,err=errdir)
       if (errdir%signal) then
           call errdir%add(' in directive shift_atom')
       else
           if (natom == 0) then
               do i=1,crystal%natoms()
                  call add_rigidb(crystal%rigidb,[i],1,.true.,.false.,val(1),0.0,0)
               enddo
           else
               do i=1,natom
                  call add_rigidb(crystal%rigidb,[vatom(i)],1,.true.,.false.,val(1),0.0,0)
               enddo
           endif
       endif
        
   elseif (word_is_contained(word,'RIGID_BODY')) then ! shift to atoms: At1 At2 ... tra rot
!              
!      find subdir rot(At) and remove it
       rot = 0
       call get_additional_dir(strdir,'rot',satom,pos,lens)
       if (pos > 0) then
           call get_atom_of_label(satom,crystal%at,rot)
           if (rot == 0) then
               call errdir%add(' in directive '//trim(strdir))
           endif
           call s_chop(strdir,pos,pos+lens-1)
       endif
       allocate(vatom(crystal%natoms()))
       call new_array(val,2)
       call get_atoms_from_string(strdir,crystal%at,vatom,natom,val,[0.5,30.0],nval,dupl=.true.,err=errdir)
       if (errdir%signal .or. natom == 0) then
           call errdir%add(' in directive '//trim(strdir))
       else
           if (natom == 1) then
               call add_rigidb(crystal%rigidb,vatom,natom,.true.,.false.,val(1),0.0,0)  ! val(2) is ignored
           else
               call add_rigidb(crystal%rigidb,vatom,natom,.true.,.true.,val(1),val(2),rot)
           endif
       endif

   elseif (word_is_contained(word,'ROTATE_AROUND_ATOM')) then ! rotate_around_axis
       call new_array(val,1)
       call get_atoms_from_string(strdir,crystal%at,ratom,natr,val,[-1.0],nval,err=errdir)
       if (errdir%signal) then
           call errdir%add(' in directive rotate_around_axis')
       else
           if (.not.conncalc) then
               call bond_to_connect(crystal%natoms(),crystal%bond,connect)
               conncalc = .true.
           endif
           rshift = val(1)
           nrot = crystal%numrot()
           call add_rotation(connect,nrot,crystal%rot,ratom(1:2),ratom(3:natr),crystal%at,crystal%cell,1,rshift)
       endif

   elseif (word_is_contained(word,'BUMP')) then ! set bump as: bump specie1 specie2 target_distance
       call bump_from_string(strdir,crystal%at,crystal%bond,crystal%cell,crystal%abres,    &
                             crystal%frag,crystal%rot,crystal%abset,ier)
       !if (ier /= 0) call errdir%set('Error in directive "'//trim(strdir0)//'"')
       if (ier /= 0) go to 10

   elseif (word_is_contained(word,'NOBUMP')) then ! set nobump as: nobump specie1 specie2
       call delete_restraints_from_string(strdir,crystal%abres,crystal%at,ABUMP,ier)
       !if (ier /= 0) call errdir%set('Error in directive "'//trim(strdir0)//'"')
       if (ier /= 0) go to 10

   elseif (word_is_contained(word,'BWEIGHT')) then
       call Getnum(strdir(:nlongs),vet,ivet,iv)
       if (iv > 0) then
!
!          Apply new scale to the exisisting anti-bump restraints
           if (crystal%numresab() > 0) then
               crystal%abres(:)%wei = vet(1)
           endif
           crystal%abset%weight =  vet(1)
       endif

   elseif (word_is_contained(word,'BSCALE')) then ! This deirective is applied to following anti-bump restraints
       call Getnum(strdir(:nlongs),vet,ivet,iv)
!!FIXME add negative value for reset to default - descrive in the documentation
       if (iv > 0) then
!!
!!          Apply new scale to the exisisting anti-bump restraints
!           if (crystal%numresab() > 0) then
!               crystal%abres(:)%targ = vet(1)*crystal%abres(:)%targ/crystal%abset%scaledist
!           endif
           crystal%abset%scaledist =  vet(1)
       endif

   elseif (word_is_contained(word,'BVCALC')) then    ! Calculate BVS and write output
       if (len_trim(strdir) > 0) then
           call fbv%fopen(strdir,'w')
           if (fbv%fail()) go to 10
           call bvs_crystal(crystal,1,fbv%handle(),ier)
           call fbv%fclose()
       else
           call bvs_crystal(crystal,1,stdout,ier)
       endif
       if (ier > 0) go to 10

   elseif (word_is_contained(word,'BVRES')) then     ! Set bv restraints: At1 val [sigma wei]
       call crystal%load_bvparam()
       call bvres_from_string(strdir(:nlongs),crystal%at,crystal%res,err=errdir)
       if (errdir%signal) then
           call errdir%add(' in directive "'//trim(strdir0)//'"')
       endif

   elseif (word_is_contained(word,'BVPAR')) then     ! Set bv parameters: El1 El2 Ro B [rmin rmax]
       call crystal%load_bvparam()
       call bvpar_from_string(strdir(:nlongs),crystal%bvpar,crystal%elem,errdir)
       if (errdir%signal) then
           call errdir%add(' in directive "'//trim(strdir0)//'"')
       endif

   elseif (word_is_contained(word,'PO')) then ! orientazione preferenziale
       call Getnum(strdir(:nlongs),vet,ivet,iv)
       if (iv == 3) then
!corr           crystal%kopcorr = 1
           crystal%gcode = 1
           crystal%hklpo(:) = ivet(1:3)
       endif

   elseif (word_is_contained(word,'TORSION')) then    ! At1 At2 At3 At4 Initial [Mode Min Max]
       call new_array(val,4)
       call get_atoms_from_string(strdir,crystal%at,ratom,natr,val,nval=nval,jolly=.false.,err=errdir)
       if (errdir%signal) then
           call errdir%add(' in directive "'//trim(strdir0)//'"')
       else
       endif

   elseif (word_is_contained(word,'REST')) then   ! restraints 
       call get_restraint_from_string(strdir,crystal%at,restemp,errdir,SIGD,WEID,inter)
       if (errdir%signal) then
           call errdir%add(' in directive "'//trim(strdir0)//'"')
       else
           call set_res_symmetry(restemp,crystal%bond,crystal%frag,inter)
           call add_restraint_to_list(crystal%res,restemp)
       endif

   elseif (word_is_contained(word,'RES_TARGET_TYPE')) then   ! target type for restraints 
       ier = 0
       call Getnum1(strdir,ivet=ivet1,iv=iv1)
       if (iv1 > 0) then
           if (ivet1(1) <= 0 .or. ivet1(1) > 3) then
               ier = 2
           else
               crystal%res_target_type = ivet1(1)
           endif
       else
           ier = 1
       endif
       if (ier /= 0) go to 10
       !if (ier > 0) then
       !    call errdir%set('Error in directive "'//trim(strdir0)//'"')
       !endif

   elseif (word_is_contained(word,'DELETEHYDRO',7)) then   ! delete H atoms 
       natold = crystal%natoms() 
       call remove_atoms_from_list(crystal%at,crystal%at%z(),H_at,crystal%bond)
       update = natold /= crystal%natoms()

   elseif (word_is_contained(word,'MOVE')) then   ! move At @(x y z) 
       kat = -1                                   ! Must be used after all %frag commands
       iv1 = 0
       call get_additional_dir(strdir,'@',scoord,pos,lens)
       if (pos > 0) then
           call Getnum1(scoord,vet=vet1,iv=iv1)
           if (iv1 == 3) then
               call s_chop(strdir,pos,pos+lens-1)
               call get_atom_of_label(strdir,crystal%at,kat)
           endif
       endif
       if (pos == 0 .or. kat <= 0 .or. iv1 /= 3) then
           call errdir%set('Error in directive "'//trim(strdir0)//'"')
           if (kat == 0) call errdir%add(". Undefined atom "//trim(strdir))
       else
           if (crystal%numfrag() == 0) call crystal%make_fragments()
           fpos = fragment_pos(crystal%frag,kat)
           if (fpos > 0) then
               call set_fragment_position(crystal%frag(fpos),vet1(:iv1),crystal%at,kat)
               update = .true.
           else  ! directive before %frag
               call errdir%set('Error in directive "'//trim(strdir0)//'"')
           endif
       endif
   else
       found = .false.
   endif

   return 
10 continue

   if (ier /= 0) then
       call errdir%set('Error in directive "'//trim(strdir0)//'"')
   endif
!
   end subroutine read_dir_frag_s

!-------------------------------------------------------------------------------------------------

   subroutine read_dir_frag_a(cmd,start,crystal,radtype,errdir,update)
   use errormod
   use crystal_phase
   use commandsmod
   type(command_type), intent(in)       :: cmd
   integer, intent(in)                  :: start    ! starting directive
   type(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                  :: radtype
   type(error_type), intent(out)        :: errdir
   logical, intent(out)                 :: update
   logical                              :: found
   integer                              :: i   !,natold
! 
   update = .false.
   if (crystal%natoms() == 0) return
!
   do i=start,cmd%ndir
      call read_dir_frag(cmd%strdir(i),crystal,radtype,errdir,found,update)
      if (errdir%signal) return
      if (.not.found) then
          call errdir%set('Error reading directive "'//trim(cmd%strdir(i))//'"')
          return
      endif
   enddo
!
   end subroutine read_dir_frag_a

!-------------------------------------------------------------------------------------------------

   subroutine read_dir_numfrag(dir,ndir,kfrag,ncopies,err)
!
!  Process directive that modify the number of atoms
!
   USE crystal_phase
   USE strutil
   USE fragmentmod
   USE errormod
   character(len=*), dimension(:), intent(in) :: dir
   integer, intent(in)                        :: ndir
   integer, intent(in)                        :: kfrag
   integer, intent(out)                       :: ncopies
   type(error_type), intent(out)              :: err
   integer                                    :: i,iv
   character(len=:), allocatable              :: sdir,word
   integer, dimension(:), allocatable         :: ivet
!
   ncopies = 1
   do i=1,ndir
      sdir = trim(dir(i))
      call cutsta(sdir,line2=word)
      if (word_is_contained(word,'numfrag')) then
          call getnum1(sdir,ivet=ivet,iv=iv)
          if (iv /= 2) go to 10
          if (ivet(1) == kfrag) then
              !if (ivet(2) == 1) cycle
              ncopies = ivet(2)
              !write(0,*)'Repeat frag. ',kfrag,ivet(2)
          endif
!corr          if (crystal%numfrag() == 0) call crystal%make_fragments()
!corr          if (ivet(1) > crystal%numfrag()) go to 10
!corr          if (ivet(2) == 0) then
!corr              crystal%frag(ivet(1))%nat = 0 ! set 0 to delete after
!corr          else
!corr              call duplicate_fragment(crystal%at,crystal%bond,crystal%elem,crystal%frag,ivet(1),ivet(2)-1)
!corr          endif
      endif
   enddo
!
!  delete fragment
!corr   i=0
!corr   do 
!corr      i = i + 1
!corr      if (crystal%frag(i)%nat == 0) call delete_fragment(crystal%at,crystal%bond,crystal%frag,i)
!corr      if (i == crystal%numfrag()) exit
!corr   enddo
!
   return
!
10 call err%set('Error reading directive '//sdir)
!
   end subroutine read_dir_numfrag

!-------------------------------------------------------------------------------------------------

   subroutine make_fragment(atom,legm,cell,spg,elem,itipo,kpos,kspec1i,kspec2i,kadd,bd,wave,radtype,nfrag)
   USE atom_type_util
   USE unit_cell
   USE spginfom
   USE connect_mod
   USE elements
   USE unit_cell
!
   type(atom_type), dimension(:), allocatable, intent(inout)    :: atom   
   type(bond_type), dimension(:), allocatable, intent(inout)    :: legm
   type(cell_type), intent(in)                                  :: cell
   type(spaceg_type), intent(in)                                :: spg
   type(element_type), dimension(:), allocatable, intent(inout) :: elem
   integer, intent(in)                                          :: itipo         ! tipo di frammento
   integer, intent(in)                                          :: kpos          ! posizione del frammento
   integer, intent(in)                                          :: kspec1i,kspec2i ! specie nel frammento
   logical                                                      :: kadd          ! true se il frammento va aggiunto in atom
   real, intent(in)                                             :: bd            ! distanza 
   real, intent(in)                                             :: wave
   integer, intent(in)                                          :: radtype
   integer, intent(in)                                          :: nfrag         ! number of fragments
   type(atom_type), allocatable, dimension(:)                   :: atom1
   integer                                                      :: i
   real, dimension(3)                                           :: xpos
   integer                                                      :: kkpos
   logical                                                      :: centr
   integer                                                      :: n1
   integer                                                      :: nat
   integer                                                      :: nadd,nlegadd
   real, dimension(3)                                           :: xrand
   logical                                                      :: addnew1,addnew2
   integer                                                      :: kspec1,kspec2
   integer                                                      :: newsize
   integer                                                      :: nf,nat0
!
   nat = numatoms(atom)
   nat0 = nat
!
!  If kspec is negative add new elements to the content (Z number = -kspec)
   if (kspec1i < 0 .or. kspec2i < 0) then
       addnew1 = .false.
       if (kspec1i < 0) then
!
!          set how many atoms to add
           select case (itipo)
               case (TETRA,SQUARE)     
                 newsize = 5
               case(OCTA,PRISM_TRIGONAL)
                 newsize = 7
               case(CUBE,APRISM_TETRA)
                 newsize = 9
               case(TRIG)
                 newsize = 4
               case(ICOSAHEDRON)
                 newsize = 13
               case(ISOLATED)
                 newsize = nfrag
           end select
           if (kpos > 0 .and. itipo /= ISOLATED) newsize = newsize - 1
!
           call add_element(elem,abs(kspec1i),wave,radtype,nw=newsize*spg%nsymop,add=addnew1)
           kspec1 = is_element(elem,abs(kspec1i)) 
       endif
       addnew2 = .false.
       if (kspec2i < 0) then
           call add_element(elem,abs(kspec2i),wave,radtype,nw=spg%nsymop,add=addnew1)
           kspec2 = is_element(elem,abs(kspec2i))  
       endif
   endif
   if (kspec1i > 0) kspec1 = kspec1i
   if (kspec2i > 0) kspec2 = kspec2i
!
!  Posizione del frammento
   if (kpos == 0) then
       call random_number(xpos)  ! in posizione random
       kkpos = -1
       centr = .true.
   else
       xpos = atom(kpos)%xc     ! nella posizione dell'atom kpos
       kkpos = kpos - nat      ! serve ad allineare la connettivita
       centr = .false.
   endif
!
!  crea il frammento
   select case(itipo)
       case(TETRA)              ! Tetrahedron
         call make_tetrahedron(atom1,bd,cell,xpos=xpos,kcentr=kkpos,centro=centr)

       case(OCTA)               ! Octahedron
         call make_octahedron(atom1,bd,cell,xpos=xpos,kcentr=kkpos,centro=centr)

       case(SQUARE)             ! Square Plane
         call make_square(atom1,bd,cell,xpos=xpos,kcentr=kkpos,centro=centr)

       case(ISOLATED)           ! Isolated Atoms
         if (kadd) then
             n1 = nat + 1
             nat = nfrag + nat
         else
             n1 = 1
             nat = nfrag
             call clear_bonds(legm)
         endif
         call resize_atoms(atom,nat)
         do i=n1,nat
            call random_number(atom(i)%xc)
            call atom(i)%set_specie_from_el(kspec1,elem)
         enddo
 
       case (CUBE)                ! Cube
         call make_cube(atom1,bd,cell,xpos)

       case (TRIG)                ! Trigonale Plane
         call make_trigonal(atom1,bd,cell,xpos)

       case (APRISM_TETRA)        ! Antiprism Tetragonal
         call make_anti_prism_tetragonal(atom1,bd,cell,xpos)

       case (PRISM_TRIGONAL)      ! Prism Trigonal
         call make_prism_trigonal(atom1,bd,cell,xpos)

       case (ICOSAHEDRON)         ! Icosahedron
         call make_icosahedron(atom1,bd,cell,xpos)

   end select
!
   if (itipo /= ISOLATED) then
       if (kpos == 0) then
           call atom1(1)%set_specie_from_el(kspec1,elem)
           call specie_from_el(atom1,kspec2,elem,2)
       else
           call specie_from_el(atom1,kspec2,elem)
       endif
!
!      combina i 2 modelli aggiungendo il nuovo frammento in fondo alla lista
       nadd = size(atom1)   ! numero atomi da aggiungere
       do nf=1,nfrag
!
!         Applica una rotazione random intorno al baricentro per kpos/=0 
!         In questo modo frammenti centrati sullo stesso atomo (kpos/=0) non vanno in sovrapposizione
          !if (itipo /= 4 .and. kpos /= 0) then
          if (kpos /= 0) then
              call random_number(xrand)
              call rand_rotate_atoms(atom1,xrand,cell)
          endif
!
!         Random translation for nf > 1
          if (nf > 1 .and. kpos == 0) then
              call random_number(xrand)
              call translate_atoms(atom1,xrand)
          endif
!
          if (.not.kadd .and. nf == 1) then
              call copy_atoms(atom,atom1)
              call clear_bonds(legm)
              nat = size(atom)
          else
              call add_atoms_to_list(atom,atom1,nat)
          endif
          n1 = nat - nadd + 1  ! puntatore al primo atomo aggiunto
          if (kpos == 0) then
              nlegadd = nadd - 1   ! numero legami da aggiungere
              call add_bonds(legm,atom,(/(n1,i=1,nlegadd)/),(/(i+n1,i=1,nlegadd)/),cell)
          else
              nlegadd = nadd       ! numero legami da aggiungere
              call add_bonds(legm,atom,(/(kpos,i=1,nlegadd)/),(/(i+n1-1,i=1,nlegadd)/),cell)
          endif
       enddo
   endif
!
!  Calcola occupanza cristallog. senza modificare le coordinate     
   if (kadd) then
       n1 = nat0 + 1
   else
       n1 = 1
   endif
   call calcola_occ(atom(n1:nat),spg,cell,modcoor=.false.)
   atom(n1:nat)%och = 1.0  ! inizializza occ. chimica
!
   call set_biso(atom(n1:nat))
!
!  Assegna intensita in base a Z
   call set_intensity(atom(n1:nat))
!          
   call atom_string(atom,elem,vet=(/(i,i=n1,nat)/))
!
   end subroutine make_fragment

!-------------------------------------------------------------------------------------------------
 
   subroutine parse_fragment_string(str,code,cform,dist,wave,radtype,strf,err) 
!
!  Parse string associated to command '%fragment'
!
   USE strutil
   USE elements
   USE errormod
   USE connect_mod
   character(len=*), intent(in)                               :: str
   integer, intent(out)                                       :: code
   real, intent(out)                                          :: dist
   real, intent(in)                                           :: wave
   integer, intent(in)                                        :: radtype
   character(len=*), intent(out)                              :: strf
   type(error_type)                                           :: err
   type(element_type), dimension(:), allocatable, intent(out) :: cform
   character(len=len_trim(str))                               :: str1,word
   integer                                                    :: ier,nlen2
   logical                                                    :: poly
   integer                                                    :: spec1,spec2
!
   code = 0
   spec1 = 0
   spec2 = 0
   dist = 0.0
!
   poly = .false.
   str1 = trim(str)
   call cutst(str1,line2=word,nlong2=nlen2)
   if (nlen2 == 0) return
   select case(lower(word))
      case ('tetra')
         code = TETRA
         poly = .true.
      case ('octa')
         code = OCTA
         poly = .true.
      case ('square')
         code = SQUARE
         poly = .true.
      case ('cube')
         code = CUBE
         poly = .true.
      case ('trigonal')
         code = TRIG
         poly = .true.
      case ('prism_tetra')
         code = APRISM_TETRA
         poly = .true.
      case ('prism_trig')
         code = PRISM_TRIGONAL
         poly = .true.
      case ('icosa')
         code = ICOSAHEDRON
         poly = .true.
      case ('atoms')
         code = ISOLATED
         call read_chemformula(str1,ier,cform)
         if (ier /= 0) goto 10
      case ('smiles')
         code = SMILES
         strf = trim(str1)
   end select
!
   if (poly) then
       call cutst(str1,line2=word)
       spec1 = z_from_specie(word)
       if (spec1 == 0) goto 10
       call add_element(cform,spec1,wave,radtype)
       call cutst(str1,line2=word)
       spec2 = z_from_specie(word)
       if (spec2 == 0) goto 10
       call add_element(cform,spec2,wave,radtype)
       if (len_trim(str1) > 0) then
           ier = s_to_r(str1,dist)
           if (ier /= 0) go to 10
       endif
       if (dist == 0.0) dist = bond_distanceZ(spec1,spec2)
   endif

   return

10 call err%set("Error reading directive: '"//trim(str)//"'")
!
   end subroutine parse_fragment_string

!-------------------------------------------------------------------------------------------------

   subroutine import_from_smiles(smiles_string, atom, legm, elem, nsymtot, wave, radtype, cell, err)
   USE atom_type_util
   USE OBmodule
   USE elements
   USE errormod
   USE unit_cell
#ifdef MPI
   USE mpi_prog
   integer                                                      :: nat, nbonds
#endif
   character(len=*), intent(in)                                 :: smiles_string
   type(atom_type), dimension(:), allocatable, intent(out)      :: atom
   type(bond_type), dimension(:), allocatable, intent(out)      :: legm
   type(element_type), dimension(:), allocatable, intent(inout) :: elem  
   real, intent(in)                                             :: wave
   integer, intent(in)                                          :: nsymtot, radtype
   type(cell_type), intent(in)                                  :: cell
   type(error_type), intent(out)                                :: err
!
#ifdef MPI
   if (mpi_prog_rank == mpi_prog_master) then
       call OBConversionFromSmiles(smiles_string,atom,legm,err)
       nat = numatoms(atom)
       nbonds = numbonds(legm)
   endif
   call get_data(err%signal)
   if (err%signal) return
   call get_data(nat)
   call get_data(nbonds)
   if (mpi_prog_rank /= mpi_prog_master) then
       call new_atoms(atom,nat)
       call new_bonds(legm,nbonds)
   endif
   call get_data(atom%ptab)
   call get_data(atom%xc(1))
   call get_data(atom%xc(2))
   call get_data(atom%xc(3))
   if (nbonds > 0) then
       call get_data(legm%n1)
       call get_data(legm%n2)
       call get_data(legm%dist)
   endif
#else
   call OBConversionFromSmiles(smiles_string,atom,legm,err)
   if (err%signal) return
#endif
!
   call make_elements(elem,atom(:)%ptab,wave,radtype,.true.,nsymtot)  ! update content
   call specie_from_ptab(atom,elem)
   call atom_string(atom,elem)                         ! label
   call set_biso(atom)                                 ! adp
   call set_intensity(atom)                            ! int.
   call cart_to_frac(atom,cell)
   !call print_atoms(atom,kpr=mpi_prog_rank+70)
   !call print_connect(atom%lab, legm, kpri=mpi_prog_rank+70)
!
   end subroutine import_from_smiles

!-------------------------------------------------------------------------------------------------

   subroutine get_smiles(smiles_string,atom,legm,elem,nsymtot,wave,radtype,cell,add,err)
   USE atom_type_util
   USE elements
   USE errormod
   USE unit_cell
   USE connect_mod
   character(len=*), intent(in)                                 :: smiles_string
   type(atom_type), dimension(:), allocatable, intent(inout)    :: atom
   type(bond_type), dimension(:), allocatable, intent(inout)    :: legm
   type(element_type), dimension(:), allocatable, intent(inout) :: elem  
   real, intent(in)                                             :: wave
   integer, intent(in)                                          :: nsymtot, radtype
   type(cell_type), intent(in)                                  :: cell
   logical, intent(in)                                          :: add
   type(error_type), intent(out)                                :: err
   type(atom_type), dimension(:), allocatable                   :: atom1
   type(bond_type), dimension(:), allocatable                   :: legm1
   integer                                                      :: natnew,natcurr
!
   call import_from_smiles(smiles_string,atom1,legm1,elem,nsymtot,wave,radtype,cell,err)
   if (err%signal) return
   if (add) then  ! add smiles to current model
       natcurr = numatoms(atom)
       call add_atoms_to_list(atom,atom1,natnew,legm1=legm,legm2=legm1)
       call remove_duplicate_labels(atom)
   else
       call copy_atoms(atom,atom1)
       call copy_bonds(legm,legm1)
   endif
!
   end subroutine get_smiles

!-------------------------------------------------------------------------------------------------------  

   subroutine make_powder_pattern_var(xc,yc,ref,cryst,thmin,thmax,thstep, &
                                      wave,ratio,radtype,anomal,fwhm,scal,&
                                      wei,back,ftype,nshape_par,shape_par,err)
   USE atom_basic
   USE calculate_pdp
   USE reflection_type_util
   USE pearsonf
   USE psvoigtf
   USE tchzf
   USE profile_function
   USE arrayutil
   USE progtype, only: refine_condition_type
   USE errormod
   USE crystal_phase
   USE nrutil
   real, dimension(:), allocatable, intent(out)                    :: xc,yc
   type(reflection_type), dimension(:), allocatable, intent(inout) :: ref
   type(crystal_phase_t), dimension(:), allocatable, intent(in)    :: cryst
   real, intent(in)                                                :: thmin,thmax,thstep
   real, dimension(:), intent(in)                                  :: wave,ratio
   integer, intent(in)                                             :: radtype
   logical, intent(in)                                             :: anomal
   real, intent(in)                                                :: fwhm,scal,back
   real, dimension(:), intent(in)                                  :: wei
   integer, intent(in)                                             :: ftype
   integer, intent(in)                                             :: nshape_par
   real, dimension(:), intent(in)                                  :: shape_par
   type(error_type), intent(out)                                   :: err
   type(crystal_phase_t)                                           :: crystal
   integer                                                         :: nc
   real                                                            :: tmax
   type(profile_function_t)                                        :: pear
   type(profile_function_t)                                        :: pvoi
   type(profile_function_t)                                        :: tch
   integer                                                         :: ier,nph,i
   real                                                            :: thmin0,thmax0,ttmin,ttmax
   real                                                            :: zmv, maxyc
!
   if (numphase(cryst) == 0) go to 10
!
!  Check range for phase without structure, reflection are not generated and h,k,l,fo are expected
   thmin0 = thmin
   thmax0 = thmax
   do nph=1,numphase(cryst)
      if (cryst(nph)%natoms() == 0) then
          ttmin = minval(cryst(nph)%ref%tthd(1))
          ttmax = maxval(cryst(nph)%ref%tthd(1))
          if (thmin0 < ttmin) thmin0 = ttmin
          if (thmax0 > ttmax) thmax0 = ttmax
      endif
   enddo
!
!  Make series tmin,tstep 
   nc = floor((thmax0 - thmin0) / thstep + 1)   ! number of counts
   call new_array(xc,nc)
   xc = arth(thmin,thstep,nc)
   tmax = xc(nc)
   call new_array(yc,nc)
!
   if (nshape_par > 1) then
       call init_function_param(pear,pvoi,tch,fwhm,par=shape_par)
   else
       call init_function_param(pear,pvoi,tch,fwhm)
   endif
   call pd_phase_init(numphase(cryst))
   yc = 0
   do nph=1,numphase(cryst)
      call crystal%set(cryst(nph)%at,spg=cryst(nph)%spg,cell=cryst(nph)%cell,elem=cryst(nph)%elem)
!FIXME: set_radtype should be necessary to manage radtype
!
      if (crystal%natoms() > 0) then
          call crystal%make_reflections(thmin0,tmax,wave)
      else
          call crystal%set_reflections(cryst(nph)%ref)
      endif
      if (crystal%numref() > 0) then
          if (cryst(nph)%natoms() > 0) then
              call crystal%sfactor(radtype,anomal)
          else
              do i=1,min(cryst(nph)%numref(),crystal%numref())
                 crystal%ref(i)%fc = crystal%ref(i)%fo
              enddo
          endif
          call crystal%LPcorr(0,.false.,size(wave))
!
          call pd_ref_init(nph,crystal%numref(),size(wave))
          call new_array(crystal%yc,nc)
          call calculate_powder_pattern(crystal%ref%fc,nc,xc,crystal%yc,crystal%ref,crystal%numref(),  &
                                        size(wave),ratio,pear,pvoi,tch,ftype,nph,ier)
          if (nph == 1) call crystal%get_reflections(ref)

          if (nph <= size(wei)) then
              if (wei(nph) >= 0.0) then
                  call crystal%set_density()
                  zmv = crystal%dens*crystal%cell%volume()**2
                  crystal%scal = wei(nph)/zmv
              endif
          endif

          yc = yc + crystal%scal*crystal%yc
      else
          go to 10
      endif
   enddo
   maxyc = maxval(yc)
   if (abs(maxyc) > epsilon(1.0)) then
       if (scal > back) then
           yc = (scal - back) * yc/ maxyc  + back
       else
           yc = scal * yc/ maxyc  + back
       endif
   endif

   return
10 call err%set('Error generating powder pattern')
!
   end subroutine make_powder_pattern_var

!--------------------------------------------------------------------------------------

   subroutine make_powder_pattern_xpd(xc,yc,ref,cryst,xpd,wei,err)
   USE reflection_type_util
   USE errormod
   USE crystal_phase
   USE elements, only: RX_SOURCE
   real, dimension(:), allocatable, intent(out)                    :: xc,yc
   type(reflection_type), dimension(:), allocatable, intent(inout) :: ref
   type(crystal_phase_t), dimension(:), allocatable, intent(in)    :: cryst
   type(xpd_option_type), intent(in)                               :: xpd
   real, dimension(:), intent(in)                                  :: wei
   type(error_type)                                                :: err
   logical                                                         :: anomal = .false.

   call make_powder_pattern_var(xc,yc,ref,cryst,xpd%ttmin,xpd%ttmax,   &
                                xpd%ttstep,xpd%wave(:xpd%nwave),xpd%ratio(:xpd%nwave),RX_SOURCE, &
                                anomal,xpd%fwhm,xpd%scal,wei,xpd%back,xpd%ftype, &
                                xpd%nshape_par,xpd%shape_par,err)

   end subroutine make_powder_pattern_xpd

!--------------------------------------------------------------------------------------

   subroutine make_powder_pattern_cmd(cmd,cryst,err)
   USE commandsmod
   USE reflection_type_util
   USE errormod
   USE crystal_phase
   USE strutil
   USE errormod
   USE filereading
   USE profile_function, only: PEARSON7, PVOIG, TCHZ
   type(command_type), intent(in)                               :: cmd
   type(crystal_phase_t), intent(in), dimension(:), allocatable :: cryst
   type(error_type), intent(out)                                :: err
   real, dimension(:), allocatable                              :: xpd,ypd
   type(reflection_type), dimension(:), allocatable             :: refpd
   type(xpd_option_type)                                        :: xpdopt
   character(len=:), allocatable                                :: strdir,word,file_name
   real, dimension(:), allocatable                              :: vet
   integer                                                      :: iv,i
   real, dimension(:), allocatable                              :: wei
!
   if (cmd%ndir == 0) return

   do i=1,cmd%ndir
      strdir = cmd%strdir(i)
      call cutsta(strdir,word)
      if (word_is_contained(word,'background')) then
          if (str_set_real(strdir, xpdopt%back, 0.0)) go to 10

      elseif (word_is_contained(word,'fwhm')) then
          if (str_set_real(strdir, xpdopt%fwhm, 0.0)) go to 10

      elseif (word_is_contained(word,'scale')) then
          if (str_set_real(strdir, xpdopt%fwhm, 0.0)) go to 10

      elseif (word_is_contained(word,'range')) then
          call getnum1(strdir,vet=vet,iv=iv)
          select case(iv)
            case (1)
              xpdopt%ttmin = vet(1)
            case (2)
              xpdopt%ttmin = vet(1)
              xpdopt%ttmax = vet(2)
            case default
              go to 10
          endselect

      elseif (word_is_contained(word,'profile')) then
          select case(trim(adjustl(strdir)))
          case ('pears')
            xpdopt%ftype = PEARSON7
          case ('p-v')
            xpdopt%ftype = PVOIG
          case ('tchz')
            xpdopt%ftype = TCHZ
          case default
            go to 10
          endselect

      elseif (word_is_contained(word,'shapepar')) then
          call getnum1(strdir,vet=vet,iv=iv)
          if (iv == 0 .or. iv > size(xpdopt%shape_par)) go to 10
          xpdopt%shape_par(:iv) = vet(:iv)
          xpdopt%nshape_par = iv
          
      elseif (word_is_contained(word,'weight')) then
          call getnum1(strdir,vet=vet,iv=iv)
          if (iv > 0) then
              allocate(wei(iv),source=vet(:iv))
              wei(:) = wei(:) / sum(wei)
          else
              go to 10
          endif

      else
          if (i==1) then
              file_name = word
          else
              go to 10
          endif
      endif
   enddo

   if (.not.allocated(wei)) allocate(wei(1),source=-1.0)
   call make_powder_pattern(xpd,ypd,refpd,cryst,xpdopt,wei,err)
   if (.not.err%signal) &
       call export_profile(file_name,xpd,ypd,string='#      2theta    yoss')  
   return
!
10 call err%set("Error reading directive "//trim(cmd%strdir(i)))
!
   end subroutine make_powder_pattern_cmd

!----------------------------------------------------------------------------------------------------

   subroutine load_chemical_tables(fpath,err)
   USE elements
   USE bondtmod
   USE bond_valence
   USE errormod
   USE spginfom
   character(len=*), intent(in)  :: fpath
   type(error_type), intent(out) :: err
!
   call read_chemical_elements(filexen_name(fpath),err)
   if (err%signal) return
   call spg_set_symmetry_file(trim(fpath)//'syminfo.lib',err)
   if (err%signal) return
   call load_spg_database(err)
   if (err%signal) return
   call load_bond_table()
   call load_bval_table()
!
   end subroutine load_chemical_tables

!----------------------------------------------------------------------------------------------------

   subroutine crystal_find_contacts(crystal,atoms,bonds,lsym,expand,hbond,shbond)
   USE crystal_phase
   USE contacts
   type(crystal_phase_t), intent(in)                                    :: crystal
   type(atom_type), dimension(:), allocatable, intent(inout)            :: atoms  ! atoms with symmetry
   type(bond_type), dimension(:), allocatable, intent(inout)            :: bonds  ! bonds with simmetria
   logical, intent(in)                                                  :: lsym   ! packing 
   integer, dimension(2), intent(in)                                    :: expand ! expand contacts
   type(bond_type), dimension(:), allocatable, intent(inout), optional  :: hbond  ! H-bonds
   type(bond_type), dimension(:), allocatable, intent(inout), optional  :: shbond ! short contacts
   type(bond_type), dimension(:), allocatable                           :: shbond1,hbond1
   logical                                                              :: lhbond,lshbond
!
   lhbond = present(hbond)
   lshbond = present(shbond)
   if (lhbond .and. lshbond) then
       call find_contacts(crystal%at,crystal%bond,crystal%cell,crystal%spg,hbond,shbond,atoms,bonds,  &
                          lsym,expand,lhbond,lshbond,1)
   else
       if (lhbond) then
           call find_contacts(crystal%at,crystal%bond,crystal%cell,crystal%spg,hbond,shbond1,atoms,bonds,  &
                              lsym,expand,lhbond,lshbond,1)
       elseif (lshbond) then
           call find_contacts(crystal%at,crystal%bond,crystal%cell,crystal%spg,hbond1,shbond,atoms,bonds,  &
                              lsym,expand,lhbond,lshbond,1)
       endif
   endif
!
   end subroutine crystal_find_contacts

!----------------------------------------------------------------------------------------------------

   subroutine pdf_command(cmd,crystal,err)
   use commandsmod
   use pdfcalc
   use crystal_phase
   use cryutil
   use strutil
   use filereading
   use errormod
   type(command_type), intent(in)    :: cmd
   type(crystal_phase_t), intent(in) :: crystal
   type(error_type), intent(out)     :: err
   character(len=:), allocatable     :: filename
   character(len=:), allocatable     :: strdir,word
   type(pdf_type)                    :: pdf
   real                              :: qmax,rmax         !,rnum
   integer                           :: i,istart,radtype  !,ier
   real, dimension(:), allocatable   :: x,y
!
   filename = trim(crystal%cr_name)//"_pdf.xy"
   radtype = RX_SOURCE
   rmax = 10.0
   qmax = 25.0
!
   if (cmd%ndir > 0) then
       istart = 1
       if (cmd%comb) then
           filename = cmd%strdir(1)
           istart = 2
       endif
       do i=istart,cmd%ndir
          strdir = cmd%strdir(i)
          call cutsta(strdir,word)
          if (word_is_contained(word,'radiation')) then
              radtype = string_to_radtype(strdir)
              if (radtype < 0) go to 10          
          else if (word_is_contained(word,'qmax')) then
              if (s_to_r(strdir,qmax) /= 0) go to 10
          else if (word_is_contained(word,'rmax')) then
              if (s_to_r(strdir,rmax) /= 0) go to 10
          endif
       enddo
   endif
!
   pdf%rmax = rmax
   pdf%qmax = qmax
   call pdf_crystal(pdf,crystal,radtype)
   call pdf%save_file(x,y)
   call write_column(filename=filename,xcol1=x,xcol2=y)
   return
!
10 call err%set("Error reading directive "//trim(cmd%strdir(i)))
!
   end subroutine pdf_command

end module gen_frm
