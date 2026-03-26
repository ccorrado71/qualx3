MODULE spginfom

!S spg_load(spaceg,spgnum,spgstr,symop,sfound,code)               Load space group  from number or from symbol or from symmetry operators
!S prn(this,kpri)                                                 Print space group properties
!S prn_shelx(this,kpr)                                            Generate crystal data for SHELX file
!S prn_cif(this,kpr)                                              Generate crystal data in CIF format
!S prn_symop(this,code,kpr)                                       Print symmetry operators.
!S prn_origins(this,kpri)                                         Print list of equivalent origins
!S init(this)                                                     Initialize spaceg_type class
!S set_p1(this)                                                   Set space group as P 1
!S set_from_symop(this,symop,symbol_xhm,symbol_hall)              Set space group from symmetry operators
!S spg_set_symmetry_file(spgfile,err)                             Change default for symmetry file name
!S spg_set_string(spgr)                                           Prepare space group input string for search on symmetry table
!S spg_set_cell(spg,cell)                                         Set cell parameters according the crystal system
!S spg_get_cell_code(spg,code)                                    Set refinement code for cell parameters according the crystal system
!F uc_is_rhombohedral(cell,tol)                                   Check for rhombohedral axes
!F uc_is_hexagonal(cell,tol)                                      Check for hexagonal axes
!S find_strspace(line,strfind,is_colon,jfile,sgnum,sfound,spaceg) Try to match input symbol with symbols on symmetry table
!F is_colon_in_spgn(spgn)                                         Check for colon in the space group symbol
!S spg_openfile(filename,j_in,ier)                                Open symmetry file
!S spg_read_block(funit,spaceg,readsymbol,err)                    Read info from syminfo.lib and fill the spaceg class
!F laue_code_from_name(laue_string) result(code)                  Get code of laue class
!F laue_class(this)                                               Get name of laue class
!F pgroup_code_from_name(pgroup_string) result(code)              Get code of point group 
!F point_group(this)                                              Get point group name
!F csys_code_from_num(num) result(code)                           Code of crystal system from number
!S string_to_symop(strop,symop,ier)                               Convert symbol to symmetry operator
!S symop_to_string(symop,strop)                                   Convert symmetry operator in symbol
!S symop_norm_trans(symop)                                        Normalize translation of symmetry operator
!S calc_origins(spaceg)                                           Return the number of alternate origins for spacegroup
!S get_indipendent_origins(spaceg)                                Get the indipendend origins removing the redundant origins
!F get_spg_info(hm_symbol,info)                                   Find number and simboli a partire dall' H-M symbol
!S range_to_limits(srange,limit,equal)                            Convert string of type 0<=y<=1/4 in limit = (/0,0.25/) equal = (/1,1/)
!F symop_equal(symop1,symop2)                                     Compare two symmetry operators
!F symop_equal_sets(nsym1, symop1, nsym2, symop2)                 Compare an entire set of symmetry operators
!F spg_equal(spg1,spg2)                                           Check if spg1 and spg2 are equal
!F lattice_type(coper,ncoper)                                     Get lattice symbol from centering operators
!F lattice_symbol(spaceg)                                         Extract the lattice symbol
!F rhomb_axes(spg,cell)                                           Check for rhombohedral axes
!S symop_gen(symop,nsym,icent,latt)                               Symmetry operators from operators without inversion and centering
!F spg_check_cell(spg,cell) result(stat)                          Check for consistency between cell dimensions and spacegroup. Latter is identified from symmetry operators. 
!F spg_check_consistency(spg,symb_hall,symb_xhm)  result(err)     Check consistency between operators and symbols
!F find_symbol(xhm,hall) result(num)                              Look up symbols in the dictionary
!F polar                                                          True if polar axis is present
!S code_for_polar(spg,code)                                       Assign zero for code corresponding to polar direction
!F integer function lattice_system(spg,cell)  result(jsys)        Crystal system + axis 
!S save_space_bin(unitbin,space)                                  Write space group on binary file
!S read_space_bin(unitbin,err)                                    Read space group from binary file
!S print_spg_freq(cs,kpr)                                         Print for crystal system cs list of space groups sorted for frequency
!S make_frequency(freq,kpr)                                       Call this subroutine to create info about frequency of spg
!F is_chiral(spg)                                                 Is spg chiral?
!F get_extinction_symbol(spg)                                     Get extinction symbol
!F csys_from_exts(ext_symbol)                                     Get crystal system from extinction symbol
!S print_extsymb(kpr)                                             Print extinction symbols for each crystal system
!F z_from_spg(spgnum)                                             Z from spg number
!F is_standardize(spg)                                            Check if space group can be standardized

   USE symm_table
   USE arrayutil, only: container_type

   implicit none 

   type symop_type
        integer, dimension(3,3) :: rot   ! rotation part
        real, dimension(3)   :: trn   ! translation part
   end type symop_type

   integer, parameter, private :: NSLEN=40

   type spaceg_type
        integer                          :: num                  ! spacegroup number
        character(len=40)                :: symbol_hall = ''     ! Hall symbol
        character(len=40)                :: symbol_xhm  = ''     ! Hermann Maugin symbol
        character(len=40), dimension(2)  :: symbol_alt  = ''     ! Alternative symbols
        character(len=6)                 :: symbol_sch  = ''     ! Schoenflies notation
        character(len=8)                 :: symbol_patt = ''     ! Patterson Spacegroup
        character(len=8)                 :: choice      = ''     ! Table setting choice

        integer                          :: nsymop               ! Multiplicity: total number of symmetry operators 
        integer                          :: nsymop_prim          ! Number of primitive symmetry operators (inversion included)
        integer                          :: nsym                 ! Number of primitive symmetry operators (inversion excluded)
        type(symop_type), dimension(192) :: symop                ! symmetry matrices
        character(NSLEN), dimension(192) :: symopstr             ! symmetry matrix symbols
        integer                          :: ncoper               ! number of centering operators
        real, dimension(3,4)             :: coper                ! centering operators
        character(len=1)                 :: lattyp               ! lattice type

        integer                          :: laue_code            ! code for laue class
        integer                          :: pgroup_code          ! code for point group
        integer                          :: symcent              ! 0=Acentric  
                                                                 ! 1=Centric (-1 at origin)  
                                                                 ! 2=Centric (-1 not at origin)
        integer                          :: csys_code            ! crystal System code

        integer                          :: norig                ! number of origins
        real, dimension(3,16)            :: orig                 ! array of origin (ridundant origins included)
        integer                          :: norig_ind            ! number of indipendent origins
        real, dimension(3,8)             :: orig_ind             ! array of origin (no ridundant origins included)
        logical                          :: polarx,polary,polarz ! true if is polar along x,y,z axis

        real, dimension(3)               :: asulim               ! origin-based map asu: upper limits
        integer, dimension(3)            :: asueq                ! lim strettamente uguali?
        character(len=80)                :: asu_descr            ! origin-based map asu: description from file

        real, dimension(3)               :: chelim               ! Cheshire cell
        character(len=80)                :: che_descr            ! Cheshire cell: description from file
        logical                          :: defined = .false.    ! false if the space group was not found after search in the syminfo.lib
        integer                          :: freq_no=0            ! Frequency as number of occurencies
        real                             :: freq_perc=0.         ! Frequency as percentage
        integer                          :: freq_rank=0          ! Rank
        logical                          :: standard = .false.   ! True for standard choice of origin
        integer                          :: standard_id = 0      ! pointer to spg_data for standard space group
        character(len=46)                :: pmat = ' '           ! Transformation matrix to non-conventional setting
        character(len=17)                :: pmat1 = ' '          ! Transformation matrix to standard setting
     
        integer                          :: id = -1              ! Index for database of spacegroups
        
   contains

        procedure :: init => init_spg
        procedure :: prn
        procedure :: laue_class
        procedure :: point_group
        procedure :: prn_symop
        procedure :: prn_symop_formatted
        procedure :: set_p1
        procedure :: set_from_symop
        generic   :: polar => polar3, polar1
        procedure :: code_for_polar
        procedure :: undef
        procedure :: axis_direction
        procedure :: is_chiral
        procedure :: extinction_symbol => get_extinction_symbol
        procedure :: spg_frequency
        procedure :: get_pmat
        procedure :: get_pmat1
        procedure :: get_id
        procedure :: id_setting

        procedure, private :: polar3
        procedure, private :: polar1

   end type spaceg_type

   interface init_spaceg_type    
      module procedure constructor
   end interface

   private spg_set_cell_ccs, spg_set_cell_sc
   interface spg_set_cell
      module procedure spg_set_cell_ccs, spg_set_cell_sc    !!!!, spg_set_cell_cc
   end interface spg_set_cell

   private get_spg_info_hm, get_spg_info_num
   interface get_spg_info
      module procedure get_spg_info_hm, get_spg_info_num
   end interface

   character(len=256), private :: spg_filename = 'syminfo.lib'

   character(len=*), dimension(0:13), parameter, private :: laue_name=[ &
   'Unknown', '-1     ','2/m    ','mmm    ','4/m    ','4/mmm  ','-3m    ',   &
   '-3     ', '-3m1   ','-31m   ','6/m    ','6/mmm  ','m-3    ','m-3m   ' ]

   character(len=*), dimension(0:40), parameter, private :: pgroup_name=  [   &
   'Unknown', '1      ', '-1     ', '2      ', 'm      ', '2/m    ', '222    ', 'mm2    ',  &
   'mmm    ', '4      ', '-4     ', '4/m    ', '422    ', '4mm    ', '-42m   ', '-4m2   ',  &
   '4/mmm  ', '3      ', '-3     ', '321    ', '312    ', '32     ', '3m1    ', '31m    ',  &
   '3m     ', '-3m1   ', '-31m   ', '-3m    ', '6      ', '-6     ', '6/m    ', '622    ',  &
   '6mm    ', '-6m2   ', '-62m   ', '6/mmm  ', '23     ', 'm-3    ', '432    ', '-43m   ',  &
   'm-3m   ' ]

   character(len=* ), dimension(7) , parameter :: cry_sys = [   &
   "Triclinic   ","Monoclinic  ","Orthorhombic","Tetragonal  ",         &
   "Trigonal    ","Hexagonal   ","Cubic       " ]

   integer, parameter :: CS_Triclinic=1, CS_Monoclinic=2, CS_Orthorhombic=3, CS_Tetragonal=4,  &
                         CS_Trigonal=5, CS_Hexagonal=6, CS_Cubic=7

   integer, parameter :: RHOMB_LATT=7, HEXA_LATT=5
   character(len=* ), dimension(230) , parameter, private :: symbol_sch = (/   &                            
   'C1^1  ', 'Ci^1  ', 'C2^1  ', 'C2^2  ', 'C2^3  ', 'Cs^1  ', 'Cs^2  ', 'Cs^3  ', 'Cs^4  ', 'C2h^1 ',     &
   'C2h^2 ', 'C2h^3 ', 'C2h^4 ', 'C2h^5 ', 'C2h^6 ', 'D2^1  ', 'D2^2  ', 'D2^3  ', 'D2^4  ', 'D2^5  ',     &
   'D2^6  ', 'D2^7  ', 'D2^8  ', 'D2^9  ', 'C2v^1 ', 'C2v^2 ', 'C2v^3 ', 'C2v^4 ', 'C2v^5 ', 'C2v^6 ',     &
   'C2v^7 ', 'C2v^8 ', 'C2v^9 ', 'C2v^10', 'C2v^11', 'C2v^12', 'C2v^13', 'C2v^14', 'C2v^15', 'C2v^16',     &
   'C2v^17', 'C2v^18', 'C2v^19', 'C2v^20', 'C2v^21', 'C2v^22', 'D2h^1 ', 'D2h^2 ', 'D2h^3 ', 'D2h^4 ',     &
   'D2h^5 ', 'D2h^6 ', 'D2h^7 ', 'D2h^8 ', 'D2h^9 ', 'D2h^10', 'D2h^11', 'D2h^12', 'D2h^13', 'D2h^14',     &
   'D2h^15', 'D2h^16', 'D2h^17', 'D2h^18', 'D2h^19', 'D2h^20', 'D2h^21', 'D2h^22', 'D2h^23', 'D2h^24',     &
   'D2h^25', 'D2h^26', 'D2h^27', 'D2h^28', 'C4^1  ', 'C4^2  ', 'C4^3  ', 'C4^4  ', 'C4^5  ', 'C4^6  ',     &
   'S4^1  ', 'S4^2  ', 'C4h^1 ', 'C4h^2 ', 'C4h^3 ', 'C4h^4 ', 'C4h^5 ', 'C4h^6 ', 'D4^1  ', 'D4^2  ',     &
   'D4^3  ', 'D4^4  ', 'D4^5  ', 'D4^6  ', 'D4^7  ', 'D4^8  ', 'D4^9  ', 'D4^10 ', 'C4v^1 ', 'C4v^2 ',     &
   'C4v^3 ', 'C4v^4 ', 'C4v^5 ', 'C4v^6 ', 'C4v^7 ', 'C4v^8 ', 'C4v^9 ', 'C4v^10', 'C4v^11', 'C4v^12',     &
   'D2d^1 ', 'D2d^2 ', 'D2d^3 ', 'D2d^4 ', 'D2d^5 ', 'D2d^6 ', 'D2d^7 ', 'D2d^8 ', 'D2d^9 ', 'D2d^10',     &
   'D2d^11', 'D2d^12', 'D4h^1 ', 'D4h^2 ', 'D4h^3 ', 'D4h^4 ', 'D4h^5 ', 'D4h^6 ', 'D4h^7 ', 'D4h^8 ',     &
   'D4h^9 ', 'D4h^10', 'D4h^11', 'D4h^12', 'D4h^13', 'D4h^14', 'D4h^15', 'D4h^16', 'D4h^17', 'D4h^18',     &
   'D4h^19', 'D4h^20', 'C3^1  ', 'C3^2  ', 'C3^3  ', 'C3^4  ', 'C3i^1 ', 'C3i^2 ', 'D3^1  ', 'D3^2  ',     &
   'D3^3  ', 'D3^4  ', 'D3^5  ', 'D3^6  ', 'D3^7  ', 'C3v^1 ', 'C3v^2 ', 'C3v^3 ', 'C3v^4 ', 'C3v^5 ',     &
   'C3v^6 ', 'D3d^1 ', 'D3d^2 ', 'D3d^3 ', 'D3d^4 ', 'D3d^5 ', 'D3d^6 ', 'C6^1  ', 'C6^2  ', 'C6^3  ',     &
   'C6^4  ', 'C6^5  ', 'C6^6  ', 'C3h^1 ', 'C6h^1 ', 'C6h^2 ', 'D6^1  ', 'D6^2  ', 'D6^3  ', 'D6^4  ',     &
   'D6^5  ', 'D6^6  ', 'C6v^1 ', 'C6v^2 ', 'C6v^3 ', 'C6v^4 ', 'D3h^1 ', 'D3h^2 ', 'D3h^3 ', 'D3h^4 ',     &
   'D6h^1 ', 'D6h^2 ', 'D6h^3 ', 'D6h^4 ', 'T^1   ', 'T^2   ', 'T^3   ', 'T^4   ', 'T^5   ', 'Th^1  ',     &
   'Th^2  ', 'Th^3  ', 'Th^4  ', 'Th^5  ', 'Th^6  ', 'Th^7  ', 'O^1   ', 'O^2   ', 'O^3   ', 'O^4   ',     &
   'O^5   ', 'O^6   ', 'O^7   ', 'O^8   ', 'Td^1  ', 'Td^2  ', 'Td^3  ', 'Td^4  ', 'Td^5  ', 'Td^6  ',     &
   'Oh^1  ', 'Oh^2  ', 'Oh^3  ', 'Oh^4  ', 'Oh^5  ', 'Oh^6  ', 'Oh^7  ', 'Oh^8  ', 'Oh^9  ', 'Oh^10 ' /)     

   integer, dimension(3,3), parameter :: identity_mat = reshape((/1, 0, 0, &
                                                               0, 1, 0, &
                                                               0, 0, 1 /),(/3,3/))

   character (len=*),dimension(0:2), parameter, private  :: strcentr = &
                                          (/ 'Acentric                  ', &
                                             'Centric (-1 at origin)    ', &
                                             'Centric (-1 not at origin)' /)


   real, dimension(3,2), parameter :: Ltr_a =reshape ( (/0.0,0.0,0.0, 0.0,0.5,0.5/), (/3,2/) )        !type A
   real, dimension(3,2), parameter :: Ltr_b =reshape ( (/0.0,0.0,0.0, 0.5,0.0,0.5/), (/3,2/) )        !type B
   real, dimension(3,2), parameter :: Ltr_c =reshape ( (/0.0,0.0,0.0, 0.5,0.5,0.0/), (/3,2/) )        !type C
   real, dimension(3,4), parameter :: &
         Ltr_f =reshape( (/0.0,0.0,0.0, 0.0,0.5,0.5, 0.5,0.0,0.5, 0.5,0.5,0.0 /),(/3,4/) )            !type F
   real, dimension(3,2), parameter :: Ltr_i =reshape ( (/0.0,0.0,0.0, 0.5,0.5,0.5/), (/3,2/) )        !type I
   real, dimension(3,3), parameter :: &
         Ltr_r =reshape( (/0.0,0.0,0.0, 2.0/3.0,1.0/3.0,1.0/3.0,1.0/3.0, 2.0/3.0, 2.0/3.0/),(/3,3/) ) !type R

   private find_strspace

   integer, parameter, private :: NSPGTOT   = 551
   integer, parameter, private :: NUMSPGMAX = 230
   type(spaceg_type), dimension(NSPGTOT), private      :: spg_data
   type(container_type), dimension(NUMSPGMAX), private :: spg_index

CONTAINS

   subroutine spg_load(spaceg,spgnum,spgstr,symop,sopt,sfound,code)
!
!  Load space group  from number or from symbol or from symmetry operators
!
   USE strutil
   type(spaceg_type), intent(out)                       :: spaceg    ! properties of space group
   integer, intent(in), optional                        :: spgnum    ! space group number
   character(len=*), intent(in), optional               :: spgstr    ! space group symbol (Hall, H-M)
   type(symop_type), dimension(:), intent(in), optional :: symop     ! symmetry operators
   character(len=*), intent(in), optional               :: sopt      ! combine 'H' (Hall symbol) and 'M' (H-M symbol)
   logical, intent(out)                                 :: sfound    ! true if space group was found
   integer, intent(inout), optional                     :: code      ! additional code for search from number
   character(len=40)                                    :: spgs
   logical                                              :: is_colon
   integer                                              :: nsym
   integer                                              :: lens
   logical                                              :: find_hall,find_hm
   integer                                              :: i, ids
!
   sfound = .false.
   if (present(spgnum)) then    ! cerca g.s. a partire dal numero
       if (spgnum > 0 .and. spgnum <= NUMSPGMAX) then
           if (present(code)) then
               if (code <= spg_index(spgnum)%nat .and. code > 0) then
                   spaceg = spg_data(spg_index(spgnum)%pos(code))
                   sfound = .true.
               endif
           else
               ids = spg_data(spg_index(spgnum)%pos(1))%standard_id
               spaceg = spg_data(ids)
               sfound = .true.
           endif
       endif

   elseif (present(spgstr)) then  ! cerca g.s. a partire dalla stringa
        find_hall = .true.
        find_hm = .true.
        if (present(sopt)) then
            find_hall = index(sopt,'H') > 0
            find_hm = index(sopt,'M') > 0
        endif
!
        spgs = spgstr
        call spg_set_string(spgs)  ! prepare string for search in syminfo.lib
        lens = len_trim(spgs)
        if (lens >= 2) then
            is_colon = is_colon_in_spgn(spgs) > 0
            sfound = .false.
            if (find_hm) then    ! Find H-M symbol
                do i=1,NSPGTOT
                   call find_hm_symbol(i,spgs,is_colon,sfound)
                   if (sfound) then
                       spaceg = spg_data(i)
                       exit
                   endif
                enddo
            endif
            if (find_hall .and. .not.sfound) then  !Find Hall symbol
                do i=1,NSPGTOT
                   sfound = s_eqidb(spg_data(i)%symbol_hall,spgs)
                   if (sfound) then
                       spaceg = spg_data(i)
                       exit 
                   endif
                enddo
            endif
        endif

   elseif (present(symop)) then   ! cerca a partire da una lista di operatori di simmetria
        nsym = size(symop)
        if (nsym == 0 .or. nsym > 192) return
        do i=1,NSPGTOT
           if (symop_equal_sets(nsym, symop, spg_data(i)%nsymop, spg_data(i)%symop)) then
               spaceg = spg_data(i)
               sfound = .true.
               exit 
           endif
        enddo
   endif
!
   end subroutine spg_load

!---------------------------------------------------------------------------

   subroutine spg_loadfile(spaceg,spgnum,spgstr,symop,sopt,sfound,code)
!
!  Load space group from number or from symbol or from symmetry operators
!  Obsolete subroutine that requires acess to file syminfo.lib
!
   USE fileutil
   USE strutil
   USE iso_fortran_env, only: ERROR_UNIT
   type(spaceg_type), intent(out)                       :: spaceg    ! properties of space group
   integer, intent(in), optional                        :: spgnum    ! space group number
   character(len=*), intent(in), optional               :: spgstr    ! space group symbol (Hall, H-M)
   type(symop_type), dimension(:), intent(in), optional :: symop     ! symmetry operators
   character(len=*), intent(in), optional               :: sopt      ! combine 'H' (Hall symbol) and 'M' (H-M symbol)
   logical, intent(out)                                 :: sfound    ! true if space group was found
   integer, intent(inout), optional                     :: code      ! additional code for search from number
   type(file_handle)                                    :: fspg
   integer                                              :: jfile
   character(len=200)                                   :: line    
   integer                                              :: ier
   integer                                              :: num
   integer                                              :: pos
   integer                                              :: lent
   character(len=40)                                    :: spgs
   logical                                              :: is_colon
   integer                                              :: numspaceg
   type(spaceg_type)                                    :: spgtemp
   type(sg_info_type)                                   :: info
   integer                                              :: nsym
   integer                                              :: lens
   integer                                              :: sgnum,iers,leni,posl
   character(len=40)                                    :: symb_hall
   logical                                              :: find_hall,find_hm,findtype
   integer                                              :: k
!
   sfound = .false.
   numspaceg = 0
   call spaceg%init()
   call fspg%fopen(spg_filename,'r')
   if (fspg%good()) then
       jfile = fspg%handle()
       if (present(spgnum)) then    ! cerca g.s. a partire dal numero
           do 
              read(jfile,'(a)',iostat=ier)line
              if (ier < 0) exit
              if (is_comment_line(line,['#'])) cycle
!
              pos = index(line,'number')
              if (pos > 0) then
                 call s_to_i(line(pos+6:),num,ier,lent)
                 if (ier == 0) then
                     if (num == spgnum) then
                         numspaceg = numspaceg + 1
                         if (numspaceg /= code) cycle
                         spaceg%num = spgnum
                         spaceg%csys_code = csys_code_from_num(spgnum)
                         spaceg%defined = .true.
                         call spg_read_block(jfile,spaceg,.true.)
                         info = get_spg_info(spaceg%symbol_xhm)
                         if (info%num > 0) then
                             spaceg%symbol_hall = info%hall
                             spaceg%choice = info%choice
                         endif
                         sfound = .true.   ! g.s. trovato!
                         code = code + 1
                         exit
                     endif
                 endif
              endif
!
            enddo

       elseif (present(spgstr)) then  ! cerca g.s. a partire dalla stringa
            find_hall = .true.
            find_hm = .true.
            if (present(sopt)) then
                find_hall = index(sopt,'H') > 0
                find_hm = index(sopt,'M') > 0
            endif
!
            spgs = spgstr
            call spg_set_string(spgs)  ! prepare string for search in syminfo.lib
            lens = len_trim(spgs)
            if (lens >= 2) then
                is_colon = is_colon_in_spgn(spgs) > 0
                sfound = .false.
!               loop 2 times for hall and H-M symbol
                loop_find: do k=1,2
                    if (k == 1 .and. .not.find_hm) cycle
                    if (k == 2 .and. .not.find_hall) exit 
                    findtype = k==2
                    !call spgtemp%init()
                    do 
                      read(jfile,'(a)',iostat=ier)line
                      if (ier < 0) exit
                      if (is_comment_line(line,['#'])) cycle
!
!                     leggi numero del gruppo spaziale
                      posl = index(line,'number')
                      if (posl > 0) then
                          call s_to_i(line(posl+6:),sgnum,iers,leni)
                          cycle
                      endif

                      posl = index(line,'Hall')
                      if (posl > 0) then
                          symb_hall = adjustl(rem_quotes(line(posl+4:)))
                          cycle
                      endif
                      call spgtemp%init()
                      call find_strspace(line,spgs,is_colon,jfile,sgnum,symb_hall,findtype,sfound,spgtemp)

                      if (sfound) then
                          spaceg = spgtemp
                          spaceg%defined = .true.
                          call spg_read_block(jfile,spaceg,.false.)
                          exit loop_find
                      endif

                    enddo
                    rewind(jfile)
                enddo loop_find
            endif

       elseif (present(symop)) then   ! cerca a partire da una lista di operatori di simmetria
            nsym = size(symop)
            if (nsym == 0 .or. nsym > 192) return
            do 
               call spaceg%init()
               call spg_read_block(jfile,spaceg,.true.,ier)
               if (ier /= 0) exit
               if (symop_equal_sets(nsym, symop, spaceg%nsymop, spaceg%symop)) then
                   info = get_spg_info(spaceg%symbol_xhm)
                   if (info%num > 0) then
                       spaceg%symbol_hall = info%hall
                       spaceg%choice = info%choice
                   endif
                   spaceg%defined = .true.
                   sfound = .true.   ! g.s. trovato!
                   exit
               endif
            enddo

       endif
       call fspg%fclose()
   else
       write(ERROR_UNIT,'(a)')'Cannot open: '//trim(spg_filename)
       write(ERROR_UNIT,'(a)')'Message: '//trim(fspg%err_msg())
   endif
!
   end subroutine spg_loadfile

!---------------------------------------------------------------------------
 
   subroutine load_spg_database(err)
   use fileutil
   use errormod
   use strutil
   use arrayutil
   use nrutil
   type(error_type), intent(out)  :: err
   type(file_handle)              :: fspg
   integer                        :: jfile,pos,numspg,i,j,num,spos
   character(len=:), allocatable  :: line
   type(sg_info_type)             :: info
   integer                        :: std_id

   call fspg%fopen(spg_filename,'r')
   if (fspg%fail()) then
       call err%set('Cannot open: '//trim(spg_filename)//char(10)//' Message: '//trim(fspg%err_msg()))
       return
   endif
!
!  Read syminfo.lib and create database
   jfile = fspg%handle()
   numspg = 0
   do while(get_line(jfile,line,trimmed=.true.,filter=.true.))
      if (len(line) == 0) cycle
      if (is_comment_line(line,['#'])) cycle
!
      pos = index(line,'begin_spacegroup')
      if (pos > 0) then
          numspg = numspg + 1
          call spg_data(numspg)%init()
          spg_data(numspg)%defined = .true.
          spg_data(numspg)%id = numspg - 1
          call spg_read_block(jfile,spg_data(numspg),.true.)
          info = get_spg_info(spg_data(numspg)%symbol_xhm,spos=spos)
          if (info%num > 0) then
              spg_data(numspg)%symbol_hall = info%hall
              spg_data(numspg)%choice = info%choice
              spg_data(numspg)%pmat = pmatrix(spos)%mat
              spg_data(numspg)%pmat1 = pmatrix(spos)%mat1
          endif
      endif
   enddo
   call fspg%fclose()
!
!  Make index for fast acess to database
   do i=1,NSPGTOT
      call container_set(spg_index(spg_data(i)%num),i)
   enddo
!
!  Set standard 
   do i=1,NUMSPGMAX
      if (spg_index(i)%nat == 1) then
          num = spg_index(i)%pos(1) 
          spg_data(num)%standard = .true.
          spg_data(num)%standard_id = num
      else
          ! The standard has pmat = identity matrix
          std_id = 0
          do j=1,spg_index(i)%nat
             num = spg_index(i)%pos(j) 
             if (s_eqi(spg_data(num)%pmat,'a,b,c')) then
                 spg_data(num)%standard = .true.
                 std_id = num
                 !if (j /= 1) then
!                !    Put the standard at 1 in the pos array
                 !    call swap(spg_index(i)%pos(1),spg_index(i)%pos(j))
                 !endif
                 exit
             endif
          enddo
          if (std_id > 0) then
              do j=1,spg_index(i)%nat
                 num = spg_index(i)%pos(j) 
                 spg_data(num)%standard_id = std_id
              enddo
          endif
      endif
   enddo

   !call spg_database_print()

   end subroutine load_spg_database

!---------------------------------------------------------------------------

   type(spaceg_type) function constructor(symb,numb,code,sopt) 
   character(len=*), intent(in), optional :: symb
   integer, intent(in), optional          :: numb
   integer, intent(in), optional          :: code
   character(len=*), intent(in), optional :: sopt
   logical                                :: sfound
   integer                                :: codes
   if (present(symb)) then
       if (present(sopt)) then
           call spg_load(constructor,spgstr=symb,sopt=sopt,sfound=sfound)
       else
           call spg_load(constructor,spgstr=symb,sfound=sfound)
       endif
       if (constructor%num == 0) then
           call constructor%set_p1()
           constructor%defined = .false.
       endif
   elseif (present(numb)) then
       codes = 1
       if (present(code)) codes = code
       call spg_load(constructor,spgnum=numb,sfound=sfound,code=codes)
       if (constructor%num == 0) then
           call constructor%set_p1()
           constructor%defined = .false.
       endif
   else
       call constructor%set_p1()
       constructor%defined = .false.
   endif
   end function constructor

!---------------------------------------------------------------------------

   subroutine prn(this,kpri,prlevel)
!
!  Print space group properties
!
   USE fractionm
   USE strutil
   class(spaceg_type)             :: this
   integer, intent(in), optional  :: kpri
   integer, intent(in), optional  :: prlevel
   integer                        :: kpr
   integer                        :: i
   type(fract_type), dimension(3) :: fr
   character(len=3)               :: adv
   integer                        :: nold,level,num
!
   if (present(kpri)) then
       kpr = kpri
   else
       kpr = 6
   endif
   if (this%undef()) then
       write(kpr,'(a)')' Undefined space group!'
       return
   endif
   if (present(prlevel)) then
       level = prlevel
   else
       level = 2
   endif
   write(kpr,*)
   write(kpr,'(a,i0)')   ' Space Group Number:     ',this%num
   if (len_trim(this%choice) > 0) then
       write(kpr,'(a,a)')' Table Setting Choice:   ',this%choice
   endif
   write(kpr,'(a,a)')    ' Crystal System:         ',cry_sys(this%csys_code)
   write(kpr,'(a,a)')    ' Hall Symbol:            ',trim(this%symbol_hall)
   write(kpr,'(a,a)')    ' Hermann-Mauguin Symbol: ',trim(this%symbol_xhm)
   if (level == 0) return
!
   nold = 0
   do i=1,2
      if (len_trim(this%symbol_alt(i)) > 0) then
          nold = nold + 1
      endif
   enddo
   if (nold == 1) then
       write(kpr,'(a,a)')  ' Alternative symbol:     ',trim(this%symbol_alt(1))
   elseif (nold == 2) then
       write(kpr,'(a,a)')  ' Alternative symbols:    ',trim(this%symbol_alt(1))//', '//trim(this%symbol_alt(2))
   endif
!
   write(kpr,'(a,a)')    ' Laue Group Symbol:      ',this%laue_class()
   write(kpr,'(a,a)')    ' Point Group Symbol:     ',this%point_group()
   write(kpr,'(a,a)')    ' Schoenflies Notation:   ',this%symbol_sch
   write(kpr,'(a,a)')    ' Patterson Space Group:  ',this%symbol_patt
   write(kpr,'(a,a)')    ' Extinction Symbol:      ',this%extinction_symbol()
   write(kpr,*)
   write(kpr,'(a)')      ' Centrosymmetry:         '//strcentr(this%symcent)
   write(kpr,'(a)')      ' Asymmetric Unit:        '//trim(this%asu_descr)
   write(kpr,'(a)')      ' Cheshire Cell:          '//trim(this%che_descr)
   write(kpr,'(a)')      ' Bravais Lattice:        '//this%lattyp
   write(kpr,'(a)')      ' Lattice Symbol:         '//lattice_symbol(this)
   write(kpr,'(a,i0)')   ' Multiplicity:           ',this%nsymop
   write(kpr,'(a,i0,a,i0)') &
                         ' Frequency(no. in CSD):  ',this%freq_no,'('//r_to_s(this%freq_perc,2)//'%), rank:',this%freq_rank
   if (.not.this%standard) then
       !num = spg_index(this%num)%pos(1)
       num = this%standard_id
       !write(kpr,'(a)') ' Non-standard space group setting ('//trim(spg_data(num)%symbol_xhm)//')'
       write(kpr,'(a,i0,a)')' The standard setting of the space group ',this%num,' is '//trim(spg_data(num)%symbol_xhm)
   endif
   if (this%is_chiral()) then
       write(kpr,'(a)')  ' Chiral space group (Sohncke space group) found!'
   endif
   write(kpr,*)
   adv = 'no'
   write(kpr,'(a)',advance=adv)   &
                         ' List of centering operators: '
   do i=1,this%ncoper
      fr(:) = fractional(this%coper(:,i))
      if (i == this%ncoper) adv = 'yes'
      write(kpr,'(1x,a)',advance=adv)             &
      '('//trim(fr(1)%string())//','//trim(fr(2)%string())//','//trim(fr(3)%string())//')+'
   enddo
   call this%prn_symop(1,kpr)
   write(kpr,*)
   if (level == 1) then
       call prn_origins(this,kpr)
   else
       if (this%ncoper > 1) then
           call this%prn_symop(2,kpr)
           write(kpr,*)
       endif
       call prn_origins(this,kpr)
       write(kpr,*)
       call prn_shelx(this,kpr)
       write(kpr,*)
       call prn_cif(this,kpr)
   endif
!
   end subroutine prn

!---------------------------------------------------------------------------

   subroutine prn_shelx(this,kpr)
!
!  Generate crystal data for SHELX file
!
   USE strutil
   class(spaceg_type)  :: this
   integer, intent(in) :: kpr
   integer             :: klatt
   integer             :: nop
   integer             :: i
   select case(this%lattyp)
      case ('P'); klatt = 1
      case ('I'); klatt = 2
      case ('R'); klatt = 3
      case ('F'); klatt = 4
      case ('A'); klatt = 5
      case ('B'); klatt = 6
      case ('C'); klatt = 7
      case default; klatt = 0
   end select
   if (this%symcent == 1) then
       write(kpr,'(a,i0)')'LATT ',klatt
       nop = this%nsymop_prim / 2
   else
       write(kpr,'(a,i0)')'LATT ',-klatt
       nop = this%nsymop_prim
   endif
   do i=2,nop
      write(kpr,'(a,a)')'SYMM ',trim(upper(this%symopstr(i)))
   enddo
!
   end subroutine prn_shelx

!---------------------------------------------------------------------------

   subroutine prn_cif(this,kpr)
!
!  Generate crystal data in CIF format
!
   USE strutil
!
   class(spaceg_type)  :: this
   integer, intent(in) :: kpr
   integer             :: i
!
   if (this%num > 0) then
       write(kpr,"(a,i0)") "_symmetry_Int_Tables_number         ",this%num
   endif
   if (this%csys_code > 0) then
       write(kpr,"(a)")    "_symmetry_cell_setting              "//lower(cry_sys(this%csys_code))
   else
       write(kpr,"(a)")    "_symmetry_cell_setting              "//"?"
   endif
   if (len_trim(this%symbol_xhm) == 0 .or. s_eqi(this%symbol_xhm,'unknown')) then
       write(kpr,"(a)")    "_symmetry_space_group_name_H-M      "//"?"
   else
       write(kpr,"(a)")    "_symmetry_space_group_name_H-M      "//s_in_quotes(this%symbol_xhm)
   endif
   if (len_trim(this%symbol_hall) == 0 .or. s_eqi(this%symbol_hall,'unknown')) then
       write(kpr,"(a)")    "_symmetry_space_group_name_hall     "//"?"
   else
       write(kpr,"(a)")    "_symmetry_space_group_name_hall     "//s_in_quotes(this%symbol_hall)
   endif
   if (this%nsymop > 0) then
       write(kpr,"(/a)") "loop_"
       write(kpr,"(a)") "    _symmetry_equiv_pos_site_id"
       write(kpr,"(a)") "    _symmetry_equiv_pos_as_xyz"
       do i=1,this%nsymop
          write(kpr,"(i5,3x,a)")i,s_in_quotes(this%symopstr(i))
       enddo
   endif
!
   end subroutine prn_cif

!---------------------------------------------------------------------------

   subroutine prn_symop(this,code,kpr)
!
!  Print symmetry operators.
!  code = 1 : only prmitive symmetry operators
!  code = 2 : all symmetry operators
!
   class(spaceg_type), intent(in) :: this
   integer, intent(in)            :: code
   integer, intent(in), optional  :: kpr
   integer                        :: nop
   integer                        :: i
   character(len=3)               :: adv
   integer                        :: forline
!
   select case (code)
     case (1)
       if (this%ncoper > 1) then
           write(kpr,'(/a/)')' List of symmetry operators without centring translation: '
       else
           write(kpr,'(/a/)')' List of all symmetry operators: '
       endif
       nop = this%nsymop_prim
     case (2)
       nop = this%nsymop
       write(kpr,'(/a/)')' List of all symmetry operators: '
   end select
   if (nop <= 8) then
       forline = 2
   else
       forline = 3
   endif
   do i=1,nop
      if (mod(i,forline) == 0 .or. i == nop) then
          adv = 'yes'
      else
          adv = 'no'
      endif
      write(kpr,'(i3,a,a)',advance=adv)i,') ',this%symopstr(i)(1:30)
   enddo
!
   end subroutine prn_symop

!---------------------------------------------------------------------------

   subroutine prn_symop_formatted(this,kpr)
!
!  Print symmetry operators in formatted way
!
   use strutil
   class(spaceg_type), intent(in) :: this
   integer, intent(in)            :: kpr
   integer                        :: i
   character(len=NSLEN)           :: strsym
   character(len=14), dimension(3) :: vsym
   integer                        :: nsym
!
   do i=1,this%nsymop_prim
      strsym = s_replace_by_blanks(this%symopstr(i),',')
      call get_words(strsym,vsym,nsym)
      write(kpr,'(a14,a14,a14)')adjustr(vsym(1)),adjustr(vsym(2)),adjustr(vsym(3))
   enddo
!
   end subroutine prn_symop_formatted

!---------------------------------------------------------------------------

   subroutine prn_origins(this,kpri)
!
!  Print list of equivalent origins
!
   USE fractionm
   class(spaceg_type), intent(in) :: this
   integer, intent(in), optional  :: kpri
   logical                        :: lpaxisx, lpaxisy, lpaxisz
   character(len=80)              :: line
   type(fract_type), dimension(3) :: fr
   integer                        :: i
   character(len=3)               :: adv
   integer                        :: kpr
!
   if (present(kpri)) then
       kpr = kpri
   else
       kpr = 6
   endif
   lpaxisx = this%polarx
   lpaxisy = this%polary
   lpaxisz = this%polarz
   line = ' '
   if( lpaxisx .and. lpaxisy .and. lpaxisz) then
      line = ' This is P1: origin anywhere'
      WRITE(kpr,'(a)')' Number of Alternate origins is infinite.'
   else if( lpaxisx .and. lpaxisy)    then
      line= ' This is a Polar spacegroup: origin anywhere in A B plane'
      WRITE(kpr,'(a,i0)')' Number of Alternate origin containing planes is: ',this%norig
   else if( lpaxisx .and. lpaxisz)    then
      line=' This is a Polar spacegroup: origin anywhere in A C plane'
      WRITE(kpr,'(a,i0)')' Number of Alternate origin containing planes is: ',this%norig
   else if( lpaxisy .and. lpaxisz)    then
      line=' This is a Polar spacegroup: origin anywhere in B C plane'
      WRITE(kpr,'(a,i0)')' Number of Alternate origin containing planes is: ',this%norig
   else if( lpaxisx)    then
      line=' This is a Polar spacegroup: origin is not fixed along A axis'  
      WRITE(kpr,'(a,i0)')' Number of Alternate origin containing lines is: ',this%norig
   else if( lpaxisy)    then
      line= ' This is a Polar spacegroup: origin is not fixed along B axis'  
      write(kpr,'(a,i0)')' Number of Alternate origin containing lines is: ',this%norig
   else if( lpaxisz)    then
      line=' This is a Polar spacegroup: origin is not fixed along C axis'
      write(kpr,'(a,i0)')' Number of Alternate origin containing lines is: ',this%norig
   else 
      write(kpr,'(a,i0)')' Number of Alternate origins is: ',this%norig
   end if

   if(line /= ' ') write(kpr,'(a)')line

   adv = 'no'
   WRITE(kpr,'(a)',advance=adv)' Origins: '
   DO I=1,this%norig
      if (i == this%norig) adv = 'yes'
      IF(lpaxisy .AND. lpaxisz .AND. LPAXISX)  THEN
          write(kpr,'(1x,a)')'(x,y,z)'
      else IF(LPAXISX .AND. lpaxisy)  THEN
          fr(3) = fractional(this%orig(3,i))
          write(kpr,'(1x,a)',advance=adv)'(x,y,'//trim(fr(3)%string())//')'
      else IF(LPAXISX .AND. lpaxisz)  THEN
          fr(2) = fractional(this%orig(2,i))
          write(kpr,'(1x,a)',advance=adv)'(x,'//trim(fr(2)%string())//',z)'
      else IF(lpaxisy .AND. lpaxisz)  THEN
          fr(1) = fractional(this%orig(1,i))
          write(kpr,'(1x,a)',advance=adv)'('//trim(fr(1)%string())//',y,z)'
      else IF( LPAXISX)    THEN
          fr(2) = fractional(this%orig(2,i))
          fr(3) = fractional(this%orig(3,i))
          write(kpr,'(1x,a)',advance=adv)'(x,'//trim(fr(2)%string())//','//trim(fr(3)%string())//')'
      else IF(lpaxisy)  THEN
          fr(1) = fractional(this%orig(1,i))
          fr(3) = fractional(this%orig(3,i))
          write(kpr,'(1x,a)',advance=adv)'('//trim(fr(1)%string())//',y,'//trim(fr(3)%string())//')'
      else IF(lpaxisz)  THEN
          fr(:2) = fractional(this%orig(:2,i))
          write(kpr,'(1x,a)',advance=adv)'('//trim(fr(1)%string())//','//trim(fr(2)%string())//','//'z)'
      else 
          fr(:) = fractional(this%orig(:,i))
          write(kpr,'(1x,a)',advance=adv)    &
          '('//trim(fr(1)%string())//','//trim(fr(2)%string())//','//trim(fr(3)%string())//')'
      END IF
   ENDDO
   if (this%norig /= this%norig_ind) then
       adv = 'no'
       WRITE(kpr,'(a)',advance=adv)' Non-equivalent origins: '
       DO I=1,this%norig_ind
          if (i == this%norig_ind) adv = 'yes'
          IF(lpaxisy .AND. lpaxisz .AND. LPAXISX)  THEN
              write(kpr,'(1x,a)')'(x,y,z)'
          ELSE IF(LPAXISX .AND. lpaxisy)  THEN
              fr(3) = fractional(this%orig_ind(3,i))
              write(kpr,'(1x,a)',advance=adv)'(x,y,'//trim(fr(3)%string())//')'
          ELSE IF(LPAXISX .AND. lpaxisz)  THEN
              fr(2) = fractional(this%orig_ind(2,i))
              write(kpr,'(1x,a)',advance=adv)'(x,'//trim(fr(2)%string())//',z)'
          ELSE IF(lpaxisy .AND. lpaxisz)  THEN
              fr(1) = fractional(this%orig_ind(1,i))
              write(kpr,'(1x,a)',advance=adv)'('//trim(fr(1)%string())//',y,z)'
          ELSE IF( LPAXISX)    THEN
              fr(2) = fractional(this%orig_ind(2,i))
              fr(3) = fractional(this%orig_ind(3,i))
              write(kpr,'(1x,a)',advance=adv)'(x,'//trim(fr(2)%string())//','//trim(fr(3)%string())//')'
          ELSE IF(lpaxisy)  THEN
              fr(1) = fractional(this%orig_ind(1,i))
              fr(3) = fractional(this%orig_ind(3,i))
              write(kpr,'(1x,a)',advance=adv)'('//trim(fr(1)%string())//',y,'//trim(fr(3)%string())//')'
          ELSE IF(lpaxisz)  THEN
              fr(:2) = fractional(this%orig_ind(:2,i))
              write(kpr,'(1x,a)',advance=adv)'('//trim(fr(1)%string())//','//trim(fr(2)%string())//','//'z)'
          ELSE
              fr(:) = fractional(this%orig_ind(:,i))
              write(kpr,'(1x,a)',advance=adv)    &
              '('//trim(fr(1)%string())//','//trim(fr(2)%string())//','//trim(fr(3)%string())//')'
      END IF
   ENDDO

   endif
!
   end subroutine prn_origins

!---------------------------------------------------------------------------

   subroutine init_spg(this)  
!
!  Initialize spaceg_type class
!
   class(spaceg_type) :: this
   this%num = 0    ! if 0 the space group is undefined
   this%symbol_hall = 'Unknown'
   this%symbol_xhm = 'Unknown'
   this%symbol_alt(1) = ' '
   this%symbol_alt(2) = ' '
   this%symbol_sch = ' '
   this%symbol_patt = 'Unknown'
   this%choice = 'Unknown'
   this%nsymop = 0
   this%nsymop_prim = 0
   this%nsym = 0
   this%ncoper = 0
   this%coper(:,:) = 0
   this%lattyp = ' '
   this%laue_code = 0
   this%pgroup_code = 0
   this%symcent = 0
   this%csys_code = 0
   this%norig = 0
   this%orig(:,:) = 0
   this%norig_ind = 0
   this%orig_ind(:,:) = 0
   this%polarx = .false.
   this%polary = .false.
   this%polarz = .false.
   this%asulim = (/1.0,1.0,1.0/)
   this%asueq = (/0,0,0/)
   this%chelim = (/0.0,0.0,0.0/)
   this%asu_descr = 'Unknown'
   this%che_descr = 'Unknown'
   this%defined = .false.
   end subroutine init_spg

!---------------------------------------------------------------------------

   subroutine set_p1(this)
!
!  Set space group as P 1
!
   class(spaceg_type) :: this
   this%num = 1
   this%symbol_hall ='P 1'
   this%symbol_xhm ='P 1'
   this%symbol_alt =' '
   this%symbol_sch = 'C1^1'
   this%symbol_patt = 'P-1'
   this%choice = ' '
   this%nsymop = 1
   this%nsymop_prim = 1
   this%nsym = 1
   this%symop(1) = symop_type(identity_mat,(/0,0,0/))
   this%symopstr(1) = 'x, y, z'
   this%ncoper = 1
   this%coper(:,1) = (/0.0,0.0,0.0/)
   this%lattyp = 'P'
   this%laue_code = 1
   this%pgroup_code = 1
   this%symcent = 0
   this%csys_code = 1
   this%norig=1
   this%orig(:,1) = 0
   this%norig_ind=1
   this%orig_ind(:,1) = 0
   this%polarx = .true.
   this%polary = .true.
   this%polarz = .true.
   this%asulim = (/1.0,1.0,1.0/)
   this%asueq = (/0,0,0/)
   this%asu_descr = '0<=x<1; 0<=y<1; 0<=z<1'
   this%chelim = (/0.0,0.0,0.0/)
   this%che_descr = '0<=x<=0; 0<=y<=0; 0<=z<=0'
   this%defined = .true.
   this%pmat = 'a,b,c'
   this%pmat1 = 'a,b,c'
   this%id = 0
   end subroutine set_p1

!---------------------------------------------------------------------------

   subroutine set_from_symop(this,symop,symbol_xhm,symbol_hall,numIT,csetting)
!
!  Set space group from symmetry operators
!
   USE strutil
   class(spaceg_type), intent(out)            :: this
   type(symop_type), dimension(:), intent(in) :: symop                    ! symmetry operators
   character(len=*), intent(in), optional     :: symbol_xhm, symbol_hall  ! symbols
   integer, intent(in)                        :: numIT                    ! number from Int Tables
   character(len=*), intent(in)               :: csetting                 ! cell setting
   integer                                    :: i
   type(spaceg_type)                          :: spgnum
   logical                                    :: numfound
   integer                                    :: sys_code
!   
!  Initialize to P1 and set as defined the space group
   call this%set_p1()   
!corr   this%defined = .false.
!
!  Set symmetry operators and their number
   this%nsymop = size(symop)
   this%nsymop_prim = this%nsymop
   this%nsym = this%nsymop
   do i=1,this%nsymop
      this%symop(i) = symop(i)
      call symop_to_string(this%symop(i),this%symopstr(i))
   enddo
!
!  Set symbols e spacegroup number
   numfound = .false.
   if (len_trim(symbol_hall) > 0) then
       this%symbol_hall = symbol_hall
       call spg_load(spgnum,spgstr=symbol_hall,sfound=numfound)   !search for spacegroup number
   else
       this%symbol_hall = 'Unknown'
   endif
   if (len_trim(symbol_xhm) > 0) then
       this%symbol_xhm = symbol_xhm
       if (.not.numfound) call spg_load(spgnum,spgstr=symbol_xhm,sfound=numfound)
   else
       this%symbol_xhm = 'Unknown'
   endif
!
!  Set number and crystal system code
   if (numfound) then
       this%num = spgnum%num
       this%csys_code = spgnum%csys_code
   else
       if (numIT > 0) this%num = numIT
       sys_code = string_locate(csetting,cry_sys)
       if (sys_code == 0) sys_code = csys_code_from_num(this%num)
       if (sys_code > 0) this%csys_code = sys_code
   endif
!
   end subroutine set_from_symop

!---------------------------------------------------------------------------

   subroutine spg_set_symmetry_file(spgfile,err)
!
!  Change default for symmetry file name
!
   USE errormod
   USE fileutil
   character(len=*), intent(in)  :: spgfile
   type(error_type), intent(out) :: err
!
   if (file_exist(spgfile)) then
       spg_filename = spgfile
   else
       call err%set('File '//trim(spgfile)//' does not exist.')
   endif
!
   end subroutine spg_set_symmetry_file

!---------------------------------------------------------------------------

   subroutine spg_set_string(spgr)
!
!  Prepare space group input string for search on symmetry table
!
   USE strutil
   character(len=*), intent(inout) :: spgr
   integer                         :: lens
   character(len=1), dimension(2)  :: srem = (/"_","~"/)
   integer                         :: i, irep
   integer                         :: pos
   logical                         :: found
!
!  Remove character in vector srem
   do i=1,len_trim(spgr)
      if (any(srem == spgr(i:i))) spgr(i:i) = ' ' 
   enddo
!
   spgr = s_blank_delete(spgr)     ! remove blanks
   call s_rep(spgr,'(1)','1',irep) ! replace '(1)' with '1'
!
!  Convert for example 'F d d d {origin @ -1 @ d d d}' in 'Fddd:2'
   pos = index(spgr,'{origin@-1')
   if (pos > 1) spgr=spgr(:pos-1)//':2'
!
!  Remove comment on origin
   pos = index(spgr,'(origin')
   if (pos > 0) spgr(pos:) = ' '
!
!  Change S (origin choice 1 of International Tables) in ':1'
!  Change Z (origin at a center of symmetry) in ':2'
   lens = len_trim(spgr)
   if (lens > 1) then
       if (spgr(lens:lens) == 'S' .or. spgr(lens:lens) == 's') then
           if (spgr(lens-1:lens-1) == ':') then   ! check if comma is present 
               spgr(lens:lens) = '1'
           else
               spgr(lens:lens+1) = ':1'
           endif
!
!          check on symmtable if the space group with :1 is in a list
           found = .false.
           do i=1,size(sg_info)
              if (s_eqi(sg_info(i)%hm1,spgr)) then
                  found = .true.
                  exit
              endif
           enddo
           if (.not.found) spgr(len_trim(spgr)-1:) = ' '  ! se non esiste elimina ':1'
       elseif (spgr(lens:lens) == 'Z' .or. spgr(lens:lens) == 'z') then
           if (spgr(lens-1:lens-1) == ':') then   ! check if comma is present 
               spgr(lens:lens) = '2'
           else
               spgr(lens:lens+1) = ':2'
           endif
       endif
   endif
!
   end subroutine spg_set_string

!---------------------------------------------------------------------------
!corr    subroutine spg_set_cell(spg,cell)
!corr !
!corr !  Set cell parameters according the crystal system
!corr !
!corr    type(spaceg_type), intent(in) :: spg
!corr    real, dimension(6), intent(inout)       :: cell
!corr    logical                                 :: hexaxes, rhombaxes, par_present
!corr    real                                    :: acheck
!corr    integer                                 :: np,ncheck
!corr    real, parameter                         :: TOL = 0.01   !!!!!, EPS = epsilon(1.0)
!corr !
!corr    select case (spg%csys_code)
!corr      case (CS_Monoclinic)
!corr         !if (present(spg)) then
!corr             if (spg%symop(2)%rot(1,1) == spg%symop(2)%rot(2,2)) then      ! 2 lungo c
!corr                 cell(4:5) = 90   ! al=bet=90
!corr             elseif (spg%symop(2)%rot(1,1) == spg%symop(2)%rot(3,3)) then  ! 2 lungo b
!corr                 cell(4) = 90     ! al=gam=90
!corr                 cell(6) = 90
!corr             elseif (spg%symop(2)%rot(2,2) == spg%symop(2)%rot(3,3)) then  ! 2 lungo a
!corr                 cell(5:6) = 90   ! bet=gam=90
!corr             endif
!corr         !else
!corr         !    if (abs(cell(1)) > EPS) then
!corr         !        cell(4:5) = 90   ! al=bet=90
!corr         !    elseif (abs(cell(2)) > EPS) then
!corr         !        cell(4) = 90     ! al=gam=90
!corr         !        cell(6) = 90
!corr         !    elseif (abs(cell(3)) > EPS) then
!corr         !        cell(5:6) = 90   ! bet=gam=90
!corr         !    endif
!corr         !endif
!corr      case (CS_Orthorhombic)
!corr         cell(4:6) = 90.0     ! al=bet=gam=90
!corr      case (CS_Tetragonal)
!corr         cell(2) = cell(1)    ! b=a
!corr         cell(4:6) = 90.0     ! al=bet=gam=90
!corr      case (CS_Trigonal)
!corr         np = count(cell > 0)
!corr         if (np > 1) then
!corr             par_present = (cell(1) > 0 .and. cell(3) > 0) &    ! a,c present
!corr                       .or. (cell(2) > 0 .and. cell(3) > 0)      ! b,c present
!corr             if (par_present) then
!corr                 select case(np)
!corr                   case (2)
!corr                       hexaxes = par_present 
!corr                            
!corr                   case default
!corr !
!corr !                   Check for a=b; al=bet=90 gam=120
!corr                         acheck = 0
!corr                         ncheck = 0
!corr                         if (cell(1) > 0 .and. cell(2) > 0) then
!corr                             ncheck = ncheck + 1
!corr                             acheck = acheck + abs(cell(1) - cell(2))
!corr                         endif
!corr                         if (cell(4) > 0) then
!corr                             ncheck = ncheck + 1
!corr                             acheck = acheck + abs(cell(4) - 90)
!corr                         endif
!corr                         if (cell(5) > 0) then
!corr                             ncheck = ncheck + 1
!corr                             acheck = acheck + abs(cell(5) - 90)
!corr                         endif
!corr                         if (cell(6) > 0) then
!corr                             ncheck = ncheck + 1
!corr                             acheck = acheck + abs(cell(6) - 120)
!corr                         endif
!corr                         hexaxes = acheck < TOL .and. ncheck > 0
!corr                 end select
!corr             else
!corr                 hexaxes = .false.
!corr             endif
!corr             if (hexaxes) then        ! hexagonal axes
!corr                 if (cell(1) > 0) then
!corr                     cell(2) = cell(1)    !    b=a
!corr                 else
!corr                     cell(1) = cell(2)    !    b=a
!corr                 endif
!corr                 cell(4:5) = 90.0     !    al=bet=90
!corr                 cell(6) = 120        !    gam=120
!corr             else                     ! rhombohedral axes
!corr !
!corr !               Check for rhombohedral axes
!corr                 par_present = (count(cell(1:3) > 0) > 0) .and. (count(cell(4:6) > 0) > 0) ! a/b/c present
!corr                                                                                           ! al/bet/gam present
!corr                 if (par_present) then
!corr                     select case (np) 
!corr                        case (2) 
!corr                          rhombaxes = par_present
!corr 
!corr                        case default
!corr                          ncheck = 0
!corr                          acheck = 0
!corr                          if (cell(1) > 0 .and. cell(2) > 0) then
!corr                              ncheck = ncheck + 1
!corr                              acheck = acheck + abs(cell(1) - cell(2))
!corr                          endif
!corr                          if (cell(1) > 0 .and. cell(3) > 0) then
!corr                              ncheck = ncheck + 1
!corr                              acheck = acheck + abs(cell(1) - cell(3))
!corr                          endif
!corr                          if (cell(2) > 0 .and. cell(3) > 0) then
!corr                              ncheck = ncheck + 1
!corr                              acheck = acheck + abs(cell(2) - cell(3))
!corr                          endif
!corr                          if (cell(4) > 0 .and. cell(5) > 0) then
!corr                              ncheck = ncheck + 1
!corr                              acheck = acheck + abs(cell(4) - cell(5))
!corr                          endif
!corr                          if (cell(4) > 0 .and. cell(6) > 0) then
!corr                              ncheck = ncheck + 1
!corr                              acheck = acheck + abs(cell(4) - cell(6))
!corr                          endif
!corr                          if (cell(5) > 0 .and. cell(6) > 0) then
!corr                              ncheck = ncheck + 1
!corr                              acheck = acheck + abs(cell(5) - cell(6))
!corr                          endif
!corr                          rhombaxes = acheck < TOL .and. ncheck > 0
!corr                     end select
!corr                 else
!corr                      rhombaxes = .false.
!corr                 endif
!corr                 if (rhombaxes) then  !it is supposed that cell(1) has been provide
!corr                     cell(2:3) = cell(1)  !    c=b=a
!corr                     cell(5:6) = cell(4)  !    gam=bet=al
!corr                 endif
!corr             endif
!corr         endif
!corr      case (CS_Hexagonal)
!corr         if (cell(1) > 0) then
!corr             cell(2) = cell(1)    ! b=a
!corr         else
!corr             cell(1) = cell(2)    ! b=a
!corr         endif
!corr         cell(4:5) = 90.0     ! al=bet=90
!corr         cell(6) = 120        ! gam=120
!corr      case (CS_Cubic)
!corr         cell(2:3) = cell(1)  ! b=c=a
!corr         cell(4:6) = 90.0     ! al=bet=gam=90
!corr    end select
!corr !
!corr    end subroutine spg_set_cell
!---------------------------------------------------------------------------

   subroutine spg_set_cell_sc(spg,cell)
!
!  Set cell parameters according the crystal system
!
   type(spaceg_type), intent(in) :: spg
   real, dimension(6), intent(inout)       :: cell
   call spg_set_cell_ccs(spg%csys_code,cell,spg%symop)
   end subroutine spg_set_cell_sc

!---------------------------------------------------------------------------
   
!corr   subroutine spg_set_cell_cc(csys_code,cell)
!corr!
!corr!  Set cell parameters according the crystal system
!corr!
!corr   integer, intent(in)                     :: csys_code
!corr   real, dimension(6), intent(inout)       :: cell
!corr   call spg_set_cell_ccs(csys_code,cell)
!corr   end subroutine spg_set_cell_cc

!---------------------------------------------------------------------------

   subroutine spg_set_cell_ccs(csys_code,cell,symop)
!
!  Set cell parameters according the crystal system
!
!corr   type(spaceg_type), intent(in), optional :: spg
   integer, intent(in)                     :: csys_code
   type(symop_type), dimension(:), intent(in), optional :: symop
   real, dimension(6), intent(inout)       :: cell
   logical                                 :: hexaxes, rhombaxes, par_present
   real                                    :: acheck
   integer                                 :: np,ncheck
   real, parameter                         :: TOL = 0.01     !!!, EPS = epsilon(1.0)
!
   select case (csys_code)
     case (CS_Monoclinic)
        if (present(symop)) then
            if (symop(2)%rot(1,1) == symop(2)%rot(2,2)) then      ! 2 lungo c
                cell(4:5) = 90   ! al=bet=90
            elseif (symop(2)%rot(1,1) == symop(2)%rot(3,3)) then  ! 2 lungo b
                cell(4) = 90     ! al=gam=90
                cell(6) = 90
            elseif (symop(2)%rot(2,2) == symop(2)%rot(3,3)) then  ! 2 lungo a
                cell(5:6) = 90   ! bet=gam=90
            endif
        else
!qualx            if (abs(cell(4)) > EPS) then
            if (cell(4) > 0.0) then
                cell(4:5) = 90   ! al=bet=90
!qualx            elseif (abs(cell(5)) > EPS) then
            elseif (cell(5) > 0.0) then
                cell(4) = 90     ! al=gam=90
                cell(6) = 90
!qualx            elseif (abs(cell(6)) > EPS) then
            elseif (cell(6) > 0.0) then
                cell(5:6) = 90   ! bet=gam=90
            endif
        endif
     case (CS_Orthorhombic)
        cell(4:6) = 90.0     ! al=bet=gam=90
     case (CS_Tetragonal)
        cell(2) = cell(1)    ! b=a
        cell(4:6) = 90.0     ! al=bet=gam=90
     case (CS_Trigonal)
        np = count(cell > 0)
        if (np > 1) then
            par_present = (cell(1) > 0 .and. cell(3) > 0) &    ! a,c present
                      .or. (cell(2) > 0 .and. cell(3) > 0)      ! b,c present
            if (par_present) then
                select case(np)
                  case (2)
                      hexaxes = par_present 
                           
                  case default
!
!                   Check for a=b; al=bet=90 gam=120
                        acheck = 0
                        ncheck = 0
                        if (cell(1) > 0 .and. cell(2) > 0) then
                            ncheck = ncheck + 1
                            acheck = acheck + abs(cell(1) - cell(2))
                        endif
                        if (cell(4) > 0) then
                            ncheck = ncheck + 1
                            acheck = acheck + abs(cell(4) - 90)
                        endif
                        if (cell(5) > 0) then
                            ncheck = ncheck + 1
                            acheck = acheck + abs(cell(5) - 90)
                        endif
                        if (cell(6) > 0) then
                            ncheck = ncheck + 1
                            acheck = acheck + abs(cell(6) - 120)
                        endif
                        hexaxes = acheck < TOL .and. ncheck > 0
                end select
            else
                hexaxes = .false.
            endif
            if (hexaxes) then        ! hexagonal axes
                if (cell(1) > 0) then
                    cell(2) = cell(1)    !    b=a
                else
                    cell(1) = cell(2)    !    b=a
                endif
                cell(4:5) = 90.0     !    al=bet=90
                cell(6) = 120        !    gam=120
            else                     ! rhombohedral axes
!
!               Check for rhombohedral axes
                par_present = (count(cell(1:3) > 0) > 0) .and. (count(cell(4:6) > 0) > 0) ! a/b/c present
                                                                                          ! al/bet/gam present
                if (par_present) then
                    select case (np) 
                       case (2) 
                         rhombaxes = par_present

                       case default
                         ncheck = 0
                         acheck = 0
                         if (cell(1) > 0 .and. cell(2) > 0) then
                             ncheck = ncheck + 1
                             acheck = acheck + abs(cell(1) - cell(2))
                         endif
                         if (cell(1) > 0 .and. cell(3) > 0) then
                             ncheck = ncheck + 1
                             acheck = acheck + abs(cell(1) - cell(3))
                         endif
                         if (cell(2) > 0 .and. cell(3) > 0) then
                             ncheck = ncheck + 1
                             acheck = acheck + abs(cell(2) - cell(3))
                         endif
                         if (cell(4) > 0 .and. cell(5) > 0) then
                             ncheck = ncheck + 1
                             acheck = acheck + abs(cell(4) - cell(5))
                         endif
                         if (cell(4) > 0 .and. cell(6) > 0) then
                             ncheck = ncheck + 1
                             acheck = acheck + abs(cell(4) - cell(6))
                         endif
                         if (cell(5) > 0 .and. cell(6) > 0) then
                             ncheck = ncheck + 1
                             acheck = acheck + abs(cell(5) - cell(6))
                         endif
                         rhombaxes = acheck < TOL .and. ncheck > 0
                    end select
                else
                     rhombaxes = .false.
                endif
                if (rhombaxes) then  !it is supposed that cell(1) has been provide
                    cell(2:3) = cell(1)  !    c=b=a
                    cell(5:6) = cell(4)  !    gam=bet=al
                endif
            endif
        endif
     case (CS_Hexagonal)
        if (cell(1) > 0) then
            cell(2) = cell(1)    ! b=a
        else
            cell(1) = cell(2)    ! b=a
        endif
        cell(4:5) = 90.0     ! al=bet=90
        cell(6) = 120        ! gam=120
     case (CS_Cubic)
        cell(2:3) = cell(1)  ! b=c=a
        cell(4:6) = 90.0     ! al=bet=gam=90
   end select
!
   end subroutine spg_set_cell_ccs

!---------------------------------------------------------------------------

   subroutine spg_get_cell_code(spg,cell,code,ier)
!
!  Set refinement code for cell parameters according the crystal system
!
   type(spaceg_type), intent(in)  :: spg
   real, dimension(6), intent(in) :: cell
   integer, dimension(6)          :: code
   integer, intent(out)           :: ier
   real, parameter                :: tolb = 0.01
!
   ier = 0
   select case (spg%csys_code)
     case (CS_TRiclinic)
        code(:) = 0
     case (CS_Monoclinic)
        code(1:3) = 0
        if (spg%undef()) then
            where(abs(cell(4:6) - 90.00) <= tolb) 
               code(4:6) = -90
            elsewhere
               code(4:6) = 0
            endwhere
        else
            if (spg%symop(2)%rot(1,1) == spg%symop(2)%rot(2,2)) then      ! 2 lungo c
                code(4:5) = -90   ! al=bet=90
                code(6)   = 0
            elseif (spg%symop(2)%rot(1,1) == spg%symop(2)%rot(3,3)) then  ! 2 lungo b
                code(4) = -90     ! al=gam=90
                code(5) = 0
                code(6) = -90 
            elseif (spg%symop(2)%rot(2,2) == spg%symop(2)%rot(3,3)) then  ! 2 lungo a
                code(4) = 0
                code(5:6) = -90   ! bet=gam=90
            endif
        endif
     case (CS_Orthorhombic)
        code(1:3) = 0
        code(4:6) = -90       ! al=bet=gam=90
     case (CS_Tetragonal)
        code(1) = 0
        code(2) = -1          ! b=a
        code(3) = 0
        code(4:6) = -90       ! al=bet=gam=90
     case (CS_Trigonal)
        code(1) = 0
        if (uc_is_hexagonal(cell,0.01)) then
            code(2) = -1          ! b=a
            code(3) = 0
            code(4:5) = -90       ! al=bet=90
            code(6) = -120        ! gam=120
        elseif (uc_is_rhombohedral(cell,0.01)) then ! rhombohedral axes
            code(2:3) = -1        ! c=b=a
            code(4) = 0           ! gam=bet=al
            code(5:6) = -1        ! gam=bet=al
        else
            code(:) = 0
            ier = 1
        endif
     case (CS_Hexagonal)
        code(1) = 0
        code(2) = -1          ! b=a
        code(3) = 0
        code(4:5) = -90       ! al=bet=90
        code(6) = -120        ! gam=120
     case (CS_Cubic)
        code(1) = 0
        code(2:3) = -1        ! b=c=a
        code(4:6) = -90       ! al=bet=gam=90
   end select
!
   end subroutine spg_get_cell_code

!---------------------------------------------------------------------------

   logical function uc_is_rhombohedral(cell,tol)
   real, dimension(6), intent(in) :: cell
   real, intent(in)               :: tol
!   
   uc_is_rhombohedral = sum(abs(cell(1:2) - cell(3))) + sum(abs(cell(4:5) - cell(6))) < tol
!
   end function uc_is_rhombohedral

!---------------------------------------------------------------------------

   logical function uc_is_hexagonal(cell,tol)
   real, dimension(6), intent(in) :: cell
   real, intent(in)               :: tol
!   
   uc_is_hexagonal = abs(cell(1) - cell(2)) + sum(abs(cell(4:5)- 90)) + abs(cell(6) - 120) < tol
!
   end function uc_is_hexagonal

!---------------------------------------------------------------------------

   subroutine find_strspace(line,strfind,is_colon,jfile,sgnum,symb_hall,find_hall,sfound,spaceg)
!
!  Try to match input symbol with symbols on symmetry table
!
   USE strutil
   character(len=*), intent(in)    :: line
   character(len=*), intent(in)    :: strfind
   logical, intent(in)             :: is_colon
   integer, intent(in)             :: jfile
   integer, intent(in)             :: sgnum
   character(len=*), intent(in)    :: symb_hall
   logical, intent(in)             :: find_hall
   logical, intent(inout)          :: sfound
   type(spaceg_type), intent(inout)  :: spaceg
   character(len=40)               :: spgn_long,spgn_short
   integer                         :: irep
   integer                         :: pxhm,posc
   type(sg_info_type)              :: info
!corr   integer                         :: oksymb
   character(len=200)              :: line1    
   character(len=40), dimension(2) :: words
   integer                         :: nwords
   integer                         :: lens
   integer                         :: pos
   character(len=40)               :: strhm
   logical                         :: checkw
!
   pxhm = index(line,'xHM')
   if (pxhm == 0) return
!
   spgn_long = adjustl(rem_quotes(line(pxhm+3:)))
   !if (len_trim(spgn_long) > 0) spaceg%symbol_xhm = spgn_long
   spaceg%symbol_xhm = spgn_long   ! this overwrite 'Unknown'

   if (find_hall) then  ! find match between strfind and hall symbol
!
!      Get alternative symbol from table
       info = get_spg_info(spaceg%symbol_xhm)
!
!      Compare with Hall symbol
       sfound = s_eqidb(info%hall,strfind)
!
!      Compare with Hall symbol from syminfo.lib
       if (.not.sfound) then     
           sfound = s_eqidb(symb_hall,strfind)
       endif

   else                 ! find match between strfind and H-M symbol
       lens = len_trim(spgn_long)
       if (lens >= 2) then
           if (.not.is_colon) then   
!              Remove ':' if colon is not in input string
!              Don't check for ':1' and choose ':2' (-1 at origin)
               if (spgn_long(lens-1:lens) == ':2' .or.    &
                   spgn_long(lens-1:lens) == ':R' .or.    &
                   spgn_long(lens-1:lens) == ':H') then
                   spgn_long(lens-1:) = ' '
               endif
           endif
!
!          Compare with symbol in syminfo.lib
           sfound = s_eqidb(spgn_long,strfind)
!
!          This check only for cubic space
           if (.not.sfound) then
               if (sgnum >= 195) then  
                   posc = index(spgn_long,'-3')
                   if (posc > 0) then
                       spgn_short = spgn_long
                       spgn_short (posc:posc) = ' '
                       sfound = s_eqidb(spgn_short,strfind)
                   endif
               endif
           endif
!
!          Compare again with short notation
           if (.not.sfound) then     ! remove ' 1': P 1 21 1 -> P 21
               spgn_short = spgn_long
               call s_s_delete(spgn_short,' 1',irep)
               if (irep > 0) sfound = s_eqidb(spgn_short,strfind)
           endif
       endif
!
!      Get alternative symbol from table
       info = get_spg_info(spaceg%symbol_xhm)
!old!
!old!      Compare with Hall symbol
!old       if (.not.sfound) then     
!old           sfound = s_eqidb(info%hall,strfind)
!old       endif
!old!
!old!      Compare with Hall symbol from syminfo.lib
!old       if (.not.sfound) then     
!old           sfound = s_eqidb(symb_hall,strfind)
!old       endif
!
!      Compare symbol with table choice (ex. P2/a:b3, B2/m:a1, ...)
       if (.not.sfound) then     
           strhm = info%hm1
           pos = index(strhm,'=')
           if (pos > 0) then
               strhm(pos:) = ' '
           endif
           sfound = s_eqi(strhm,strfind)
           if (.not.sfound) then  ! forza il confronto rimuovendo i :
               call s_s_delete(strhm,':',irep)
               if (irep > 0) sfound = s_eqi(strhm,strfind)
           endif
       endif
!
!      Try to read the old symbol
       if (.not.sfound) then
           read(jfile,'(a)')line1
           call cutst(line1)
           call get_words_quotes(line1,words,nwords)
           if (nwords > 0) then ! read only the first old symbol
!
!              se il vecchio simbolo differisce solo per :1 salto questo controllo per favorire il :2 (es. strfind=I 41/a ma old: I 41 / a)
               checkw = .true.
               if (lens > 2) then
                   checkw = .not.(spgn_long(lens-1:lens) == ':1' .and. s_eqidb(spgn_long(:lens-2),words(1))) ! old symbol equal to spg without :1
               endif
               if (checkw) sfound = s_eqidb(words(1),strfind)
           endif
           if (nwords == 2) then
!
!              only for symbol: H-3m, H-3c, ...
               if (words(2)(1:1) == 'H') sfound = s_eqidb(words(2),strfind)
           endif
           if (sfound) backspace(jfile)
       endif
   endif
!
   if (sfound) then
       if (info%num > 0) then ! symbols found by get_symbols
           spaceg%symbol_hall = adjustl(info%hall)
           spaceg%choice = info%choice
       else
           spaceg%symbol_hall = symb_hall
       endif
       backspace(jfile)
       backspace(jfile)
       backspace(jfile)
       backspace(jfile)
       backspace(jfile)
   endif
!
   end subroutine find_strspace  

!---------------------------------------------------------------------------

   !subroutine find_hm_symbol(kord,line,strfind,is_colon,jfile,sgnum,symb_hall,find_hall,sfound,spaceg)
   subroutine find_hm_symbol(kord,strfind,is_colon,sfound)
!
!  Try to match input symbol with symbols on symmetry table
!
   USE strutil
   integer, intent(in)          :: kord
   character(len=*), intent(in) :: strfind
   logical, intent(in)          :: is_colon
   logical, intent(inout)       :: sfound
   character(len=40)            :: spgn_long,spgn_short
   integer                      :: irep
   integer                      :: posc
   integer                      :: lens
   integer                      :: pos
   character(len=40)            :: strhm
   logical                      :: checkw
!
   !pxhm = index(line,'xHM')
   !if (pxhm == 0) return
!
   !spgn_long = adjustl(rem_quotes(line(pxhm+3:)))
   spgn_long = spg_data(kord)%symbol_xhm
   !if (len_trim(spgn_long) > 0) spaceg%symbol_xhm = spgn_long
   !spaceg%symbol_xhm = spgn_long   ! this overwrite 'Unknown'

   lens = len_trim(spgn_long)
   if (lens >= 2) then
       if (.not.is_colon) then   
!          Remove ':' if colon is not in input string
!          Don't check for ':1' and choose ':2' (-1 at origin)
           if (spgn_long(lens-1:lens) == ':2' .or.    &
               spgn_long(lens-1:lens) == ':R' .or.    &
               spgn_long(lens-1:lens) == ':H') then
               spgn_long(lens-1:) = ' '
           endif
       endif
!
!      Compare with symbol in syminfo.lib
       sfound = s_eqidb(spgn_long,strfind)
!
!      This check only for cubic space
       if (.not.sfound) then
           !if (sgnum >= 195) then  
           if (spg_data(kord)%num >= 195) then  
               posc = index(spgn_long,'-3')
               if (posc > 0) then
                   spgn_short = spgn_long
                   spgn_short (posc:posc) = ' '
                   sfound = s_eqidb(spgn_short,strfind)
               endif
           endif
       endif
!
!      Compare again with short notation
       if (.not.sfound) then     ! remove ' 1': P 1 21 1 -> P 21
           spgn_short = spgn_long
           call s_s_delete(spgn_short,' 1',irep)
           if (irep > 0) sfound = s_eqidb(spgn_short,strfind)
       endif
   endif
!
!  Compare symbol with table choice (ex. P2/a:b3, B2/m:a1, ...)
   if (.not.sfound) then     
!
!      Get alternative symbol from table
       if (len_trim(spg_data(kord)%choice) > 0 .and. spg_data(kord)%choice /= 'Unknown') then
           !strhm = spg_data(kord)%symbol_xhm
           strhm = spgn_short
           pos = index(strhm,':')
           if (pos > 0) then
               strhm(pos:) = ' '
           endif
           strhm = trim(strhm)//':'//trim(spg_data(kord)%choice)
           !write(70,*)'TABLE:',trim(strhm),spg_data(kord)%num
           sfound = s_eqidb(strhm,strfind)
           if (.not.sfound) then  ! forza il confronto rimuovendo i :
               call s_s_delete(strhm,':',irep)
               if (irep > 0) sfound = s_eqi(strhm,strfind)
           endif
       endif
   endif
!
!  Try to read the old symbol
   if (.not.sfound) then
       !nwords = 0
       !do i=1,2
       !   if (len_trim(spg_data(kord)%symbol_alt(i)) > 0) then
       !       nwords = nwords + 1
       !       words(i) = spg_data(kord)%symbol_alt(i)
       !   endif
       !enddo
       !read(jfile,'(a)')line1
       !call cutst(line1)
       !call get_words_quotes(line1,words,nwords)
       !if (nwords > 0) then ! read only the first old symbol
       if (len_trim(spg_data(kord)%symbol_alt(1))> 0) then ! read only the first old symbol
!
!          se il vecchio simbolo differisce solo per :1 salto questo controllo per favorire il :2 (es. strfind=I 41/a ma old: I 41/a)
           checkw = .true.
           if (lens > 2) then
               checkw = .not.(spgn_long(lens-1:lens) == ':1' .and.  &
                        s_eqidb(spgn_long(:lens-2),spg_data(kord)%symbol_alt(1))) ! old symbol equal to spg without :1
           endif
           if (checkw) sfound = s_eqidb(spg_data(kord)%symbol_alt(1),strfind)
       endif
       !if (nwords == 2) then
       if (.not.sfound) then
           if (len_trim(spg_data(kord)%symbol_alt(2))> 0) then ! read only the first old symbol
!
!              only for symbol: H-3m, H-3c, ...
               if (spg_data(kord)%symbol_alt(2)(1:1) == 'H') sfound = s_eqidb(spg_data(kord)%symbol_alt(2),strfind)
           endif
       endif
       !if (sfound) backspace(jfile)
   endif
!
   end subroutine find_hm_symbol  

!---------------------------------------------------------------------------

   integer function is_colon_in_spgn(spgn)  result(pos)
!   
!  Check for colon in the space group symbol
!
   character(len=*), intent(in) :: spgn
   integer                      :: lens
   pos = 0
   lens = len_trim(spgn)
   if (lens >= 2) then
       if (spgn(lens-1:lens) == ':1' .or. spgn(lens-1:lens) == ':2'   &
      .or. spgn(lens-1:lens) == ':r' .or. spgn(lens-1:lens) == ':R'   &
      .or. spgn(lens-1:lens) == ':h' .or. spgn(lens-1:lens) == ':H') pos = lens-1
   endif
   end function is_colon_in_spgn

!---------------------------------------------------------------------------
!corr
!corr   subroutine spg_openfile(filename,j_in,ier)
!corr!
!corr!  Open symmetry file
!corr!
!corr   USE fileutil
!corr   character(len=*), intent(in) :: filename
!corr   integer, intent(out)         :: j_in
!corr   integer, intent(out)         :: ier
!corr!
!corr   call get_unit(j_in)
!corr   !open(newunit=j_in,file=filename,status='OLD',action='read',iostat=ier)
!corr   open(unit=j_in,file=filename,status='OLD',action='read',iostat=ier)
!corr!
!corr   end subroutine spg_openfile
!corr
!---------------------------------------------------------------------------

   subroutine spg_read_block(funit,spaceg,readsymbol,err)
!
!  Read info from syminfo.lib and fill the spaceg class
!
   USE strutil
   USE math_util, only: equal_matrix, equal_vector
   integer, intent(in)              :: funit
   type(spaceg_type), intent(inout) :: spaceg
   logical, intent(in)              :: readsymbol
   integer, intent(out), optional   :: err
   character(len=200)               :: line,sinfo
   integer                          :: nlong1,nlong2
   integer                          :: ier,iers
   integer                          :: leni
   character(len=40), dimension(2)  :: words
   integer                          :: nwords
   integer                          :: pos
   type(symop_type)                 :: symopc
   character(len=80), dimension(193) :: symstr
   character(len=10), dimension(2)   :: sympatt
   integer                           :: i,j
   integer                           :: ns
   real, dimension(2)                :: lim
   integer, dimension(2)             :: equ
   character(len=40)                 :: symb
   integer :: nold
!
   spaceg%nsymop_prim = 0
   spaceg%ncoper = 0
   spaceg%symcent = 0
!
   do 
      read(funit,'(a)',iostat=ier)line
      if (ier == 0) then
          call cutst(line,nlong1,sinfo,nlong2)

          select case(sinfo(1:nlong2))
             case ('number')
                   call s_to_i(line,spaceg%num,iers,leni)
                   spaceg%csys_code = csys_code_from_num(spaceg%num)

             case ('symbol')
                   call cutst(line,nlong1,sinfo,nlong2)
                   select case(sinfo(1:nlong2))
                           case ('Hall')
                             if (readsymbol) spaceg%symbol_hall = adjustl(rem_quotes(line))
                           case ('xHM')
                             symb = adjustl(rem_quotes(line))
                             if (len_trim(symb) > 0) then
                                 spaceg%symbol_xhm = symb
                             endif
                           case ('old')
                             nold = 0
                             call get_words_quotes(line,words,nwords)
                             do i=1,nwords
                                if(.not.s_eqi(words(i),spaceg%symbol_xhm)) then !salva solo gli old diversi da xhm
                                   nold = nold + 1
                                   spaceg%symbol_alt(nold) = words(i)
                                endif
                             enddo
                           case ('laue')
                             call get_words_quotes(line,words,nwords)
                             spaceg%laue_code = laue_code_from_name(words(2))
                           case ('pgrp')
                             call get_words_quotes(line,words,nwords)
                             spaceg%pgroup_code = pgroup_code_from_name(words(2))
                           case ('patt')
                             call get_words_quotes(line,words,nwords)
                             sympatt(1) = trim(words(1))
                             sympatt(2) = trim(words(2))
                             
                   end select
             case ('symop') 
                   spaceg%nsymop_prim = spaceg%nsymop_prim + 1
                   pos = index(line,",")
                   call s_blanks_insert(line,pos+1,pos+1)
                   pos = index(line,",",back=.true.)
                   call s_blanks_insert(line,pos+1,pos+1)
                   symstr(spaceg%nsymop_prim) = trim(line)

             case ('cenop') 
                   spaceg%ncoper = spaceg%ncoper + 1
                   call string_to_symop(line,symopc)
                   spaceg%coper(:,spaceg%ncoper) = symopc%trn

             case ('mapasu')
                   call cutst(line,nlong1,sinfo,nlong2)
                   if(sinfo(1:nlong2) == 'zero') then
                      spaceg%asu_descr = trim(line)
                      call cutst(line,nlong1,sinfo,nlong2)
                      call range_to_limits(sinfo,lim,equ)
                      spaceg%asulim(1) = lim(2)
                      spaceg%asueq(1) = equ(2)
                      call cutst(line,nlong1,sinfo,nlong2)
                      call range_to_limits(sinfo,lim,equ)
                      spaceg%asulim(2) = lim(2)
                      spaceg%asueq(2) = equ(2)
                      call cutst(line,nlong1,sinfo,nlong2)
                      call range_to_limits(sinfo,lim,equ)
                      spaceg%asulim(3) = lim(2)
                      spaceg%asueq(3) = equ(2)
                   endif

             case ('cheshire')
                    symb = trim(line)
                    if (len_trim(symb) > 0) then
                        spaceg%che_descr = symb
                        call cutst(line,nlong1,sinfo,nlong2)
                        call range_to_limits(sinfo,lim,equ)
                        spaceg%chelim(1) = lim(2)
                        call cutst(line,nlong1,sinfo,nlong2)
                        call range_to_limits(sinfo,lim,equ)
                        spaceg%chelim(2) = lim(2)
                        call cutst(line,nlong1,sinfo,nlong2)
                        call range_to_limits(sinfo,lim,equ)
                        spaceg%chelim(3) = lim(2)
                    endif

             case ('end_spacegroup')
                   exit
          end select 

      else
          exit
      endif
   enddo
!
   if (ier == 0) then
!
!      Fill symop matrices
       spaceg%nsymop = spaceg%nsymop_prim*spaceg%ncoper
       ns = 0
       do i=1,spaceg%nsymop_prim
          ns = ns + 1
          call string_to_symop(symstr(i),spaceg%symop(ns))
       enddo
       if (spaceg%nsymop /= spaceg%nsymop_prim) then
           do i=2,spaceg%ncoper
              do j=1,spaceg%nsymop_prim
                 ns = ns + 1
                 spaceg%symop(ns)%rot(:,:) = spaceg%symop(j)%rot(:,:)
                 spaceg%symop(ns)%trn(:) = spaceg%symop(j)%trn(:) + spaceg%coper(:,i)
                      !corr          tr(:) = spaceg%symop(ns)%trn
                 call symop_norm_trans(spaceg%symop(ns))
              enddo
           enddo
       endif
!
!      Fill symopstring
       do i=1,spaceg%nsymop
          call symop_to_string(spaceg%symop(i),spaceg%symopstr(i))
       enddo
!
!      Check for centrosymmetric space group
       spaceg%symcent = 0
       do i=1,spaceg%nsymop
          if (equal_matrix(spaceg%symop(i)%rot,-identity_mat)) then
              if(equal_vector(spaceg%symop(i)%trn,(/0.0,0.0,0.0/))) then
                 spaceg%symcent = 1
                 exit
              else
                 spaceg%symcent = 2
              endif
          endif
       enddo
!
!      Set number of symmetry operators (inversion excluded)
       if (spaceg%symcent == 1) then
           spaceg%nsym = spaceg%nsymop_prim / 2
       else
           spaceg%nsym = spaceg%nsymop_prim
       endif
!
!      Schoenflies notation
       spaceg%symbol_sch = symbol_sch(spaceg%num)
!
!      Patterson spacegroup
       if (sympatt(2) == 'Unknown') then
           spaceg%symbol_patt = trim(sympatt(2))
       else
           spaceg%symbol_patt = sympatt(1)(2:2)//trim(sympatt(2))
       endif
!  
!      Calcola le origini
       call calc_origins(spaceg)
!
!      Lattice
       spaceg%lattyp = lattice_type(spaceg%coper,spaceg%ncoper)
!
!      Frequency
       call spaceg%spg_frequency()
   endif
   if (present(err)) err = ier
!
   end subroutine spg_read_block

!---------------------------------------------------------------------------

   integer function laue_code_from_name(laue_string) result(code)
!
!  Get code of laue class
!
   USE strutil
   character(len=*), intent(in) :: laue_string
   integer                      :: i
   code = 0
   do i=1,size(laue_name) - 1
      if (s_eqi(laue_string,laue_name(i))) then
          code = i
          exit
      endif
   enddo
   end function laue_code_from_name

!---------------------------------------------------------------------------

   function laue_class(this)
!
!  Get name of laue class
!
   class(spaceg_type), intent(in) :: this
   character(len=7)               :: laue_class
   if (this%laue_code > 0 .and. this%laue_code <  size(laue_name)) then
       laue_class = laue_name(this%laue_code)
   else
       laue_class = laue_name(0)
   endif
   end function laue_class

!---------------------------------------------------------------------------

   integer function pgroup_code_from_name(pgroup_string) result(code)
!
!  Get code of point group 
!
   USE strutil
   character(len=*), intent(in) :: pgroup_string
   integer                      :: i
   code = 0
   do i=1,size(pgroup_name) - 1
      if (s_eqi(pgroup_string,pgroup_name(i))) then
          code = i
          exit
      endif
   enddo
   end function pgroup_code_from_name

!---------------------------------------------------------------------------

   function point_group(this)
!
!  Get point group name
!
   class(spaceg_type), intent(in) :: this
   character(len=7)               :: point_group
   if (this%pgroup_code > 0 .and. this%pgroup_code < size(pgroup_name)) then
       point_group = pgroup_name(this%pgroup_code)
   else
       point_group = pgroup_name(0)
   endif
   end function point_group

!---------------------------------------------------------------------------

   integer function csys_code_from_num(num) result(code)
!
!  Code of crystal system from number
!
   integer, intent(in) :: num
!
   select case(num)
      case (1:2)     ! Triclinic
        code = CS_Triclinic
      case (3:15)    ! Monoclinic
        code = CS_Monoclinic
      case (16:74)   ! Orthorombic
        code = CS_Orthorhombic
      case (75:142)  ! Tetragonal
        code = CS_Tetragonal
      case (143:167) ! Rhombohedral
        code = CS_Trigonal
      case (168:194) ! Hexagonal
        code = CS_Hexagonal
      case (195:230) ! Cubic
        code = CS_Cubic
   end select
!
   end function csys_code_from_num

!---------------------------------------------------------------------------
    
   subroutine string_to_symop(strop,symop,ier)
!
!  Convert symbol to symmetry operator
!
   character(len=*), intent(in)   :: strop
   type(symop_type), intent(out)  :: symop
   integer, intent(out), optional :: ier
   real, dimension(4,4,1)         :: rot
   integer                        :: ns 
   integer :: iflag
   ns = 1
   call symfr3(strop,1,ns,rot,iflag)
   symop%rot(:,:) = nint(rot(:3,:3,1))
   symop%trn(:) = rot(:3,4,1)
   call symop_norm_trans(symop)
   if (present(ier)) ier = iflag
   end subroutine string_to_symop

!---------------------------------------------------------------------------

   subroutine symop_to_string(symop,strop)
!
!  Convert symmetry operator in symbol
!
   USE strutil
   type(symop_type), intent(in)  :: symop
   character(len=*), intent(out) :: strop
   real, dimension(4,4,1)        :: rot
   integer                       :: pos
   rot(:3,:3,1) = symop%rot(:,:)
   rot(:3,4,1) = symop%trn(:)
   rot(4,:4,1) = [0,0,0,1]
   call symtr3(1,rot,strop,0)
   strop = lower(strop)
   strop = s_blank_delete(strop)
!
!  add space after comma
   pos = index(strop,",")
   call s_blanks_insert(strop,pos+1,pos+1)
   pos = index(strop,",",back=.true.)
   call s_blanks_insert(strop,pos+1,pos+1)
!
   end subroutine symop_to_string

!---------------------------------------------------------------------------

   subroutine symop_norm_trans(symop)
!
!  Normalize translation of symmetry operator
!
   type(symop_type), intent(inout) :: symop
   symop%trn(:) = mod(symop%trn+10,1.0)
   end subroutine symop_norm_trans

!---------------------------------------------------------------------------
   
   subroutine calc_origins(spaceg)
!
!  Return the number of alternate origins for spacegroup
!
   type(spaceg_type), intent(inout)        :: spaceg
   real, dimension(4,4,spaceg%nsymop_prim) :: rsym
   integer                                 :: i
!
   call calc_orig_ps_new(spaceg)
   call get_indipendent_origins(spaceg)
                  return
   do i=1,spaceg%nsymop_prim
      rsym(:3,:3,i) = spaceg%symop(i)%rot(:,:)
      rsym(:3,4,i) = spaceg%symop(i)%trn(:)
      rsym(4,:4,i) = (/0.0,0.0,0.0,1.0/)
   enddo
   call calc_orig_ps(spaceg%nsymop_prim,rsym,   &
        spaceg%norig,spaceg%orig,spaceg%polarx,spaceg%polary,spaceg%polarz)
!
   end subroutine calc_origins

!---------------------------------------------------------------------------

   function get_spg_info_hm(hm_symbol,alls,spos) result(info)
!
!  Cerca simboli alternativi a partire dall' H-M symbol
!
   USE strutil
   character(len=*), intent(in) :: hm_symbol
   type(sg_info_type)           :: info
   logical, optional            :: alls ! forse search in H-M standard symbol
   integer, optional            :: spos
   integer                      :: i
   character(len=LEN_HM)        :: str
   integer                      :: kfound
!
   info = sg_info_type()
   kfound = 0
   if (present(spos))spos = 0
   if (len_trim(hm_symbol) == 0) return
   do i=1,size(sg_info)
      str = get_hm1(sg_info(i))
      if(s_eqidb(hm_symbol,str)) then
         info = sg_info(i)
         info%hall = adjustl(info%hall)
         kfound = i
      endif
   enddo
   if (present(alls) .and. kfound == 0) then
       if (alls) then
           do i=1,size(sg_info)
              if(s_eqidb(hm_symbol,sg_info(i)%hm2)) then
                 info = sg_info(i)
                 info%hall = adjustl(info%hall)
                 kfound = i
              endif
           enddo
       endif
   endif
   if (present(spos)) spos = kfound
!
   end function get_spg_info_hm

!---------------------------------------------------------------------------

   function get_spg_info_num(num,spos)  result(info)
!
!  Cerca simboli alternativi from number
!
   integer, intent(in)  :: num
   type(sg_info_type)   :: info
   integer, intent(out) :: spos
   integer              :: i
!
   spos = 0
   if (num == 0) return
   do i=1,size(sg_info)
      if (sg_info(i)%num == num) then
          info = sg_info(i)
          info%hall = adjustl(info%hall)
          spos = i
          exit ! get first occurance
      endif
   enddo
!
   end function get_spg_info_num

!---------------------------------------------------------------------------

   subroutine range_to_limits(srange,limit,equal)
!
!  Convert string of type 0<=y<=1/4 in limit = (/0,0.25/) equal = (/1,1/)
!  other example: 0<=x<-1 limit = (/0,-1/) equal = (/1,0/)
!
   USE strutil
   character(len=*), intent(in)    :: srange
   real, dimension(2), intent(out) :: limit
   integer, dimension(2)           :: equal
   character(len=1)                :: ch
   logical                         :: in_value,frac,neg
   real                            :: val1,val2
   integer                         :: ier,len
   integer                         :: i
!
   in_value = .true.
   frac = .false.
   neg = .false.
   equal(:) = 0
   do i=1,len_trim(srange)
      ch = srange(i:i)
      select case (ch)
         case ('<')
           if (in_value) then  
!              fine lettura valore inferiore
               limit(1) = val1
               if (frac) limit(1) = limit(1) / val2
               if (neg) limit(1) = -limit(1)
               neg = .false.
               frac = .false.
               in_value = .false.
           else                
!              inizio lettura valore superiore
               in_value = .true.
           endif
         case ('-')
           neg = .true.
         case ('/')
           frac = .true.
         case ('=')
           if (in_value) then
               equal(2) = 1
           else
               equal(1) = 1
           endif
         case (';',' ')
         case default
           if (in_value) then
               if (frac) then
                   ier = s_to_r(ch,val2,len)
               else
                   ier = s_to_r(ch,val1,len)
               endif
           endif
      end select
   enddo
!
!  finishing upper value
   limit(2) = val1
   if (frac) limit(2) = limit(2) / val2
   if (neg) limit(2) = -limit(2)
!
   end subroutine range_to_limits

!---------------------------------------------------------------------------

   logical function symop_equal(symop1,symop2)  result(equal)
!
!  Compare two symmetry operators
!
   type(symop_type), intent(in) :: symop1,symop2
   integer                      :: i,j
   real, parameter              :: eps = 0.0002
!
   equal = .false.
   do i=1,3
      if (abs(symop1%trn(i) - symop2%trn(i)) > eps) return
   enddo
!
   do i=1,3
      do j=1,3
         if (abs(symop1%rot(i,j) - symop2%rot(i,j)) > eps) return
      enddo
   enddo
   equal = .true.
!
   end function symop_equal
   
!---------------------------------------------------------------------------

   logical function symop_equal_sets(nsym1, symop1, nsym2, symop2) result(equal)
!
!  Compare an entire set of symmetry operators
!
   type(symop_type), dimension(:), intent(in) :: symop1,symop2
   integer, intent(in)                        :: nsym1, nsym2
   logical                                    :: equal_symop
   integer                                    :: i,j
!
   equal = .false.
   if (nsym1 /= nsym2) return
!
!  Compare all pairs of symmetry operators
   do i=1,nsym1
      do j=1,nsym2
         equal_symop = symop_equal(symop1(i),symop2(j)) 
         if (equal_symop) exit
      enddo
      if (.not. equal_symop) exit ! operatore i non trovato
   enddo
   equal = equal_symop
!
   end function symop_equal_sets

!---------------------------------------------------------------------------

   logical function spg_equal(spg1,spg2)
!
!  Check if spg1 and spg2 are equal
!
   type(spaceg_type), intent(in) :: spg1,spg2
!
   spg_equal = symop_equal_sets(spg1%nsymop, spg1%symop, spg2%nsymop, spg2%symop)
!
   end function spg_equal

!---------------------------------------------------------------------------

   function lattice_type(coper,ncoper)   result(lattyp)
!
!  Get lattice symbol from centering operators
!
   USE math_util, only: equal_vector
   real, dimension(:,:), intent(in)   :: coper  ! (3,4) centering operators  
   integer, intent(in)                :: ncoper ! number of centering operators
   character(len=1)                   :: lattyp
   integer, dimension(3)              :: tt
   integer                            :: i,j
   integer, dimension(3,6), parameter :: lattice=reshape((/0,6,6, 6,0,6, &
                                                 6,6,0, 6,6,6, 8,4,4, 4,8,8/),shape(lattice))
   logical :: latt_p, latt_a, latt_b, latt_c, latt_i, latt_r, latt_f, latt_z
   integer, dimension(6) :: latt_given
!
   if (ncoper == 1) then
       lattyp = 'P'
   elseif (ncoper > 4) then
       lattyp = 'Z'   ! non conventional centring
   else
       latt_p = .true.
       latt_a = .false.
       latt_b = .false.
       latt_c = .false.
       latt_i = .false.
       latt_r = .false.
       latt_f = .false.
       latt_z = .false.
       do i=2,ncoper
          tt(:) = nint(12.0*coper(:,i))
          latt_given(:) = 0
          do j=1,6
             if (equal_vector(tt,lattice(:,j))) then
                 latt_given(j) = 1
                 select case(j)
                    case (1) 
                      latt_a = .true.
                    case (2) 
                      latt_b = .true.
                    case (3) 
                      latt_c = .true.
                    case (4) 
                      latt_i = .true.
                    case (5,6) 
                      latt_r = .true.
                 end select
                 exit
             endif
          enddo
       enddo

       if (latt_z) then
           lattyp="Z"
           return
       end if
       if ( (latt_a .and. latt_b .and. latt_c) .or. (latt_a .and. latt_b) .or. &
            (latt_a .and. latt_c) .or. (latt_b .and. latt_c) ) then
            latt_f=.true.
            latt_a=.false.
            latt_b=.false.
            latt_c=.false.
            latt_p=.false.
            latt_i=.false.
       end if
       if (latt_p) lattyp="P"
       if (latt_a) lattyp="A"
       if (latt_b) lattyp="B"
       if (latt_c) lattyp="C"
       if (latt_i) lattyp="I"
       if (latt_r) lattyp="R"
       if (latt_f) lattyp="F"
   endif
!
   end function lattice_type

!---------------------------------------------------------------------------

   function lattice_symbol(spaceg)   result(lsymb)
!
!  Extract the lattice symbol
!
   USE strutil
   class(spaceg_type), intent(in) :: spaceg
   character(len=2)              :: lsymb
!
   lsymb = lower(cry_sys(spaceg%csys_code)(1:1)) // spaceg%lattyp
!
   end function lattice_symbol

!---------------------------------------------------------------------------

   subroutine calc_orig_ps_new(spaceg)
!
!  creates list of equivalent origins for the named spacegroup.
!  C.Cuocci modified the Alexei Vagin's routine  (CCP4)
!  Reference: International Tables for Crystallography (2001). Tables 2.2.3.2 & 2.2.3.4 in Vol. B
!
   USE fractionm
   type(spaceg_type), intent(inout) :: spaceg
   real, dimension(8), parameter    :: xor = (/0,2,6,4,8,3,9,10/) / 12.0 ! 1/6 1/2 1/3 2/3 1/4 3/4 5/6
   integer                          :: k1,k2,k3
   real, dimension(3)               :: tryor
   integer                          :: i
   real, dimension(3,3)             :: mat
   real, dimension(3)               :: vet
   integer                          :: nver
   integer                          :: nc
   logical                          :: kpr = .false.
   logical                          :: verif
   real, dimension(3)               :: xpos,xs
   type(fract_type), dimension(3)   :: fr
!
   spaceg%polarx = .true.
   spaceg%polary = .true.
   spaceg%polarz = .true.
!  find which axes are "polar" and cannot have alternate origins.
!   0.13, 0.17, 0.19 cannot be special positions in any spacegroup.
   xs(:) = (/0.13,0.17,0.19/)
   do i=2,spaceg%nsymop_prim
      xpos(:) = matmul(spaceg%symop(i)%rot,xs)
      if((abs(xs(1)-xpos(1))) > 0.01) spaceg%polarx=.false.
      if((abs(xs(2)-xpos(2))) > 0.01) spaceg%polary=.false.
      if((abs(xs(3)-xpos(3))) > 0.01) spaceg%polarz=.false.
   enddo
   if (kpr) write(72,*)'Polar=',spaceg%polarx, spaceg%polary, spaceg%polarz
!  first origin is 0,0,0
   spaceg%norig = 1
   spaceg%orig(:,spaceg%norig) = (/0,0,0/)
!
!  check which points can be an alternate origin.
!                   1/2 1/3 2/3 1/4 3/4
!--- only six possibilities which are 0  1/2 1/3 2/3 1/4 3/4
   do k1=1,8
      do k2=1,8
         do k3=1,8
            if(k1 == 1 .and. k2 == 1 .and. k3 == 1) cycle
            if( spaceg%polarx .and. k1 /= 1) cycle
            if( spaceg%polary .and. k2 /= 1) cycle
            if( spaceg%polarz .and. k3 /= 1) cycle
            tryor(:) = (/xor(k1),xor(k2),xor(k3)/)
            nver = 1
            do i=2,spaceg%nsymop_prim
               do nc = 1,spaceg%ncoper
                  !vet = matmul((spaceg%symop(i)%rot(:,:) - identity_mat(:,:)),tryor) - spaceg%coper(:,nc) ! bug ifort 19.1.2.254 with -check all
                  mat = spaceg%symop(i)%rot(:,:) - identity_mat(:,:)
                  vet = matmul(mat,tryor) - spaceg%coper(:,nc)
                  verif = all(mod(abs(vet)+10,1.0) <= 0.05) 
                  if (kpr)then
                      fr = fractional(tryor)
                      write(72,'(3a,3f10.4,l4,a,i0,a,i0)')fr(1)%string(),fr(2)%string(),fr(3)%string(),vet,verif,' op=',i,' c=',nc
                      !write(72,*)nc,i,'vet=',vet,verif
                      !write(72,*)'mod=',mod(abs(vet)+10,1.0)
                  endif
                  if (verif) exit
               enddo
               if (verif) then
                   nver = nver + 1
               else
                   exit
               endif
            enddo
            if (nver == spaceg%nsymop_prim) then
                spaceg%norig = spaceg%norig + 1
                spaceg%orig(:,spaceg%norig) = tryor(:)
                if (spaceg%norig == 16) return ! if more then 16 symmetry operators are wrong 
            endif
         enddo
      enddo
   enddo
   end subroutine calc_orig_ps_new
   
!---------------------------------------------------------------------------

   subroutine get_indipendent_origins(spaceg)
!
!  Get the indipendend origins removing the redundant origins
!
   USE math_util, only: equal_vector
   type(spaceg_type), intent(inout) :: spaceg
   integer                          :: i,j,k
   logical, dimension(16)           :: accept
   real, dimension(3)               :: ortr,coper(3)
!
   if (spaceg%ncoper == 1) then
!      non ci sono origini ridondanti
       spaceg%norig_ind = spaceg%norig
       do i=1,spaceg%norig
          spaceg%orig_ind(:,i) = spaceg%orig(:,i)
       enddo
   else
       accept(:spaceg%norig) = .true.
       loop_orig: do i=1,spaceg%norig
             if (.not.accept(i)) cycle
             do j=2,spaceg%ncoper
                coper(:) = spaceg%coper(:,j)
                if (spaceg%polarx) coper(1) = 0
                if (spaceg%polary) coper(2) = 0
                if (spaceg%polarz) coper(3) = 0
                ortr(:) = spaceg%orig(:,i) + coper(:)
                ortr(:) = mod(ortr+10,1.0)
                do k=2,spaceg%norig
                   if ( k == i ) cycle
                   if (.not.accept(k)) cycle
                   if (equal_vector(spaceg%orig(:,k),ortr(:))) then
!                        come decidere cosa eliminare e tenere fra k e j?
!                        es. 0.5 0.5 0.5  e' meglio di 0 0 0.5 in cella F
                         if (spaceg%ncoper > 2) then   ! per cella F faccio cosi'
                             if (all(spaceg%orig(:,k) == spaceg%orig(1,k))) then  
                                 accept(i) = .false.
                                 cycle loop_orig
                             else
                                 accept(k) = .false.
                                 exit
                             endif
                         else
                             accept(k) = .false.
                             exit
                         endif
                   endif
                enddo
             enddo
       enddo loop_orig
       spaceg%norig_ind = 0
       do i=1,spaceg%norig
          if (accept(i)) then
              spaceg%norig_ind = spaceg%norig_ind + 1
              spaceg%orig_ind(:,spaceg%norig_ind) = spaceg%orig(:,i)
          endif
       enddo
   endif
!
   end subroutine get_indipendent_origins

!---------------------------------------------------------------------------

   subroutine calc_orig_ps(nsym,rsym,norig,orig,lpaxisx,lpaxisy,lpaxisz)
!
! -p- calc_orig_ps - creates list of equivalent origins for the named spacegroup.
!
!                  arguments
!                  ---------
!                    (i) nsym - number of symmetry operations
!                    (i) rsym(4,4,nsym) - symmetry ops stored as 4x4 matrices
!                    (o) norig - number of origins.
!                    (o) orig(3,i) - vector of alternate origin 
!                               (for example : 0.5,0.0,0.5)
!                               only positive components.
!                               include vector: (0,0,0)
!                    (o) lpaxisx - logical; set true if s/grp is polar along x axis
!                    (o) lpaxisy - logical; set true if s/grp is polar along y axis
!                    (o) lpaxisz - logical; set true if s/grp is polar along z axis
!
!                        
!    taken from alexei vagin
!
   integer, intent(in)                :: nsym
   real, dimension(:,:,:), intent(in) :: rsym
   integer, intent(out)               :: norig
   real, dimension(:,:), intent(out)  :: orig
   logical, intent(out)               :: lpaxisx,lpaxisy,lpaxisz
   real                               :: rsymd(3,3),x,y,z,xx,yy,zz,chk
   integer                            :: is(3),ifx,ify,ifz,i,k1,k2,k3,j,k,l
!   these 6 fractions can represent an origin shift
   integer, dimension(6), parameter   :: id = (/0,6,4,8,3,9/)
!                                           1/2 1/3 2/3 1/4 3/4
!  initialise logicals
   lpaxisx=.true.
   lpaxisy=.true.
   lpaxisz=.true.
!  find which axes are "polar" and cannot have alternate origins.
   ifx=0
   ify=0
   ifz=0
!   0.13, 0.17, 0.19 cannot be special positions in any spacegroup.
   x=0.13
   y=0.17
   z=0.19
   do i=2,nsym
      xx=     rsym(1,1,i)*x + rsym(1,2,i)*y + rsym(1,3,i)*z
      yy=     rsym(2,1,i)*x + rsym(2,2,i)*y + rsym(2,3,i)*z
      zz=     rsym(3,1,i)*x + rsym(3,2,i)*y + rsym(3,3,i)*z

      if((abs(x-xx)).gt.0.01) lpaxisx=.false.
      if((abs(y-yy)).gt.0.01) lpaxisy=.false.
      if((abs(z-zz)).gt.0.01) lpaxisz=.false. 
   enddo

!  first origin is 0,0,0
   norig=1
   orig(1,1)=0.0
   orig(2,1)=0.0
   orig(3,1)=0.0

!  check which points can be an alternate origin.
!                   1/2 1/3 2/3 1/4 3/4
!--- only six possibilities which are 0,  1/2 1/3 2/3 1/4 3/4

   do k1=1,6
    do k2=1,6
     do k3=1,6
      if(k1.eq.1.and.k2.eq.1.and.k3.eq.1) go to 200
       is(1)=id(k1)
       is(2)=id(k2)
       is(3)=id(k3)

       if( lpaxisx.and.is(1).ne.0) go to 200
       if( lpaxisy.and.is(2).ne.0) go to 200
       if( lpaxisz.and.is(3).ne.0) go to 200

!      Let [Si] =[RSYMi] be (3x4) symmetry operator.
!      Need to Check if the symmetry operator shifts of each alternate origin 
!      [ORx,ORy,ORz)  are preserved for each symmetry operator.
!      Origin (0,0,0) shifts to        Ti(1),     Ti(2)      Ti(3) 
!                                == RSYMi(1,4),RSYMi(2,4),RSYMi(3,4) 

!     [RSYMi] [OR]  =  [OR] + [Ti] + n[I]  = [1 0 0 RSYMi(1,4)] [OR1] +  n[I]
!                                            [0 1 0 RSYMi(2,4)] [OR2]
!                                            [0 0 1 RSYMi(3,4)] [OR3]

!     Hence  [RSYMi(1,1) -1   RSYMi(1,2)      RSYMi(1,3)      0] [OR1]   = n[I]
!            [RSYMi(2,1)      RSYMi(2,2) -1   RSYMi(2,3)      0] [OR2] 
!            [RSYMi(3,1)      RSYMi(3,2)      RSYMi(3,3) -1   0] [OR3] 
!            [   0                0               0           1] [1  ]

!      Use RSYM(..1) to respresent indentity.. Enough to use 3x3 matrix..
       do j=2,nsym
         do k=1,3
          do l=1,3
           rsymd(k,l)=rsym(k,l,j)-rsym(k,l,1)
          enddo
         enddo
         do k=1,3
           chk= rsymd(k,1)*real(is(1))+ rsymd(k,2)*real(is(2))+ rsymd(k,3)*real(is(3)) + 12000
           if(abs(mod(chk,12.0)).gt.0.05) go to 200
         enddo
       enddo
       norig=norig+1
       orig(1,norig)=real(is(1))/12.0
       orig(2,norig)=real(is(2))/12.0
       orig(3,norig)=real(is(3))/12.0
 200   continue
     enddo
    enddo
   enddo

   return
   end subroutine calc_orig_ps

!---------------------------------------------------------------------------

   subroutine symtr3(nsm,rsm,symchs,iprint)
!
!--ymtr3(nsm,rsm)
!       symmetry translation from matrix back to characters
!  
!       this translates the symmetry matrices into int tab
!       character strings                      
!  
!       it gives the real space operations.
!            eg     x,y,z
!            eg     -y,x-y, z
!       that is more complicated than you might think!!
!  
!--rguments :
!  
!  sm       (I)     integer         number of symmetry operations
!  
!  sm       (I)     real            array of dimension (4,4,at least nsm)
!                                   containing symmetry operations on input
!  
!  ymchs    (O)     character*(*)   array of dimension at least nsm
!                                   containing int tab char strings on output
!  
!  print    (I)     integer         print flag
!                                   =0 no printing
!                                   =1 print the int tab strings
!  
!  .. scalar arguments ..
   integer iprint,nsm
!  ..
!  .. array arguments ..
   real rsm(4,4,*)
   character symchs(*)*(*)
!  ..
!  .. local scalars ..
   integer i1,i2,ich,ist,itr,jcount,jdo10,jdo20,jdo30,jdo40,irsm,lstr,icount,istart,ineg,istr
   character(len=80)  :: symchk
   character(len=400) ::  strout
!  ..
!  .. local arrays ..
   integer npntr1(10),npntr2(10)
   character axiscr(3)*1,numb(9)*1
!  ..
!  .. intrinsic functions ..
   intrinsic abs,min,nint
!  ..
!  .. data statements ..
!
   data axiscr/'x','y','z'/
   data numb/'1','2','3','4','5','6','7','8','9'/
   data npntr1/0,1,1,1,0,1,0,2,3,5/
   data npntr2/0,6,4,3,0,2,0,3,4,6/
!  ..
!
   do 40 jdo40 = 1,nsm
!
!-- clear symchs
!
     symchs(jdo40) = ' '
     ich = 1
     symchs(jdo40) (ich:ich) = '0'
!
     do 20 jdo20 = 1,3
!
!-- ist is flag for first character of operator
!
     ist = 0
! 
     do 10 jdo10 = 1,4
!
       if (rsm(jdo20,jdo10,jdo40).ne.0) then
         irsm = nint(abs(rsm(jdo20,jdo10,jdo40)))
!
         if (rsm(jdo20,jdo10,jdo40).gt.0.0 .and. ist.gt.0) then
           if (ich.gt.len(symchs(1))) write(0, '("symtr3: character array too short")')
           symchs(jdo40) (ich:ich) = '+'
           ich = ich + 1
         end if
!
         if (rsm(jdo20,jdo10,jdo40).lt.0) then
           if (ich.gt.len(symchs(1))) write(0, '("symtr3: character array too short")')
           symchs(jdo40) (ich:ich) = '-'
           ist = 1
           ich = ich + 1
         end if
!
         if (jdo10.ne.4) then
           if (ich.gt.len(symchs(1))) write(0, '("symtr3: character array too short")')
           if(irsm.ne.1) write(symchs(jdo40) (ich:ich+1),'(i1,a1)') irsm, axiscr(jdo10)
           if(irsm.eq.1) write(symchs(jdo40) (ich:ich+1),'(1x,a1)') axiscr(jdo10)
!           symchs(jdo40) (ich:ich) = axiscr(jdo10)
           ist = 1
           ich = ich + 2
         end if
!
         if (jdo10.eq.4 .and. rsm(jdo20,4,jdo40).ne.0) then
           itr = nint(abs(rsm(jdo20,4,jdo40)*12.0))
           i1 = npntr1(itr)
           i2 = npntr2(itr)
           if (ich+2.gt.len(symchs(1))) write(0, '("symtr3: character array too short")')
           symchs(jdo40) (ich:ich+2) = numb(i1)//'/'//numb(i2)
           ich = ich + 3
         end if
       end if
10   continue
!
!---- add comma  space
!
       if (jdo20.ne.3) then
! nothing reset into this space - advance counter
         if( symchs(jdo40) (ich:ich) .eq. '0') ich = ich + 1
         if (ich+2.gt.len(symchs(1))) write(0, '("symtr3: character array too short")')
         symchs(jdo40) (ich:ich+2) = ',  '
         ich = ich + 3
         symchs(jdo40) (ich:ich) = '0'
       end if
20   continue
!
!---- get rid of spaces after - sign; get rid of first  ;
!----- formats " - x " etc..
!
     lstr = len_trim(symchs(jdo40))
       symchk = '                                   '
       icount = 0
       istart = 1
       if( symchs(jdo40)(1:1) .eq. ' ') istart = 2
       ineg = 0
       do 22 istr = istart,lstr
         if(ineg .eq. -1 .and. symchs(jdo40)(istr:istr) .eq. ' ') go to 22
       
         ineg = 1
         if(symchs(jdo40)(istr:istr) .eq. '-' ) ineg = -1
         icount = icount + 1
         symchk(icount:icount) = symchs(jdo40)(istr:istr)
22     continue

       symchs(jdo40) = symchk
!
!- write a message if required
!
   if (iprint.eq.1) then
          write (strout,fmt='(a,i3,5x,a)') 'symmetry',jdo40,symchs(jdo40) (1:min(350,max(1,len_trim(symchs(jdo40)))))
          do 30 jdo30 = 1,4
            write (strout,fmt='(4f6.2)') (rsm(jdo30,jcount,jdo40),jcount=1,4)
30     continue
   end if
40 continue
   end subroutine symtr3

!---------------------------------------------------------------------------

   subroutine symfr3(icol,i1,ns,rot,eflag)
!  
!
!--read and interpret symmetry operations
!
!--arguments :
!
!  icol      (i)	character*80    line containing the symmetry ops
!
!  i1        (i)	integer         first character to look at
!                            	(say after keyword 'sym')
!
!  ns        (i/o)	integer         is the number of the first symmetry
!                            	operation to be read, & returns with the
!                            	number of the last one read (ie you can
!                            	have more than one on a line!)
!
!  rot       (o)	real            array (4,4,at_least_ns),
!                            	on exit contains the real-space
!                            	symmetry matrices, in standard
!                            	convention, ie
!                              	[x']    = [s][x]
!                  			x'(i)=sum(j=1,3)rot(i,j,ns)*x(j) + rot(i,4,ns)
!
!                  			rot(i,4,ns) contains the fractional translations
!
!  eflag     (o)	integer         error flag - on exit,
!                               	if 0 then ok,
!                               	gt 0, an error occurred.
!
!
!  .. scalar arguments ..
   character(len=*), intent(in) :: icol
   integer, intent(in)          :: i1
   integer, intent(inout)       :: ns
   real, intent(out)            :: rot(4,4,*)
   integer, intent(out)         ::  eflag
!  ..
!  .. local scalars ..
   real a,s,t
   integer i,icomst,ierr,ifound,imax,ip,isl,j,jdo40,jdo50,jdo80,nop,np
   character ich*1
!  ..
!  .. data statements ..
   integer, dimension(10), parameter          :: num = (/1,2,3,4,5,6,7,8,9,0/)
   character(len=1), dimension(10), parameter :: inum = (/'1','2','3','4','5','6','7','8','9','0'/)
!  ..
   logical :: kpr=.false.
!
   imax = len_trim(icol)
   ierr = 0
   eflag = 0
!
   i = i1 - 1
   ns = ns - 1
30 continue
   ns = ns + 1
   nop = 1
!
   do 50 jdo50 = 1,4
     do 40 jdo40 = 1,4
       rot(jdo50,jdo40,ns) = 0.0
40   continue
50 continue
!
   rot(4,4,ns) = 1.0
60 continue
   s = 1.0
!
!--set j=4 for translation vector
!
   j = 4
   t = 0.0
   ip = 0
   np = 0
   isl = 0
   ifound = 0
   icomst = 0
70 continue
   i = i + 1
!
   if (i.le.imax) then
     ich = icol(i:i)
!
     if (ich.eq.' ') then
       go to 70
     else if (ich.ne.',' .and. ich.ne.'*') then
       ifound = 1
!
       if (ich.eq.'x' .or. ich.eq.'X') then
         j = 1
         if (t.eq.0.0) t = s
         go to 70
       else if (ich.eq.'y' .or. ich.eq.'Y') then
         j = 2
         if (t.eq.0.0) t = s
         go to 70
       else if (ich.eq.'z' .or. ich.eq.'Z') then
         j = 3
         if (t.eq.0.0) t = s
         go to 70
       else if (ich.eq.'+') then
         s = 1.0
!
         if (t.eq.0.0 .and. j.eq.4) then
           go to 70
         else
           go to 100
         end if
!
       else if (ich.eq.'-') then
         s = -1.0
!
         if (t.eq.0.0 .and. j.eq.4) then
           go to 70
         else
           go to 100
         end if
!
       else if (ich.eq.'/') then
         isl = 1
         go to 70
       else if (ich.eq.'.') then
         ip = 1
         go to 70
       else
!
         do 80 jdo80 = 1,10
           if (ich.eq.inum(jdo80)) go to 90
80       continue
         if (kpr) then
             write(0,'("**symmetry operator error**")')
             write(0,'(a)')' **invalid character...' // ich // ' **'
         endif
!
!don't know how to fix now.
         if (kpr) write(0,'(a)')trim(icol)
!
         eflag = eflag + 1
         ierr = 1
         go to 70
90       a = num(jdo80)
!
         if (isl.eq.1) then
           t = t/a
         else if (ip.eq.1) then
           np = np + 1
           t = s*a/10**np + t
         else
           t = 10.0*t + a*s
         end if
!
         go to 70
       end if
     end if
   end if
!
   if (t.eq.0.0 .and. j.eq.4) then
     go to 110
   else
     icomst = 1
   end if
!
  100 rot(nop,j,ns) = t
   j = 4
   t = 0.0
   ip = 0
   np = 0
   isl = 0
   if (icomst.eq.0) go to 70
!
   if (ifound.eq.0 .and. i.le.imax) then
!
!--don't know how to fix. icol comes from outside
        if (kpr) then
            write(0,'("**symmetry operator error**")')
            write(0,'(" **blank operator field**")')
            write(0,'(a)')trim(icol)
        endif
   end if
!
   if (i.le.imax) then
     nop = nop + 1
!
     if (nop.le.3) then
       go to 60
     else
       go to 30
     end if
!
   else
     go to 120
   end if
!
110 continue         
    if (kpr) then
        write(0,'("**symmetry operator error**")')
        write(0,'("**no operator**")')
        write(0,'(a)')trim(icol)
    endif
!
   go to 140
!
  120 if (nop.ne.1 .or. ifound.ne.0) then
        if (nop.eq.3 .and. ifound.eq.1) then
          go to 130
        else
          ierr = 1
          if (kpr) then
              write(0,'("**symmetry operator error**")')
              write(0,'("**last general position is incomplete**")')
              write(0,'(a)')trim(icol)
          endif
        end if
      end if
      ns = ns - 1
 130  if (ierr.ne.1) return
!
  140 if (kpr) write(0,'("**symmetry operator error**")')
!
      eflag = eflag + 1
!
      end subroutine symfr3

!---------------------------------------------------------------------------

   subroutine symop_gen(symop,nsym,icent,latt)
!
!  Generate all symmetry operators from operators without inversion and centering
!
   type(symop_type), dimension(:), intent(inout) :: symop
   integer, intent(inout)                        :: nsym
   integer, intent(in)                           :: icent
   character(len=1), intent(in)                  :: latt
   real, dimension(3,3)                          :: ltr
   integer                                       :: nlat
   integer                                       :: i,j
   integer                                       :: nsymop
!
   if (icent == 1) then
       do i=1,nsym
          symop(nsym + i)%rot = matmul(symop(i)%rot,-identity_mat)
          symop(nsym + i)%trn(:) = symop(i)%trn(:)
       enddo
       nsym = nsym*2
   endif
!
   select case(latt)
      case ('P')
        nlat = 0
      case ('A')
        nlat = 1
        ltr(:,1) = ltr_a(:,2)
      case ('B')
        nlat = 1
        ltr(:,1) = ltr_b(:,2)
      case ('C')
        nlat = 1
        ltr(:,1) = ltr_c(:,2)
      case ('I')
        nlat = 1
        ltr(:,1) = ltr_i(:,2)
      case ('R')
        nlat = 2
        ltr(:,1) = ltr_r(:,2)
        ltr(:,2) = ltr_r(:,3)
      case ('F')
        nlat = 3
        ltr(:,1) = ltr_f(:,2)
        ltr(:,2) = ltr_f(:,3)
        ltr(:,3) = ltr_f(:,4)
      case default
        nlat = 0
   end select
!
   nsymop = nsym
   do i=1,nlat
      do j=1,nsymop
         nsym = nsym + 1
         symop(nsym)%rot(:,:) = symop(j)%rot(:,:)
         symop(nsym)%trn = symop(j)%trn + ltr(:,i)
         call symop_norm_trans(symop(nsym))
      enddo
   enddo
!
   end subroutine symop_gen

!---------------------------------------------------------------------------

   logical function spg_check_cell(spg,cell) result(stat)
!
!  Check for consistency between cell dimensions and spacegroup. Latter is identified from symmetry operators. 
!  Returns true if they are consistent, 0 if there is a problem
!
   type(spaceg_type), intent(in)  :: spg
   real, dimension(6), intent(in) :: cell
!   
   stat = .true.
   if (index(spg%symbol_xhm,':R') > 0) then
       stat = uc_is_rhombohedral(cell,0.01)
   else if (index(spg%symbol_xhm,':H') > 0) then
       stat = uc_is_hexagonal(cell,0.01)
   else if (spg%num >= 168 .and. spg%num <= 194) then
       stat = uc_is_hexagonal(cell,0.01)
   endif
!
   end function spg_check_cell

!---------------------------------------------------------------------------

   logical function spg_check_csys(spg,cell) result(stat)
!
!  Check if crystal system is compatible with cell. Not all check are performed
!
   type(spaceg_type), intent(in)  :: spg
   real, dimension(6), intent(in) :: cell
   integer                        :: csys_cell
!
   stat = .true.
   csys_cell = csys_from_cell(cell)
   select case (spg%csys_code)
     case (CS_Monoclinic,CS_Orthorhombic,CS_Tetragonal,CS_Cubic)
       stat = csys_cell >= spg%csys_code
   end select
!
   end function spg_check_csys

!---------------------------------------------------------------------------

   function spg_check_consistency(spg,symb_hall,symb_xhm)  result(err)
!
!  Check consistency between operators and symbols
!
   USE errormod
   USE strutil
   type(error_type)              :: err
   type(spaceg_type), intent(in) :: spg
   character(len=*), intent(in)  :: symb_hall, symb_xhm
   integer                       :: lxhm, lhall
   type(spaceg_type)             :: spgs
   logical                       :: found
   character(len=40)             :: symb
!
!  Try to compare symbol
   lxhm = len_trim(symb_xhm)
   if (lxhm > 0 .and. s_eqidb(spg%symbol_xhm,symb_xhm)) return
   lhall = len_trim(symb_hall)
   if (lhall > 0 .and. s_eqidb(spg%symbol_hall,symb_hall)) return
!
!  Try to compare symmetry operators
   if (lhall > 0) then
       call spg_load(spgs,spgstr=symb_hall,sfound=found)
       if (found) then
           if (.not.symop_equal_sets(spgs%nsymop, spgs%symop, spg%nsymop, spg%symop)) then
               call err%setw("Hall symbol '"//trim(symb_hall)//"' and symmetry operators are inconsistent")
               return
           endif
       endif
   endif
   if (lxhm > 0) then
       call spg_load(spgs,spgstr=symb_xhm,sfound=found)
       if (found) then
           if (.not.symop_equal_sets(spgs%nsymop, spgs%symop, spg%nsymop, spg%symop)) then
!
!              Manage space group with ':'
               if (index(spgs%symbol_xhm,':') > 0 .and. index(symb_xhm,':') == 0) then  
                   symb = spgs%symbol_xhm
                   if (index(symb,':1') > 0) then
                       call s_rep(symb,':1',':2')
                   elseif (index(symb,':2') > 0) then
                       call s_rep(symb,':2',':1')
                   elseif (index(symb,':R') > 0) then
                       call s_rep(symb,':R',':H')
                   elseif (index(symb,':H') > 0) then
                       call s_rep(symb,':H',':R')
                   else
                       return
                   endif
                   call spg_load(spgs,spgstr=symb,sfound=found)
                   if (.not.symop_equal_sets(spgs%nsymop, spgs%symop, spg%nsymop, spg%symop)) then
                       call err%setw("Symbol '"//trim(symb_xhm)//"' and symmetry operators are inconsistent")
                   endif
               else
                   call err%setw("Symbol '"//trim(symb_xhm)//"' and symmetry operators are inconsistent")
               endif
           endif
       endif
   endif
!
   end function spg_check_consistency

!---------------------------------------------------------------------------
  
   integer function find_symbol(xhm,hall) result(num)
!
!  Look up symbols in the dictionary
!
   character(len=*), intent(in), optional :: xhm,hall
   type(spaceg_type)                      :: spg
   logical                                :: sfound
!
   num = 0
   if (present(hall)) then
       if (len_trim(hall) > 0) then
           call spg_load(spg,spgstr=hall,sfound=sfound)
           num = -spg%num  ! negative if hall symbol was found
       endif
   endif
   if (present(xhm) .and. num == 0) then
       if (len_trim(xhm) > 0) then
           call spg_load(spg,spgstr=xhm,sfound=sfound)
           num = spg%num
       endif
   endif
!
   end function find_symbol

!---------------------------------------------------------------------------

   logical function polar3(spg)
   class(spaceg_type), intent(in) :: spg
   polar3 = spg%polarx .or. spg%polary .or. spg%polarz
   end function polar3

!---------------------------------------------------------------------------

   logical function polar1(spg,pos)
   class(spaceg_type), intent(in) :: spg
   integer, intent(in)            :: pos
   polar1 = .false.
   if (pos == 1 .and. spg%polarx) then
       polar1 = .true.
       return
   endif
   if (pos == 2 .and. spg%polary) then
       polar1 = .true.
       return
   endif
   if (pos == 3 .and. spg%polarz) then
       polar1 = .true.
       return
   endif
   end function polar1

!---------------------------------------------------------------------------

   subroutine code_for_polar(spg,code,cvalue)
!
!  Assign zero for code corresponding to polar direction
!
   class(spaceg_type), intent(in)       :: spg
   integer, dimension(:), intent(inout) :: code
   integer, intent(in), optional        :: cvalue
!
   if (present(cvalue)) then
       if (spg%polarx) code(1) = cvalue
       if (spg%polary) code(2) = cvalue
       if (spg%polarz) code(3) = cvalue
   else
       if (spg%polarx) code(1) = 0
       if (spg%polary) code(2) = 0
       if (spg%polarz) code(3) = 0
   endif
!
   end subroutine code_for_polar

!---------------------------------------------------------------------------

   logical function undef(spg)
   class(spaceg_type), intent(in) :: spg
   undef = .not.spg%defined
   end function undef

!---------------------------------------------------------------------------
  
   integer function csys_from_cell(cell) result(csys)
!
!  Lattice system from cell parameters. Code CS_trigonal = rhombohedral cell, CS_Hexagonal = hexagonal cell
!
   real, dimension(:), intent(in) :: cell
   real, parameter                :: tola = 0.001, tolb = 0.01
   logical                        :: a_eq_b, b_eq_c
   logical                        :: al_eq_bet, bet_eq_gam, al_eq_gam
!
   csys = CS_Triclinic
   a_eq_b = abs(cell(1) - cell(2)) <= tola
   b_eq_c = abs(cell(2) - cell(3)) <= tola
   al_eq_bet = abs(cell(4) - cell(5)) <= tolb
   bet_eq_gam = abs(cell(5) - cell(6)) <= tolb
   if (a_eq_b) then
       if(b_eq_c) then
          if (al_eq_bet .and. bet_eq_gam) then
              if (abs(cell(4) - 90.0) <= tolb) then
                  csys = CS_Cubic    ! a=b=c; al=bet=gam=90
              else
                  csys = CS_Trigonal ! a=b=c; al=bet=gam /= 90  (rhombohedral cell)
              endif
              return 
          endif
       else
          if (al_eq_bet) then
              if (bet_eq_gam) then
                  if ((abs(cell(4) - 90.0) <= tolb)) then
                       csys = CS_Tetragonal
                       return
                  endif
              else
                  if ((abs(cell(4) - 90.0) <= tolb)) then
                      if (abs(cell(6) - 120.0) <= tolb) then
                          csys = CS_Hexagonal     ! or CS_Trigonal; a=b/=c; al=bet; gam=120 (hexagonal cell)
                          return                  ! hexagonal cell could be CS_Trigonal or CS_Hexagonal
                      endif
                  endif
              endif
          endif
       endif
   endif
!
   al_eq_bet = al_eq_bet .and. (abs(cell(4) - 90.0) <= tolb)
   bet_eq_gam = bet_eq_gam .and. (abs(cell(5) - 90.0) <= tolb)
   al_eq_gam = (abs(cell(4) - cell(6)) <= tolb) .and. (abs(cell(4) - 90.0) <= tolb)
   if (al_eq_bet .and. bet_eq_gam) then
       csys = CS_Orthorhombic
   elseif (al_eq_bet .or. bet_eq_gam .or. al_eq_gam) then
       csys = CS_Monoclinic
   endif
!
   end function csys_from_cell

!---------------------------------------------------------------------------

   integer function lattice_system(spg,cell)  result(jsys)
!
!  Crystal system + axis 
!
   type(spaceg_type), intent(in)  :: spg
   real, dimension(6), intent(in) :: cell
!
   select case (spg%csys_code)
     !case (CS_Triclinic); jsys = 1
     case (CS_Monoclinic); jsys = 2
     case (CS_Orthorhombic); jsys = 3
     case (CS_Tetragonal); jsys = 4
     case (CS_Hexagonal); jsys = HEXA_LATT  ! hexagonal lattice
     case (CS_Cubic); jsys = 6
     case (CS_Trigonal)             ! Trigonal: hexagonal axis or rhombohedral axis
       if (index(spg%symbol_xhm,':R') > 0 .or. uc_is_rhombohedral(cell,0.01)) then
           jsys = RHOMB_LATT  ! rhombohedral lattice
       else
           jsys = HEXA_LATT  ! hexagonal lattice
       endif
     case default
       jsys = 1  ! set 1 for undefined space group
   end select
!
   end function lattice_system

!---------------------------------------------------------------------------

   logical function rhomb_axes(spg,cell)
!
!  Check for rhombohedral axes
!  R lattice: check cell parametrs
!      rhombohedral axes :  A = B  = C, ALPHA = BETA = GAMMA
!      hexagonal axes    :  A = B /= C, ALPHA = BETA = 90, GAMMA = 120
!
   character(len=*), intent(in)   :: spg
   real, dimension(6), intent(in) :: cell
!
   !rhomb_axes = uc_is_rhombohedral(cell,0.01) .and. (spg(1:1) == 'R')
   !rhomb_axes = index(spg,':R') > 0 .or. uc_is_rhombohedral(cell,0.01)
   rhomb_axes = index(spg,':R') > 0 .or. (uc_is_rhombohedral(cell,0.01) .and. cell(4) /= 90.) 
!
   end function rhomb_axes

!---------------------------------------------------------------------------

   function axis_direction(spg)  result(dir)
!
!  Direction of axis 2 
!
   class(spaceg_type), intent(in) :: spg
   character(len=1)               :: dir
!
   if (spg%symop(2)%rot(1,1) == spg%symop(2)%rot(2,2)) then        ! 2 along c
       dir = 'c'
   else if (spg%symop(2)%rot(1,1) == spg%symop(2)%rot(3,3)) then   ! 2 along b
       dir = 'b'
   else if (spg%symop(2)%rot(2,2) == spg%symop(2)%rot(3,3)) then   ! 2 along a
       dir = 'a'
   else
       dir = ' '
   endif
!
   end function axis_direction

!--------------------------------------------------------------------------------------

   subroutine save_space_bin(unitbin,space)
!
!  Write space group on binary file
!
   integer, intent(in)           :: unitbin
   type(spaceg_type), intent(in) :: space
!   
   write(unitbin)space
!
   end subroutine save_space_bin

!--------------------------------------------------------------------------------------

   subroutine read_space_bin(unitbin,space,err)
!
!  Read space group from binary file
!
   USE errormod
   integer, intent(in)            :: unitbin
   type(spaceg_type), intent(out) :: space
   type(error_type), intent(out)  :: err
   integer                        :: ier
!
   read(unitbin,iostat=ier)space
   if (ier /= 0) then
       call err%set('Error on reading space group')
   endif
!
   end subroutine read_space_bin

!--------------------------------------------------------------------------------------

   subroutine spg_frequency(spg)
!
!  Fill info about frequency from CSD (6 January 2016). Call subroutine make_frequency to create array freq_perc and freq_rank
!
   class(spaceg_type), intent(inout) :: spg
   integer, dimension(230), parameter :: freq_no = [7662, 198014,    142,  41791,   6826,     21,   3447,    293,   8450,    110,  &
                                                    4023,   4094,   5232, 279041,  67434,     35,     79,   3293,  58438,   1412,  &
                                                      58,     27,    190,     60,     11,    131,     19,     13,   5968,    106,  &
                                                     499,    151,  11145,    251,      6,   1142,    105,     22,     51,    145,  &
                                                     869,     69,   2751,     70,    468,    114,     30,     58,     18,     79,  &
                                                      52,    852,    115,    382,    222,   2848,    800,    568,    247,   6867,  &
                                                   26951,   8734,    786,   1009,    111,     89,     58,    383,     60,    874,  &
                                                      87,    336,    230,    171,     45,    742,     84,    619,    226,    207,  &
                                                     185,   1104,     38,     99,    710,   1060,    532,   2927,      8,     58,  &
                                                      68,   1574,      9,    140,     59,   1364,     48,     83,      3,      4,  &
                                                       6,     24,     23,     95,      2,     82,     13,     29,     37,    307,  &
                                                       6,     28,    211,   1003,      4,     27,     56,    153,     28,     84,  &
                                                     148,    520,    134,     74,     21,    175,     64,     90,    175,    383,  &
                                                      40,     16,     34,     48,     80,    139,     96,     97,    158,     72,  &
                                                     151,    412,    187,    586,    575,   1020,    930,   5217,     15,     79,  &
                                                      25,    725,     17,    542,    367,      8,     15,     74,    276,    239,  &
                                                     790,     25,    341,     86,    537,    360,   1294,     24,    494,    466,  &
                                                      59,     45,    533,     22,     36,    946,      8,    223,    170,     54,  &
                                                      35,    103,      4,      9,     27,    107,     31,     15,     27,    126,  &
                                                      48,     94,     51,    186,     15,     77,    142,    499,     58,     19,  &
                                                      31,     39,     88,    102,    732,     99,      9,      5,     31,     37,  &
                                                      24,     34,     46,     24,     51,     50,    270,    118,     87,    242,  &
                                                     128,    103,     61,     25,    532,     48,    127,    116,    130,     82]

  real, parameter :: sum_freq = 100.0/sum(freq_no)
!corr  real, dimension(230), parameter :: freq_perc = [0.95, 24.53,  0.02,  5.18,  0.85,  0.00,  0.43,  0.04,  1.05,  0.01,  &
!corr                                                  0.50,  0.51,  0.65, 34.57,  8.35,  0.00,  0.01,  0.41,  7.24,  0.17,  &
!corr                                                  0.01,  0.00,  0.02,  0.01,  0.00,  0.02,  0.00,  0.00,  0.74,  0.01,  &
!corr                                                  0.06,  0.02,  1.38,  0.03,  0.00,  0.14,  0.01,  0.00,  0.01,  0.02,  &
!corr                                                  0.11,  0.01,  0.34,  0.01,  0.06,  0.01,  0.00,  0.01,  0.00,  0.01,  &
!corr                                                  0.01,  0.11,  0.01,  0.05,  0.03,  0.35,  0.10,  0.07,  0.03,  0.85,  &
!corr                                                  3.34,  1.08,  0.10,  0.13,  0.01,  0.01,  0.01,  0.05,  0.01,  0.11,  &
!corr                                                  0.01,  0.04,  0.03,  0.02,  0.01,  0.09,  0.01,  0.08,  0.03,  0.03,  &
!corr                                                  0.02,  0.14,  0.00,  0.01,  0.09,  0.13,  0.07,  0.36,  0.00,  0.01,  &
!corr                                                  0.01,  0.19,  0.00,  0.02,  0.01,  0.17,  0.01,  0.01,  0.00,  0.00,  &
!corr                                                  0.00,  0.00,  0.00,  0.01,  0.00,  0.01,  0.00,  0.00,  0.00,  0.04,  &
!corr                                                  0.00,  0.00,  0.03,  0.12,  0.00,  0.00,  0.01,  0.02,  0.00,  0.01,  &
!corr                                                  0.02,  0.06,  0.02,  0.01,  0.00,  0.02,  0.01,  0.01,  0.02,  0.05,  &
!corr                                                  0.00,  0.00,  0.00,  0.01,  0.01,  0.02,  0.01,  0.01,  0.02,  0.01,  &
!corr                                                  0.02,  0.05,  0.02,  0.07,  0.07,  0.13,  0.12,  0.65,  0.00,  0.01,  &
!corr                                                  0.00,  0.09,  0.00,  0.07,  0.05,  0.00,  0.00,  0.01,  0.03,  0.03,  &
!corr                                                  0.10,  0.00,  0.04,  0.01,  0.07,  0.04,  0.16,  0.00,  0.06,  0.06,  &
!corr                                                  0.01,  0.01,  0.07,  0.00,  0.00,  0.12,  0.00,  0.03,  0.02,  0.01,  &
!corr                                                  0.00,  0.01,  0.00,  0.00,  0.00,  0.01,  0.00,  0.00,  0.00,  0.02,  &
!corr                                                  0.01,  0.01,  0.01,  0.02,  0.00,  0.01,  0.02,  0.06,  0.01,  0.00,  &
!corr                                                  0.00,  0.00,  0.01,  0.01,  0.09,  0.01,  0.00,  0.00,  0.00,  0.00,  &
!corr                                                  0.00,  0.00,  0.01,  0.00,  0.01,  0.01,  0.03,  0.01,  0.01,  0.03,  &
!corr                                                  0.02,  0.01,  0.01,  0.00,  0.07,  0.01,  0.02,  0.01,  0.02,  0.01]

  integer, dimension(230), parameter :: freq_rank = [ 10,   2,  96,   5,  12, 202,  18,  69,   9, 111,  &
                                                      17,  16,  14,   1,   3, 177, 136,  19,   4,  24,  &
                                                     154, 190,  82, 149, 215, 101, 205, 214,  13, 113,  &
                                                      56,  93,   7,  72, 222,  27, 114, 200, 160,  95,  &
                                                      36, 144,  22, 143,  58, 109, 184, 152, 206, 137,  &
                                                     159,  37, 108,  63,  79,  21,  38,  48,  73,  11,  &
                                                       6,   8,  40,  31, 110, 125, 156,  61, 148,  35,  &
                                                     127,  67,  76,  88, 169,  41, 131,  45,  77,  81,  &
                                                      85,  28, 173, 119,  44,  29,  52,  20, 221, 155,  &
                                                     145,  23, 218,  98, 151,  25, 165, 132, 229, 226,  &
                                                     224, 195, 199, 122, 230, 134, 213, 185, 175,  68,  &
                                                     223, 186,  80,  32, 227, 188, 157,  91, 187, 130,  &
                                                      94,  54, 100, 140, 203,  87, 146, 124,  86,  62,  &
                                                     171, 208, 180, 166, 135,  99, 121, 120,  90, 142,  &
                                                      92,  60,  83,  46,  47,  30,  34,  15, 211, 138,  &
                                                     194,  43, 207,  49,  64, 219, 212, 141,  70,  75,  &
                                                      39, 193,  66, 129,  50,  65,  26, 197,  57,  59,  &
                                                     150, 170,  51, 201, 176,  33, 220,  78,  89, 158,  &
                                                     178, 116, 228, 217, 191, 112, 182, 210, 189, 105,  &
                                                     164, 123, 162,  84, 209, 139,  97,  55, 153, 204,  &
                                                     181, 172, 126, 117,  42, 118, 216, 225, 183, 174,  &
                                                     198, 179, 168, 196, 161, 163,  71, 106, 128,  74,  &
                                                     103, 115, 147, 192,  53, 167, 104, 107, 102, 133]
!
   if (spg%num == 0) return
   spg%freq_no = freq_no(spg%num)
!corr   spg%freq_perc = freq_perc(spg%num)
   spg%freq_perc = sum_freq*freq_no(spg%num)
   spg%freq_rank = freq_rank(spg%num)
!
   end subroutine spg_frequency

!--------------------------------------------------------------------------------------

   subroutine make_frequency(freq,kpr)
!
!  Call this subroutine to create info about frequency of spg
!
   USE nr
   integer, dimension(:), intent(in) :: freq
   integer, intent(in)               :: kpr
   integer, dimension(230)           :: iord
   integer                           :: i
!corr   real                              :: costp
!
!  write No. of occurencies
   write(kpr,'(10(i7,","),"  &")')(freq(i),i=1,230)
!
!  write %
!corr   costp = 100.0/sum(freq)  
!corr   write(kpr,'(10(f6.2,","),"  &")')(freq(i)*costp,i=1,230)
!
!  write rank
   call indexx(freq,iord)
   iord(iord) = [(i,i=230,1,-1)]
   write(kpr,'(10(i4,","),"  &")')(iord(i),i=1,230)
   end subroutine make_frequency

!--------------------------------------------------------------------------------------

   logical function is_chiral(spg)
!
!  Is spg chiral?
!
   class(spaceg_type), intent(in) :: spg
   select case (spg%num)
     case(1, 3:5, 16:24, 75:80, 89:98, 143:146, 149:155, 168:173, 177:182, 195:199,  207:214)
       is_chiral = .true.
     case default
       is_chiral = .false.
   end select
   end function is_chiral

!--------------------------------------------------------------------------------------

   function get_extinction_symbol(spg)  result(ext)
!
!  Get extinction symbol
!
   class(spaceg_type), intent(in) :: spg
   character(len=:), allocatable  :: ext
   type(sg_info_type)             :: infos
   integer                        :: spos,iext
!
   ext = ' '
   if (spg%num == 0) return
!
   infos = get_spg_info(spg%symbol_xhm,alls=.true.,spos=spos)
   if (spos == 0) return
   iext = get_index_exts(spos)
   if (iext == 0) return
   ext = trim(extsy(iext)%symb)
!
   end function get_extinction_symbol

!--------------------------------------------------------------------------------------

   integer function csys_from_exts(ext_symbol)
!
!  Get crystal system from extinction symbol
!
   type(extsymb_t), intent(in) :: ext_symbol
!
   csys_from_exts = csys_code_from_num(sg_info(ext_symbol%spgn(1))%num)
!
   end function csys_from_exts

!--------------------------------------------------------------------------------------

   subroutine print_extsymb(kpr)
!
!  Used to generate csv file with extinction symbols and corresponding space groups.
!  Convert in html: Open csv file in Libreoffice > Table > Convert table to text > Export > html
!
   USE symm_table
   USE strutil
   integer, intent(in) :: kpr
   integer             :: i,j,ns
   integer             :: last_csys, new_csys
!
   last_csys = -1
   write(kpr,'(a)')"#;Extinction Symbol;Space Groups"
   do i=1,size(extsy)
!
!     Print crystal system combining trigonal and hexagonal
      new_csys = csys_from_exts(extsy(i))
      if (new_csys == CS_Trigonal) new_csys = CS_Hexagonal
      if (new_csys /= last_csys) then
          if (new_csys == CS_Hexagonal) then
              write(kpr,'(a)')trim(cry_sys(CS_Trigonal))//"/"//trim(cry_sys(CS_Hexagonal))
          else
              write(kpr,'(a)')trim(cry_sys(new_csys))
          endif
          last_csys = new_csys
      endif
!
!     Print space group list
      !write(kpr,'(a)',advance='no')trim(extsy(i)%symb)//';'
      write(kpr,'(i0,a)',advance='no')i,';'//trim(extsy(i)%symb)//';'
      do j=1,extsy(i)%nspg-1
         ns = extsy(i)%spgn(j)
         write(kpr,'(a,1x)',advance='no')trim(s_blank_delete(sg_info(ns)%hm2))//'('//i_to_s(sg_info(ns)%num)//')'//','
      enddo
      ns = extsy(i)%spgn(extsy(i)%nspg)
      write(kpr,'(a)',advance='yes')trim(s_blank_delete(sg_info(ns)%hm2))//'('//i_to_s(sg_info(ns)%num)//')'
   enddo
!
   end subroutine print_extsymb

!--------------------------------------------------------------------------------------

   integer function z_from_spg(spgnum)
!
!  Z from spg number
!
   integer, intent(in)                :: spgnum
   integer, dimension(230), parameter :: vetz = [ 1,   2,   2,   2,   4,   2,   2,   4,   4,   4,  &  
                                                  4,   8,   4,   4,   8,   4,   4,   4,   4,   8,  &
                                                  8,  16,   8,   8,   4,   4,   4,   4,   4,   4,  &
                                                  4,   4,   4,   4,   8,   8,   8,   8,   8,   8,  &
                                                  8,  16,  16,   8,   8,   8,   8,   8,   8,   8,  &
                                                  8,   8,   8,   8,   8,   8,   8,   8,   8,   8,  &
                                                  8,   8,  16,  16,  16,  16,  16,  16,  32,  32,  &
                                                 16,  16,  16,  16,   4,   4,   4,   4,   8,   8,  &
                                                  4,   8,   8,   8,   8,   8,  16,  16,   8,   8,  &
                                                  8,   8,   8,   8,   8,   8,  16,  16,   8,   8,  &
                                                  8,   8,   8,   8,   8,   8,  16,  16,  16,  16,  &
                                                  8,   8,   8,   8,   8,   8,   8,   8,  16,  16,  &
                                                 16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  &
                                                 16,  16,  16,  16,  16,  16,  16,  16,  32,  32,  &
                                                 32,  32,   3,   3,   3,   9,   6,  18,   6,   6,  &
                                                  6,   6,   6,   6,  18,   6,   6,   6,   6,  18,  &
                                                 18,  12,  12,  12,  12,  36,  36,   6,   6,   6,  &
                                                  6,   6,   6,   6,  12,  12,  12,  12,  12,  12,  &
                                                 12,  12,  12,  12,  12,  12,  12,  12,  12,  12,  &
                                                 24,  24,  24,  24,  12,  48,  24,  12,  24,  24,  &
                                                 24,  96,  96,  48,  24,  48,  24,  24,  96,  96,  &
                                                 48,  24,  24,  48,  24,  96,  48,  24,  96,  48,  &
                                                 48,  48,  48,  48, 192, 192, 192, 192,  96,  96]

   z_from_spg = 0
   if (spgnum < 0 .and. spgnum > 230) return
   z_from_spg = vetz(spgnum)
   end function z_from_spg

!--------------------------------------------------------------------------------------
!
!   subroutine read_egrfile()
!   USE fileutil
!   USE symm_table
!   USE strutil
!   type(file_handle)             :: feg,fsym
!   character(len=:), allocatable :: line,line1
!   integer                       :: iext,ispg,i,spos
!   logical                       :: lvar
!   integer, parameter            :: MAXSPG=16,MAXEXT=216
!   type(sg_info_type)            :: infos
!!
!   call feg%fopen('expo.egr')
!   call fsym%fopen('extg.txt')
!   write(70,'(a,i0,a)') 'type(extsymb_t), dimension(',MAXEXT,') :: extsy = [  &'
!   do while(get_line(feg%handle(),line,trimmed=.true.))
!      read(line,*)iext,ispg
!      lvar = get_line(fsym%handle(),line1,trimmed=.true.)
!      write(70,'(3x,a,i0,a)',advance='no'),"extsymb_t('"//trim(line1)//"',",ispg,", ["
!      !if (maxleg < len_trim(line1)) maxleg = len_trim(line1)
!      do i=1,ispg
!         lvar = get_line(feg%handle(),line,trimmed=.true.)
!             infos = get_spg_info(line(1:index(line,';')-1),alls=.true.,spos=spos)
!             write(71,*)'SPGN=',infos%num,trim(line(1:index(line,';')-1))
!             write(72,*)'SPGN=',infos%num,lower(trim(sg_info(spos)%hm2))
!         if (i==ispg) then
!             if (i == MAXSPG) then
!                 !write(70,'(a)',advance='yes')"'"//trim(line(1:index(line,';')-1))//"']),   &"
!                 write(70,'(i0,a)',advance='yes')spos,"]),   &"
!             else
!                 !write(70,'(a,i0,a,i0,a)',advance='yes')"'"//trim(line(1:index(line,';')-1))//"',(' ',i=",ispg+1,",",MAXSPG,")]),  &"
!                 write(70,'(i0,a,i0,a,i0,a)',advance='yes')spos,",(0,i=",ispg+1,",",MAXSPG,")]),  &"
!             endif
!         else
!             !write(70,'(a)',advance='no')"'"//trim(line(1:index(line,';')-1))//"',"
!             write(70,'(i0,a)',advance='no')spos,","
!         endif
!      enddo
!   enddo
!!
!   end subroutine read_egrfile
!--------------------------------------------------------------------------------------

   subroutine print_spg_freq(cs,kpr)
!
!  Print for crystal system cs list of space groups sorted for frequency
!
   USE nr
   USE strutil
   integer, intent(in)                :: cs,kpr
   character(len=11), dimension(530)  :: spg_string
   real, dimension(530)               :: spg_freq
   type(spaceg_type)                  :: spg
   integer, dimension(:), allocatable :: iord
   integer                            :: i,nspg
!
   nspg = 0
   do i=1,530
      if (csys_code_from_num(sg_info(i)%num) == cs) then
          if (nspg > 0) then
              if (s_eqi(sg_info(i)%hm2,spg_string(nspg))) cycle
          endif
          nspg = nspg + 1
          spg_string(nspg) = sg_info(i)%hm2
          spg%num = sg_info(i)%num
          call spg_frequency(spg)
          spg_freq(nspg) = spg%freq_rank
      endif
   enddo
!
   allocate(iord(nspg))
   call indexx(spg_freq(:nspg),iord)
   spg_freq(:nspg) = spg_freq(iord)
   spg_string(:nspg) = spg_string(iord)
   do i=1,nspg
      write(kpr,'(a)')spg_string(i)
   enddo
!
   end subroutine print_spg_freq

!--------------------------------------------------------------------------------------

   subroutine string_to_pmatrix(str,pmat,pvet)
   use strutil
   character(len=*), intent(in)                :: str
   character(len=len(str))                     :: smat
   real, dimension(3,3), intent(out)           :: pmat
   real, dimension(3), intent(out)             :: pvet
   character(len=:), allocatable, dimension(:) :: wordv
   integer                                     :: nword
   integer                                     :: i
!
   smat = str
   call s_rep_ch(smat,',',' ')
!
   call get_words1(smat,wordv,nword)
   do i=1,nword
      call string_to_pmatrix_col(wordv(i),pmat(:,i),pvet(i))
      !write(0,'(a,a,3i5,f10.2)')'word=',wordv(i),pmat(i,:),pvet(i)
   enddo
!
   end subroutine string_to_pmatrix

!--------------------------------------------------------------------------------------
   
   subroutine string_to_pmatrix_col(str,vet,tr)
   use strutil
   character(len=*), intent(in)    :: str
   real, dimension(3), intent(out) :: vet
   real, intent(out)               :: tr
   integer                         :: i,ich,lens
   integer                         :: isgn, num, den
   integer                         :: ier
   real                            :: frac
   logical                         :: is_frac
!
   vet(:) = 0
   tr = 0 
   isgn = 1
   i = 0
   lens = len_trim(str)
   is_frac = .false.
   do
     i = i + 1
     if (i > lens) exit
     if (ch_is_alpha(str(i:i))) then
         ich = ichar(str(i:i)) - ichar('a') + 1
         vet(ich) = isgn
         isgn = 1
         if (is_frac) then
             vet(ich) = vet(ich)*frac
             is_frac = .false.
         endif
     elseif (ch_is_digit(str(i:))) then
         call s_to_i(str(i:i),num,ier)
         i = i + 1
         if (str(i:i) == '/') then
             call get_next_number(str,i,inum=den,ier=ier)
             frac = isgn * real(num) / den
             if (i == lens) then
                 tr = frac
             else
                 if (ch_is_alpha(str(i+1:i+1))) then
                     is_frac = .true.
                 endif
             endif
         endif
     elseif (str(i:i) == '-') then
         isgn = -1
     elseif (str(i:i) == '+') then
         isgn = 1
     endif
   enddo
!
   end subroutine string_to_pmatrix_col
 
!--------------------------------------------------------------------------------------

   subroutine get_pmat(spg,mat,vet)
   class(spaceg_type), intent(in)    :: spg
   real, dimension(3,3), intent(out) :: mat
   real, dimension(3), intent(out)   :: vet
!
   call string_to_pmatrix(spg%pmat,mat,vet)
!
   end subroutine get_pmat

!--------------------------------------------------------------------------------------

   subroutine get_pmat1(spg,mat,vet)
   class(spaceg_type), intent(in)    :: spg
   real, dimension(3,3), intent(out) :: mat
   real, dimension(3), intent(out)   :: vet
!
   call string_to_pmatrix(spg%pmat1,mat,vet)
!
   end subroutine get_pmat1

!--------------------------------------------------------------------------------------

   function standard_spg(spg) result(std_spg)
   type(spaceg_type), intent(in) :: spg
   type(spaceg_type)             :: std_spg
   !integer                       :: num
!
   if (spg%num == 0 .or. spg%standard_id <= 0) then
       call std_spg%init()
       return
   endif

   !num = spg_index(spg%num)%pos(1)
   !std_spg = spg_data(num)
   std_spg = spg_data(spg%standard_id)
!
   end function standard_spg

!--------------------------------------------------------------------------------------

   logical function is_standardize(spg)
!
!  Check if space group can be standardized
!
   type(spaceg_type), intent(in) :: spg
!
   is_standardize = .false.
   if (.not.spg%defined .or. spg%num == 0 .or. spg%standard) return
   if(len_trim(spg%pmat1) == 0) return
   is_standardize = .true.
!
   end function is_standardize

!--------------------------------------------------------------------------------------

   integer function get_id(spg)
!
!  Get the index of spacegroup in the database
!
   class(spaceg_type), intent(in) :: spg
!
   get_id = spg%id
!
   end function get_id

!--------------------------------------------------------------------------------------

   integer function id_setting(spg)
!
!  Get setting id of spacegroup
!
   class(spaceg_type), intent(in) :: spg
   integer                        :: i
!
   id_setting = 0
   if (spg%id >= 0) then
       do i=1,spg_index(spg%num)%nat
          if (spg_index(spg%num)%pos(i) == spg%id+1) then
              id_setting = i
              exit
          endif
       enddo
   endif
!
   end function id_setting

!--------------------------------------------------------------------------------------

   subroutine spg_database_print()
   use strutil
   integer :: i
   !integer :: irep
   !character(len=40) :: spg_alternative
   !type(spaceg_type) :: std_spg

   do i=1,NSPGTOT
      call spg_data(i)%prn()
      !std_spg = standard_spg(spg_data(i))
      !write(6,'(i4,") ",a,l5,l5,2x,i5,i5,3x,a)')spg_data(i)%num, spg_data(i)%symbol_xhm, spg_data(i)%standard,   &
      !                                  spg_data(i)%id + 1 == spg_data(i)%standard_id,                           &
      !                                  spg_data(i)%id_setting(), spg_data(i)%standard_id,std_spg%symbol_xhm
      write(6,'(1x,100("="))')
   enddo

   !do i=1,NUMSPGMAX
   !   num = spg_index(i)%pos(1)
   !   write(70,*)'NUM=',num,spg_data(num)%standard
   !enddo

   !do i=1,NSPGTOT
   !   write(70,'(a)')"========================================================"
   !   write(70,'(i5,a10,5x,a)')spg_data(i)%num,trim(spg_data(i)%symbol_xhm),trim(spg_data(i)%pmat)
   !   call string_to_pmatrix(spg_data(i)%pmat,pmat,pvet)
   !   do j=1,3
   !      write(70,'(3f10.3,f10.2)')pmat(j,:),pvet(j)
   !   enddo
   !enddo

   !do i=1,NSPGTOT
   !   if (spg_data(i)%symbol_xhm /= 'Unknown') then
   !       if (spg_data(i)%csys_code == CS_Monoclinic) then
!
!  !           Strip ' 1' for space groups with 2 along b
   !           if (spg_data(i)%axis_direction() == 'b') then
   !               spg_alternative = spg_data(i)%symbol_xhm 
   !               call s_s_delete(spg_alternative,' 1',irep)
   !               write(70,'(a,i4,a)')'{',spg_data(i)%num,', "'//trim(adjustl(spg_alternative))//'" },'
   !               cycle
   !           endif
   !       endif
   !       write(70,'(a,i4,a)')'{',spg_data(i)%num,', "'//trim(adjustl(spg_data(i)%symbol_xhm))//'" },'
   !   endif
   !   if (spg_data(i)%num == 230) exit
   !enddo

   end subroutine spg_database_print

END MODULE spginfom
