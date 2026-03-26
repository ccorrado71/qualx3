MODULE cif_frm

   implicit none

   type cifword_type
     character(len=50)               :: str     ! stringa da cercare nel file cif
     character(len=50)               :: alt_str ! alternative string
     integer                         :: col     ! ... in quale colonna
     logical                         :: wok     ! la stringa è stata trovata
     integer                         :: ktype   ! che tipo di variabile
     real, dimension(:), allocatable :: vet     ! vettore reale associato alla stringa
     character(len=100), dimension(:), allocatable :: svet   ! vettore carattere associato alla stringa
     integer                         :: nv      ! numero di elementi nel vettore
   end type cifword_type

   integer, parameter :: UNKNOWN_VALUE = -999999

   type additional_info_type
!
!       Experimental details: key _exptl_
        character(len=100) :: crystal_description = ' '
        character(len=100) :: crystal_colour = ' '   
        real               :: crystal_size_max = UNKNOWN_VALUE    
        real               :: crystal_size_mid = UNKNOWN_VALUE  
        real               :: crystal_size_min = UNKNOWN_VALUE   
        real               :: crystal_density_meas = UNKNOWN_VALUE
        real               :: crystal_density_diffrn = UNKNOWN_VALUE 
        character(len=100) :: crystal_density_method = ' '
        real               :: crystal_F_000  = UNKNOWN_VALUE        
        real               :: absorpt_coefficient_mu = UNKNOWN_VALUE   
        character(len=100) :: absorpt_correction_type = ' '
        real               :: absorpt_correction_T_min = UNKNOWN_VALUE
        real               :: absorpt_correction_T_max = UNKNOWN_VALUE
        character(len=100) :: absorpt_process_details = ' '
!
!       key _diffrn_
        character(len=100) :: radiation_source = ' '
        character(len=100) :: radiation_monochromator = ' '
        character(len=100) :: measurement_device_type = ' '
        character(len=100) :: measurement_method = ' '
        real               :: detector_area_resol_mean = UNKNOWN_VALUE
        real               :: standards_number = UNKNOWN_VALUE       
        real               :: standards_interval_count = UNKNOWN_VALUE
        real               :: standards_interval_time = UNKNOWN_VALUE
        real               :: standards_decay_perc = UNKNOWN_VALUE  ! standards_decay_%  
        real               :: reflns_number = UNKNOWN_VALUE  
        real               :: reflns_av_R_equivalents = UNKNOWN_VALUE  
        real               :: reflns_av_sigmaI_over_netI = UNKNOWN_VALUE ! reflns_av_sigmaI/netI
        real               :: reflns_limit_h_min = UNKNOWN_VALUE     
        real               :: reflns_limit_h_max = UNKNOWN_VALUE    
        real               :: reflns_limit_k_min = UNKNOWN_VALUE   
        real               :: reflns_limit_k_max = UNKNOWN_VALUE  
        real               :: reflns_limit_l_min = UNKNOWN_VALUE 
        real               :: reflns_limit_l_max = UNKNOWN_VALUE
        real               :: reflns_theta_min = UNKNOWN_VALUE 
        real               :: reflns_theta_max = UNKNOWN_VALUE
!
!       key _reflns_
        real               :: number_total = UNKNOWN_VALUE
        real               :: number_gt = UNKNOWN_VALUE
        character(len=100) :: threshold_expression = '>2sigma(I)'
!
!       key _computing
        character(len=100) :: data_collection = ' '
        character(len=100) :: cell_refinement = ' ' 
        character(len=100) :: data_reduction = ' '   
        character(len=100) :: structure_solution = ' '
        character(len=100) :: structure_refinement = 'SIR2014 (Burla, 2015)'
        character(len=100) :: molecular_graphics = ' '
        character(len=100) :: publication_material = ' '
!
!       Refinement details: key _refine_ls_
        character(len=10)  :: factor_coef = 'Fsqd'
        character(len=10)  :: matrix_type = 'Full'
        character(len=10)  :: weighting_scheme = 'calc'
        character(len=200) :: weighting_details = '?'
        character(len=10)  :: hydrogen_treatment = 'mixed'
        character(len=10)  :: extinction_method = '?'
        real               :: extinction_coef = UNKNOWN_VALUE
        character(len=100) :: extinction_expression = ' '
        character(len=100) :: abs_structure_details = ' '
        real               :: abs_structure_Flack = UNKNOWN_VALUE
        real               :: number_reflns = UNKNOWN_VALUE     
        real               :: number_parameters = UNKNOWN_VALUE   
        real               :: number_restraints = UNKNOWN_VALUE 
        real               :: R_factor_all = UNKNOWN_VALUE     
        real               :: R_factor_gt = UNKNOWN_VALUE      
        real               :: wR_factor_ref = UNKNOWN_VALUE   
        real               :: wR_factor_gt = UNKNOWN_VALUE   
        real               :: goodness_of_fit_ref = UNKNOWN_VALUE  
        real               :: restrained_S_all = UNKNOWN_VALUE    
        real               :: shift_over_su_max = UNKNOWN_VALUE  ! shift/su_max
        real               :: shift_over_su_mean = UNKNOWN_VALUE ! shift/su_mean
   end type additional_info_type

   integer, parameter, private    :: ERRCIF_VALUE = -9999
   integer, dimension(7), private :: pos_anisU, pos_anisB
   integer, parameter             :: LENLINE_CIF = 200
   integer, parameter             :: EXPORT_POWCIF_RIET=1, EXPORT_POWCIF_OBS=2

   private :: write_angles

CONTAINS

   subroutine cifword_fill(cifword,unit,newdata,nline,dataname,error)
   USE strutil
   USE arrayutil
   USE errormod
   type(cifword_type), dimension(:), allocatable, intent(inout) :: cifword
   integer, intent(in)                           :: unit
   logical, intent(out)                          :: newdata
   integer, intent(inout)                        :: nline
   character(len=*), intent(out)                 :: dataname
   type(error_type), intent(inout)               :: error
   integer                                       :: i
   integer                                       :: ierror
   integer                                       :: vdim
   integer, dimension(200)                       :: loopvet
   integer                                       :: nloop
   integer                                       :: pos
   character(len=LENLINE_CIF)                    :: line,line1
   integer, parameter                            :: NAVET = 1000
   logical                                       :: ok_loop
   integer                                       :: nlong1,nlong2
   integer                                       :: ier
   integer                                       :: nrw
   logical                                       :: jumpl
   character(len=80), dimension(100)             :: word
   integer                                       :: nword
   integer                                       :: nloop_read
   integer                                       :: nline_loop
   integer                                       :: pos_loop
   integer                                       :: ndatab
   integer                                       :: posl
   real                                          :: rnum
   character(len=:), allocatable                 :: strc
!
   call error%reset()
!
!  Inizializza cifword
   do i=1,size(cifword)
      cifword(i)%wok = .false.
      cifword(i)%nv = 0
   enddo
!
   ndatab = 0
   newdata = .false.
   dataname = ' '
   do
      read(unit,'(a)',end=30) line
      call set_linecif(line,jumpl,nline,ndatab,newdata,dataname)
      if (newdata) go to 20
      if (jumpl) cycle
      ok_loop = index(lower(line),'loop_') > 0
      if (ok_loop) then
          pos_loop = nline
          nloop = 0
          loopvet(:) = 0
          call cutst(line,nlong1)
!
!         Leggo quali campi sono nel loop e la loro posizione
          do
              if (nlong1 == 0) then   ! leggi solo quando sei a fine record
                  read(unit,'(a)',iostat=ier) line
                  if (ier < 0) go to 30 !exit  ! uscita per end-file  
              endif
              call set_linecif(line,jumpl,nline,ndatab,newdata)
              if (jumpl) then
                  nlong1 = 0 ! force reading new line. es. _atom_type_symbol #hello world!
                  cycle
              endif
              if (newdata) go to 20
              if (line(1:1) == '_') then
                  call cutst(line,nlong1,line1,nlong2)
                  nloop = nloop + 1
                  !corr pos = string_locate(line1(:nlong2),cifword%str)
                  pos = cif_string_locate(line1(:nlong2),cifword)
                  if (pos > 0) then
                      cifword(pos)%wok = .true.
                      cifword(pos)%nv = 0
                      if (cifword(pos)%ktype == 1) then
                          call new_array(cifword(pos)%vet,NAVET)
                      else
                          call new_array(cifword(pos)%svet,NAVET,nlen=100)
                      endif
                  endif
                  loopvet(nloop) = pos
              else
                  exit
              endif
          enddo
!
!         check for data items in loop
          !if (nloop == 0) then
          !    call set_error(error,'Line '//trim(i_to_s(pos_loop))//'. No data items in loop_')
          !endif
          !if (error%signal) go to 30
          if (nloop == 0) cycle
!
!         Leggo le colonne
          nloop_read = count(loopvet(:nloop) > 0)  ! numero effettivo di colonne da leggere
          nline_loop = 0          ! contatore del numero di linee lette del loop
          posl = 0
          do 
!
!           se la linea contiene '_' allora il blocco e' finito
            if (line(1:1) == '_' .or. index(line,'loop_') > 0) then 
                backspace(unit)
                nline = nline - 1
                go to 100
            endif
!
            nline_loop = nline_loop + 1
!
            if (nloop_read > 0) then
!
!               estrai stringhe dalla linea
                call get_words_quotes1(line,word,nword)  ! spezza tutta la linea in parole
!                 
                do i=1,nword
!
!                  check for comment at the end of loop line
                   if (posl == nloop .and. word(i)(1:1) == '#') exit
!
!                  get posl (the position in a loop line)
                   posl = posl + 1
                   posl = mod(posl,nloop)
                   if (posl == 0) posl = nloop
                   pos = loopvet(posl)
!
                   if (pos == 0) cycle
                   cifword(pos)%nv = cifword(pos)%nv + 1
                   nrw = cifword(pos)%nv
                   if (cifword(pos)%ktype == 2) then             ! campo stringa
                       vdim = size_array(cifword(pos)%svet)
                       !if (nrw > vdim) call reallocate(cifword(pos)%svet,vdim+NAVET,savevet=.true.,nlen=100)
                       if (nrw > vdim) call resize_array(cifword(pos)%svet,vdim+NAVET,nlen=100)
                       word(i) = rem_quotes(word(i))
                       cifword(pos)%svet(nrw) = trim(adjustl(word(i)))
                   else                               ! campo numerico
                       vdim = size_array(cifword(pos)%vet)
                       if (nrw > vdim) call resize_array(cifword(pos)%vet,vdim+NAVET)
                       call s_detag(word(i)) ! elimina caratteri tra parentesi
                       if (word(i)(1:1) == '?' .or.     &
                           (word(i)(1:1) == '.' .and. len_trim(word(i)) == 1)) then    ! attention to number e.g. .01234
                           cifword(pos)%vet(nrw) = ERRCIF_VALUE
                       else
                           ier = s_to_r(word(i),cifword(pos)%vet(nrw),nlong2)
                       endif
                   endif
                enddo
            endif

            do 
                read(unit,'(a)',end=30) line      
                call set_linecif(line,jumpl,nline,ndatab,newdata)
                if (jumpl) cycle
                if (newdata) go to 20
!
!               manage semicolon in a loop
                call manage_semicolon_block(unit,line,ier,strc)
                if (ier /= 0) return
                if (len_trim(strc) > 0) line = strc
                exit
            enddo

          enddo
100       continue
      else
!
!         Gestione campi non in loop
          call cutst(line,nlong1,line1,nlong2)
          !pos = string_locate(line1(:nlong2),cifword%str)
          pos = cif_string_locate(line1(:nlong2),cifword)
          if (pos > 0) then
              if (.not.cifword(pos)%wok) then    ! subsiquent assignment will be not cosidered
                  line = adjustl(rem_quotes(line))   ! rimuovi eventuali apici
                  if (len_trim(line) > 0 .and. line(1:1) /= '?') then
                      if (cifword(pos)%ktype == 1) then  ! campo numerico
                          ierror = s_to_r(line,rnum,nlong1)
                          if (ierror == 0) then
                              !call reallocate(cifword(pos)%vet,1)
                              call new_array(cifword(pos)%vet,1)
                              cifword(pos)%vet(1) = rnum
                              cifword(pos)%wok = .true.
                          endif
                      else                               ! campo stringa
                          !call reallocate(cifword(pos)%svet,1,nlen=100)
                          call new_array(cifword(pos)%svet,1,nlen=100)
                          cifword(pos)%svet(1) = trim(adjustl(line))
                          cifword(pos)%wok = .true.
                      endif
                  endif
              endif
          else
!
!             for unmanaged keys check blocks with semicolon
!corr              if (manage_semicolon_block(unit,line1) /= 0) return 
              call manage_semicolon_block(unit,line1,ier,strc); if (ier /= 0) return
          endif
      endif
   enddo

20 if (newdata) backspace(unit)
30 continue
 
   end subroutine cifword_fill

  !---------------------------------------------------------------------------------------------

   logical function cifkey_is_ok(cifw)
   type(cifword_type), intent(in) :: cifw
   cifkey_is_ok = .false.
   if (cifw%nv == 0) return
   cifkey_is_ok = all(cifw%vet(:cifw%nv) /= ERRCIF_VALUE)
   end function cifkey_is_ok

  !---------------------------------------------------------------------------------------------

   integer function cif_string_locate(str,cifword) result(pos)
   USE strutil
   character(len=*), intent(in)                 :: str
   type(cifword_type), dimension(:), intent(in) :: cifword
   pos = string_locate(str,cifword%str)
   if (pos == 0) pos = string_locate(str,cifword%alt_str)
   end function cif_string_locate

  !---------------------------------------------------------------------------------------------

   subroutine load_cif_dictionary(cifword)
   type(cifword_type), dimension(:), allocatable, intent(inout) :: cifword
   integer, parameter                                           :: NW = 53
   integer :: i
   type cifkey_type
     character(len=40) :: str =      ' ' !main name
     character(len=40) :: alt_str = ' '  !alternative name
     integer           :: typ = 1        !Tipo: 1 per variabile numerica, 2 per variabile stringa
   end type
   type(cifkey_type), dimension(NW) :: cifkey = (/ &
   cifkey_type('_pd_meas_2theta_scan                ','                                ',1),  &
   cifkey_type('_pd_meas_intensity_total            ','                                ',1),  &
   cifkey_type('_pd_meas_counts_total               ','                                ',1),  &
   cifkey_type('_pd_meas_2theta_fixed               ','                                ',1),  &
   cifkey_type('_pd_proc_intensity_total            ','                                ',1),  &
   cifkey_type('_pd_meas_2theta_range_min           ','                                ',1),  &
   cifkey_type('_pd_meas_2theta_range_max           ','                                ',1),  &
   cifkey_type('_pd_meas_2theta_range_inc           ','                                ',1),  &
   cifkey_type('_pd_meas_2theta_corrected           ','                                ',1),  &
   cifkey_type('_pd_proc_2theta_corrected           ','                                ',1),  &
   cifkey_type('_pd_proc_intensity_net              ','                                ',1),  &
   cifkey_type('_pd_proc_2theta_fixed               ','                                ',1),  &
   cifkey_type('_pd_proc_d_spacing                  ','                                ',1),  &
   cifkey_type('_diffrn_radiation_wavelength        ','                                ',1),  &    ! fine pdCIF
   cifkey_type('_atom_site_fract_x                  ','                                ',1),  &
   cifkey_type('_atom_site_fract_y                  ','                                ',1),  &
   cifkey_type('_atom_site_fract_z                  ','                                ',1),  &
   cifkey_type('_atom_site_label                    ','                                ',2),  &
   cifkey_type('_atom_site_type_symbol              ','                                ',2),  &                           
   cifkey_type('_cell_length_a                      ','                                ',1),  &
   cifkey_type('_cell_length_b                      ','                                ',1),  &
   cifkey_type('_cell_length_c                      ','                                ',1),  &
   cifkey_type('_cell_angle_alpha                   ','                                ',1),  &
   cifkey_type('_cell_angle_beta                    ','                                ',1),  &
   cifkey_type('_cell_angle_gamma                   ','                                ',1),  &
   cifkey_type('_atom_site_cartn_x                  ','                                ',1),  &
   cifkey_type('_atom_site_cartn_y                  ','                                ',1),  &
   cifkey_type('_atom_site_cartn_z                  ','                                ',1),  &
   cifkey_type('_atom_site_U_iso_or_equiv           ','                                ',1),  &
   cifkey_type('_atom_site_B_iso_or_equiv           ','                                ',1),  &
   cifkey_type('_atom_site_occupancy                ','                                ',1),  &
   cifkey_type('_symmetry_Int_Tables_number         ','_space_group_IT_number          ',1),  &
   cifkey_type('_symmetry_cell_setting              ','_space_group_crystal_system     ',2),  &
   cifkey_type('_symmetry_space_group_name_Hall     ','_space_group_name_Hall          ',2),  &
   cifkey_type('_symmetry_space_group_name_H-M      ','_space_group_name_H-M_alt       ',2),  &
   cifkey_type('_symmetry_equiv_pos_as_xyz          ','_space_group_symop_operation_xyz',2),  &
   cifkey_type('_atom_site_aniso_U_11               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_U_22               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_U_33               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_U_12               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_U_13               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_U_23               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_B_11               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_B_22               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_B_33               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_B_12               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_B_13               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_B_23               ','                                ',1),  &
   cifkey_type('_atom_site_aniso_label              ','                                ',2),  &
   cifkey_type('_atom_site_adp_type                 ','                                ',2),  &
   cifkey_type('_atom_site_thermal_displace_type    ','                                ',2),  &
   cifkey_type('_atom_site_calc_flag                ','                                ',2),  &
   cifkey_type('_atom_site_disorder_group           ','                                ',1)   &
   /)
!
   if (allocated(cifword)) deallocate(cifword)
   allocate(cifword(NW))
   cifword(:)%str = cifkey(:)%str
   cifword(:)%alt_str = cifkey(:)%alt_str
   cifword(:)%ktype = cifkey(:)%typ
!
   do i=1,NW
      select case (trim(cifword(i)%str))
         case ('_atom_site_aniso_U_11'); pos_anisU(1) = i
         case ('_atom_site_aniso_U_22'); pos_anisU(2) = i
         case ('_atom_site_aniso_U_33'); pos_anisU(3) = i
         case ('_atom_site_aniso_U_12'); pos_anisU(4) = i
         case ('_atom_site_aniso_U_13'); pos_anisU(5) = i
         case ('_atom_site_aniso_U_23'); pos_anisU(6) = i
         case ('_atom_site_aniso_B_11'); pos_anisB(1) = i
         case ('_atom_site_aniso_B_22'); pos_anisB(2) = i
         case ('_atom_site_aniso_B_33'); pos_anisB(3) = i
         case ('_atom_site_aniso_B_12'); pos_anisB(4) = i
         case ('_atom_site_aniso_B_13'); pos_anisB(5) = i
         case ('_atom_site_aniso_B_23'); pos_anisB(6) = i
         case ('_atom_site_aniso_label'); pos_anisU(7) = i; pos_anisB(7) = i;
      end select
   enddo
!
   end subroutine load_cif_dictionary

  !---------------------------------------------------------------------------------------------

   subroutine load_cif_dictionary_hkl(cifword)
   type(cifword_type), dimension(:), allocatable, intent(inout) :: cifword
   integer, parameter                                           :: NW = 9
   type cifkey_type
     character(len=40) :: str =      ' ' !main name
     character(len=40) :: alt_str = ' '  !alternative name
     integer           :: typ = 1        !Tipo: 1 per variabile numerica, 2 per variabile stringa
   end type
   type(cifkey_type), dimension(NW) :: cifkey = (/ &
   cifkey_type('_refln_index_h                     ','                                ',1),  & 
   cifkey_type('_refln_index_k                     ','                                ',1),  & 
   cifkey_type('_refln_index_l                     ','                                ',1),  &  
   cifkey_type('_refln_F_squared_calc              ','                                ',1),  &
   cifkey_type('_refln_F_squared_meas              ','                                ',1),  &
   cifkey_type('_refln_F_squared_sigma             ','                                ',1),  &
   cifkey_type('_refln_F_meas                      ','                                ',1),  &
   cifkey_type('_refln_F_sigma                     ','                                ',1),  &
   cifkey_type('_refln_observed_status	           ','                                ',2)   &
                                                 /)
   if (allocated(cifword)) deallocate(cifword)
   allocate(cifword(NW))
   cifword(:)%str = cifkey(:)%str
   cifword(:)%alt_str = cifkey(:)%alt_str
   cifword(:)%ktype = cifkey(:)%typ

   end subroutine load_cif_dictionary_hkl
 
  !---------------------------------------------------------------------------------------------

   subroutine manage_semicolon_block(funit,line,ier,str)
!
!  From CIF guide:
!  'If you need to write a block of text that stretches over more
!  than one line, you must surround it with semicolons as the first
!  character on the first line and trailing line'
!
   integer, intent(in)           :: funit
   character(len=*), intent(in)  :: line
   integer, intent(out)          :: ier
   character(len=:), allocatable :: str ! last line in block
   character(len=LENLINE_CIF)    :: line1
   integer                       :: lent
!
   ier = 0
   if (line(1:1) == ';') then
       str = '?'  !set as unknown
       do
          read(funit,'(a)',iostat=ier) line1
          if (ier /= 0) exit
          lent = len_trim(line1)
          if (lent == 0) cycle
          !corr if (line1(lent:lent) == ';') then
          if (line1(1:1) == ';') then  ! semicolon should be on the first column
              exit
          else
              str = line1(:lent)
          endif
       enddo
   else
       str = ' '
   endif
!
   end subroutine manage_semicolon_block

  !---------------------------------------------------------------------------------------------

   subroutine set_linecif(line,jumpl,nline,ndatab,newdata,dataname)
   USE strutil
   character(len=*), intent(inout) :: line
   logical, intent(out)            :: jumpl
   integer, intent(inout)          :: nline
   integer, intent(inout)          :: ndatab
   logical, intent(out)            :: newdata
   character(len=*), intent(out), optional   :: dataname
   integer                         :: pos
!
   call s_filter(line)                            ! filtra linea
   jumpl =  len_trim(line) == 0                   ! linea vuota?
   if (.not. jumpl) then
       line = adjustl(line)                       ! shift a sinistra della linea
!
!      cerca commento
       pos = index(line,'#')                 
       if (pos == 1) then    ! segnala commento a inizio riga
           jumpl = .true.
       endif
!
       if (lower(line(1:5)) == 'data_') then  ! trovato un nuovo data_block
           ndatab = ndatab + 1
           newdata = ndatab > 1
           if (present(dataname) .and. ndatab == 1) dataname = trim(line)
           !if (present(dataname) .and. ndatab == 1) write(0,*)nline+1,'DATA=',trim(line)
       endif
   endif
   nline = nline + 1
!   
   end subroutine set_linecif

  !---------------------------------------------------------------------------------------------

   subroutine read_CIFfile(filename,cif_phase,ndata,is_cell,cella,errcif,etype,dummy,bisotype,checkb)
   USE errormod
   USE atom_type_util
   USE strutil
   USE ccryst
   USE elements
   USE spginfom
   USE crystal_phase
   USE prog_constants
   USE fileutil
   character(len=*), intent(in)                               :: filename
   type(crystal_phase_t), dimension(:), allocatable, intent(out) :: cif_phase
   real, dimension(6) :: cellcif
   integer, intent(out)                                       :: ndata
   logical, intent(in)                                        :: is_cell  ! esiste gia' una cella?
   type(spaceg_type)                                          :: spg      ! space group properties
   type(error_type), dimension(:), allocatable, intent(out)   :: errcif
   character(len=1), intent(in), optional                     :: etype    ! 'w' error as set warn, 's' as severe error
   logical, intent(in), optional                              :: dummy    ! if .true. dummy atoms are considered ghost
   integer, intent(in), optional                              :: bisotype ! there are 2 type of assignement for biso
   logical, intent(in), optional                              :: checkb   ! check on adp betaij
   type(atom_type), dimension(:), allocatable                 :: atom
   type(cifword_type), dimension(:), allocatable              :: cifword
   type(error_type)                                           :: error
   integer                                                    :: i,j
   logical                                                    :: cart
   integer                                                    :: nat
   logical                                                    :: newdata
   logical                                                    :: is_u_aniso
   type(cell_type), intent(in)                                :: cella
   integer                                                    :: kanisolab
   integer                                                    :: pos
   real, dimension(6)                                         :: rcell
   real, dimension(3,3)                                       :: rgmat,gmat
   character(len=40)                                          :: symb_hall, symb_hm
   logical                                                    :: found, err_hall, err_hm
   integer                                                    :: nsymop
   type(symop_type), dimension(192)                           :: symop
   type(symop_type)                                           :: symoptemp
   integer                                                    :: ier
   integer                                                    :: kcell
   integer                                                    :: kcart,kcryst
   integer, dimension(3)                                      :: posw
   character(len=1), dimension(3)                             :: scoord = ['x','y','z']
   integer                                                    :: nloop_min
   integer                                                    :: pos_label, pos_symbol, pos_u_iso
   integer                                                    :: pos_b_iso, pos_occ, pos_adp_type, pos_adp
   integer                                                    :: pos_dum, pos_sitef, pos_disorder
   integer                                                    :: kb
   integer, dimension(7)                                      :: pos_anis
   character(len=4)                                           :: adp_type
   integer                                                    :: nsymb
   integer                                                    :: nline
   integer, parameter                                         :: NMAXCIF = 10
   character(len=100)                                         :: dataname
   integer                                                    :: numIT,bisotyp
   character(len=:), allocatable                              :: cell_settings
   logical                                                    :: dummya,checkbij
   type(file_handle)                                          :: ff
   integer                                                    :: j_in
!
   ndata = 0
   call ff%fopen(filename)
   if (ff%fail()) then
       ndata = ndata + 1
       call reallocate_err(errcif,1,.false.)
       call errcif(1)%set('Cannot open: '//trim(filename)//char(10)//'Message: '//trim(ff%err_msg()))
       return
   endif
   j_in = ff%handle()
!
   call load_cif_dictionary(cifword)
   call new_phases(cif_phase,NMAXCIF)
   call reallocate_err(errcif,NMAXCIF,.false.)
!
   if (present(dummy)) then
       dummya = dummy
   else
       dummya = .false.
   endif
   if (present(bisotype)) then
       bisotyp = bisotype
   else
       bisotyp = 1
   endif
   if (present(checkb)) then
       checkbij = checkb
   else
       checkbij = .false.
   endif
!
   nline = 0
   do      !loop sui data_blocks
       call cifword_fill(cifword,j_in,newdata,nline,dataname,error)  ! leggi il data_block
!
       if (.not.error%signal) then
!
           ndata = ndata + 1
           is_u_aniso = .false.
           kanisolab = 0
!
           cellcif = -(/10.0,10.0,10.0,90.,90.,90./) 
           symb_hall = ' '
           symb_hm = ' '
           nsymop = 0
           numIT = 0
           cell_settings = ' '
!
           nat = 0
           kcart = 0
           kcryst = 0
!
!          Find fractional coordinates
           do i=1,3
              posw(i) = string_locate('_atom_site_fract_'//scoord(i),cifword%str)
              if (posw(i) > 0) then
                  if (cifword(posw(i))%wok) kcryst = kcryst + 1
              endif
           enddo
!
           if (kcryst /= 3) then
!
!              Find cartesian coordinates
               do i=1,3
                  posw(i) = string_locate('_atom_site_cartn_'//scoord(i),cifword%str)
                  if (posw(i) > 0) then
                      if (cifword(posw(i))%wok) kcart = kcart + 1
                  endif
               enddo
               if (kcart /= 3) nat = 0
           endif
           cart = kcart == 3 .and. kcryst /= 3
           if (kcryst == 3 .or. kcart == 3) then
               nloop_min = min(cifword(posw(1))%nv,cifword(posw(2))%nv,cifword(posw(3))%nv)
               do i=1,nloop_min
                  if (cifword(posw(1))%vet(i) /= ERRCIF_VALUE .and. cifword(posw(2))%vet(i) /= ERRCIF_VALUE   &
                                                         .and. cifword(posw(2))%vet(i) /= ERRCIF_VALUE) then
                      nat = nat + 1
                  endif
               enddo
           endif
!
           call resize_atoms(atom,nat)
           if (nat > 0) then
               nat = 0
               pos_label = string_locate('_atom_site_label',cifword%str)
               pos_symbol = string_locate('_atom_site_type_symbol',cifword%str)
               pos_u_iso = string_locate('_atom_site_U_iso_or_equiv',cifword%str)
               pos_b_iso = string_locate('_atom_site_B_iso_or_equiv',cifword%str)
               pos_occ = string_locate('_atom_site_occupancy',cifword%str)
               pos_adp_type = string_locate('_atom_site_thermal_displace_type',cifword%str)
               pos_sitef = string_locate('_atom_site_calc_flag',cifword%str)
               pos_disorder = string_locate('_atom_site_disorder_group',cifword%str)
               if (.not.cifword(pos_adp_type)%wok) then
                   pos_adp_type = string_locate('_atom_site_adp_type',cifword%str)
               endif
               if (cifword(pos_disorder)%wok) then
                   atom%rcod(1) = 1
               endif
               do i=1,nloop_min
                  if (cifword(posw(1))%vet(i) /= ERRCIF_VALUE .and. cifword(posw(2))%vet(i) /= ERRCIF_VALUE   &
                                                         .and. cifword(posw(2))%vet(i) /= ERRCIF_VALUE) then
                      nat = nat + 1
                      atom(nat)%xc(1) =cifword(posw(1))%vet(i)
                      atom(nat)%xc(2) =cifword(posw(2))%vet(i)
                      atom(nat)%xc(3) =cifword(posw(3))%vet(i)
!
!                     Fill label
                      if (cifword(pos_label)%wok) then
                          if (i <= cifword(pos_label)%nv) atom(nat)%lab =trim(cifword(pos_label)%svet(i))
                      endif
!
!                     Fill atom type
                      if (cifword(pos_symbol)%wok) then
                          if (i <= cifword(pos_symbol)%nv) then
                              atom(nat)%ptab = pxen_from_string(cifword(pos_symbol)%svet(i))
                          endif
                      endif
!
!                     Fill adp
                      atom(nat)%biso = ERRCIF_VALUE
!
!                     Read adp type if present 
                      adp_type = ' '
                      if (cifword(pos_adp_type)%wok) then
                          if (i <= cifword(pos_adp_type)%nv) then
                              if (s_eqi(cifword(pos_adp_type)%svet(i),'Biso')) then
                                  adp_type = 'Biso'
                              elseif (s_eqi(cifword(pos_adp_type)%svet(i),'Uiso')) then
                                  adp_type = 'Uiso'
                              endif
                          endif
                      endif
!
!                     Read adp value e don't overwrite adp_type
                      pos_adp = 0
                      if (cifword(pos_u_iso)%wok) then
                          if (i <= cifword(pos_u_iso)%nv) then
                              if (cifword(pos_u_iso)%vet(i) /= ERRCIF_VALUE) then
                                  if (len_trim(adp_type) == 0) adp_type = 'Uiso'   ! type already assigned?
                                  pos_adp = pos_u_iso
                              endif
                          endif
                      else
                          if (cifword(pos_b_iso)%wok) then
                              if (i <= cifword(pos_b_iso)%nv) then
                                  if (cifword(pos_b_iso)%vet(i) /= ERRCIF_VALUE) then
                                      if (len_trim(adp_type) == 0) adp_type = 'Biso' ! type already assigned?
                                      pos_adp = pos_b_iso
                                  endif
                              endif
                          endif
                      endif
                      if (pos_adp > 0) then
                          select case (adp_type)
                            case ('Uiso')
                                 atom(nat)%biso = 8*cifword(pos_adp)%vet(i)*pi**2
                            case ('Biso')
                                 atom(nat)%biso = cifword(pos_adp)%vet(i)
                          end select
                      endif
!
!                     Fill occupancy factor
                      if (cifword(pos_occ)%wok) then
                          if (i <= cifword(pos_occ)%nv) then
                             if (cifword(pos_occ)%vet(i) /= ERRCIF_VALUE) then
                                 atom(nat)%och = cifword(pos_occ)%vet(i)
                             endif
                          endif
                      endif
!
!                     Disorder atom sites
                      if (cifword(pos_disorder)%wok) then
                           if (i <= cifword(pos_disorder)%nv) then
                               if (cifword(pos_disorder)%vet(i) /= ERRCIF_VALUE) then
                                   !if (nint(cifword(pos_disorder)%vet(i)) /= 1) then
                                   if (nint(cifword(pos_disorder)%vet(i)) > 1) then  ! exclude -1_ disorder around special position
                                       atom(nat)%rcod(1) = nat
                                   endif
                               endif
                          endif
                      endif
                  endif
               enddo
!
!              Fill anisotropic thermal factor
               if (all(cifword(pos_anisU(:))%wok .eqv. .true.)) then      ! aniso U are present
                   pos_anis(:) = pos_anisU(:)
               elseif (all(cifword(pos_anisB(:))%wok .eqv. .true.)) then  ! aniso B are present
                   pos_anis(:) = pos_anisB(:)
               else                                                       ! aniso absent
                   pos_anis(:) = 0
               endif
               if (pos_anis(1) > 0) then  ! aniso present?
                   do i=1,cifword(pos_anis(1))%nv
                      pos = string_locate(cifword(pos_anis(7))%svet(i),atom(:)%lab)
                      if (pos > 0) then
                          do kb=1,6
                             if (cifword(pos_anis(kb))%vet(i) == ERRCIF_VALUE) then
                                 atom(pos)%bij(1:kb) = 0
                                 exit
                             else
                                 atom(pos)%bij(kb) = cifword(pos_anis(kb))%vet(i)
                             endif
                          enddo
                      endif
                   enddo
               endif
           endif
!
           do i=1,size(cifword)
              if (cifword(i)%wok) then
                  select case (trim(cifword(i)%str))
                     case ('_cell_length_a')
                       cellcif(1) = cifword(i)%vet(1)
                     case ('_cell_length_b')
                       cellcif(2) = cifword(i)%vet(1)
                     case ('_cell_length_c')
                       cellcif(3) = cifword(i)%vet(1)
                     case ('_cell_angle_alpha')
                       cellcif(4) = cifword(i)%vet(1)
                     case ('_cell_angle_beta')
                       cellcif(5) = cifword(i)%vet(1)
                     case ('_cell_angle_gamma')
                       cellcif(6) = cifword(i)%vet(1)
                     case ('_symmetry_Int_Tables_number')
                       numIT = int(cifword(i)%vet(1))
                     case ('_symmetry_cell_setting')
                       cell_settings = trim(cifword(i)%svet(1))
                     case ('_symmetry_space_group_name_Hall')
                       symb_hall = trim(cifword(i)%svet(1))
                     case ('_symmetry_space_group_name_H-M','_space_group_name_H-M_alt')
                       symb_hm = trim(cifword(i)%svet(1))
                     case ('_symmetry_equiv_pos_as_xyz')
                       do j=1,cifword(i)%nv
                          call string_to_symop(cifword(i)%svet(j),symoptemp,ier)
                          if (ier == 0) then
                              nsymop = nsymop + 1
                              symop(nsymop) = symoptemp
                          else
                              nsymop = 0
                              exit
                          endif
                       enddo
                  end select
              endif
           enddo
!          
!          Get properties of space group
           found = .false.
           call spg%init()  ! init space as unknown
           if (len_trim(symb_hall) == 0 .and. len_trim(symb_hm) == 0 .and. nsymop == 0) then
               call error%setw('Missing space group!',code=ERR_MISSING_SPG)
           else
               err_hm = .false.
               err_hall = .false.
               if (nsymop > 0) then
                   call spg_load(spg,symop=symop(:nsymop),sfound=found)
                   if (.not.found) then
                       call spg%set_from_symop(symop(:nsymop),symb_hm,symb_hall,numIT,cell_settings)
                       found = .true.
                       nsymb = find_symbol(symb_hm,symb_hall) 
                       if (nsymb < 0) then
                           call error%setw("Symbol "//trim(symb_hall)//" and symmetry operators are inconsistent")  ! hall symbol was found but non the operators
                       elseif (nsymb > 0) then
                           call error%setw("Symbol "//trim(symb_hm)//" and symmetry operators are inconsistent")    ! hm symbol was found but non the operators
                       else
                           call error%setw("Unrecognized space group: symmetry operators from CIF file are used")       ! symbol and operators was not found
                       endif
                   else
                       error = spg_check_consistency(spg,symb_hall,symb_hm)
                   endif
               else
                   if (len_trim(symb_hall) > 0) then                 ! try to read hall symbol
                       call spg_load(spg,spgstr=symb_hall,sopt='H',sfound=found)
                       err_hall = .not.found
                   endif
                   if (.not.found .and. len_trim(symb_hm) > 0) then  ! try to read hm symbol
                       call spg_load(spg,spgstr=symb_hm,sopt='M',sfound=found)
                       err_hm = .not.found
                   endif
               endif
               if (.not.found) then
                   if (err_hm) then
                       call error%setw('Unknown space group name: '//trim(symb_hm),ERR_UNKNOWN_SPG)
                   elseif (err_hall) then
                       call error%setw('Unknown space group name: '//trim(symb_hall),ERR_UNKNOWN_SPG)
                   else
                       call error%setw('Unknown space group',ERR_UNKNOWN_SPG)
                   endif
               endif
           endif
           !moved after if (found) call cif_phase(ndata)%set_spg(spg)
!          
!          Gestione cella
           kcell = count(cellcif > 0)  ! quanti parametri di cella ho trovato?
           if (kcell > 0 .and. kcell < 6 .and. found) then ! estraggo dello s.g. i parametri mancanti
               call spg_set_cell(spg,cellcif)
!          
!              Ci sono ancora parametri non assegnati?
               do i=1,6  
                  if (cellcif(i) < 0) then
                      call error%setw('Reading cell from CIF failed',ERR_CELL_PARAM)
                      cellcif = abs(cellcif)  ! default
                      exit
                  endif
               enddo
           else    
               cellcif = abs(cellcif)
               if (kcell == 0) call error%setw('Missing cell parameters',ERR_CELL_PARAM) ! nessuna cella
           endif
!
!          Check if cell and space group are consistent
           if (kcell /= 0 .and. found) then
               if (index(spg%symbol_xhm,':R') > 0 .and. .not.uc_is_rhombohedral(cellcif,0.01)) then
                   if (uc_is_hexagonal(cellcif,0.01)) then  ! if this condition is not verified the cell has problem
                       symb_hm = spg%symbol_xhm
                       call s_rep(symb_hm,':R',':H')
                       call spg_load(spg,spgstr=symb_hm,sfound=found)
                   else   ! force rhombohedral cell
                       cellcif(2:3) = -1; cellcif(5:6) = -1
                       call spg_set_cell(spg,cellcif)
                   endif
               elseif (index(spg%symbol_xhm,':H') > 0 .and. .not.uc_is_hexagonal(cellcif,0.01)) then
                   if (uc_is_rhombohedral(cellcif,0.01)) then  ! if this condition is not verified the cell has problem
                       symb_hm = spg%symbol_xhm
                       call s_rep(symb_hm,':H',':R')
                       call spg_load(spg,spgstr=symb_hm,sfound=found)
                   else   ! force hexagonal cell
                       cellcif(2) = -1; cellcif(4:6) = -1
                       call spg_set_cell(spg,cellcif)
                   endif
               endif
           endif
           cif_phase(ndata)%cell = set_cell_type(cellcif)
           call cif_phase(ndata)%set_spg(spg)
!
           nat = numatoms(atom)
           if (nat > 0) then
               if (is_cell) then  ! se esiste gia' una cella e ...
                   if (cart) then
                       call cart_to_frac(atom,cella%get_ortoi())
                       call translate_in_cell(atom)  ! trasla in cella per coord. cart.
                   else
!
!                      ... se la cella cif è diversa riporta la struttura nella cella corrente
                       if (any(cellcif /= cella%get_par())) then
                           call coord_in_newcell(atom,set_cell_type(cellcif),cella)
                       endif
                   endif
               else
                   if (cart) then
                       call cart_to_frac(atom,orthomatrixi_std(cellcif))
                       call translate_in_cell(atom)  ! trasla in cella per coord. cart.
                   endif
               endif
!
!              Gestione specie: se ptab e' 0 forza la lettura dalla label
               pos_dum = string_locate('_atom_site_calc_flag',cifword%str)
               if (.not.dummya) cifword(pos_dum)%wok = .false.
               do i=1,numatoms(atom)
                  if (cifword(pos_dum)%wok) then   ! set nz=0 for dummy atoms
                      if (s_eqi(cifword(pos_sitef)%svet(i),'dum')) then
!corr                          write(6,*) ' Atomo dummy i, NZ(i)=',i,atom(i)%get_nz()
                          atom(i)%ptab = 0
                          cycle
                      endif
                  endif
                  if (atom(i)%ptab == 0) then
                      !corr atom(i)%nz = pxen_from_label(atom(i)%lab)    !!! FIXME- togliere
                      atom(i)%ptab = pxen_from_label(atom(i)%lab)
                  endif
               enddo
!
!              Gestione fattori termici
               if (pos_anis(1) > 0) then      ! aniso are present?
                   rgmat = matrice_mreciproca(cellcif)
                   rcell = cella_reciproca(cellcif)
                   if (pos_anis(1) == pos_anisU(1)) then    ! if U apply conversion in B
                       !rgmat = matrice_mreciproca(cellcif)
                       !rcell = cella_reciproca(cellcif)
                       do i=1,size(atom)
                          if (atom(i)%bij(1) > 0) then
                              atom(i)%bij = beta_from_uij(atom(i)%bij,rgmat,rcell)
                          endif
                       enddo
                   endif
                   gmat = matrice_metrica(cellcif)
                   do i=1,size(atom)
                      !if (atom(i)%bij(1) > 0) then
                      if (atom(i)%bij(1) /= 0) then
!!!!!FIXME - insert check if adp have physical meaning
                          if (checkbij) then
                              if (.not.aniso_adp_is_ok(atom(i)%bij,rgmat,rcell)) then
                                  !write(0,'(a,i0,1x,a)')'problem with adp at: ',i,trim(atom(i)%lab)
                                  atom(i)%bij(1) = 0
                              endif
                          endif
!
!                         se biso non esiste assegnalo dai beta
                          if (atom(i)%biso == ERRCIF_VALUE) then
                              atom(i)%biso = bequiv_from_beta(atom(i)%bij,gmat)
                          endif
                      endif
                   enddo
               endif
!
!              Assegna biso per quegli atomi che non hanno ancora un biso
               if (bisotyp == 1) then
                   do i=1,size(atom)
                      if (atom(i)%biso == ERRCIF_VALUE) call set_biso(atom(i),z_from_pxen(atom(i)%ptab)) 
                   enddo
               else
                   do i=1,size(atom)
                      if (atom(i)%biso == ERRCIF_VALUE) call set_biso_powcod(atom(i),z_from_pxen(atom(i)%ptab)) 
                   enddo
               endif
!
               call cif_phase(ndata)%set(atom)
           else
               if (kcell < 6 .and. .not.found) then   ! cell and space group are absent!
                   call error%set('Problems reading a CIF file: no structure found!',ERR_STRUCTURE)
               else
                   call error%setw('Problems reading a CIF file: no structure found!',ERR_STRUCTURE)
               endif
           endif
!
           errcif(ndata) = error
           call cif_phase(ndata)%set(pname=dataname)
       endif
!
       if (present(etype)) then  
           if (lower(etype) == 's' .and.  &
                (errcif(ndata)%code == ERR_MISSING_SPG .or. errcif(ndata)%code == ERR_UNKNOWN_SPG  &
            .or. errcif(ndata)%code == ERR_CELL_PARAM .or. errcif(ndata)%code == ERR_STRUCTURE))  &
              call errcif(ndata)%as_severe()
       endif
!
!      Se non c'e' un ulteriore data block da leggere esci dal loop
       if (.not.newdata) exit 
!
       if (ndata == numphase(cif_phase)) then
           call resize_phases(cif_phase,ndata+NMAXCIF)
           call reallocate_err(errcif,ndata+NMAXCIF)
       endif
!
   enddo    ! fine loop sui data_blocks
   call ff%fclose()
   call resize_phases(cif_phase,ndata)
!
   end subroutine read_CIFfile

!------------------------------------------------------------------------------------------

   subroutine create_ciffile(atom,cell,spg,elem,progname,commline,bond,std,symm,datas,filename,funit,data_block_name)
!
!  Create cif file
!   
   USE fileutil
   USE atom_type_util
   USE elements
   USE strutil
   USE spginfom
   USE unit_cell
   USE connect_mod
   USE cgeom
   USE ccryst
   USE datasetmod
   type(atom_type), dimension(:), allocatable, intent(in)     :: atom        ! atomi 
   type(cell_type), intent(in)                                :: cell
   type(spaceg_type), intent(in)                              :: spg         ! gruppo spaziale
   type(element_type), dimension(:), allocatable, intent(in)  :: elem        ! cell content
   character(len=*), intent(in)                               :: progname    ! nome del programma
   character(len=*), intent(in), optional                     :: commline    ! breve commento ad inizio file
   type(bond_type), dimension(:), allocatable, optional       :: bond
   logical, intent(in), optional                              :: std         ! write std
   logical, intent(in), optional                              :: symm        ! write symmetry code
   type(dataset_type), intent(in), optional                   :: datas
   character(len=*), intent(in), optional                     :: filename    ! se presente apre il file
   integer, intent(in), optional                              :: funit
   character(len=*), intent(in), optional                     :: data_block_name
   integer                                                    :: j_in
   integer                                                    :: i,k
   character(len=:), allocatable                              :: sfac_ref
   type(angle_type), dimension(:), allocatable                :: ang
   type(torsion_type), dimension(:), allocatable              :: tors
   real                                                       :: stdvol
   type(file_handle)                                          :: fcif
   character(len=3)                                           :: adv 
   real, dimension(3,3,6)                                     :: der_ortho
   logical                                                    :: sdistok
   real, dimension(3,3)                                       :: ortosd
   real, dimension(6)                                         :: uij
   real, dimension(3,3)                                       :: rgmat
   real, dimension(6)                                         :: rcell,cella
   type(atom_type), dimension(:), allocatable                 :: atoms
   type(bond_type), dimension(:), allocatable                 :: bonds
   logical                                                    :: symminfo,stdd
   real, dimension(6)                                         :: stdcell
   type(atom_type), dimension(:), allocatable                 :: atomcart
   character(len=:), allocatable                              :: strf
   type(element_type), dimension(:), allocatable              :: chform
   real                                                       :: mw,dens
   integer                                                    :: z_val, maxlab
   character(len=:), allocatable                              :: sform
!
!  Open file
   if (present(filename)) then
       call fcif%fopen(filename,'w')
       j_in = fcif%handle()
   else
       j_in = funit
   endif
!
!  Head information
   if (present(commline)) then
       write(j_in,"(a)")'# '//trim(commline)
   else
       write(j_in,"(a)") "#============================================================================="
   endif
   if (present(data_block_name)) then
       write(j_in,"(a)") data_block_name
   else
       write(j_in,"(a)") "data_global"
   endif
   write(j_in,"(a)") "#============================================================================="
   write(j_in,"(/a)") "_audit_creation_method              "//trim(progname)
!
   write(j_in,"(/a)")'_chemical_name_systematic     ?'
   if (numelem(elem) > 0 .and. numatoms(atom) > 0) then
       z_val = Z_value(atom,spg%nsymop,chform)
       call chemical_formula(chform,strf,fform=2,ord=.true.,hide1=.true.,hidecharge=.true.)
       strf=s_in_quotes(strf)
       write(j_in,"(a)") '_chemical_formula_moiety      '//strf
       write(j_in,"(a)") '_chemical_formula_sum         '//strf
       if (numelem(chform) > 0) then
           mw = molecular_weight(chform%ptab,chform%nw)
       else
           mw = 0.0 ! Only ghost atoms!
       endif
       write(j_in,"(a)") '_chemical_formula_weight      '//r_to_s(mw,3)
   else
       z_val = 1
       write(j_in,"(a)") '_chemical_formula_moiety      ?'
       write(j_in,"(a)") '_chemical_formula_sum         ?'
       write(j_in,"(a)") '_chemical_formula_weight      ?'
   endif
!
!  Crystal data
   write(j_in,"(/a)")"loop_"
   write(j_in,"(a)") "     _atom_type_symbol"
   write(j_in,"(a)") "     _atom_type_description"
   write(j_in,"(a)") "     _atom_type_scat_source"
   if (numelem(elem) > 0) then
       if (elem(1)%radtype == ELECTRON_SOURCE) then  
           sfac_ref = 'P. A. Doyle and P. S. Turner Acta Cryst. (1968). A24, 390-397'
       else
           sfac_ref = 'International Tables Vol C Tables 4.2.6.8 and 6.1.1.4'
       endif
       do i=1,numelem(elem)
          write(j_in,"(4x,a,3x,a,3x,a)")s_in_quotes(elem(i)%lab),s_in_quotes(elem(i)%name),s_in_quotes(sfac_ref)  
       enddo
   endif
!
   if (present(std)) then
       stdd = std
   else
       stdd = .false.
   endif
   cella = cell%get_par()
   stdcell = cell%get_sd()
   if (stdd) then
       write(j_in,"(/a,a)") "_cell_length_a                   ",string_esd(cella(1),stdcell(1),5)
       write(j_in,"(a,a)")  "_cell_length_b                   ",string_esd(cella(2),stdcell(2),5)
       write(j_in,"(a,a)")  "_cell_length_c                   ",string_esd(cella(3),stdcell(3),5)
       write(j_in,"(a,a)")  "_cell_angle_alpha                ",string_esd(cella(4),stdcell(4),3)
       write(j_in,"(a,a)")  "_cell_angle_beta                 ",string_esd(cella(5),stdcell(5),3)
       write(j_in,"(a,a)")  "_cell_angle_gamma                ",string_esd(cella(6),stdcell(6),3)
       stdvol = cell_volume_std(cell%volume(),cella,stdcell)
       write(j_in,"(a,a)")  "_cell_volume                     ",string_esd(cell%volume(),stdvol)
   else
       write(j_in,"(/a,f15.5)")   "_cell_length_a                   ",cella(1)
       write(j_in,"(a,f15.5)")    "_cell_length_b                   ",cella(2)
       write(j_in,"(a,f15.5)")    "_cell_length_c                   ",cella(3)
       write(j_in,"(a,5x,f10.3)") "_cell_angle_alpha                ",cella(4)
       write(j_in,"(a,5x,f10.3)") "_cell_angle_beta                 ",cella(5)
       write(j_in,"(a,5x,f10.3)") "_cell_angle_gamma                ",cella(6)
       write(j_in,"(a,5x,f10.3)") "_cell_volume                     ",cell%volume()
   endif
   write(j_in,"(a,i0)")      "_cell_formula_units_Z            ",z_val
   if (present(datas)) then
       if (datas%datatype == POW_DATA) then
           write(j_in,"(a)")         "_exptl_crystal_description    powder"
       else
           write(j_in,"(a)")         "_exptl_crystal_description      ?"
       endif
   else
       write(j_in,"(a)")         "_exptl_crystal_description      ?"
   endif
   write(j_in,"(a)")         "_exptl_crystal_colour           ?"
   if (numelem(elem) > 0 .and. numatoms(atom) > 0) then
       dens = density_value(molecular_weight(atom%ptab,atom%och*atom%ocry),cell%volume(),spg%nsymop)
       write(j_in,"(a/)")        "_cell_measurement_temperature    ?"
       write(j_in,"(a,f10.3)")   "_exptl_crystal_density_diffrn    ",dens
       write(j_in,"(a)")         "_exptl_crystal_density_meas      ?"
       write(j_in,"(a)")         "_exptl_crystal_density_method 'not measured'"
       if (present(datas)) then
           write(j_in,"(a,f10.3)")"_exptl_absorpt_coefficient_mu    ",linear_abs_coeff(atom,dens,datas%wave(1))/10
       endif
   endif
!
   call prn_cif(spg,j_in)
!
!  Atomic coordinates
   write(j_in,"(/a)") "loop_"
   write(j_in,"(a)")  "    _atom_site_type_symbol"
   write(j_in,"(a)")  "    _atom_site_label"
   write(j_in,"(a)")  "    _atom_site_fract_x"
   write(j_in,"(a)")  "    _atom_site_fract_y"
   write(j_in,"(a)")  "    _atom_site_fract_z"
   write(j_in,"(a)")  "    _atom_site_U_iso_or_equiv"
   write(j_in,"(a)")  "    _atom_site_occupancy"
   write(j_in,"(a)")  "    _atom_site_adp_type"
   if (numatoms(atom) > 0) then
       sform = '(a3,1x,a'//i_to_s(maxval(len_trim(atom(:)%lab)))//',1x)'
   endif
   do i=1,numatoms(atom)
      adv = 'no'
      write(j_in,sform,advance=adv)atom(i)%specie(),atom(i)%lab
      do k=1,3
         if (abs(atom(i)%xsd(k)) > 0.0 .and. stdd) then
             write(j_in,'(a,1x)',advance=adv)string_esd(atom(i)%xc(k),atom(i)%xsd(k))
         else
             write(j_in,'(a,1x)',advance=adv)trim(r_to_s(atom(i)%xc(k),4))
         endif
      enddo
      if (abs(atom(i)%bsd) > 0.0 .and. stdd) then
          write(j_in,'(a,1x)',advance=adv)string_esd(u_from_b(atom(i)%biso),u_from_b(atom(i)%bsd))
      else
          write(j_in,'(a,1x)',advance=adv)trim(r_to_s(u_from_b(atom(i)%biso),4))
      endif
      if (abs(atom(i)%och) > 0.0 .and. stdd) then
          write(j_in,'(a,1x)',advance=adv)string_esd(atom(i)%och,atom(i)%osd)
      else
          !write(j_in,'(f10.4,1x)',advance=adv)atom(i)%och
          write(j_in,'(a,1x)',advance=adv)trim(r_to_s(atom(i)%och,4))
      endif
      write(j_in,'(a)')'Uiso'
   enddo
!
   if (numatoms(atom) > 0) then
!
!      Anisotropic displacement parameters
       if (any(atom%bij(1) > 0.0)) then
           write(j_in,"(/a)") "loop_"
           write(j_in,"(a)")  "      _atom_site_aniso_label"
           write(j_in,"(a)")  "      _atom_site_aniso_U_11"
           write(j_in,"(a)")  "      _atom_site_aniso_U_22"
           write(j_in,"(a)")  "      _atom_site_aniso_U_33"
           write(j_in,"(a)")  "      _atom_site_aniso_U_12"
           write(j_in,"(a)")  "      _atom_site_aniso_U_13"
           write(j_in,"(a)")  "      _atom_site_aniso_U_23"
           rgmat = matrice_mreciproca(cella)
           rcell = cella_reciproca(cella)
           do i=1,size(atom)
              if (atom(i)%bij(1) > 0.0) then
                  uij = u_from_bij(atom(i)%bij,rgmat,rcell)
                  write(j_in,'(a,6(1x,f10.6))')trim(atom(i)%lab),uij
              endif
           enddo
       endif
!
!      Bonds and angles and torsions
       if (present(bond)) then
           if (present(symm)) then
               symminfo = symm
           else
               symminfo = .false.
           endif
           !ortosd = orthomatrix_std(cella)
           ortosd = cell%get_ortom()
           if (stdd) then
               der_ortho = deriv_orthomatrix_std(cella)
               sdistOK = .true.
           else
               sdistOK = .false.
           endif
           maxlab = get_maxlen_labels(atom)
           bondsif: if (numbonds(bond) > 0) then
               if (symminfo) then
                   call init_for_symm(atom,bond,atoms,bonds)
                   call expand_contacts(atom,cell,spg,atoms,bonds,.false.) 
                   call frac_to_cart_copy(atoms,atomcart,cell%get_ortom())
                   call check_angle(atomcart,cell,bonds,angl=30.0) ! important in presence of shared sites
                   call sort_bonds(atoms,bonds)
                   if (stdd) then
                       call write_distances(j_in,atoms,bonds,cella,sdistok,ortosd,stdcell)
                   else
                       call write_distances(j_in,atoms,bonds,cella,sdistok,ortosd)
                   endif
                   call bond_to_angle(bonds,atoms,cell%get_g(),ang)
                   if (numangles(ang) > 0) then
                       call write_angles(j_in,atoms,bonds,ang,sdistok,cella,ortosd,numatoms(atom),maxlab)
                       call bonds_to_torsions(bonds,atoms,cell,tors)
                       if (numtorsions(tors) > 0) then
                           call write_torsions(j_in,atoms,tors,sdistok,cell)
                       endif
                   endif
               else
                   if (stdd) then
                       call write_distances(j_in,atom,bond,cella,sdistok,ortosd,stdcell)
                   else
                       call write_distances(j_in,atom,bond,cella,sdistok,ortosd)
                   endif
                   call bond_to_angle(bond,atom,cell%get_g(),ang)
                   if (numangles(ang) > 0) then
                       call write_angles(j_in,atom,bond,ang,sdistok,cella,ortosd,numatoms(atom),maxlab)
                       call bonds_to_torsions(bond,atom,cell,tors)
                       if (numtorsions(tors) > 0) then
                           call write_torsions(j_in,atom,tors,sdistok,cell)
                       endif
                   endif
               endif
           endif bondsif
       endif
   endif
!
   if (present(datas)) then
       call write_instrument(j_in,datas)
   endif
!
   if (present(filename)) then
       call fcif%fclose()
   endif
!
   end subroutine create_ciffile

!------------------------------------------------------------------------------------------

   subroutine write_wavelengths(j_in,datas)
   use datasetmod
   integer, intent(in)            :: j_in
   type(dataset_type), intent(in) :: datas
!
   if (datas%nwave > 0) then
       write(j_in,"(a,f10.6)") '_diffrn_radiation_wavelength   ',datas%wave(1)
   else
       write(j_in,"(a)")       '_diffrn_radiation_wavelength    ?'
   endif
   write(j_in,"(a)")          '_diffrn_radiation_type          '//diffrn_radiation_string(datas%radtype,datas%wave(1))
!
   end subroutine write_wavelengths

!------------------------------------------------------------------------------------------

   subroutine write_instrument(j_in,datas)
   use datasetmod
   use elements, only: ELECTRON_SOURCE
   integer, intent(in)            :: j_in
   type(dataset_type), intent(in) :: datas
!
   write(j_in,*)
   if(datas%radtype == ELECTRON_SOURCE) then
      write(j_in,"(a)")      "_cell_measurement_radiation      "//diffrn_radiation_string(datas%radtype,datas%wave(1))
   endif
   write(j_in,"(a)")           '_diffrn_ambient_temperature     ?'
   call write_wavelengths(j_in,datas)
   write(j_in,"(a)")           '_diffrn_measurement_device_type ?'
!
   end subroutine write_instrument

!------------------------------------------------------------------------------------------

   subroutine write_block_datacif(j_in,datas,code,rinfo)
   USE datasetmod
   use progtype, only: refine_info_type
   integer, intent(in)                          :: j_in
   type(dataset_type), intent(in)               :: datas
   integer, intent(in)                          :: code
   type(refine_info_type), intent(in), optional :: rinfo
   integer                                      :: i
!
! raw / calculated data loop
   if (datas%datatype == POW_DATA) then
       select case (code)
         case (EXPORT_POWCIF_RIET)
           write(j_in,'(a/)')'# POWDER PROFILE'
           write(j_in,'(a,1x,f10.3)') '_pd_meas_2theta_range_min',datas%tmin
           write(j_in,'(a,1x,f10.3)') '_pd_meas_2theta_range_max',datas%tmax
           write(j_in,'(a,1x,i10)')   '_pd_proc_number_of_points',datas%npoints()
           if (present(rinfo)) then
               write(j_in,*)
               call write_block_rinfo(j_in,rinfo)
           endif
           write(j_in,*)
           write(j_in,'(a)') 'loop_'
           write(j_in,'(a)') '     _pd_meas_2theta_scan'
           write(j_in,'(a)') '     _pd_meas_counts_total'
           write(j_in,'(a)') '     _pd_proc_2theta_corrected'
           write(j_in,'(a)') '     _pd_calc_intensity_total'
           write(j_in,'(a/)')'     _pd_proc_intensity_bkg_calc'
           do i=1,datas%npointsc()
              write(j_in,'(5(1x,f15.6))') datas%x(i),datas%y(i),datas%x0(i),datas%yc(i),datas%yb(i)
           enddo

         case (EXPORT_POWCIF_OBS)
           write(j_in,'(a/)')'# POWDER PROFILE'
           write(j_in,'(a,1x,f10.3)') '_pd_meas_2theta_range_min',datas%tmin
           write(j_in,'(a,1x,f10.3)') '_pd_meas_2theta_range_max',datas%tmax
           write(j_in,'(a,1x,i10)')   '_pd_proc_number_of_points',datas%npoints()
           write(j_in,*)
           call write_wavelengths(j_in,datas)
           write(j_in,*)
           write(j_in,'(a)') 'loop_'
           write(j_in,'(a)') '     _pd_meas_2theta_scan'
           write(j_in,'(a)') '     _pd_proc_d_spacing'
           write(j_in,'(a)') '     _pd_meas_counts_total'
           if (datas%has_back()) then
               write(j_in,'(a)')'     _pd_proc_intensity_intensity_net'
               write(j_in,'(a)')'     _pd_proc_intensity_bkg_calc'
               do i=1,datas%npoints()
                  write(j_in,'(5(1x,f15.6))') datas%x(i),datas%dval(i),datas%y(i),  &
                                              datas%y(i)-datas%yb(i),datas%yb(i)
               enddo
           else
               do i=1,datas%npoints()
                  write(j_in,'(3(1x,f15.6))') datas%x(i),datas%dval(i),datas%y(i)
               enddo
           endif
         end select
   endif
!
   end subroutine write_block_datacif

!------------------------------------------------------------------------------------------

   subroutine write_block_rinfo(j_in,rinfo)
   use progtype, only: refine_info_type
   integer, intent(in)                :: j_in
   type(refine_info_type), intent(in) :: rinfo
   write(j_in,'(a,1x,f12.5)')  '_pd_proc_ls_prof_R_factor     ',rinfo%rp/100
   write(j_in,'(a,1x,f12.5)')  '_pd_proc_ls_prof_wR_factor    ',rinfo%rwp/100
   write(j_in,'(a,1x,f12.5/)') '_pd_proc_ls_prof_wR_expected  ',rinfo%rexp/100

   write(j_in,'(a,i0)')        '_refine_ls_number_reflns      ',rinfo%nref
   write(j_in,'(a,i0)')        '_refine_ls_number_parameters  ',rinfo%npartot
   write(j_in,'(a,i0)')        '_refine_ls_number_restraints  ',0           
   if (rinfo%riding) then
       write(j_in,'(a)')       '_refine_ls_hydrogen_treatment  constr'
   endif
   write(j_in,'(a,1x,f12.5)')  '_refine_ls_R_I_factor         ',rinfo%rbragg/100
!  This is the same as chi, i.e. the square root of 'chi squared'
   write(j_in,'(a,1x,f12.5)')'_refine_ls_goodness_of_fit_all',sqrt(rinfo%chi2) 
   write(j_in,'(a,1x,f12.5)')'_refine_ls_restrained_S_all   ',rinfo%sall
!
   end subroutine write_block_rinfo 

!------------------------------------------------------------------------------------------

   subroutine export_data_cif(filename,datas)
   USE datasetmod
   USE fileutil
   character(len=*), intent(in)   :: filename
   type(dataset_type), intent(in) :: datas
   type(file_handle)              :: fpatt
!
   call fpatt%fopen(filename,'w')
!
   if (fpatt%good()) then
       call write_block_datacif(fpatt%handle(),datas,EXPORT_POWCIF_OBS)
       call fpatt%fclose()
   endif
!
   end subroutine export_data_cif

!------------------------------------------------------------------------------------------

   subroutine write_distances(j_in,atom,legm,cella,sdistok,ortosd,stdcell)
   USE atom_type_util
   USE connect_mod
   USE unit_cell
   USE strutil
   USE cgeom
   integer, intent(in)                                       :: j_in
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm
   real, dimension(6)                                        :: cella
   logical, intent(in)                                       :: sdistok 
   real, dimension(3,3), intent(in)                          :: ortosd
   real, dimension(6), intent(in), optional                  :: stdcell
   integer                                                   :: i
   integer                                                   :: n1,n2
   real, dimension(3,3,6)                                    :: der_ortho
!
   if (present(stdcell)) then
       der_ortho = deriv_orthomatrix_std(cella)
   endif
!
   write(j_in,"(/a)") "loop_"
   write(j_in,"(a)")  "    _geom_bond_atom_site_label_1"
   write(j_in,"(a)")  "    _geom_bond_atom_site_label_2"
   write(j_in,"(a)")  "    _geom_bond_distance"
   write(j_in,"(a)")  "    _geom_bond_site_symmetry_2"
   do i=1,numbonds(legm)
      n1 = legm(i)%n1
      n2 = legm(i)%n2
      if (sdistok .and. is_present_sd(atom(n1)) .and. is_present_sd(atom(n2))) then
          call distance_and_sd(stdcell,ortosd,der_ortho,atom(n1)%xc,atom(n2)%xc,  &
                               atom(n1)%xsd,atom(n2)%xsd,legm(i)%dist,legm(i)%sigma)
          write(j_in,"(a,1x,a)")trim(atom(n1)%lab)//" "//trim(atom(n2)%lab)//     &
          "  "//string_esd(legm(i)%dist,legm(i)%sigma),atom(n2)%symm_code()
               !write(0,*)'BOND=',i,legm(i)%sigma
      else
          legm(i)%dist = xdistance(atom(n1)%xc,atom(n2)%xc,ortosd)
          write(j_in,"(a)")trim(atom(n1)%lab)//" "//trim(atom(n2)%lab)//       &
          "  "//r_to_s(legm(i)%dist)//" "//atom(n2)%symm_code()
              !write(j_in,*)i,legm(i)%n1,legm(i)%n2,'atom2=',atom(n2)%xc
      endif
   enddo
!
   end subroutine write_distances 

!------------------------------------------------------------------------------------------

   subroutine write_angles(j_in,atom,legm,ang,sdistok,cella,ortosd,natau,maxl)
   USE atom_type_util
   USE connect_mod
   USE cgeom
   USE trig_constants
   USE strutil
   integer, intent(in)                                     :: j_in
   type(atom_type), dimension(:), allocatable, intent(in)  :: atom
   type(bond_type), dimension(:), allocatable, intent(in)  :: legm
   type(angle_type), dimension(:), allocatable, intent(in) :: ang
   logical, intent(in)                                     :: sdistok 
   real, dimension(6)                                      :: cella
   real, dimension(3,3)                                    :: ortosd
   integer, intent(in)                                     :: natau
   integer, intent(in)                                     :: maxl
   integer                                                 :: n1,n2,n3
   real                                                    :: d12,d23,d13,sd1,sd2,sd3,sang
   integer                                                 :: i,pos
   character(len=:), allocatable                           :: frmt
!
   write(j_in,"(/a)") "loop_"
   write(j_in,"(a)")  "_geom_angle_atom_site_label_1"
   write(j_in,"(a)")  "_geom_angle_atom_site_label_2"
   write(j_in,"(a)")  "_geom_angle_atom_site_label_3"
   write(j_in,"(a)")  "_geom_angle"
   write(j_in,"(a)")  "_geom_angle_site_symmetry_1"
   write(j_in,"(a)")  "_geom_angle_site_symmetry_3"
   frmt = "(a,t"//i_to_s(maxl*3+2+1)//",1x,f8.2,1x,a7,1x,a7)" ! +2 for space, +1 next position
   if (sdistok) then
       do i=1,numangles(ang)
          n1 = ang(i)%n1
          n2 = ang(i)%n2   
          if (n2 > natau) cycle ! central atom should not be outside asymmetric unit
          n3 = ang(i)%n3
          if (is_present_sd(atom(n1)) .and. is_present_sd(atom(n2)) .and. is_present_sd(atom(n3))) then
!
!             square of distances
              pos = bond_position(legm,n1,n2)
              d12 = legm(pos)%dist*legm(pos)%dist
              pos = bond_position(legm,n2,n3)
              d23 = legm(pos)%dist*legm(pos)%dist
              d13 = xdistance(atom(n1)%xc,atom(n3)%xc,ortosd)**2
!
!             square of ortogonalized SD 
              sd1 = sum((cella(1:3)*atom(n1)%xsd)**2) / 3
              sd2 = sum((cella(1:3)*atom(n2)%xsd)**2) / 3
              sd3 = sum((cella(1:3)*atom(n3)%xsd)**2) / 3
!
              sang = sqrt(sd1/d12 + sd3/d23 + sd2*d13/(d12*d23))*rtod
!
              write(j_in,"(a,1x,a,1x,a)")trim(atom(n1)%lab)//" "//trim(atom(n2)%lab)//" "//            &
              trim(atom(n3)%lab)//" "//string_esd(ang(i)%val,sang),atom(n1)%symm_code(),atom(n3)%symm_code()
          else
              !write(j_in,"(a,1x,f8.2,1x,a,1x,a)")    &
              write(j_in,frmt)    &
              trim(atom(n1)%lab)//" "//trim(atom(n2)%lab)//" "//trim(atom(n3)%lab),    &
              ang(i)%val,atom(n1)%symm_code(),atom(n3)%symm_code()
          endif
       enddo
   else
       do i=1,numangles(ang)
          if (ang(i)%n2 > natau) cycle ! central atom should not be outside asymmetric unit
          !write(j_in,"(a,1x,f8.2,1x,a,1x,a)")  &
          write(j_in,frmt)    &
          trim(atom(ang(i)%n1)%lab)//" "//trim(atom(ang(i)%n2)%lab)//" "//trim(atom(ang(i)%n3)%lab), &
          ang(i)%val,atom(ang(i)%n1)%symm_code(),atom(ang(i)%n3)%symm_code()
       enddo
   endif
!
   end subroutine write_angles

!------------------------------------------------------------------------------------------

   subroutine write_torsions(j_in,atom,tors,sdistok,cell)
   USE atom_type_util
   USE connect_mod
   USE strutil
   USE cgeom
   USE trig_constants
   USE unit_cell
   integer, intent(in)                                       :: j_in
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(torsion_type), dimension(:), allocatable, intent(in) :: tors
   logical, intent(in)                                       :: sdistok 
   type(cell_type), intent(in)                               :: cell
   integer                                                   :: i,k
   real                                                      :: r12,r23,r34,fact1,fact2
   real                                                      :: phi1,cphi1,sphi1,cotphi1
   real                                                      :: phi2,cphi2,sphi2,cotphi2
   integer, dimension(4)                                     :: nt
   real, dimension(4)                                        :: sdiso
   real                                                      :: stors,ctors

   write(j_in,"(/a)") "loop_"
   write(j_in,"(a)")  "   _geom_torsion_atom_site_label_1"
   write(j_in,"(a)")  "   _geom_torsion_atom_site_label_2"
   write(j_in,"(a)")  "   _geom_torsion_atom_site_label_3"
   write(j_in,"(a)")  "   _geom_torsion_atom_site_label_4"
   write(j_in,"(a)")  "   _geom_torsion"
   write(j_in,"(a)")  "   _geom_torsion_site_symmetry_1"
   write(j_in,"(a)")  "   _geom_torsion_site_symmetry_2"
   write(j_in,"(a)")  "   _geom_torsion_site_symmetry_3"
   write(j_in,"(a)")  "   _geom_torsion_site_symmetry_4"
   if (sdistok) then
       do i=1,numtorsions(tors)
          nt(:) = [tors(i)%n1,tors(i)%n2,tors(i)%n3,tors(i)%n4]
          if (is_present_sd(atom(nt(1))) .and. is_present_sd(atom(nt(2)))               &
        .and. is_present_sd(atom(nt(3))) .and. is_present_sd(atom(nt(4)))) then
!
!             set atoms as isotropic and compute variance on cartesian coordinates
              do k=1,4
                 sdiso(k) = sum(matmul(cell%get_ortom(),atom(nt(k))%xsd)**2) / 3
              enddo
!
!             Use formula (16) from Acta Cryst. (1972). A28,213
              phi1 = angleC(atom(nt(1))%xc,atom(nt(2))%xc,atom(nt(3))%xc,cell%get_g())
              cphi1 = cos(phi1)
              sphi1 = sin(phi1)
              cotphi1 = 1/tan(phi1)
              phi2 = angleC(atom(nt(2))%xc,atom(nt(3))%xc,atom(nt(4))%xc,cell%get_g())
              cphi2 = cos(phi2)
              sphi2 = sin(phi2)
              cotphi2 = 1/tan(phi2)
              ctors = cos(tors(i)%val*dtor)
              r12 = distanzaC(atom(nt(1))%xc,atom(nt(2))%xc,cell%get_g())
              r23 = distanzaC(atom(nt(2))%xc,atom(nt(3))%xc,cell%get_g())
              r34 = distanzaC(atom(nt(3))%xc,atom(nt(4))%xc,cell%get_g())
              fact1 = (r23 - r12*cphi1)/(r12*sphi1)
              fact2 = (r23 - r34*cphi2)/(r34*sphi2)
              stors = sdiso(1)/(r12*sphi1**2) +  &
                     (sdiso(2)/r23**2)*(fact1**2 - 2*fact1*cotphi2*ctors+cotphi2*cotphi2) + &
                     (sdiso(3)/r23**2)*(cotphi1*cotphi1 - 2*fact2*cotphi1*ctors + fact2**2) + &
                     sdiso(4)/(sphi2**2 * r34**2)
              stors = sqrt(stors)*rtod
               !write(71,'(i5,a,16(f10.3))')i,'tors=',tors(i)%val,stors,fact1,fact2,r12,r23,r34,cphi1,sphi1,cphi2,sphi2,ctors,cotphi1,cotphi2,phi1,phi2
              write(j_in,"(a,4(1x,a))")    &
              trim(atom(tors(i)%n1)%lab)//" "//trim(atom(tors(i)%n2)%lab)//" "//           &
              trim(atom(tors(i)%n3)%lab)//" "//trim(atom(tors(i)%n4)%lab)//" "//string_esd(tors(i)%val,stors), &
              atom(tors(i)%n1)%symm_code(),atom(tors(i)%n2)%symm_code(),atom(tors(i)%n3)%symm_code(),atom(tors(i)%n4)%symm_code()
              !write(j_in,*)'angle=',phi1,phi2
          else
              write(j_in,"(a,1x,f8.2,4(1x,a))")    &
              trim(atom(nt(1))%lab)//" "//trim(atom(nt(2))%lab)//" "//     &
              trim(atom(nt(3))%lab)//" "//trim(atom(nt(4))%lab),tors(i)%val, &
              atom(tors(i)%n1)%symm_code(),atom(tors(i)%n2)%symm_code(),atom(tors(i)%n3)%symm_code(),atom(tors(i)%n4)%symm_code()
          endif
       enddo
   else
       do i=1,numtorsions(tors)
          write(j_in,"(a,1x,f8.2,4(1x,a))")    &
          trim(atom(tors(i)%n1)%lab)//" "//trim(atom(tors(i)%n2)%lab)//" "//     &
          trim(atom(tors(i)%n3)%lab)//" "//trim(atom(tors(i)%n4)%lab),tors(i)%val, &
          atom(tors(i)%n1)%symm_code(),atom(tors(i)%n2)%symm_code(),atom(tors(i)%n3)%symm_code(),atom(tors(i)%n4)%symm_code()
       enddo
   endif
 
   end subroutine write_torsions

!------------------------------------------------------------------------------------------

   function diffrn_radiation_string(radtype,wavel) result(str)
!
!  Convert wavelength in a string for cif file
!
   USE elements
   integer, intent(in)                       :: radtype
   real, intent(in)                          :: wavel
   character(len=:), allocatable             :: str
   character(len=9), dimension(5), parameter :: srad_vect = &
                                                 ['Cu K\a~1~','Mo K\a~1~','Cr K\a~1~','Co K\a~1~','Fe K\a~1~']
   integer :: rcode
!
   select case (radtype)
     case (RX_SOURCE)
       rcode = radiation_code(wavel)
       select case (rcode)
         case (1:5)
           str = "'"//srad_vect(rcode)//"'"
         case default
           str = '?'
       end select
     case (NEUTRON_SOURCE)
       str = 'neutron'

     case (ELECTRON_SOURCE)
       str = 'electron'
   end select
!
   end function diffrn_radiation_string

!------------------------------------------------------------------------------------------

   logical function is_cif_with_reflections(filename) result(is_cif)
   use fileutil
   character(len=*), intent(in)    :: filename
   type(file_handle)               :: fnw
   character(len=200)              :: line
   integer                         :: ier 
!
   is_cif = .false.
   call fnw%fopen(filename,'r')
   if (.not.fnw%good()) return
!
   do while (.not.is_cif)
     read(fnw%handle(),'(a)',iostat=ier)line
     if (ier /= 0) exit
     if (is_comment_line(line,['#'])) cycle
     is_cif = index(line,"_refln_index_h") > 0
   enddo 
   call fnw%fclose()
!
   end function is_cif_with_reflections

END MODULE cif_frm
