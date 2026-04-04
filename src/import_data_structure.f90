module import_data_struct

contains

   subroutine import_structure(filename,crystal,err)
   use gen_frm
   use crystal_phase
   use variables, only: dataset
   use errormod
   use datasetmod
   character(len=*), intent(in)       :: filename
   type(crystal_phase_t), intent(out) :: crystal
   type(error_type), intent(out)      :: err
   type(cell_type)                    :: cell
   type(spaceg_type)                  :: spg
   logical                            :: has_symmetry = .false.
   logical                            :: gui = .false.
   logical                            :: nowarning = .false.
   logical                            :: cancel_req
   logical                            :: mergecont = .false.
!
   call import_crystal(filename,ALL_FILES,crystal%at,crystal%bond,crystal%elem,   &
                       cell,spg,has_symmetry,get_wave1(dataset),   &
                       get_radtype(dataset),gui,nowarning,err,cancel_req,mergecont)
   if (.not.err%signal) then
       call crystal%set_symmetry(spg,cell)
   endif
!
   end subroutine import_structure

!-----------------------------------------------------------------------------------------------------

   subroutine test_import()
   use errormod
   use crystal_phase
   use exportfiles
   use fileutil
   type(crystal_phase_t)          :: crystal
   type(error_type)               :: err
   integer                        :: ier
   character(len=:), allocatable  :: filename
   integer                        :: nat, nrefl, zval
   character(len=:), allocatable  :: sform
   real, dimension(6)             :: cellpar
   integer, dimension(6)          :: icell
   real                           :: vol, dens, mu, rir, wavelen
   real, dimension(:), allocatable :: inte

   !call import_structure('/home/corrado/test_expo/merca_true.cif',crystal,err)
   filename = '/home/corrado/test_expo/1000099.cif'
   call import_structure(filename,crystal,err)
   if (.not.err%signal) then
       call crystal%print(0)
       call intensity_file_for_database(crystal, ier, &
                                        nat, sform, cellpar, icell, vol, dens, zval, mu, &
                                        nrefl, rir, inte, wavelen)
       if (ier == 0) then
           call write_intensity_file(file_change_ext(filename,'int'), nat, sform,        &
                                     crystal%elem, cellpar, icell,                        &
                                     crystal%spg%symbol_xhm, crystal%spg%csys_code,      &
                                     vol, dens, zval, mu, nrefl, rir, crystal%ref, inte, &
                                     wavelen)
       endif
   endif

   end subroutine test_import

!-----------------------------------------------------------------------------------------------------

   subroutine get_crystal_info_from_cif(cif_file,                                   &
                                         nat_c, cellpar_c, icell_c, vol_c, dens_c,   &
                                         zval_c, mu_c, nrefl_c, rir_c, wavelen_c,    &
                                         sform_c, subfile_c, spg_sym_c, crysys_c,    &
                                         refl_h, refl_k, refl_l,                     &
                                         refl_tth, refl_d, refl_mult,                &
                                         refl_lp, refl_fc2, refl_inte, refl_ipct,    &
                                         nrefl_print_c,                              &
                                         nelem_c, specie_label_c,                    &
                                         ier_c)                                      &
       bind(C, name='get_crystal_info_from_cif')
   use iso_c_binding
   use exportfiles,        only: intensity_file_for_database
   use crystal_phase
   use errormod
   use atom_type_util,     only: is_organic
   use reflection_type_util
   use spginfom,           only: uc_is_rhombohedral, uc_is_hexagonal, cry_sys, CS_Trigonal
   use counts,             only: dvalue
   use strutil,            only: copy_string_to_c_array
   use general, only: lo
   use elements, only: specie_from_pxen
   implicit none
   ! --- Input ---
   character(kind=C_CHAR), dimension(*), intent(in)   :: cif_file
   ! --- Output scalars ---
   integer(C_INT),  intent(out)                       :: nat_c, zval_c
   integer(C_INT),  intent(out)                       :: nrefl_c, nrefl_print_c, ier_c
   real(C_FLOAT),   dimension(6), intent(out)         :: cellpar_c
   integer(C_INT),  dimension(6), intent(out)         :: icell_c
   real(C_FLOAT),   intent(out)                       :: vol_c, dens_c, mu_c, rir_c, wavelen_c
   ! --- Output strings (null-terminated C strings) ---
   character(kind=C_CHAR), dimension(256), intent(out) :: sform_c
   character(kind=C_CHAR), dimension(32),  intent(out) :: subfile_c
   character(kind=C_CHAR), dimension(64),  intent(out) :: spg_sym_c
   character(kind=C_CHAR), dimension(64),  intent(out) :: crysys_c
   ! --- Output reflection arrays (up to NMAXREFLD=500 entries) ---
   integer(C_INT), dimension(500), intent(out)        :: refl_h, refl_k, refl_l, refl_mult
   real(C_FLOAT),  dimension(500), intent(out)        :: refl_tth, refl_d
   real(C_FLOAT),  dimension(500), intent(out)        :: refl_lp, refl_fc2, refl_inte, refl_ipct
   ! --- Output element species (up to MAXELEM_C=100 elements, 2-char symbol + null) ---
   integer(C_INT),  intent(out)                       :: nelem_c
   character(kind=C_CHAR), dimension(3, 100), intent(out) :: specie_label_c
   ! --- Local Fortran variables ---
   type(crystal_phase_t)                              :: crystal
   type(error_type)                                   :: err
   integer                                            :: ier_f, nat_f, zval_f, nrefl_f
   integer                                            :: nrefl_print_f, n, i
   character(len=:), allocatable                      :: sform_f, filename_f
   character(len=:), allocatable                      :: crysys_f
   real, dimension(6)                                 :: cellpar_f
   integer, dimension(6)                              :: icell_f
   real                                               :: vol_f, dens_f, mu_f, rir_f, wavelen_f
   real, dimension(:), allocatable                    :: inte_f, inte_c_arr
   type(reflection_type), dimension(:), allocatable   :: reflc
   real                                               :: maxinte
   integer                                            :: isorg
   character(len=*), dimension(0:2), parameter        :: subf = &
       ['inorganic    ','organic      ','metallorganic']
   integer, parameter                                 :: NMAXREFLD = 500
   character(len=2), allocatable                      :: specie_label(:)
!
!  Convert null-terminated C string to Fortran string
   n = 0
   do while (cif_file(n+1) /= C_NULL_CHAR .and. n < 4096)
       n = n + 1
   enddo
   allocate(character(n) :: filename_f)
   do i = 1, n
       filename_f(i:i) = cif_file(i)
   enddo
!
!  Import crystal structure from CIF
   call import_structure(filename_f, crystal, err)
   if (err%signal) then
       ier_c = INT(-1, C_INT)
       return
   endif
!
!  Compute all crystal data
   call intensity_file_for_database(crystal, ier_f, nat_f, sform_f, cellpar_f, icell_f, &
                                     vol_f, dens_f, zval_f, mu_f, nrefl_f, rir_f,        &
                                     inte_f, wavelen_f)
   ier_c = INT(ier_f, C_INT)
   if (ier_f /= 0) return
   allocate(specie_label(crystal%numelem()))
   do i=1,crystal%numelem()
      specie_label(i) = specie_from_pxen(crystal%elem(i)%z)
   enddo
!
!  Fill element species output
   nelem_c = INT(min(crystal%numelem(), 100), C_INT)
   do i = 1, nelem_c
       call copy_string_to_c_array(trim(specie_label(i)), specie_label_c(:, i), 3)
   enddo
!
!  Fill scalar outputs
   nat_c      = INT(nat_f,      C_INT)
   cellpar_c  = REAL(cellpar_f, C_FLOAT)
   icell_c    = INT(icell_f,    C_INT)
   vol_c      = REAL(vol_f,     C_FLOAT)
   dens_c     = REAL(dens_f,    C_FLOAT)
   zval_c     = INT(zval_f,     C_INT)
   mu_c       = REAL(mu_f,      C_FLOAT)
   nrefl_c    = INT(nrefl_f,    C_INT)
   rir_c      = REAL(rir_f,     C_FLOAT)
   wavelen_c  = REAL(wavelen_f, C_FLOAT)
!
!  Fill formula string
   call copy_string_to_c_array(trim(sform_f), sform_c, size(sform_c))
!
!  Fill subfile string
   if (nat_f > 0) then
       isorg = is_organic(crystal%elem%z, nint(crystal%elem%nw))
       call copy_string_to_c_array(trim(subf(isorg)), subfile_c, size(subfile_c))
   else
       call copy_string_to_c_array('undefined', subfile_c, size(subfile_c))
   endif
!
!  Fill space group symbol
   call copy_string_to_c_array(trim(crystal%spg%symbol_xhm), spg_sym_c, size(spg_sym_c))
!
!  Build crystal system string (with rhombohedral/hexagonal disambiguation)
   if (crystal%spg%csys_code == CS_Trigonal) then
       if (uc_is_rhombohedral(cellpar_f, 0.01)) then
           crysys_f = trim(cry_sys(crystal%spg%csys_code))//' (rhombohedral axes)'
       elseif (uc_is_hexagonal(cellpar_f, 0.01)) then
           crysys_f = trim(cry_sys(crystal%spg%csys_code))//' (hexagonal axes)'
       else
           crysys_f = trim(cry_sys(crystal%spg%csys_code))
       endif
   else
       crysys_f = trim(cry_sys(crystal%spg%csys_code))
   endif
   call copy_string_to_c_array(crysys_f, crysys_c, size(crysys_c))
!
!  Process reflections: sort by intensity, filter I%>0 (same logic as write_reflections code=5)
   n = min(NMAXREFLD, nrefl_f)
   allocate(reflc(n),     source=crystal%ref(:n))
   allocate(inte_c_arr(n), source=inte_f(:n))
   maxinte = maxval(inte_c_arr)
   if (abs(maxinte) > epsilon(1.0)) then
       call sort_reflections(reflc, ORD_BY_INT, inte_c_arr)
       nrefl_print_f = count(inte_c_arr >= epsilon(1.0)*maxinte/10)
   else
       maxinte = 1.0
       nrefl_print_f = 0
   endif
   nrefl_print_c = INT(nrefl_print_f, C_INT)
!
!  Fill reflection output arrays
   do i = 1, nrefl_print_f
       refl_h(i)    = INT(reflc(i)%hkl(1),  C_INT)
       refl_k(i)    = INT(reflc(i)%hkl(2),  C_INT)
       refl_l(i)    = INT(reflc(i)%hkl(3),  C_INT)
       refl_tth(i)  = REAL(reflc(i)%tthd(1),  C_FLOAT)
       refl_d(i)    = REAL(dvalue(reflc(i)%tthd(1), wavelen_f), C_FLOAT)
       refl_mult(i) = INT(reflc(i)%m,          C_INT)
       refl_lp(i)   = REAL(reflc(i)%lp(1),     C_FLOAT)
       refl_fc2(i)  = REAL(reflc(i)%fc**2,      C_FLOAT)
       refl_inte(i) = REAL(inte_c_arr(i),        C_FLOAT)
       refl_ipct(i) = REAL(1000.0*(inte_c_arr(i)/maxinte), C_FLOAT)
   enddo
!
   end subroutine get_crystal_info_from_cif


end module import_data_struct
