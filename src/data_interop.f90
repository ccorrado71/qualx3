module data_interop

   use iso_c_binding
   implicit none

contains

! Returns sizes needed to pre-allocate C arrays before calling get_diffraction_data.
   subroutine get_diffraction_data_size(ndata, nyb, nwave) bind(C, name='get_diffraction_data_size')
   USE variables, only: dataset
   integer(c_int), intent(out) :: ndata, nyb, nwave

   if (allocated(dataset) .and. size(dataset) > 0) then
       ndata = int(dataset(1)%npoints(), c_int)
       nwave = int(dataset(1)%nwave, c_int)
       if (dataset(1)%has_back()) then
           nyb = int(size(dataset(1)%yb), c_int)
       else
           nyb = 0_c_int
       endif
   else
       ndata = 0_c_int
       nyb   = 0_c_int
       nwave = 0_c_int
   endif

   end subroutine get_diffraction_data_size

! ---------------------------------------------------------------------------

! Fills pre-allocated C arrays with x, y, yb, wave, ratio, radtype and status flags.
! Caller must pre-allocate each array to the size returned by get_diffraction_data_size.
! nyb=0 and/or nwave=0 are allowed; corresponding pointers are not accessed.
   subroutine get_diffraction_data(x, y, yb, nyb, has_back_c, back_sub_c, &
                                   wave_c, ratio_c, nwave, radtype_c) &
              bind(C, name='get_diffraction_data')
   USE variables, only: dataset
   integer(c_int),  value,        intent(in)  :: nyb, nwave
   real(c_float),   dimension(*), intent(out) :: x, y, yb, wave_c, ratio_c
   logical(c_bool),               intent(out) :: has_back_c, back_sub_c
   integer(c_int),                intent(out) :: radtype_c
   integer :: n

   n = dataset(1)%npoints()
   x(:n) = dataset(1)%x(:n)
   y(:n) = dataset(1)%y(:n)
   if (nyb   > 0) yb(:nyb)        = dataset(1)%yb(:nyb)
   if (nwave > 0) wave_c(:nwave)  = dataset(1)%wave(:nwave)
   if (nwave > 0) ratio_c(:nwave) = dataset(1)%ratio(:nwave)
   has_back_c = logical(dataset(1)%has_back(),      c_bool)
   back_sub_c = logical(dataset(1)%back_subtracted, c_bool)
   radtype_c  = int(dataset(1)%radtype, c_int)

   end subroutine get_diffraction_data

! ---------------------------------------------------------------------------

! Receives diffraction data from C++ after loading a project.
   subroutine set_diffraction_data(x, y, ndata, yb, nyb, has_back_c, back_sub_c, &
                                   wave_c, ratio_c, nwave, radtype_c, &
                                   filename, filename_len) &
              bind(C, name='set_diffraction_data')
   USE strutil
   USE datasetmod
   USE molcom, only: jscreen
   USE variables, only: dataset
   USE view
   integer(c_int),          value,        intent(in) :: ndata, nyb, nwave
   real(c_float),           dimension(*), intent(in) :: x, y, yb, wave_c, ratio_c
   logical(c_bool),         value,        intent(in) :: has_back_c, back_sub_c
   integer(c_int),          value,        intent(in) :: radtype_c
   character(kind=c_char),               intent(in) :: filename(*)
   integer(c_int),          value,        intent(in) :: filename_len
   type(dataset_type)                               :: xpdataset
   character(len=:), allocatable                    :: filenam

   filenam = toFortranString(filename, filename_len)

   xpdataset%radtype = radtype_c
   call xpdataset%set_wave(nwave, wave_c(:nwave), ratio_c(:nwave))
   call xpdataset%set_data(x(1:ndata), y(1:ndata), ndata, filenam, POW_DATA)
   if (has_back_c) call xpdataset%set_background(yb(1:nyb))
   xpdataset%back_subtracted = back_sub_c

   call clear_dataset(dataset)
   call push_back_dataset(dataset, xpdataset)
   if (jscreen > 0) then
       call vedinew(8)
   endif

   end subroutine set_diffraction_data

! ---------------------------------------------------------------------------

! Returns lightweight metadata about the current dataset for the report.
! wave and ratio arrays must be pre-allocated by the caller (max 4 elements).
   subroutine get_dataset_info(filename, filename_len, npoints, &
                                tmin, tmax, tstep,              &
                                back_subtracted, alpha2_subtracted, &
                                radtype, nwave, wave, ratio)    &
              bind(C, name='get_dataset_info')
   USE strutil
   USE variables, only: dataset
   integer(C_INT), value,        intent(in)  :: filename_len
   character(kind=C_CHAR),       intent(out) :: filename(*)
   integer(C_INT),               intent(out) :: npoints, radtype, nwave
   real(C_FLOAT),                intent(out) :: tmin, tmax, tstep
   logical(C_BOOL),              intent(out) :: back_subtracted, alpha2_subtracted
   real(C_FLOAT),                intent(out) :: wave(*), ratio(*)
   integer :: n, i

   if (.not. allocated(dataset) .or. size(dataset) == 0) then
       call copy_string_to_c_array('', filename, filename_len)
       npoints           = 0_C_INT
       tmin              = 0.0_C_FLOAT
       tmax              = 0.0_C_FLOAT
       tstep             = 0.0_C_FLOAT
       back_subtracted   = logical(.false., C_BOOL)
       alpha2_subtracted = logical(.false., C_BOOL)
       radtype           = 0_C_INT
       nwave             = 0_C_INT
       return
   end if

   n = dataset(1)%npoints()
   call copy_string_to_c_array(trim(dataset(1)%fname), filename, filename_len)
   npoints           = int(n,                             C_INT)
   tmin              = real(dataset(1)%tmin,              C_FLOAT)
   tmax              = real(dataset(1)%tmax,              C_FLOAT)
   tstep             = real(dataset(1)%tstep,             C_FLOAT)
   back_subtracted   = logical(dataset(1)%back_subtracted,   C_BOOL)
   alpha2_subtracted = logical(dataset(1)%alpha2_subtracted, C_BOOL)
   radtype           = int(dataset(1)%radtype,            C_INT)
   nwave             = int(dataset(1)%nwave,              C_INT)
   do i = 1, dataset(1)%nwave
       wave(i)  = real(dataset(1)%wave(i),  C_FLOAT)
       ratio(i) = real(dataset(1)%ratio(i), C_FLOAT)
   end do

   end subroutine get_dataset_info

end module data_interop
