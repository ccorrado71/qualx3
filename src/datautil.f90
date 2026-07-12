module datautil

implicit none

private :: combine_dataset_t, combine_dataset_f
interface combine_dataset
  module procedure combine_dataset_t, combine_dataset_f
end interface

contains

   subroutine load_datafile(this,filename,gui,ier,filetyp,sync)
!
!  Load dataset from external file
!
   USE prog_constants
   USE filereading
   USE fileutil
   USE counts
   USE elements, only: RX_SOURCE,NEUTRON_SOURCE
   USE datasetmod
   interface
     subroutine openwave(radtype, nwave, wave, ratio) bind(C,name="openwave")
     use iso_c_binding
     integer(c_int)              :: radtype, nwave
     real(c_float), dimension(*) :: wave, ratio
     end subroutine openwave
   end interface
   class(dataset_type), intent(inout)          :: this
   character(len=*), intent(in)                :: filename
   logical, intent(in)                         :: gui
   integer, intent(out)                        :: ier
   integer, intent(in), optional               :: filetyp
   logical, intent(in), optional               :: sync
   integer                                     :: count_type
   integer                                     :: ftype
   real                                        :: thmino, thstepo, thmaxo   !, wavelen
   integer                                     :: ncount
   type(point_type), allocatable, dimension(:) :: countp
   type(error_type)                            :: error
   integer                                     :: irad,nwave
   real, dimension(3)                          :: wave,ratio
!
   if (present(filetyp)) then
       ftype = filetyp
   else
       ftype = filetype_from_filename(filename)
   endif
   call read_datafile(filename,ftype,thmino,    &
        thstepo,thmaxo,countp,ncount,.false.,nwave,wave,ratio,count_type,error)
   if (error%signal) then
       call error%print()
       ier = 1
   else
       ier = 0
       this%radtype = RX_SOURCE
       if (present(sync)) then
           this%sync = sync
       else
           this%sync = .false.
       endif
       this%tstep = thstepo
       this%tmin = thmino
       this%tmax = thmaxo
!
!      Set wavelength 
       if (this%nwave == 0) then     ! wavelenght is not still defined
           if (nwave == 0) then      ! wavelength is not in data file
               wave(1:2) = [DEF_WAVE,DEF_WAVE2]
               ratio(1:2) = [1.0,0.5]
               if (gui) then
                   call openwave(irad,nwave,wave,ratio)
                   select case (irad)
                     case (2)
                       this%sync = .true.
                     case (3)
                       this%radtype = NEUTRON_SOURCE
                   end select
               else
                   nwave = 1
               endif
           endif
           call this%set_wave(nwave,wave,ratio)
       endif
!
       if (count_type == COUNT_D_TYPE) countp = thvalue(countp,this%wave(1))
       call this%set_data(countp(:)%x,countp(:)%y,size(countp),filename,POW_DATA)
   endif
   end subroutine load_datafile

!-----------------------------------------------------------------

   subroutine combine_dataset_t(dataset,datasum,err)
   use datasetmod
   use arrayutil
   use nrutil
   use nr
   use filereading
   use errormod
   type(dataset_type), dimension(:), allocatable, intent(in) :: dataset
   type(dataset_type), intent(out)                           :: datasum
   type(error_type), intent(out)                             :: err
   real                                                      :: xmin,xmax,tstep
   real, dimension(:), allocatable                           :: xc,yc,yd2,scal
   integer                                                   :: i,j,nc,ndset,xpmin,xpmax
!
   if (ndataset(dataset) < 2) go to 10
!
   ndset = 0
   do i=1,ndataset(dataset)
      if (dataset(i)%npoints() > 0) ndset = ndset + 1
   enddo
   if (ndset < 2) go to 10
!
!  Define range
   xmin = tiny(1.0)
   xmax = huge(1.0)
   do i=1,ndataset(dataset)
      if (dataset(i)%npoints() == 0) cycle
      if (dataset(i)%tmin > xmin) then
          xmin = dataset(i)%tmin
      endif
      if (dataset(i)%tmax < xmax) then
          xmax = dataset(i)%tmax
      endif
   enddo
!
!  Define step
   tstep = huge(1.0)
   do i=1,ndataset(dataset)
      if (dataset(i)%npoints() == 0) cycle
      if (dataset(i)%tstep < tstep) tstep = dataset(i)%tstep
   enddo
!
!  Make arrays and fill x
   nc = floor((xmax - xmin) / tstep + 1)
   call new_array(xc,nc)
   xc = arth(xmin,tstep,nc)
   xmax = xc(nc)
   call new_array(yc,nc)
   yc(:) = 0
   allocate(scal(ndataset(dataset)))
!
!  Fill y
   scal = 1.0  !could be improved ...
   do i=1,ndataset(dataset)
      if (dataset(i)%npoints() == 0) cycle
      xpmin = clocate(dataset(i)%x,xmin)
      xpmax = clocate(dataset(i)%x,xmax)
      call new_array(yd2,xpmax-xpmin+1)
      call spline(dataset(i)%x(xpmin:xpmax),dataset(i)%y(xpmin:xpmax),0.0,0.0,yd2)
      do j=1,nc
         yc(j) = yc(j) + scal(i)*splint(dataset(i)%x(xpmin:xpmax),dataset(i)%y(xpmin:xpmax),yd2,xc(j))
      enddo
   enddo
!
!  Make dataset
   datasum%radtype = dataset(1)%radtype
   datasum%sync = dataset(1)%sync
   call datasum%set_data(xc,yc,nc,"generated_data.xy",POW_DATA)
   datasum%tstep = get_tstep(datasum%x)
   call datasum%set_wave(dataset(1)%nwave,dataset(1)%wave,dataset(1)%ratio)
   
   return

10 call err%set("Error combining datasets")
!
   end subroutine combine_dataset_t

!-----------------------------------------------------------------

   subroutine combine_dataset_f(dataset,file_name,err)
   use errormod
   use datasetmod
   use filereading
   type(dataset_type), dimension(:), allocatable, intent(in) :: dataset
   character(len=*), intent(in)                              :: file_name
   type(error_type), intent(out)                             :: err
   type(dataset_type)                                        :: datasum
!
   call combine_dataset_t(dataset,datasum,err)
   if (.not.err%signal) & 
       call export_profile(file_name,datasum%x,datasum%y,string='#      2theta    yoss')
!
   end subroutine combine_dataset_f

end module datautil
