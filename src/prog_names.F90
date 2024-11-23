module prognames
 
   implicit none 

   character(len=:), allocatable, protected :: package_name       ! e.g. expo
   character(len=:), allocatable, protected :: package_alt_name   ! e.g. expo2.0
   character(len=:), allocatable, protected :: version            ! e.g. 2.0.0

   interface
      subroutine get_app_name_and_version(app_name,len_name,app_version,len_version) &
                 bind(C, name="get_app_name_and_version")
      use iso_c_binding, only: c_char, c_int
      character(kind=c_char), intent(in) :: app_name(*)
      integer(c_int), intent(in)         :: len_name
      character(kind=c_char), intent(in) :: app_version(*)
      integer(c_int), intent(in)         :: len_version
      end subroutine get_app_name_and_version
   end interface

contains

   subroutine get_app_name()
   use iso_c_binding, only: c_char, c_int
   use strutil
   character(kind=c_char), dimension(20) :: app_name, app_version
   integer(c_int)                        :: len_name, len_version
   integer                               :: pos

   call get_app_name_and_version(app_name,len_name,app_version,len_version)
!
   package_name = toFortranString(app_name,len_name)
   version = toFortranString(app_version,len_version)
   pos = index(version,'.',back=.true.)
   if (pos > 1) then
       package_alt_name = package_name//version(1:pos-1)
   else
       package_alt_name = package_name
   endif
!
   end subroutine get_app_name

end module prognames
