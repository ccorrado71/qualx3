module messagemod

implicit none

private

public :: set_status_message

interface
   subroutine c_set_status_message(msg) bind(C, name='c_set_status_message')
      use iso_c_binding, only: c_char
      character(kind=c_char), dimension(*), intent(in) :: msg
   end subroutine c_set_status_message
end interface

contains

   subroutine set_status_message(msg)
   use iso_c_binding, only: c_null_char
   character(len=*), intent(in) :: msg
!
   call c_set_status_message(trim(msg)//c_null_char)
!
   end subroutine set_status_message

end module messagemod
