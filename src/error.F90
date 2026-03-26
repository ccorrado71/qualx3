!#define NOGUI 1
MODULE errormod

implicit none

private

type, public :: error_type
   logical            :: signal  = .false.  ! severe error
   logical            :: warning = .false.  ! warning
   character(len=500) :: message = ' '      
   integer            :: code = 0           ! error code, 0 is undefined

contains 
   procedure :: reset     => reset_error
   procedure :: set       => set_error
   procedure :: setw      => set_warning
   procedure :: print     => print_error_message
   procedure :: add       => add_message
   procedure :: as_warn   => change_as_warning
   procedure :: as_severe => change_as_severe
   procedure :: msg       => get_message
end type error_type

public reallocate_err

CONTAINS

   subroutine reset_error(err)
   class(error_type), intent(inout) :: err
!
   err%signal = .false.
   err%warning = .false.
   err%message = ' '
!   
   end subroutine reset_error
   
!----------------------------------------------------------------------------------------------------   

   subroutine set_error(err,message,code)
   class(error_type), intent(inout)       :: err
   character(len=*), intent(in), optional :: message
   integer, intent(in), optional          :: code
!
   err%warning = .false.
   err%signal = .true.
   if (present(message)) then
       err%message = message
   else
       err%message = ' '
   endif
   if (present(code)) then
       err%code = code
   else
       err%code = 0
   endif
!   
   end subroutine set_error
   
!----------------------------------------------------------------------------------------------------   

   subroutine set_warning(err,message,code)
!
!  Segnala warning se non esiste gia' un errore severo
!
   class(error_type), intent(inout) :: err
   character(len=*), intent(in)     :: message
   integer, intent(in), optional    :: code
!
   if (.not. err%signal) then
       err%warning = .true.
       err%message = message
       if (present(code)) then
           err%code = code
       else
           err%code = 0
       endif
   endif
!   
   end subroutine set_warning

!----------------------------------------------------------------------------------------------------   

   subroutine change_as_warning(err)
!
!  Change severe error in warning
!
   class(error_type), intent(inout) :: err
   if (err%signal) then
       err%signal = .false.
       err%warning = .true.
   endif
!
   end subroutine change_as_warning

!----------------------------------------------------------------------------------------------------   

   subroutine change_as_severe(err)
!
!  Change severe error in warning
!
   class(error_type), intent(inout) :: err
   if (err%warning) then
       err%signal = .true.
       err%warning = .false.
   endif
!
   end subroutine change_as_severe

!----------------------------------------------------------------------------------------------------   

   subroutine add_message(err,message)
   class(error_type), intent(inout) :: err
   character(len=*), intent(in)     :: message
   err%message = trim(err%message)//trim(message)
   end subroutine add_message

!----------------------------------------------------------------------------------------------------   

   function get_message(err)
   class(error_type), intent(in) :: err
   character(len=:), allocatable :: get_message
   get_message = trim(err%message)
   end function get_message

!----------------------------------------------------------------------------------------------------   

   subroutine print_error_message(err)
#if NOGUI
   USE iso_fortran_env
#else
   USE prog_constants
#endif
   class(error_type), intent(in) :: err
!   integer, optional             :: parent
   integer                       :: nExitCode
   character(len=10)             :: errtype
!
   if (err%warning) then
       errtype = 'WARNING'
   else
       errtype = 'ERROR'
   endif
#if NOGUI
   write(ERROR_UNIT,'(a)')trim(err%message)
#else
   call MsgWinErr(errtype,err%message,WARN_WINDOW,nExitCode)
#endif
!   
   end subroutine
   
!----------------------------------------------------------------------------------------------------   

   subroutine reallocate_err(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo error_type.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(error_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                          :: n
   logical, optional, intent(in)                :: savevet
   logical                                      :: savev
   integer                                      :: nv
   type(error_type), allocatable                :: vsav(:)
   integer                                      :: nsav
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
!          nsav contiene qual è la porzione di vetr da salvare
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
           deallocate(vetr)
           allocate(vetr(n))
       endif
   endif
!
   end subroutine reallocate_err

END MODULE errormod
