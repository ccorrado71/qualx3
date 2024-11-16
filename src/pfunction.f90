module profile_function
   use progtype, only: rparam_type
   implicit none

   enum, bind(C)
     enumerator :: PEARSON7, PVOIG, TCHZ
   endenum
   !integer, parameter, public :: PEARSON7 = 1
   !integer, parameter, public :: PVOIG    = 2
   !integer, parameter, public :: TCHZ     = 3

   type :: profile_function_t
     integer                          :: ftype
     type(rparam_type), dimension(10) :: par
   end type profile_function_t

   type :: profinfo_type
     integer                         :: iniz,ifin
     real, dimension(:), allocatable :: func
     real                            :: fconst
   end type profinfo_type

   type :: pd_phase_info
     real, dimension(:,:,:), allocatable :: pvet
   end type pd_phase_info

   type(pd_phase_info), dimension(:), allocatable   :: pdph

contains

   subroutine pd_phase_init(numphase)
!
!  Allocate array pdph to number of crystal phases
!
   integer, intent(in) :: numphase
!
   if (allocated(pdph)) then
       if (numphase == size(pdph)) return
       deallocate(pdph)
   endif
   allocate(pdph(numphase))
   end subroutine pd_phase_init

!-------------------------------------------------------------------

   subroutine pd_ref_init(nph,nref,nwave)
!
!  Allocate array pdph for phase nph to number of reflections,wave
!
   use arrayutil
   integer, intent(in) :: nph
   integer, intent(in) :: nref
   integer, intent(in) :: nwave
!
   call new_array(pdph(nph)%pvet,[1,1,1],[nref,nwave,6])
!
   end subroutine pd_ref_init

!----------------------------------------------------------------------------------------------------

   integer function numprofilef(pf)
   type(profile_function_t), dimension(:), allocatable, intent(in) :: pf
!   
   if (allocated(pf)) then
       numprofilef = size(pf)
   else
       numprofilef = 0
   endif
!
   end function numprofilef

!----------------------------------------------------------------------------------------------------

   subroutine new_profilef(vetr,n)
!
!  Create new profile functions
!
   type(profile_function_t), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                                  :: n

   if (n < 0) return
   if (numprofilef(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_profilef

!----------------------------------------------------------------------------------------------------

   subroutine resize_profilef(vetr,n,savevet)
!
!  Resize array of profile functions
!
   type(profile_function_t), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n
   logical, optional, intent(in)               :: savevet
   logical                                     :: savev
   integer                                     :: nv
   type(profile_function_t), allocatable                :: vsav(:)
   integer                                     :: nsav
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
!          nsav contiene qual e' la porzione di vetr da salvare
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
           if (nv /= n) then
               deallocate(vetr)
               allocate(vetr(n))
           endif
       endif
   endif
!
   end subroutine resize_profilef

end module profile_function
