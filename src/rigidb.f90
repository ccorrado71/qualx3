module rigid_body

implicit none

type rigid_body_type
  integer, dimension(:), allocatable :: at
  integer                            :: nat  = 0
  integer, dimension(6)              :: rcod = 0
  real, dimension(3)                 :: tra,rot
  integer                            :: rat  = 0 ! rotation around rat, 0 for rotation around center of mass
  real, dimension(3)                 :: xc       ! coordinates of centre of rotation when rat != 0

contains

  procedure :: set => set_rigid_body

end type rigid_body_type

private copy_rigidb_s, copy_rigidb_v
interface copy_rigidb
   module procedure copy_rigidb_s, copy_rigidb_v
end interface 

contains 

   subroutine set_rigid_body(rigidb,at,nat,rcod,tra,rot,rat)
   use arrayutil
   class(rigid_body_type), intent(out) :: rigidb
   integer, dimension(:), intent(in)   :: at
   integer, intent(in)                 :: nat
   integer, dimension(6), intent(in)   :: rcod
   real, dimension(3), intent(in)      :: tra,rot
   integer, intent(in)                 :: rat
   if (nat == 0) return
   call new_array(rigidb%at,nat)
   rigidb%at = at(:nat)
   rigidb%nat = nat
   rigidb%rcod = rcod
   rigidb%tra = tra
   rigidb%rot = rot
   rigidb%rat = rat
   end subroutine set_rigid_body

!----------------------------------------------------------------------------------------------------   

   subroutine add_rigidb(rigidb,at,nat,is_tra,is_rot,tra,rot,rat)
   type(rigid_body_type), dimension(:), allocatable, intent(inout) :: rigidb
   integer, dimension(:), intent(in)                               :: at
   integer, intent(in)                                             :: nat
   logical, intent(in)                                             :: is_tra,is_rot
   real, intent(in)                                                :: tra,rot
   integer, intent(in)                                             :: rat
   type(rigid_body_type)                                           :: rb
   integer, dimension(6)                                           :: rcod
   if (nat <= 0) return
   if (size(at) < nat) return
!!!!!
   rcod(:) = 0
   if (is_tra) then
       if (tra > 0.0) then
           rcod(1:3) = 1
       endif
   endif
   if (is_rot) then
       if (rot > 0.0) then
           rcod(4:6) = 1
       endif
   endif
!!!!!
!   if (is_tra) then
!       rcod(1:3) = 1
!   else
!       rcod(1:3) = 0
!   endif
!   if (is_rot) then
!       rcod(4:6) = 1
!   else
!       rcod(4:6) = 0
!   endif
   call rb%set(at,nat,rcod,[tra,tra,tra],[rot,rot,rot],rat)
   if (.not.exist_rigidb(rigidb,rb)) then
       call push_back_rigidb(rigidb,rb)
   endif
   end subroutine add_rigidb

!----------------------------------------------------------------------------------------------------   

   subroutine print_rigidb(rigidb,atom,kpr)
   use atom_basic
   type(rigid_body_type), dimension(:), allocatable, intent(in) :: rigidb
   type(atom_type), dimension(:), allocatable, intent(in)       :: atom
   integer, intent(in)                                          :: kpr
   integer                                                      :: i
   if (numrigidb(rigidb) > 0) then
       write(kpr,'(/2x,80("-")/15x,"List of rigid bodies"/)')
       write(kpr,'(2x,a/2x,80("-"))')'   n.      Atoms'
       do i=1,numrigidb(rigidb)
          if (rigidb(i)%rat > 0) then
              write(kpr,'(1x,i5,10x,a,a)')i,slabvet(rigidb(i)%at,atom%lab),atom(rigidb(i)%rat)%glab()
          else
              write(kpr,'(1x,i5,10x,a)')i,slabvet(rigidb(i)%at,atom%lab)
          endif
       enddo
   endif
   end subroutine print_rigidb

!----------------------------------------------------------------------------------------------------   

   integer function numrigidb(rigidb)
   type(rigid_body_type), dimension(:), allocatable, intent(in) :: rigidb
!
   if (allocated(rigidb)) then
       numrigidb = size(rigidb)
   else
       numrigidb = 0
   endif
!
   end function numrigidb

!----------------------------------------------------------------------------------------------------   

   subroutine resize_rigidb(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo rigid_body_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(rigid_body_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                               :: n
   logical, optional, intent(in)                     :: savevet
   logical                                           :: savev
   integer                                           :: nv
   type(rigid_body_type), allocatable                :: vsav(:)
   integer                                           :: nsav
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
           deallocate(vetr)
           allocate(vetr(n))
       endif
   endif
!
   end subroutine resize_rigidb  

!----------------------------------------------------------------------------------------------------

   subroutine push_back_rigidb(arr,val)
!
!  Adds a new fragment at the end of the array
!
   type(rigid_body_type), dimension(:), allocatable, intent(inout) :: arr
   type(rigid_body_type), intent(in)                               :: val
   integer                                                       :: ndim
   ndim = numrigidb(arr)
   call resize_rigidb(arr,ndim+1)
   arr(ndim+1) = val
   end subroutine push_back_rigidb

!----------------------------------------------------------------------------------------------------

   subroutine new_rigidb(vetr,n)
!
!  Create new fragments
!
   type(rigid_body_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                             :: n

   if (n < 0) return
   if (numrigidb(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_rigidb

!----------------------------------------------------------------------------------------------------

   subroutine clear_rigidb(vetr)
!
!  Delete all atoms
!
   type(rigid_body_type), allocatable, intent(inout) :: vetr(:)

   if (allocated(vetr)) deallocate(vetr)

   end subroutine clear_rigidb

!----------------------------------------------------------------------------------------------------

   subroutine copy_rigidb_s(rigidb1,rigidb2)
!
!  Copy rigidb1 in rigidb2
!
   use arrayutil
   type(rigid_body_type), intent(in)  :: rigidb1
   type(rigid_body_type), intent(out) :: rigidb2
   integer                            :: nat
!
   nat = rigidb1%nat
   call new_array(rigidb2%at,nat)
   rigidb2 = rigidb1
   end subroutine copy_rigidb_s

!----------------------------------------------------------------------------------------------------

   subroutine copy_rigidb_v(rigidb1,rigidb2)
!
!  Array copy of rigidb1 in rigidb2
!
   type(rigid_body_type), allocatable, intent(in)    :: rigidb1(:)
   type(rigid_body_type), allocatable, intent(inout) :: rigidb2(:)
   integer                                           :: nrb1,nrb2,i
!
   nrb1 = numrigidb(rigidb1)
   nrb2 = numrigidb(rigidb2)
   if (nrb2 /= nrb1) call new_rigidb(rigidb2,nrb1)
   do i=1,nrb1
      call copy_rigidb_s(rigidb1(i),rigidb2(i))
   enddo
   end subroutine copy_rigidb_v

!----------------------------------------------------------------------------------------------------
 
   integer function find_rigidb(rigidb,at,vet) result(nb)
!
!  Find rigid bodies containing atom at
!
   use arrayutil
   type(rigid_body_type), allocatable, intent(in)    :: rigidb(:)
   integer, intent(in)                               :: at
   integer, dimension(:), allocatable, intent(inout) :: vet
   integer                                           :: i,nbtot
!
   nb = 0
   nbtot = numrigidb(rigidb)
   do i=1,nbtot
      if (any(rigidb(i)%at == at)) then
          if (size_array(vet) <= nb) call resize_array(vet,nbtot)
          nb=nb+1
          vet(nb) = i
      endif
   enddo
!
   end function find_rigidb
   
!----------------------------------------------------------------------------------------------------

   logical function equal_rigidb(rb1,rb2)
   use arrayutil
   type(rigid_body_type), intent(in)  :: rb1,rb2
!
   equal_rigidb = .false.
   if (rb1%nat /= rb2%nat) return
   if (rb1%nat == 0) then
       equal_rigidb = .true.
       return
   endif
!
   equal_rigidb = check_container(rb1%at,rb2%at)
!
   end function equal_rigidb

!----------------------------------------------------------------------------------------------------

   logical function exist_rigidb(rigidb,rb)
!
!  Check if rb is present in the array riggidb
!
   type(rigid_body_type), allocatable, intent(in) :: rigidb(:)
   type(rigid_body_type), intent(in)              :: rb
   integer                                        :: i
!
   exist_rigidb = .true.
   do i=1,numrigidb(rigidb)
      if (equal_rigidb(rigidb(i),rb)) return
   enddo
   exist_rigidb = .false.
!
   end function exist_rigidb

end module rigid_body
