MODULE centroids

   implicit none

   type centroid_type
     real, dimension(3)                 :: x   = 0.0    ! coordinates of centroid
     integer, dimension(:), allocatable :: at           ! pointers to atoms involved in the centroids
     integer                            :: nat = 0      ! number of atoms in the centroids

   contains
     procedure :: prn => print_centroid    
     procedure :: set => set_centroid
   end type centroid_type

CONTAINS

   subroutine find_centroids(atom,legm,centr,ncentr)
!
!  Find centroids from list of atoms
!
   USE progtype
   USE atom_type_util
   USE connect_mod
   USE atom_basic
   USE fileutil
   USE arrayutil
   type(atom_type), dimension(:), allocatable, intent(in)      :: atom
   type(bond_type), dimension(:), allocatable, intent(in)      :: legm
   type(centroid_type), dimension(:), allocatable, intent(out) :: centr
   integer, intent(out)                                        :: ncentr
   integer                                                     :: nring
   type(container_type), dimension(:), allocatable             :: ring
   integer                                                     :: i
   logical                                                     :: kpr = .false.
   type(file_handle) :: fpdb
!
   ncentr = 0
   if (numatoms(atom) == 0) return
   if (numbonds(legm) == 0) return
!
   call find_sssr(atom,legm,ring,nring)
   if (nring > 0) then
       if (kpr) then
           call fpdb%fopen('centroids.out','w')
           write(fpdb%handle(),'(1x,70("-")/,"            List of ring systems"/,1x,70("-"))')
           write(fpdb%handle(),'(1x,"   n            Atoms                                   Planarity"/,1x,70("-"))')
           do i=1,nring
              write(fpdb%handle(),'(i5,t10,a,t60,f10.2)')i,slabvet(ring(i)%pos,atom%lab),ring_planarity_estimate(atom,ring(i))
           enddo
           write(fpdb%handle(),'(//)')
       endif
       call reallocate_centr(centr,nring)
       ncentr = 0
       do i=1,nring
          if (ring_planarity(atom,ring(i))) then
              !write(0,'(a,*(i4))')'ring:',ring(i)%pos
              ncentr = ncentr + 1
              call centr(ncentr)%set(atom,ring(i)%pos)
          endif
       enddo
       call reallocate_centr(centr,ncentr)
       if (kpr) then
           call print_centroids(centr,atom,fpdb%handle())
           call fpdb%fclose()
       endif
   endif
!
   end subroutine find_centroids

!----------------------------------------------------------------------------------------------------

   subroutine set_centroid(centr,atom,pos) 
   USE arrayutil
   USE atom_type_util
   class(centroid_type), intent(inout)       :: centr
   type(atom_type), dimension(:), intent(in) :: atom
   integer, dimension(:), intent(in)         :: pos
!
   centr%x(:) = baricentro(atom(pos))
   centr%nat = size(pos)
   call new_array(centr%at,centr%nat)
   centr%at(:) = pos(:)
!
   end subroutine set_centroid

!----------------------------------------------------------------------------------------------------

   integer function numcentr(centr)
   type(centroid_type), dimension(:), allocatable :: centr
!   
   if (allocated(centr)) then
       numcentr = size(centr)
   else
       numcentr = 0
   endif
!
   end function numcentr

!----------------------------------------------------------------------------------------------------

   subroutine print_centroid(centr,atom,kpr)
   USE atom_type_util
   USE strutil
   class(centroid_type), intent(in)          :: centr
   type(atom_type), dimension(:), intent(in) :: atom
   integer, intent(in)                       :: kpr
!
   call write_svet(atom(centr%at)%lab,centr%nat,kpr)
!
   end subroutine print_centroid

!----------------------------------------------------------------------------------------------------

   subroutine print_centroids(centr,atom,kpr)
   USE atom_type_util
   type(centroid_type), dimension(:), allocatable, intent(in) :: centr
   type(atom_type), dimension(:), intent(in)                  :: atom
   integer, intent(in)                                        :: kpr
   integer                                                    :: ncentr,i
!
   ncentr = numcentr(centr)
   if (ncentr > 0) then
       write(kpr,'(1x,50("-")/,"            List of centroids"/,1x,50("-"))')
       write(kpr,'(1x,"   n            Atoms"/,1x,50("-"))')
       do i=1,numcentr(centr)
          write(kpr,'(i5,t10)',advance='no') i
          call centr(i)%prn(atom,kpr)
       enddo
   endif
!
   end subroutine print_centroids

!----------------------------------------------------------------------------------------------------

   subroutine reallocate_centr(centr,n,savevet)
!
!  Rialloca ad n un vettore reale.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(centroid_type), allocatable, intent(inout) :: centr(:)
   integer, intent(in)              :: n
   logical, optional, intent(in)    :: savevet
   logical                          :: savev
   integer                          :: nv
   type(centroid_type), allocatable                :: vsav(:)
   integer                          :: nsav
!
!  se n = 0 (riallocazione a 0): dealloca ed esci
   if (n == 0) then
       if (allocated(centr)) deallocate(centr)
       return
   endif
!
   if (.not.allocated(centr)) then
       allocate(centr(n))
   else
!
       nv = size(centr)
       if (present(savevet)) then
           savev = savevet
       else
           savev = .true.
       endif
!
       if (savev) then
!
!          nsav contiene qual e' la porzione di centr da salvare
           select case(nv-n)
             case (1:)       ! compatta x ad n
               nsav = n
             case (:-1)      ! espandi x ad n
               nsav = nv
             case (0)
               return        ! n=nv non fare niente
           end select
           allocate(vsav(n))
           vsav(:nsav) = centr(:nsav)
           call move_alloc(vsav,centr)
       else
           deallocate(centr)
           allocate(centr(n))
       endif
   endif
!
   end subroutine reallocate_centr
END MODULE centroids
