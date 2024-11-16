module molgraph

implicit none

integer, parameter, private                   :: INFNUM = huge(1)  ! represents positive infinity 
integer, dimension(:,:), private, allocatable :: dmat              ! distance matrix
integer, dimension(:,:), private, allocatable :: pth               ! array used to reconstruct shortest path

contains

   subroutine compute_dist_matrix(atom,bond,path)
!
!  Compute the distance matrix using the Floyd-Warshall algorithm 
!
   USE atom_type_util
   USE connect_mod
   USE prnutil
   type(atom_type), dimension(:), intent(in)              :: atom
   type(bond_type), dimension(:), allocatable, intent(in) :: bond
   logical, intent(in)                                    :: path ! true if you want recunstruct the complete path
   integer                                                :: nat
   integer                                                :: i,j,k
   !integer, dimension(:), allocatable                     :: vpath
!
   nat = size(atom)
   if (nat == 0) return
!
!  initialize dmat 
   if (allocated(dmat)) then 
       if (size(dmat,1) /= nat) then
           deallocate(dmat)
           allocate(dmat(nat,nat))
       endif
   else
       allocate(dmat(nat,nat))
   endif
!
   do i=1,nat
      do j=1,nat
         if (bond_position(bond,i,j) > 0) then
             dmat(i,j) = 1
         else
             dmat(i,j) = INFNUM
         endif
      enddo
      dmat(i,i) = 0
   enddo
!
   if (path) then
!
!      Initialize pth
       if (allocated(pth)) then
           if (size(pth,1) /= nat) then
               deallocate(pth)
               allocate(pth(nat,nat))
           endif
       else
           allocate(pth(nat,nat))  
       endif
       pth(:,:) = 0
!
       if (numbonds(bond) > 0) then
           do k=1,nat
              do i=1,nat
                 do j=1,nat
                    if (dmat(i,k) == INFNUM .or. dmat(k,j) == INFNUM) cycle
                    if (dmat(i,k) + dmat(k,j) < dmat(i,j)) then
                        dmat(i,j) = dmat(i,k) + dmat(k,j)
                        pth(i,j) = k
                    endif
                 enddo
              enddo
           enddo
       endif
   else
       if (numbonds(bond) == 0) then
           do k=1,nat
              do i=1,nat
                 do j=1,nat
                    if (dmat(i,k) == INFNUM .or. dmat(k,j) == INFNUM) cycle
                    if (dmat(i,k) + dmat(k,j) < dmat(i,j)) then
                        dmat(i,j) = dmat(i,k) + dmat(k,j)
                    endif
                 enddo
              enddo
           enddo
       endif
   endif
   !do i=1,nat
   !   do j=1,nat
   !      write(0,*)'DMAT=',i,j,path_length(i,j)
   !   enddo
   !enddo
   !call print_matrix(dmat,0)
   !allocate(vpath(nat))
   !do i=1,nat-1
   !   do j=i+1,nat
   !      !nv = 0
   !      call get_path(i,j,vpath,nv)
   !      if (nv == -1) then
   !          write(0,'(a,i0,a,i0)')'Path not found from ',i,' to ', j
   !      else
   !          write(0,'(a,i0,a,i0,a,*(i4))')'Path from ',i,' to ',j,':',vpath(:nv)
   !      endif
   !   enddo
   !enddo
!
   end subroutine compute_dist_matrix

!----------------------------------------------------------------------------------------------------

   subroutine free_dist_matrix()
   if (allocated(dmat)) deallocate(dmat)
   if (allocated(pth)) deallocate(pth)
   end subroutine free_dist_matrix 

!----------------------------------------------------------------------------------------------------

   integer function path_length(i,j)  result(plen)
!
!  Lenght of the shortest path that connects vertices i and j
!
   integer, intent(in) :: i,j
!
   plen = 0
   if (.not.allocated(dmat)) return
   if (dmat(i,j) /= INFNUM) plen = dmat(i,j)
   end function path_length

!----------------------------------------------------------------------------------------------------

   subroutine get_path(i,j,vpath,nv)
!
!  Recunstruct the shortest path from i to j using dmat and pth
!
   integer, intent(in)                :: i,j
   integer, dimension(:), intent(out) :: vpath
   integer, intent(out)               :: nv   ! nv = -1 if no path was found
!
   nv = 0
   if (.not.allocated(pth)) return
   if (i == j) return
   call get_pathr(i,j,vpath,nv)
!
   end subroutine get_path

!----------------------------------------------------------------------------------------------------

   recursive subroutine get_pathr(i,j,vpath,nv)
   integer, intent(in) :: i,j
   integer, dimension(:), intent(inout) :: vpath
   integer, intent(inout)               :: nv
   integer                              :: tmp
!
   if (dmat(i,j) == INFNUM) then
       nv = -1 ! No path
   else
       tmp = pth(i,j)
       if (tmp /= 0) then   ! if tmp = 0 there is an edge from i to j
           call get_pathr(i,tmp,vpath,nv)
           nv = nv + 1
           vpath(nv) = tmp
           call get_pathr(tmp,j,vpath,nv)
       endif
   endif
!
   end subroutine get_pathr

end module molgraph
