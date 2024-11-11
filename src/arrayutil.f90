MODULE arrayutil

implicit none

 type container_type
   integer, dimension(:), allocatable :: pos 
   integer  :: nat = 0                      
 end type container_type

 private size_array_i, size_array_r, size_array_s, size_array_ss, size_array_l, size_array_c
 interface size_array
   module procedure size_array_i, size_array_r, size_array_s, size_array_ss, size_array_l,  &
                    size_array_c, size_array_rr
 end interface

 private :: new_array_i, new_array_ii, new_array_iii, new_array_iiii, &
            new_array_r, new_array_rr, new_array_rrr, new_array_s, new_array_ss, new_array_l
 interface new_array
   module procedure new_array_i, new_array_ii, new_array_iii, new_array_iiii,      &
                    new_array_r, new_array_rr, new_array_rrr, new_array_s, new_array_ss, new_array_l
 end interface

 private :: delete_array_i, delete_array_r, delete_array_s, delete_array_ss
 interface delete_array
   module procedure delete_array_i, delete_array_r, delete_array_s, delete_array_ss
 end interface

 private :: resize_array_i, resize_array_r, resize_array_d, resize_array_s, resize_array_l
 interface resize_array
   module procedure resize_array_i, resize_array_r, resize_array_d, resize_array_s, resize_array_l
 end interface

 private :: push_back_array_i
 interface push_back_array
   module procedure push_back_array_i
 end interface

 private :: copy_reals, copy_integers
 interface copy_array
   module procedure copy_reals, copy_integers
 end interface

 private :: sort_array_i, sort_array_r
 interface sort_array
   module procedure sort_array_i, sort_array_r
 end interface 

 private ::  container_set_s, container_set_v
 interface container_set
   module procedure ::  container_set_s, container_set_v
 end interface 

 private :: container_update_remove_s, container_update_remove_v
 interface container_update_remove
   module procedure :: container_update_remove_s, container_update_remove_v
 end interface 

 integer, parameter :: CONT_DELETED=-999

!F size_array(arr)                         Size of array
!S new_array(arr,n)                        Create new array
!S delete_array(arr)                       Deallocate an array
!S resize_array(arr,n)                     Resize an array saving content
!S push_back_array_i(arr,val)              Adds a new element at the end of the array
!S copy_array(arr1,arr2)                   Copy an array
!F clocate(xv,x)                           Cerca la locazione del punto del vettore xv piu' vicino a x
!F clocate1(xv,x)                          Locate position of a number in an unordered array
!F check_container(vet1,vet2)              okcont=.true. se tutto vet2 è contenuto in vet1
!F ncheck_container(vet1,vet2,n)           okcont=.true. if vet2 is all in vet1. n is the number of elements of vet2 in vet1
!S remove_duplicate(arr,arru)              Remove duplicate numbers from an array
!S append_array(arr1,arr2)                 Append arr2 to arr1 
!S insert_array(arr1,arr2,pos)             Insert array arr2 after pos
!S append_merge_array(arr1,arr2)           Append arr2 to arr1 avoiding the duplicate elements
!S sort_array(arr)                         Sort an array
!S resize_container(vetr,n,savevet)        Resize container
!S new_container(vetr,n)                   Create new container
!S container_set(conn,n,vet)               Set value for container_type
!S container_update_remove_s(conn,vrem)    Update connect for removing atoms in vrem
!S get_containers(kel,cont,vcont,nvcont)   Get pointer to all containers with element kel
!F logical_to_int(l)/int_to_logical(ival)  Convert logical/int

CONTAINS


   integer function size_array_i(vet) result(sizev)
   integer, dimension(:), allocatable :: vet
   if (allocated(vet)) then
       sizev = size(vet)
   else
       sizev = 0
   endif
   end function size_array_i

!--------------------------------------------------------------------------------------------------

   integer function size_array_r(vet) result(sizev)
   real, dimension(:), allocatable :: vet
   if (allocated(vet)) then
       sizev = size(vet)
   else
       sizev = 0
   endif
   end function size_array_r

!--------------------------------------------------------------------------------------------------

   integer function size_array_rr(vet,kdim) result(sizev)
   real, dimension(:,:), allocatable :: vet
   integer, intent(in)               :: kdim
   if (allocated(vet)) then
       sizev = size(vet,kdim)
   else
       sizev = 0
   endif
   end function size_array_rr

!--------------------------------------------------------------------------------------------------

   integer function size_array_s(vet) result(sizev)
   character(len=*), dimension(:), allocatable :: vet
   if (allocated(vet)) then
       sizev = size(vet)
   else
       sizev = 0
   endif
   end function size_array_s

!--------------------------------------------------------------------------------------------------

   integer function size_array_ss(vet,kdim) result(sizev)
   character(len=*), dimension(:,:), allocatable :: vet
   integer, intent(in)                           :: kdim
   if (allocated(vet)) then
       sizev = size(vet,kdim)
   else
       sizev = 0
   endif
   end function size_array_ss

!--------------------------------------------------------------------------------------------------

   integer function size_array_l(vet) result(sizev)
   logical, dimension(:), allocatable :: vet
   if (allocated(vet)) then
       sizev = size(vet)
   else
       sizev = 0
   endif
   end function size_array_l

!----------------------------------------------------------------------------------------------------

   integer function size_array_c(vet)
   type(container_type), dimension(:), allocatable, intent(in) :: vet
!
   if (allocated(vet)) then
       size_array_c = size(vet)
   else
       size_array_c = 0
   endif
!
   end function size_array_c

!----------------------------------------------------------------------------------------------------

   subroutine new_array_r(arr,n,val)
!
!  Create new array of integers
!
   real, dimension(:), allocatable, intent(inout) :: arr
   integer, intent(in)                            :: n
   real, optional, intent(in)                     :: val

   if (n < 0) return
   if (size_array(arr) /= n) then
       if (allocated(arr))deallocate(arr)
       if (n > 0) then
           if (present(val)) then
               allocate(arr(n),source=val)
           else
               allocate(arr(n))
           endif
       endif
   endif

   end subroutine new_array_r

!----------------------------------------------------------------------------------------------------

   subroutine new_array_rr(arr,lb,ub)
!
!  Create new array of integers
!
   real, dimension(:,:), allocatable, intent(inout) :: arr
   integer, intent(in)                              :: ub(2),lb(2)

   if (allocated(arr)) then
       if (all(ub-lb+1 == 0)) then
           deallocate(arr)
           return
       endif
       if (any(ubound(arr) /= ub) .or. any(lbound(arr) /= lb)) then
           deallocate(arr)
           allocate(arr(lb(1):ub(1),lb(2):ub(2)))
       endif
   else
       if (all(ub-lb+1 > 0)) allocate(arr(lb(1):ub(1),lb(2):ub(2)))
   endif

   end subroutine new_array_rr

!----------------------------------------------------------------------------------------------------

   subroutine new_array_rrr(arr,lb,ub)
!
!  Create new array of integers
!
   real, dimension(:,:,:), allocatable, intent(inout) :: arr
   integer, intent(in)                                :: ub(3),lb(3)

   if (allocated(arr)) then
       if (all(ub-lb+1 == 0)) then
           deallocate(arr)
           return
       endif
       if (any(ubound(arr) /= ub) .or. any(lbound(arr) /= lb)) then
           deallocate(arr)
           allocate(arr(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))
       endif
   else
       if (all(ub-lb+1 > 0)) allocate(arr(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))
   endif

   end subroutine new_array_rrr

!corr   subroutine new_array_rr(arr,n,m)
!corr!
!corr!  Create new array of integers
!corr!
!corr   real, dimension(:,:), allocatable, intent(inout) :: arr
!corr   integer, intent(in)                              :: n,m
!corr
!corr   if (n < 0 .or. m < 0) return
!corr   if (allocated(arr)) then
!corr       if (n == 0 .or. m == 0) then
!corr           deallocate(arr)
!corr           return
!corr       endif
!corr       if (size(arr,1) /= n .or. size(arr,2) /= m) then
!corr           deallocate(arr)
!corr           allocate(arr(n,m))
!corr       endif
!corr   else
!corr       if (n /= 0 .and. m /= 0) allocate(arr(n,m))
!corr   endif
!corr
!corr   end subroutine new_array_rr

!----------------------------------------------------------------------------------------------------

   subroutine new_array_i(arr,n,ival)
!
!  Create new array of integers
!
   integer, dimension(:), allocatable, intent(inout) :: arr
   integer, intent(in)                               :: n
   integer, intent(in), optional                     :: ival

   if (n < 0) return
   if (size_array(arr) /= n) then
       if (allocated(arr))deallocate(arr)
       if (n > 0) then
           if (present(ival)) then
               allocate(arr(n),source=ival)
           else
               allocate(arr(n))
           endif
       endif
   endif

   end subroutine new_array_i

!----------------------------------------------------------------------------------------------------

   subroutine new_array_ii(arr,lb,ub)
!
!  Create new array of integers
!
   integer, dimension(:,:), allocatable, intent(inout) :: arr
   integer, intent(in)                                 :: ub(2),lb(2)

   if (allocated(arr)) then
       if (all(ub-lb+1 == 0)) then
           deallocate(arr)
           return
       endif
       if (any(ubound(arr) /= ub) .or. any(lbound(arr) /= lb)) then
           deallocate(arr)
           allocate(arr(lb(1):ub(1),lb(2):ub(2)))
       endif
   else
       if (all(ub-lb+1 > 0)) allocate(arr(lb(1):ub(1),lb(2):ub(2)))
   endif

   end subroutine new_array_ii

!----------------------------------------------------------------------------------------------------

   subroutine new_array_iii(arr,lb,ub)
!
!  Create new array of integers
!
   integer, dimension(:,:,:), allocatable, intent(inout) :: arr
   integer, intent(in)                                   :: ub(3),lb(3)

   if (allocated(arr)) then
       if (all(ub-lb+1 == 0)) then
           deallocate(arr)
           return
       endif
       if (any(ubound(arr) /= ub) .or. any(lbound(arr) /= lb)) then
           deallocate(arr)
           allocate(arr(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))
       endif
   else
       if (all(ub-lb+1 > 0)) allocate(arr(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))
   endif

   end subroutine new_array_iii

!----------------------------------------------------------------------------------------------------

   subroutine new_array_iiii(arr,lb,ub)
!
!  Create new array of integers
!
   integer, dimension(:,:,:,:), allocatable, intent(inout) :: arr
   integer, intent(in)                                     :: ub(4),lb(4)

   if (allocated(arr)) then
       if (all(ub-lb+1 == 0)) then
           deallocate(arr)
           return
       endif
       if (any(ubound(arr) /= ub) .or. any(lbound(arr) /= lb)) then
           deallocate(arr)
           allocate(arr(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)))
       endif
   else
       if (all(ub-lb+1 > 0)) allocate(arr(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)))
   endif

   end subroutine new_array_iiii

!----------------------------------------------------------------------------------------------------

   subroutine new_array_s(arr,n,nlen)
!
!  Create new array of integers
!
   integer, intent(in)                                           :: nlen
   character(len=nlen), dimension(:), allocatable, intent(inout) :: arr
   integer, intent(in)                                           :: n

   if (n < 0) return
   if (size_array(arr) /= n) then
       if (allocated(arr))deallocate(arr)
       if (n > 0) allocate(arr(n))
   endif

   end subroutine new_array_s

!----------------------------------------------------------------------------------------------------

   subroutine new_array_ss(arr,n,m,nlen)
!
!  Create new array of integers
!
   integer, intent(in)                                             :: nlen
   character(len=nlen), dimension(:,:), allocatable, intent(inout) :: arr
   integer, intent(in)                                             :: n,m

   if (n < 0 .or. m < 0) return
   if (size_array(arr,1) /= n .and. size_array(arr,2) /= m) then
       if (allocated(arr))deallocate(arr)
       if (n > 0 .and. m > 0) allocate(arr(n,m))
   endif

   end subroutine new_array_ss

!----------------------------------------------------------------------------------------------------

   subroutine new_array_l(arr,n)
!
!  Create new array of logicals
!
   logical, dimension(:), allocatable, intent(inout) :: arr
   integer, intent(in)                               :: n

   if (n < 0) return
   if (size_array(arr) /= n) then
       if (allocated(arr))deallocate(arr)
       if (n > 0) allocate(arr(n))
   endif

   end subroutine new_array_l

!----------------------------------------------------------------------------------------------------

   subroutine delete_array_r(arr)
!
!  Delete array
!
   real, allocatable, intent(inout) :: arr(:)

   if (allocated(arr)) deallocate(arr)

   end subroutine delete_array_r

!----------------------------------------------------------------------------------------------------

   subroutine delete_array_i(arr)
!
!  Delete array
!
   integer, allocatable, intent(inout) :: arr(:)

   if (allocated(arr)) deallocate(arr)

   end subroutine delete_array_i

!----------------------------------------------------------------------------------------------------

   subroutine delete_array_s(arr)
!
!  Delete array
!
   character(len=*), allocatable, intent(inout) :: arr(:)

   if (allocated(arr)) deallocate(arr)

   end subroutine delete_array_s

!----------------------------------------------------------------------------------------------------

   subroutine delete_array_ss(arr)
!
!  Delete array
!
   character(len=*), allocatable, intent(inout) :: arr(:,:)

   if (allocated(arr)) deallocate(arr)

   end subroutine delete_array_ss
 
!----------------------------------------------------------------------------------------------------

   subroutine resize_array_r(vetr,n,savevet)
!
!  Rialloca ad n un vettore reale.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   real, allocatable, intent(inout) :: vetr(:)
   integer, intent(in)              :: n
   logical, optional, intent(in)    :: savevet
   logical                          :: savev
   integer                          :: nv
   real, allocatable                :: vsav(:)
   integer                          :: nsav
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
   end subroutine resize_array_r

!----------------------------------------------------------------------------------------------------

   subroutine resize_array_rr(arr,lb,ub)
!
!  Rialloca ad n un vettore reale.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   real, dimension(:,:), allocatable, intent(inout) :: arr
   integer, intent(in)                              :: ub(2),lb(2)
   integer                                          :: i
   real, dimension(:,:), allocatable                :: vsav
   integer, dimension(2)                            :: ubarr,lbarr
   integer, dimension(2)                            :: ubsav,lbsav
!
!  se n = 0 (riallocazione a 0): dealloca ed esci
   if (all(ub-lb+1 == 0)) then
       if (allocated(arr)) deallocate(arr)
       return
   endif
!
   if (.not.allocated(arr)) then
       allocate(arr(lb(1):ub(1),lb(2):ub(2)))
   else
       lbarr = lbound(arr)
       ubarr = ubound(arr)
       if (all(lb <= ubarr) .and. all(ub >= lbarr)) then
           ubsav = 0
           lbsav = 0
           do i=1,2
              if (ub(i) > ubarr(i)) then
                  ubsav(i) = ubarr(i)
              else
                  ubsav(i) = ub(i)
              endif
              if (lb(i) < lbarr(i)) then
                  lbsav(i) = lbarr(i)
              else
                  lbsav(i) = lb(i)
              endif
           enddo
           allocate(vsav(lb(1):ub(1),lb(2):ub(2)))
           vsav(lbsav(1):ubsav(1),lbsav(2):ubsav(2)) = arr(lbsav(1):ubsav(1),lbsav(2):ubsav(2))
           call move_alloc(vsav,arr)
       else
           deallocate(arr)
           allocate(arr(lb(1):ub(1),lb(2):ub(2)))
       endif
   endif
!
   end subroutine resize_array_rr

!--------------------------------------------------------------------------------------------------

   subroutine resize_array_i(vetr,n,savevet)
!
!  Rialloca ad n un vettore reale.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   integer, allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                 :: n
   logical, optional, intent(in)       :: savevet
   logical                             :: savev
   integer                             :: nv
   integer, allocatable                :: vsav(:)
   integer                             :: nsav
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
   end subroutine resize_array_i

!--------------------------------------------------------------------------------------------------

   subroutine resize_array_d(vetr,n,savevet)
!
!  Rialloca ad n un vettore in doppia precisione
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   USE type_constants, only: DP
   real(DP), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                  :: n
   logical, optional, intent(in)        :: savevet
   logical                              :: savev
   integer                              :: nv
   real(DP), allocatable                :: vsav(:)
   integer                              :: nsav
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
   end subroutine resize_array_d

!--------------------------------------------------------------------------------------------------

   subroutine resize_array_s(vetr,n,nlen,savevet)
!
!  Rialloca ad n un vettore reale.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
!mr   character(len=nlen), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                 :: n
   integer, intent(in)                 :: nlen
   logical, optional, intent(in)       :: savevet
   logical                             :: savev
   integer                             :: nv
   character(len=nlen), allocatable, intent(inout) :: vetr(:)
   character(len=nlen), allocatable                :: vsav(:)
   integer                             :: nsav
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
   end subroutine resize_array_s

!----------------------------------------------------------------------------------------------------

   subroutine resize_array_l(vetr,n,savevet)
!
!  Rialloca ad n un vettore logical.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   logical, allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                 :: n
   logical, optional, intent(in)       :: savevet
   logical                             :: savev
   integer                             :: nv
   logical, allocatable                :: vsav(:)
   integer                             :: nsav
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
   end subroutine resize_array_l

!----------------------------------------------------------------------------------------------------

   subroutine push_back_array_i(arr,val)
!
!  Adds a new element at the end of the array
!
   integer, allocatable, intent(inout) :: arr(:)
   integer, intent(in)                 :: val
   integer                             :: ndim
   ndim = size_array(arr)
   call resize_array(arr,ndim+1)
   arr(ndim+1) = val
   end subroutine push_back_array_i

!----------------------------------------------------------------------------------------------------

   subroutine copy_reals(vet1,vet2)
!
!  Copia vet2 in vet1
!
   real, dimension(:), allocatable, intent(inout) :: vet1
   real, dimension(:), allocatable, intent(in)    :: vet2
   integer                                        :: dim2,dim1
!
   dim2 = size_array(vet2)
   dim1 = size_array(vet1)
   if (dim1 == dim2) then
       if (dim1 /= 0) vet1 = vet2
   else
       if (allocated(vet1)) deallocate(vet1)
       if (dim2 > 0) then
           allocate(vet1(dim2),source=vet2)
       endif
   endif
!
   end subroutine copy_reals

!----------------------------------------------------------------------------------------------------

   subroutine copy_integers(vet1,vet2)
!
!  Copia vet2 in vet1
!
   integer, dimension(:), allocatable, intent(inout) :: vet1
   integer, dimension(:), allocatable, intent(in)    :: vet2
   integer                                        :: dim2,dim1
!
   dim2 = size_array(vet2)
   dim1 = size_array(vet1)
   if (dim1 == dim2) then
       if (dim1 /= 0) vet1 = vet2
   else
       if (allocated(vet1)) deallocate(vet1)
       if (dim2 > 0) then
           allocate(vet1(dim2),source=vet2)
       endif
   endif
!
   end subroutine copy_integers

!----------------------------------------------------------------------------------------------------

   subroutine copy_container(vet1,vet2)
!
!  Copia vet2 in vet1
!
   type(container_type), dimension(:), allocatable, intent(inout) :: vet1
   type(container_type), dimension(:), allocatable, intent(in)    :: vet2
   integer                                                        :: dim2,dim1
!
   dim2 = size_array(vet2)
   dim1 = size_array(vet1)
   if (dim1 == dim2) then
       if (dim1 /= 0) vet1 = vet2
   else
       if (allocated(vet1)) deallocate(vet1)
       if (dim2 > 0) then
           allocate(vet1(dim2),source=vet2)
       endif
   endif
!
   end subroutine copy_container

!----------------------------------------------------------------------------------------------------

   integer function clocate(xv,x)
!
!  Cerca la locazione del punto del vettore xv piu' vicino a x
!
   real, dimension(:), intent(in) :: xv
   real              , intent(in) :: x
   integer                        :: n
   integer                        :: jl,ju,jm
!
   n = size(xv)
   if (x <= xv(1)) then
       clocate=1
   else if (x >= xv(n)) then
       clocate=n
   else
       jl = 1  !!!0
       ju = n  !!!!n + 1
       do
         if (ju-jl <= 1) exit
         jm = (ju+jl)/2
         if (x >= xv(jm)) then
             jl = jm
         else
             ju = jm
         endif
       enddo
       if (x-xv(jl) < xv(ju)-x) then
           clocate=jl
       else
           clocate=jl+1
       endif
   endif
!
   end function clocate

! -------------------------------------------------------------------------

   integer function clocate1(xv,x)  result(loc)
!
!  Locate position of a number in an unordered array
!
   integer, dimension(:), intent(in) :: xv
   integer, intent(in)               :: x
   integer                           :: i
!
   loc = 0
   do i=1,size(xv)
      if (xv(i) == x) then
          loc = i
          exit
      endif
   enddo
!
   end function clocate1

! -------------------------------------------------------------------------

   logical function check_container(vet1,vet2)  result(okcont)
!
!  okcont=.true. se tutto vet2 è contenuto in vet1
!
   integer, dimension(:), intent(in) :: vet1,vet2
   integer                           :: i
   integer                           :: nv1,nv2
!
   nv1 = size(vet1)
   nv2 = size(vet2)
   if (nv2 > nv1) then
       okcont = .false.
   else
       okcont = .true.
       do i=1,size(vet2)
          if (.not.any(vet1 == vet2(i))) then
              okcont = .false.
              exit
          endif
       enddo
   endif
!
   end function check_container

! -------------------------------------------------------------------------

   logical function ncheck_container(vet1,vet2,n)  result(okcont)
!
!  okcont=.true. if vet2 is all in vet1. n is the number of elements of vet2 in vet1
!
   integer, dimension(:), intent(in) :: vet1,vet2
   integer, intent(out)              :: n
   integer                           :: i
   integer                           :: nv1,nv2
!
   nv1 = size(vet1)
   nv2 = size(vet2)
   n = 0
   do i=1,size(vet2)
      if (any(vet1 == vet2(i))) then
          n = n + 1
      endif
   enddo
   okcont = n == size(vet2)
!
   end function ncheck_container

! -------------------------------------------------------------------------

   subroutine remove_duplicate(arr,arru)
!
!  Remove duplicate numbers from an array
!
   integer, dimension(:), intent(in)                 :: arr   ! array with duplicates
   integer, dimension(:), allocatable, intent(inout) :: arru  ! array with unique elements
   integer                                           :: i, nu

   call new_array(arru,size(arr))
   arru(1) = arr(1)
   nu = 1
   do i=2,size(arr)
      if (.not.any(arru(:nu) == arr(i))) then
          nu = nu + 1
          arru(nu) = arr(i)
      endif
   enddo
   call resize_array(arru,nu)
   end subroutine remove_duplicate

! -------------------------------------------------------------------------

   subroutine append_array(arr1,arr2)
!
!  Append arr2 to arr1 
!
   integer, dimension(:), allocatable, intent(inout) :: arr1 
   integer, dimension(:), intent(in)                 :: arr2
   integer                                           :: size1
!
   size1 = size_array(arr1)
   call resize_array(arr1,size1+size(arr2))
   arr1(size1+1:) = arr2
!
   end subroutine append_array

! -------------------------------------------------------------------------

   subroutine insert_array(arr1,arr2,pos)
!
!  Insert array arr2 after pos
!
   integer, dimension(:), allocatable, intent(inout) :: arr1 
   integer, dimension(:), intent(in)                 :: arr2
   integer, intent(in)                               :: pos
   integer, parameter                                :: NPLUS=3 !000
!
   if (pos+size(arr2) > size_array(arr1)) then
       call resize_array(arr1,size_array(arr1)+NPLUS)
   endif
   arr1(pos+1:pos+size(arr2)) = arr2(:)
!
   end subroutine insert_array

! -------------------------------------------------------------------------

   subroutine append_merge_array(arr1,arr2)
!
!  Append arr2 to arr1 avoiding the duplicate elements
!
   integer, dimension(:), allocatable, intent(inout) :: arr1 
   integer, dimension(:), intent(in)                 :: arr2
   integer                                           :: i,size1,newsize
!
   size1 = size_array(arr1)
   newsize = size1
   call resize_array(arr1, size1 + size(arr2))
   do i=1,size(arr2)
      if (any(arr1(:size1) == arr2(i))) cycle
      newsize = newsize + 1
      arr1(newsize) = arr2(i)
   enddo
   call resize_array(arr1,newsize)
!
   end subroutine append_merge_array

!--------------------------------------------------------------------------------------------------

   subroutine sort_array_i(arr,desc)
   USE nr
   integer, dimension(:), intent(inout) :: arr
   logical, intent(in), optional        :: desc
   integer, dimension(size(arr))        :: ind
   call indexx(arr,ind)
   if (present(desc)) then
       if (desc) then
           ind = ind(size(arr):1:-1)
           arr = arr(ind)
       endif
   endif
   arr = arr(ind)
   end subroutine sort_array_i

!--------------------------------------------------------------------------------------------------

   subroutine sort_array_r(arr,desc)
   USE nr
   real, dimension(:), intent(inout) :: arr
   logical, intent(in), optional     :: desc
   integer, dimension(size(arr))     :: ind
   call indexx(arr,ind)
   if (present(desc)) then
       if (desc) then
           ind = ind(size(arr):1:-1)
           arr = arr(ind)
       endif
   endif
   arr = arr(ind)
   end subroutine sort_array_r

!-------------------------------------------------------------------------------------------------

   subroutine resize_container(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo container_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(container_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                               :: n
   logical, optional, intent(in)                     :: savevet
   logical                                           :: savev
   integer                                           :: nv
   type(container_type), allocatable                :: vsav(:)
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
!          nsav contiene qual � la porzione di vetr da salvare
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
           !if (nv /= n) then   ! non conviene se c'e' ulteriore vettore allocabile all'interno
               deallocate(vetr)
               allocate(vetr(n))
           !endif
       endif
   endif
!
   end subroutine resize_container

!-------------------------------------------------------------------------------------------------

   subroutine new_container(vetr,n)
!
!  Create new container
!
   type(container_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                              :: n

   if (n < 0) return
   if (size_array(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif
   if (n > 0) vetr%nat = 0

   end subroutine new_container

!----------------------------------------------------------------------------------------------------

   subroutine container_set_s(cont,val)
!
!  Set value for container_type
!
   type(container_type), intent(inout) :: cont
   integer, intent(in)                 :: val
   integer                             :: sizec
!
   cont%nat = cont%nat + 1
   sizec = size_array(cont%pos)
   if (sizec < cont%nat) then
       call resize_array(cont%pos,sizec+10)
   endif
   cont%pos(cont%nat) = val
!
   end subroutine container_set_s 

!----------------------------------------------------------------------------------------------------

   subroutine container_set_v(conn,n,vet)
!
!  Set value for container_type
!
   type(container_type), intent(inout) :: conn
   integer, intent(in)                 :: n
   integer, dimension(:), intent(in)   :: vet
   integer                             :: sizec
!
   if (n > 0) then
       if (allocated(conn%pos)) then
           sizec = conn%nat + n
           call resize_array(conn%pos,sizec)
           conn%pos(conn%nat+1:) = vet(:n)
           conn%nat = sizec
       else
           allocate(conn%pos(n),source=vet(:n))
           conn%nat = n
       endif
   endif
!
   end subroutine container_set_v

!-------------------------------------------------------------------------------------------------

   subroutine container_update_remove_s(conn,vrem)
!
!  Update connect for removing atoms in vrem. conn%pos is not reallocated
!
   type(container_type), intent(inout) :: conn
   integer, intent(in)                 :: vrem
   integer                             :: j
   integer                             :: nconn
!
   nconn = 0
   do j=1,conn%nat
      if (conn%pos(j) /= vrem) then
          nconn = nconn + 1
          conn%pos(nconn) = conn%pos(j)
      endif
   enddo
   conn%nat = nconn
!
   end subroutine container_update_remove_s

!-------------------------------------------------------------------------------------------------

   subroutine container_update_remove_v(conn,vrem)
!
!  Update container for removing element in vrem. conn%pos is not reallocated
!
   type(container_type), dimension(:), intent(inout) :: conn
   integer, dimension(:), intent(in)                 :: vrem
   integer                                           :: i,j
   integer                                           :: nconn
!
   do i=1,size(conn)
      nconn = 0
      do j=1,conn(i)%nat
         nconn = nconn + 1
         if (any(vrem(:) == conn(i)%pos(j))) then
             conn(i)%pos(j) = 0
             nconn = nconn - 1
         endif
      enddo
      if (nconn < conn(i)%nat) then
          nconn = 0
          do j=1,conn(i)%nat
             if (conn(i)%pos(j) /= 0) then
                 nconn = nconn + 1
                 conn(i)%pos(nconn) = conn(i)%pos(j)
             endif
          enddo
          conn(i)%nat = nconn
      endif
   enddo
!
   end subroutine container_update_remove_v

!-------------------------------------------------------------------------------------------------

   subroutine get_containers(kel,cont,vcont,nvcont)
!
!  Get pointer to all containers with element kel
!
   integer, intent(in)                             :: kel
   type(container_type), dimension(:), allocatable :: cont
   integer, dimension(:), allocatable, intent(out) :: vcont
   integer, intent(out)                            :: nvcont
   integer                                         :: i,nc
!
   nvcont = 0
   nc = size_array(cont)
   if (nc > 0) then
       call new_array(vcont,nc)
       do i=1,nc
          if (cont(i)%nat > 0) then
              if (any(cont(i)%pos == kel)) then
                  nvcont = nvcont + 1
                  vcont(nvcont) = i
              endif
          endif
       enddo
       call resize_array(vcont,nvcont)
   endif
!
   end subroutine get_containers

!-------------------------------------------------------------------------------------------------

   subroutine cont_set_as_deleted(cont)
   type(container_type) :: cont
   cont%nat = CONT_DELETED
   end subroutine cont_set_as_deleted

!-------------------------------------------------------------------------------------------------

   subroutine delete_container(cont,vdel)
   type(container_type), dimension(:), allocatable :: cont
   integer, dimension(:), intent(in), optional     :: vdel
   integer                                         :: nc
!
   if (size_array(cont) == 0) return
   if (present(vdel)) cont(vdel)%nat = CONT_DELETED
   nc = count(cont%nat /= CONT_DELETED)
   cont(:nc) = pack(cont,mask=cont%nat /= CONT_DELETED)
   call resize_container(cont,nc)
!
   end subroutine delete_container

!-------------------------------------------------------------------------------------------------

   integer elemental function logical_to_int(l)
   logical, intent(in) :: l
   if (l) then
      logical_to_int = 1
   else
      logical_to_int = 0
   endif
   end function logical_to_int

!-------------------------------------------------------------------------------------------------

   logical elemental function int_to_logical(ival)
   integer, intent(in) :: ival
   int_to_logical = (ival /= 0)
   end function int_to_logical

END MODULE arrayutil

module pointmod

implicit none

type ipoint_type
   integer  :: x,y
end type

type point_type
   real     :: x,y
end type


interface add_points
    module procedure add_point, add_pointv
end interface

contains

   subroutine resize_points_mm(points,minp,maxp)
!
!  Ridimensiona un vettore di tipo point alla dimensione tra tmin e tmax
!
   USE arrayutil
   type(point_type), allocatable, dimension(:), intent(inout) :: points
   real, intent(in), optional                                 :: minp,maxp
   integer                                                    :: ipmin,ipmax
   integer                                                    :: newdim,olddim
!
   olddim = size(points)
   if (present(minp)) then
       ipmin = clocate(points%x,minp)                  ! locazione del punto + vicino
       if (points(ipmin)%x < minp) ipmin = ipmin + 1   ! ma non deve essere < di minp
   else
       ipmin = 1
   endif
   if (present(maxp)) then
       ipmax = clocate(points%x,maxp)                ! locazione del punto + vicino
       if (points(ipmax)%x > maxp) ipmax = ipmax - 1 ! ma non deve essere > di maxp
   else
       ipmax = olddim
   endif
   newdim = ipmax - ipmin + 1
   if (newdim > 0 .and. newdim /= olddim) then
       points(1:newdim) = points(ipmin:ipmax)
       call resize_points(points,newdim)
   endif
!
   end subroutine resize_points_mm

! -------------------------------------------------------------------------

   subroutine add_point(points,dati,iposi)
!
!  Aggiunge punto alla fine o in posizione ipos
!
   !USE progtype
   type(point_type), allocatable, dimension(:), intent(inout) :: points
   !type(point_type), allocatable, dimension(:) :: pointsc
   type(point_type), intent(in)                               :: dati
   integer, intent(in), optional                              :: iposi
   integer                                                    :: ipos
   integer                                                    :: np
   !integer                                                    :: iperr
!
   np = size(points)
!
!  Definisci la posizione in cui inserire il punto
   !iperr = 0
   if (present(iposi)) then
       if (iposi > 0 .and. iposi <= np) then
           ipos = iposi
           call resize_points(points,np+1)
           !call resize_points(pointsc,np-ipos+1)
           !pointsc = points(ipos:np)
           !points(ipos+1:np+1) = points(ipos:np)  ! problema con ifort
           !points(ipos+1:) = pointsc(:)
           !points(ipos) = dati
           points(ipos:) = [dati,points(ipos:np)]
       endif
   else
       ipos = np+1
       call resize_points(points,np+1)
       points(ipos) = dati
   endif
!
   end subroutine add_point

! -------------------------------------------------------------------------

   subroutine add_pointv(points,dati,iposi)
!
!  Aggiunge punto alla fine o in posizione ipos
!
   !USE progtype
   type(point_type), allocatable, dimension(:), intent(inout) :: points
   type(point_type), allocatable, dimension(:)                :: pointsc
   type(point_type), dimension(:), intent(in)                 :: dati
   integer, intent(in), optional                              :: iposi
   integer                                                    :: ipos
   integer                                                    :: np,nd
   integer                                                    :: iperr
   integer                                                    :: i
!
   np = size(points)
   nd = size(dati)
!
!  Definisci la posizione in cui inserire il punto
   iperr = 0
   if (present(iposi)) then
       if (iposi > 0 .and. iposi <= np) then
           ipos = iposi
           call resize_points(points,np+nd)
           call resize_points(pointsc,np-ipos+1)
           pointsc = points(ipos:np)
           !points(ipos+1:np+1) = points(ipos:np)  ! problema con ifort
           points(ipos+nd:) = pointsc(:)
       else
           iperr = 1   ! iposi non ha un valore ragionevole
       endif
   else
       ipos = np+1
       call resize_points(points,np+nd)
   endif
!
   if (iperr == 0) then
       do i=1,nd
          points(ipos+i-1) = dati(i)
       enddo
   endif
!
   end subroutine add_pointv

! -------------------------------------------------------------------------

   integer function numpoints(points)
   type(point_type), allocatable, dimension(:), intent(in) :: points
   if (allocated(points)) then
       numpoints = size(points)
   else
       numpoints = 0
   endif
   end function numpoints

! -------------------------------------------------------------------------

   subroutine del_point(points,ipos)
   type(point_type), allocatable, dimension(:), intent(inout) :: points
   integer, intent(in)                                        :: ipos
   integer                                                    :: np
!
!corr   np = size(points)
   np = numpoints(points)
   points(ipos:np-1) = points(ipos+1:np)
   call resize_points(points,np-1)
!
   end subroutine del_point

! -------------------------------------------------------------------------

   subroutine copy_points(point1,point2)
   type(point_type), dimension(:), allocatable, intent(inout) :: point1
   type(point_type), dimension(:), allocatable, intent(in)    :: point2
   integer                                                   :: npoint2,npoint1
!
   npoint2 = numpoints(point2)
   npoint1 = numpoints(point1)
   if (npoint1 == npoint2) then
       if (npoint1 /= 0) point1 = point2
   else
       if (allocated(point1)) deallocate(point1)
       if (npoint2 > 0) then
           allocate(point1(npoint2),source=point2)
       endif
   endif
   end subroutine copy_points

!----------------------------------------------------------------------------------------------------

   subroutine new_points(arr,n)
!
!  Create new array of integers
!
   type(point_type), dimension(:), allocatable, intent(inout) :: arr
   integer, intent(in)                                        :: n

   if (n < 0) return
   if (numpoints(arr) /= n) then
       if (allocated(arr))deallocate(arr)
       if (n /= 0) allocate(arr(n))
   endif

   end subroutine new_points

!--------------------------------------------------------------------------------------------------

   subroutine resize_points(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo model
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(point_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                          :: n
   logical, optional, intent(in)                :: savevet
   logical                                      :: savev
   integer                                      :: nv
   type(point_type), allocatable                :: vsav(:)
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
   end subroutine resize_points

end module pointmod
