module FOMmod

implicit none

type ass_type
     integer :: ps = 0
     integer :: pd = 0
end type ass_type

contains

   subroutine computeFOM(tth,tsize,cfomd)  bind(C,name="computeFOM")
   use variables, only: dataset
   use peak_mod, only: pkind, numpeaks
   use, intrinsic :: iso_c_binding, only: c_double, c_int
   use arrayutil, only: clocate
   real(c_double), dimension(*), intent(in) :: tth     ! 2theta array for the card
   integer(c_int), intent(in), value        :: tsize   ! size of the 2theta array
   real(c_double), intent(out)              :: cfomd
   type(ass_type), allocatable              :: ass(:)
   real                                     :: fomd
   real                                     :: delta = 0.25
   real, dimension(:), allocatable          :: xd, Id
   real                                     :: tthmin, tthmax
   real                                     :: diffss
   integer                                  :: nd,ns
   integer                                  :: i, natot, nlm
   integer, allocatable                     :: pkas(:), pkad(:)
!
   cfomd = 0
!
!  Define the experimental range
   tthmin = dataset(1)%tmin - delta
   tthmax = dataset(1)%tmax + delta
!
!  Count number of peaks in the range
   nd = count((tth(:tsize) >= tthmin .and. tth(:tsize) <= tthmax))
   if (nd == 0) return
   allocate(xd(nd), Id(nd), ass(nd), pkad(nd))
!
!  Fill the xd and Id arrays with the peaks in the range
   nd = 0
   do i=1, tsize
      if (tth(i) >= tthmin .and. tth(i) <= tthmax) then
          nd = nd + 1
          xd(nd) = real(tth(i))
          Id(nd) = 1.0     ! FIX later
      endif
   enddo
!
!  Allocate array to size of experimental peaks (ns)
   ns = numpeaks(pkind)
   allocate(pkas(ns))
!
!  Match one or more experimental peaks to the database reflection/peak.
   natot = 0
   fomd = 0.0
   pkas(:) = 0
   pkad(:) = 0
   do i=1,nd
      nlm = clocate(pkind%getx(),xd(i))
      diffss = abs(pkind(nlm)%getx() - xd(i))
      if (diffss < delta) then
          natot = natot + 1
          fomd = fomd + diffss
          ass(natot) = ass_type(nlm,i)
          pkad(i) = nlm
          pkas(nlm) = i
      endif
   enddo
!
   if (natot > 0) then
       fomd = 1.0 - fomd/(natot*delta)
       cfomd = real(fomd, c_double)
   endif
   !write(0,*)'FOMD = ',fomd,natot,nd
!
   end subroutine computeFOM

end module FOMmod
