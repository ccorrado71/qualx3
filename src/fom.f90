module FOMmod

implicit none

contains

   subroutine computeFOM(tth,tsize,cfomd)  bind(C,name="computeFOM")
   use variables, only: dataset
   use peak_mod, only: pkind
   use, intrinsic :: iso_c_binding, only: c_double, c_int
   use arrayutil, only: clocate
   real(c_double), dimension(*), intent(in) :: tth     ! 2theta array for the card
   integer(c_int), intent(in), value        :: tsize   ! size of the 2theta array
   real(c_double), intent(out)              :: cfomd
   real                                     :: fomd
   real                                     :: delta = 0.25
   real, dimension(:), allocatable          :: xd, Id
   real                                     :: tthmin, tthmax
   real                                     :: diffss
   integer                                  :: nd
   integer                                  :: i, natot, nlm
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
   allocate(xd(nd), Id(nd))
!
!  Fill the xd and Id arrays with the peaks in the range
   nd = 0
   do i=1, tsize
      if (tth(i) >= tthmin .and. tth(i) <= tthmax) then
          nd = nd + 1
          xd(nd) = tth(i)
          Id(nd) = 1.0     ! FIX later
      endif
   enddo
!
   !write(0,*)'xd=',xd
   natot = 0
   fomd = 0.0
   do i=1,nd
      nlm = clocate(pkind%getx(),xd(i))
      diffss = abs(pkind(nlm)%getx() - xd(i))
      if (diffss < delta) then
          natot = natot + 1
          fomd = fomd + diffss
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
