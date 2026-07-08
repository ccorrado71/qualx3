module FOMmod

implicit none

type ass_type
     integer :: ps = 0
     integer :: pd = 0
end type ass_type

! Work buffers for computeFOM, kept at module scope and reused across calls
! to avoid allocate/deallocate on every candidate card during a search
! (computeFOM is called once per candidate; the search loop is single-threaded).
! Buffers only grow, never shrink, so every access below must be bounded
! explicitly by nd/exp_size rather than by size(buffer) or "(:)".
real, dimension(:), allocatable    :: xd_buf, Id_buf, Idmult_buf, exp_tth_r_buf
type(ass_type), allocatable        :: ass_buf(:)
integer, dimension(:), allocatable :: pkas_buf, pkad_buf
logical, dimension(:), allocatable :: iass_buf
integer :: cap_nd  = 0
integer :: cap_exp = 0

contains

   subroutine computeFOM(tth,intensity,tsize,cfom_tot,w2thetad,w_intensity,w_phases,delta2theta, &
                         fompeakpos_out,fomintensity_out,scale_out, &
                         exp_tth,exp_intensity,exp_size)  bind(C,name="computeFOM")
   use variables, only: dataset
!corr   use peak_mod, only: pkind, numpeaks
   use, intrinsic :: iso_c_binding, only: c_double, c_int
   use arrayutil, only: clocate
   real(c_double), dimension(*), intent(in) :: tth           ! 2theta array for the card
   real(c_double), dimension(*), intent(in) :: intensity     ! intensity array for the card
   integer(c_int), intent(in), value        :: tsize         ! size of the card arrays
   real(c_double), intent(out)              :: cfom_tot
   real(c_double), intent(in), value        :: w2thetad
   real(c_double), intent(in), value        :: w_intensity
   real(c_double), intent(in), value        :: w_phases
   real(c_double), intent(in), value        :: delta2theta
   real(c_double), intent(out)              :: fompeakpos_out
   real(c_double), intent(out)              :: fomintensity_out
   real(c_double), intent(out)              :: scale_out
   real(c_double), dimension(*), intent(in) :: exp_tth       ! 2theta of experimental peaks
   real(c_double), dimension(*), intent(in) :: exp_intensity ! intensity of experimental peaks
   integer(c_int), intent(in), value        :: exp_size      ! number of experimental peaks
   real                                     :: fomd, fomI, fomdb, foms, fom_tot
   real                                     :: tthmin, tthmax
   real                                     :: diffss, sumId2, sumIoId, Idiff
   integer                                  :: nd
   integer                                  :: i, j, kd, natot, nlm, nassm, icl
   real                                     :: bscale
   integer                                  :: nda, nsa, ndan, ndmin
   integer                                  :: ntph, nts
   real                                     :: sumsa, sumda, sumdtot, sumstot, SQRTDS, SQRTDA
   integer                                  :: numass
!
!  External variables to add
   !real                                     :: delta2theta = 0.25
   !real                                     :: w_phases = 0.5
   !real                                     :: w2thetad = 0.5
   !real                                     :: w_intensity = 0.5
!  Input variables to add
   logical :: kscal = .true.
   real    :: crd_scale = 1.0
!  Fom value have to be in output
!
   fom_tot = 0
!
!  Define the experimental range
   tthmin = dataset(1)%tmin - delta2theta
   tthmax = dataset(1)%tmax + delta2theta
!
!  Count number of peaks in the range
   nd = count((tth(:tsize) >= tthmin .and. tth(:tsize) <= tthmax))
   if (nd == 0) return
   if (nd > cap_nd) then
       if (allocated(xd_buf)) deallocate(xd_buf, Id_buf, ass_buf, pkad_buf)
       allocate(xd_buf(nd), Id_buf(nd), ass_buf(nd), pkad_buf(nd))
       cap_nd = nd
   endif
!
!  Fill the xd and Id arrays with the peaks in the range
   nd = 0
   do i=1, tsize
      if (tth(i) >= tthmin .and. tth(i) <= tthmax) then
          nd = nd + 1
          xd_buf(nd) = real(tth(i))
          Id_buf(nd) = real(intensity(i))
      endif
   enddo
!
!  Allocate array to size of experimental peaks (ns)
!corr   ns = numpeaks(pkind)
   if (exp_size > cap_exp) then
       if (allocated(pkas_buf)) deallocate(pkas_buf, Idmult_buf, exp_tth_r_buf, iass_buf)
       allocate(pkas_buf(exp_size), Idmult_buf(exp_size), exp_tth_r_buf(exp_size), iass_buf(exp_size))
       cap_exp = exp_size
   endif
   exp_tth_r_buf(1:exp_size) = real(exp_tth(:exp_size))
!
!  Match one or more experimental peaks to the database reflection/peak.
   natot = 0
   fomd = 0.0
   pkas_buf(1:exp_size) = 0
   pkad_buf(1:nd) = 0
   do i=1,nd
!corr      nlm = clocate(pkind%getx(),xd(i))
!corr      diffss = abs(pkind(nlm)%getx() - xd(i))
      nlm = clocate(exp_tth_r_buf(1:exp_size),xd_buf(i))
      diffss = abs(exp_tth(nlm) - xd_buf(i))
      if (diffss < delta2theta) then
          natot = natot + 1
          fomd = fomd + diffss
          ass_buf(natot) = ass_type(nlm,i)
          pkad_buf(i) = nlm
          pkas_buf(nlm) = i
      endif
   enddo
!
   if (natot > 0) then
       Idmult_buf(1:exp_size) = 0
       iass_buf(1:exp_size) = .false.
       do j=1,natot
          i  = ass_buf(j)%ps
          kd = ass_buf(j)%pd
          Idmult_buf(i) = Idmult_buf(i) + Id_buf(kd)
          iass_buf(i) = .true.
       enddo

       if (kscal) then
           sumId2 = 0.0
           sumIoId = 0.0
           do i=1,exp_size
              if (iass_buf(i)) then
                  sumId2  = sumId2 + Idmult_buf(i)*Idmult_buf(i)
                  sumIoId = sumIoId + exp_intensity(i) * Idmult_buf(i)
              endif
           enddo
           if (sumId2 > 0) then
               bscale = sumIoId / sumId2                ! compute slope
           else
               bscale = 1.0
           endif
    !      write(23,*)' SCALA retta regressione lineare,sumId2=', bscale,sumId2
           Idmult_buf(1:exp_size) = Idmult_buf(1:exp_size) * bscale
!FIX LATER          crd%scal = bscale
!FIX LATER          crd%I(:) = crd%I0(:) * bscale
           crd_scale = bscale
                 !write(23,*)' minIs, MaxIs, ns nd KSCAL=', minIs,maxIs,ns,nd,kscal,crd%scal
 !!!
       else
           Idmult_buf(1:exp_size) = Idmult_buf(1:exp_size) * crd_scale
!FIX LATER           crd%I(:) = crd%I0(:) * scale
       endif
!
       nda = count(pkad_buf(1:nd) > 0) ! Number of associated database peaks
       nsa = count(pkas_buf(1:exp_size) > 0) ! Number of associated sample peaks
!
!      Compute the 4 contributions to the fom
!
!      1) Calculate the fomd that takes into account the difference between the position
       fomd = 1.0 - fomd/(natot*delta2theta)
!
!      2) Calculate fomI that takes into account the difference on intensities
       fomI = 0.0
       if (crd_scale > 0.0) then
           nassm = 0
           do i=1,exp_size
              if (iass_buf(i)) then
                  nassm = nassm + 1
                  fomI = fomI + abs(exp_intensity(i) - Idmult_buf(i)) / max(exp_intensity(i),Idmult_buf(i))
              endif
           enddo
           fomI = 1.0 - fomI/nassm
       endif
!
!      3) Calculate the contribution of unassociated peak intensities
       ndan = nd - nda  ! number of unassociated database peaks
       ndmin = 0
       if (ndan > 0) then
           do i=1,nd
              if (pkad_buf(i) == 0) then
!
!                 Database peak not associated: find the closest count
                  icl = clocate(dataset(1)%x,xd_buf(i))
                  Idiff = Id_buf(i)*crd_scale - dataset(1)%y(icl)
                  if (Idiff < 0) ndmin = ndmin + 1  !count unassociated peaks below observed
              endif
           enddo
       endif
!
!      Calculate ntph = teoretical number of peaks per phase
       ntph = nint(-9*w_phases+10)  ! teoretical number of phases
       nts = exp_size / ntph
       if (nts < 1) nts = 1
!
!      3) Fom that takes into account the contribution of associated database peaks
       if (nda >= nts) then
           fomdb = nda/real(nd-ndmin)
       else
           fomdb = (nda/real(nd-ndmin)) * sqrt(nda/real(nts))
       endif  
!
!      4) Fom that takes into account the contribution of associated sample peaks
       foms = (nsa/real(exp_size))
!
!      Combine all contributions to the final FOM
       sumsa=0.0
       sumda = 0.0
       numass = 0
       do i=1,exp_size
          if (iass_buf(i)) then
              sumsa = sumsa + exp_intensity(i)
              sumda = sumda + Idmult_buf(i)
              numass=numass + 1
          endif
       enddo
       sumdtot = sum(Id_buf(1:nd)*crd_scale)
       sumstot = sum(exp_intensity(:exp_size))
       SQRTDS = sqrt((sumsa/sumstot)*foms)
       SQRTDA = sqrt((sumda/sumdtot)*fomdb)

       if (numass > 1) then
           fom_tot = SQRTDA*(w2thetad*fomd + w_intensity*fomI + w_phases*SQRTDS)/   &
                            (w2thetad+w_intensity+w_phases)
       else
           fom_tot = SQRTDA*(w2thetad*fomd + w_phases*SQRTDS)/(w2thetad+w_phases) 
       endif
       fom_tot = sqrt(fom_tot)
   endif

   cfom_tot          = real(fom_tot,    c_double)
   fompeakpos_out    = real(fomd,       c_double)
   fomintensity_out  = real(fomI,       c_double)
   scale_out         = real(crd_scale,  c_double)
   !write(0,*)'FOMD = ',fomd,natot,nd
!
   end subroutine computeFOM

end module FOMmod
