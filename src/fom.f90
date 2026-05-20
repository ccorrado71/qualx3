module FOMmod

implicit none

type ass_type
     integer :: ps = 0
     integer :: pd = 0
end type ass_type

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
   type(ass_type), allocatable              :: ass(:)
   real                                     :: fomd, fomI, fomdb, foms, fom_tot
   real, dimension(:), allocatable          :: xd, Id, Idmult
   real                                     :: tthmin, tthmax
   real                                     :: diffss, sumId2, sumIoId, Idiff
   integer                                  :: nd
   integer                                  :: i, j, kd, natot, nlm, nassm, icl
   integer, allocatable                     :: pkas(:), pkad(:)
   logical, allocatable                     :: iass(:)  ! CONVERT TO LOGICAL
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
   allocate(xd(nd), Id(nd), ass(nd), pkad(nd))
!
!  Fill the xd and Id arrays with the peaks in the range
   nd = 0
   do i=1, tsize
      if (tth(i) >= tthmin .and. tth(i) <= tthmax) then
          nd = nd + 1
          xd(nd) = real(tth(i))
          Id(nd) = real(intensity(i))
      endif
   enddo
!
!  Allocate array to size of experimental peaks (ns)
!corr   ns = numpeaks(pkind)
   allocate(pkas(exp_size), Idmult(exp_size))
!
!  Match one or more experimental peaks to the database reflection/peak.
   natot = 0
   fomd = 0.0
   pkas(:) = 0
   pkad(:) = 0
   do i=1,nd
!corr      nlm = clocate(pkind%getx(),xd(i))
!corr      diffss = abs(pkind(nlm)%getx() - xd(i))
      nlm = clocate(real(exp_tth(:exp_size)),xd(i))
      diffss = abs(exp_tth(nlm) - xd(i))
      if (diffss < delta2theta) then
          natot = natot + 1
          fomd = fomd + diffss
          ass(natot) = ass_type(nlm,i)
          pkad(i) = nlm
          pkas(nlm) = i
      endif
   enddo
!
   if (natot > 0) then
       allocate(iass(exp_size))
       do i=1,exp_size
          Idmult(i) = 0
          iass(i) = .false.
          do j=1,natot
             if (ass(j)%ps == i) then
                 kd = ass(j)%pd
                 Idmult(i) = Idmult(i) + Id(kd)
                 iass(i) = .true.
             endif
          enddo
       enddo

       if (kscal) then               
           sumId2 = 0.0
           sumIoId = 0.0
           do i=1,exp_size
              if (iass(i)) then
                  sumId2  = sumId2 + Idmult(i)*Idmult(i)
                  sumIoId = sumIoId + exp_intensity(i) * Idmult(i)
              endif
           enddo
           if (sumId2 > 0) then
               bscale = sumIoId / sumId2                ! compute slope
           else
               bscale = 1.0
           endif
    !      write(23,*)' SCALA retta regressione lineare,sumId2=', bscale,sumId2
           Idmult(:) = Idmult(:) * bscale
!FIX LATER          crd%scal = bscale
!FIX LATER          crd%I(:) = crd%I0(:) * bscale
           crd_scale = bscale
                 !write(23,*)' minIs, MaxIs, ns nd KSCAL=', minIs,maxIs,ns,nd,kscal,crd%scal
 !!!
       else
           Idmult(:) = Idmult(:) * crd_scale
!FIX LATER           crd%I(:) = crd%I0(:) * scale
       endif       
!
       nda = count(pkad > 0) ! Number of associated database peaks
       nsa = count(pkas > 0) ! Number of associated sample peaks
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
              if (iass(i)) then
                  nassm = nassm + 1
                  fomI = fomI + abs(exp_intensity(i) - Idmult(i)) / max(exp_intensity(i),Idmult(i))
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
              if (pkad(i) == 0) then
!
!                 Database peak not associated: find the closest count
                  icl = clocate(dataset(1)%x,xd(i))
                  Idiff = Id(i)*crd_scale - dataset(1)%y(icl)
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
          if (iass(i)) then
              sumsa = sumsa + exp_intensity(i)
              sumda = sumda + Idmult(i)
              numass=numass + 1
          endif
       enddo
       sumdtot = sum(Id(:)*crd_scale)
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
