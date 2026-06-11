module interop_range
   implicit none

   private
   
   contains

   subroutine set_range(kaction, tminvalue, tmaxvalue) bind(C,name="set_range")
   use iso_c_binding
   use variables, only: dataset
   use arrayutil
!corr   use patternref, only: thminp,thmaxp,minrange,maxrange,modify_range,thmin,thmax
   use peak_mod
!corr   use conteggi, only: theta_int,npunti
   use view
   use enable_amb
   use datamod
   use datasetmod
!corr   use general, only: nrefl
   integer(c_int), value, intent(in)       :: kaction
   real(c_double)                          :: tminvalue
   real(c_double)                          :: tmaxvalue
!corr   real, allocatable, dimension(:,:), save :: thetac
   logical, save                           :: modif
   type(dataset_type), save :: datac
   common /limiti/ nlini, nlfin
   integer :: nlini, nlfin
   integer :: nc
   real :: thminp, thmaxp
!
   select case(kaction)
     case (0)     ! Open dialog
!corr!
       tminvalue = dataset(1)%tmin
       tmaxvalue = dataset(1)%tmax
!corr!
!corr!      Save counts
!corr       allocate(thetac(npunti,7))
!corr       thetac(:,:) = theta_int(:,:)
!corr       npuntisav = npunti
       datac = dataset(1)
       modif = .false.

     case (1,2)                    ! 1=Apply, 2=OK
!corr       nlini = clocate(theta_int(:,7),real(tminvalue))
!corr       nlfin = clocate(theta_int(:,7),real(tmaxvalue))
!corr       if (nlini /= 1 .or. nlfin /= npunti) then
!corr           thminp = theta_int(nlini, 7)
!corr           thmaxp = theta_int(nlfin, 7)
       nc = dataset(1)%npoints()
       call dataset(1)%resize(real(tminvalue),real(tmaxvalue))
       if (nc /= dataset(1)%npoints()) then
!
!          resize peaks
           if (numpeaks(pkind) > 0) then
               thminp = dataset(1)%tmin
               thmaxp = dataset(1)%tmax
               call peak_select(pkind,thminp,thmaxp,DELETE_PEAK,outside=.true.)
               call peak_delete_selected(pkind)
           endif
!
           modif = .true.
       endif
!corr       endif

       call vedinew(pinioss=1,pfinoss=dataset(1)%npoints())

       if (kaction == 2) then
!corr           deallocate(thetac)
           call abilita_tasti(PatternAction,1)
           !call abilita_tasti('pattern')
           !call wmenu_set_state('Range'//char(0),1)
           !call toolbar_set_state('continue'//char(0),1)
!corr           if (modif) then  ! update modify_range for input file 
!corr               modify_range = .true.
!corr               minrange = thmin
!corr               maxrange = thmax
!corr           endif
       endif

     case (3)                    ! Cancel
!
!      Ripristina i conteggi in caso di modifica
       if (dataset(1)%npoints() /= datac%npoints()) then
           dataset(1) = datac
       endif
!corr       if (npunti /= npuntisav) then
!corr           call AllocaConteggi(2)
!corr           npunti = npuntisav
!corr           call AllocaConteggi(1)
!corr!!!!FIX PROBLEM: update dataset or Cancel doesn't work properly
!corr!!!! use %update_data or %save/%restore
!corr           theta_int(:,:) = thetac(:,:)
!corr           thmin = theta_int(1,7)
!corr           thmax = theta_int(npunti,7)
!corr!corr           if (nrefl > 0) call gener(dataset(1)%wave,iflagref,.false.)
!corr       endif
!
!      Disegna anche per forzare la scomparsa delle barre
       !call vedinew(pinioss=1,pfinoss=npunti)
!
!corr       deallocate(thetac)
       call abilita_tasti(PatternAction,1)
       !call abilita_tasti('pattern')
       !call wmenu_set_state('Range'//char(0),1)
       !call toolbar_set_state('continue'//char(0),1)
       modif = .false.

   end select
!
   end subroutine set_range

end module interop_range
