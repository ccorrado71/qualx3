module calculate_pdp

implicit none

integer, parameter :: RIET_REFINE=0, LEBAIL_DECOMP=1, PAWLEY_DECOMP=2

private :: calculate_powder_pattern_s, calculate_powder_pattern_v
interface calculate_powder_pattern
   module procedure :: calculate_powder_pattern_s, calculate_powder_pattern_v
end interface calculate_powder_pattern

contains

   subroutine calculate_powder_pattern_s(fval,ncu,xczero,yc,ref,nref,nwave,ratio,pear,pvoi,tch,typef,nph,ier)
   USE progtype
   USE pearsonf
   USE psvoigtf
   USE tchzf
   USE profile_function
   USE reflection_type_util
!
   real, dimension(:), intent(in)                                  :: fval      ! structure factors used for computing
!corr   type(refine_condition_type), intent(in)                         :: rfc       ! settings for powder pattern
   integer, intent(in)                                             :: ncu
   real, dimension(:), intent(in)                                  :: xczero    ! 2-theta with zero correction
   real, dimension(:), intent(out)                                 :: yc        ! calculated pattern
   type(reflection_type), dimension(:), allocatable, intent(inout) :: ref       ! reflections
   integer, intent(in)                                             :: nref      ! number of reflections
   integer, intent(in)                                             :: nwave     ! 1 or 2 (for second wavelength)
   real, dimension(:), intent(in)                                  :: ratio     ! ratio of intensity between 2 wavel.
   type(profile_function_t), intent(in)                            :: pear
   type(profile_function_t), intent(in)                            :: pvoi
   type(profile_function_t), intent(in)                            :: tch
   integer, intent(in)                                             :: typef
   integer, intent(in)                                             :: nph
   integer, intent(out)                                            :: ier
!corr   real, dimension(:), allocatable                                 :: fcstr
   integer                                                         :: posfr
!
   posfr = nref
!corr   allocate(fcstr(posfr))
!corr   if (rfc%raction > 0) then
!corr       fcstr(:) = ref(:posfr)%fo
!corr   else
!corr       fcstr(:) = ref(:posfr)%fc
!corr   endif
!
   select case (typef)
     case (PEARSON7)
       call pearson(pear,ref(:posfr),xczero(:ncu),fval,yc,nwave,ratio,nph,ier)
     case (PVOIG)
       call psvoigt(pvoi,ref(:posfr),xczero(:ncu),fval,yc,nwave,ratio,nph,ier)
     case (TCHZ)
       call psvoigt_tchz(tch,ref(:posfr),xczero(:ncu),fval,yc,nwave,ratio,nph,ier)
     case default
       ier = 1
   end select
!
   end subroutine calculate_powder_pattern_s

!--------------------------------------------------------------------------------------

   subroutine calculate_powder_pattern_v(rfc,dataset,cryst,ier)
   USE datasetmod
   USE crystal_phase
   USE progtype
   USE profile_function
   USE arrayutil
   type(refine_condition_type), intent(in)            :: rfc       ! settings for powder pattern
   type(dataset_type), dimension(:), intent(inout)    :: dataset
   type(crystal_phase_t), dimension(:), intent(inout) :: cryst
   integer, intent(out)                               :: ier
   integer                                            :: nph,nref
!  
   call pd_phase_init(size(cryst))
!
   dataset(1)%yc = 0
   do nph=1,size(cryst)
      nref = cryst(nph)%numref()
      call pd_ref_init(nph,cryst(nph)%numref(),dataset(1)%nwave)
      call new_array(cryst(nph)%yc,dataset(1)%npointsc())
      if (rfc%raction == RIET_REFINE .or. .not.cryst(nph)%is_extraction) then
          call calculate_powder_pattern_s(cryst(nph)%ref%fc,dataset(1)%nc2,dataset(1)%x0,  &
          cryst(nph)%yc,cryst(nph)%ref,nref,dataset(1)%nwave,                                  &
          dataset(1)%ratio,dataset(1)%pear(nph),dataset(1)%pvoi(nph),                          &
          dataset(1)%tch(nph),dataset(1)%typefun(nph),nph,ier)
      else
          call calculate_powder_pattern_s(cryst(nph)%ref%fo,dataset(1)%nc2,dataset(1)%x0,  &
          cryst(nph)%yc,cryst(nph)%ref,nref,dataset(1)%nwave,                                  &
          dataset(1)%ratio,dataset(1)%pear(nph),dataset(1)%pvoi(nph),                          &
          dataset(1)%tch(nph),dataset(1)%typefun(nph),nph,ier)
      endif
      dataset(1)%yc = dataset(1)%yc + cryst(nph)%scal*cryst(nph)%yc
   enddo
   end subroutine calculate_powder_pattern_v

!--------------------------------------------------------------------------------------

   subroutine init_data_profile(datas,numph)
   USE datasetmod
   USE profile_function
   USE arrayutil
   USE counts
   type(dataset_type), intent(inout) :: datas
   integer, intent(in)               :: numph
   integer                           :: nph
   real                              :: fwhm
!
!  Allocate array
   call new_array(datas%typefun,numph)
   call new_profilef(datas%pear,numph)
   call new_profilef(datas%pvoi,numph)
   call new_profilef(datas%tch,numph)
!
!  Init array
   do nph=1,numph
      fwhm =  delta_from_lambda(0.1,1.54056,datas%wave(1),4.0)
      call init_function_param(datas%pear(nph),datas%pvoi(nph),datas%tch(nph),fwhm)
      !call init_function_param(datas%pear(nph),datas%pvoi(nph),datas%tch(nph))
   enddo
   !datas%typefun = PVOIG
   !datas%typefun = TCHZ
   datas%typefun = PEARSON7
!
   end subroutine init_data_profile

!--------------------------------------------------------------------------------------

   integer function num_pfunction_ref(datas,numph) result(nprof)
   USE datasetmod
   USE pearsonf
   USE psvoigtf
   USE tchzf
   USE profile_function
   type(dataset_type), intent(inout) :: datas
   integer, intent(in)               :: numph
   integer                           :: nph,npar

   nprof = 0
   do nph=1,numph
      select case (datas%typefun(nph))
        case (PEARSON7)
          do npar=PV_UPAR,PV_ASYM4PAR
             if (datas%pear(nph)%par(npar)%rcod > 0) nprof = nprof + 1
          enddo

        case (PVOIG)
          do npar=PV_UPAR,PV_ASYM4PAR
             if (datas%pvoi(nph)%par(npar)%rcod > 0) nprof = nprof + 1
          enddo

        case (TCHZ)
          do npar=T_UPAR,T_ASYM4PAR
             if (datas%tch(nph)%par(npar)%rcod > 0) nprof = nprof + 1
          enddo

      end select
!corr      if (cryst(nph)%scode > 0) nprof = nprof + 1
   enddo

   end function num_pfunction_ref

!--------------------------------------------------------------------------------------

   subroutine init_function_param(pears,psvoig,tch,fwhm,par)
!  
!  Initialize profile functions
!  
   USE pearsonf
   USE psvoigtf
   USE tchzf
   USE profile_function
   type(profile_function_t), intent(out), optional :: pears
   type(profile_function_t), intent(out), optional :: psvoig
   type(profile_function_t), intent(out), optional :: tch
   real, intent(in), optional                      :: fwhm
   real, dimension(:), intent(in), optional        :: par
!  
   if (present(pears)) then
       pears = init_pearson_param()
       if (present(fwhm)) pears%par(P_WPAR)%val = fwhm*fwhm
       if (present(par)) call set_function_param(pears,par)
   endif
!  
   if (present(psvoig)) then
       psvoig = init_psvoigt_param()
       if (present(fwhm)) psvoig%par(PV_WPAR)%val = fwhm*fwhm
       if (present(par)) call set_function_param(psvoig,par)
   endif
!  
   if (present(tch)) then
       tch = init_tchz_param()
       if (present(fwhm)) tch%par(T_WPAR)%val = fwhm*fwhm
       if (present(par)) call set_function_param(tch,par)
   endif
!  
   end subroutine init_function_param

!--------------------------------------------------------------------------------------

   subroutine set_function_param(pfunct,xpar)
   use profile_function
   type(profile_function_t), intent(inout) :: pfunct
   real, dimension(:), intent(in)          :: xpar
   integer                                 :: nsize
!
   nsize = min(size(xpar),size(pfunct%par))
   pfunct%par(1:nsize)%val = xpar(1:nsize)
!
   end subroutine set_function_param

end module calculate_pdp
