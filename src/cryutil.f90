module cryutil

implicit none
private :: to_coord_cry_s, to_coord_cry_v
interface to_coord_cry
   module procedure to_coord_cry_s, to_coord_cry_v
end interface

contains


   subroutine to_coord_cry_s(crystal,kpr,code)
   use crystal_phase
   use compare_struct
!   use sinterface
   use testmodel, only: vett_test,rmsd
!
   type(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                  :: kpr
   integer, intent(in)                  :: code
!
!   call set_spaceg_new(crystal%spg)  ! TOFIX: necessary for call to examin
!   call to_coord_sir(code,crystal%at,crystal%ref,size(crystal%ref),crystal%cell,crystal%spg,kpr,crystal%atpub)
   crystal%dmean = vett_test(4)
   crystal%natfound = vett_test(3)
   crystal%rmsd = rmsd
!
   end subroutine to_coord_cry_s

!------------------------------------------------------------------------------------------------------------------

   subroutine to_coord_cry_v(crystal,kpr,code,msg)
   use crystal_phase
   type(crystal_phase_t), dimension(:), allocatable, intent(inout) :: crystal
   integer, intent(in)                                             :: kpr,msg,code
   integer                                                         :: nph
!   
   do nph=1,numphase(crystal)
      if (crystal(nph)%natoms() > 0) then
          if (crystal(nph)%is_known()) then
              call to_coord_cry_s(crystal(nph),kpr,code)
!corr              if (msg > 0) call write_message_found1(msg+nph-1,crystal(nph)%natfound,crystal(nph)%natpub(),   &
!corr                           crystal(nph)%rmsd)
          endif
      endif
   enddo
!
   end subroutine to_coord_cry_v

!------------------------------------------------------------------------------------------------------------------

   subroutine print_bv_info(crystal,fout)
   use crystal_phase
   use symmgrid
   use nr
   use arrayutil
   use strutil
   type(crystal_phase_t), intent(in)            :: crystal
   integer, intent(in)                          :: fout
   integer                                      :: ia,ncoord
   real, dimension(:), allocatable              :: bvs
   type(bond_info_t), dimension(:), allocatable :: dinfo
   integer, dimension(:), allocatable           :: iord
!
   call crystal%print_bvparam(fout)
   allocate(bvs(crystal%natoms()))
   do ia=1,crystal%natoms()
      call crystal%compute_bvs(ia,bvs(ia))
      write(fout,'(/a)') 'BV-sum of '//crystal%at(ia)%glab()//': '//r_to_s(bvs(ia),4)//'v.u.'
      ncoord = crystal%coord_number(ia)
      if (ncoord > 0) then
          call new_bond_info(dinfo,ncoord)
          call crystal%valence_atom_info(ia,dinfo,bvs(ia),ncoord)
          call new_array(iord,ncoord)
          call indexx(dinfo%at2,iord)
          dinfo(:) = dinfo(iord)
          call print_bond_info(crystal%at,crystal%spg,dinfo,fout)
      endif
   enddo
   write(fout,'(/a,f10.3)')'Global instability index GII = '//r_to_s(crystal%gii_index(bvs),4)//'v.u.'
!
   end subroutine print_bv_info

!--------------------------------------------------------------------

   subroutine bvs_crystal(crystal, code, kpr, ier)
   use crystal_phase
   type(crystal_phase_t), intent(inout) :: crystal
   integer, intent(in)                  :: code  ! rebuild symmetry for code=1
   integer                              :: kpr
   integer, intent(out)                 :: ier

   ier = 0
   if (crystal%natoms() == 0 .or. crystal%numelem() == 0) then
       ier = 1
       return
   endif

   if (code == 1) then
       call crystal%load_bvparam()
       call crystal%make_symmetry_cell()
   endif
   call crystal%allocate_grid(cutoff=crystal%bvs_cutoff())
   call crystal%fill_grid()

   if (kpr > 0) then
       call print_bv_info(crystal,kpr)
   endif

   end subroutine bvs_crystal

!------------------------------------------------------------------------------------

   subroutine pearson_init(crystal,xcount,pear,nwave,ratio,nph,ier) 
   USE trig_constants, only:dtor,pi
   USE arrayutil
   USE profile_function
   USE asymfunc
   USE pearsonf
   USE crystal_phase
!
   type(crystal_phase_t), intent(inout)               :: crystal
   type(profile_function_t), intent(in)               :: pear
   real, dimension(:), intent(in)                     :: xcount
   real                                               :: b0,b1,b2
   real                                               :: fu,fv,fw
   real                                               :: asym
   integer, intent(in)                                :: nwave
   real, dimension(2), intent(in)                     :: ratio
   integer, intent(in)                                :: nph
   integer, intent(out)                               :: ier
   integer                                            :: j,nnn,kal
   real                                               :: tthdeg
   real                                               :: tdeg
   real                                               :: uvwg,uvw2,uvwqg
   real                                               :: deltath,deltath2
   real                                               :: argbeta
   real                                               :: pears
   real                                               :: beta,pconst  
   integer                                            :: negfw
   real                                               :: cpear,cpear2
   real                                               :: gamra
!   integer                                            :: nnref
   intrinsic :: gamma
!                
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
   if (.not.allocated(crystal%profi)) allocate(crystal%profi(crystal%numref(),nwave)) 
   fu = pear%par(P_UPAR)%val    
   fv = pear%par(P_VPAR)%val    
   fw = pear%par(P_WPAR)%val    
   b0 = pear%par(P_B0PAR)%val   
   b1 = pear%par(P_B1PAR)%val   
   b2 = pear%par(P_B2PAR)%val   
   asym = pear%par(P_ASYM1PAR)%val
   negfw = 0
   ier = 0
   !nnref = size(ref)
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=1,crystal%numref()
         tthdeg = crystal%ref(j)%tthd(kal)
!
         beta = b0 + b1/tthdeg + b2/(tthdeg**2)
         if (beta < betamin .or. beta > betamax) then
             !write(6,*)'WARN BETA=',beta
             ier = 1
             !return
         endif
         if (beta < betamin) beta = betamin
         if (beta > betamax) beta = betamax
         gamra = gamma(beta) / gamma(beta-0.5)

         tdeg = tan (0.5 * tthdeg * dtor)
         uvwqg = fU*tdeg**2 + fV*tdeg + fW
         if (uvwqg.lt.0.0001) then
             uvwqg = 0.0001
             negfw = negfw + 1
             ier = 1
         endif
         uvwg = sqrt(uvwqg)
         pconst = 2.0**(1.0/beta) - 1.0
         cpear = (2.0 * sqrt(pconst/pi)) / uvwg
         cpear2 = (4.0 * pconst) / uvwqg
         crystal%ref(j)%fwhm(kal) = uvwg
         uvw2 =  uvwg * crystal%ref(j)%pk * crystal%ref(j)%rapI
         pdph(nph)%pvet(j,kal,BETAPAR) = beta
         pdph(nph)%pvet(j,kal,GAMRATIO) = gamra
         pdph(nph)%pvet(j,kal,PEARPAR1) = cpear
         pdph(nph)%pvet(j,kal,PEARPAR2) = cpear2
!
         crystal%profi(j,kal)%iniz = clocate(xcount,tthdeg-uvw2)
         crystal%profi(j,kal)%ifin = clocate(xcount,tthdeg+uvw2)
         allocate(crystal%profi(j,kal)%func(crystal%profi(j,kal)%ifin-crystal%profi(j,kal)%iniz+1))
         crystal%profi(j,kal)%func = 0.0  ! forse non serve
!
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(crystal%profi(j,kal)%iniz:crystal%profi(j,kal)%ifin)-tthdeg,  &
                                tthdeg,crystal%ref(j)%fwhm(kal),pear%par(P_ASYM1PAR:P_ASYM4PAR)%val,beta)
!
         LOOP_CONTEGGI : do nnn=crystal%profi(j,kal)%iniz,crystal%profi(j,kal)%ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            argbeta=(1.0+cpear2*deltath2)
            pears = gamra * cpear * argbeta**(-beta)
            crystal%profi(j,kal)%func(nnn-crystal%profi(j,kal)%iniz+1) = pears*asymval(nnn-crystal%profi(j,kal)%iniz+1)
         enddo LOOP_CONTEGGI
         crystal%profi(j,kal)%fconst = crystal%ref(j)%m*crystal%ref(j)%lp(kal)*ratio(kal)
      enddo LOOP_RIFLESSI
   enddo LOOP_KALPHA
!corr   maxlim = maxval(profi(:,nwave)%ifin)
!
   end subroutine pearson_init

!------------------------------------------------------------------------------------

   subroutine pearson_calc(crystal,ycount,sumyco,nwave)
   USE profile_function
   USE pearsonf
   USE crystal_phase
!
   type(crystal_phase_t), intent(in)                  :: crystal
   real, dimension(:), intent(out)                    :: ycount
   real, intent(out)                                  :: sumyco
   integer, intent(in)                                :: nwave
   integer                                            :: j,nnn
   real                                               :: ffx
   real                                               :: addyco
   integer                                            :: iniz0,kal
!
   ycount(:) = 0.0
   sumyco = 0.0
!
   if (crystal%gcode > 0) then
       LOOP_KALPHAP : do kal=1,nwave
           do j=1,size(crystal%ref)
              ffx = crystal%ref(j)%fc*crystal%profi(j,kal)%fconst*crystal%ref(j)%po
              iniz0 = crystal%profi(j,kal)%iniz - 1
              do nnn=crystal%profi(j,kal)%iniz,crystal%profi(j,kal)%ifin
                 addyco =  ffx*crystal%profi(j,kal)%func(nnn-iniz0)
                 sumyco = sumyco + addyco
                 ycount(nnn)=ycount(nnn) + addyco
              enddo 
           enddo 
       enddo LOOP_KALPHAP
   else
       LOOP_KALPHA : do kal=1,nwave
           do j=1,size(crystal%ref)
              ffx = crystal%ref(j)%fc*crystal%profi(j,kal)%fconst
              iniz0 = crystal%profi(j,kal)%iniz - 1
              do nnn=crystal%profi(j,kal)%iniz,crystal%profi(j,kal)%ifin
                 addyco =  ffx*crystal%profi(j,kal)%func(nnn-iniz0)
                 sumyco = sumyco + addyco
                 ycount(nnn)=ycount(nnn) + addyco
              enddo 
           enddo 
       enddo LOOP_KALPHA
   endif
!
   end subroutine pearson_calc
 
!--------------------------------------------------------------------

   subroutine psvoigt_init(crystal,xcount,pvoi,nwave,ratio,nph)
   USE reflection_type_util, only: reflection_type
   USE arrayutil
   USE profile_function
   USE asymfunc
   USE psvoigtf
   USE crystal_phase
!
   type(crystal_phase_t), intent(inout)               :: crystal
!corr   type(reflection_type), dimension(:), intent(inout) :: ref
   real, dimension(:), intent(in)                     :: xcount
   type(profile_function_t), intent(in)               :: pvoi
!corr   integer, intent(out)                               :: maxlim
   integer, intent(in)                                :: nwave
   integer                                            :: j,nnn,kal
   real, dimension(2), intent(in)                     :: ratio
   integer, intent(in)                                :: nph
   real                                               :: eta,eta0,eta1,eta2
   real                                               :: fu,fv,fw
   real                                               :: uvw2,uvwqg
   real                                               :: tthdeg
   real                                               :: rdeg,tdeg,tdeg2
   real                                               :: cgaus,cgaus2,cloren,cloren2
   real                                               :: deltath,deltath2
   real                                               :: pvoigt
!
!corr   if (.not.allocated(profi)) allocate(profi(size(ref),nwave)) 
   if (.not.allocated(crystal%profi)) allocate(crystal%profi(crystal%numref(),nwave)) 
!
   if (size_array(asymval) < size(xcount)) call new_array(asymval,size(xcount))
!
   fu   =   pvoi%par(PV_UPAR)%val    
   fv   =   pvoi%par(PV_VPAR)%val    
   fw   =   pvoi%par(PV_WPAR)%val    
   eta0 =   pvoi%par(PV_E0PAR)%val   
   eta1 =   pvoi%par(PV_E1PAR)%val   
   eta2 =   pvoi%par(PV_E2PAR)%val   
   LOOP_KALPHA : do kal=1,nwave
      LOOP_RIFLESSI : do j=1,crystal%numref() !size(ref)
         tthdeg = crystal%ref(j)%tthd(kal)
         rdeg = 0.5 * tthdeg * dtor
         !tdeg = sin(rdeg) / cos(rdeg)
         tdeg = tan(rdeg)
         tdeg2 = tdeg * tdeg
!
         eta = eta0 + eta1*tdeg + eta2*tdeg2
         uvwqg = fu*tdeg2 + fv*tdeg + fw
         if (uvwqg < 0.0001)  uvwqg = 0.0001

         pdph(nph)%pvet(j,kal,ETAPAR) = eta
         pdph(nph)%pvet(j,kal,UVWPAR) = 1./uvwqg

         crystal%ref(j)%fwhm(kal) = sqrt(uvwqg)
         cgaus =   GAUSCONST / crystal%ref(j)%fwhm(kal)
         cloren = CONSTLOREN / crystal%ref(j)%fwhm(kal)
         cgaus2 =   GAUSCONST2 * pdph(nph)%pvet(j,kal,UVWPAR)
         cloren2 = 4.0 * pdph(nph)%pvet(j,kal,UVWPAR)
!
         uvw2 =  crystal%ref(j)%fwhm(kal)* crystal%ref(j)%pk * crystal%ref(j)%rapI
!
         crystal%profi(j,kal)%iniz = clocate(xcount,tthdeg-uvw2)
         crystal%profi(j,kal)%ifin = clocate(xcount,tthdeg+uvw2)
         allocate(crystal%profi(j,kal)%func(crystal%profi(j,kal)%ifin-crystal%profi(j,kal)%iniz+1))
         crystal%profi(j,kal)%func = 0.0  ! forse non serve
         call compute_asymmetry(BERAR_BALDINOZZI_TYPE,asymval,xcount(crystal%profi(j,kal)%iniz:crystal%profi(j,kal)%ifin)-tthdeg, &
                                tthdeg,crystal%ref(j)%fwhm(kal),pvoi%par(PV_ASYM1PAR:PV_ASYM4PAR)%val)
!
         LOOP_CONTEGGI : do nnn=crystal%profi(j,kal)%iniz,crystal%profi(j,kal)%ifin
            deltath=xcount(nnn)-tthdeg
            deltath2=deltath**2
            pvoigt = eta*cloren/(1.0+cloren2*deltath2) + (1.0-eta)*cgaus*exp(-cgaus2*deltath2)
            crystal%profi(j,kal)%func(nnn-crystal%profi(j,kal)%iniz+1) = pvoigt*asymval(nnn-crystal%profi(j,kal)%iniz+1)
         enddo LOOP_CONTEGGI
         !profi(j)%fconst = ref(j)%m*ref(j)%po*ref(j)%lp
         crystal%profi(j,kal)%fconst = crystal%ref(j)%m*crystal%ref(j)%lp(kal)*ratio(kal)
      enddo LOOP_RIFLESSI
   enddo LOOP_KALPHA
!corr   maxlim = maxval(profi(:,nwave)%ifin)
!
   end subroutine psvoigt_init

!--------------------------------------------------------------------

   subroutine close_profile(crystal)
   use crystal_phase
   type(crystal_phase_t), intent(inout) :: crystal
!
   if (allocated(crystal%profi)) deallocate(crystal%profi)
!   
   end subroutine close_profile

!--------------------------------------------------------------------

   subroutine block_data_ref(j_in,crystal,wave)
   use reflection_type_util
   use counts
   use crystal_phase
   use nr
   integer, intent(in)                                          :: j_in
   type(crystal_phase_t), dimension(:), allocatable, intent(in) :: crystal
   real, intent(in)                                             :: wave
   integer                                                      :: ntotref
   integer, dimension(:), allocatable                           :: idph
   type(reflection_type), dimension(:), allocatable             :: ref
   integer, dimension(:), allocatable                           :: ord
   real, dimension(:), allocatable                              :: dval,ical
   integer                                                      :: i,nph,nref
   real                                                         :: scal,dhigh,dlow
   integer, dimension(3)                                        :: hklmax,hklmin
!
   if (numphase(crystal) == 0) return

   if (numphase(crystal) > 1) then
       call combine_reflections(crystal,ntotref,ref,idph)
       if (ntotref == 0) return
!
       allocate(dval(ntotref),ical(ntotref))
       ntotref = 0
       do nph=1,numphase(crystal)
          scal = sum(crystal(nph)%ref%fc)/sum(crystal(nph)%ref%fo)
          do i=1,crystal(nph)%numref()
             ntotref = ntotref + 1
             ref(ntotref)%fo = scal*ref(ntotref)%fo
             dval(ntotref) = dvalue(crystal(nph)%ref(i)%tthd(1),wave)
             if (crystal(nph)%is_extraction) then
                 ical(ntotref) = crystal(nph)%ref(i)%fo**2*get_mcorr(crystal(nph)%ref(i),1)
             else
                 ical(ntotref) = crystal(nph)%ref(i)%fc**2*get_mcorr(crystal(nph)%ref(i),1)
             endif
          enddo
       enddo
!
       allocate(ord(ntotref))
       call indexx(dval,ord)
       ref(:) = ref(ord)
       dval(:) = dval(ord)
       idph(:) = idph(ord)
!
       nref = ntotref
       do i=1,3
          hklmax(i) = maxval(ref%hkl(i))
          hklmin(i) = minval(ref%hkl(i))
       enddo
       dhigh = dval(1)
       dlow = dval(nref)
    else
       nref = crystal(1)%numref()
       do i=1,3
          hklmax(i) = maxval(crystal(1)%ref%hkl(i))
          hklmin(i) = minval(crystal(1)%ref%hkl(i))
       enddo
       dhigh = dvalue(crystal(1)%ref(1)%tthd(1),wave)
       dlow = dvalue(crystal(1)%ref(nref)%tthd(1),wave)
       allocate(ical(crystal(1)%numref()))
       if (crystal(1)%is_extraction) then
           ical(:) = crystal(1)%ref%fo**2*get_mcorr(crystal(1)%ref,1)
       else
           ical(:) = crystal(1)%ref%fc**2*get_mcorr(crystal(1)%ref,1)
       endif
    endif
!
!  write reflection info
   write(j_in,'(a,i0)')  '_reflns_number_total      ',nref
   write(j_in,'(a,i0)')  '_reflns_limit_h_min       ',hklmin(1)
   write(j_in,'(a,i0)')  '_reflns_limit_h_max       ',hklmax(1)
   write(j_in,'(a,i0)')  '_reflns_limit_k_min       ',hklmin(2)
   write(j_in,'(a,i0)')  '_reflns_limit_k_max       ',hklmax(2)
   write(j_in,'(a,i0)')  '_reflns_limit_l_min       ',hklmin(3)
   write(j_in,'(a,i0)')  '_reflns_limit_l_max       ',hklmax(3)
   write(j_in,'(a,f0.3)')'_reflns_d_resolution_high ',dhigh
   write(j_in,'(a,f0.3)')'_reflns_d_resolution_low  ',dlow
   write(j_in,*)
!
   write(j_in,'(a)')'loop_'
   write(j_in,'(a)')'      _refln_index_h'
   write(j_in,'(a)')'      _refln_index_k'
   write(j_in,'(a)')'      _refln_index_l'
   if (numphase(crystal) > 1)  &
   write(j_in,'(a)')'      _pd_refln_phase_id'
   write(j_in,'(a)')'      _refln_F_squared_meas'
   if (crystal(1)%is_extraction) then
       write(j_in,'(a)')'      _refln_d_spacing'
       write(j_in,'(a)')'      _refln_intensity_meas'
       if (numphase(crystal) > 1) then
           do i=1,ntotref
              write(j_in,'(3(1x,i3),1x,i2,1x,f12.3,1x,f12.5,1x,f12.3)')   &
                          ref(i)%hkl,idph(i),ref(i)%fo**2,dval(i),ical(i)
           enddo
       else
           do i=1,crystal(1)%numref()
              write(j_in,'(3(1x,i3),1x,f12.3,1x,f12.5,1x,f12.3)')   &
                          crystal(1)%ref(i)%hkl,crystal(1)%ref(i)%fo**2,   &
                          dvalue(crystal(1)%ref(i)%tthd(1),wave),ical(i)
           enddo
       endif
   else
       write(j_in,'(a)')'      _refln_F_squared_calc'
       write(j_in,'(a)')'      _refln_phase_calc'
       write(j_in,'(a)')'      _refln_d_spacing'
       write(j_in,'(a)')'      _refln_intensity_calc'
!
       ical(:) = 1000.*ical(:)/maxval(ical(:))
       if (numphase(crystal) > 1) then
           do i=1,ntotref
              write(j_in,'(3(1x,i3),1x,i2,1x,f12.3,1x,f12.3,1x,f10.3,1x,f12.5,1x,f12.3)')   &
                          ref(i)%hkl,idph(i),ref(i)%fo**2,ref(i)%fc**2,real(ref(i)%ph),dval(i),ical(i)
           enddo
       else
           scal = sum(crystal(1)%ref%fc)/sum(crystal(1)%ref%fo)
           do i=1,crystal(1)%numref()
              write(j_in,'(3(1x,i3),1x,f12.3,1x,f12.3,1x,f10.3,1x,f12.5,1x,f10.3)')   &
                          crystal(1)%ref(i)%hkl,(scal*crystal(1)%ref(i)%fo)**2,crystal(1)%ref(i)%fc**2,   &
                          real(crystal(1)%ref(i)%ph),dvalue(crystal(1)%ref(i)%tthd(1),wave),ical(i)
           enddo
       endif
   endif
!
   end subroutine block_data_ref

!--------------------------------------------------------------------

   subroutine combine_reflections(crystal,ntotref,ref,idph,idref)
   use crystal_phase
   use reflection_type_util
   type(crystal_phase_t), dimension(:), allocatable, intent(in)  :: crystal
   integer, intent(out)                                          :: ntotref
   integer, dimension(:), allocatable, intent(out)               :: idph
   integer, dimension(:), allocatable, intent(out), optional     :: idref
   type(reflection_type), dimension(:), allocatable, intent(out) :: ref
   integer                                                       :: i,nph
!
!  Total number of reflections
   ntotref = crystal(1)%numref()
   do nph=2,numphase(crystal)
      ntotref = ntotref + crystal(nph)%numref()
   enddo
   if (ntotref == 0) return
!
!  Combine all reflections in the array ref
   allocate(idph(ntotref),ref(ntotref))
!
   if (present(idref)) then
       allocate(idref(ntotref))
       ntotref = 0
       do nph=1,numphase(crystal)
          do i=1,crystal(nph)%numref()
             ntotref = ntotref + 1
             ref(ntotref) = crystal(nph)%ref(i)
             idph(ntotref) = nph
             idref(ntotref) = i
          enddo
       enddo
   else
       ntotref = 0
       do nph=1,numphase(crystal)
          do i=1,crystal(nph)%numref()
             ntotref = ntotref + 1
             ref(ntotref) = crystal(nph)%ref(i)
             idph(ntotref) = nph
          enddo
       enddo
   endif
!
   end subroutine combine_reflections

!--------------------------------------------------------------------

   subroutine export_phase_cif(crystal,datas,filename,std,rinfo)
   use crystal_phase
   use fileutil
   use prognames
   use cif_frm
   use datasetmod
   use progtype, only: refine_info_type
   use strutil, only: i_to_s
   type(crystal_phase_t), dimension(:), allocatable, intent(in) :: crystal
   type(dataset_type), intent(in)                               :: datas
   character(len=*), intent(in)                                 :: filename
   logical, intent(in)                                          :: std
   type(refine_info_type), intent(in), optional                 :: rinfo
   integer                                                      :: nph
   type(file_handle)                                            :: fcif
   character(len=:), allocatable                                :: data_name
!
   call fcif%fopen(filename,'w')
   do nph=1,numphase(crystal)
      if (numphase(crystal) == 1) then
          data_name = 'data_global'
      else
          data_name = 'data_phase_'//i_to_s(nph)
      endif
      call create_ciffile(crystal(nph)%at,crystal(nph)%cell,crystal(nph)%spg,crystal(nph)%elem,          &
           package_alt_name,bond=crystal(nph)%bond,std=std,symm=.true.,datas=datas,funit=fcif%handle(),  &
           data_block_name=data_name)
   enddo
   write(fcif%handle(),*)
   if (present(rinfo)) then
       call write_block_datacif(fcif%handle(),datas,EXPORT_POWCIF_RIET,rinfo)
   else
       call write_block_datacif(fcif%handle(),datas,EXPORT_POWCIF_RIET)
   endif
   write(fcif%handle(),*)
   call block_data_ref(fcif%handle(),crystal,datas%wave(1))
   call fcif%fclose()
!
   end subroutine export_phase_cif

!--------------------------------------------------------------------

   subroutine pdf_crystal(pdf,crystal,radtype)
   use crystal_phase
   use pdfcalc
   use molpnew
   type(pdf_type), intent(inout)                 :: pdf
   type(crystal_phase_t), intent(in)             :: crystal
   integer, intent(in)                           :: radtype
   type(element_type), dimension(:), allocatable :: elpdf
   type(atom_type), dimension(:), allocatable    :: atomcr
   !real, dimension(:), allocatable               :: x,y
!
   pdf%rfmax = pdf%rmax
   pdf%rfmin = pdf%rmin
!
   call copy_elem(elpdf,crystal%elem)
   call elem_set_radiation(elpdf,radtype)
!
   pdf%nscat = crystal%natoms()
   call make_crystal(crystal%at,crystal%cell,crystal%spg,pdf%cr_icc,atomcr)
   call thermal_displ(atomcr,crystal%cell)
!
   call pdf%setup(crystal%cell,elpdf,atomcr,radtype)
   call pdf%determine(atomcr,crystal%cell)
!
   end subroutine pdf_crystal

end module cryutil
