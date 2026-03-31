MODULE exportfiles

implicit none

integer, parameter, private   :: FOBS_TYPE = 1, FCALC_TYPE = 2, PHASE_TYPE = 3

integer                       :: nexportf    ! number of files to export
character(250), dimension(10) :: export_name ! name of file
logical, dimension(10)        :: is_export   ! se vero il file e stato gia esportato

CONTAINS

   subroutine intensity_file_for_database(cry, ier, nat, sform, cellpar, icell, &
                                           vol, dens, zval, mu, nrefl, rir, inte, wavelen)
   use crystal_phase
   use prog_constants
   use general, only: lo, is_anomalous
   USE atom_type_util
   USE reflection_type_util
   USE fileutil
   USE ccryst, only: aniso_adp_is_ok, bequiv_from_beta
   type(crystal_phase_t), intent(inout)             :: cry
   integer, intent(out)                             :: ier
   integer, intent(out)                             :: nat
   character(len=:), allocatable, intent(out)       :: sform
   real, dimension(6), intent(out)                  :: cellpar
   integer, dimension(6), intent(out)               :: icell
   real, intent(out)                                :: vol, dens
   integer, intent(out)                             :: zval
   real, intent(out)                                :: mu
   integer, intent(out)                             :: nrefl
   real, intent(out)                                :: rir
   real, dimension(:), allocatable, intent(out)     :: inte
   real, intent(out)                                :: wavelen
   integer                                          :: nghost
   character(len=*), dimension(0:2), parameter      :: subf = ['inorganic    ','organic      ','metallorganic']
   real                                             :: rapp_I,rapp_rho_v
   real, parameter                                  :: maxf_Cu_corundum = 340862.156  ! Calc. Intens. Corundum (from COD cif n. 1000017) b-iso=1.
   real, parameter                                  :: rho_vsq_Cu_corundum = 259079.140625 ! Calc. rho*vol*vol  Corundum  (from COD cif n. 1000017)
   integer, dimension(6)                            :: code
   type(element_type), dimension(:), allocatable    :: chform
   character(len=3)                                 :: adv
   integer                                          :: ierc, i, ng
   integer, parameter                               :: MAXBISO = 100
   real, parameter                                  :: TTHETAMIN = 1.0
   real, parameter                                  :: TTHETAMAX = 150.0
   type(gref_type), dimension(:), allocatable       :: gref
   real                                             :: biso
   integer, parameter                               :: radtype = RX_SOURCE
   real, dimension(1)                               :: wavel = [DEF_WAVE]
!
   ier = 0
   wavelen = wavel(1)
   call cry%make_reflections(TTHETAMIN,TTHETAMAX,wavel)
   if (cry%numref() > 0) then
       nghost = numatomspec(cry%at,0)  ! this is the number of ghost atoms
       nat = cry%natoms() - nghost
       if (nghost > 0) then
           adv = 'no'
           write(lo,'(a)',advance=adv)'BIG TROUBLE: some atoms with Z=0: '
           ng = 0
           do i=1,cry%natoms()
              if (cry%at(i)%get_nz() == 0) then
                  ng = ng + 1
                  if (ng == nghost) then
                      adv = 'yes'
                      write(lo,'(a)',advance=adv)trim(cry%at(i)%lab)
                  else
                      write(lo,'(a)',advance=adv)trim(cry%at(i)%lab)//', '
                  endif
              endif
           enddo
           if (real(nghost) / nat > 0.7.and.nat> 10) nat = 0
       endif
       if (nat > 0) then
           call calcola_occ_new(cry%at,cry%spg,cry%cell%get_g(),0.3)
!
!          check for adp
           do i=1,cry%natoms()
              if (cry%at(i)%get_nz() > 0) then
                  if (cry%at(i)%bij(1) > 0) then
                      biso = bequiv_from_beta(cry%at(i)%bij,cry%cell%get_g())
                  else
                      biso = cry%at(i)%biso
                  endif
                  if (biso <= 0 .or. biso > MAXBISO) then
                      cry%at(i)%bij = 0
                      call set_biso(cry%at(i))
                      !write(0,*)'atom ',trim(cry%at(i)%lab),' set as iso, b=',cry%at(i)%biso
                  endif
              endif
           enddo
           call fcalcang(cry%ref,cry%at,cry%spg,cry%elem,radtype,is_anomalous,anis=.true.)
       else
           cry%ref%fc = 0; cry%ref%ph = 0;
       endif
       call cry%LPcorr(radtype,.false.,1)
!
!      Compute cell parameters
       cellpar = cry%cell%get_par()
       if (.not. spg_check_cell(cry%spg,cellpar)) then
           write(lo,'(a)')'Cell dimensions and spacegroup are non consistent'
       endif
!
!      Generate groups and calculate intensities
       nrefl = cry%numref()
       allocate(inte(nrefl))
       cry%ref(:)%fwhm(1) = 1.0
       call make_ref_groups(cry%ref,gref,0.00005)
       do i=1,size(gref)
          inte(gref(i)%vref(1)) = gref(i)%Ic
          if (gref(i)%code > 1) then
              inte(gref(i)%vref(2:)) = 0
          endif
       enddo
!
       if (nat > 0) then
           dens = cry%density()
           mu = linear_abs_coeff(cry%at,dens,wavel(1))
!
           vol = cry%cell%volume()
           rapp_rho_v = (rho_vsq_Cu_corundum )/(vol*vol*dens)
           rapp_I = maxval(inte)/maxf_Cu_corundum
           !write(6,'(a,f0.8)')'Rho_v2=     ',vol*vol*dens
           rir = rapp_rho_v * rapp_I
           !write(0,'(a,f0.3)')'RIR=   ',rir
!
           zval = Z_value(cry%at,cry%spg%nsymop,chform)
           call chemical_formula(chform,sform,fform=2,ord=.true.,hide1=.true.,hidecharge=.true.)
       else
           rir = 0
           dens = 0
           zval = 0
           mu = 0
           vol = 0
           allocate(character(0) :: sform)
       endif
       if (cry%spg%undef()) then
           cry%spg%csys_code = csys_from_cell(cellpar)
       endif
       call spg_get_cell_code(cry%spg,cellpar,code,ierc)
       icell(:) = 1
       do i=1,6
          if(code(i) /= 0) icell(i) = 0
       enddo
!
!      --- Original file writing (commented) ---
!      call fileh%fopen(filename,ios='w')
!      if (fileh%good()) then
!!
!!          Generate groups and calculate intensities
!!           nrefl = cry%numref()
!!           allocate(inte(nrefl))
!!           cry%ref(:)%fwhm(1) = 1.0
!!           call make_ref_groups(cry%ref,gref,0.00005)
!!           do i=1,size(gref)
!!              inte(gref(i)%vref(1)) = gref(i)%Ic
!!              if (gref(i)%code > 1) then
!!                  inte(gref(i)%vref(2:)) = 0
!!              endif
!!           enddo
!!
!!           if (nat > 0) then
!!                dens = cry%density()
!!                mu = linear_abs_coeff(cry%at,dens,wavel(1))
!!
!!                vol = cry%cell%volume()
!!               rapp_rho_v = (rho_vsq_Cu_corundum )/(vol*vol*dens)
!!               rapp_I = maxval(inte)/maxf_Cu_corundum
!!               !write(6,'(a,f0.8)')'Rho_v2=     ',vol*vol*dens
!!               rir = rapp_rho_v * rapp_I
!!               !write(0,'(a,f0.3)')'RIR=   ',rir
!!
!!               zval = Z_value(cry%at,cry%spg%nsymop,chform)
!!               call chemical_formula(chform,sform,fform=2,ord=.true.,hide1=.true.,hidecharge=.true.)
!!
!!               write(fileh%handle(),'(1x,a,a)')'FORMULA: ',trim(sform)
!!               write(fileh%handle(),'(1x,a,a)')'SUBFILE: ',subf(is_organic(cry%elem%z,nint(cry%elem%nw)))
!!           else
!!               rir = 0
!!               dens = 0
!!               zval = 0
!!               mu = 0
!!               write(fileh%handle(),*) 'SUBFILE: undefined'
!!           endif
!!           if (cry%spg%undef()) then
!!               cry%spg%csys_code = csys_from_cell(cellpar)
!!           endif
!!           call spg_get_cell_code(cry%spg,cellpar,code,ierc)
!!           icell(:) = 1
!!           do i=1,6
!!              if(code(i) /= 0) icell(i) = 0
!!           enddo
!!           write(fileh%handle(),'(1x,a,3(1x,f0.4),3(1x,f0.3))')    'CELL:    ',cellpar
!!           write(fileh%handle(),'(1x,a,6i6)')    'WRITE CELL: ',icell(1:6)
!!           write(fileh%handle(),'(1x,a,a)')    'SPG:     ',trim(cry%spg%symbol_xhm)
!!           if (cry%spg%csys_code == CS_Trigonal) then
!!               if (uc_is_rhombohedral(cellpar,0.01)) then
!!                   write(fileh%handle(),'(1x,a,a)')    'CRY SYS: ',trim(cry_sys(cry%spg%csys_code))//' (rhombohedral axes)'
!!               elseif (uc_is_hexagonal(cellpar,0.01)) then
!!                   write(fileh%handle(),'(1x,a,a)')    'CRY SYS: ',trim(cry_sys(cry%spg%csys_code))//' (hexagonal axes)'
!!               endif
!!           else
!!               write(fileh%handle(),'(1x,a,a)')    'CRY SYS: ',trim(cry_sys(cry%spg%csys_code))
!!           endif
!!           write(fileh%handle(),'(1x,a,f0.3)')   'VOLUME : ',vol
!!           write(fileh%handle(),'(1x,a,f0.3,a)') 'Density: ',dens,' g cm-3'
!!           write(fileh%handle(),'(1x,a,i5)')     'Z : ',zval
!!           write(fileh%handle(),'(1x,a,f0.3,a)') 'mu(CuKa):',mu,' cm-1'
!!           write(fileh%handle(),'(1x,a,i0)')   'NAtoms: ',nat
!!           write(fileh%handle(),'(1x,a,i0)')   'NReflections: ',nrefl
!!           write(fileh%handle(),'(1x,a,f0.3)')   'RIR     :',rir
!!           write(fileh%handle(),'(1x,a)') '----------------------------------------------------------------------------- '
!!           write(fileh%handle(),'(1x,a)') '  Remarks: Diffraction pattern calculated by EXPO from COD database cif file'
!!           write(fileh%handle(),'(1x,a)') '  Remarks: RIR calculated by EXPO     '
!!           write(fileh%handle(),'(1x,a)') '----------------------------------------------------------------------------- '
!!
!!           call write_reflections(jfile=fileh%handle(),refl=cry%ref,code=5,   &
!!                                  nref=min(NMAXREFLD,nrefl),intensity=inte,wave=wavel(1),sortby=ORD_BY_INT)
!!           call fileh%fclose()
!!      endif
   else
       ier = 2
   endif

   end subroutine intensity_file_for_database

!----------------------------------------------------------------------

   subroutine write_intensity_file(filename, nat, sform, elem, cellpar, icell, &
                                   spg_symbol, csys_code, vol, dens, &
                                   zval, mu, nrefl, rir, ref, inte, wavelen)
   USE atom_type_util
   USE elements, only: element_type
   USE reflection_type_util
   USE fileutil
   USE spginfom, only: uc_is_rhombohedral, uc_is_hexagonal, cry_sys, CS_Trigonal
   character(len=*), intent(in)                                  :: filename
   integer, intent(in)                                           :: nat
   character(len=*), intent(in)                                  :: sform
   type(element_type), dimension(:), allocatable, intent(in)     :: elem
   real, dimension(6), intent(in)                                :: cellpar
   integer, dimension(6), intent(in)                             :: icell
   character(len=*), intent(in)                                  :: spg_symbol
   integer, intent(in)                                           :: csys_code
   real, intent(in)                                              :: vol, dens
   integer, intent(in)                                           :: zval
   real, intent(in)                                              :: mu
   integer, intent(in)                                           :: nrefl
   real, intent(in)                                              :: rir
   type(reflection_type), dimension(:), allocatable, intent(in)  :: ref
   real, dimension(:), intent(in)                                :: inte
   real, intent(in)                                              :: wavelen
   type(file_handle)                                             :: fileh
   character(len=*), dimension(0:2), parameter                   :: subf = ['inorganic    ','organic      ','metallorganic']
   integer, parameter                                            :: NMAXREFLD = 500
!
   call fileh%fopen(filename,ios='w')
   if (fileh%good()) then
       if (nat > 0) then
           write(fileh%handle(),'(1x,a,a)')'FORMULA: ',trim(sform)
           write(fileh%handle(),'(1x,a,a)')'SUBFILE: ',subf(is_organic(elem%z,nint(elem%nw)))
       else
           write(fileh%handle(),*) 'SUBFILE: undefined'
       endif
       write(fileh%handle(),'(1x,a,3(1x,f0.4),3(1x,f0.3))')    'CELL:    ',cellpar
       write(fileh%handle(),'(1x,a,6i6)')    'WRITE CELL: ',icell(1:6)
       write(fileh%handle(),'(1x,a,a)')    'SPG:     ',trim(spg_symbol)
       if (csys_code == CS_Trigonal) then
           if (uc_is_rhombohedral(cellpar,0.01)) then
               write(fileh%handle(),'(1x,a,a)')    'CRY SYS: ',trim(cry_sys(csys_code))//' (rhombohedral axes)'
           elseif (uc_is_hexagonal(cellpar,0.01)) then
               write(fileh%handle(),'(1x,a,a)')    'CRY SYS: ',trim(cry_sys(csys_code))//' (hexagonal axes)'
           endif
       else
           write(fileh%handle(),'(1x,a,a)')    'CRY SYS: ',trim(cry_sys(csys_code))
       endif
       write(fileh%handle(),'(1x,a,f0.3)')   'VOLUME : ',vol
       write(fileh%handle(),'(1x,a,f0.3,a)') 'Density: ',dens,' g cm-3'
       write(fileh%handle(),'(1x,a,i5)')     'Z : ',zval
       write(fileh%handle(),'(1x,a,f0.3,a)') 'mu(CuKa):',mu,' cm-1'
       write(fileh%handle(),'(1x,a,i0)')   'NAtoms: ',nat
       write(fileh%handle(),'(1x,a,i0)')   'NReflections: ',nrefl
       write(fileh%handle(),'(1x,a,f0.3)')   'RIR     :',rir
       write(fileh%handle(),'(1x,a)') '----------------------------------------------------------------------------- '
       write(fileh%handle(),'(1x,a)') '  Remarks: Diffraction pattern calculated by EXPO from COD database cif file'
       write(fileh%handle(),'(1x,a)') '  Remarks: RIR calculated by EXPO     '
       write(fileh%handle(),'(1x,a)') '----------------------------------------------------------------------------- '
!
       call write_reflections(jfile=fileh%handle(),refl=ref,code=5,   &
                              nref=min(NMAXREFLD,nrefl),intensity=inte,wave=wavelen,sortby=ORD_BY_INT)
       call fileh%fclose()
   endif

   end subroutine write_intensity_file

END MODULE exportfiles
