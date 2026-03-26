MODULE exportfiles

implicit none

integer, parameter, private   :: FOBS_TYPE = 1, FCALC_TYPE = 2, PHASE_TYPE = 3

integer                       :: nexportf    ! number of files to export
character(250), dimension(10) :: export_name ! name of file
logical, dimension(10)        :: is_export   ! se vero il file e stato gia esportato 

CONTAINS
   
   subroutine intensity_file_for_database(crystal,filename,ier)
   use crystal_phase
   use prog_constants
   use general, only: lo
!   USE General, only:nrefl,lo,cell,spacegr,celem,neutro,nsyn,alambda,is_anomalous
!   USE patternref, only:thmin,thmax
!   USE atom_type_util
!   USE reflection_type_util
!   USE variables, only:atm
!   USE proginterface
!   USE fileutil
!   USE elements
!   USE spginfom
!   USE strutil
!   USE ccryst, only: aniso_adp_is_ok, bequiv_from_beta
!   USE unit_cell
!   USE datamod
!   use variables, only: dataset
!   use datasetmod
!   use filereading
!   use counts
!   use arrayutil
   type(crystal_phase_t), intent(inout)             :: crystal
   character(len=*), intent(in)                     :: filename
   integer, intent(out)                             :: ier
!   type(reflection_type), dimension(:), allocatable :: reflb
   integer                                          :: nat,nghost
!   type(file_handle)                                :: fileh
!   character(len=*), dimension(0:2), parameter      :: subf = ['inorganic    ','organic      ','metallorganic']
!   character(len=:), allocatable                    :: sform
!   real                                             :: mu, dens, vol
!   integer                                          :: zval,i,ng
!   integer, dimension(6)                            :: icell
!   real                                             :: rapp_I,rapp_rho_v,rir
!   type(cell_type)                                  :: cellt
!   real, parameter                                  :: maxf_Cu_corundum = 340862.156  ! Calc. Intens. Corundum (from COD cif n. 1000017) b-iso=1.
!   real, parameter                                  :: rho_vsq_Cu_corundum = 259079.140625 ! Calc. rho*vol*vol  Corundum  (from COD cif n. 1000017)
!   integer, dimension(6)                            :: code
!   type(element_type), dimension(:), allocatable    :: chform
!   character(len=3)                                 :: adv
!   integer                                          :: ierc,iflag
!   integer, parameter                               :: MAXBISO = 100
    real, parameter                                  :: TTHETAMIN = 1.0
    real, parameter                                  :: TTHETAMAX = 150.0
!   integer, parameter                               :: NMAXREFLD = 500
!   real, dimension(:), allocatable                  :: inte
!   type(gref_type), dimension(:), allocatable       :: gref
!   real                                             :: biso
!   logical :: laue = .false.
!   integer :: sortyp
!!   
!   icell(:) = 1
   ier = 0
!   if (nrefl == 0) then
!       if (alambda == 0) alambda = 1.54056
!       if (ndataset(dataset) == 0) then
!           thmin = 1.0; thmax = TTHETAMAX
!       else
!           thmin = dataset(1)%tmin; thmax = dataset(1)%tmax
!       endif
!       if (laue) then
!           if (spacegr%laue_code == 0) return
!           spacegr = init_spaceg_type('P '//spacegr%laue_class())
!           thmax = thvalue(1.3,alambda)
!       endif
!       call gener([alambda],iflag,.false.)
        call crystal%make_reflections(TTHETAMIN,TTHETAMAX,[DEF_WAVE])
        write(0,*)'Number of reflections: ',crystal%numref()
!   endif
   if (crystal%numref() > 0) then
       nghost = numatomspec(crystal%at,0)  ! this is the number of ghost atoms
       nat = crystal%natoms() - nghost
       if (nghost > 0) then
           adv = 'no'
           write(lo,'(a)',advance=adv)'BIG TROUBLE: some atoms with Z=0: '
           ng = 0
           do i=1,crystal%natoms()
              if (crystal%at(i)%get_nz() == 0) then
                  ng = ng + 1
                  if (ng == nghost) then
                      adv = 'yes'
                      write(lo,'(a)',advance=adv)trim(crystal%at(i)%lab)
                  else
                      write(lo,'(a)',advance=adv)trim(crystal%at(i)%lab)//', '
                  endif
              endif
           enddo
           if (real(nghost) / nat > 0.7.and.nat> 10) nat = 0
       endif
!       allocate(reflb(nrefl))
!       call fillref(reflb)
       if (nat > 0) then
!           cellt = set_cell_type(cell)
           call calcola_occ_new(crystal%at,crystal%spg,crystal%cell%get_g(),0.3)
!          check for adp
!           do i=1,nat
!              if (atm(i)%bij(1) > 0) then
!                  biso = bequiv_from_beta(atm(i)%bij,cellt%get_g())
!              else
!                  biso = atm(i)%biso
!              endif
!              if (biso <= 0 .or. biso > MAXBISO) then
!                  atm(i)%bij = 0
!                  call set_biso(atm(i))
!                  !write(0,*)'atom ',trim(atm(i)%lab),' set as iso, b=',atm(i)%biso
!              endif
!           enddo
!           call fcalcang(reflb,atm,spacegr,celem,neutro,is_anomalous,anis=.true.)
       else
!           !ier = 1
!           !return
!           reflb%fc = 0; reflb%ph = 0;
       endif
!       reflb(:)%lp(1) = lp_correction(reflb%tthd(1),neutro,.false.)
!       call fileh%fopen(filename,ios='w')
!       if (fileh%good()) then
!           vol = cell_volume(cell)
!           if (.not. spg_check_cell(spacegr,cell)) then
!               write(lo,'(a)')'Cell dimensions and spacegroup are non consistent'
!           endif
!!
!!          Generate groups and calculate intensities
!           allocate(inte(nrefl))
!           if (laue) then
!               inte(:) = reflb%m*reflb%lp(1)*reflb%fc**2
!           else
!               reflb(:)%fwhm(1) = 1.0     
!               call make_ref_groups(reflb,gref,0.00005)
!               do i=1,size(gref)
!                  inte(gref(i)%vref(1)) = gref(i)%Ic
!                  if (gref(i)%code > 1) then 
!                      inte(gref(i)%vref(2:)) = 0
!                  endif
!               enddo
!           endif
!!
!           if (nat > 0) then
!               dens = density_value(molecular_weight(atm%ptab,atm%och*atm%ocry),vol,spacegr%nsymop)
!               mu = linear_abs_coeff(atm,dens,alambda)
!!
!               rapp_rho_v = (rho_vsq_Cu_corundum )/(vol*vol*dens)
!               rapp_I = maxval(inte)/maxf_Cu_corundum
!               write(6,'(a,f0.8)')'Rho_v2=     ',vol*vol*dens
!               rir = rapp_rho_v * rapp_I
!               !write(0,'(a,f0.3)')'RIR=   ',rir
!!
!               zval = Z_value(atm,spacegr%nsymop,chform)
!               call chemical_formula(chform,sform,fform=2,ord=.true.,hide1=.true.,hidecharge=.true.)
!!               
!               write(fileh%handle(),'(1x,a,a)')'FORMULA: ',trim(sform)
!               write(fileh%handle(),'(1x,a,a)')'SUBFILE: ',subf(is_organic(celem%z,nint(celem%nw)))
!           else
!               rir = 0
!               dens = 0
!               zval = 0
!               mu = 0
!               write(fileh%handle(),*) 'SUBFILE: undefined'
!           endif
!           if (spacegr%undef()) then
!               spacegr%csys_code = csys_from_cell(cell)
!           endif
!           call spg_get_cell_code(spacegr,cell,code,ierc)
!           do i=1,6
!              if(code(i).ne.0) icell(i) = 0
!           enddo
!           write(fileh%handle(),'(1x,a,3(1x,f0.4),3(1x,f0.3))')    'CELL:    ',cell(1:6)
!           write(fileh%handle(),'(1x,a,6i6)')    'WRITE CELL: ',icell(1:6)
!           write(fileh%handle(),'(1x,a,a)')    'SPG:     ',trim(spacegr%symbol_xhm)
!           if (spacegr%csys_code == CS_Trigonal) then
!               if (uc_is_rhombohedral(cell,0.01)) then
!                   write(fileh%handle(),'(1x,a,a)')    'CRY SYS: ',trim(cry_sys(spacegr%csys_code))//' (rhombohedral axes)'
!               elseif (uc_is_hexagonal(cell,0.01)) then
!                   write(fileh%handle(),'(1x,a,a)')    'CRY SYS: ',trim(cry_sys(spacegr%csys_code))//' (hexagonal axes)'
!               endif
!           else
!               write(fileh%handle(),'(1x,a,a)')    'CRY SYS: ',trim(cry_sys(spacegr%csys_code))
!           endif
!           write(fileh%handle(),'(1x,a,f0.3)')   'VOLUME : ',vol
!           write(fileh%handle(),'(1x,a,f0.3,a)') 'Density: ',dens,' g cm-3'
!           write(fileh%handle(),'(1x,a,i5)')     'Z : ',zval
!           write(fileh%handle(),'(1x,a,f0.3,a)') 'mu(CuKa):',mu,' cm-1'
!           write(fileh%handle(),'(1x,a,i0)')   'NAtoms: ',nat
!           write(fileh%handle(),'(1x,a,i0)')   'NReflections: ',nrefl
!           write(fileh%handle(),'(1x,a,f0.3)')   'RIR     :',rir
!           write(fileh%handle(),'(1x,a)') '----------------------------------------------------------------------------- '
!           write(fileh%handle(),'(1x,a)') '  Remarks: Diffraction pattern calculated by EXPO from COD database cif file'
!           write(fileh%handle(),'(1x,a)') '  Remarks: RIR calculated by EXPO     '
!           write(fileh%handle(),'(1x,a)') '----------------------------------------------------------------------------- '
!!
!           if (laue) then
!               sortyp = ORD_BY_2THETA
!           else
!               sortyp = ORD_BY_INT
!           endif
!           call write_reflections(jfile=fileh%handle(),refl=reflb,code=5,   &
!                                  nref=min(NMAXREFLD,nrefl),intensity=inte,wave=alambda,sortby=sortyp)
!           call fileh%fclose()
!       endif
   else
       ier = 2
   endif

   end subroutine intensity_file_for_database

END MODULE exportfiles
