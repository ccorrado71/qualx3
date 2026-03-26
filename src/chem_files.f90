MODULE compare_struct

USE atom_basic, only: atom_type

implicit none

type compare_info_type
   integer                                    :: nfound     ! numero di trovati nel modello 2
   real                                       :: distmin    ! distanza minima
   real                                       :: distm      ! distanza media
   real                                       :: rmsd       ! RMSD calculated on all atoms
   real                                       :: rmsd_nonH  ! RMSD calculated on all non H-atoms
   integer, dimension(:), allocatable         :: nv1        ! atomo 1 dell'associazione
   integer, dimension(:), allocatable         :: nv2        ! aromo 2 dell'associazione
   real, dimension(:), allocatable            :: vdist      ! distanza dell'associazione
   real                                       :: errph      ! errore di fase
   integer                                    :: nref       ! numero di riflessi per l'errore di fase
   real, dimension(4)                         :: shift      ! shift applicato
   type(atom_type), dimension(:), allocatable :: at1        ! modello 1
   type(atom_type), dimension(:), allocatable :: at2        ! modello 2
   integer                                    :: nass       ! numero di atomi associati nel modello 1
end type compare_info_type

CONTAINS

   subroutine to_coord_sir(kdstampa,atom,refl,numb,cell,spg,kpr,atmp)
   USE PhaseModule, only: shiftexam,isumal,erore
   USE TestModel, only: vett_test,distv,npunt,rmsd,distCoord
   USE reflection_type_util
   USE arrayutil
   USE atom_type_util
   USE unit_cell
   USE cgeom
   USE spginfom
   USE prognames
 
   type(atom_type), dimension(:), allocatable, intent(in)       :: atom
   type(reflection_type), dimension(:), allocatable, intent(in) :: refl
   integer, intent(in)                                          :: kdstampa,numb
   type(cell_type), intent(in)                                  :: cell
   type(spaceg_type), intent(in)                                :: spg
   integer, intent(in)                                          :: kpr
   type(atom_type), dimension(:), allocatable, intent(in)       :: atmp
   integer                                                      :: nref
   type(atom_type), dimension(:), allocatable                   :: atmsir
   character(len=1)                                             :: segnochar
   real xn(3), xo(3), xeq(3), s(3), xyzMin(3)
   real, allocatable, dimension(:) :: distRms
   integer                         :: i,j,k,l 
   integer                         :: izero,ifmin,nbru,ngost
   integer                         :: natoms,jpunt
   real                            :: dmedia,distm,distmA,adistmean,d
   integer                         :: natom,nCoordPub,nrmsd
!
   natom = numatoms(atom)
   if (natom == 0) return
   call copy_atoms(atmsir,atom)
   !if (present(atmp)) call copy_atoms(atmpub,atmp)
!
! --- calcolo gli shift
!
   shiftexam(1:7) = 0.0
   shiftexam(4)   = 1.0
   izero = 0
   ifmin = kdstampa 
   ifmin = 0
   if (kdstampa /= 99 ) then
       nbru = 4321
       nref = numrefl(refl)
!not implemented       call examin(nbru,[(refl(i)%hkl,i=1,nref)],refl%phv,refl%ph,numb,izero,[(1.0,i=1,nref)],ifmin)
   endif
!
! --- ngost = numero di ghost tra i picchi SIR
!
   ngost = count(atom%z() == 0)
!
! --- applica lo shift alle coordinate SIR
   do j=1,natom
      do i=1,3
         if (shiftexam(4).gt.0.0) then
             atmsir(j)%xc(i)= atom(j)%xc(i) - shiftexam(i)
         else
             atmsir(j)%xc(i) = - ( atom(j)%xc(i) + shiftexam(i))
         endif
      enddo
   enddo
!
! --- calcola le distanze minime
!
   natoms = 0
   dmedia = 0.0
   nCoordPub = numatoms(atmp)
   allocate(distRms(nCoordPub),source=-1.0)
   call new_array(npunt,natom)
   call new_array(distv,natom)
   npunt(:) = 0
   do i=1, nCoordPub
      xo(1:3) = atmp(i)%xc
      distm=9999
      jpunt = 0
      distmA=9999
      do j=1,natom
         if (npunt(j) == 0) then
             xn(1:3) = atmsir(j)%xc
             do k=1,spg%nsymop
                xeq = atom_symm(xn,spg%symop(k))
                call xdisteqs(xo,xeq,cell%get_g(),d,s)
                if (d < distmA) then
                    distmA=d
                    distRms(i) = distmA
                endif
                if (d < distm .and. d < distCoord) then
                    distm=d
                    jpunt=j
                    xyzMin(1:3) = xeq(1:3)
                endif
             enddo
         endif
      enddo
      if (jpunt.ne.0) then
          npunt(jpunt) = i
          distv(jpunt) = distm
          call PosizioneAssoluta(xyzMin, xo, cell%get_g())
          atmsir(jpunt)%xc = xyzMin(1:3)
          natoms = natoms + 1
          dmedia = dmedia + distm
      endif
   enddo
   nrmsd = count(distRms >= 0.0) ! possibile if nCoordPub > natom
   distRms(:nrmsd) = pack(distRms, mask=distRms >= 0.0)
   rmsd = sqrt(dot_product(distRms(:nrmsd),distRms(:nrmsd))/nrmsd)
   if (natoms > 0) then
       adistmean = dmedia / real(natoms)
   else
       adistmean = 0.0
   endif
   if (kdstampa >= 0) then
       if (shiftexam(4) > 0.0) then
           segnochar = '+'
       else
           segnochar = '-'
       endif
       write(kpr,100) package_name,natom, nCoordPub, natoms, shiftexam(1:3),          &
            segnochar, erore, isumal, distCoord, adistmean, rmsd
  100  format(a,' coordinates: ',i6,/,                                    &
            'Model coordinates:',i6,/,                                    &
            'Matches found:    ',i6,/,                                    &
            'Shift applied:    ',3f6.3,'  Sign is ',a1,/,                 &
            'Mean Phase Error: ',f6.3,' using ',i8,' reflections',/,      &
            'Distance limit:   ',f6.3,/,                                  &
            '<Dist>:           ',f6.3,/,                                  &
            'RMSD:             ',f6.3)
   endif
   vett_test(1) = nCoordPub
   vett_test(2) = natom
   vett_test(3) = natoms
   vett_test(4) = adistmean
   if (kdstampa > 0 .and. natoms > 0) then
      if (kdstampa /= 2) then
          !write(kpr,200)
          write(kpr,'(a)')'----- '//package_name//' -----    Distance  --- Model coordinates ---'
      else
          !write(kpr,210)
          write(kpr,'(a)')'  --- '//package_name//' peaks ---------              ------ publ. coord. -------    distance'
      endif
      do j=1,natom
         if (npunt(j).ne.0) then
             l = npunt(j)
             if (kdstampa /= 2) then
                 write(kpr,301)   &
                 atmsir(j)%lab,atmsir(j)%xc(:),distv(j),(atmp(l)%xc(k),k=1,3),trim(atmp(l)%lab)
             else
                 write(kpr,310)  &
                 j,atmsir(j)%xc,atmsir(j)%inte,atmp(l)%lab(1:8),l,(atmp(l)%xc(k),k=1,3),distv(j)
             endif
         endif
      enddo
   endif
  301 format(a8,3f7.3,f10.5,2x,3f7.3,3x,a)
  310 format(i6,2x,3f6.3,f10.0,3x,a8,i6,2x,3f6.3,f10.5)
!
   return
!
   return
   end subroutine to_coord_sir
!----------------------------------------------------------

   subroutine PosizioneAssoluta(xyzMin, xyzP, gmat)
   USE cgeom
!
!--moves xyzMin close to xyzP deleting cell translations
!
   implicit none
   integer i1, i2, i3
   real trasla(3), zn(3), s(3), dm, xyzMin(3), xyzP(3)
   real dist
   real, dimension(3,3), intent(in) :: gmat
!
   dm = 9999
   do i1=-2,2
      trasla(1) = i1
      do i2=-2,2
         trasla(2) = i2
         do i3=-2,2
            trasla(3) = i3
            zn(1:3) = xyzMin(1:3) + trasla(1:3)
            !d(1:3) = zn(1:3) - xyzP(1:3)
            !dist = cdist(g,d,dist2)
            dist = distanzac(zn,xyzP,gmat)
            if (dist < dm) then
                dm = dist
                s(1:3) =  zn(1:3)
            endif
         enddo
      enddo
   enddo
!
   xyzMin(1:3) = s(1:3)
!
   return
   end subroutine PosizioneAssoluta
  
!-------------------------------------------------------------------------------------------------------  

   subroutine drawup_models(atom1,legm1,atom2,legm2,cell,spg,shifte,kfix,kenant,kinorg)
!
!  Avvicina il modello atom2 al modello atom1. atom1 viene mantenuto fisso se kfix e' true
!
   USE connect_mod
   USE fragmentmod
   USE atom_type_util
   USE molpnew
   USE unit_cell
   USE spginfom
   type(atom_type), dimension(:), intent(inout)              :: atom1    ! modello fisso
   type(bond_type), dimension(:), allocatable, intent(in)    :: legm1    ! legami del modello fisso
   type(atom_type), dimension(:), intent(inout)              :: atom2    ! modello da avvicinare
   type(bond_type), dimension(:), allocatable, intent(inout) :: legm2    ! legami modello da avvicinare
   type(cell_type), intent(in)                               :: cell
   type(spaceg_type), intent(in)                             :: spg
   real, dimension(:), intent(in), optional                  :: shifte   ! shift (usa examin)
   logical, intent(in), optional                             :: kfix     ! se true atom1 viene fissato
   logical, intent(in), optional                             :: kenant   ! se true puoi eseguire l'inversione
   logical, intent(in), optional                             :: kinorg   ! se true inorganic fragment are disconnected
   logical                                                   :: kfix1,kenant1,kinorg1
   type(atom_type), dimension(:), allocatable                :: atomu
   integer                                                   :: nat1,nat2
   type(fragment_type), dimension(:), allocatable            :: frag1,frag2   !!!!!,frag3
   integer                                                   :: nfrag1,nfrag2
   integer                                                   :: i,j
   integer                                                   :: ninorg
   logical                                                   :: force_overlay = .false.
!
   nat1 = size(atom1)
   nat2 = size(atom2)
!
   if (present(kenant)) then
       kenant1 = kenant
   else
       kenant1 = .true.
   endif
!
   if (present(kinorg)) then
       kinorg1 = kinorg
   else
       kinorg1 = .true.
   endif
!
!  Applica shift ad atom2
   if (present(shifte)) then
       if (shifte(4) > 0) then
           call translate_atoms(atom2,-shifte(1:3))
       else
           if (spg%symcent == 1 .or. kenant1) then   ! puoi passare all'enantiomorfo
               do i=1,nat2
                  atom2(i)%xc = -(atom2(i)%xc + shifte(1:3))
               enddo
           else   
               call translate_atoms(atom2,-shifte(5:7))
           endif
       endif
   endif
!
!  Il modello potrebbe andare fuori cella!
   call translate_in_cell(atom2)
!
   if (present(kfix)) then
       kfix1 = kfix
   else
       kfix1 = .false.
   endif
!
!  Unisci i 2 modelli
   allocate(atomu(nat1+nat2))
   atomu(:nat1) = atom1(:)
   atomu(nat1+1:) = atom2(:)
!
!  estrai frammenti da atom1
   call get_fragments(atom1,cell,legm1,nfrag1,frag1)
   if (kfix1) then
       do i=1,nfrag1
          frag1(i)%rcod(:3) = 0  ! blocca i frammenti di atom1
       enddo
   endif
!
   ninorg = 0
   if (force_overlay) then
!
!      Make a fragment for each atom
       do j=1,size(atom2)
          call add_fragment(frag2,atom2,[j])
       enddo
       nfrag2 = numfragments(frag2)
   else
!
!      build fragments from atom2
       call get_fragments(atom2,cell,legm2,nfrag2,frag2)
!
       if (kinorg1) then
!
!          Delete connectivity for inorganic fragment
           do i=1,nfrag2
              if (frag2(i)%nat > 1 .and. is_organic(atom2(frag2(i)%pos)) == 0) then
                  ninorg = ninorg + 1
                  call remove_bond_from_atom(legm2,frag2(i)%pos)
                  do j=1,frag2(i)%nat
                     call add_fragment(frag2,atom2,[frag2(i)%pos(j)])
                  enddo
                  frag2(i)%nat = 0  ! frag2(i) will be removed
              endif
           enddo
!       
!          pack the array to exclude fragments with nat == 0     
           if (ninorg > 0) then
               nfrag2 = count(frag2%nat > 0) 
               frag2(:nfrag2) = pack(frag2, mask=frag2%nat > 0)
           endif
       endif
   endif
!
   call overlay_fragment(frag1,frag2(:nfrag2),atom1,atom2,cell,spg)
   if (ninorg > 0 .or. force_overlay) call create_connectivity(atom2,legm2,cell,spg)
!!
!!  export both models if .mol file
!   if (kexport) then
!       call copia_legm(legm,legm1)
!       call combine_legm(legm,legm2,nat1)
!       allocate(atomu(nat1+nat2))
!       atomu(:nat1) = atom1(:)
!       atomu(nat1+1:) = atom2(:)
!       call create_molfile(trim(StructureName)//'_comp.mol',cella=cell,atom=atomu,legm=legm,progname=package_alt_name,error=error)
!   endif
!!
   end subroutine drawup_models
   
!---------------------------------------------------------------------      

   subroutine overlay_models(atom1,legm1,atom2,legm2,ref,cell,spg,elem,wave,wavetype,anomal)
!
!  Sovrappone atom2 su atom1
!
   USE connect_mod
   USE PhaseModule, only:shiftexam
   USE reflection_type_util
   USE Counts
   USE unit_cell
   USE spginfom
   USE elements
   type(atom_type), dimension(:), allocatable, intent(inout)    :: atom1
   type(atom_type), dimension(:), allocatable, intent(inout)    :: atom2
   type(bond_type), dimension(:), allocatable, intent(in)       :: legm1   !!!!,legm2
   type(bond_type), dimension(:), allocatable, intent(inout)    :: legm2
   type(reflection_type), dimension(:), allocatable, intent(in) :: ref
   type(cell_type), intent(in)                                  :: cell
   type(spaceg_type), intent(in)                                :: spg
   type(element_type), dimension(:), allocatable, intent(in)    :: elem
   real, intent(in)                                             :: wave
   integer, intent(in)                                          :: wavetype
   logical, intent(in)                                          :: anomal
   type(reflection_type), dimension(:), allocatable             :: ref1
   type(reflection_type), dimension(:), allocatable             :: ref2
   integer                                                      :: i
   integer                                                      :: ier
   real, parameter                                              :: DMAX = 1.7
   integer                                                      :: nref
   integer, dimension(3)                                        :: ihmx
!
!  Genera i riflessi se non esistono
   ier = 0
   if (numrefl(ref) == 0) then
       call create_reflections(0.0,thvalue(DMAX,wave),cell%get_par(),spg,[wave],ref1,nref,ihmx)
       if (nref == 0) ier = 1
   else
       call copy_ref(ref1,ref)
       nref = numrefl(ref1)
   endif
!
!  Calcolo dello shift di origine tra i 2 modelli
   if (ier == 0) then
       allocate(ref2(nref))
       do i=1,nref
          ref2(i) = ref1(i)
       enddo
       call fcalcang(ref1,atom1,spg,elem,wavetype,anomal)  ! calcola fasi del modello 1
       call fcalcang(ref2,atom2,spg,elem,wavetype,anomal)  ! calcola fasi del modello 2
       call examing(ref1,ref2)
   else
       shiftexam(1:7) = 0; shiftexam(4) = 1
   endif
!
!  Sovrappono i modelli
   call drawup_models(atom1,legm1,atom2,legm2,cell,spg,shiftexam,kfix=.true.)
!
   end subroutine overlay_models

!--------------------------------------------------------------------------------------

   subroutine examing(ref1,ref2)
!
!  Calcola errore di phase (erore) e shift di fase (shiftexam)
!
   USE reflection_type_util
   type(reflection_type), dimension(:), intent(in) :: ref1,ref2
   integer                                         :: nrefex
   integer                                         :: i,j
   integer, dimension(3,size(ref1))                :: khl
   integer                                         :: kpr
!
   nrefex = size(ref1)
   do i=1,nrefex
      khl(:,i) = ref1(i)%hkl
   enddo
   kpr = 0
!corr not-implemented   call examin(1,khl,ref1(:)%ph,ref2(:)%ph,nrefex,0,(/(1.0,j=1,nrefex)/),kpr)
!
   end subroutine examing

!--------------------------------------------------------------------------------------

   subroutine compare_models(atom1,atom2,ref,spg,cell,elem,wave,wavetype,anomal,dist,kpr,infoc)
!
!  Confronta 2 modelli. Nella stampa solo le coordinate di atom1 vengono modificate.
!
   USE atom_type_util
   USE PhaseModule, only:shiftexam,erore
   USE reflection_type_util
   USE cgeom
   USE counts
   USE unit_cell
   USE spginfom
   USE elements
   type(atom_type), dimension(:), allocatable, intent(in)       :: atom1
   type(atom_type), dimension(:), allocatable, intent(in)       :: atom2
   type(reflection_type), dimension(:), allocatable, intent(in) :: ref
   type(spaceg_type), intent(in)                                :: spg
   type(cell_type), intent(in)                                  :: cell
   type(element_type), dimension(:), allocatable, intent(in)    :: elem
   real, intent(in)                                             :: wave
   integer, intent(in)                                          :: wavetype
   logical, intent(in)                                          :: anomal
   real, optional, intent(in)                                   :: dist
   integer, intent(in), optional                                :: kpr
   type(compare_info_type), optional, intent(out)               :: infoc
   type(reflection_type), dimension(:), allocatable             :: ref1
   type(reflection_type), dimension(:), allocatable             :: ref2
   integer                                                      :: i,j,k
   integer                                                      :: nat1,nat2
   integer, dimension(:), allocatable                           :: npunt
   real                                                         :: distm
   integer                                                      :: jpunt
   real, dimension(3,spg%nsymop)                                :: xeq
   real                                                         :: dd
   real, dimension(3)                                           :: ktra,ktramin
   type(compare_info_type)                                      :: info
   integer                                                      :: kprint
   real, dimension(:), allocatable                              :: distrmsd
   integer                                                      :: ier
   real, parameter                                              :: DMAX = 1.7
   integer                                                      :: nref
   logical :: rmsdset
   integer :: nat_rmsd, nat_rmsda
   integer, dimension(3) :: ihmx
!
   nat1 = numatoms(atom1)
   nat2 = numatoms(atom2)
   if (nat1 == 0 .or. nat2 == 0) return
!
   allocate(npunt(nat2),distrmsd(nat1))
   call init_compare_info_type(info,nat1)
   call copy_atoms(info%at2,atom2)
   call copy_atoms(info%at1,atom1)
!
!  Genera i riflessi se non esistono
   ier = 0
   if (numrefl(ref) == 0) then
       call create_reflections(0.0,thvalue(DMAX,wave),cell%get_par(),spg,[wave],ref1,nref,ihmx)
       if (nref == 0) ier = 1
   else
       call copy_ref(ref1,ref)
       nref = numrefl(ref1)
   endif
!
!  Calcola shift di fase
   if (ier == 0) then
       allocate(ref2(nref))
       do i=1,nref
          ref2(i) = ref1(i)
       enddo
       call fcalcang(ref1,atom1,spg,elem,wavetype,anomal)  ! calcola fasi del modello 1
       call fcalcang(ref2,atom2,spg,elem,wavetype,anomal)  ! calcola fasi del modello 2
       call examing(ref1,ref2)
   else
       erore = 0
       shiftexam(1:7) = 0; shiftexam(4) = 1
   endif
!
   info%errph = erore  ! errore di fase
   info%nref = nref
   info%shift(:) = shiftexam(:4)
!
!  Applica lo shift di fase al modello 1
   if (shiftexam(4) > 0) then
       call translate_atoms(info%at1,-shiftexam(1:3))
   else
       do i=1,nat2
          info%at1(i)%xc = -(info%at1(i)%xc + shiftexam(1:3))
       enddo
   endif
!
   if (present(kpr)) then
       kprint = kpr
   else
       kprint = -1
   endif
!
   if (present(dist)) then
       info%distmin = dist
   else
       info%distmin = 0.6
   endif
!
   npunt(:) = 0
   distrmsd(:) = huge(1.0)
   info%rmsd = 0
   info%rmsd_nonH = 0
   nat_rmsd = 0
   nat_rmsda = 0
   do i=1,nat1
      call get_equivalent1(info%at1(i)%xc,spg,xeq)
      jpunt = 0
      distm = huge(1.0)
      rmsdset = .false.
      do j=1,nat2
         if (npunt(j) /= 0) cycle  ! atom already associated
         do k=1,spg%nsymop    
            call xdisteqs(atom2(j)%xc,xeq(:,k),cell%get_g(),dd,ktra)
            if (dd < distrmsd(i)) then
                distrmsd(i) = dd
                rmsdset = .true.
            endif
            if (dd < distm .and. dd < info%distmin) then
                distm = dd
                ktramin(:) = ktra(:)
                jpunt = j
            endif
         enddo
      enddo
      if (rmsdset) then   ! rmsd non set when all npunt /= 0
          nat_rmsd = nat_rmsd + 1
          if (atom1(i)%z() /= H_at) nat_rmsda = nat_rmsda + 1
      else
          distrmsd(i) = 0
      endif
      if (jpunt > 0) then
          info%nass = info%nass + 1   ! conta le associazioni nel modello 1
          info%nv1(info%nass) = i
          info%nv2(info%nass) = jpunt
          info%vdist(info%nass) = distm
          info%at1(i)%xc = atom2(jpunt)%xc + ktramin
          if (npunt(jpunt) == 0) info%nfound = info%nfound + 1  ! conta i trovati nel modello 2
          npunt(jpunt) = i
      endif
   enddo
!
!  Compute RMSD
   info%rmsd = sqrt(dot_product(distrmsd,distrmsd)/nat_rmsd)     ! all atoms
   if (is_hydrogen(atom1)) then
       if (nat_rmsda > 0) then
           info%rmsd_nonH = sqrt(sum(distrmsd(:)**2,mask=atom1%z() /= H_at)/nat_rmsda)  ! all non H-atoms
       else
           info%rmsd_nonH = 0
       endif
   else
       info%rmsd_nonH = -info%rmsd
   endif
!
   if (info%nfound > 0) then
       info%distm = sum(info%vdist(:info%nfound))/info%nfound
   endif
!
   if (kprint > 0) call print_compare_info(info,kprint)
!
   if (present(infoc)) then
       infoc = info
   endif
!
   end subroutine compare_models

!---------------------------------------------------------------------      

   subroutine init_compare_info_type(info,npos)
   type(compare_info_type), intent(inout) :: info
   integer, intent(in)                    :: npos
!
   info%nfound = 0
   allocate(info%nv1(npos))
   allocate(info%nv2(npos))
   allocate(info%vdist(npos))
   info%distm = 0.0
   info%nass = 0
!
   end subroutine init_compare_info_type

!---------------------------------------------------------------------      

   subroutine print_compare_info(info,kpr)
   type(compare_info_type), intent(in) :: info
   integer, intent(in)                 :: kpr
   integer                             :: i
   integer                             :: n1,n2
!
   write(kpr,'(a,i0)')         'Atoms in model1:      ',size(info%at1)
   write(kpr,'(a,i0)')         'Atoms in model2:      ',size(info%at2)
   write(kpr,'(a,i0)')         'Matches found:        ',info%nfound
   if (info%shift(4) > 0) then
       write(kpr,'(a,3f10.3,a)')     'Shift applied:        ',info%shift(:3),' Sign is +'
   else
       write(kpr,'(a,3f10.3,a)')     'Shift applied:        ',info%shift(:3),' Sign is -'
   endif
   write(kpr,'(a,f0.3,a,i0,a)')'Mean Phase Error:     ',info%errph,' using ',info%nref,' reflections'
   write(kpr,'(a,f0.3)')       'Distance limit:       ',info%distmin
   write(kpr,'(a,f0.3)')       '<Dist>                ',info%distm
   write(kpr,'(14("-")," Model 1 ",15("-"),5x,"Distance",4x,10("-")," Model 2 ",10("-"))')
   do i=1,info%nfound
      n1 = info%nv1(i)
      n2 = info%nv2(i)
      write(kpr,'(a8,3f10.3,f12.5,3f10.3,2x,a8)')info%at1(n1)%lab,info%at1(n1)%xc,info%vdist(i),info%at2(n2)%xc,info%at2(n2)%lab
   enddo
!
   end subroutine print_compare_info

END MODULE compare_struct
