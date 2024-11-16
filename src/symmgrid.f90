module symmgrid
USE atom_basic, only: op_type
implicit none

type bond_info_t
  real               :: dist       ! bond length
  integer            :: at1,at2    ! atom pointer to asu
  real, dimension(3) :: xat1,xat2  ! coordinates
  type(op_type)      :: op1, op2   ! symmetry operators
  integer            :: dupl       ! pointer to similar bond for duplicate
  real               :: bv         ! bond valence
end type bond_info_t

contains

   subroutine init_grid(grid,gridbnd,cell,cutoff,bound,sizegr)
   USE unit_cell
   USE arrayutil
   type(container_type), dimension(:,:,:), allocatable, intent(inout) :: grid
   integer, dimension(:,:), allocatable, intent(inout)                :: gridbnd
   type(cell_type), intent(in)                                        :: cell
   real, intent(in)                                                   :: cutoff
   integer, intent(in)                                                :: bound
   integer, dimension(3), intent(out)                                 :: sizegr
   integer                                                            :: k1,k2,k3
   integer, parameter                                                 :: SIZEPOS = 8
   integer, dimension(3)                                              :: lbindex,ubindex
   integer                                                            :: nbnd
   logical                                                            :: lprint = .false.
   integer                                                            :: i,j
   integer, dimension(3)                                              :: sizeg
!
   !sizeg = floor(cell%get_abc()/cutoff)
   sizeg =  floor(matmul(cell%get_ortoi(),[cutoff,cutoff,cutoff]))
   where(sizeg == 0) sizeg = 1 ! cutoff > a,b,c
   if (any(sizeg /= sizegr)) then
       sizegr = sizeg
       if (lprint) then
           write(71,*)'a,b,c=',cell%get_abc()
           write(71,*)'Size grid=',sizegr
           write(71,*)'Size cell=',1.0/sizegr
       endif
!
       if (allocated(grid)) then
           deallocate(grid)
       endif
       allocate(grid(sizegr(1),sizegr(2),sizegr(3)))
       do k1=1,sizegr(1)
          do k2=1,sizegr(2)
             do k3=1,sizegr(3)
                allocate(grid(k1,k2,k3)%pos(SIZEPOS))
             enddo
          enddo
       enddo
!
       nbnd = maxval(sizegr) + bound
       call new_array(gridbnd,[1,-bound+1],[3,nbnd])
       lbindex = 1
       ubindex = sizegr
       do i=1,3
          do j=-bound+1,nbnd
             gridbnd(i,j) = j
             do
                if(gridbnd(i,j) >= lbindex(i)) exit
                gridbnd(i,j) = gridbnd(i,j) + sizegr(i)
             enddo
             do
               if(gridbnd(i,j) <= ubindex(i)) exit
               gridbnd(i,j) = gridbnd(i,j) - sizegr(i)
             enddo
          enddo
       enddo
       
       if (lprint) then
           DO j = 1, 3
              DO i = - bound+1, nbnd
                 write(71,*)'i,j=',i,j,gridbnd(j,i)
              enddo
           enddo
       endif
   endif
!
   end subroutine init_grid

!---------------------------------------------------------------------------------------

   subroutine print_grid(atsym,grid,kpr)
   USE atom_basic
   USE arrayutil
   type(atom_type), dimension(:), intent(in)          :: atsym
   type(container_type), dimension(:,:,:), intent(in) :: grid
   integer, intent(in)                                :: kpr
   integer                                            :: k1,k2,k3,ia,kat
!
   do k1=1,size(grid,1)
      do k2=1,size(grid,2)
         do k3=1,size(grid,3)
            if (grid(k1,k2,k3)%nat == 0) cycle
            write(kpr,'(a,3i5,a,*(i5))')'CELL=',k1,k2,k3,' AT=',grid(k1,k2,k3)%pos(:grid(k1,k2,k3)%nat)
            do ia=1,grid(k1,k2,k3)%nat
               kat = grid(k1,k2,k3)%pos(ia)
               write(kpr,'(5x,a5,2i5,3f10.3)')trim(atsym(kat)%lab),kat,atsym(kat)%asym,atsym(kat)%xc
            enddo
         enddo
      enddo
   enddo
!
   end subroutine print_grid

!---------------------------------------------------------------------------------------

   subroutine print_grid_bd(atsym,grid,gridbnd,kpr)
   USE atom_basic
   USE arrayutil
   type(atom_type), dimension(:), intent(in)          :: atsym
   type(container_type), dimension(:,:,:), intent(in) :: grid
   integer, dimension(:,:), allocatable, intent(in)   :: gridbnd
   integer, intent(in)                                :: kpr
   integer                                            :: k1,k2,k3,ia,kat
   integer, dimension(3)                              :: ind,kc
   integer :: bd = 1
   integer :: ip
!
   do k1=1-bd,size(grid,1)+bd
      do k2=1-bd,size(grid,2)+bd
         do k3=1-bd,size(grid,3)+bd
            ind = [k1,k2,k3]
            do ip=1,3
               kc(ip) = gridbnd(ip,ind(ip))
            enddo
            if (grid(kc(1),kc(2),kc(3))%nat == 0) cycle
            write(kpr,'(2(a,3i5))')'CELL=',ind,' => ',kc
            do ia=1,grid(kc(1),kc(2),kc(3))%nat
               kat = grid(kc(1),kc(2),kc(3))%pos(ia)
               write(kpr,'(5x,a5,2i5,3f10.3)')trim(atsym(kat)%lab),kat,atsym(kat)%asym,atsym(kat)%xc
            enddo
         enddo
      enddo
   enddo

!
   end subroutine print_grid_bd

!---------------------------------------------------------------------------------------

   subroutine print_bond_info(atom,spg,dinfo,kpr)
   USE spginfom
   USE atom_type_util
   type(atom_type), dimension(:),   intent(in) :: atom
   type(spaceg_type), intent(in)               :: spg
   type(bond_info_t), dimension(:), intent(in) :: dinfo
   integer, intent(in)                         :: kpr
   integer                                     :: id, iat1, iat2
!
   write(kpr,'(a)')'Bonds for atom:'
   iat1 = dinfo(1)%at1
   write(kpr,'(i6,1x,a8,1x,a8,3f8.4,1x,a35,a)')iat1,atom(iat1)%glab(),adjustr(atom(iat1)%specie()), &
                                             dinfo(1)%xat1,str_symop(dinfo(1)%op1,spg)
   write(kpr,'(100("-"))')
   write(kpr,'(a)')'     N.     Atom      Type         xyz                      Sym. Op.                    d(A) BV(v.u.)'
   write(kpr,'(100("-"))')
   do id=1,size(dinfo)
      iat2 = dinfo(id)%at2
      write(kpr,'(i6,1x,a8,1x,a8,3f8.4,1x,a35,2f8.3)')iat2,atom(iat2)%glab(),adjustr(atom(iat2)%specie()),  &
                                                      dinfo(id)%xat2,str_symop(dinfo(id)%op2,spg),dinfo(id)%dist,dinfo(id)%bv
   enddo
!
   end subroutine print_bond_info

!---------------------------------------------------------------------------------------

   subroutine fillgrid(atsym,grid,sizegrid)
   USE arrayutil
   USE atom_basic
   type(atom_type), dimension(:), intent(in)             :: atsym
   type(container_type), dimension(:,:,:), intent(inout) :: grid
   integer, dimension(3), intent(in)                     :: sizegrid
   integer               :: i
   integer, dimension(3) :: kgrid
!
   grid(:,:,:)%nat = 0
   do i=1,size(atsym)
      kgrid = grid_position(atsym(i),sizegrid)
      call container_set(grid(kgrid(1),kgrid(2),kgrid(3)),i)
   enddo
!
   end subroutine fillgrid

!---------------------------------------------------------------------------------------

   subroutine fillgrid_vet(atsym,grid,sizegrid,vet)
   USE arrayutil
   USE atom_basic
   type(atom_type), dimension(:), intent(in)             :: atsym
   type(container_type), dimension(:,:,:), intent(inout) :: grid
   integer, dimension(3), intent(in)                     :: sizegrid
   integer, dimension(:), intent(in)                     :: vet
   integer                                               :: i
   integer, dimension(3)                                 :: kgrid
!
   grid(:,:,:)%nat = 0
   do i=1,size(atsym)
      if (any(vet == atsym(i)%asym)) then
          kgrid = grid_position(atsym(i),sizegrid)
          call container_set(grid(kgrid(1),kgrid(2),kgrid(3)),i)
      endif
   enddo
!
   end subroutine fillgrid_vet

!---------------------------------------------------------------------------------------

   function grid_position(atom,sizegrid)  result(kgrid)
   USE atom_basic
   type(atom_type), intent(in)       :: atom
   integer, dimension(3), intent(in) :: sizegrid
   integer, dimension(3)             :: kgrid
   kgrid = int(atom%xc*sizegrid + 1)
   where(kgrid == sizegrid+1) kgrid=kgrid-1  
   end function grid_position

!---------------------------------------------------------------------------------------

   subroutine compute_doc_grid_all(atsym,grid,gridbnd,sizegr,atom,cell,distmin)
   USE cgeom
   USE spginfom
   USE unit_cell
   USE atom_basic
   USE arrayutil
   type(atom_type), dimension(:), intent(in) :: atsym
   type(container_type), dimension(:,:,:), allocatable, intent(in) :: grid
   integer, dimension(:,:), allocatable, intent(in)                :: gridbnd
   integer, dimension(3), intent(in)                               :: sizegr
   type(atom_type), dimension(:), intent(inout) :: atom
   type(cell_type), intent(in)                  :: cell
   real, intent(in)                             :: distmin
   integer                                      :: nat
   integer                                      :: i
   real                                         :: dd
   integer, dimension(3)                        :: kgrid
   integer                                      :: k1,k2,k3,ia,kag,asym,kz,ip
   integer, dimension(3)                        :: icell
   integer, dimension(3)                        :: ind
   real, dimension(3)                           :: offset
   real, dimension(3) :: sizecell
!
   nat = size(atom)
   atom%ocry = 1.0
   sizecell = 1.0/sizegr
   do i=1,nat
      !kgrid = atomcr(i)%op%tra
      kgrid = grid_position(atsym(i),sizegr)
      kz = atom(i)%get_nz()
      do k1=kgrid(1)-1,kgrid(1)+1
         do k2=kgrid(2)-1,kgrid(2)+1
            do k3=kgrid(3)-1,kgrid(3)+1
               ind = [k1,k2,k3]
               do ip=1,3
                  icell(ip) = gridbnd(ip,ind(ip))
                  offset(ip) = (ind(ip) - icell(ip))*sizecell(ip)
               enddo
               do ia=1,grid(icell(1),icell(2),icell(3))%nat
                  kag = grid(icell(1),icell(2),icell(3))%pos(ia)
                  if (kz == atsym(kag)%get_nz() .and. atsym(kag)%asym >= i) then
                      if (kag == i) cycle
                      dd = distanzaC(atsym(i)%xc,atsym(kag)%xc+offset,cell%get_g())
                      if (dd < distmin) then
                          dd = distmin - dd
                          atom(i)%ocry = atom(i)%ocry + dd
                          asym = atsym(kag)%asym
                          if (i /= asym) atom(asym)%ocry = atom(asym)%ocry + dd   !!!new occ
                          !write(72,*)trim(atom(i)%lab)//'-'//trim(atsym(kag)%lab),distmin-dd,i,kag
                          !call atsym(i)%prn(72)
                          !call atsym(kag)%prn(72)
                          !write(72,*)'XC_TRA=',atsym(kag)%xc+offset
                      endif
                   endif
               enddo
            enddo
         enddo
      enddo
   enddo
   atom%ocry = 1/atom%ocry
   !do i=1,nat
   !   write(0,*)'OCC.'//trim(atom(i)%lab),atom(i)%ocry
   !enddo
!
   end subroutine compute_doc_grid_all

!---------------------------------------------------------------------------------------

   subroutine compute_doc_grid_vet(atsym,grid,gridbnd,sizegr,atom,cell,distmin,vet)
   USE cgeom
   USE spginfom
   USE unit_cell
   USE atom_basic
   USE arrayutil
   type(atom_type), dimension(:), intent(in) :: atsym
   type(container_type), dimension(:,:,:), allocatable, intent(in) :: grid
   integer, dimension(:,:), allocatable, intent(in)                :: gridbnd
   integer, dimension(3), intent(in)                               :: sizegr
   type(atom_type), dimension(:), intent(inout) :: atom
   type(cell_type), intent(in)                  :: cell
   real, intent(in)                             :: distmin
   integer, dimension(:), intent(in)            :: vet
   integer                                      :: nat
   integer                                      :: i,iv
   real                                         :: dd
   integer, dimension(3)                        :: kgrid
   integer                                      :: k1,k2,k3,ia,kag,asym,kz,ip
   integer, dimension(3)                        :: icell
   integer, dimension(3)                        :: ind
   real, dimension(3)                           :: offset
   real, dimension(3) :: sizecell
!
   nat = size(vet)
   atom(vet)%ocry = 1.0
   sizecell = 1.0/sizegr
   do iv=1,nat
      i = vet(iv)
      !kgrid = atomcr(i)%op%tra
      kgrid = grid_position(atsym(i),sizegr)
      kz = atom(i)%get_nz()
      do k1=kgrid(1)-1,kgrid(1)+1
         do k2=kgrid(2)-1,kgrid(2)+1
            do k3=kgrid(3)-1,kgrid(3)+1
               ind = [k1,k2,k3]
               do ip=1,3
                  icell(ip) = gridbnd(ip,ind(ip))
                  offset(ip) = (ind(ip) - icell(ip))*sizecell(ip)
               enddo
               do ia=1,grid(icell(1),icell(2),icell(3))%nat
                  kag = grid(icell(1),icell(2),icell(3))%pos(ia)
                  if (kz == atsym(kag)%get_nz() .and. atsym(kag)%asym >= i) then
                      if (kag == i) cycle
                      dd = distanzaC(atsym(i)%xc,atsym(kag)%xc+offset,cell%get_g())
                      if (dd < distmin) then
                          dd = distmin - dd
                          atom(i)%ocry = atom(i)%ocry + dd
                          asym = atsym(kag)%asym
                          if (i /= asym) atom(asym)%ocry = atom(asym)%ocry + dd   !!!new occ
                          !write(72,*)trim(atom(i)%lab)//'-'//trim(atsym(kag)%lab),distmin-dd,i,kag
                          !call atsym(i)%prn(72)
                          !call atsym(kag)%prn(72)
                          !write(72,*)'XC_TRA=',atsym(kag)%xc+offset
                      endif
                   endif
               enddo
            enddo
         enddo
      enddo
   enddo
   atom(vet)%ocry = 1/atom(vet)%ocry
   !do i=1,nat
   !   write(0,*)'OCC.'//trim(atom(i)%lab),atom(i)%ocry
   !enddo
!
   end subroutine compute_doc_grid_vet

!---------------------------------------------------------------------------------------

   subroutine compute_bvs_grid(atsym,grid,gridbnd,sizegr,kat,atom,cell,bvtab,val)
!   
!  Compute valence for atom kat
!
   USE atom_type_util
   USE spginfom
   USE cgeom
   USE bond_valence
   USE unit_cell
   USE arrayutil
   type(atom_type), dimension(:), intent(in)                       :: atsym
   type(container_type), dimension(:,:,:), allocatable, intent(in) :: grid
   integer, dimension(:,:), allocatable, intent(in)                :: gridbnd
   integer, dimension(3), intent(in)                               :: sizegr
   integer, intent(in)                                             :: kat
   type(atom_type), dimension(:), intent(in)                       :: atom
   type(cell_type), intent(in)                                     :: cell
   real, dimension(3,3)                                            :: gmat
   type(bond_valence_t), dimension(:,:), intent(in)                :: bvtab   
   real, intent(out)                                               :: val
   integer                                                         :: nat
   integer                                                         :: nzk,nzi,ip
   real                                                            :: dd
   real, dimension(3)                                              :: xf   !,ktra
   integer, dimension(3)                                           :: kgrid
   integer                                                         :: k1,k2,k3,ia,kag
   integer                                                         :: bd
   integer, dimension(3)                                           :: icell
   integer, dimension(3)                                           :: ind
   real, dimension(3)                                              :: offset
   real, dimension(3)                                              :: sizec 
!
   val = 0
   nat = size(atom)
   if (kat > nat .or. nat == 0) return
   nzk = atom(kat)%kscatt()
   sizec = 1.0/sizegr
!
   kgrid = grid_position(atsym(kat),sizegr)
   bd = 1 !!!!ceiling(sizegr(1)*maxval(bvtab(nzk,:)%rmax)/cell%get_a())
   gmat = cell%get_g()
   !write(71,'(a,a)')'BVS for atom: ',trim(atom(kat)%lab)
   do k1=kgrid(1)-bd,kgrid(1)+bd
      do k2=kgrid(2)-bd,kgrid(2)+bd
         do k3=kgrid(3)-bd,kgrid(3)+bd
            ind = [k1,k2,k3]
            do ip=1,3
               icell(ip) = gridbnd(ip,ind(ip))
               offset(ip) = (ind(ip) - icell(ip))*sizec(ip)
            enddo
            loop_grid: do ia=1,grid(icell(1),icell(2),icell(3))%nat
               kag = grid(icell(1),icell(2),icell(3))%pos(ia)
               if (kag == kat) cycle
               xf(:) = atsym(kag)%xc+offset
               dd = distanzaC(atsym(kat)%xc,xf,gmat)
               nzi = atsym(kag)%kscatt()
               !write(71,'(2(a,3i5),a,i5,a,3f10.2)')'CELL=',k1,k2,k3,'=>',icell,' NAT:', &
               !          grid(icell(1),icell(2),icell(3))%nat,' OFF:',offset
               if (dd <= bvtab(nzi,nzk)%rmax .and. dd >= bvtab(nzi,nzk)%rmin) then
                   !write(71,'(a,2f10.3,1x,a,3f10.3,1x,2i5)')'   VAL=',exp((bvtab(nzi,nzk)%ro - dd) / bvtab(nzi,nzk)%b),    &
                   !dd,trim(atsym(kag)%lab),xf,atsym(kag)%kscatt(),kat
                   val = val + exp((bvtab(nzi,nzk)%ro - dd) / bvtab(nzi,nzk)%b)*atom(atsym(kag)%asym)%ocry*atom(atsym(kag)%asym)%och
               endif
            enddo loop_grid
         enddo
      enddo
   enddo
!
   end subroutine compute_bvs_grid

!---------------------------------------------------------------------------------------------------------------------

   subroutine bvalence_atom_info(atsym,grid,gridbnd,sizegr,kat,atom,cell,bvtab,dinfo,bvs)
!   
!  Compute valence for atom kat and additional info. dinfo must be  allocated to maximum size with bvalence_atom and new_bond_info
!
   USE atom_type_util
   USE spginfom
   USE cgeom
   USE bond_valence
   USE unit_cell
   USE arrayutil
   type(atom_type), dimension(:), intent(in)                       :: atsym
   type(container_type), dimension(:,:,:), allocatable, intent(in) :: grid
   integer, dimension(:,:), allocatable, intent(in)                :: gridbnd
   integer, dimension(3), intent(in)                               :: sizegr
   integer, intent(in)                                             :: kat
   type(atom_type), dimension(:), intent(in)                       :: atom
   type(cell_type), intent(in)                                     :: cell
   real, dimension(3,3)                                            :: gmat
   type(bond_valence_t), dimension(:,:), intent(in)                :: bvtab   
   type(bond_info_t), dimension(:), allocatable, intent(inout)     :: dinfo 
   real, intent(out)                                               :: bvs
   integer                                                         :: ndinfo
   integer                                                         :: nat
   integer                                                         :: nzk,nzi,ip
   real                                                            :: dd
   real, dimension(3)                                              :: xf
   integer, dimension(3)                                           :: kgrid
   integer                                                         :: k1,k2,k3,ia,kag,j
   integer                                                         :: bd
   integer, dimension(3)                                           :: icell
   integer, dimension(3)                                           :: ind
   real, dimension(3)                                              :: offset
   real, dimension(3)                                              :: sizec 
   real, dimension(3,size(dinfo,1))                                :: xfind
   integer, dimension(size(dinfo,1))                               :: at_type
   logical                                                         :: dupl
!
   nat = size(atom)
   if (kat > nat .or. nat == 0) return
   nzk = atom(kat)%kscatt()
   sizec = 1.0/sizegr
   ndinfo = 0
!
   kgrid = grid_position(atsym(kat),sizegr)
   bd = 1 
   gmat = cell%get_g()
   bvs = 0
     !write(0,*)'BD=',bd,maxval(bvtab(nzk,:)%rmax)
!
!  Loop to compute ndinfo and allocate dinfo
   do k1=kgrid(1)-bd,kgrid(1)+bd
      do k2=kgrid(2)-bd,kgrid(2)+bd
         do k3=kgrid(3)-bd,kgrid(3)+bd
            ind = [k1,k2,k3]
            do ip=1,3
               icell(ip) = gridbnd(ip,ind(ip))
               offset(ip) = (ind(ip) - icell(ip))*sizec(ip)
            enddo
            loop_grid: do ia=1,grid(icell(1),icell(2),icell(3))%nat
               kag = grid(icell(1),icell(2),icell(3))%pos(ia)
               if (kag == kat) cycle
               xf(:) = atsym(kag)%xc+offset
               dd = distanzaC(atsym(kat)%xc,xf,gmat)
               nzi = atsym(kag)%kscatt()
               if (dd < bvtab(nzi,nzk)%rmax .and. dd > bvtab(nzi,nzk)%rmin) then
                   !write(70,'(a,2f10.3,1x,a,3f10.3,1x,2i5)')'   VAL=',exp((bvtab(nzi,nzk)%ro - dd) / bvtab(nzi,nzk)%b),    &
                   !dd,trim(atsym(kag)%lab),xf,atsym(kag)%kscatt(),kat
                   !val = val + exp((bvtab(nzi,nzk)%ro - dd) / bvtab(nzi,nzk)%b)*atom(atsym(kag)%asym)%ocry
!
!                  search for duplicate bonds
                   dupl = .false.
                   do j=1,ndinfo
                      if (at_type(j) == nzi) then
                          if (distanzaC(xfind(:,j),xf,gmat) <= 0.5) then
                              dupl = .true.
                              dinfo(j)%dupl = dinfo(j)%dupl + 1 !duplicate atom
                              !cycle loop_grid
                              exit
                          endif
                      endif
                   enddo
                   if (.not. dupl) then
!
!                      save info for this bond
                       ndinfo = ndinfo + 1
                       dinfo(ndinfo)%dist = dd
                       dinfo(ndinfo)%at1 = kat
                       dinfo(ndinfo)%at2 = atsym(kag)%asym
                       dinfo(ndinfo)%xat1 = atsym(kat)%xc
                       dinfo(ndinfo)%xat2 = xf
                       dinfo(ndinfo)%op1 = atsym(kat)%op
                       dinfo(ndinfo)%op2 = op_type(atsym(kag)%op%op,nint(atsym(kag)%op%tra+offset))
!                      if duplicates are excluded at%ocry must be non included
                       dinfo(ndinfo)%bv = exp((bvtab(nzi,nzk)%ro - dd) / bvtab(nzi,nzk)%b)*atom(atsym(kag)%asym)%och
                       dinfo(ndinfo)%dupl = 0
                       xfind(:,ndinfo) = xf
                       at_type(ndinfo) = nzi
                       bvs = bvs + dinfo(ndinfo)%bv
                   endif
               endif
            enddo loop_grid
         enddo
      enddo
   enddo
!
   call resize_bond_info(dinfo,ndinfo)   ! shrink array
!
   end subroutine bvalence_atom_info

!---------------------------------------------------------------------------------------------------------------------

   integer function bvalence_atom(atsym,grid,gridbnd,sizegr,kat,atom,cell,bvtab)  result(ndinfo)
!   
!  Compute valence for atom kat and additional info
!
   USE atom_type_util
   USE spginfom
   USE cgeom
   USE bond_valence
   USE unit_cell
   USE arrayutil
   type(atom_type), dimension(:), intent(in)                       :: atsym
   type(container_type), dimension(:,:,:), allocatable, intent(in) :: grid
   integer, dimension(:,:), allocatable, intent(in)                :: gridbnd
   integer, dimension(3), intent(in)                               :: sizegr
   integer, intent(in)                                             :: kat
   type(atom_type), dimension(:), intent(in)                       :: atom
   type(cell_type), intent(in)                                     :: cell
   real, dimension(3,3)                                            :: gmat
   type(bond_valence_t), dimension(:,:), intent(in)                :: bvtab   
   integer                                                         :: nat
   integer                                                         :: nzk,nzi,ip
   real                                                            :: dd
   real, dimension(3)                                              :: xf   !,ktra
   integer, dimension(3)                                           :: kgrid
   integer                                                         :: k1,k2,k3,ia,kag
   integer                                                         :: bd
   integer, dimension(3)                                           :: icell
   integer, dimension(3)                                           :: ind
   real, dimension(3)                                              :: offset
   real, dimension(3)                                              :: sizec 
   !real, dimension(3,size(dinfo,1))                                :: xfind
   !integer, dimension(size(dinfo,1))                               :: at_type
   !real, dimension(:,:), allocatable                                :: xfind
   !integer, dimension(:), allocatable                               :: at_type
      !real :: nd
!
   nat = size(atom)
   ndinfo = 0
   !nd = 0
   if (kat > nat .or. nat == 0) return
   nzk = atom(kat)%kscatt()
   sizec = 1.0/sizegr
!
   kgrid = grid_position(atsym(kat),sizegr)
   bd = 1 
   gmat = cell%get_g()
!
!  Loop to compute ndinfo
   do k1=kgrid(1)-bd,kgrid(1)+bd
      do k2=kgrid(2)-bd,kgrid(2)+bd
         do k3=kgrid(3)-bd,kgrid(3)+bd
            ind = [k1,k2,k3]
            do ip=1,3
               icell(ip) = gridbnd(ip,ind(ip))
               offset(ip) = (ind(ip) - icell(ip))*sizec(ip)
            enddo
            loop_grid: do ia=1,grid(icell(1),icell(2),icell(3))%nat
               kag = grid(icell(1),icell(2),icell(3))%pos(ia)
               if (kag == kat) cycle
               xf(:) = atsym(kag)%xc+offset
               dd = distanzaC(atsym(kat)%xc,xf,gmat)
               nzi = atsym(kag)%kscatt()
               if (dd < bvtab(nzi,nzk)%rmax .and. dd > bvtab(nzi,nzk)%rmin) then
                     !write(71,*)'AT=',atom(kat)%glab(),nzi,nzk,bvtab(nzi,nzk)%rmin,bvtab(nzi,nzk)%rmax,dd
                   ndinfo = ndinfo + 1
                   !nd = nd + atom(atsym(kag)%asym)%ocry
               endif
            enddo loop_grid
         enddo
      enddo
   enddo
     !write(0,*)'ND=',nd,ndinfo
     !ndinfo = nint(nd)
!
   end function bvalence_atom

!---------------------------------------------------------------------------------------------------------------------

   real function antibump_function_grid(atsym,grid,gridbnd,sizegr,bump,atom,cell,kpr) result(bfunc)
!
!  Compute cost function for anti-bump restraints
!
   USE atom_type_util
   USE fragmentmod
   USE connect_mod
   USE cgeom
   USE rrestr
   USE spginfom
   USE unit_cell
   USE atom_basic
   USE arrayutil
   type(atom_type), dimension(:), intent(in) :: atsym
   type(container_type), dimension(:,:,:), allocatable, intent(in) :: grid
   integer, dimension(:,:), allocatable, intent(in)                :: gridbnd
   integer, dimension(3), intent(in)                               :: sizegr
   type(restraint_type), dimension(:), allocatable, intent(inout) :: bump
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(cell_type), intent(in)                               :: cell
   real, dimension(3,3) :: gmat
   logical, intent(in) :: kpr
   integer :: nbump
   integer :: n1, n2
   real :: dd
!corr   type(atom_type) :: at
   integer :: i
!corr   real, dimension(3) :: ktra
   logical :: same_frag
!corr   real :: dist
   real, parameter :: EPS = epsilon(1.0)
   real, parameter :: DDOC = 1.0
   integer, dimension(3) :: kgrid
   integer :: k1,k2,k3,ia,kag
   integer :: bd,ip,nat,posb
   integer, dimension(3) :: icell
   integer, dimension(3) :: ind
   real, dimension(3) :: offset
   logical, dimension(size(atom)) :: isbump
   integer, dimension(size(atom),size(atom)) :: btab
   real, dimension(3) :: sizec
   !integer, dimension(size(atom)) :: bound
!
   bfunc = 0
   nbump = nrestraints(bump)
   gmat = cell%get_g()
   nat = numatoms(atom)
   bd = 1 !!!ceiling(sizegrid(1)*maxval(bump%targ)/cell%get_a())   !!!!TOFIX
   isbump=.false.
   do i=1,size(bump)
      isbump(abs(bump(i)%na(1))) = .true.
   enddo
   btab(:,:) = 0
   do i=1,size(bump)
      n1 = abs(bump(i)%na(1))
      n2 = bump(i)%na(2)
      btab(n1,n2) = i
      !btab(n2,n1) = i
   enddo
   sizec = 1.0/sizegr
   !bound = 0
   !do i=1,size(bump)
   !   if (bound(abs(bump(i)%na(1))) <  bump(i)%targ) bound(abs(bump(i)%na(1))) = bump(i)%targ
   !enddo
   !   write(0,*)'BOUN=',bound
   !   write(0,*)'BOUN=',bd
   do i=1,nat
      !posb  = restraint_position(bump,i)
      !if (posb > 0) then
      !    kgrid = grid_position(atsym(n1))
      !endif
      if (.not.isbump(i)) cycle
   !bd = ceiling(sizegrid(1)*maxval(bump(btab(i,:))%targ,mask=btab(i,:)/=0)/cell%get_a())   !!!!TOFIX
      !bd = bound(i)
      kgrid = grid_position(atsym(i),sizegr)
      do k1=kgrid(1)-bd,kgrid(1)+bd
         do k2=kgrid(2)-bd,kgrid(2)+bd
            do k3=kgrid(3)-bd,kgrid(3)+bd
               ind = [k1,k2,k3]
               do ip=1,3
                  icell(ip) = gridbnd(ip,ind(ip))
                  offset(ip) = (ind(ip) - icell(ip))*sizec(ip)
               enddo
               loop_grid: do ia=1,grid(icell(1),icell(2),icell(3))%nat
                  kag = grid(icell(1),icell(2),icell(3))%pos(ia)
                  !posb  = restraint_position(bump,restraint_type(code=ABUMP,na=[i,atsym(kag)%asym]))
                  posb = btab(i,atsym(kag)%asym)
                  if (posb > 0) then
                      !write(71,*)'POSB=',posb,bump(posb)%na(:)
                      if (abs(bump(posb)%na(1)) == i) then
                          n1 = bump(posb)%na(1)
                          n2 = bump(posb)%na(2)
                          if (atsym(kag)%asym == n2) then
                              if (n1 < 0) then
                                  n1 = abs(n1)
                                  same_frag = .true.
                              else
                                  same_frag = .false.
                              endif
                              if (kag == n1) cycle loop_grid 
                              if (same_frag .and. atsym(n1)%op == op_type(atsym(kag)%op%op,nint(atsym(kag)%op%tra+offset))) then
!
!                                 Atoms in same fragment and with same operator are non considered for bump
                                  cycle
                              endif
                              dd = distanzaC(atsym(n1)%xc,atsym(kag)%xc+offset,gmat)
                              if (dd < bump(posb)%targ) then
                                  bfunc = bfunc + (((1/bump(posb)%sigma)*(dd - bump(posb)%targ))**4)*atom(n2)%ocry
                                  if(kpr) then
                                     write(71,'(i5,a,a,f8.4,i5,i5)')i,'activate bump:',atom(n1)%lab//' '//atom(n2)%lab,dd,n1,kag
                                  endif
                              endif
                          endif
                      endif
                  endif
               enddo loop_grid
            enddo
          enddo
      enddo
   enddo
   if (kpr) then
       write(71,*)'fbump=',bfunc
   endif
!
   end function antibump_function_grid

!---------------------------------------------------------------------------------------------------------------------

   subroutine check_occ(atom,spg,cell,kpr)
   USE atom_type_util
   USE spginfom
   USE unit_cell
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(spaceg_type), intent(in)                          :: spg
   type(cell_type), intent(in)                            :: cell
   integer, intent(in)                                    :: kpr
   type(atom_type), dimension(:), allocatable             :: atomc
   integer                                                :: i
!
   call copy_atoms(atomc,atom)
   call compute_doc(atomc,spg,cell%get_g(),1.0)
   if (any(abs(atomc%ocry-atom%ocry) > 0.01)) then
       write(kpr,*)'OCC are different'
       do i=1,size(atom)
          write(kpr,*)'OCC.'//trim(atom(i)%lab),atomc(i)%ocry,atom(i)%ocry
       enddo
   endif
!
   end subroutine check_occ

!---------------------------------------------------------------------------------------------------------------------

   subroutine new_bond_info(binfo,n)
   type(bond_info_t), dimension(:), allocatable, intent(inout) :: binfo
   integer, intent(in)                                         :: n
   integer                                                     :: sizeb
!
   if (n < 0) return
   if (allocated(binfo)) then
       sizeb = size(binfo)
   else
       sizeb = 0
   endif
   if (sizeb /= n) then
       if (allocated(binfo))deallocate(binfo)
       if (n > 0) allocate(binfo(n))
   endif
!
   end subroutine new_bond_info

!----------------------------------------------------------------------------------------------------

   subroutine resize_bond_info(vetr,n,savevet)
!
!  Resize array of atoms
!
   type(bond_info_t), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                           :: n
   logical, optional, intent(in)                 :: savevet
   logical                                       :: savev
   integer                                       :: nv
   type(bond_info_t), allocatable                :: vsav(:)
   integer                                       :: nsav
!
!  se n = 0 (riallocazione a 0): dealloca ed esci
   if (n == 0) then
       if (allocated(vetr)) deallocate(vetr)
       return
   endif
!
   if (.not.allocated(vetr)) then
       allocate(vetr(n))
   else
!
       nv = size(vetr)
       if (present(savevet)) then
           savev = savevet
       else
           savev = .true.
       endif
!
       if (savev) then
!
!          nsav contiene qual e' la porzione di vetr da salvare
           select case(nv-n)
             case (1:)       ! compatta x ad n
               nsav = n
             case (:-1)      ! espandi x ad n
               nsav = nv
             case (0)
               return        ! n=nv non fare niente
           end select
           allocate(vsav(n))
           vsav(:nsav) = vetr(:nsav)
           call move_alloc(vsav,vetr)
       else
           if (nv /= n) then
               deallocate(vetr)
               allocate(vetr(n))
           endif
       endif
   endif
!
   end subroutine resize_bond_info

end module symmgrid
