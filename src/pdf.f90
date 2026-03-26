module pdfcalc

implicit none

type pdf_type
  real    :: rmin = 0.001     ! Minimum distance to calculate
  real    :: rmax = 50.0      ! Maximum distance to calculate
  real    :: rfmax = 0.01     ! distance range for refinement
  real    :: rfmin = 0.01     
  real, private    :: deltar = 0.001   ! Delta r
  real, private    :: deltars = 0.0005
  real, private    :: deltaru = 0.01
  integer    :: nscat = 0
  integer, private :: bin = 1
  real, private    :: xq = 0.0         ! Q=4pi*sin(theta)/lambda
  real    :: qmax = 50.0      ! maximum measurable value of Q
  integer, private    :: nmol = 0
  integer, private :: nbnd
  logical, private :: gauss = .false.
  real, private    :: qalp = 0.00
  integer, private :: ndat = 1
  real, private    :: scal = 1.0
  real, private    :: dnorm = 1.0
  logical, private :: lexact = .false.
  !integer, dimension(3) :: cr_icc = [1,1,1] ! crystal dimensions: number of unit cells in x,y,z direction
  integer, dimension(3) :: cr_icc = [3,3,3] ! crystal dimensions: number of unit cells in x,y,z direction
  real, private, dimension(:,:), allocatable  :: weight
  logical, private, dimension(:), allocatable :: allowed_i,allowed_j ! which atom types shall be included in the PDF
  real, dimension(:), allocatable :: calc,sincc

contains
!
! Public procedure
  procedure :: setup
  procedure :: determine => pdf_determine
  procedure :: save_file
!
! Private procedure
  procedure, private :: addcorr_e_all
  procedure, private :: addcorr_n_fast
  procedure, private :: alloc => pdf_allocate
  procedure, private :: convtherm
  procedure, private :: convert

end type pdf_type

  real, dimension(:,:,:), allocatable, private      :: asymtab
  integer, dimension(:,:,:,:), allocatable, private :: pdf_temp
  real, dimension(:), allocatable, private          :: pdf_corr
  integer, dimension(:,:), allocatable :: pdf_bnd
  !integer, dimension(:,:,:), allocatable :: cr_index_start,cr_index_end
  logical, private :: lprint = .false.

contains

   subroutine setup(pdf,cell,elem,atom,radtype)
! 
!  Setup for various arrays and functions for PDF calculations
! 
   USE elements
   USE trig_constants
   USE atom_basic
   USE unit_cell
   USE molpnew, only: cr_index_start
   class(pdf_type), intent(inout)               :: pdf
   type(cell_type), intent(in)                  :: cell
   type(element_type), dimension(:), intent(in) :: elem
   type(atom_type), dimension(:), intent(in)    :: atom
   integer, intent(in)                          :: radtype
   real, dimension(size(elem))                  :: scattv
   real                                         :: rho,bave,rtot,ract
   integer                                      :: i,j,cr_natoms,is,js,nn,nnn
   !logical :: kpr = .true.
   real :: rcut,z
   real, parameter :: sincut = 0.025
   integer :: max_bnd
   integer, dimension(3) :: lbindex,ubindex
!
   pdf%rmin = pdf%deltaru
   pdf%rfmin = pdf%deltaru
!
   rcut     = 1.0
   if(pdf%qmax> 0) THEN
      rcut     = 1.0 / (pdf%qmax * sincut)
   endif
   nn       = INT (rcut / pdf%deltar) + 100
   nnn      = INT ( (pdf%rfmax + rcut) / pdf%deltar)
   pdf%ndat = MAX ( nn, nnn) + 1
!
!  Periodic boundaries
   max_bnd = max(1,maxval(int(pdf%rmax/cell%get_par())))
   pdf%nbnd  = maxval(pdf%cr_icc) + 1 + max_bnd
!
   call pdf%alloc(pdf%nscat,pdf%ndat,pdf%nbnd)
!
   lbindex = lbound(cr_index_start)
   ubindex = ubound(cr_index_start)
   do i=1,3
      do j=-pdf%nbnd,pdf%nbnd
         pdf_bnd(i,j) = j
         do 
            if(pdf_bnd(i,j) >= lbindex(i)) exit
            pdf_bnd(i,j) = pdf_bnd(i,j) + pdf%cr_icc(i)
         enddo
         do 
           if(pdf_bnd(i,j) <= ubindex(i)) exit
           pdf_bnd(i,j) = pdf_bnd(i,j) - pdf%cr_icc(i)
         enddo
      enddo
   enddo
   if (lprint) then
       DO j = 1, 3
          DO i = - pdf%nbnd, pdf%nbnd
             write(71,*)'i,j=',i,j,pdf_bnd(j,i)
          enddo
       enddo
   endif
!
   cr_natoms = size(atom)
! 
!  Setting up weighting b(i)b(j)/<b**2>
   rho = (pdf%xq/(4*pi))**2
   scattv = at_scatt(elem,rho,radtype)
! 
   bave = 0.0
   do i=1,cr_natoms
      !bave = bave + atom(i)%och*atom(i)%ocry*scattv(atom(i)%kscatt())
      bave = bave + scattv(atom(i)%kscatt())
      if (lprint) write(78,*)'FORM=',i,scattv(atom(i)%kscatt())
   enddo
   bave = bave / cr_natoms
! 
   do i=1,pdf%nscat
      do j=1,pdf%nscat
         pdf%weight(i,j) = scattv(atom(i)%kscatt())*scattv(atom(j)%kscatt())/bave**2
      enddo
   enddo
!
!  Get the ratio total pairs/selected weight in structure
   if (.not.(all(pdf%allowed_i) .and. all(pdf%allowed_j))) then
       rtot = 0.0
       ract = 0.0
!                                                                     
       DO i = 1, cr_natoms
          DO j = 1, cr_natoms
             is = atom(i)%asym
             js = atom(j)%asym
             rtot = rtot + pdf%weight (is, js)
             IF ( (pdf%allowed_i(is) .and. pdf%allowed_j(js) ) .or.  &
                  (pdf%allowed_j(is) .and. pdf%allowed_i(js) ) )     &
                   ract = ract + pdf%weight (is, js)
          ENDDO
       ENDDO
       pdf%dnorm = ract / rtot
   else
       pdf%dnorm = 1.0
   endif
!
   pdf%bin = int(pdf%rmax/pdf%deltar) + 1   !!!TOFIX
!                                                                       
!--Setting up SINC function for convolution with PDF               
   if (pdf%qmax > 0.0) then
!corr       sincut = 0.025
       rcut = 1.0 / (pdf%qmax * sincut)
       z = pdf%deltar * pdf%qmax
       if ( (pdf%rfmax + rcut) .gt.pdf%rmax) then
          nnn = int ( (pdf%rfmax + rcut) / pdf%deltar)
          if (nnn.le.pdf%ndat) then
              pdf%rmax = pdf%rfmax + rcut
              pdf%bin = int (pdf%rmax / pdf%deltar)
          else
!TODO             ier_num = - 2
!TODO             ier_typ = ER_PDF
!TODO             RETURN
          endif
       endif
!                                                                       
         j = SIZE(pdf%sincc)+1
         do i = 1,j/2
            pdf%sincc(i+1) = sin(z*i) / (pdf%deltar*i)
            pdf%sincc(j-i) = sin(z*i) / (pdf%deltar*i)
            !if (lprint) write(77,*)'pdf_sincc=',i,pdf%sincc(i+1),pdf%sincc(j-1)
         enddo
         pdf%sincc(1) = pdf%qmax
   endif


   if (lprint) then
       write(0,*)'=========================================='
       write(0,*)'xq=',pdf%xq
       write(0,*)'nscat=',pdf%nscat
       write(0,*)'natom=',cr_natoms
       write(0,*)'qmax=',pdf%qmax
       write(0,*)'deltar=',pdf%deltar,pdf%deltars
       write(0,*)'ndat=',pdf%ndat,size(pdf%calc)
       write(0,*)'rmax,rfmax=',pdf%rmax,pdf%rfmax
       write(0,*)'rmin,rfmin=',pdf%rmin,pdf%rfmin
       write(0,*)'bin=',pdf%bin
       write(0,*)'natoms=',cr_natoms
       write(0,*)'bave=',bave
       write(0,*)'dnorm=',pdf%dnorm
       write(0,*)'nbnd=',pdf%nbnd
       write(0,*)'LB=',lbound(pdf_bnd)
       write(0,*)'UB=',ubound(pdf_bnd)
       !do i=1,pdf%nscat
       !   do j=1,pdf%nscat
       !      write(0,*)'weight=',i,j,pdf%weight(i,j)
       !   enddo
       !enddo
       write(0,*)'=========================================='
       write(0,*)'PDF_SINCC='
       do i=1,size(pdf%sincc)
          write(77,'(i10,f12.6)')i,pdf%sincc(i)
       enddo
   endif

   end subroutine 

!-----------------------------------------------------------------------------------------

   subroutine pdf_allocate(pdf,nscat,ndat,nbnd)
! 
!  Allocate the arrays needed by PDF
!
   USE arrayutil
   class(pdf_type), intent(inout) :: pdf
   integer, intent(in)            :: nscat,ndat,nbnd
   integer, parameter             :: PDF_MAXSINCC=2**12+1
   integer                        :: ndat2
!
   ndat2 = 2**(INT(LOG(FLOAT(pdf%ndat))/LOG(2.))+2)
   call new_array(pdf%calc,ndat2)
   call new_array(pdf%weight,[1,1],[nscat,nscat])
   call new_array(pdf_bnd,[1,-nbnd],[3,nbnd])
!
   if (.not.allocated(pdf%allowed_i)) then
       call new_array(pdf%allowed_i,nscat)
       call new_array(pdf%allowed_j,nscat)
       pdf%allowed_i = .true.
       pdf%allowed_j = .true.
   endif
!
   call new_array(pdf_corr,ndat)
   call new_array(pdf%sincc,PDF_MAXSINCC)
!
   call new_array(pdf_temp,[0,0,0,0],[ndat,nscat,nscat,nscat])
   !if (.not.allocated(pdf_temp)) then
   !    allocate(pdf_temp(0:ndat,0:nscat,0:nscat,0:nscat))
   !else
   !    if (ubound(pdf_temp,1) /= ndat .and. ubound(pdf_temp,2) /= nscat) then
   !        deallocate(pdf_temp)
   !        allocate(pdf_temp(0:ndat,0:nscat,0:nscat,0:nscat))
   !    endif
   !endif
!
   end subroutine pdf_allocate
!!!TODO: procedure pdf%free()
!-----------------------------------------------------------------------------------------

   subroutine pdf_determine(pdf,atom,cell)
!                                                                       
!  Calculate PDF of current structure                                
!                                                  
   USE atom_type_util
   USE unit_cell
   class(pdf_type), intent(inout)                            :: pdf
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   type(cell_type), intent(in)                               :: cell
   real                                                      :: rsum
!
   if (pdf%lexact) then
       call frac_to_cart(atom,cell)
   endif
!
   pdf_corr(:) = 0.0
   pdf_temp(:,:,:,:) = 0
!
!  Start the calculation
   if (pdf%lexact) then
       call pdf%addcorr_e_all(atom)
   else
       call pdf%addcorr_n_fast(atom,cell)
   endif
!
   call pdf%convtherm(1.0,rsum)
!
   call pdf%convert(numatoms(atom),cell)
!
   if (pdf%lexact) then
       call cart_to_frac(atom,cell)
   endif
!
   end subroutine pdf_determine

!-----------------------------------------------------------------------------------------

   subroutine addcorr_e_all(pdf,atom)
!                                                                      
!  Calculate correlation for given atom ia, exact version, all atoms
!                                                                      
   USE atom_type_util
   USE cgeom
   class(pdf_type), intent(inout)             :: pdf
   type(atom_type), dimension(:), allocatable :: atom ! atoms in cartesian form
   integer                                    :: ia,iatom,is,js,crnat
   integer                                    :: ipdf_rmax,ibin
!
   ipdf_rmax = int(pdf%rmax/pdf%deltar) + 1
   crnat = numatoms(atom)
   if (lprint) write(0,*)'=====ipdf_rmax=',ipdf_rmax,pdf%deltars,pdf%deltar,pdf%rmax
   do ia=1,crnat  !outer loop over all atoms
      is = atom(ia)%asym
      pdf_temp(0,is,is,0) = pdf_temp(0,is,is,0) + 1
      do iatom=ia+1,crnat    !inner loop over all atoms
         js = atom(iatom)%asym
         ibin = int((distanzaC(atom(ia)%xc,atom(iatom)%xc) + pdf%deltars)/pdf%deltar)
            if (lprint) write(81,*)'IBIN=',ibin,distanzaC(atom(ia)%xc,atom(iatom)%xc),atom(ia)%xc,atom(iatom)%xc
         if (ibin <= ipdf_rmax) then
             pdf_temp (ibin, is, js,0) = pdf_temp (ibin, is, js,0) +1
             pdf_temp (ibin, js, is,0) = pdf_temp (ibin, js, is,0) +1
             !write(70,*)'TEMP=',ibin,is,js,pdf_temp(ibin,is,js,0),distanzaC(atom(ia)%xc,atom(iatom)%xc)
         endif
      enddo
   enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!
      DO is=1,pdf%nscat    ! Outer loop over all atoms
         DO js=1,pdf%nscat
            do ibin=0,ipdf_rmax
               if (pdf_temp(ibin,is,js,0) > 0) then
               if (lprint) write(70,*)'TEMP=',ibin,is,js,pdf_temp(ibin,is,js,0)
               endif
            enddo
         enddo
      enddo
!
   end subroutine addcorr_e_all 

!-----------------------------------------------------------------------------------------

   subroutine addcorr_n_fast(pdf,atom,cell)
   USE atom_type_util
   USE unit_cell
   USE cgeom
   USE molpnew, only: cr_index_start, cr_index_end
   class(pdf_type), intent(inout)                         :: pdf
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(cell_type), intent(in)                            :: cell
   integer                                                :: ia,i,j,k,ii
   integer, dimension(3) :: istart,iend
      integer, dimension(3) :: tra
   integer, dimension(3) :: lbindex,ubindex
   !integer :: natc1,natc2,ind
   integer :: natc1,ind
   real :: dist
   integer :: is,js,ibin
   real, dimension(3) :: offset
   integer, dimension(3) :: cellpos,icell
!
!!!TODO: offset is important, pdf_bnd
   lbindex = lbound(cr_index_start)
   ubindex = ubound(cr_index_start)
   do ia=1,numatoms(atom)
      tra = atom(ia)%op%tra
      is = atom(ia)%asym
      if (pdf%allowed_i(is) .or. pdf%allowed_j(is)) then
          do i=1,3
             !istart(i) = tra(i) - 1 - int(pdf%rmax/cell%get_par(i))
             istart(i) = tra(i) - ceiling(pdf%rmax/cell%get_par(i))

             !iend(i) =  tra(i) + 1 + int(pdf%rmax/cell%get_par(i))
             iend(i) =  tra(i) + ceiling(pdf%rmax/cell%get_par(i))
             !istart(i) = max(istart(i),lbindex(i))
             !iend(i) = min(iend(i),ubindex(i))
          enddo
          do i=1,3
          enddo
          !write(79,*)'IA=',ia,atom(ia)%op%tra,atom(ia)%xc,[(istart(i),iend(i),i=1,3)]
!!!!TODO: islook,offzero,self correlation peak
          natc1 = 0
          do i=istart(1),iend(1)
             do j=istart(2),iend(2)
                do k=istart(3),iend(3)
                   !write(79,*)'IA=',i,j,k,cr_index_start(i,j,k),cr_index_start(i,j,k)
!
!                  Compute offset
                   cellpos = [i,j,k]
                   do ii=1,3
                      icell(ii) = pdf_bnd(ii,cellpos(ii))
                      offset(ii) = real(cellpos(ii) - icell(ii))
                   enddo
                   if (lprint) write(79,*)'I,J,K=',i,j,k,'icell=',icell,' OFF=',offset
                   !do ind = cr_index_start(i,j,k),cr_index_end(i,j,k)
                   do ind = cr_index_start(icell(1),icell(2),icell(3)),cr_index_end(icell(1),icell(2),icell(3))
                      js = atom(ind)%asym
                      if (pdf%allowed_i(js) .and. pdf%allowed_j(js)) then
                          dist = distanzaC(atom(ind)%xc+offset,atom(ia)%xc,cell%get_g())
                          if (dist <= pdf%rmax) then
                              ibin = int((dist + pdf%deltars)/pdf%deltar)
                              pdf_temp(ibin,is,js,0) = pdf_temp(ibin,is,js,0) + 1
                              natc1 = natc1 + 1
                          endif
                      endif
                   enddo
                enddo
             enddo
          enddo
          if (lprint) write(79,*)'IA_NATC=',natc1
!!!!uncomment for check
          !natc2 = 0
          !do i=1,numatoms(atom)
          !   if (distanzaC(atom(i)%xc,atom(ia)%xc,cell%get_g()) <= pdf%rmax) then
          !       natc2 = natc2 + 1
          !   endif
          !enddo
          !write(79,*)'IA_NATC=',natc2
          !if (natc1 /= natc2) write(79,*)'ERROR'
!!!!check
      endif
   enddo
     !stop
!
   end subroutine addcorr_n_fast

!-----------------------------------------------------------------------------------------

   subroutine convtherm(pdf,rsign,rsum)
!                                                                      
!  Convolute the pair correlation histograms with the thermal Gaussian                                                  
!                                                                      
   USE trig_constants
   class(pdf_type), intent(inout) :: pdf
   real, intent(in)               :: rsign
   real, intent(out)              :: rsum
   real                           :: fac,sqrt_zpi,dist,dist2
   integer                        :: cr_nscat
   integer                        :: is,js,il,ii,ibin
!
   fac = 1.0 / (2.0 * twopi**2)
   sqrt_zpi =1.0/sqrt(twopi)
   cr_nscat = size(pdf%weight,1)
   ii = int (pdf%rmax / pdf%deltar) + 1
   if (lprint) write(0,*)'=====pdf_gauss,pdf_qalp,sqrt_zpi=',pdf%gauss,pdf%qalp,sqrt_zpi,pdf%nmol,ii,cr_nscat,pdf%nmol
   rsum = 0
   do is = 1, cr_nscat
      do js = 1, cr_nscat
         if ( (pdf%allowed_i (is) .and. pdf%allowed_j (js) ) .or.   &
              (pdf%allowed_j (is) .and.pdf%allowed_i (js) ) ) then
               do il=0,pdf%nmol
                  do ibin = 1, ii
                     if(pdf_temp(ibin,is,js,il)>0) then
!                                                                       
!                       Convolute with Gaussian                               
                        if (pdf%gauss .or. pdf%qalp > 0) then
                            dist = ibin * pdf%deltar
                            dist2 = dist**2
!!!!!!!!!!!!!!!!!TODO
                        else
                            pdf_corr(ibin) = pdf_corr(ibin) + pdf_temp(ibin,is,js,il) * rsign * pdf%weight(is,js)
                            !if (lprint) write(72,*)'pdf_corr=',ibin,pdf_corr(ibin)
                        endif
                        rsum = rsum + pdf%weight(is,js)
                     endif
                  enddo
               enddo
         endif
      enddo
   enddo
           
                            if (lprint) then
                                do ibin=1,size(pdf_corr)
                                   write(72,'(a,i10,f12.6)')'pdf_corr=',ibin,pdf_corr(ibin)
                                enddo
                            endif
!
   end subroutine convtherm 

!-----------------------------------------------------------------------------------------
  
   subroutine convert(pdf,cr_natoms,cell)
!                                                                       
!  Convert to G(r) and do convolution                                
!                                                                  
   USE trig_constants
   USE unit_cell
   USE arrayutil
   USE nr, only: convlv
   USE trig_constants
   class(pdf_type), intent(inout) :: pdf
   integer, intent(in)            :: cr_natoms
   type(cell_type), intent(in)    :: cell
   integer                        :: i,ncc
   real                           :: norm,r,rr,r0,factor
   real, dimension(:), allocatable :: conv
!
   ncc = pdf%cr_icc(1)*pdf%cr_icc(2)*pdf%cr_icc(3)
   r0 = cr_natoms / (cell%volume()*ncc)
!
   norm = pdf%scal / cr_natoms
   if (.not.pdf%gauss .and. pdf%qalp == 0.0) norm = norm / pdf%deltar

        pdf%calc(:) = 0  !!!TO CHECK: it seems necessary in convlv because the loop 1:pdf%bin doesn0t fill all array
   do i=1,pdf%bin
      r = i*pdf%deltar
      rr = 2.0 * twopi * r * r0 * pdf%dnorm
      pdf%calc(i) = norm * pdf_corr(i) / r - rr
      if (lprint) write(74,'(a,i10,4f12.6)')'pdf_calc=',i,pdf_corr(i),pdf%calc(i),norm,r-rr
   enddo
!                                                                       
!  Convolute with SINC function                                    
   if (pdf%qmax > 0.0) then
       call new_array(conv,size(pdf%calc))
       if (lprint) write(0,*)'SIZEconv=',size(pdf%calc),size(pdf%sincc),pdf%bin,maxval(pdf%calc),minval(pdf%calc)
       conv = convlv(pdf%calc,pdf%sincc,1)
       factor = pdf%deltar / twopi * 2.
       write(75,*)'FACTOR=',factor
       do i = 1, pdf%bin
            if (lprint) write(75,'(a,2f20.6)')'conv=',conv(i),pdf%calc(i)
          pdf%calc(i) = conv(i) * factor
            !if (lprint) write(76,*)')i*pdf%deltar,pdf%calc(i)
       enddo
   endif
!
   end subroutine convert

!-----------------------------------------------------------------------------------------
  
   subroutine save_file(pdf,x,y)
   USE arrayutil
   class(pdf_type), intent(inout) :: pdf
   real, dimension(:), allocatable, optional :: x,y
   integer :: nmi,nma,i,np
!
   nmi = nint(pdf%rfmin/pdf%deltar)
   nma = nint(pdf%rfmax/pdf%deltar)
   if (present(x) .and. present(y)) then
       call new_array(x,nma-nmi+1)
       call new_array(y,nma-nmi+1)
       np = 0
       do i=nmi,nma
          np = np + 1
          x(np) = i*pdf%deltar
          y(np) = pdf%calc(i)*pdf%scal
          !write(71,*)i*pdf%deltar,pdf%calc(i)*pdf%scal
          if (lprint) write(71,*)x(np),y(np)
       enddo
   endif
!
   end subroutine save_file

!-----------------------------------------------------------------------------------------
!corr #if 0
!corr    subroutine make_crystal(atom,cell,spg,np,atomcr)
!corr    USE atom_type_util
!corr    USE spginfom
!corr    USE unit_cell
!corr    USE arrayutil
!corr    type(atom_type), dimension(:), allocatable, intent(in)  :: atom
!corr    type(cell_type), intent(in)                             :: cell
!corr    type(spaceg_type), intent(in)                           :: spg
!corr    integer, dimension(3), intent(in)                       :: np
!corr    type(atom_type), dimension(:), allocatable, intent(out) :: atomcr
!corr    type(atom_type)                                         :: atmp
!corr    integer                                                 :: nat,natcr
!corr    integer                                                 :: i,j,ia
!corr    integer                                                 :: k1,k2,k3,pos
!corr    integer, dimension(3)                                   :: ncr_i,ncr_f
!corr ! 
!corr    nat = numatoms(atom)
!corr    if (nat == 0) return
!corr ! 
!corr    do i=1,3
!corr       if (mod(np(i),2) == 0) then
!corr           ncr_f(i) = np(i)/2
!corr           ncr_i(i) = -ncr_f(i) + 1
!corr       else
!corr           ncr_f(i) = np(i)/2
!corr           ncr_i(i) = -ncr_f(i)
!corr       endif
!corr       !write(0,*)'MAKE CRYSTAL:',ncr_i(i),ncr_f(i)
!corr    enddo
!corr ! 
!corr    if (allocated(asymtab)) deallocate(asymtab)  !!!TOFIX
!corr    allocate(asymtab(nat,3,spg%nsymop))
!corr    call make_symmetry_table(atom,spg,asymtab)
!corr    !call new_atoms(atomcr,spg%nsymop*nat*((2*np(1)+1)*(2*np(2)+1)*(2*np(3)+1)))
!corr    call new_atoms(atomcr,spg%nsymop*nat*np(1)*np(2)*np(3))
!corr    call new_array(cr_index_start,ncr_i,ncr_f)
!corr    call new_array(cr_index_end,ncr_i,ncr_f)
!corr ! 
!corr !  Copy a.u.
!corr    atomcr(:nat) = atom(:)
!corr    do i=1,nat
!corr       call translate_in_cell(atomcr(i))
!corr       atomcr(i)%asym = i
!corr       atomcr(i)%op = op_type()
!corr    enddo
!corr    natcr = nat
!corr ! 
!corr !  Apply symmetry operators
!corr    do i=1,nat
!corr       do j=2,spg%nsymop
!corr          atmp = atomcr(i)
!corr          atmp%xc = asymtab(i,:3,j)
!corr          atmp%op%op = j
!corr          call translate_in_cell(atmp)
!corr          if (check_position(atmp,atomcr,natcr,cell) == 0) then
!corr              natcr = natcr + 1
!corr              atomcr(natcr) = atmp
!corr          endif
!corr       enddo
!corr    enddo
!corr    pos = natcr
!corr    cr_index_start(0,0,0) = 1
!corr    cr_index_end(0,0,0) = natcr
!corr ! 
!corr !  Apply translation np to group of atoms
!corr    do k1=ncr_i(1),ncr_f(1)
!corr       do k2=ncr_i(2),ncr_f(2)
!corr          do k3=ncr_i(3),ncr_f(3)
!corr             if (all([k1,k2,k3] == 0)) cycle
!corr             atomcr(natcr+1:natcr+pos) = atomcr(:pos)
!corr             call translate_atoms(atomcr(natcr+1:natcr+pos),real([k1,k2,k3]))
!corr !
!corr !           set cell index, ex. -2,-1,0,1,2 for 5x5
!corr             do ia=natcr+1,natcr+pos
!corr                atomcr(ia)%op%tra = [k1,k2,k3]
!corr             enddo
!corr             cr_index_start(k1,k2,k3) = natcr+1
!corr             natcr = natcr + pos
!corr             cr_index_end(k1,k2,k3) = natcr
!corr          enddo
!corr       enddo
!corr    enddo
!corr    if (lprint) write(0,*)'NATCR=',natcr,numatoms(atomcr)
!corr    call resize_atoms(atomcr,natcr)
!corr !TODO !!!temporary additional check
!corr    do i=1,natcr
!corr       pos = check_position(atomcr(i),atomcr,natcr,cell)
!corr       if (pos /= i) then
!corr           if (lprint) write(0,*)'ATOM=',i,pos
!corr       endif
!corr    enddo
!corr ! 
!corr    end subroutine make_crystal
!corr 
!corr !-----------------------------------------------------------------------------------------
!corr 
!corr    integer function check_position(atom,atoms,nats,cell) result(pos)
!corr ! 
!corr !  Check for duplicate atoms
!corr ! 
!corr    USE unit_cell
!corr    USE atom_basic
!corr    type(atom_type), intent(in)               :: atom
!corr    type(atom_type), dimension(:), intent(in) :: atoms
!corr    integer, intent(in)                       :: nats
!corr    type(cell_type), intent(in)               :: cell
!corr    real, dimension(3)                        :: dx
!corr    real                                      :: djk
!corr    real, parameter                           :: D2MIN = 0.3*0.3   ! square of minimum distance
!corr    integer                                   :: i
!corr ! 
!corr    pos = 0
!corr    do i=1,nats
!corr       if (atom%asym == atoms(i)%asym) then
!corr           dx = atom%xc - atoms(i)%xc
!corr           djk = DOT_PRODUCT(dx,MATMUL(cell%get_g(),dx))
!corr           if (djk < D2MIN) then
!corr               pos = i
!corr               return
!corr           endif
!corr       endif
!corr    enddo
!corr ! 
!corr    end function check_position
!corr #endif
!-----------------------------------------------------------------------------------------

   subroutine thermal_displ(atom,cell)
!
!  Displaces atoms in a completely random fashion. The mean    
!  square displacement is given by the temperature factor. 
!
   USE trig_constants
   USE rand_mod
   USE unit_cell
   USE atom_type_util
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   type(cell_type), intent(in)                               :: cell
   real, parameter                                           :: bfac = 1.0/(4.0*twopis)
   integer                                                   :: i,ii
   real                                                      :: a
   real, dimension(3) :: up,uc
   real, dimension(3,3) :: cr_gmat
   logical :: lprint = .false.
!
   if (lprint) then
       write(82,*)'GMAT=',cell%get_g()
       write(82,*)'RMAT=',cell%get_r()
   endif
   call trafo([0.,0.,1.],cell,cr_gmat)
   if (lprint) write(82,*)'CR_GMAT=',cr_gmat
   do i=1,numatoms(atom)
      a = sqrt(bfac*atom(i)%biso)
      write(82,*)'=====A =',a,atom(i)%biso
      do ii=1,3
         up(ii) = random_normal()*a
      enddo
      uc = matmul(cr_gmat,up)
      if (lprint) then
          write(82,*)'=====UP =',up
          write(82,*)'=====UC =',uc
      endif
      atom(i)%xc = atom(i)%xc + uc
   enddo
!
   end subroutine thermal_displ 

!-----------------------------------------------------------------------------------------

   subroutine test_pdf(atom,cell,spg,elem,radtype)
   USE atom_type_util
   USE spginfom
   USE unit_cell
   USE elements
   USE molpnew
            !USE cgeom
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(cell_type), intent(in)                               :: cell
   type(spaceg_type), intent(in)                             :: spg
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   integer, intent(in)                                       :: radtype
   type(atom_type), dimension(:), allocatable                :: atomcr
   type(pdf_type)                                            :: pdf
!corr     real, dimension(3) :: h,k
!corr     type(cell_type) :: cell0
   type(element_type), dimension(:), allocatable :: elpdf
   integer :: radpdf
   real, dimension(:), allocatable :: x,y
! 
!corr!
!corr!  input parameters
   pdf%cr_icc = [3,3,3]
   pdf%rmax = 10.0
   !pdf%rmax = 6.5
   pdf%qmax = 25
!corr!
!corr   pdf%deltars = pdf%deltar/2.
   pdf%rfmax = pdf%rmax
   pdf%rfmin = pdf%rmin
!corr   nn = int (pdf%rmax / pdf%deltar) + 1
!corr   pdf%ndat = max(pdf%ndat,nn)
!corr   pdf%bin = nn
!
!  For pdf radiation
   radpdf = RX_SOURCE ! RX_SOURCE, NEUTRON_SOURCE, ELECTRON_SOURCE
   call copy_elem(elpdf,elem)
   call elem_set_radiation(elpdf,radpdf)
!
   pdf%nscat = numatoms(atom)
   call make_crystal(atom,cell,spg,pdf%cr_icc,atomcr)
   !call print_atoms(atomcr,kpr=73)
   !call thermal_displ(atomcr,cell)
   !call print_atoms(atomcr,kpr=73)
! 
   call pdf%setup(cell,elpdf,atomcr,radpdf)
   call pdf%determine(atomcr,cell)
   call pdf%save_file(x,y)
!
   end subroutine test_pdf

end module pdfcalc
