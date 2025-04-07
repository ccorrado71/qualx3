MODULE datasetmod

   USE pointmod, only: point_type
   USE background, only: back_condition_type
   USE profile_function, only: profile_function_t
   USE plotstyle, only: style_type
   USE ssmoothing, only: smooth_condition_type 

   implicit none

   integer, parameter :: POW_DATA=1,SCRYS_DATA=2
   integer, parameter :: BB_GEOMETRY=1, DS_GEOMETRY=2

   type dataset_type
     real, dimension(:), allocatable             :: x        ! observed x
     real, dimension(:), allocatable             :: x0       ! observed corrected
     real, dimension(:), allocatable             :: y        ! observed y
     real, dimension(:), allocatable             :: yc       ! calculated on all crystal phases
     real, dimension(:), allocatable             :: ys       ! observed y subtracted for background = y-yb
     real, dimension(:), allocatable             :: wei      ! weight of data
     integer                                     :: nwave = 0
     real, dimension(:), allocatable             :: wave
     real, dimension(:), allocatable             :: ratio
     integer                                     :: radtype
     logical                                     :: sync = .false.
     character(len=256)                          :: fname = ' '
     real                                        :: tstep,tmin,tmax,dmin
     integer                                     :: datatype = POW_DATA
     integer                                     :: nc1,nc2  ! range for calculated (yb and yc)
!
!    dataset variables
     integer                                     :: geom = BB_GEOMETRY
     real                                        :: zero=0.0,zsd=0.0
     real                                        :: sdisp=0.0,sdsd=0.0
     real                                        :: stran=0.0,stsd=0.0
     integer                                     :: zcode=0,sdcode=0,stcode=0
!
!    background variables
     real, dimension(:), allocatable             :: yb          ! background
     type(back_condition_type)                   :: cond        ! settings for create backgrund
     type(point_type), dimension(:), allocatable :: points
     real                                        :: thzerob=0.0 ! zero of polynomial
     real, dimension(:), allocatable             :: coef        ! background coefficients
     integer, dimension(:), allocatable          :: bcode       ! code for coefficients refinement
     real, dimension(:), allocatable             :: bsd         ! sd for coefficients
!
!    smoothing variables
     type(smooth_condition_type)                 :: scond
     real, dimension(:), allocatable             :: smoothvec
!
!    profile function variables
     integer, dimension(:), allocatable                  :: typefun
     type(profile_function_t), dimension(:), allocatable :: pear
     type(profile_function_t), dimension(:), allocatable :: pvoi
     type(profile_function_t), dimension(:), allocatable :: tch
!
     type(style_type) :: style = style_type(1,26,36,1,1,1,3,8,1)

   contains

     ! Data retrieval methods
     procedure          :: data_read => read_data_bin_s
     procedure          :: npoints => get_npoints
     procedure          :: npointsc => get_npointsc
     procedure          :: npoints_back
     procedure, private :: get_points_p
     procedure, private :: get_points_a
     generic            :: get_points => get_points_a, get_points_p
     procedure          :: get_coef
     procedure          :: zeroback
     procedure          :: get_cond
     procedure          :: n_ref_bcoef
     procedure          :: xminc => get_xminc
     procedure          :: xmaxc => get_xmaxc
     procedure          :: xminc0 => get_xminc0
     procedure          :: xmaxc0 => get_xmaxc0
     procedure          :: resolution
     procedure          :: has_back

     ! Data modification methods
     procedure          :: set_wave
     procedure          :: set_data
     procedure          :: resize
     procedure          :: set_points
     procedure          :: make_background
     procedure          :: set_cond
     procedure          :: smooth_calculate
     procedure          :: smooth_apply
     procedure, private :: set_limit_n
     procedure, private :: set_limit_x
     generic            :: set_limit => set_limit_n, set_limit_x
     procedure          :: set_calculate
     procedure          :: apply_shifts
     generic            :: set_ys => set_ys_y, set_ys_n
     procedure          :: set_ys_y
     procedure          :: set_ys_n
     procedure          :: set_wei
     procedure          :: scale_data

     ! Output
     procedure          :: data_save => save_data_bin_s
     procedure          :: prn => print_info

   end type dataset_type

CONTAINS

   integer function get_npoints(datas)
!
!  Number of observed points in the daset
!
   USE arrayutil
   class(dataset_type), intent(in) :: datas
   get_npoints = size_array(datas%x)
   end function get_npoints 

!-------------------------------------------------------------------------

   integer function get_npointsc(datas)
!
!  Number of calculated/background points
!
   USE arrayutil
   class(dataset_type), intent(in) :: datas
   get_npointsc = size_array(datas%yc)
   end function get_npointsc 

!-------------------------------------------------------------------------

   subroutine set_data(this,x,y,np,fname,datatype,x0)
   USE arrayutil
   class(dataset_type), intent(inout)       :: this
   real, dimension(:), intent(in)           :: x,y
   integer, intent(in)                      :: np
   character(len=*), intent(in)             :: fname
   integer, intent(in)                      :: datatype
   real, dimension(:), optional, intent(in) :: x0
!
   this%datatype = datatype
   this%fname = fname
   if (np > 0) then
       call new_array(this%x,np)
       call new_array(this%x0,np)
       call new_array(this%y,np)
       this%x(:) = x(:np)
       this%y(:) = y(:np)
       this%tmin = this%x(1)
       this%tmax = this%x(np)
       this%nc1 = 1
       this%nc2 = size(x)
       if (present(x0)) then
           this%x0 = x0
       else
           this%x0 = x
       endif
   endif
!
   end subroutine set_data

!-------------------------------------------------------------------------

   subroutine set_calculate(this)
   USE arrayutil
   class(dataset_type), intent(inout) :: this
   call new_array(this%yc,this%nc2-this%nc1+1)
   end subroutine set_calculate

!-------------------------------------------------------------------------

   subroutine apply_shifts(this)
   class(dataset_type), intent(inout) :: this
   this%x0 = this%x - 2.0 * this%zero + sample_displacement(this%x0,this%sdisp,this%geom)   &
                    + sample_transparency(this%x0,this%stran,this%geom)
   end subroutine apply_shifts

!-------------------------------------------------------------------------

   subroutine set_ys_y(this,ys)
   class(dataset_type), intent(inout) :: this
   real, dimension(:)                 :: ys
   if (allocated(this%ys)) deallocate(this%ys)
   allocate(this%ys(size(ys)),source=ys)
   end subroutine set_ys_y

!-------------------------------------------------------------------------

   subroutine set_ys_n(this,n)
   class(dataset_type), intent(inout) :: this
   integer, intent(in)                :: n
   if (allocated(this%ys)) deallocate(this%ys)
   allocate(this%ys(n),source=this%y(:n)-this%yb(:n))
   end subroutine set_ys_n

!-------------------------------------------------------------------------

   subroutine set_wei(this,wei)
   class(dataset_type), intent(inout) :: this
   real, dimension(:)                 :: wei
   if (allocated(this%wei)) deallocate(this%wei)
   allocate(this%wei(size(wei)),source=wei)
   end subroutine set_wei

!-------------------------------------------------------------------------

   subroutine scale_data(this,scal)
   class(dataset_type), intent(inout) :: this
   real, optional, intent(in)         :: scal
   real                               :: scald
   if (present(scal)) then
       scald = scal
   else
       scald = 1000.
   endif
   this%y = scald * this%y / maxval(this%y)
   end subroutine scale_data

!-------------------------------------------------------------------------

   subroutine set_wave(this,nwave,wavel,ratio)
   USE arrayutil
   class(dataset_type), intent(inout)       :: this
   integer, intent(in)                      :: nwave
   real, dimension(:), intent(in)           :: wavel
   real, dimension(:), intent(in), optional :: ratio
   real, dimension(2), parameter            :: RATIODEF = [1.0,0.5]
!
   if (nwave == 0) return
   call new_array(this%wave,nwave)
   call new_array(this%ratio,nwave)
   this%nwave = nwave
   if (present(ratio)) then
       this%ratio = ratio(:nwave)
   else
       if (this%nwave <= size(ratiodef)) then
           this%ratio(:this%nwave) = RATIODEF(:this%nwave)
       else
           this%ratio(:size(RATIODEF)) = RATIODEF(:)
           this%ratio(size(RATIODEF)+1:) = 0.0
       endif
   endif
   this%wave = wavel(:nwave)
!
   end subroutine set_wave

!-------------------------------------------------------------------------

   subroutine save_data_bin_s(this,unitbin)
   USE arrayutil
   class(dataset_type), intent(in) :: this
   integer, intent(in)             :: unitbin
   integer                         :: np,npyc,npb,npbp,npc
!
   np = this%npoints()
   write(unitbin) this%datatype,np,this%nwave
   if (this%nwave > 0) then
       write(unitbin)this%wave,this%ratio
   endif
   if (np > 0) then
       npyc = size_array(this%yc)
       npb = size_array(this%yb)
       npbp = this%npoints_back()
       npc = size_array(this%coef)
       write(unitbin) this%x,this%x0,this%y,this%tstep,    &
                      this%tmin,this%tmax,this%radtype,this%sync,this%fname,this%datatype, &
                      this%nc1,this%nc2,this%cond,this%thzerob,npyc,npb,npbp,npc
       if (npyc > 0) then
           write(unitbin)this%yc
       endif
       if (npb > 0) then
           write(unitbin)this%yb
       endif
       if (npbp > 0) then
           write(unitbin)this%points
       endif
       if (npc > 0) then
           write(unitbin)this%coef
       endif
       write(unitbin)this%style
   endif
!
   end subroutine save_data_bin_s

!-------------------------------------------------------------------------
   
   subroutine save_data_bin(datas,unitbin)
   type(dataset_type), dimension(:), allocatable, intent(in) :: datas
   integer, intent(in)                                       :: unitbin
   integer :: i
!
   write(unitbin) ndataset(datas)
   do i=1,ndataset(datas)
      call datas(i)%data_save(unitbin)
   enddo
!
   end subroutine save_data_bin 

!-------------------------------------------------------------------------

   subroutine read_data_bin_s(this, unitbin, err)
   USE arrayutil
   USE errormod
   USE pointmod
   class(dataset_type), intent(inout) :: this
   integer, intent(in)                :: unitbin
   type(error_type), intent(out)      :: err
   integer                            :: ier
   integer                            :: np,npyc,npb,npbp,npc
!
   read(unitbin,iostat=ier,err=10)this%datatype,np,this%nwave
   if (this%nwave > 0) then
       call new_array(this%wave,this%nwave)
       call new_array(this%ratio,this%nwave)
       read(unitbin,iostat=ier,err=10)this%wave,this%ratio
   endif
   if (np > 0) then
       call new_array(this%x,np)
       call new_array(this%x0,np)
       call new_array(this%y,np)
       read(unitbin,iostat=ier,err=10)                                                     &
                      this%x,this%x0,this%y,this%tstep,    &
                      this%tmin,this%tmax,this%radtype,this%sync,this%fname,this%datatype, &
                      this%nc1,this%nc2,this%cond,this%thzerob,npyc,npb,npbp,npc
       if (npyc > 0) then
           call new_array(this%yc,npyc)
           read(unitbin,iostat=ier,err=10)this%yc
       endif
       if (npb > 0) then
           call new_array(this%yb,npb)
           read(unitbin,iostat=ier,err=10)this%yb
       endif
       if (npbp > 0) then
           call new_points(this%points,npbp)
           read(unitbin,iostat=ier,err=10)this%points
       endif
       if (npc > 0) then
           call new_array(this%coef,npc)
           read(unitbin,iostat=ier,err=10)this%coef
       endif
       read(unitbin,iostat=ier,err=10)this%style
   endif

10 continue
   if (ier /= 0) then
       call err%set('Error on reading diffraction data')
   endif
   end subroutine read_data_bin_s

!-------------------------------------------------------------------------

   subroutine read_data_bin(datas,unitbin,err)
   USE errormod
   type(dataset_type), intent(inout), dimension(:), allocatable :: datas
   integer, intent(in)                                          :: unitbin
   type(error_type), intent(out)                                :: err
   integer                                                      :: i,ier,ndat

   read(unitbin,iostat=ier) ndat
   if (ier /= 0) then
       call err%set("Error on reading datasets")
       return
   endif
! 
   call new_dataset(datas,ndat)
   do i=1,ndat
      call datas(i)%data_read(unitbin,err)
      if (err%signal) return
   enddo

   end subroutine read_data_bin

!-------------------------------------------------------------------------

   subroutine resize(this,xmin,xmax)
   USE arrayutil
   class(dataset_type), intent(inout) :: this
   real, intent(in), optional         :: xmin,xmax
   integer                            :: ipmin,ipmax,olddim,newdim
!
   olddim = size_array(this%x)
   if (olddim == 0) return

   if (present(xmin)) then
       ipmin = clocate(this%x,xmin)                  ! locazione del punto + vicino
       if (this%x(ipmin) < xmin) ipmin = ipmin + 1   ! ma non deve essere < di xmin
   else
       ipmin = 1
   endif
   if (present(xmax)) then
       ipmax = clocate(this%x,xmax)                  ! locazione del punto + vicino
       if (this%x(ipmax) > xmax) ipmax = ipmax - 1   ! ma non deve essere > di xmax
   else
       ipmax = olddim
   endif
   newdim = ipmax - ipmin + 1
   if (newdim > 0 .and. newdim /= olddim) then
       this%x(1:newdim) = this%x(ipmin:ipmax)
       this%x0(1:newdim) = this%x0(ipmin:ipmax)
       this%y(1:newdim) = this%y(ipmin:ipmax)
       call resize_array(this%x,newdim)
       call resize_array(this%x0,newdim)
       call resize_array(this%y,newdim)
       call this%set_limit(n2=newdim)
!corr       this%nc1 = 1
!corr       this%nc2 = size(x)
!corr!
!corr       if (allocated(this%yb)) then
!corr           this%yb(1:newdim) = this%yb(ipmin:ipmax)
!corr           call resize_array(this%yb,newdim)
!corr       endif
!corr       if (allocated(this%yc)) then
!corr           this%yc(1:newdim) = this%yc(ipmin:ipmax)
!corr           call resize_array(this%yc,newdim)
!corr       endif
       this%tmin = this%x(1)
       this%tmax = this%x(newdim)
   endif
!
   end subroutine resize

!----------------------------------------------------------------------------------------------

   subroutine print_info(this,kpr,endw)
   USE arrayutil
   class(dataset_type), intent(in) :: this
   integer, intent(in)             :: kpr
   logical, intent(in)             :: endw
   integer                         :: i
!
   write(kpr,'(/16x,52("*"))')
   select case (this%datatype)
     case (POW_DATA)
       write(kpr,'(16x,a)')             '*              Pattern information                 *'
       write(kpr,'(16x,a)')             '*                                                  *'
       write(kpr,'(16x,a,f8.4,15x,a)')  '*          2-Theta min    = ',this%tmin,'*'
       write(kpr,'(16x,a,f8.4,15x,a)')  '*          2-Theta max    = ',this%tmax,'*'
       write(kpr,'(16x,a,f8.4,15x,a)')  '*          Step           = ',this%tstep,'*'
       if (this%nwave > 0) then
           if (this%nwave == 1) then
       write(kpr,'(16x,a,f10.6,13x,a)') '*          Wavelength     = ',this%wave(1),'*'
           else
       write(kpr,'(16x,a,2f10.6,3x,a)') '*          Wavel.,ratio   = ',this%wave(1),this%ratio(1),'*'
               do i=2,this%nwave
       write(kpr,'(16x,a,2f10.6,3x,a)') '*                           ',this%wave(i),this%ratio(i),'*'
               enddo
           endif
       endif
       write(kpr,'(16x,a,i8,15x,a)')    '*          Ncounts        = ',size_array(this%x),'*'
       write(kpr,'(16x,a)')             '*                                                  *'

     case (SCRYS_DATA)

   end select
   if (endw) write(kpr,'(16x,52("*"))')
   end subroutine print_info

!----------------------------------------------------------------------------------------------

   integer function ndataset(datas)
   type(dataset_type), allocatable, intent(in) :: datas(:)
!
   if (allocated(datas)) then
       ndataset = size(datas)
   else
       ndataset = 0
   endif
!
   end function ndataset

!----------------------------------------------------------------------------------------------

   subroutine resize_dataset(datas,n)
!
!  Rialloca ad n un vettore reale.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(dataset_type), allocatable, intent(inout) :: datas(:)
   integer, intent(in)                            :: n
   integer                                        :: nv
   type(dataset_type), allocatable                :: vsav(:)
   integer                                        :: nsav
!
!  se n = 0 (riallocazione a 0): dealloca ed esci
   if (n == 0) then
       if (allocated(datas)) deallocate(datas)
       return
   endif
!
   if (.not.allocated(datas)) then
       allocate(datas(n))
   else
!
       nv = size(datas)
!
!      nsav contiene qual è la porzione di datas da salvare
       select case(nv-n)
         case (1:)       ! compatta x ad n
           nsav = n
         case (:-1)      ! espandi x ad n
           nsav = nv
         case (0)
           return        ! n=nv non fare niente
       end select
       allocate(vsav(n))
       vsav(:nsav) = datas(:nsav)
       call move_alloc(vsav,datas)
   endif
!
   end subroutine resize_dataset

!----------------------------------------------------------------------------------------------------

   subroutine new_dataset(datas,n)
!
!  Create new atoms
!
   type(dataset_type), allocatable, intent(inout) :: datas(:)
   integer, intent(in)                            :: n

   if (n < 0) return
   if (ndataset(datas) /= n) then
       if (allocated(datas))deallocate(datas)
       if (n > 0) allocate(datas(n))
   endif

   end subroutine new_dataset

!----------------------------------------------------------------------------------------------------

   subroutine clear_dataset(datas)
!
!  Delete all phases
!
   type(dataset_type), allocatable, intent(inout) :: datas(:)

   if (allocated(datas)) deallocate(datas)

   end subroutine clear_dataset

!----------------------------------------------------------------------------------------------------

   subroutine push_back_dataset(datas,val)
!
!  Adds a new phase at the end of the array
!
   type(dataset_type), allocatable, intent(inout) :: datas(:)
   type(dataset_type), intent(in)                 :: val
   integer                                        :: ndim
   ndim = ndataset(datas)
   call resize_dataset(datas,ndim+1)
   datas(ndim+1) = val
   end subroutine push_back_dataset

!----------------------------------------------------------------------------------------

   integer function npoints_back(datas)
   class(dataset_type), intent(in)                         :: datas
   if (allocated(datas%points)) then
       npoints_back = size(datas%points)
   else
       npoints_back = 0
   endif
   end function npoints_back

!----------------------------------------------------------------------------------------

   subroutine get_points_p(datas,points)
!
!  Get background points
!
   USE pointmod
   class(dataset_type), intent(in)                          :: datas
   type(point_type), dimension(:), allocatable, intent(out) :: points
   call copy_points(points,datas%points)
   end subroutine get_points_p

!----------------------------------------------------------------------------------------

   subroutine get_points_a(datas,vet)
!
!  Get background points
!
   USE pointmod
   USE arrayutil
   class(dataset_type), intent(in)                :: datas
   real, dimension(:,:), allocatable, intent(out) :: vet
   integer                                        :: np,i
   np = datas%npoints_back()
   if (np > 0) then
       call new_array(vet,[1,1],[np,2])
       do i=1,np
          vet(i,1) = datas%points(i)%x
          vet(i,2) = datas%points(i)%y
       enddo
   endif
   end subroutine get_points_a

!----------------------------------------------------------------------------------------

   subroutine set_points(datas,points)
!
!  Get background points
!
   USE pointmod
   class(dataset_type), intent(inout)                      :: datas
   type(point_type), dimension(:), allocatable, intent(in) :: points
   call copy_points(datas%points,points)
   end subroutine set_points

!----------------------------------------------------------------------------------------

   subroutine make_background(datas)
   USE arrayutil
   USE background, only: NMAXCOEFB, compute_background
   class(dataset_type), intent(inout) :: datas
   call resize_array(datas%coef,NMAXCOEFB)
   call resize_array(datas%bcode,NMAXCOEFB)
   call resize_array(datas%bsd,NMAXCOEFB)
   datas%bcode = 0
   datas%bsd = 0
   call new_array(datas%yb,datas%nc2-datas%nc1+1)
   call compute_background(datas%x0(datas%nc1:datas%nc2),datas%y(datas%nc1:datas%nc2),datas%yb,   &
                          datas%points,datas%coef,datas%thzerob,datas%cond,datas%wave(1))
   end subroutine make_background

!----------------------------------------------------------------------------------------

   subroutine get_coef(datas,coef)  !!!!FIXME - add allocation on coef
   class(dataset_type), intent(in) :: datas
   real, dimension(:), intent(out)    :: coef
   coef = datas%coef
   end subroutine get_coef

!----------------------------------------------------------------------------------------

   real function zeroback(datas)
   class(dataset_type), intent(in) :: datas
   zeroback = datas%thzerob
   end function zeroback

!----------------------------------------------------------------------------------------

   subroutine set_limit_n(datas,n1,n2)
!
!  Set limit for calculate. Default is lbound(x):ubound(x)
!
   USE arrayutil
   class(dataset_type), intent(inout) :: datas
   integer, intent(in), optional      :: n1,n2
   integer                            :: nc1,nc2
!
   if (present(n1)) then
       nc1 = n1
   else
       nc1 = lbound(datas%x,dim=1)
   endif
   if (present(n2)) then
       nc2 = n2
   else
       nc2 = ubound(datas%x,dim=1)
   endif
   datas%nc1 = nc1
   datas%nc2 = nc2
!
   if (allocated(datas%yb)) call resize_array(datas%yb,nc2-nc1+1)
   if (allocated(datas%yc)) call resize_array(datas%yc,nc2-nc1+1)
!
   end subroutine set_limit_n

!----------------------------------------------------------------------------------------

   subroutine set_limit_x(datas,xmin,xmax)
   use arrayutil
   class(dataset_type), intent(inout) :: datas
   real, intent(in)                   :: xmin,xmax
   integer                            :: nc1,nc2
   nc1 = clocate(datas%x,xmin)
   if (datas%x(nc1) < xmin .and. nc1 /= ubound(datas%x,dim=1)) nc1=nc1+1
   nc2 = clocate(datas%x,xmax)
   if (datas%x(nc2) > xmax .and. nc2 /= 1) nc2=nc2-1
   call datas%set_limit_n(nc1,nc2)
   end subroutine set_limit_x

!----------------------------------------------------------------------------------------

   subroutine set_cond(datas,cond)
!
!  Set conditions for background
!
   class(dataset_type), intent(inout) :: datas
   type(back_condition_type), intent(in) :: cond
   datas%cond = cond
   end subroutine set_cond

!----------------------------------------------------------------------------------------

   integer function n_ref_bcoef(datas)
!
!  Number of refined background coefficients
!    
   class(dataset_type), intent(in) :: datas
   integer                         :: i
!
   n_ref_bcoef =  0
   do i=1, datas%cond%ncoef
      if (datas%bcode(i) > 0) n_ref_bcoef = n_ref_bcoef + 1
   enddo
!
   end function n_ref_bcoef

!----------------------------------------------------------------------------------------

   subroutine get_cond(datas,cond)
!
!  Get conditions for background
!
   class(dataset_type), intent(in)     :: datas
   type(back_condition_type), intent(out) :: cond
   cond = datas%cond
   end subroutine get_cond

!----------------------------------------------------------------------------------------------------

   real function  get_xminc(datas)
   class(dataset_type), intent(in) :: datas
   get_xminc = datas%x(datas%nc1)
   end function get_xminc

!----------------------------------------------------------------------------------------------------

   real function get_xmaxc(datas)
   class(dataset_type), intent(in) :: datas
   get_xmaxc = datas%x(datas%nc2)
   end function get_xmaxc

!----------------------------------------------------------------------------------------------------

   real function  get_xminc0(datas)
   class(dataset_type), intent(in) :: datas
   get_xminc0 = datas%x0(datas%nc1)
   end function get_xminc0

!----------------------------------------------------------------------------------------------------

   real function get_xmaxc0(datas)
   class(dataset_type), intent(in) :: datas
   get_xmaxc0 = datas%x0(datas%nc2)
   end function get_xmaxc0

!----------------------------------------------------------------------------------------------------
 
   real function resolution(datas)
!
!  Compute data resolution = dmin
!
   use counts, only: dvalue
   class(dataset_type), intent(in) :: datas
   
   if (datas%datatype == POW_DATA) then
       resolution = dvalue(datas%tmax,datas%wave(1))
   elseif (datas%datatype == SCRYS_DATA) then
       resolution = datas%dmin
   endif
!
   end function resolution

!----------------------------------------------------------------------------------------------------

   logical function has_back(datas)
   use background, only: BK_NONE
   class(dataset_type), intent(in) :: datas
!
   has_back = datas%npoints_back() > 0  .and. datas%cond%btype /= BK_NONE
!
   end function has_back

!----------------------------------------------------------------------------------------------------

   real function tthvalue(resval,wave)
!   
!  Calcola 2theta dalla risoluzione
!
   USE trig_constants
   real, intent(in) :: resval,wave
!   
   tthvalue = 2*rtod*asin(wave/(2*resval))
!   
   end function tthvalue

!----------------------------------------------------------------------------------------------------

   real function resvalue(tthval,wave)
!   
!  Calcola la risoluzione dal 2theta
!
   USE trig_constants
   real, intent(in) :: tthval,wave
!   
   resvalue = wave / (2*sin(dtor*tthval/2))
!  
   end function resvalue

!----------------------------------------------------------------------------------------------------

   function sample_displacement(tt,dis,geom) result(corr)
!
!  2theta = -2s*cos(theta)*(180/pi)/R = dis*cos(theta)*(180/pi)  [in degree]
!
   USE trig_constants
   real, dimension(:), intent(inout) :: tt   ! 2theta in degree
   real, intent(in)                  :: dis  ! refined parameter = -2s/R
   integer, intent(in)               :: geom ! geometry
   real, dimension(size(tt))         :: corr
!
   select case (geom)
     case (BB_GEOMETRY)
       corr = dis * cos(dtor*tt/2) * rtod

     case (DS_GEOMETRY)    !!!TODO
   end select
!
   end function sample_displacement

!----------------------------------------------------------------------------------------------------

   function sample_transparency(tt,trasp,geom) result(corr)
!
!  2theta = (1/2*mu*R)*sin(2theta)*(180/pi) = trasp*sin(2theta)*(180/pi)  [in degree]
!
   USE trig_constants
   real, dimension(:), intent(inout) :: tt    ! 2theta in degree
   real, intent(in)                  :: trasp ! refined parameter = 1/2*mu*R
   integer, intent(in)               :: geom  ! geometry
   real, dimension(size(tt))         :: corr
!
   select case (geom)
     case (BB_GEOMETRY)
       corr = trasp * sin(dtor*tt) * rtod

     case (DS_GEOMETRY)    !!!TODO
   end select
!
   end function sample_transparency

!----------------------------------------------------------------------------------------------------

   subroutine smooth_calculate(datas)
   use arrayutil
   use ssmoothing
   class(dataset_type), intent(inout) :: datas

   call resize_array(datas%smoothvec,datas%npoints()) 
   datas%smoothvec = smooth_calc(datas%y,datas%scond)
   end subroutine smooth_calculate
   
!-----------------------------------------------------------------

   subroutine smooth_apply(datas)
   use arrayutil
   class(dataset_type), intent(inout) :: datas
   if (allocated(datas%smoothvec)) then
       datas%y = datas%smoothvec
       call delete_array(datas%smoothvec)
   endif
   end subroutine smooth_apply

!-----------------------------------------------------------------

   real function get_wave1(datas)
   use prog_constants
   type(dataset_type), dimension(:), allocatable, intent(in) :: datas
!
   if (ndataset(datas) > 0) then
       get_wave1 = datas(1)%wave(1)
   else
       get_wave1 = DEF_WAVE
   endif
!
   end function get_wave1

!-----------------------------------------------------------------

   function get_wave(datas)
   use prog_constants
   type(dataset_type), dimension(:), allocatable, intent(in) :: datas
   real, dimension(:), allocatable :: get_wave
!
   if (ndataset(datas) > 0) then
       allocate(get_wave(datas(1)%nwave),source=datas(1)%wave)
   else
       allocate(get_wave(2),source=[DEF_WAVE,DEF_WAVE2])
   endif
!
   end function get_wave

!-----------------------------------------------------------------

   integer function get_nwave(datas)
   type(dataset_type), dimension(:), allocatable, intent(in) :: datas
!
   if (ndataset(datas) > 0) then
       get_nwave = datas(1)%nwave
   else
       get_nwave = 1
   endif
!
   end function get_nwave

!-----------------------------------------------------------------

   integer function get_radtype(datas)
   use elements
   type(dataset_type), dimension(:), allocatable, intent(in) :: datas
!
   if (ndataset(datas) > 0) then
       get_radtype = datas(1)%radtype
   else
       get_radtype = RX_SOURCE
   endif
!
   end function get_radtype

END MODULE datasetmod
