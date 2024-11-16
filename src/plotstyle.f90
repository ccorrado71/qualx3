MODULE plotstyle

  implicit none

  type style_type
       integer         :: plot_type  ! linee, punti, linee+punti
       integer         :: colline    ! colore delle linee
       integer         :: colmarker  ! colore dei marker
       integer         :: linetype   ! tipo della linea
       integer         :: linewidth  ! tipo della linea
       integer         :: markertype ! tipo di marker
       integer         :: sizemarker ! dimensione del marker
       integer         :: font_size  ! dimensione del carattere
       integer         :: vis = 0    ! visibile? 0 non visibile, 1 visibile, -1 disabilitato
  end type style_type
!
  type style_class
       type(style_type), dimension(:), allocatable, private :: style
       integer                                              :: np = 0
  contains
       procedure :: load => load_style
       procedure :: get  => get_style
  end type style_class
!
  type(style_class) :: stylecryst
!corr  type(style_class) :: styledata
!
! plot_type puo' assumere i seguenti valori
  integer, parameter   :: LINEPLOT = 1      ! solo linee
  integer, parameter   :: SCATTERPLOT = 2   ! solo punti
  integer, parameter   :: LINESCATTER = 3   ! linee+punti
  integer, parameter   :: BARPLOT = 4       ! bar-plot

  integer, parameter   :: NITEMS = 13

  type(style_type), dimension(NITEMS) :: style 

  integer, parameter :: STYLE_BACK = 1
  integer, parameter :: STYLE_CALC = 2
  integer, parameter :: STYLE_DIFF = 3
  integer, parameter :: STYLE_CUMUL = 4
  integer, parameter :: STYLE_PEAKS = 5
  integer, parameter :: STYLE_AXIS = 6
  integer, parameter :: STYLE_BACKP = 7
  integer, parameter :: STYLE_INTERVALS = 8
  integer, parameter :: STYLE_BG = 9
  integer, parameter :: STYLE_LEGEND = 10
  integer, parameter :: STYLE_UND_PEAKS = 11
  integer, parameter :: STYLE_SMOOTH = 12
!corr  integer, parameter :: STYLE_ABSENCES = 13

  contains

   integer function numstyle(style)
   type(style_type), dimension(:), allocatable, intent(in) :: style
!   
   if (allocated(style)) then
       numstyle = size(style)
   else
       numstyle = 0
   endif
!
   end function numstyle

!----------------------------------------------------------------------

   subroutine new_style(vetr,n)
!
!  Create new atoms
!
   type(style_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                          :: n

   if (n < 0) return
   if (numstyle(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_style

!corr!----------------------------------------------------------------------
!corr
!corr   subroutine load_style_crys(sty)
!corr   type(style_type), dimension(:), intent(in) :: sty
!corr   np_style_crys = 0
!corr   allocate(stylecrys(size(sty)),source=sty)
!corr   end subroutine load_style_crys
!corr
!corr!----------------------------------------------------------------------
!corr  
!corr   function style_crys()
!corr   type(style_type) :: style_crys
!corr   integer          :: npp
!corr!
!corr   if (numstyle(stylecrys) == 0) then
!corr       style_crys =  style_type(1,7,0,1,1,1,3,8,1)
!corr   else
!corr!
!corr!      cyclic assignment of style
!corr       np_style_crys = np_style_crys + 1
!corr       npp = mod(np_style_crys,numstyle(stylecrys))
!corr       if (npp == 0) npp = numstyle(stylecrys)
!corr       style_crys = stylecrys(npp)
!corr   endif
!corr!
!corr   end function style_crys
!corr
!corr!----------------------------------------------------------------------

   subroutine load_style(stc,sty)
   class(style_class), intent(inout)          :: stc
   type(style_type), dimension(:), intent(in) :: sty
   stc%np = 0
   allocate(stc%style(size(sty)),source=sty)
   end subroutine load_style

!----------------------------------------------------------------------
  
   function get_style(stc,ns)
   class(style_class), intent(inout) :: stc
   integer, optional, intent(in)     :: ns
   type(style_type)                  :: get_style
   integer                           :: npp,nss
!
   if (numstyle(stc%style) == 0) then
       get_style =  style_type(1,7,0,1,1,1,3,8,1)
   else
!
!      cyclic assignment of style
       if (present(ns)) then
           nss = ns
       else
           stc%np = stc%np + 1
           nss = stc%np
       endif
       npp = mod(nss,numstyle(stc%style))
       if (npp == 0) npp = numstyle(stc%style)
       get_style = stc%style(npp)
   endif
!
   end function get_style

END MODULE plotstyle

