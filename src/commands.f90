MODULE commandsmod

   implicit none 

   character(len=1)                  :: beg = '%'      ! controllare se usato
   character(len=1)                  :: commen = '>'   ! controllare se usato
   integer, parameter                :: NUM_COMMANDS = 48
   character(len=*), parameter, dimension(NUM_COMMANDS)  :: allcom = (/      &
     '              ','              ','              ', &  !  3
     'data          ','extraction    ','normal        ', &  !  6
     'seminvariants ','invariants    ','phase         ', &  !  9
     'fourier       ','menu          ','end           ', &  !  12
     'continue      ','              ','sdirect       ', &  !  15
     'polyhedra     ','recycle       ','              ', &  !  18
     'export        ','zmatrix       ','automatic     ', &  !  21
     'patterson     ','ntreor        ','rietveld      ', &  !  24
     'piero         ','crystal       ','sannealing    ', &  !  27
     'structure     ','window        ','nowindow      ', &  !  30
     'initialize    ','job           ','fragment      ', &  !  33
     'save          ','load          ','alltrials     ', &  !  36
     'random        ','refine        ','sannel        ', &  !  39
     'dicvol        ','mcmaille      ','calculate_xpd ', &  !  42
     'sflip         ','oldbuild      ','pdf           ', &  !  45
     'lebail        ','pawley        ','              '/)   !  48

integer, parameter :: DATA_CMD=4  
integer, parameter :: EXTRA_CMD=5, NORM_CMD=6, INVAR_CMD=8, PHASE_CMD=9, FOUR_CMD=10, MENU_CMD=11, END_CMD=12
integer, parameter :: CONTINUE_CMD=13, POLY_CMD=16, CLAB_CMD=18, EXPORT_CMD=19, ZMAT_CMD=20 
integer, parameter :: AUTO_CMD=21, PATTERSON_CMD=22, NTREOR_CMD=23, RIET_CMD=24, CRYSTAL_CMD=26, SA_CMD=27
integer, parameter :: STRUCT_CMD=28, WINDOW_CMD=29, NOWINDOW_CMD=30, INIT_CMD=31, JOB_CMD=32
integer, parameter :: FRAG_CMD=33, SAVE_PRJ=34, LOAD_PRJ=35, ALLTRIALS_CMD=36, REFINE_CMD=38, SA2_CMD=39
integer, parameter :: DICVOL_CMD=40, MCMAILLE_CMD=41, CALCXPD_CMD=42,CFLIP_CMD=43,OLDBUILD_CMD=44,PDF_CMD=45
integer, parameter :: LEBAIL_CMD=46, PAWLEY_CMD=47


integer, parameter, private :: DELETED_COMMAND = -999
!
   integer, parameter :: NLENDIR = 780
!corr   integer, parameter :: NMAXDIR = 20
   type command_type
        integer                                    :: code = 0        ! codice comando
        !character(len=NLENDIR), dimension(NMAXDIR) :: strdir = ' '    ! stringa con direttiva
        character(len=NLENDIR), dimension(:), allocatable :: strdir    ! stringa con direttiva
        integer                                    :: ndir = 0        ! num. di direttive
        logical                                    :: comb = .false.  ! first directive is combined with command
                                                                      ! e.g.: %crystal nome_file.cif
   contains
        procedure          :: get_name
        procedure          :: set_as_deleted
        procedure, private :: set_command_str
        procedure, private :: set_command_ip
        procedure, private :: set_command_dir
        procedure, private :: set_command_from_id
        generic, public    :: set => set_command_str, set_command_ip, set_command_dir, set_command_from_id
        procedure, private :: add_dir_v
        procedure, private :: add_dir_s
        generic, public    :: add => add_dir_v, add_dir_s
        procedure          :: getdir 
        procedure          :: getpar
        procedure          :: prn

   end type command_type

   type(command_type), dimension(:), allocatable :: commands

private command_position_str, command_position_code, command_position_vet
interface command_position
   module procedure command_position_str, command_position_code, command_position_vet
end interface

private next_command_is_s, next_command_is_c
interface next_command_is
   module procedure next_command_is_s, next_command_is_c
end interface

private replace_command_id, replace_command_pos
interface replace_command
   module procedure replace_command_pos, replace_command_id     
end interface 

private command_is_str, command_is_code
interface command_is
   module procedure command_is_str, command_is_code
end interface

!F ncommands(commd)                               Number of commands
!F command_position(commd,code,back,pos)          Locate position of command
!F get_name(cmd)                                  Get the name of command from array allcom
!F command_is_before(cmd,code,pos)                Check if command code is before command in position pos
!F command_is_after(cmd,code,pos)                 Check if command code is after command in position pos
!F function next_command_code(commd, icpos)       Get the id of the  next command. Return 0 if no more commands
!F is_directive(cmd,dirstring)                    Find firt occurence of directive dirstring in the command cmd
!S is_directive_all(cmd,dirstring,vet,n)          Check the exisistence of directive dirstring in the command cmd, all occurences
!S delete_commands(cmd,pos)                       Delete commands
!S delete_commands_code(cmd,code)                 Delete commands from code
!F get_ipdir(commd,icom)  result(ipd)             Pointer to first directory of the command icom
!S replace_command_pos(cmd,...)                   Replace command 
!S get_additional_dir(dir,subdir,valued,pos,lens) Find subdir of type: subdir(valued)
!S set_priority(cmd,strp)                         Set priority of directives according to array strp

CONTAINS
   
   subroutine create_commands(cmd,ipcom,icomqq,cardq)
   type(command_type), dimension(:), allocatable, intent(inout) :: cmd
   integer, intent(in)                                          :: ipcom
   integer, dimension(:,:), intent(in)                          :: icomqq
   character(len=*), dimension(:)                               :: cardq
   integer                                                      :: i    !!!old,j
!old   integer                                                      :: icd
   integer                                                      :: icd1,icd2
!
!corr   if (allocated(cmd)) deallocate(cmd)
!corr   allocate(cmd(ipcom))
!old   icd = 0
!old   do i=1,ipcom
!old      commd(i)%code = icomqq(i,1)
!old!corr      cmd(i)%ndir =icomqq(i,2)
!old      cmd(i)%ndir =min(icomqq(i,2),NMAXDIR)
!old      if (cmd(i)%ndir /= 0) then
!old!corr          allocate(cmd(i)%strdir(cmd(i)%ndir))
!old          do j=1,cmd(i)%ndir
!old             icd = icd + 1
!old             cmd(i)%strdir(j) = cardq(icd)
!old          enddo
!old      endif
!old   enddo
!!!!!FIXME 
   call new_cmd(cmd,ipcom)
   icd2 = 0
   do i=1,ipcom
      if (icomqq(i,2) > 0) then
          icd1 = icd2 + 1
          icd2 = icd2 + icomqq(i,2)
          call cmd(i)%set(allcom(icomqq(i,1)), cardq(icd1:icd2))
      else
              !write(0,*)'CR=',allcom(icomqq(i,1))
          call cmd(i)%set(allcom(icomqq(i,1)))
      endif
   enddo
!
   end subroutine create_commands

!-----------------------------------------------------------------------------------------

   subroutine create_expo_commands(commd, ipcom, icomq, card)
   type(command_type), dimension(:), intent(in) :: commd
   integer, intent(out) :: ipcom
   integer, dimension(:,:) :: icomq
   character(len=*), dimension(:) :: card
   integer :: ncd
   integer :: i,j
!   
   ipcom = size(commd)
   ncd = 0
   do i=1, ipcom
      icomq(i,1) = commd(i)%code
      icomq(i,2) = commd(i)%ndir
      do j=1,commd(i)%ndir
         ncd = ncd + 1
         card(ncd) = trim(commd(i)%strdir(j))
      enddo
   enddo
!
   end subroutine create_expo_commands

!-----------------------------------------------------------------------------------------

   integer function ncommands(commd)
!
!  Number of commands
!
   type(command_type), dimension(:), allocatable, intent(in) :: commd
!
   if (allocated(commd)) then
       ncommands = size(commd)
   else
       ncommands = 0
   endif
!
   end function ncommands

!-----------------------------------------------------------------------------------------

   function get_name(cmd)  result(str)
!
!  Get the name of command from array allcom
!
   class(command_type), intent(in) :: cmd
   character(len=:), allocatable   :: str
!
   if (cmd%code < 1 .and. cmd%code > size(allcom)) then
       str = ' '
   else
       str = trim(allcom(cmd%code))
   endif
!
   end function get_name

!-----------------------------------------------------------------------------------------

   integer function command_is_before(cmd,code,pos) result(cpos)
!
!  Check if command code is before command in position pos
!
   type(command_type), dimension(:), allocatable, intent(in) :: cmd  ! list of command
   integer, intent(in)                                       :: code ! code of command to search
   integer, intent(in)                                       :: pos  ! position of command in cmd list
   integer :: i,newpos
!
   cpos = 0
   newpos = min(pos,ncommands(cmd))
   if (newpos <= 1) return
!
   do i=newpos-1,1,-1
      if (cmd(i)%code == code) then
          cpos = i
          return
      endif
   enddo
!
   end function command_is_before

!-----------------------------------------------------------------------------------------

   integer function command_is_after(cmd,code,pos) result(cpos)
!
!  Check if command code is after command in position pos
!
   type(command_type), dimension(:), allocatable, intent(in) :: cmd  ! list of command
   integer, intent(in)                                       :: code ! code of command to search
   integer, intent(in)                                       :: pos  ! position of command in cmd list
   integer :: i
!
   cpos = 0
   if (pos < 0) return
!
   do i=pos+1,ncommands(cmd)
      if (cmd(i)%code == END_CMD) return
      if (cmd(i)%code == code) then
          cpos = i
          return
      endif
   enddo
!
   end function command_is_after

!-----------------------------------------------------------------------------------------

   subroutine set_as_deleted(cmd)
!
!  Get the name of command from array allcom
!
   class(command_type), intent(inout) :: cmd
!
   cmd%code = DELETED_COMMAND
!
   end subroutine set_as_deleted
  
!-----------------------------------------------------------------------------------------

   integer function command_position_str(commd,stringcomm,back,startc,endc)  result(nposc)
!
!  Cerca un comando nella lista e ne restituisce la posizione
!  il risultato è 0 se non esiste
!
   USE strutil
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   character(len=*), intent(in)                              :: stringcomm
   logical, intent(in), optional                             :: back
   integer, intent(in), optional                             :: startc,endc
   integer                                                   :: ini,fin
!
   nposc = 0
   if (ncommands(commd) == 0) return
!
   if (present(startc)) then
       if (startc > ubound(commd,dim=1)) return
       ini = startc
   else
       ini = lbound(commd,dim=1)
   endif
   if (present(endc)) then
       if (endc < lbound(commd,dim=1) .or. endc > ubound(commd,dim=1)) return
       fin = endc
   else
       fin = ubound(commd,dim=1)
   endif
   if (present(back)) then
       nposc = string_locate(stringcomm,allcom(commd(ini:fin)%code),back=back)
   else
       nposc = string_locate(stringcomm,allcom(commd(ini:fin)%code))
   endif
   if (nposc > 0) then
       nposc = nposc + ini - 1
   endif
!
   end function command_position_str

!-----------------------------------------------------------------------------------------

   integer function command_position_code(commd,code,back,startc,endc)  result(nposc)
!
!  Locate position of command
!
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer, intent(in)                                       :: code
   logical, intent(in), optional                             :: back
   integer, intent(in), optional                             :: startc,endc
   integer                                                   :: i,ini,fin
!
   nposc = 0
   if (ncommands(commd) == 0) return
!
   if (present(startc)) then
       if (startc > ubound(commd,dim=1)) return
       ini = startc
   else
       ini = lbound(commd,dim=1)
   endif
   if (present(endc)) then
       if (endc < lbound(commd,dim=1) .or. endc > ubound(commd,dim=1)) return
       fin = endc
   else
       fin = ubound(commd,dim=1)
   endif
   if (present(back)) then
       do i=fin,ini,-1
          if (commands(i)%code == code) then
              nposc = i
              exit
          endif
       enddo
   else
       do i=ini,fin
          if (commands(i)%code == code) then
              nposc = i
              exit
          endif
       enddo
   endif
!
   end function command_position_code 

!-----------------------------------------------------------------------------------------

   integer function command_position_vet(commd,vet,ivpos,startc,endc)  result(nposc)
   USE strutil
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer, dimension(:), intent(in)                         :: vet
   integer, optional                                         :: ivpos
   integer, intent(in), optional                             :: startc,endc
   integer                                                   :: i
!
   nposc = 0
   if (present(ivpos)) ivpos = 0
   if (present(startc) .and. present(endc)) then
       do i=size(vet),1,-1
          nposc = command_position_code(commd,vet(i),startc=startc,endc=endc)
          if (nposc /= 0) then
              nposc = nposc + startc - 1
              if (present(ivpos)) ivpos = i
              return
          endif
       enddo
   else
       do i=size(vet),1,-1
          nposc = command_position_code(commd,vet(i))
          if (nposc /= 0) then
              if (present(ivpos)) ivpos = i
              return
          endif
       enddo
   endif
!
   end function command_position_vet 

!-----------------------------------------------------------------------------------------

   integer function is_directive(cmd,dirstring)
!
!  Check the exisistence of directive dirstring in the command cmd, only the first occurence
!
   USE strutil
   type(command_type), intent(in)                    :: cmd
   character(len=*), intent(in)                      :: dirstring
   character(len=NLENDIR), dimension(:), allocatable :: commdir
   character(len=NLENDIR)                            :: strd
   integer                                           :: i
!
   if (cmd%ndir > 0) then
!
!      Isola la prima stringa per ogni direttiva
       allocate(commdir(cmd%ndir))
       do i=1,cmd%ndir
          strd = cmd%strdir(i)
          call cutst(strd,line2=commdir(i))
       enddo
!
       is_directive = string_locate(dirstring,commdir,exact=.false.)
   else
       is_directive = 0
   endif
!
   end function is_directive

!-----------------------------------------------------------------------------------------

   subroutine is_directive_all(cmd,dirstring,vet,n)
!
!  Check the exisistence of directive dirstring in the command cmd, all occurences
!
   USE strutil
   type(command_type), intent(in)                    :: cmd
   character(len=*), intent(in)                      :: dirstring
   integer, dimension(:), intent(out)                :: vet  ! must be allocated to cmd%ndir size
   integer, intent(out)                              :: n
   character(len=NLENDIR), dimension(:), allocatable :: commdir
   character(len=NLENDIR)                            :: strd
   integer                                           :: i
!
   n = 0
   if (cmd%ndir > 0) then
!
!      Isola la prima stringa per ogni direttiva
       allocate(commdir(cmd%ndir))
       do i=1,cmd%ndir
          strd = cmd%strdir(i)
          call cutst(strd,line2=commdir(i))
       enddo
!
       call string_locate_all(dirstring, commdir(:), vet, n)
   endif
!
   end subroutine is_directive_all

!-----------------------------------------------------------------------------------------

   function get_directive(commd,dirstring)  result(isdir)
!
!  Estrai una direttiva
!
   type(command_type), dimension(:), intent(in) :: commd
   character(len=*), intent(in)                 :: dirstring
   integer                                      :: i
   integer, dimension(2)                        :: isdir
   integer                                      :: kdir
!
   isdir(:) = 0
   do i=1,size(commd)  
      kdir = is_directive(commd(i),dirstring)
      if (kdir > 0) then
          isdir(1) = i
          isdir(2) = kdir
          exit
      endif
   enddo
!
   end function get_directive

!-----------------------------------------------------------------------------------------

   function get_string_command(commd,comstr,ipc)  result(str)
!
!  Extract directive associated to command comstr
!
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   character(len=*), intent(in)                              :: comstr
   integer, optional, intent(out)                            :: ipc
   character(len=:), allocatable                             :: str
   integer                                                   :: ipcmd
!
   ipcmd = command_position(commd,comstr)
   if (present(ipc)) ipc = ipcmd
   if (ipcmd > 0) then
       if (commd(ipcmd)%ndir > 0) then
           str = trim(commd(ipcmd)%strdir(1))
           return
       endif
   endif
!
   str = ' '
!
   end function get_string_command 

!-----------------------------------------------------------------------------------------

   function get_string_directive(commd,dirstring) result(str)
!
!  Estrai la stringa associata ad una direttiva
!
   USE strutil
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   character(len=*), intent(in)                              :: dirstring
   character(len=:), allocatable                             :: str
   integer, dimension(2)                                     :: kdir
!
   str = ' '
   if (ncommands(commd) == 0) return
!
   kdir(:) = get_directive(commd,dirstring)
   if (kdir(1) > 0) then
       str = trim(commd(kdir(1))%strdir(kdir(2)))
       call cutst(str)
   endif
!
   end function get_string_directive

!-----------------------------------------------------------------------------------------

   logical function is_directive_cmd(commd, dirstring)
!
!  Esiste la direttiva dirstring?
!
   type(command_type), dimension(:), intent(in) :: commd
   character(len=*), intent(in)                 :: dirstring
   integer, dimension(2)                        :: isdir
!
   isdir(:) = get_directive(commd,dirstring)
   is_directive_cmd = isdir(1) > 0
!
   end function is_directive_cmd

!-----------------------------------------------------------------------------------------

    logical function is_directive_in_command(commd,strcomm,strdir)   result(isdir)
!
!   Controlla se esiste la direttiva strdir nel comando strcomm
!
    type(command_type), dimension(:), allocatable, intent(in) :: commd
    character(len=*), intent(in)                              :: strcomm,strdir
    integer                                                   :: nposc
    integer, dimension(2)                                     :: kdir
!
!   Locazione del comando strcomm
    nposc = command_position(commd,strcomm)
!
    if (nposc > 0) then
        kdir(:) = get_directive(commd(nposc:nposc),strdir) 
        isdir = kdir(1) > 0
    else
        isdir = .false.
    endif
!
    end function is_directive_in_command

!-----------------------------------------------------------------------------------------

   subroutine prn(cmd,kpr)
   class(command_type), intent(in) :: cmd
   integer, intent(in)             :: kpr
   integer                         :: j
!
   if (cmd%ndir == 0) then
       write(kpr,'(11x,a,1x,a)')'%'//allcom(cmd%code),'default'
   else
       write(kpr,'(11x,a,1x,a)')'%'//allcom(cmd%code),trim(cmd%strdir(1))
       do j=2,cmd%ndir
          write(kpr,'(27x,a)')trim(cmd%strdir(j))
       enddo
   endif
!
   end subroutine prn

!-----------------------------------------------------------------------------------------

   subroutine print_commands(cmd,kpr)
!
!  Print all commands
!
   type(command_type), dimension(:), allocatable, intent(in) :: cmd
   integer, intent(in)                                       :: kpr
   integer                                                   :: i
!
   if (ncommands(cmd) == 0) return
   write(kpr,'(a/)')' Used  commands & directives:'
   do i=1,ncommands(cmd)
      call cmd(i)%prn(kpr)
      write(kpr,*)
   enddo
!
   end subroutine print_commands

!-----------------------------------------------------------------------------------------

   logical function is_content_required(commd) result(lreq)
!
!  Controllo se il contenuto e' necessario
!
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer                                                   :: i
   integer, dimension(6), parameter :: contreq = [EXTRA_CMD,NORM_CMD,INVAR_CMD,PATTERSON_CMD,ALLTRIALS_CMD,CONTINUE_CMD]
!
   lreq = .false.
!
   do i=1,ncommands(commd)
      if (commd(i)%code == DATA_CMD) then   ! comando 'data'
!
!        Se esiste la direttiva findspace il contenuto e' richiesto
         lreq = is_directive(commd(i),'findspace') > 0
      else
         lreq = any(contreq == commd(i)%code)
      endif
      if (lreq) return
   enddo
!
   end function is_content_required

!-----------------------------------------------------------------------------------------

   logical function is_symm_required(commd) result(lreq)
!
!  Check if symmetry info are
!
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer                                                   :: i
!
   lreq = .false.
!
   do i=1,ncommands(commd)
      if (commd(i)%code == NORM_CMD .or. commd(i)%code == RIET_CMD) then
          lreq = .true.
          exit
      endif
      if (commd(i)%code == CRYSTAL_CMD) exit
   enddo
!
   end function is_symm_required

!-----------------------------------------------------------------------------------------

   subroutine set_command_str(commd, strcomm, dir)
   USE strutil
   class(command_type), intent(out)         :: commd
   character(len=*), intent(in)             :: strcomm
   character(len=*), dimension(:), optional :: dir
   integer                                  :: poscom
!
   poscom = string_locate(strcomm,allcom)
   if (poscom /= 0) then
       if (present(dir)) then
           call set_command_ip(commd,poscom,dir)
       else
           call set_command_from_id(commd,poscom)
       endif
   endif
!
   end subroutine set_command_str

 !-----------------------------------------------------------------------------------------

   subroutine set_command_from_id(commd,id)
!
!  Set command from position (id) in array allcom
!
   USE strutil
   class(command_type), intent(inout) :: commd
   integer, intent(in)                :: id
!
   if (id > 0 .and. id <= size(allcom)) then
       commd%code = id 
       commd%ndir = 0
   endif
!
   end subroutine set_command_from_id

 !-----------------------------------------------------------------------------------------

   subroutine set_command_ip(commd, id, dir)
!
!  Set command from position (id) in array allcom, set directives
!
   class(command_type), intent(inout) :: commd
   character(len=*), dimension(:)     :: dir
   integer, intent(in)                :: id
!
   if (id > 0 .and. id <= size(allcom)) then
       commd%code = id 
       commd%ndir = 0
   endif
   call commd%set_command_dir(dir)
!
   end subroutine set_command_ip
  
!-----------------------------------------------------------------------------------------

   subroutine set_command_dir(commd, dir)
!
!  Set directives for command
!
   USE strutil
   class(command_type), intent(inout) :: commd
   character(len=*), dimension(:)     :: dir
!
   commd%ndir = 0
   call commd%add(dir)
!
   end subroutine set_command_dir

!-----------------------------------------------------------------------------------------

   subroutine add_dir_v(commd, dir)
!
!  Set directives for command
!
   USE strutil
   class(command_type), intent(inout) :: commd
   character(len=*), dimension(:)     :: dir
   integer                            :: i
!
   do i=1,size(dir)
      call commd%add_dir_s(dir(i))
!corr      if (len_trim(dir(i)) > 0) then
!corr          if (commd%ndir + 1 > NMAXDIR) exit
!corr          commd%ndir = commd%ndir + 1
!corr          commd%strdir(i) = dir(i)
!corr      endif
   enddo
!
   end subroutine add_dir_v

!-----------------------------------------------------------------------------------------

   subroutine add_dir_s(commd, dir)
!
!  Set directives for command
!
   USE strutil
   USE arrayutil
   class(command_type), intent(inout) :: commd
   character(len=*)                   :: dir
   integer, parameter                 :: NDIRADD = 20
!
   if (len_trim(dir) == 0) return
   !if (commd%ndir + 1 > NMAXDIR) return
   if (commd%ndir + 1 > size_array(commd%strdir)) then
       call resize_array(commd%strdir,size_array(commd%strdir)+NDIRADD,NLENDIR)
   endif
   commd%ndir = commd%ndir + 1
   commd%strdir(commd%ndir) = dir
!
   end subroutine add_dir_s

!-----------------------------------------------------------------------------------------

   subroutine set_command_for(strcomm,ipcom,icomqq)
   USE strutil
   character(len=*), intent(in)           :: strcomm
   integer, intent(inout)                 :: ipcom
   integer, dimension(:,:), intent(inout) :: icomqq
   integer                                :: i, lens
!
   lens = len_trim(strcomm)
   do i=1,size(allcom)
      if (lower(strcomm) == allcom(i)(1:lens)) then
          ipcom = ipcom + 1
          icomqq(ipcom,1) = i
          icomqq(ipcom,2) = 0
      endif
   enddo
!
   end subroutine set_command_for

!-----------------------------------------------------------------------------------------

   function getdir(commd,kdir) result(dir)
   class(command_type), intent(in) :: commd
   integer, intent(in), optional   :: kdir
   character(len=:), allocatable   :: dir
   integer                         :: kdirective
   if (present(kdir)) then
       kdirective = kdir
   else
       kdirective = 1
   endif
   if (kdirective <= commd%ndir .and. kdirective > 0) then
       dir = trim(commd%strdir(kdirective))
   else
       dir = ' '
   endif
   end function getdir

!-----------------------------------------------------------------------------------------

   function getpar(commd,kdir) result(par)
   use strutil
   class(command_type), intent(in) :: commd
   integer, intent(in)             :: kdir
   character(len=:), allocatable   :: par
!
   par = commd%getdir(kdir)
   if (len_trim(par) > 0) then
       call cutsta(par)
   endif
!
   end function getpar

!-----------------------------------------------------------------------------------------

   logical function command_is_str(commd,icpos,scomm)  result(is_commd)
!
!  Check if the command icpos is scomm
!
   USE strutil
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer, intent(in)                                       :: icpos  ! current command position
   character(len=*), intent(in)                              :: scomm  ! command name
!
   is_commd = .false.
   if (ncommands(commd) < icpos) return
   is_commd = s_eqi(allcom(commd(icpos)%code),scomm)
!
   end function command_is_str

!-----------------------------------------------------------------------------------------

   logical function command_is_code(commd,icpos,code)  result(is_commd)
!
!  Check if the command icpos is scomm
!
   USE strutil
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer, intent(in)                                       :: icpos ! current command position
   integer, intent(in)                                       :: code  ! command code
!
   is_commd = .false.
   if (ncommands(commd) < icpos) return
   is_commd = commands(icpos)%code == code
!
   end function command_is_code

!-----------------------------------------------------------------------------------------

   integer function next_command_code(commd, icpos)
!
!  Get the id of the  next command. Return 0 if no more commands
!
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer, intent(in)                                       :: icpos  ! current command position
!
   if (ncommands(commd) <= icpos) then
       next_command_code = 0
   else
       next_command_code = commd(icpos+1)%code
   endif
!
   end function next_command_code

!-----------------------------------------------------------------------------------------

   logical function next_command_is_s(commd,icpos,scomm)   result(is_commd)
!
!  Check if the next command icpos+1 is scomm
!
   USE strutil
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer, intent(in)                                       :: icpos  ! current command position
   character(len=*), intent(in)                              :: scomm  ! command name
   integer                                                   :: ic_next
!
   ic_next = icpos + 1  ! next command position
   is_commd = command_is(commd,ic_next,scomm)
!
   end function next_command_is_s

 !-----------------------------------------------------------------------------------------

   logical function next_command_is_c(commd,icpos,code)   result(is_commd)
!
!  Check if the next command icpos+1 is scomm
!
   USE strutil
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer, intent(in)                                       :: icpos ! current command position
   integer, intent(in)                                       :: code  ! command code
!
!corr    is_commd = .false.
!corr    if (ncommands(commd) <= icpos) return
!corr    is_commd = commd(icpos+1)%code == code
   is_commd = next_command_code(commd,icpos) == code
!
   end function next_command_is_c
 
!-----------------------------------------------------------------------------------------

   subroutine resize_cmd(vetr,n,savevet)
!
!  Rialloca ad n un array di tipo command_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(command_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                            :: n
   logical, optional, intent(in)                  :: savevet
   logical                                        :: savev
   integer                                        :: nv
   type(command_type), allocatable                :: vsav(:)
   integer                                        :: nsav
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
           deallocate(vetr)
           allocate(vetr(n))
       endif
   endif
!
   end subroutine resize_cmd

!-----------------------------------------------------------------------------------------

   subroutine new_cmd(vetr,n)
!
!  Create new atoms
!
   type(command_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                            :: n

   if (n < 0) return
   if (ncommands(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_cmd

!-----------------------------------------------------------------------------------------

   subroutine delete_cmd(vetr)
!
!  Delete all atoms
!
   type(command_type), allocatable, intent(inout) :: vetr(:)

   if (allocated(vetr)) deallocate(vetr)

   end subroutine delete_cmd

!----------------------------------------------------------------------------------------------------
!old  
!old   subroutine add_directive(comm,sdir)
!old!
!old!  Add diective sdir(:) to the command comm
!old!
!old   type(command_type), intent(inout)          :: comm
!old   character(len=*), dimension(:), intent(in) :: sdir
!old   integer                                    :: i
!old!   
!old   do i=1,size(sdir)
!old      if (is_directive(comm,sdir(i)) == 0)  then
!old          comm%ndir = comm%ndir + 1
!old          comm%strdir(comm%ndir) = sdir(i)
!old          if (comm%ndir > NMAXDIR) exit
!old      endif
!old   enddo
!old!
!old   end subroutine add_directive
!old
!----------------------------------------------------------------------------------------------------

   subroutine add_continue(comm,ipstart,nwave)
!
!  Add command for '%continue' starting from ipstart+1
!
   type(command_type), allocatable, intent(inout) :: comm(:)
   integer, intent(in)                            :: ipstart
   integer, intent(in)                            :: nwave
   integer, dimension(7)                          :: vetc
   integer                                        :: inic,finc,icomm,kadd,i
!
   if (nwave == 1) then
       vetc(1) = EXTRA_CMD
   else
       vetc(1) = LEBAIL_CMD
   endif
   vetc(2:) = [NORM_CMD,INVAR_CMD,PHASE_CMD,FOUR_CMD,MENU_CMD,END_CMD]
!
!  Check if ipstart is one of the command in the array vetc
   icomm = 0
   if (ipstart > 0) then
       do i=1,size(vetc)  
          if (comm(ipstart)%code == vetc(i)) then
              icomm = i
              exit
          endif
       enddo
   endif
!
   kadd = size(vetc) - icomm ! commands to add
   call resize_cmd(comm,ipstart+kadd)
!
   inic = ipstart+1
   finc = inic 
   do i=1,kadd
      call comm(ipstart+i)%set(vetc(icomm+i))
   enddo
!
   end subroutine add_continue

!----------------------------------------------------------------------------------------------------

   subroutine replace_command_pos(cmd,pos,vcmd)
!
!  Replace command pos with commands cmdi
!
   type(command_type), dimension(:), allocatable, intent(inout) :: cmd
   integer, intent(in)                                          :: pos
   integer, dimension(:), intent(in)                            :: vcmd
   integer :: i, ncmd, ntotc, nic
!
   nic = ncommands(cmd)
   if (pos == 0 .or. pos > ncommands(cmd)) return
   ncmd = size(vcmd)
   if (ncmd == 0) return
!
   ntotc = ncommands(cmd)+ncmd - 1
   call resize_cmd(cmd,ntotc)
   call cmd(pos)%set(vcmd(1))
!
   do i=pos+1,nic
      cmd(i+ncmd-1) = cmd(i)
   enddo
!
   do i=2,ncmd
      call cmd(pos+i-1)%set(vcmd(i))
   enddo
!
   end subroutine replace_command_pos

!----------------------------------------------------------------------------------------------------

   subroutine replace_command_id(cmd,id1,id2,dir,last)
   type(command_type), dimension(:), allocatable, intent(inout) :: cmd
   integer, intent(in)                                          :: id1,id2
   character(len=*), dimension(:), intent(in), optional         :: dir
   logical, intent(in), optional                                :: last
   integer                                                      :: pos
!   
   pos = command_position(cmd,id1,last)
   if (pos > 0) then
       call cmd(pos)%set(id2)
       if (present(dir)) then
           call cmd(pos)%set(dir)
       endif
   endif
!
   end subroutine replace_command_id

!----------------------------------------------------------------------------------------------------

   subroutine add_command(cmd,pos,vcmd)
!
!  Add commands vcmd after pos 
!
   type(command_type), dimension(:), allocatable, intent(inout) :: cmd
   integer, intent(in)                                          :: pos
   integer, dimension(:), intent(in)                            :: vcmd
   integer :: i, ncmd, ntotc, nic
!
   nic = ncommands(cmd)
!corr   if (nic == 0 .or. pos == 0 .or. pos > ncommands(cmd)) return
   if (pos > ncommands(cmd)) return
   ncmd = size(vcmd)
   if (ncmd == 0) return
!
   ntotc = ncommands(cmd)+ncmd
   call resize_cmd(cmd,ntotc)
!
!corr   do i=pos+1,nic
!corr      cmd(i+ncmd) = cmd(i)
!corr   enddo
   if (nic /= 0) then
       cmd(pos+ncmd+1:ntotc)=cmd(pos+1:nic)
   endif
!
   do i=1,ncmd
      call cmd(pos+i)%set(vcmd(i))
   enddo
!
   end subroutine add_command

!----------------------------------------------------------------------------------------------------

   subroutine delete_commands(cmd,pos)
!
!  Delete commands
!
   type(command_type), dimension(:), allocatable, intent(inout) :: cmd
   integer, intent(in), optional                                :: pos
   integer                                                      :: ncmd,ncnew
!
   ncmd = ncommands(cmd)
   if (ncmd == 0) return
   if (present(pos)) then
       if (pos <= ncmd .and. pos > 0) then
           cmd(pos:ncmd-1) = cmd(pos+1:ncmd)
           call resize_cmd(cmd,ncmd-1)
       endif
   else
       ncnew = count(cmd%code /= DELETED_COMMAND)
       cmd(:ncnew) = pack(cmd, mask=cmd%code /= DELETED_COMMAND)
       call resize_cmd(commands,ncnew)
   endif
!
   end subroutine delete_commands

!----------------------------------------------------------------------------------------------------

   subroutine delete_commands_code(cmd,code)
!
!  Delete commands from code
!
   type(command_type), dimension(:), allocatable, intent(inout) :: cmd
   integer, intent(in)                                          :: code
!
   if (ncommands(cmd) == 0) return
   where(cmd%code == code) cmd%code = DELETED_COMMAND
   call delete_commands(cmd)
!
   end subroutine delete_commands_code 

!----------------------------------------------------------------------------------------------------

   subroutine read_commands_from_file(filename,cmd,errf)
   USE fileutil
   USE errormod
   USE strutil
   character(len=*), intent(in)                  :: filename
   type(command_type), dimension(:), allocatable :: cmd
   type(file_handle)                             :: fcmd
   type(error_type), intent(out)                 :: errf
   character(len=:), allocatable                 :: line,scomm,liner
   integer                                       :: pos, ncomm
   integer                                       :: MINLEN = 4
   integer                                       :: SIZECOM = 100
!
   call fcmd%fopen(filename)
   if (fcmd%fail()) then
       call errf%set('Cannot open: '//trim(filename)//char(10)//' Message: '//trim(fcmd%err_msg()))
       return
   endif
!
   call new_cmd(cmd,SIZECOM)
   ncomm = 0
!
   do while(get_line(fcmd%handle(),line,trimmed=.true.,filter=.true.))
      if (len(line) == 0) cycle
      if (len(line)  < MINLEN .and. line(1:1) == '%') then  !exclude unreasonable short command
          call errf%setw('Line '//line//' will be ignored')
          cycle
      endif
      if (is_comment_line(line)) cycle
      line = s_delete_comment1(line,'!')
!
      if (line(1:1) == '%') then
!
!         Command found!
          liner = line
          call cutsta(line,line2=scomm)
          if (len(scomm) > 1) then
              pos = string_locate(scomm(2:),allcom(:),exact=.false.)
          else
              pos = 0
          endif
          if (pos == 0) then
              call errf%set('Wrong command on line: '//char(10)//liner)
              return
          endif
          ncomm = ncomm + 1
          call cmd(ncomm)%set(pos)
          if (len_trim(line) > 0) then
              call cmd(ncomm)%add(line)
              cmd(ncomm)%comb = .true.
          endif
      else
          if (ncomm > 0) then
              call cmd(ncomm)%add(line)
          else
              call errf%set(' no command specified for directive '//line)
              return
          endif
      endif
!
   enddo
   call fcmd%fclose()
   call resize_cmd(cmd,ncomm)
!
   end subroutine read_commands_from_file

!----------------------------------------------------------------------------------------------------

   integer function get_ipdir(commd,icom)  result(ipd)
!
!  Pointer to last directory of the command icom-1. ipd + 1 is the first command of icom
!
   type(command_type), dimension(:), allocatable, intent(in) :: commd
   integer, intent(in)                                       :: icom
   ipd = 0
   if (ncommands(commd) == 0 .or. icom == 0) return
   ipd = sum(commd(1:icom-1)%ndir)
   end function get_ipdir

!--------------------------------------------------------------------------------------------------------

   subroutine get_additional_dir(dir,subdir,valued,pos,lens)
!
!  Find subdir of type: subdir(valued)
!
   use strutil
   character(len=*), intent(in)               :: dir,subdir
   character(len=:), allocatable, intent(out) :: valued
   integer, intent(out)                       :: pos   ! position of first character of subdir
   integer, intent(out)                       :: lens  ! length of subdir+'('+valued+')'
   integer                                    :: bpos,ier,lenv
!
   pos = 0
   if (len_trim(subdir) == 0) return
!
   pos = find_string_pos(dir,subdir//'(',')',lens)
   if (pos == 0) return
   lenv = lens - len(subdir) - 2
   if (lenv == 0) then
       valued = ' '
       return
   endif
   allocate(character(lenv)::valued)
   call get_string_in_brackets(dir(pos+len(subdir):),valued,bpos,ier)
   if (ier /= 0) then
       pos = 0
       return
   endif
!
   end subroutine get_additional_dir

!--------------------------------------------------------------------------------------------------------

   subroutine set_priority(cmd,strp)
!
!  Set priority of directives according to array strp
!
   type(command_type), intent(inout)                 :: cmd
   character(len=*), dimension(:), intent(in)        :: strp
   integer                                           :: ifound,ifp,nv
   integer                                           :: i,j
   integer, dimension(:), allocatable                :: prvet,vet
   character(len=NLENDIR), dimension(:), allocatable :: dir
   logical                                           :: ok
!
   if (cmd%ndir < 2) return
   if (size(strp) == 1) return
   allocate(prvet(cmd%ndir),source = 0)
   allocate(vet(cmd%ndir))
   ifound = 0
   do i=1,size(strp)
      call is_directive_all(cmd,strp(i),vet,nv)
      if (nv > 0) then
          do j=1,nv
             ifound = ifound + 1
             prvet(ifound) = vet(j)
          enddo
      endif
   enddo
   if (ifound > 1) then
       allocate(dir(cmd%ndir))
!
!      Order only directives in the array prvet, don't move other directives
       ifp = 0
       do i=1,cmd%ndir
          ok = .false.
          do j=1,ifound
             if (i == prvet(j)) then
                 ok = .true.
                 ifp = ifp + 1
                 dir(i) = cmd%strdir(prvet(ifp))
                 exit
             endif
          enddo
          if (.not.ok) dir(i) = cmd%strdir(i)
       enddo
       cmd%strdir(:cmd%ndir) = dir(:)
   endif
!
   end subroutine set_priority

END MODULE commandsmod
