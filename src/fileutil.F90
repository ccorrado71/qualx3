MODULE fileutil

!S get_unit ( iunit )                             returns a free FORTRAN unit number.
!F function file_exist(namefile)                  Controlla se esiste un file dal nome
!S get_extension(filename,ext,ier)                estrai l'estensione dal nome del file
!F has_extension(filename)                        Check if filename has extension
!F extension_is(filename,ext)                     Check if filename has extension equal to ext
!F file_rem_ext(filename)                         rimuove l'estensione dal nome di un file
!F is_comment_line(line,svet)                     Check if line contains comment character at first row
!F s_delete_comment(line,scomm) result(str)       Elimina da line la parte commentata con scomm
!F s_delete_comment1(line,scomm) result(str)      Elimina da line la parte commentata con scomm (allocatable version)
!S find_key_file(fhandle,keystr,nline,line,ier)   Read file and stop when find keystr string
!S find_key_file_a(fhandle,keystr,nline,line,ier) Read file and stop when find keystr string (allocatable version)
!F run_system(command)                            Run command
!F get_recordtype(filename)  result(stype)        Determine the type of record
!F get_line(aunit, InLine, trimmed) result(OK)    Reads a complete line (end-of-record terminated) from a file.
!F function jump_non_numeric(aunit)               Jump non numeric and empty lines
!F ignore_lines(aunit,nlines)  result(ier)        Ignore lines in a file
!F get_homepath()                                 Get path of home
!F get_cwd()                                      Get current working directory
!F is_file_open(filename) result(open_flag)       Check if file is open
!F is_file_writable(filename) result(open_flag)   Check if file is writable

   !USE strutil

implicit none

   integer, parameter :: IO_MSG_LEN = 200

   type file_handle 
      private 
      integer                   :: handle_value
      integer                   :: ier = 0
      character(len=IO_MSG_LEN) :: io_msg

   contains
      procedure :: fclose
      procedure :: fopen
      procedure :: good
      procedure :: fail
      procedure :: handle => get_handle
      procedure :: err => file_error
      procedure :: err_msg 
      procedure :: rew 

      !final :: file_close
   end type file_handle

   character(len=*), dimension(3), parameter, private :: DEF_COMM_CHAR = ['>','!','#']

CONTAINS

   subroutine fopen(fileh,filename,ios)
!
!  Open file
!
   class(file_handle), intent(inout)      :: fileh
   character(len=*), intent(in)           :: filename
   character(len=*), intent(in), optional :: ios
   character(len=5)                       :: saction
   character(len=7)                       :: stat
   character(len=11)                      :: sform
   call get_unit(fileh%handle_value)
   saction = 'read'
   stat = 'old'
   sform = 'formatted'
   if (present(ios)) then
       if (index(ios,'w') > 0) then
           saction = 'write'
           stat = 'unknown'
       endif
       if (index(ios,'b') > 0) then
           sform = 'unformatted'
       endif
   endif
   fileh%io_msg = ' '
   open(unit=fileh%handle_value,file=filename,iostat=fileh%ier,status=stat,action=saction,form=sform,iomsg=fileh%io_msg)
   end subroutine fopen

!-------------------------------------------------------------------------------------------------------

   subroutine fclose(fileh)   
!
!  Close file
!
   class(file_handle), intent(inout) :: fileh
   close(fileh%handle_value,iostat=fileh%ier)
   end subroutine fclose

!-------------------------------------------------------------------------------------------------------

   subroutine file_close(fileh)
   type(file_handle), intent(inout) :: fileh
   call fileh%fclose()
   end subroutine file_close

!-------------------------------------------------------------------------------------------------------
  
   logical function good(fileh)
   class(file_handle), intent(in) :: fileh
   good = fileh%ier == 0
   end function good

!-------------------------------------------------------------------------------------------------------

   logical function fail(fileh)
   class(file_handle), intent(in) :: fileh
   fail = fileh%ier /= 0
   end function fail

!-------------------------------------------------------------------------------------------------------

   integer function get_handle(fileh) result(handle)
   class(file_handle), intent(in) :: fileh
   handle = fileh%handle_value
   end function get_handle

!-------------------------------------------------------------------------------------------------------

   integer function file_error(fileh)
   class(file_handle), intent(in) :: fileh
   file_error = fileh%ier
   end function file_error

!-------------------------------------------------------------------------------------------------------

   function err_msg(fileh) 
   class(file_handle), intent(in) :: fileh
   character(len=IO_MSG_LEN)      :: err_msg
   err_msg = fileh%io_msg
   end function err_msg

!-------------------------------------------------------------------------------------------------------

   subroutine rew(fileh)
   class(file_handle), intent(in) :: fileh
   rewind (fileh%handle())
   end subroutine rew

!-------------------------------------------------------------------------------------------------------

  subroutine get_unit ( iunit )
!
! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer IUNIT, the free unit number.
!
   implicit none

   integer i
   integer ios
   integer iunit
   logical lopen
  
   iunit = 0
  
   do i = 1, 99
  
     if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

       inquire ( unit = i, opened = lopen, iostat = ios )
  
       if ( ios == 0 ) then
         if ( .not. lopen ) then
           iunit = i
           return
         end if
       end if

     end if
  
   end do

   return
   end subroutine get_unit

  !---------------------------------------------------------------------------------------------

   logical function file_exist(filename)
!   
!  Controlla se esiste un file dal nome
!
   character(len=*), intent(in) :: filename
!
   inquire(file=filename,exist=file_exist)
!   
   end function file_exist

  !---------------------------------------------------------------------------------------------

   subroutine file_delete(filename)
!
!  Delete file
!
   character(len=*), intent(in) :: filename
   integer                      :: stat
   integer                      :: handle
   open(newunit=handle, iostat=stat, file=filename, status='old')
   if (stat == 0) close(handle, status='delete')
   end subroutine file_delete

  !---------------------------------------------------------------------------------------------

   function get_extension(filename)  result(ext)
!
!  Estrai l'estensione dal nome del file
!
   USE STRUTIL, only:upper
   character(len=*), intent(in)  :: filename
   character(len=:), allocatable :: ext
   integer                       :: pos
   integer                       :: len_file,len_ext
!
   pos = index(filename,'.',back=.true.)
   len_file = len_trim(filename)
   len_ext = len_file - pos
!
   if (len_ext == 0 .or. pos == 0) then !  Se non esiste un'estensione 
       ext = ' '
   else
       ext = filename(pos+1:len_file)
   endif
!
   end function get_extension

 !----------------------------------------------------------------------------------

   logical function has_extension(filename)
!
!  Check if filename has extension
!
   character(len=*), intent(in) :: filename
!
   has_extension = index(filename,'.',back=.true.) > 0
!
   end function has_extension

 !----------------------------------------------------------------------------------

   logical function extension_is(filename,ext)
!
!  Check if filename has extension equal to ext
!
   USE strutil
   character(len=*), intent(in) :: filename,ext
   extension_is = s_eqi(get_extension(filename),ext)
   end function extension_is

 !----------------------------------------------------------------------------------

   character(len=1) function get_separ()
#if _WIN32
   get_separ = '\'     ! Windows
#else
   get_separ = '/'     ! Linux, Mac
#endif
   end function get_separ

 !----------------------------------------------------------------------------------

   function file_get_name(pfilename)  result(filename)
!
!  Input  : pfilename  = percorso + nome del file 
!  Output : filename   = nome del file
!
   character(len=*), intent(in)  :: pfilename
   character(len=:), allocatable :: filename
   integer                       :: pos
!
   pos = index(pfilename,get_separ(),back=.true.)  
   if (pos == 0) then
       filename = trim(pfilename)
       return 
   endif
   if (pos == len_trim(pfilename)) then
       filename = ' '
       return
   endif
   filename = trim(pfilename(pos+1:))
!   
   end function file_get_name

  !---------------------------------------------------------------------------------------------

   function file_get_dir(pfilename) result(dir)
   character(len=*), intent(in)       :: pfilename
   character(len=len_trim(pfilename)) :: dir
   integer                            :: pos
!
   pos = index(pfilename,get_separ(),back=.true.)  
   if (pos > 0) then
       dir = pfilename(:pos)
   else
       dir = ' '
   endif
!
   end function file_get_dir

  !---------------------------------------------------------------------------------------------

   function file_rem_ext(filename)  result(sfile)
!
!  Rimuove l'estensione dal nome di un file
!   
   character(len=*), intent(in)  :: filename
   character(len=:), allocatable :: sfile
   integer                       :: pos
   integer                       :: lenext
!
   sfile = trim(filename)
   pos = index(filename,'.',back=.true.)
   if (pos > 0) then                    !se esiste un'estensione
       lenext = len_trim(filename)-pos  !lunghezza dell'estensione
       if (lenext <= 4) then            !max length of extension is 4
           sfile = filename(1:pos-1)
       endif
   endif
!   
   end function file_rem_ext

  !---------------------------------------------------------------------------------------------

   function file_basename(filename)
   character(len=*), intent(in)  :: filename
   character(len=:), allocatable :: file_basename

   file_basename = file_rem_ext(file_get_name(filename))
   
   end function file_basename

  !---------------------------------------------------------------------------------------------

   function file_change_ext(filename,ext) result(sfile)
!
!  Change extension for filename
!
   character(len=*), intent(in)  :: filename ! name of file
   character(len=*), intent(in)  :: ext      ! new extension without point
   character(len=:), allocatable :: sfile
!
   sfile = file_rem_ext(filename)//'.'//trim(ext)
!
   end function file_change_ext

  !---------------------------------------------------------------------------------------------

   function file_change_name(filename,newfilename) result(sfile)
   character(len=*), intent(in)  :: filename
   character(len=*), intent(in)  :: newfilename
   character(len=:), allocatable :: sfile
!
   sfile = trim(file_get_dir(filename))
   if (len_trim(sfile) > 0) then
       sfile = sfile//trim(newfilename)
   else
       sfile = trim(newfilename)
   endif
!
   end function file_change_name

  !---------------------------------------------------------------------------------------------

   logical function is_comment_line(line,svet)  result(is)
!
!  Check if line contains comment character at first row. Line should be left trimmed
!
   character(len=*), intent(in)           :: line
   character(len=*), dimension(:), intent(in), optional :: svet
   integer :: i
!
   if (present(svet)) then
       do i=1,size(svet)
          is = index(line,trim(svet(i))) == 1
          if (is) return
       enddo
   else
       do i=1,size(DEF_COMM_CHAR)
          is = index(line,DEF_COMM_CHAR(i)) == 1
          if (is) return
       enddo
   endif
!
   end function is_comment_line

  !---------------------------------------------------------------------------------------------
!old
!old   logical function is_comment_line(line,s1,s2,s3)
!old!
!old!  Controlla se la linea ha un  commento del tipo s1 o s2 o s3 all'inizio
!old!
!old   character(len=*), intent(in)           :: line
!old   character(len=*), intent(in)           :: s1
!old   character(len=*), intent(in), optional :: s2,s3
!old   logical, dimension(3)                  :: is_comm
!old!
!old   is_comm(1) = index(line,s1) == 1
!old   if (present(s2)) then
!old       is_comm(2) = index(line,s2) == 1
!old   else
!old       is_comm(2) = .false.
!old   endif
!old   if (present(s3)) then
!old       is_comm(3) = index(line,s3) == 1
!old   else
!old       is_comm(3) = .false.
!old   endif
!old   is_comment_line = any(is_comm .eqv. .true.)
!old!
!old   end function
!old
  !---------------------------------------------------------------------------------------------
  
   function s_delete_comment(line,scomm) result(str)
!
!  Elimina da line la parte commentata con scomm
!
   character(len=*), intent(in) :: line
   character(len=*), intent(in) :: scomm
   integer                      :: pos
   character(len=len(line))     :: str
!
   str = line
   pos = index(str,scomm) 
   if (pos > 0) then
       str = line(1:pos-1)
       write(0,*)'line=',trim(str)
   endif
!
   end function s_delete_comment

  !---------------------------------------------------------------------------------------------
  
   function s_delete_comment1(line,scomm) result(str)
!
!  Elimina da line la parte commentata con scomm
!
   character(len=*), intent(in) :: line
   character(len=*), intent(in) :: scomm
   integer                      :: pos
   character(len=:), allocatable :: str
!
   pos = index(line,scomm) 
   if (pos > 0) then
       str = trim(line(1:pos-1))
   else
       str = line
   endif
!
   end function s_delete_comment1

!old  !---------------------------------------------------------------------------------------------
!old
!old   subroutine handle_open_error(fname,errcode,errmsg)
!old#if defined(__GFORTRAN__)
!old#elif defined(__INTEL_COMPILER)
!old   include 'for_iosdef.for'
!old#endif
!old   integer, intent(in)           :: errcode
!old   character(len=*), intent(in)  :: fname
!old   character(len=*), intent(out) :: errmsg
!old!
!old#if defined(__GFORTRAN__)
!old   errmsg = 'Error on opening file '//trim(fname)
!old#elif defined(__INTEL_COMPILER)
!old   select case (errcode)
!old       case (FOR$IOS_FILNOTFOU)
!old          errmsg = 'Cannot find file '//trim(fname)
!old
!old       case (FOR$IOS_FILNAMSPE)
!old          errmsg = 'File '//trim(fname)//' was bad'
!old
!old       case default
!old          errmsg = 'Error on opening file '//trim(fname)
!old   end select
!old#else
!old   errmsg = 'Error on opening file '//trim(fname)
!old#endif
!old!
!old   end subroutine handle_open_error
!old
!old  !---------------------------------------------------------------------------------------------
!old
!old   subroutine set_file_error(fname,errcode,error)
!old   USE errormod
!old   integer, intent(in)             :: errcode
!old   character(len=*), intent(in)    :: fname
!old   type(error_type), intent(inout) :: error
!old!
!old   call error%set()
!old   call handle_open_error(fname,errcode,error%message)
!old!
!old   end subroutine set_file_error
!old
  !---------------------------------------------------------------------------------------------

   subroutine find_key_file(fhandle,keystr,nline,line,ier)
!
!  Read file and stop when find keystr string or the upto string
!
   integer, intent(in)             :: fhandle
   character(len=*), intent(in)    :: keystr
   integer, intent(inout)          :: nline
   character(len=*), intent(inout) :: line
   integer, intent(out)            :: ier
   integer                         :: nline_sav
!
   nline_sav = nline
   do
      nline = nline + 1
      read(fhandle,'(a)',iostat=ier)line
      if (ier < 0) then
          nline = nline_sav
          exit
      endif
      if(index(line,keystr) > 0 ) exit
   enddo
!
   end subroutine find_key_file

  !---------------------------------------------------------------------------------------------

   subroutine find_key_file_a(fhandle,keystr,nline,line,ier)
!
!  Read file and stop when find keystr string or the upto string
!
   integer, intent(in)                          :: fhandle
   character(len=*), intent(in)                 :: keystr
   integer, intent(inout)                       :: nline
   character(len=:), intent(inout), allocatable :: line
   integer, intent(out)                         :: ier
   integer, parameter                           :: line_buf_len= 1024*4
   character(LEN=line_buf_len)                  :: InS
!
   line = ' '
   do
      nline = nline + 1
      read(fhandle,'(a)',iostat=ier)InS
      if (ier /= 0) exit
      if(index(InS,keystr) > 0) then
         line = trim(InS)
         exit
      endif
   enddo
!
   end subroutine find_key_file_a

  !---------------------------------------------------------------------------------------------

   integer function run_system(command) result(ier)
#ifdef __INTEL_COMPILER
   USE IFPORT
#endif
   character(len=*), intent(in) :: command
#ifdef __INTEL_COMPILER
   ier = system(command)
#elif GCC_VERSION>=6 
   call execute_command_line(command,cmdstat=ier)
#else
   ier = 0
   call system(command)
#endif
   end function run_system

  !---------------------------------------------------------------------------------------------

   !subroutine get_executabledir()
   !USE IFPORT
   !character(len=:), allocatable :: current_dir
   !integer :: pos, length, error
   !character(len=500) :: cdir
   !call get_command_argument(number=0,length=length,status=error)
   !   write(0,*)error,'DIR=',length
   !allocate(character(length) :: current_dir)
   !call get_command_argument(number=0,value=current_dir,status=error)
   !pos = index(current_dir,get_separ(),back=.true.)
   !   write(0,*)'DIR='//current_dir(:pos)
   !   error = getcwd(cdir)
   !   write(0,*)'DIR='//trim(cdir)
   !end subroutine get_executabledir

  !---------------------------------------------------------------------------------------------

   function get_recordtype(filename)  result(stype)
!
!  Determine the type of record
!
   character(len=*), intent(in) :: filename
   character(len=11)            :: stype
   type(file_handle)            :: fileh  
   character(len=500)           :: line
   character(len=*), parameter :: LF = char(10), CR = char(13)
   stype = ' '
   call fileh%fopen(filename,'b')
   if (fileh%good()) then
       read(fileh%handle()) line
       if (index(line,CR//LF) == 0 .and. index(line,CR) > 0) then
           stype = 'STREAM_CR'
       elseif (index(line,CR//LF) > 0) then
           stype = 'STREAM_CRLF'
       elseif (index(line,LF) > 0) then
           stype = 'STREAM_LF'
       endif
       call fileh%fclose()
   endif
   end function

  !---------------------------------------------------------------------------------------------

   function get_line(aunit, InLine, trimmed, filter) result(OK)
!
!  Reads a complete line (end-of-record terminated) from a file.
!
   USE strutil
   integer, intent(IN)                     :: aunit
   character(LEN=:), allocatable, optional :: InLine
   logical, intent(in), optional           :: trimmed
   logical, intent(in), optional           :: filter
   integer, parameter                      :: line_buf_len= 1024*4
   character(LEN=line_buf_len)             :: InS
   logical                                 :: OK, set
   integer                                 :: size,stat
   
   OK = .false.
   set = .true.
   do
       read (aunit,'(a)',advance='NO',iostat=stat,size=size) InS
       OK = .not. IS_IOSTAT_END(stat)
       if (.not. OK) return
       if (present(InLine)) then
           if (set) then
               InLine = InS(1:size)
               set=.false.
           else
               InLine = InLine // InS(1:size)
           end if
       end if
       if (IS_IOSTAT_EOR(stat)) exit
   end do
   if (present(InLine)) then
       if (present(filter)) then
           if (filter) then
               !call s_filter(InLine)
               call s_tab_blank(InLine)
           endif
       endif
       if (present(trimmed)) then
           if (trimmed) InLine = trim(adjustl(InLine))
       end if
   endif
   
   end function get_line

  !--------------------------------------------------------------------------

   subroutine set_record_pos(unitfile,pos)
   integer, intent(in) :: unitfile
   integer, intent(in) :: pos
   integer             :: i
!
   rewind(unitfile)
   do i=1,pos
      read(unitfile,*)
   enddo
!   
   end subroutine set_record_pos

  !---------------------------------------------------------------------------------------------

   logical function jump_non_numeric(aunit)  result(iend)
!
!  Jump non numeric and empty lines
!
   USE strutil
   integer, intent(IN)           :: aunit
   character(len=:), allocatable :: line
   integer                       :: nrec 
!
   iend = .true.
   nrec = 0
   do while(get_line(aunit,line,trimmed=.true.))
      nrec = nrec + 1
      if (len_trim(line) == 0) cycle
      if (ch_is_digit(line(1:1))) then
          iend = .false.
          !backspace(aunit,iostat=ier) ! don't trust on basckspace after get_line with intel
          call set_record_pos(aunit,nrec-1)
          return
      endif
   enddo
!
   end function jump_non_numeric

  !---------------------------------------------------------------------------------------------

   integer function ignore_lines(aunit,nlines,errline)  result(ier)
!
!  Ignore lines in a file
!
   integer, intent(in) :: aunit,nlines
   integer, optional   :: errline
   integer             :: i
!
   ier = 0
   do i=1,nlines
      read(aunit,*,iostat=ier)
      if (ier /= 0) then
          if (present(errline)) errline = i
          exit
      endif
   enddo
!
   end function ignore_lines

 !---------------------------------------------------------------------------------------------

   function get_homepath()
!
!  Get path of home
!
#ifdef __INTEL_COMPILER
   USE IFPORT
#endif
   character(len=260)            :: homedir
   character(len=:), allocatable :: get_homepath
#if _WIN32
   character(len=3)              :: systemdrive
   call getenv ("SystemDrive",systemdrive)
   call getenv("HOMEPATH",homedir)
   get_homepath = trim(systemdrive)//trim(homedir)
#else
   call getenv("HOME",homedir)
   get_homepath = trim(homedir)
#endif
   end function get_homepath

 !---------------------------------------------------------------------------------------------

   function get_cwd()
!
!  Get current working directory
!
#ifdef __INTEL_COMPILER
   USE IFPORT
   integer                       :: istat
#endif
   character(len=:), allocatable :: get_cwd
   character(len=260)            :: cwd
!
#ifdef __INTEL_COMPILER
   istat = getcwd(cwd)
#else
   call getcwd(cwd)
#endif
   get_cwd = trim(cwd)
!
   end function get_cwd

 !---------------------------------------------------------------------------------------------

   function is_file_open(filename) result(status)
   implicit none
   character(len=*), intent(in) :: filename
   logical :: status
   integer :: unit, io_status

   ! Inquire a new unit number to use for opening the file
   !inquire(newunit=unit)
   call get_unit(unit)

   ! Attempt to open the file in read mode
   open(unit, file=filename, status='old', action='read', iostat=io_status)

   if (io_status /= 0) then
       ! If an error occurs, assume the file is open by another program
       status = .true.
   else
       ! If opened successfully, close the file and indicate it's not open elsewhere
       close(unit)
       status = .false.
   end if
   end function is_file_open

 !---------------------------------------------------------------------------------------------

    function is_file_writable(filename) result(status)
    implicit none
    character(len=*), intent(in) :: filename
    logical :: status
    integer :: unit, io_status

    ! Inquire a new unit number to safely open the file
    !inquire(newunit=unit,file=filename)
    call get_unit(unit)

    ! Attempt to open the file in write mode with status 'unknown'
    open(unit, file=filename, action='write', status='unknown', iostat=io_status)

    if (io_status /= 0) then
        ! If an error occurs, assume the file is not writable
        status = .false.
    else
        ! If opened successfully, close the file and indicate it's writable
        close(unit)
        status = .true.
    end if
    end function is_file_writable

END MODULE fileutil
