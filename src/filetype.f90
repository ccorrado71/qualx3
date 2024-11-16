MODULE filereading

 USE errormod
 USE pointmod, only:point_type
 
 implicit none

 enum, bind(c)
   enumerator :: FTYPE_ALL=1,FTYPE_COUNTS,FTYPE_XY,FTYPE_GSAS,FTYPE_CIF,FTYPE_XYE,FTYPE_UXD
   enumerator :: FTYPE_CPI,FTYPE_XDD,FTYPE_DBW,FTYPE_XDA,FTYPE_UDF,FTYPE_XRDML,FTYPE_POW,FTYPE_DOUBLE
 endenum

 integer, parameter :: COUNT_2THETA_TYPE = 1
 integer, parameter :: COUNT_D_TYPE = 2

 CONTAINS

   subroutine read_datafile(filename,ftype,tth_min,tth_step,tth_max,counts,ncounts,use_step,nwave,wave,ratio,count_type,errf)
   USE fileutil
   character(len=*), intent(in)                 :: filename
   integer, intent(in)                          :: ftype
   real, intent(inout)                          :: tth_min,tth_max,tth_step
   type(point_type), allocatable, intent(inout) :: counts(:)
   integer, intent(out)                         :: ncounts
   logical, intent(in)                          :: use_step
   integer, intent(out)                         :: nwave
   real, dimension(3), intent(out)              :: wave,ratio
   integer, intent(out)                         :: count_type
   type(error_type), intent(out)                :: errf
   type(file_handle)                            :: fileh
!
   nwave = 0
   wave = 0.0
   ratio = [1.0,0.0,0.0]
   count_type = COUNT_2THETA_TYPE
   call fileh%fopen(filename)
   if (fileh%good()) then
       select case(ftype)

           case (FTYPE_XY,FTYPE_XYE)             ! leggi 2 colonne con 2-theta e conteggi
             call read2columns(fileh%handle(),filename,tth_min,tth_max,tth_step,counts,ncounts,nwave,wave,ratio,use_step,errf)

           case (FTYPE_COUNTS,FTYPE_DBW)         ! leggi file *.dat; DBWS data (*.dbw)
             call readonlyc(fileh%handle(),filename,tth_min,tth_step,tth_max,counts,ncounts,.false.,errf)

           case (FTYPE_GSAS)                     ! leggi file GSAS
             call readgsas(fileh%handle(),tth_min,tth_step,tth_max,counts,ncounts,errf)
    
           case (FTYPE_CIF)                      ! leggi file pdCIF
             call readcif(fileh%handle(),tth_min,tth_step,tth_max,counts,ncounts,nwave,wave,ratio,count_type,errf)

           case (FTYPE_UXD)                      ! Siemens data (*.uxd)
             call readuxd(fileh%handle(),filename,tth_min,tth_step,tth_max,counts,nwave,wave,ratio,errf)

           case (FTYPE_CPI)                      ! Sietronics Sieray data (*.cpi)
             call readcpi(fileh%handle(),filename,tth_min,tth_step,tth_max,counts,nwave,wave,ratio,errf)

           case (FTYPE_XDD)                      ! XDD data (*.xdd)
             call readxdd(fileh%handle(),filename,tth_min,tth_step,tth_max,counts,errf)

           case (FTYPE_XDA)                      ! XDA data (*.xda)
             call readxda(fileh%handle(),filename,tth_min,tth_step,tth_max,counts,errf)

           case (FTYPE_UDF)                      ! Philips UDF data (*.udf)
             call readudf(fileh%handle(),filename,tth_min,tth_step,tth_max,counts,nwave,wave,ratio,errf)

           case (FTYPE_XRDML)                    ! PANalytical XRDML data (*.xrdml)
             call readxrdml(fileh%handle(),filename,tth_min,tth_step,tth_max,counts,nwave,wave,ratio,errf)

           case (FTYPE_POW)
!
!            leggi prima xy ed eventualmente segnala l'errore sulla lettura di xy
             call read2columns(fileh%handle(),filename,tth_min,tth_max,tth_step,counts,ncounts,nwave,wave,ratio,use_step,errf)
             if (errf%signal) then
                 rewind(fileh%handle())
                 call readonlyc(fileh%handle(),filename,tth_min,tth_step,tth_max,counts,ncounts,.false.,errf)
             endif
 
           case (FTYPE_DOUBLE)                   ! leggi riga conteggi e riga 2theta
             call readonlyc(fileh%handle(),filename,tth_min,tth_step,tth_max,counts,ncounts,.true.,errf)

           case default
             call errf%set('Unknown File Type.'//char(10)//'File: '//trim(filename))
       end select
!
       call fileh%fclose()
   else
       call errf%set('Cannot open '//trim(filename)//char(10)//' Message: '//trim(fileh%err_msg()))
   endif
!
   end subroutine read_datafile

  !---------------------------------------------------------------------------------------------

   integer function filetype_from_string(str) result(ftype)
   USE strutil
   character(len=*), intent(in) :: str
   character(len=len_trim(str)) :: str1
!
   str1 = upper(str)
   select case(str1)
      case ('COUNTS')
        ftype = FTYPE_COUNTS
      case ('XY')
        ftype = FTYPE_XY
      case ('DOUBLE')
        ftype = FTYPE_DOUBLE
      case ('GSAS')
        ftype = FTYPE_GSAS
      case ('CIF')
        ftype = FTYPE_CIF
      case default
        ftype = 0
   end select
!
   end function filetype_from_string

  !---------------------------------------------------------------------------------------------

   integer function filetype_from_filename(filename) result(ftype)
!
!  Identifica il tipo di file dall'estensione
!
   USE fileutil
   USE strutil
   character(len=*), intent(in)      :: filename
   character(len=len_trim(filename)) :: ext
!
   ext = get_extension(filename)
   if (len_trim(ext) == 0) then
       ftype = 0    !!!!!FTYPE_POW
   else
       ext = upper(ext)
       select case (ext(:Lung(ext)))
         case ('XY')
           ftype = FTYPE_XY
         case ('CIF','RTV')
           ftype = FTYPE_CIF
         case ('GDA')
           ftype = FTYPE_GSAS
         case ('POW')
           ftype = FTYPE_POW
         case ('DAT','TXT')
           ftype = FTYPE_POW
         case ('XYE')
           ftype = FTYPE_XYE
         case ('UXD') 
           ftype = FTYPE_UXD
         case ('CPI') 
           ftype = FTYPE_CPI
         case ('XDD') 
           ftype = FTYPE_XDD
         case ('DBW')
           ftype = FTYPE_DBW
         case ('XDA')
           ftype = FTYPE_XDA
         case ('UDF')
           ftype = FTYPE_UDF
         case ('XRDML')
           ftype = FTYPE_XRDML
         case default   
           ftype = 0   !!!!!FTYPE_POW

       end select
   endif

   end function filetype_from_filename

  !---------------------------------------------------------------------------------------------

   subroutine jump_rows(unit)
!
!  Salta le righe iniziali contrassegnate dal carattere '#' o '>' o '!'
!
   integer, intent(in)          :: unit
   character(len=80)            :: line
   integer                      :: ierror
!
   do
      read(unit,*,iostat=ierror)line
      if (ierror < 0) return             ! controllo per fine file
      line = adjustl(line)
      if (scan(line(1:1),'#>!') == 0) exit
   enddo
!
!  spostati all'ultimo record
   backspace (unit)
!
   end subroutine jump_rows
   
  !---------------------------------------------------------------------------------------------

   function read_start_step_stop(aunit,ftype,tmin,tstep,tmax,ncounts)  result(err)
!
!  Read line with: 
!                 start,step,stop  ! type = 1 e.g., xdd, dat
!                 start,stop,step  ! type = 2 e.g., xda
!
   USE strutil
   USE fileutil
   integer, intent(in)             :: aunit
   integer, intent(in)             :: ftype
   real, intent(out)               :: tmin,tstep,tmax
   integer, intent(out)            :: ncounts
   type(error_type)                :: err
   real, dimension(:), allocatable :: vet
   integer                         :: iv
   character(len=:), allocatable   :: line
!
   if (.not. get_line(aunit,line,trimmed=.true.)) then
       call err%set("Error reading 2theta min., 2theta step, 2theta max. before the countings")
       return
   endif
   call getnum1(line,vet,iv=iv)
   if (iv < 3) then
       call err%set('It expects 2theta min., 2theta step, 2theta max. before the countings')
       return
   endif
   tmin = vet(1)
   if (ftype == 1) then
       tstep = vet(2)
       tmax = vet(3)
   else
       tmax = vet(2)
       tstep = vet(3)
   endif
   ncounts = floor((tmax-tmin)/tstep + 1)
   if (ncounts <= 1) then
       call err%set('2theta min ='//r_to_s(tmin)//char(10)//'2theta max. ='//r_to_s(tmax)//char(10)//    &
                            '2theta step ='//r_to_s(tstep)//char(10)//                                     &
       'Please insert correct 2theta min., 2theta step, 2theta max. before the countings or select the correct format of file')
   endif
!
   end function read_start_step_stop

  !---------------------------------------------------------------------------------------------

   subroutine readonlyc(aunit,filename,tmin,tstep,tmax,dati,ndata,double,error)
!
!  Legge un file di soli conteggi.
!  2theta min,2theta step,2theta max in prima riga e conteggi nelle righe successive.
!  Se si genera errore le variabili tmin,tstep,tmax,xco,yco,nc non vengono modificate. 
!
   USE strutil
   USE fileutil
   USE pointmod
   USE arrayutil
   integer, intent(in)                          :: aunit 
   character(len=*), intent(in)                 :: filename     
   real, intent(inout)                          :: tmin,tstep,tmax   ! 2theta min., 2theta step, 2theta max.
   type(point_type), allocatable, intent(inout) :: dati(:)
   integer, intent(inout)                       :: ndata
   logical, intent(in)                          :: double
   type(error_type), intent(out)                :: error
   integer                                      :: nc
   character(len=:), allocatable                :: line
   real, dimension(:), allocatable              :: vet
   integer                                      :: iv
   integer                                      :: i
   real, dimension(:), allocatable              :: xdata
   integer                                      :: istart,iend
   integer, parameter                           :: INITDIM=10000
!
!  Jump non numeric line
   if(jump_non_numeric(aunit)) then
      call error%set('Unexpected end-of-file: file'//char(10)//trim(filename))
      return
   endif
!
!  Read thmin,thmax,thstep
   error = read_start_step_stop(aunit,1,tmin,tstep,tmax,nc)
   if (error%signal) return
!
   if (double) then
       ndata = 0
       istart = 0
       iend = 0
       call new_array(xdata,INITDIM)
       do while(get_line(aunit,line,trimmed=.true.))
          call s_filter(line)
          if (len(line) == 0) cycle
          call getnum1(line,vet,iv=iv)
          if (iv == 0 .or. err_string) exit
          istart = iend + 1
          iend = istart + iv - 1
          if (iend > size(xdata)) then
              call resize_array(xdata,size(xdata) + INITDIM)
          endif
          xdata(istart:iend) = vet(:iv)
          if (.not.get_line(aunit)) exit
       enddo
       call resize_array(xdata,iend)
   else
       call read_data_block(aunit,xdata)
   endif
!
   ndata = size_array(xdata)
   if (ndata == 0) then
       call error%set('Error reading file: '//char(10)//trim(filename))
       return
   endif
   if (ndata > nc) ndata = nc  ! compure with number of counts from tmax
!
!  Assign counts
   call resize_points(dati,ndata)
   dati%y = xdata(:ndata)
   dati(:)%x = tmin + (/(i*tstep,i=0,ndata-1)/)
   tmax = dati(ndata)%x
!
   end subroutine readonlyc

 !---------------------------------------------------------------------------------------------

   subroutine read2columns(unit,filename,tmino,tmaxo,tstep,dati,nco,nwave,wave,ratio,usestp,error)
!
!  Legge un file con due colonne (2theta e conteggi)   
!  Se si genera errore le variabili tmin,tstep,tmax,xco,yco,nc non vengono modificate. 
! 
   USE strutil
   USE fileutil
   USE pointmod
   integer, intent(in)                          :: unit
   character(len=*), intent(in)                 :: filename
   real, intent(out)                            :: tmino,tmaxo
   real, intent(inout)                          :: tstep
   type(point_type), allocatable, intent(inout) :: dati(:)
   integer, intent(out)                         :: nco
   integer, intent(out)                         :: nwave
   real, dimension(3), intent(out)              :: wave,ratio
   logical, intent(in)                          :: usestp
   type(error_type), intent(out)                :: error
!corr   real, intent(out)                            :: wavel ! if <0 wave is not available
   type(point_type), allocatable                :: datic(:)
   integer                                      :: nc
   integer                                      :: i
   character(len=200)                           :: line
   integer                                      :: ierror
   real, dimension(40)                          :: vet
   integer, dimension(40)                       :: ivet
   integer                                      :: iv
   character(len=6)                             :: strn
   integer                                      :: ncc
!corr   real                                         :: wval
!
   !call reset_error(error)
   call error%reset()
!
!  salta le prime righe con '#'
   call jump_rows(unit)
!
!  Calcola il numero di conteggi
   call count_line(unit,nc,ierror)
   if (ierror > 0 .or. nc == 0) then
       call error%set('Error reading from file '//trim(filename))
       return
   endif
!
!  Alloca variabili su ncounts
   allocate(datic(nc))
!
!  Leggi 2-theta e conteggi
   call jump_rows(unit)
   ncc = 0
!corr   wavel = -1
   nwave = 0
!corr   wval = -1
   do i=1,nc
      read(unit,'(a)') line
      call s_filter(line)              !filtra linea
      if (len_trim(line) == 0) cycle   !salta le linee vuote
      if (is_comment_line(line)) cycle
      call getnum(line,vet,ivet,iv)
      if (ncc == 0 .and. iv == 1) then !wavelength found!
          nwave = 1
          wave(1) = vet(1)
          ratio(1) = 1.0
!corr          wval = vet(1)
          cycle
      endif
      if (iv < 2) then   !!!!corr .or. err_string) then
          write(strn,'(i6)')i
          call error%set('Error reading row '//trim(adjustl(strn))//char(10)//trim(line))
          go to 10
      endif
      ncc = ncc + 1
      datic(ncc)%x = vet(1)
      datic(ncc)%y = vet(2)
   enddo
!
   if (ncc == 0) then
       call error%set('Error reading from file '//trim(filename))
       return
   endif
!
!  Esegui controllo sui 2theta: ogni 2theta deve essere inferiore al suo successivo
   if (any(datic(2:ncc)%x < datic(1:ncc-1)%x)) then
       call error%set('Error reading column of 2theta')
       go to 10
   endif
!
!  ora in assenza di errore puoi trasferire in xco e yco   
!corr   if (wval > 0) wavel = wval
   nco = ncc
   tmino = datic(1)%x
   tmaxo = datic(ncc)%x   
   call new_points(dati,ncc)
   do i=1,ncc
      dati(i) = datic(i)
   enddo
!
   if (usestp) then
!
!     Ricalcola i conteggi usando lo step 
      dati%x = tmino + (/(i*tstep,i=0,ncc-1)/)
      tmaxo = dati(ncc)%x
   else
!
!     Calcola lo step e usa i conteggi del file
!corr      tstep = sum(dati(2:ncc)%x - dati(1:ncc-1)%x)/(ncc-1)
      tstep = get_tstep(dati%x)
   endif
!
10 return
!
   end subroutine read2columns

 !---------------------------------------------------------------------------------------------

   real function get_tstep(xdata)
   real, dimension(:), intent(in) :: xdata
   integer                        :: ncc
   ncc = size(xdata)
   get_tstep = 0
   if (ncc < 2) return
   get_tstep = sum(xdata(2:ncc) - xdata(1:ncc-1))/(ncc-1)
   end function get_tstep

 !---------------------------------------------------------------------------------------------

   subroutine readgsas(unit,tmino,tstepo,tmaxo,dati,nco,error)
!
!  Legge un file di soli conteggi.
!  2theta min,2theta step,2theta max in prima riga e conteggi nelle righe successive.
!  Se si genera errore le variabili tmin,tstep,tmax,xco,yco,nc non vengono modificate. 
!
   USE strutil
   USE pointmod
   integer, intent(in)                          :: unit                 ! unit� di lettura
   real, intent(inout)                          :: tmino,tstepo,tmaxo   ! 2theta min., 2theta step, 2theta max.
   type(point_type), allocatable, intent(inout) :: dati(:)
   integer, intent(inout)                       :: nco                  ! numero di conteggi
   type(error_type), intent(out)                :: error
   real                                         :: tmin,tstep,tmax
   type(point_type), allocatable                :: datic(:)              
   integer                                      :: nc
   character(len=80)                            :: line,line1
   integer                                      :: nlong1,nlong2
   character(len=5)                             :: form = '(a80)'
   integer                                      :: ierror
   integer                                      :: n
   integer                                      :: nini,nfin
   integer                                      :: i
   integer                                      :: typef
   integer                                      :: ndatap
!
   call error%reset()
!
!  Cerca la linea con la stringa 'BANK' 
   do 
      read(unit,form,iostat=ierror) line
      if (ierror < 0) then
          call error%set("It doesn't seem GSAS file")
          go to 10
      endif
      call s_filter(line)  !filtra linea
      if (line(1:4) == 'BANK') then
          call cutst(line,nlong1,line1,nlong2)    ! cut 'BANK'
          call cutst(line,nlong1,line1,nlong2)    ! cut IBANK
          call cutst(line,nlong1,line1,nlong2)    ! leggi il numero di punti
          call s_to_i(line1,nc,ierror,nlong2)
          call cutst(line,nlong1,line1,nlong2)    ! cut NREC
          call cutst(line,nlong1,line1,nlong2)    ! leggi BYNTYP
          if (trim(line1) /= 'CONST') then
              call error%set('Cannot read GSAS file with BYNTYP='//trim(line1))
              go to 10
          endif
          call cutst(line,nlong1,line1,nlong2)    ! leggi BCOEF(1) = thmin
          ierror = s_to_r(line1,tmin,nlong2)
          tmin = tmin*0.01
          call cutst(line,nlong1,line1,nlong2)    ! leggi BCOEF(2) = thstep
          ierror = s_to_r(line1,tstep,nlong2)
          tstep = tstep*0.01
          call cutst(line,nlong1,line1,nlong2)    ! cut BCOEF(3)
          call cutst(line,nlong1,line1,nlong2)    ! cut BCOEF(4) 
          call cutst(line,nlong1,line1,nlong2)    ! leggi type
          select case (line1)
            case ('STD')
              ndatap = 10
              typef = 1
            case ('ESD')
              ndatap = 5
              typef = 2
            case ('ALT','FXY','FXYE')
              call error%set('Cannot read GSAS file with keyword TYPE='//trim(line1))
              go to 10
            case default
              ndatap = 10
              typef = 1
          end select
          exit
      endif
   enddo
!
!  Alloca conteggi ad ncounts
   allocate(datic(nc))
!
!  Leggi conteggi
   nfin = 0
   n = 0
   do
      read(unit,form,iostat=ierror) line
      if (ierror < 0) then
          nc = nfin  ! potrebbe aver letto meno conteggi degli nc previsti
          exit
      endif
      call s_filter(line)   
      nini = nfin + 1
      nfin = nini + ndatap - 1
      if (nfin > nc) nfin = nc !se thmax non coincide con l'ultimo conteggio
      select case (typef)
         case (1)     ! 'STD' or blank
           read(line,'(10(2x,f6.0))') datic(nini:nfin)%y

         case (2)     ! 'ESD'
           read(line,'(10(f8.0,8x))') datic(nini:nfin)%y
      end select
      if (nfin == nc) exit
   enddo
   datic%x = tmin + (/(n*tstep,n=0,nc-1)/)
   tmax = datic(nc)%x
!
!  In assenza di errore puoi aggiornare tmino,tstepo,tmaxo,xco,yco,nco 
   nco = nc
   tmino = tmin
   tmaxo = tmax
   tstepo = tstep
   call new_points(dati,nc)
   do i=1,nco
      dati(i) = datic(i)
   enddo   

   return

10 continue 
!
   end subroutine readgsas

  !---------------------------------------------------------------------------------------------

   subroutine readuxd(aunit,filename,tmin,tstep,tmax,dati,nwave,wave,ratio,err)
   USE pointmod
   USE fileutil
   USE strutil
   USE arrayutil
   USE pointmod
   integer, intent(in)                          :: aunit
   character(len=*), intent(in)                 :: filename
   real, intent(out)                            :: tmin,tstep,tmax
   type(point_type), allocatable, intent(inout) :: dati(:)
   integer, intent(out)                         :: nwave
   real, dimension(3), intent(out)              :: wave,ratio
   type(error_type), intent(out)                :: err
   character(len=:), allocatable                :: line
   real                                         :: rnum
   integer                                      :: ier
   real, dimension(:), allocatable              :: xdata
   integer                                      :: ndata,i
!
   nwave = 0
   do while(get_line(aunit,line,trimmed=.true.))
      call s_filter(line)
      if (len(line) == 0) cycle
      if (match_word(line,'_WL1')) then
          call get_next_number(line,'=',rnum,ier)
          if (ier /= 0) go to 10
          nwave = nwave + 1
          wave(1) = rnum
          ratio(1) = 1.0
      elseif (match_word(line,'_WL2')) then
          call get_next_number(line,'=',rnum,ier)
          if (ier /= 0) go to 10
          nwave = nwave + 1
          wave(2) = rnum
          ratio(2) = 0.5
      elseif (match_word(line,'_WL3')) then
          call get_next_number(line,'=',rnum,ier)
          if (ier /= 0) go to 10
          nwave = nwave + 1
          wave(3) = rnum
          ratio(3) = 0.2
      elseif (match_word(line,'_WLRATIO')) then
          call get_next_number(line,'=',rnum,ier)
          if (ier /= 0) go to 10
          ratio =  rnum
      elseif (match_word(line,'_START')) then
          call get_next_number(line,'=',rnum,ier)
          if (ier /= 0) go to 10
          tmin = rnum
      elseif (match_word(line,'_STEPSIZE')) then
          call get_next_number(line,'=',rnum,ier)
          if (ier /= 0) go to 10
          tstep = rnum
      elseif (match_word(line,'_COUNTS')) then
          call read_data_block(aunit,xdata)
          exit
      endif
   enddo
   ndata = size_array(xdata)
   if (ndata == 0) go to 10
!
!  Assign counts
   call resize_points(dati,ndata)
   dati%y = xdata
   dati(:)%x = tmin + (/(i*tstep,i=0,ndata-1)/)
   tmax = dati(ndata)%x

   return
!
10 call err%set('Error reading file: '//char(10)//trim(filename))
!
   end subroutine readuxd

  !---------------------------------------------------------------------------------------------

   subroutine readcpi(aunit,filename,tmin,tstep,tmax,dati,nwave,wave,ratio,err)
!
!  Read Sietronics Sieray CPI Files (*.cpi)
!  Format example:
!  SIETRONICS XRD SCAN
!  0.15
!  154.950
!  0.050
!  Cu
!  1.54056
!  01-28-2008
!  1
!  La/W/O
!  SCANDATA
!  0
!  41
!  49
!  50
!  61
!  52
!  50
!  50
!  68
!  ............. 
!
   USE fileutil
   USE strutil
   USE arrayutil
   USE pointmod
   integer, intent(in)                          :: aunit
   character(len=*), intent(in)                 :: filename
   real, intent(out)                            :: tmin,tstep,tmax
   type(point_type), allocatable, intent(inout) :: dati(:)
   integer, intent(out)                         :: nwave
   real, dimension(3), intent(out)              :: wave,ratio
   type(error_type), intent(out)                :: err
   character(len=:), allocatable                :: line
   integer                                      :: nline
   integer                                      :: ier,i
   real, dimension(:), allocatable              :: xdata
   integer                                      :: ndata
   logical                                      :: okline
!
   nwave = 0
   okline = get_line(aunit,line,trimmed=.true.)
   okline = get_line(aunit,line,trimmed=.true.)  ! line 2: tmin
   if (s_to_r(line,tmin) /= 0) go to 10 
   okline = get_line(aunit,line,trimmed=.true.)  ! line 3: tmax  (ignored)
   okline = get_line(aunit,line,trimmed=.true.)  ! line 4: tstep
   if (s_to_r(line,tstep) /= 0) go to 10
   okline = get_line(aunit,line,trimmed=.true.)  ! line 5
   okline = get_line(aunit,line,trimmed=.true.)  ! line 6: wavelenght
   if (s_to_r(line,wave(1)) /= 0) go to 10
   nwave = 1
   ratio(1) = 1.0
!
!  ignore the next lines and go to data block
   line = 'SCANDATA' ! allocate line
   call find_key_file(aunit,'SCANDATA',nline,line,ier)
   if (ier /= 0) go to 10
   call read_data_block(aunit,xdata)
!
   ndata = size_array(xdata)
   if (ndata == 0) go to 10
!
!  Assign counts
   call resize_points(dati,ndata)
   dati%y = xdata
   dati(:)%x = tmin + (/(i*tstep,i=0,ndata-1)/)
   tmax = dati(ndata)%x
   return
!
10 call err%set('Error reading file: '//char(10)//trim(filename))
!
   end subroutine readcpi

  !---------------------------------------------------------------------------------------------

   subroutine skip_c_comments(aunit)
   USE fileutil
   integer, intent(in)             :: aunit
   character(len=:), allocatable   :: line
   logical                         :: okline
   okline = get_line(aunit,line,trimmed=.true.)
   if (.not. okline .or. len(line) < 2) return
   if (line(1:2) /= '/*') return
   do  while(get_line(aunit,line,trimmed=.true.))
       if (len(line) >=2) then
           if (line(len(line)-1:len(line)) == '*/') return
       endif
   enddo
   end subroutine skip_c_comments

  !---------------------------------------------------------------------------------------------

   subroutine readxdd(aunit,filename,tmin,tstep,tmax,dati,err)
   USE pointmod
   USE errormod
   USE arrayutil
   integer, intent(in)                          :: aunit
   character(len=*), intent(in)                 :: filename
   real, intent(out)                            :: tmin,tstep,tmax
   type(point_type), allocatable, intent(inout) :: dati(:)
   type(error_type), intent(out)                :: err
   integer                                      :: ncounts
   real, dimension(:), allocatable              :: xdata
   integer                                      :: ndata,i
!
   call skip_c_comments(aunit)
   err = read_start_step_stop(aunit,1,tmin,tstep,tmax,ncounts)
   if (err%signal) return
!
   call read_data_block(aunit,xdata)
!
   ndata = size_array(xdata)
   if (ndata == 0) then
       call err%set('Error reading file: '//char(10)//trim(filename))
       return
   endif
!
!  Assign counts
   call resize_points(dati,ndata)
   dati%y = xdata
   dati(:)%x = tmin + (/(i*tstep,i=0,ndata-1)/)
   tmax = dati(ndata)%x
!
   end subroutine readxdd

  !---------------------------------------------------------------------------------------------

   subroutine readxda(aunit,filename,tmin,tstep,tmax,dati,err)
   USE pointmod
   USE errormod
   USE arrayutil
   USE fileutil
   integer, intent(in)                          :: aunit
   character(len=*), intent(in)                 :: filename
   real, intent(out)                            :: tmin,tstep,tmax
   type(point_type), allocatable, intent(inout) :: dati(:)
   type(error_type), intent(out)                :: err
   integer                                      :: ncounts
   real, dimension(:), allocatable              :: xdata
   integer                                      :: ndata,i
!
   if (ignore_lines(aunit,2) /= 0) go to 10
!
   err = read_start_step_stop(aunit,2,tmin,tstep,tmax,ncounts)
   if (err%signal) return
!
   call read_data_block(aunit,xdata)
!
   ndata = size_array(xdata)
   if (ndata == 0) go to 10
!
!  Assign counts
   call resize_points(dati,ndata)
   dati%y = xdata
   dati(:)%x = tmin + (/(i*tstep,i=0,ndata-1)/)
   tmax = dati(ndata)%x
   return
!
10 call err%set('Error reading file: '//char(10)//trim(filename))
!
   end subroutine readxda

  !---------------------------------------------------------------------------------------------

   subroutine readudf(aunit,filename,tmin,tstep,tmax,dati,nwave,wave,ratio,err)
   USE pointmod
   USE errormod
   USE arrayutil
   USE fileutil
   USE strutil
   integer, intent(in)                          :: aunit
   character(len=*), intent(in)                 :: filename
   real, intent(out)                            :: tmin,tstep,tmax
   type(point_type), allocatable, intent(inout) :: dati(:)
   integer, intent(out)                         :: nwave
   real, dimension(3), intent(out)              :: wave,ratio
   type(error_type), intent(out)                :: err
   real, dimension(:), allocatable              :: xdata
   integer                                      :: ndata,i
   character(len=:), allocatable                :: line,key
   real, dimension(:), allocatable              :: vet
   integer                                      :: iv
!
   tmin = 0
   tstep = 0
!   wave = -1
   nwave = 0
   do while(get_line(aunit,line,trimmed=.true.))
      call s_filter(line)
      if (len(line) == 0) cycle
!
      if (index(line,'RawScan') /= 0) exit
!
      call s_ch_blank(line,',')
      if (len(line) == 0) cycle
      call Cutsta(line,line2=key)

      if (index(key,'LabdaAlpha1') /= 0) then
          call getnum1(line,vet,iv=iv)
          if (iv < 2) go to 10
          nwave = 1
          wave(1) = vet(1)
          ratio(1) = 1.0
      elseif (index(key,'DataAngleRange') /= 0) then
          call getnum1(line,vet,iv=iv)
          if (iv < 2) go to 10
          tmin = vet(1)
      elseif (index(key,'ScanStepSize') /= 0) then
          call getnum1(line,vet,iv=iv)
          if (iv == 0) go to 10
          tstep = vet(1)
      endif
!
   enddo
!
   call read_data_block(aunit,xdata,',')
!
   ndata = size_array(xdata)
   if (ndata == 0)  go to 10
!
!  Assign counts
   call resize_points(dati,ndata)
   dati%y = xdata
   dati(:)%x = tmin + (/(i*tstep,i=0,ndata-1)/)
   tmax = dati(ndata)%x
   return
!
10 call err%set('Error reading file: '//char(10)//trim(filename))
!
   end subroutine readudf

  !---------------------------------------------------------------------------------------------

   subroutine readxrdml(aunit,filename,tmin,tstep,tmax,dati,nwave,wave,ratio,err)
   USE pointmod
   USE errormod
   USE arrayutil
   USE fileutil
   USE strutil
   integer, intent(in)                          :: aunit
   character(len=*), intent(in)                 :: filename
   real, intent(out)                            :: tmin,tstep,tmax
   type(point_type), allocatable, intent(inout) :: dati(:)
   integer, intent(out)                         :: nwave
   real, dimension(3), intent(out)              :: wave,ratio
   type(error_type), intent(out)                :: err
   real, dimension(:), allocatable              :: xdata
   integer                                      :: ndata,i,ninfo
   character(len=:), allocatable                :: line
   logical                                      :: datatag_open
   character(len=:), allocatable, dimension(:)  :: taginfo
   real                                         :: wavel
   
   datatag_open = .false.
   tmin = 0.
   tmax = 0.
   tstep = 0.
   !corrwave = -1
   nwave = 0
   do while(get_line(aunit,line,trimmed=.true.))
      call s_filter(line)
      if (len(line) == 0) cycle
      if (datatag_open) then
          if(is_xml_tag(line,'/dataPoints')) exit  ! tag dataPoints closed
          if (is_xml_tag(line,'positions')) then
              call get_xml_tag_info(line,'positions',taginfo,ninfo)
              do i=1,ninfo
                 if (index(taginfo(i),'2Theta') > 0) then
                     if (get_line(aunit,line,trimmed=.true.,filter=.true.)) call read_xml_data(line,tmin)   ! tmin expected
                     if (get_line(aunit,line,trimmed=.true.,filter=.true.)) call read_xml_data(line,tmax)   ! tmax expected
                     cycle
                 endif
              enddo
          elseif (is_xml_tag(line,'intensities')) then
              call read_xml_data_block(line,xdata)
              exit
          endif
      else
          datatag_open = is_xml_tag(line,'dataPoints')
!
          call get_xml_tag_info(line,'usedWavelength',taginfo,ninfo)
          if (ninfo > 0) then
              if (get_line(aunit,line,trimmed=.true.,filter=.true.)) then
                  call read_xml_data(line,wavel)  ! read kAlpha1
                  if (wavel > 0) then
                      nwave = 1
                      wave(1) = wavel
                      ratio(1) = 1.0
                  endif
              endif
          endif
      endif
   enddo
!
   ndata = size_array(xdata)
   if (ndata == 0)  go to 10
!
!  Assign counts
   call resize_points(dati,ndata)
   dati%y = xdata
   tstep = (tmax - tmin)/(size_array(xdata) - 1)
   dati(:)%x = tmin + (/(i*tstep,i=0,ndata-1)/)
   tmax = dati(ndata)%x
   return

10 call err%set('Error reading file: '//char(10)//trim(filename))
!
   end subroutine readxrdml

  !---------------------------------------------------------------------------------------------

   subroutine read_xml_data_block(line,xdata)
!
!  Read data block in record line containting init (< ...>) tag and the end tag (</ >)
!
   USE strutil
   USE arrayutil
   character(len=*), intent(in)                 :: line
   real, dimension(:), allocatable, intent(out) :: xdata
   integer                                      :: lent,kinit,kend,iv
!
   lent = len_trim(line) 
   if (lent == 0) return  
!
   kinit = index(line,'>')
   if (kinit > 0 .and. kinit /= lent) then
       kend = index(line(kinit+1:),'</')
       if (kend > 0) then
           call getnum1(line(kinit+1:kinit+kend-1),xdata,iv=iv)
           call resize_array(xdata,iv)
       endif
   endif
!
   end subroutine read_xml_data_block

  !---------------------------------------------------------------------------------------------

   subroutine read_xml_data(line,xdata)
!
!  Read data in record line containting init (< ...>) tag and the end tag (</ >)
!
   USE strutil
   USE arrayutil
   character(len=*), intent(in) :: line
   real, intent(out)            :: xdata
   integer                      :: lent,kinit,kend
   integer                      :: ier
!
   lent = len_trim(line) 
   if (lent == 0) return  
!
   kinit = index(line,'>')
   if (kinit > 0 .and. kinit /= lent) then
       kend = index(line(kinit+1:),'</')
       if (kend > 0) then
           ier = s_to_r(line(kinit+1:kinit+kend-1),xdata)
       endif
   endif
!
   end subroutine read_xml_data

  !---------------------------------------------------------------------------------------------

   logical function is_xml_tag(line,tag,posbr) result(is_tag)
   character(len=*), intent(in)                 :: line
   character(len=*), intent(in)                 :: tag
   integer, dimension(2), intent(out), optional :: posbr
   integer                                      :: lenl,lent,kpos
   integer, dimension(2)                        :: posb
!   
   is_tag = .false.
   if (present(posbr)) posbr = 0
   lenl = len_trim(line)
   lent = len(tag)
   if (lent == 0 .or. lenl == 0)  return
!
   kpos = index(line,tag)
   if (kpos > 0 .and. kpos+lent-1 < lenl) then
!
!      check if tag is in brackets
       posb(1) = index(line(:kpos),'<')  ! open bracket
       if (posb(1) > 0)  then  ! open bracket
           posb(2) = index(line(kpos+lent:),'>')
           if (posb(2) > 0) then
               is_tag = .true.
               if (present(posbr)) then
                   posbr(1) = kpos + lent
                   posbr(2) = posb(2) + kpos + lent-2
               endif
           endif
       endif 
   endif
!
   end function is_xml_tag

  !---------------------------------------------------------------------------------------------

   subroutine get_xml_tag_info(line,tag,info,ninfo)
   USE strutil
   character(len=*), intent(in)                 :: line
   character(len=*), intent(in)                 :: tag
   character(len=:), allocatable, dimension(:), intent(inout) :: info
   integer, intent(out)                          :: ninfo
   integer, dimension(2) :: posb
!
   ninfo = 0
   if (is_xml_tag(line,tag,posb)) then
       call get_words_quotes_a(line(posb(1):posb(2)),info,ninfo,'"')
   endif
!
   end subroutine get_xml_tag_info

  !---------------------------------------------------------------------------------------------

   subroutine read_data_block(aunit,xdata,sep)
   USE fileutil
   USE arrayutil
   USE strutil
   integer, intent(in)                    :: aunit
   real, dimension(:), allocatable        :: xdata
   character(len=*), intent(in), optional :: sep
   integer, parameter                     :: INITDIM = 10000
   integer                                :: ndata
   character(len=:), allocatable          :: line
   real, dimension(:), allocatable        :: vet
   integer                                :: iv
   integer                                :: istart,iend
   logical                                :: lsep
!
   ndata = 0
   istart = 0
   iend = 0
   call new_array(xdata,INITDIM)
!
   if (present(sep)) then
       lsep = len_trim(sep) > 0
   else
       lsep = .false.
   endif
   if (lsep) then
       do while(get_line(aunit,line,trimmed=.true.))
          call s_filter(line)
          if (len(line) == 0) cycle
          line = s_replace_by_blanks(line,sep)
          if (len(line) == 0) cycle
          call getnum1(line,vet,iv=iv)
          if (iv == 0 .or. err_string) exit
          istart = iend + 1
          iend = istart + iv - 1
          if (iend > size(xdata)) then
              call resize_array(xdata,size(xdata) + INITDIM)
          endif
          xdata(istart:iend) = vet(:iv)
       enddo
   else
       do while(get_line(aunit,line,trimmed=.true.))
          call s_filter(line)
          if (len(line) == 0) cycle
          call getnum1(line,vet,iv=iv)
          if (iv == 0 .or. err_string) exit
          istart = iend + 1
          iend = istart + iv - 1
          if (iend > size(xdata)) then
              call resize_array(xdata,size(xdata) + INITDIM)
          endif
          xdata(istart:iend) = vet(:iv)
       enddo
   endif
   call resize_array(xdata,iend)
!
   end subroutine read_data_block

  !---------------------------------------------------------------------------------------------

   subroutine readcif(unit,tmino,tstepo,tmaxo,dati,nco,nwave,wave,ratio,count_type,error)
!
!  Legge un file con due colonne (2theta e conteggi)   
!  Se si genera errore le variabili tmin,tstep,tmax,xco,yco,nc non vengono modificate. 
! 
   USE strutil
   USE cif_frm
   USE pointmod
   integer, intent(in)                           :: unit
   real, intent(out)                             :: tmino,tstepo,tmaxo
   type(point_type), allocatable, intent(inout)  :: dati(:)
   integer, intent(out)                          :: nco
   integer, intent(out)                          :: nwave
   real, dimension(3), intent(out)               :: wave,ratio
   integer, intent(out)                          :: count_type ! 2theta or d
   type(error_type), intent(out)                 :: error
   type(point_type), allocatable                 :: datic(:)
   integer                                       :: nc
   integer                                       :: i
   integer, parameter                            :: NW=10
   integer, parameter                            :: NAVET = 1000
   type(cifword_type), dimension(:), allocatable :: cifword
   real                                          :: range_min,range_max,range_inc
   integer                                       :: n
   logical                                       :: countx_fill,county_fill
   logical                                       :: newdata
   integer                                       :: nline 
   character(len=100)                            :: dataname
!
   call error%reset()
!
   call load_cif_dictionary(cifword)
!
   count_type = COUNT_2THETA_TYPE
   nline = 0
   loop_data: do 
      call cifword_fill(cifword,unit,newdata,nline,dataname,error)
!     
      nc = 0
      range_min = -1
      range_max = -1
      range_inc = -1
      county_fill = .false.
      countx_fill = .false.
      nwave = 0
!     
      do i=size(cifword),1,-1
         if (cifword(i)%wok) then
             select case (trim(cifword(i)%str))
                case ('_pd_meas_2theta_scan','_pd_meas_2theta_fixed','_pd_proc_2theta_corrected',  &
                      '_pd_proc_2theta_fixed','_pd_meas_2theta_corrected')
                  if (cifkey_is_ok(cifword(i))) then
                      if (.not.countx_fill) then
                          nc = cifword(i)%nv
                          if(.not.allocated(datic))allocate(datic(nc))
                          datic(:)%x = cifword(i)%vet(:nc)
                          countx_fill = .true.
                      endif 
                  endif
                case ('_pd_proc_d_spacing')
                  if (cifkey_is_ok(cifword(i))) then
                      if (.not.countx_fill) then
                          nc = cifword(i)%nv
                          if(.not.allocated(datic))allocate(datic(nc))
                          datic(:)%x = cifword(i)%vet(:nc)
                          countx_fill = .true.
                          count_type = COUNT_D_TYPE
                      endif
                  endif
                case ('_pd_meas_intensity_total','_pd_proc_intensity_total','_pd_meas_counts_total','_pd_proc_intensity_net')
                  if (cifkey_is_ok(cifword(i))) then
                      if (.not.county_fill) then
!     
!                         riempio le y solo una volta 
                          county_fill = .true.
                          nc = cifword(i)%nv
                          if(.not.allocated(datic))allocate(datic(nc))
                          datic(:)%y = cifword(i)%vet(:nc)
                      endif
                  endif
                case ('_pd_meas_2theta_range_min')
                  range_min = cifword(i)%vet(1)
                case ('_pd_meas_2theta_range_max')
                  range_max = cifword(i)%vet(1)
                case ('_pd_meas_2theta_range_inc')
                  range_inc = cifword(i)%vet(1)
                case ('_diffrn_radiation_wavelength')
                  nwave = 1
                  wave(1) = cifword(i)%vet(1)
                  ratio(1) = 1.0
             end select
         endif
      enddo
!     
!     ora in assenza di errore puoi trasferire i conteggi
      if (nc > 0 .and. county_fill) then
          if(countx_fill) then
             if (range_inc < 0) range_inc = sum(datic(2:nc)%x - datic(1:nc-1)%x)/(nc-1)
          else
             if (range_min < 0 .and. range_inc < 0) then
                 call error%set('Undefined range in pdCIF file')
                 go to 10
             endif
             datic%x = range_min + (/(n*range_inc,n=0,nc-1)/)
          endif
          tstepo = range_inc
          nco = nc
          tmino = datic(1)%x
          tmaxo = datic(nc)%x   
          call new_points(dati,nc)
          do i=1,nc
             dati(i) = datic(i)
          enddo
      else
          call error%set('Cannot find intensity measurements in pdCIF file')
      endif
!
!     Se non c'e' un ulteriore data block da leggere esci dal loop
      if (.not.newdata) exit loop_data

   enddo loop_data
!
   return
!
10 continue 
!
   end subroutine readcif

  !---------------------------------------------------------------------------------------------

   subroutine read_reflection_file(filename,refl,numref,stype,error)
   USE reflection_type_util
   character(len=*), intent(in)                        :: filename
   type(reflection_type), dimension(:), allocatable    :: refl
   integer, intent(out)                                :: numref
   character(len=*), intent(in)                        :: stype
   type(error_type), intent(out)                       :: error

   select case(stype)
      case ('hkl','shelx','fc')
        call read_reflection_file_hkl(filename,refl,numref,stype,error)
      case ('cif')
        call read_reflection_file_cif(filename,refl,numref,error)
      case default
        call read_reflection_file_hkl(filename,refl,numref,stype,error)
   end select

   end subroutine read_reflection_file

  !---------------------------------------------------------------------------------------------

   subroutine read_reflection_file_hkl(filename,refl,numref,stype,error)
!
!  Legge un file di riflessi con nelle prime 3 colonne gli indici h,k,l
!  fo e fc conterranno rispettivamente fo and sigma
!
   USE strutil
   USE fileutil
   USE reflection_type_util
   character(len=*), intent(in)                        :: filename
   type(reflection_type), dimension(:), allocatable    :: refl
   integer, intent(out)                                :: numref
   type(error_type), intent(out)                       :: error
   character(len=*), intent(in)                        :: stype
   integer                                             :: ierror
   integer                                             :: nline
   character(len=250)                                  :: line
   real, dimension(40)                                 :: vet
   integer, dimension(40)                              :: ivet
   integer                                             :: iv
   integer                                             :: i,j
   character(len=20)                                   :: strn
   type(file_handle)                                   :: fileh
   integer                                             :: ierl
   character(len=:), dimension(:), allocatable         :: wordv
!
   call error%reset()
   call fileh%fopen(filename,'r')
   if (fileh%good()) then
!
       call count_line(fileh%handle(),nline,ierror)
       if (ierror > 0 .or. nline == 0) go to 10
!
       call resize_reflections(refl,nline)
!
       numref = 0
       do i=1,nline
          read(fileh%handle(),'(a)') line
          call s_filter(line)  !filtra linea
          if (len_trim(line) == 0) cycle
          if (is_comment_line(line)) cycle
          select case (stype)
             case ('shelx')
               read(line,'(3i4,2f8.2)',iostat=ierl)ivet(1:3),vet(4),vet(5)
             case ('fc')
               read(line,'(3i4,3f8.2)',iostat=ierl)ivet(1:3),vet(4),vet(5),vet(6)
               if (ierl == 0) refl(numref+1)%ph = nint(vet(6))
             case default
               call get_words1(line,wordv,iv)
               !call getnum(line,vet,ivet,iv)
               if (iv < 4) then
                   ierl = 1
               else
!
!                  First 3 numbers (h,k,l) should be integer
                   do j=1,3
                      !call s_to_i(wordv(j),ivet(j),ierl)
                      !if (ierl /= 0) exit
                      !write(72,*)'IVET: ',i,j,wordv(j),ivet(j)
                      if (.not.s_is_i(wordv(j),ivet(j))) then
                          ierl = 1
                          exit
                      endif
                   enddo
!
!                  get intensity and sigma
                   if (ierl == 0) then
                       ierl = s_to_r(wordv(4),vet(4))
                       if (ierl == 0) then
                           if (iv == 4) then
                               vet(5) = 0.0  ! sigma(Fo) is absent
                           else
                               ierl = s_to_r(wordv(5),vet(5))
                           endif
                       endif
                   endif
               endif
          end select
          if (ierl /= 0) then
              write(strn,'(i0)')i
              call error%set('Error reading row '//trim(adjustl(strn))//    &
                             ' in reflection file '//trim(filename)//char(10)//trim(line))
              call fileh%fclose()
              return
          endif
          numref = numref + 1
          refl(numref)%hkl = ivet(1:3)
          refl(numref)%fo = vet(4)
          refl(numref)%fc = vet(5)  ! sigma(Fo)
          !write(72,'(4i4,2f8.2)')numref,ivet(1:3),vet(4),vet(5)
       enddo
       call resize_reflections(refl,numref)
   else
       !call set_file_error(filename,fileh%err(),error)
       call error%set(fileh%err_msg())
   endif
   call fileh%fclose()
!
   return
!
10 call error%set('Error reading from reflection file '//trim(filename))
   call fileh%fclose()
!
   end subroutine read_reflection_file_hkl

  !---------------------------------------------------------------------------------------------

   subroutine read_reflection_file_cif(filename,refl,numref,error)
   USE reflection_type_util
   USE cif_frm
   USE fileutil
   character(len=*), intent(in)                     :: filename
   type(reflection_type), dimension(:), allocatable :: refl
   integer, intent(out)                             :: numref
   type(error_type), intent(out)                    :: error
   type(file_handle)                                :: fileh
   type(cifword_type), dimension(:), allocatable    :: cifword
   logical                                          :: newdata
   integer                                          :: nline,i,nc
   character(len=100)                               :: dataname
   type(reflection_type), dimension(:), allocatable :: dataref
   logical, dimension(4)                            :: okfield
   logical                                          :: oksigma
!
   numref = 0

   call fileh%fopen(filename,'r')
   if (fileh%fail()) then
       call error%set(fileh%err_msg())
       return
   endif

   call load_cif_dictionary_hkl(cifword)
   
   nline = 0
   call cifword_fill(cifword,fileh%handle(),newdata,nline,dataname,error)

   okfield = .false.
   oksigma = .false.
   do i=1,size(cifword)
      if (cifword(i)%wok) then
          select case (trim(cifword(i)%str))
             case ('_refln_index_h','_refln_index_k','_refln_index_l',  &
                   '_refln_F_squared_meas','_refln_F_squared_sigma',    &
                   '_refln_F_meas','_refln_F_sigma')   
               if (cifkey_is_ok(cifword(i))) then
                   nc = cifword(i)%nv
                   if (numrefl(dataref) == 0) call new_reflections(dataref,nc)
                   select case (trim(cifword(i)%str))
                      case ('_refln_index_h')
                        dataref(:)%hkl(1) = cifword(i)%vet(:nc)
                        okfield(1) = .true.
                      case ('_refln_index_k')
                        dataref(:)%hkl(2) = cifword(i)%vet(:nc)
                        okfield(2) = .true.
                      case ('_refln_index_l')
                        dataref(:)%hkl(3) = cifword(i)%vet(:nc)
                        okfield(3) = .true.
                      case ('_refln_F_squared_meas')
                        dataref(:)%fo = cifword(i)%vet(:nc)
                        okfield(4) = .true.
                      case ('_refln_F_meas')
                        dataref(:)%fo = cifword(i)%vet(:nc)**2
                        okfield(4) = .true.
                      case ('_refln_F_squared_sigma')
                        dataref(:)%fc = cifword(i)%vet(:nc)
                        oksigma = .true.
                      case ('_refln_F_sigma')
                        dataref(:)%fc = cifword(i)%vet(:nc)**2
                        oksigma = .true.
                   end select
               endif
          end select
      endif
   enddo

   if (all(okfield)) then
       if (.not.oksigma) dataref(:)%fc = 0.0
       call copy_ref(refl,dataref)
       numref = numrefl(dataref)
   else
       call error%set('Error reading from reflection file '//trim(filename))
   endif

   call fileh%fclose()
!
   end subroutine read_reflection_file_cif

  !---------------------------------------------------------------------------------------------
   
   subroutine count_line(unit,nline,ierror)
!
!  Conta il numero di linee in un file 
!
   integer, intent(in)  :: unit
   integer, intent(out) :: nline
   integer, intent(out) :: ierror ! > 0 se si verifica errore
!
   nline = 0
   ierror = 0
   do
      read(unit,*,iostat=ierror)
      if (ierror /= 0) exit                   ! fine file o errore
      nline = nline + 1
   enddo
   if (ierror < 0) ierror = 0
   rewind unit
!
   end subroutine count_line

  !---------------------------------------------------------------------------------------------

   subroutine write_column(filename,jfile,xcol1,xcol2,metad)
!
!  Scrive su file in colonne
!
   USE fileutil
   character(len=*), intent(in), optional  :: filename
   integer, intent(in), optional           :: jfile
   real, dimension(:), intent(in)          :: xcol1
   real, dimension(:), intent(in),optional :: xcol2
   character(len=*), intent(in), optional  :: metad
   integer                                 :: j_in
   integer                                 :: i
   integer                                 :: ier
   type(file_handle)                       :: fcol
!
!  Apertura file
   if (present(filename)) then
       call fcol%fopen(filename,'w')
       j_in = fcol%handle()
       ier = fcol%err()
   else
       j_in = jfile
       ier = 0
   endif
!
!  Scrittura su file
   if (ier == 0) then
       if (present(metad)) write(j_in,'(a)')trim(metad)
       if (present(xcol2)) then
           write(j_in,'(f0.6,1x,f0.6)')(xcol1(i),xcol2(i),i=1,min(size(xcol1),size(xcol2)))
       else
           write(j_in,'(f0.6)')(xcol1(i),i=1,size(xcol1))
       endif
       close(j_in)
   endif
!
   end subroutine write_column

  !---------------------------------------------------------------------------------------------

   subroutine read_column(filename,xcol,ncol,error)
!
!  Legge un file su colonne
!
   USE fileutil
   USE strutil
   USE arrayutil
   character(len=*), intent(in)                   :: filename
   real, dimension(:), allocatable, intent(inout) :: xcol
   integer, intent(out)                           :: ncol
   type(error_type), intent(out)                  :: error
   integer                                        :: ncol0
   integer                                        :: ierror
   character(len=250)                             :: line
   character(len=20)                              :: strn
   real, dimension(40)                            :: vet
   integer, dimension(40)                         :: ivet
   integer                                        :: iv
   integer                                        :: i
   type(file_handle)                              :: fcol
!
   ncol = 0
!
!  Apertura file
   call fcol%fopen(filename,'r')
!
   if (fcol%good()) then
       call count_line(fcol%handle(),ncol0,ierror)   ! conta le linee nel file
       if (ierror > 0 .or. ncol0 == 0) then
           call error%set('Error reading from file '//trim(filename))
           call fcol%fclose()
           return
       endif
!
       call new_array(xcol,ncol0)  ! prealloca xcol al num. di linee
!
       do i=1,ncol0
          read(fcol%handle(),'(a)') line
          call s_filter(line)  !filtra linea
          if (is_comment_line(line)) cycle
          call getnum(line,vet,ivet,iv)
          if (iv == 0) then
              write(strn,'(i0)')i
              call error%set('Error reading line '//trim(adjustl(strn))//' in '//trim(filename)//char(10)//trim(line))
              call fcol%fclose()
              return
          endif
          ncol = ncol + 1
          xcol(ncol) = vet(1)
       enddo
       call resize_array(xcol,ncol)
       call fcol%fclose()
   else
       call error%set('Cannot open: '//trim(filename)//char(10)//' Message: '//trim(fcol%err_msg()))
   endif
!
   end subroutine read_column

  !---------------------------------------------------------------------------------------------

   subroutine get_info_datafile(fname,tmin,tmax,wavel,ier)
   USE fileutil
   character(len=*), intent(in)                :: fname
   integer, intent(out)                        :: ier
   real, intent(out)                           :: tmin,tmax,wavel
   integer                                     :: count_type
   type(point_type), allocatable, dimension(:) :: countp
   real                                        :: tstep
   integer                                     :: ftype,nwave
   integer                                     :: ncount
   type(error_type)                            :: error
   real, dimension(3)                          :: wave,ratio
!
!  Open file
   ier = 0
   ftype =  filetype_from_filename(fname)
   call read_datafile(fname,ftype,tmin,                &
        tstep,tmax,countp,ncount,.false.,nwave,wave,ratio,count_type,error)
   if (error%signal) then
       ier = 1
       return
   endif
   if (nwave == 0) then
       wavel = 1.54056
   else
       wavel = wave(1)
   endif
!
   end subroutine get_info_datafile

  !---------------------------------------------------------------------------------------------

   subroutine export_profile(fname,x,yoss,ycalc,yback,string)
   USE fileutil
   character(len=*), intent(in), optional   :: fname
   real, dimension(:), intent(in)           :: x,yoss
   real, dimension(:), intent(in), optional :: ycalc,yback
   character(len=*), intent(in), optional   :: string
   integer                                  :: i
   integer                                  :: j_in
   type(file_handle)                        :: fpatt
   integer, dimension(4)                    :: vsize
!
   call fpatt%fopen(fname,'w')
!
   if (fpatt%good()) then
       j_in = fpatt%handle()
       if (present(string)) write(j_in,'(a)')trim(string)
       if (present(ycalc) .and. present(yback)) then
          vsize(:) = [size(x),size(yoss),size(ycalc),size(yback)]
          write(j_in,'(f15.3,1x,f15.3,f15.6,1x,f15.6,1x,f15.6)')   &
               (x(i),yoss(i),ycalc(i),yback(i),yoss(i)-ycalc(i),i=1,minval(vsize))
       else
          write(j_in,'(f15.3,1x,f15.3)')(x(i),yoss(i),i=1,size(x))
       endif
       call fpatt%fclose()
   endif
!
   end subroutine export_profile

END MODULE filereading
