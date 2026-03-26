module fra_frm

implicit none

contains

   subroutine read_fra_file(filename,atom,is_bfac,is_occ,error)
   USE strutil
   USE errormod
   USE atom_type_util
   USE elements
   USE fileutil
   character(len=*), intent(in)                              :: filename
   type(atom_type), allocatable, dimension(:), intent(inout) :: atom    ! atomi in uscita
   logical, intent(out)                                      :: is_bfac ! esiste il fattore termico?
   logical, intent(out)                                      :: is_occ  ! esiste l'occupanza?
   type(error_type), intent(out)                             :: error   ! errore in lettura
   integer                                                   :: natom
   integer                                                   :: ier
   integer                                                   :: nline
   character(len=500)                                        :: line,line2
   integer                                                   :: nlong1,nlong2
   real, dimension(50)                                       :: vet
   integer, dimension(50)                                    :: ivet
   integer                                                   :: iv
   integer                                                   :: i
   character(len=10)                                         :: strn
   type(file_handle)                                         :: ff
   integer                                                   :: j_in 
!
   call ff%fopen(filename)
   if (ff%fail()) then 
       call error%set('Cannot open: '//trim(filename)//char(10)//'Message: '//trim(ff%err_msg()))
       return
   endif
   j_in = ff%handle()
!
!  Conta gli atomi per allocare atom
   natom = 0
   do 
      read(j_in,'(a)',iostat=ier)line
      if (ier < 0) exit
      line = adjustl(line)
      if (len_trim(line) == 0) cycle
      if (line(1:1) /= '>' .and. line(1:1) /= '!') natom = natom + 1
   enddo
   if (natom == 0) then
       call error%set('Cannot read file '//trim(filename))
       go to 20
   else
       call new_atoms(atom,natom)
   endif
   rewind (j_in)
!
!  Leggi gli atomi
   nline = 0
   natom = 0
   is_bfac = .true.
   is_occ = .true.
   do
      nline = nline + 1
      read(j_in,'(a)',iostat=ier)line
      if (ier < 0) exit
      call s_filter(line)
      if (len_trim(line) == 0) cycle
      if (line(1:1) /= '>' .and. line(1:1) /= '!') then
!
!         Leggi simbolo chimico 
          call cutst(line,nlong1,line2,nlong2)
          natom = natom + 1
          atom(natom)%lab = trim(line2)
!
!         Leggi coordinate x,y,z,b,occ
          call Getnum(line,vet,ivet,iv)
          if (iv < 3) go to 10
          select case (iv)
             case (3)
              atom(natom)%xc = vet(1:3)
              is_bfac = .false.
              atom(natom)%och = 1.0
              is_occ = .false.
             case (4)
              atom(natom)%xc = vet(1:3)
              atom(natom)%biso = vet(4)
              atom(natom)%och = 1.0
              is_occ = .false.
             case (5:)
              atom(natom)%xc = vet(1:3)
              atom(natom)%biso = vet(4)
              atom(natom)%och = vet(5)
          end select
      endif
   enddo
!
!  Metti in nz il puntatore al file xen
   do i=1,natom
      atom(i)%ptab = pxen_from_specie(atom(i)%lab)
   enddo
!
   call ff%fclose()
   return

10 write(strn,'(i0)')nline
   call error%set('Error on reading line '//trim(strn)//' in file '//trim(filename))   
20 continue
   call ff%fclose()
!
   end subroutine read_fra_file

!-------------------------------------------------------------------------------------------------

   subroutine read_fractional(filename,cellc,is_cell,atom,is_bfac,is_occ,error)
   USE strutil
   USE errormod
   USE unit_cell
   USE atom_type_util
   USE elements
   USE fileutil
   character(len=*), intent(in)                              :: filename
   type(cell_type), intent(inout)                            :: cellc
   logical, intent(in)                                       :: is_cell ! esiste gia' una cella?
   type(atom_type), allocatable, dimension(:), intent(inout) :: atom    ! atomi in uscita
   logical, intent(out)                                      :: is_bfac ! esiste il fattore termico?
   logical, intent(out)                                      :: is_occ  ! esiste l'occupanza?
   type(error_type), intent(out)                             :: error   ! errore in lettura
   integer                                                   :: nline
   character(len=500)                                        :: line,line2
   integer                                                   :: nlong1,nlong2
   real, dimension(50)                                       :: vet
   integer, dimension(50)                                    :: ivet
   integer                                                   :: iv
   integer                                                   :: natom
   real, dimension(6)                                        :: cellfile
   integer                                                   :: i
   character(len=10)                                         :: strn
   integer                                                   :: ier
   type(file_handle)                                         :: ff
   integer                                                   :: j_in 
!
   call ff%fopen(filename)
   if (ff%fail()) then 
       call error%set('Cannot open: '//trim(filename)//char(10)//'Message: '//trim(ff%err_msg()))
       return
   endif
   j_in = ff%handle()
!
!  Conta gli atomi per allocare atom
   nline = 1
   read(j_in,*,err=10,end=10)
   nline = nline + 1
   read(j_in,*,err=10,end=10)
   natom = 0
   do 
      read(j_in,'(a)',iostat=ier)line
      if (ier < 0) exit
      call s_filter(line)
      if (len_trim(line) == 0) cycle   ! righe vuote
      natom = natom + 1
   enddo
   if (natom == 0) then
       call error%set('Cannot read this file')
       go to 20
   else
       allocate(atom(natom))
   endif
   rewind (j_in)
!
   nline = 1
   read(j_in,*,err=10)
!
!  Leggi la cella
   nline = nline + 1
   read(j_in,'(a)',err=10)line
   call s_filter(line)
   call Getnum(line,vet,ivet,iv)
   if (iv /= 6) go to 10
   cellfile = vet(1:6)
!
   natom = 0
   is_bfac = .true.
   is_occ = .true.
   do 
      nline = nline + 1
      read(j_in,'(a)',iostat=ier)line
      if (ier < 0) exit
      call s_filter(line)
      if (len_trim(line) == 0) cycle   ! gestione righe vuote
!
      call s_detag(line) ! elimina caratteri tra parentesi
!
!     Leggi simbolo chimico 
      call cutst(line,nlong1,line2,nlong2)
!corr-for ionic species      if (nlong2 > 2 .or. .not.s_is_alpha(line2(:nlong2))) go to 10   
      natom = natom + 1
      atom(natom)%lab = line2
!
!     Leggi coordinate frazionarie
      call Getnum(line,vet,ivet,iv)
      if (iv < 3) go to 10
      select case (iv)
         case (3)
          atom(natom)%xc = vet(1:3)
          is_bfac = .false.
          atom(natom)%och = 1.0
          is_occ = .false.
         case (4)
          atom(natom)%xc = vet(1:3)
          atom(natom)%biso = vet(4)
          atom(natom)%och = 1.0
          is_occ = .false.
         case (5:)
          atom(natom)%xc = vet(1:3)
          atom(natom)%biso = vet(4)
          atom(natom)%och = vet(5)
      end select
   enddo
!
   if (is_cell) then                                 ! metti nella cella corrente se esiste
       call frac_to_cart(atom,orthomatrix(cellfile))
       call cart_to_frac(atom,cellc%get_ortoi())
   else
       cellc = set_cell_type(cellfile(:))              ! altrimenti la cella corrente diventa quella letta dal file
   endif
!
!  Metti in nz il puntatore al file xen
   do i=1,natom
      atom(i)%ptab = pxen_from_specie(atom(i)%lab)
   enddo
!
   call ff%fclose()
   return
10 write(strn,'(i0)')nline
   call error%set('Error on reading line '//trim(strn)//' in file '//trim(filename))   
20 continue
   call ff%fclose()
!
   end subroutine read_fractional

!------------------------------------------------------------------------------------------

   subroutine create_frafile(filename,atom,cell,comment)
!
!  Crea un Free Form Fractional file 
!   
   USE fileutil
   USE atom_basic
   USE unit_cell
   character(len=*), intent(in)              :: filename
   type(atom_type), dimension(:), intent(in) :: atom
   type(cell_type), intent(in)               :: cell
   character(len=*), intent(in)              :: comment
   integer                                   :: i
   integer                                   :: j_in
   type(file_handle)                         :: ff
!
!  Apertura del file
   call ff%fopen(filename,'w')
   j_in = ff%handle()
!
!  Scrivi un commento sulla prima riga
   write(j_in,'(a)')trim(comment)
!
!  Scrivi la cella sulla seconda riga
   write(j_in,'(6(1x,f10.5))')cell%get_par()
!
!  Scrivi simbolo chimico e coordinate frazionarie sulle altre righe
   do i=1,size(atom)
      write(j_in,'(a2,3(1x,f10.5))')atom(i)%specie(),atom(i)%xc
   enddo          
!
   call ff%fclose()
!
   end subroutine create_frafile

end module fra_frm
