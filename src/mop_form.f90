module mop_frm

implicit none

contains

   subroutine read_mopcartfile(filename,cell,atom,error)
!
!  Read MOPAC Cartesian format
!   
   USE unit_cell
   USE atom_type_util
   USE strutil
   USE errormod
   USE elements
   USE fileutil
   character(len=*), intent(in)                              :: filename
   type(cell_type), intent(in)                               :: cell
   type(atom_type), allocatable, dimension(:), intent(inout) :: atom
   character(len=10)                                         :: strn
   integer                                                   :: i,ier
   type(error_type), intent(out)                             :: error 
   character(len=500)                                        :: line1,line2
   integer                                                   :: nlong1,nlong2
   integer                                                   :: nline
   real, dimension(50)                                       :: vet
   integer, dimension(50)                                    :: ivet
   integer                                                   :: iv
   integer                                                   :: natom
   integer                                                   :: ncomm
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
!  Controlla se le 3 righe di commento esistono
   ncomm = 0
   do i=1,3
      read(j_in,'(a)',iostat=ier)line1
      if (ier < 0) exit
!      
!     controlla se esiste una stringa di 1-2 caratteri (la specie chimica)
      call cutst(line1,nlong1,line2,nlong2)
      if (nlong2 == 1 .or. nlong2 == 2) then
!
!         controlla il numero di campi numerici
          call Getnum(line1,vet,ivet,iv)
          if (iv == 6) exit
      endif
      ncomm = ncomm + 1
   enddo
!
   if (ncomm /= 3) backspace (j_in)  ! rileggi l'ultima riga
!
!  Conta le righe nel file per allocare 
   nline = 0
   do 
      read(j_in,'(a)',iostat=ier)line1
      if (ier < 0) exit
      if (len_trim(line1) == 0) exit ! righe vuote a fine file
      nline = nline + 1
   enddo
   natom = nline !!!!!- ncomm
   allocate(atom(natom))
   rewind (j_in)
!   
!  Non legge le prime 3 righe se ci sono
   nline = 0
   do i=1,ncomm
      nline = nline + 1
      read(j_in,*,err=10)
   enddo
!   
   do i=1,natom
      nline = nline + 1
      read(j_in,'(a)',err=10)line1
!      
!     Leggi la specie sulla prima colonna
      call cutst(line1,nlong1,line2,nlong2)
      if (nlong2 > 2) go to 10
      atom(i)%lab = line2(:nlong2)
!      
!     Leggi il resto della riga
      call Getnum(line1,vet,ivet,iv)
      if (iv < 6) go to 10 
      atom(i)%xc(1) = vet(1)
      atom(i)%xc(2) = vet(3)
      atom(i)%xc(3) = vet(5)
   enddo
!
!  Converti in coordinate cristallografiche
   call cart_to_frac(atom,cell%get_ortoi())
!
!  Metti in nz il puntatore al file xen
   do i=1,natom
      atom(i)%ptab = pxen_from_specie(atom(i)%lab)
   enddo
!
   call ff%fclose()
   return
!
10 write(strn,'(i0)')nline
   call error%set('Error on reading line '//trim(strn)//' of MOPAC file')   
   call ff%fclose()
!   
   end subroutine read_mopcartfile

!------------------------------------------------------------------------------------------
   
   subroutine create_mopcrtfile(filename,atom,cell,title)
!
!  Create MOP file in cartesian format
!   
   USE atom_type_util
   USE unit_cell
   USE fileutil
   character(len=*), intent(in)              :: filename  ! se presente apre il file
   type(atom_type), dimension(:), intent(in) :: atom  
   type(cell_type), intent(in)               :: cell
   character(len=*), intent(in)              :: title
   type(atom_type), dimension(size(atom))    :: atomc 
   integer                                   :: j_in
   integer                                   :: ocode
   integer                                   :: i,j
   type(file_handle)                         :: fmop
!
!  Apertura del file
   call fmop%fopen(filename,'w')
   j_in = fmop%handle()
!
!  Header block
   write(j_in,'(a)')'PM6'                        ! tipo di ottimizzazione
   write(j_in,'(a)')trim(title)                  ! titolo
   write(j_in,'(a)')'All coordinates are cartesian'
!
!  Atom block
   atomc = atom
   ocode = 1       ! codice di ottimizzazione
   call frac_to_cart(atomc,cell%get_ortom())
   do i=1,size(atomc)
      write(j_in,'(a,3(f10.4,i5))')atomc(i)%specie(),(atomc(i)%xc(j),ocode,j=1,3)
   enddo          
   call fmop%fclose()
!
   end subroutine create_mopcrtfile

!------------------------------------------------------------------------------------------

   subroutine read_mopintfile(filename,cell,atom,error)
!
!  Read MOPAC Internal format
!   
   USE unit_cell
   USE atom_type_util
   USE strutil
   USE errormod
   USE elements
   USE cgeom
   USE fileutil
   character(len=*), intent(in)                              :: filename
   type(cell_type), intent(in)                               :: cell
   type(atom_type), allocatable, dimension(:), intent(inout) :: atom    ! atomi in uscita
   real, dimension(:,:), allocatable                         :: I_coor  ! Internal coordinates (d,ang,tors)
   integer, dimension(:,:), allocatable                      :: Conn    ! connettivita in zmt
   character(len=10)                                         :: strn
   integer                                                   :: i
   type(error_type), intent(out)                             :: error 
   character(len=500)                                        :: line1,line2
   integer                                                   :: nlong1,nlong2
   integer                                                   :: ier
   integer                                                   :: nline
   real, dimension(50)                                       :: vet
   integer, dimension(50)                                    :: ivet
   integer                                                   :: iv
   integer                                                   :: natom
   logical                                                   :: kpr = .false.
   integer                                                   :: ncomm
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
!  Controlla se le 3 righe di commento esistono
   ncomm = 0
   do i=1,3
      read(j_in,'(a)',iostat=ier)line1
      if (ier < 0) exit
!      
!     controlla se esiste una stringa di 1-2 caratteri (la specie chimica)
      call cutst(line1,nlong1,line2,nlong2)
      if (nlong2 == 1 .or. nlong2 == 2) then
!
!         controlla il numero di campi numerici
          call Getnum(line1,vet,ivet,iv)
          if (iv == 9) exit
      endif
      ncomm = ncomm + 1
   enddo
!
   if (ncomm /= 3) backspace (j_in)  ! rileggi l'ultima riga
!
!  Conta le righe nel file per allocare 
   nline = 0
   do 
      read(j_in,'(a)',iostat=ier)line1
      if (ier < 0) exit
      if (len_trim(line1) == 0) exit ! righe vuote a fine file
      nline = nline + 1
   enddo
   natom = nline !!!!!- ncomm
   allocate(atom(natom),Conn(3,natom),I_coor(3,natom))
   rewind (j_in)
!   
!  Non legge le prime 3 righe se ci sono
   nline = 0
   do i=1,ncomm
      nline = nline + 1
      read(j_in,*,err=10)
   enddo
!   
   do i=1,natom
      nline = nline + 1
      read(j_in,'(a)',err=10)line1
!      
!     Leggi la specie sulla prima colonna
      call cutst(line1,nlong1,line2,nlong2)
      if (nlong2 > 2) go to 10
      atom(i)%lab = line2(:nlong2)
!      
!     Leggi il resto della riga
      call Getnum(line1,vet,ivet,iv)
      if (iv < 9) go to 10 
      I_coor(1:3,i) = vet(1:6:2)
   ! cosi si corregge errore segnalato da Akihiro
      !if(i>3)I_coor(3,i) = dangle_norm(I_coor(3,i))
      if(i>3)I_coor(3,i) = -dangle_norm(I_coor(3,i))
      conn(1:3,i) = ivet(7:9)
   enddo
!
!  Converti coordinate interne in coordinate cartesiane
   call Zmatrix_to_Cartesian(natom,I_coor,conn)
   if (kpr) then
       do i=1,natom
          write(0,'(i4,3f10.3,3i6)')i,I_coor(:3,i),conn(:3,i)
       enddo
   endif        
!   
!  Trasferisci in atom le coordinate cartesiane
   do i=1,natom
      atom(i)%xc = I_coor(:3,i)
   enddo
!
!  Converti in coordinate cristallografiche
   call cart_to_frac(atom,cell%get_ortoi())
!
!  Metti in nz il puntatore al file xen
   do i=1,natom
      atom(i)%ptab = pxen_from_specie(atom(i)%lab)
   enddo
!
   call ff%fclose()
   return
!
10 write(strn,'(i0)')nline
   call error%set('Error on reading line '//trim(strn)//' of zmt file')   
   call ff%fclose()
!   
   end subroutine read_mopintfile

end module mop_frm
