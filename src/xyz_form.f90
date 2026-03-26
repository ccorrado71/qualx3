module xyz_frm

implicit none

contains

   subroutine read_xyzfile(filename,cellc,atom,error)
   USE strutil
   USE atom_type_util
   USE errormod
   USE unit_cell
   USE elements
   USE fileutil
   character(len=*), intent(in)                              :: filename
   type(cell_type), intent(in)                               :: cellc
   type(atom_type), allocatable, dimension(:), intent(inout) :: atom    ! atomi in uscita
   type(error_type), intent(out)                             :: error   ! errore in lettura
   integer                                                   :: nline
   character(len=500)                                        :: line,line2
   integer                                                   :: nlong1,nlong2
   real, dimension(50)                                       :: vet
   integer, dimension(50)                                    :: ivet
   integer                                                   :: iv
   integer                                                   :: natom
   integer                                                   :: i
   character(len=10)                                         :: strn
   integer                                                   :: ier
   integer                                                   :: n,nread
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
!  Leggi il numero di atomi dalla prima riga
   nline = 1
   read(j_in,*,err=10,end=10)natom
   if (natom == 0) then
       call error%set('Cannot read this file')
       go to 20
   else
       allocate(atom(natom))
   endif
!
!  Salta riga con commento  
   nline = nline + 1
   read(j_in,*,err=10)
!
!  Leggi natom atomi
   nread = 0   ! conta il numero di atomi letti
   do n=1,natom
      nline = nline + 1
      read(j_in,'(a)',iostat=ier)line
      if (ier < 0) exit
!
!     Leggi simbolo chimico 
      call cutst(line,nlong1,line2,nlong2)
      if (nlong2 > 2 .or. .not.s_is_alpha(line2(:nlong2))) go to 10   
      nread = nread + 1
      atom(nread)%lab = line2(1:2)
!
!     Leggi coordinate frazionarie
      call Getnum(line,vet,ivet,iv)
      if (iv /= 3) go to 10
      atom(nread)%xc = vet(1:3)
   enddo
!  considera che per errore
!  nread potrebbe essere inferiore ad natom
   call resize_atoms(atom,nread)
!
!  Non modificare la cella corrente 
   call cart_to_frac(atom,cellc%get_ortoi())
!
!  Metti in nz il puntatore al file xen
   do i=1,natom
      atom(i)%ptab = pxen_from_specie(atom(i)%lab)
   enddo
!
   return
10 write(strn,'(i0)')nline
   call error%set('Error on reading line '//trim(strn))   

20 continue
!
   end subroutine read_xyzfile

!-------------------------------------------------------------------------------------------------

   subroutine create_xyzfile(filename,atom,cell,comment)
!
!  Crea un XYZ chemical file format
!   
   USE unit_cell
   USE atom_type_util
   USE fileutil
   character(len=*), intent(in), optional    :: filename  ! se presente apre il file
   type(atom_type), dimension(:), intent(in) :: atom
   type(cell_type), intent(in)               :: cell
   character(len=*), intent(in)              :: comment
   type(atom_type), dimension(size(atom))    :: atomc
   integer                                   :: num_atoms
   integer                                   :: i
   integer                                   :: j_in
   type(file_handle)                         :: ff
!
!  Apertura del file
   call ff%fopen(filename,'w')
   j_in = ff%handle()
!
!  Scrvi il numero di atomi sulla prima riga
   num_atoms = size(atom)
   write(j_in,'(i0)')num_atoms
!
!  Scrivi un commento sulla seconda riga
   write(j_in,'(a)')trim(comment)
!
!  Scrivi simbolo chimico e coordinate cartesiane sulle altre righe
   atomc = atom
   call frac_to_cart(atomc,cell%get_ortom()) 
   do i=1,num_atoms
      write(j_in,'(a2,3(1x,f10.5))')atomc(i)%spec(),atomc(i)%xc
   enddo          
!
   call ff%fclose()
!
   end subroutine create_xyzfile

end module xyz_frm
