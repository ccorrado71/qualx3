module mol2_frm

implicit none

contains

   subroutine read_mol2file(filename,atom,bonds,spg,cellmol2,error)
!
!  Legge da MDL (Molecular Design Limited) MolFile specie,coord,connettivita'
!   
   USE atom_type_util
   USE strutil
   USE cgeom
   USE errormod
   USE connect_mod
   USE spginfom
   USE elements
   USE fileutil
   USE unit_cell
!
   character(len=*), intent(in)                              :: filename
   type(atom_type), allocatable, dimension(:), intent(out) :: atom  ! atomi in uscita
   type(bond_type), dimension(:), allocatable, intent(out) :: bonds  ! connettivita'
   type(spaceg_type), intent(out)                            :: spg   ! space group
   type(cell_type), intent(out)                              :: cellmol2
   type(error_type), intent(out)                             :: error ! errore in lettura
   integer                                                   :: i,j
   character(len=500)                                        :: buff
   character(len=:), allocatable                             :: line,line2
   integer                                                   :: pos
   integer                                                   :: natom
   integer                                                   :: nconn
   real, dimension(:), allocatable                           :: vet
   integer, dimension(:), allocatable                        :: ivet
   integer                                                   :: iv
   integer                                                   :: icc
   integer                                                   :: nline
   integer                                                   :: ier
   character(len=10)                                         :: strn
   integer                                                   :: nlong1
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
!  Leggi blocco @<TRIPOS>MOLECULE
   nline = 0
   do
      nline = nline + 1
      read(j_in,'(a)',iostat=ier)buff
      if (ier < 0) then
          call error%set('It does not seem mol2 file')
          go to 20
      endif    
      if(index(buff,'@<TRIPOS>MOLECULE') > 0 ) exit
   enddo
   nline = nline + 1
   read(j_in,'(a)',err=10)buff
   nline = nline + 1
   read(j_in,'(a)',err=10)buff
   line = buff
   call Getnum1(line,ivet=ivet,iv=iv)
   if (iv < 2) go to 10
   natom = ivet(1)
   if (natom == 0) then
       go to 10
   else           
       call new_atoms(atom,natom)
   endif
   nconn = ivet(2)

   spg = init_spaceg_type()
   cellmol2 = cell_init()

   call ff%rew()
   nline = 0

   do while(get_line(j_in,line,trimmed=.true.)) 
!   
!     Leggi blocco @<TRIPOS>ATOM
      nline = nline + 1
      if (index(line,'@<TRIPOS>ATOM') > 0) then
          !call find_key_file(j_in,'@<TRIPOS>ATOM',nline,line,ier)
          !if (ier < 0) then
          !    call error%set('It does not seem mol2 file')
          !    go to 20
          !endif    
          do i=1,natom
             nline = nline + 1
             !read(j_in,'(a)',err=10)line2
             if (.not.get_line(j_in,line2,trimmed=.true.)) exit
             call cutsta(line2,line)
             call cutsta(line2,line)
!             
!            Leggi le coordinate
             do j=1,3
                call cutsta(line2,line)
                if (len_trim(line) == 0) go to 10
                ier = s_to_r(line,atom(i)%xc(j),nlong1)
                if (ier > 0) go to 10
             enddo
!             
!            Leggi atom type
             call cutsta(line2,line)
             if (len_trim(line) == 0) go to 10
             pos = index(line,'.') - 1
             if (pos <= 0) then
                 atom(i)%lab = line
             else              
                 atom(i)%lab = line(:pos)
             endif
          enddo
          natom = i - 1
          call resize_atoms(atom,natom)

      elseif (index(line,'@<TRIPOS>BOND') > 0) then
!
!         Leggi blocco @<TRIPOS>BOND
          !call find_key_file(j_in,'@<TRIPOS>BOND',nline,line,ier)
          !if (ier < 0) then
          !    call error%set('Cannot find <TRIPOS>BOND section')
          !    go to 20
          !endif    
          call new_bonds(bonds,nconn)
          icc = 1
          do i=1,nconn
             nline = nline + 1
             !read(j_in,'(a)',err=10)line
             if (.not.get_line(j_in,line,trimmed=.true.)) exit
             call Getnum1(line,ivet=ivet,iv=iv)
             if (iv < 3) go to 10
             !if (ivet(2) > natom) then
             !    write(0,30)nline,ivet(2)
             !    cycle
             !endif    
             !if (ivet(3) > natom) then
             !    write(0,30)nline,ivet(3)
             !    cycle
             !endif    
             bonds(i)%n1 = ivet(2)
             bonds(i)%n2 = ivet(3)
             !bonds(i)%dist = distanzaC(atom(ivet(2))%xc,atom(ivet(3))%xc)
             bonds(i)%sigma = 0.03
          enddo   

      elseif (index(line,'@<TRIPOS>CRYSIN') > 0) then
!
!
!         Read crystallografic information
          !call find_key_file(j_in,'@<TRIPOS>CRYSIN',nline,line,ier)
          !if (ier == 0) then
              !read(j_in,'(a)',iostat=ier)line
              if (get_line(j_in,line,trimmed=.true.)) then
                  if (ier == 0) then
                      call Getnum1(line,vet,ivet,iv)
                      if (iv == 8) then
                          cellmol2 = set_cell_type(vet(:6))
                          spg = init_spaceg_type(numb = ivet(7),code=ivet(8))
                      endif
                  endif
              endif
         !endif
      endif
   enddo
!
!  Set bonds
   do i=1,numbonds(bonds)
      if (bonds(i)%n1 > natom .or. bonds(i)%n2 > natom) call bonds(i)%set_as_deleted()
      bonds(i)%dist = distanzaC(atom(bonds(i)%n1)%xc,atom(bonds(i)%n2)%xc)
   enddo
   call bond_delete_selected(bonds)
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
   call error%set('Error on reading line '//trim(strn)//' of mol2 file')   
20 continue
   call ff%fclose()
!
   end subroutine read_mol2file  

end module mol2_frm
