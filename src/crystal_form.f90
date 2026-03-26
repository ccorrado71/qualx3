module crystal_frm

implicit none

integer, parameter, private :: CRYSTAL_TYPE=1, MOLECULE_TYPE=2
character(len=*), dimension(4), parameter, private :: cry_ver=['CRYSTAL23','CRYSTAL14','CRYSTAL17','CRYSTAL06']

contains
   
   subroutine write_crystal_file(filename,atom,cell,spg,structname)
   USE unit_cell
   USE atom_type_util
   USE fileutil
   USE elements
   USE spginfom
   USE strutil
   character(len=*), intent(in)                           :: filename  
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(cell_type), intent(in)                            :: cell     
   type(spaceg_type), intent(in)                          :: spg
   character(len=*), intent(in)                           :: structname
   type(file_handle)                                      :: fnw
   integer                                                :: j_in,i,cpos,nc,ncell
   integer                                                :: iflag,ifhr,ifso
   integer, dimension(6)                                  :: codec
   character(len=:), allocatable                          :: strf
   type(element_type), dimension(:), allocatable          :: elem
!
   call fnw%fopen(filename,'w')
   if (.not.fnw%good()) return

   j_in = fnw%handle()
   write(j_in,'(a)')trim(structname)
   write(j_in,'(a)')'CRYSTAL'
!
!  Space group
   iflag = 1 
   if (lattice_system(spg,cell%get_par()) == RHOMB_LATT) then
       ifhr = 1
   else
       ifhr = 0
   endif
   if (spg%symcent == 2) then
       ifso = 1
   else
       ifso = 0
   endif
   write(j_in,'(3(i0,2x))')iflag,ifhr,ifso
   cpos = index(spg%symbol_xhm,':)')
   if (cpos > 0) then
       write(j_in,'(a)')spg%symbol_xhm(cpos:len_trim(spg%symbol_xhm))
   else
       write(j_in,'(a)')trim(spg%symbol_xhm)
   endif
!
!  Lattice parameters
   codec(:) = cell_ref_code(spg)
   ncell = count(codec == 0)
   nc = 0
   do i=1,6
      if (codec(i) == 0) then
          if (i < 4) then
              strf = '(f0.5,2x)'
          else
              strf = '(f0.3,2x)'
          endif
          nc = nc + 1
          if (nc == ncell) then
              write(j_in,strf,advance='yes')cell%get_par(i)
          else
              write(j_in,strf,advance='no')cell%get_par(i)
          endif
      endif
   enddo
!  
!  Atoms
   write(j_in,'(i0)') numatoms(atom)
   do i=1,numatoms(atom)
      write(j_in,'(i3,3(1x,f12.6))')atom(i)%z(),atom(i)%xc(:)
   enddo
!
!  Keywords for geometry optimization
   write(j_in,'(a)')'OPTGEOM'
   write(j_in,'(a)')'ATOMONLY'
   write(j_in,'(a)')'ENDOPT'
   write(j_in,'(a)')'END'  ! end of the geometry input section
!
!  Basis sets
   call elements_from_atom(atom,elem)
   write(j_in,'(a)')'!Copy basis sets of '//cat_svet(elem(:)%lab,numelem(elem),',')
   write(j_in,'(a)')'!from http://www.crystal.unito.it/basis-sets.php'
   write(j_in,'(a)')'!and paste in this section'
   write(j_in,'(a)')'99 0'
   write(j_in,'(a)')'END'
   write(j_in,'(a)')'DFT'
   write(j_in,'(a)')'PBE0-D3'
   write(j_in,'(a)')'ENDDFT'
   write(j_in,'(a)')'SHRINK'
   write(j_in,'(a)')'8 8'
   write(j_in,'(a)')'MAXCYCLE'
   write(j_in,'(a)')'200'
   write(j_in,'(a)')'TOLDEE'
   write(j_in,'(a)')'8'
   write(j_in,'(a)')'FMIXING'
   write(j_in,'(a)')'60'
   write(j_in,'(a)')'TOLINTEG'
   !write(j_in,'(a)')'7 7 7 12 20'
   write(j_in,'(a)')'7 7 7 7 20'
   write(j_in,'(a)')'END'
!
   call fnw%fclose()
!
   end subroutine write_crystal_file

!-----------------------------------------------------------------------------------------------

   subroutine read_crystal_file(filename,atom,cell,spg,errc)
   USE unit_cell
   USE atom_type_util
   USE fileutil
   USE elements
   USE spginfom
   USE errormod
   USE strutil
   USE unit_cell
   character(len=*), intent(in)                            :: filename  
   type(atom_type), dimension(:), allocatable, intent(out) :: atom
   type(cell_type), intent(out)                            :: cell
   type(spaceg_type), intent(out)                          :: spg
   type(error_type), intent(out)                           :: errc
   type(file_handle)                                       :: fnw
   integer                                                 :: jout,ier,nline
   character(len=200)                                      :: line,line1
   integer                                                 :: i,pos,pos1
   real, dimension(20) :: vet
   integer, dimension(20) :: ivet
   integer :: iv,nat
   integer :: ctype,natcell,zval,lenz,nat0
   type(spaceg_type) :: spg0
!
   call fnw%fopen(filename,'r')
   if (.not.fnw%good()) return
!
   jout = fnw%handle()
!
!  Find type of calculation
   ctype = 0
   do
     read(jout,'(a)',iostat=ier)line
     if (ier /= 0) goto 10
     if (index(line,'CRYSTAL CALCULATION') > 0) then
         ctype = CRYSTAL_TYPE
         exit
     endif
     if (index(line,'MOLECULAR CALCULATION') > 0) then
         ctype = MOLECULE_TYPE
         exit
     endif
   enddo
   if (ctype == 0) goto 10
!
   select case (ctype)
     case(CRYSTAL_TYPE)
       call find_key_file(jout,'SPACE GROUP',nline,line,ier)
       if (ier /= 0) go to 10
       pos = index(line,':')
       spg = init_spaceg_type(trim(line(pos+1:)))
       !call spg%prn(0)
       call find_key_file(jout,'LATTICE PARAMETERS',nline,line,ier)
       read(jout,'(a)',iostat=ier)
       read(jout,'(a)',iostat=ier)line
       call getnum(line,vet,iv=iv)
       if (iv /= 6) go to 10
       cell = set_cell_type(vet(1:6))

     case(MOLECULE_TYPE)
       call find_key_file(jout,'CORRESPONDING SPACE GROUP',nline,line,ier)
       pos = index(line,':')
       spg0 = init_spaceg_type(trim(line(pos+1:)))
       call spg%set_p1()
       !call spg%prn(0)
!corr       cell = cellt%get_par()
       cell = cell_init()

   end select
!
!  Read number of atoms
   call find_key_file(jout,'NUMBER OF IRREDUCIBLE ATOMS',nline,line,ier)
   pos = index(line,':')
   call getnum(line(pos+1:),vet,ivet,iv=iv)
   if (ctype == CRYSTAL_TYPE) then
       call new_atoms(atom,ivet(1))
       !write(0,*)'NAT=',size(atom)
!
!      Read coordinates
       call find_key_file(jout,'ATOM AT. N.              COORDINATES',nline,line,ier)
       do i=1,size(atom)
          read(jout,'(a)')line   ! read: n. Z x y z
          call getnum(line,vet,ivet,iv)
          atom(i)%ptab = pxen_from_z(ivet(2))
          atom(i)%xc = vet(3:5)
          !write(0,*)'VET=',ivet(2),vet(3:5)
       enddo
   else
       call new_atoms(atom,spg0%nsymop*ivet(1))
       call find_key_file(jout,'N. ATOM EQUIV AT. N.',nline,line,ier)
       nat = 0
       do 
          read(jout,'(a)')line
          if (len_trim(line) == 0) cycle
          if (index(line,'NUMBER OF SYMMETRY') > 0) exit
          call cutst(line)
          call cutst(line)
          call cutst(line)
          call cutst(line,line2=line1)  ! read Z
          call s_to_i(line1,zval,ier,lenz)
          nat = nat + 1
          atom(nat)%ptab = pxen_from_z(zval)
          call cutst(line)
          call getnum(line,vet,iv=iv)
          atom(nat)%xc(:) = vet(1:3)
       enddo
       call resize_atoms(atom,nat)
   endif
!
   call find_key_file(jout,'FINAL OPTIMIZED GEOMETRY',nline,line,ier)
   if (ier == 0) then
       if (ctype == CRYSTAL_TYPE) then
           call find_key_file(jout,'PRIMITIVE CELL',nline,line,ier)
           if (ier == 0) then
               call read_cell_section(jout,cell,ier)
               if (ier /= 0) goto 10
           endif
       endif

       call find_key_file(jout,'ATOMS IN THE ASYMMETRIC UNIT',nline,line,ier)
       if (ier == 0) then
           pos = index(line,'UNIT')
           pos1 = index(line,'-')
           call getnum(line(pos+4:pos1-1),vet,ivet,iv=iv)
           !write(0,*)pos,iv,'VET=',vet(1),ivet(1),'NAT='//trim(line(pos+4:pos1-1))
           nat0 = ivet(1)
           pos = index(line,':')
           call getnum(line(pos+1:),vet,iv=iv)
           natcell = nint(vet(1))
           if (ctype == CRYSTAL_TYPE) then
               call new_atoms(atom,nat0)
           else
               call new_atoms(atom,natcell)
           endif
           !write(0,*)'NATCELL=',natcell,nat0

           read(jout,'(/)')
           call read_atom_section(jout,atom,natcell,ctype,ier)
           if (ier /= 0) goto 10
!corr   call print_atoms(atom,kpr=0)

           call find_key_file(jout,'CRYSTALLOGRAPHIC CELL (VOLUME=',nline,line,ier)
           if (ier == 0) then
               call read_cell_section(jout,cell,ier)
               if (ier /= 0) goto 10

               call find_key_file(jout,'COORDINATES IN THE CRYSTALLOGRAPHIC CELL',nline,line,ier)
               if (ier == 0) then
                   read(jout,'(/)')
                   call read_atom_section(jout,atom,natcell,ctype,ier)
                   if (ier /= 0) goto 10
               endif
           endif
       endif
   endif
   if (ctype == MOLECULE_TYPE) then
       call cart_to_frac(atom,cell%get_ortoi())
   endif
   call print_atoms(atom,kpr=0)
!
   call fnw%fclose()
   return

10 call errc%set('Error reading CRYSTAL file: '//trim(filename))
   call fnw%fclose()
!
   end subroutine read_crystal_file

!-----------------------------------------------------------------------------------------------
 
   subroutine read_cell_section(jfile,cell,ier)
   USE strutil
   USE unit_cell
   integer, intent(in)          :: jfile
   type(cell_type), intent(out) :: cell
   integer, intent(out)         :: ier
   real, dimension(10)          :: vet
   integer                      :: iv
   character(len=100)           :: line

   read(jfile,'(a)',iostat=ier)
   read(jfile,'(a)',iostat=ier)line
   if (ier /= 0) return
   call getnum(line,vet,iv=iv)
   if (iv == 6) then
       cell = set_cell_type(vet(1:6))
   else
       ier = 1
   endif

   end subroutine read_cell_section 

!-----------------------------------------------------------------------------------------------

   subroutine read_atom_section(jfile,atom,natr,ctype,ier)
   USE atom_type_util
   USE strutil
   USE elements
   integer, intent(in)                          :: jfile
   type(atom_type), dimension(:), intent(inout) :: atom
   integer, intent(in)                          :: natr
   integer, intent(in)                          :: ctype
   integer, intent(out)                         :: ier
   integer                                      :: nat,i,iv,zval,lenz
   character(len=100)                           :: line1,line2
!corr   integer, dimension(10)                       :: ivet
   real, dimension(10)                          :: vet
!
   ier = 0
   nat = 0
   do i=1,natr   !!!FIX: natcell here is wrong for molecule
      read(jfile,'(a)')line1
      call cutst(line1)
      call cutst(line1,line2=line2)      ! read T or F
      if (s_eqi(line2,'F') .and. ctype == CRYSTAL_TYPE) cycle
      call cutst(line1,line2=line2)  ! read Z
      call s_to_i(line2,zval,ier,lenz)
      nat = nat + 1
      atom(nat)%ptab = pxen_from_z(zval)
      call cutst(line1)
      call getnum(line1,vet,iv=iv)
      atom(nat)%xc(:) = vet(1:3)
      !write(0,*)nat,'LINE=',atom(nat)%ptab
   enddo
!
   end subroutine read_atom_section

!-----------------------------------------------------------------------------------------------

   integer function is_crystal_file(filename)  result(ver)
!
!  Check if the file is a CRYSTAL file
!
   USE fileutil
   character(len=*), intent(in)              :: filename  
   type(file_handle)                         :: fnw
   integer                                   :: i
   character(len=200)                        :: line
   integer                                   :: ier
!
   ver = 0
   call fnw%fopen(filename,'r')
   if (.not.fnw%good()) return
!
   loop_line: do 
     read(fnw%handle(),'(a)',iostat=ier)line
     if (ier /= 0) exit
     do i=1,size(cry_ver) 
        if (index(line,cry_ver(i)) > 0) then
            ver = i
            exit loop_line
        endif
     enddo
   enddo loop_line
   call fnw%fclose()
!
   end function is_crystal_file

end module crystal_frm
