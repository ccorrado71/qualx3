MODULE pdb_frm
 
implicit none

   type pdb_atom_info_type
     character(len=1) :: alt_loc = ' '   ! Alternate location indicator
     character(len=3) :: res_name = ' '  ! Residual name
     character(len=1) :: chain_id = ' '  ! Chain identifier 
     integer          :: res_seq = 0     ! Residue sequence number
     character(len=1) :: atype = ' '     ! A = ATOM, H = HETATM  
   end type pdb_atom_info_type

   type secondary_struct_type
     character(len=1) :: stype = ' '         ! 'H' = helix, 'S' = sheet
     character(len=1) :: init_chain_id = ' ' ! Chain identifier 
     integer          :: init_seq_num = 0    ! Sequence number of the initial residue
     integer          :: end_seq_num = 0     ! Sequence number of the terminal residue
   end type secondary_struct_type

   integer, private :: NSEC_STRUCT_INIT = 500

   type(pdb_atom_info_type), allocatable, dimension(:)    :: atom_info
   type(secondary_struct_type), allocatable, dimension(:) :: sec_struct
   integer                                                :: nsecstr

   private :: GetSecondary

CONTAINS

   subroutine read_PDBfile(filename,cella,is_cell,atom,bond,conect,spacepdb,error)
!
!  Legge da PDB specie,coord,connettivita'
!   
   USE unit_cell
   USE atom_type_util
   USE strutil
   USE errormod
   USE connect_mod
   USE elements
   USE arrayutil
   USE fileutil
!
   character(len=*), intent(in)                              :: filename
   type(cell_type), intent(inout)                            :: cella
   logical, intent(in)                                       :: is_cell  ! esiste gia' una cella
   type(atom_type), allocatable, dimension(:), intent(inout) :: atom     ! atomi in uscita
   type(bond_type), dimension(:), allocatable, intent(out)   :: bond     ! connettivita'
   logical, intent(out)                                      :: conect   ! esiste la connettivita'
   character(len=16)                                         :: spacepdb ! gruppo spaziale
   type(error_type), intent(out)                             :: error    ! errore in lettura
   integer                                                   :: i,j,k
   character(len=500)                                        :: line
   integer                                                   :: natom
   integer                                                   :: nline
   integer                                                   :: ier
   character(len=10)                                         :: strn
   integer                                                   :: lenv
   real                                                      :: rvalue
   integer                                                   :: inil,finl
   integer, allocatable, dimension(:)                        :: vser
   integer                                                   :: ivalue,ivalue1,ivalue2
   integer                                                   :: iser
   integer                                                   :: nconn, nconntot
   real, dimension(3,3)                                      :: scalevet
   real, dimension(3)                                        :: uvet
   integer                                                   :: iscale
   logical                                                   :: is_cell_pdb
   real, dimension(6)                                        :: cellpdb
   integer                                                   :: ispec
   character(len=4)                                          :: strname
   integer                                                   :: kpos1,kpos2
   type(container_type), dimension(:), allocatable           :: conn
   integer, dimension(10)                                    :: vetatom
   integer                                                   :: numl,pos_res,lastchain
   logical                                                   :: is_secondary,is_atominfo
   logical, parameter                                        :: infolab = .true.
   type(cell_type)                                           :: celldef
   character(len=3)                                          :: resname
   character(len=3), dimension(32) :: RESCODE = [ &
!      code for amino acids 
       'ALA','CYS','ASP','GLU','PHE',             &
       'GLY','HIS','ILE','LYS','LEU',             &
       'MET','ASN','PRO','GLN','ARG',             &
       'SER','THR','VAL','TRP','TYR',             &
!      code for deoxyribonucleotides
       ' DA',' DC',' DG',' DT',' DI',             &
!      code for ribonucleotides
       '  A','  C','  G','  U','  I',             &
!      code for modified residues
       'MSE','CBR' ]
!
!  code for unknown residue name
   character(len=3), dimension(4) :: UNKCODE = ['UNK','UNX','UNL','  N']
   integer, parameter :: UNKID = -999999
!
!  variables for extra chemical component
   integer, parameter                        :: NMAXEXTRARES = 100
   integer                                   :: nextrares, posextra
   character(len=3), dimension(NMAXEXTRARES) :: extrares
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
!  Conta gli atomi per allocare atom e i terminatori
   natom = 0
   do 
      read(j_in,'(a)',iostat=ier)line
      if (ier < 0) exit
      if (line(1:4) == 'ATOM' .or. line(1:6) == 'HETATM') natom = natom + 1
      if (line(1:6) == 'ENDMDL') exit
   enddo
   is_secondary = .true.
   is_atominfo = .true.
   if (natom == 0) then
       call error%set('Cannot read this PDB file')
       go to 20
   else
       allocate(atom(natom),conn(natom),vser(natom))
       if (is_atominfo) then
!!mr
           if (allocated(atom_info)) then
               deallocate(atom_info)
           endif
!!end mr
           allocate(atom_info(natom))
       endif
   endif
   rewind (j_in)
!
   nline = 0
   nconntot = 0
   iscale = 0
   conect = .false.
   is_cell_pdb = .false.
   spacepdb = 'P 1'
   natom = 0
   nextrares = 0
   !secondary = present(nsec) .and. present(sec_struct)
   if (is_secondary) then
       nsecstr = 0
       call new_sec_struct(sec_struct,NSEC_STRUCT_INIT)
   endif
   do
      nline = nline + 1
      read(j_in,'(a)',iostat=ier)line
      if (ier < 0) exit
      select case (line(1:6))
         case ('CRYST1')
!
!          cella
           ier = s_to_r(line(7:15),cellpdb(1),lenv)
           if (ier > 0) go to 10
           ier = s_to_r(line(16:24),cellpdb(2),lenv)
           if (ier > 0) go to 10
           ier = s_to_r(line(25:33),cellpdb(3),lenv)
           if (ier > 0) go to 10
           ier = s_to_r(line(34:40),cellpdb(4),lenv)
           if (ier > 0) go to 10
           ier = s_to_r(line(41:47),cellpdb(5),lenv)
           if (ier > 0) go to 10
           ier = s_to_r(line(48:54),cellpdb(6),lenv)
           if (ier > 0) go to 10
           is_cell_pdb = .true.
!
!          gruppo spaziale
           spacepdb = adjustl(line(56:66))
           
         case ('HETATM','ATOM  ')      !Coordinate Section
!           
!          serial number
           call s_to_i(line(7:11),iser,ier,lenv)
           if (ier > 0) go to 10
           natom = natom + 1
           vser(natom) = iser
!              
!          othogonal coordinate for X,Y,Z
           do i=1,3
              inil = 31+(i-1)*8
              finl = inil+7
              ier = s_to_r(line(inil:finl),rvalue,lenv)
              if (ier > 0) go to 10
              atom(natom)%xc(i) = rvalue
           enddo
!         
!          Occupancy
           ier = s_to_r(line(55:60),rvalue,lenv)
           if (ier == 0 .and. rvalue /= 0.0) then
               atom(natom)%och = rvalue
           else   !occ assente come nel pdb di chem3d 
               atom(natom)%och = 1.0
           endif
!         
!          Temperature factor 
           ier = s_to_r(line(61:66),rvalue,lenv)
           if (ier == 0) then
               atom(natom)%biso = rvalue
           else   !b assente come nel pdb di chem3d 
               atom(natom)%biso = 0.0
           endif
!
!          Atom name
           atom(natom)%lab = trim(adjustl(line(13:16)))
!
!          Alternate location indicator
           if (len_trim(line(17:17)) > 0) then
               atom(natom)%rcod(1) = ichar(lower(line(17:17)))
               if (is_atominfo) then
                   atom_info(natom)%alt_loc = upper(line(17:17))
               endif
           endif
!
!          Residue name
           if (is_atominfo) then
               atom_info(natom)%atype = line(1:1)
               pos_res = 0
               resname = line(18:20)
               if (len_trim(resname) > 0) then
                   pos_res = string_locate(line(18:20),RESCODE)
                   atom_info(natom)%res_name = adjustl(resname)
                   if (pos_res == 0 .and. string_locate(resname,UNKCODE) == 0) then
                       if (nextrares > 0) then
                           posextra = string_locate(resname,extrares(:nextrares))
                       else
                           posextra = 0
                       endif
                       if (posextra == 0) then
                           nextrares = nextrares + 1
                           extrares(nextrares) = resname
                       endif
                   endif
               else
                   atom_info(natom)%res_name = 'UNK'
               endif
!
!              Chain identifier
               if (line(22:22) /= ' ') then
                   atom_info(natom)%chain_id = upper(line(22:22))
               else
!
!                  Chain identifier was absent: assign A if resname was defined
                   if (pos_res > 0)  atom_info(natom)%chain_id = 'A'
!
!                  Assign W for water molecules
                   if (atom_info(natom)%chain_id == ' ') then
                       if (s_eqidb(resname,'HOH')) then
                           atom_info(natom)%chain_id = 'W'
                       endif
                   endif
               endif
!
!              Residue sequence number
               if (len_trim(line(23:26)) > 0) then
                   call s_to_i(line(23:26),ivalue,ier,lenv)
                   if (ier == 0) atom_info(natom)%res_seq = ivalue
               endif
           endif
!
!          Element symbol in line(77:78)
           ispec = pxen_from_specie(trim(adjustl(line(77:78))))
           if (ispec == 0) then   ! potrebbe non esserci 
!
!              rimuovi caratteri non alphabetici all'inizio (es. 1HA2 -> HA2)
               strname = line(13:16)
               do k=1,len(strname)
                  if (ch_is_alpha(strname(k:k))) exit
                  strname(k:k) = ' '
               enddo
               ispec = pxen_from_label(adjustl(strname(1:2)))  ! estrai dai primi 2 caratteri dell'atom name 
           endif
           atom(natom)%ptab = ispec
!           
         case ('CONECT')             !Connectivity Section
           call s_to_i(line(7:11),ivalue1,ier,lenv)
           kpos1 = get_atom_position(ivalue1)
!
           if (kpos1 > 0) then
               finl = 11
               nconn = 0
               do j=1,10
                  inil = finl + 1
                  finl = inil + 4
                  call s_to_i(line(inil:finl),ivalue2,ier,lenv)
                  if (ivalue2 == 0) exit
                  kpos2 = get_atom_position(ivalue2)
                  if (kpos2 > 0) then
                      nconn = nconn + 1
                      vetatom(nconn) = kpos2
                  endif
               enddo
               nconntot = nconntot + nconn
               call container_set(conn(kpos1),nconn,vetatom)
           endif

         case ('SCALE1','SCALE2','SCALE3')             !record 1,2,3 for ortho. matrix
           call s_to_i(line(6:6),iscale,ier,lenv)
           ier = s_to_r(line(11:20),scalevet(iscale,1),lenv)
           ier = s_to_r(line(21:30),scalevet(iscale,2),lenv)
           ier = s_to_r(line(31:40),scalevet(iscale,3),lenv)
           ier = s_to_r(line(46:55),uvet(iscale),lenv)
           !write(*,*)line(1:6),scalevet(iscale,:),uvet(iscale)

         case ('HELIX','SHEET')    ! Secondary Structure Section
           if (is_secondary) then
               nsecstr = nsecstr + 1
               call check_capacity_secondary_struct(sec_struct,nsecstr)
               if (line(1:1) == 'S') then
                   sec_struct(nsecstr)%stype = 'S'
                   sec_struct(nsecstr)%init_chain_id = line(22:22)
                   call s_to_i(line(23:26),sec_struct(nsecstr)%init_seq_num,ier,lenv)
                   call s_to_i(line(34:37),sec_struct(nsecstr)%end_seq_num,ier,lenv)
               elseif (line(1:1) == 'H') then
                   sec_struct(nsecstr)%stype = 'H'
                   sec_struct(nsecstr)%init_chain_id = line(20:20)
                   call s_to_i(line(22:25),sec_struct(nsecstr)%init_seq_num,ier,lenv)
                   call s_to_i(line(34:37),sec_struct(nsecstr)%end_seq_num,ier,lenv)
               endif
               if (ier /= 0) nsecstr = nsecstr - 1
           endif

          case ('ENDMDL')
           exit
      end select
   enddo
!
!  Converti in coordinate cristallografiche
   if (is_cell) then
       call cart_to_frac(atom,cella%get_ortoi())     
   else
       if (is_cell_pdb) then                             ! ... usa la cella del pdb
           cella = set_cell_type(cellpdb)
           if (iscale == 3) then
               do i=1,natom
                  atom(i)%xc = MATMUL(scalevet,atom(i)%xc) + uvet
               enddo
           else
               call cart_to_frac(atom,cella%get_ortoi())   
           endif
       else
           cella = celldef
           call cart_to_frac(atom,cella%get_ortoi())      
       endif
   endif
!
   if (natom == 0) return
!
!  Segnala l'esistenza della connettivita' se ti sembra ragionevole: almeno mezzo legame per atomo
   if (nconntot > 0 .and. nconntot > 0.5*natom) then
       conect = .true.
       call connect_to_leg(conn,atom,cella%get_g(),bond,numl,sizel=sum(conn%nat))
   endif    
!
!  Add chain identifier and residue sequence number to atom_info
   if (is_atominfo) then
       if (any(atom_info%chain_id == ' ')) then
!
!          Find the last chain identifier
           lastchain = maxval(ichar(atom_info%chain_id))
           if (achar(lastchain) == ' ') lastchain = ichar('A') - 1
       endif
       do i=1,natom
!
!         Assign the last chain identifier if resname is unknown
          if (atom_info(i)%chain_id == ' ') then
              if (string_locate(atom_info(i)%res_name,UNKCODE) > 0 .or. len_trim(atom_info(i)%res_name) == 0) then
                  atom_info(i)%chain_id = achar(lastchain+nextrares+1)
              endif
          endif
!
!         Save extra chemical component e set chain identifier as negative value
          if (atom_info(i)%chain_id == ' ') then
              if (nextrares > 0) then
                  posextra = string_locate(atom_info(i)%res_name,extrares(:nextrares))
                  atom_info(i)%chain_id = achar(lastchain+posextra)
              endif
          endif
       enddo
       if (infolab) then
!!mr       if (infolab .and. nsecstr > 0) then
!
!          Modify label adding chain information
!           do i=1,natom
!              if (atom_info(i)%alt_loc == ' ') then
!                  atom(i)%lab = trim(atom(i)%lab)//'_'//trim(atom_info(i)%res_name)//     &
!                         '_'//trim(i_to_s(atom_info(i)%res_seq))//'_'//atom_info(i)%chain_id
!              else
!                  atom(i)%lab = trim(atom(i)%lab)//'/'//atom_info(i)%alt_loc//'_'//trim(atom_info(i)%res_name)//   &
!                         '_'//trim(i_to_s(atom_info(i)%res_seq))//'_'//atom_info(i)%chain_id
!              endif
!              !write(71,*)'LAB=',trim(atom(i)%lab),atom_info(i)%atype
!           enddo
           do i=1,natom
              if (atom_info(i)%alt_loc == ' ') then
                  atom(i)%chain = trim(atom_info(i)%res_name)//     &
                         '_'//trim(i_to_s(atom_info(i)%res_seq))//'_'//atom_info(i)%chain_id
              else
                  atom(i)%chain = '/'//atom_info(i)%alt_loc//'_'//trim(atom_info(i)%res_name)//   &
                         '_'//trim(i_to_s(atom_info(i)%res_seq))//'_'//atom_info(i)%chain_id
              endif
           enddo
           call GetSecondary(atom%chain, natom)
       endif
   endif
!
   if (is_secondary) then
       call resize_sec_struct(sec_struct,nsecstr,.true.)
   endif
!
   call ff%fclose()
   return      
!      
10 write(strn,'(i0)')nline
   call error%set('Error on reading line '//trim(strn)//' of PDB file')   
20 call error%print()
   call ff%fclose()
!
   CONTAINS 

      integer function get_atom_position(iser) result(pos)
!      
!     Passa dal seriale al numero d'ordine dell'atomo nella lista
!
      integer, intent(in) :: iser
      pos = 0
      do i=1,natom
         if(vser(i) == iser) then
            pos = i
            exit
         endif
      enddo
!
      end function get_atom_position

   end subroutine read_PDBfile  

!------------------------------------------------------------------------------------------

   subroutine GetSecondary(sec_vett, natom)
   character(len=*), dimension(:), intent(inout) :: sec_vett
   integer                                       :: i, j, n1, n2, natom
   character (len=1)                             :: Catena, Secondary

   do j=1, nsecstr
      if (sec_struct(j)%init_chain_id /= ' ') then
          Catena = sec_struct(j)%init_chain_id
      else
          Catena = 'A'
      endif
      Secondary = sec_struct(j)%stype
      n1 = sec_struct(j)%init_seq_num
      n2 = sec_struct(j)%end_seq_num
      do i=1, natom
         if (atom_info(i)%chain_id /= Catena) cycle
         if (atom_info(i)%res_seq >= n1 .and.  atom_info(i)%res_seq <= n2) then
             !sec_vett(i:i) = Secondary
             sec_vett(i) = trim(sec_vett(i))//Secondary
         endif
      enddo
   enddo
   if(allocated(atom_info)) then
      do i=1, natom
         if (atom_info(i)%atype == 'H') then
             !sec_vett(i:i) = 'L'
             sec_vett(i) = trim(sec_vett(i))//'L'
         endif
      enddo
   endif

   end subroutine GetSecondary

!------------------------------------------------------------------------------------------

   subroutine create_pdbfile(filename,atoms,cell,bond,spg)
!
!  Crea un PDB file 
!   
   USE elements, only:NLEN_LAB
   USE unit_cell
   USE atom_type_util
   USE connect_mod
   USE fileutil
   USE spginfom
   character(len=*), intent(in)                           :: filename  ! se presente apre il file
   type(atom_type), dimension(:), allocatable, intent(in) :: atoms
   type(cell_type), intent(in)                            :: cell
   type(bond_type), dimension(:), allocatable, intent(in) :: bond   
   type(spaceg_type), intent(in)                          :: spg
   real, dimension(3)                                     :: uvet
   type(atom_type), dimension(:), allocatable             :: atomc
   integer                                                :: i
   integer                                                :: resq
   character(len=NLEN_LAB)                                :: labat
   real, dimension(3,3)                                   :: ortop
   integer                                                :: j_in
   type(file_handle)                                      :: fpdb
!
!  Apertura del file
   call fpdb%fopen(filename,'w')
   j_in = fpdb%handle()
!
!  Title Section
   write(j_in,'(a)')'HEADER    This PDB file is created by expo'
!
!  Crystallographic section
   write(j_in,'(a,3f9.3,3f7.2,1x,a)')'CRYST1',cell%get_par(),trim(spg%symbol_xhm)
   ortop = cell%get_ortoi()
   uvet(:) = 0
   write(j_in,'(a,t11,3f10.6,t46,f10.5)')'SCALE1',ortop(1,:3),uvet(1)
   write(j_in,'(a,t11,3f10.6,t46,f10.5)')'SCALE2',ortop(2,:3),uvet(2)
   write(j_in,'(a,t11,3f10.6,t46,f10.5)')'SCALE3',ortop(3,:3),uvet(3)
!
!  Coordinate Section
   call frac_to_cart_copy(atoms,atomc,cell%get_ortom()) ! convert in cartesian coordinate
   resq = 1
   do i=1,numatoms(atomc)
      labat = atomc(i)%specie()
      write(j_in,'(a,i5,1x,a,1x,a,t23,i4,t31,3f8.3,2f6.2,t77,a)')'HETATM',i,atomc(i)%lab(1:4),'UNK',resq, &
                   atomc(i)%xc,atomc(i)%och,atomc(i)%biso,adjustr(labat(1:2))
   enddo
!
!  Connectivity Section
   call pdb_print_connect(j_in,numatoms(atoms),bond)
!
   write(j_in,'(a)')'END'
!
   call fpdb%fclose()
!
   end subroutine create_pdbfile

!--------------------------------------------------------------------------------------------------

   subroutine pdb_print_connect(jout,natom,bond)
   use strutil
   use connect_mod
   use arrayutil
   integer, intent(in)                                    :: jout
   type(bond_type), dimension(:), allocatable, intent(in) :: bond   
   type(container_type), dimension(:), allocatable        :: conn
   integer, intent(in)                                    :: natom
   integer, parameter                                     :: STEP=4
   character(len=:), allocatable                          :: sform
   integer                                                :: i,ini,fin
!
   call bond_to_connect(natom,bond,conn)
   sform = '(a,i5,'//i_to_s(STEP)//'i5)'
   do i=1,natom
      if (conn(i)%nat == 0) cycle
      fin = 0
      do
         ini = fin + 1
         fin = ini + STEP - 1
         if (fin > conn(i)%nat) fin = conn(i)%nat
         write(jout,sform)'CONECT',i,conn(i)%pos(ini:fin)
         if (fin == conn(i)%nat) exit
      enddo
   enddo
!
   end subroutine pdb_print_connect

!--------------------------------------------------------------------------------------------------

   subroutine resize_sec_struct(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo secondary_struct_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(secondary_struct_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                                     :: n
   logical, optional, intent(in)                           :: savevet
   logical                                                 :: savev
   integer                                                 :: nv
   type(secondary_struct_type), allocatable                :: vsav(:)
   integer                                                 :: nsav
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
           if (nv /= n) then
               deallocate(vetr)
               allocate(vetr(n))
           endif
       endif
   endif
!
   end subroutine resize_sec_struct

!----------------------------------------------------------------------------------------------------

   subroutine new_sec_struct(vetr,n)
!
!  Create new secondary structure
!
   type(secondary_struct_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                                     :: n

   if (n < 0) return
   if (numsecondary(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_sec_struct

!--------------------------------------------------------------------------------------------------

   integer function numsecondary(secv)
   type(secondary_struct_type), dimension(:), allocatable, intent(in) :: secv
!   
   if (allocated(secv)) then
       numsecondary = size(secv)
   else
       numsecondary = 0
   endif
!
   end function numsecondary

!--------------------------------------------------------------------------------------------------

   subroutine check_capacity_secondary_struct(secv,nsec)
   type(secondary_struct_type), dimension(:), allocatable, intent(inout) :: secv
   integer, intent(in)                                                   :: nsec
   integer                                                               :: capacity
   capacity = numsecondary(secv)
   if (capacity < nsec) then
       call resize_sec_struct(secv,capacity+NSEC_STRUCT_INIT)
   endif
   end subroutine check_capacity_secondary_struct

END MODULE pdb_frm
