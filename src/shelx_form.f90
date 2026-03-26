module shelx_frm

implicit none

contains

   subroutine read_shelxfile(filename,atom,cella,is_cell,spg,error)
   USE atom_type_util
   USE errormod
   USE strutil
   USE elements
   USE arrayutil
   USE trig_constants, only:twopi
   USE fileutil
   USE unit_cell
   USE spginfom
   USE ccryst
   character(len=*), intent(in)                              :: filename
   type(atom_type), dimension(:), allocatable, intent(inout) :: atom
   type(cell_type), intent(inout)                            :: cella
   logical, intent(in)                                       :: is_cell   ! esiste gia' una cella corrente
   integer                                                   :: icentsh
   type(spaceg_type), intent(out)                            :: spg
   type(error_type), intent(out)                             :: error 
   character(len=80)                                         :: line1,line2
   integer                                                   :: nlong1,nlong2
   integer                                                   :: ier
   integer                                                   :: pos
   integer, dimension(50)                                    :: ivet
   real, dimension(50)                                       :: vet
   integer                                                   :: iv
   logical                                                   :: cont_line
   integer                                                   :: nv
   character(len=80), dimension(50)                          :: word
   integer                                                   :: nword
   integer, dimension(:), allocatable                        :: zval
   integer                                                   :: nspec
   integer                                                   :: i
   integer, parameter                                        :: NMAXA=100
   integer                                                   :: natom
   integer                                                   :: znum
   real, dimension(6)                                        :: cellsh
   logical                                                   :: is_cell_shelx
   integer                                                   :: klastb
   integer                                                   :: lenv
   integer                                                   :: lattx
   logical                                                   :: is_matrix
   real, dimension(6)                                        :: rcell
   real, dimension(3,3)                                      :: rgmat,gmat
   character(len=4), dimension(73)                           :: shelx_word =              &
      (/'titl', 'cell', 'zerr', 'latt', 'symm', 'sfac', 'unit', 'fvar', 'affi', 'devi',   &
        'hkl ', 'wght', 'exti', 'omit', 'merg', 'l.s.', 'fmed', 'plan', 'exyz', 'eadp',   &
        'sump', 'rem ', 'list', 'fmap', 'hklf', 'mole', 'afix', 'dfix', 'bond', 'bloc',   &
        'anis', 'size', 'conf', 'eqiv', 'htab', 'acta', 'dang', 'isor', 'resi', 'part',   &
        'disp', 'end ', 'bind', 'free', 'slim', 'temp', 'wpdb', 'grid', 'hfix', 'rtab',   &
        'mpla', 'sadi', 'same', 'flat', 'chiv', 'bump', 'conn', 'swat', 'delu', 'simu',   &
        'damp', 'defs', 'spec', 'shel', 'time', 'more', 'laue', 'twin', 'basf', 'move',   &
        'cgls', 'frag', 'fend'/)
   type(symop_type), dimension(192) :: symop
   integer :: nsymop
   logical :: sfound
   character(len=1) :: lattsh
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
   cont_line = .false.
   is_cell_shelx = .false.
   natom = 0
   nspec = 0
   klastb = 0
   nsymop = 1
   icentsh = 1
   is_matrix = .false.
   lattsh = 'P'
   symop(1) = symop_type(identity_mat,(/0,0,0/))
   call new_atoms(atom,NMAXA)
!
   do 
      read(j_in,'(a)',iostat=ier)line1
      if (ier < 0) exit
      call s_filter(line1)                           ! filtra linea
      if (is_comment_line(line1,['REM','!  '])) cycle  ! commento a inizio riga
      line1 = s_delete_comment(line1,'!')            ! rimuovi commenti con '!'
      nlong1 = len_trim(line1)
      if (nlong1 == 0) cycle                         ! linea vuota
!
      if (.not.cont_line) then                     ! se non c'e' il carattere di continuazione
          call cutst(line1,nlong1,line2,nlong2)    ! taglia la stringa iniziale
          pos = string_locate(line2,shelx_word)     
          nv = 0                                   ! azzera contatatore del vettore da leggere
      endif
!
!     controlla se la nuova linea ha il carattere di continuazione
      if (nlong1 > 0) then
          cont_line = line1(nlong1:nlong1) == '='
          if (cont_line) line1(nlong1:nlong1) = ' '
      else
          cont_line = .false.
      endif
!
      select case (pos)
         case (0)       ! atomi
          call Getnum(line1,vet(nv+1:),ivet(nv+1:),iv)
          nv = nv + iv
          if (.not.cont_line) then  ! aggiungi l'atomo
              if (nv >= 4) then
                  if (ivet(1) <= 0 .or. ivet(1) > nspec) cycle
                  natom = natom + 1
                  if (size(atom) < natom) call resize_atoms(atom,natom+NMAXA)  ! check sulla dimensione di atom
                  atom(natom)%lab = trim(line2)
                  atom(natom)%ptab = zval(ivet(1))
                  atom(natom)%xc(:) = mod(vet(2:4),10.0)
!              
!                 cerca fattori termici 
                  select case (nv)
                     case (10:)   ! fattori anisotropi
                      if (.not.is_matrix) then   ! genera matrici per conversione uij->beta
                          rgmat = matrice_mreciproca(cellsh)
                          gmat = matrice_metrica(cellsh)
                          rcell = cella_reciproca(cellsh)
                          is_matrix = .true.
                      endif
!                     shelx convention:                    u11    u22   u33    u23     u13     u12 
                      atom(natom)%bij = beta_from_uij((/vet(6),vet(7),vet(8),vet(11),vet(10),vet(9)/),rgmat,rcell)
                      atom(natom)%biso = bequiv_from_beta(atom(natom)%bij,gmat)
                      klastb = natom   ! memorizza l'ultimo atomo con b positivo

                     case (6:9)   ! fattore isotropo
                      if (vet(6) < 0) then
                          if (klastb > 0) then
                              atom(natom)%biso = abs(atom(klastb)%biso*vet(6))
                          else
                              atom(natom)%biso = 0  ! non dovrebbe verificarsi
                          endif
                      else
                          atom(natom)%biso = b_from_u(vet(6))
                          klastb = natom   ! memorizza l'ultimo atomo con b positivo
                      endif

                     case (4:5)   ! fattore termico assente
                      call set_biso(atom(natom),atom(natom)%ptab)

                  end select
!
!                 cerca l'occupanza
                  if (nv >= 5) then
                      atom(natom)%och = abs(mod(vet(5),10.0))
                      !if (abs(vet(5)) > 10) then
                      !    atom(natom)%och = 1/nint(abs(vet(5))/10)
                      !else
                          !atom(natom)%och = 1 
                      !endif
                  else
                      !atom(natom)%ocry = 1.0
                      atom(natom)%och = 1.0
                  endif

              endif
          endif

         case (2)       ! cell
          call Getnum(line1,vet,ivet,iv)
          if (iv >= 7) then
              is_cell_shelx = .true.
              cellsh(:) = vet(2:7)
          endif
          
         case (4)       ! latt
          call s_to_i(line1,lattx,ier,lenv)
          if (lattx > 0) then
              icentsh = 1
          else
              icentsh = 0
              lattx = iabs(lattx)
          endif
          if (lattx == 1) then      ! primitive
              lattsh = 'P'
          elseif (lattx == 2) then  ! I lattice
              lattsh = 'I'
          elseif (lattx == 3) then  ! R lattice
              lattsh = 'R'
          elseif (lattx == 4) then  ! F lattice
              lattsh = 'F'
          elseif (lattx == 5) then  ! A lattice
              lattsh = 'A'
          elseif (lattx == 6) then  ! B lattice
              lattsh = 'B'
          elseif (lattx == 7) then  ! C lattice
              lattsh = 'C'
          endif
          
         case (5)       ! symm
          nsymop = nsymop + 1
          call string_to_symop(lower(line1),symop(nsymop),ier)

         case (6)       ! sfac: specie chimiche
          call get_words(line1,word(nv+1:),nword) 
          nv = nv + nword
          if (.not.cont_line) then    ! converti specie in Z
              !nspec = nv
              !call reallocate(zval,nv+nspec,.true.)
              call resize_array(zval,nv+nspec)
              do i=1,nv
                 znum = pxen_from_specie(word(i))
                 if (znum > 0) then
                     nspec = nspec + 1
                     zval(nspec) = znum
                 endif
              enddo
          endif

         case (42)       ! end
          exit
           
      end select
!
   enddo
!
!  Get space group from strings of simmetry operators
   call symop_gen(symop,nsymop,icentsh,lattsh)
   call spg_load(spg,symop=symop(:nsymop),sfound=sfound)
   if (.not.sfound) spg = init_spaceg_type()
!
   call resize_atoms(atom, natom)
!
   if (natom == 0) then
       call error%set('No coordinates available')
   else
       if (is_cell_shelx) then
           if (is_cell) then
!
!              Converti nella cella corrente
               call coord_in_newcell(atom,set_cell_type(cellsh),cella)
           else
               cella = set_cell_type(cellsh(:))
           endif
       endif
   endif
!
   end subroutine read_shelxfile

!------------------------------------------------------------------------------------------

   subroutine create_shelxfile(filename,atom,legm,cell,spg,lambda,ele,sname,pkgname)
   USE fileutil
   USE unit_cell
   USE spginfom
   USE elements
   USE atom_type_util
   USE connect_mod
   USE contacts
   USE ccryst
   USE strutil
   USE arrayutil
!
   implicit none
   character(len=*), intent(in)                              :: filename
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(bond_type), dimension(:), allocatable, intent(in)    :: legm
   type(cell_type), intent(in)                               :: cell
   type(spaceg_type),intent(in)                              :: spg
   real, intent(in)                                          :: lambda
   type(file_handle)                                         :: fshelx
   integer                                                   :: j_in
   type(element_type), dimension(:), allocatable, intent(in) :: ele
   character(len=*), intent(in)                              :: sname,pkgname
   type(element_type), dimension(:), allocatable             :: el
   character(len=3)                                          :: adv
   integer, dimension(size(atom))                            :: zval
   integer                                                   :: natom,nspec
   real, dimension(6)                                        :: uvet
   real, dimension(3,3)                                      :: rgmat
   real, dimension(6)                                        :: rcell
   logical                                                   :: is_hydro
   type(container_type), dimension(:), allocatable           :: connH
   integer                                                   :: hpos
   integer                                                   :: i, j
   integer, dimension(1)                                     :: izpki
   integer                                                   :: ns, posC, posH, z_val
!
   call fshelx%fopen(filename,'w')
   if (.not.fshelx%good()) return
   j_in = fshelx%handle()
!
   natom = numatoms(atom)
   zval(:) = atom%z()
!
!  Comment
   write(j_in,'(a/a/a)')'REM','REM Shelx file produced by '//pkgname//' on '//date_time_string(),'REM'
!
!  Title
   write(j_in,'(a)')'TITL '//trim(sname)
!
!  lambda, cell
   write(j_in,'(a,f10.5,3f10.5,3f9.3)')'CELL ',lambda,cell%get_par()
!
!  symmetry info
   z_val = Z_value(atom,spg%nsymop)
   write(j_in,'(a,f10.2,3f10.5,3f9.3)')'ZERR ',real(z_val),cell%get_sd()
   call prn_shelx(spg,j_in)
!
!  scattering factors
   nspec = numelem(ele)
   if (nspec > 0) then
!
!      create array el containg elements oredered as C,H,others
       call copy_elem(el,ele)
       ns = 0
       posC = is_element(el,C_at)
       if (posC > 0) then
           ns = ns + 1
           el(1) = ele(posC)
       endif
       posH = is_element(el,H_at)
       if (posH > 0) then
           ns = ns + 1
           el(2) = ele(posH)
       endif
       do i=1,nspec
          if (i /= posC .and. i /= posH) then 
              ns = ns + 1
              el(ns) = ele(i)
          endif
       enddo
       adv = 'no'
       write(j_in,'(a)',advance=adv)'SFAC '
       do i=1,nspec
          if (i == nspec) adv='yes'
          write(j_in,'(a)',advance=adv)adjustr(eleminfo(el(i)%z)%lab) ! convert charged in neutral species
       enddo
       adv = 'no'
       write(j_in,'(a)',advance=adv)'UNIT '
       do i=1,nspec
          if (i == nspec) adv='yes'
          write(j_in,'(i8)',advance=adv)nint(el(i)%nw)  ! i8 because 8 is len of lab
       enddo
!
!      Additional command for refinement
       write(j_in,'(a)')'L.S.       5'
       write(j_in,'(a)')'WGHT     0.1'
       write(j_in,'(a)')'FMAP       2'
       write(j_in,'(a)')'PLAN      25'
       write(j_in,'(a)')'FVAR     1.0'
!                                         
!      Atom list
       !rgmat = matrice_mreciproca(cella)
       rgmat = cell%get_r()
       rcell = cella_reciproca(cell%get_par())
       is_hydro = is_element(el,H_at) > 0
       if (is_hydro) call get_conn_hydrogens(connH,atom,natom,legm)  ! extract hydrogens
       do i=1,size(atom)
          if (zval(i) <= 1) cycle
          izpki = minloc(abs(el(:)%z-zval(i)))     ! good because el is a small array
          if (izpki(1) > 0) call atom_instruction(i)
          if (is_hydro) then   ! add hydrogen after the non-H atom
              do j=1,connH(i)%nat
                 hpos = connH(i)%pos(j)
                 izpki = minloc(abs(el(:)%z-zval(hpos)))
                 call atom_instruction(hpos)
              enddo
          endif
       enddo
   endif
   write(j_in,'(a)')'END'
   call fshelx%fclose()
!
   CONTAINS 
                                                     
       subroutine atom_instruction(pat)
       integer, intent(in) :: pat
       if (atom(pat)%bij(1) > 0) then
           uvet = u_from_bij(atom(pat)%bij,rgmat,rcell)   ! u11 u22 u33 u12 u13 u23
           write(j_in,'(a,i5,6f10.5," =")')atom(pat)%lab(1:4),izpki(1),atom(pat)%xc,      &
                 atom(pat)%ocry*atom(pat)%och+10,uvet(1),uvet(2)      ! ... u11 u22 
           write(j_in,'(6x,4f10.5)')uvet(3),uvet(6),uvet(5),uvet(4)   ! u33 u23 u13 u12
       else
           write(j_in,'(a,i5,5f10.5)')atom(pat)%lab(1:4),izpki(1),atom(pat)%xc,           &
                 atom(pat)%ocry*atom(pat)%och+10,u_from_b(atom(pat)%biso)
       endif
       end subroutine  atom_instruction

   end subroutine create_shelxfile

end module shelx_frm
