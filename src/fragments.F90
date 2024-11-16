MODULE rotationmod
!
!  Questo modulo contiene procedure per la gestione delle rotazioni
!
!S add_rotation(nrot,rotat,vax,veta)          Aggiunge rotazione alla lista delle rotazioni
!S sort_rotations(rot)                        Sort rotations according the number of associated free torsions
!S set_limit_for_rotat(rot,rrange)            Set bounds of torsion according to the mode
!S set_torsion_for_rotat(rot)                 Associa una torsione alla rotazione
!S set_torsion_from_atoms(tors,atom,cell,rot) Set torsion associated to rotation. Axis is non changed.
!F torsion_value_for_rotat(rot,atom)          Calcola angolo di torsione per la rotazione
!F ang_to_torsion(ang,tors0)                  Calcola torsione dall'angolo di rotazione 
!F torsion_to_ang(tors,tors0)                 Calcola angolo dalla torsione
!S print_rotation(rotat,codew)                Stampa la lista delle rotazioni
!F integer function numrotat(rot)             Number of allocated rotations
!F num_connected_rotation(rot)  result(num)   Number of rotations included one in another, useful to measure the flexibility
!S get_rotation_from_leg(atom,nrot,rotat)     Utilizza la connettività per individuare tutti i DOFs interni
!S set_rotations(rotat,natmax)                Data una rotazione definisce gli atomi da ruotare
!S get_conn_rotations(rot,kr,vet,n)           Get all associated rotations to kr rotation
!F rotation_position(rotat,axis)              Trova DOF nella lista
!S reallocate_rotation(vetr,n,savevet)        Modifica la dimensione della lista
!S compare_rotat(rot1,rot2)                   Controllo sulle rotazioni (generalmente non usata)
       
implicit none
       
type rotation_type
  integer, dimension(2)              :: pax       ! atomi che individuano l'asse di rotazione
  integer                            :: nat       ! numero di atomi da routare
  integer, dimension(:), allocatable :: pat       ! atomi da ruotare
  integer                            :: rcod      ! codice di affinamento
  integer, dimension(4)              :: ators = 0 ! atomi della torsione associata alla rotazione
  real                               :: torsval   ! valore iniziale dell'angolo di torsione
  real, dimension(6)                 :: trbound   ! limiti sulla torsione
  integer                            :: mode = 1  ! 2 = bimodal, 3 = trimodal 4 = bimodal and planar
end type

CONTAINS

   subroutine add_rotation(connt,nrot,rotat,vax,veta,atom,cell,rcode,rrange)
!
!  Aggiunge rotazione alla lista delle rotazioni
!
   USE nr
   USE Connect_mod
   USE atom_type_util
   USE unit_cell
   USE arrayutil
   type(container_type), dimension(:), intent(in)                :: connt
   integer, intent(inout)                                        :: nrot   ! numero di rotazioni incrementato qui
   type(rotation_type), dimension(:), allocatable, intent(inout) :: rotat  ! lista delle rotazioni
   integer, dimension(2), intent(in)                             :: vax    ! puntatori agli atomi dell'asse
   integer, dimension(:), intent(inout)                          :: veta   ! puntatori agli atomi da ruotare
   type(atom_type), dimension(:), intent(in)                     :: atom   ! list of atoms
   type(cell_type), intent(in)                                   :: cell
   integer, intent(in)                                           :: rcode
   real, intent(in), optional                                    :: rrange ! range for rotation
   integer                                                       :: nveta  
!
   nveta = size(veta)
   if (nveta >= 1) then
       nrot = nrot + 1                    ! incrementa il numero di rotazioni
       call reallocate_rotation(rotat,nrot)
!
!      Definisci la rotazione 
       rotat(nrot)%pax = vax        
       rotat(nrot)%nat = nveta       
       call new_array(rotat(nrot)%pat,nveta)
!
!      L'ordinamento degli atomi permette di migliorare l'accesso alla cache 
       !call indexx(veta(:),iord)
       !veta(:) = veta(iord)
!
       rotat(nrot)%pat = veta(:)
       rotat(nrot)%rcod = rcode
       call set_torsion_for_rotat(connt,rotat(nrot))
       rotat(nrot)%torsval =  torsion_value_for_rotat(rotat(nrot),atom,cell)
       if (present(rrange)) then
           call set_limit_for_rotat(rotat(nrot),rrange)
       else
           rotat(nrot)%trbound(1) = -180.0
           rotat(nrot)%trbound(2) = 180.0
       endif
   endif
!  
   end subroutine add_rotation

!----------------------------------------------------------------------------------------------------      

   subroutine sort_rotations(rot)
!
!  Sort rotations according the number of associated free torsions
!  Atoms in rotations should be ordered according they sequence in the chain
!
   USE arrayutil
   USE nr
   type(rotation_type), dimension(:), allocatable, intent(inout) :: rot
   integer                                                       :: nrot
   integer                                                       :: i,j
   integer, dimension(:), allocatable                            :: vetc,vord,wrot
   integer                                                       :: nc,at1,at2,vc0,ini,fin
   logical, dimension(:), allocatable                            :: skip
   type(container_type), dimension(:), allocatable               :: vcont
!
   nrot = numrotat(rot)
   if (nrot == 0) return
   allocate(vetc(nrot),wrot(nrot),vord(nrot))
   allocate(skip(nrot),source=.false.)
   call new_container(vcont,nrot)
   !vcont%nc = 0
   do i=1,nrot
      !nc = 1
      !vetc(1) = i
      if (skip(i)) cycle
      call get_conn_rotations(rot,i,vetc,nc)   ! get associated rotations
      !write(0,*)'ROT N.',i,vetc(:nc)
      !if (nc > 1) then
      if (nc > 0) then
          !do j=2,nc
          do j=1,nc
             at1 = rot(vetc(j))%pax(1)
             at2 = rot(vetc(j))%pax(2)
             !write(0,*)'NC n.',j,clocate1(rot(i)%pat,at1),clocate1(rot(i)%pat,at2)
             !wrot(j-1) = clocate1(rot(i)%pat,at1) + clocate1(rot(i)%pat,at2)
             wrot(j) = clocate1(rot(i)%pat,at1) + clocate1(rot(i)%pat,at2)
          enddo
!
!         Sort associated rotations by sequence in chain 
          call indexx(wrot(:nc),vord(:nc))
          vetc(:nc) = vetc(vord(:nc))
          skip(vetc(:nc)) = .true.
      endif
      call container_set(vcont(i),nc+1,[i,vetc(:nc)])
   enddo
!
!  Reset (vcont=0) all sequence included in un'other
   do i=1,nrot
      if (vcont(i)%nat > 0) then
          do j=1,nrot
             if (i == j) cycle
             if (vcont(j)%nat > 0) then
                 if (check_container(vcont(j)%pos,vcont(i)%pos)) then
                     vcont(i)%nat = 0
                     exit
                 endif
             endif
          enddo
      endif
   enddo
   !do i=1,nrot
   !   if (vcont(i)%nat > 0) write(0,*)'ROT n. ',i,vcont(i)%pos(:vcont(i)%nat)
   !enddo
!
!  Sort the sequence not included in other 
   call indexx(vcont%nat,vord(nrot:1:-1))
   vc0 = count(vcont%nat > 0)
   !write(0,*)'VORD=',vord(:vc0)
   vcont(:vc0) = vcont(vord(:vc0))
!
!  Now insert the other sequence in array vetc
   fin = 0
   do i=1,vc0
   !   write(0,*)'VC0=',vcont(i)%pos
      ini = fin + 1
      fin = ini + vcont(i)%nat - 1
      vetc(ini:fin) = vcont(i)%pos(:)
   enddo
   !write(0,*)'FINAL ORD:',vetc
   rot(:) = rot(vetc)
!
   end subroutine sort_rotations

!----------------------------------------------------------------------------------------------------

   subroutine sort_rotations1(rot)
!
!  Sort rotations according to the maxmium number of rotated atoms
!
   USE nr
   type(rotation_type), dimension(:), allocatable, intent(inout) :: rot
   integer                                                       :: nrot
   integer, dimension(:), allocatable                            :: ord
!
   nrot = numrotat(rot)
   if (nrot == 0) return
!
   allocate(ord(nrot))
   call indexx(rot%nat,ord)
   rot(nrot:1:-1) = rot(ord)
!
   end subroutine sort_rotations1

!----------------------------------------------------------------------------------------------------

   subroutine set_limit_for_rotat(rot,rrange)
!
!  Set bounds of torsion according to the mode
!
   use atom_type_util
   type(rotation_type), intent(inout) :: rot
   real, intent(in)                   :: rrange ! variability range for rotation
!
   if (rrange > 0) then
!TOFIX: write function to
!TOFIX: normalize trbound(1) and trbound(1), check order, split into 2 range 
!TOFIX: es. -220 -150 -> norm: 140 -150 -> inver: -150 140 -> split: -150 -180; 140 180
       rot%trbound(1) = rot%torsval - rrange
       rot%trbound(2) = rot%torsval + rrange
       if (rot%mode == 2) then
!
!          Check if angle is planar
           if (abs(mod(rot%torsval,180.0)) <= 30              &       ! trap angle close to 0 +/- n*180 
          .or. abs(abs(mod(rot%torsval,180.0))-180) <= 30) then       ! trap angle close to 180 or -180 +/- n*180
               rot%mode = 4
               !rot%trbound(3) = 180 + rot%trbound(1)
               !rot%trbound(4) = -180 + rot%trbound(2)
               rot%trbound(3) = norm_torsion(rot%trbound(1) + 180)
               rot%trbound(4) = norm_torsion(rot%trbound(2) + 180)
           else
               rot%trbound(3) = -rot%trbound(1)  ! es. 30 - 45 --> -30 - -45  or -40 - -10  --> 40 - 10
               rot%trbound(4) = -rot%trbound(2)
           endif
       else if (rot%mode == 3) then
           rot%trbound(3) = rot%trbound(1) + 120
           rot%trbound(4) = rot%trbound(2) + 120
           rot%trbound(5) = rot%trbound(1) - 120
           rot%trbound(6) = rot%trbound(2) - 120
       endif
   else
       rot%trbound(1) = -180.0
       rot%trbound(2) = 180.0
   endif
   end subroutine set_limit_for_rotat

!----------------------------------------------------------------------------------------------------

   subroutine set_torsion_for_rotat(connt,rot)
!
!  Associa ad una rotazione un'opportuna torsione
!
   USE arrayutil
   type(container_type), dimension(:), intent(in) :: connt
   type(rotation_type), intent(inout)             :: rot ! rotazione
   integer                                        :: i
   integer                                        :: kpos, rotated_at, not_rotated_at
   integer                                        :: pos1,pos2
!
   rot%ators(2) = rot%pax(1)
   rot%ators(3) = rot%pax(2)
!
!  Find rotated atom and not rotated atom but bound to different atom pax(1) and pax(2)
   rotated_at = 0
   not_rotated_at = 0
   pos1 = rot%pax(1)
   pos2 = rot%pax(2)
   do i=1,connt(pos1)%nat
      kpos = connt(pos1)%pos(i)
      if (kpos == pos2) cycle
      if (any(rot%pat == kpos)) then
          rotated_at = kpos
      else
          not_rotated_at = kpos
      endif
      exit
   enddo
!
   if (rotated_at == 0 .and. not_rotated_at > 0) then
       do i=1,connt(pos2)%nat
          kpos = connt(pos2)%pos(i)
          if (kpos == pos1) cycle
          if (any(rot%pat == kpos)) then
              rotated_at = kpos
              exit
          endif
       enddo
       rot%ators(1) = not_rotated_at
       rot%ators(4) = rotated_at
       return
   endif
   if (not_rotated_at == 0 .and. rotated_at > 0) then
       do i=1,connt(pos2)%nat
          kpos = connt(pos2)%pos(i)
          if (kpos == pos1) cycle
          if (.not.any(rot%pat == kpos)) then
              not_rotated_at = kpos
              exit
          endif
       enddo
       rot%ators(1) = rotated_at
       rot%ators(4) = not_rotated_at
       return
   endif
!
   if (not_rotated_at == 0 .or. rotated_at == 0) then
!
!      Impossibile set torsion
       rot%ators(1) = rot%pat(1)
       rot%ators(4) = rot%pat(1)
   endif
!
   end subroutine set_torsion_for_rotat

!----------------------------------------------------------------------------------------------------

   subroutine set_torsion_from_atoms(tors,atom,cell,rot)
!
!  Set torsion associated to rotation. Axis is non changed.
!  Tors must be a valid torsion to associate to rotation
!
   use unit_cell
   use atom_basic
   integer, dimension(4), intent(in)         :: tors
   type(atom_type), dimension(:), intent(in) :: atom
   type(cell_type), intent(in)               :: cell
   type(rotation_type), intent(inout)        :: rot
!
   if (rot%pax(1) == tors(2)) then
       rot%ators = tors
   elseif (rot%pax(1) == tors(3)) then
       rot%ators = tors(4:1:-1)
   endif
!
!  Reset mode
   rot%mode = 1
   rot%trbound(1) = -180.0
   rot%trbound(2) = 180.0
!
   rot%torsval =  torsion_value_for_rotat(rot,atom,cell)
!
   end subroutine set_torsion_from_atoms 

!----------------------------------------------------------------------------------------------------

   function torsion_value_for_rotat(rot,atom,cell) result(value)
!
!  Calcola angolo di torsione per la rotazione
!
   USE atom_type_util
   USE cgeom
   USE unit_cell
   type(rotation_type), intent(in)       :: rot   ! rotazione
   type(atom_type), intent(in), dimension(:) :: atom  ! atomi
   type(cell_type), intent(in)               :: cell
   real                                  :: value
   type(atom_type), dimension(4)             :: att
!
   att = atom(rot%ators)
   call frac_to_cart(att,cell%get_ortom())
   value =  Angle_Dihedral(att(1)%xc,att(2)%xc,att(3)%xc,att(4)%xc)
!
   end function torsion_value_for_rotat

!----------------------------------------------------------------------------------------------------
!corr
!corr   real function ang_to_torsion(ang,tors0)   result(tors)
!corr!
!corr!  Calcola l'angolo di torsione a partire dall'angolo di rotazione dell'asse e 
!corr!  dal valore iniziale della torsione
!corr!
!corr   real, intent(in) :: ang    ! angolo di rotazione dell'asse in gradi
!corr   real, intent(in) :: tors0  ! valore della torsione prima della rotazione
!corr!
!corr   tors = tors0 + ang
!corr   if (tors > 180) then
!corr       tors = tors - 360
!corr   elseif (tors < -180) then
!corr       tors = 360 + tors
!corr   endif
!corr!
!corr   end function ang_to_torsion
!corr
!----------------------------------------------------------------------------------------------------
!corr
!corr   real function torsion_to_ang(tors,tors0) result(ang)
!corr!
!corr!  Calcola l'angolo di rotazione dell'asse dalla torsione
!corr!
!corr   real, intent(in) :: tors,tors0
!corr!
!corr   ang = tors - tors0
!corr   if (ang > 180) then
!corr       ang = ang - 360
!corr   elseif (ang < -180) then
!corr       ang = 360 + ang
!corr   endif
!corr!
!corr   end function torsion_to_ang
!corr
!----------------------------------------------------------------------------------------------------

   subroutine print_rotation(lab,rotat,kpr,codew)
!
!  Stampa la lista delle rotazioni
!   
   USE atom_basic
   character(len=*), dimension(:), intent(in)                 :: lab
   type(rotation_type), dimension(:), allocatable, intent(in) :: rotat
   integer, intent(in)                                        :: kpr
   integer, intent(in)                                        :: codew
   integer                                                    :: nrot
   integer                                                    :: i
   character(len=:), allocatable                              :: strl
   character(len=100)                                         :: strt
   character(len=1), dimension(0:1)                           :: yn = (/'N','Y'/)
   integer                                                    :: icod
   integer                                                    :: nt1,nt2,nt3,nt4
   integer                                                    :: posc
   integer, parameter                                         :: MAXLENS = 100
!
   nrot = numrotat(rotat)
   if (nrot > 0) then
       if (codew > 0)then
           write(kpr,'(/2x,80("-")/15x,"List of Internal DOFs"/)')
           write(kpr,'(2x,a/2x,80("-"))')'   n.   refined             axis                             rotated atoms'
       endif
       do i=1,nrot
          strl = slabvet(rotat(i)%pat,lab)
          if (rotat(i)%rcod > 0) then
              icod = 1
          else
              icod = 0
          endif
          nt1 = rotat(i)%ators(1)
          nt2 = rotat(i)%ators(2)
          nt3 = rotat(i)%ators(3)
          nt4 = rotat(i)%ators(4)
          strt = trim(slabnum(lab(nt1),nt1))//':'//trim(slabnum(lab(nt2),nt2))           &
          //'-'//trim(slabnum(lab(nt3),nt3))//':'//trim(slabnum(lab(nt4),nt4))
          if (len(strl) > MAXLENS) then
              posc = index(strl(1:MAXLENS),',',back=.true.)  ! cut at ','
              write(kpr,'(1x,i5,6x,a,6x,a,t52,a)')i,yn(icod),trim(strt),trim(strl(1:posc))//'...'
          else
              write(kpr,'(1x,i5,6x,a,6x,a,t52,a)')i,yn(icod),trim(strt),trim(strl)
          endif
       enddo
   endif
!
   end subroutine print_rotation
   
!----------------------------------------------------------------------------------------------------

   integer function numrotat(rot)
!
!  Number of allocated rotations
!
   type(rotation_type), dimension(:), allocatable :: rot
!   
   if (allocated(rot)) then
       numrotat = size(rot)
   else
       numrotat = 0
   endif
!
   end function numrotat

!----------------------------------------------------------------------------------------------------

   integer function num_connected_rotation(rot)  result(num)
!
!  Number of rotations included one in another, useful to measure the flexibility
!
   USE arrayutil
   type(rotation_type), dimension(:), allocatable, intent(in) :: rot
   integer, dimension(:), allocatable                         :: vrot
   integer                                                    :: numrot
   integer                                                    :: i,j
!
   numrot = numrotat(rot)
   if (numrot > 0) then
       allocate(vrot(numrot), source=0)
       do i=1,numrot
          if (rot(i)%rcod > 0) then
              do j=1,numrot
                 if (j==i) cycle
                 if (rot(j)%rcod > 0) then
                     if (check_container(rot(i)%pat,rot(j)%pat)) then
                         vrot(i) = vrot(i) + 1
                     endif
                 endif
              enddo
          endif
       enddo
       num = sum(vrot)
   else
       num = 0
   endif
!
   end function num_connected_rotation

!----------------------------------------------------------------------------------------------------

   subroutine get_rotation_from_leg(atom,connt,bond,cell,nrot,rotat,sort)
!
!  Utilizza bond per individuare i DOF interni. 
!  La routine si basa sulla considerazione che un legame e' un DOF se
!  le catene di atomi nelle due direzioni non contengono gli stessi atomi
!
   USE Connect_mod
   USE atom_basic, only: atom_type
   USE unit_cell
   USE cgeom
   USE arrayutil
   type(atom_type), dimension(:), intent(in)                   :: atom
   type(container_type), dimension(:), intent(in)              :: connt
   type(bond_type), dimension(:), allocatable, intent(in)      :: bond
   type(cell_type), intent(in)                                 :: cell
   integer, intent(out)                                        :: nrot
   type(rotation_type), dimension(:), allocatable, intent(out) :: rotat
   logical, intent(in)                                         :: sort
   integer, dimension(size(connt))                             :: veta1,veta2,vetleg
   integer                                                     :: i
   integer                                                     :: nata1,nata2,rcode
   integer, dimension(2)                                       :: vax
   real, parameter                                             :: RTOLA = 2  ! tol to remove linear bond
   real, parameter                                             :: RTOLA1 = 7 ! tol to unfix linear bond
   real                                                        :: diffang
!
   nrot = 0
   do i=1,numbonds(bond)
      vetleg(:) = 0
      
      veta1(:) = 0
      nata1 = 1
      veta1(1) = bond(i)%n1
      call get_chain(connt,bond(i)%n1,bond(i)%n2,nata1,veta1)  ! catena in una direzione
!          
      if (nata1 == 1) cycle                            ! legame terminale
      vetleg(veta1(2:nata1)) = vetleg(veta1(2:nata1)) + 1
      veta2(:) = 0
      nata2 = 1
      veta2(1) = bond(i)%n2
      call get_chain(connt,bond(i)%n2,bond(i)%n1,nata2,veta2)  ! catena nell'altra direzione
      if (nata2 > 1) then
          vetleg(veta2(2:nata2)) = vetleg(veta2(2:nata2)) + 1
          if (.not.any(vetleg==2)) then
!
!             Ora routa il gruppo di atomi più piccolo
              vax = (/bond(i)%n1,bond(i)%n2/)
              rcode = 1
              if (nata1 < nata2) then             
                  if (connt(vax(1))%nat == 2) then  ! check for linear bond
                      diffang = abs(angleC(atom(vax(2))%xc,atom(vax(1))%xc,atom(veta1(2))%xc,cell%get_g())*rtod - 180)
                      if (diffang <= RTOLA) cycle  ! torsion is excluded
                      if (diffang <= RTOLA1) then
                          rcode = 0  ! unselect almost linear bond
                      else
                          rcode = 1
                      endif
                  endif
                  !write(0,*)'ANG2='//trim(atom(vax(2))%lab)//trim(atom(vax(1))%lab)//trim(atom(veta1(2))%lab),  &
                  !rtod*angleC(atom(vax(2))%xc,atom(vax(1))%xc,atom(veta1(2))%xc,cell%get_g()), &
                  !abs(angleC(atom(vax(2))%xc,atom(vax(1))%xc,atom(veta1(2))%xc,cell%get_g())*rtod - 180) <= RTOLA1,rcode

                  call add_rotation(connt,nrot,rotat,vax,veta1(2:nata1),atom,cell,rcode)
              else
                  if (connt(vax(2))%nat == 2) then  ! check for linear bond
                      diffang = abs(angleC(atom(vax(1))%xc,atom(vax(2))%xc,atom(veta2(2))%xc,cell%get_g())*rtod - 180)
                      if (diffang <= RTOLA) cycle  ! torsion is excluded
                      if (diffang <= RTOLA1) then
                          rcode = 0  ! unselect almost linear bond
                      else
                          rcode = 1
                      endif
                  endif
                  !write(0,*)'ANG2='//trim(atom(vax(1))%lab)//trim(atom(vax(2))%lab)//trim(atom(veta2(2))%lab),  &
                  !rtod*angleC(atom(vax(1))%xc,atom(vax(2))%xc,atom(veta2(2))%xc,cell%get_g()), &
                  !abs(angleC(atom(vax(1))%xc,atom(vax(2))%xc,atom(veta2(2))%xc,cell%get_g())*rtod - 180) <= RTOLA1,rcode
                  call add_rotation(connt,nrot,rotat,vax,veta2(2:nata2),atom,cell,rcode)
              endif
          endif
      endif
   enddo   
   if (sort) call sort_rotations(rotat)
!   
   end subroutine get_rotation_from_leg
  
!----------------------------------------------------------------------------------------------------

   subroutine set_rotations(connt,rotat,natmax)    !!!FIXME - unused
!
!  Calcola automaticamnte per ogni rotazione quali atomi ruotare.
!  Ruota sempre il gruppo di atomi più piccolo
!
   USE connect_mod
   USE arrayutil
   type(container_type), dimension(:), intent(in)  :: connt
   type(rotation_type), dimension(:), intent(inout) :: rotat
   integer, intent(in)                              :: natmax
   integer                                          :: i
   integer, dimension(natmax)                       :: veta1,veta2
   integer                                          :: nata1,nata2
!   
   do i=1,size(rotat)
      if (rotat(i)%nat == 0) then
          veta1(:) = 0
          nata1 = 1
          veta1(1) = rotat(i)%pax(1)
          call get_chain(connt,rotat(i)%pax(1),rotat(i)%pax(2),nata1,veta1)
!          
          veta2(:) = 0
          nata2 = 1
          veta2(1) = rotat(i)%pax(2)
          call get_chain(connt,rotat(i)%pax(2),rotat(i)%pax(1),nata2,veta2)
!
          if (nata1 < nata2) then
              nata1 = nata1 - 1
              rotat(i)%nat = nata1
              allocate(rotat(i)%pat(nata1))
              rotat(i)%pat = veta1(2:nata1+1)
          else
              nata2 = nata2 - 1
              rotat(i)%nat = nata2
              allocate(rotat(i)%pat(nata2))
              rotat(i)%pat = veta2(2:nata2+1)          
          endif          
      endif
   enddo
!   
   end subroutine set_rotations
   
!----------------------------------------------------------------------------------------------------      

   recursive subroutine get_conn_rotations(rot,kr,vet,n)
!
!  Get all associated rotations to kr rotation
!
   USE arrayutil
   type(rotation_type), dimension(:), allocatable, intent(in) :: rot
   integer, intent(in)                                        :: kr
   integer, dimension(:), intent(out)                         :: vet
   integer, intent(inout)                                     :: n
   integer                                                    :: i !,nrotmin,diff,mindiff,n1
!
   n = 0
   do i=1,numrotat(rot)
      if (i == kr) cycle
      if (check_container(rot(kr)%pat,rot(i)%pat)) then
          n = n + 1
          vet(n) = i
      endif
   enddo
!
   end subroutine get_conn_rotations

!----------------------------------------------------------------------------------------------------

   function rotation_position(rotat,axis) result(pos)    
!
!  Cerca la posizione (pos) di un DOF nella lista a partire dall'asse di rotazine
!   
   USE arrayutil
   USE progtype
   type(rotation_type), dimension(:), intent(in) :: rotat
   integer, dimension(2), intent(in)             :: axis
   integer                                       :: pos
   integer                                       :: i
!
   pos = 0
   do i=1,size(rotat)
      if (check_container(rotat(i)%pax,axis)) then
          pos = i
          exit
      endif
   enddo
!
   end function rotation_position

!----------------------------------------------------------------------------------------------------
!corr
!corr   function find_torsion(rotat,tors,bond)
!corr!
!corr!  Find a torsion in the array of rotations
!corr!
!corr   type(rotation_type), dimension(:), intent(in) :: rotat
!corr   integer, dimension(4), intent(in)             :: tors
!corr   type(bond_type), dimension(:),  intent(in)    :: bond   
!corr   integer                                       :: pos
!corr!
!corr   pos = rotation_position(rotat,tors(2:3))
!corr   if (pos == 0) return
!corr!   
!corr   end function find_torsion
!corr
!----------------------------------------------------------------------------------------------------

   subroutine reallocate_rotation(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo rotation_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(rotation_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)              :: n
   logical, optional, intent(in)    :: savevet
   logical                          :: savev
   integer                          :: nv
   type(rotation_type), allocatable                :: vsav(:)
   integer                          :: nsav
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
   end subroutine reallocate_rotation

END MODULE rotationmod

MODULE fragmentmod
!
!  Questo modulo contiene procedure per la gestione dei frammenti
!
!S add_fragment(nfrag,fragm,atom,veta)              Aggiunge un frammento alla lista dei frammenti fragm
!F numfragments(fragm)                              Numero di frammenti
!S print_fragment(fragm)                            Stampa la lista dei frammenti 
!S resize_fragments(vetr,n,savevet)                 resize array of fragments
!S new_fragments(vetr,n,savevet)                    create new array of fragments
!S clear_fragments(vetr)                             Delete all fragments
!S push_back_fragment(arr,val)                      Adds a new fragment at the end of the array
!F fragment_pos(fragm,posat)                        Find fragment containing atoms in vet
!F fragment_natom(fragm,posat)                      Da quanti atomi e' formato il frammento contenente l'atomo posat
!S get_fragments_from_leg                           Utilizza la connettivit� per generare frammenti 
!S set_atm_in_frag(fragm,atom)                      Modifica le coordinate atomiche dei frammenti
!S set_fragment_position(fragm,pos,atom)            Posiziona il baricentro del frammento in pos
!S regroup_fragment(fragm,atom)                     Raggruppa i frammenti
!S overlay_fragment(frag1,frag2,atom1,atom2)        Overlay frag2 to frag1
!S fragment_distance_sym(frag1,frag2,atom,distmin,kmin,ktransmin) Calcola la distanza tra 2 frammenti restituendo l'operatore corrispondente al minimo
!S get_legm_from_fragment(fragm,legmtot,legmfrag)   Estrai i legami presenti in un frammento a partire da tutti i legami
!F is_angle_ok(na,nb,legm,numleg,atom,anglim)       Controlla se gli angoli formati dal legame n1-n2 sono ragionevoli
!S get_branch(atom,connt,legm,nconnbr,connbr)       Genera tutte le possibili ramificazioni
!S fragments_in_cell(fragm,atom)                    Translate all fragments in cell
!S frag_set_centre(frag,atom,conn)                  Set origin for rotation
!S duplicate_fragment(atom,bond,elem,frag,nfrag,ncopies)   Duplicate a fragment

USE atom_basic, only:atom_type

implicit none

type fragment_type
  real, dimension(3)                         :: xt       ! traslazione del frammento
  real, dimension(3)                         :: xr       ! rotazione del frammento
  integer, dimension(:), allocatable         :: pos      ! num. d'ordine degli atomi del frammento
  integer                                    :: nat      ! num. di atomi nel frammento
  integer, dimension(6)                      :: rcod     ! codici di affinamento
  type(atom_type), dimension(:), allocatable :: atm0     ! posizione iniziale degli atomi nel frammento
  real, dimension(3)                         :: tlow     ! limiti inferiori sulla traslazione
  real, dimension(3)                         :: thigh    ! limiti superiori sulla traslazione
  real, dimension(2)                         :: rbound   ! limiti sulla rotazione
  integer                                    :: rat  = 0 ! rotation around rat, 0 for rotation around center of mass

contains

  procedure :: ref => frag_refined

end type

interface fragment_in_cell
  module procedure fragment_in_cells, fragment_in_cellv
end interface

private :: fragment_pos_s, fragment_pos_v
interface fragment_pos
  module procedure fragment_pos_s, fragment_pos_v
end interface

CONTAINS


   subroutine add_fragment(fragm,atom,veta)
!
!  Aggiunge un frammento alla lista dei frammenti fragm
!
   USE atom_type_util
   type(fragment_type), dimension(:), allocatable, intent(inout) :: fragm  ! lista dei frammenti
   type(atom_type), dimension(:), intent(in)                     :: atom   ! gli atomi
   integer, dimension(:), intent(in)                             :: veta   ! atomi che faranno parte del frammento
   integer                                                       :: nveta  
   integer                                                       :: nfrag
!
   nveta = size(veta)
   nfrag = numfragments(fragm)
   if (nveta >= 1) then
       nfrag = nfrag + 1                  ! incrementa il numero di frammenti
       call resize_fragments(fragm,nfrag) ! espandi la lista
!
!      Definisci il frammento
       fragm(nfrag)%nat = nveta
       allocate(fragm(nfrag)%pos(nveta),fragm(nfrag)%atm0(nveta))
       fragm(nfrag)%pos = veta(:nveta)        
       fragm(nfrag)%atm0 = atom(veta(:nveta))
       fragm(nfrag)%xt = baricentro(atom(fragm(nfrag)%pos))
       fragm(nfrag)%xr = 0.0
       fragm(nfrag)%rcod(1:3) = 1       ! affina la traslazione
       if (fragm(nfrag)%nat > 1) then   ! non affinare la rotazione per frammenti con un solo atomo
           fragm(nfrag)%rcod(4:6) = 1      
       else
           fragm(nfrag)%rcod(4:6) = 0 
       endif
       fragm(nfrag)%tlow(:) = 0.0
       fragm(nfrag)%thigh(:) = 1.0
       fragm(nfrag)%rbound(1) = 0.0
       fragm(nfrag)%rbound(2) = 1.0       
   endif
!  
   end subroutine add_fragment
   
!----------------------------------------------------------------------------------------------------   

   integer function numfragments(fragm)
   type(fragment_type), dimension(:), allocatable, intent(in) :: fragm
!
   if (allocated(fragm)) then
       numfragments = size(fragm)
   else
       numfragments = 0
   endif
!
   end function numfragments

!----------------------------------------------------------------------------------------------------   

   subroutine get_fragments(atom,cell,legm,nfrag,fragm)
   USE Connect_mod
   USE unit_cell
   USE arrayutil
   type(atom_type), dimension(:)                                 :: atom
   type(cell_type), intent(in)                                   :: cell
   type(bond_type), dimension(:), allocatable, intent(in)        :: legm 
   integer, intent(out)                                          :: nfrag
   type(fragment_type), dimension(:), allocatable, intent(out)   :: fragm
   type(container_type), dimension(:), allocatable              :: connt
!
   call bond_to_connect(size(atom),legm,connt)
   call get_fragments_from_leg(atom,cell,legm,connt,nfrag,fragm)
!
   end subroutine get_fragments

!----------------------------------------------------------------------------------------------------   

   logical function frag_refined(frag)
   class(fragment_type), intent(in) :: frag
   frag_refined = any(frag%rcod > 0)
   end function frag_refined

!----------------------------------------------------------------------------------------------------   

   subroutine get_fragments_from_leg(atom,cell,legmi,connt,nfrag,fragm)
!
!  Utilizza la connettivita' per generare tutti i frammenti dal  modello strutturale atm
!   
   USE Connect_mod
   USE unit_cell
   USE arrayutil
   type(atom_type), dimension(:)                                    :: atom
   type(cell_type), intent(in)                                      :: cell
   type(bond_type), dimension(:), allocatable, intent(in), optional :: legmi
   type(container_type), dimension(:), intent(in)                  :: connt
   integer, intent(out)                                             :: nfrag
   type(bond_type), dimension(:), allocatable                       :: legm
   type(fragment_type), dimension(:), allocatable, intent(out)      :: fragm
   integer, dimension(size(atom))                                   :: vatom,veta1,veta2,vetf
   integer                                                          :: nata1,nata2
   integer                                                          :: i,j
   integer                                                          :: nf
   integer                                                          :: natom
   integer                                                          :: nleg
!
   vatom(:) = 0  ! indica a quale frammento appartiene l'atomo
   nfrag = 0
   natom = size(atom)
!
!  Cerca atomi non connessi e associa ad ognuno di essi un frammento
   do i=1,natom
      if (connt(i)%nat == 0) then
          nfrag = nfrag + 1
          vatom(i) = nfrag
      endif
   enddo   
!
!  genera legm se assente
   if (.not.present(legmi)) then
       call connect_to_leg(connt,atom,cell%get_g(),legm,nleg)
   else
       call copy_bonds(legm,legmi)
       nleg = numbonds(legm)
   endif
!
!  Per ogni legame ricostruisci la catena che da esso parte   
   do i=1,nleg
      if (vatom(legm(i)%n1) > 0) cycle                  ! atomo gia' considerato ... salta
      nfrag = nfrag + 1
!
!     Considera catena in una direzione
      veta1(:) = 0
      nata1 = 1
      veta1(1) = legm(i)%n1
      call get_chain(connt,legm(i)%n1,legm(i)%n2,nata1,veta1)  
      vatom(veta1(:nata1)) = nfrag
!
!     Considera catena nell'altra direzione
      veta2(:) = 0
      nata2 = 1
      veta2(1) = legm(i)%n2
      call get_chain(connt,legm(i)%n2,legm(i)%n1,nata2,veta2)  ! catena nell'altra direzione
      vatom(veta2(:nata2)) = nfrag        
   enddo
!
!  Genera i frammenti
   do i=1,nfrag
      nf = 0
      do j=1,natom
         if(vatom(j) == i) then
            nf = nf + 1
            vetf(nf) = j
         endif
      enddo
      call add_fragment(fragm,atom,vetf(:nf))
   enddo
!   
   end subroutine get_fragments_from_leg
              
!----------------------------------------------------------------------------------------------------

   subroutine print_fragment(fragm,atom,kpr)
!
!  Stampa la lista dei frammenti 
!   
   USE atom_basic
   type(fragment_type), dimension(:), intent(in) :: fragm
   type(atom_type), dimension(:), intent(in)     :: atom
   integer, intent(in)                           :: kpr
   integer                                       :: nfrag
   character(len=:), allocatable                 :: strl
   integer                                       :: i
   integer, parameter                            :: MAXLENS = 100
   integer                                       :: posc
!
   nfrag = size(fragm)
   if (nfrag > 0) then
       write(kpr,'(/2x,80("-")/15x,"List of fragments"/)')
       write(kpr,'(2x,a/2x,80("-"))')'   n.  Refined DOFs     Origin for rotation    Atoms'
       do i=1,nfrag
          if (fragm(i)%rat == 0) then
              if (fragm(i)%nat == 1) then
                  write(kpr,'(1x,i5,5x,i3,8x,a14)', advance='no')i,sum(fragm(i)%rcod),'-'
              else
                  write(kpr,'(1x,i5,5x,i3,8x,a14)', advance='no')i,sum(fragm(i)%rcod),'centre of mass'
              endif
          else
              write(kpr,'(1x,i5,5x,i3,8x,a14)', advance='no')i,sum(fragm(i)%rcod),trim(atom(fragm(i)%rat)%lab)
          endif
!       
!         Concatena le label in un'unica stringa       
          strl = slabvet(fragm(i)%pos,atom%lab)
!
!         Se la stringa e' troppo lunga aggiungi dei puntini          
          if (len(strl) > MAXLENS) then 
              posc = index(strl(1:MAXLENS),',',back=.true.)  ! cut at ','
              !write(6,'(1x,i5,5x,i3,t30,a,t130,2x,a)')i,sum(fragm(i)%rcod),strl(1:posc)//'...',origin
              write(kpr,'(13x,a)')strl(1:posc)//'...'
          else
              write(kpr,'(13x,a)')trim(strl)
          endif
       enddo
   endif
!
   end subroutine print_fragment

!----------------------------------------------------------------------------------------------------

   subroutine resize_fragments(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo fragment_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(fragment_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)              :: n
   logical, optional, intent(in)    :: savevet
   logical                          :: savev
   integer                          :: nv
   type(fragment_type), allocatable                :: vsav(:)
   integer                          :: nsav
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
!          nsav contiene qual � la porzione di vetr da salvare
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
   end subroutine resize_fragments  

!----------------------------------------------------------------------------------------------------

   subroutine push_back_fragment(arr,val)
!
!  Adds a new fragment at the end of the array
!
   type(fragment_type), dimension(:), allocatable, intent(inout) :: arr
   type(fragment_type), intent(in)                               :: val
   integer                                                       :: ndim
   ndim = numfragments(arr)
   call resize_fragments(arr,ndim+1)
   arr(ndim+1) = val
   end subroutine push_back_fragment

!----------------------------------------------------------------------------------------------------

   subroutine new_fragments(vetr,n)
!
!  Create new fragments
!
   type(fragment_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                             :: n

   if (n < 0) return
   if (numfragments(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_fragments

!----------------------------------------------------------------------------------------------------

   subroutine clear_fragments(vetr)
!
!  Delete all fragments
!
   type(fragment_type), allocatable, intent(inout) :: vetr(:)
!
   if (allocated(vetr)) deallocate(vetr)
!
   end subroutine clear_fragments

!----------------------------------------------------------------------------------------------------

   integer function fragment_pos_s(fragm,posat) result(posfrag)
!
!  Individua a quale frammento appartine l'atomo posat  
!
   type(fragment_type), dimension(:), allocatable, intent(in) :: fragm
   integer, intent(in)                                        :: posat
   integer                                                    :: i
!   
   posfrag = 0
   do i=1,numfragments(fragm)
      if (any(fragm(i)%pos == posat)) then
          posfrag = i
          exit
      endif
   enddo
!   
   end function fragment_pos_s

 !----------------------------------------------------------------------------------------------------

   integer function fragment_pos_v(fragm,vet) result(posfrag)
!
!  Find fragment containing the atoms in vet. Result is 0 if atoms are in different fragments
!
   type(fragment_type), dimension(:), allocatable, intent(in) :: fragm
   integer, dimension(:), intent(in)                          :: vet
   integer                                                    :: i,posf
!   
   posfrag = 0
   posf = fragment_pos_s(fragm,vet(1))
   do i=2,size(vet)
      if (posf /= fragment_pos_s(fragm,vet(i))) return
   enddo
   posfrag = posf
!   
   end function fragment_pos_v
  
!----------------------------------------------------------------------------------------------------

   integer function fragment_natom(fragm,posat)   result(natf)
!
!  Individua da quanti atomi e' formato il frammento contenente l'atomo posat
!
   type(fragment_type), dimension(:), allocatable, intent(in) :: fragm
   integer, intent(in)                                        :: posat
   integer                                                    :: frpos
!   
!  Prendi in quale frammento � posizionato
   frpos = fragment_pos(fragm,posat)
!
   if (frpos > 0) then
       natf = fragm(frpos)%nat
   else
       natf = 0
   endif
!
   end function fragment_natom

!----------------------------------------------------------------------------------------------------

   subroutine set_atm_in_frag(fragm,atom)
!
!  Modifica le coordinate atomiche dei frammenti
!
   USE atom_type_util
   type(fragment_type), dimension(:), intent(inout) :: fragm
   type(atom_type), dimension(:)                    :: atom
   integer                                          :: i
!
   do i=1,size(fragm)
      fragm(i)%atm0 = atom(fragm(i)%pos)
      fragm(i)%xt = baricentro(atom(fragm(i)%pos))
      fragm(i)%xr = 0.0   
   enddo
!   
   end subroutine set_atm_in_frag
   
!----------------------------------------------------------------------------------------------------

   subroutine set_fragment_position(fragm,xpos,atom,kat,modifi)
!
!  Move fragment in xpos
!   
   USE atom_type_util
   type(fragment_type), intent(inout)           :: fragm
   real, dimension(3)                           :: xpos
   type(atom_type), dimension(:), intent(inout) :: atom
   integer, intent(in)                          :: kat
   logical, intent(in), optional                :: modifi 
   logical                                      :: modif
   real, dimension(3)                           :: xtra
   type(atom_type), dimension(fragm%nat)        :: atomf
!
   if (present(modifi)) then
       modif = modifi
   else
       modif = .true.
   endif
!
!  Isola gli atomi del frammento in atomf
   atomf = atom(fragm%pos)
!
!  Calcola lo spostamento da eseguire sul frammento
   if (kat == 0) then
       xtra = xpos - baricentro(atomf)
   else
       xtra = xpos - atom(kat)%xc
   endif
!
!  Trasla gli atomi del frammento   
   call translate_atoms(atomf,xtra)
!
!  Aggiorna le coordinate atomiche
   atom(fragm%pos) = atomf
!
!  Aggiorna il frammento   
   if(modif)then
      fragm%atm0 = atomf
      fragm%xt = xpos
   endif
!   
   end subroutine set_fragment_position
   
!----------------------------------------------------------------------------------------------------   

   subroutine regroup_fragment(fragm,atom,cell,spg,findbond)
!
!  Raggruppa i frammenti
!
   USE atom_type_util
   USE cgeom
   USE nr
   USE unit_cell
   USE spginfom
   type(fragment_type), dimension(:), allocatable, intent(inout) :: fragm
   type(atom_type), dimension(:), intent(inout)     :: atom
   type(cell_type), intent(in)                      :: cell
   type(spaceg_type), intent(in)                    :: spg
   logical, intent(in), optional                    :: findbond
   type(atom_type), dimension(size(atom))           :: atmop
   integer, dimension(size(fragm)) :: ford,fordold
   type(fragment_type) :: fragtot
   integer                                          :: nfrag
   integer                                          :: i,j
   real                                             :: distb
   logical                                          :: kpr=.false.
   integer, parameter                               :: kpru = 0
   integer                                          :: nblock
   integer                                          :: koper
   integer                                          :: natf
   real, dimension(3)                               :: ktra
   logical, dimension(size(fragm))                  :: blocked
   logical                                          :: blockedt
   integer                                          :: firstb
   real, dimension(size(fragm))                     :: distbv
   real, dimension(size(fragm),3)                   :: ktrav
   integer, dimension(size(fragm))                  :: koperv
   integer, dimension(size(fragm))                  :: nordf
   integer, dimension(1)                            :: posf
   integer                                          :: ntf,jf,jpos
   logical                                          :: findbond1,findbd
!
   if (present(findbond)) then
       findbond1 = findbond
   else
       findbond1 = .true.
   endif
!
   if (kpr) write(kpru,*)'inizio regroup'
!
   nfrag = numfragments(fragm)
!   
!  Ordina frammenti in base al numero di atomi
   call indexx(fragm(:)%nat,ford); ford(:) = ford(nfrag:1:-1)
   fragm(:) = fragm(ford)
   do i=1,nfrag   ! salvo il vecchio ordine
      fordold(ford(i)) = i
   enddo
!
!  Individua frammenti bloccati
   nblock = 0
   firstb = 0
   do i=1,nfrag
      if (sum(fragm(i)%rcod(1:3)) == 0) then  ! il frammento non deve traslare e restare fisso
          if (nblock == 0) firstb = i ! segnala il primo bloccato
          nblock = nblock + 1         ! conta i frammenti bloccati
          blocked(i) = .true.
      else
          blocked(i) = .false.
      endif
      if (kpr) write(kpru,*)'frag n.',i,fragm(i)%nat,'atoms',fragm(i)%pos
   enddo
!
!  Fa in modo che il primo frammento sia un frammento bloccato
   if (nblock == 0) then
       blocked(1) = .true.
   else
       if (firstb /= 1) then
           call swap_fragment(fragm(1),fragm(firstb))
           blockedt = blocked(1)
           blocked(1) = blocked(firstb)
           blocked(firstb) = blockedt
       endif
   endif
!   
   call copy_fragment(fragm(1),fragtot)
   do i=2,nfrag
      if (blocked(i)) call join_fragment(fragm(i),fragtot)
   enddo
!
   do i=2,nfrag
!   
!     Calcola distanza tra 2 frammenti
      ntf = 0
      do j=1,nfrag
         if (.not.blocked(j)) then
             ntf = ntf + 1
!
!            disable findbond in case of atoms in same site            
             if (fragm(j)%nat == 1 .and. (atom(fragm(j)%pos(1))%och /= 1.0)) then
                 findbd = .false.
             else
                 findbd = findbond1
             endif
!
             call fragment_distance_sym(fragtot,fragm(j),atom(fragtot%pos),atom(fragm(j)%pos),distb,koper,ktra,.false.,cell,spg)
             distbv(ntf) = distb
             koperv(ntf) = koper
             ktrav(ntf,:) = ktra
             nordf(ntf) = j
             if (kpr) write(kpru,'(a,g12.3,a,3f10.2,a,i4,a,i4)')'dist=',distb,' tra=',ktra,' frag=',j,' op=',koper
         endif
      enddo
!      
!     Avvicina fragm(i)
      if (ntf > 0) then
          posf(:) = minloc(distbv(:ntf))
          jpos = posf(1)
          jf = nordf(posf(1))
          natf = fragm(jf)%nat
          atmop(:natf) = atom(fragm(jf)%pos)
          if (kpr) then
              write(kpru,*)'traslo frag.',jf,' di ',ktrav(jpos,:),' oper=',koperv(jpos)
              write(kpru,*)'frag. ',jf,atmop(1)%xc
          endif
          !call applica_sym_oper(koperv(jpos),atmop(:natf))  ! applica operatore k al frammento
          call apply_sym_oper(atmop(:natf),spg%symop(koperv(jpos)))  ! applica operatore k al frammento
          call translate_atoms(atmop(:natf),ktrav(jpos,:))    ! applica traslazione
          atom(fragm(jf)%pos) = atmop(:natf) 
      else
          jf = i
      endif
      blocked(jf) = .true.
!      
!     Ingloba frag(i) in fragtot
      !if (kpr) write(0,*)'jf=',jf
      call join_fragment(fragm(jf),fragtot)
   enddo
!
!  Ripristina l'ordine iniziale
   if(nblock > 0 .and. firstb /= 1) call swap_fragment(fragm(firstb),fragm(1))
   fragm(:) = fragm(fordold)
!   
   if (kpr) write(kpru,*)'fine regroup'
!
   end subroutine regroup_fragment

!----------------------------------------------------------------------------------------------------   

   subroutine overlay_fragment(frag1,frag2,atom1,atom2,cell,spg)
!
!  Overlay frag2 to frag1
!
   USE atom_type_util
   USE unit_cell
   USE spginfom
   type(fragment_type), dimension(:), intent(in)    :: frag1 ! fixed list of fragments
   type(fragment_type), dimension(:), intent(inout) :: frag2 ! these fragments are moved searching the best overlap with frag1
   type(atom_type), dimension(:), intent(in)        :: atom1 ! atoms in frag1
   type(atom_type), dimension(:), intent(inout)     :: atom2 ! atoms in frag2
   type(cell_type), intent(in)                      :: cell
   type(spaceg_type), intent(in)                    :: spg
   integer                                          :: nfrag1, nfrag2
   integer                                          :: i,j
   real                                             :: distb, distmin
   integer                                          :: koper, kopermin
   real, dimension(3)                               :: ktra, ktramin
   type(atom_type), dimension(size(atom2))          :: atomp
   integer                                          :: natf
!
   nfrag1 = size(frag1)
   nfrag2 = size(frag2)
!   write(0,*)'INIZIO OVERLAY',frag1%nat,frag2%nat
   do i=1,nfrag2     ! loop on not fixed fragments
      distmin = huge(1.0)
      do j=1,nfrag1  ! loop on fixed fragments
         call fragment_distance_sym(frag1(j),frag2(i),atom1(frag1(j)%pos),atom2(frag2(i)%pos),distb,koper,ktra,.false.,cell,spg)
!         write(0,'(a,2i4,a,i4,a,3f8.1,a,f8.3)')'TRY =',i,j,' oper=',koper,' tra=',ktra,' dist=',distb
         if (distb < distmin) then
             kopermin = koper
             ktramin(:) = ktra(:)
             distmin = distb
!             write(0,'(4x,a,2i4,a,i4,a,3f8.1,a,f8.3)')'MIN for =',i,j,' oper=',kopermin,' tra=',ktramin,' dist=',distmin
         endif
      enddo
      natf = frag2(i)%nat
      atomp(:natf) = atom2(frag2(i)%pos)
!      write(0,'(6x,a,i4,a,i4,a,3f8.1,a,f8.3)')'MOVE ',i,' oper=',kopermin,' tra=',ktramin,' dist=',distmin
      call apply_sym_oper(atomp(:natf),spg%symop(kopermin))  ! applica operatore k al frammento
      call translate_atoms(atomp(:natf),ktramin(:))            ! applica traslazione
      atom2(frag2(i)%pos) = atomp(:natf) 
   enddo
!
   end subroutine overlay_fragment

!----------------------------------------------------------------------------------------------------   

   subroutine copy_fragment(frag1,frag2)
!   
!  Copy frag1 in frag2
!
   USE arrayutil
   USE atom_type_util
   type(fragment_type), intent(in)  :: frag1
   type(fragment_type), intent(out) :: frag2
   integer                          :: natf
!
   natf = frag1%nat
   call new_array(frag2%pos,natf)
   call new_atoms(frag2%atm0,natf)
   frag2 = frag1
!
   end subroutine copy_fragment

!----------------------------------------------------------------------------------------------------   

   subroutine swap_fragment(frag1,frag2)
!
!  Scambia frammento 1 con 2
!
   type(fragment_type), intent(inout)  :: frag1,frag2
   type(fragment_type)                 :: fragtemp
!   
   call copy_fragment(frag1,fragtemp)
   call copy_fragment(frag2,frag1)
   call copy_fragment(fragtemp,frag2)
!
   end subroutine swap_fragment

!----------------------------------------------------------------------------------------------------   

   subroutine join_fragment(frag1,frag2)
!   
!  Combina frag1+frag2 in frag2
!
   type(fragment_type), intent(in)    :: frag1
   type(fragment_type), intent(inout) :: frag2
   type(fragment_type)                :: frag_temp
   integer                            :: nat1,nat2,ntot
!
   nat1 = frag1%nat
   nat2 = frag2%nat
   ntot = nat1 + nat2
   call copy_fragment(frag2,frag_temp)
   deallocate(frag2%pos,frag2%atm0)
   allocate(frag2%pos(ntot),frag2%atm0(ntot))
   frag2%nat = ntot
   frag2%pos(1:nat1) = frag1%pos
   frag2%atm0(1:nat1) = frag1%atm0
   frag2%pos(nat1+1:ntot) = frag_temp%pos
   frag2%atm0(nat1+1:ntot) = frag_temp%atm0
!
   end subroutine join_fragment

!----------------------------------------------------------------------------------------------------   

   subroutine fragment_distance_sym(frag1,frag2,atm1,atm2,distmin,kmin,ktransmin,findbond,cell,spg)
!
!  Calcola la distanza tra 2 frammenti restituendo l'operatore corrispondente al minimo
!
   USE CGEOM
   USE atom_type_util
   USE connect_mod
   USE unit_cell
   USE spginfom
   type(fragment_type), intent(in)           :: frag1,frag2
   real, intent(out)                         :: distmin      ! distanza minima
   integer, intent(out)                      :: kmin         ! operatore corrispondente al minimo
   real, dimension(3), intent(out)           :: ktransmin    ! traslazione corrispondente al minimo
   logical, intent(in)                       :: findbond     ! favor new bond between fragments
   type(cell_type), intent(in)               :: cell
   type(spaceg_type), intent(in)             :: spg
   type(atom_type), dimension(:), intent(in) :: atm1,atm2
   type(atom_type), dimension(size(atm2))    :: atm2s
   real, dimension(frag1%nat)                :: vdist, vdist1, vdist2, distv
   type(op_type), dimension(frag1%nat)       :: koper, koper1, koper2
   real                                      :: dista1,dista2,distb,distss
   integer                                   :: i,j,k
   real, dimension(3)                        :: ktrans
   real, dimension(3)                        :: xpos1,xpos2a
   integer                                   :: nat1,nat2
   integer, dimension(1)                     :: idloc
   integer, dimension(size(atm1))            :: zval1
   integer, dimension(size(atm2))            :: zval2
   integer                                   :: btype
   real                                      :: distab
   logical                                   :: conn_found
   real                                      :: dmed,ddmed
   integer                                   :: nmed  
   integer                                   :: overlap, maxoverlap
   integer :: nconn_found
!#define PRINT_INFO
#if defined(PRINT_INFO)
   integer, parameter              :: kpru = 71
#endif
!
!  prendi gli atomi dei frammenti
!corr   atm1(:) = atom(frag1%pos)
!corr   atm2(:) = atom(frag2%pos)
   nat1 = frag1%nat
   nat2 = frag2%nat
!corr   zval1(:) = atomic_number(atm1)
!corr   zval2(:) = atomic_number(atm2)
   zval1(:) = atm1%z()
   zval2(:) = atm2%z()
#if defined(PRINT_INFO)
   write(kpru,'(60("=")/,a,2i5)')'Atoms in frag.',nat1,nat2
#endif
!
   kmin = 1
   ktransmin = (/1.0,1.0,1.0/)
   distmin = huge(1.0)
   conn_found = .false.
   maxoverlap = 0
   do k=1,spg%nsymop
      atm2s(:) = atm2(:)
      !call applica_sym_oper(k,atm2s)  ! applica operatore k al frammento 2
      call apply_sym_oper(atm2s,spg%symop(k))  ! applica operatore k al frammento 2
      nconn_found = 0
      do i=1,nat1
         xpos1(:) = atm1(i)%xc
         dista1 = huge(1.0)
         dista2 = huge(1.0)
         vdist1(i) = -10
         vdist2(i) = -10
         do j=1,nat2
            xpos2a(:) = atm2s(j)%xc
            call xdisteqs(xpos1,xpos2a,cell%get_g(),distb,ktrans)
!
!           If required find bond between atoms
            if (findbond)  then
                call bond_info(zval1(i),zval2(j),distb,btype,distab)   ! find chemical bond
            else
                btype = 0
            endif
            if (btype > 0) then
                conn_found = .true.
                nconn_found = nconn_found + 1
#if defined(PRINT_INFO)
                write(kpru,*)'dist=',trim(atm2(j)%lab),trim(atm1(i)%lab),distb
#endif
                if (distb < dista1) then
                    koper1(i) = op_type(k,nint(xpos1 + ktrans - xpos2a))
                    dista1 = distb
                    vdist1(i) = distb
                endif
            else
                if (distb < dista2) then
                    koper2(i) = op_type(k,nint(xpos1 + ktrans - xpos2a))
                    dista2 = distb
                    vdist2(i) = distb
                endif
            endif
         enddo
      enddo   
      if (conn_found) then
          vdist(:) = vdist1(:)
          koper(:) = koper1(:)
      else
          vdist(:) = vdist2(:)
          koper(:) = koper2(:)
      endif
      if (conn_found .or. frag1%nat < 10 .or. frag2%nat < 10) then
!
!         Find operator with minumum distance
!!!!!!!!!!!!!FIXME - find better solution
          if (any(vdist >= 0)) then
          distss = minval(vdist(:),mask=vdist >= 0)
                   distss = distss - 0.2*nconn_found  ! add contribution of number of bonds
          if (distss < distmin) then
              distmin  = distss
              idloc(:) = minloc(vdist, mask = vdist >= 0)
              ktransmin(:) = koper(idloc(1))%tra
              kmin = koper(idloc(1))%op
#if defined(PRINT_INFO)
              write(kpru,*)'OPERATOR:',frag1%nat,frag2%nat,kmin,distmin
#endif
          endif
          endif
      else
!
!         Find operator with max overlap
          distss = huge(1.0)
          do i=1,frag1%nat
             if (vdist(i) >= 0 .and. koper(i)%op > 0) then
                 dmed = vdist(i)
                 nmed = 1
                 distv(nmed) = vdist(i)
                 do j=i+1,frag1%nat
                    if (koper(i) == koper(j) .and. vdist(j) >= 0) then
                        koper(j)%op = -koper(j)%op
                       !    if (vdist(j) < 4) then
                        nmed = nmed + 1
                        dmed = dmed + vdist(j)
                        distv(nmed) = vdist(j)
                       !    endif
                    endif
                 enddo
                   !    dmed = sum(distv(:nmed))/nmed
                 dmed = dmed / nmed
                   !    nmed = count(distv(:nmed) < 4)
                   !    dmed = sum(distv(:nmed),mask=distv(:nmed)<4)/nmed
                   !dmed = (minval(distv(:nmed)) + dmed) / 2
                   dmed = min(dmed,4.0)
                 overlap = count(distv(:nmed) <= dmed)
!
!                opzione 1
!                 if (overlap > maxoverlap) then
!                     maxoverlap = overlap
!                     !newktransmin(:) = tra_opcode(koper(i))
!                     !newkmin = op_from_opcode(koper(i))
!                     ktransmin(:) = koper(i)%tra
!                     kmin = koper(i)%op
!                     distmin = sum(distv(:nmed),mask=distv(:nmed) <= dmed) / overlap
!#if defined(PRINT_INFO)
!                     write(kpru,'(a,i5,4i5)')'min for op:',koper(i)%op,koper(i)%tra,overlap
!#endif
!                 endif
!
!                opzione 2
                 ddmed = sum(distv(:nmed),mask=distv(:nmed) <= dmed) / overlap
                 if (ddmed < distmin) then
                     distmin = ddmed
                     !newktransmin(:) = tra_opcode(koper(i))
                     !newkmin = op_from_opcode(koper(i))
                     ktransmin(:) = koper(i)%tra
                     kmin = koper(i)%op
#if defined(PRINT_INFO)
                     write(kpru,'(a,i5,4i5,f10.3)')'min for op:',koper(i)%op,koper(i)%tra,overlap,ddmed
#endif
                 endif
#if defined(PRINT_INFO)
                 write(kpru,'(a,f10.3,i5,i5,i5,3i5)')'media for op:',dmed,nmed,overlap,koper(i)%op,koper(i)%tra
#endif
             endif
          enddo
          where(koper(:)%op < 0) koper(:)%op = -koper(:)%op
      endif
   enddo
#if defined(PRINT_INFO)
   write(kpru,'(a,i4,3f10.2,l3/60("="))')'distanza per traslazione minima:',kmin,ktransmin,conn_found
#endif
!
   end subroutine fragment_distance_sym

!----------------------------------------------------------------------------------------------------   
#if 0
!!!!TODO: new fragment_distance to complete and test in overlay and regroup
!!!!TODO: usa atomsutil::get_minumum_distance to write get_minumum_distance with array of at 
   subroutine fragment_distance_new(frag1,frag2,atm1,atm2,distmin,kmin,ktransmin,findbond,cell,spg)
!
!  Calcola la distanza tra 2 frammenti restituendo l'operatore da applicare a frag2
!
   USE CGEOM
   USE atom_type_util
   USE connect_mod
   USE unit_cell
   USE spginfom
   type(fragment_type), intent(in)           :: frag1,frag2
   real, intent(out)                         :: distmin      ! distanza minima
   integer, intent(out)                      :: kmin         ! operatore corrispondente al minimo
   real, dimension(3), intent(out)           :: ktransmin    ! traslazione corrispondente al minimo
   logical, intent(in)                       :: findbond     ! favor new bond between fragments
   type(cell_type), intent(in)               :: cell
   type(spaceg_type), intent(in)             :: spg
   type(atom_type), dimension(:), intent(in) :: atm1,atm2
   type(atom_type), dimension(size(atm2)) :: atm2s
   real, dimension(frag1%nat)                :: vdist, vdist1, vdist2, distv
   type(operator_type), dimension(frag1%nat) :: koper, koper1, koper2
   real                                      :: dista1,dista2,distb,distss
   integer                                   :: i,j,k
   real, dimension(3)                        :: ktrans
   real, dimension(3)                        :: xpos1,xpos2a
   integer                                   :: nat1,nat2
   integer, dimension(1)                     :: idloc
   integer, dimension(size(atm1))            :: zval1
   integer, dimension(size(atm2))            :: zval2
   integer                                   :: btype
   real                                      :: distab
   logical                                   :: conn_found
   real                                      :: dmed,ddmed
   integer                                   :: nmed  
   integer                                   :: overlap, maxoverlap
   integer :: nconn_found
   real, dimension(3) :: bar0
   integer, dimension(3) :: diff
   integer, dimension(3)                                               :: xtra
   integer                                                             :: k1,k2,k3
#define PRINT_INFO
#if defined(PRINT_INFO)
   integer, parameter              :: kpru = 71
#endif
!
!  prendi gli atomi dei frammenti
   nat1 = frag1%nat
   nat2 = frag2%nat
   zval1(:) = atm1%z()
   zval2(:) = atm2%z()
#if defined(PRINT_INFO)
   write(kpru,'(60("=")/,a,2i5)')'Atoms in frag.',nat1,nat2
#endif
!
   kmin = 1
   ktransmin = (/1.0,1.0,1.0/)
   distmin = huge(1.0)
   conn_found = .false.
   maxoverlap = 0
   bar0 = baricentro(atm1)
   do i=1,spg%nsymop
      atm2s(:) = atm2(:)
      call apply_sym_oper(atm2s,spg%symop(k))  ! applica operatore k al frammento 2
      diff = nint(baricentro(atm2s) - bar0(:))
      do k1=-1,1
         do k2=-1,1
            do k3=-1,1
               xtra = (/k1,k2,k3/) + diff

            enddo
         enddo
      enddo
   enddo
#if defined(PRINT_INFO)
   write(kpru,'(a,i4,3f10.2,l3/60("="))')'distanza per traslazione minima:',kmin,ktransmin,conn_found
#endif
!
   end subroutine fragment_distance_new
#endif
!----------------------------------------------------------------------------------------------------   

   integer function nfragments(fragm)
!
   type(fragment_type), dimension(:), allocatable, intent(in) :: fragm
!
   if (allocated(fragm)) then
       nfragments = size(fragm)
   else
       nfragments = 0
   endif
!
   end function nfragments

!----------------------------------------------------------------------------------------------------   

   subroutine connect_fragment(fragm,atomc,legm,cell,usecov)
   USE cgeom
   USE bondtmod
   USE connect_mod
   USE unit_cell
   USE atom_type_util
   USE unit_cell
   type(fragment_type), dimension(:), allocatable, intent(in) :: fragm  ! i frammenti
   type(atom_type), dimension(:), allocatable, intent(in)     :: atomc  ! gli atomi
   type(bond_type), dimension(:), allocatable, intent(inout)  :: legm   ! legami interni ai frammenti
   type(cell_type), intent(in)                                :: cell
   logical, intent(in)                                        :: usecov
   type(atom_type), dimension(:), allocatable                 :: atom   ! atomi in coord. frazionarie
   integer                                                    :: nfrag
   integer                                                    :: i,j   !!!,k
   integer                                                    :: ia,ja
   integer                                                    :: ipa,jpa
   real                                                       :: sd
   integer                                                    :: nat
   integer, dimension(size(atomc))                             :: zval
   integer                                                    :: nleg
   integer                                                    :: ndimleg
   type(bond_type) :: legnew
!
   nfrag = nfragments(fragm) 
   if (nfrag > 1) then
       nat = size(atomc)
!
!      converti in coordinate cartesiane       
       call copy_atoms(atom,atomc)
       call frac_to_cart(atom,cell%get_ortom())
!
       sd = 0.3
!
!      genera Z di tutti gli atomi
       zval(:) = atom%z()
!
       nleg = numbonds(legm)
       ndimleg = nat+nleg
       call resize_bonds(legm,ndimleg)
       do i=1,nfrag-1                ! loops sui frammenti
          do j=i+1,nfrag
             do ia=1,fragm(i)%nat    ! loops sugli atomi
                do ja=1,fragm(j)%nat
!
!                  Controlla se ia e ja sono legati
                   ipa = fragm(i)%pos(ia)
                   jpa = fragm(j)%pos(ja)
                   call create_bond(ipa,jpa,atom(ipa)%xc,atom(jpa)%xc,zval(ipa),zval(jpa),0.0,0.0,legnew,usecov)
                   if (legnew%n1 > 0) then  ! se 0 non c'è legame tra i due atomi
                       if (is_angle_ok(ipa,jpa,legm,nleg,atom,55.0)) then 
                           nleg = nleg + 1
                           if (nleg > ndimleg) then   ! check sulla dimensione di legm
                               ndimleg = ndimleg + nat/2
                               call resize_bonds(legm,ndimleg)
                           endif
                           legm(nleg) = legnew
                       endif
                   endif
                enddo
             enddo
          enddo
       enddo
       call resize_bonds(legm,nleg)  ! dimensiona legm a nleg
   endif
!
   end subroutine connect_fragment

!----------------------------------------------------------------------------------------------------   

   subroutine get_legm_from_fragment(fragm,legmtot,legmfrag)
!
!  Estrai i legami presenti in un frammento a partire da tutti i legami
!
   USE connect_mod
   type(fragment_type), intent(in)                           :: fragm    ! il frammento
   type(bond_type), dimension(:), allocatable, intent(in)    :: legmtot  ! tutti i legami 
   type(bond_type), dimension(:), allocatable, intent(inout) :: legmfrag ! solo i legami nel frammento fragm
   integer                                                   :: nleg,nlegf
   integer                                                   :: i,j
   integer                                                   :: n1,n2
   integer                                                   :: n1f,n2f
!
   nleg = numbonds(legmtot)
   if (nleg > 0) then
       nlegf = 0
       call new_bonds(legmfrag,nleg)
       do i=1,nleg
          n1 = legmtot(i)%n1
          n2 = legmtot(i)%n2
          !if (any(fragm%pos(:) == n1) .and. any(fragm%pos(:) == n2)) then
          !    nlegf = nlegf + 1
          !    legmfrag(nlegf) = legmtot(i)
          !endif
          n1f = 0
          n2f = 0
          do j=1,fragm%nat
             if (fragm%pos(j) == n1) then
                 n1f = j
                 exit
             endif
          enddo
          if (n1f > 0) then
              do j=1,fragm%nat
                 if (fragm%pos(j) == n2) then
                     n2f = j
                     exit
                 endif
              enddo
          endif
          if (n1f > 0  .and. n2f > 0) then
              nlegf = nlegf + 1
              legmfrag(nlegf)%n1 = n1f
              legmfrag(nlegf)%n2 = n2f
              legmfrag(nlegf)%dist = legmtot(i)%dist
              legmfrag(nlegf)%sigma = legmtot(i)%sigma
              legmfrag(nlegf)%ord = legmtot(i)%ord
          endif
       enddo
       call resize_bonds(legmfrag,nlegf)
   endif
!
   end subroutine get_legm_from_fragment

!----------------------------------------------------------------------------------------------------   

   logical function is_angle_ok(na,nb,legm,numleg,atom,anglim) 
!
!  Controlla se gli angoli formati dal legame na-nb sono ragionevoli
!
   USE connect_mod
   USE trig_constants
   USE cgeom
   USE atom_type_util
   integer, intent(in)                                    :: na,nb  ! legame da controllare
   type(bond_type), dimension(:), allocatable, intent(in) :: legm   ! gli altri legami esistenti
   integer                                                :: numleg ! numero di legami esistenti
   type(atom_type), dimension(:), intent(in)              :: atom   ! atoms in cartesian coord.
   real                                                   :: anglim ! angoli di legami inferiori ad anglim non sono validi
   integer                                                :: i
   logical                                                :: metal_at
   real                                                   :: alimit
!
   is_angle_ok = .true.
!corr   metal_at = any(is_metal([atom(na),atom(nb)]))
   metal_at = any_is_metal([atom(na),atom(nb)])
   do i=1,numleg
      alimit = anglim
!
!     Check atomo n1
      if (legm(i)%n1 == na) then
          if (metal_at .or. is_metal(atom(legm(i)%n2))) alimit = ANGLIM_METAL
          is_angle_ok = rtod*angleC(atom(legm(i)%n2)%xc,atom(na)%xc,atom(nb)%xc) > alimit
      elseif (legm(i)%n1 == nb) then
          if (metal_at .or. is_metal(atom(legm(i)%n2))) alimit = ANGLIM_METAL
          is_angle_ok = rtod*angleC(atom(na)%xc,atom(nb)%xc,atom(legm(i)%n2)%xc) > alimit
      endif
      if (.not.is_angle_ok) exit
!
!     Check atom n2
      if (legm(i)%n2 == na) then
          if (metal_at .or. is_metal(atom(legm(i)%n1))) alimit = ANGLIM_METAL
          is_angle_ok = rtod*angleC(atom(legm(i)%n1)%xc,atom(na)%xc,atom(nb)%xc) > alimit
      elseif (legm(i)%n2 == nb) then
          if (metal_at .or. is_metal(atom(legm(i)%n1))) alimit = ANGLIM_METAL
          is_angle_ok = rtod*angleC(atom(na)%xc,atom(nb)%xc,atom(legm(i)%n1)%xc) > alimit
      endif
      if (.not.is_angle_ok) exit
   enddo
!
   end function is_angle_ok

!----------------------------------------------------------------------------------------------------

   subroutine get_branch(atom,cell,connt,legm,nconnbr,connbr)
!
!  Genera tutte le possibili ramificazioni
!
   USE atom_basic, only:atom_type
   USE connect_mod
   USE unit_cell
   USE arrayutil
   type(atom_type), dimension(:), intent(in)                      :: atom
   type(cell_type), intent(in)                                    :: cell
   type(container_type), dimension(:), intent(in)                 :: connt
   type(bond_type), dimension(:), allocatable, intent(in)         :: legm
   type(container_type), dimension(:), allocatable, intent(inout) :: connbr
   integer, dimension(size(connt))                                :: veta1,veta2,vetleg
   integer                                                        :: i,j,k
   integer                                                        :: nata1,nata2
   integer                                                        :: nconnbr
   integer                                                        :: nleg
   integer                                                        :: nfrag
   type(fragment_type), dimension(:), allocatable                 :: fragm
   logical                                                        :: kfound
!
   nleg = numbonds(legm)
   if (nleg > 0) call new_container(connbr,nleg*2)
   nconnbr = 0
   do i=1,numbonds(legm)
      vetleg(:) = 0
      
      veta1(:) = 0
      nata1 = 1
      veta1(1) = legm(i)%n1
      call get_chain(connt,legm(i)%n1,legm(i)%n2,nata1,veta1)  ! catena in una direzione
!          
      if (nata1 > 1) then
          vetleg(veta1(2:nata1)) = vetleg(veta1(2:nata1)) + 1
      endif
      veta2(:) = 0
      nata2 = 1
      veta2(1) = legm(i)%n2
      call get_chain(connt,legm(i)%n2,legm(i)%n1,nata2,veta2)  ! catena nell'altra direzione
      if (nata2 > 1) then
          vetleg(veta2(2:nata2)) = vetleg(veta2(2:nata2)) + 1
      endif
!
      if (.not.any(vetleg==2)) then   ! se verificato: il legame e' in un sistema ciclico
          nconnbr = nconnbr + 1
          connbr(nconnbr)%nat = nata1
          allocate(connbr(nconnbr)%pos(nata1))
          connbr(nconnbr)%pos(:) = veta1(:nata1)
!
          nconnbr = nconnbr + 1
          connbr(nconnbr)%nat = nata2
          allocate(connbr(nconnbr)%pos(nata2))
          connbr(nconnbr)%pos(:) = veta2(:nata2)
      endif
   enddo   
!
!  Frammenti ciclici e senza ramificazione (es. benzene senza H) vengono aggiunti
   call get_fragments_from_leg(atom,cell,legm,connt,nfrag,fragm) ! estrai i frammenti
   do i=1,nfrag
      if (fragm(i)%nat > 2) then
          kfound = .false.
          loop_frag: do j=1,fragm(i)%nat
             do k=1,nconnbr
                if (any(connbr(k)%pos(:) == fragm(i)%pos(j))) then
                    kfound = .true.
                    exit loop_frag
                endif
             enddo
          enddo loop_frag
          if (.not.kfound) then  ! il frammento non e' incluso in alcuna ramificazione
              nconnbr = nconnbr + 1
              connbr(nconnbr)%nat = fragm(i)%nat
              allocate(connbr(nconnbr)%pos(fragm(i)%nat))
              connbr(nconnbr)%pos(:) = fragm(i)%pos(:)
              !write(0,*)'sistema ciclico:',fragm(i)%pos
          endif
      endif
   enddo
   if (nleg > 0) call resize_container(connbr,nconnbr)
!   
   end subroutine get_branch

!----------------------------------------------------------------------------------------------------

   subroutine fragment_in_cellv(fragm,atom)
!   
!  Translate all fragments in cell
!
   USE atom_type_util
   type(fragment_type), dimension(:), allocatable, intent(inout) :: fragm
   type(atom_type), dimension(:), intent(inout), optional        :: atom
   integer :: i
!
   if (present(atom)) then
       do i=1,size(fragm)
          fragm(i)%atm0 = atom(fragm(i)%pos)
       enddo
   endif
   do i=1,size(fragm)
      call translate_in_cell(fragm(i)%atm0)
   enddo
   if (present(atom)) then
       do i=1,size(fragm)
          atom(fragm(i)%pos) = fragm(i)%atm0
       enddo
   endif
!
   end subroutine fragment_in_cellv

!----------------------------------------------------------------------------------------------------

   subroutine fragment_in_cells(fragm,atom)
!   
!  Translate fragment in cell
!
   USE atom_type_util
   type(fragment_type), intent(inout)                     :: fragm
   type(atom_type), dimension(:), intent(inout), optional :: atom
!
   if (present(atom)) then
       fragm%atm0 = atom(fragm%pos)
   endif
   call translate_in_cell(fragm%atm0)
   if (present(atom)) then
       atom(fragm%pos) = fragm%atm0
   endif
!
   end subroutine fragment_in_cells

!----------------------------------------------------------------------------------------------------

   subroutine move_fragment(atom,vet,tx,rx,codt,codr,cell,typer,xc)
!
!  Applica trasformazione R*X + T   
!   
   USE atom_type_util
   USE unit_cell
   type(atom_type), dimension(:), intent(inout) :: atom
   integer, dimension(:), intent(in)            :: vet
   real, dimension(3), intent(in)               :: rx,tx     ! R e T    
   integer, intent(in)                          :: codr,codt ! applico R?, applico T?
   type(cell_type), intent(in)                  :: cell
   integer, intent(in)                          :: typer     ! type of rotation
   real, dimension(3), intent(in), optional     :: xc        ! coordinates for centre of rotation
   type(atom_type), dimension(size(vet))        :: atr0
   real, dimension(3)                           :: rxx
!
   atr0 = atom(vet)
!
!  Rotazione: applica R se richiesto 
   if (codr > 0) then
       rxx(:2) = rx(:2)
       if (rx(3) < 0) then ! check su z
           rxx(3) = 0
       elseif (rx(3) > 1) then
           rxx(3) = 1
       else
           rxx(3) = rx(3)
       endif
       if (typer == 0) then
           call rand_rotate_atoms(atr0,rxx,cell)
       else
           if (present(xc)) then
               call rand_rotate_atoms(atr0,rxx,cell,xc)
           else
               call rand_rotate_atoms(atr0,rxx,cell,atom(typer)%xc)
           endif
       endif
   endif
!
!  Traslazione: applica T se richiesto
   if (codt > 0) then
       call translate_atoms(atr0,tx)
   endif
!
!  Aggiorna i parametri atomici   
   atom(vet) = atr0
   !do i=1,3
   !   atom(frag%pos)%xc(i) = atr0%xc(i)
   !enddo
!   
   end subroutine move_fragment

!----------------------------------------------------------------------------------------------------

   subroutine randomize_molecule(atom,leg,cell,frag,rot)
   USE connect_mod
   USE rotationmod
   USE trig_constants
   USE rand_mod
   USE atom_type_util
   USE rand_mod
   USE cgeom
   USE unit_cell
   USE arrayutil
   type(atom_type), dimension(:), intent(inout)             :: atom
   type(bond_type), dimension(:), allocatable, intent(in)   :: leg
   type(cell_type), intent(in)                              :: cell
   type(fragment_type), dimension(:), allocatable, optional :: frag
   type(rotation_type), dimension(:), allocatable, optional :: rot
   type(fragment_type), dimension(:), allocatable           :: fragm
   type(rotation_type), dimension(:), allocatable           :: rotm
   type(atom_type), dimension(size(atom))                   :: atmr
   integer                                                  :: nfrag
   real, dimension(3)                                       :: tx,rx
   integer                                                  :: codt, codr
   integer                                                  :: i
   integer                                                  :: nat
   type(container_type), dimension(:), allocatable          :: conn
   integer                                                  :: numr
   real                                                     :: xrand
   integer                                                  :: natrot
   real, dimension(3)                                       :: pa1,pa2
   real                                                     :: theta
   real, dimension(3)                                       :: bar
   integer                                                  :: j
!
!  Get fragments
   nat = size(atom)
   if (.not.present(frag) .or. .not.present(rot))call bond_to_connect(nat,leg,conn)
   if (present(frag)) then
       nfrag = numfragments(frag)
       allocate(fragm(nfrag),source=frag)
   else
       call get_fragments_from_leg(atom,cell,leg,conn,nfrag,fragm)
   endif
!
!  External DOF randomization
   do i=1,nfrag
      codt = sum(fragm(i)%rcod(1:3))
      codr = sum(fragm(i)%rcod(4:6))
      if (codt > 0) then ! randomize in range low-high 
          do j=1,3
             tx(j) = randvalue(fragm(i)%tlow(j),fragm(i)%thigh(j))
          enddo
          bar = baricentro(atom(fragm(i)%pos))
          tx = tx - bar
      endif
      if (codr > 0) call random_number(rx)
      call move_fragment(atom,fragm(i)%pos,tx,rx,codt,codr,cell,fragm(i)%rat)
      if (codt > 0) call fragment_in_cell(fragm,atom)
   enddo
!
!  Get rotations
   if (present(rot)) then
       if (allocated(rot)) then
           numr = size(rot)
           allocate(rotm(numr),source=rot)
       else
           numr = 0
       endif
   else
       call get_rotation_from_leg(atom,conn,leg,cell,numr,rotm,.false.)
   endif
!
!  Internal DOF randomization
   do i=1,numr
      if (rotm(i)%rcod > 0) then
          pa1 = matmul(cell%get_ortom(),atom(rotm(i)%pax(1))%xc)
          pa2 = matmul(cell%get_ortom(),atom(rotm(i)%pax(2))%xc)
          xrand = randvalue(-0.5,0.5)
          theta = 2.0*pi*xrand
          natrot = rotm(i)%nat
          atmr(:natrot) = atom(rotm(i)%pat)
          call rotate_atoms(atmr(:natrot),pa1,direction_cos(pa1,pa2),theta,cell)
          atom(rotm(i)%pat) = atmr(:natrot)
      endif
   enddo
!
   end subroutine randomize_molecule

!----------------------------------------------------------------------------------------------------

   subroutine frag_set_centre(frag,conn)
!
!  Set origin for rotation
!
   USE atom_type_util
   USE arrayutil
   type(fragment_type), dimension(:), allocatable, intent(inout) :: frag
   type(container_type), dimension(:), allocatable              :: conn
   integer                                                       :: i
!
   do i=1,numfragments(frag)
      frag(i)%rat = origin_for_rotation(conn,frag(i)%pos)
   enddo
!
   end subroutine frag_set_centre

!----------------------------------------------------------------------------------------------------

   subroutine set_centre_from_string(str,frag,atom,err)
!
!  Set centre of rotation 
!
   USE errormod
   USE atom_type_util
   USE strutil
   character(len=*), intent(in)                                  :: str
   type(fragment_type), dimension(:), allocatable, intent(inout) :: frag
   type(atom_type), dimension(:), allocatable, intent(in)        :: atom
   type(error_type), intent(out)                                 :: err
   character(len=20), dimension(size(atom))                      :: wordv
   integer                                                       :: nword
   integer                                                       :: i
   integer, dimension(2)                                         :: vpos

   if (numfragments(frag) == 0) return
   if (numatoms(atom) == 0) return
   if (len_trim(str) == 0) return

   call get_words(str,wordv,nword)
   if (nword == 0 .or. nword > 2) then
       call err%set('Error in directive centre_of_rotation')
       return
   endif

   if (s_eqi(wordv(1),'@com')) then  ! @com At
       if (nword == 1) then
           call err%set('Error in directive centre_of_rotation')
           return
       else
           vpos(1) = string_locate(wordv(2),atom(:)%lab)
           if (vpos(1) == 0) call err%set('Undefined atom(s) '//trim(wordv(2)))
           frag(fragment_pos(frag,vpos(1)))%rat = 0
       endif
   else
       if (nword == 1) then         ! At will the centre for fragment containing At
           vpos(1) = string_locate(wordv(1),atom(:)%lab)
           if (vpos(1) == 0) then
               call err%set('Undefined atom(s) '//trim(wordv(1)))
               return
           endif
           frag(fragment_pos(frag,vpos(1)))%rat = vpos(1)
       else
           do i=1,nword             ! At1 At2: At1 will be the centre of rotation for fragment containing At2
              vpos(i) = string_locate(wordv(i),atom(:)%lab)
              if (vpos(i) == 0) then
                  if (err%signal) then
                      call err%add(', '//wordv(i))
                  else
                      call err%set('Undefined atom(s) '//trim(wordv(i)))
                  endif
              endif
           enddo
           if (err%signal) return
           frag(fragment_pos(frag,vpos(2)))%rat = vpos(1)
       endif
   endif

   end subroutine set_centre_from_string

!----------------------------------------------------------------------------------------------------

   subroutine duplicate_fragment(atom,bond,elem,frag,nfrag,ncopies)
!
!  Duplicate a fragment
! 
   USE atom_type_util
   USE connect_mod
   USE elements
   type(atom_type), dimension(:), allocatable, intent(inout)     :: atom
   type(bond_type), dimension(:), allocatable, intent(inout)     :: bond
   type(element_type), dimension(:), allocatable, intent(in)     :: elem
   type(fragment_type), dimension(:), allocatable, intent(inout) :: frag
   integer, intent(in)                                           :: nfrag   ! number of frag to duplicate
   integer, intent(in)                                           :: ncopies ! number of copies 
   integer                                                       :: i
   type(fragment_type)                                           :: fragc
!
   if (nfrag > numfragments(frag)) return
!
   fragc = frag(nfrag)  !make copy to avoid side effect calling push_back_fragment
   do i=1,ncopies
      call duplicate_atoms(atom,bond,elem,frag(nfrag)%pos,rand=.false.)
      call push_back_fragment(frag,fragc)
   enddo
!
   end subroutine duplicate_fragment

!----------------------------------------------------------------------------------------------------

   subroutine delete_fragment(atom,bond,frag,nfrag)
!
!  Duplicate a fragment
! 
   USE atom_type_util
   USE connect_mod
   type(atom_type), dimension(:), allocatable, intent(inout)     :: atom
   type(bond_type), dimension(:), allocatable, intent(inout)     :: bond
   type(fragment_type), dimension(:), allocatable, intent(inout) :: frag
   integer, intent(in)                                           :: nfrag   ! number of frag to duplicate
   integer, dimension(:), allocatable                            :: iord
   integer                                                       :: i,numfrag
!
   numfrag = numfragments(frag)
   if (nfrag > numfrag) return
!
   call remove_atoms_vet(atom,bond,frag(nfrag)%pos,iord)
!
!  shift array
   frag(nfrag:numfrag-1) = frag(nfrag+1:)
   call resize_fragments(frag,numfrag-1)
!
!  reorder atoms in frag
   do i=1,numfrag-1
      frag(i)%pos = iord(frag(i)%pos)
   enddo
!
   end subroutine delete_fragment

END MODULE fragmentmod
