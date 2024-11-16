 MODULE constraints

! F nconstraints(constr)                                  Number of constraints
! S add_constraint(code,vpar,vat,atom,coefi)              Aggiunge un nuovo constraint alla lista
! S add_constraints_for_riding(constr,atom,legm)          Create constraints for riding model
! S set_overall_B(constr,atom,radtype)                    Set an overall B applying the riding model if necessary
! S set_species_B(constr,atom,legm,elem,radtype)          Imponi che atomi della stessa specia abbiano lo stesso fattore termico
! S set_molecules_B(constr,atom,frag,radtype)             Set an overall B for each molecular fragment
! S check_constraints(constr)                             Check e modify constraints considering the presence of same parameters
! F constr_compare(con1,con2) result(vcomp)               Find similar parameters in constraints con1 and con2
! F constr_merge(con1,con2,vcomp)  result(con3)           Merge 2 constraints
! S normalize_constr(constr)                              Last parameter should have coef equal 1.0
! F constr_aveb(constr,atom,bmin,bmax)  result(bmed)      Compute an average b for atom in constr
! F par_in_constraint(kat,npar) result(kconstr)           Controlla se un atomo e' implicato in un constraint
! F coef_of_constraint(kconstr,nat,npar) result(coef)     Prende il coefficente del parametro npar del constraint
! F function blocked_constraint(constr,atom)              Has constraint blocked parameters?
! S refine_constraint(constr,atom,code)                   Chiedo di affinare (code=1) o non affinare (code=0) un constraint
! S delete_all_constraints(constr,code)                   Elimina tutti i constrain di codice uguale a code
! S delete_constraint(constr,id)                          Delete constraint id 
! S resize_constraints(vetr,n,savevet)                    Rialloca ad n un vettore di tipo restraint
! S new_constraints(vetr,n)                               Create new constraints
! S clear_constraints()                                   Azzera constraints
! S push_back_constraint(constrv,con)                     Add constraint con to array constrv
! S set_constr_on_position()                              Cerca atomi in posizione speciale generando nconstr constraints
! S set_constr_on_sof(atom,constr)                        Set constraints on sof
! S make_constraints(atom,...)                            Set program constraints
! S print_constraints(constr,atom,kpr,is_all)             Print constraints
! S constr_tf_from_string(str,constr,atom)                Set constraint on thermal factor from string
! S constraint_from_equation(str,atom,elem,constr,err)    Set constraint from equation
! S equation_from_string(str,....)                        Parse equation  extracting parameters

 implicit none

! General form of constraint:
!                             coef(1)*vpar(1) + coef(2)*vpar(2) + ... = sumc

 type constraint_type
   integer                            :: code     ! codice identificativo del tipo di constraint (1=B,2=co.,3=sof,4=BH)
   integer, dimension(:), allocatable :: vpar     ! i parametri coinvolti
   integer, dimension(:), allocatable :: vat      ! gli atomi coinvolti
   real, dimension(:), allocatable    :: coef     ! coefficienti
   real                               :: sumc = 0 ! known term in constraint equation
   integer                            :: npar     ! il numero totale di parametri coinvolti
   integer                            :: npari    ! numero di parametri indipendenti
   integer                            :: ref      ! codice di affinamento
   integer                            :: typec    ! settato da 1=user 2=programma

 contains
   procedure :: prn   
   procedure :: blocked => blocked_constraint

 end type constraint_type

integer, parameter :: CONSTR_B=1, CONSTR_CO=2, CONSTR_SOF=3, CONSTR_H=4
integer, parameter :: CONSTR_ADP_NONE=1, CONSTR_ADP_OVERALL=2, CONSTR_ADP_SPECIES=3, CONSTR_ADP_MOLECULES=4

private :: refine_constraint_s, refine_constraint_arr 
interface refine_constraint
  module procedure refine_constraint_s, refine_constraint_arr 
end interface refine_constraint

private :: push_back_constraint_s, push_back_constraint_v
interface push_back_constraint
  module procedure push_back_constraint_s, push_back_constraint_v
end interface push_back_constraint 

 CONTAINS
 


   subroutine prn(this,kpr,atom)
   USE atom_basic
   class(constraint_type), intent(in)                     :: this
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   integer, intent(in)                                    :: kpr
   write(kpr,'(2x,a)')string_constraint(this,atom)
   write(kpr,'(a,*(i4))')   'Atom: ',this%vat
   write(kpr,'(a,*(i4))')   'Par:  ',this%vpar
   write(kpr,'(a,*(f10.2))')'coef: ',this%coef
   write(kpr,'(a,*(f10.2))')'sumc: ',this%sumc
   end subroutine prn

!--------------------------------------------------------------------------

   integer function nconstraints(constr)
!
!  Number of constraints
!
   type(constraint_type), dimension(:), allocatable, intent(in) :: constr
!   
   if (allocated(constr)) then
       nconstraints = size(constr)
   else
       nconstraints = 0
   endif
!
   end function nconstraints

!--------------------------------------------------------------------------

   integer function nconstraints_ref(constr)
!
!  Number of constraints
!
   type(constraint_type), dimension(:), allocatable, intent(in) :: constr
   integer                                                      :: i
!   
   nconstraints_ref = 0
   do i=1,nconstraints(constr)
      if (constr(i)%ref > 0) nconstraints_ref = nconstraints_ref + constr(i)%npari
   enddo
!
   end function nconstraints_ref

!--------------------------------------------------------------------------
 
   subroutine add_constraint(constr,code,vpar,vat,atom,coefi,npari,typec,sumc)
!
!  Aggiunge un nuovo constraint alla lista
!
   USE atom_basic
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
   integer, intent(in)                                             :: code  ! codice constraint
   integer, dimension(:), intent(in)                               :: vpar  ! n. d'ordine dei parametri coinvolti
   integer, dimension(:), intent(in)                               :: vat   ! n. d'ordine degli atomi coinvolti
   type(atom_type), dimension(:), intent(inout)                    :: atom
   real, dimension(:), intent(in), optional                        :: coefi ! coefficienti
   integer, intent(in)                                             :: npari ! n. di parametri indipendenti
   integer, intent(in), optional                                   :: typec
   real, intent(in), optional                                      :: sumc
   integer                                                         :: typecc
   real                                                            :: sumcc
   real, dimension(size(vat))                                      :: coef
   type(constraint_type)                                           :: con
!
   if (present(coefi)) then
       coef(:) = coefi(:)
   else
       coef(:) = 1.0
   endif
!
   if (present(typec)) then
       typecc = typec
   else
       typecc = 2
   endif
!
   if (present(sumc)) then
       sumcc = sumc
   else
       sumcc = 0.0
   endif
!  
   call set_constraint(con,code,vpar,vat,coef,sumcc,npari,0,typecc) 
   call refine_constraint(con,atom,0)  ! azzera codice di affinamento e codice degli atomi
   call push_back_constraint(constr,con)
!
   end subroutine add_constraint

!--------------------------------------------------------------

   subroutine set_constraint(constr,code,vpar,vat,coef,sumc,npari,ref,typec)
   USE arrayutil
   type(constraint_type), intent(inout) :: constr
   integer, intent(in)                  :: code
   integer, dimension(:), intent(in)    :: vpar,vat
   real, dimension(:), intent(in)       :: coef
   real, intent(in)                     :: sumc
   integer, intent(in)                  :: npari
   integer, intent(in)                  :: ref
   integer, intent(in)                  :: typec
!
   constr%code = code
   constr%npar = size(vpar)
   call new_array(constr%vpar,constr%npar)
   call new_array(constr%vat,constr%npar)
   call new_array(constr%coef,constr%npar)
   constr%vpar = vpar
   constr%vat = vat
   constr%coef = coef
   constr%sumc = sumc
   constr%npari = npari
   constr%ref = ref
   constr%typec = typec
!
   end subroutine set_constraint

!--------------------------------------------------------------

   subroutine add_constraints_for_riding(constr,atom,legm,spg,cell,adp_factor)
!
!  Create constraints for riding model
!
   USE connect_mod
   USE contacts
   USE atom_type_util
   USE spginfom
   USE unit_cell
   USE arrayutil
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
   type(atom_type), dimension(:), intent(inout)                    :: atom
   type(bond_type), dimension(:), allocatable, intent(in)          :: legm
   type(spaceg_type), intent(in)                                   :: spg
   type(cell_type), intent(in)                                     :: cell
   real, intent(in)                                                :: adp_factor
   type(container_type), dimension(:), allocatable                :: connH
   integer                                                         :: i,j,k,nat
   integer, dimension(10)                                          :: code
!
   if (.not.is_hydrogen(atom)) return
!
   nat = size(atom)
   call get_conn_hydrogens(connH,atom,nat,legm)
   do i=1,nat
      if (connH(i)%nat > 0) then
          code(:) = lsq_conditions(atom(i),spg,cell,i)
          do j=1,connH(i)%nat  ! loop on H-atoms connected to j-atom
             do k=1,3
                if (code(k) == 0) then
                    atom(connH(i)%pos(j))%rcod(k) = -1 ! fix position of H-atom
                else
                    call add_constraint(constr,CONSTR_CO,[k,k],[i,connH(i)%pos(j)],atom,npari=1,typec=2)
                endif
             enddo
          enddo
!
!         coef multiply the dipendent variables: coef1*x = x; coef2
          call add_constraint(constr,CONSTR_B,(/(4,j=1,connH(i)%nat+1)/),[connH(i)%pos,i],    &
               atom,[(adp_factor,j=1,connH(i)%nat),1.0],npari=1,typec=2)
          atom(connH(i)%pos)%biso = adp_factor*atom(i)%biso   ! change B on H
      endif
   enddo
!
   end subroutine add_constraints_for_riding

!--------------------------------------------------------------

   subroutine set_overall_B(constr,atom,radtype)
!
!  Set an overall B applying the riding model if necessary
!
   USE strutil
   USE atom_type_util
   USE connect_mod
   USE elements
   type(constraint_type), allocatable, intent(inout) :: constr(:)
   type(atom_type), allocatable, intent(inout)       :: atom(:)
   integer, intent(in)                               :: radtype
   integer                                           :: i
   integer                                           :: numat,nh,nhh,nat
   integer, dimension(:), allocatable                :: vat
   real, dimension(:), allocatable                   :: vcoef
   real, parameter                                   :: adp_factor = 1.2
!
   numat = numatoms(atom)
   if (numat == 1) then
       atom(1)%rcod(4) = 1
       return
   endif
!
   nh = number_of_hydrogens(atom)
   if (nh > 0 .and. radtype == RX_SOURCE) then
       allocate(vat(numat),vcoef(numat))
       nhh = 0
       nat = nh
       do i=1,numat
          if (is_hydrogen(atom(i))) then
              nhh = nhh + 1
              vat(nhh) = i
              vcoef(nhh) = adp_factor
          else
              nat = nat + 1
              vat(nat) = i
              vcoef(nat) = 1.0
          endif
       enddo
       call add_constraint(constr,CONSTR_B,[(4,i=1,numat)],vat,atom,vcoef,npari=1,typec=1)
   else
       call add_constraint(constr,CONSTR_B,[(4,i=1,numat)],[(i,i=1,numat)],atom,npari=1,typec=1)
   endif
!
   end subroutine set_overall_B

!--------------------------------------------------------------

   subroutine set_species_B(constr,atom,legm,elem,radtype)
!
!  Imponi che atomi della stessa specia abbiano lo stesso fattore termico
!
   USE atom_type_util
   USE elements
   USE connect_mod
   USE arrayutil
!
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
   type(bond_type), dimension(:), allocatable, intent(in)          :: legm
   type(atom_type), dimension(:), allocatable, intent(inout)       :: atom
   type(element_type), dimension(:), allocatable                   :: elem
   integer, intent(in)                                             :: radtype
   integer                                                         :: i,j
   integer, dimension(size(atom))                                  :: vats
   integer, dimension(:), allocatable                              :: vatH,vatot
   integer                                                         :: nsat,natH,natot
   real, parameter                                                 :: adp_factor = 1.2
!
   if (is_hydrogen(atom) .and. radtype == RX_SOURCE) then
       do i=1,numelem(elem)
          if (elem(i)%z == H_at) cycle
          call get_atoms_of_specie(elem(i)%ptab,atom,vats,nsat)
          if (nsat > 0) then
              !vatc = vats
!
!             Copy H connected to vats in array vatot
              call delete_array(vatot)
              do j=1,nsat
                 call get_connected_atoms_spec(legm,vats(j),atom%z(),H_at,vatH,natH)
                 if (natH > 0) then
                     if (.not.allocated(vatot)) then
                         call copy_array(vatot,vatH)
                     else
                         call append_merge_array(vatot,vatH)   ! merge necessary in rare case H is connected to more 1 atoms
                     endif
                     !write(0,*)'AT ',vatot
                 endif
              enddo
!
              natot = size_array(vatot)
              if (natot == 0) then   ! no H atoms found
                  if (nsat == 1) then
                      atom(vats(1))%rcod(4) = 1
                  else
                      call add_constraint(constr,CONSTR_B,[(4,i=1,nsat)],vats(:nsat),atom,npari=1,typec=1)
                  endif
              else
                  call add_constraint(constr,CONSTR_B,[(4,i=1,natot+nsat)],[vatot,vats(:nsat)],    &
                                      atom,[(adp_factor,i=1,natot),(1.0,i=1,nsat)],npari=1,typec=1)
              endif
          endif
       enddo
   else
       do i=1,numelem(elem)
          call get_atoms_of_specie(elem(i)%ptab,atom,vats,nsat)
          if (nsat == 1) then
              atom(vats(1))%rcod(4) = 1
          elseif (nsat > 1) then
              call add_constraint(constr,CONSTR_B,[(4,i=1,nsat)],vats(:nsat),atom,npari=1,typec=1)
          endif
       enddo
   endif
!
   end subroutine set_species_B

!--------------------------------------------------------------------------------------------------

   subroutine set_molecules_B(constr,atom,frag,radtype)
!
!  Set an overall B for each molecular fragment
!
   USE strutil
   USE atom_type_util
   USE connect_mod
   USE fragmentmod
   type(constraint_type), allocatable, intent(inout) :: constr(:)
   type(atom_type), allocatable, intent(inout)       :: atom(:)
   type(fragment_type), allocatable, intent(in)      :: frag(:)
   integer, intent(in)                               :: radtype
   integer                                           :: i,last
   type(atom_type), allocatable                      :: atomf(:)
!
   do i=1,numfragments(frag) 
      if (frag(i)%nat == 1) then
          atom(frag(i)%pos(1))%rcod(4) = 1
      else
          call new_atoms(atomf,frag(i)%nat)
          atomf(:) = atom(frag(i)%pos)
          call set_overall_B(constr,atomf,radtype)
          atom(frag(i)%pos)%rcod(4) = atomf%rcod(4)
          last = ubound(constr,dim=1)
          constr(last)%vat(:) = frag(i)%pos(constr(last)%vat)
      endif
   enddo
!
   end subroutine set_molecules_B

 !--------------------------------------------------------------------------------------------------

   subroutine set_groups_B(constr,atom,vat,radtype)
!
!  Set an overall B for each molecular fragment
!
   USE strutil
   USE atom_type_util
   USE connect_mod
   USE fragmentmod
   type(constraint_type), allocatable, intent(inout) :: constr(:)
   type(atom_type), allocatable, intent(inout)       :: atom(:)
   integer, dimension(:), intent(in)                 :: vat(:)
   integer, intent(in)                               :: radtype
   integer                                           :: last
   type(atom_type), allocatable                      :: atomf(:)
!
   if (size(vat) == 1) then
       atom(vat(1))%rcod(4) = 1
   else
       call new_atoms(atomf,size(vat))
       atomf(:) = atom(vat)
       call set_overall_B(constr,atomf,radtype)
       atom(vat)%rcod(4) = atomf%rcod(4)
       last = ubound(constr,dim=1)
       constr(last)%vat(:) = vat(constr(last)%vat)
   endif
!
   end subroutine set_groups_B

!--------------------------------------------------------------------------------------------------

   subroutine check_constraints(constr)
!
!  Check e modify constraints considering the presence of same parameters
!
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
   type(constraint_type)                                           :: conm
   integer                                                         :: i,j,nc
   integer, dimension(2)                                           :: vcomp
   logical, allocatable, dimension(:)                              :: vrem
!
   nc = nconstraints(constr)  
   if (nc == 0) return
!
   allocate(vrem(nc), source=.false.)
   do i=1,nc-1
      if (vrem(i)) cycle
      do j=i+1,nc
         if (vrem(j)) cycle
            !write(0,*)i,'VAT=',allocated(constr(i)%vat)
         vcomp = constr_compare(constr(i),constr(j))
         if (vcomp(1) /= 0) then
             !write(6,*)'compare',i,j,'=',vcomp
             conm = constr_merge(constr(i),constr(j),vcomp)
             !write(6,*)'VAT=',allocated(conm%vat)
             !write(6,*)j,' was updated'
             constr(j) = conm
             !write(0,*)'VAT=',constr(j)%vat
             !call add_constraint(constr,conm)
             vrem(i) = .true.
             !vrem(j) = .true.
         endif
      enddo
   enddo
!
!  find constraints with blocked atoms
!   
!  delete constraints
   do i=nc,1,-1
      if (vrem(i))call delete_constraint(constr,i)
   enddo
!
   end subroutine check_constraints

!--------------------------------------------------------------------------------------------------

   function constr_compare(con1,con2) result(vcomp)
!
!  Find similar parameters in constraints con1 and con2
!
   type(constraint_type), intent(in) :: con1,con2
   integer, dimension(2)             :: vcomp
   integer                           :: i,j
!
   vcomp(:) = 0
   do i=1,con1%npar
      do j=1,con2%npar
         if ((con1%vat(i) == con2%vat(j)) .and. (con1%vpar(i) == con2%vpar(j)) .and. (con1%code == con2%code)) then
             vcomp(:) = [i,j]
             return
         endif
      enddo
   enddo
!
   end function constr_compare

!--------------------------------------------------------------------------------------------------
   
   function constr_merge(con1,con2,vcomp)  result(con3)
!
!  Merge 2 constraints
!
   USE math_util
   type(constraint_type), intent(in)  :: con1,con2
   type(constraint_type)              :: con3
   integer, dimension(2)              :: vcomp
   integer                            :: i,kp
   integer, dimension(:), allocatable :: vpar3,vat3
   real, dimension(:), allocatable    :: coef3
   integer                            :: refcode,typec
!
   if (vcomp(1) == 0) return
   con3%npar = con1%npar + con2%npar - 1
   allocate(vpar3(con3%npar),vat3(con3%npar),coef3(con3%npar))
   !if (equal_vector(con1%coef,[(1.0,i=1,con1%npar)]) .and. con2%npar == vcomp(2)) then
   !if (equal_vector(con1%coef,[(1.0,i=1,con1%npar)])) then
   !    vpar3(1:con2%npar) = con2%vpar
   !    vat3(1:con2%npar) = con2%vat
   !    coef3(1:con2%npar) = con2%coef
   !    kp = con2%npar
   !    do i=1,con1%npar
   !       if (i == vcomp(1)) cycle
   !       kp = kp + 1
   !       vpar3(kp) = con1%vpar(i)
   !       vat3(kp) = con1%vat(i)
   !       coef3(kp) = con1%coef(i)
   !    enddo
   !    call set_constraint(con3,con1%code,vpar3,vat3,coef3,con1%npari,0,1) 
   !    return
   !endif
   !if (equal_vector(con2%coef,[(1.0,i=1,con1%npar)]) .and. con1%npar == vcomp(1)) then
   !if (equal_vector(con2%coef,[(1.0,i=1,con2%npar)])) then
   !write(6,*)'MERGE con1'
   !call con1%prn(6)
   !write(6,*)'MERGE con2'
   !call con2%prn(6)
   !if (con1%coef(vcomp(1)) == con2%coef(vcomp(2))) then ! same coefficients!
   vpar3(1:con1%npar) = con1%vpar
   vat3(1:con1%npar) = con1%vat
   coef3(1:con1%npar) = con1%coef
   if (con1%ref > 0 .or. con2%ref > 0) then
       refcode = 1
   else
       refcode = 0
   endif
   kp = con1%npar
   do i=1,con2%npar
      !if (i == vcomp(2)) cycle
      if (in_constraint(con1,con2%vat(i),con2%vpar(i))) cycle
      kp = kp + 1
      vpar3(kp) = con2%vpar(i)
      vat3(kp) = con2%vat(i)
      coef3(kp) = con2%coef(i)
   enddo
   con3%npar = kp
   if (con1%typec == 1 .or. con2%typec == 1) then
       typec = 1
   else
       typec = 2
   endif
   call set_constraint(con3,con1%code,vpar3(:kp),vat3(:kp),coef3(:kp),0.0,con1%npari,refcode,typec=typec) 
   call normalize_constr(con3)
!   write(6,*)'AFTER MERGE con3'
!   call con3%prn(6)
   !    return
   !endif
!
   end function constr_merge

!--------------------------------------------------------------------------------------------------

   subroutine normalize_constr(constr)
!
!  Last parameter should have coef equal 1.0
!
   USE nrutil
   type(constraint_type), intent(inout) :: constr
   real, parameter                      :: EPS = epsilon(1.0)
   integer                              :: cfound,i
!
   if (abs(constr%coef(constr%npar) - 1.0) <= EPS) return
!
   cfound = 0
   do i=1,constr%npar-1
      if (abs(constr%coef(i) - 1.0) <= EPS)  then    
          cfound = i
          exit
      endif
   enddo
   if (cfound > 0) then
       call swap(constr%vpar(i),constr%vpar(constr%npar))
       call swap(constr%vat(i),constr%vat(constr%npar))
       call swap(constr%coef(i),constr%coef(constr%npar))
   endif
!
   end subroutine normalize_constr

!--------------------------------------------------------------------------------------------------

   real function constr_aveb(constr,atom,bmin,bmax)  result(bmed)
!
!  Compute an average b for atom in constr
!
   USE atom_type_util
   type(constraint_type), intent(in)         :: constr
   type(atom_type), dimension(:), intent(in) :: atom
   real, intent(in)                          :: bmin,bmax
!
   bmed = sum(atom(constr%vat)%biso) / constr%npar
   if (bmed < bmin) bmed = bmin
   if (bmed > bmax) bmed = bmax
!
   end function constr_aveb
   
!--------------------------------------------------------------------------------------------------

   integer function par_in_constraint(constr,kat,npar,par) result(kconstr)
!
!  Controlla se un atomo e' implicato in un constraint
!
   type(constraint_type), dimension(:), allocatable, intent(in) :: constr
   integer, intent(in)                                          :: kat   ! n. dell'atomo
   integer, intent(in)                                          :: npar  ! n. del parametro
   integer, intent(out), optional                               :: par   ! n. del parametro nel constraints
   integer                                                      :: i,j
!
   kconstr = 0
   loop_constr: do i=1,nconstraints(constr)     
      loop_par: do j=1,constr(i)%npar  
         if (constr(i)%vpar(j) == npar .and. constr(i)%vat(j) == kat) then
             kconstr = i
             if (present(par)) par = j
             exit loop_constr
         endif
      enddo loop_par
   enddo loop_constr
!
   end function par_in_constraint

!--------------------------------------------------------------------------------------------------

   logical function in_constraint(constr,kat,par)
   type(constraint_type), intent(in) :: constr
   integer, intent(in)               :: kat,par
   integer                           :: i
   in_constraint = .false.
   do i=1,constr%npar
      if ((constr%vat(i) == kat) .and. (constr%vpar(i) == par)) then
          in_constraint = .true.
          return
      endif
   enddo
   end function in_constraint

!--------------------------------------------------------------------------------------------------

   real function coef_of_constraint(constr,nat,npar) result(coef)
!
!  Prende il coefficente del parametro npar del constraint
!
   type(constraint_type), intent(in) :: constr
   integer, intent(in) :: nat      !l'atomo
   integer, intent(in) :: npar     !il parametro
   integer             :: i
!
   coef = 0.0
   do i=1,constr%npar
      if (constr%vpar(i)==npar .and. constr%vat(i) == nat) then
          coef = constr%coef(i)
      endif
   enddo
!
   end function coef_of_constraint

!--------------------------------------------------------------------------------------------------

   logical function blocked_constraint(constr,atom)  result(blocked)
!
!  Has constraint blocked parameters?
!
   USE atom_basic
   class(constraint_type), intent(in)           :: constr
   type(atom_type), dimension(:), intent(inout) :: atom
   integer                                      :: i
!
   blocked = .false.
   do i=1,constr%npar
      blocked =  atom(constr%vat(i))%rcod(constr%vpar(i)) < 0
      if (blocked) return
   enddo
!
   end function blocked_constraint

!--------------------------------------------------------------------------------------------------

   subroutine refine_constraint_s(constr,atom,coderef)
!
!  Chiedo di affinare (coderef=1) o non affinare (coderef=0) un constraint
!
   USE atom_basic
   type(constraint_type), intent(inout)         :: constr
   type(atom_type), dimension(:), intent(inout) :: atom
   integer, intent(in)                          :: coderef
   integer                                      :: i
   integer                                      :: kat,kpar
!
   if (constr%code == CONSTR_CO .and. coderef == 1 .and. constr%blocked(atom)) return   !Do not refine constraints with x,y,z blocked
!
   constr%ref = coderef
!
!  se affino il constraint tutti i parametri coinvolti nel constr. vanno fissati 
   do i=1,constr%npar
      kat=constr%vat(i)
      kpar=constr%vpar(i)
      if (atom(kat)%rcod(kpar) > 0) atom(kat)%rcod(kpar) = 0
   enddo
!
   end subroutine refine_constraint_s

!--------------------------------------------------------------------------------------------------

   subroutine refine_constraint_arr(constrv,atom,code)
!
!  Chiedo di affinare (code=1) o non affinare (code=0) un constraint
!
   USE atom_basic
   type(constraint_type), dimension(:), intent(inout), allocatable :: constrv
   type(atom_type), dimension(:), intent(inout)                    :: atom
   integer, intent(in)                                             :: code
   integer                                                         :: i
!
   do i=1,nconstraints(constrv)
      call refine_constraint_s(constrv(i),atom,code)
   enddo
!
   end subroutine refine_constraint_arr

!--------------------------------------------------------------------------------------------------

   subroutine delete_constraint(constr,id)
!
!  Delete constraint id 
!
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
   integer, intent(in)                                             :: id
   integer                                                         :: ncon
!
   ncon = nconstraints(constr)
   if (ncon == 0 .or. id <= 0 .or. id > ncon) return
!
   constr(id:ncon-1) = constr(id+1:)
   call resize_constraints(constr,ncon-1)
!
   end subroutine delete_constraint

!--------------------------------------------------------------------------------------------------

   subroutine delete_all_constraints(constr,code,typec)
!
!  Elimina tutti i constrain di codice uguale a code
!
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
   integer, intent(in)                       :: code
   integer, intent(in), optional             :: typec
   integer                                   :: nconstrn
   integer                                   :: i
!
   if (nconstraints(constr) == 0) return
!
   nconstrn = 0
   if (present(typec)) then
       do i=1,nconstraints(constr)
          if (constr(i)%code /= code .or. constr(i)%typec /= typec) then
              nconstrn = nconstrn + 1
              constr(nconstrn) = constr(i)
          endif 
       enddo
   else
       do i=1,nconstraints(constr)
          if (constr(i)%code /= code) then
              nconstrn = nconstrn + 1
              constr(nconstrn) = constr(i)
          endif 
       enddo
   endif
   call resize_constraints(constr,nconstrn)
   !   write(0,*)nconstrn,'size constrv=',size(constrv)
   !do i=1,nconstrn
   !   constrv(i) = constrvn(i)
   !enddo
   !if (nconstrn > 0) constrv(:nconstrn) = constrvn(:nconstrn)
!corr   nconstr = nconstrn
!
   end subroutine delete_all_constraints

!--------------------------------------------------------------------------------------------------

   subroutine resize_constraints(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo restraint
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(constraint_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                              :: n
   logical, optional, intent(in)                    :: savevet
   logical                                          :: savev
   integer                                          :: nv
   type(constraint_type), allocatable                :: vsav(:)
   integer                                          :: nsav
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
!          nsav contiene qual è la porzione di vetr da salvare
           select case(nv-n)
             case (1:)       ! compatta x ad n
               nsav = n
             case (:-1)      ! espandi x ad n
               nsav = nv
             case (0)
               return        ! n=nv non fare niente
           end select
!
!          salva vetr fino a nsav
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
   end subroutine resize_constraints

!--------------------------------------------------------------------------------------------------

   subroutine new_constraints(vetr,n)
!
!  Create new constraints
!
   type(constraint_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                               :: n

   if (n < 0) return
   if (nconstraints(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_constraints

!--------------------------------------------------------------------------

   subroutine reset_constraints(constr)
!
!  Azzera constraints
!
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
   if (allocated(constr)) deallocate(constr)
!
   end subroutine reset_constraints

!--------------------------------------------------------------

   subroutine copy_constraints(constr1,constr2)
!
!  Copy constraints constr2 in constr1
!
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr1
   type(constraint_type), dimension(:), allocatable, intent(in)    :: constr2
   integer                                                         :: nc1,nc2
!
   nc1 = nconstraints(constr1)
   nc2 = nconstraints(constr2)
   if (nc1 == nc2) then
       if (nc1 /= 0) constr1 = constr2
   else
      if (allocated(constr1)) deallocate(constr1)
      if (nc2 > 0) then
          allocate(constr1(nc2),source=constr2)
      endif
   endif
!
   end subroutine copy_constraints

!--------------------------------------------------------------

   subroutine push_back_constraint_s(constrv,con)
!
!  Add constraint con to array constrv
!
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constrv
   type(constraint_type), intent(in)                               :: con
   integer                                                         :: nconstr
!
   nconstr = nconstraints(constrv) + 1
   call resize_constraints(constrv,nconstr)    
   constrv(nconstr) = con
!
   end subroutine push_back_constraint_s

!--------------------------------------------------------------

   subroutine push_back_constraint_v(constrv,con)
!
!  Add constraint con to array constrv
!
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constrv
   type(constraint_type), dimension(:), allocatable, intent(in)    :: con
   integer                                                         :: nconstr
!
   if (nconstraints(con) == 0) return
   nconstr = nconstraints(constrv)
   call resize_constraints(constrv,nconstr+size(con))    
   constrv(nconstr+1:) = con
!
   end subroutine push_back_constraint_v

!--------------------------------------------------------------------------------------------------

   subroutine set_constr_on_position(constr,atom,spg,cell)
!
!  Cerca atomi in posizione speciale generando nconstr constraints
!  e riempiendo  constrv da usare poi nel calcolo delle derivate dei constraints
!
!corr   Use General, only: newvet
   USE atom_basic
   USE unit_cell
   USE spginfom
   USE kspec_mod
!
   implicit none
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
   type(atom_type), dimension(:), intent(inout)                    :: atom
   type(spaceg_type), intent(in)                                   :: spg
   type(cell_type), intent(in)                                     :: cell
   integer                  :: i,k,num
   real   , dimension(11)   :: xo,xn
   integer, dimension(10)   :: key
   integer                  :: ksp,khead
   integer                  :: num_key_eq
   integer, dimension(3)    :: vat,vpar
   real, dimension(3)       :: vcoef
   integer                  :: npar
   integer                  :: kp,js
!
!  Non so se le seguenti  3 righe servono
   js = lattice_system(spg,cell%get_par())
!
   !!!!!call delete_constraint(code=2,typec=2)
   do i=1,size(atom)
      xo(1:3)  = atom(i)%xc(:) 
      if (i == 1) khead = 0 
      ksp=kspecb_new(xo,xn,key,spg,js,cell%get_g(),2,i,atom(i)%lab,khead)
!corr      atom(i)%xc(:) = xn(1:3)
!
!     Se key(1:3) e' del tipo 1,2,3 e quindi la somma e' 6
!     l' atomo non e' in posizione speciale
      if (sum(key(1:3)) == 6) cycle
!
!     Se l'atomo e' in posizione speciale controllo se nel vettore key
!     c'e' la ripetizione dei numeri 1,2,3 (in valore assoluto)
!     num_key_eq = indica quante volte si ripete key
      num_key_eq = 0
      do num=1,3
         do k=1,3
            if (iabs(key(k)) == num) then
                num_key_eq = num_key_eq + 1
            elseif ( mod(iabs(key(k)),10) == num) then
                num_key_eq = num_key_eq + 1
                key(k) = sign(2,key(k))
            else
                key(k) = 0
            endif
         enddo
         if (num_key_eq > 1) then
             npar = 0
             do kp=1,3
                if (key(kp) /= 0) then
                    npar = npar + 1
                    vat(npar) = i
                    vpar(npar) = kp
                    vcoef(npar) = key(kp)
                endif
             enddo
             call add_constraint(constr,2,vpar(:npar),vat(:npar),atom(:),vcoef(:npar),npari=1,typec=2)
             call normalize_constr(constr(nconstraints(constr)))
             exit
         endif
      enddo
   enddo
!
   end subroutine set_constr_on_position

!--------------------------------------------------------------

   subroutine set_constr_on_sof(atom,constr,cellt)
!
!  Set constraints on sof. Should be called after set_lsq_conditions
!
   USE atom_type_util
   USE unit_cell
   type(atom_type), dimension(:), intent(inout)                    :: atom
   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
   type(cell_type), intent(in)                                     :: cellt
   type(atom_type), dimension(size(atom))                          :: atomc
   integer                                                         :: nat
   integer, dimension(size(atom),size(atom))                       :: softable
   integer, dimension(size(atom))                                  :: vet
   integer                                                         :: i,j
   real, dimension(3)                                              :: xdiff
   real                                                            :: dist,sumc
   integer                                                         :: npos,nposc,ico
!
   nat = size(atom)   
   atomc(:) = atom(:)
   call frac_to_cart(atomc,cellt%get_ortom())
   softable(:,:) = 0
   vet(:) = 0
!
!  Riempi la tabella softable, usa vet per escudere gli atomi gia' presi
   do i=1,nat-1
      if (vet(i) == 1) cycle
      do j=i+1,nat
         if (vet(j) == 1) cycle
         xdiff(:) = atom(i)%xc - atom(j)%xc
         dist = sqrt(xdiff(1)**2+xdiff(2)**2+xdiff(3)**2)
         if (dist < 0.001) then
             softable(i,j) = 1
             vet(j) = 1
         endif
      enddo
   enddo
!
!  Aggiungi i constraints
   vet(:) = 0
   do i=1,nat
      nposc = sum(softable(i,:)) + 1
      if (nposc > 1) then
            !write(0,*)i,'sof=',softable(i,:)
!
!         In vet gli atomi che sono nella stessa posizione
          vet(1) = i
          npos = 1
          do j=1,nat
             if (softable(i,j) == 1) then
                 npos = npos + 1
                 vet(npos) = j
             endif
          enddo
!
!         constrain sull'occupanza
          sumc = sum(atom(vet(:npos))%och)  ! define sumc in constraint: occ1 + occ2 + .. = sumc
          call add_constraint(constr,3,(/(5,j=1,npos)/),vet(:npos),atom,npari=npos-1,sumc=sumc)
                  !write(0,*)'OCC=',atom(vet(:npos))%och
                !call constr(size(constr))%prn(0)
!
!         constrain sulla posizione
          loop_coord: do ico=1,3  ! loop on coordinates x,y,z
!
!            Check if coordinates are blocked for polar axis or special position (set_lsq_conditions)
             do j=1,npos
                if (atom(vet(j))%rcod(ico) < 0) then
                    atom(vet(:npos))%rcod(ico) = atom(vet(j))%rcod(ico)  ! apply the same code to all atoms in the groups
                    cycle loop_coord                                     ! go to next coordinate
                endif
             enddo
             call add_constraint(constr,2,(/(ico,j=1,npos)/),vet(1:npos),atom,npari=1,typec=2)  ! apply constraint on ico coordinate
          enddo loop_coord
!
          !atom(vet(:npos))%och = 1.0/npos
!
!         constrain sui b
          call add_constraint(constr,1,(/(4,j=1,npos)/),vet(:npos),atom,npari=1,typec=2)

!!!          write(0,*)'Atomi ',vet(:npos),' are in the same site'
      endif
   enddo
!
   end subroutine set_constr_on_sof

!--------------------------------------------------------------------------------
!corr
!corr   subroutine make_constraints(atom,legm,constr,spg,cell,riding_model,adp_factor)
!corr!
!corr!  Set program constraints
!corr!
!corr   USE unit_cell
!corr   USE spginfom
!corr   USE atom_type_util
!corr   USE setref
!corr   USE connect_mod
!corr!
!corr   implicit none
!corr   type(atom_type), dimension(:), allocatable, intent(inout)       :: atom
!corr   type(bond_type), dimension(:), allocatable, intent(in)          :: legm
!corr   type(constraint_type), dimension(:), allocatable, intent(inout) :: constr
!corr   type(spaceg_type), intent(in)                                   :: spg
!corr   type(cell_type), intent(in)                                     :: cell
!corr   logical, intent(in)                                             :: riding_model
!corr   real, intent(in)                                                :: adp_factor
!corr   integer :: i
!corr!
!corr!  Delete program constraints
!corr   do i=1,3
!corr      call delete_all_constraints(constr,code=i,typec=2)
!corr   enddo
!corr!
!corr!  Set constraits for special positions
!corr   call set_constr_on_position(constr,atom,spg,cell)
!corr   !call print_lsqcond()
!corr   call set_lsq_conditions(atom,spg,cell)  ! and polar axis
!corr!
!corr!  Constraints on sof, set_lsq_conditions must be called before this
!corr   call set_constr_on_sof(atom,constr,cell)
!corr!
!corr   if (riding_model) then
!corr       call add_constraints_for_riding(constr,atom,legm,spg,cell,adp_factor)
!corr   endif
!corr!
!corr!  Isolate independent equations, e.g. B constr + constr of riding; B constr + constr. on site
!corr   call check_constraints(constr)
!corr   !call print_constraints(constr,atom,0,is_all=.true.)
!corr!
!corr   end subroutine make_constraints
!corr
!------------------------------------------------------------------------

   subroutine print_constraints(constr,atom,kpr,is_all,onshift)
!
!  Print constraints
!
   USE atom_basic
   USE strutil
   type(constraint_type), dimension(:), allocatable, intent(in) :: constr
   type(atom_type), dimension(:), intent(in)                    :: atom
   integer, intent(in)                                          :: kpr
   logical, intent(in), optional                                :: is_all  !stampa tutti i constraints
   logical, intent(in), optional                                :: onshift !constraints on shifts?
   integer                                                      :: i
   integer, dimension(:), allocatable                           :: vprint
   integer                                                      :: nconstr
   logical                                                      :: onshift1
!
   nconstr = nconstraints(constr)
   if (nconstr > 0) then
       allocate(vprint(nconstr))
       if (present(is_all)) then
           if (is_all) then
               vprint(:) = 1
           else
               vprint(:) = constr(:)%ref ! print only refined constraints
           endif
       else
           vprint(:) = constr(:)%ref ! print only refined constraints
       endif
!
       if (any(vprint(:) > 0)) then
           if (present(onshift)) then
               onshift1 = onshift
           else
               onshift1 = .true.
           endif
           if (onshift1) then
               write(kpr,'(a)')centra_str('Costraints on L.Sq. shifts',80)
           else
               write(kpr,'(a)')centra_str('Costraints on parameters',80)
           endif
       endif
   endif
!
   do i=1,nconstr
      if (vprint(i) > 0) then
          select case (constr(i)%code)
             case (1:2)
               write(kpr,'(2x,a)')string_constraint(constr(i),atom)

             case (3)
               write(kpr,'(2x,a)')string_constraint(constr(i),atom)
           
             case (4)
               write(kpr,'(2x,a)')string_constraint(constr(i),atom)
          end select
      endif
   enddo
!
   end subroutine print_constraints

!--------------------------------------------------------------------------------------------------

   function string_constraint(constr,atom)  result(stringc)
   USE atom_basic
   USE strutil
   type(constraint_type), intent(in)         :: constr
   type(atom_type), dimension(:), intent(in) :: atom
   character(len=:), allocatable             :: stringc
   integer                                   :: kat,kpar,j,natc
   character(len=1), dimension(4)            :: xyzstring = (/'x','y','z','B'/)
!
   select case (constr%code)

      case (1:2)
        stringc = ' '
        natc = size(constr%vat)
!
!       coef is the same in chain of equalities coef*x1 = coef*x2 = ...=xn, where xn is indipendent
!                                               B(at1) = coef*x1, B(at2) = coef*x2, .., B(atn) = xn
        if (all(constr%coef(:) == 1.0)) then
!
!           simple chain of equalities
            kat = constr%vat(1)
            kpar = constr%vpar(1)
            stringc = xyzstring(kpar)//'('//trim(atom(kat)%lab)//')'
            do j=2,natc
               kat = constr%vat(j)
               kpar = constr%vpar(j)
               stringc = trim(stringc)//' = '//xyzstring(kpar)//'('//trim(atom(kat)%lab)//')'
            enddo
        else
            kat = constr%vat(natc)
            kpar = constr%vpar(natc)
            stringc = 'par='//xyzstring(kpar)//'('//trim(atom(kat)%lab)//');'
            do j=1,natc-1
               kat = constr%vat(j)
               kpar = constr%vpar(j)
               if (constr%coef(j) == 1.0) then
                   stringc = trim(stringc)//' '//xyzstring(kpar)//'('//trim(atom(kat)%lab)//')='//'par'
               else
                   stringc = trim(stringc)//' '//xyzstring(kpar)//'('//trim(atom(kat)%lab)//')='//  &
                         trim(r_to_s(constr%coef(j),1))//'*'//'par'
               endif
               if (j /= natc-1) stringc = trim(stringc)//';'
            enddo
        endif

      case (3)
        stringc = ' '
        kat = constr%vat(1)
        stringc = 'occ('//trim(atom(kat)%lab)//')'
        do j=2,size(constr%vat)
           kat = constr%vat(j)
           stringc = trim(stringc)//' + occ('//trim(atom(kat)%lab)//')'
        enddo
        stringc=trim(stringc)//' = '//r_to_s(constr%sumc)
        !write(kpr,'(2x,a)')trim(stringc)

      case (4)
        stringc='H = '//'B('//trim(atom(constr%vat(1))%lab)//')'
        !write(kpr,'(2x,a)')'H = '//'B('//trim(atom(constr(i)%vat(1))%lab)//')'
   end select
!
   end function string_constraint

!--------------------------------------------------------------------------------------------------

   subroutine constr_tf_from_string(str,constr,atom,legm,frag,elem,radtype,ier)
!
!  Set constraint on thermal factor from string
!
   USE strutil
   USE atom_type_util
   USE connect_mod
   USE elements
   USE fragmentmod
   USE arrayutil
   character(len=*), intent(in)                      :: str
   type(constraint_type), allocatable, intent(inout) :: constr(:)
   type(atom_type), allocatable, intent(inout)       :: atom(:)
   type(bond_type), allocatable, intent(in)          :: legm(:)
   type(fragment_type), allocatable, intent(in)      :: frag(:)
   type(element_type), allocatable, intent(in)       :: elem(:)
   integer, intent(in)                               :: radtype
   integer, intent(out)                              :: ier
   character(len=len_trim(str))                      :: str1,word
   integer                                           :: nlen2,i
   character(len=20), dimension(4)                   :: wordv
   integer                                           :: nword
   integer, dimension(size(atom))                    :: vat1
   integer                                           :: nat1
   integer, dimension(:), allocatable                :: vatot
!
   ier = 0
   str1 = str
   call cutst(str1,line2=word,nlong2=nlen2)
   if (nlen2 == 0) then
       ier = 1
       return
   endif
!
   select case(word)
     case ('overall')
       call set_overall_B(constr,atom,radtype)

     case ('species')
       call set_species_B(constr,atom,legm,elem,radtype)

     case ('molecules')
       call set_molecules_B(constr,atom,frag,radtype)
  
     case ('atoms')
       call get_words(str1,wordv,nword)
       if (nword == 0) then
           ier = 3
       else
           do i=1,nword
              call get_atoms_of_string(wordv(i),atom,vat1,nat1)
              if (nat1 > 0) then
                  atom(vat1(:nat1))%rcod(4) = 1
              else
                  ier = 3
                  return
              endif
           enddo
       endif

     case ('groups')
       call get_words(str1,wordv,nword)
       if (nword == 0) then
           ier = 4
       else
           do i=1,nword
              call get_atoms_of_string(wordv(i),atom,vat1,nat1)
              if (nat1 > 0) then
                  call append_array(vatot,vat1(:nat1))
              else
                  ier = 4
                  return
              endif
           enddo
       if (size_array(vatot) > 0) call set_groups_B(constr,atom,vatot,radtype)
       endif
    
     case default
       ier = 2
   end select
!
   end subroutine constr_tf_from_string

!--------------------------------------------------------------------------------------------------

   subroutine make_constr_adp(adp_type,constr,atom,legm,frag,elem,radtype)
   USE atom_type_util
   USE connect_mod
   USE elements
   USE fragmentmod
   USE arrayutil
   integer, intent(in)                               :: adp_type
   type(constraint_type), allocatable, intent(inout) :: constr(:)
   type(atom_type), allocatable, intent(inout)       :: atom(:)
   type(bond_type), allocatable, intent(in)          :: legm(:)
   type(fragment_type), allocatable, intent(in)      :: frag(:)
   type(element_type), allocatable, intent(in)       :: elem(:)
   integer, intent(in)                               :: radtype
!
   select case(adp_type)
     case (CONSTR_ADP_NONE)
     case (CONSTR_ADP_OVERALL)
       call set_overall_B(constr,atom,radtype)
     case (CONSTR_ADP_SPECIES)
       call set_species_B(constr,atom,legm,elem,radtype)
     case (CONSTR_ADP_MOLECULES)
       call set_molecules_B(constr,atom,frag,radtype)
   end select
!
   end subroutine make_constr_adp

!--------------------------------------------------------------------------------------------------

   subroutine constraint_from_equation(str,atom,constr,err)
!
!  Set constraint from equation
!
   USE errormod
   USE atom_basic
!corr   USE elements
   character(len=*), intent(in)                                  :: str
   type(atom_type), allocatable, intent(inout)                   :: atom(:)
   type(constraint_type), dimension(:), allocatable, intent(out) :: constr
!corr   type(element_type), allocatable, intent(in)                   :: elem(:)
   type(error_type), intent(out)                                 :: err
   integer, dimension(:), allocatable                            :: vat,vpar
   real                                                          :: knwt
   logical                                                       :: eqtype
   integer                                                       :: i,ico,j,nconstr
!
   call equation_from_string(str,atom,vat,vpar,knwt,eqtype,err)
   if (err%signal) return
         !write(0,*)'VAT=',atom(vat(:))%lab
         !write(0,*)'VPAR=',vpar
         !write(0,*)'TYPE=',eqtype
         !write(0,*)'knwt=',knwt
!
   if (all(vpar ==  4)) then   
!
!      Constraints on thermal factor
       call new_constraints(constr,1)
       call set_constraint(constr(1),CONSTR_B,[(4,i=1,size(vat))],vat,[(1.0,i=1,size(vat))],0.0,1,0,1)
       call refine_constraint(constr(1),atom,0)  ! reset refinement codes for atoms
   elseif (all(vpar == 1)) then
!
!      Constraint on position
       nconstr = 0
       call new_constraints(constr,3)
       loop_coord: do ico=1,3  ! loop on coordinates x,y,z
!
!         Check if coordinates are blocked for polar axis or special position (set_lsq_conditions)
          do j=1,size(vat)
             if (atom(vat(j))%rcod(ico) < 0) then
                 atom(vat(:))%rcod(ico) = atom(vat(j))%rcod(ico)  ! apply the same code to all atoms in the groups
!corr                 cycle loop_coord                                 ! go to next coordinate
             endif
          enddo
          nconstr = nconstr + 1
          call set_constraint(constr(nconstr),CONSTR_CO,[(ico,j=1,size(vat))],vat,[(1.0,i=1,size(vat))],0.0,1,0,1)  ! apply constraint on ico coordinate
          call refine_constraint(constr(nconstr),atom,0)  ! reset refinement codes for atoms
       enddo loop_coord
       call resize_constraints(constr,nconstr)

   elseif (all(vpar == 5)) then
!
!      Constraints on SOF
       call new_constraints(constr,1)
       call set_constraint(constr(1),CONSTR_SOF,[(5,i=1,size(vat))],vat,[(1.0,i=1,size(vat))],knwt,size(vat)-1,0,1)
       call refine_constraint(constr,atom,0) 
   else
       call err%set('Constraint '//trim(str)//' was not accepted')
   endif
!
   end subroutine constraint_from_equation

!--------------------------------------------------------------------------------------------------

   subroutine equation_from_string(str,atom,vat,vpar,knwt,eqtype,err)
!
!  Parse equation  extracting parameters
!
   USE strutil
   USE atom_type_util
   USE connect_mod
!corr   USE elements
   USE fragmentmod
   USE arrayutil
   USE errormod
   character(len=*), intent(in)                    :: str
   type(atom_type), allocatable, intent(in)        :: atom(:)
!corr   type(element_type), allocatable, intent(in)     :: elem(:)
   integer, dimension(:), allocatable, intent(out) :: vat,vpar
   real, intent(out)                               :: knwt
   logical,  intent(out)                           :: eqtype
   type(error_type), intent(out)                   :: err
   character(len=:), allocatable                   :: str1
!corr   integer                                         :: pos
   character(len=:), dimension(:), allocatable     :: wordv
   integer                                         :: nword,i,j
   integer, dimension(size(atom))                  :: vats
   integer                                         :: natom,kpar
   integer                                         :: npartot
   character(len=:), allocatable                   :: sig
   integer                                         :: sizew
   integer                                         :: ier
!
!  Strip '+' and '=' not in brackets
   str1 = s_blank_delete(str)
   sig = ' '
   do i=1,len_trim(str1)
      if (str1(i:i) == '+' .or. str1(i:i) == '=') then
          if (is_in_brackets(str1,i)) cycle   ! str1(i:i) is in brackets?
          sig = trim(sig)//str1(i:i)
          str1(i:i) = ' '
      endif
   enddo
!corr   do 
!corr     pos = scan(str1,'+=')
!corr     if (pos == 0) exit    
!corr     !if (is_in_brackets(str1,pos)) exit   ! pos is in brackets?
!corr     if (is_in_brackets(str1,pos)) cycle   ! pos is in brackets?
!corr     sig = trim(sig)//str1(pos:pos)
!corr     str1(pos:pos) = ' '
!corr   enddo
!
   eqtype = all_ch_eq((sig),'=')  ! sequence of equalities
!
!  Extract parameters
   call get_words1(str1,wordv,nword)
!
!  Initial allocation
   if (eqtype .or. nword == 1) then
       sizew = nword
   else
       sizew = nword-1
   endif
   call new_array(vpar,sizew)
   call new_array(vat,sizew)
!
   npartot = 0
   do i=1,sizew
      !call get_param_of_string(wordv(i),atom,elem,kpar,vats,natom,err)
      call get_param_of_string(wordv(i),atom,kpar,vats,natom,err)
      !write(0,*)'NW=',i,wordv(i),natom
      if (err%signal) then
          call err%add(' in: '//trim(str))
          exit
      endif
!
      if (npartot + natom > size(vpar)) then
          call resize_array(vpar,npartot+natom)
          call resize_array(vat,npartot+natom)
      endif
      do j=1,natom
         npartot = npartot + 1
         vpar(npartot) = kpar
         vat(npartot) = vats(j)
      enddo
      !write(0,*)'PAR=',vpar(:npartot),vat(:npartot)
   enddo
   if (.not.eqtype) then  !known term is expected
       ier = s_to_r(wordv(nword),knwt)
       if (ier /= 0) call err%set('Error on reading known term in equation: '//trim(str))
   endif
!
   end subroutine equation_from_string

 END MODULE constraints
