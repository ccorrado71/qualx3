MODULE model_util

! S model_set(model,at,leg,fom)                    Definizione degli elementi del modello
! S model_get(model,at,leg,fom)                    Estrai informazioni dal modello
! S model_copy(model1,model2)                      Copy model1 in model2
! F nummodels(model)                               Il numero di modelli allocati
! S model_add(model,at,fom,leg)                    Aggiungi un modello alla lista
! S model_get_best(model,kselfom,at,fomv,leg)      Prendi il modello migliore dalla lista
! S model_sort(model,keycode,keyadd)               Ordina i modelli della lista dal più' piccolo al piu' grande
! S model_print_info(model,ninfoi,iveti,mark)      Stampa info sui modelli
! S model_update_minimum(model,at,fom,kselfom,leg,k) Aggiorna il modello migliore rimuovendo il peggiore (k)
! F model_average(model,kselfom)  result(ave)      Calcola il valore medio di una della grandezza fom(kselfom)
! S save_models_bin(unitbin,models)                Write models on binary file
! S read_models_bin(unitbin,models,ier)            Read models from binary file
! F find_model

USE connect_mod, only: bond_type
USE atom_basic, only: atom_type

implicit none

  integer, parameter :: NMAXINFO = 10
  type model_type
       type(atom_type), dimension(:), allocatable :: at
       type(bond_type), dimension(:), allocatable :: leg
       real, dimension(NMAXINFO)                  :: info
  end type model_type

private :: model_add_at, model_add_model
interface model_add
   module procedure model_add_at, model_add_model
end interface

private :: model_copy_scal, model_copy_vect
interface model_copy
   module procedure model_copy_scal, model_copy_vect
end interface

CONTAINS

   subroutine model_set(model,at,leg,fom)   !!!!!,symm)
!
!  Definizione degli elementi del modello
!
   USE atom_type_util, only: copy_atoms
   USE connect_mod, only: copy_bonds
   type(model_type), intent(inout)                                      :: model
   type(atom_type), dimension(:), allocatable, intent(in), optional     :: at
   type(bond_type), dimension(:), allocatable, intent(in), optional     :: leg
   real, dimension(:), intent(in), optional                             :: fom
   integer                                                              :: i
!
   if (present(at)) then
       call copy_atoms(model%at,at)
   endif
!
   if (present(leg)) then
       call copy_bonds(model%leg,leg)
   endif
!
   if (present(fom)) then
       do i=1,min(size(fom),NMAXINFO)
          model%info(i) = fom(i)
       enddo
   endif
!
   end subroutine model_set

!--------------------------------------------------------------------------------------
 
   subroutine model_get(model,at,leg,fom)
!
!  Estrai informazioni dal modello
!
   USE atom_type_util, only: copy_atoms
   USE connect_mod, only: copy_bonds
   type(model_type), intent(in)                                      :: model
   type(atom_type), dimension(:), allocatable, intent(out), optional :: at
   type(bond_type), dimension(:), allocatable, intent(out), optional :: leg
   real, dimension(:), intent(out), optional                         :: fom
   integer                                                           :: i
!
   if (present(at)) then
       call copy_atoms(at,model%at)
   endif
!
   if (present(leg)) then
       call copy_bonds(leg,model%leg)
   endif
!
   if (present(fom)) then
       do i=1,min(size(fom),NMAXINFO)
          fom(i) = model%info(i)
       enddo
   endif
!
   end subroutine model_get

!--------------------------------------------------------------------------------------------

   subroutine model_copy_scal(model1,model2)
!
!  Copy model1 in model2
!
   type(model_type), intent(in)  :: model1
   type(model_type), intent(out) :: model2
!
   call model_set(model2,model1%at,model1%leg,model1%info)   !!!!!,model1%symm)
!
   end subroutine model_copy_scal

!--------------------------------------------------------------------------------------------

   subroutine model_copy_vect(model1,model2)
!
!  Array copy of model1 in model2
!
   type(model_type), dimension(:), allocatable, intent(in)    :: model1
   type(model_type), dimension(:), allocatable, intent(inout) :: model2
   integer                                                    :: nmod1,i
!
   nmod1 = nummodels(model1)
   call new_models(model2,nmod1)
   do i=1,nmod1
      call model_copy_scal(model1(i),model2(i))
   enddo
!
   end subroutine model_copy_vect

!--------------------------------------------------------------------------------------------

   integer function nummodels(model)
!
!  Il numero di modelli allocati
!
   type(model_type), dimension(:), allocatable, intent(in) :: model
!   
   if (allocated(model)) then
       nummodels = size(model)
   else
       nummodels = 0
   endif
!
   end function nummodels

!--------------------------------------------------------------------------------------------

   subroutine model_add_at(model,at,fom,leg)
!
!  Aggiungi un modello alla lista
!
   type(model_type), dimension(:), allocatable, intent(inout)       :: model  
   type(atom_type), dimension(:), allocatable, intent(in)           :: at
   real, dimension(:), intent(in), optional                         :: fom
   type(bond_type), dimension(:), allocatable, intent(in), optional :: leg
   integer                                                          :: nmodels
!
   nmodels = nummodels(model) + 1
   call resize_models(model,nmodels)
   if (present(leg)) then
       call model_set(model(nmodels),at,leg,fom)
   else
       call model_set(model(nmodels),at,fom=fom)
   endif
!  
   end subroutine model_add_at
  
!--------------------------------------------------------------------------------------------

   subroutine model_add_model(model,model_to_add,nadd)
!
!  Add new models to the list
!
   type(model_type), dimension(:), allocatable, intent(inout) :: model
   type(model_type), dimension(:), allocatable, intent(in)    :: model_to_add
   integer, intent(in), optional                              :: nadd
   integer                                                    :: nmodels, nmodels_to_add, nmodels_tot
   integer                                                    :: i
!
   if (present(nadd)) then
       nmodels_to_add = min(nummodels(model_to_add),nadd)
   else
       nmodels_to_add = nummodels(model_to_add)
   endif
   if (nmodels_to_add == 0) return  ! nothing to add
!
   nmodels = nummodels(model)
   nmodels_tot = nmodels + nmodels_to_add
   call resize_models(model,nmodels_tot)
   do i=1,nmodels_to_add
      call model_set(model(nmodels+i),model_to_add(i)%at,model_to_add(i)%leg,    &
                     model_to_add(i)%info)   !!!!!,model_to_add(i)%symm)
   enddo
!  
   end subroutine model_add_model

!--------------------------------------------------------------------------------------------

   subroutine model_get_best(model,kselfom,at,fomv,leg,keyadd)
!
!  Prendi il modello migliore dalla lista
!
   type(model_type), dimension(:), intent(in)              :: model     
   integer, intent(in)                                     :: kselfom
   type(atom_type), dimension(:), allocatable, intent(out) :: at   
   real, dimension(:), intent(out)                         :: fomv
   type(bond_type), dimension(:), allocatable, intent(out), optional :: leg   
   integer, intent(in), optional                           :: keyadd
   integer, dimension(1)                                   :: loc
   integer                                                 :: nmodel
!
   nmodel = size(model)
   if (present(keyadd)) then
       loc = minloc(model(:)%info(kselfom),mask=model(:)%info(keyadd)>0)
   else
       loc = minloc(model(:)%info(kselfom))
   endif
   if (present(leg)) then
       call model_get(model(loc(1)),at,leg,fomv)
   else
       call model_get(model(loc(1)),at,fom=fomv)
   endif
!   
   end subroutine model_get_best

!--------------------------------------------------------------------------------------

   !subroutine model_sort(model,keycode,keyadd)
   subroutine model_sort(model,keycode,keyadd)
!
!  Ordina i modelli della lista dal più' piccolo al piu' grande
!  l'ordine e' invertito se keycode < 0
!  i modelli con keyadd = 0 vengono accodati alla lista
!
   USE nr, only: indexx
   !type(model_type), dimension(:), allocatable, intent(inout) :: model
   type(model_type), dimension(:), intent(inout) :: model
   integer, intent(in)                           :: keycode
   integer, intent(in), optional                 :: keyadd
!corr   integer, dimension(size(model))               :: iord
!corr   integer                                       :: keyord
   integer                                       :: nmodel
   real                                          :: mfom
   real, dimension(:), allocatable               :: vetr
   integer                                       :: i
   integer, dimension(:), allocatable :: iord
   type(model_type), dimension(:), allocatable :: md
!
!corr   keyord = abs(keycode)
   !nmodel = nummodels(model)  !!!size(model)
   nmodel = size(model)
   if (nmodel > 0) then
       allocate(iord(nmodel))
       if (present(keyadd)) then
           mfom = maxval(model(:)%info(keycode))
           allocate(vetr(nmodel))
           do i=1,nmodel
              if (model(i)%info(keyadd) == 0) then
                  vetr(i) = mfom + 1000*i
              else
                  vetr(i) = model(i)%info(keycode)
              endif
           enddo
           call indexx(vetr,iord)
       else
           call indexx(model(:)%info(abs(keycode)),iord)
       endif
       if (keycode < 0) iord(:) = iord(nmodel:1:-1)
       !model = model(iord)  ! problem with ifort 18.0.0 
       !md = model
       call new_models(md,nmodel)
       do i=1,nmodel
          call model_copy(model(i),md(i))
       enddo
       !model = md(iord)  ! problem with ifort 18.0.0
       do i=1,nmodel
          call model_copy(md(iord(i)),model(i))
       enddo
   endif
!   
   end subroutine model_sort

!--------------------------------------------------------------------------------------------
   
   subroutine model_print_info(model,ninfoi,iveti,mark)
!
!  Stampa info sulla lista
!
   type(model_type), dimension(:), intent(in)  :: model      
   integer, intent(in), optional               :: ninfoi ! numero di campi da stampare
   integer, dimension(:), intent(in), optional :: iveti  ! vettore per specificare campi interi
   integer, dimension(:), allocatable          :: ivet
   integer                                     :: ninfo
   integer                                     :: i,j
   character(len=30)                           :: strnum
   character(len=300)                          :: stringa
   character(len=*), intent(in), optional      :: mark
   integer                                     :: nmodel
!
   nmodel = size(model)
   if (nmodel == 0) return
!
   if (present(ninfoi)) then
       ninfo = ninfoi
   else
       ninfo = size(model(1)%info)
   endif
!
   if (present(iveti)) then
       allocate(ivet(size(iveti)))
       ivet(:) = iveti(:)
   else
       allocate(ivet(ninfo))
       ivet(:) = 0
   endif
!
   do i=1,nmodel
      stringa = ' '
      do j=1,ninfo
         if (any(ivet == j)) then
             write(strnum,'(3x,i10)',err=10)int(model(i)%info(j))
         else
             write(strnum,'(3x,f10.4)',err=10)model(i)%info(j)
         endif
         stringa = trim(stringa)//trim(strnum)
      enddo
      if (present(mark)) then
          write(6,'(a,2x,a)',err=10)trim(mark),trim(stringa)
      else
          write(6,'(2x,a)',err=10)trim(stringa)
      endif
   enddo 
   return
!
10 write(6,*)'PROGRAM ERROR on writing output file'
!
   end subroutine model_print_info 

!--------------------------------------------------------------------------------------------

   subroutine model_update_minimum(model,at,fom,kselfom,kmax,leg)
!
!  Aggiorna il modello migliore rimuovendo il peggiore (kmax)
!
   type(model_type), dimension(:), intent(inout)                    :: model
   type(atom_type), dimension(:), allocatable, intent(in)           :: at
   real, dimension(:), intent(in)                                   :: fom
   integer, intent(in)                                              :: kselfom
   type(bond_type), dimension(:), allocatable, intent(in), optional :: leg
   integer, dimension(1)                                            :: loc
   integer                                                          :: kmax
!
   if (size(model) == 0) return
!
   loc = maxloc(model%info(kselfom))
   kmax = loc(1)
   if (fom(kselfom) < model(kmax)%info(kselfom)) then
       if (present(leg)) then
           call model_set(model(kmax),at,leg,fom)
       else
           call model_set(model(kmax),at,fom=fom)
       endif
   else
       kmax = 0
   endif
!
   end subroutine model_update_minimum

!--------------------------------------------------------------------------------------------

   real function model_average(model,kselfom)  result(ave)
!
!  Calcola il valore medio di una della grandezza fom(kselfom)
!
   type(model_type), dimension(:), intent(in) :: model
   integer, intent(in)                        :: kselfom
!
   ave = sum(model(:)%info(kselfom)) / size(model)
!
   end function model_average

!--------------------------------------------------------------------------------------------

   subroutine model_delete(models,kmodel)
   type(model_type), dimension(:), intent(inout) :: models
   integer                                       :: kmodel
   integer                                       :: nmodels
!
   nmodels = size(models)
   models(kmodel:nmodels-1) = models(kmodel+1:nmodels)
!
   end subroutine model_delete
   
!--------------------------------------------------------------------------------------------

   subroutine save_models_bin(unitbin,models)
!
!  Write models on binary file
!
   USE atom_type_util
   integer, intent(in)                                     :: unitbin
   type(model_type), dimension(:), allocatable, intent(in) :: models
   integer                                                 :: nmod
   integer                                                 :: i
   nmod = nummodels(models)
   write(unitbin)nmod
   do i=1,nmod
      call save_structure_bin(unitbin,models(i)%at,models(i)%leg)
      write(unitbin)models(i)%info
   enddo
   end subroutine save_models_bin

!--------------------------------------------------------------------------------------------

   subroutine read_models_bin(unitbin,models,err)
!
!  Read models from binary file
!
   USE atom_type_util
   USE errormod
   integer, intent(in)                                        :: unitbin
   type(model_type), dimension(:), allocatable, intent(inout) :: models
   type(error_type), intent(out)                              :: err
   integer                                                    :: nmod
   integer                                                    :: ier
   integer                                                    :: i
!
!  read number of models
   read(unitbin,iostat=ier,err=10) nmod
   call new_models(models,nmod)
   do i=1,nmod
      call read_structure_bin(unitbin,models(i)%at,models(i)%leg,err)
      if (err%signal) exit
      read(unitbin,iostat=ier,err=10)models(i)%info
   enddo
!
10 continue
   if (ier /= 0) then
       call err%set('Error on reading structure')
   endif
!
   end subroutine read_models_bin

!--------------------------------------------------------------------------------------------

   integer function find_model(models,nmod,kwhere,kwhat) result(kmodel)
   type(model_type), dimension(:), allocatable, intent(inout) :: models
   integer, intent(in)                                        :: nmod
   integer, intent(in)                                        :: kwhere
   integer, intent(in)                                        :: kwhat
   integer                                                    :: i
!
   kmodel = 0
   do i=1,min(nummodels(models),nmod)
      if (models(i)%info(kwhere) == kwhat) then
          kmodel = i; exit
      endif
   enddo
!
   end function find_model

!--------------------------------------------------------------------------------------------

   subroutine new_models(vetr,n)
!
!  Create new atoms
!
   type(model_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                          :: n

   if (n < 0) return
   if (nummodels(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_models

!--------------------------------------------------------------------------------------------

   subroutine resize_models(vetr,n)
!
!  Rialloca ad n un vettore reale.
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(model_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                          :: n
   integer                                      :: nv
   type(model_type), allocatable                :: vsav(:)
   integer                                      :: nsav
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
!
!      nsav contiene qual è la porzione di vetr da salvare
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
   endif
!
   end subroutine resize_models

!--------------------------------------------------------------------------------------------

   subroutine clear_models(vetr)
!
!  Delete all phases
!
   type(model_type), allocatable, intent(inout) :: vetr(:)

   if (allocated(vetr)) deallocate(vetr)

   end subroutine clear_models

END MODULE model_util
