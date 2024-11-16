MODULE fastfcal
!Questo modulo contiene procedure per velocizzare il calcolo dei fattori di struttura attraverso
!la memorizzazione di opportune variabili in sfactor e limitando il calcolo solo agli atomi per i
!quali si verificano variazioni di coord,fattori termici,occ.
!
!S fcalc_init(ref,atx)                Subroutine di inizializzazione
!S fcalc_close()                      Dealloca e azzera tutto   
!S fcalc_for_tfocc(ref,atx,pafc)      Ricalcola tfocc soltanto per gli atomi atx 
!S fcalc_from_atm(ref,atx,pafc)       Calcola fattori di struttura per atomi atx se cambiano le coordinate 
!S fcalc_from_thermalf((ref,atx,pafc) Calcola fattori di struttura per atomi atx se cambiano fatt. term e occ.
!S fcalc_save_atm(pafc)               Salva i sen e cos per gli atomi indicati nel vettore pafc
!S fcalc_save_thermalf(pafc)          Salva tfocc per gli atomi indicati nel vettore pafc
!S fcalc_ripr_atm(pafc)               Ripristina sen e cos per gli atomi indicati nel vettore pafc
!S fcalc_ripr_thermalf(pafc)          Ripristina tfocc per gli atomi indicati nel vettore pafc
!S fcalc_get_phases(ref)              Calcola le fasi
USE atom_basic, only: atom_type

implicit none

type sfactor_type
     real, dimension(:),   allocatable :: ff             !fattori di scattering(f.s.)
     real, dimension(:,:), allocatable :: hkle           !matrici di simmetria
     real, dimension(:),   allocatable :: ht
     real, dimension(:),   allocatable :: fcosat,fsinat  !contributo per atomo al coseno e seno
     real, dimension(:),   allocatable :: tfocc          !termine dipendente da f.s.,occ.,fattore termico
end type

!
! generazione tabella seni/coseni
integer, private                 :: ipt
real, parameter, private         :: to_rad = 3.141592653589793238462643383279502884197 / 180
real, dimension(0:3599), parameter :: sintable = (/(sin(to_rad*(ipt)/10),ipt=0,3599)/)
real, dimension(0:3599), parameter :: costable = (/(cos(to_rad*(ipt)/10),ipt=0,3599)/)
!
!Sin/Cos table is faster with Intel compiler
#if defined(__INTEL_COMPILER)
#define TRIG_TABLE 1
#endif

CONTAINS

   subroutine fcalc_init(ref,atx,spg,elem,radtype,sfact,sfactc,kpscat)
!
!  Subroutine di inizializzazione. Alloca sfactor(c) e precalcola tutto.
!   
   USE arrayutil
   USE atom_basic
   USE reflection_type_util
   USE spginfom
   USE elements
   USE trig_constants
!
   type(reflection_type), dimension(:), intent(inout) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)          :: atx    ! modello strutturale
   type(spaceg_type), intent(in)                      :: spg
   type(element_type), dimension(:), allocatable      :: elem
   integer, intent(in)                                :: radtype
   type(sfactor_type), dimension(:), allocatable, intent(inout) :: sfact,sfactc !sfactor e una sua copia
   integer, dimension(:), allocatable, intent(inout)  :: kpscat           !punta al fattore di scattering
   integer, dimension(3)                              :: i1
   integer                                            :: i,j   !!!!!,k,l
   real                                               :: rho
   real                                               :: const
   integer :: nref, nat
!
!  Numero di riflessi e di atomi su cui verra eseguito il calcolo di fattori di struttura
   nref = size(ref)
   nat = size(atx)
!
!  Alloca sfactor
   if (.not.allocated(sfact)) then
        allocate(sfact(nref),sfactc(nref))
        do i=1,nref
           allocate(sfact(i)%ff(numelem(elem)))
           allocate(sfact(i)%hkle(3,spg%nsym),sfactc(i)%hkle(3,spg%nsym),sfact(i)%ht(spg%nsym))
           allocate(sfact(i)%fcosat(nat),sfactc(i)%fcosat(nat))
           if (spg%symcent /= 1) allocate(sfact(i)%fsinat(nat),sfactc(i)%fsinat(nat))
           allocate(sfact(i)%tfocc(nat),sfactc(i)%tfocc(nat))
        enddo
   endif
!
!  Calcola matrici e fattori di scattering
   loop_riflessi: do i=1,nref
      i1 = ref(i)%hkl
      do j=1,spg%nsym
         sfact(i)%ht(j) = dot_product(i1,spg%symop(j)%trn(:))  ! H*T
         sfact(i)%hkle(:,j) = matmul(i1,spg%symop(j)%rot(:,:)) ! H*R
      enddo
      rho=ref(i)%slaq
!
      sfact(i)%ff(:) = spg%ncoper*at_scatt(elem,rho,radtype)
   enddo loop_riflessi
!
!  Ingloba la constante nei valori precalcolati
   if (spg%symcent == 1) then
       const = twopi
   else
#ifdef TRIG_TABLE
       const = 360
#else
       const = twopi
#endif
   endif
   do i=1,nref
      sfact(i)%hkle(:,:) = sfact(i)%hkle(:,:)*const
      sfact(i)%ht(:) = sfact(i)%ht(:)*const
   enddo
!
!  Memorizza il puntatore al fattore di scattering
   call resize_array(kpscat,nat)
   kpscat(:) = atx(:)%kscatt()
!
!  Calcola tfocc per tutti gli atomi
   call fcalc_for_tfocc(ref,atx,(/(i,i=1,nat)/),sfact,kpscat)
!
!  Calocla fattori di struttura per tutti gli atomi
   call fcalc_from_atm(ref,atx,(/(i,i=1,nat)/),spg,sfact)
   call fcalc_save_atm((/(i,i=1,nat)/),spg,sfact,sfactc)
!
   end subroutine fcalc_init

!-----------------------------------------------------------------------

   subroutine fcalc_close(sfact,sfactc)
!
!  Dealloca e azzera tutto   
!
   type(sfactor_type), dimension(:), allocatable, intent(inout) :: sfact,sfactc
   if (allocated(sfact)) then
       deallocate(sfact,sfactc)
   endif
!
   end subroutine fcalc_close

!-----------------------------------------------------------------------

   subroutine fcalc_for_tfocc(ref,atx,pafc,sfact,kpscat)
!
!  Ricalcola tfocc soltanto per gli atomi atx 
!   
   USE reflection_type_util, only: reflection_type
   USE spginfom
!
   type(reflection_type), dimension(:), intent(in) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)       :: atx    ! atomi per i quali calcolare tfocc
   integer, dimension(:), intent(in)               :: pafc   ! num. d'ordine degli atomi atx
   type(sfactor_type), dimension(:), allocatable, intent(inout) :: sfact
   integer, dimension(:), allocatable, intent(in)  :: kpscat 
   integer                                         :: i,jj
   real                                            :: rho
   integer                                         :: kz
   integer                                         :: natfc
!
   natfc = size(atx)
   do i=1,size(ref)
      rho=ref(i)%slaq
      do jj=1,natfc
         kz=kpscat(pafc(jj))
         sfact(i)%tfocc(pafc(jj))=sfact(i)%ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
      enddo 
   enddo 
!
   end subroutine fcalc_for_tfocc
   
!-----------------------------------------------------------------------

   subroutine fcalc_from_atm(ref,atx,pafc,spg,sfact)
!
!  Calcola Fc**2 dopo aver ricalcolato i contibuti fcosat e fsinat solo per atx
!  Va usata se cambiano le coordinate di uno o piu atomi
!   
   USE spginfom
   USE atom_basic
   USE reflection_type_util, only: reflection_type
!
   type(reflection_type), dimension(:), intent(inout) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)          :: atx    ! atomi per i quali ricalcolare cos e sin
   integer, dimension(:), intent(in)                  :: pafc   ! num. d'ordine degli atomi atx
   type(spaceg_type), intent(in)                      :: spg
   type(sfactor_type), dimension(:), allocatable, intent(inout) :: sfact
   integer                                            :: i,j,jj
   real                                               :: at,bt
   real                                               :: argg
   integer                                            :: natfc
   integer :: k
   real :: dot_pro
   real :: cs,ss
   integer :: pos
   integer :: iangle
!
   natfc = size(atx)
   if (spg%symcent == 1) then
       do i=1,size(ref)
          do jj=1,natfc
             at=0.0
             do j=1,spg%nsym
                dot_pro = 0
                do k = 1,3
                   dot_pro = dot_pro + sfact(i)%hkle(k,j)*atx(jj)%xc(k)
                enddo                             
                argg = (dot_pro + sfact(i)%ht(j))
                at = at + cos(argg)
             enddo
             sfact(i)%fcosat(pafc(jj)) = at
          enddo 
          dot_pro = dot_product(sfact(i)%fcosat,sfact(i)%tfocc)
          !ref(i)%fc = 4.0*dot_pro*dot_pro  ! eliminato il 4
          ref(i)%fc = dot_pro*dot_pro
       enddo 
   else
       do i=1,size(ref)
          do jj=1,natfc
             at=0.0
             bt=0.0
             do j=1,spg%nsym
                dot_pro = 0.0
                do k = 1,3
                   dot_pro = dot_pro + sfact(i)%hkle(k,j)*atx(jj)%xc(k)
                enddo                
!
                argg = (dot_pro + sfact(i)%ht(j))
#ifdef TRIG_TABLE
                iangle = mod(argg+36000,360.)*10 
                cs = costable(iangle)
                ss = sintable(iangle)
                at = at + cs
                bt = bt + ss
#else
                !call sincos(argg,ss,cs)
                !at = at + cs
                !bt = bt + ss
                at = at + cos(argg)
                bt = bt + sin(argg)
#endif
             enddo
             pos = pafc(jj)
             sfact(i)%fcosat(pos) = at   
             sfact(i)%fsinat(pos) = bt
          enddo
          ref(i)%fc = dot_product(sfact(i)%fcosat,sfact(i)%tfocc)**2 +  &
                      dot_product(sfact(i)%fsinat,sfact(i)%tfocc)**2                              
       enddo 
   endif
!
   end subroutine fcalc_from_atm

!-----------------------------------------------------------------------

   real function cos_32(angle)
   USE trig_constants
!
!  This is the main cosine approximation "driver"
!  It reduces the input argument's range to [0, pi/2],
!  and then calls the approximator. 
!
   real, intent(in) :: angle
   real, parameter  :: two_over_pi= 2.0/3.1415926535897932384626433    
   real             :: x
   integer          :: quad              ! what quadrant are we in?
   real, parameter :: c1= 0.99940307
   real, parameter :: c2=-0.49558072
   real, parameter :: c3= 0.03679168
   real            :: x2
!
   x=mod(angle, twopi)                   ! Get rid of values > 2* pi
   if(x<0)x=-x                           ! cos(-x) = cos(x)
   quad=int(x * two_over_pi)             ! Get quadrant # (0 to 3) we're in
   select case(quad)
      case (0)
   x2=x * x
   cos_32 = (c1 + x2*(c2 + c3 * x2))
         !cos_32 = cos_32s(x)
      case (1)
   x = pi - x
   x2=x * x
   cos_32 = -(c1 + x2*(c2 + c3 * x2))
         !cos_32 = -cos_32s(pi-x)
      case (2)
   x = x - pi
   x2=x * x
   cos_32 = -(c1 + x2*(c2 + c3 * x2))
         !cos_32 = -cos_32s(x-pi)
      case (3)
   x = twopi - x
   x2=x * x
   cos_32 = -(c1 + x2*(c2 + c3 * x2))
         !cos_32 =  cos_32s(twopi-x)
   end select 
   end function cos_32

!-----------------------------------------------------------------------

   subroutine fcalc_from_thermalf(ref,atx,pafc,spg,sfact)
!
!  Calcola i fattori di struttura dopo aver ricalcolato i contibuti fcosat e fsinat solo per atx
!  Va usata se cambiano i fattori termici o le occupanze
!   
   USE reflection_type_util, only: reflection_type
   USE spginfom
!
   type(reflection_type), dimension(:), intent(inout) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)          :: atx    ! modello strutturale
   integer, dimension(:), intent(in)                  :: pafc
   type(spaceg_type), intent(in)                      :: spg
   type(sfactor_type), dimension(:), allocatable, intent(inout) :: sfact
   integer                                            :: i,jj
   real                                               :: rho
   integer                                            :: kz
   integer                                            :: natfc
   real                                               :: dot_pro
!
   natfc = size(atx)
   if (spg%symcent == 1) then
       do i=1,size(ref)
          rho=ref(i)%slaq
          do jj=1,natfc
             kz=atx(jj)%kscatt()
             !sfact(i)%tfocc(pafc(jj))=spg%ncoper*sfact(i)%ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
             sfact(i)%tfocc(pafc(jj))=sfact(i)%ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
          enddo
          !ref(i)%fc = sqrt(4.0*dot_product(sfact(i)%fcosat,sfact(i)%tfocc)**2)
          !ref(i)%fc = 2.0*abs(dot_product(sfact(i)%fcosat,sfact(i)%tfocc))
          dot_pro = dot_product(sfact(i)%fcosat,sfact(i)%tfocc)
          !ref(i)%fc = 4.0*dot_pro*dot_pro
          ref(i)%fc = dot_pro*dot_pro
       enddo
   else
       do i=1,size(ref)
          rho=ref(i)%slaq
          do jj=1,natfc
             kz=atx(jj)%kscatt()
             !sfact(i)%tfocc(pafc(jj))=spg%ncoper*sfact(i)%ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
             sfact(i)%tfocc(pafc(jj))=sfact(i)%ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
          enddo
          ref(i)%fc = dot_product(sfact(i)%fcosat,sfact(i)%tfocc)**2 +     &
                         dot_product(sfact(i)%fsinat,sfact(i)%tfocc)**2
       enddo
   endif
!
   end subroutine fcalc_from_thermalf

!-----------------------------------------------------------------------

   subroutine fcalc_save_atm(pafc,spg,sfact,sfactc)
!
!  Salva i contributi per gli atomi indicati nel vettore pafc
!   
   USE spginfom
!
   integer, dimension(:), intent(in) :: pafc
   type(sfactor_type), dimension(:), intent(in) :: sfact !sfactor e una sua copia
   type(sfactor_type), dimension(:), intent(inout) :: sfactc !sfactor e una sua copia
   type(spaceg_type), intent(in)     :: spg
   integer                           :: jj
   integer                           :: natfc
   integer                           :: i
   integer                           :: pos
!
   natfc = size(pafc)
   if (spg%symcent == 1) then
       do i=1,size(sfact)
        !write(0,*)'SIZE=',i,size(sfactc(i)%fcosat),size(sfactor(i)%fcosat)
          do jj=1,natfc
             pos = pafc(jj)
             sfactc(i)%fcosat(pos) = sfact(i)%fcosat(pos)
          enddo 
       enddo   
   else
       do i=1,size(sfact)
          do jj=1,natfc
             pos = pafc(jj)
             sfactc(i)%fcosat(pos) = sfact(i)%fcosat(pos)
             sfactc(i)%fsinat(pos) = sfact(i)%fsinat(pos)
          enddo 
       enddo
   endif
!
   end subroutine fcalc_save_atm

!-----------------------------------------------------------------------

   subroutine fcalc_save_thermalf(pafc,sfact,sfactc)
!
!  Salva i contributi per gli atomi indicati nel vettore pafc
!   
   integer, dimension(:), intent(in) :: pafc
   type(sfactor_type), dimension(:), intent(in) :: sfact !sfactor e una sua copia
   type(sfactor_type), dimension(:), intent(inout) :: sfactc !sfactor e una sua copia
   integer                           :: jj
   integer                           :: natfc
   integer                           :: i
   integer                           :: pos
!
   natfc = size(pafc)
   loop_riflessi: do i=1,size(sfact)
      loop_atomi: do jj=1,natfc
         pos = pafc(jj)
         sfactc(i)%tfocc(pos)=sfact(i)%tfocc(pos)
      enddo loop_atomi
   enddo loop_riflessi
!
   end subroutine fcalc_save_thermalf

!-----------------------------------------------------------------------

   subroutine fcalc_ripr_atm(pafc,spg,sfact,sfactc)
!
!  Ripristina i contributi per gli atomi indicati nel vettore pafc
!      
   USE spginfom
   integer, dimension(:), intent(in) :: pafc
   type(spaceg_type), intent(in)     :: spg
   type(sfactor_type), dimension(:), intent(inout) :: sfact !sfactor e una sua copia
   type(sfactor_type), dimension(:), intent(in) :: sfactc !sfactor e una sua copia
   integer                           :: jj
   integer                           :: natfc
   integer                           :: i
   integer                           :: pos
!
!--------Warning: prova ad ottimizzare parametrizzando pafc(jj) e spostando la copia
! in una variabile di tipo derivato con solo cos e sen
!
   natfc = size(pafc)
   if (spg%symcent == 1) then
       do i=1,size(sfact)
          do jj=1,natfc
             pos = pafc(jj)
             sfact(i)%fcosat(pos) = sfactc(i)%fcosat(pos)
          enddo
       enddo
   else
       do i=1,size(sfact)
          do jj=1,natfc
             pos = pafc(jj)
             sfact(i)%fcosat(pos) = sfactc(i)%fcosat(pos)
             sfact(i)%fsinat(pos) = sfactc(i)%fsinat(pos)
          enddo
       enddo
   endif
!
   end subroutine fcalc_ripr_atm

!-----------------------------------------------------------------------

   subroutine fcalc_ripr_thermalf(pafc,sfact,sfactc)
!
!  Ripristina i contributi per gli atomi indicati nel vettore pafc
!    
   integer, dimension(:), intent(in) :: pafc
   type(sfactor_type), dimension(:), intent(inout) :: sfact !sfactor e una sua copia
   type(sfactor_type), dimension(:), intent(in) :: sfactc !sfactor e una sua copia
   integer                           :: jj
   integer                           :: natfc
   integer                           :: i
   integer                           :: pos
!
   natfc = size(pafc)
   loop_riflessi: do i=1,size(sfact)
      loop_atomi: do jj=1,natfc
         pos = pafc(jj)
         sfact(i)%tfocc(pos)=sfactc(i)%tfocc(pos)
      enddo loop_atomi
   enddo loop_riflessi
!
   end subroutine fcalc_ripr_thermalf


END MODULE fastfcal
