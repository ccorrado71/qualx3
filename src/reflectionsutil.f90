MODULE reflection_type_util

!S sftref(refl)                                      Calcola Fc usando refl
!S calculate_2theta                                  Calculate 2theta for reflections
!S defovlexr(refl,sogh,indov)                        Calcola overlapping
!S make_ref_groups(refl,gref,sogi,selcodei)          Genera gruppi di riflessi in overlapping
!F id_refg(gref,iref)                                Locate group containing ref iref
!S print_refg(gref)                                  Stampa i gruppi
!S update_int_gruppi(refl,gref,codei)                Aggiorna intensita' dei gruppi
!F number_of_group(nref,gref)  result(num)           Trova a quale gruppo appartine il riflesso nref
!S print_fcalc(refl)                                 Stampa info sui riflessi
!S write_reflections(jfile,fname,refl,code)          Stampa i riflessi
!S statvet(svety,svetx,nbin)                         Esegue statistica sul vettore svety ordinato  
!!!!!!!!!!!!!!!S set_reflection_table(table,vet,ier)               Usata per generare una tabella di informazioni sui riflessi
!F LPcorrection(tthd)                                Correzione Lorentz-polarizzazione
!S sort_reflections(ref,key)                         sort reflections
!S save_refl_bin(unitbin,refl)                       write reflections on binary file
!S read_refl_bin(unitbin,refl,ier)                   read reflections from binary file
!S select_ref(refl,code) --FIXME--                   Seleziona i reflessi in base ad un criterio definito da code  
!S copy_ref(ref1,ref2)                               Copy ref2 in ref1
!F function get_mcorr(ref,kwave)                     Compute m*corr to apply to F squared to obtain intensity
!S get_norm(ref,spg,elem,bt,radtype,eo,ec)           Compute normalized structure factors

  implicit none
  type reflection_type
     integer, dimension(3) :: hkl         ! indici hkl
     integer               :: m = 1       ! molteplicità del riflesso
     real, dimension(2)    :: tthd        ! 2-theta del riflesso in gradi 
     real                  :: slaq        ! ((sen(theta) / lambda))**2 = rho**2 = (1/(2d))**2; d = 0.5/sqrt(slaq)
     real                  :: fo = 0.0    ! fattori di struttura osservati 
     real                  :: fc = 0.0    ! fattori di struttura calcolati dal modello 
     integer               :: ph = 0      ! fasi calcolate dal modello
     real                  :: fv = 0.0    ! fattori di struttura veri
     integer               :: phv = 0     ! fase vera
     real, dimension(2)    :: lp = 1.0    ! correzione di Lorentz-polarizzazione 
     real, dimension(2)    :: ab = 1.0    ! absorption factor
     real, dimension(2)    :: fwhm = 0.01 ! fwhm 
     real                  :: po = 1.0    ! P.O. correction
     real                  :: pk = 20.0   ! peak range
     real                  :: rapI = 1.0  ! intensity ratio: I/Imax
     integer               :: jcode = 13  ! code reflection
     integer               :: rcod = 0    ! refinement code for reflection

  contains
     procedure :: dval => get_dvalue

  end type reflection_type

  type gref_type
    integer, dimension(:), allocatable :: vref  ! riflessi del gruppo
    integer                            :: code  ! num. di riflessi
    real                               :: Io    ! int. osservata
    real                               :: Ic    ! int. calcolata
  end type

integer, parameter :: SINGLE_CRYSTAL_EXP=2, POWDER_EXP=3


integer, parameter :: ORD_BY_2THETA=1, ORD_BY_INT=2, ORD_BY_D=3

interface create_reflections
   module procedure create_reflections_mat, create_reflections_spg, create_reflections_spg_d
end interface

private :: sfcalc_anomal, sfcalc_no_anomal, sfcalc_anis_anomal, sfcalc_anis_no_anomal
private :: fcalcadern_anomal, fcalcadern_noanomal

 CONTAINS

   elemental real function get_dvalue(ref)
   class(reflection_type), intent(in) :: ref
!
   get_dvalue =  0.5 / sqrt(ref%slaq)
!
   end function get_dvalue

!---------------------------------------------------------------------------------------

   subroutine calculate_2theta(ref,celp,wave)
   USE unit_cell
!                                                 T *
!  2-theta = 2.0 * arcosen ( 0.5 * wave * sqrt ( H G H ) ) in radians
!
   type(reflection_type),dimension(:), intent(inout) :: ref
   real, dimension(6), intent(in)                    :: celp   ! cell
   real, dimension(:), intent(in)                    :: wave   ! wavelength
   integer                                           :: i,j
   real, dimension(3,3)                              :: ggr
!
   ggr = matrice_mreciproca(celp)   ! G*
!
   do i=1,size(ref) ! loop sui riflessi
      ref(i)%slaq = 0.25*DOT_PRODUCT(ref(i)%hkl,MATMUL(ggr,ref(i)%hkl))       ! (sentheta / wave)**2 = 0.5/sqrt(d)
      do j=1,size(wave)
         ref(i)%tthd(j) = 2.0*asin(min(wave(j)*sqrt(ref(i)%slaq),1.0))*rtod   ! 2theta in degrees
      enddo
   enddo
!
   end subroutine calculate_2theta

!---------------------------------------------------------------------------------------

   pure integer function numrefl(refl) result(nref)
   type(reflection_type), dimension(:), allocatable, intent(in) :: refl
   if (allocated(refl)) then
       nref = size(refl)
   else
       nref = 0
   endif
   end function numrefl

!---------------------------------------------------------------------------------------

   subroutine defovlexr(refl,sogh,indov)
!
!  Calcola overlapping
!
   type(reflection_type), dimension(:), intent(in) :: refl
   real, intent(in)                                :: sogh
   integer, intent(out), dimension(:)              :: indov
   integer                                         :: js,jg
   integer                                         :: ifl
   integer                                         :: nr
!
   nr = size(indov)
   indov = 1
!
   DO JS=1,nr-1
      If(indov(JS).EQ.0) cycle
      indov(JS) = 1
      DO JG=JS+1,nr
         IFL = 0
         IF(ABS(refl(jg)%tthd(1)-refl(jg-1)%tthd(1)) <= sogh*refl(jg)%fwhm(1)) THEN
            indov(JS) = indov(JS)+1
            indov(JG) = 0
            IFL = 1
         END IF
         IF(IFL == 0) exit
      enddo
   enddo
!
   end subroutine defovlexr

!---------------------------------------------------------------------------------------

   subroutine make_ref_groups(refl,gref,sogi,selcodei)
!
!  Genera gruppi di riflessi in overlapping
!
   type(reflection_type),        dimension(:), intent(in)    :: refl     ! riflessi
   type(gref_type), allocatable, dimension(:), intent(inout) :: gref     ! gruppi
   real, intent(in), optional                                :: sogi     ! soglia di overlap
   integer, intent(in), optional                             :: selcodei ! seleziona i gruppi con code >= selcode
   integer, dimension(size(refl))                            :: over
   real                                                      :: sog
   integer                                                   :: selcode
   integer                                                   :: i
   integer                                                   :: nrrf
   integer                                                   :: ngref
   integer                                                   :: n
!
   nrrf = size(refl)
!
!  Calcola sovrapposizione tra i riflessi
   if (present(sogi)) then
       sog = sogi
   else
       sog = 0.6
   endif
   call defovlexr(refl,sog,over)
!
!  Seleziona i gruppi con code >= selcode
   if (present(selcodei)) then
       selcode = selcodei
   else
       selcode = 1
   endif
!
!  alloca gref
   ngref = 0
   do i=1,nrrf
      if (over(i) >= selcode) then
          ngref = ngref + 1
      endif
   enddo
   if (allocated(gref)) deallocate(gref)
   allocate(gref(ngref))
!
!  riempi gref
   ngref = 0
   n = 0
   do 
     n = n + 1
     if (n > nrrf) exit
     if (over(n) >= selcode) then
         ngref = ngref + 1
         gref(ngref)%code = over(n)
         allocate(gref(ngref)%vref(over(n)))
         gref(ngref)%vref(1) = n
         do i=2,gref(ngref)%code
            n = n + 1
            gref(ngref)%vref(i) = n
         enddo
     endif
   enddo
   call update_int_refg(refl,gref,3)
!
   end subroutine make_ref_groups

!---------------------------------------------------------------------------------------

   integer function id_refg(gref,iref)
!
!  Locate group containing reflections iref
!
   USE arrayutil
   type(gref_type), dimension(:), intent(inout) :: gref  ! gruppi
   integer, intent(in)                          :: iref
   real, dimension(size(gref))                  :: rgref
   integer                                      :: i
!
   do i=1,size(gref)
      rgref(i) = real(gref(i)%vref(1))
   enddo
!not permitted by gfortran   id_refg = clocate(real(gref%vref(1)),real(iref))
   id_refg = clocate(rgref,real(iref))
   if (gref(id_refg)%vref(1) > iref) id_refg = id_refg - 1
!
   end function id_refg

!---------------------------------------------------------------------------------------

   subroutine print_refg(gref,kpri)
   type(gref_type), dimension(:), intent(inout) :: gref  ! gruppi
   integer, intent(in), optional                :: kpri
   integer                                      :: i
   character(len=10)                            :: strnum
   character(len=100)                           :: strf
   integer                                      :: kpr
!
   if (present(kpri)) then
       kpr = kpri
   else
       kpr = 6
   endif
   write(kpr,'(a)')'   n.        Io         Ic         code           ref'
   do i=1,size(gref)
      write(strnum,'(i0,"i5")')gref(i)%code
      strf = '(i5,2f15.2,i4,'//trim(strnum)//')'
      write(kpr,strf)i,gref(i)%Io,gref(i)%Ic,gref(i)%code,gref(i)%vref(:)
   enddo
!
   end subroutine print_refg

!---------------------------------------------------------------------------------------

   subroutine update_int_refg(refl,gref,codei)
   type(reflection_type), dimension(:), intent(in) :: refl  ! riflessi
   type(gref_type), dimension(:), intent(inout)    :: gref
   integer, intent(in), optional                   :: codei
   integer                                         :: code
   integer                                         :: i
!
   if (present(codei)) then
       code = codei
   else
       code = 3
   endif
   select case(code)
      case (1)                
        do i=1,size(gref)
           gref(i)%Io = sum(refl(gref(i)%vref)%m*refl(gref(i)%vref)%lp(1)*refl(gref(i)%vref)%fo**2)
        enddo

      case (2)
        do i=1,size(gref)
           gref(i)%Ic = sum(refl(gref(i)%vref)%m*refl(gref(i)%vref)%lp(1)*refl(gref(i)%vref)%fc**2)
        enddo

      case (3)
        do i=1,size(gref)
           gref(i)%Io = sum(refl(gref(i)%vref)%m*refl(gref(i)%vref)%lp(1)*refl(gref(i)%vref)%fo**2)
           gref(i)%Ic = sum(refl(gref(i)%vref)%m*refl(gref(i)%vref)%lp(1)*refl(gref(i)%vref)%fc**2)
        enddo
   end select
!
   end subroutine update_int_refg

!---------------------------------------------------------------------------------------

   integer function number_of_group(nref,gref)  result(num)
!
!  Trova a quale gruppo appartine il riflesso nref
!
   integer, intent(in)                       :: nref
   type(gref_type), dimension(:), intent(in) :: gref
   integer                                   :: i,j
!
   do i=1,size(gref)
      do j=1,gref(i)%code
         if (gref(i)%vref(j) == nref) then 
             num = i
             exit
         endif
      enddo
   enddo
!
   end function number_of_group

!---------------------------------------------------------------------------------------
!corr
!corr   subroutine print_fcalc(refl)
!corr   type(reflection_type), dimension(:), intent(in) :: refl
!corr   integer                                         :: i
!corr   real                                            :: scalf
!corr   real, dimension(size(refl))                     :: vetf
!corr!   
!corr   scalf = sum(refl%fv)/sum(refl%fo)
!corr   write(6,*)'  n       hkl       2-theta      fo     scal*fo     fv abs(fv-fo) phv'
!corr   do i=1,size(refl)
!corr      write(6,'(i5,3i5,5f10.3,i5)')i,refl(i)%hkl,refl(i)%tthd,refl(i)%fo,         &
!corr          scalf*refl(i)%fo,refl(i)%fv,abs(refl(i)%fv-scalf*refl(i)%fo),refl(i)%phv
!corr   enddo
!corr   vetf = abs(refl%fv - scalf*refl%fo) / sum(refl%fv)
!corr   write(6,*)'RF=',sum(abs(refl%fv-scalf*refl%fo))/sum(refl%fv),sum(vetf)
!corr   call statvet(vetf,refl%tthd,5)
!corr!   
!corr   end subroutine print_fcalc
!corr
!----------------------------------------------------------------------------------------------------

   subroutine write_reflections(jfile,fname,refl,code,nref,nwave,wave,intensity,title,sortby)
!
!  Stampa i riflessi
!        code = 1   ! h  k  l
!        code = 2   ! h  k  l   Fo^2  sigma
!        code = 3   ! h  k  l   Fc^2  sigma
!        code = 4   ! h  k  l   Fo    Fc   phase   (file fc)
!        code = 5   ! h  k  l   d     mult  lp   Fc^2    I      I(%)
!        code = 6   ! h  k  l   Fc     phase 
!        code = 7   ! h  k  l   2theta   Fc      Fo       Fv   |Fo-Fv|   ph      phv   overlap   group
   USE fileutil
   USE counts
   integer, intent(in), optional                    :: jfile
   character(len=*), intent(in), optional           :: fname
   type(file_handle)                                :: fileh
   type(reflection_type), dimension(:), intent(in)  :: refl
   integer, intent(in)                              :: code
   integer, optional, intent(in)                    :: nref
   integer, optional, intent(in)                    :: nwave
   real, optional, intent(in)                       :: wave
   real, dimension(:), intent(in), optional         :: intensity
   character(len=*), intent(in), optional           :: title
   integer, optional, intent(in)                    :: sortby
   real                                             :: std
   integer                                          :: nrefw
   integer                                          :: i
   real, dimension(:), allocatable                  :: inte
   type(reflection_type), dimension(:), allocatable :: reflc
   real                                             :: maxinte,maxfo
   integer                                          :: j_out
   type(gref_type), dimension(:), allocatable       :: gref  
   real, dimension(size(refl))                      :: vetf
   integer                                          :: idg,nw
   real                                             :: scalf,fo
   integer                                          :: sortbyy
         real :: scal
!
   if (present(fname)) then
       call fileh%fopen(fname,'w')
       j_out = fileh%handle()
   else
       j_out = jfile
   endif
!
   if (present(sortby)) then
       sortbyy = sortby
   else
       sortbyy = 0
   endif
!
   if (present(title)) then
       write(j_out,'(a)') title
   endif
   nrefw = size(refl)
   if (present(nref)) nrefw = min(nref,nrefw)
   std = 0.01
   select case (code)
      case (1)      ! h k l  
        if (present(nwave)) then
            nw = nwave
        else
            nw = 1
        endif
        do i=1,nrefw
           write(j_out,'(i5,a1,3i4,*(f8.3))')i,')',refl(i)%hkl,refl(i)%tthd(1:nw)
        enddo
      case (2)      ! h k l       Fo^2     std        (file di tipo shelX)
        !write(j_out,'(3i4,2x,2f8.2)')(refl(i)%hkl,refl(i)%fo**2,std,i=1,nrefw)
        maxfo = maxval(refl%fo)
        if (abs(maxfo) < epsilon(1.0)) then ! check for maxinte = 0
            scal = 0.0
        else
            scal = 1000/(maxfo)**2
        endif
        call write_hkl_file(j_out,refl,scal*refl%fo**2)
      case (3)      ! h k l       Fc^2     sigma      (file di tipo shelX)
        call write_hkl_file(j_out,refl,refl%fc**2)
      case (4)      ! h k l       Fo       Fc     phase    (file fc)
        write(j_out,*)'  h   k  l      Fo      Fc     phase'
        write(j_out,'(3i4,3f8.2)')(refl(i)%hkl,refl(i)%fo,refl(i)%fc,real(refl(i)%ph),i=1,nrefw)
      case (5)
        allocate(inte(nrefw),source=intensity(:nrefw))       
        allocate(reflc(nrefw),source=refl(:nrefw))       
        maxinte = maxval(inte)
        if (abs(maxinte) < epsilon(1.0)) then ! check for maxinte = 0
            maxinte = 1.0 
        else
            if (sortbyy > 0) call sort_reflections(reflc,sortbyy,inte)   
        endif
        if (sortbyy == ORD_BY_INT) nrefw = count(inte >= epsilon(1.0)*maxinte/10)  ! print only reflections with I% > 0
        write(j_out,'(a)')'  h   k  l      2theta    d      mult           LP'//    &
        '               Fc^2             Intensity                   Intensity(%)'
        write(j_out,'(3i4,2f10.4,i4,4f20.4)')                       & 
         (reflc(i)%hkl,reflc(i)%tthd(1),dvalue(reflc(i)%tthd(1),wave),reflc(i)%m,reflc(i)%lp(1),reflc(i)%fc**2,   &
         inte(i),1000*(inte(i)/maxinte),i=1,nrefw)
      case (6)      ! h k l           Fc     phase 
        write(j_out,*)'  h    k   l         Fc      phase'
        write(j_out,'(3(i4,1x),f10.2,1x,f8.2)')(refl(i)%hkl,refl(i)%fc,real(refl(i)%ph),i=1,nrefw)
      case (7)      ! more details about reflections
        write(j_out,'(/a)')'                               List of Reflections'
        write(j_out,'(a)') '   n.  h   k  l    2theta   Fc      Fo       Fv   |Fo-Fv|   ph      phv   overlap   group'
        write(j_out,'(a)') ' -----------------------------------------------------------------------------------------'
        call make_ref_groups(refl,gref)
        !call print_gruppi(gref,6)
        scalf = sum(refl%fv)/sum(refl%fo)
        do i=1,nrefw
           idg = id_refg(gref,i)
           fo = scalf*refl(i)%fo
           write(j_out,'(4i4,7f8.2,i8,i8)')i,refl(i)%hkl,refl(i)%tthd(1),refl(i)%fc,fo,refl(i)%fv,abs(fo-refl(i)%fv),  &
                                           real(refl(i)%ph),real(refl(i)%phv),gref(idg)%code,gref(idg)%vref(1)
        enddo
        vetf = abs(refl%fv - scalf*refl%fo) / sum(refl%fv)
        write(j_out,'(/a,f10.5)')' RF=',sum(abs(refl%fv-scalf*refl%fo))/sum(refl%fv)
        call statvet(vetf,refl%tthd(1),5,j_out)
   end select
!
   if (present(fname)) call fileh%fclose()
!
   end subroutine write_reflections

!----------------------------------------------------------------------------------------------------

   subroutine write_hkl_file(jout,ref,vet)
   USE strutil
   integer, intent(in)                             :: jout
   type(reflection_type), dimension(:), intent(in) :: ref
   real, dimension(:), intent(in)                  :: vet  ! array after h,k,l
   real, parameter                                 :: STD = 0.01
   character(len=20)                               :: frm
   integer                                         :: ndig,i
!
   ndig = ndigits(nint(maxval(vet)))
   if (ndig <= 5) then
       write(jout,'(3i4,2f8.2)')(ref(i)%hkl,vet(i),STD,i=1,size(ref))   ! shelx format
   else
       frm = '(3i4,1x,f'//trim(i_to_s(ndig+3))//'.2,f8.2)'
       write(jout,frm)(ref(i)%hkl,vet(i),STD,i=1,size(ref))             ! free format
   endif
!
   end subroutine write_hkl_file

!----------------------------------------------------------------------------------------------------

   subroutine statvet(svety,svetx,nbin,kpr)
!   
!  Esegue statistica sul vettore svety ordinato   
!
   real, dimension(:), intent(in) :: svety ! vettore ordinato y
   real, dimension(:), intent(in) :: svetx ! vettore ordinato x della stessa dim. di y
   integer, intent(in)            :: nbin  ! numero di intervalli
   integer, intent(in)            :: kpr
   integer, dimension(nbin)       :: ivet
   integer                        :: iresto
   integer                        :: nsv
   integer                        :: i,nini,nfin
!   
!  calcola numero di valori per intervallo in ivet
   nsv = size(svety)
   ivet(:) = nsv / nbin
!   
!  gestisci il resto
   iresto = mod(nsv,nbin)
   ivet(:iresto) = ivet(:iresto) + 1
!   
!  stampa la statistica
   nini = 1
   nfin = 0
   write(kpr,'(/a)')'        n.       interval            value             R'
   do i=1,nbin
      nfin = nini + ivet(i) - 1
      write(kpr,'(i10,")",2x,i10,"-",i0,t30,f10.3,f10.3,f10.3)')i,nini,nfin,svetx(nini),svetx(nfin),sum(svety(nini:nfin))
      nini = nfin + 1
   enddo
!
   end subroutine statvet

!-----------------------------------------------------------------------

   function lp_correction(tthd,radtype,sync)  result(lp)
!  Correzione Lorentz-polarizzazione
!      Formula usata :
!                     Lp = (1+kCos(2theta)**2) / (2*Sen(theta)**2*Cos(theta))
!                        = 0.5*(1+kCos(2theta)**2) * Csc(theta)**2*Sec(theta)
   USE trig_constants, only:dtor
   USE elements
   real, dimension(:), intent(in) :: tthd
   integer, intent(in)            :: radtype
   logical, intent(in)            :: sync
   real, dimension(size(tthd))    :: lp
   integer                        :: numb
   integer                        :: i
   real                           :: tth,th
   real                           :: Csc_th, Sec_th, Sin_th
   real, parameter                :: KPOL = 0.8
!
   numb = size(tthd)
   if (radtype == NEUTRON_SOURCE .or. sync) then
       do i=1,numb
          th = 0.5*tthd(i)*dtor
          lp(i) = 1/(2*sin(th)**2*cos(th))
       enddo
   else
       do i=1,numb
          tth = tthd(i)*dtor
          th = 0.5 * tth
          Sin_th = Sin(th); Csc_th = 1.0/Sin_th ; Sec_th = 1.0/cos(th)
          lp(i) = 0.5*(1 + KPOL*Cos(tth)**2)*Csc_th**2*Sec_th
       enddo
   endif
!
   end function lp_correction

!-----------------------------------------------------------------------

   subroutine sort_reflections(refl,key,inte,ord)
   USE nr
   type(reflection_type), dimension(:), intent(inout) :: refl
   integer, intent(in)                                :: key
   integer, dimension(size(refl)), optional           :: ord
   integer, dimension(size(refl))                     :: iord
   real, dimension(:), intent(inout), optional        :: inte
!
   select case(key)
          case (ORD_BY_2THETA)              ! ordina by 2theta
            call indexx(refl%tthd(1),iord)
            refl(:) = refl(iord)
            if (present(inte)) then
                inte(:) = inte(iord)
            endif

          case (ORD_BY_INT)                 ! ordina by intensity
            call indexx(inte,iord)
            iord(:) = iord(size(inte):1:-1)
            refl(:) = refl(iord)
            inte(:) = inte(iord)
  
          case (ORD_BY_D)                   ! order by d = 0.5/sqrt(slaq)
            call indexx(refl%slaq,iord)
            refl(:) = refl(iord)

          case default
            return
   end select
   if (present(ord)) then
       ord = iord
   endif
!
   end subroutine sort_reflections

!-----------------------------------------------------------------------

   subroutine save_refl_bin(unitbin,refl)
!
!  write reflections on binary file
!
   integer, intent(in)                                          :: unitbin
   type(reflection_type), dimension(:), allocatable, intent(in) :: refl
   integer                                                      :: nref
   nref = numrefl(refl)
   write(unitbin)nref
   if (nref > 0) then
       write(unitbin)refl(:)
   endif
   end subroutine save_refl_bin

!-----------------------------------------------------------------------

   subroutine read_refl_bin(unitbin,refl,err)
!
!  read reflections from binary file
!
   USE errormod
   integer, intent(in)                                             :: unitbin
   type(reflection_type), dimension(:), allocatable, intent(inout) :: refl
   type(error_type), intent(out)                                   :: err
   integer                                                         :: ier
   integer                                                         :: nref
   read(unitbin, iostat=ier, err=10) nref
   if (nref > 0) then
       !call reallocate(refl,nref,.false.)
       call new_reflections(refl,nref)
       read(unitbin, iostat=ier, err=10)refl(:)
   endif
10 continue
   if (ier /= 0) then
       call err%set('Error on reading reflections')
   endif
   end subroutine

!-----------------------------------------------------------------------

   subroutine select_ref(refl,code)
!
!  Seleziona i reflessi in base ad un criterio definito da code  
!  
   type(reflection_type), dimension(:), allocatable, intent(inout) :: refl
   integer, intent(in)                                             :: code
   integer, dimension(:), allocatable                              :: overl
   integer                                                         :: nrrf,nrrfnew
   integer                                                         :: i
   integer                                                         :: ovr
   
   select case (code)
      case (3)
!  
!       Seleziona riflessi singoli e doppietti
        nrrf = size(refl)
   
        allocate(overl(nrrf))
        call defovlexr(refl,0.6,overl)
        i = 0
        nrrfnew = 0
        do
           i = i + 1
           if (i > nrrf) exit
           ovr = overl(i)
           if (ovr == 1 .or. ovr == 2) then
               refl(nrrfnew+1:nrrfnew+ovr) = refl(i:i+1)
               nrrfnew = nrrfnew + ovr
               i = i + ovr - 1
           endif
        enddo
   
   end select
   call resize_reflections(refl,nrrfnew)
!   
   end subroutine select_ref

!---------------------------------------------------------------------------------------------------------------

   subroutine copy_ref(ref1,ref2)
!
!  copy ref2 in ref1
!
   type(reflection_type), dimension(:), allocatable, intent(inout) :: ref1
   type(reflection_type), dimension(:), allocatable, intent(in)    :: ref2
   integer                                                         :: nref1,nref2
!
   nref1 = numrefl(ref1)
   nref2 = numrefl(ref2)
   if (nref1 == nref2) then
       if (nref1 /= 0) ref1 = ref2
   else
       if (allocated(ref1)) deallocate(ref1)
       if (nref2 > 0) then
           allocate(ref1(nref2),source=ref2)
       endif
   endif
!
   end subroutine copy_ref

!---------------------------------------------------------------------------------------------------------------

   subroutine create_reflections_spg(thmin,thmax,cell,spg,wave,ref,nrefl,ihmx)
   USE spginfom
   USE counts
   real, intent(in)                                                :: thmin,thmax
   real, dimension(6), intent(in)                                  :: cell
   type(spaceg_type), intent(in)                                   :: spg
   real, dimension(:), intent(in)                                  :: wave
   type(reflection_type), allocatable, dimension(:), intent(inout) :: ref 
   integer, intent(out)                                            :: nrefl
   integer, dimension(3), intent(out)                              :: ihmx
   real                                                            :: rhomax,rhomin
   integer, dimension(spg%nsymop,3,3)                              :: kmat
   real, dimension(spg%nsymop,3)                                   :: tmat
   integer                                                         :: icent,nsym,jsys,latt
!
   rhomin = 1. / (2*dvalue(thmin,wave(1)))**2
   rhomax = 1. / (2*dvalue(thmax,wave(1)))**2
   call get_symm_from_spg(spg,cell,kmat,tmat,jsys,latt,nsym,icent)
   call create_reflections_mat(rhomin,rhomax,cell,spg,kmat,tmat,nsym,jsys,icent,latt,wave,ref,nrefl,ihmx)
!
   end subroutine create_reflections_spg

!---------------------------------------------------------------------------------------------------------------

   subroutine create_reflections_spg_d(dval,cell,spg,wave,ref,nrefl,ihmx)
   USE spginfom
   USE counts
   real, intent(in)                                                :: dval
   real, dimension(6), intent(in)                                  :: cell
   type(spaceg_type), intent(in)                                   :: spg
   real, dimension(:), intent(in)                                  :: wave
   type(reflection_type), allocatable, dimension(:), intent(inout) :: ref 
   integer, intent(out)                                            :: nrefl
   integer, dimension(3), intent(out)                              :: ihmx
   real                                                            :: rhomax,rhomin
   integer, dimension(spg%nsymop,3,3)                              :: kmat
   real, dimension(spg%nsymop,3)                                   :: tmat
   integer                                                         :: icent,nsym,jsys,latt
!
   rhomin = 0.
   rhomax = 1. / (2*dval)**2
   call get_symm_from_spg(spg,cell,kmat,tmat,jsys,latt,nsym,icent)
   call create_reflections_mat(rhomin,rhomax,cell,spg,kmat,tmat,nsym,jsys,icent,latt,-wave,ref,nrefl,ihmx)
!
   end subroutine create_reflections_spg_d

!---------------------------------------------------------------------------------------------------------------

   subroutine get_symm_from_spg(spg,cell,kmat,tmat,jsys,latt,nsym,icent)
!
!  Extract from spg variables required by create_reflections
!
   USE spginfom
   type(spaceg_type), intent(in)          :: spg
   real, dimension(6), intent(in)         :: cell
   integer, dimension(:,:,:), intent(out) :: kmat
   real, dimension(:,:), intent(out)      :: tmat
   integer, intent(out)                   :: jsys,latt,nsym,icent
   integer                                :: i
!
   if (spg%symcent == 1) then
       icent = 1
   else
       icent = 0
   endif
   nsym = spg%nsym
!
!  Lattice code
   select case(spg%lattyp)
      case ('P'); latt = 1
      case ('A'); latt = 2
      case ('B'); latt = 3
      case ('C'); latt = 4
      case ('I'); latt = 5
      case ('F'); latt = 6
      case ('R'); latt = 7
   end select
!
!  Crystal system. jsys is the lattice system 
   jsys = lattice_system(spg,cell)
!   
!  Operators
   do i=1,spg%nsymop
      kmat(i,:,:) = spg%symop(i)%rot
      tmat(i,:) = spg%symop(i)%trn
   enddo
!
   end subroutine get_symm_from_spg

!---------------------------------------------------------------------------------------------------------------

   subroutine create_reflections_mat(rhomin,rhomax,cell,spg,kmat,tmat,nsym,jsys,icent,latt,wave,refob,nrefl,ihmx)
   USE unit_cell
   USE spginfom
   implicit none
   real, intent(in)                      :: rhomin
   real, intent(inout)                   :: rhomax
   real, dimension(6), intent(in)        :: cell  ! cell parameters
   type(spaceg_type), intent(in)         :: spg
   integer, dimension(:,:,:), intent(in) :: kmat  ! symmetry operators (rotation part)
   real, dimension(:,:), intent(in)      :: tmat  ! symmetry operators (translation part)
   integer, intent(in)                   :: nsym  ! number of symmetry operators (inversion and centering excluded)
   integer, intent(in)                   :: jsys  ! crystal system
   integer, intent(in)                   :: icent ! =1 if centric with -1 at origin
   integer, intent(in)                   :: latt 
   real, dimension(:), intent(in)        :: wave
   type(reflection_type), allocatable, dimension(:), intent(inout) :: refob 
   integer, intent(out)                  :: nrefl
   integer, dimension(3), intent(out)    :: ihmx
   real, dimension(6)                    :: pp
   real, dimension(3,3)                  :: matr
   integer :: klatt
   integer :: ksym
   integer :: iahmax, iakmax, ialmax
   integer :: ihmaxsav, ikmaxsav, ilmaxsav
   integer :: hmin,hmax,kmin,kmax,lmin,lmax
   integer, dimension(96) :: vsym
   integer, dimension(3) :: kv,i1
   integer, parameter :: NINITREF = 3000
   integer, parameter :: NMAXREF = 50000
   integer :: sizerefob
   integer :: i,j
   integer :: nr, jex, idelt, jcode, isgnn, id1
   integer :: nhmax, nkmax, nlmax
   integer :: indh, indk, indl, indh1, indk1, indl1
   real :: rho, rho2, eps, rhomax1
   integer :: multi, inbl
   integer :: ifl
   integer, dimension(6,13), parameter :: ibrav = reshape((/    &
                                                                1, 1, 1, 1, 3, 1,      &
                                                                0, 1, 1, 1, 0,-1,      &
                                                                1, 0, 1, 1, 1, 1,      &
                                                                1, 1, 0, 1, 1, 1,      &
                                                                2, 2, 2, 2, 2, 3,      &
                                                                0, 0, 0, 0, 1, 0,      &
                                                                0, 0, 0, 0, 0, 0,      &
                                                                0, 0, 0, 0, 1, 0,      &
                                                                0, 0, 0, 0, 2, 0,      &
                                                                0, 0, 0, 0, 1, 0,      &
                                                                0, 0, 0, 0, 1, 0,      &
                                                                0, 0, 0, 0, 0, 0,      &
                                                                0, 0, 0, 0, 2, 0       &
                                                               /),(/6,13/))

   iahmax = 0
   iakmax = 0
   ialmax = 0
   call resize_reflections(refob,NINITREF)
   sizerefob = NINITREF
!
   matr = matrice_mreciproca(cell)
   pp(:) = (/0.25*matr(1,1),0.25*matr(2,2),0.25*matr(3,3),0.5*matr(1,2),0.5*matr(1,3),0.5*matr(2,3)/)
!
!  ksym=number of translational matrices different from ( 0.0 0.0 0.0 )
   ksym=0
   do i=2,nsym
      ifl=0
      do j=1,3
         if (abs(tmat(i,j)).gt.0.00001) ifl=1
      enddo
      if (ifl.eq.0) cycle
      ksym=ksym+1
      vsym(ksym)=i
   enddo
   klatt = latt - 1
!
   nr=0
   ihmx(:) = 100
   kv(2)=2*ihmx(3)+1
   kv(3)=ihmx(2)*kv(2)+ihmx(3)
   kv(1)=kv(2)*(2*ihmx(2)+1)
!--
   !call indmm(hmin,hmax,kmin,kmax,lmin,lmax,jsys,ihmx,spgstr)
   call indmm1(hmin,hmax,kmin,kmax,lmin,lmax,jsys,ihmx,spg)
!--
   nhmax=hmin
   nkmax=kmin
   nlmax=lmin
   rhomax1=rhomax
   ihmx(:)=-100
!--
   loop_indh: do indh=hmin,hmax
      do indk=kmin,kmax
         do indl=lmin,lmax
!---
!---        vedere se il nuovo riflesso generato e' equivalente
!---        ad uno gia' considerato, in questo caso :
!---
         if (indh.eq.0.and.indk.eq.0.and.indl.eq.0) cycle
!
!       TRIGONALE : va' considerata la condizione aggiuntiva |h|>|k|
!             nel caso (+-+)
         if (jsys.eq.5.and.indk.lt.0.and.iabs(indh).lt.iabs(indk)) cycle
!
!       CUBICO :  va' considerata la condizione aggiuntiva h<=k<=l
         if(jsys.eq.6.and.(indk.lt.indh.and.indk.gt.indl)) cycle
!
         indh1=indh
         indk1=indk
         indl1=indl
         rho=rhof(pp,indh1,indk1,indl1)
         rho2=rho*rho
           !if (rhomax /= rhomax1) write(70,*)'RHOMAX=',rhomax
         if(rho2.gt.rhomax.or.rho2.lt.rhomin) cycle
         call extin(jex,indh1,indk1,indl1,ibrav,klatt,ksym,vsym,nsym,kmat,tmat,icent)
         if (jex.eq.1) cycle
!
         ihmaxsav = iahmax
         ikmaxsav = iakmax
         ilmaxsav = ialmax
         call geneq(1,indh1,indk1,indl1,idelt,jcode,isgnn,id1,rho,kmat,tmat,icent,nsym,rhomax,ihmx,pp,iahmax,iakmax,ialmax)
         inbl=indh1*kv(1) + indk1*kv(2) + indl1 + kv(3)
         inbl=iabs(inbl)
         !rhomax=rhomax1
         if(nr.gt.0)then
            if (any(refob(:nr)%ph == inbl)) cycle
         endif
         rhomax=rhomax1
         if (rho.gt.rhomax1.or.rho.lt.rhomin) then
             iahmax = ihmaxsav
             iakmax = ikmaxsav
             ialmax = ilmaxsav
             cycle
         endif
!
         if(iahmax.ge.nhmax) nhmax=iahmax
         if(iakmax.ge.nkmax) nkmax=iakmax
         if(ialmax.ge.nlmax) nlmax=ialmax
!
         nr=nr+1
         refob(nr)%ph=inbl  ! temporary save superindice in %ph
         if (nr == sizerefob) then
             call resize_reflections(refob,sizerefob+NINITREF)
             sizerefob = sizerefob + NINITREF
         endif
!
         i1(:) = [indh1,indk1,indl1]
!
!        Recompute rho2: necessary if cell is incompatible with s.g. (es. P 1 2 1 and angle alpha 90 90)
!corr         rho2=pp(1)*i1(1)*i1(1) + pp(2)*i1(2)*i1(2) + pp(3)*i1(3)*i1(3) + pp(4)*i1(1)*i1(2) + pp(5)*i1(1)*i1(3) + pp(6)*i1(2)*i1(3)
!
!corr         twot = ctheta_lam(rho2,wave)
!corr         call cmult2016(i1,nsym,icent,kmat,eps,multi,POWDER_EXP)  
!canna
         call calcola_mult_new(i1,nsym,icent,eps,multi,kmat,POWDER_EXP) 
!canna
         refob(nr)%hkl(:) = i1(:) 
!corr-old         refob(nr)%phv = jcode
         refob(nr)%jcode = jcode
         refob(nr)%m = multi
         if (nr == NMAXREF) exit loop_indh
         enddo
      enddo
   enddo loop_indh
   nrefl=nr
!
   ihmx(1)=nhmax
   ihmx(2)=nkmax
   ihmx(3)=nlmax
!
   call resize_reflections(refob,nrefl)
   if (nrefl == 0) return
   refob%ph = 0.0   !reset superindex
!
   if (wave(1) > 0.0) then
       call calculate_2theta(refob,cell,wave)   ! 2theta and slaq
       call sort_reflections(refob(:nrefl),ORD_BY_2THETA)
   endif
!
   end subroutine create_reflections_mat

!---------------------------------------------------------------------------------------------------------------

   subroutine cmult2016(i1,nsym,icent,kmat,eps,mult,exptype)
   USE math_util
!
!-- compute epsilon and multiplicity by generating equivalent
!-- reflexions
!-- epsylon = number of times same reflexion appears in list
!-- multiplicity = number different reflexions in list
!
   integer, dimension(3), intent(in)     :: i1
   integer, intent(in)                   :: nsym,icent
   integer, dimension(:,:,:), intent(in) :: kmat
   real, intent(out)                     :: eps
   integer, intent(out)                  :: mult
   integer, intent(in)                   :: exptype
   integer, dimension(3)                 :: i2
   integer                               :: j  !!!,k1,k2,ik1
   logical                               :: eq_k1k2,eq_ik1k2
!
   eps=1.0
   mult=1
!-- in triclinic space groups eps = 1.0 and mult = 1
   if (nsym.ne.1) then
       do j=2,nsym
          i2(:) = matmul(i1,kmat(j,:,:))
          eq_k1k2 = equal_vector(i1,i2)
          eq_ik1k2 = equal_vector(-i1,i2)
          if (eq_k1k2) then
              eps = eps + 1.0
          endif
          if (icent /= 0 .and. eq_ik1k2) then
              eps = eps + 1.0
          endif
          if (eq_k1k2 .or. eq_ik1k2) then
              mult = mult + 1
          endif
       enddo
   endif
   mult = nsym / mult 

   if (exptype == POWDER_EXP) mult = 2*mult
!
   end subroutine cmult2016

!---------------------------------------------------------------------------------------------------------------

   subroutine calcola_mult_new(i1,nsym,icent,eps,mult,kmat,exptype)
   use math_util
!
!-- compute epsilon and multiplicity by generating equivalent
!-- reflexions
!-- epsylon = number of times same reflexion appears in list
!-- multiplicity = number different reflexions in list
!
   integer, dimension(3), intent(in)     :: i1
   integer, intent(in)                   :: nsym,icent
   integer, dimension(:,:,:), intent(in) :: kmat
   real, intent(out)                     :: eps
   integer, intent(out)                  :: mult
   integer, intent(in)                   :: exptype
   integer                               :: i2(3),i3(3),ipreso(nsym)
!   integer                               :: i,j,ikk,k1,ik1,k2,multt
   integer                               :: j,ikk
   logical                               :: eq_k3k2,eq_ik3k2
!
! in triclinic space groups eps = 1.0  and mult = 1
!
   ipreso(:) = 1
   eps = 1.0   
!
   do j=1,nsym-1
!
      if (ipreso(j)==0) cycle
      i3(:) = matmul(i1,kmat(j,:,:))
      !k1 = i3(1) * ifat512a + i3(2) * ifat512b + i3(3)
      !ik1 = - k1
! 
      do ikk = j+1,nsym
         if (ipreso(ikk)==0) cycle
         i2(:) = matmul(i1,kmat(ikk,:,:))
         eq_k3k2 = equal_vector(i3,i2)
         eq_ik3k2 = equal_vector(-i3,i2)
!
         if (j==1) then
            if (eq_k3k2) then
                 eps = eps + 1.0
             endif
             if (icent /= 0 .and. eq_ik3k2) then
                 eps = eps + 1.0
             endif
         endif
!
         !k2 = i2(1) * ifat512a + i2(2) * ifat512b + i2(3)
         !if (k2.eq.k1.or.k2.eq.ik1) then
         if (eq_k3k2 .or. eq_ik3k2) then
             ipreso(ikk) = 0
         endif
      enddo
   enddo
!
   mult=sum(ipreso)
   if (exptype == POWDER_EXP) mult = 2*mult
!
   end subroutine calcola_mult_new

!---------------------------------------------------------------------------------------------------------------

   real function ctheta_lam(rho,lambda)
!
!-- compute 2-Theta value from ( sin(theta)/lambda ) ** 2
   USE trig_constants
   implicit none
   real, intent(in) :: rho,lambda
   real :: theta
   theta=sqrt(rho)*lambda
   if (theta.gt.1.) theta=1.
   theta=asin(theta)*rtod
   ctheta_lam=2.0*theta
   end function ctheta_lam

!---------------------------------------------------------------------------------------------------------------

      subroutine indmm(hmin,hmax,kmin,kmax,lmin,lmax,jsys,ihmx,ntc)
      implicit none
      integer  hmin,hmax,kmin,kmax,lmin,lmax                  
      character(len=*), intent(in) :: ntc
      integer, intent(in) :: jsys
      integer, dimension(3), intent(in) :: ihmx
! --
! --   calcolo hmin e hmax , kmin e kmax , lmin e lmax                  
! --
        hmin=-ihmx(1)
        hmax=ihmx(1)
        kmin=-ihmx(2)
        kmax=ihmx(2)
        lmin=-ihmx(3)
        lmax=ihmx(3)
! --
      select case(jsys)
      case (1)    ! TRICLINO
        hmin=0
! --
! -- MONOCLINO
! --
      case (2)    ! MONOCLINO
       if (ntc(3:3).eq.'1') then
        if (ntc(5:5).eq.'1') then
! -- 2||c
            hmin=0
            lmin=0         
              !write(6,*)'AX=2||C',hmin,kmin,lmin
        else 
! -- 2||b
            hmin=0
            kmin=0         
              !write(6,*)'AX=2||B',hmin,kmin,lmin
        endif      
       else
        if (ntc(4:4).eq.'1') then
         if (ntc(6:6).eq.'1') then
! -- 2||a
            hmin=0
            kmin=0
              !write(6,*)'AX=2||A',hmin,kmin,lmin
         else
! -- 2||b
            hmin=0
            kmin=0         
              !write(6,*)'AX=2||B',hmin,kmin,lmin
         endif
        else
         if(ntc(5:5).eq.'1') then
! -- 2||a
            kmin=0
            hmin=0
              !write(6,*)'AX=2||A',hmin,kmin,lmin
         else
! -- 2||b
            hmin=0
            kmin=0         
              !write(6,*)'AX=2||B',hmin,kmin,lmin
         endif
        endif
       endif
! --
      case (3)   ! ORTOROMBICO
          hmin=0
          kmin=0
          lmin=0

      case (4)   ! TETRAGONALE
          hmin=0
          kmin=0
          lmin=0

      case (5)   ! ESAGONALE E TRIGONALE : 
          hmin=0
          lmin=0

      case (6)   ! CUBICO :  va' considerata la condizione aggiuntiva h<=k<=l
          hmin=0
          kmin=0
          lmin=0

      case (7)   ! RHOMBOHEDRIC CASE
          hmin=0
      end select
      return
      end subroutine indmm
!---------------------------------------------------------------------------------------------------------------

      subroutine indmm1(hmin,hmax,kmin,kmax,lmin,lmax,jsys,ihmx,spg)
      USE spginfom
      implicit none
      integer  hmin,hmax,kmin,kmax,lmin,lmax                  
      type(spaceg_type), intent(in) :: spg
      integer, intent(in) :: jsys
      integer, dimension(3), intent(in) :: ihmx
! --
! --   calcolo hmin e hmax , kmin e kmax , lmin e lmax                  
! --
        hmin=-ihmx(1)
        hmax=ihmx(1)
        kmin=-ihmx(2)
        kmax=ihmx(2)
        lmin=-ihmx(3)
        lmax=ihmx(3)
! --
      select case(jsys)
      case (1)    ! TRICLINO
        hmin=0
! --
! -- MONOCLINO
! --
      case (2)    ! MONOCLINO
          select case(spg%axis_direction())
            case ('a')
              hmin=0
              kmin=0
              !write(6,*)'AX=2||A',hmin,kmin,lmin
            case ('b')
              hmin=0
              kmin=0         
              !write(6,*)'AX=2||B',hmin,kmin,lmin
            case ('c')
              hmin=0
              lmin=0         
              !write(6,*)'AX=2||C',hmin,kmin,lmin
          end select

      case (3)   ! ORTOROMBICO
          hmin=0
          kmin=0
          lmin=0

      case (4)   ! TETRAGONALE
          hmin=0
          kmin=0
          lmin=0

      case (5)   ! ESAGONALE E TRIGONALE : 
          hmin=0
          lmin=0

      case (6)   ! CUBICO :  va' considerata la condizione aggiuntiva h<=k<=l
          hmin=0
          kmin=0
          lmin=0

      case (7)   ! RHOMBOHEDRIC CASE
          hmin=0
      end select
      return
      end subroutine indmm1

!-----------------------------------------------------------------

      subroutine extin(jex,kg1,kg2,kg3,ibrav,klatt,ksym,vsym,nsym,kmat,tmat,icent)
!   check for extinction (jex=0/1 allowed/absent)
!   first: check bravais lattice type
!   type  p   a  b    c   i    f   r
!   latt: 1   2   3   4   5    6   7
!   ibrav(    1   2   3   4    5   6  )  x  13
      implicit none
      integer :: jex, kg1, kg2, kg3, klatt, ksym
      integer ibrav(6,13),kg(3) !!!,vsym(48)    !!!!,mr(3)
      integer, intent(in)                   :: nsym
      integer, dimension(:,:,:), intent(in) :: kmat
      real, dimension(:,:), intent(in)      :: tmat
      integer, intent(in)                   :: icent
      integer, dimension(:), intent(in) :: vsym
      integer, dimension(3) :: nhh
      integer :: nbrav, lsym
      integer :: i,j,jj
      integer :: ica, jc, kc, isum, lb, nh, isym
      real    :: ca
!
      kg(1)=kg1
      kg(2)=kg2
      kg(3)=kg3
      do 10 i=1,3
      if (kg(i).ne.0) go to 20
   10 continue
      jex=1
      return
   20 jex=0
      if(klatt.eq.0) go to 420
      nbrav=ibrav(klatt,1)
      do 410 i=1,nbrav
      isum=0
      do j=1,3
         lb=1+(i-1)*4+j
         isum=isum+kg(j)*ibrav(klatt,lb)
      enddo
      lb=4*i+1
      if(mod(isum,ibrav(klatt,lb)).eq.0) go to 410
      jex=1
      go to 700
410   continue
420   continue
!   second: check translations from symmetry operators
!         first condition for h to absent:  h*ri=h   (h=kg)
      if (ksym.eq.0) go to 700
      jc=icent+1
      do 490 kc=1,jc
      do 480 lsym=1,ksym
      isym=vsym(lsym)
      ca=0.
          if (kc == 2) then
              nhh(:) = matmul(kg,kmat(isym+nsym,:,:))
          else
              nhh(:) = matmul(kg,kmat(isym,:,:))
          endif
      do jj=1,3
!corr         nh=0
!corr         do j=1,3
!corr           mr(j)=0
!corr         enddo
!corr         do i=1,2
!corr            do j=1,3
!corr               ikk=ismat(i,j,isym)
!corr                ka=iabs(ikk)
!corr               if (ka == jj) then
!corr                   if (kc.eq.2) ikk=-ikk
!corr                   mr(j)=isign(1,ikk)
!corr               endif
!corr            enddo
!corr         enddo
!corr         do j=1,3
!corr            nh=kg(j)*mr(j)+nh
!corr         enddo
!corr         write(71,*)abs(nhh(jj)-nh),jj,'nh=',nh,nhh(jj),kg(jj)
         nh = nhh(jj)
         if(kg(jj).ne.nh)goto 480
!corr         ca=float(nh)*tsmat(jj,isym)   +  ca
         ca=float(nh)*tmat(isym,jj)   +  ca
      enddo
!  second condition:    h*ts ; integer, for an ext. reflection.
      ca=abs(ca)
      ica=int((ca+0.0005)*1000)
      if(mod(ica,1000).eq.0)goto 480
!  variable ca is not an integer number,the reflection is absent.
      jex=1
      goto 700
480     continue
490   continue
700     continue
      return
      end subroutine extin
!-----------------------------------------------------------------------
      subroutine geneq(lty,ih,kk,il,idelt,jcode,isnn,istd,rho,kmat,tmat,icent,nsym,rhomax,ihmx,pp,iahmax,iakmax,ialmax)
      implicit none
      integer, intent(in)                   :: lty
      integer, intent(inout)                :: ih,kk,il
      integer, intent(out)                  :: jcode, isnn
      integer, dimension(:,:,:), intent(in) :: kmat
      real, dimension(:,:), intent(in)      :: tmat
      integer, intent(in)                   :: icent
      integer, intent(in)                   :: nsym
      real, intent(inout)                   :: rhomax
      integer, dimension(3), intent(inout)  :: ihmx
      real, dimension(6), intent(in)        :: pp
      integer, intent(inout)                :: iahmax,iakmax,ialmax
      integer, dimension(nsym)              :: kl1,kl2,kl3
      integer, dimension(3)                 :: i1,i2,i1s
      integer                               :: i,j,ind,ido,maxi,ifl,istd,idelt
      integer                               :: jcode1,jcode2,jm1,ii1,ii2
      real                                  :: rho
      integer                               :: iah,iak,ial
!      common/indexmax/iah,iak,ial,iahmax,iakmax,ialmax
!      integer :: iah,iak,ial,iahmax,iakmax,ialmax
!
      isnn=1
      i1(1)=ih
      i1(2)=kk
      i1(3)=il
      jcode=13
      ido=1
      maxi=1
      do 250 j=1,nsym
         kl1(j)=2400
         kl2(j)=2400
         do i=1,3
            i2(i)=0
            kl1(j)=kl1(j)-i1(i) * int(tmat(j,i)*24+0.01)
            kl2(j)=kl2(j)-i1(i) * int(tmat(j,i)*12+0.01)
         enddo
         kl1(j)=mod(kl1(j),24)
         i2(:) = matmul(i1,kmat(j,:,:))
!
         if(icent.eq.1.or.ido.ne.1) goto 230
         jcode=1
         do 225 i=1,3
            if(i1(i)+i2(i).ne.0) goto 230
  225    continue
         jcode=mod(kl2(j),12)+1
         if(jcode.le.1) jcode=jcode+12
         ido=0
  230    ind=262144 * i2(1) + 512 * i2(2) + i2(3)
         kl3(j)=131328 + iabs(ind)
         rho=rhof(pp,i2(1),i2(2),i2(3))
         if (lty.eq.1) then
             rhomax=amax1(rhomax,rho)
             if(ihmx(1).lt.iabs(i2(1))) ihmx(1)=iabs(i2(1))
             if(ihmx(2).lt.iabs(i2(2))) ihmx(2)=iabs(i2(2))
             if(ihmx(3).lt.iabs(i2(3))) ihmx(3)=iabs(i2(3))
             rho=rho*rho
!
             ii1 = iabs(i1(1))
             ii2 = iabs(i2(1))
             iah=max0(ii1,ii2)
             if (iahmax.le.iah) iahmax = iah
!
             ii1 = iabs(i1(2))
             ii2 = iabs(i2(2))
             iak=max0(ii1,ii2)
             if (iakmax.le.iak) iakmax = iak
!
             ii1 = iabs(i1(3))
             ii2 = iabs(i2(3))
             ial=max0(ii1,ii2)
             if (ialmax.le.ial) ialmax = ial
!
         endif
         if(j.eq.1) goto 250
         jm1=j-1
         do 240 i=1,jm1
            if(kl3(i).eq.kl3(j)) kl3(j)=0
  240    continue
         if(kl3(j).le.kl3(maxi)) go to 250
         maxi=j
         isnn=isign(1,ind)
  250 continue
      call unpack2 (kl3(maxi),ih,kk,il)
      idelt=kl1(maxi)*15
      istd=0
      if (icent.eq.0) then
          ido=1
          i1s(1)=ih
          i1s(2)=kk
          i1s(3)=il
          do 300 j=1,nsym
             kl1(j)=24000
             do 260 i=1,3
                i2(i)=0
                kl1(j)=kl1(j)+i1s(i) * nint(tmat(j,i)*12)
  260        continue
             i2(:) = matmul(i1s,kmat(j,:,:))
             if (ido.eq.1) then
                 jcode=1
                 ifl = 0
                 do 290 i=1,3
                    if (i1s(i)+i2(i).ne.0) ifl = 1
  290            continue
                 if (ifl.eq.0) then
                     jcode1 = mod ( kl1(j) , 24 )
                     jcode2 = mod ( ( jcode1 + 12 ), 24 )
                     jcode = min0 ( jcode1 , jcode2 )
                     jcode = jcode + 1
                     if (jcode.le.1) jcode=jcode+12
                     ido=0
                   endif
               endif
  300     continue
!
          ii1 = iabs(i1s(1))
          ii2 = iabs(i2(1))
          iah=max0(ii1,ii2)
          if (iahmax.le.iah) iahmax = iah
          ii1 = iabs(i1s(2))
          ii2 = iabs(i2(2))
          iak=max0(ii1,ii2)
          if (iakmax.le.iak) iakmax = iak
          ii1 = iabs(i1s(3))
          ii2 = iabs(i2(3))
          ial=max0(ii1,ii2)
          if (ialmax.le.ial) ialmax = ial
!
      endif
!
      if (lty.eq.1) return
      if (i1(1).eq.ih.and.i1(2).eq.kk.and.i1(3).eq.il) istd=1
      return
      end subroutine geneq

!-----------------------------------------------------------------------

   subroutine unpack2(ind,ih,kk,il)
   implicit none
   integer, intent(in)  :: ind
   integer, intent(out) :: ih, kk, il
   integer              :: is
   ih = ind/262144
   is = ind - 262144*ih
   kk = is/512-256
   il = is-512*(kk+256)-256
   return
   end subroutine unpack2

!----------------------------------------------------------------------
   real function rhof(p,jh,jk,jl)
   implicit none
   real, dimension(6), intent(in) :: p
   integer, intent(in)            :: jh,jk,jl
   real                           :: rho2
   rho2=p(1)*float(jh*jh)+p(2)*float(jk*jk)   &
    +p(3)*float(jl*jl)+p(4)*float(jh*jk)      &
    +p(5)*float(jh*jl)+p(6)*float(jk*jl)
   rhof=sqrt(rho2)
   end function rhof

!-----------------------------------------------------------------------

   real function derivataLP(tthd,kpol)
   USE trig_constants, only:dtor
   real, intent(in) :: tthd
   real, intent(in) :: kpol
   real             :: tth,th
   real             :: Csc_th, Sec_th, Tan_th, Sin_th
!
   tth = tthd*dtor  
   th = 0.5 * tth
   Sin_th = Sin(th); Csc_th = 1.0/Sin_th ; Sec_th = 1.0/cos(th); Tan_th = Sin_th / Cos(th)
   derivataLP = 0.5*((1 + kpol)*Csc_th - 2*(1 + kpol)*Csc_th**3 + 4*kpol*Sin_th + (1 + kpol)*Sec_th*Tan_th)
!
   end function derivataLP

!-----------------------------------------------------------------------

   function derteta_risp_celldir(spg,cell,hkl,ttetad,lambda) result(der)
!
!  Questa function calcola le derivate di theta rispetto ai parametri di cella per un riflesso
!
   USE trig_constants
   USE spginfom
!
   implicit none
   real, dimension(6)             :: der
   type(spaceg_type), intent(in)  :: spg                         ! simmetria
   real, dimension(6), intent(in) :: cell                        ! cella
   integer, dimension(3)          :: hkl                         ! h,k,l del riflesso
   real                           :: ttetad                      ! 2theta in gradi del riflesso
   real   , intent(in)            :: lambda                      ! lunghezza d'onda
   integer                        :: h,k,l                       ! indici h,k,l del riflesso j
   real                           :: a,b,c
   real                           :: cos_alfa,cos_beta,cos_gamma
   real                           :: sen_alfa,sen_beta,sen_gamma
   real                           :: cot_beta,csc_beta
   real                           :: dhnum,dhden                 ! numeratore e denominatore formula per 1/d**2
   real                           :: dertetad
   integer                        :: ksy
!
   h        = hkl(1)            ; k        = hkl(2)            ; l         = hkl(3)
   a        = cell(1)           ; b        = cell(2)           ; c         = cell(3)
   cos_alfa = Cos(cell(4)*dtor) ; cos_beta = Cos(cell(5)*dtor) ; cos_gamma = Cos(cell(6)*dtor)
   sen_alfa = Sin(cell(4)*dtor) ; sen_beta = Sin(cell(5)*dtor) ; sen_gamma = Sin(cell(6)*dtor)
   cot_beta = cos_beta/sen_beta ; csc_beta = 1.0/sen_beta
!
!  Calcolo derivata di theta rispetto a (1/d)**2 = (2sen(theta) / lambda)**2
!
!  dertetad = lambda**2 / (4*sen(2theta))
!
   dertetad = lambda**2 / (4.0*sin(ttetad*dtor))
!
!  Derivata di theta rispetto al parametro = dertetad * (derivata di (1/d)**2 rispetto al corrisp. parametro)
!
   der = 0.0
   ksy = lattice_system(spg,cell)
   select case(ksy)

     case (1)                      ! TRICLINO

      dhnum = ( (h**2*sen_alfa**2)/a**2 + (k**2*sen_beta**2)/b**2 + (l**2*sen_gamma**2)/c**2  +    &
              (2*k*l*(-cos_alfa + cos_beta*cos_gamma))/(b*c)                                  +    &
              (2*h*l*(-cos_beta + cos_alfa*cos_gamma))/(a*c)                                  +    &
              (2*h*k*(cos_alfa*cos_beta - cos_gamma))/(a*b) )
      dhden = (1 - cos_alfa**2 - cos_beta**2 - cos_gamma**2 + 2*cos_alfa*cos_beta*cos_gamma)
      der(1) = dertetad * ( (-2*h*k*(cos_alfa*cos_beta  - cos_gamma))/(a**2*b) -          &
             ( 2*h*l*(cos_alfa*cos_gamma - cos_beta)) /(a**2*c) - ( 2*h**2*sen_alfa**2)/a**3 ) / dhden
      der(2) = dertetad * ( (-2*h*k*(cos_alfa*cos_beta - cos_gamma)) /(a*b**2) -          &
             ( 2*k*l*(-cos_alfa + cos_beta*cos_gamma))/(b**2*c) - ( 2*k**2*sen_beta**2)/b**3 ) / dhden
      der(3) = dertetad * ( (-2*h*l*(-cos_beta + cos_alfa*cos_gamma))/(a*c**2) -          &
             ( 2*k*l*(-cos_alfa + cos_beta*cos_gamma))/(b*c**2) - (2*l**2*sen_gamma**2)/c**3 ) / dhden
      der(4) = dertetad * (((2*k*l*sen_alfa)/(b*c) + (2*h**2*cos_alfa*sen_alfa)/a**2 -    &
               (2*h*k*cos_beta* sen_alfa)/(a*b) - (2*h*l*cos_gamma*sen_alfa)/(a*c) ) / dhden          -    &
              ((2*cos_alfa*sen_alfa - 2*cos_beta*cos_gamma*sen_alfa)*dhnum) / dhden**2)
      der(5) = dertetad * (((2*h*l*sen_beta)/(a*c) - (2*h*k*cos_alfa*sen_beta)/(a*b) +    &
              (2*k**2*cos_beta*sen_beta)/b**2 - (2*k*l*cos_gamma*sen_beta)/(b*c) ) / dhden            -    &
             ((2*cos_beta*sen_beta - 2*cos_alfa*cos_gamma*sen_beta)*dhnum) / dhden**2)
      der(6) = dertetad * (((2*h*k*sen_gamma)/(a*b) - (2*h*l*cos_alfa*sen_gamma)/(a*c) -  &
               (2*k*l*cos_beta*sen_gamma)/(b*c) + (2*l**2*cos_gamma*sen_gamma)/c**2 ) / dhden           -  &
              ((-2*cos_alfa*cos_beta*sen_gamma + 2*cos_gamma*sen_gamma)*dhnum) / dhden**2)
     case (2)                      ! MONOCLINO

      der(1) = dertetad * ((2*h*l*cot_beta*csc_beta)/(a**2*c) - (2*h**2*csc_beta**2)/a**3)
      der(2) = dertetad * (-2*k**2)/b**3
      der(3) = dertetad * ((2*h*l*cot_beta*csc_beta)/(a*c**2) - (2*l**2*csc_beta**2)/c**3)
      der(5) = dertetad * ((2*h*l*cot_beta**2*csc_beta)/(a*c) - (2*h**2*cot_beta*csc_beta**2)/a**2 -   &
                                         (2*l**2*cot_beta*csc_beta**2)/c**2 + (2*h*l*csc_beta**3)/(a*c))
     case (3)                      ! ORTOROMBICO

      der(1) = dertetad * (-2*h**2)/a**3
      der(2) = dertetad * (-2*k**2)/b**3
      der(3) = dertetad * (-2*l**2)/c**3

     case (4)                      ! TETRAGONALE

      der(1) = dertetad * (-2*(h**2 + k**2))/a**3
      der(3) = dertetad * (-2*l**2)/c**3

     case (5)                      ! ESAGONALE

      der(1) = dertetad * (-8*(h**2 + h*k + k**2))/(3.*a**3)
      der(3) = dertetad * (-2*l**2)/c**3

     case (6)                      ! CUBICO

      der(1) = dertetad * (-2*(h**2 + k**2 + l**2))/a**3

   end select
!
!  Le derivate degli angoli vanno calcolate in gradi per avere degli shift in gradi
   der(4:6) = dtor * der(4:6)
!
   end function derteta_risp_celldir

!--------------------------------------------------------------------------------------------------

   subroutine resize_reflections(vetr,n,savevet)
!
!  Rialloca ad n un vettore di tipo model
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(reflection_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                               :: n
   logical, optional, intent(in)                     :: savevet
   logical                                           :: savev
   integer                                           :: nv
   type(reflection_type), allocatable                :: vsav(:)
   integer                                           :: nsav
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
   end subroutine resize_reflections
   
!----------------------------------------------------------------------------------------------------

   subroutine new_reflections(vetr,n)
!
!  Create new atoms
!
   type(reflection_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                         :: n

   if (n < 0) return
   if (numrefl(vetr) /= n) then
       if (allocated(vetr))deallocate(vetr)
       if (n > 0) allocate(vetr(n))
   endif

   end subroutine new_reflections

!----------------------------------------------------------------------------------------------------

   subroutine clear_reflections(vetr)
!
!  Delete all atoms
!
   type(reflection_type), allocatable, intent(inout) :: vetr(:)

   if (allocated(vetr)) deallocate(vetr)

   end subroutine clear_reflections

!----------------------------------------------------------------------------------------------------

   subroutine sfcalc_anis(ref,atx,spg,elem,radtype,anomal)
   USE spginfom
   USE trig_constants, only: twopi,rtod
   USE elements
   USE atom_basic
!
   type(reflection_type), dimension(:), intent(inout) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)          :: atx    ! modello strutturale
   type(spaceg_type), intent(in)                      :: spg
   type(element_type), dimension(:), intent(in)       :: elem
   integer, intent(in)                                :: radtype
   logical, intent(in)                                :: anomal

   if (anomal) then
       call sfcalc_anis_anomal(ref,atx,spg,elem,radtype)
   else
       call sfcalc_anis_no_anomal(ref,atx,spg,elem,radtype)
   endif

   end subroutine sfcalc_anis

!----------------------------------------------------------------------------------------------------

   subroutine sfcalc_anis_no_anomal(ref,atx,spg,elem,radtype)
   USE spginfom
   USE trig_constants, only: twopi,rtod
   USE elements
   USE atom_basic
!
   type(reflection_type), dimension(:), intent(inout) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)          :: atx    ! modello strutturale
   type(spaceg_type), intent(in)                      :: spg
   type(element_type), dimension(:), intent(in)       :: elem
   integer, intent(in)                                :: radtype
   integer                                            :: natoms
   real, dimension(4,spg%nsym)                        :: hkle
   integer, dimension(3)                              :: i1
   integer                                            :: i,j,jj
   real                                               :: rho
   integer                                            :: kz
   real                                               :: contc
   real                                               :: conts
   real                                               :: tfocc
   real                                               :: act,bct
   real                                               :: faze
   real                                               :: at,bt
   real                                               :: argg
   real                                               :: anis
   real, dimension(6)                                 :: beta
   real, dimension(3)                                 :: h
   real, dimension(size(elem)) :: ff

   natoms = size(atx)
   if (spg%symcent == 1) then
       loop_riflessi: do i=1,size(ref)
          i1 = ref(i)%hkl
!-- compute H*R and H*T
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo
!
          rho=ref(i)%slaq
!
          ff(:) = at_scatt(elem,rho,radtype)
!
          act=0.0
          loop_atomi: do jj=1,natoms
             kz = atx(jj)%kscatt()
             if (atx(jj)%bij(1) > 0) then
                 tfocc=spg%ncoper*ff(kz)*atx(jj)%och*atx(jj)%ocry
                 at=0.0
                 !bt=0.0
                 beta(:) = atx(jj)%bij(:) ! b11,b22,b33,b12,b13,b23
                 do j=1,spg%nsym
                    argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                    h(:) = hkle(:3,j)
                    anis =    h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                         +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                    anis=exp(-anis)
                    at = at + cos(argg)*anis
                 enddo
             else
                 tfocc=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
                 at=0.0
                 do j=1,spg%nsym
                    argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                    at = at + cos(argg)
                 enddo
             endif
             act=act+at*tfocc
          enddo loop_atomi
!
          if (act >= 0.0) then
              ref(i)%fc = 2.0*act
              ref(i)%ph = 0
          else
              ref(i)%fc = -2.0*act
              ref(i)%ph = 180
          endif
          !write(71,'(4i5,3f10.3,i5)')i,ref(i)%hkl,2*act,0.0,ref(i)%fc,ref(i)%ph
       enddo loop_riflessi
   else
        loop_riflessi1: do i=1,size(ref)
          i1 = ref(i)%hkl
!-- compute H*R and H*T
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo

          rho=ref(i)%slaq
!
          ff(:) = at_scatt(elem,rho,radtype)
!
          act=0.0
          bct=0.0
          loop_atomi1: do jj=1,natoms
             kz = atx(jj)%kscatt()
             if (atx(jj)%bij(1) > 0) then
                 tfocc=spg%ncoper*ff(kz)*atx(jj)%och*atx(jj)%ocry
                 at=0.0
                 bt=0.0
                 beta(:) = atx(jj)%bij(:) ! b11,b22,b33,b12,b13,b23
                 do j=1,spg%nsym
                    argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                    h(:) = hkle(:3,j)
                    anis =    h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                         +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                    anis=exp(-anis)
                    at = at + cos(argg)*anis
                    bt = bt + sin(argg)*anis
                 enddo
             else
                 tfocc=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
                 at=0.0
                 bt=0.0
                 do j=1,spg%nsym
                    argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                    at = at + cos(argg)
                    bt = bt + sin(argg)
                 enddo
             endif
             contc=at
             conts=bt
             act=act+contc*tfocc
             bct=bct+conts*tfocc
          enddo loop_atomi1
!
          ref(i)%fc = sqrt(act**2+bct**2)
          faze=rtod*atan2(bct,act)+360.0
          ref(i)%ph = mod(int(faze+0.5),360)
          !write(71,'(4i5,3f10.3,i5)')i,ref(i)%hkl,act,bct,ref(i)%fc,ref(i)%ph
       enddo loop_riflessi1
   endif
!
   end subroutine sfcalc_anis_no_anomal

!----------------------------------------------------------------------------------------------------

   subroutine sfcalc_anis_anomal(ref,atx,spg,elem,radtype)
   USE spginfom
   USE trig_constants, only: twopi,rtod
   USE elements
   USE atom_basic
!
   type(reflection_type), dimension(:), intent(inout) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)          :: atx    ! modello strutturale
   type(spaceg_type), intent(in)                      :: spg
   type(element_type), dimension(:), intent(in)       :: elem
   integer, intent(in)                                :: radtype
   integer                                            :: natoms
   real, dimension(4,spg%nsym)                        :: hkle
   integer, dimension(3)                              :: i1
   integer                                            :: i,j,jj
   real                                               :: rho
   integer                                            :: kz
   real                                               :: tconst,tfocc,tfoccd
   real                                               :: freal,fimag
   real                                               :: freal1,fimag1,freal2,fimag2
   real                                               :: faze
   real                                               :: at,bt
   real                                               :: argg
   real                                               :: anis
   real, dimension(6)                                 :: beta
   real, dimension(3)                                 :: h
   real, dimension(size(elem)) :: ff

   natoms = size(atx)
   if (spg%symcent == 1) then
       loop_riflessi: do i=1,size(ref)
          i1 = ref(i)%hkl
!-- compute H*R and H*T
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo
!
          rho=ref(i)%slaq
!
          ff(:) = at_scatt(elem,rho,radtype) + elem(:)%f1
!
          freal=0.0
          fimag=0.0
          loop_atomi: do jj=1,natoms
             kz = atx(jj)%kscatt()
             if (atx(jj)%bij(1) > 0) then
                 !tfocc=spg%ncoper*ff(kz)*atx(jj)%och*atx(jj)%ocry
                 tconst=spg%ncoper*atx(jj)%och*atx(jj)%ocry
                 tfocc=tconst*ff(kz)
                 tfoccd=tconst*elem(kz)%f2
                 at=0.0
                 beta(:) = atx(jj)%bij(:) ! b11,b22,b33,b12,b13,b23
                 do j=1,spg%nsym
                    argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                    h(:) = hkle(:3,j)
                    anis =    h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                         +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                    anis=exp(-anis)
                    at = at + cos(argg)*anis
                 enddo
             else
!!!!                 tfocc=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
                 tconst=spg%ncoper*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
                 tfocc=tconst*ff(kz)
                 tfoccd=tconst*elem(kz)%f2
                 at=0.0
                 do j=1,spg%nsym
                    argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                    at = at + cos(argg)
                 enddo
             endif
!!!!             act=act+at*tfocc
             freal=freal+at*tfocc
             fimag=fimag+at*tfoccd
             write(70,*)'REF:',i,jj,freal,fimag,tfocc,tfoccd
          enddo loop_atomi
!
!!!!          if (act >= 0.0) then
!!!!              ref(i)%fc = 2.0*act
!!!!              ref(i)%ph = 0
!!!!          else
!!!!              ref(i)%fc = -2.0*act
!!!!              ref(i)%ph = 180
!!!!          endif

          freal = 2*freal
          fimag = 2*fimag
          ref(i)%fc = sqrt(freal**2 + fimag**2)
          faze=rtod*atan2(fimag,freal)+360.0
          ref(i)%ph = mod(int(faze+0.5),360)
          !write(71,'(4i5,3f12.5,2i5)')i,ref(i)%hkl,freal,fimag,ref(i)%fc,ref(i)%ph,ref(i)%m
       enddo loop_riflessi
   else
        loop_riflessi1: do i=1,size(ref)
          i1 = ref(i)%hkl
!-- compute H*R and H*T
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo

          rho=ref(i)%slaq
!
          ff(:) = at_scatt(elem,rho,radtype)
!
!!!!          act=0.0
!!!!          bct=0.0
          freal1=0.0
          fimag1=0.0
          freal2=0.0
          fimag2=0.0
          loop_atomi1: do jj=1,natoms
             kz = atx(jj)%kscatt()
             if (atx(jj)%bij(1) > 0) then
!!!!                 tfocc=spg%ncoper*ff(kz)*atx(jj)%och*atx(jj)%ocry
                 tconst=spg%ncoper*atx(jj)%och*atx(jj)%ocry
                 tfocc=tconst*ff(kz)
                 tfoccd=tconst*elem(kz)%f2
                 at=0.0
                 bt=0.0
                 beta(:) = atx(jj)%bij(:) ! b11,b22,b33,b12,b13,b23
                 do j=1,spg%nsym
                    argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                    h(:) = hkle(:3,j)
                    anis =    h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                         +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                    anis=exp(-anis)
                    at = at + cos(argg)*anis
                    bt = bt + sin(argg)*anis
                 enddo
             else
!!!!                 tfocc=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
                 tconst=spg%ncoper*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
                 tfocc=tconst*ff(kz)
                 tfoccd=tconst*elem(kz)%f2
                 at=0.0
                 bt=0.0
                 do j=1,spg%nsym
                    argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                    at = at + cos(argg)
                    bt = bt + sin(argg)
                 enddo
             endif
!!!!             contc=at
!!!!             conts=bt
!!!!             act=act+contc*tfocc
!!!!             bct=bct+conts*tfocc
             freal1=freal1+at*tfocc
             freal2=freal2+bt*tfoccd
             fimag1=fimag1+bt*tfocc
             fimag2=fimag2+at*tfoccd
          enddo loop_atomi1
!
!!!!          ref(i)%fc = sqrt(act**2+bct**2)
!!!!          faze=rtod*atan2(bct,act)+360.0
!!!!          ref(i)%ph = mod(int(faze+0.5),360)
          freal = freal1 - freal2
          fimag = fimag1 + fimag2
          ref(i)%fc = sqrt(freal**2+fimag**2)
          faze=rtod*atan2(fimag,freal)+360.0
          ref(i)%ph = mod(int(faze+0.5),360)
          !write(71,'(4i5,3f12.5,2i5)')i,ref(i)%hkl,freal,fimag,ref(i)%fc,ref(i)%ph,ref(i)%m
       enddo loop_riflessi1
   endif
!
   end subroutine sfcalc_anis_anomal

!-----------------------------------------------------------------------

   subroutine fcalcadern(ref,atx,spg,elem,alambda,radtype,anomal,der,derft)
   USE trig_constants, only: twopi,pi
   USE spginfom
   USE elements
   USE atom_basic
   USE progtype, only: deratom
!
   type(reflection_type), dimension(:), intent(in)            :: ref  ! i riflessi
   type(atom_type), dimension(:), intent(in)                  :: atx  ! il modello strutturale
   type(spaceg_type), intent(in)                              :: spg
   type(element_type), dimension(:), intent(in)               :: elem
   real, intent(in)                                           :: alambda
   integer, intent(in)                                        :: radtype
   logical, intent(in)                                        :: anomal
   type(deratom), dimension(size(ref),size(atx)), intent(out) :: der
   real, dimension(size(ref)), optional                       :: derft

   if (anomal) then
       call fcalcadern_anomal(ref,atx,spg,elem,alambda,radtype,der,derft)
   else
       call fcalcadern_noanomal(ref,atx,spg,elem,alambda,radtype,der,derft)
   endif

   end subroutine fcalcadern
 
!-----------------------------------------------------------------------

   subroutine fcalcadern_noanomal(ref,atx,spg,elem,alambda,radtype,der,derft)
   USE trig_constants, only: twopi,pi
   USE spginfom
   USE elements
   USE atom_basic
   USE progtype, only: deratom
!
   type(reflection_type), dimension(:), intent(in)            :: ref  ! i riflessi
   type(atom_type), dimension(:), intent(in)                  :: atx  ! il modello strutturale
   type(spaceg_type), intent(in)                              :: spg
   type(element_type), dimension(:), intent(in)               :: elem
   real, intent(in)                                           :: alambda
   integer, intent(in)                                        :: radtype
   type(deratom), dimension(size(ref),size(atx)), intent(out) :: der
   real, dimension(size(ref)), optional                       :: derft
   real, dimension(4,spg%nsym)                                :: hkle
   integer, dimension(3)                                      :: i1
   integer                                                    :: natoms
   integer                                                    :: i,j,k,jj
   real                                                       :: rho
   integer                                                    :: kz
   real                                                       :: contc
   real                                                       :: conts
   real                                                       :: effe
   real                                                       :: tfocc
   real                                                       :: tfocp
   real                                                       :: s,ss 
   real                                                       :: cc,c  
   real                                                       :: act
   real                                                       :: bct
   real                                                       :: fact,fbct
   real                                                       :: at,bt
   real                                                       :: hkltf
   real, dimension(5,size(atx))                               :: apdph,bpdph
   real                                                       :: argg,act2
   real, dimension(size(elem))                                :: ff
!
   natoms = size(atx)
   if (spg%symcent == 1) then
       loop_riflessi1: do i=1,size(ref)
!      
!-- compute H*R and H*T
          i1 = ref(i)%hkl
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo
       
          rho=ref(i)%slaq
          ss=ref(i)%slaq*(alambda**2)
          s=sqrt(ss)
          cc=1.0-ss
          c=sqrt(cc)
          act=0.0
          fact=0.0
          ff(:) = at_scatt(elem,rho,radtype)
          loop_atomi1: do jj=1,natoms
             do k=1,5
                apdph(k,jj) = 0.0
             enddo
             kz = atx(jj)%kscatt()
             effe=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)
             tfocc=effe*atx(jj)%och*atx(jj)%ocry
             tfocp=tfocc*twopi
             at=0.0
             do j=1,spg%nsym
                argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                at = at + cos(argg)
                do k=1,3
                   hkltf=hkle(k,j)*tfocp*2.0
                   apdph(k,jj)=apdph(k,jj)-hkltf*sin(argg)
                enddo
             enddo
             contc=2.0*at
             apdph(5,jj) = effe*contc*atx(jj)%ocry
             contc=contc*tfocc
             act=act+contc
             fact=fact+contc*(-2.0*c*s*atx(jj)%biso)/(alambda**2)
             apdph(4,jj) = -rho*contc
          enddo loop_atomi1
          act2 = 2.0*act
          if (present(derft)) then
              derft(i) = act2*fact
          endif
          do jj=1,natoms
             der(i,jj)%co  = apdph(:3,jj)*act2
             der(i,jj)%b   = apdph(4,jj)*act2
             der(i,jj)%occ = apdph(5,jj)*act2
          enddo
       enddo loop_riflessi1
   else
       loop_riflessi: do i=1,size(ref)
!-- compute H*R and H*T
          i1 = ref(i)%hkl
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo
       
          rho=ref(i)%slaq
          ss=ref(i)%slaq*(alambda**2)
          s=sqrt(ss)
          cc=1.0-ss
          c=sqrt(cc)
          act=0.0
          bct=0.0
          fact=0.0
          fbct=0.0
          ff(:) = at_scatt(elem,rho,radtype)
          loop_atomi: do jj=1,natoms
             do k=1,5
                apdph(k,jj) = 0.0
                bpdph(k,jj) = 0.0
             enddo
             kz = atx(jj)%kscatt()
             effe=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)
             tfocc=effe*atx(jj)%och*atx(jj)%ocry
             tfocp=tfocc*twopi
             at=0.0
             bt=0.0
             do j=1,spg%nsymop_prim
                argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                at = at + cos(argg)
                bt = bt + sin(argg)
                do k=1,3
                   hkltf=hkle(k,j)*tfocp
                   apdph(k,jj)=apdph(k,jj)-hkltf*sin(argg)
                   bpdph(k,jj)=bpdph(k,jj)+hkltf*cos(argg)
                enddo
             enddo
             contc=at
             conts=bt
             apdph(5,jj) = effe*contc*atx(jj)%ocry
             bpdph(5,jj) = effe*conts*atx(jj)%ocry
             contc=contc*tfocc
             conts=conts*tfocc
             act=act+contc
             bct=bct+conts
             fact=fact+contc*(-2.0*c*s*atx(jj)%biso)/(alambda**2)
             fbct=fbct+conts*(-2.0*c*s*atx(jj)%biso)/(alambda**2)
             apdph(4,jj) = -rho*contc
             bpdph(4,jj) = -rho*conts
          enddo loop_atomi
          if (present(derft)) then
              derft(i) = 2.0*(act*fact+bct*fbct)
          endif
          do jj=1,natoms
             der(i,jj)%co  = 2.0*(apdph(:3,jj)*act + bpdph(:3,jj)*bct)
             der(i,jj)%b   = 2.0*(apdph(4,jj)*act + bpdph(4,jj)*bct)
             der(i,jj)%occ = 2.0*(apdph(5,jj)*act + bpdph(5,jj)*bct)
          enddo
       enddo loop_riflessi
   endif
!
   end subroutine fcalcadern_noanomal

!-----------------------------------------------------------------------

   subroutine fcalcadern_anomal(ref,atx,spg,elem,alambda,radtype,der,derft)
   USE trig_constants, only: twopi,pi
   USE spginfom
   USE elements
   USE atom_basic
   USE progtype, only: deratom
!
   type(reflection_type), dimension(:), intent(in)            :: ref  ! i riflessi
   type(atom_type), dimension(:), intent(in)                  :: atx  ! il modello strutturale
   type(spaceg_type), intent(in)                              :: spg
   type(element_type), dimension(:), intent(in)               :: elem
   real, intent(in)                                           :: alambda
   integer, intent(in)                                        :: radtype
   type(deratom), dimension(size(ref),size(atx)), intent(out) :: der
   real, dimension(size(ref)), optional                       :: derft
   real, dimension(4,spg%nsym)                                :: hkle
   integer, dimension(3)                                      :: i1
   integer                                                    :: natoms
   integer                                                    :: i,j,k,jj
   real                                                       :: rho
   integer                                                    :: kz
   real                                                       :: contc,contcd
   real                                                       :: conts,contsd
   real                                                       :: effe,effed
   real                                                       :: tfocc,tfoccd
   real                                                       :: tfocp,tfocpd
   real                                                       :: s,ss 
   real                                                       :: cc,c  
   real                                                       :: act,dct,cct,bct
   real                                                       :: fact,fdct,fbct,fcct
   real                                                       :: at,bt
   real                                                       :: hkltf,hkltfd
   real, dimension(5,size(atx))                               :: a_der,d_der,b_der,c_der
   real                                                       :: argg,act2,cct2
   real, dimension(size(elem))                                :: ff
   real                                                       :: alambda2, bisoder
!
!  |Fh|^2 = [A(x) - D(x)]^2 + [B(x) + C(x)]^2
!  d(|Fh|^2)/dx = 2[A(x)-D(x)][A'(x)-D'(x)] + 2[B(x)+C(x)][B'(x)+C'(x)]
!
!  Center of inversion: D(x) = 0, B(x) = 0 
!  d(|Fh|^2)/dx = 2*[A(x)A'(x) + C(x)C'(x)]
!
   natoms = size(atx)
   alambda2 = alambda*alambda
   if (spg%symcent == 1) then
       loop_riflessi1: do i=1,size(ref)
!      
!-- compute H*R and H*T
          i1 = ref(i)%hkl
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo
       
          rho=ref(i)%slaq
          !ss=ref(i)%slaq*(alambda**2)
          ss=ref(i)%slaq*(alambda2)
          s=sqrt(ss)
          cc=1.0-ss
          c=sqrt(cc)
          act=0.0
          cct=0.0
          fact=0.0
          fcct=0.0
          ff(:) = at_scatt(elem,rho,radtype) + elem(:)%f1
          loop_atomi1: do jj=1,natoms
             do k=1,5
                a_der(k,jj) = 0.0
                c_der(k,jj) = 0.0
             enddo
             kz = atx(jj)%kscatt()

             effe=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)
             effed=spg%ncoper*elem(kz)%f2*exp(-atx(jj)%biso*rho)
             tfocc=effe*atx(jj)%och*atx(jj)%ocry
             tfoccd=effed*atx(jj)%och*atx(jj)%ocry
             tfocp=tfocc*twopi
             tfocpd=tfoccd*twopi

             at=0.0
             do j=1,spg%nsym
                argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                at = at + cos(argg)
                do k=1,3
                   hkltf=hkle(k,j)*tfocp*2.0
                   hkltfd=hkle(k,j)*tfocpd*2.0
                   a_der(k,jj)=a_der(k,jj)-hkltf*sin(argg)
                   c_der(k,jj)=c_der(k,jj)-hkltfd*sin(argg)
                enddo
             enddo
             contc=2.0*at
             a_der(5,jj) = effe*contc*atx(jj)%ocry
             c_der(5,jj) = effed*contcd*atx(jj)%ocry
             contc=contc*tfocc
             contcd=contc*tfoccd
             act=act+contc
             cct=cct+contcd
             bisoder = (-2.0*c*s*atx(jj)%biso)/(alambda2)
             fact=fact+contc*bisoder
             fcct=fcct+contcd*bisoder
             !fact=fact+contc*(-2.0*c*s*atx(jj)%biso)/(alambda2)
             !fcct=fcct+contcd*(-2.0*c*s*atx(jj)%biso)/(alambda2)
             a_der(4,jj) = -rho*contc
             c_der(4,jj) = -rho*contcd
          enddo loop_atomi1
          act2 = 2.0*act
          cct2 = 2.0*cct
          if (present(derft)) then
              derft(i) = act2*fact + cct2*fcct
          endif
          do jj=1,natoms
!            d|Fh|/dx = 2*[A(x)A'(x) + C(x)C'(x)]
             der(i,jj)%co  = a_der(:3,jj)*act2 + c_der(:3,jj)*cct2
             der(i,jj)%b   = a_der(4,jj)*act2 + c_der(4,jj)*cct2
             der(i,jj)%occ = a_der(5,jj)*act2 + c_der(5,jj)*cct2
          enddo
       enddo loop_riflessi1
   else
       loop_riflessi: do i=1,size(ref)
!-- compute H*R and H*T
          i1 = ref(i)%hkl
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo
       
          rho=ref(i)%slaq
          ss=ref(i)%slaq*(alambda2)
          !ss=ref(i)%slaq*(alambda**2)
          s=sqrt(ss)
          cc=1.0-ss
          c=sqrt(cc)
          act=0.0
          dct=0.0
          bct=0.0
          cct=0.0
          fact=0.0
          fdct=0.0
          fbct=0.0
          fcct=0.0
          ff(:) = at_scatt(elem,rho,radtype) + elem(:)%f1
          loop_atomi: do jj=1,natoms
             do k=1,5
                a_der(k,jj) = 0.0
                d_der(k,jj) = 0.0
                b_der(k,jj) = 0.0
                c_der(k,jj) = 0.0
             enddo
             kz = atx(jj)%kscatt()

             effe=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)
             effed=spg%ncoper*elem(kz)%f2*exp(-atx(jj)%biso*rho)
             tfocc=effe*atx(jj)%och*atx(jj)%ocry
             tfoccd=effed*atx(jj)%och*atx(jj)%ocry
             tfocp=tfocc*twopi
             tfocpd=tfoccd*twopi

             at=0.0
             bt=0.0
             do j=1,spg%nsymop_prim
                argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                at = at + cos(argg)
                bt = bt + sin(argg)
                do k=1,3
                   hkltf=hkle(k,j)*tfocp
                   hkltfd=hkle(k,j)*tfocpd
                   a_der(k,jj)=a_der(k,jj)-hkltf*sin(argg)
                   d_der(k,jj)=d_der(k,jj)+hkltfd*cos(argg)
                   b_der(k,jj)=b_der(k,jj)+hkltf*cos(argg)
                   c_der(k,jj)=c_der(k,jj)-hkltfd*sin(argg)
                enddo
             enddo
             contc=at
             conts=bt
             a_der(5,jj) = effe*contc*atx(jj)%ocry
             d_der(5,jj) = effed*contsd*atx(jj)%ocry
             b_der(5,jj) = effe*conts*atx(jj)%ocry
             c_der(5,jj) = effed*contcd*atx(jj)%ocry
             contc=contc*tfocc
             contcd=contc*tfoccd
             conts=conts*tfocc
             contsd=conts*tfoccd
             act=act+contc
             dct=dct+contsd
             bct=bct+conts
             cct=cct+contcd
             bisoder = (-2.0*c*s*atx(jj)%biso)/(alambda2)
             fact=fact+contc *bisoder 
             fdct=fdct+contsd*bisoder
             fbct=fbct+conts *bisoder
             fcct=fcct+contcd*bisoder
             !fact=fact+contc *(-2.0*c*s*atx(jj)%biso)/(alambda2)
             !fdct=fdct+contsd*(-2.0*c*s*atx(jj)%biso)/(alambda2)
             !fbct=fbct+conts *(-2.0*c*s*atx(jj)%biso)/(alambda2)
             !fcct=fcct+contcd*(-2.0*c*s*atx(jj)%biso)/(alambda2)
             a_der(4,jj) = -rho*contc
             d_der(4,jj) = -rho*contsd
             b_der(4,jj) = -rho*conts
             c_der(4,jj) = -rho*contcd
          enddo loop_atomi
          if (present(derft)) then
              derft(i) = 2.0*((act - dct)*(fact - fdct) + (bct + cct)*(fbct - fcct))
          endif
          do jj=1,natoms
!            d(|Fh|^2)/dx = 2[A(x)-D(x)][A'(x)-D'(x)] + 2[B(x)+C(x)][B'(x)+C'(x)]
             der(i,jj)%co  = 2.0*((act - dct)*(a_der(:3,jj) - d_der(:3,jj)) +  &
                                  (bct + cct)*(b_der(:3,jj) + c_der(:3,jj)))
             der(i,jj)%b   = 2.0*((act - dct)*(a_der(4,jj) - d_der(4,jj)) +  &
                                  (bct + cct)*(b_der(4,jj) + c_der(4,jj)))
             der(i,jj)%occ = 2.0*((act - dct)*(a_der(5,jj) - d_der(5,jj)) +  &
                                  (bct + cct)*(b_der(5,jj) + c_der(5,jj)))
          enddo
       enddo loop_riflessi
   endif
!
   end subroutine fcalcadern_anomal

!-----------------------------------------------------------------------

   subroutine sfcalc(ref,atx,spg,elem,radtype,anomal)
   USE trig_constants
   USE spginfom
   USE elements
   USE atom_basic
!
   type(reflection_type), dimension(:), intent(inout) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)          :: atx    ! modello strutturale
   type(spaceg_type), intent(in)                      :: spg
   type(element_type), dimension(:), intent(in)       :: elem
   integer, intent(in)                                :: radtype
   logical, intent(in)                                :: anomal
!
   if (anomal) then
       call sfcalc_anomal(ref,atx,spg,elem,radtype)
   else
       call sfcalc_no_anomal(ref,atx,spg,elem,radtype)
   endif
!
   end subroutine sfcalc

!-----------------------------------------------------------------------

   subroutine sfcalc_no_anomal(ref,atx,spg,elem,radtype)
   USE trig_constants
   USE spginfom
   USE elements
   USE atom_basic
!
   type(reflection_type), dimension(:), intent(inout) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)          :: atx    ! modello strutturale
   type(spaceg_type), intent(in)                      :: spg
   type(element_type), dimension(:), intent(in)       :: elem
   integer, intent(in)                                :: radtype
   integer                                            :: natoms
   real, dimension(4,spg%nsym)                        :: hkle
   integer, dimension(3)                              :: i1
   integer                                            :: i,j,jj
   real                                               :: rho
   integer                                            :: kz
   real                                               :: contc
   real                                               :: conts
   real                                               :: tfocc
   real                                               :: freal,fimag
   real                                               :: faze
   real                                               :: at,bt
   real                                               :: argg
   real, dimension(size(elem)) :: ff

   natoms = size(atx)
   if (spg%symcent == 1) then
       loop_riflessi1: do i=1,size(ref)
          i1 = ref(i)%hkl
!--     compute H*R and H*T
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo
          rho=ref(i)%slaq
!      
          ff(:) = at_scatt(elem,rho,radtype)
!      
          freal=0.0
          loop_atomi1: do jj=1,natoms
             kz = atx(jj)%kscatt()
             tfocc=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
             at=0.0
             do j=1,spg%nsym
                argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                at = at + cos(argg)
             enddo
             freal=freal+at*tfocc
          enddo loop_atomi1
!      
          freal = 2.0*freal
          if (freal >= 0.0) then
              ref(i)%fc = freal
              ref(i)%ph = 0
          else
              ref(i)%fc = -freal
              ref(i)%ph = 180
          endif
          !write(71,'(4i5,2f10.3,i5)')i,ref(i)%hkl,freal,ref(i)%fc,ref(i)%ph
      enddo loop_riflessi1
   else
      loop_riflessi: do i=1,size(ref)
         i1 = ref(i)%hkl
         do j=1,spg%nsym
            hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
            hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
         enddo
         rho=ref(i)%slaq
!     
         ff(:) = at_scatt(elem,rho,radtype)
!     
         freal=0.0
         fimag=0.0
         loop_atomi: do jj=1,natoms
            kz = atx(jj)%kscatt()
            tfocc=spg%ncoper*ff(kz)*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
            at=0.0
            bt=0.0
            do j=1,spg%nsym
               argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
               at = at + cos(argg)
               bt = bt + sin(argg)
            enddo
            contc=at
            conts=bt
            freal=freal+contc*tfocc
            fimag=fimag+conts*tfocc
         enddo loop_atomi
!     
         ref(i)%fc = sqrt(freal**2+fimag**2)
         faze=rtod*atan2(fimag,freal)+360.0
         ref(i)%ph = mod(int(faze+0.5),360)
         !write(71,'(4i5,3f10.3,i5)')i,ref(i)%hkl,freal,fimag,ref(i)%fc,ref(i)%ph
      enddo loop_riflessi
   endif
!
   end subroutine sfcalc_no_anomal

!-----------------------------------------------------------------------

   subroutine sfcalc_anomal(ref,atx,spg,elem,radtype)
   USE trig_constants
   USE spginfom
   USE elements
   USE atom_basic
!
   type(reflection_type), dimension(:), intent(inout) :: ref    ! i riflessi
   type(atom_type), dimension(:), intent(in)          :: atx    ! modello strutturale
   type(spaceg_type), intent(in)                      :: spg
   type(element_type), dimension(:), intent(in)       :: elem
   integer, intent(in)                                :: radtype
   integer                                            :: natoms
   real, dimension(4,spg%nsym)                        :: hkle
   integer, dimension(3)                              :: i1
   integer                                            :: i,j,jj
   real                                               :: rho
   integer                                            :: kz
   real                                               :: tconst,tfocc,tfoccd
   real                                               :: freal,fimag
   real                                               :: freal1,fimag1,freal2,fimag2
   real                                               :: faze
   real                                               :: at,bt
   real                                               :: argg
   real, dimension(size(elem))                        :: ff

   natoms = size(atx)
   if (spg%symcent == 1) then
       loop_riflessi1: do i=1,size(ref)
          i1 = ref(i)%hkl
!--     compute H*R and H*T
          do j=1,spg%nsym
             hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
             hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
          enddo
          rho=ref(i)%slaq
!      
          ff(:) = at_scatt(elem,rho,radtype) + elem(:)%f1
!      
          freal=0.0
          fimag=0.0
          loop_atomi1: do jj=1,natoms
             kz = atx(jj)%kscatt()
             tconst=spg%ncoper*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
             tfocc=tconst*ff(kz)
             tfoccd=tconst*elem(kz)%f2
             at=0.0
             do j=1,spg%nsym
                argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
                at = at + cos(argg)
             enddo
             freal=freal+at*tfocc
             fimag=fimag+at*tfoccd
          enddo loop_atomi1
!      
          freal = 2*freal
          fimag = 2*fimag
          ref(i)%fc = sqrt(freal**2 + fimag**2)
          faze=rtod*atan2(fimag,freal)+360.0
          ref(i)%ph = mod(int(faze+0.5),360)
!corr          if (freal >= 0.0) then  ! assuming f2 << f0 + f1
!corr              ref(i)%ph = 0
!corr          else
!corr              ref(i)%ph = 180
!corr          endif
          !write(71,'(4i5,3f10.3,2i5)')i,ref(i)%hkl,freal,fimag,ref(i)%fc,ref(i)%ph,ref(i)%m
      enddo loop_riflessi1
   else
      loop_riflessi: do i=1,size(ref)
         i1 = ref(i)%hkl
         do j=1,spg%nsym
            hkle(4,j) = dot_product(i1,spg%symop(j)%trn(:)) ! H*T
            hkle(:3,j) = matmul(i1,spg%symop(j)%rot(:,:))   ! H*R
         enddo
         rho=ref(i)%slaq
!     
         ff(:) = at_scatt(elem,rho,radtype) + elem(:)%f1
!     
         freal1=0.0
         fimag1=0.0
         freal2=0.0
         fimag2=0.0
         loop_atomi: do jj=1,natoms
            kz = atx(jj)%kscatt()
            tconst=spg%ncoper*exp(-atx(jj)%biso*rho)*atx(jj)%och*atx(jj)%ocry
            tfocc=tconst*ff(kz)
            tfoccd=tconst*elem(kz)%f2
            at=0.0
            bt=0.0
            do j=1,spg%nsym
               argg = twopi*(dot_product(hkle(:3,j),atx(jj)%xc) + hkle(4,j))
               at = at + cos(argg)
               bt = bt + sin(argg)
            enddo
!corr            contc=at
!corr            conts=bt
            freal1=freal1+at*tfocc
            freal2=freal2+bt*tfoccd
            fimag1=fimag1+bt*tfocc
            fimag2=fimag2+at*tfoccd
         enddo loop_atomi
!     
         freal = freal1 - freal2
         fimag = fimag1 + fimag2
         ref(i)%fc = sqrt(freal**2+fimag**2)
         faze=rtod*atan2(fimag,freal)+360.0
         ref(i)%ph = mod(int(faze+0.5),360)
         !write(71,'(4i5,3f12.5,2i5)')i,ref(i)%hkl,freal,fimag,ref(i)%fc,ref(i)%ph,ref(i)%m
      enddo loop_riflessi
   endif
!
   end subroutine sfcalc_anomal

!---------------------------------------------------------------------------------------

   subroutine fcalcang(refl,atom,spg,elem,radtype,anomal,anis)
!
!  Calcola fattori di struttura in presenza di ghost atoms
!
   USE atom_type_util
   USE spginfom
   USE elements
   type(reflection_type), dimension(:), intent(inout)        :: refl
   type(atom_type), dimension(:), allocatable, intent(in)    :: atom
   type(spaceg_type), intent(in)                             :: spg
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   type(atom_type), dimension(:), allocatable                :: atom1
   integer, intent(in)                                       :: radtype
   logical, intent(in)                                       :: anomal
   logical, intent(in), optional                             :: anis
   logical                                                   :: banis
!
   banis = .false.
   if (present(anis)) banis = anis
   if (any(atom(:)%get_nz() == 0)) then    ! ci sono ghost atoms?
       call copy_atoms(atom1,atom)
!
!      rimuovi ghosts
       call delete_ghosts(atom1)
!
       if (numatoms(atom1) > 0) then
           if (banis) then
               call sfcalc_anis(refl,atom1,spg,elem,radtype,anomal)
           else
               call sfcalc(refl,atom1,spg,elem,radtype,anomal)
           endif
       else                     ! caso limite: ho solo ghost atoms
           refl(:)%fc = 0.
           refl(:)%ph = 0
       endif
   else
       if (banis) then
           call sfcalc_anis(refl,atom,spg,elem,radtype,anomal)
       else
           call sfcalc(refl,atom,spg,elem,radtype,anomal)
       endif
   endif
!
   end subroutine fcalcang

!--------------------------------------------------------------------

   subroutine calcola_correzioneOP(refl,gmax,jplane,cell,spg)
   USE CGEOM
   USE unit_cell
   USE spginfom
   type(reflection_type), dimension(:), intent(inout) :: refl
   real, intent(in)                                   :: gmax
   integer, dimension(3), intent(in)                  :: jplane
   type(cell_type), intent(in)                        :: cell
   type(spaceg_type), intent(in)                      :: spg
   integer                                            :: i,l
   integer, dimension(3)                              :: jh1,jh2
   integer                                            :: numb
   integer                                            :: mmk
   real                                               :: angolo,sinq,cosq
   real                                               :: pref
   real                                               :: sumden,denom
   real, dimension(3,3)                               :: gr
!
   numb = size(refl)
   gr = cell%get_r()
!
   LOOP_RIFLESSI : do i = 1 , numb
      jh1(:)=refl(i)%hkl 
      mmk = 0
      sumden = 0
      do l=1,spg%nsym  ! loop per riflessi equivalenti
         jh2(:) = matmul(jh1,spg%symop(l)%rot)
         angolo = angle_vectors(jh2,jplane,gr) * dtor
         cosq =  cos ( angolo )
         sinq =  sin ( angolo )
         pref=(gmax**2)*(cosq**2)+(sinq**2)/gmax
         if (pref < 0.0) cycle     ! puo verificarsi che pref sia < 0 e pref**(-1.5) = INDEFINITO
         sumden = sumden + pref**(-1.5)
         mmk = mmk + 1
      enddo
      if (mmk > 0)then
          denom = sumden/mmk
      else
          denom=1.0
      endif
      refl(i)%po = denom
   enddo LOOP_RIFLESSI
!
   end subroutine calcola_correzioneOP

!--------------------------------------------------------------------

   subroutine calcola_derivataOP(refl,gmax,jplane,cell,spg,derOP)
   USE CGEOM
   USE unit_cell
   USE trig_constants
   USE spginfom
   type(reflection_type), dimension(:), intent(inout) :: refl
   real, intent(in)                                   :: gmax
   integer, dimension(3), intent(in)                  :: jplane
   type(cell_type), intent(in)                        :: cell
   type(spaceg_type), intent(in)                      :: spg
   real, dimension(:), intent(out)            :: derOP
   integer                :: i,l
   integer, dimension(3)  :: jh1,jh2   !!!!,jplane
   integer                :: mmk
!   real                   :: gmax
   real                   :: angolo,sinq,cosq
   real                   :: pref,preff
   real                   :: sumden,denom
   real, dimension(3,3) :: gr
!
   derOP(:)=0.0
   gr = cell%get_r()
   LOOP_RIFLESSI : do i=1,size(derOP) 
      jh1(:)=refl(i)%hkl 
      mmk = 0
      sumden = 0
      do l=1,spg%nsym  ! loop per riflessi equivalenti
         jh2(:) = matmul(jh1,spg%symop(l)%rot)
         angolo = angle_vectors(jh2,jplane,gr) * dtor
         cosq =  cos ( angolo )
         sinq =  sin ( angolo )
         pref=(gmax**2)*(cosq**2)+(sinq**2)/gmax
         if (pref < 0.0) cycle     ! puo verificarsi che pref sia < 0 e pref**(-1.5) = INDEFINITO
         preff=2.0*gmax*(cosq**2)-(sinq**2)/(gmax**2)
         sumden = sumden + pref**(-1.5)
!
         derOP(i) = derOP(i) + preff*(pref**(-2.5))
!
         mmk = mmk + 1
      enddo
      denom=1.0
      if (mmk > 0)then
          denom = sumden/mmk
          derOP(i)=(-1.5/mmk)*derOP(i)
      endif
      refl(i)%po = denom
   enddo LOOP_RIFLESSI
!
   end subroutine calcola_derivataOP

!--------------------------------------------------------------------

   real elemental function get_mcorr(ref,kwave)
!
!  Compute m*corr to apply to F squared to obtain intensity
!
   type(reflection_type), intent(in) :: ref
   integer, intent(in)               :: kwave
!
   get_mcorr = ref%m*ref%po*ref%lp(kwave)*ref%ab(kwave)
!
   end function get_mcorr

!--------------------------------------------------------------------

   subroutine get_norm(ref,spg,elem,bt,scal2,radtype,eo,ec)
!
!  Compute normalized structure factors
!
   use elements
   use spginfom
   type(reflection_type), dimension(:), intent(in)           :: ref
   type(spaceg_type), intent(in)                             :: spg
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   real, intent(in)                                          :: bt
   real, intent(in)                                          :: scal2 ! scale on F squared
   integer, intent(in)                                       :: radtype
   real, dimension(size(ref)), intent(out), optional         :: eo,ec
   integer                                                   :: i,j
   real, dimension(size(ref))                                :: eden
   integer                                                   :: icent,multi
   real                                                      :: eps,sumff,scal
   integer, dimension(spg%nsymop,3,3)                        :: kmat
!
   if (.not.present(eo) .and. .not.present(ec)) return
   if (spg%symcent == 1) then
       icent = 1
   else
       icent = 0
   endif
   do i=1,spg%nsymop
      kmat(i,:,:) = spg%symop(i)%rot
   enddo
   do i=1,size(ref)
      call calcola_mult_new(ref(i)%hkl,spg%nsym,icent,eps,multi,kmat,POWDER_EXP)  ! compute epsilon
      sumff = 0
      do j=1,numelem(elem)
         sumff = sumff + elem(j)%nw*at_scatt(elem(j),ref(i)%slaq,radtype)**2
      enddo
      eden(i) = sqrt(spg%ncoper*(exp(-2.0*bt*ref(i)%slaq))*sumff*eps)
   enddo
   if (present(eo)) then
       eo=ref%fo/eden
       if (scal2 > 0) then
           scal = scal2
       else
           scal = sum(ref%m*eo**2) / sum(ref%m)
       endif
       eo = eo / sqrt(scal)
   endif
   if (present(ec)) ec=ref%fc/eden
!
   end subroutine get_norm

!--------------------------------------------------------------------

   subroutine transform_reflections(ref,pmat)
!
!  (h',k',l') = (h,k,l) P
!
   use unit_cell
   type(reflection_type), dimension(:), allocatable, intent(inout) :: ref
   real, dimension(3,3), intent(in)                                :: pmat
   integer                                                         :: i
!
   do i=1,numrefl(ref)
      ref(i)%hkl = nint(matmul(ref(i)%hkl,pmat))
   enddo
!
   end subroutine transform_reflections

END MODULE reflection_type_util   
