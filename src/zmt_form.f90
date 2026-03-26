module zmt_frm

implicit none

contains
   
   subroutine read_zmtfile(filename,cell,atom,error)
!   
!  Legge Fenske-Hall zmatrix
!  Generica riga i: 
!    label(i) Conn(1,i) I_coor(1,i) Conn(2,i) I_coor(2,i) Conn(3,i) I_coor(3,i)
!
   USE unit_cell
   USE atom_type_util
   USE strutil
   USE errormod
   USE elements
   USE cgeom
   USE fileutil
   character(len=*), intent(in)                              :: filename
   type(cell_type), intent(in)                               :: cell
   type(atom_type), allocatable, dimension(:), intent(inout) :: atom    ! atomi in uscita
   real, dimension(:,:), allocatable                         :: I_coor  ! Internal coordinates (d,ang,tors)
   integer, dimension(:,:), allocatable                      :: Conn    ! interi in colonna 2,4,6
   integer                                                   :: natom   ! numero di righe = num. di atomi
   logical                                                   :: kpr = .false.
   integer                                                   :: nline
   type(error_type), intent(out)                             :: error 
   character(len=500)                                        :: line1,line2
   integer                                                   :: nlong1,nlong2
   integer                                                   :: i
   real, dimension(50)                                       :: vet
   integer, dimension(50)                                    :: ivet
   integer                                                   :: iv
   character(len=10)                                         :: strn
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
!  La prima riga e' vuota
   nline = 1
   read(j_in,*,err=10)
!   
!  La seconda riga contiene il numero di righe da leggere
   nline = nline + 1
   read(j_in,*,err=10)natom
   if (natom <= 0) then
       call error%set('Error in zmt file: not atoms are defined')
       go to 20
   endif        
   allocate(atom(natom),Conn(3,natom),I_coor(3,natom))
   Conn(:,:) = 0
   I_coor(:,:) = 0.0
!   
   do i=1,natom
      nline = nline + 1
      read(j_in,'(a)',err=10)line1
!      
!     Leggi la specie sulla prima colonna
      call cutst(line1,nlong1,line2,nlong2)
      if (nlong2 > 2) go to 10
      atom(i)%lab = line2(:nlong2)
!      
!     Leggi il resto della riga
      call Getnum(line1,vet,ivet,iv)
      select case (i) 
         case (1)       ! prima riga
           if(iv < 1) go to 10        
           Conn(1,i) = ivet(1)
           if (kpr) write(0,'(a,1x,i4)')atom(i)%lab,Conn(1,i)

         case (2)       ! seconda riga
           if(iv < 2) go to 10        
           Conn(1,i) = ivet(1)
           I_coor(1,i) = vet(2)
           if (kpr) write(0,'(a,1x,i4,1x,f10.4)')atom(i)%lab,Conn(1,i),I_coor(1,i)

         case (3)       ! terza riga      
           if(iv < 4) go to 10        
           Conn(1,i) = ivet(1)
           I_coor(1,i) = vet(2)
           Conn(2,i) = ivet(3)
           I_coor(2,i) = vet(4)
           if (kpr) write(0,'(a,2(1x,i4,1x,f10.4))')atom(i)%lab,Conn(1,i),I_coor(1,i), &
                    Conn(2,i),I_coor(2,i)

         case (4:)      ! le altre righe  
           if(iv < 6) go to 10        
           Conn(1,i) = ivet(1)
           I_coor(1,i) = vet(2)
           Conn(2,i) = ivet(3)
           I_coor(2,i) = vet(4)
           Conn(3,i) = ivet(5)
!corr           I_coor(3,i) = dangle_norm(vet(6))
   ! cosi si corregge errore segnalato da Akihiro
           I_coor(3,i) = -dangle_norm(vet(6))
           if (kpr) write(0,'(a,3(1x,i4,1x,f10.4))')atom(i)%lab,Conn(1,i),I_coor(1,i),  &
                    Conn(2,i),I_coor(2,i),Conn(3,i),I_coor(3,i)

      end select
   enddo
!
!  Converti coordinate interne in coordinate cartesiane
   call Zmatrix_to_Cartesian(natom,I_coor,conn)
   if (kpr) then
       do i=1,natom
          write(0,'(i4,3f10.3,3i6)')i,I_coor(:3,i),conn(:3,i)
       enddo
   endif        
!   
!  Trasferisci in atom le coordinate cartesiane
   do i=1,natom
      atom(i)%xc = I_coor(:3,i)
   enddo
!
!  Converti in coordinate cristallografiche
   call cart_to_frac(atom,cell%get_ortoi())
!
!  Metti in nz il puntatore al file xen
   do i=1,natom
      atom(i)%ptab = pxen_from_specie(atom(i)%lab)
   enddo
! 
   return
!   
10 write(strn,'(i0)')nline
   call error%set('Error on reading line '//trim(strn)//' of zmt file')   
!
20 continue
!   
   end subroutine read_zmtfile

!-------------------------------------------------------------------------------------------------

   subroutine create_zmtfile(filename,atom,legm,cell)
!
!  Crea un file z-matric secondo lo standard stan
!
   USE atom_type_util
   USE connect_mod
   USE fileutil
   USE cgeom
   USE unit_cell
   character(len=*), intent(in)                           :: filename  ! se presente apre il file
   type(atom_type), dimension(:), intent(in)              :: atom      ! atomi in uscita
   type(bond_type), dimension(:), allocatable, intent(in) :: legm      ! atomi in uscita
   type(cell_type), intent(in)                            :: cell      ! cella elementare   
   type(atom_type), dimension(size(atom))                 :: atomc     ! atomi in uscita   
   type(bond_type), dimension(:), allocatable             :: legmc 
   integer                                                :: j_in,i
   real, dimension(3,size(atom))                          :: I_coor 
   integer, dimension(3,size(atom))                       :: conn 
   integer                                                :: nat
   type(file_handle)                                      :: fz
!
!  Apertura del file
   call fz%fopen(filename,'w')
   j_in = fz%handle()
!
   call copy_bonds(legmc,legm)
!
   nat = size(atom)
!
!  Connetti forzatamente atomi isolati
   do i=1,nat
      if (number_of_bonds(legmc,i) == 0) then
          call force_connectivity(atom,legmc,i,cell)
      endif
   enddo
!
!  Converti in coord. cartesiane
   atomc(:) = atom
   call frac_to_cart(atomc,orthomatrix_std(cell%get_par()))
!
   call cartesian_to_zmatrix(atomc,legmc,I_coor,conn)

   write(j_in,'(/i0)')nat
   write(j_in,'(a,2x,i6)')atomc(1)%specie(),conn(1,1)
   if (nat > 1) then
       write(j_in,'(a,2x,i6,f8.3)')atomc(2)%specie(),conn(1,2),I_coor(1,2)
       if (nat > 2) write(j_in,'(a,2x,i6,f8.3,i6,f10.3)')atomc(3)%specie(),conn(1,3),I_coor(1,3),conn(2,3),I_coor(2,3)
   endif
   do i=4,nat
      if (I_coor(3,i) < 0) I_coor(3,i) = 360 + I_coor(3,i) ! normalizzazione tra 0-360
      write(j_in,'(a,2x,i6,f8.3,i6,f10.3,i6,f10.1)')atomc(i)%specie(),conn(1,i),   &
      I_coor(1,i),conn(2,i),I_coor(2,i),conn(3,i),I_coor(3,i)
   enddo
!
   call fz%fclose()
!
   end subroutine create_zmtfile

end module zmt_frm
