module gamess_frm

implicit none

contains

   subroutine write_gmfile(filename,atom,cell,structname)
   USE unit_cell
   USE atom_type_util
   USE fileutil
   USE elements
   character(len=*), intent(in), optional                 :: filename
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(cell_type), intent(in)                            :: cell
   character(len=*), intent(in)                           :: structname
   type(file_handle)                                      :: fgm
   integer                                                :: j_in,i
!corr   integer                                                :: z
!
   call fgm%fopen(filename,'w')
   if (.not.fgm%good()) return

   j_in = fgm%handle()
   write(j_in,'(a)')'! File created for GAMESS'
!
   write(j_in,'(a)') ' $SYSTEM MWORDS=20 $END'
   write(j_in,'(a)') ' $CONTRL RUNTYP=Optimize $END'
   write(j_in,'(a)') ' $STATPT  OptTol=1e-5 NStep=500 $END'
   write(j_in,'(a)') ' $CONTRL SCFTYP=RHF $END'
   write(j_in,'(a)') ' $DFT  DFTTYP=B3LYP $END'            !Optimize geometry with DFT and B3LYP
   write(j_in,'(a)') ' $CONTRL ICHARG=0  MULT=1 $END'
   write(j_in,'(a/)')' $BASIS GBASIS=N31 NGAUSS=6 $END'    !Basis set: 6-31G
!
!  Geometry
   write(j_in,'(a)')' $DATA'
   write(j_in,'(a)')trim(structname)
   write(j_in,'(a)')'C1'
   do i=1,numatoms(atom)
      if (atom(i)%z() > 0) write(j_in,'(a,1x,f5.1,3f15.5)')atom(i)%lab(1:10), &
                                 real(atom(i)%z()),xyz_cart(atom(i),cell)
   enddo
   write(j_in,'(a)')' $END'
   call fgm%fclose()
!
   end subroutine write_gmfile

end module gamess_frm
