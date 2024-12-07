MODULE datamod

   implicit none

CONTAINS

   subroutine open_diffraction_patt(fileIn,lengthIn,fileOut,lengthOut,add_data,ier) bind(C,name="open_diffraction_patt")
   USE iso_c_binding, only: c_char, c_int
   USE strutil
   character(kind=c_char), intent(in) :: fileIn(*)
   integer(c_int), intent(in), value  :: lengthIn
   character(kind=c_char), intent(in) :: fileOut(*)
   integer(c_int), intent(in), value  :: lengthOut
   integer(c_int), intent(in), value  :: add_data
   integer(c_int), intent(out)        :: ier
   character(len=:), allocatable      :: filnam,filout
!
   ier = 0
   filnam = toFortranString(fileIn,lengthIn)
   filout = toFortranString(fileOut,lengthOut)
   call open_pattern(filnam,filout,add_data,ier)
!     
   end subroutine open_diffraction_patt

!---------------------------------------------------------------------------

   subroutine open_pattern(input_file,output_file,add_data,ier)
   USE fileutil
!   USE General, only: StructureName, fname, ext, INP_FILE, OUT_FILE
   USE datasetmod
   USE datautil
!   USE molcom, only:ifProject
   USE strutil
!   USE General, only:lo
   USE variables, only: dataset   !,cryst
   character(len=*), intent(in) :: input_file,output_file
   integer, intent(in), value   :: add_data
   integer, intent(out)         :: ier
   integer                      :: iflag
   integer                      :: itype, iform, nlenn, jop, jopen, ier2
   integer                      :: i,ierc
   type(dataset_type)           :: datas
!
   ier = 0
   call load_datafile(datas,input_file,.true.,ier)
   if (ier == 0) then
       if (add_data == 0) then
!           if (ifProject.ne.0) then   ! importante se esegui run successivi
!               iflag = 2
!!              comment the next line to add pattern
!               call chiudi_bin(iflag) ! call InitExpo
!           endif
!           StructureName = file_rem_ext(file_get_name(input_file))
!           call cryst(1)%set_name(StructureName)
!!
!           do i=1,size(fname)
!              if (i /= INP_FILE .and. i /= OUT_FILE) then
!                  fname(i) = s_blank_delete(trim(StructureName)//ext(i))
!              endif
!           enddo
!           fname(INP_FILE) = input_file
!           fname(OUT_FILE) = output_file
!!
!           close(lo,iostat=ierc)
!           itype = 0
!           iform = 1
!           nlenn = 80
!           jop = jopen(lo,fname(OUT_FILE),itype,iform,ier2)
!!
           call clear_dataset(dataset)
       endif
       call push_back_dataset(dataset,datas)
       call work_on_pattern(dataset(1),add_data)
   endif
!
   end subroutine open_pattern

END MODULE datamod

!---------------------------------------------------------------------------

   subroutine work_on_pattern(datas,add_data)
!   USE Conteggi, only: npunti
   USE view, only: vedinew
   USE messagemod
!   USE enable_amb
   USE datasetmod, only: dataset_type
   USE molcom, only: jscreen
!   USE datamod, only: abilita_tasti2
   implicit none
   type(dataset_type), intent(in) :: datas
   integer, intent(in)            :: add_data
!
!   if(add_data == 0) call dataset_to_expo(datas)

   if (jscreen > 0) then
       call init_messages()
!       call write_message_pattern(datas%fname)
       call vedinew(8,1,npunti)
!       call abilita_tasti2()
   endif

!   if (add_data == 0) call init_dati()
!
   end subroutine work_on_pattern

!---------------------------------------------------------------------------

