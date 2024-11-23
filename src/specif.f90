      subroutine MsgWinErr(String,mesg,nType,iExitC)
      USE General, only: lo, ifAutomatic
      USE prog_constants, only: SEVERE_ERR_WINDOW, QUEST_WINDOW_NO_YES
      USE Molcom, only: jscreen
      USE prognames, only: package_alt_name
      USE strutil, only: Lung,write_formatted_message,f_to_c
      USE mpi_prog, only: mpi_prog_rank
      implicit none
      interface
         subroutine MsgWinErrC(inte, msg, tipo, exitC) bind(C,name="MsgWinErrC1")
         use iso_c_binding
         character(c_char), intent(in)     :: inte(*), msg(*)
         integer(c_int), intent(in), value :: tipo
         integer(c_int), intent(out)       :: exitC
         end subroutine MsgWinErrC
      end interface 
!
      character*(*), intent(in)     :: String, mesg
      integer, intent(in)           :: nType
      integer, intent(out)          :: iExitC
      integer                       :: kf
      integer                       :: l2
      character(len=:), allocatable :: Inte, Msg
!
      l2 = min0(Lung(mesg), 240)
      if (l2.gt.0) then
          Msg = mesg(1:l2)
      else
          Msg = ' '
      endif
!
      kf = Lung(Msg)
      if (nType.eq.SEVERE_ERR_WINDOW) then
          Msg(kf+1:) = char(10)//package_alt_name//' will be terminated!!!'
      endif

      if (mpi_prog_rank == 0) then
          l2 = min0(Lung(String), 80)
          if (l2.gt.0) then
              Inte = String(1:l2)
          else
              Inte = ' '
          endif
          call write_formatted_message(lo,Inte,Msg,74)
      endif
      
! ---
      if (jscreen > 0 .and. ifAutomatic == 0) then
          call MsgWinErrC(f_to_c(String), f_to_c(mesg), nType, iExitC)
      else
          if (nType == QUEST_WINDOW_NO_YES) then
              iExitC = 0
          else
              iExitC = 1
          endif
      endif
!
      return
      end subroutine MsgWinErr
