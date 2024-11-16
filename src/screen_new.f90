MODULE enable_amb

    implicit none

    enum, bind(c)
      enumerator :: STOP_EVENT=1, SKIP_EVENT, CYCLE_EVENT
    endenum

    enum, bind(c)
      enumerator :: InitAction, PatternAction, StructureAction,  &
                    PatternAndStructureAction, DialogOpenAction, &
                    RunAction, RunSkipAction, NextAction,        & 
                    PhaseAction, SaveAction, RestoreAction,      &
                    IntervalsAction, ExtraAction,                &
                    ExtraBackgroundAction, ProfileAction,        & 
                    CycleAction, PeaksAction
    endenum

CONTAINS 

    subroutine abilita_tasti(kaction,state)
    USE molcom, only: jscreen
    integer, intent(in)  :: kaction
    integer, intent(in), optional :: state   ! 0=set no sensitive, 1=sensitive, 2=hide
    integer                       :: statet
    interface
       subroutine c_enableActions(kaction, state) bind(C,name="c_enableActions")
       use iso_c_binding
       integer(c_int), intent(in), value :: kaction
       integer(c_int), intent(in), value :: state
       end subroutine c_enableActions
    end interface
!
    if (jscreen  > 0) then
        if (present(state)) then
            statet = state
        else
            statet = 1
        endif

        call c_enableActions(kaction,statet)
    endif

    end subroutine abilita_tasti
 
!----------------------------------------------------------------------------------------------------

   integer function event_from_gui() result(event)
   USE Molcom, only: jscreen, ifBrekke
!
   if (jscreen > 0 .and. ifBrekke /= 0) then
       select case (ifBrekke)
          case (SKIP_EVENT)  
            event = ifBrekke
          case (STOP_EVENT) 
            event = ifBrekke
          case (CYCLE_EVENT)
            event = ifBrekke
          case default
            event = 0
       end select
       ifBrekke = 0
   else
       event = 0
   endif
!
   end  function event_from_gui

END MODULE enable_amb
