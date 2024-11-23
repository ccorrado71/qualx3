MODULE mpi_prog
   USE errormod

   implicit none

   integer, parameter :: mpi_prog_master = 0   ! rank of master process 
   integer            :: mpi_prog_rank = 0     ! rank of the process
   integer            :: mpi_prog_numprocs = 1 ! number of process
   type(error_type)   :: mpi_prog_err          ! error in MPI

#ifdef MPI
private :: gather_data_sl, gather_data_sr
interface gather_data
   module procedure gather_data_sl, gather_data_sr
end interface

private :: get_data_si, get_data_vi, get_data_sl, get_data_vr
interface get_data
   module procedure get_data_si, get_data_vi, get_data_sl, get_data_vr
end interface

CONTAINS 

   subroutine mpi_prog_init()
   USE mpi
   integer :: mpi_err
!
!  Initialize the MPI system
   call MPI_Init(mpi_err)
   if (mpi_err /= 0) then
       call mpi_prog_err%set('MPI SYSTEM could not be initialized',code=mpi_err)
       return
   endif
!
!  Get the rank of this process, store in mpi_prog_rank
   call MPI_Comm_rank(MPI_COMM_WORLD, mpi_prog_rank, mpi_err)
   if (mpi_err /= 0) then
       call mpi_prog_err%set('MPI SYSTEM did not return RANK',code=mpi_err)
       return
   endif
!
!  Get the number of processes, store in mpi_prog_numprocs
   call MPI_Comm_size(MPI_COMM_WORLD, mpi_prog_numprocs, mpi_err)
   if (mpi_err /= 0) then
       call mpi_prog_err%set('MPI SYSTEM did not return SIZE',code=mpi_err)
       return
   endif

   end subroutine mpi_prog_init

!----------------------------------------------------------------------------------------------

   integer function mpi_prog_ntrials(ntot)  result(ntrials)
!
!  Compute the number of trials for each process. Distribute the remainder
!
   integer, intent(in) :: ntot ! total number of trials
!
   if (mpi_prog_numprocs < ntot) then
       ntrials = ntot / mpi_prog_numprocs
!
!      balance remainder
       if (mpi_prog_rank < mod(ntot,mpi_prog_numprocs)) then
           ntrials = ntrials + 1
       endif
   else
       ntrials = 1
   endif
!
   end function mpi_prog_ntrials

!----------------------------------------------------------------------------------------------

   subroutine mpi_prog_print(kpr)
   integer, intent(in) :: kpr
!
   write(kpr,'(a,i0)')'Rank of this process: ',mpi_prog_rank
   write(kpr,'(a,i0)')'Number of processes:  ',mpi_prog_numprocs
!
   end subroutine mpi_prog_print

!----------------------------------------------------------------------------------------------

   subroutine gather_data_sr(myval,vector)
!
!  Every processes send logical myval to the master
!
   USE mpi
   real, intent(in)                :: myval
   real, dimension(:), intent(out) :: vector
   integer                         :: mpi_err
!
   call MPI_gather(myval,1,MPI_REAL,vector,1,MPI_REAL,0,MPI_COMM_WORLD,mpi_err)
!
   end subroutine gather_data_sr

!----------------------------------------------------------------------------------------------

   subroutine gather_data_sl(myval,vector)
!
!  Every processes send logical myval to the master
!
   USE mpi
   logical, intent(in)                :: myval
   logical, dimension(:), intent(out) :: vector
   integer                            :: mpi_err
!
   call MPI_gather(myval,1,MPI_LOGICAL,vector,1,MPI_LOGICAL,0,MPI_COMM_WORLD,mpi_err)
!
   end subroutine gather_data_sl

!----------------------------------------------------------------------------------------------

   subroutine get_data_si(val)
!
!  Master send integer to all processes
!
   USE mpi
   integer :: val
   integer :: mpi_err
!
   call MPI_bcast(val,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpi_err)
!
   end subroutine get_data_si

!----------------------------------------------------------------------------------------------

   subroutine get_data_vi(vec)
!
!  Master send array vec to all processes
!
   USE mpi
   integer, dimension(:) :: vec
   integer               :: mpi_err
!
   call MPI_bcast(vec,size(vec),MPI_INTEGER,0,MPI_COMM_WORLD,mpi_err)
!
   end subroutine get_data_vi

!----------------------------------------------------------------------------------------------

   subroutine get_data_sl(val)
!
!  Master send integer to all processes
!
   USE mpi
   logical :: val
   integer :: mpi_err
!
   call MPI_bcast(val,1,MPI_LOGICAL,0,MPI_COMM_WORLD,mpi_err)
!
   end subroutine get_data_sl

!----------------------------------------------------------------------------------------------

   subroutine get_data_vr(vec)
!
!  Master send array vec to all processes
!
   USE mpi
   real, dimension(:) :: vec
   integer            :: mpi_err
!
   call MPI_bcast(vec,size(vec),MPI_REAL,0,MPI_COMM_WORLD,mpi_err)
!
   end subroutine get_data_vr

!----------------------------------------------------------------------------------------------

   subroutine send_minimum_to_master(val, valarray, min_val, min_valarray, kpr)
!
!  This function send the minumum value of real val and the corrisponding array to task 0
!
   USE mpi
   real, intent(in)                :: val
   real, dimension(:), intent(in)  :: valarray
   real, intent(out)               :: min_val
   real, dimension(size(valarray)) :: min_valarray
   integer, intent(in)             :: kpr
   real, dimension(2,1)            :: indata
   real, dimension(2,1)            :: outdata
   integer                         :: mpi_err
   integer                         :: min_rank
   integer                         :: tag, status(MPI_STATUS_SIZE)
!
!  All tasks will receive the minimum and its location
   indata(1,1) = val
   indata(2,1) = real(mpi_prog_rank)
   call MPI_Allreduce(indata, outdata, 1, MPI_2REAL, MPI_MINLOC, MPI_COMM_WORLD, mpi_err)
   min_val = outdata(1,1)         ! minimum value
   min_rank = nint(outdata(2,1))  ! rank that has the minimum value
   if (kpr > 0) then
       if (mpi_prog_rank == min_rank) then
           write(kpr,'(a,i4,a,f10.3)') 'Rank: ', mpi_prog_rank, ' I have the minimum: ',min_val
       else
           write(kpr,'(a,i4,a,f10.3)') 'Rank: ', mpi_prog_rank, ' I have the minimum too: ',min_val
       endif
   endif
!
!  If this process has the minimum value of val, send its valarray to process 0
   if (min_rank /= mpi_prog_master) then
       if (mpi_prog_rank == min_rank) then
           tag = 1
           call MPI_Send(valarray, size(valarray), MPI_REAL, mpi_prog_master, tag, MPI_COMM_WORLD, mpi_err)
       endif

       if (mpi_prog_rank == mpi_prog_master) then
           tag = 1
           call MPI_RECV(min_valarray, size(valarray), MPI_REAL, min_rank, tag, MPI_COMM_WORLD, status, mpi_err)
           if (kpr > 0) write(kpr,'(a,i4,a,f10.3,a,*(f8.3))')'Rank: ', mpi_prog_rank, &
                          ' has the minimum, min: ',min_val, ' array: ',min_valarray
       endif
   else
       min_valarray(:) = valarray(:)
   endif
!
   end subroutine send_minimum_to_master

#endif

END MODULE mpi_prog
