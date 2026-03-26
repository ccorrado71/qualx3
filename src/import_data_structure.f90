module import_data_struct

contains

   subroutine import_structure(filename,crystal,err)
   use gen_frm
   use crystal_phase
   use variables, only: dataset
   use errormod
   use datasetmod
   character(len=*), intent(in)       :: filename
   type(crystal_phase_t), intent(out) :: crystal
   type(error_type), intent(out)      :: err
   type(cell_type)                    :: cell
   type(spaceg_type)                  :: spg
   logical                            :: has_symmetry = .false.
   logical                            :: gui = .false.
   logical                            :: nowarning = .false.
   logical                            :: cancel_req
   logical                            :: mergecont = .false.
!
   call import_crystal(filename,ALL_FILES,crystal%at,crystal%bond,crystal%elem,   &
                       cell,spg,has_symmetry,get_wave1(dataset),   &
                       get_radtype(dataset),gui,nowarning,err,cancel_req,mergecont)
   if (.not.err%signal) then
       call crystal%set_symmetry(spg,cell)
   endif
!
   end subroutine import_structure

!-----------------------------------------------------------------------------------------------------

   subroutine test_import()
   use errormod
   use crystal_phase
   use exportfiles
   type(crystal_phase_t) :: crystal
   type(error_type)      :: err
   integer               :: ier

   call import_structure('/home/corrado/test_expo/merca_true.cif',crystal,err)
   if (.not.err%signal) then
       call crystal%print(0)
       call intensity_file_for_database(crystal,'/home/corrado/test_expo/merca_true.int',ier)
   endif

   end subroutine test_import

end module import_data_struct
