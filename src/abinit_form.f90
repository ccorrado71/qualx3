module abinit_frm

implicit none

contains

   subroutine write_ab_file(filename,atom,cell,spg)
!
!  Write .in file for abinit
!
   USE unit_cell
   USE atom_type_util
   USE fileutil
   USE elements
   USE spginfom
   character(len=*), intent(in), optional                 :: filename
   type(cell_type), intent(in)                            :: cell
   type(spaceg_type), intent(in)                          :: spg
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(file_handle)                                      :: fab
   integer                                                :: j_in,i
   type(element_type), dimension(:), allocatable          :: elem
!
   call fab%fopen(filename,'w')
   if (.not.fab%good()) return

   j_in = fab%handle()

   write(j_in,'(a)') '#Geometrical optimization'
   write(j_in,'(a)') 'ionmov 3               # Use the modified Broyden algorithm' 
   write(j_in,'(a)') 'ntime   10             # Maximum number of Broyden "timesteps"'
   write(j_in,'(a)') 'tolmxf  5.0d-4         # Stopping criterion for the geometry optimization : when'
   write(j_in,'(a)') '                       # the residual forces are less than tolmxf, the Broyden' 
   write(j_in,'(a)') '                       # algorithm can stop'
   write(j_in,'(a)') 'toldff  5.0d-5         # Will stop the SCF cycle when, twice in a row,'
   write(j_in,'(a)') '                       # the difference between two consecutive evaluations o'
   write(j_in,'(a)') '                       # forces differ by less than toldff (in Hartree/Bohr)'
   write(j_in,*)
 
   write(j_in,'(a)') '#unit cell and symmetry'
   write(j_in,'(a,3f15.6,a)')'acell ',cell%get_a(),cell%get_b(),cell%get_c(),' Angstr'
   write(j_in,'(a,3f15.3)')'angdeg ',cell%get_alpha(),cell%get_beta(),cell%get_gamma()
   write(j_in,'(a,i0)')    'spgroup ',spg%num
   write(j_in,*)

   write(j_in,'(a)')'#Atomic positions, atom types'
   write(j_in,'(a,i0)')'natom ',nint(natom_cell(atom,spg%nsymop))
   write(j_in,'(a,i0)')'natrd ',numatoms(atom)
!!!!TODO: decide between xangst and xred
   !write(j_in,'(a)')'xangst'
   write(j_in,'(a)')'xred'
   !cellstd = cell
   !call cellstd%set_std()
   do i=1,numatoms(atom)
      !if (atom(i)%z() > 0) write(j_in,'(3f15.5)')xyz_cart(atom(i),cellstd)
      if (atom(i)%z() > 0) write(j_in,'(3f15.5)')atom(i)%xc
   enddo
   call elements_from_atom(atom,elem)
   if (numelem(elem) > 0) then
       write(j_in,'(a,i0)')      'ntypat ',numelem(elem)
       write(j_in,'(a,*(1x,i0))')'znucl ', elem%z
       write(j_in,'(a,*(1x,i0))')'typat', (atom(i)%type_at(elem),i=1,numatoms(atom))
   endif
   write(j_in,*)

   write(j_in,'(a)') '#For automatic optimisation of the lattice parameters use:'
   write(j_in,'(a)') '#optcell 1'
   write(j_in,'(a)') '#ionmov  3'
   write(j_in,'(a)') '#ntime  10'
   write(j_in,'(a)') '#dilatmx 1.05'
   write(j_in,'(a)') '#ecutsm  0.5'
   write(j_in,*)
   
   write(j_in,'(a)') '#Basis set, k-point grid'
   write(j_in,'(a)') 'ecut 40.0  # Plane Wave cutoff (Ha)'
   write(j_in,'(a)') 'kptopt 1'
   write(j_in,'(a,3(1x,i0))') 'ngkpt',cell%define_kmesh(0.15)
   write(j_in,'(a)') 'nshiftk 1'
   write(j_in,'(a)') 'shiftk 0.5 0.5 0.5'
   write(j_in,'(a)') 'ixc 11' ! GGA, Perdew-Burke-Ernzerhof GGA functional
   write(j_in,*)
   
   write(j_in,'(a)') '#Van der Waals DFT-D2'
   write(j_in,'(a)') 'vdw_xc 5' ! vdw-DFT-D2
   write(j_in,'(a)') 'vdw_tol 1.e-8'

   call fab%fclose()
!
!  Create "files" file
   call write_abfile_files(filename,elem)
!
   end subroutine write_ab_file

!----------------------------------------------------------------------------------------------------

   subroutine write_abfile_files(filename,elem)
!
!  Create a "files" file. Run this file as: abinit < ab.files >& log 
!
   USE elements
   USE fileutil
   character(len=*), intent(in)                :: filename
   type(element_type), allocatable, intent(in) :: elem(:)
   type(file_handle)                           :: fab
   character(len=:), allocatable               :: fname
   character(len=:), allocatable               :: files_name
   integer                                     :: j_in,i
!
!  Check if file already esists
   fname = file_rem_ext(filename)
   write(0,'(a)')'FNAME=',fname
   files_name = fname//'.files'
   !if (file_exist(files_name)) return
!
   write(0,'(a)')'Write file files: '//trim(files_name)
   call fab%fopen(files_name,'w')
   if (.not.fab%good()) return
   j_in = fab%handle()
!
   write(j_in,'(a)')filename        !main input file
   write(j_in,'(a)')fname//'.out'   !main output file
   write(j_in,'(a)')fname//'i'      !root of name of input wavefunctions
   write(j_in,'(a)')fname//'o'      !root of name of output wavefunctions
   write(j_in,'(a)')'tmp'           !root of names of temporary files
!
!  List of pseudopotential input files in the same order of the types of atoms defined in .in file
   do i=1,numelem(elem)
      if (elem(i)%z < 10) then
          write(j_in,'(i0.2,a)')elem(i)%z,'-'//trim(elem(i)%lab)//'.GGA.fhi'
      else
          write(j_in,'(i0,a)')elem(i)%z,'-'//trim(elem(i)%lab)//'.GGA.fhi'
      endif
   enddo
!
   call fab%fclose()
!
   end subroutine write_abfile_files

end module abinit_frm
