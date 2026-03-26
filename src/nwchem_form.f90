module nwchem_frm

implicit none

integer, parameter, private :: DFT=1,NWPW=2

contains
   
   subroutine write_nw_file(filename,atom,cell,spg,structname,nwtheory)
   USE unit_cell
   USE atom_type_util
   USE fileutil
   USE elements
   USE spginfom
   character(len=*), intent(in), optional                 :: filename  
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(cell_type), intent(in)                            :: cell     
   type(spaceg_type), intent(in)                          :: spg
   character(len=*), intent(in)                           :: structname
   integer, intent(in)                                    :: nwtheory
   type(file_handle)                                      :: fnw
   integer                                                :: j_in,i
!
   call fnw%fopen(filename,'w')
   if (.not.fnw%good()) return

   j_in = fnw%handle()
   write(j_in,'("start molecule"//)')      
   write(j_in,'(a,a)')'title ',trim(structname)
   write(j_in,'("echo")')
   write(j_in,'("charge 0"//)')

   select case(nwtheory)

   case (DFT)   ! Optimize geometry using Density Functional Theory
       write(j_in,'(a)')'geometry units angstroms print xyz autosym'
       do i=1,numatoms(atom)
          if (atom(i)%z() > 0) write(j_in,'(a3,3f15.5)')atom(i)%specie(),xyz_cart(atom(i),cell)
       enddo
       write(j_in,'("end"/)')

       write(j_in,'(3(a/))') 'basis',               &
                             '  * library 6-31G*',  &
!corr                             '  * library aug-cc-pvdz',  &
                             'end' 

       write(j_in,'(5(a/))') 'dft',                 &  
                             '  xc b3lyp',          &
                             '  mult 1',            &
                             '  maxiter 100',       &
                             'end' 

       write(j_in,'(a)')     'driver'      
       write(j_in,'(a)')     '  maxiter 1000'
       write(j_in,'(a/)')    'end'                   

       write(j_in,'(a)')     'task dft optimize'

   case (NWPW)  ! Optimize geometry using Plane-Wave Density Functional Theory 
       write(j_in,'(a)')      '#**** Enter the geometry using fractional coordinates ****'
       write(j_in,'(a)')      'geometry center noautosym noautoz print'
       write(j_in,'(a)')      '  system crystal'
       write(j_in,'(a,f15.5)')'    lat_a ',cell%get_a()
       write(j_in,'(a,f15.5)')'    lat_b ',cell%get_b()
       write(j_in,'(a,f15.5)')'    lat_c ',cell%get_c()
       write(j_in,'(a,f15.3)')'    alpha ',cell%get_alpha()
       write(j_in,'(a,f15.3)')'    beta  ',cell%get_beta()
       write(j_in,'(a,f15.3)')'    gamma ',cell%get_gamma()
       write(j_in,'(a)')      '  end'
       write(j_in,'(a)')      '  symmetry '//trim(nwchem_space_group(spg))
       do i=1,numatoms(atom)
          if (atom(i)%z() > 0) write(j_in,'(2x,a3,3f15.5)')atom(i)%specie(),atom(i)%xc
       enddo
       write(j_in,'("end"/)')
       
       write(j_in,'(a/)')'memory 3500 mb'

       write(j_in,'(a)') 'nwpw'
       write(j_in,'(a)') '  ewald_rcut 3.0'
       write(j_in,'(a)') '  ewald_ncut 8'
       write(j_in,'(a)') '  cutoff 60'
       write(j_in,'(a)') '  lmbfgs'
       write(j_in,'(a)') '  xc pbe96-grimme3'
       write(j_in,'(a,3(1x,i0))') '  monkhorst-pack',cell%define_kmesh(0.15)
       write(j_in,'(a/)')'end'
   
       write(j_in,'(a)')     'driver'       
       write(j_in,'(a)')     '  maxiter 1000' 
       write(j_in,'(a/)')    'end'                   

       write(j_in,'(a)')    '#This option optimize the unit cell'
       write(j_in,'(a/)')    '#set includestress .true.'

       write(j_in,'(a)') 'task pspw optimize'

   end select

   call fnw%fclose()
!
   end subroutine write_nw_file

!-----------------------------------------------------------------------------------------------

   function nwchem_space_group(spg) result(str)
!
!  Convert Hermann Maugin symbol in NWCHEM symbol 
!
   USE spginfom
   USE strutil
   type(spaceg_type), intent(in) :: spg
   character(len=40)             :: str
!
   str = spg%symbol_xhm
   select case (spg%csys_code)
     case (CS_Monoclinic)
       call s_s_delete(str,' 1')
       call s_rep(str,'21','2_1')

     case (CS_Orthorhombic)
       call s_rep(str,'21','2_1')
       
     case (CS_Tetragonal)
       call s_rep(str,'41','4_1')
       call s_rep(str,'42','4_2')
       call s_rep(str,'43','4_3')
       call s_rep(str,'21','2_1')
   
     case (CS_Trigonal)
       call s_rep(str,'31','3_1')
       call s_rep(str,'32','3_2')

     case (CS_Hexagonal)
       call s_rep(str,'61','6_1')
       call s_rep(str,'62','6_2')
       call s_rep(str,'63','6_3')
       call s_rep(str,'64','6_4')

     case (CS_Cubic)
       call s_rep(str,'21','2_1')
       call s_rep(str,'41','4_1')
       call s_rep(str,'42','4_2')
       call s_rep(str,'43','4_3')

   end select
   str = s_blank_delete(str)
!
   end function nwchem_space_group

end module nwchem_frm
