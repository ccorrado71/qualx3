module qe_frm

!
! For input file description see: 
!                   http://www.quantum-espresso.org/wp-content/uploads/Doc/INPUT_PW.html
!

implicit none 

type pseudo_table_type
     character(len=2)  :: el
     real              :: cutoff
     real              :: rho_cutoff
     character(len=30) :: filename
end type pseudo_table_type

integer, parameter, private :: PS_NUMBER = 85

type(pseudo_table_type), dimension(PS_NUMBER), private :: pseudo_table = [ &
pseudo_table_type( "Ag" , 50.00 , 200.00 , "Ag_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "Al" , 30.00 , 240.00 , "Al.pbe-n-kjpaw_psl.1.0.0.UPF  " ),  &
pseudo_table_type( "Ar" , 60.00 , 240.00 , "Ar_ONCV_PBE-1.1.oncvpsp.upf   " ),  &
pseudo_table_type( "As" , 35.00 , 280.00 , "As.pbe-n-rrkjus_psl.0.2.UPF   " ),  &
pseudo_table_type( "Au" , 45.00 , 180.00 , "Au_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "B " , 35.00 , 280.00 , "b_pbe_v1.4.uspp.F.UPF         " ),  &
pseudo_table_type( "Ba" , 30.00 , 240.00 , "Ba.pbe-spn-kjpaw_psl.1.0.0.UPF" ),  &
pseudo_table_type( "Be" , 40.00 , 320.00 , "be_pbe_v1.4.uspp.F.UPF        " ),  &
pseudo_table_type( "Bi" , 45.00 , 360.00 , "Bi_pbe_v1.uspp.F.UPF          " ),  &
pseudo_table_type( "Br" , 30.00 , 240.00 , "br_pbe_v1.4.uspp.F.UPF        " ),  &
pseudo_table_type( "C " , 45.00 , 360.00 , "C.pbe-n-kjpaw_psl.1.0.0.UPF   " ),  &
pseudo_table_type( "Ca" , 30.00 , 240.00 , "Ca_pbe_v1.uspp.F.UPF          " ),  &
pseudo_table_type( "Cd" , 60.00 , 480.00 , "Cd.pbe-dn-rrkjus_psl.0.3.1.UPF" ),  &
pseudo_table_type( "Ce" , 40.00 , 320.00 , "Ce.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Cl" , 40.00 , 320.00 , "cl_pbe_v1.4.uspp.F.UPF        " ),  &
pseudo_table_type( "Co" , 45.00 , 360.00 , "Co_pbe_v1.2.uspp.F.UPF        " ),  &
pseudo_table_type( "Cr" , 40.00 , 320.00 , "cr_pbe_v1.5.uspp.F.UPF        " ),  &
pseudo_table_type( "Cs" , 30.00 , 240.00 , "Cs_pbe_v1.uspp.F.UPF          " ),  &
pseudo_table_type( "Cu" , 55.00 , 440.00 , "Cu_pbe_v1.2.uspp.F.UPF        " ),  &
pseudo_table_type( "Dy" , 40.00 , 320.00 , "Dy.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Er" , 40.00 , 320.00 , "Er.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Eu" , 40.00 , 320.00 , "Eu.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "F " , 45.00 , 360.00 , "f_pbe_v1.4.uspp.F.UPF         " ),  &
pseudo_table_type( "Fe" , 90.00 , 1080.00, "Fe.pbe-spn-kjpaw_psl.0.2.1.UPF" ),  &
pseudo_table_type( "Ga" , 70.00 , 560.00 , "Ga.pbe-dn-kjpaw_psl.1.0.0.UPF " ),  &
pseudo_table_type( "Gd" , 40.00 , 320.00 , "Gd.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Ge" , 40.00 , 320.00 , "ge_pbe_v1.4.uspp.F.UPF        " ),  &
pseudo_table_type( "H " , 60.00 , 480.00 , "H.pbe-rrkjus_psl.1.0.0.UPF    " ),  &
pseudo_table_type( "He" , 50.00 , 200.00 , "He_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "Hf" , 50.00 , 200.00 , "Hf-sp.oncvpsp.upf             " ),  &
pseudo_table_type( "Hg" , 50.00 , 200.00 , "Hg_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "Ho" , 40.00 , 320.00 , "Ho.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "I " , 35.00 , 280.00 , "I.pbe-n-kjpaw_psl.0.2.UPF     " ),  &
pseudo_table_type( "In" , 50.00 , 400.00 , "In.pbe-dn-rrkjus_psl.0.2.2.UPF" ),  &
pseudo_table_type( "Ir" , 55.00 , 440.00 , "Ir_pbe_v1.2.uspp.F.UPF        " ),  &
pseudo_table_type( "K " , 60.00 , 480.00 , "K.pbe-spn-kjpaw_psl.1.0.0.UPF " ),  &
pseudo_table_type( "Kr" , 45.00 , 180.00 , "Kr_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "La" , 40.00 , 320.00 , "La.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Li" , 40.00 , 320.00 , "li_pbe_v1.4.uspp.F.UPF        " ),  &
pseudo_table_type( "Lu" , 45.00 , 360.00 , "Lu.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Mg" , 30.00 , 240.00 , "Mg.pbe-n-kjpaw_psl.0.3.0.UPF  " ),  &
pseudo_table_type( "Mn" , 65.00 , 780.00 , "mn_pbe_v1.5.uspp.F.UPF        " ),  &
pseudo_table_type( "Mo" , 35.00 , 140.00 , "Mo_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "N " , 60.00 , 480.00 , "N.pbe-n-radius_5.UPF          " ),  &
pseudo_table_type( "Na" , 40.00 , 320.00 , "na_pbe_v1.5.uspp.F.UPF        " ),  &
pseudo_table_type( "Nb" , 40.00 , 320.00 , "Nb.pbe-spn-kjpaw_psl.0.3.0.UPF" ),  &
pseudo_table_type( "Nd" , 40.00 , 320.00 , "Nd.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Ne" , 50.00 , 200.00 , "Ne_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "Ni" , 45.00 , 360.00 , "ni_pbe_v1.4.uspp.F.UPF        " ),  &
pseudo_table_type( "O " , 50.00 , 400.00 , "O.pbe-n-kjpaw_psl.0.1.UPF     " ),  &
pseudo_table_type( "Os" , 40.00 , 320.00 , "Os_pbe_v1.2.uspp.F.UPF        " ),  &
pseudo_table_type( "P " , 30.00 , 240.00 , "P.pbe-n-rrkjus_psl.1.0.0.UPF  " ),  &
pseudo_table_type( "Pb" , 40.00 , 320.00 , "Pb.pbe-dn-kjpaw_psl.0.2.2.UPF " ),  &
pseudo_table_type( "Pd" , 45.00 , 180.00 , "Pd_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "Pm" , 40.00 , 320.00 , "Pm.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Po" , 75.00 , 600.00 , "Po.pbe-dn-rrkjus_psl.1.0.0.UPF" ),  &
pseudo_table_type( "Pr" , 40.00 , 320.00 , "Pr.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Pt" , 35.00 , 280.00 , "pt_pbe_v1.4.uspp.F.UPF        " ),  &
pseudo_table_type( "Rb" , 30.00 , 120.00 , "Rb_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "Re" , 30.00 , 240.00 , "Re_pbe_v1.2.uspp.F.UPF        " ),  &
pseudo_table_type( "Rh" , 35.00 , 140.00 , "Rh_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "Rn" , 120.00, 960.00 , "Rn.pbe-dn-kjpaw_psl.1.0.0.UPF " ),  &
pseudo_table_type( "Ru" , 35.00 , 140.00 , "Ru_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "S " , 35.00 , 280.00 , "s_pbe_v1.4.uspp.F.UPF         " ),  &
pseudo_table_type( "Sb" , 40.00 , 320.00 , "sb_pbe_v1.4.uspp.F.UPF        " ),  &
pseudo_table_type( "Sc" , 40.00 , 160.00 , "Sc_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "Se" , 30.00 , 240.00 , "Se_pbe_v1.uspp.F.UPF          " ),  &
pseudo_table_type( "Si" , 30.00 , 240.00 , "Si.pbe-n-rrkjus_psl.1.0.0.UPF " ),  &
pseudo_table_type( "Sm" , 40.00 , 320.00 , "Sm.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Sn" , 60.00 , 480.00 , "Sn_pbe_v1.uspp.F.UPF          " ),  &
pseudo_table_type( "Sr" , 30.00 , 240.00 , "Sr_pbe_v1.uspp.F.UPF          " ),  &
pseudo_table_type( "Ta" , 45.00 , 360.00 , "Ta_pbe_v1.uspp.F.UPF          " ),  &
pseudo_table_type( "Tb" , 40.00 , 320.00 , "Tb.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Tc" , 30.00 , 120.00 , "Tc_ONCV_PBE-1.0.oncvpsp.upf   " ),  &
pseudo_table_type( "Te" , 30.00 , 240.00 , "Te_pbe_v1.uspp.F.UPF          " ),  &
pseudo_table_type( "Ti" , 35.00 , 280.00 , "ti_pbe_v1.4.uspp.F.UPF        " ),  &
pseudo_table_type( "Tl" , 50.00 , 400.00 , "Tl_pbe_v1.2.uspp.F.UPF        " ),  &
pseudo_table_type( "Tm" , 40.00 , 320.00 , "Tm.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "V " , 35.00 , 280.00 , "v_pbe_v1.4.uspp.F.UPF         " ),  &
pseudo_table_type( "W " , 30.00 , 240.00 , "W_pbe_v1.2.uspp.F.UPF         " ),  &
pseudo_table_type( "Xe" , 60.00 , 240.00 , "Xe_ONCV_PBE-1.1.oncvpsp.upf   " ),  &
pseudo_table_type( "Y " , 35.00 , 280.00 , "Y_pbe_v1.uspp.F.UPF           " ),  &
pseudo_table_type( "Yb" , 40.00 , 320.00 , "Yb.GGA-PBE-paw-v1.0.UPF       " ),  &
pseudo_table_type( "Zn" , 40.00 , 320.00 , "Zn_pbe_v1.uspp.F.UPF          " ),  &
pseudo_table_type( "Zr" , 30.00 , 240.00 , "Zr_pbe_v1.uspp.F.UPF          " )   ]

CONTAINS

   subroutine write_qe_file(filename,atom,cell,spg,structname,optimizeH)
   USE fileutil
   USE atom_type_util
   USE unit_cell
   USE spginfom
   USE elements
   USE arrayutil
#if 0
   USE connect_mod
   USE molpnew
#endif
   character(len=*), intent(in)                           :: filename
   type(atom_type), dimension(:), allocatable, intent(in) :: atom
   type(cell_type), intent(in)                            :: cell
   type(spaceg_type), intent(in)                          :: spg
   character(len=*), intent(in)                           :: structname
   logical, intent(in)                                    :: optimizeH  ! optimize only Hydrogen positions
   type(file_handle)                                      :: fqe
   integer                                                :: j_in
   type(element_type), dimension(:), allocatable          :: elem
   integer                                                :: ibrav,i
   real, parameter                                        :: EV_TO_RY=0.0734986176
   integer, dimension(:), allocatable                     :: ppindex
   integer, dimension(3)                                  :: optcode
#if 0
   type(bond_type), dimension(:), allocatable :: bond,bonds
   type(atom_type), dimension(:), allocatable :: atoms
#endif
!    real, dimension(3,3) :: v1
!
   call fqe%fopen(filename,'w')
   if (.not.fqe%good()) return
   j_in = fqe%handle()
!
   write(j_in,'(a)')'&CONTROL'
   write(j_in,'(4x,a)')"calculation='relax',"
   write(j_in,'(4x,a)')"!calculation='vc-relax',"
   write(j_in,'(4x,a)')"prefix='"//trim(structname)//"',"
   write(j_in,'(4x,a)')"pseudo_dir='"//get_cwd()//get_separ()//"SSSP_efficiency_pseudos"//"',"
   write(j_in,'(4x,a)')"outdir='"//get_homepath()//get_separ()//"scratch"//"',"
   write(j_in,'(4x,a)')"nstep=500,"
   write(j_in,'(4x,a)')"etot_conv_thr=5.D-5,"
   write(j_in,'(4x,a)')"forc_conv_thr=5.D-4,"
   write(j_in,'(4x,a)')"verbosity='high'"
   write(j_in,'("/"/)')
!
   write(j_in,'(a)')'&SYSTEM'
   ibrav = 0
   select case(spg%csys_code)

     case (CS_Cubic)
       select case (spg%lattyp)
         case ('P')
           ibrav = 1
         case ('F')
           ibrav = 2
         case ('I')
           ibrav = 3
       end select
       write(j_in,'(4x,a,i0,a)')"ibrav=",ibrav,","
       write(j_in,'(4x,a,f10.4,a)')"a=",cell%get_a(),","

     case (CS_Trigonal,CS_Hexagonal)
       if (uc_is_hexagonal(cell%get_par(),0.01)) then        ! a=b; al=bet=90; gam=120
           ibrav = 4
           write(j_in,'(4x,a,i0,a)')"ibrav=",ibrav,","
           write(j_in,'(4x,a,f10.4,a)')"a=",cell%get_a(),","
           write(j_in,'(4x,a,f10.4,a)')"c=",cell%get_c(),","
       elseif (uc_is_rhombohedral(cell%get_par(),0.01)) then ! a=b=c; al=bet=gam/=90
           if (spg%lattyp == 'P') then
               ibrav = 5
           else
               ibrav = -5
           endif
           write(j_in,'(4x,a,i0,a)')"ibrav=",ibrav,","
           write(j_in,'(4x,a,f10.4,a)')"a=",cell%get_a(),","
           write(j_in,'(4x,a,f12.6,a)')"cosbc=",cos(dtor*cell%get_alpha()),","
       endif

     case (CS_Tetragonal)
       select case (spg%lattyp)
         case ('P')
           ibrav = 6
         case ('I')
           ibrav = 7
       end select
       write(j_in,'(4x,a,i0,a)')"ibrav=",ibrav,","
       write(j_in,'(4x,a,f10.4,a)')"a=",cell%get_a(),","
       write(j_in,'(4x,a,f10.4,a)')"c=",cell%get_c(),","

     case (CS_Orthorhombic)
       select case (spg%lattyp)
         case ('P')
           ibrav = 8
!corr?         case ('A')
!corr?           ibrav = -9
         case ('C')
           ibrav = 9
         case ('F')
           ibrav = 10
         case ('I')
           ibrav = 11
       end select
       write(j_in,'(4x,a,i0,a)')"ibrav=",ibrav,","
       write(j_in,'(4x,a,f10.4,a)')"a=",cell%get_a(),","
       write(j_in,'(4x,a,f10.4,a)')"b=",cell%get_b(),","
       write(j_in,'(4x,a,f10.4,a)')"c=",cell%get_c(),","

     case (CS_Monoclinic)
       if (axis_direction(spg) == 'b') then
           if (spg%ncoper == 1) then
               ibrav = -12
           else
               ibrav = -13
           endif
           write(j_in,'(4x,a,i0,a)')"ibrav=",ibrav,","
           write(j_in,'(4x,a)')"uniqueb=.true.,"
           write(j_in,'(4x,a,f10.4,a)')"a=",cell%get_a(),","
           write(j_in,'(4x,a,f10.4,a)')"b=",cell%get_b(),","
           write(j_in,'(4x,a,f10.4,a)')"c=",cell%get_c(),","
           write(j_in,'(4x,a,f12.6,a)')"cosac=",cos(dtor*cell%get_beta()),","
       else 
           if (spg%ncoper == 0) then
               ibrav = 12
           else
               ibrav = 13
           endif
           write(j_in,'(4x,a,i0,a)')"ibrav=",ibrav,","
           write(j_in,'(4x,a)')"uniqueb=.false.,"
           write(j_in,'(4x,a,f10.4,a)')"a=",cell%get_a(),","
           write(j_in,'(4x,a,f10.4,a)')"b=",cell%get_b(),","
           write(j_in,'(4x,a,f10.4,a)')"c=",cell%get_c(),","
           write(j_in,'(4x,a,f12.6,a)')"cosab=",cos(dtor*cell%get_gamma()),","
       endif

     case (CS_Triclinic)
       write(j_in,'(4x,a,i0,a)')"ibrav=",14,","
       write(j_in,'(4x,a,f10.4,a)')"a=",cell%get_a(),","
       write(j_in,'(4x,a,f10.4,a)')"b=",cell%get_b(),","
       write(j_in,'(4x,a,f10.4,a)')"c=",cell%get_c(),","
       write(j_in,'(4x,a,f12.6,a)')"cosbc=",cos(dtor*cell%get_alpha()),","
       write(j_in,'(4x,a,f12.6,a)')"cosac=",cos(dtor*cell%get_beta()),","
       write(j_in,'(4x,a,f12.6,a)')"cosab=",cos(dtor*cell%get_gamma()),","
     
   end select
   call elements_from_atom(atom,elem)
   write(j_in,'(4x,a,i0,a)')"nat=",numatoms(atom),","
   write(j_in,'(4x,a,i0,a)')"ntyp=",numelem(elem),","
!
!  Set index to pseudopotentials
   call new_array(ppindex,numelem(elem))
   do i=1,numelem(elem)
      ppindex(i) = pseudopotential(elem(i)%z)
   enddo
   if (numelem(elem) > 0) then
       write(j_in,'(4x,a,f0.2)')"ecutrho=",maxval(pseudo_table(ppindex)%rho_cutoff)
       write(j_in,'(4x,a,f0.2)')"ecutwfc=",maxval(pseudo_table(ppindex)%cutoff)
!   else
!!
!!      We use 520eV: the same plane-wave energy cutoff of Acta Cryst. (2010). B66, 544–558
!       write(j_in,'(4x,a,f0.2)')"ecutwfc=",520*EV_TO_RY
   endif
!
   write(j_in,'(4x,a)')     "!occupations='fixed',"
   write(j_in,'(4x,a)')     "!occupations='smearing',"
   write(j_in,'(4x,a)')     "!degauss=0.01"
   write(j_in,'(4x,a,i0,a)')"space_group=",spg%num,","
   write(j_in,'(4x,a)')     "vdw_corr = 'grimme-d3',"
   write(j_in,'("/"/)')
!
   write(j_in,'(a)')'&ELECTRONS'
   write(j_in,'(4x,a)')"!conv_thr=1.D-6,"
   write(j_in,'(4x,a)')"!electron_maxstep = 200,"
   write(j_in,'("/"/)')
!
   write(j_in,'(a)')'&IONS'
   write(j_in,'(4x,a)')"!ion_dynamics='bfgs',"
   write(j_in,'("/"/)')
!
   write(j_in,'(a)')'&CELL'
   write(j_in,'("/"/)')
!
   write(j_in,'(a)')'ATOMIC_SPECIES'
   do i=1,numelem(elem)
      if (ppindex(i) == 0) cycle
      !write(j_in,'(4x,a,f10.4,4x,a)')eleminfo(elem(i)%z)%lab,elem(i)%weight,trim(eleminfo(elem(i)%z)%lab)//'.pbe-mt_fhi.UPF'
      write(j_in,'(4x,a,f10.4,4x,a)')eleminfo(elem(i)%z)%lab,elem(i)%weight,trim(pseudo_table(ppindex(i))%filename)
   enddo
!
#if 0
   write(j_in,'(a)')'CELL_PARAMETERS angstrom'
   v1 = get_lattice_vectors_P(cell,spg)
   do i=1,3
      write(j_in,'(3f12.6)')v1(i,:)
   enddo
#endif
   write(j_in,'(/a)')'ATOMIC_POSITIONS crystal_sg'
   if (optimizeH) then
       do i=1,numatoms(atom)
          if (is_hydrogen(atom(i))) then
              optcode(:) = 1
          else
              optcode(:) = 0
          endif
          write(j_in,'(a5,3f12.5,3(2x,i1))')atom(i)%spec(),atom(i)%xc,optcode
       enddo
   else
       optcode(:) = 1
       do i=1,numatoms(atom)
          write(j_in,'(a5,3f12.5,3(2x,i1))')atom(i)%spec(),atom(i)%xc,optcode
       enddo
   endif
#if 0
!!!!supercell
!!!TO FIX a) write make_crystal1 similar to make_crystal but without translation and array index
!!!! add cell parameters section, using triclinic and crystal system without ambiguities
   call apply_symmetry(atom,atoms,bond,bonds,level=[0.0,0.0,0.0],fitopt=2,cell=cell,spg=spg)
   write(j_in,'(/a)')'ATOMIC_POSITIONS crystal_sg'
   do i=1,numatoms(atoms)
      write(j_in,'(a5,3f12.5,3(2x,i1))')atoms(i)%spec(),atoms(i)%xc,1,1,1
   enddo
#endif
!
   write(j_in,'(/a)')'K_POINTS gamma'
   write(j_in,'(a)')'!K_POINTS automatic'
   write(j_in,'(a,6(1x,i0))')'!',cell%define_kmesh(0.15),0,0,0
!
   call fqe%fclose()
!
   end subroutine write_qe_file

!-----------------------------------------------------------------------------------------------

   function pseudopotential(z) result(ps)
   use elements
   use strutil
   integer, intent(in) :: z
   integer             :: ps
   integer             :: i
!
   do i=1,PS_NUMBER
      if (s_eqi(eleminfo(z)%lab,pseudo_table(i)%el)) then
          ps = i
          return
      endif
   enddo
   ps = 0
!
   end function pseudopotential

!-----------------------------------------------------------------------------------------------

   function get_lattice_vectors_P(cell,spg) result(v1)
   use unit_cell
   use spginfom
   type(cell_type), intent(in)   :: cell
   type(spaceg_type), intent(in) :: spg
   real, dimension(3,3)          :: v1
   integer                       :: i
!
   select case(spg%csys_code)
     case (CS_Orthorhombic,CS_Cubic)
       v1 = 0
       do i=1,3
          v1(i,i) = cell%get_par(i)
       enddo

     case (CS_Tetragonal)  
       v1 = 0
       v1(1,1) = cell%get_a()
       v1(2,2) = v1(1,1)
       v1(3,3) = cell%get_c()

     case default
       v1 = transpose(cell%get_ortom())
   end select
!
   end function get_lattice_vectors_P

!-----------------------------------------------------------------------------------------------

   function lattice_vectors_to_cell(v1)  result(cell)
   use unit_cell
   real, dimension(3,3), intent(in) :: v1
   real, dimension(6)               :: cell
   real, dimension(3,3)             :: gmat
!                                         T
   gmat = matmul(v1,transpose(v1)) ! G = M M
   cell = par_from_g(gmat)
!
   end function lattice_vectors_to_cell

!-----------------------------------------------------------------------------------------------
!corr
!corr   subroutine test(cell,spg)
!corr   use unit_cell
!corr   use spginfom
!corr   type(cell_type), intent(in)   :: cell
!corr   type(spaceg_type), intent(in) :: spg
!corr   real, dimension(3,3) :: lvet
!corr   type(cell_type) :: cellnew
!corr   integer :: i
!corr!
!corr   call cell%write(0)
!corr   lvet(:,:) = get_lattice_vectors_P(cell,spg)  !/cell%get_a()
!corr       write(0,*)'LVET='
!corr       do i=1,3
!corr          write(0,*)lvet(i,:)
!corr       enddo
!corr   write(0,*)'PAR=',lattice_vectors_to_cell(lvet)
!corr!
!corr   end subroutine test
!corr
!-----------------------------------------------------------------------------------------------

   subroutine read_qe_file(filename,atom,cell,spg,errc)
   USE unit_cell
   USE atom_type_util
   USE fileutil
   USE elements
   USE spginfom
   USE errormod
   USE strutil
   USE trig_constants
   USE prog_constants
   character(len=*), intent(in)                            :: filename
   type(atom_type), dimension(:), allocatable, intent(out) :: atom
   type(cell_type), intent(out)                            :: cell
   type(spaceg_type), intent(out)                          :: spg
   type(error_type), intent(out)                           :: errc
   type(file_handle)                                       :: fqe
   integer                                                 :: jout,ier
   character(len=:), allocatable                           :: line,line2
   real, dimension(6)                                      :: rnum
   integer                                                 :: ibrav
   integer                                                 :: nat,iv
   logical                                                 :: okatoms,crystal
   integer, parameter                                      :: start_size = 100
   real, dimension(3)                                      :: vet
   real                                                    :: alat
   logical                                                 :: okcell,is_lvet
   integer                                                 :: ncell
   real, dimension(3,3)                                    :: lvet
!
   call fqe%fopen(filename,'r')
   if (.not.fqe%good()) return
   jout = fqe%handle()
   okatoms = .false.
   okcell = .false.
   is_lvet = .false.
   crystal = .false.
   call spg%set_p1()
   call new_atoms(atom,start_size)
   nat = 0
!
   do while(get_line(jout,line,trimmed=.true.))
      if (index(line,'bravais-lattice index') > 0) then
          call get_next_number(line,'=',ibrav,ier)

      elseif (index(line,'celldm') > 0) then
          call parse_line_reals(line,[2,4,6],rnum(1:3),ier)
          if (ier /= 0) go to 10
          if (.not.get_line(jout,line,trimmed=.true.)) goto 10
          call parse_line_reals(line,[2,4,6],rnum(4:6),ier)
          if (ier /= 0) go to 10
          select case (ibrav)
            case (0)            ! use only celldm(1) and read crystal axes
              alat = rnum(1)
              if (.not. get_line(jout,line,trimmed=.true.)) exit ! empty line
              if (.not. get_line(jout,line,trimmed=.true.)) exit ! 'crystal axes'
              if (.not. get_line(jout,line,trimmed=.true.)) exit ! a(1)
              call parse_line_reals(line,[4,5,6],lvet(1,1:3),ier)
              if (.not. get_line(jout,line,trimmed=.true.)) exit ! a(2)
              call parse_line_reals(line,[4,5,6],lvet(2,1:3),ier)
              if (.not. get_line(jout,line,trimmed=.true.)) exit ! a(3)
              call parse_line_reals(line,[4,5,6],lvet(3,1:3),ier)
              is_lvet = .true.

            case (1,2,3)        ! Cubic
              rnum(2:3) = rnum(1)
              rnum(4:6) = 90

            case (4)            ! Hexagonal and trigonal P
              rnum(2) = rnum(1)
              rnum(3) = rnum(3)*rnum(1)
              rnum(4:5) = 90
              rnum(6) = 120
 
            case (5,-5)         ! Trigonal R
              rnum(2:3) = rnum(1)
              rnum(4:6) = rtod*acos(rnum(4))

            case (6,7)          ! Tetragonal P,I
              rnum(2) = rnum(1)
              rnum(3) = rnum(3)*rnum(1)
              rnum(4:6) = 90

            case (8,9,-9,10,11) ! Orthorhomic P, base-centered, face-centered, body-centered
              rnum(2:3) = rnum(2:3) * rnum(1)
              rnum(4:6) = 90

            case (12,-12,13,-13)    ! Monoclinic P, unique axis c,b,base-centered
              rnum(2:3) = rnum(2:3) * rnum(1)
              rnum(4:6) = rtod*acos(rnum(4:6))

            case (14)           ! Triclinic
              rnum(2:3) = rnum(2:3) * rnum(1)
              rnum(4:6) = rtod*acos(rnum(4:6))
             
              
          end select
          rnum(1:3) = AU_TO_ANG*rnum(1:3)
          call cell%set(rnum)

      elseif (okatoms) then
          if (len(line) > 0 .and. index(line,'End final coordinates') == 0) then
              call cutsta(line,line2=line2)
              !write(0,*)'SPEC=',line2,' AT=',line
              nat = nat + 1
              if (numatoms(atom) < nat) call resize_atoms(atom,numatoms(atom)+start_size) 
              atom(nat)%ptab = pxen_from_specie(line2)
              call getnum(line,vet,iv=iv)
              if (iv /= 3) go to 10
              atom(nat)%xc = vet
          else
              okatoms = .false.
          endif

      elseif (okcell) then
          if (len(line) > 0 .or. ncell < 3) then
              call getnum(line,vet,iv=iv)
              if (iv /= 3) go to 10
              ncell = ncell + 1
              lvet(ncell,:) = vet(1:3)
              if (ncell == 3) is_lvet = .true.
          else
              okcell = .false.
          endif

      elseif (index(line,'CELL_PARAMETERS') > 0) then
          call get_next_number(line,'=',alat,ier)
          okcell = .true.
          ncell = 0

      elseif (index(line,'ATOMIC_POSITIONS') > 0) then
          crystal = index(line,'crystal') > 0
          okatoms = .true.
          nat = 0

      endif
   enddo

!
!  Compute cell from lattice vectors
   if (is_lvet) then
       rnum(1:6) = lattice_vectors_to_cell(lvet*alat*AU_TO_ANG)
       call cell%set(rnum)
   endif

   call resize_atoms(atom,nat)
   if (nat > 0) then
       if (.not.crystal) call cart_to_frac(atom,cell%get_ortoi())
   else
       call errc%set('Problems reading a QE output file: no structure found!',ERR_STRUCTURE)
   endif 

   call fqe%fclose()

   return

10 call errc%set('Error reading QE file: '//trim(filename)//char(10)//'Line: '//trim(line))

   end subroutine read_qe_file

!-----------------------------------------------------------------------------------------------

   logical function is_espresso_file(filename)
!
!  Check if the file is a QE file
!
   USE fileutil
   character(len=*), intent(in) :: filename
   type(file_handle)            :: fnw
   character(len=200)           :: line
   integer                      :: ier
!
   is_espresso_file = .false.
   call fnw%fopen(filename,'r')
   if (.not.fnw%good()) return
!
   do while (.not.is_espresso_file)
     read(fnw%handle(),'(a)',iostat=ier)line
     if (ier /= 0) exit
     is_espresso_file = index(line,"Quantum ESPRESSO suite") > 0
   enddo 
   call fnw%fclose()
!
   end function is_espresso_file

end module qe_frm
