MODULE BONDTMOD

implicit none

real, dimension(:,:,:), allocatable, protected :: bond_table_min, bond_table_max
integer, parameter, private                    :: NELEMENTS = 103
integer, parameter, private                    :: MAXLENSTR = 10

type bond_table_type
  integer :: z1,z2            ! atomo1,atomo2
  real    :: distmin,distmax  ! distanza minima e massimo
  integer :: typeb            ! tipo di legame
  integer :: kleg             ! numero d'ordine che differenzia legami dello stesso tipo
end type bond_table_type

CONTAINS

   subroutine unload_bond_table()
!
!  Unload the bond table
!
   if (allocated(bond_table_min)) deallocate(bond_table_min,bond_table_max)
!
   end subroutine unload_bond_table

! ----------------------------------------------------------------------------

   subroutine load_bond_table()
   USE elements

   type bond_tab_type
     integer :: z1=0,z2=0
     real    :: dmin,dmax
     integer :: kleg=1
   end type
   type(bond_tab_type), dimension(1000) :: bond
   integer                              :: i
   integer                              :: nb
!
!  List created using function write_bond_list
!                                    Zmin   Zmax     dmin     dmax  bond order
   nb=1   ; bond(nb) = bond_tab_type(H_at,  B_at,   0.9410,  1.2770,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  C_at,   0.7590,  1.3590,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  N_at,   0.7330,  1.3330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  O_at,   0.6670,  1.3000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  F_at,   0.8000,  1.4000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  S_at,   1.0100,  1.6100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Cr_at,  1.4230,  2.0230,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Fe_at,  1.3700,  1.9700,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Co_at,  1.4280,  2.0280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Ni_at,  1.3910,  1.9910,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Cu_at,  1.3980,  1.9980,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Zn_at,  1.3170,  1.9170,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Mo_at,  1.3840,  1.9840,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Ru_at,  1.4820,  2.0820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Rh_at,  1.5470,  2.1470,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Ta_at,  1.4690,  2.0690,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  W_at,   1.4320,  2.0320,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Re_at,  1.3840,  1.9840,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Os_at,  1.3590,  1.9590,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Ir_at,  1.3030,  1.9030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Pt_at,  1.3100,  1.9100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(H_at,  Th_at,  1.7690,  2.3690,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Li_at, N_at,   1.3520,  2.7950,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Li_at, O_at,   1.6490,  2.6000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Li_at, F_at,   1.6300,  2.2300,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Li_at, S_at,   2.2100,  2.8100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Li_at, Cl_at,  2.1610,  2.7610,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Li_at, Br_at,  2.5230,  3.1230,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Li_at, I_at,   2.7790,  3.3790,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Be_at, O_at,   1.3110,  1.9110,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Be_at, F_at,   1.2060,  1.8060,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Be_at, Cl_at,  1.7000,  2.3000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  B_at,   1.4750,  2.0750,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  C_at,   1.3060,  1.9060,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  N_at,   1.2490,  1.8490,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  O_at,   1.1680,  1.7680,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  F_at,   1.0660,  1.6660,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  P_at,   1.6220,  2.2220,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  S_at,   1.5020,  2.1020,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Cl_at,  1.4580,  2.0580,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Sc_at,  2.2280,  2.8280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Ti_at,  1.8780,  2.4780,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  V_at,   2.0220,  2.6220,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Cr_at,  1.9940,  2.5940,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Mn_at,  1.9550,  2.5550,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Fe_at,  1.8400,  2.4400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Co_at,  1.9180,  2.5180,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Ni_at,  1.7860,  2.3860,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Cu_at,  1.9090,  2.5090,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Zn_at,  1.9190,  2.5190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  As_at,  1.7410,  2.3410,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Br_at,  1.6670,  2.2670,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Y_at,   2.2740,  2.8740,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Zr_at,  2.0350,  2.6350,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Mo_at,  2.1680,  2.7680,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Ru_at,  1.9630,  2.5630,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Rh_at,  2.0670,  2.6670,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Pd_at,  1.9240,  2.5240,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Ag_at,  2.0520,  2.6520,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  I_at,   1.9200,  2.5200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  W_at,   2.1030,  2.7030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Re_at,  1.9920,  2.5920,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Os_at,  1.9830,  2.5830,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Ir_at,  1.9360,  2.5360,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Pt_at,  1.9460,  2.5460,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Au_at,  1.9380,  2.5380,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  Hg_at,  1.9860,  2.5860,  1)
   nb=nb+1; bond(nb) = bond_tab_type(B_at,  U_at,   2.2690,  2.8690,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  C_at,   1.1700,  1.7500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  C_at,   1.0200,  1.6500,  2)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  C_at,   0.9600,  1.3400,  3)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  N_at,   1.2140,  1.6720,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  N_at,   1.1730,  1.4690,  2)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  N_at,   1.0310,  1.2650,  3)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  O_at,   1.1950,  1.5940,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  O_at,   1.0870,  1.3560,  2)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  F_at,   1.0400,  1.6400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Na_at,  2.4300,  3.0300,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Si_at,  1.5630,  2.1630,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  P_at,   1.5360,  2.1360,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  P_at,   1.3510,  1.9510,  2)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  S_at,   1.4510,  2.0510,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  S_at,   1.3600,  1.9600,  2)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Cl_at,  1.4940,  2.0940,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Sc_at,  2.1920,  2.7920,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Ti_at,  1.9150,  2.5150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  V_at,   1.7740,  2.3740,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Cr_at,  1.7120,  2.0760,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Cr_at,  1.8000,  2.2000,  2)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Mn_at,  1.6310,  2.0430,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Fe_at,  1.6280,  2.2280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Co_at,  1.5960,  2.1960,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Ni_at,  1.5820,  2.1820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Cu_at,  1.5610,  2.1610,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Zn_at,  1.7030,  2.3030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  As_at,  1.6430,  2.2430,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Se_at,  1.5930,  2.1930,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Br_at,  1.6100,  2.2100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Y_at,   2.3490,  2.9490,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Zr_at,  2.0200,  2.6200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Nb_at,  1.9410,  2.5410,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Mo_at,  1.7590,  2.3590,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Mo_at,  1.4990,  2.0990,  2)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Tc_at,  1.5840,  2.1840,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Ru_at,  1.7490,  2.3490,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Rh_at,  1.8220,  2.4220,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Pd_at,  1.6530,  2.2530,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Ag_at,  1.7930,  2.3930,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Sn_at,  1.8000,  2.4000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Te_at,  1.8160,  2.4160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  I_at,   1.7950,  2.3950,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Ce_at,  2.4480,  3.0480,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Pr_at,  2.3540,  2.9540,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Sm_at,  2.5090,  3.1090,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Eu_at,  2.5150,  3.1150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Gd_at,  2.4380,  3.0380,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Er_at,  2.3670,  2.9670,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Yb_at,  2.3570,  2.9570,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Lu_at,  2.1250,  2.7250,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Hf_at,  1.8900,  2.4900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Ta_at,  1.5500,  2.1500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  W_at,   1.5150,  2.1150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Re_at,  1.4420,  2.0420,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Os_at,  1.7600,  2.3600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Ir_at,  1.7410,  2.3410,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Pt_at,  1.6920,  2.2920,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Au_at,  1.7030,  2.3030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Hg_at,  1.7420,  2.3420,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  Th_at,  2.2540,  2.8540,  1)
   nb=nb+1; bond(nb) = bond_tab_type(C_at,  U_at,   2.2090,  2.8090,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  N_at,   1.1200,  1.7200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  N_at,   0.9400,  1.5400,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  N_at,   0.8240,  1.4240,  3)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  O_at,   1.1380,  1.7380,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  O_at,   0.9150,  1.5150,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  F_at,   1.1060,  1.7060,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Si_at,  1.4430,  2.0430,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  P_at,   1.3520,  1.9520,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  P_at,   1.2710,  1.8710,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  S_at,   1.3420,  1.9420,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  S_at,   1.2410,  1.8410,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Cl_at,  1.4280,  2.0280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ca_at,  2.2400,  2.7400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ti_at,  1.6200,  2.2200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  V_at,   1.7970,  2.3970,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  V_at,   1.4300,  2.0300,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Cr_at,  1.6800,  2.2800,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Mn_at,  1.6190,  2.2190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Mn_at,  2.0000,  2.6000,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Fe_at,  1.7290,  2.3290,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Co_at,  1.6710,  2.2710,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ni_at,  1.7820,  2.3820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Cu_at,  1.6830,  2.2830,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Zn_at,  1.6880,  2.2880,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ga_at,  1.6660,  2.2660,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  As_at,  1.5580,  2.1580,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  As_at,  1.5370,  2.1370,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Se_at,  1.5460,  2.1460,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Se_at,  1.4900,  2.0900,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Br_at,  1.5430,  2.1430,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Zr_at,  1.7600,  2.3600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Nb_at,  1.7430,  2.3430,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Mo_at,  1.8480,  2.4480,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Tc_at,  1.7490,  2.3490,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ru_at,  1.6430,  2.2430,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ru_at,  1.4420,  2.0420,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Pd_at,  1.7230,  2.3230,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ag_at,  1.9810,  2.5810,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ag_at,  1.8000,  2.4000,  2)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Cd_at,  1.9450,  2.5450,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Te_at,  1.6800,  2.2800,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  I_at,   1.7420,  2.3420,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  La_at,  2.4140,  3.0140,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ce_at,  2.3150,  2.9150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Gd_at,  2.3440,  2.7440,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Nd_at,  2.1710,  2.7710,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Dy_at,  2.1330,  2.7330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Yb_at,  2.1690,  2.7690,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Hf_at,  1.8640,  2.4640,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ta_at,  1.4960,  2.0960,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  W_at,   1.4400,  2.0400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Re_at,  1.7240,  2.3240,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Os_at,  1.4060,  2.0060,  1)
!corr   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ir_at,  1.5340,  2.1340,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Ir_at,  1.9640,  2.2160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Pt_at,  1.7250,  2.3250,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Au_at,  1.8420,  2.4420,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Hg_at,  1.7160,  2.3160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Pb_at,  2.0000,  2.6000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Er_at,  1.8120,  2.9290,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  Th_at,  2.1960,  2.7960,  1)
   nb=nb+1; bond(nb) = bond_tab_type(N_at,  U_at,   2.1390,  2.7390,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  O_at,   1.1820,  1.7820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Na_at,  2.0360,  2.9000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Mg_at,  1.7160,  2.3160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Al_at,  1.3180,  1.9180,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Al_at,  1.6700,  2.2700,  2)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Si_at,  1.3450,  1.9450,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  P_at,   1.3650,  1.7650,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  S_at,   1.1430,  1.7430,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  S_at,   1.2760,  1.8760,  2)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Cl_at,  1.1140,  1.7140,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  K_at,   2.2640,  3.8000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ca_at,  2.0300,  2.8600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Sc_at,  1.7510,  2.3510,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ti_at,  1.7040,  2.3040,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  V_at,   1.3510,  1.9510,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  V_at,   1.6800,  2.2800,  2)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Cr_at,  1.6640,  2.2640,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Cr_at,  1.3750,  1.9750,  2)
!18/01/2019   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Mn_at,  1.6150,  2.2150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Mn_at,  1.7800,  2.4000,  1)    ! Mn3+ < Mn2+
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Mn_at,  1.5600,  1.6200,  2)    ! Mn7+
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Fe_at,  1.6290,  2.2290,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Fe_at,  1.7360,  2.3360,  2)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Co_at,  1.7210,  2.3210,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ni_at,  1.7030,  2.3030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Cu_at,  1.6350,  2.7000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Zn_at,  1.6810,  2.2810,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ga_at,  1.5030,  2.1030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ge_at,  1.4210,  2.0210,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  As_at,  1.4100,  2.0100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  As_at,  1.3600,  1.9600,  2)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Se_at,  1.3080,  1.9080,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Br_at,  1.2810,  1.8810,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Rb_at,  2.5870,  3.1870,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Sr_at,  2.1870,  2.7870,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Y_at,   1.9870,  2.5870,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Zr_at,  1.7340,  2.3340,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Nb_at,  1.6200,  2.3200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Mo_at,  1.4150,  2.0150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Mo_at,  1.6210,  2.2210,  2)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Tc_at,  1.4100,  2.0100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ru_at,  1.7560,  2.3560,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Rh_at,  1.6590,  2.2590,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Pd_at,  1.7100,  2.3100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ag_at,  2.2000,  2.8000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Cd_at,  1.9190,  2.5190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  In_at,  1.7820,  2.3820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Sn_at,  1.7280,  2.3280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Sb_at,  1.6770,  2.2770,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Te_at,  1.6270,  2.2270,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  I_at,   1.8440,  2.4440,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Xe_at,  1.4250,  2.0250,  1)
!corr   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Cs_at,  2.7520,  3.3520,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Cs_at,  2.7520,  3.500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ba_at,  2.4380,  3.0380,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  La_at,  2.1000,  3.0000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ce_at,  2.0700,  2.6700,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Pr_at,  2.0910,  2.6910,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Nd_at,  2.0540,  2.6540,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Sm_at,  1.9880,  2.5880,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Eu_at,  2.0140,  2.6140,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Gd_at,  1.9820,  2.5820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Tb_at,  1.9940,  2.5940,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Dy_at,  1.9690,  2.5690,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ho_at,  1.9760,  2.5760,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Er_at,  1.9080,  2.9880,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Tm_at,  1.9330,  2.5330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Yb_at,  1.9180,  2.5180,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Lu_at,  1.9040,  2.5040,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Hf_at,  1.7280,  2.3280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ta_at,  1.6200,  2.2200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  W_at,   1.5500,  2.1500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Re_at,  1.4510,  2.0510,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Os_at,  1.4400,  2.0400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Ir_at,  1.6500,  2.2500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Pt_at,  1.7160,  2.3160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Au_at,  1.6830,  2.2830,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Hg_at,  1.7930,  2.3930,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Tl_at,  2.2760,  3.0760,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Pb_at,  2.0050,  3.4520,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Bi_at,  1.9700,  2.5700,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Th_at,  2.0770,  2.6770,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Pa_at,  2.0370,  2.6370,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  U_at,   1.5130,  2.1130,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Np_at,  1.4300,  2.0300,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Pu_at,  2.0500,  2.6500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(O_at,  Am_at,  2.0650,  2.6650,  1)
!corr   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Na_at,  2.0080,  2.6080,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Na_at,  2.0080,  2.8000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Mg_at,  1.6880,  2.2880,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Al_at,  1.5060,  2.1060,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Si_at,  1.3360,  1.9360,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  P_at,   1.2790,  1.8790,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  S_at,   1.2270,  1.8270,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  K_at,   2.3930,  2.9930,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ca_at,  2.0090,  2.6090,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Sc_at,  1.7680,  2.3680,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ti_at,  1.6180,  2.2180,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  V_at,   1.5330,  2.1330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Cr_at,  1.6260,  2.2260,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Mn_at,  1.5090,  2.1090,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Fe_at,  1.6000,  2.2000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Co_at,  1.7090,  2.3090,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ni_at,  1.5600,  2.1600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Cu_at,  1.6560,  2.2560,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Zn_at,  1.6720,  2.2720,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ga_at,  1.6120,  2.2120,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ge_at,  1.4670,  2.0670,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  As_at,  1.3780,  1.9780,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Br_at,  1.4800,  2.0800,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Rb_at,  2.6420,  3.2420,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Sr_at,  2.1650,  2.7650,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Y_at,   1.9670,  2.5670,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Zr_at,  1.7310,  2.3310,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Nb_at,  1.6500,  2.2500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Mo_at,  1.6010,  2.2010,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ru_at,  1.6630,  2.2630,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Pd_at,  1.7680,  2.3680,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ag_at,  1.7550,  2.3550,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Cd_at,  1.9210,  2.5210,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  In_at,  1.7400,  2.3400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Sn_at,  1.7560,  2.3560,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Sb_at,  1.6340,  2.2340,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Te_at,  1.6370,  2.2370,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  I_at,   1.5540,  2.1540,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Cs_at,  2.8130,  3.4130,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ba_at,  2.3660,  2.9660,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  La_at,  2.1090,  2.7090,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ce_at,  1.8650,  2.4650,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Nd_at,  2.1280,  2.7280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Sm_at,  2.0200,  2.6200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Gd_at,  1.9660,  2.5660,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Tb_at,  1.9900,  2.5900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ho_at,  1.9240,  2.5240,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Er_at,  1.8600,  2.4600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Yb_at,  1.8720,  2.4720,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Lu_at,  1.8020,  2.4020,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Hf_at,  1.7900,  2.3900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ta_at,  1.6060,  2.2060,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  W_at,   1.5350,  2.1350,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Re_at,  1.5760,  2.1760,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Os_at,  1.5280,  2.1280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Ir_at,  1.9720,  2.5720,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Pt_at,  1.5900,  2.1900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Au_at,  1.6610,  2.2610,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Hg_at,  2.1270,  2.7270,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Tl_at,  2.5260,  3.1260,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Pb_at,  2.1750,  2.7750,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Bi_at,  2.0570,  2.6570,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Th_at,  2.0330,  2.6330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Pa_at,  1.8650,  2.4650,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  U_at,   1.8180,  2.4180,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Np_at,  1.8580,  2.4580,  1)
   nb=nb+1; bond(nb) = bond_tab_type(F_at,  Pu_at,  2.1150,  2.7150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Na_at, S_at,   2.6520,  3.2520,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Na_at, Cl_at,  2.5270,  3.1270,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Na_at, Br_at,  2.7380,  3.3380,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Na_at, I_at,   3.1210,  3.7210,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Mg_at, S_at,   2.1940,  2.7940,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Mg_at, Cl_at,  2.2090,  2.8090,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Al_at, S_at,   1.9570,  2.5570,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Al_at, Cl_at,  1.8110,  2.4110,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Al_at, Br_at,  1.9650,  2.5650,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Al_at, I_at,   2.1830,  2.7830,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Si_at,  2.0590,  2.6590,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, P_at,   1.9640,  2.5640,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, S_at,   1.8450,  2.4450,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Cl_at,  1.7200,  2.3200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Mn_at,  1.9540,  2.5540,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Fe_at,  2.0290,  2.6290,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, As_at,  2.0510,  2.6510,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Br_at,  1.9840,  2.5840,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Mo_at,  2.3030,  2.9030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Ru_at,  2.1230,  2.7230,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Rh_at,  2.0790,  2.6790,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, W_at,   2.2860,  2.8860,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Re_at,  2.2430,  2.8430,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Os_at,  2.1000,  2.7000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Ir_at,  2.0980,  2.6980,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Pt_at,  1.9330,  2.5330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Si_at, Hg_at,  2.2330,  2.8330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  P_at,   1.9140,  2.5140,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  P_at,   1.7380,  2.3380,  2)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  S_at,   1.7180,  2.3180,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  S_at,   1.6130,  2.2130,  2)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Cl_at,  1.6320,  2.2320,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Ti_at,  2.2850,  2.8850,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  V_at,   2.0570,  2.6570,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Cr_at,  1.9880,  2.5880,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Mn_at,  1.9890,  2.5890,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Fe_at,  1.9040,  2.5040,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Co_at,  1.9330,  2.5330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Ni_at,  1.8890,  2.4890,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Cu_at,  2.0570,  2.6570,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Ga_at,  2.2840,  2.5840,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  As_at,  2.0500,  2.6500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  As_at,  1.8240,  2.4240,  2)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Se_at,  1.7930,  2.3930,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Br_at,  2.0660,  2.6660,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Zr_at,  2.3920,  2.9920,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Nb_at,  2.3970,  2.9970,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Mo_at,  1.9740,  2.5740,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Tc_at,  2.1940,  2.7940,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Ru_at,  2.0000,  2.6000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Rh_at,  2.1250,  2.7250,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Pd_at,  2.1520,  2.7520,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Ag_at,  2.1190,  2.7190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Cd_at,  2.2240,  2.8240,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Te_at,  2.0270,  2.6270,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  I_at,   2.1450,  2.7450,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Hf_at,  2.2000,  2.8000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Ta_at,  2.2890,  2.8890,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  W_at,   2.1850,  2.7850,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Re_at,  2.1780,  2.7780,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Os_at,  2.0790,  2.6790,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Ir_at,  2.0550,  2.6550,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Pt_at,  1.9820,  2.5820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Au_at,  1.9780,  2.5780,  1)
   nb=nb+1; bond(nb) = bond_tab_type(P_at,  Hg_at,  2.1900,  2.7900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  S_at,   1.7310,  2.3310,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Cl_at,  1.7720,  2.3720,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  K_at,   2.9500,  3.5500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ca_at,  2.5380,  3.1380,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Sc_at,  2.2490,  2.8490,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ti_at,  2.1250,  2.7250,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  V_at,   1.8830,  2.4830,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Cr_at,  2.1010,  2.7010,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Mn_at,  2.1610,  2.7610,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Fe_at,  2.0040,  2.6040,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Co_at,  2.1500,  2.7500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ni_at,  1.9940,  2.5940,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Cu_at,  1.9600,  2.5600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Zn_at,  2.0280,  2.6280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ga_at,  1.9330,  2.5330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ge_at,  1.8590,  2.4590,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  As_at,  1.9750,  2.5750,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  As_at,  1.7800,  2.3800,  2)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Se_at,  1.8930,  2.4930,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Br_at,  1.9060,  2.5060,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Rb_at,  3.0820,  3.6820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Sr_at,  2.7080,  3.3080,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Y_at,   2.4500,  3.0500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Zr_at,  2.2720,  2.8720,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Nb_at,  2.1280,  2.7280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Mo_at,  2.0640,  2.6640,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Tc_at,  2.0020,  2.6020,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ru_at,  2.0630,  2.6630,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Rh_at,  1.9350,  2.5350,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Pd_at,  2.0270,  2.6270,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ag_at,  2.2270,  2.8270,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Cd_at,  2.2440,  2.8440,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  In_at,  2.1700,  2.7700,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Sn_at,  2.3310,  2.9310,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Sb_at,  2.0340,  2.6340,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Te_at,  2.1050,  2.7050,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  I_at,   2.3290,  2.9290,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Cs_at,  3.2880,  3.8880,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ba_at,  2.8730,  3.4730,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  La_at,  2.6280,  3.2280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ce_at,  2.6050,  3.2050,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Pr_at,  2.5840,  3.1840,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Nd_at,  2.5680,  3.1680,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Sm_at,  2.4690,  3.0690,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Eu_at,  2.6250,  3.2250,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Gd_at,  2.5900,  3.1900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Dy_at,  2.4410,  3.0410,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ho_at,  2.3380,  2.9380,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Er_at,  2.3170,  2.9170,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Tm_at,  2.5700,  3.1700,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Yb_at,  2.4130,  3.0130,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Lu_at,  2.3740,  2.9740,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Hf_at,  2.2940,  2.8940,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ta_at,  2.1260,  2.7260,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  W_at,   2.2030,  2.8030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Re_at,  2.0340,  2.6340,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Os_at,  2.0770,  2.6770,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Ir_at,  2.1620,  2.7620,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Pt_at,  1.9470,  2.5470,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Au_at,  2.0410,  2.6410,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Hg_at,  2.1450,  2.7450,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Tl_at,  2.7540,  3.3540,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Pb_at,  2.4440,  3.0440,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Bi_at,  2.3310,  2.9310,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Th_at,  2.5390,  3.1390,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  U_at,   2.4780,  3.0780,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Np_at,  2.4700,  3.0700,  1)
   nb=nb+1; bond(nb) = bond_tab_type(S_at,  Pu_at,  2.6330,  3.2330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Cl_at,  2.0060,  2.6060,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, K_at,   2.8880,  3.4880,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ca_at,  2.5620,  3.1620,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Sc_at,  2.2570,  2.8570,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ti_at,  2.0500,  2.6500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, V_at,   2.0000,  2.6000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Cr_at,  2.1160,  2.7160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Mn_at,  2.1450,  2.7450,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Fe_at,  2.0500,  2.6500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Co_at,  2.0560,  2.6560,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ni_at,  2.0830,  2.6830,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Cu_at,  2.1550,  2.7550,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Zn_at,  1.9750,  2.5750,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ga_at,  1.8280,  2.4280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ge_at,  1.9730,  2.5730,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, As_at,  1.8160,  2.4160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Se_at,  1.8780,  2.4780,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Br_at,  2.1020,  2.7020,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Rb_at,  3.0640,  3.6640,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Sr_at,  2.7240,  3.3240,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Y_at,   2.4400,  3.0400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Zr_at,  2.1500,  2.7500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Nb_at,  2.0280,  2.6280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Mo_at,  2.1010,  2.7010,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Tc_at,  2.1810,  2.7810,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ru_at,  2.0400,  2.6400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Rh_at,  2.0230,  2.6230,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Pd_at,  1.8030,  2.4030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ag_at,  2.3190,  2.9190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Cd_at,  2.2800,  2.8800,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, In_at,  2.1870,  2.7870,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Sn_at,  2.0870,  2.6870,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Sb_at,  2.0310,  2.6310,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Te_at,  2.1190,  2.7190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, I_at,   2.2130,  2.8130,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Cs_at,  3.2690,  3.8690,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ba_at,  2.8630,  3.4630,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, La_at,  2.6310,  3.2310,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ce_at,  2.3050,  2.9050,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Pr_at,  2.5620,  3.1620,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Nd_at,  2.6450,  3.2450,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Eu_at,  2.5540,  3.1540,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Gd_at,  2.4440,  3.0440,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Tb_at,  2.4100,  3.0100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Er_at,  2.3130,  2.9130,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Yb_at,  2.4720,  3.0720,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Hf_at,  2.0650,  2.6650,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ta_at,  1.9750,  2.5750,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, W_at,   1.8920,  2.4920,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Re_at,  2.0160,  2.6160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Os_at,  2.0930,  2.6930,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Ir_at,  2.0540,  2.6540,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Pt_at,  2.0150,  2.6150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Au_at,  2.1110,  2.7110,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Hg_at,  2.2050,  2.8050,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Tl_at,  2.1820,  2.7820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Pb_at,  2.1900,  2.7900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Bi_at,  2.6280,  3.2280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Th_at,  2.6190,  3.2190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, U_at,   2.3460,  2.9460,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Pu_at,  2.5100,  3.1100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cl_at, Am_at,  2.5150,  3.1150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(K_at,  Br_at,  3.0180,  3.6180,  1)
   nb=nb+1; bond(nb) = bond_tab_type(K_at,  I_at,   3.3360,  3.9360,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ca_at, Br_at,  2.7060,  3.3060,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ca_at, I_at,   2.9450,  3.5450,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ti_at, As_at,  2.3770,  2.9770,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ti_at, Br_at,  2.4000,  3.0000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(V_at,  As_at,  2.2360,  2.8360,  1)
   nb=nb+1; bond(nb) = bond_tab_type(V_at,  Br_at,  2.2500,  2.8500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(V_at,  I_at,   2.3530,  2.9530,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cr_at, As_at,  2.1600,  2.7600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cr_at, Se_at,  2.1530,  2.7530,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cr_at, Br_at,  2.1410,  2.7410,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cr_at, Te_at,  2.5010,  3.1010,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cr_at, I_at,   2.3690,  2.9690,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Mn_at, As_at,  2.1000,  2.7000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Mn_at, Se_at,  2.0580,  2.6580,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Mn_at, Br_at,  2.2880,  2.8880,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Mn_at, Te_at,  2.1860,  2.7860,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Mn_at, I_at,   2.4190,  3.0190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Fe_at, As_at,  2.0520,  2.6520,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Fe_at, Se_at,  2.0930,  2.6930,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Fe_at, Br_at,  2.1700,  2.7700,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Fe_at, Te_at,  2.2600,  2.8600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Fe_at, I_at,   2.2930,  2.8930,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Co_at, As_at,  2.0230,  2.6230,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Co_at, Br_at,  2.1160,  2.7160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Co_at, I_at,   2.3400,  2.9400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ni_at, As_at,  2.0330,  2.6330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ni_at, Se_at,  2.0510,  2.6510,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ni_at, Br_at,  2.1100,  2.7100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ni_at, I_at,   2.3730,  2.9730,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cu_at, As_at,  2.0670,  2.6670,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cu_at, Se_at,  2.8090,  3.4090,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cu_at, Br_at,  2.1120,  2.7120,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cu_at, I_at,   2.2940,  2.8940,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Zn_at, Br_at,  2.0900,  2.6900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Zn_at, I_at,   2.2740,  2.8740,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ga_at, Br_at,  2.0000,  2.6000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ga_at, I_at,   2.2300,  2.8300,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ge_at, Ge_at,  2.3760,  2.8400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ge_at, Mo_at,  2.3530,  2.8490,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ge_at, I_at,   2.2430,  2.8430,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, As_at,  2.1590,  2.7590,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Se_at,  2.0550,  2.6550,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Br_at,  2.0200,  2.6200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Nb_at,  2.4410,  3.0410,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Mo_at,  2.2820,  2.8820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Tc_at,  2.2120,  2.8120,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Ru_at,  2.1460,  2.7460,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Rh_at,  2.1160,  2.7160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Pd_at,  2.0860,  2.6860,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Te_at,  2.2710,  2.8710,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, I_at,   2.2900,  2.8900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, W_at,   2.2490,  2.8490,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Re_at,  2.2750,  2.8750,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Os_at,  2.1810,  2.7810,  1)
   nb=nb+1; bond(nb) = bond_tab_type(As_at, Pt_at,  2.0660,  2.6660,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Se_at,  2.0400,  2.6400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Br_at,  2.2080,  2.8080,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Zr_at,  2.3390,  2.9390,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Mo_at,  2.1910,  2.7910,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Rh_at,  2.1570,  2.7570,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Ag_at,  2.2810,  2.8810,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Te_at,  2.2240,  2.8240,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, W_at,   2.3350,  2.9350,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Re_at,  2.2720,  2.8720,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Os_at,  2.2410,  2.8410,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Ir_at,  2.2290,  2.8290,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Pt_at,  2.1000,  2.7000,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Se_at, Hg_at,  2.3240,  2.9240,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Br_at,  2.2420,  2.8420,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Rb_at,  3.1920,  3.7920,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Sr_at,  2.7540,  3.3540,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Zr_at,  2.3650,  2.9650,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Nb_at,  2.2330,  2.8330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Mo_at,  2.3160,  2.9160,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Tc_at,  2.1400,  2.7400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Ru_at,  2.2210,  2.8210,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Rh_at,  2.2300,  2.8300,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Pd_at,  2.1580,  2.7580,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Ag_at,  2.1500,  2.7500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Cd_at,  2.3110,  2.9110,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, In_at,  2.3260,  2.9260,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Sn_at,  2.2100,  2.8100,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Sb_at,  2.2400,  2.8400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Te_at,  2.3920,  2.9920,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, I_at,   2.3460,  2.9460,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Cs_at,  3.4220,  4.0220,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Ba_at,  3.0190,  3.6190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, La_at,  2.8030,  3.4030,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Nd_at,  2.7700,  3.3700,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Gd_at,  2.6050,  3.2050,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Ta_at,  2.3040,  2.9040,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, W_at,   2.3190,  2.9190,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Re_at,  2.2730,  2.8730,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Os_at,  2.2500,  2.8500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Ir_at,  2.2800,  2.8800,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Pt_at,  2.1600,  2.7600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Au_at,  2.1130,  2.7130,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Hg_at,  2.2390,  2.8390,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Tl_at,  2.2330,  2.8330,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Pb_at,  2.6600,  3.2600,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Bi_at,  2.7770,  3.3770,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Th_at,  2.5860,  3.1860,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, U_at,   2.4970,  3.0970,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Br_at, Pu_at,  2.7900,  3.3900,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Rb_at, I_at,   3.4200,  4.0200,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Sr_at, I_at,   2.7570,  3.3570,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Zr_at, I_at,   2.5500,  3.1500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Nb_at, I_at,   2.4450,  3.0450,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Mo_at, Te_at,  2.4910,  3.0910,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Mo_at, I_at,   2.5670,  3.1670,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ru_at, I_at,   2.4440,  3.0440,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Rh_at, I_at,   2.4150,  3.0150,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Pd_at, I_at,   2.3240,  2.9240,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Ag_at, I_at,   2.5280,  3.1280,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Cd_at, I_at,   2.4500,  3.0500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(In_at, I_at,   2.4590,  3.0590,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Sn_at, I_at,   2.3340,  2.9340,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Sb_at, I_at,   2.7720,  3.3720,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Te_at, Te_at,  2.4040,  3.0040,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Te_at, I_at,   2.6260,  3.2260,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Te_at, Pt_at,  2.2750,  2.8750,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Te_at, Hg_at,  2.4320,  3.0320,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  I_at,   2.6170,  3.2170,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Cs_at,  3.6340,  4.2340,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Ba_at,  3.1820,  3.7820,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Eu_at,  3.0050,  3.6050,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Yb_at,  2.7270,  3.3270,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  W_at,   2.5400,  3.1400,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Re_at,  2.4180,  3.0180,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Os_at,  2.4740,  3.0740,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Ir_at,  2.4290,  3.0290,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Pt_at,  2.3580,  2.9580,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Au_at,  2.3500,  2.9500,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Hg_at,  2.4020,  3.0020,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Tl_at,  3.1760,  3.7760,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Pb_at,  2.9270,  3.5270,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  Bi_at,  2.9260,  3.5260,  1)
   nb=nb+1; bond(nb) = bond_tab_type(I_at,  U_at,   2.9050,  3.5050,  1)
   nb=nb+1; bond(nb) = bond_tab_type(Pb_at, Pb_at,  2.3810,  4.1010,  1)

   if (.not. allocated(bond_table_min))  &
      allocate(bond_table_min(3,MAXELEMENTS,MAXELEMENTS), bond_table_max(3,MAXELEMENTS,MAXELEMENTS))

   bond_table_min(:,:,:) = 0.0
   bond_table_max(:,:,:) = 0.0

   do i=1,nb
      bond_table_min(bond(i)%kleg,bond(i)%z1,bond(i)%z2) = bond(i)%dmin
      bond_table_min(bond(i)%kleg,bond(i)%z2,bond(i)%z1) = bond(i)%dmin
      bond_table_max(bond(i)%kleg,bond(i)%z1,bond(i)%z2) = bond(i)%dmax
      bond_table_max(bond(i)%kleg,bond(i)%z2,bond(i)%z1) = bond(i)%dmax
   enddo
   !call check_bond_table()

   end subroutine load_bond_table

! ----------------------------------------------------------------------------

   subroutine write_bond_list()
   USE elements
   USE strutil
   integer :: n1,n2
   integer :: j
   integer :: nbond
!
   nbond = 0
   do n1=1,NELEMENTS
      do n2=n1,NELEMENTS
         do j=1,3
            if (bond_table_max(j,n1,n2) == 0) exit
            nbond = nbond + 1
            write(70,'(3x,a,t22,a,t45,a,t51,f8.4,a,f8.4,a,i3,a)')'nb=nb+1; bond(nb)','= bond_tab_type('     &
            //trim(eleminfo(n1)%lab)//'_at,',trim(eleminfo(n2)%lab)//'_at,',  &
            bond_table_min(j,n1,n2),',',bond_table_max(j,n1,n2),',',j,')'
         enddo
      enddo
   enddo
!
   end subroutine write_bond_list

! ----------------------------------------------------------------------------

   subroutine check_bond_table()
   USE elements
   integer :: i,j
   do i=1,NELEMENTS
      write(71,*)'==========================================='
      write(71,*)'   Distance for '//trim(eleminfo(i)%lab)
      write(71,*)'==========================================='
      !do j=1,size(bond_table_max,3)
      do j=i,NELEMENTS
         if (bond_table_max(1,i,j) == 0) then
             write(71,*)'Undefined distance: ',trim(eleminfo(i)%lab)//'-'//trim(eleminfo(j)%lab)
         endif
      enddo
   enddo
   end subroutine check_bond_table

! ----------------------------------------------------------------------------

   real function bond_table(kleg,z1,z2) result(dist)
!
!  calcola un valore medio della distanza 
!
   integer, intent(in) :: kleg,z1,z2
!
   dist = ( bond_table_max(kleg,z1,z2) + bond_table_min(kleg,z1,z2) ) / 2
!
   end function bond_table

! ----------------------------------------------------------------------------

   subroutine get_bond_table(zvet,table,nbond)
!
!  Genera a partire dai Z la tabella dei legami
!
   integer, dimension(:), intent(in)                             :: zvet
   type(bond_table_type), dimension(:), allocatable, intent(out) :: table
   integer, intent(out)                                          :: nbond
   integer, dimension(NELEMENTS)                                 :: countz,zelem
   integer                                                       :: i,j
   integer                                                       :: nelem, kleg
   real                                                          :: bond_min
   integer                                                       :: el1,el2
!
!  Gestisce ripetizioni nel vettore zvet
   countz(:) = 0
   do i=1,size(zvet)
      if (zvet(i) > 0 .and. zvet(i) < NELEMENTS) then
          countz(zvet(i)) = countz(zvet(i)) + 1 
      endif
   enddo
!
!  Conta gli elementi distinti e mettili in zelem dal piu' pesante
   nelem = 0
   do i=NELEMENTS,1,-1
      if (countz(i) > 0) then
          nelem = nelem + 1
          zelem(nelem) = i
      endif
   enddo
!
   if (nelem > 0) then
!
!      Conta i legami e alloca table
       nbond = 0
       do i=1,nelem
          do j=i,nelem
             el1 = zelem(i)
             el2 = zelem(j)
             do kleg=1,3
                bond_min = bond_table_min(kleg,el1,el2)
                if (bond_min == 0.0 .and. kleg > 1) exit  ! se 0 aggiungi il legame una sola volta
                nbond = nbond + 1
             enddo
          enddo
       enddo
       allocate(table(nbond))
!
!      Riempi la tabella
       nbond = 0
       do i=1,nelem
          do j=i,nelem
             el1 = zelem(i)
             el2 = zelem(j)
             do kleg=1,3
                bond_min = bond_table_min(kleg,el1,el2)
                if (bond_min == 0.0 .and. kleg > 1) exit
                !if (bond_min > 0) then
                    nbond = nbond + 1
                    table(nbond)%z1 = el1
                    table(nbond)%z2 = el2
                    table(nbond)%distmin = bond_min
                    table(nbond)%distmax = bond_table_max(kleg,el1,el2)
                    table(nbond)%kleg = kleg
                    table(nbond)%typeb = kleg
                    !write(0,*)'table=',nbond,table(nbond)
                !else
                !    exit
                !endif
             enddo
          enddo
       enddo
   else
       nbond = 0
   endif
!
   end subroutine get_bond_table
   
! ----------------------------------------------------------------------------

   subroutine update_bond_table(table)
   type(bond_table_type), dimension(:), intent(in) :: table
   integer                                         :: i
!
   do i=1,size(table)
      bond_table_max(table(i)%kleg, table(i)%z1, table(i)%z2) = table(i)%distmax     
      bond_table_max(table(i)%kleg, table(i)%z2, table(i)%z1) = table(i)%distmax     
      bond_table_min(table(i)%kleg, table(i)%z1, table(i)%z2) = table(i)%distmin     
      bond_table_min(table(i)%kleg, table(i)%z2, table(i)%z1) = table(i)%distmin     
   enddo
!
   end subroutine update_bond_table

! ----------------------------------------------------------------------------

   function bond_table_string(kleg,ela,elb) result(str)
   integer, intent(in)                    :: kleg,ela,elb
   character(len=MAXLENSTR)               :: str
   character(len=MAXLENSTR), dimension(3) :: strbond = (/'single','double','triple'/)
   integer                                :: el1,el2
!
   if (ela > elb) then
       el1 = elb
       el2 = ela
   else
       el1 = ela
       el2 = elb
   endif
!
   select case(el1)
      case (6)   ! C
        select case (el2)
           case (6) ! C-C
                write(0,*)'kleg=',kleg
             str = strbond(kleg)
        end select

      case default
        str = ' '
   end select
!
   end function bond_table_string

! ----------------------------------------------------------------------------

   subroutine find_distance(z1,z2,dtype,dist,ier)
!
!  Find the distance z1-z2 with type dtype or less
!
   integer, intent(in)  :: z1,z2
   integer, intent(in)  :: dtype
   real, intent(out)    :: dist
   integer, intent(out) :: ier
   integer              :: i
!
   ier = 1
   do i=dtype,1,-1
      dist = bond_table(dtype,z1,z2)
      if (abs(dist) >= epsilon(1.0)) then
          ier = 0
          return
      endif
   enddo
!
   end subroutine find_distance

END MODULE BONDTMOD

