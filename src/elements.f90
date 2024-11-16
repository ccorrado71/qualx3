MODULE elements
   USE arrayutil, only:check_container
   USE type_constants, only: DP
   USE scatt_params, only: SCATT_PAR_LOBATO, SCATT_PAR_DOYLE_TURNER, SCATT_PAR_KIRKLAND

   implicit none

   integer, parameter :: NLEN_LAB = 8

   type :: element_type
      integer                 :: z                  ! atomic number Z
      character(len=20)       :: name               ! name
      character(len=NLEN_LAB) :: lab                ! label
      real                    :: weight             ! atomic weight
      real                    :: c_radius = 0.0     ! covalent radius
      real                    :: w_radius = 0.0     ! van der Waals radius
      real, dimension(4)      :: ax                 ! coefficients a per calcolo scattering factors (x-ray)
      real, dimension(4)      :: bx                 ! coefficients b per calcolo scattering factors (x-ray)
      real                    :: cx                 ! coefficients c per calcolo scattering factors (x-ray)
      real, dimension(2)      :: fMo                ! f',f'' at Mo wavelength - obsolete value
      real, dimension(2)      :: fCu                ! f',f'' at Cu wavelength - obsolete value
      real                    :: f1 = 0.0, f2 = 0.0 ! f',f'' at specified wavelength from NIST tables
      real, dimension(4)      :: ae                 ! coefficients a per calcolo scattering factors (electron) - obsolete
      real, dimension(4)      :: be                 ! coefficients b per calcolo scattering factors (electron) - obsolete
      real                    :: ce                 ! coefficients c per calcolo scattering factors (electron) - obsolete
      real                    :: fact               ! fattore di scattering dei neutroni
      real(DP), dimension(12) :: factE              ! electron scattering factors
      real, dimension(4)      :: al = 0.0           ! used scattering factors
      real, dimension(4)      :: bs = 0.0           ! used scattering factors
      real                    :: cl = 0.0           ! used scattering factors
      real                    :: nw                 ! number of elements
      real                    :: mac = 0.0          ! mass attenuation coefficients (cm2 g-1) 
      integer                 :: radtype = 0        ! tipo di radiazione usata
      integer                 :: charge             ! charge 
      integer                 :: ptab               ! pointer to table of elements
      real                    :: zeff
      real                    :: ifMottFormula
   end type element_type

! S subroutine read_chemical_elements()                 Load information about chemical elements 
! S integer function numelem(elem)                      Size di elem
! S function chemical_element_info(label)               Restituisce info su elemento chimico a partire dalla label
! F vdw_radius(zval) result(vdw)                        Restituisce il raggio di van der Waals da Z
! S elem_set_radiation(elem,wave)                       Set radiation type
! S ordina_elements(elem)                               Ordina le specie chimiche sulla base del fattore di scattering
! S print_cell_content(elem,kpr,radtype)                Print cell content
! S print_scatt_factors(elem,kpr,radtype,wave)          Print scattering factors
! S print_elements(info,kpr)                            Print cell content and scattering factors
! F function molecular_weight(pxen,pw) result(mw)       Compute Molecular Weigth from pointer to chemical table
! F content_volume(specv,nspecv)  result(vol)           Calcolo approssimato del volume del contenuto
! F z_from_specie(spec)  result(zval)                   Dalla specie al numero atomico 
! F z_from_pxen(pxen)  result(zval)                     Dalla posizione nel file xen al numero atomico
! F pxen_from_specie(spec)  result(pxen)                Dalla specie al numero atomico 
! S get_charge(lab,charge,z)                            Get charge and atomic number
! F trim_charge(lab)                                    Trim charge from chemical label
! F elem_string(z,charge)                               Convert Z and numeric charge in a formatted string
! F form_charge(charge) result(str)                     Convert numeric charge in a formatted string (es. 1+,2+)
! F pxen_from_charge(z,charge) result(pxen)             Restiruisce pxen con la carica più vicina
! F pxen_from_string(str)  result(pxen)                 Fornisce la specie carica più vicina
! F pxen_from_label(label) result(pxen)                 Cerca nelle stringa una specie chimica ed estrai il pxen
! F specie_from_pxen(z) result(spec)                    Dal puntatore al file xen alla specie
! F number_of_elem(zelem,zval)                          Numero di elementi con Z=zelem 
! F is_element(elem,zval or label) result(ptr)          return pointer to array elem if z/lab exists
! S add_element(elem,zval,nw,add)                       Add new element to array only if it doesn't already esist
! S update_elem(elem,ptab,neutro,nsymtot)               Update array elem with new species in ptab
! S copy_elem(elem1,elem2)                              Copy elem2 in elem1
! F info_specie_from_pxen(zxen,nums,nspec,xspec)
! F electronegativity(zval) 
! S elem_from_atoms(elem,pxen)                          Estraggo elem dallo pxen di tutti gli atomi
! F oxidation_number(zval)                              Get the most common oxidation number 
! S get_oxidation_states(zval,oxc,noxc,ox,nox)          Get the oxidation states
! F equal_elements(elem1,elem2)  result(equal)          true se le due liste di elementi sono diverse
! F equal_element(elem1,elem2) result(equal)            true se i 2 elementi sono uguali
! S read_chemformula(sform,ier,elem,strfor)             Leggi una stringa contenente una formula chimica
! F contains_specie                                     check if zvet contains species
! S hill_order(spec,ord)                                Generate index for chemical elements according to Hill notation
! S chemical_formula                                    generate chemical formula
! F elemental function average_volume(zval)             Get the average volume of element with atomic number zval
! S save_elem_bin(unitp,elem)                           Save elements on binary file
! S read_elem_bin(unitp,elem,ier)                       Read elements from binary file
! F nasym_unit(elem,nsymop,excludeH)  result(nasym)     Number of atoms in the asymmetric unit
! F s3s2_value(elem,wavetype)                           Compute sum(ni*zi**3) / sqrt(sum(ni*zi**2))
! F at_scatt_scalar(elem,rho,radtype) result(sf)        Compute scattering factor. rho = sin(theta)/lambda
! F is_organic_el(z)                                    True if z is organic
! F order_is_ok(z1,z2)                                  Check order of atoms for formula
! F function getmin_el(elem,excludeH)                   Get location of minimum Z in elem
! F function getmax_el(elem)                            Get location of maximum Z in elem
! F group_number(zval)  result(gn)                      Group into periodic table
! F real function ma_coeff(z,wave)                      Mass attenuation coefficients
! S elem_set_mac(elem,wave)                             Set mac for element_type array
! F H_is_excluded(radtype)                              Evaluete if H is excluded from radiation type

!
!  Average volumes of elements (Act Cryst. (2002). B57, 489-493)
   real, dimension(100), parameter, private :: average_vol = (/                                      &
   5.08,10.00,                                                                                       &
!  H    He     
   22.6,36.,13.24,13.87,11.8,11.39,11.17,20.00,                                                      &
!  Li   Be  B     C     N    O     F     Ne
   26., 36.,39.6, 37.3, 29.5,25.2, 25.8, 30.00,                                                      &
!  Na   Mg  Al    Si    P    S     Cl    Ar
   36., 45.,42.,  27.3, 24.0,28.1, 31.9, 30.4, 29.4, 26.,26.9,39., 37.8,41.6,36.4,30.3, 32.7,40.00,  &
!  K    Ca  Sc    Ti    V    Cr    Mn    Fe    Co    Ni  Cu   Zn   Ga   Ge   As   Se    Br   Kr
   42., 47.,44.,  27.,  37., 38.,  38.,  37.3, 31.2, 35.,35., 51., 55., 52.8,48.0,46.7, 46.2,45.,    &
!  Rb   Sr  Y     Zr    Nb   Mo    Tc    Ru    Rh    Pd  Ag   Cd   In   Sn   Sb   Te    I    Xe
   46., 66.,                                                                                         &
!  Cs   Ba 
           58.,   54.,  57., 50.,  55.00,50.,  53.,  56.,45., 50., 42., 54., 49., 59.,  35.,         &
!          La     Ce    Pr   Nd    Pm    Sm    Eu    Gd  Tb   Dy   Ho   Er   Tm   Yb    Lu
                  40.,  43., 38.8, 42.7, 41.9, 34.3, 38.,43., 38.0,54., 52., 60., 50.00,55.00,60.00, &
!                 Hf    Ta   W     Re    Os    Ir    Pt  Au   Hg   Tl   Pb   Bi   Po    At    Rn
   70.00,60.00,                                                                                      &
!  Fr    Ra
           74.,   56.,  60., 58.,  45.,  70.00,17.,  70.00,70.00,70.00,70.00,70.00/)
!          Ac     Th    Pa   U     Np    Pu    Am    Cm    Bk    Cf    Es    Fm

!
!  The most common oxidation states. Table of Greenwood and Earnshaw in Chemistry of the Elements (2nd ed.) - pp. 27-28
   integer, dimension(13,108), parameter, private :: ox_numb = reshape([             &
                                        2, 2, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !H
                                        1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !He
                                        1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Li
                                        2, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Be
                                        3, 1, 3, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0,     & !B 
                                        8, 8, 4, 3, 2, 1,-1,-2,-3,-4, 0, 0, 0,     & !C 
                                        8, 3,-3, 3, 5, 1, 2, 4,-1,-2, 0, 0, 0,     & !N 
                                        4, 1,-2,-1, 1, 2, 0, 0, 0, 0, 0, 0, 0,     & !O
                                        1, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !F
                                        1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Ne
                                        2, 1, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Na
                                        2, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Mg
                                        5, 1, 3, 2, 1,-1,-2, 0, 0, 0, 0, 0, 0,     & !Al
                                        8, 2, 4,-4, 3, 2, 1,-1,-2,-3, 0, 0, 0,     & !Si
                                        8, 3, 5, 3,-3, 4, 2, 1,-1,-2, 0, 0, 0,     & !P
                                      !  8, 4, 4, 6, 2,-2, 5, 3, 1,-1, 0, 0, 0,     & !S
                                        8, 4, 6, 4, 2,-2, 5, 3, 1,-1, 0, 0, 0,     & !S
                                        8, 5,-1, 1, 3, 5, 7, 2, 4, 6, 0, 0, 0,     & !Cl
                                        1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Ar
                                        2, 1, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !K
                                        2, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Ca
                                        3, 1, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0,     & !Sc
                                        6, 1, 4, 3, 2, 1,-1,-2, 0, 0, 0, 0, 0,     & !Ti
                                        7, 1, 5, 4, 3, 2, 1,-1,-3, 0, 0, 0, 0,     & !V
                                        9, 2, 6, 3, 5, 4, 2, 1,-1,-2,-4, 0, 0,     & !Cr
                                       10, 3, 4, 2, 7, 6, 5, 3, 1,-1,-2,-3, 0,     & !Mn
                                       10, 3, 3, 2, 6, 7, 5, 4, 1,-1,-2,-4, 0,     & !Fe
                                        7, 2, 3, 2, 5, 4, 1,-1,-3, 0, 0, 0, 0,     & !Co
                                        5, 1, 2, 4, 3, 2,-1, 0, 0, 0, 0, 0, 0,     & !Ni
                                        5, 1, 2, 1, 3, 4,-2, 0, 0, 0, 0, 0, 0,     & !Cu
                                        3, 1, 2, 1,-2, 0, 0, 0, 0, 0, 0, 0, 0,     & !Zn
                                        7, 1, 3, 2, 1,-1,-2,-4,-5, 0, 0, 0, 0,     & !Ga
                                        8, 3, 4, 2,-4, 3, 1,-1,-2,-3, 0, 0, 0,     & !Ge
                                        8, 3, 3, 5,-3, 4, 2, 1,-1,-2, 0, 0, 0,     & !As
                                        8, 4, 6, 4, 2,-2, 5, 3, 1,-2, 0, 0, 0,     & !Se
                                        6, 4,-1, 1, 3, 5, 7, 4, 0, 0, 0, 0, 0,     & !Br
                                        1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Kr
                                        2, 1, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Rb
                                        2, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Sr
                                        3, 1, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0,     & !Y
                                        5, 1, 4, 3, 2, 1,-2, 0, 0, 0, 0, 0, 0,     & !Zr
                                        7, 1, 5, 4, 3, 2, 1,-1,-3, 0, 0, 0, 0,     & !Nb
                                        9, 2, 6, 4, 5, 3, 2, 1,-1,-2,-4, 0, 0,     & !Mo
                                        9, 2, 7, 4, 6, 5, 3, 2, 1,-1,-3, 0, 0,     & !Tc
                                       10, 2, 3, 4, 8, 7, 6, 5, 2, 1,-2,-4, 0,     & !Ru
                                        8, 1, 3, 6, 5, 4, 2, 1,-1,-3, 0, 0, 0,     & !Rh
                                        4, 2, 2, 4, 3, 1, 0, 0, 0, 0, 0, 0, 0,     & !Pd
                                        5, 1, 1, 2, 3,-1,-2, 0, 0, 0, 0, 0, 0,     & !Ag
                                        3, 1, 2, 1,-2, 0, 0, 0, 0, 0, 0, 0, 0,     & !Cd
                                        6, 1, 3, 2, 1,-1,-2,-5, 0, 0, 0, 0, 0,     & !In
                                        8, 3, 4, 2,-4, 3, 1,-1,-2,-3, 0, 0, 0,     & !Sn
                                        8, 3, 3, 5,-3, 4, 2, 1,-1,-2, 0, 0, 0,     & !Sb
                                        8, 4, 6, 4, 2,-2, 5, 3, 1,-1, 0, 0, 0,     & !Te
                                        7, 5,-1, 1, 3, 5, 7, 6, 4, 0, 0, 0, 0,     & !I
                                        4, 3, 4, 2, 6, 8, 0, 0, 0, 0, 0, 0, 0,     & !Xe
                                        2, 1, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Cs
                                        2, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Ba
                                        3, 1, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0,     & !La
                                        3, 2, 3, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0,     & !Ce
                                        4, 1, 3, 5, 4, 2, 0, 0, 0, 0, 0, 0, 0,     & !Pr
                                        3, 1, 3, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0,     & !Nd
                                        2, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Pm
                                        2, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Sm
                                        2, 2, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Eu
                                        3, 1, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0,     & !Gd
                                        4, 1, 3, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0,     & !Tb
                                        3, 1, 3, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0,     & !Dy
                                        2, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Ho
                                        2, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Er
                                        2, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Tm
                                        2, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Yb
                                        2, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Lu
                                        5, 1, 4, 3, 2, 1,-2, 0, 0, 0, 0, 0, 0,     & !Hf
                                        7, 1, 5, 4, 3, 2, 1,-1,-3, 0, 0, 0, 0,     & !Ta
                                        9, 2, 6, 4, 5, 3, 2, 1,-1,-2,-4, 0, 0,     & !W
                                        9, 1, 4, 7, 6, 5, 3, 2, 1,-1,-2, 0, 0,     & !Re
                                       11, 1, 4, 8, 7, 6, 5, 3, 2, 1,-1,-2,-4,     & !Os
                                       11, 2, 3, 4, 9, 8, 7, 6, 5, 2, 1,-1,-3,     & !Ir
                                        9, 2, 2, 4, 6, 5, 3, 1,-1,-2,-3, 0, 0,     & !Pt
                                        7, 1, 3, 5, 2, 1,-1,-2,-3, 0, 0, 0, 0,     & !Au
                                        3, 2, 2, 1,-2, 0, 0, 0, 0, 0, 0, 0, 0,     & !Hg
                                        6, 2, 3, 1, 2,-1,-2,-5, 0, 0, 0, 0, 0,     & !Tl
                                        7, 2, 2, 4, 3, 1,-1,-2,-5, 0, 0, 0, 0,     & !Pb
                                        8, 1, 3, 5, 4, 2, 1,-1,-2,-3, 0, 0, 0,     & !Bi
                                        5, 2, 2, 4,-2, 6, 5, 0, 0, 0, 0, 0, 0,     & !Po
                                        5, 2,-1, 1, 7, 5, 3, 0, 0, 0, 0, 0, 0,     & !At
                                        2, 1, 2, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Rn
                                        1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Fr
                                        1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Ra
                                        1, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Ac
                                        4, 1, 4, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0,     & !Th
                                        3, 1, 5, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0,     & !Pa
                                        6, 1, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0,     & !U
                                        6, 1, 5, 7, 6, 4, 3, 2, 0, 0, 0, 0, 0,     & !Np
                                        6, 1, 4, 7, 6, 5, 3, 2, 0, 0, 0, 0, 0,     & !Pu
                                        6, 1, 3, 7, 6, 5, 4, 2, 0, 0, 0, 0, 0,     & !Am
                                        3, 1, 3, 6, 4, 0, 0, 0, 0, 0, 0, 0, 0,     & !Cm
                                        2, 1, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Bk
                                        3, 1, 3, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0,     & !Cf
                                        3, 1, 3, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0,     & !Es
                                        2, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Fm
                                        2, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Md
                                        2, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !No
                                        1, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Lr
                                        1, 1, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Rf
                                        1, 1, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Db
                                        1, 1, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Sg
                                        1, 1, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     & !Bh
                                        1, 1, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0      & !Hs
                                       ],shape(ox_numb))

   enum, bind(c)
   enumerator :: H_at=1, He_at, Li_at, Be_at, B_at, C_at, N_at, O_at, F_at, Ne_at,       &
                 Na_at, Mg_at, Al_at, Si_at, P_at, S_at, Cl_at, Ar_at, K_at, Ca_at,      &
                 Sc_at, Ti_at, V_at, Cr_at, Mn_at, Fe_at, Co_at, Ni_at, Cu_at, Zn_at,    &
                 Ga_at, Ge_at, As_at, Se_at, Br_at, Kr_at, Rb_at, Sr_at, Y_at, Zr_at,    &
                 Nb_at, Mo_at, Tc_at, Ru_at, Rh_at, Pd_at, Ag_at, Cd_at, In_at, Sn_at,   &
                 Sb_at, Te_at, I_at, Xe_at, Cs_at, Ba_at, La_at, Ce_at, Pr_at, Nd_at,    &
                 Pm_at, Sm_at, Eu_at, Gd_at, Tb_at, Dy_at, Ho_at, Er_at, Tm_at, Yb_at,   &
                 Lu_at, Hf_at, Ta_at, W_at, Re_at, Os_at, Ir_at, Pt_at, Au_at, Hg_at,    &
                 Tl_at, Pb_at, Bi_at, Po_at, At_at, Rn_at, Fr_at, Ra_at, Ac_at, Th_at,   &
                 Pa_at, U_at, Np_at, Pu_at, Am_at, Cm_at, Bk_at, Cf_at, Es_at, Fm_at,    &
                 Md_at, No_at, Lr_at, Rf_at, Db_at, Sg_at, Bh_at, Hs_at, Mt_at, Ds_at,   &
                 Rg_at, D_at
   endenum

   integer, dimension(11), parameter, private :: org_elements = [C_at,H_at,B_at,N_at,O_at,P_at,S_at,F_at,Cl_at,Br_at,I_at]

   type(element_type), protected, dimension(:), allocatable :: eleminfo
   integer, dimension(0:111)                                :: pxen_from_z
   integer, protected                                       :: N_ELEMENTS = 0
   integer, parameter                                       :: MAXELEMENTS = 111
   integer, parameter                                       :: NSCATT_PARAM = 9

   integer, parameter :: RX_SOURCE=0, NEUTRON_SOURCE=1, ELECTRON_SOURCE=2
   character(len=*), dimension(0:2), parameter :: radiation_string = &
                     ['X-ray   ', 'Neutron ', 'Electron']
   !integer, parameter :: ELECTRON_SCATT_TYPE = SCATT_PAR_DOYLE_TURNER
   integer, parameter :: ELECTRON_SCATT_TYPE = SCATT_PAR_LOBATO

   character(len=5), protected :: file_release = ' '

   private :: pxen_from_specie_s, pxen_from_specie_v
   interface pxen_from_specie
     module procedure pxen_from_specie_s, pxen_from_specie_v
   end interface

   private :: z_from_specie_s, z_from_specie_v
   interface z_from_specie
     module procedure z_from_specie_s, z_from_specie_v
   end interface

   private :: chemical_formula_s, chemical_formula_sn, chemical_formula_el
   interface chemical_formula
     module procedure chemical_formula_s, chemical_formula_sn, chemical_formula_el
   end interface

   private :: check_container, z_contains_specie_string
   interface contains_specie
     module procedure check_container, z_contains_specie_string
   end interface 

   private :: add_element_from_Z, add_element_from_el
   interface add_element
      module procedure add_element_from_Z, add_element_from_el
   end interface

   private elem_set_scatt_s, elem_set_scatt_v
   interface elem_set_scatt
      module procedure elem_set_scatt_s, elem_set_scatt_v
   end interface

   private elem_set_radiation_s, elem_set_radiation_v
   interface elem_set_radiation
      module procedure elem_set_radiation_s, elem_set_radiation_v
   end interface

   private at_scatt_scalar, at_scatt_vect
   interface at_scatt
      module procedure at_scatt_scalar, at_scatt_vect
   end interface

   private at_scatt0_scalar, at_scatt0_vect
   interface at_scatt0
      module procedure at_scatt0_scalar, at_scatt0_vect
   end interface

   private is_element_z, is_element_lab
   interface is_element
      module procedure is_element_z, is_element_lab
   end interface

   private elem_set_nist_factors_s, elem_set_nist_factors_v
   interface elem_set_nist_factors
      module procedure elem_set_nist_factors_s, elem_set_nist_factors_v
   end interface

   private nasym_unit_lg, nasym_unit_int
   interface nasym_unit
      module procedure nasym_unit_lg, nasym_unit_int
   end interface

CONTAINS

   subroutine read_chemical_elements(filename,err)
!
!  Leggi elementi chimici da file esterno
!
   USE strutil
   USE fileutil
   USE errormod
   USE iso_fortran_env, only: ERROR_UNIT
   USE prog_constants, only: DEF_WAVE
   USE scatt_params
   character(len=*), intent(in)  :: filename
   type(error_type), intent(out) :: err
   type(file_handle)             :: fileh
   integer                       :: file_unit
   integer                       :: nline
   character(len=200)            :: line,line2
   integer                       :: ierror
   integer                       :: nlong1,nlong2
   integer                       :: nelem
   integer, dimension(20)        :: ivet
   real, dimension(20)           :: vet
   integer                       :: iv
   integer                       :: i
   integer                       :: ndimelem
   type(error_type)              :: err_nist
   integer                       :: err_el
!
   call fileh%fopen(filename)
   if (fileh%good()) then
       file_unit = fileh%handle()
!
!      Skip intro del file
       nline = 1
       read(file_unit,'(a5)',iostat=ierror,err=10)file_release
       nline = 2
       read(file_unit,'(a)',iostat=ierror,err=10)line
!
!      Read commented block
       do 
          read(file_unit,'(a)',iostat=ierror,err=10)line
          if (.not.is_comment_line(line,['#'])) exit
          nline = nline + 1
       enddo
       backspace(file_unit)
!
!      allocazione iniziale di elem
       ndimelem = 220
       call reallocate_elem(eleminfo,ndimelem,start=0)
!
       pxen_from_z(:) = 0
       nelem = 0
       loop_elements: do 
         do i=1,9
            nline = nline + 1
            read(file_unit,'(a)',iostat=ierror,err=10)line
            if (ierror < 0) exit loop_elements
            call s_filter(line)                   ! filtra le linea
            call cutst(line,nlong1,line2,nlong2)  ! taglia il simbolo chimico
            select case (i)
               case (1) ! lab Atomic   Weight    coval_radius    vanderWaals_radius
                 nelem = nelem + 1
                 if (nelem > ndimelem) then
                     ndimelem = ndimelem + 10
                     call reallocate_elem(eleminfo,ndimelem,start=0)
                 endif
                 line2(:1) = upper(line2(:1))    ! rendi maiuscolo il primo char. della label
                 eleminfo(nelem)%lab = line2(:nlong2)
                 call Getnum(line,vet,ivet,iv)
                 if (iv /= 4) go to 10
                 eleminfo(nelem)%z = ivet(1)
                 eleminfo(nelem)%weight = vet(2)
                 eleminfo(nelem)%c_radius = vet(3)
                 eleminfo(nelem)%w_radius = vet(4)
                 eleminfo(nelem)%ptab = nelem
                 if (pxen_from_z(ivet(1)) == 0) pxen_from_z(ivet(1)) = nelem

               case (2) !scattering factor (x-ray)
                 call Getnum(line,vet,ivet,iv)
                 if (iv /= 6) go to 10
                 eleminfo(nelem)%ax(:3) = vet(1:6:2)
                 eleminfo(nelem)%bx(:3) = vet(2:6:2)
                 eleminfo(nelem)%al(:3) = vet(1:6:2)
                 eleminfo(nelem)%bs(:3) = vet(2:6:2)

               case (3) ! a(4),b(4),c x-ray
                 call Getnum(line,vet,ivet,iv)
                 if (iv /= 3) go to 10
                 eleminfo(nelem)%ax(4) = vet(1)
                 eleminfo(nelem)%bx(4) = vet(2)
                 eleminfo(nelem)%cx = vet(3)
                 eleminfo(nelem)%al(4) = vet(1)
                 eleminfo(nelem)%bs(4) = vet(2)
                 eleminfo(nelem)%cl = vet(3)

               case(4)  ! f'(Mo),f"(Mo)     !!!!!,mu/rho 
                 call Getnum(line,vet,ivet,iv)
                 if (iv /= 3) go to 10
                 !eleminfo(nelem)%fMo(:) = vet(1:3)
                 eleminfo(nelem)%fMo(:) = vet(1:2)

               case(5)  ! f'(Cu),f"(Cu) !!!!!,mu/rho
                 call Getnum(line,vet,ivet,iv)
                 if (iv /= 3) go to 10
                 !eleminfo(nelem)%fCu(:) = vet(1:3)
                 eleminfo(nelem)%fCu(:) = vet(1:2)

               case(6)  ! scattering factor (electrons)
                 call Getnum(line,vet,ivet,iv)
                 if (iv /= 6) go to 10
                 eleminfo(nelem)%ae(:3) = vet(1:6:2)
                 eleminfo(nelem)%be(:3) = vet(2:6:2)

               case(7)  ! a(4),b(4),c (electrons)
                 call Getnum(line,vet,ivet,iv)
                 if (iv /= 3) go to 10
                 eleminfo(nelem)%ae(4) = vet(1)
                 eleminfo(nelem)%be(4) = vet(2)
                 eleminfo(nelem)%ce = vet(3)

               case(8)  ! fact (neutrons)
                 call Getnum(line,vet,ivet,iv)
                 if (iv /= 1) go to 10
                 eleminfo(nelem)%fact = vet(1)

               case(9)  ! name
                 eleminfo(nelem)%name = trim(line)

            end select
         enddo
         call get_charge(eleminfo(nelem)%lab,eleminfo(nelem)%charge)
         !eleminfo(nelem)%mac = ma_coeff(eleminfo(nelem)%z,DEF_WAVE)  ! mac at default wavelength
         call elem_set_nist_factors(eleminfo(nelem),DEF_WAVE,RX_SOURCE,err_nist)
         call get_electron_params(eleminfo(nelem)%z,0,eleminfo(nelem)%factE,ELECTRON_SCATT_TYPE,err_el)
       enddo loop_elements
!
       call reallocate_elem(eleminfo,nelem,start=0)  ! ora rialloca elem al numero di specie lette
       eleminfo(:)%radtype = 0
       eleminfo(:)%nw = 0
       eleminfo(:)%zeff = eleminfo(:)%z 
       eleminfo(:)%ifMottFormula = 0
       N_ELEMENTS = nelem
       call fileh%fclose()
       eleminfo(0)%z = 0
       eleminfo(0)%ptab = 0
       eleminfo(0)%lab = 'Q'
       call fileh%fclose()
   else
       write(ERROR_UNIT,'(a)')'Cannot open: '//trim(filename)
       write(ERROR_UNIT,'(a)')'Message: '//trim(fileh%err_msg())
       call err%set('Message: '//trim(fileh%err_msg()))
   endif
   return
!
10 call err%set('Error on reading line '//i_to_s(nline)//' in configuration file '//trim(filename))
   write(ERROR_UNIT,'(a)')trim(err%message)
!
   end subroutine read_chemical_elements

!--------------------------------------------------------------------------------------------------

   function filexen_release()
   character(len=:), allocatable :: filexen_release
   filexen_release = file_release
   end function filexen_release

!--------------------------------------------------------------------------------------------------

   function filexen_name(path)
   character(len=*), intent(in)  :: path
   character(len=:), allocatable :: filexen_name
   filexen_name = trim(path)//'AtomProperties.xen'
   end function filexen_name

!--------------------------------------------------------------------------------------------------

   subroutine reallocate_elem(vetr,n,savevet,start)
!
!  Rialloca ad n un vettore di tipo elemnt_type
!  Se savevet = .true. o non esiste si salva il suo contenuto.
!
   type(element_type), allocatable, intent(inout) :: vetr(:)
   integer, intent(in)                            :: n
   logical, intent(in), optional                  :: savevet
   integer, intent(in), optional                  :: start
   logical                                        :: savev
   integer, dimension(1)                          :: nv
   type(element_type), allocatable                :: vsav(:)
   integer                                        :: nsav
   integer                                        :: lb
!
!  se n = 0 (riallocazione a 0): dealloca ed esci
   if (n == 0) then
       if (allocated(vetr)) deallocate(vetr)
       return
   endif
   if (present(start)) then
       lb = start
   else
       lb = 1
   endif
!
   if (.not.allocated(vetr)) then
       allocate(vetr(lb:n))
   else
!
       nv = ubound(vetr)
       if (present(savevet)) then
           savev = savevet
       else
           savev = .true.
       endif
!
       if (savev) then
!
!          nsav contiene qual è la porzione di vetr da salvare
           select case(nv(1)-n)
             case (1:)       ! compatta x ad n
               nsav = n
             case (:-1)      ! espandi x ad n
               nsav = nv(1)
             case (0)
               return        ! n=nv non fare niente
           end select
           allocate(vsav(lb:n))
           vsav(:nsav) = vetr(:nsav)
           call move_alloc(vsav,vetr)
       else
           if (nv(1) /= n) then
               deallocate(vetr)
               allocate(vetr(lb:n))
           endif
       endif
   endif
!
   end subroutine reallocate_elem

!----------------------------------------------------------------------------------------

   integer function numelem(elem)
!
!  Size di elem
!
   type(element_type), allocatable, intent(in) :: elem(:)
   if (allocated(elem)) then
       numelem = size(elem)
   else
       numelem = 0
   endif
   end function numelem

!----------------------------------------------------------------------------------------

   function chemical_element(pxen,label,z) result(info)
!
!  Restituisce info su elemento chimico a partire 
!  dalla label (oppure dalla posizione nel file xen) e dal tipo di radiazione 
!  info%z = 0 se la label non corrisponde a nessuna specie chimica
!
   USE strutil
   integer, intent(in), optional          :: pxen   ! posizione della specie nel file xen
   character(len=*), intent(in), optional :: label  ! simbolo chimica
   integer, intent(in), optional          :: z      ! numero atomico
   type(element_type)                     :: info
   integer                                :: kfound
!
   if (present(pxen)) then
!
!      assegnata la posizione nel file xen
       if (pxen <= N_ELEMENTS) then
           kfound = pxen
       else
           kfound = 0
       endif
   elseif (present(label)) then
!
!      assegnata la label?
       if (present(label)) then
           kfound = pxen_from_specie(label)
       else
           kfound = 0
       endif
   else
       if (z > 0 .and. z <= ubound(pxen_from_z,dim=1)) then
           kfound = pxen_from_z(z)
       else
           kfound = 0
       endif
   endif
!
   if (kfound == 0) then   ! label non trovata
       info%z = 0
   else                    ! label trovata
       info = eleminfo(kfound)
   endif
!
   end function chemical_element

!--------------------------------------------------------------------

   real function vdw_radius(zval) result(vdw)
!
!  Restituisce il raggio di van der Waals
!
   integer, intent(in) :: zval   ! posizione della specie nel file xen
   integer             :: pxen
!
   pxen = pxen_from_z(zval)      
   if (pxen > 0) then
       vdw = eleminfo(pxen)%w_radius
   else
       vdw = 0
   endif
!
   end function vdw_radius
!--------------------------------------------------------------------

   subroutine elem_set_scatt_v(elem)
!
!  Set used scattering factor for vector elem
!
   type(element_type), dimension(:), allocatable, intent(inout) :: elem
   integer                                                      :: i
!
   do i=1,numelem(elem)
      call elem_set_scatt_s(elem(i))
   enddo
!
   end subroutine elem_set_scatt_v

!--------------------------------------------------------------------

   subroutine elem_set_scatt_s(elem)
!
!  Set used scattering factor
!
   type(element_type), intent(inout) :: elem
!
   select case (elem%radtype)
     case (RX_SOURCE)          
           elem%al = elem%ax
           elem%bs = elem%bx
           elem%cl = elem%cl
     case (NEUTRON_SOURCE)    
           elem%al = 0
           elem%bs = 0
           elem%cl = elem%fact
     case (ELECTRON_SOURCE)  
           elem%al = elem%ae
           elem%bs = elem%be
           elem%cl = elem%ce
   end select
!
   end subroutine elem_set_scatt_s

!--------------------------------------------------------------------
 
   subroutine elem_set_radiation_v(elem,radtype)
!
!  Set radiation type
!
   type(element_type), dimension(:), allocatable, intent(inout) :: elem
   integer, intent(in)                                          :: radtype
!
   elem(:)%radtype = radtype
   call elem_set_scatt(elem)
!
   end subroutine elem_set_radiation_v

!--------------------------------------------------------------------
 
   subroutine elem_set_radiation_s(elem,radtype)
!
!  Set radiation type
!
   type(element_type), intent(inout) :: elem
   integer, intent(in)               :: radtype
!
   elem%radtype = radtype
   call elem_set_scatt(elem)
!
   end subroutine elem_set_radiation_s

!--------------------------------------------------------------------

   subroutine ordina_elements(elem,radtype,sort,ord)
!
!  Ordina le specie chimiche sulla base del fattore di scattering
!
   USE nr
   USE arrayutil
   type(element_type), dimension(:), intent(inout) :: elem
   integer, intent(in)                             :: radtype
   integer, intent(in), optional                   :: sort
   integer, dimension(:), allocatable, intent(inout), optional :: ord
   integer, dimension(size(elem))                  :: iord
!
   if (size(elem) == 0) return
!
   select case (radtype)
      case (RX_SOURCE)          ! raggi X
         call indexx(elem(:)%z,iord)
      case (NEUTRON_SOURCE)     ! neutroni
         call indexx(abs(elem(:)%fact),iord)
      case (ELECTRON_SOURCE)    ! elettroni
         call indexx(elem(:)%zeff,iord)
   end select
   if (present(sort)) then
       if (sort < 0) iord = iord(size(elem):1:-1)
   endif
   elem(:) = elem(iord)
   if (present(ord)) then
       call new_array(ord,size(elem))
       ord = iord
   endif
!
   end subroutine ordina_elements

!--------------------------------------------------------------------

   subroutine print_cell_content(elem,kpr,radtype)
   type(element_type), dimension(:), intent(in) :: elem
   integer, intent(in)                          :: kpr
   integer, intent(in)                          :: radtype 
   integer                                      :: n, nelem
!
   nelem = size(elem)
   if (radtype == NEUTRON_SOURCE) then
       write(kpr,'(/15x,a/)')'Unit cell content and scattering factor constants'
       write(kpr,'(12x,a)')"Atom      Symbol   Number in cell   Atomic number   Weight   Radius        f'"
       do n=1,nelem
          write(kpr,'(10x,a,2x,a,i13,i16,3x,2f9.3,3x,f10.5)')elem(n)%name(1:12),elem(n)%lab(1:4), &
                nint(elem(n)%nw),elem(n)%z,elem(n)%weight,elem(n)%c_radius,elem(n)%cl
       enddo
   else
       write(kpr,'(/30x,a/)')'Unit cell content'
       write(kpr,'(12x,a)')'Atom      Symbol   Number in cell   Atomic number   Weight   Radius'
       do n=1,nelem
          write(kpr,'(10x,a,2x,a,i13,i16,3x,2f9.3)')elem(n)%name(1:12),elem(n)%lab(1:4),   &
                nint(elem(n)%nw),elem(n)%z,elem(n)%weight,elem(n)%c_radius
       enddo
   endif
!
   end subroutine print_cell_content

!--------------------------------------------------------------------

   subroutine print_scatt_factors(elem,kpr,radtype,wave)
   use strutil
   use nist_mod
   type(element_type), dimension(:), intent(in) :: elem
   integer, intent(in)                          :: kpr
   integer, intent(in)                          :: radtype 
   real, intent(in)                             :: wave
   integer                                      :: i,n
   integer                                      :: nelem

   if (radtype == NEUTRON_SOURCE) return ! neutron scatt. factors are printed with cell content

   nelem = size(elem)
   if (nelem == 0) return
   write(kpr,'(/25x,a)')trim(radiation_string(radtype))//' scattering factor constants'
   select case (radtype)
     case (RX_SOURCE)          ! raggi X
       write(kpr,'(20x,a/,22x,a//,10x,a)')  &
            'f = sum (  a(i) * exp(-b(i)*s2)  )  i=1,4 + c ',  &
            '(International Tables Vol C Table 6.1.1.4)',                                &
            'a(1)      b(1)      a(2)      b(2)      a(3)      b(3)      a(4)      b(4)      c'
       do n=1,nelem
          write(kpr,'(1x,a,9f10.5)')elem(n)%lab(1:4),(elem(n)%al(i),elem(n)%bs(i),i=1,4),elem(n)%cl
       enddo
       write(kpr,'(/x,a,a,a,a,a)')"X-ray dispersion coefficients for lambda = ", r_to_s(wave,6), &
                                       " ang => ", r_to_s(angstroms_to_kev(wave),6) ," kev"
       write(kpr,'(a)')" (NIST Standard Reference Database 126)"
       write(kpr,'(a)')"          f'         f''     mu/r (cm^2/g)"
       do n=1,nelem
          write(kpr,'(1x,a,3f10.5)')elem(n)%lab(1:4),elem(n)%f1,elem(n)%f2,elem(n)%mac
       enddo

     case (ELECTRON_SOURCE)    ! electrons
       select case(ELECTRON_SCATT_TYPE)
          case (SCATT_PAR_DOYLE_TURNER)
            write(kpr,'(20x,a/,10x,a//,10x,a)') &
                      'f = sum (  a(i) * exp(-b(i)*s2)  )  i=1,4', &
                      '(P. A. Doyle and P. S. Turner, Acta Cryst. (1968). A24, 390)', &
                      'a(1)      b(1)      a(2)      b(2)      a(3)      b(3)      a(4)      b(4)'
            do n=1,nelem
               write(kpr,'(1x,a,8f10.5)')elem(n)%lab(1:4),elem(n)%factE(1:8)
            enddo

          case (SCATT_PAR_LOBATO)
            write(kpr,'(25x,a/,20x,a/,2(/,15x,a))') &
                      'f = sum ( a(i) * (2 + b(i)*s2) / (1 + b(i)*s2)) i=1,5', &
                      '(I. Lobato and D. Van Dyck, Acta Cryst. (2014). A70, 636-649)', &
                      'a(1)                a(2)                a(3)                a(4)                a(5)', &
                      'b(1)                b(2)                b(3)                b(4)                b(5)'
            do n=1,nelem
               write(kpr,'(1x,a,5e20.10/,5x,5e20.10)')elem(n)%lab(1:4),elem(n)%factE(1:10)
            enddo

          case (SCATT_PAR_KIRKLAND)
            write(kpr,'(20x,a/,10x,a/,3(/,15x,a))') &
                      'f = sum (a(i) / (b(i) + s2)) + sum(c(i) * exp(-d(i)*s2)) i=1,3', &
                      '(Kirkland, E. J. (1998). Advanced Computing in Electron Microscopy. Appendix D)', &
                      'a(1)                b(1)                a(2)                b(2)', &
                      'a(3)                b(3)                c(1)                d(1)', &
                      'c(2)                d(2)                c(3)                d(3)'
            do n=1,nelem
               write(kpr,'(1x,a,4e20.8,2(/,5x,4e20.8))')elem(n)%lab(1:4),elem(n)%factE(1:12)
            enddo
       end select
   end select
   end subroutine print_scatt_factors

!--------------------------------------------------------------------

   subroutine print_elements(elem,kpr,radtype, wave)
   use nist_mod
   use strutil
   type(element_type), dimension(:), intent(in) :: elem
   integer, intent(in)                          :: kpr
   integer, intent(in)                          :: radtype 
   real, intent(in)                             :: wave
   integer                                      :: nelem
!
   nelem = size(elem)
   if (nelem == 0) return
!
   call print_cell_content(elem,kpr,radtype)
   call print_scatt_factors(elem,kpr,radtype,wave)
!
   end subroutine print_elements

!--------------------------------------------------------------------

   real function molecular_weight(pxen,pw) result(mw)
!
!  Compute Molecular Weigth from pointer to chemical table
!
   integer, dimension(:) :: pxen   ! pointer to chemical table
   real, dimension(:)    :: pw     ! contribution
   integer               :: i
   mw = 0
   do i=1,size(pxen)
      if (pxen(i) > 0) mw = mw + eleminfo(pxen(i))%weight*pw(i)
   enddo
   end function molecular_weight

!corr!--------------------------------------------------------------------
!corr
!corr   real function molecular_weight_el(elem) result(mw)
!corr!
!corr!  Compute Molecular Weigth from element type
!corr!
!corr   type(element_type), dimension(:), allocatable, intent(in) :: elem
!corr!
!corr   if (numelem(elem) == 0) then
!corr       mw = 0
!corr       return
!corr   endif
!corr   mw = molecular_weight_pxen(elem%ptab,elem%nw)
!corr!
!corr   end function molecular_weight_el
!corr
!--------------------------------------------------------------------

   real function content_volume(ptab,nspecv)  result(vol)
!
!  Calcolo approssimato del volume del contenuto
!
   integer, dimension(:), intent(in) :: ptab
   real, dimension(:), intent(in)    :: nspecv   ! num. di atomi per specie
   integer                           :: i
!
   vol = 0
   do i=1,size(ptab)
      vol = vol + nspecv(i)*average_volume(eleminfo(ptab(i))%z)
   enddo
!
   end function content_volume

!--------------------------------------------------------------------

   integer function z_from_specie_s(spec)  result(zval)
!
!  Dalla specie al numero atomico 
!
   USE strutil
   character(len=*), intent(in) :: spec
   integer                      :: i
!
   zval = 0
   do i=1,N_ELEMENTS
      if (s_eqi(spec,eleminfo(i)%lab)) then
          zval = eleminfo(i)%z
          exit
      endif
   enddo
!
   end function z_from_specie_s

!--------------------------------------------------------------------

   function z_from_specie_v(spec)  result(zval)
!
!  Dalla specie al numero atomico 
!
   USE strutil
   character(len=*), dimension(:), intent(in) :: spec
   integer, dimension(size(spec))             :: zval
   integer                                    :: i
!
   do i=1,size(spec)
      zval(i) = z_from_specie_s(spec(i))
   enddo
!
   end function z_from_specie_v

!------------------------------------------------------------------------------------

   elemental function z_from_pxen(pxen)  result(zval)
!
!  Dalla posizione nel file xen al numero atomico
!
   integer, intent(in) :: pxen
   integer             :: zval
!
   if(pxen > 0)then
      zval = eleminfo(pxen)%z
   else
      zval = 0
   endif
!
   end function z_from_pxen

!-------------------------------------------------------------------------------------------------

   integer function z_number(elem)
!
!  Extract z number 
!
   type(element_type), intent(in) :: elem
   z_number = eleminfo(elem%ptab)%z
   end function z_number

!-------------------------------------------------------------------------------------------------
   
    integer function pxen_from_specie_s(spec)  result(pxen)
!
!   Dalla specie al numero atomico 
!
    USE strutil
    character(len=*), intent(in) :: spec
    integer                      :: i
!
    pxen = 0
    do i=1,N_ELEMENTS
       if (s_eqi(spec,eleminfo(i)%lab)) then
           pxen = i
           exit
       endif
    enddo
!
    end function pxen_from_specie_s

!------------------------------------------------------------------------------------

    function pxen_from_specie_v(spec)  result(pxen)
    USE strutil
    character(len=*), dimension(:), intent(in) :: spec
    integer, dimension(size(spec))             :: pxen
    integer                                    :: j
    do j=1,size(spec)
       pxen(j) = pxen_from_specie_s(spec(j))
    enddo
    end function pxen_from_specie_v

!------------------------------------------------------------------------------------

    subroutine get_charge(lab,charge,kal1)
!
!   Get charge and atomic number. Sign after charge: es. Na1+ and not Na+1
!
    USE strutil
    character(len=*), intent(in)   :: lab
    integer, intent(out)           :: charge
    integer, intent(out), optional :: kal1   ! Last character before number. kal1 = 0 in case of problem in the string
    integer                        :: lenl
    integer                        :: i
    integer                        :: charge_sign, charge_value
    integer                        :: kal
    logical                        :: lsign, ldig
    character(len=1)               :: ch
    character(len_trim(lab))       :: lab1
    charge = 0
    charge_sign = 1
    charge_value = 0 
    if (present(kal1)) kal1 = 0
    kal = 0
    ldig = .false.
    lsign = .false.
    lab1 = s_blank_delete(lab)
    lenl = len_trim(lab1)   
    i = 0
    do 
       i = i + 1
       if (i > lenl) exit
       ch = lab1(i:i)
       if (ch == ' ') cycle
       if (ch_is_alpha(ch)) then
           if (kal == 2) return         ! 3 caratteri consec.
           if (ldig .or. lsign) return  ! es. N+N, N2N
           kal = kal + 1
       elseif (ch_is_digit(ch)) then
           if (kal == 0) return  ! es. 2N
           ldig = .true.
           call ch_to_digit(ch,charge_value)
       elseif (ch == '+' .or. ch == '-') then
           if (kal == 0) return  ! es. +Na
           lsign = .true.
           if (ch == '-') charge_sign = -1
       else
           return
       endif
    enddo
    charge = charge_value*charge_sign
    if (present(kal1)) then
        !z = pxen_from_specie(lab1(1:kal))
        kal1 = kal
    endif
    end subroutine get_charge

!------------------------------------------------------------------------------------

    function trim_charge(lab)
!
!   Trim charge from chemical label (E.g., 'O2-' => 'O')
!
    USE strutil
    character(len=*), intent(in)   :: lab
    character(len=:), allocatable  :: trim_charge
    integer                        :: i
    character(len_trim(lab))       :: lab1
!
    lab1 = s_blank_delete(lab)
    do i=len_trim(lab1),1,-1
       if (ch_is_digit(lab1(i:i)) .or. lab1(i:i) == '+' .or. lab1(i:i) == '-') then
           lab1(i:i) = ' '
       else
           exit
       endif
    end do
    if (i == 0) then
        trim_charge = ' '
    else
        trim_charge = lab1(1:i)
    endif
!
    end function trim_charge

!--------------------------------------------------------------------------------------------------

    function elem_string(z,charge) result(str)
!
!   Convert Z and numeric charge in a formatted string
!
    integer, intent(in)           :: z,charge
    character(len=:), allocatable :: str
!
    if (charge == 1) then
        str = specie_from_pxen(z)
    else
        str = specie_from_pxen(z)//form_charge(charge)
    endif
!
    end function elem_string

!--------------------------------------------------------------------------------------------------

    function form_charge(charge) result(str)
!
!   Convert numeric charge in a formatted string (es. 1+,2+)
!
    USE strutil  
    integer, intent(in)           :: charge
    character(len=:), allocatable :: str
    str = ' '
    if (charge == 0) return
    if (charge > 0) then
        str = trim(i_to_s(charge))//'+'
    else
        str = trim(i_to_s(abs(charge)))//'-'
    endif
    end function form_charge

!--------------------------------------------------------------------------------------------------

    integer function pxen_from_charge(str,charge) result(pxen)
!
!   Restiruisce pxen con la carica più vicina
!
    use strutil
    character(len=*), intent(in) :: str
    integer, intent(in)          :: charge
    integer                      :: i,zstr
    integer                      :: diffmin, diff
!
    if (charge == 0) then
        pxen = pxen_from_specie(str)
    else
        pxen = 0
        zstr = z_from_specie(str)
        diffmin = huge(0)
        do i=1,N_ELEMENTS
           if (zstr == eleminfo(i)%z) then
               diff = abs(charge - eleminfo(i)%charge)
               if (diff == 0) then
                   pxen = i
                   exit
               elseif (diff < diffmin) then
                   diffmin = diff
                   pxen = i
               endif
           endif
        enddo
    endif
!
    end function pxen_from_charge

!------------------------------------------------------------------------------------

    integer function pxen_from_string(str)  result(pxen)
!
!   Dalla specie a pxen. Nel caso di specie cariche assenti fornisce la specie neutra più vicina
!
    USE strutil
    character(len=*), intent(in) :: str
    integer                      :: lens
    integer                      :: charge, kal
!    
    pxen = 0
    lens = len_trim(str)
    if (lens > 4) return   
    call get_charge(str,charge,kal)
    if (kal == 0) return ! str has problem
    pxen = pxen_from_charge(str(1:kal),charge)
!
    end function pxen_from_string

!------------------------------------------------------------------------------------

    integer function pxen_from_label(label) result(pxen)
!
!   Cerca nelle stringa una specie chimica ed estrai il pxen
!
    character(len=*), intent(in) :: label
    character(len_trim(label))   :: lab
    integer                      :: i
    integer                      :: lenl
!
    lab = adjustl(label)
    lenl = len_trim(lab)
    pxen = 0
    do i=min(lenl,4),1,-1
       pxen = pxen_from_specie(lab(1:i))
       if (pxen > 0) exit
    enddo
!
    end function pxen_from_label

!------------------------------------------------------------------------------------

    function specie_from_pxen(z) result(spec)
!
!   Dal puntatore al file xen o dallo z alla specie
!
    character(len=:), allocatable :: spec
    integer, intent(in)           :: z
!
    if (z > 0 .and. z <= N_ELEMENTS) then
        spec = trim(eleminfo(z)%lab)
    else
        spec = ' '
    endif
!
    end function specie_from_pxen

!-------------------------------------------------------------------------------------------------

    integer function number_of_elem(zelem,zval)  result(num)
!
!   Numero di elementi con Z=zelem 
!
    integer, intent(in)               :: zelem   ! number of atoms with Z=zelem
    integer, dimension(:), intent(in) :: zval    ! zval di tutti gli atomi
!
    num = count(zval(:) == zelem)
!
    end function number_of_elem

!-------------------------------------------------------------------------------------------------

    integer function is_element_z(elem,zval) result(ptr)
!
!   esiste la specie con z = zval. If exists return the pointer to elem array
!
    type(element_type), allocatable, intent(in) :: elem(:)
    integer, intent(in)                         :: zval
    integer                                     :: i
    ptr = 0
    do i=1,numelem(elem)
       if (elem(i)%z == zval) then
           ptr = i
           exit
       endif
    enddo
    end function is_element_z

!-------------------------------------------------------------------------------------------------

    integer function is_element_lab(elem,lab) result(ptr)
!
!   esiste la specie con lab. If exists return the pointer to elem array
!
    USE strutil
    type(element_type), allocatable, intent(in) :: elem(:)
    character(len=*), intent(in)                :: lab
    integer                                     :: i
    ptr = 0
    do i=1,numelem(elem)
       if (s_eqi(lab,elem(i)%lab)) then
           ptr = i
           exit
       endif
    enddo
    end function is_element_lab

!-------------------------------------------------------------------------------------------------

   subroutine copy_elem(elem1,elem2)
!
!  Copy elem2 in elem1
!
   type(element_type), dimension(:), allocatable, intent(inout) :: elem1
   type(element_type), dimension(:), allocatable, intent(in)    :: elem2
   integer                                                   :: nel2,nel1
!
   nel2 = numelem(elem2)
   nel1 = numelem(elem1)
   if (nel1 == nel2) then
       if (nel1 /= 0) elem1 = elem2
   else
       if (allocated(elem1)) deallocate(elem1)
       if (nel2 > 0) then
           allocate(elem1(nel2),source=elem2)
       endif
   endif
!
   end subroutine copy_elem

!-------------------------------------------------------------------------------------------------

   subroutine add_element_from_Z(elem,zval,wave,radtype,nw,nsym,add)
!
!  Add new element to array only if it doesn't already esist
!
   use prog_constants, only: DEF_WAVE
   use errormod
   type(element_type), allocatable, intent(inout) :: elem(:)   ! array of elements
   integer, intent(in)                            :: zval      ! z number to add
   real, intent(in)                               :: wave
   integer, intent(in)                            :: radtype   ! radiation type
   integer, intent(in), optional                  :: nw        ! number for content
   logical, intent(out), optional                 :: add       ! true if new element was added
   integer, intent(in), optional                  :: nsym
   integer                                        :: nelem,kel
   integer                                        :: nwadd
   type(error_type)                               :: err
!
   if (zval <= 0) return
   if (present(add)) add = .false.
   kel = is_element(elem,zval)
   if (kel == 0) then   
       nelem = numelem(elem)
       call reallocate_elem(elem,nelem+1,savevet=.true.)
       elem(nelem+1) = chemical_element(z=zval)
       if (present(nw)) elem(nelem+1)%nw = nw
       if (present(nsym)) then
           if (nsym > 0) elem(nelem+1)%nw = nsym*elem(nelem+1)%nw
       endif
       if (present(add)) add = .true.
       !if (present(type)) then 
           call elem_set_radiation(elem(nelem+1),radtype)
       !else
       !    if (nelem > 1) call elem_set_radiation(elem(nelem+1),elem(1)%radtype) ! set type from first element
       !endif
       !if (radtype == RX_SOURCE) call elem_set_nist_factors_s(elem(nelem+1),wave,err)
       call elem_set_nist_factors_s(elem(nelem+1),wave,radtype,err)
   elseif (kel > 0) then
       if (present(nw)) then
           nwadd = nw
           if (present(nsym)) then
               if (nsym > 0) nwadd = nsym*nwadd
           endif
           elem(kel)%nw = elem(kel)%nw + nwadd
       endif
   endif
!
   end subroutine add_element_from_Z

!-------------------------------------------------------------------------------------------------

   subroutine add_element_from_el(elem,elemadd)
   type(element_type), dimension(:), allocatable, intent(inout) :: elem
   type(element_type), dimension(:), allocatable, intent(in)    :: elemadd
   integer                                                      :: neladd, nel, i, nel0
!
   neladd = numelem(elemadd)
   if (neladd == 0) return
   nel = numelem(elem)
   nel0 = nel
   call reallocate_elem(elem,nel+neladd)
   do i=1,neladd
      if (any(elem(:nel0)%ptab == elemadd(i)%ptab)) cycle
      nel = nel + 1
      elem(nel) = elemadd(i)
   enddo
   call reallocate_elem(elem,nel)
!
   end subroutine add_element_from_el

!-------------------------------------------------------------------------------------------------

   subroutine make_elements(elem,ptab,wave,radtype,emerge,nsymtot,occ)
!
!  Create array elements from ptab
!
   use errormod
   type(element_type), dimension(:), allocatable, intent(inout) :: elem
   integer, dimension(:), intent(in)                            :: ptab
   real, intent(in)                                             :: wave
   integer, intent(in)                                          :: radtype
   logical, intent(in)                                          :: emerge
   integer, intent(in)                                          :: nsymtot
   real, dimension(:), intent(in), optional                     :: occ
   type(element_type), dimension(:), allocatable                :: elemnew
   type(error_type)                                             :: err
!
   if (present(occ) .and. nsymtot > 0) then
       call elem_from_atoms(elemnew,ptab,occ*nsymtot)
   else
       call elem_from_atoms(elemnew,ptab)
       if(nsymtot > 0) elemnew%nw = elemnew%nw*nsymtot
   endif
   if (numelem(elemnew) == 0) return
!
   !if (radtype == RX_SOURCE) call elem_set_nist_factors(elemnew,wave,err)
   call elem_set_nist_factors(elemnew,wave,radtype,err)
   call elem_set_radiation(elemnew,radtype)
   call ordina_elements(elemnew,radtype)
   if (emerge) then
       call add_element(elem,elemnew)
   else
       call copy_elem(elem,elemnew)
   endif
!
   end subroutine make_elements

!-------------------------------------------------------------------------------------------------

   subroutine info_specie_from_pxen(zxen,nums,nspec,xspec)
   USE nr
   integer, dimension(:), intent(in)            :: zxen   ! puntatore al file xen per ogni atomo
   integer, intent(out)                         :: nums   ! numero di specie
   integer, dimension(:), intent(out), optional :: nspec  ! numero per ciscuna specie
   integer, dimension(:), intent(out), optional :: xspec  ! puntatore al file xen 
   integer, dimension(0:N_ELEMENTS)             :: zvet
   integer                                      :: zv
   integer, allocatable, dimension(:)           :: iord
   integer                                      :: i
!
   zvet(:) = 0
   do i=1,size(zxen)
      zv = zxen(i)
      zvet(zv) = zvet(zv) + 1
   enddo
!
   nums = 0
   do i=1,N_ELEMENTS
      if (zvet(i) /= 0) then
          nums = nums + 1
          if (present(nspec)) nspec(nums) = zvet(i)
          if (present(xspec)) xspec(nums) = i
      endif
   enddo
!
   if (nums > 0 .and. present(nspec) .and. present(xspec)) then
!
!      ordina per numero atomico
       allocate(iord(nums))
       call indexx(xspec(:nums),iord) 
       xspec(:nums) = xspec(iord)
       nspec(:nums) = nspec(iord)
   endif 
!
   end subroutine info_specie_from_pxen

!-------------------------------------------------------------------------------------------------

    subroutine elem_from_atoms(elem,pxen,occ)
!
!   Estraggo elem dallo pxen di tutti gli atomi
!
    type(element_type), dimension(:), allocatable, intent(inout) :: elem
    integer, dimension(:), intent(in)                            :: pxen
    real, dimension(:), intent(in), optional                     :: occ
    real, dimension(0:N_ELEMENTS)                                :: zvet
    integer                                                      :: zv
    integer :: i
    integer :: nums
!
!   Calcola quanti per ogni specie
    zvet(:) = 0
    if (present(occ)) then
        do i=1,size(pxen)
           zv = pxen(i)
           zvet(zv) = zvet(zv) + occ(i)
        enddo
    else
        do i=1,size(pxen)
           zv = pxen(i)
           zvet(zv) = zvet(zv) + 1
        enddo
    endif
!
!   allocate elem
    nums = count(zvet(1:) /= 0)
    call reallocate_elem(elem,nums,.false.)
!
!   fill elem
    nums = 0
    do i=1,N_ELEMENTS
       if (zvet(i) /= 0) then
           nums = nums + 1
           elem(nums) = eleminfo(i)
           elem(nums)%nw = nint(zvet(i))
       endif
    enddo
!
    end subroutine elem_from_atoms

!-------------------------------------------------------------------------------------------------

    integer elemental function oxidation_number(zval)  result(ox)
!
!   Get the most common oxidation number 
!
    integer, intent(in) :: zval
    select case (zval)
       case (:0, 109:)
         ox = 0
       case (1:108)
         ox = ox_numb(3,zval)
    end select
    end function oxidation_number

!-------------------------------------------------------------------------------------------------

    subroutine get_oxidation_states(zval,oxc,noxc,ox,nox)
!
!   Get the oxidation states
!
    integer, intent(in)                          :: zval
    integer, dimension(:), intent(out), optional :: oxc   ! the most common oxidation states
    integer, intent(out), optional               :: noxc
    integer, dimension(:), intent(out), optional :: ox    ! all oxidation states
    integer, intent(out), optional               :: nox
!
    if (present(oxc) .and. present(noxc)) then
        if (zval > size(ox_numb,2)) then
            noxc = 1
            oxc(1) = 0
        else
            noxc = ox_numb(2,zval)
            oxc(:noxc) = ox_numb(3:noxc+2,zval)
        endif
    endif
!
    if (present(ox) .and. present(nox)) then
        if (zval > size(ox_numb,2)) then
            nox = 1
            ox(1) = 0
        else
            nox = ox_numb(1,zval)
            ox(:nox) = ox_numb(3:nox+2,zval)
        endif
    endif
!
    end subroutine get_oxidation_states

!-------------------------------------------------------------------------------------------------
 
    integer elemental function maximum_valence(zval)  result(mv)
!
!   Maximum valence of the elements
!
    integer, intent(in) :: zval
    integer, dimension(108), parameter :: max_valence = (/            &
!          H                                                  He
            1,                                                 0,     &
!          Li Be                                B  C  N  O  F Ne
            1, 2,                               3, 4, 5, 2, 1, 0,     &
!          Na Mg                               Al Si  P  S Cl Ar
            1, 2,                               3, 4, 5, 6, 7, 2,     &
!           K Ca Sc Ti  V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr
            1, 2, 3, 4, 5, 6, 7, 6, 5, 4, 4, 2, 3, 4, 5, 6, 7, 2,     &    
!          Rb Sr  Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te  I Xe
            1, 2, 3, 4, 5, 6, 7, 8, 6, 6, 4, 2, 3, 4, 5, 6, 7, 8,     &
!          Cs Ba 
            1, 2,                                                     &
!                La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu
                  3, 4, 4, 4, 3, 3, 3, 3, 4, 4, 3, 3, 3, 3, 3,        &
!                   Hf Ta  W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn
                     4, 5, 6, 7, 8, 8, 6, 5, 4, 3, 4, 5, 6, 7, 6,     &
!          Fr Ra
            1, 2,                                                     &
!                Ac Th Pa  U Np Pu Am Cm Bk Cf Es Fm Md No Lr Rf Db Sg Bh Hs
                  3, 4, 5, 6, 7, 8, 7, 8, 4, 4, 4, 3, 3, 3, 3, 4, 5, 6, 7, 8/)
    select case (zval)
       case (:0)
         mv = 0
       case (1:108)
         mv = max_valence(zval)
       case (109:)
         mv = 3
    end select

    end function maximum_valence
!-------------------------------------------------------------------------------------------------

    real elemental function electronegativity(zval)  result(eneg)
    integer, intent(in) :: zval
    real, dimension(103), parameter :: electronegativity_value = (/    &
!          H                                                                                                     He
           2.20,                                                                                                 0.0,     &
!          Li    Be                                                                B     C     N     O     F     Ne
           0.98, 1.57,                                                             2.04, 2.55, 3.04, 3.44, 3.98, 0.0,     &
!          Na    Mg                                                                Al    Si    P     S     Cl    Ar
           0.93, 1.31,                                                             1.61, 1.90, 2.19, 2.58, 3.16, 0.0,     &
!          K     Ca    Sc    Ti    V     Cr    Mn    Fe    Co    Ni    Cu    Zn    Ga    Ge    As    Se    Br    Kr
           0.82, 1.31, 1.36, 1.54, 1.63, 1.66, 1.55, 1.83, 1.88, 1.91, 1.90, 1.65, 1.81, 2.01, 2.18, 2.55, 2.96, 3.00,  &
!          Rb    Sr    Y     Zr    Nb    Mo    Tc    Ru    Rh    Pd    Ag    Cd    In    Sn    Sb    Te    I     Xe
           0.82, 0.95, 1.22, 1.33, 1.6,  2.16, 1.9,  2.2,  2.28, 2.20, 1.93, 1.69, 1.78, 1.96, 2.05, 2.1,  2.66, 2.60,  &
!          Cs    Ba 
           0.96, 0.98,                                                &
!                La    Ce    Pr    Nd    Pm    Sm    Eu    Gd    Tb    Dy    Ho    Er    Tm    Yb    Lu
                 1.1,  1.12, 1.13, 1.14, 1.13, 1.17, 1.2,  1.2,  1.1,  1.22, 1.23, 1.24, 1.25, 1.1,  1.27,              &
!                Hf    Ta    W     Re    Os    Ir    Pt    Au    Hg    Tl    Pb    Bi    Po    At    Rn
                 1.3,   1.5, 2.36, 1.9,  2.2,  2.20, 2.28, 2.54, 2.00, 1.62, 1.87, 2.02, 2.0,  2.2,  2.2,                &  
!          Fr    Ra
           0.7,  0.92,                                                                                                  &
!                Ac    Th    Pa    U     Np    Pu    Am    Cm    Bk    Cf    Es    Fm    Md    No    Lr
                 1.1,  1.3,  1.5,  1.38, 1.36, 1.28, 1.13, 1.28, 1.3,  1.28, 1.3,  1.3,  1.3,  1.3,  1.3  /)
    select case (zval)
       case (:0)
         eneg = 0
       case (1:103)
         eneg = electronegativity_value(zval)
       case (104:)
         eneg = 1.3
    end select

    end function electronegativity

!-------------------------------------------------------------------------------------------------

    logical function equal_elements(elem1,elem2)  result(equal)
!
!   equal = true se le due liste di elementi sono diverse
!
    type(element_type), dimension(:), allocatable, intent(in) :: elem1,elem2
    integer                                                   :: nelem1,nelem2
    integer                                                   :: i,j
    logical                                                   :: eq_elem
!
    nelem1 = numelem(elem1)
    nelem2 = numelem(elem2)
    equal = .false.
    if (nelem1 == nelem2) then
        do i=1,nelem1
           do j=1,nelem2
              eq_elem = equal_element(elem1(i),elem2(j))
              if (eq_elem) exit
           enddo
           if (.not.eq_elem) exit
        enddo
        equal = eq_elem
    endif
!
    end function equal_elements

!-------------------------------------------------------------------------------------------------

    logical function equal_element(elem1,elem2) result(equal)
!
!   equal = true se i 2 elementi sono uguali
!
    USE strutil
    type(element_type), intent(in) :: elem1,elem2
    equal = s_eqi(elem1%lab,elem2%lab) .and. (elem1%nw == elem2%nw)
    end function equal_element

!-------------------------------------------------------------------------------------------------

    subroutine read_chemformula(sform,ier,elem,strfor,ord,fform,hide1)
!
!   Convert string in elem, elements are ordered according to file of elements
!
    character(len=*), intent(in)                                         :: sform  ! chemical formula
    integer, intent(out)                                                 :: ier    ! error on reading formula on character ier
    type(element_type), dimension(:), allocatable, intent(out), optional :: elem   ! elementi nella formula
    character(len=:), allocatable, intent(out), optional                 :: strfor ! formula in format with space (es. C 1 H 4)
    logical, intent(in), optional                                        :: ord    ! Hill order for formula
    integer, intent(in), optional                                        :: fform  ! format for formula
    logical, intent(in), optional                                        :: hide1  ! hide 1
    integer                                                              :: nelem
    real, dimension(N_ELEMENTS)                                          :: vsp
    integer                                                              :: i
    type(element_type), dimension(:), allocatable                        :: elf
    logical                                                              :: hide11
!
    if (len_trim(sform) == 0) then
        ier = 1
        return
    endif
!
    vsp(:) = 0
    call read_string_formula(sform,vsp,ier)
    if (ier == 0 .and. (present(elem) .or. present(strfor))) then
        nelem = count(vsp /= 0) 
        if (nelem > 0) then
            call reallocate_elem(elf,nelem) 
            nelem = 0
            do i=1,N_ELEMENTS
               if (vsp(i) > 0) then
                   nelem = nelem + 1
                   elf(nelem) = eleminfo(i)
                   !elf(nelem)%nw = nint(vsp(i))
                   elf(nelem)%nw = vsp(i)    ! save real number 
                   !write(0,'(a,a,f0.3)')'SPEC='//trim(elf(nelem)%lab),' n=',elf(nelem)%nw
               endif
            enddo
        endif
        if (present(strfor)) then
            if (present(hide1)) then
                hide11 = hide1
            else
                hide11 = .false.
            endif
            if (present(ord)) then
                if (present(fform)) then
                    call chemical_formula(elf,strfor,fform=fform,ord=ord,hide1=hide11)
                else
                    call chemical_formula(elf,strfor,fform=1,ord=ord,hide1=hide11)
                endif
            else
                if (present(fform)) then
                    call chemical_formula(elf,strfor,fform=fform,ord=.false.,hide1=hide11)
                else
                    call chemical_formula(elf,strfor,fform=1,ord=.false.,hide1=hide11)
                endif
            endif
        endif
        if (present(elem) .and. nelem > 0) then
            call reallocate_elem(elem,nelem) 
            elem(:) = elf(:)
        endif
    endif
!
    end subroutine read_chemformula

!-------------------------------------------------------------------------------------------------

    subroutine build_scatterers(sform, wave, radtype, elem, ier)
    use errormod
    character(len=*), intent(in)                               :: sform   ! chemical formula
    real, intent(in)                                           :: wave    ! wavelength
    integer, intent(in)                                        :: radtype ! radiation type
    type(element_type), dimension(:), allocatable, intent(out) :: elem    ! elementi nella formula
    integer, intent(out)                                       :: ier
    type(error_type)                                           :: err
!    
    call read_chemformula(sform,ier,elem)
    if (ier /= 0) return
!    if (radtype == RX_SOURCE) call elem_set_nist_factors(elem,wave,err)
    call elem_set_nist_factors(elem,wave,radtype,err)
    if (err%signal) ier = 1
!
    end subroutine build_scatterers

!-------------------------------------------------------------------------------------------------

    subroutine elem_set_nist_factors_s(elem,wave,radtype,err)
    use nist_mod
    use errormod
    type(element_type), intent(inout) :: elem
    real, intent(in)                  :: wave
    integer, intent(in)               :: radtype
    type(error_type), intent(out)     :: err
    real                              :: f1,f2,mac
!
    elem%f1 = 0.0
    elem%f2 = 0.0
    elem%mac = 0.0
    if (radtype == RX_SOURCE) then
        call get_nist_parameters(elem%z,wave,f1,f2,mac,err)
        if (err%signal) return
        elem%f1 = f1
        elem%f2 = f2
        elem%mac = mac
    endif
!
    end subroutine elem_set_nist_factors_s

!-------------------------------------------------------------------------------------------------

    subroutine elem_set_nist_factors_v(elem,wave,radtype,err)
    use nist_mod
    use errormod
    type(element_type), dimension(:), allocatable, intent(inout) :: elem
    real, intent(in)                                             :: wave
    integer, intent(in)                                          :: radtype
    type(error_type), intent(out)                                :: err
    integer                                                      :: i
!
    do i=1,numelem(elem)
       call elem_set_nist_factors_s(elem(i),wave,radtype,err)
       if (err%signal) exit
    enddo
!
    end subroutine elem_set_nist_factors_v

!-------------------------------------------------------------------------------------------------
 
    logical function z_contains_specie_string(zval, specv)  result(okcont)
    integer, dimension(:), intent(in) :: zval
    character(len=*), dimension(:), intent(in) :: specv
    okcont = check_container(zval,z_from_specie(specv))
    end function z_contains_specie_string

!------------------------------------------------------------------------------------------

    subroutine hill_order(spec,ord)
!
!   Generate index for chemical elements according to Hill notation
!
    use strutil
    character(len=*), dimension(:), intent(in)              :: spec
    integer, dimension(size(spec)), intent(out)             :: ord
    !character(len=len(spec(1))), dimension(size(spec)) :: specv
    character(len=len(spec(1))), dimension(:), allocatable  :: specv
    integer :: i
!
    if (size(spec) == 1) then
        ord(1) = 1
    else
        allocate(specv(size(spec)))
        do i=1,size(spec)
           if (s_eqidb(trim_charge(spec(i)),'C')) then
               specv(i) = '1'
           elseif (s_eqidb(trim_charge(spec(i)),'H')) then
               specv(i) = '2'
           else
               specv(i) = spec(i)
           endif
        enddo
        call svec_sort_heap_a_index(size(specv),specv,ord)
    endif
!
    end subroutine hill_order

!-------------------------------------------------------------------------------------------------

    subroutine chemical_formula_sn(specv,nspecv,fstring,fform,ord,hide1,hidecharge)
    USE strutil
    USE nr
    USE arrayutil
    character(len=*), dimension(:), intent(in) :: specv
    real, dimension(:), intent(in)             :: nspecv
    character(len=:), allocatable, intent(out) :: fstring
    integer, intent(in), optional              :: fform
    logical, intent(in), optional              :: ord
    logical, optional, intent(in)              :: hide1       ! 1 must be hidden if true. e.g. 'Na' and not 'Na1'
    logical, optional, intent(in)              :: hidecharge  ! 'O' and not 'O2-'
    integer                                    :: i,j
    integer                                    :: lens, numl
    character(len=10)                          :: strnum
    integer                                    :: kform
    integer, dimension(size(specv))            :: iord  !, zval
    logical                                    :: kord
    integer                                    :: intval
    logical                                    :: hide11,hidecharge1
!
    if (present(fform)) then
        kform = fform
    else
        kform = 1
    endif
    if (present(ord)) then
        kord = ord
    else
        kord = .true.
    endif
    if (present(hide1)) then
        hide11 = hide1
    else
        hide11 = .false.
    endif
    if (present(hidecharge)) then
        hidecharge1 = hidecharge
    else
        hidecharge1 = .false.
    endif
    numl = size(specv)
    fstring = ' '
    if (numl > 0) then
        if (kord) then
            call hill_order(specv,iord)
        else
            iord = (/(i,i=1,numl)/)
        endif
        lens = len(fstring)
        do j=1,numl
           i = iord(j)
           if (nspecv(i) == 0) cycle
           intval = nint(nspecv(i))
           if (abs(intval - nspecv(i)) > 0.05 .or. intval == 0) then
               strnum = trim(r_to_s(nspecv(i),2))
               call s_trim_zeros(strnum)
           else
               if (intval == 1 .and. hide11) then
                   strnum = ' '
               else
                   strnum = trim(i_to_s(intval))
               endif
           endif
           if (hidecharge1) then
               fstring = trim(fstring)//' '//trim_charge(specv(i))
           else
               fstring = trim(fstring)//' '//trim(specv(i))
           endif
           if (kform == 1) then
               fstring = trim(fstring)//' '//trim(strnum)
           else
               fstring = trim(fstring)//trim(strnum)
           endif
           !if (kform == 1) then
           !    fstring = trim(fstring)//' '//trim(specv(i))//' '//trim(strnum)
           !else
           !    fstring = trim(fstring)//' '//trim(specv(i))//trim(strnum)
           !endif
        enddo
        fstring = adjustl(fstring)
    endif
!
    end subroutine chemical_formula_sn

!-------------------------------------------------------------------------------------------------

    
    subroutine chemical_formula_s(specv,fstring,fform,ord)    !!! FIXME - forse non serve
    USE strutil
    character(len=*), dimension(:), intent(in)    :: specv
    character(len=:), allocatable, intent(out)    :: fstring
    integer, intent(in), optional                 :: fform
    logical, intent(in), optional                 :: ord
    integer                                       :: numl
    type(element_type), dimension(:), allocatable :: elemf
    logical                                       :: kord
!
    numl = size(specv)
    if (numl > 0) then
        if (present(ord)) then
            kord = ord
        else
            kord = .true.
        endif
        call elem_from_atoms(elemf,pxen_from_specie(specv))
        if (present(fform)) then
            call chemical_formula_el(elemf,fstring,fform,kord)
        else
            call chemical_formula_el(elemf,fstring,fform,kord)
        endif
    else
        fstring = ' '
    endif
!
    end subroutine chemical_formula_s

!-------------------------------------------------------------------------------------------------

    subroutine chemical_formula_el(elem,string,fform,ord,hide1,hidecharge)
!
!   Convert elements in formula string
!
    USE strutil
    type(element_type), dimension(:), allocatable, intent(in) :: elem
    character(len=:), allocatable, intent(inout)              :: string
    integer, intent(in), optional                             :: fform
    logical, intent(in), optional                             :: ord
    logical, intent(in), optional                             :: hide1,hidecharge
    logical                                                   :: kord,hide11,hidecharge1
!
    if (numelem(elem) > 0) then
        if (present(ord)) then
            kord = ord
        else
            kord = .true.
        endif
        if (present(hide1)) then
            hide11 = hide1
        else
            hide11 = .false.
        endif
        if (present(hidecharge)) then
            hidecharge1 = hidecharge
        else
            hidecharge1 = .false.
        endif
        if (present(fform)) then
            call chemical_formula_sn(elem%lab,elem%nw,string,fform,kord,hide11,hidecharge1)
        else
            call chemical_formula_sn(elem%lab,elem%nw,string,1,kord,hide11,hidecharge1)
        endif
    else
        string = ' '
    endif
!
    end subroutine chemical_formula_el

!-------------------------------------------------------------------------------------------------

    recursive subroutine read_string_formula(sform,vsp,ier)
!
!   recursive function to convert a formula string in vsp vector
!
    USE strutil
    character(len=*), intent(in) :: sform
    real, dimension(:), intent(inout) :: vsp
    integer, intent(out)         :: ier
    integer                      :: lens
    character(len=1)             :: ch
    integer                      :: i
    integer                      :: cini,cfin
    integer                      :: kal, kdig
    integer, parameter           :: MAXDIGIT = 10
    integer, parameter           :: MAXLAB = 10
    character(len=MAXDIGIT)      :: digit
    character(len=MAXLAB)        :: label
    integer                      :: len
    integer                      :: ieri
    real                         :: num
    integer                      :: lastch
    logical                      :: readf, lsign
    integer, parameter           :: CALPHA=1,CDIGIT=2,CSIGN=3,CBLANK=4,CBRACKET=5,CPOINT=6
    character(len=len_trim(sform)) :: sformb
    integer                        :: pxen
    logical                        :: kpr = .false.
    real                           :: numb
    real, dimension(size(vsp))     :: vsp1
    integer                        :: bpos
!
    ier = 0
    lens = len_trim(sform)
    if (lens == 0) return
    !vsp(:) = 0
!   
    kal = 0
    kdig = 0
    cini = 0
    cfin = 0
    lsign = .false.
    lastch = 0
    label = ' '
    i = 0
    do 
       i = i + 1
       if (i > lens) exit
       ch = sform(i:i)
       if (ch_is_alpha(ch)) then
           if (cini == 0) then
               cini = i
           else
               readf = .false.
               if (kal == 2) then
                   readf = .true.
               elseif (kal == 1) then
!
                   if (lastch == CALPHA) then
!                      2 caratteri consecutivi. Valuta! 
!                      Es. SB e sB diventano S e B. Sb e sb diventano Sb
                       if (ch_is_upper(ch)) then
                           readf = .true.
                           if (i == lens) then   ! ultimo carattere
                               if (pxen_from_string(ch) == 0) readf = .false.  ! es. CL
                           else
                               if (.not.ch_is_alpha(sform(i+1:i+1))) then           ! il carattere successivo non e' una lettera
                                   if (pxen_from_string(ch) == 0) readf = .false.  ! es. 'CL C' ma non 'CLa'
                               endif
                           endif
                      !     if (pxen_from_string(ch) /= 0) then  ! controlla anche che ch sia ultimo o abbia spazio dopo
                      !     readf = .true.
                      !     endif
                       endif
                   else
                       readf = .true.
                   endif
               endif
               if (readf) then
                   if (kdig > 0 .and. digit(:kdig) /= '.') then  
                       ieri = s_to_r(digit(:kdig),num,len)
                   else
                       num = 1
                   endif
                   pxen = pxen_from_string(label(:cfin))
                   if (kpr) write(0,'(a,i0,a,f0.1)')'pxen di '//label(:cfin)//'=',pxen,' num=',num
                   if (pxen > 0) then
                       vsp(pxen) = vsp(pxen) + num
                       cini = i
                       cfin = 0
                       lsign = .false.
                       kal = 0
                       kdig = 0
                       label = ' '
                   else
                       ier = cini
                       exit
                   endif
               endif
           endif
           cfin = cfin + 1
           if (cfin - cini + 1 > MAXLAB) then
               cfin = MAXLAB
               ier = i
               exit
           endif
           kal = kal + 1
           label(kal:kal) = ch
           lastch = CALPHA
       elseif (ch_is_digit(ch) .or. ch == '.') then
           if (cini == 0 .or. (kdig > 0 .and. lastch == CBLANK) .or. kdig == MAXDIGIT .or.    &
               (kdig > 0 .and. lastch == CPOINT .and. ch == '.')) then
!
!              la prima cifra è un numero (es. 1N) o secondo numero dopo spazio (Ca2+ 2 3...)
!              o piu' punti consecutivi (Ca..)
               ier = i
               exit
           endif
           kdig = kdig + 1
           digit(kdig:kdig) = ch
           if (ch == '.') then
               lastch = CPOINT
           else
               lastch = CDIGIT
           endif
       elseif (ch == '+' .or. ch == '-') then
           if (cini == 0 .or. lsign) then
               ier = i
               exit
           else
               cfin = cfin + kdig + 1
               if (cfin - cini + 1 > MAXLAB) then
                   ier = i
                   exit
               endif
               label = trim(label)//digit(:kdig)//ch
               digit(:kdig) = ' '
               kdig = 0
           endif
           lsign = .true.
           lastch = CSIGN
       elseif (ch_is_space(ch)) then
           lastch = CBLANK
       elseif (ch == '(' .or. ch == '[' .or. ch == '{') then 
           call get_string_in_brackets(sform(i:),sformb,bpos,ier)
           if (ier ==  0) then
               vsp1(:) = 0
               call read_string_formula(sformb,vsp1,ier)
               if (ier == 0) then
                 !  i = i + len_trim(sformb) + 1  ! bracket position
                   i = i + bpos - 1  ! bracket position
                   call get_next_number(sform,i,numb,ier=ieri)  ! get number after bracket
                   if (ieri == 0) then    ! numero trovato
                       vsp(:) = vsp(:) + vsp1(:)*numb
                   else                   ! numero non trovato
                       vsp(:) = vsp(:) + vsp1(:)
                   endif
               else  ! errore nella parentesi
                   ier = i+ier
                   exit
               endif
           else   ! parentesi non chiusa
               ier = i
               exit
           endif
       else
           ier = i
           exit
       endif
    enddo
!
    if (cfin > 0) then
        if (kdig > 0 .and. digit(:kdig) /= '.') then  
            ieri = s_to_r(digit(:kdig),num,len)
        else
            num = 1
        endif
        pxen = pxen_from_string(label(:cfin))
        if (pxen > 0) then
            vsp(pxen) = vsp(pxen) + num
            if (kpr) write(0,'(a,i0,a,f0.1)')'pxen di '//label(:cfin)//'=',pxen_from_string(label(:cfin)),' num=',num
        else
            ier = cini
        endif
    endif
    if (ier > 0 .and. kpr) then
        write(0,*)'error on ch.'//sform(ier:ier),ier
    endif
!
    end subroutine read_string_formula

!-------------------------------------------------------------------------------------------------

    real elemental function average_volume(zval)  result(vol)
!
!   Get the average volume of element with atomic number zval
!
    integer, intent(in) :: zval
    select case (zval)
       case (:0)
          vol = 0
       case (1:100)
          vol = average_vol(zval)
       case (101:)
          vol = 70
    endselect
    end function average_volume

!-------------------------------------------------------------------------------------------------

    subroutine save_elem_bin(unitp,elem,strelem)
!
!   Save elements on binary file
!
    USE strutil
    integer, intent(in)                                       :: unitp
    type(element_type), dimension(:), allocatable, intent(in) :: elem
    character(len=*), intent(in)                              :: strelem
    integer                                                   :: nelem
    nelem = numelem(elem)
    write(unitp)nelem
    if (nelem > 0) write(unitp) elem(:)
    call write_string_bin(unitp,strelem)
    end subroutine save_elem_bin

!-------------------------------------------------------------------------------------------------

    subroutine read_elem_bin(unitp,elem,strelem,err)
!
!   Read elements from binary file
!
    USE errormod
    USE strutil
    integer, intent(in)                                        :: unitp
    type(element_type), dimension(:), allocatable, intent(out) :: elem
    character(len=:), allocatable, intent(out)                 :: strelem
    type(error_type), intent(out)                              :: err
    integer                                                    :: ier
    integer                                                    :: nelem
!
    ier = 0
    read(unitp,iostat=ier) nelem
    if (nelem > 0 .and. ier == 0) then
        call reallocate_elem(elem,nelem)
        read(unitp,iostat=ier)elem(:)
    endif
    if (ier == 0) then
        ier = read_string_bin(unitp,strelem)
    else
        call err%set('Error on reading cell content')
    endif
!
    end subroutine read_elem_bin

!-------------------------------------------------------------------------------------------------

    real function nasym_unit_int(elem,nsymop,radtype)  result(nasym)
!
!   Number of atoms in the asymmetric unit
!
    type(element_type), dimension(:), allocatable, intent(in) :: elem
    integer, intent(in)                                       :: nsymop
    integer, intent(in)                                       :: radtype
!
    if (radtype == NEUTRON_SOURCE) then
        nasym = nasym_unit_lg(elem,nsymop,.false.)
    else
        nasym = nasym_unit_lg(elem,nsymop,.true.)
    endif
!
    end function nasym_unit_int

!-------------------------------------------------------------------------------------------------

    real function nasym_unit_lg(elem,nsymop,excludeH)  result(nasym)
!
!   Number of atoms in the asymmetric unit
!
    type(element_type), dimension(:), allocatable, intent(in) :: elem
    integer, intent(in)                                       :: nsymop
    logical, intent(in)                                       :: excludeH 
!corr    logical                                                   :: excludeHat
!
    if (numelem(elem) == 0) then
        nasym = 0
        return
    endif
!
!corr    if (present(excludeH)) then
!corr        excludeHat = excludeH
!corr    else
!corr        excludeHat = .false.
!corr    endif
!corr    if (excludeHat) then
    if (excludeH) then
        nasym = sum(elem%nw,mask=elem%z /= H_at)
    else
        nasym = sum(elem%nw)
    endif
    nasym = nasym/real(nsymop)
    if (nasym < 1) nasym = 1
!
    end function nasym_unit_lg

!--------------------------------------------------------------------------------------------------

    real function s3s2_value(elem,wavetype,ncentr) result(s3s2)
    type(element_type), dimension(:), allocatable, intent(in) :: elem
    integer, intent(in)                                       :: wavetype
    integer, intent(in), optional                             :: ncentr
    integer :: nelem,i 
    real :: ss,s2,s3
!
    nelem = numelem(elem)
    if (nelem > 0) then
        s2 = 0.0
        s3 = 0.0
!corr        if (wavetype == 0) then    ! X-Ray
        select case (wavetype)
          case (RX_SOURCE)
            do i=1,nelem
               ss = elem(i)%nw*elem(i)%z*elem(i)%z
               s2 = s2 + ss
               s3 = s3 + ss*elem(i)%z
            enddo

          case (NEUTRON_SOURCE)
            do i=1,nelem
               ss = elem(i)%nw*elem(i)%fact*elem(i)%fact
               s2 = s2 + ss
               s3 = s3 + ss*elem(i)%fact
            enddo

          case (ELECTRON_SOURCE)
            do i=1,nelem   !!!!TOFIX with zeff
               ss = elem(i)%nw*elem(i)%z*elem(i)%z
               s2 = s2 + ss
               s3 = s3 + ss*elem(i)%z
            enddo

        end select
        s3s2 = s3/sqrt(s2**3)
        if (present(ncentr)) then
            if (ncentr > 0) then
                s3s2 = sqrt(s3s2*s3s2*ncentr)
            endif
        endif
    else
        s3s2 = 0.0
    endif
!
    end function s3s2_value

!--------------------------------------------------------------------------------------------------

    real function at_scatt0_scalar(elem,radtype) result(sf)
!
!   Compute scattering factor for rho2 = rho^2 = (sin(theta)/lambda)^2 = 0
!
    type(element_type), intent(in) :: elem
    integer, intent(in)            :: radtype
!
    sf = at_scatt_scalar(elem,0.0,radtype)   ! = Z for X-rays
!corr    sf = elem%cl 
!corr    if (radtype /= NEUTRON_SOURCE) then
!corr        sf = sf + sum(elem%al)   ! = Z
!corr    endif
!
    end function at_scatt0_scalar

!--------------------------------------------------------------------------------------------------

    function at_scatt0_vect(elem,radtype) result(sf)
!
!   Compute scattering factor for rho2 = rho^2 = (sin(theta)/lambda)^2 = 0
!
    type(element_type), dimension(:), intent(in) :: elem
!    real, intent(in)               :: rho2
    integer, intent(in)            :: radtype
    real, dimension(size(elem)) :: sf
    integer                     :: i
!
    do i=1,size(elem)
       sf(i) = at_scatt0_scalar(elem(i),radtype)
    enddo
!
    end function at_scatt0_vect

!--------------------------------------------------------------------------------------------------

    real function at_scatt_scalar(elem,rho2,radtype) result(sf)
!
!   Compute scattering factor. rho2 = rho^2 = (sin(theta)/lambda)^2
!
    use scatt_params
    use type_constants, only:DP
    type(element_type), intent(in) :: elem
    real, intent(in)               :: rho2
    integer, intent(in)            :: radtype
!
    select case(radtype)
      case (RX_SOURCE)
        sf = elem%cl + sum(elem%al*exp(-elem%bs*rho2))
      case (NEUTRON_SOURCE)
        sf = elem%cl 
      case (ELECTRON_SOURCE)
        sf = real(fact_electron(elem%factE,real(rho2,DP),ELECTRON_SCATT_TYPE))
    end select
!
    end function at_scatt_scalar

!--------------------------------------------------------------------------------------------------

    function at_scatt_vect(elem,rho2,radtype) result(sf)
!
!   Compute scattering factor. rho2 = rho^2 = (sin(theta)/lambda)^2
!
    type(element_type), dimension(:), intent(in) :: elem
    real, intent(in)               :: rho2
    integer, intent(in)            :: radtype
    real, dimension(size(elem)) :: sf
    integer                     :: i
!
    do i=1,size(elem)
       sf(i) = at_scatt_scalar(elem(i),rho2,radtype)
    enddo
!
    end function at_scatt_vect

!--------------------------------------------------------------------------------------------------

    subroutine set_neutron_factor(elem,fact)
    type(element_type), intent(inout) :: elem
    real, intent(in)                  :: fact
    elem%fact = fact
    end subroutine set_neutron_factor

!--------------------------------------------------------------------------------------------------

    logical function is_organic_el(z)
!
!   true if z is organic
!
    integer, intent(in) :: z
    is_organic_el = any(org_elements(:) == z)
!
    end function is_organic_el

!--------------------------------------------------------------------------------------------------

   logical function order_is_ok(z1,z2) result(ok)
!
!  Check order of atoms for formula
!
   integer, intent(in) :: z1,z2
!
   ok = .true.
   if (z1 == z2) return
!
!  H after
   if (z2 == H_at) return 
   if (z1 == H_at) then
       ok = .false.
       return
   endif
!
   if (is_organic_el(z1) .and. is_organic_el(z2)) then
!
!      Apply Hill rule
       if (z1 < z2) return    
   else
       if (z2 < z1) return    ! es. Si-O
   endif
!
   ok = .false.
!
   end function order_is_ok

!--------------------------------------------------------------------------------------------------

   integer function getmin_el(elem,excludeH)
!
!  Get location of minimum Z in elem
!
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   logical, intent(in)                                       :: excludeH
   integer, dimension(1) :: loc
!
   getmin_el = 0
   if (numelem(elem) == 0) return
   if (numelem(elem) == 1) then
       getmin_el = 1
       return
   endif
!
   if (excludeH) then
       loc = minloc(elem%z,mask=elem%z /= H_at)
   else
       loc = minloc(elem%z)
   endif
   getmin_el = loc(1)
!
   end function getmin_el

!--------------------------------------------------------------------------------------------------

   integer function getmax_el(elem)
!
!  Get location of maximum Z in elem
!
   type(element_type), dimension(:), allocatable, intent(in) :: elem
   integer, dimension(1) :: loc
!
   getmax_el = 0
   if (numelem(elem) == 0) return
   if (numelem(elem) == 1) then
       getmax_el = 1
       return
   endif
!
   loc = maxloc(elem%z)
   getmax_el = loc(1)
!
   end function getmax_el

!--------------------------------------------------------------------------------------------------

   integer function group_number(zval)  result(gn)
!
!  Group into periodic table
!
   integer, intent(in) :: zval
!
   select case (zval)
     case (1)         ! H
      gn = 1

     case (2)         ! He
      gn = 18

     case (3:4)       ! Li-Be
      gn = zval - 2

     case (5:10)      ! B-Ne
      gn = zval + 8

     case (11:12)     ! Na-Mg
      gn = zval - 10

     case (13:18)     ! Al-Ar
      gn = zval

     case (19:56)     ! K-Ba
      gn = mod(zval,18)
      if (gn == 0) gn = 18

     case (57:70, 89:102)  ! block f
      gn = 3

     case (71:88)     ! Lu-Ra
      gn = mod(zval-14,18)
      if (gn == 0) gn = 18

     case (103:118)   ! Lr-Uuo
      gn = mod(zval-28,18)
      if (gn == 0) gn = 18
   end select
!
   end function group_number

!--------------------------------------------------------------------------------------------------

   integer function radiation_code(wave) result(rcode)
   USE prog_constants
   real, intent(in) :: wave
   real, parameter  :: EPS = 0.001
!
   rcode = 0
   if (abs(wave - CUwave(1)) < EPS) rcode = 1
   if (abs(wave - Cu_WAVE) < EPS) rcode = 1
   if (abs(wave - MOwave(1)) < EPS) rcode = 2
   if (abs(wave - MO_WAVE) < EPS) rcode = 2
   if (abs(wave - CRwave(1)) < EPS) rcode = 3
   if (abs(wave - COwave(1)) < EPS) rcode = 4
   if (abs(wave - Fewave(1)) < EPS) rcode = 5
!
   end function radiation_code

!--------------------------------------------------------------------------------------------------

   real function ma_coeff(z,wave)
!
!  Obsolte function
!  Mass attenuation coefficients (cm2 g-1) (ITC Vol C: table 4.2.4.3 pag. 230-236)
!
   use arrayutil
   use nr
   integer, intent(in) :: z
   real, intent(in)    :: wave
   real, parameter     :: ang_to_mev = 0.0123984193 ! h*c*10^-6 = 4135667516 × 10^-15 (eV*s) * 299792458 (m/s) * 10^-6
!   integer             :: pose
!   real                :: dy
   real, dimension(24) :: yd2
!
!  Energy (MeV). Kα is the weighted average value, calculated as (2 Kα1 + Kα2 )/3.
   real, dimension(24) :: energy = [  &
!  Ti-Kα     Ti-Kβ1    Cr-Kα     Mn-Kα     Cr-Kβ1    Fe-Kα     Mn-Kβ1    Co-Kα     Fe-Kβ1    Ni-Kα     Co-Kβ1    Cu-Kα
   4.509E-03,4.932E-03,5.412E-03,5.895E-03,5.947E-03,6.400E-03,6.490E-03,6.925E-03,7.058E-03,7.472E-03,7.649E-03,8.041E-03,   &
!  Ni-Kβ1    Zn-Kα     Cu-Kβ1    Zn-Kβ1    Mo-Kα     Mo-Kβ1    Rh-Kα     Pd-Kα     Ag-Kα     Rh-Kβ1    Pd-Kβ1    Ag-Kβ1
   8.265E-03,8.631E-03,8.905E-03,9.572E-03,1.744E-02,1.961E-02,2.017E-02,2.112E-02,2.210E-02,2.272E-02,2.382E-02,2.494E-02    ]
!
!  Mass attenuation coefficients (cm2 g-1)
   real, dimension(24,98), parameter :: mac_itc = reshape([             &
   4.33E-01,4.21E-01,4.12E-01,4.05E-01,4.05E-01,4.00E-01,4.00E-01,3.97E-01,3.96E-01,3.94E-01,3.93E-01,3.91E-01,   &     !Z=1 (H)
   3.90E-01,3.89E-01,3.88E-01,3.86E-01,3.73E-01,3.70E-01,3.69E-01,3.68E-01,3.67E-01,3.66E-01,3.65E-01,3.63E-01,   &
   7.12E-01,5.92E-01,4.98E-01,4.31E-01,4.25E-01,3.81E-01,3.74E-01,3.43E-01,3.35E-01,3.14E-01,3.07E-01,2.92E-01,   &     !Z=2 (He)
   2.85E-01,2.74E-01,2.68E-01,2.55E-01,2.02E-01,1.97E-01,1.96E-01,1.94E-01,1.93E-01,1.92E-01,1.90E-01,1.89E-01,   &
   2.18E+00,1.68E+00,1.30E+00,1.03E+00,1.01E+00,8.39E-01,8.10E-01,6.93E-01,6.63E-01,5.84E-01,5.55E-01,5.00E-01,   &     !Z=3 (Li)
   4.73E-01,4.36E-01,4.12E-01,3.64E-01,1.98E-01,1.87E-01,1.85E-01,1.82E-01,1.79E-01,1.77E-01,1.74E-01,1.72E-01,   &
   6.00E+00,4.56E+00,3.44E+00,2.66E+00,2.59E+00,2.09E+00,2.01E+00,1.67E+00,1.58E+00,1.35E+00,1.27E+00,1.11E+00,   &     !Z=4 (Be)
   1.03E+00,9.23E-01,8.53E-01,7.16E-01,2.56E-01,2.29E-01,2.24E-01,2.16E-01,2.09E-01,2.05E-01,2.00E-01,1.95E-01,   &
   1.33E+01,1.01E+01,7.59E+00,5.84E+00,5.69E+00,4.55E+00,4.37E+00,3.59E+00,3.39E+00,2.87E+00,2.67E+00,2.31E+00,   &     !Z=5 (B)
   2.14E+00,1.89E+00,1.73E+00,1.41E+00,3.68E-01,3.09E-01,2.98E-01,2.81E-01,2.67E-01,2.59E-01,2.47E-01,2.37E-01,   &
   2.62E+01,1.99E+01,1.50E+01,1.16E+01,1.12E+01,8.99E+00,8.62E+00,7.07E+00,6.68E+00,5.62E+00,5.24E+00,4.51E+00,   &     !Z=6 (C)
   4.15E+00,3.65E+00,3.33E+00,2.69E+00,5.76E-01,4.58E-01,4.35E-01,4.02E-01,3.74E-01,3.58E-01,3.35E-01,3.15E-01,   &
   4.30E+01,3.28E+01,2.47E+01,1.91E+01,1.86E+01,1.49E+01,1.42E+01,1.17E+01,1.10E+01,9.29E+00,8.66E+00,7.44E+00,   &     !Z=7 (N)
   6.85E+00,6.01E+00,5.48E+00,4.42E+00,8.45E-01,6.45E-01,6.07E-01,5.51E-01,5.03E-01,4.77E-01,4.37E-01,4.04E-01,   &
   6.52E+01,4.99E+01,3.78E+01,2.92E+01,2.84E+01,2.28E+01,2.19E+01,1.80E+01,1.70E+01,1.43E+01,1.33E+01,1.15E+01,   &     !Z=8 (O)
   1.05E+01,9.25E+00,8.42E+00,6.78E+00,1.22E+00,9.08E-01,8.48E-01,7.60E-01,6.85E-01,6.44E-01,5.82E-01,3.29E-01,   &
   8.84E+01,6.78E+01,5.15E+01,3.99E+01,3.89E+01,3.13E+01,3.00E+01,2.47E+01,2.33E+01,1.97E+01,1.83E+01,1.58E+01,   &     !Z=9 (F)
   1.45E+01,1.28E+01,1.16E+01,9.35E+00,1.63E+00,1.19E+00,1.11E+00,9.84E-01,8.79E-01,8.22E-01,7.35E-01,6.60E-01,   &
   1.26E+02,9.72E+01,7.41E+01,5.76E+01,5.61E+01,4.52E+01,4.34E+01,3.58E+01,3.38E+01,2.85E+01,2.66E+01,2.29E+01,   &     !Z=10 (Ne)
   2.11E+01,1.86E+01,1.69E+01,1.36E+01,2.35E+00,1.69E+00,1.57E+00,1.39E+00,1.23E+00,1.15E+00,1.02E+00,9.06E-01,   &
   1.61E+02,1.24E+02,9.49E+01,7.40E+01,7.21E+01,5.82E+01,5.59E+01,4.62E+01,4.37E+01,3.69E+01,3.45E+01,2.97E+01,   &     !Z=11 (Na)
   2.74E+01,2.41E+01,2.20E+01,1.77E+01,3.03E+00,2.17E+00,2.01E+00,1.77E+00,1.56E+00,1.45E+00,1.28E+00,1.13E+00,   &
   2.12E+02,1.65E+02,1.26E+02,9.87E+01,9.62E+01,7.78E+01,7.47E+01,6.19E+01,5.85E+01,4.96E+01,4.63E+01,4.00E+01,   &     !Z=12 (Mg)
   3.69E+01,3.25E+01,2.96E+01,2.40E+01,4.09E+00,2.92E+00,2.70E+00,2.37E+00,2.09E+00,1.93E+00,1.70E+00,1.50E+00,   &
   2.59E+02,2.01E+02,1.55E+02,1.21E+02,1.18E+02,9.59E+01,9.21E+01,7.64E+01,7.23E+01,6.13E+01,5.73E+01,4.96E+01,   &     !Z=13 (Al)
   4.58E+01,4.03E+01,3.68E+01,2.98E+01,5.11E+00,3.64E+00,3.36E+00,2.94E+00,2.59E+00,2.39E+00,2.10E+00,1.85E+00,   &
   3.27E+02,2.55E+02,1.96E+02,1.54E+02,1.51E+02,1.22E+02,1.18E+02,9.78E+01,9.27E+01,7.87E+01,7.36E+01,6.37E+01,   &     !Z=14 (Si)
   5.89E+01,5.20E+01,4.75E+01,3.85E+01,6.64E+00,4.73E+00,4.36E+00,3.81E+00,3.35E+00,3.09E+00,2.71E+00,2.38E+00,   &
   3.79E+02,2.97E+02,2.30E+02,1.81E+02,1.77E+02,1.44E+02,1.39E+02,1.15E+02,1.09E+02,9.30E+01,8.70E+01,7.55E+01,   &     !Z=15 (P)
   6.98E+01,6.17E+01,5.64E+01,4.58E+01,7.97E+00,5.67E+00,5.23E+00,4.57E+00,4.01E+00,3.70E+00,3.24E+00,2.84E+00,   &
   4.60E+02,3.62E+02,2.81E+02,2.22E+02,2.17E+02,1.77E+02,1.70E+02,1.42E+02,1.35E+02,1.15E+02,1.07E+02,9.33E+01,   &     !Z=16 (S)
   8.63E+01,7.63E+01,6.98E+01,5.68E+01,9.99E+00,7.11E+00,6.55E+00,5.72E+00,5.02E+00,4.64E+00,4.05E+00,3.55E+00,   &
   5.11E+02,4.04E+02,3.16E+02,2.50E+02,2.44E+02,2.00E+02,1.92E+02,1.61E+02,1.52E+02,1.30E+02,1.22E+02,1.06E+02,   &     !Z=17 (Cl)
   9.81E+01,8.69E+01,7.95E+01,6.48E+01,1.15E+01,8.20E+00,7.55E+00,6.61E+00,5.79E+00,5.35E+00,4.67E+00,4.09E+00,   &
   5.56E+02,4.38E+02,3.42E+02,2.72E+02,2.66E+02,2.18E+02,2.10E+02,1.76E+02,1.67E+02,1.43E+02,1.34E+02,1.16E+02,   &     !Z=18 (Ar)
   1.08E+02,9.55E+01,8.75E+01,7.14E+01,1.28E+01,9.14E+00,8.42E+00,7.37E+00,6.46E+00,5.96E+00,5.21E+00,4.56E+00,   &
   6.80E+02,5.38E+02,4.21E+02,3.36E+02,3.28E+02,2.70E+02,2.60E+02,2.18E+02,2.07E+02,1.77E+02,1.66E+02,1.45E+02,   &     !Z=19 (K)
   1.34E+02,1.19E+02,1.09E+02,8.94E+01,1.62E+01,1.16E+01,1.07E+01,9.33E+00,8.19E+00,7.56E+00,6.60E+00,5.78E+00,   &
   7.81E+02,6.24E+02,4.90E+02,3.91E+02,3.82E+02,3.14E+02,3.03E+02,2.55E+02,2.42E+02,2.08E+02,1.95E+02,1.70E+02,   &     !Z=20 (Ca)
   1.58E+02,1.40E+02,1.29E+02,1.05E+02,1.93E+01,1.38E+01,1.27E+01,1.12E+01,9.79E+00,9.04E+00,7.90E+00,6.92E+00,   &
   8.08E+02,6.52E+02,5.16E+02,4.12E+02,4.03E+02,3.32E+02,3.19E+02,2.69E+02,2.56E+02,2.20E+02,2.06E+02,1.80E+02,   &     !Z=21 (Sc)
   1.67E+02,1.49E+02,1.37E+02,1.12E+02,2.08E+01,1.49E+01,1.38E+01,1.20E+01,1.06E+01,9.76E+00,8.53E+00,7.47E+00,   &
   1.09E+02,8.54E+01,5.90E+02,4.57E+02,4.44E+02,3.58E+02,3.45E+02,2.91E+02,2.77E+02,2.40E+02,2.27E+02,2.00E+02,   &     !Z=22 (Ti)
   1.86E+02,1.66E+02,1.52E+02,1.25E+02,2.34E+01,1.68E+01,1.55E+01,1.36E+01,1.19E+01,1.10E+01,9.61E+00,8.43E+00,   &
   1.23E+02,9.65E+01,7.47E+01,4.89E+02,4.79E+02,3.99E+02,3.85E+02,3.25E+02,3.09E+02,2.66E+02,2.50E+02,2.19E+02,   &     !Z=23 (V)
   2.03E+02,1.81E+02,1.66E+02,1.37E+02,2.60E+01,1.87E+01,1.73E+01,1.51E+01,1.33E+01,1.23E+01,1.07E+01,9.42E+00,   &
   1.43E+02,1.12E+02,8.68E+01,6.86E+01,6.70E+01,4.92E+02,4.80E+02,4.08E+02,3.85E+02,3.18E+02,2.93E+02,2.47E+02,   &     !Z=24 (Cr)
   2.27E+02,2.01E+02,1.85E+02,1.55E+02,2.99E+01,2.15E+01,1.99E+01,1.75E+01,1.54E+01,1.42E+01,1.24E+01,1.09E+01,   &
   1.61E+02,1.26E+02,9.75E+01,7.72E+01,7.53E+01,6.16E+01,5.92E+01,3.93E+02,3.75E+02,3.25E+02,3.06E+02,2.70E+02,   &     !Z=25 (Mn)
   2.51E+02,2.24E+02,2.07E+02,1.70E+02,3.31E+01,2.38E+01,2.20E+01,1.93E+01,1.70E+01,1.57E+01,1.37E+01,1.21E+01,   &
   1.85E+02,1.45E+02,1.13E+02,8.90E+01,8.69E+01,7.10E+01,6.84E+01,5.72E+01,5.43E+01,3.62E+02,3.42E+02,3.02E+02,   &     !Z=26 (Fe)
   2.81E+02,2.52E+02,2.32E+02,1.92E+02,3.76E+01,2.71E+01,2.51E+01,2.20E+01,1.94E+01,1.79E+01,1.57E+01,1.38E+01,   &
   2.04E+02,1.60E+02,1.24E+02,9.83E+01,9.60E+01,7.85E+01,7.55E+01,6.32E+01,6.00E+01,5.13E+01,4.81E+01,3.21E+02,   &     !Z=27 (Co)
   3.00E+02,2.69E+02,2.48E+02,2.06E+02,4.10E+01,2.96E+01,2.74E+01,2.41E+01,2.12E+01,1.96E+01,1.72E+01,1.51E+01,   &
   2.37E+02,1.86E+02,1.44E+02,1.14E+02,1.12E+02,9.13E+01,8.78E+01,7.35E+01,6.98E+01,5.97E+01,5.60E+01,4.88E+01,   &     !Z=28 (Ni)
   4.53E+01,3.02E+02,2.79E+02,2.33E+02,4.69E+01,3.40E+01,3.15E+01,2.77E+01,2.44E+01,2.26E+01,1.98E+01,1.74E+01,   &
   2.51E+02,1.97E+02,1.53E+02,1.21E+02,1.18E+02,9.68E+01,9.31E+01,7.80E+01,7.40E+01,6.33E+01,5.94E+01,5.18E+01,   &     !Z=29 (Cu)
   4.80E+01,4.27E+01,3.92E+01,2.40E+02,4.91E+01,3.57E+01,3.30E+01,2.91E+01,2.56E+01,2.38E+01,2.08E+01,1.83E+01,   &
   2.80E+02,2.20E+02,1.71E+02,1.35E+02,1.32E+02,1.08E+02,1.04E+02,8.71E+01,8.27E+01,7.08E+01,6.64E+01,5.79E+01,   &     !Z=30 (Zn)
   5.37E+01,4.77E+01,4.38E+01,3.59E+01,5.40E+01,3.93E+01,3.63E+01,3.20E+01,2.82E+01,2.62E+01,2.30E+01,2.02E+01,   &
   2.99E+02,2.35E+02,1.83E+02,1.45E+02,1.42E+02,1.16E+02,1.11E+02,9.34E+01,8.86E+01,7.59E+01,7.12E+01,6.21E+01,   &     !Z=31 (Ga)
   5.76E+01,5.12E+01,4.70E+01,3.85E+01,5.70E+01,4.15E+01,3.84E+01,3.38E+01,2.98E+01,2.77E+01,2.43E+01,2.14E+01,   &
   3.26E+02,2.56E+02,1.99E+02,1.58E+02,1.55E+02,1.27E+02,1.22E+02,1.02E+02,9.69E+01,8.29E+01,7.78E+01,6.79E+01,   &     !Z=32 (Ge)
   6.30E+01,5.59E+01,5.14E+01,4.22E+01,6.12E+01,4.46E+01,4.13E+01,3.64E+01,3.21E+01,2.98E+01,2.62E+01,2.31E+01,   &
   3.57E+02,2.81E+02,2.19E+02,1.74E+02,1.70E+02,1.39E+02,1.34E+02,1.12E+02,1.06E+02,9.11E+01,8.55E+01,7.47E+01,   &     !Z=33 (As)
   6.93E+01,6.15E+01,5.65E+01,4.64E+01,6.61E+01,4.82E+01,4.46E+01,3.93E+01,3.48E+01,3.23E+01,2.84E+01,2.50E+01,   &
   3.81E+02,3.00E+02,2.34E+02,1.86E+02,1.82E+02,1.49E+02,1.43E+02,1.20E+02,1.14E+02,9.76E+01,9.16E+01,8.00E+01,   &     !Z=34 (Se)
   7.42E+01,6.59E+01,6.05E+01,4.97E+01,6.95E+01,5.08E+01,4.71E+01,4.16E+01,3.68E+01,3.41E+01,3.00E+01,2.65E+01,   &
   4.23E+02,3.33E+02,2.60E+02,2.06E+02,2.02E+02,1.65E+02,1.59E+02,1.33E+02,1.27E+02,1.09E+02,1.02E+02,8.90E+01,   &     !Z=35 (Br)
   8.26E+01,7.33E+01,6.74E+01,5.53E+01,7.56E+01,5.55E+01,5.15E+01,4.55E+01,4.03E+01,3.74E+01,3.29E+01,2.91E+01,   &
   4.50E+02,3.55E+02,2.77E+02,2.20E+02,2.15E+02,1.76E+02,1.70E+02,1.42E+02,1.35E+02,1.16E+02,1.09E+02,9.52E+01,   &     !Z=36 (Kr)
   8.83E+01,7.85E+01,7.21E+01,5.92E+01,7.93E+01,5.84E+01,5.43E+01,4.80E+01,4.25E+01,3.95E+01,3.48E+01,3.07E+01,   &
   4.92E+02,3.88E+02,3.03E+02,2.41E+02,2.36E+02,1.93E+02,1.86E+02,1.56E+02,1.48E+02,1.27E+02,1.19E+02,1.04E+02,   &     !Z=37 (Rb)
   9.68E+01,8.60E+01,7.90E+01,6.49E+01,8.51E+01,6.30E+01,5.85E+01,5.18E+01,4.59E+01,4.27E+01,3.76E+01,3.32E+01,   &
   5.32E+02,4.21E+02,3.28E+02,2.62E+02,2.56E+02,2.10E+02,2.02E+02,1.70E+02,1.61E+02,1.38E+02,1.30E+02,1.13E+02,   &     !Z=38 (Sr)
   1.05E+02,9.35E+01,8.59E+01,7.06E+01,9.06E+01,6.72E+01,6.25E+01,5.54E+01,4.91E+01,4.57E+01,4.03E+01,3.56E+01,   &
   5.80E+02,4.59E+02,3.58E+02,2.86E+02,2.79E+02,2.29E+02,2.21E+02,1.85E+02,1.76E+02,1.51E+02,1.42E+02,1.24E+02,   &     !Z=39 (Y)
   1.15E+02,1.02E+02,9.40E+01,7.73E+01,9.70E+01,7.21E+01,6.71E+01,5.95E+01,5.29E+01,4.91E+01,4.34E+01,3.84E+01,   &
   6.22E+02,4.93E+02,3.86E+02,3.08E+02,3.00E+02,2.47E+02,2.38E+02,2.00E+02,1.91E+02,1.63E+02,1.54E+02,1.39E+02,   &     !Z=40 (Zr)
   1.24E+02,1.10E+02,1.01E+02,8.35E+01,1.63E+01,7.61E+01,6.25E+01,6.29E+01,5.59E+01,5.20E+01,4.60E+01,4.07E+01,   &
   6.71E+02,5.32E+02,4.16E+02,3.32E+02,3.25E+02,2.67E+02,2.57E+02,2.16E+02,2.05E+02,1.76E+02,1.66E+02,1.45E+02,   &     !Z=41 (Nb)
   1.34E+02,1.20E+02,1.10E+02,9.04E+01,1.77E+01,8.10E+01,7.55E+01,6.71E+01,5.98E+01,5.56E+01,4.92E+01,4.36E+01,   &
   7.12E+02,5.65E+02,4.42E+02,3.53E+02,3.45E+02,2.84E+02,2.73E+02,2.30E+02,2.19E+02,1.88E+02,1.76E+02,1.54E+02,   &     !Z=42 (Mo)
   1.43E+02,1.27E+02,1.17E+02,9.65E+01,1.88E+01,1.38E+01,7.95E+01,7.71E+01,7.20E+01,6.80E+01,6.03E+01,5.25E+01,   &
   7.61E+02,6.04E+02,4.74E+02,3.79E+02,3.70E+02,3.05E+02,2.94E+02,2.47E+02,2.35E+02,2.02E+02,1.90E+02,1.66E+02,   &     !Z=43 (Tc)
   1.54E+02,1.37E+02,1.26E+02,1.04E+02,2.04E+01,1.49E+01,1.38E+01,7.41E+01,6.60E+01,6.15E+01,5.45E+01,4.84E+01,   &
   8.04E+02,6.39E+02,5.01E+02,4.01E+02,3.92E+02,3.23E+02,3.11E+02,2.62E+02,2.49E+02,2.14E+02,2.01E+02,1.76E+02,   &     !Z=44 (Ru)
   1.63E+02,1.46E+02,1.34E+02,1.10E+02,2.17E+01,1.58E+01,1.47E+01,1.29E+01,1.14E+01,7.00E+01,5.69E+01,5.06E+01,   &
   8.60E+02,6.83E+02,5.36E+02,4.29E+02,4.20E+02,3.46E+02,3.33E+02,2.80E+02,2.67E+02,2.29E+02,2.16E+02,1.89E+02,   &     !Z=45 (Rh)
   1.75E+02,1.56E+02,1.44E+02,1.18E+02,2.33E+01,1.70E+01,1.58E+01,1.39E+01,1.23E+01,1.14E+01,6.01E+01,5.35E+01,   &
   9.01E+02,7.16E+02,5.63E+02,4.51E+02,4.41E+02,3.63E+02,3.50E+02,2.95E+02,2.81E+02,2.41E+02,2.27E+02,1.99E+02,   &     !Z=46 (Pd)
   1.85E+02,1.65E+02,1.51E+02,1.25E+02,2.47E+01,1.80E+01,1.67E+01,1.47E+01,1.30E+01,1.21E+01,1.06E+01,5.55E+01,   &
   9.61E+02,7.65E+02,6.02E+02,4.83E+02,4.72E+02,3.89E+02,3.75E+02,3.16E+02,3.01E+02,2.59E+02,2.43E+02,2.13E+02,   &     !Z=47 (Ag)
   1.98E+02,1.77E+02,1.63E+02,1.34E+02,2.65E+01,1.94E+01,1.79E+01,1.58E+01,1.40E+01,1.30E+01,1.15E+01,1.01E+01,   &
   9.95E+02,7.95E+02,6.26E+02,5.02E+02,4.90E+02,4.05E+02,3.90E+02,3.29E+02,3.13E+02,2.69E+02,2.53E+02,2.22E+02,   &     !Z=48 (Cd)
   2.07E+02,1.84E+02,1.69E+02,1.40E+02,2.78E+01,2.02E+01,1.88E+01,1.66E+01,1.46E+01,1.36E+01,1.20E+01,1.06E+01,   &
   1.05E+03,8.41E+02,6.63E+02,5.31E+02,5.19E+02,4.28E+02,4.13E+02,3.49E+02,3.32E+02,2.86E+02,2.69E+02,2.36E+02,   &     !Z=49 (In)
   2.19E+02,1.95E+02,1.80E+02,1.48E+02,2.95E+01,2.16E+01,2.00E+01,1.76E+01,1.56E+01,1.45E+01,1.27E+01,1.13E+01,   &
   1.09E+03,8.76E+02,6.91E+02,5.54E+02,5.42E+02,4.47E+02,4.31E+02,3.64E+02,3.47E+02,2.99E+02,2.81E+02,2.47E+02,   &     !Z=50 (Sn)
   2.29E+02,2.04E+02,1.88E+02,1.55E+02,3.10E+01,2.26E+01,2.10E+01,1.85E+01,1.64E+01,1.52E+01,1.34E+01,1.18E+01,   &
   9.91E+02,9.15E+02,7.23E+02,5.82E+02,5.70E+02,4.71E+02,4.54E+02,3.83E+02,3.65E+02,3.14E+02,2.96E+02,2.59E+02,   &     !Z=51 (Sb)
   2.41E+02,2.15E+02,1.98E+02,1.64E+02,3.27E+01,2.39E+01,2.22E+01,1.96E+01,1.73E+01,1.60E+01,1.41E+01,1.25E+01,   &
   7.51E+02,9.32E+02,7.40E+02,5.98E+02,5.85E+02,4.83E+02,4.66E+02,3.94E+02,3.74E+02,3.23E+02,3.04E+02,2.67E+02,   &     !Z=52 (Te)
   2.48E+02,2.21E+02,2.04E+02,1.68E+02,3.38E+01,2.47E+01,2.29E+01,2.02E+01,1.79E+01,1.66E+01,1.46E+01,1.29E+01,   &
   2.83E+02,1.00E+03,7.96E+02,6.45E+02,6.31E+02,5.22E+02,5.03E+02,4.25E+02,4.08E+02,3.49E+02,3.30E+02,2.88E+02,   &     !Z=53 (I)
   2.68E+02,2.39E+02,2.20E+02,1.82E+02,3.67E+01,2.68E+01,2.18E+01,2.19E+01,1.94E+01,1.80E+01,1.59E+01,1.40E+01,   &
   2.65E+02,1.03E+03,7.21E+02,6.66E+02,6.52E+02,5.40E+02,5.20E+02,4.40E+02,4.22E+02,3.62E+02,3.43E+02,2.99E+02,   &     !Z=54 (Xe)
   2.78E+02,2.49E+02,2.29E+02,1.90E+02,3.82E+01,2.80E+01,2.27E+01,2.29E+01,2.02E+01,1.88E+01,1.65E+01,1.46E+01,   &
   3.30E+02,2.60E+02,7.60E+02,7.00E+02,6.86E+02,5.69E+02,5.49E+02,4.65E+02,4.46E+02,3.83E+02,3.63E+02,3.17E+02,   &     !Z=55 (Cs)
   2.95E+02,2.63E+02,2.43E+02,2.01E+02,4.07E+01,2.98E+01,2.42E+01,2.43E+01,2.15E+01,2.00E+01,1.76E+01,1.56E+01,   &
   3.34E+02,3.14E+02,5.70E+02,6.60E+02,6.45E+02,5.86E+02,5.66E+02,4.80E+02,4.61E+02,3.96E+02,3.76E+02,3.25E+02,   &     !Z=56 (Ba)
   3.06E+02,2.73E+02,2.52E+02,2.09E+02,4.23E+01,3.10E+01,2.52E+01,2.54E+01,2.24E+01,2.08E+01,1.83E+01,1.62E+01,   &
   3.55E+02,2.84E+02,2.25E+02,7.60E+02,7.44E+02,6.18E+02,5.97E+02,5.07E+02,4.83E+02,4.19E+02,3.95E+02,3.48E+02,   &     !Z=57 (La)
   3.24E+02,2.89E+02,2.66E+02,2.21E+02,4.49E+01,3.29E+01,3.05E+01,2.69E+01,2.38E+01,2.21E+01,1.95E+01,1.72E+01,   &
   3.57E+02,3.00E+02,2.38E+02,5.12E+02,4.94E+02,5.61E+02,5.47E+02,5.35E+02,5.10E+02,4.42E+02,4.17E+02,3.68E+02,   &     !Z=58 (Ce)
   3.43E+02,3.06E+02,2.82E+02,2.33E+02,4.77E+01,3.49E+01,3.24E+01,2.86E+01,2.53E+01,2.35E+01,2.07E+01,1.83E+01,   &
   3.75E+02,3.00E+02,2.38E+02,1.93E+02,1.88E+02,4.48E+02,6.16E+02,5.65E+02,5.39E+02,4.68E+02,4.41E+02,3.90E+02,   &     !Z=59 (Pr)
   3.63E+02,3.24E+02,2.99E+02,2.47E+02,5.07E+01,3.72E+01,3.45E+01,3.04E+01,2.69E+01,2.50E+01,2.20E+01,1.95E+01,   &
   3.97E+02,3.14E+02,2.51E+02,2.03E+02,1.98E+02,4.55E+02,4.39E+02,5.05E+02,4.92E+02,4.84E+02,4.57E+02,4.04E+02,   &     !Z=60 (Nd)
   3.76E+02,3.36E+02,3.10E+02,2.57E+02,5.30E+01,3.88E+01,3.60E+01,3.18E+01,2.81E+01,2.61E+01,2.30E+01,2.04E+01,   &
   4.62E+02,3.69E+02,2.94E+02,2.37E+02,2.32E+02,1.94E+02,4.68E+02,4.00E+02,5.88E+02,5.11E+02,4.82E+02,4.26E+02,   &     !Z=61 (Pm)
   3.97E+02,3.55E+02,3.28E+02,2.73E+02,5.63E+01,4.13E+01,3.83E+01,3.38E+01,2.99E+01,2.78E+01,2.45E+01,2.17E+01,   &
   4.35E+02,3.50E+02,2.79E+02,2.25E+02,2.21E+02,2.04E+02,1.66E+02,1.76E+02,1.63E+02,3.71E+02,3.54E+02,4.34E+02,   &     !Z=62 (Sm)
   4.05E+02,3.63E+02,3.35E+02,2.79E+02,5.78E+01,4.24E+01,3.94E+01,3.48E+01,3.08E+01,2.86E+01,2.52E+01,2.23E+01,   &
   4.88E+02,3.90E+02,3.09E+02,2.49E+02,2.44E+02,2.03E+02,1.95E+02,4.19E+02,4.08E+02,3.75E+02,4.80E+02,4.34E+02,   &     !Z=63 (Eu)
   4.24E+02,3.80E+02,3.52E+02,2.93E+02,6.09E+01,4.47E+01,4.15E+01,3.66E+01,3.24E+01,3.01E+01,2.66E+01,2.35E+01,   &
   4.69E+02,3.74E+02,2.98E+02,2.41E+02,2.35E+02,1.95E+02,1.89E+02,1.61E+02,1.53E+02,3.56E+02,3.35E+02,4.03E+02,   &     !Z=64 (Gd)
   4.33E+02,3.89E+02,3.60E+02,3.00E+02,6.26E+01,4.60E+01,4.27E+01,3.77E+01,3.34E+01,3.10E+01,2.74E+01,2.42E+01,   &
   5.24E+02,4.19E+02,3.32E+02,2.69E+02,2.63E+02,2.19E+02,2.11E+02,1.80E+02,1.71E+02,1.49E+02,3.60E+02,3.21E+02,   &     !Z=65 (Tb)
   4.52E+02,4.06E+02,3.76E+02,3.14E+02,6.58E+01,4.83E+01,4.49E+01,3.96E+01,3.51E+01,3.26E+01,2.88E+01,2.55E+01,   &
   5.15E+02,4.10E+02,3.25E+02,2.62E+02,2.57E+02,2.14E+02,2.07E+02,1.76E+02,1.68E+02,1.46E+02,1.38E+02,3.62E+02,   &     !Z=66 (Dy)
   3.36E+02,4.19E+02,3.87E+02,3.24E+02,6.83E+01,5.02E+01,4.66E+01,4.12E+01,3.65E+01,3.39E+01,2.99E+01,2.65E+01,   &
   5.47E+02,4.38E+02,3.47E+02,2.80E+02,2.72E+02,2.28E+02,2.20E+02,1.87E+02,1.78E+02,1.55E+02,1.46E+02,1.29E+02,   &     !Z=67 (Ho)
   4.44E+02,3.98E+02,4.02E+02,3.36E+02,7.13E+01,5.24E+01,4.87E+01,4.30E+01,3.81E+01,3.54E+01,3.13E+01,2.77E+01,   &
   5.54E+02,4.43E+02,3.52E+02,2.85E+02,2.78E+02,2.32E+02,2.24E+02,1.91E+02,1.82E+02,1.58E+02,1.49E+02,1.32E+02,   &     !Z=68 (Er)
   1.23E+02,2.87E+02,4.17E+02,3.49E+02,7.44E+01,5.48E+01,5.09E+01,4.50E+01,3.99E+01,3.71E+01,3.27E+01,2.90E+01,   &
   6.21E+02,4.94E+02,3.86E+02,3.12E+02,3.05E+02,2.53E+02,2.43E+02,2.06E+02,1.96E+02,1.69E+02,1.59E+02,1.40E+02,   &     !Z=69 (Tm)
   1.31E+02,1.17E+02,1.08E+02,3.65E+02,7.79E+01,5.74E+01,5.33E+01,4.71E+01,4.18E+01,3.89E+01,3.43E+01,3.04E+01,   &
   6.19E+02,4.92E+02,3.87E+02,3.11E+02,3.04E+02,2.51E+02,2.44E+02,2.06E+02,1.96E+02,1.69E+02,1.59E+02,1.42E+02,   &     !Z=70 (Yb)
   1.31E+02,1.17E+02,1.08E+02,3.75E+02,8.04E+01,5.93E+01,5.50E+01,4.87E+01,4.32E+01,4.01E+01,3.54E+01,3.14E+01,   &
   6.88E+02,5.47E+02,4.31E+02,3.47E+02,3.39E+02,2.80E+02,2.70E+02,2.29E+02,2.18E+02,1.89E+02,1.78E+02,1.56E+02,   &     !Z=71 (Lu)
   1.46E+02,1.31E+02,1.21E+02,3.91E+02,8.40E+01,6.19E+01,5.75E+01,5.09E+01,4.51E+01,4.20E+01,3.71E+01,3.28E+01,   &
   6.78E+02,5.39E+02,4.25E+02,3.41E+02,3.34E+02,2.77E+02,2.67E+02,2.27E+02,2.16E+02,1.87E+02,1.76E+02,1.55E+02,   &     !Z=72 (Hf)
   1.45E+02,1.30E+02,1.20E+02,1.00E+02,8.69E+01,6.41E+01,5.95E+01,5.27E+01,4.67E+01,4.35E+01,3.84E+01,3.40E+01,   &
   6.85E+02,5.46E+02,4.32E+02,3.46E+02,3.39E+02,2.83E+02,2.73E+02,2.31E+02,2.20E+02,1.90E+02,1.79E+02,1.58E+02,   &     !Z=73 (Ta)
   1.47E+02,1.32E+02,1.22E+02,1.02E+02,9.04E+01,6.67E+01,6.20E+01,5.48E+01,4.87E+01,4.53E+01,4.00E+01,3.54E+01,   &
   7.25E+02,5.79E+02,4.57E+02,3.69E+02,3.61E+02,3.01E+02,2.88E+02,2.46E+02,2.34E+02,2.03E+02,1.91E+02,1.68E+02,   &     !Z=74 (W)
   1.57E+02,1.41E+02,1.30E+02,1.08E+02,9.38E+01,6.92E+01,6.43E+01,5.69E+01,5.05E+01,4.70E+01,4.15E+01,3.68E+01,   &
   7.94E+02,6.33E+02,5.01E+02,4.05E+02,3.94E+02,3.27E+02,3.16E+02,2.68E+02,2.57E+02,2.22E+02,2.09E+02,1.87E+02,   &     !Z=75 (Re)
   1.72E+02,1.55E+02,1.43E+02,1.19E+02,9.74E+01,7.19E+01,6.69E+01,5.92E+01,5.25E+01,4.89E+01,4.32E+01,3.83E+01,   &
   7.92E+02,6.31E+02,4.99E+02,4.03E+02,3.92E+02,3.27E+02,3.14E+02,2.68E+02,2.55E+02,2.21E+02,2.09E+02,1.84E+02,   &     !Z=76 (Os)
   1.71E+02,1.54E+02,1.42E+02,1.18E+02,1.00E+02,7.41E+01,6.89E+01,6.10E+01,5.41E+01,5.04E+01,4.45E+01,3.95E+01,   &
   8.26E+02,6.59E+02,5.20E+02,4.18E+02,4.11E+02,3.40E+02,3.30E+02,2.78E+02,2.65E+02,2.30E+02,2.16E+02,1.91E+02,   &     !Z=77 (Ir)
   1.78E+02,1.60E+02,1.48E+02,1.23E+02,1.04E+02,7.70E+01,7.16E+01,6.34E+01,5.63E+01,5.24E+01,4.64E+01,4.11E+01,   &
   8.19E+02,6.83E+02,5.41E+02,4.34E+02,4.23E+02,3.57E+02,3.25E+02,2.76E+02,2.61E+02,2.27E+02,2.14E+02,1.88E+02,   &     !Z=78 (Pt)
   1.75E+02,1.57E+02,1.45E+02,1.21E+02,1.07E+02,7.97E+01,7.41E+01,6.57E+01,5.83E+01,5.43E+01,4.80E+01,4.26E+01,   &
   8.76E+02,6.99E+02,5.51E+02,4.45E+02,4.34E+02,3.61E+02,3.48E+02,2.95E+02,2.79E+02,2.43E+02,2.29E+02,2.01E+02,   &     !Z=79 (Au)
   1.88E+02,1.68E+02,1.55E+02,1.30E+02,1.12E+02,8.29E+01,7.71E+01,6.83E+01,6.07E+01,5.65E+01,5.00E+01,4.44E+01,   &
   8.97E+02,6.99E+02,5.41E+02,4.27E+02,4.16E+02,3.39E+02,3.27E+02,2.73E+02,2.60E+02,2.30E+02,2.16E+02,1.88E+02,   &     !Z=80 (Hg)
   1.74E+02,1.54E+02,1.41E+02,1.16E+02,1.15E+02,8.54E+01,7.95E+01,7.04E+01,6.26E+01,5.83E+01,5.16E+01,4.58E+01,   &
   9.89E+02,7.15E+02,5.97E+02,5.00E+02,4.87E+02,4.03E+02,3.90E+02,3.31E+02,3.14E+02,2.71E+02,2.57E+02,2.26E+02,   &     !Z=81 (Tl)
   2.11E+02,1.89E+02,1.75E+02,1.45E+02,1.18E+02,8.79E+01,8.18E+01,7.25E+01,6.45E+01,6.00E+01,5.31E+01,4.72E+01,   &
   1.03E+03,8.15E+02,6.43E+02,5.18E+02,5.07E+02,4.20E+02,4.06E+02,3.43E+02,3.27E+02,2.83E+02,2.67E+02,2.35E+02,   &     !Z=82 (Pb)
   2.16E+02,1.96E+02,1.81E+02,1.51E+02,1.22E+02,9.08E+01,8.45E+01,7.49E+01,6.66E+01,6.20E+01,5.49E+01,4.88E+01,   &
   1.06E+03,8.44E+02,6.66E+02,5.35E+02,5.24E+02,4.34E+02,4.21E+02,3.55E+02,3.39E+02,2.95E+02,2.76E+02,2.44E+02,   &     !Z=83 (Bi)
   2.28E+02,2.04E+02,1.88E+02,1.57E+02,1.26E+02,9.41E+01,8.76E+01,7.77E+01,6.91E+01,6.44E+01,5.70E+01,5.06E+01,   &
   1.10E+03,8.30E+02,6.91E+02,5.58E+02,5.44E+02,4.52E+02,4.35E+02,3.70E+02,3.54E+02,3.05E+02,2.88E+02,2.54E+02,   &     !Z=84 (Po)
   2.37E+02,2.12E+02,1.96E+02,1.63E+02,1.32E+02,9.83E+01,9.15E+01,8.12E+01,7.23E+01,6.73E+01,5.96E+01,5.30E+01,   &
   1.08E+03,8.60E+02,6.80E+02,5.45E+02,5.33E+02,4.44E+02,4.26E+02,3.63E+02,3.45E+02,2.99E+02,2.82E+02,2.48E+02,   &     !Z=85 (At)
   2.31E+02,2.07E+02,1.86E+02,1.71E+02,1.17E+02,1.02E+02,9.50E+01,8.43E+01,7.51E+01,7.00E+01,6.20E+01,5.51E+01,   &
   1.18E+03,9.32E+02,7.34E+02,5.89E+02,5.76E+02,4.77E+02,4.60E+02,3.92E+02,3.73E+02,3.21E+02,3.04E+02,2.67E+02,   &     !Z=86 (Rn)
   2.49E+02,2.23E+02,2.05E+02,1.71E+02,1.08E+02,1.01E+02,9.36E+01,8.32E+01,7.21E+01,6.90E+01,6.12E+01,5.45E+01,   &
   1.21E+03,9.61E+02,7.58E+02,6.02E+02,5.97E+02,4.93E+02,4.77E+02,4.03E+02,3.84E+02,3.32E+02,3.12E+02,2.77E+02,   &     !Z=87 (Fr)
   2.57E+02,2.30E+02,2.13E+02,1.77E+02,8.70E+01,1.04E+02,9.72E+01,8.64E+01,7.70E+01,7.18E+01,6.37E+01,5.67E+01,   &
   1.33E+03,9.41E+02,7.43E+02,5.99E+02,5.85E+02,4.87E+02,4.70E+02,3.98E+02,3.80E+02,3.29E+02,3.10E+02,2.73E+02,   &     !Z=88 (Ra)
   2.54E+02,2.28E+02,2.10E+02,1.75E+02,8.80E+01,1.08E+01,1.00E+02,8.90E+01,7.93E+01,7.40E+01,6.56E+01,5.84E+01,   &
   1.05E+03,8.83E+02,7.39E+02,6.29E+02,6.18E+02,5.30E+02,5.21E+02,4.61E+02,4.44E+02,3.99E+02,3.81E+02,3.17E+02,   &     !Z=89 (Ac)
   3.09E+02,3.03E+02,2.85E+02,2.49E+02,9.08E+01,1.10E+02,1.04E+02,9.24E+01,8.24E+01,7.68E+01,6.82E+01,6.07E+01,   &
   1.23E+03,9.78E+02,7.68E+02,6.23E+02,5.09E+02,4.85E+02,4.46E+02,4.06E+02,3.89E+02,3.69E+02,3.48E+02,3.06E+02,   &     !Z=90 (Th)
   2.85E+02,2.55E+02,2.19E+02,1.70E+02,9.65E+01,9.87E+01,1.06E+02,9.41E+01,8.39E+01,7.82E+01,6.95E+01,6.19E+01,   &
   1.24E+03,9.83E+02,7.38E+02,5.93E+02,5.82E+02,4.82E+02,4.65E+02,3.94E+02,3.75E+02,3.25E+02,3.06E+02,2.71E+02,   &     !Z=91 (Pa)
   2.52E+02,2.25E+02,2.08E+02,1.73E+02,1.01E+02,1.19E+02,1.11E+02,9.84E+01,8.78E+01,8.19E+01,7.27E+01,6.48E+01,   &
   1.23E+03,9.66E+02,7.66E+02,6.32E+02,6.17E+02,5.28E+02,4.96E+02,4.20E+02,4.00E+02,3.47E+02,3.26E+02,2.88E+02,   &     !Z=92 (U)
   2.68E+02,2.40E+02,2.22E+02,1.85E+02,1.02E+02,7.49E+01,1.12E+02,9.93E+01,8.86E+01,8.27E+01,7.35E+01,6.55E+01,   &
   9.65E+02,1.01E+03,8.00E+02,6.45E+02,6.30E+02,5.52E+02,5.05E+02,4.30E+02,4.10E+02,3.55E+02,3.35E+02,3.14E+02,   &     !Z=93 (Np)
   2.75E+02,2.46E+02,2.27E+02,1.90E+02,4.22E+01,1.25E+02,1.16E+02,1.04E+02,9.25E+01,8.63E+01,7.67E+01,6.84E+01,   &
   9.00E+02,9.62E+02,7.60E+02,6.12E+02,6.00E+02,4.98E+02,4.08E+02,4.08E+02,3.89E+02,3.36E+02,3.17E+02,2.80E+02,   &     !Z=94 (Pu)
   2.62E+02,2.34E+02,2.16E+02,1.80E+02,3.99E+01,1.29E+02,1.20E+02,1.07E+02,5.60E+01,8.89E+01,7.91E+01,7.05E+01,   &
   9.55E+02,1.03E+03,7.95E+02,6.42E+02,6.27E+02,5.81E+02,5.03E+02,4.26E+02,4.07E+02,3.52E+02,3.33E+02,3.22E+02,   &     !Z=95 (Am)
   2.73E+02,2.41E+02,2.27E+02,1.89E+02,4.81E+01,1.31E+02,1.22E+02,1.09E+02,5.95E+01,9.08E+01,8.08E+01,7.20E+01,   &
   9.84E+02,1.03E+03,8.12E+02,6.55E+02,6.40E+02,5.90E+02,5.15E+02,4.37E+02,4.21E+02,3.60E+02,3.43E+02,3.38E+02,   &     !Z=96 (Cm)
   2.80E+02,2.51E+02,2.32E+02,1.94E+02,4.90E+01,1.34E+02,1.10E+02,1.11E+02,6.43E+01,6.00E+01,8.24E+01,7.35E+01,   &
   1.04E+03,1.09E+03,8.52E+02,6.78E+02,6.64E+02,5.92E+02,5.26E+02,4.43E+02,4.22E+02,3.62E+02,3.57E+02,3.52E+02,   &     !Z=97 (Bk)
   2.77E+02,2.46E+02,2.26E+02,1.86E+02,4.90E+01,1.25E+02,1.02E+02,1.03E+02,6.10E+01,8.51E+01,7.52E+01,6.66E+01,   &
   1.05E+03,1.10E+03,8.71E+02,7.03E+02,6.87E+02,6.07E+02,5.52E+02,4.69E+02,4.48E+02,3.86E+02,3.66E+02,3.60E+02,   &     !Z=98 (Cf)
   3.01E+02,2.70E+02,2.49E+02,2.08E+02,5.00E+01,1.34E+02,1.25E+02,1.11E+02,6.92E+01,9.26E+01,8.24E+01,7.35E+01    ], shape(mac_itc))
!
   if (z < 1 .or. z > 98) then
       ma_coeff = 0
       return
   endif
   !pose = clocate(energy,ang_to_mev/wave)
   !ma_coeff = mac_itc(pose,z)
   !call polint(energy,mac_itc(:,z),ang_to_mev/wave,ma_coeff,dy)  ! bad!
   call spline(energy,mac_itc(:,z),0.0,0.0,yd2)
   ma_coeff = splint(energy,mac_itc(:,z),yd2,ang_to_mev/wave)
!
   end function ma_coeff

!--------------------------------------------------------------------------------------------------
!corr
!corr   subroutine elem_set_mac(elem,wave)
!corr!
!corr!  Set mac for element_type array
!corr!
!corr   type(element_type), dimension(:), allocatable :: elem
!corr   real, intent(in)                              :: wave
!corr   integer                                       :: i
!corr!
!corr   do i=1,numelem(elem)
!corr      elem(i)%mac = ma_coeff(elem(i)%z,wave)
!corr   enddo
!corr!
!corr   end subroutine elem_set_mac
!corr
!--------------------------------------------------------------------------------------------------

   integer function string_to_radtype(str) result(radtype)
   use strutil
   character(len=*), intent(in) :: str
   character(len_trim(str))     :: str1

   radtype = -1
   if (len_trim(str) == 0) return
   str1 = lower(str)
   select case(str1)
      case ('x-ray','xray')
        radtype = RX_SOURCE
      case ('electron')
        radtype = ELECTRON_SOURCE
      case ('neutron')
        radtype = NEUTRON_SOURCE
   end  select

   end function string_to_radtype

!--------------------------------------------------------------------------------------------------

   function get_scattering_param(elem,radtype) result(par)
   type(element_type), intent(in) :: elem
   integer, intent(in)            :: radtype
   real, dimension(NSCATT_PARAM)  :: par
!
   select case (radtype)
     case (RX_SOURCE)
       par(1:4) = elem%ax
       par(5:8) = elem%bx
       par(9) = elem%cx
     case (ELECTRON_SOURCE)
       par(1:4) = elem%ae
       par(5:8) = elem%be
       par(9) = elem%ce
     case (NEUTRON_SOURCE)
       par(1) = elem%fact
   end select
!
   end function get_scattering_param

!--------------------------------------------------------------------------------------------------

   logical function H_is_excluded(radtype) result(excl)
   integer, intent(in) :: radtype
!
   excl = .true.
   if (radtype == NEUTRON_SOURCE) excl = .false.
!
   end function H_is_excluded

END MODULE elements
