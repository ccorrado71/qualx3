module mapinfo

implicit none

enum, bind(c)
  enumerator :: FOUR_FO=1, FOUR_2FO_FC, FOUR_FO_FC, FOUR_FC
endenum

!integer, parameter             :: FOUR_FO=1, FOUR_2FO_FC=2, FOUR_FO_FC=3, FOUR_FC=4
character(len=8), dimension(4) :: four_name = [ 'Fo      ', '2Fo - Fc', 'Fo - Fc ', 'Fc      ' ]

type map_info_type
   logical :: active = .false.       ! map visualization active?
   integer :: ftype  = FOUR_FO_FC    ! map type? 
   real    :: radius = -1            ! radius of map
   real    :: sog = 0.5              ! display map above sog
end type map_info_type

contains

   real function radmap_from_asu(cell,spg) result(radmap)
!
!  Set radius of map from size of asymmetric unit
!
   use unit_cell
   use spginfom
   type(cell_type), intent(in)   :: cell
   type(spaceg_type), intent(in) :: spg
   integer                       :: kmin
   kmin = maxloc(spg%asulim,dim=1)
   radmap = cell%get_par(kmin)*spg%asulim(kmin) / 2
   end function radmap_from_asu

!----------------------------------------------------------------------------------------------------

   real function radmap_from_atoms(atom, cell, frag)  result(radmap)
!
!  Ser radius of map from centre of mass of molecules
!
   use atom_type_util
   use fragmentmod
   use unit_cell
   type(atom_type), dimension(:), allocatable, intent(in)     :: atom
   type(cell_type), intent(in)                                :: cell
   type(fragment_type), dimension(:), allocatable, intent(in) :: frag
   type(atom_type), dimension(:), allocatable                 :: atomcart
   integer                                                    :: i
   real                                                       :: maxradmap
!
   radmap = -1
   if (numatoms(atom) == 0) return
   if (numfragments(frag) == 0) return
   call frac_to_cart_copy(atom,atomcart,cell%get_ortom())
   maxradmap = tiny(1.0)
   do i=1,numfragments(frag)
      call get_radius_molecule(atomcart(frag(i)%pos),radmap)
      if (radmap > maxradmap) maxradmap = radmap
   enddo
   radmap = maxradmap
!
   end function radmap_from_atoms

end module mapinfo
