MODULE povray_frm

implicit none

CONTAINS

   subroutine create_povrayfile(filename,atom,legm,cell,elem,cols,colc,iscell,rmat)
   USE atom_type_util
   USE connect_mod
   USE fileutil
   USE elements
   USE strutil
   USE unit_cell
   USE cgeom
   USE atom_basic
   character(len=*), intent(in)                           :: filename  ! se presente apre il file
   type(atom_type), dimension(:), intent(in)              :: atom
   type(bond_type), dimension(:), allocatable, intent(in) :: legm
   type(cell_type), intent(in)                            :: cell
   type(element_type), dimension(:), intent(in)           :: elem
   real, dimension(:,:), intent(in)                       :: cols      ! col(i,j) = colore della specie i
   real, dimension(3), intent(in)                         :: colc      ! colore della cella
   logical, intent(in)                                    :: iscell
   real, dimension(3,3)                                   :: rmat
   integer                                                :: j_in
   integer                                                :: i    !!!!!, j
   integer                                                :: nspec
   type(atom_type), dimension(:), allocatable             :: atomc
   integer                                                :: iz
   real, dimension(3)                                     :: bar
   real                                                   :: fact
   real                                                   :: wire = 0.1
   integer                                                :: n1,n2
   real                                                   :: rad
   real, dimension(3)                                     :: cpos
   real, dimension(3)                                     :: xm
   character(len=200)                                     :: texture
   integer                                                :: nat
   type(file_handle)                                      :: fpov
   real :: xmin, xmax, ymin, ymax, scalapov, dx, dy
!
!  Apertura del file
   call fpov%fopen(filename,'w')
   j_in = fpov%handle()
!
   nspec = size(elem)
   nat = size(atom)
!
!  Colori delle specie
   write(j_in,'(a)')'#include "colors.inc"'
   do i=1,nspec
      write(j_in,'(a,2(f0.2,","),f0.2,a)')'#declare color_'//trim(elem(i)%lab)//' = '//'rgb < ',cols(i,:),' >;'
   enddo
!
!  Colore della cella
   if (iscell) then
      write(j_in,'(a,2(f0.2,","),f0.2,a)')'#declare color_cell = rgb < ',colc(:),' >;'
   endif
!
!  Costanti numeriche
   write(j_in,'("#declare Wire =",f0.3,"; // Wire thickness")')wire
!
!  Se presente la cella aggiungi 8 atomi in più in corrispondenza dei vertici   
   if(iscell) then
      call resize_atoms(atomc,nat+8)
      atomc(:nat) = atom(:)
      do i=nat+1,nat+8
         atomc(i) = atomc(1)
      enddo
      atomc(nat+1)%xc = (/0,0,0/)
      atomc(nat+2)%xc = (/1,0,0/)
      atomc(nat+3)%xc = (/0,1,0/)
      atomc(nat+4)%xc = (/0,0,1/)
      atomc(nat+5)%xc = (/1,1,1/)
      atomc(nat+6)%xc = (/0,1,1/)
      atomc(nat+7)%xc = (/1,0,1/)
      atomc(nat+8)%xc = (/1,1,0/)
   else
      call resize_atoms(atomc,nat)
      atomc(:) = atom
   endif
!
!  Converti in cartesiane
   !orto = orthomatrix(cell)
   call frac_to_cart(atomc,cell%get_ortom())
   do i=1,nat
      atomc(i)%xc = matmul(rmat,atomc(i)%xc)
   enddo
   write(j_in,'(a)')'#declare Surface = finish { ambient < 0.5 0.5 0.5 1 1 > phong 1 phong_size 10 }'
   write(j_in,'(a)')'background { color White }'
!
!  Locate the camera. La camera guarda il baricentro della molecola
!!!!!  Posiziona la camera ad una distanza pari a 1.8*rad dal bar. lungo la retta che unisce il bar. all'origine (rimosso)
   call get_radius_molecule(atomc,rad,bar)
!!mr
   call translate_atoms(atomc,-bar)
   xmin = minval(atomc(:)%xc(1))
   xMax = maxval(atomc(:)%xc(1))
   ymin = minval(atomc(:)%xc(2))
   ymax = maxval(atomc(:)%xc(2))
   dx = xMax - xmin
   dy = yMax - ymin
   if (dx.gt.dy) then
       if (dx > 0.001) then
           scalapov = 0.5 / dx
       else
           scalapov = 0.5
       endif
   else
       if (dy > 0.001) then
           scalapov = 0.5 / dy
       else
           scalapov = 0.5
       endif
   endif
!!end mr

   !!!!!!cpos =  bar(:) + bar(:) * 1.8 * rad/distanzac(bar,(/0.0,0.0,0.0/))
!!!FIXME - improve scale
   cpos = (/0.0,0.0,1.8*rad/)
   write(j_in,'(a)')'camera { orthographic'
   write(j_in,'(a,2(f0.2,","),f0.2,a)')'         location < ',cpos,' >'
   !write(j_in,'(a,2(f0.2,","),f0.2,a)')'         look_at  < ',bar(:),' >'
   !write(j_in,'(a,f0.2,a)')'         location < 0 0 ',cpos(3),' >'
   !write(j_in,'(a,f0.2,a)')'         location < 0 ',cpos(3),' 0 >'
   write(j_in,'(a)')'         look_at  < 0 0 0 >'
   write(j_in,'(a)')'         up y'
   write(j_in,'(a)')'         right -4/3*x'
   write(j_in,'(a)')'       }'
!
!!!!!!FIXME - improve light position
   write(j_in,'(a,2(f0.2,","),f0.2,a)')'light_source { <',cpos,'> color White }'
!
!  Disegna atomi
   fact = 0.5
   write(j_in,'(a)')'#declare molecule = union {'
   do i=1,nat
      iz = atomc(i)%kscatt()
      if (iz == 0) cycle   ! non disegno i ghost
      write(j_in,'(a)')'// Atom '//trim(atomc(i)%lab)
      write(j_in,'(a,2(f0.5,","),f0.5,a,f0.3)')'sphere{<',atomc(i)%xc,'>,',fact*elem(iz)%c_radius
      write(j_in,'(a)')'       texture { pigment {'//' color color_'//trim(elem(iz)%lab)//' } finish { Surface } } }'
   enddo
!
!  Disegna i legami
   do i=1,numbonds(legm)
      n1 = legm(i)%n1
      n2 = legm(i)%n2
      write(j_in,'(a)')'// Bond '//trim(atomc(i)%lab)//'-'//trim(atomc(i)%lab)
      xm(:) = (atomc(n1)%xc + atomc(n2)%xc) / 2
      iz = atomc(n1)%kscatt()
      texture = '       texture { pigment { color color_'//trim(elem(iz)%lab)//' } finish { Surface } }'
      call draw_cylinder_pov(j_in,atomc(n1)%xc,xm,texture)
      iz = atomc(n2)%kscatt()
      texture = '       texture { pigment { color color_'//trim(elem(iz)%lab)//' } finish { Surface } }'
      call draw_cylinder_pov(j_in,atomc(n2)%xc,xm,texture)
   enddo
!
!  Disegna la cella ovvero disegna 12 linee
   if (iscell) then
       texture = '       texture { pigment { color color_cell } finish { Surface } }'
       write(j_in,'(a)')'// Cell '
       call draw_cylinder_pov(j_in,atomc(nat+1)%xc,atomc(nat+2)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+1)%xc,atomc(nat+3)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+1)%xc,atomc(nat+4)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+5)%xc,atomc(nat+6)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+5)%xc,atomc(nat+7)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+5)%xc,atomc(nat+8)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+7)%xc,atomc(nat+4)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+7)%xc,atomc(nat+2)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+4)%xc,atomc(nat+6)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+3)%xc,atomc(nat+6)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+3)%xc,atomc(nat+8)%xc,texture)
       call draw_cylinder_pov(j_in,atomc(nat+8)%xc,atomc(nat+2)%xc,texture)
   endif
!
   write(j_in,'(a)')'                          }'
!
   write(j_in,'(a)')'object { molecule'
   write(j_in,'(a)')'         translate <0.0, 0.0, 0.0>'
   write(j_in,'(a)')'         rotate    <0.0, 0.0, 0.0>'
!!!   write(j_in,'(a)')'         scale     <1.0, 1.0, 1.0>'
   write(j_in,'(a,2(f0.5,","),f0.5,a)')'         scale     < ',scalapov, scalapov, scalapov,' >'
   write(j_in,'(a)')'       }'
!
   call fpov%fclose()
!
   end subroutine create_povrayfile

!------------------------------------------------------------------------------------------

   subroutine draw_cylinder_pov(junit,x1,x2,text)
   integer, intent(in)            :: junit
   real, dimension(3), intent(in) :: x1,x2
   character(len=*), intent(in)   :: text
!
   write(junit,'("cylinder{ <",2(f0.5,","),f0.5,">,")')x1
   write(junit,'("          <",2(f0.5,","),f0.5,">, Wire")')x2
   write(junit,'(a)')trim(text)//' }'
!
   end subroutine draw_cylinder_pov

END MODULE povray_frm
