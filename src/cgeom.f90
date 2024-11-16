  MODULE CGEOM

  implicit none

  private distanzaCris, distanzaCart, angleCris, angleCart
  interface distanzaC
     module procedure distanzaCris,distanzaCart
  end interface

  interface angleC
     module procedure angleCris,angleCart
  end interface

  CONTAINS

   real function distanzaCris(x1,x2,gm)
!
!  Calcola la distanza tra 2 atomi in coord. frazionarie
!
   implicit none
   real, intent(in), dimension(3)     :: x1,x2     ! coordinate dei 2 atomi
   real, intent(in), dimension(3,3)   :: gm        ! matrice metrica
   real, dimension(3)                 :: dx
!
   dx = x1-x2
   distanzaCris = sqrt(DOT_PRODUCT(dx,MATMUL(gm,dx)))
!
   end function distanzaCris

!-------------------------------------------------------------------------

   real function distanzaCrisFast(x1,x2,gdist)
!
!  Calcola la distanza tra 2 atomi in coord. frazionarie
!
   implicit none
   real, intent(in), dimension(3) :: x1,x2     ! coordinate dei 2 atomi
   real, dimension(6), intent(in) :: gdist
   real, dimension(3)             :: dx
!
   dx = x1-x2
   distanzaCrisFast = sqrt(dx(1)*dx(1)*gdist(1) + dx(2)*dx(2)*gdist(2) + dx(3)*dx(3)*gdist(3) + &
                           dx(1)*dx(2)*gdist(6) + dx(1)*dx(3)*gdist(5) + dx(2)*dx(3)*gdist(4))
!
   end function distanzaCrisFast

!-------------------------------------------------------------------------

   real function distanzaCart(x1,x2)
!
!  Calcola la distanza tra 2 atomi in coord. frazionarie
!
   implicit none
   real, intent(in), dimension(3)     :: x1,x2     ! coordinate dei 2 atomi
   real, dimension(3)                 :: dx
!
   dx = x1-x2
   distanzaCart = sqrt(DOT_PRODUCT(dx,dx))
!
   end function distanzaCart

!-------------------------------------------------------------------------

   real function xdistance(x1,x2,orto)
!
!  Calcola la distanza tra 2 atomi in coord. frazionarie by using ortho matrix
!
   implicit none
   real, dimension(3), intent(in)   :: x1,x2     ! fractional coordinates
   real, dimension(3,3), intent(in) :: orto
   real, dimension(3)               :: xmat
!
   xmat = matmul(orto,x2-x1)
   xdistance = sqrt(dot_product(xmat,xmat))
!
   end function xdistance

!-------------------------------------------------------------------------

   real function angleCris(x1,x2,x3,gm)
!
!  Calcola l'angolo in radianti tra 3 atomi in coordinate cristallografiche
!
   implicit none
   real, intent(in), dimension(3)   :: x1,x2,x3  ! coordinate dei tre atomi
   real, intent(in), dimension(3,3) :: gm        ! matrice metrica
   real                             :: dis12,dis23,XGX
   real                             :: argcos
!
   dis12 = distanzaC(x1,x2,gm)
   dis23 = distanzaC(x2,x3,gm)
   XGX = DOT_PRODUCT(x1-x2,MATMUL(gm,x3-x2))
   argcos = XGX / (dis12 * dis23)
   if (argcos > 1.0) argcos = 1.0
   if (argcos < -1.0) argcos = -1.0
   angleCris = acos ( argcos )
!
   end function angleCris

!-------------------------------------------------------------------

   real function angleCart(x1,x2,x3)
!
!  Calcola l'angolo in radianti tra 3 atomi in coordinate cartesiane
!
   implicit none
   real, intent(in), dimension(3)             :: x1,x2,x3  ! coordinate dei tre atomi
   real                                       :: dis12,dis23,XGX
   real                                       :: argcos
!
   dis12 = sqrt(dot_product(x1-x2,x1-x2))
   dis23 = sqrt(dot_product(x2-x3,x2-x3))
   XGX = dot_product(x1-x2,x3-x2)
   argcos = XGX / (dis12 * dis23)
   if (argcos > 1.0) argcos = 1.0
   if (argcos < -1.0) argcos = -1.0
   angleCart = acos ( argcos )
!
   end function angleCart

!-------------------------------------------------------------------

   real function angle_vectors(vet1,vet2,gm)
   USE trig_constants, only:rtod
!
!  Calcola l'angolo tra 2 vettori
!
   implicit none
   integer, dimension(3)     :: vet1,vet2 ! componenti dei due vettori
   real,    dimension(3,3)   :: gm        ! matrice metrica
   real                      :: modvet1,modvet2
   real                      :: argcos
   real                      :: prod_vettor
!
   modvet1 = sqrt(DOT_PRODUCT(vet1,MATMUL(gm,vet1)))  !modulo del vettore vet1
   modvet2 = sqrt(DOT_PRODUCT(vet2,MATMUL(gm,vet2)))  !modulo del vettore vet2
   prod_vettor = DOT_PRODUCT(vet1,MATMUL(gm,vet2))   !prodotto scalare tra vet1 e vet2
!
   argcos = prod_vettor / (modvet1 * modvet2)
   if (argcos > 1.0) argcos = 1.0
   if (argcos < -1.0) argcos = 1.0
   angle_vectors = acos (argcos) * rtod
!
   end function angle_vectors

!-------------------------------------------------------------------------

   real function xdisteq(xb2,xeq,gm)  result(dist)
!
!  Function used to compute the distance between an atom and its equivalent
!
   real, dimension(3), intent(in)   :: xb2,xeq
   real, dimension(3,3), intent(in) :: gm
   real, dimension(8,3)             :: delta
   real, dimension(3)               :: dp,ds
   real, dimension(8)               :: dd
   real                             :: d
   integer                          :: i
   real                             :: dmin
!
   do i=1,3
      d=xeq(i)-xb2(i)
      dp(i)=d-int(d)
      if (dp(i) <= 0.0) then
          ds(i)=dp(i)+1.0
      else
          ds(i)=dp(i)-1.0
      endif
   enddo
   delta(1,:) = dp
   delta(2,:) = (/dp(1),dp(2),ds(3)/)
   delta(3,:) = (/dp(1),ds(2),dp(3)/)
   delta(4,:) = (/dp(1),ds(2),ds(3)/)
   delta(5,:) = (/ds(1),dp(2),dp(3)/)
   delta(6,:) = (/ds(1),dp(2),ds(3)/)
   delta(7,:) = (/ds(1),ds(2),dp(3)/)
   delta(8,:) = ds
!
   dmin = DOT_PRODUCT(delta(1,:),MATMUL(gm,delta(1,:)))
   do i=2,8
      dd(i) = DOT_PRODUCT(delta(i,:),MATMUL(gm,delta(i,:)))
      if (dd(i) < dmin) dmin = dd(i)     
   enddo
   dist = sqrt(dmin)
!
   end function xdisteq

!-------------------------------------------------------------------------

   subroutine xdisteqs(xb2,xeq,gm,dist,ddelta)
!
!  Subroutine used to compute the distance between an atom and its equivalent
!  xeq' = xb2 + ddelta and dist = distance(xb2,xeq')
!  Cell translation to apply to xeq is: ddelta - (xeq - xb2)
!
   real, dimension(3), intent(in)   :: xb2,xeq
   real, dimension(3,3), intent(in) :: gm
   real, intent(out)                :: dist
   real, dimension(3), intent(out)  :: ddelta
   real, dimension(8,3)             :: delta
   real, dimension(3)               :: dp,ds
   real, dimension(8)               :: dd
   real                             :: d
   integer, dimension(1)            :: lmin
   integer                          :: i
!
   do i=1,3
      d=xeq(i)-xb2(i)
      dp(i)=d-int(d)
      if (dp(i) <= 0.0) then
          ds(i)=dp(i)+1.0
      else
          ds(i)=dp(i)-1.0
      endif
   enddo
   delta(1,:) = dp
   delta(2,:) = (/dp(1),dp(2),ds(3)/)
   delta(3,:) = (/dp(1),ds(2),dp(3)/)
   delta(4,:) = (/dp(1),ds(2),ds(3)/)
   delta(5,:) = (/ds(1),dp(2),dp(3)/)
   delta(6,:) = (/ds(1),dp(2),ds(3)/)
   delta(7,:) = (/ds(1),ds(2),dp(3)/)
   delta(8,:) = ds
!
   do i=1,8
      dd(i) = dot_product(delta(i,:),matmul(gm,delta(i,:)))
   enddo
   lmin = minloc(dd)
   dist = sqrt(dd(lmin(1)))
   ddelta = delta(lmin(1),:3)
!
   end subroutine xdisteqs

!-------------------------------------------------------------------------

   subroutine xdisteqs_intra(xb2,xeq,gm,dist,ddelta)
!
!  Subroutine used to compute the distance between an atom and its equivalent
!  xeq = xb2 + ddelta
!  Cell translation to apply to xeq is: ddelta - (xeq - xb2)
!
   real, dimension(3), intent(in)   :: xb2,xeq
   real, dimension(3,3), intent(in) :: gm
   real, intent(out)                :: dist
   real, dimension(3), intent(out)  :: ddelta
   real, dimension(7,3)             :: delta
   real, dimension(3)               :: dp,ds
   real, dimension(7)               :: dd
   real                             :: d
   integer, dimension(1)            :: lmin
   integer                          :: i
!
   do i=1,3
      d=xeq(i)-xb2(i)
      dp(i)=d-int(d)
      if (dp(i) <= 0.0) then
          ds(i)=dp(i)+1.0
      else
          ds(i)=dp(i)-1.0
      endif
   enddo
!corr   delta(1,:) = dp
   delta(1,:) = (/dp(1),dp(2),ds(3)/)
   delta(2,:) = (/dp(1),ds(2),dp(3)/)
   delta(3,:) = (/dp(1),ds(2),ds(3)/)
   delta(4,:) = (/ds(1),dp(2),dp(3)/)
   delta(5,:) = (/ds(1),dp(2),ds(3)/)
   delta(6,:) = (/ds(1),ds(2),dp(3)/)
   delta(7,:) = ds
!
      !write(70,*)0,'DD=',sqrt(dot_product(dp,matmul(gm,dp))),xb2 + dp - xeq
   do i=1,7
      dd(i) = dot_product(delta(i,:),matmul(gm,delta(i,:)))
      !!write(70,*)i,'DD=',sqrt(dd(i)),xb2 + delta(i,:) - xeq
   enddo
   lmin = minloc(dd)
   dist = sqrt(dd(lmin(1)))
   ddelta = delta(lmin(1),:3)
!
   end subroutine xdisteqs_intra

!-------------------------------------------------------------------------

   subroutine distance_equivalent(x1,x2,xg,spg,dmin,x1eq,ks) 
!
!  Find minimum distance between x1 and all equivalent of x2. ks=2 for only equivalent atoms
!
   USE spginfom
   real, dimension(3), intent(in)  :: x1,x2
   real, dimension(3,3),intent(in) :: xg
   type(spaceg_type), intent(in)   :: spg
   real, intent(out)               :: dmin
   real, dimension(3), intent(out) :: x1eq ! equivalent of x1 close to x2
   integer, intent(in)             :: ks
   real, dimension(3)              :: xeq, ktra, ktramin
   integer                         :: k
   real                            :: dd
!
   dmin = huge(1.0)
   do k=ks,spg%nsymop
      xeq = matmul(spg%symop(k)%rot,x2) + spg%symop(k)%trn
      call xdisteqs(x1,xeq,xg,dd,ktra)
      if (dd < dmin) then
          dmin = dd
          ktramin = ktra
      endif
   enddo
   x1eq = x1 + ktramin
!
   end subroutine distance_equivalent

!--------------------------------------------------------------------------

   subroutine lsqplane(at,coef)
!
!  Piano nella forma nx=-p; coef(:)=nx,ny,nz,p; d(x0-piano) = n*x0+p
!
   implicit none
   real, dimension(:,:), intent(inout) :: at     !coordinate cartesiane atomi
   real, dimension(4), intent(out)     :: coef   !componenti del vettore perpendicolare al piano
   integer                             :: nat
   real, dimension(3,3)                :: sm
   real, dimension(3)                  :: eval
   real, dimension(3,3)                :: evec
   integer                             :: icod
   integer                             :: i
   integer, dimension(1)               :: lmin
   real, dimension(3)                  :: bar
!
!  num. degli atomi
   nat = size(at,2)
!
!  sposta atomi in modo da avere l'origine sul baricentro(=sum(at(i,:)) / nat)
   do i=1,3
      bar(i) = sum(at(i,:)) / nat  
      at(i,:) = at(i,:) - bar(i)
   enddo
!
   sm = matmul(at,transpose(at))
!
!  calcola autovalori (eval) e autovettori (evec) della matrice sm
   call autov(sm,eval,evec,icod)
!
!  prendi l'autovettore coincidente con l'autovalore minimo
   lmin = minloc(eval)
   coef(:3) = evec(:,lmin(1))
   coef(4) = -dot_product(coef(:3),bar(:))
     !do i=1,nat
     !   write(0,*)i,'dist. dal piano=',dot_product(coef(:3),at(:,i)+bar(:))+coef(4)
     !enddo
!
   end subroutine lsqplane

 !----------------------------------------------------------------------------------------------------

   subroutine get_equivalent(x,gm,spg,xeq,neq)
   USE spginfom
   real, dimension(3), intent(in) :: x
   real, dimension(3,3), intent(in) :: gm
   type(spaceg_type), intent(in)    :: spg
   real, dimension(3)             :: xe
   real, dimension(3)             :: s
   real, dimension(3,spg%nsymop)  :: xeq
   real                           :: d
   integer                        :: neq
   integer                        :: i,k
!
   neq = 1
   xeq(:,1) = matmul(spg%symop(1)%rot(:,:),x) + spg%symop(1)%trn(:)
   loop_symm: do k=2,spg%nsymop
      xe(:) = matmul(spg%symop(k)%rot(:,:),x) + spg%symop(k)%trn(:)
      do i=1,neq
        call xdisteqs(xe,xeq(:,i),gm,d,s)
        if(d < 0.3) cycle loop_symm
      enddo
      neq = neq + 1
      xeq(:3,neq)=xe(:)
   enddo loop_symm
!
   end subroutine get_equivalent

 !----------------------------------------------------------------------------------------------------

   subroutine get_equivalent1(x,spg,xeq)  
   USE spginfom
   real, dimension(3), intent(in) :: x
   type(spaceg_type), intent(in)  :: spg
   real, dimension(3,spg%nsymop)  :: xeq
   integer                        :: k
!
   do k=1,spg%nsymop
      xeq(:,k) = matmul(spg%symop(k)%rot(:,:),x) + spg%symop(k)%trn(:)
   enddo
!
   end subroutine get_equivalent1

!----------------------------------------------------------------------------------------------------

   function Angle_Dihedral(ri,rj,rk,rn) result(angle)
!
!  Dati quattro atomi definiti dai vettori ri,rj,rk,rn calcola l'angolo di torsione
!   
   USE trig_constants, only:rtod
   real, dimension(3), intent(in) :: ri,rj,rk,rn
   real                           :: angle
   real, dimension(3)             :: u,v,w
   real                           :: uvmod,vwmod,sig
   real, dimension(3)             :: uv,vw
   real, parameter                :: eps = 0.00001
   real                           :: arg
!
   u = rj-ri
   v = rk-rj
   w = rn-rk
!
   angle=0.0
!
   uv=cross_product(u,v)
   vw=cross_product(v,w)
   !sig = -sign(1.0, dot_product(cross_product(uv,vw),v))  ! sembra errato!
   sig = sign(1.0, dot_product(cross_product(uv,vw),v))
   uvmod=sqrt(dot_product(uv,uv))
   vwmod=sqrt(dot_product(vw,vw))
   if (uvmod < eps .or. vwmod < eps) return
   arg = dot_product(uv,vw)/uvmod/vwmod
   if (arg > 1.0) arg = 1.0
   if (arg < -1.0) arg = -1.0
   angle=rtod*acos(arg)*sig
!   
   End Function Angle_Dihedral
   
!----------------------------------------------------------------------------------------------------

   function Cross_Product(u,v) Result(w)       
   real, dimension(3), intent( in) :: u,v
   real, dimension(3)              :: w
!
   w(1)=u(2)*v(3)-u(3)*v(2)  ! i  j   k !
   w(2)=u(3)*v(1)-u(1)*v(3)  !u1  u2  u3! = (u2.v3 - u3.v2)i + (v1.u3 - u1.v3)j + (u1.v2-u2.v1)k
   w(3)=u(1)*v(2)-u(2)*v(1)  !v1  v2  v3!
!
   end function Cross_Product

!----------------------------------------------------------------------------------------------------
   
   function rand_rotation_matrix(x)  result(rmat)
!
!  Ref. 'Fast Random Rotation Matrices', J. Arvo (Internet)
!   
   USE trig_constants, only:twopi
   real, dimension(3), intent(in)    :: x     ! three random variables in the range [0,1]
   real, dimension(3,3)              :: rmat  ! rotation matrix
   real                              :: theta,phi,z
   real                              :: r
   real                              :: Vx,Vy,Vz
   real                              :: st,ct
   real                              :: Sx,Sy
!
   theta = twopi*x(1)
   phi   = twopi*x(2)
   z     = 2*x(3)
!   
   r = sqrt(z)
   Vx = cos(phi)*r
   Vy = sin(phi)*r
   Vz = sqrt(2.0-z)
!
   st = sin(theta)
   ct = cos(theta)
   Sx = Vx*ct - Vy*st
   Sy = Vx*st + Vy*ct
!
   rmat(1,1) = Vx*Sx - ct
   rmat(1,2) = Vx*Sy - st
   rmat(1,3) = Vx*Vz
   
   rmat(2,1) = Vy*Sx + st
   rmat(2,2) = Vy*sy - ct
   rmat(2,3) = Vy*Vz
   
   rmat(3,1) = Vz*Sx
   rmat(3,2) = Vz*Sy
   rmat(3,3) = 1.0-z
!
   end function rand_rotation_matrix
   
!----------------------------------------------------------------------------------------------------

   function rotation_matrix(l,m,n,theta)  result(rmat)
!
!  Rotation matrix discibing a rotation through an angle theta about an axis with direction cosines l,m.n
!  Ref. 'Quaternions transformation of molecular orientation', Mackay  (Acta Cryst.)   
!
   real, intent(in)     :: l,m,n  !direction cosines
   real, intent(in)     :: theta  !angolo di rotazione in radianti
   real, dimension(3,3) :: rmat
   real                 :: st,ct,dt
   real                 :: mld,nld,nmd
   real                 :: ls,ms,ns
!
   st = sin(theta)
   ct = cos(theta)
   dt = 1 - ct
!
   rmat(1,1) = dt*l**2 + ct       
   rmat(2,2) = dt*m**2 + ct
   rmat(3,3) = dt*n**2 + ct
!
   mld = m*l*dt
   ns =  n*st
   rmat(1,2) = mld + ns
   rmat(2,1) = mld - ns
!
   nld = n*l*dt
   ms = m*st
   rmat(1,3) = nld - ms
   rmat(3,1) = nld + ms
!
   nmd = n*m*dt
   ls = l*st
   rmat(2,3) = nmd + ls
   rmat(3,2) = nmd - ls        
!   
   end function rotation_matrix  
     
!--------------------------------------------------------------------------------------------

   Subroutine Get_Cartesian_from_Z(ci,ri,rj,rk,rn)
!   
!  Calculate the cartesian coordinates of an atom (i)
!  when its distance (dij=ci(1)) to another atom (j), the angle (aijk=ci(2))
!  spanned with another atom (k) centred at (j), the torsion angle
!  (bijkn=ci(3)) with a fourth atom (n) and the coordinates of
!  the three atoms (jkn), rj,rk,rn are all given.
!
   USE trig_constants, only:dtor
   real, dimension(3), intent ( in) :: ci,rj,rk,rn
   real, dimension(3), intent (out) :: ri
   real                             :: ca,cb,sa
   real, dimension(3)               :: r,e1,e2,e3
   real, dimension(3,3)             :: M

   ca = cos(ci(2)*dtor)              ! cos(aijk)
   sa = sqrt(abs(1.0 - ca*ca))       ! sin(aijk)
   cb = cos(ci(3)*dtor)              ! cos(bijkn)
   r(1) = ci(1) * ca                 ! Coordinates in the local system
   r(2) = ci(1)*cb*sa
   r(3) = ci(1)*sqrt(abs(1.0 - ca*ca - sa*sa*cb*cb )) *sign(1.0,ci(3))

   e1  = rk - rj
   e1  = e1/sqrt(dot_product(e1,e1))
   e3  = cross_product( rk - rj, rn - rk)
   e3  = e3/sqrt(dot_product(e3,e3))
   e2  = cross_product( e3, e1)
   M(:,1) = e1
   M(:,2) = e2
   M(:,3) = e3
   ri = rj + matmul(M,r)

   return
   End Subroutine Get_Cartesian_from_Z

!--------------------------------------------------------------------------------------------

   Subroutine Get_Z_from_Cartesian(ci,ri,rj,rk,rn)
!  Subroutine to calculate the distance of an atom (i)
!  (dij=ci(1)) to another atom (j), the angle (aijk=ci(2))
!  spanned with another atom (k) centred at (j) and  the torsion angle
!  (bijkn=ci(3)) with a fourth atom (n) when the cartesian coordinates are given
   USE trig_constants, only:rtod
   real, dimension(3), intent ( in) :: ri,rj,rk,rn
   real, dimension(3), intent (out) :: ci
   real                             :: dji,djk
   real, dimension(3)               :: rji,rjk

   rji = ri-rj
   ci(1) = sqrt(dot_product(rji,rji))
   rjk = rk-rj
   dji = ci(1)
   djk = sqrt(dot_product(rjk,rjk))
   ci(2) = rtod*acos( dot_product(rji,rjk)/dji/djk)
      
   ci(3) = angle_dihedral(ri,rj,rk,rn)
   if (abs(ci(3)+180.00) <= 0.001) ci(3)=180.0

   return
   End Subroutine Get_Z_from_Cartesian

!--------------------------------------------------------------------------------------------

   function plane3points(p1,p2,p3,ptype)  result(vp)
!
!  Piano per 3 punti in due forme diverse
!  ptype = 1 --> nx=-p
!  ptype = 2 --> ax+by+cz+d=0
!  N.B. per ptype = 1 la distanza punto-piano si calcola come: n*x0+p
!
   real, dimension(3), intent(in) :: p1,p2,p3
   integer, intent(in), optional  :: ptype
   real, dimension(4)             :: vp
   real, dimension(3)             :: np
   integer                        :: ktype
!
   if (present(ptype)) then
       ktype = ptype
   else
       ktype = 1
   endif
!
!  vettore normale al piano
   np(:) = cross_product(p2-p1,p3-p1)
!
!  Piano nella forma ax+by+cz+d=0; vp(:)=a,b,c,d
   vp(:3) = np(:)
   vp(4) = -dot_product(vp(:3),p1)
!
!  Piano nella forma nx=-p; vp(:)=nx,ny,nz,p; d(x0-piano) = n*x0+p
   if (ktype == 1) then
       vp(:) = vp(:) / sqrt(dot_product(np,np))
   endif
!
   end function plane3points

!--------------------------------------------------------------------------------------------

   function direction_cos(p1,p2)
!
!  Direction cosines of vector between 2 points in cartesian coordinates
!
   real, dimension(3), intent(in) :: p1,p2
   real, dimension(3)             :: direction_cos, pd
   pd = p2 - p1
   direction_cos = pd / sqrt(pd(1)*pd(1) + pd(2)*pd(2) + pd(3)*pd(3))
   end function direction_cos

!----------------------------------------------------------------------------------------------------

   function dangle_norm(angle) result(dang)
!   
!  Normalizza un angolo in gradi tra -180 e 180
!
   real, intent(in) :: angle
   real             :: dang
!   
   dang = mod(angle+1080,360.0)   ! 1080= 180*6
   if (dang > 180) dang = dang - 360
!
   end function dangle_norm

  END MODULE CGEOM
