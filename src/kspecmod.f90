module kspec_mod

implicit none

integer, dimension(64), parameter, private :: mpv=(/                              &   
        273,139537,140563, 65697, 16545,131857,135961,  2132, &
      32852,  8977,  9498,  4875,  1290,  4362,  1291,  4378, &
     131338,136458,   673,  8276, 43092, 82593,   140,    98, &
      33890,  6284, 17506, 69772, 49250, 67724,139545,131859, &
     140049,   785,   787, 17057, 66209,  8465,  8473, 41044, &
      10324,131345,135450,132363,135434,132362,135947,140570, &
       5386,   266, 82081, 34900,    84,   161, 71820, 50274, &
      16482, 65676, 32866,  2188,  1122,  4236,  4889,  9491/)

integer, dimension(194), parameter, private  :: jsiti=(/                   &
       0,11417,16666,16906,17170,17434,17674,17938,20826,21074             &   
      ,21458,21722,21850,22098,22490,22738,34570,34578,34586,16667,16907   &
      ,17171,17435,17675,17939,33059,33315,33571,34571,34579,34587,34595   &
      ,67363,-16668,-16908,17172,17436,17676,17940,18492,18740,18996       &
      ,19260,33316,33572,33580,33604,34580,34596,34604,34612,34620,34628   &
      ,34860,35116,66372,67364,67372,-67396,67404,132932,16765,17061       &
      ,17173,17581,17773,17941,18493,18741,18997,19261,19821,20141,20349   &
      ,20645,25477,33141,33461,33581,33653,33717,34581,34605,34613,34621   &
      ,34669,34677,34685,34725,34733,34741,34861,35117,36213,36533         &
      ,-50053,-51077,67445,99205,-100229,198533,16670,16910,18494,18742    &
      ,20830,21078,21462,21726,31126,31438,31678,31942,33102,33518,34590   &
      ,34598,34606,34638,34790,-34798,34862,35118,37198,37454,37870        &
      ,38126,-51094,-51134,-51142,-51150,55702,56014,56254,56518,65894     &
      ,66278,66374,-67398,67406,-67430,-67558,67566,-100238,100246         &
      ,100286,100294,100302,132934,132966,133094,-198542,395150,16767      &
      ,17063,18495,25479,-51079,18329,17678,17438,17942,33062,33318        &
      ,33574,33126,33510,33606,18998,19262,22102,22494,22742,17174,67509   &
      ,67373,67366,34574,34582,67374,34588,34572,33582,34662,34630,19263   &
      ,20351,34623,34687,34727,50055,100231,20647,21854/)

character(len=*), dimension(85), parameter, private :: sitin = (/     & 
       'h.y.s.'                                                       & 
      ,'1     ','-1    ','2..   ','.2.   ','..2   ','..2   ','..2   ' &   !  8
      ,'..2   ','..2   ','m 011 ','m 101 ','m -101','m 01-1','m..   ' &   ! 15
      ,'.m.   ','..m   ','2/m.. ','.2/m. ','..2/m ','222   ','222.  ' &   ! 22
      ,'222.. ','2mm   ','m2m   ','mm2   ','mmm   ','2.mm  ','2.22  ' &   ! 29
      ,'m.2m  ','m.m2  ','m.mm  ','4..   ','-4..  ','422   ','42.2  ' &   ! 36
      ,'4m2   ','-42m  ','-4m2  ','4m.m  ','-42.m ','-4m.2 ','4/m.. ' &   ! 43
      ,'4/mmm ','4/mm.m','3..   ','-3..  ','.3.   ','32.   ','3.2   ' &   ! 50
      ,'.32   ','3m.   ','.3m   ','.-3m  ','-3m.  ','-3.m  ','.-3m  ' &   ! 57
      ,'m-3.  ','m-3m  ','6..   ','-6..  ','6mm   ','-6m2  ','6/m.. ' &   ! 63
      ,'622   ','6/mmm ','23.   ','-43m  ','432   ','mmm.  ','2mm.  ' &   ! 71
      ,'m2m.  ','.-3.  ','32    ','3.    ','.2    ','4mm   ','3.m   ' &   ! 78
      ,'-3m   ','3m    ','mm2.. ','mmm.. ','.m    ','.2/m  ','-3.   '/)

integer, dimension(194), parameter, private  :: cvet = (/                  &
       1, 2, 4, 5, 6,15,16,17, 7,10, 8, 9,11,14,13,12,19,20,18,            &
       4, 5, 6,15,16,17,24,25,26,19,20,18,21,27, 5, 5, 4,16,16,15, 6, 6,   &   
      17,17,72,71,28,33,18,22,29,20,20,34,30,30,77,70,32,38,32,44, 5, 5,   &
       4,17,17,15, 5, 6,17,16, 6, 6,16,16,46,25,25,24,24,24,18,21,20,19,   &
      20,21,19,19,20,21,25,26,26,26,49,50,27,62,55,66, 4, 4, 6, 6, 6, 6,   &
       6, 6,48,48,48,48,28,28,18,23,29,29,34,29,31,31,31,31,31,31,51,51,   &
      73,51,53,53,53,53,40,40,40,43,32,41,41,32,54,54,54,54,54,45,45,45,   &
      68,59,76,76,76,75,74, 3,15,15,15,81,81,81,33,33,33,17,17,17,17,17,   &
       4,27,27,82,18,18,32,19,19,28,34,34,83,83,84,84,84,80,79,83,17/)

integer, dimension(25,2), parameter, private :: vcoll = reshape (         &
     (/64208,56006,56106,44208,-53312,53306,53006,63012,-53006            &
      ,44008,42002,62004,-63324,-44108,64024,-63006,-64108,63006,64008,   &
      56412,56012,-56112,64624,-73006,73006,42,60,61,39,79,80,74,67,47,   &
      35,5,29,58,43,69,73,43,51,36,63,65,64,68,85,74/),shape(vcoll))

integer, dimension(32), parameter, private :: indv=(/1,2,2,4,4,2,2,4,4,2,3,3,4,4,6,6,2,2,2,2,2,2,3,3,3,3,3,3,3,3,2,2/)

real, parameter                        :: DEF_DISTMIN = 0.6
 
integer, dimension(192,3), private :: newvet
contains

    integer  function kspecb_new(xo,xn,key,spg,jsys,gmat,iactn,iser,stype,khead,ddmin)  result(kspecb)
!
!-- function to handle atoms in special position
!
!   return value
!          kspecb = +1 atom in general position
!                 = -1 atom in special position
!-- meaning of parameters
!     xo    = array containing original values for x,y,z,occ,u's,c-occ
!     xn    = array containing modified values for x,y,z,occ,u's,c-occ
!    key    = array containing conditions for      x,y,z,    u's,c-occ
!  itype    = type of atom
!   iser    = serial number
!  khead    =  0  heading for special position to be print
!           =  1  heading not to be printed
!     iactn =     action to be taken :
!           =  1  compute crystallographic occupation
!           =  2  generate key array and update x's and u's
!           =  3  print information
!    newvet = matrix containing information about
!             the symmetry operators involved
      USE general, only: lo  !!!!,gmat
      USE spginfom
      real, dimension(11), intent(in)        :: xo   
      real, dimension(11), intent(out)       :: xn  
      integer, dimension(10), intent(out)    :: key
      type(spaceg_type), intent(in)          :: spg
      integer, intent(in)                    :: jsys
      real, dimension(3,3)                   :: gmat
      integer, intent(in)                    :: iactn
      integer, intent(in), optional          :: iser
      character(len=*), intent(in), optional :: stype
      integer, intent(inout), optional       :: khead
      real, intent(in), optional             :: ddmin
!corr      integer, dimension(192,3)              :: newvet
      integer                                :: i,n,ksite,jump
          character(len=6) :: strsite
!
      do i=1,9
         key(i)=i
      enddo
      key(10)=1
!
      if (present(ddmin)) then
          call xnuova(xo,xn,key,n,spg,gmat,ddmin,lo)
      else
          call xnuova(xo,xn,key,n,spg,gmat,DEF_DISTMIN,lo)
      endif
      kspecb=1
      if (key(10) > 1) then
          kspecb=-1
          if (iactn > 1) then
              if (key(10).ne.1) then
                  call xelles(n,key,spg)
                  call xtherm(n,key,spg,jsys)
              endif
              if (iactn > 2 .and. present(iser) .and. present(stype)) then
                  jump=2
                  call site_new(key,jump,jsys,spg,strsite,ksite)
                  call xprint(key,stype,iser,lo,khead,strsite)
              endif
          endif
      endif

      end function kspecb_new

!----------------------------------------------------------------------

      subroutine xnuova(xo,xn,key,nr,spg,gmat,ddmin,kpr)
      USE spginfom
      USE cgeom
!--
!-- subroutines to recognize if a peak is in a special position.
!-- if it is very close to it, the peak is "moved" in it.
!--
!-- check if atoms are in special position
!-- computing the distance between an atom
!-- and its symmetry equivalents
!--
      real, dimension(11), intent(in)        :: xo
      real, dimension(11), intent(out)       :: xn
      integer, dimension(10), intent(inout)  :: key
      integer, intent(out)                   :: nr
!corr      integer, dimension(192,3), intent(out) :: newvet
      type(spaceg_type), intent(in)          :: spg
      real, dimension(3,3), intent(in)       :: gmat ! metric tensor
      real, intent(in)                       :: ddmin
      integer, intent(in)                    :: kpr
      real, dimension(2)                     :: vet
      real, dimension(3)                     :: xeq
      integer, dimension(3,3)                :: m
      real, dimension(3)                     :: s,xb1,xb2
      integer                                :: i,j,k,l,ii,kt,ifi
      integer                                :: kcond,inew,ind,ifin
      real                                   :: coef,d
!
      vet(1)= 1.0
      vet(2)=-1.0
      do i=1,11
         xn(i)=xo(i)
      enddo
      do i=1,3
         xb1(i)=xo(i)
         xb2(i)=xo(i)
      enddo
!      ifin=symcent+1
      if (spg%symcent == 1) then
          ifin = 2
      else
          ifin = 1
      endif
      nr=0
!      do kt=1,ncentr
      do kt=1,spg%ncoper
         do ifi=1,ifin
            coef=vet(ifi)
            do k=1,spg%nsym
               if (k.eq.1.and.ifi.eq.1.and.kt.eq.1) cycle
               xeq = xequi_new(spg,xb2,k,coef,kt)
               call xdisteqs(xb2,xeq,gmat,d,s)
               if (d.le.ddmin) then
                   nr=nr+1
                   newvet(nr,3)=k*coef
                   do j=1,3
                      xb1(j)=xb1(j)+xb2(j)+s(j)
                   enddo
               endif
            enddo
         enddo 
      enddo
      if (nr.ne.0) then
          do l=1,nr
             k=newvet(l,3)
             coef=1.0
             if (k.lt.0) then
                 k=-k
                 coef=-1.0
             endif
             do ii=1,3
                do j=1,3
!corr                   m(ii,j)=coef*kmat(k,ii,j)
                   m(ii,j)=coef*spg%symop(k)%rot(ii,j)
                enddo
             enddo
             call xdepac(m,ind,inew,kpr)
             newvet(l,1)=inew
             newvet(l,2)=ind
          enddo
          do j=1,3
             xb1(j)=xb1(j)/float(nr+1)
          enddo
      else
          do j=1,3
             newvet(1,j)=1
          enddo
      endif
!
      kcond=nr+1
      do i=1,3
         xn(i)=xb1(i)
      enddo
      xn(11)=1.0/float(kcond)
      key(10)=kcond
      return
      end subroutine xnuova
!-----------------------------------------------------------------------
      subroutine xdepac(m,ind,inew,kpr)
!--
!-- using a code associated to the matrix in use
!-- finds out which kind of symmetry operator
!-- is involved
!--
      integer,dimension(3,3), intent(in) :: m
      integer, intent(out)               :: ind,inew
      integer                            :: i,j,in,mp,inw
      integer, intent(in)                :: kpr
!
      call xcmat(m,mp)
!
      in=0
      do 10 i=1,64
         if (mp.eq.mpv(i)) in=i
   10 continue
!
      if (in.eq.0) then
                      write(kpr,*) 'stop speclib - xdepac '
                      write(kpr,'(3i4)') ((m(i,j),j=1,3),i=1,3)
                      write(kpr,'(i10)')      mp
                      stop 'speclib - xdepac '
                    endif
!
      ind=in
      inw=1
      if (in.gt.32) then
                      inw=-1
                      in=in-32
                     endif
      inew=inw*indv(in)
      return
      end subroutine xdepac
!------------------------------------------------------------------
      subroutine xcmat(m,mp)
!--
!-- this subroutine is used to transform a rotational
!-- matrix in a unique number to use like a pointer
!-- to recognize which kind of operator is involved
!-- ( mirror , 2-fold axis etc. )
!--
      implicit none
      integer,dimension(3,3), intent(in) :: m
      integer, intent(out)               :: mp
      integer                            :: i,j,iexp
!
      mp=0
      iexp=-1
      do i=1,3
         do j=1,3
            iexp=iexp+1
            if (m(i,j).ge.0) then
                mp=mp+m(i,j)*2**iexp
            else
                mp=mp+2**iexp+2**(iexp+9)
            endif
         enddo
      enddo
      return
      end subroutine xcmat
!----------------------------------------------------------------------
      subroutine xelles(n,key,spg)
!
!-- this subroutine computes the
!-- conditions for least squares shifts
!
      USE spginfom
!corr      integer, intent(in)                   :: newvet(192,3)
      integer, intent(in)                   :: n
      type(spaceg_type), intent(in)         :: spg
      integer, dimension(10), intent(inout) :: key
      integer xeq(3),xyz(3),xyo(3),iflc(3)
      integer mult(3)
      integer :: i,j,k,l
      integer :: ixxx,ifl,ifl1,ifl2,nn,m
      real    :: coef
!
      nn=n+1
      mult(1)=-1
      mult(2)= 1
      mult(3)= 2
      xyo(1)=3
      xyo(2)=11
      xyo(3)=13
      do 10 i=1,3
         xyz(i)=xyo(i)
   10 continue
      do 30 m=1,n
         coef=1.0
         k=newvet(m,3)
         if (k.lt.0) then
                       k=-k
                       coef=-1
                     endif
         do l=1,3
            xeq(l)=0
            do j=1,3
!corr   20          xeq(l)=xeq(l)+xyo(j)*kmat(k,l,j)*coef
               xeq(l)=xeq(l)+xyo(j)*spg%symop(k)%rot(l,j)*coef
            enddo
         enddo
         do 25 j=1,3
            if (xyz(j).ne.0) then
                               xyz(j)=xyz(j)+xeq(j)
                             endif
   25 continue
   30 continue
      ifl=0
      do 35 i=1,3
      if (xyz(i).ne.0) then
                         if (mod(xyz(i),nn).ne.0) ifl=1
                       endif
   35 continue
      if (ifl.eq.0) then
                      do i=1,3
                         xyz(i)=xyz(i)/nn
                      enddo
                    endif
!
      do 40 i=1,3
         iflc(i)=0
   40 continue
      k=0
      do 60 i=1,3
      if (xyz(i).ne.0) then
      if (k.eq.0) then
                    k=1
                    if (xyz(i).lt.0) then
                                       do 50 j=1,3
                                          xyz(j)=-xyz(j)
   50                                  continue
                                     endif
                  endif
                  endif
   60 continue
      do 110 i=1,2
         if (iflc(i).eq.0) then
            if (xyz(i).ne.0) then
               k=1
               ifl2=0
               do 80 j=i+1,3
                  ifl1=0
                  do 70 l=1,3
                     if (xyz(i)*mult(l).eq.xyz(j)) then
                                                     iflc(j)=mult(l)
                                                     iflc(i)=1
                                                     ifl1=1
                                                   endif
                     if (ifl1.eq.0) then
                     if (xyz(i).eq.xyz(j)*mult(l)) then
                                                     iflc(i)=mult(l)
                                                     iflc(j)=1
                                                     ifl1=1
                                                   endif
                                    endif
   70             continue
               ifl2=ifl2+ifl1
   80          continue
                  if (ifl2.ne.0) then
                                  l=0
                                  do 90 j=1,3
                                     if (iflc(j).ne.0) then
                                                         if (l.eq.0) l=j
                                                       endif
   90                             continue
                                  do 100 j=1,3
                           if (iflc(j).ne.0) xyz(j)=xyo(l)*iflc(j)
  100                             continue
                                 endif
                             endif
                       endif
  110 continue
      do 130 i=1,3
         ifl=0
         if (xyz(i).ne.0) then
                          do j=1,3
                             do k=1,3
                                if (xyz(i).eq.xyo(j)*mult(k)) ifl=1
                             enddo
                          enddo
                          if (ifl.eq.0) xyz(i)=xyo(i)
                          endif
  130 continue
      do 150 j=1,3
         if (xyz(j).ne.0) then
            do 140 k=1,3
                    if (xyz(j).eq.xyo(k))   then
                                              xyz(j)=k
               else if (xyz(j).eq.2*xyo(k)) then
                                              xyz(j)=20+k
               else if (xyz(j).eq.-xyo(k))  then
                                              xyz(j)=-k
                                             endif
  140       continue
         endif
  150 continue
      do 155 i=1,3
        ixxx=xyz(i)
        ifl=0
        if (ixxx.lt.0) then
                       do 153 j=1,3
                          if (xyz(j).eq.-ixxx) ifl=1
  153                  continue
                       if (ifl.eq.0) xyz(i)=-xyz(i)
                     endif
  155 continue
      do i=1,3
         key(i)=xyz(i)
      enddo
      if (key(3).eq.23.or.key(3).eq.-3) key(3)=3
      return
      end subroutine xelles
!----------------------------------------------------------------------
      subroutine xprint(key,stype,iser,kpr,khead,strsite)
!
!-- print symmetry restrictions on atomic parametrs
!
!--   key  array containing symmetry restrictions
!   itype  atom type (hollerith*4)
!    iser  serial
!      xn  array containing values for atomic parameters
!
      integer, dimension(10)         :: key
      integer, intent(in)            :: iser
      character(len=*), intent(in)   :: stype
      integer, intent(in)            :: kpr
      integer, intent(inout)         :: khead
      character(len=*), intent(in)   :: strsite
      character(len=42)              :: line
      character(len=2), dimension(9) :: keyc = [' x',' y',' z','11','22','33','23','13','12']
      integer :: j,k,l,mm,kj
!
      k=1
      mm=5
      do j=1,9
         if (j.le.4) then
             mm=4
         else
             mm=5
         endif
         kj=key(j)
         l=k+5
         if (kj.ge.0) then
             if (kj.eq.0) then
                 write(line(k:l),"('  0  ')")
             else if (kj.le.j) then
                 write(line(k:l),"(' ',a2,'   ')") keyc(kj)
             else if (kj.gt.9) then
                 if (kj.gt.20) then
                     kj=kj-20
                     write(line(k:l),"(a2,'*2 ')") keyc(kj)
                 else
                     kj=kj-10
                     write(line(k:l),"(a2,'/2 ')") keyc(kj)
                 endif
             else if (kj.lt.j) then
                 write(line(k:l),"(' ',a2,'  ')") keyc(kj)
             endif
         else
             kj=-kj
             if (j.le.3) then
                 write(line(k:l),"(' -',a1,'  ')")keyc(kj)(2:2)
             else
                 write(line(k:l),"('-',a2,'  ')") keyc(kj)
             endif
         endif
         k=k+mm
      enddo
!
!--   crystals output
      if (khead == 0) then
          write(kpr,'(" Atom Serial    Symmetry Restrictions on Atomic Parameters  Site")')
          khead = 1
      endif
      write(kpr,'(2x,a4,i4,4x,a42,4x,a)') stype,iser,line,strsite
!
      end subroutine xprint
!-----------------------------------------------------------------------
      subroutine xtherm(nr,key,spg,jsys)
!
!-- this subroutine computes the conditions for
!-- thermal parameters according to:
!
!           w.j.a.m. peterse and j.h. palme
!           acta cryst. (1966). 20, 147
!
      USE spginfom
      integer :: nr
!corr      integer, intent(in)                   :: newvet(192,3)
      integer, dimension(10), intent(inout) :: key
      type(spaceg_type), intent(in)         :: spg
      integer, intent(in)                   :: jsys
      integer ivet(192),s,q(3,3),binv(9,6),ijt(6,2),itt(6)
      integer q12,q13,q23,vterm(6),bin1(6,6)
      integer :: i,j,k,l,kk,isk,ki,ict,n,kj,ifl,ich,is,js,jsc,lsc,mi,mk,ifls
      integer :: ls,ibi,ibk,jpreva,jprevb,jv
      real :: coef
!
      do 10 i=1,6
         vterm(i)=99
   10 continue
      itt(1)=1
      itt(2)=5
      itt(3)=9
      itt(4)=6
      itt(5)=3
      itt(6)=2
      do 18 k=1,6
         ict=0
         do i=1,3
            do j=1,3
               ict=ict+1
               if (ict.eq.itt(k)) then
                   ijt(k,1)=i
                   ijt(k,2)=j
               endif
            enddo
         enddo
   18 continue
      ivet(1)=1
      do 20 i=1,nr
         ivet(i+1)=newvet(i,3)
   20 continue
      n=nr+1
      kk=0
      do i=1,3
         do j=1,3
            do k=1,3
               do l=1,3
                  q(k,l)=0
                  do js=1,n
                     coef=1.0
                     s=ivet(js)
                     if (s.lt.0) then
                                   s=-s
                                   coef=-1
                                 endif
!corr                     q(k,l)=q(k,l)+kmat(s,i,k)*kmat(s,j,l)
                     q(k,l)=q(k,l)+spg%symop(s)%rot(i,k)*spg%symop(s)%rot(j,l)
                  enddo
               enddo
            enddo
            q12=q(1,2)+q(2,1)
            q13=q(1,3)+q(3,1)
            q23=q(2,3)+q(3,2)
            kk=kk+1
            do 40 k=1,3
               binv(kk,k)=q(k,k)
   40       continue
            binv(kk,4)=q12
            binv(kk,5)=q13
            binv(kk,6)=q23
         enddo
      enddo
      do k=1,6
         isk=0
         i=ijt(k,1)
         j=ijt(k,2)
         kk=itt(k)
         do l=1,6
            bin1(k,l)=binv(kk,l)
         enddo
      enddo
      if (jsys.eq.5) then
          do i=4,6
             do l=1,6
                bin1(i,l)=2*bin1(i,l)
             enddo
          enddo
      endif
      do 90 i=1,6
         ki=ijt(i,1)
         kj=ijt(i,2)
         ifl=0
         do 80 j=1,6
            if (bin1(i,j).ne.0) ifl=1
   80    continue
         if (ifl.eq.0) vterm(i)=0
   90 continue
      kk=1
      do 120 i=1,5
         ich=0
         if (vterm(i).eq.99) then
             do k=i+1,6
                do is=-1,1,2
                   do js=0,1
                      jsc=mod(js+1,2)
                      do ls=0,1
                         lsc=mod(ls+1,2)
                         mi=ls*2**js+lsc
                         mk=ls*2**jsc+lsc
                         ifls=0
                         do 100 j=1,6
                            ibi=bin1(i,j)*mi
                            ibk=bin1(k,j)*is*mk
                            if (ibi.ne.ibk) ifls=1
  100                    continue
                         if (ifls.eq.0) then
                             if (ich.eq.0) kk=kk+1
                             ich=1
                             vterm(i)=kk*mk
                             vterm(k)=kk*is*mi
                         endif
                      enddo
                   enddo
                enddo
             enddo
             if (vterm(i).eq.99) vterm(i)=1
         endif
  120 continue
      if (vterm(6).eq.99) vterm(6)=1
      jpreva=0
      jprevb=0
      do 150 i=1,6
         vterm(i)=6-vterm(i)
               if (vterm(i).eq.0) then
                                    if (jprevb.eq.0) then
                                                 vterm(i)=20+i
                                                 jprevb=i
                                                else
                                                  vterm(i)=20+jprevb
                                                endif
          else if (vterm(i).eq.3) then
                                    if (jprevb.eq.0) then
                                                  vterm(i)=i
                                                  jprevb=i
                                                else
                                                  vterm(i)=jprevb
                                                endif
          else if (vterm(i).eq.4) then
                                    if (jpreva.eq.0) then
                                                  vterm(i)=i
                                                  jpreva=i
                                                else
                                                  vterm(i)=jpreva
                                                endif
          else if (vterm(i).eq.5) then
                                    vterm(i)=i
          else if (vterm(i).eq.6) then
                                    vterm(i)=0
          else if (vterm(i).eq.9) then
                                    if (jprevb.eq.0) then
                                                  vterm(i)=-i
                                                  jprevb=i
                                                else
                                                  vterm(i)=-jprevb
                                                endif
                                  endif
  150 continue
      do j=4,9
         i=j-3
         jv=vterm(i)
         if (jv.ne.0) then
            if (jv.le.9) then
                jv=jv+3*isign(1,jv)
            else
                jv=(mod(jv,20)+3)+20
            endif
         endif
         key(j)=jv
      enddo
      if (key(9).le.6.and.key(9).ge.4) key(9)=key(9)+10
      return
      end subroutine xtherm
!----------------------------------------------------------------------
      function xequi_new(spg,xb2,k,coef,kt)  result(xeq)
      USE spginfom
!--
!--   computes the symmetry equivalent of an atom
!--
      type(spaceg_type), intent(in)  :: spg
      real, dimension(3), intent(in) :: xb2
      integer, intent(in)            :: k
      real, intent(in)               :: coef
      integer, intent(in)            :: kt
      real, dimension(3)             :: xeq

      xeq=coef*matmul(spg%symop(k)%rot(:,:),xb2) + spg%symop(k)%trn(:) + spg%coper(:,kt)

      end function xequi_new

!-----------------------------------------------------------------------
      subroutine site_new(key,jump,jsys,spg,string,k)
      USE spginfom
      USE iso_fortran_env
!
!
!--   lsq shifts  code           therm. param. restr.  code
!--
!--      x 0 0      110000             - - - 0 - 0      1
!--      0 y 0      001200             - - - - 0 0      2
!--      0 0 z      000013             - - - 0 0 -      3
!--      0 y z      001213             - - - 0 0 0      4
!--      x 0 z      110013             a a - - 0 0      5
!--      x y 0      111200             a a - - b b      6
!--      0 0 0      000000             a a - - b-b      7
!--      x x 0      111100             a a - 0 0 0      8
!--      x-x 0      110900             - a a 0 0 -      9
!--      x x z      111113             - a a b b -     10
!--      x-x z      110913             - a a-b b -     11
!--      x y z      111213             - a a 0 0 0     12
!--      x2x 0      113100             - a - a - 0     13
!--     2x x 0      311100             - a - a 0 0     14
!--      x2x z      113113             - a - a b2b     15
!--     2x x z      311113             a a - a 0 0     16
!--      0 y y      001212             a a a 0 0 0     17
!--      0 y-y      001208             a a a b b b     18
!--      x 0 x      110011             - - - - - -     19
!--      x 0-x      110009             a - - a2b b     20
!--      x y-y      111208             a - - a 0 -     21
!--      x y y      111212             a - - a 0 0     22
!--      x y x      111211             a a a b-b b     23
!--      x y-x      111209             a a a b b-b     24
!--      x x x      111111             a a a b-b-b     25
!--      x x-x      111109             a - a b --b     26
!--      x-x x      110911             a - a b - b     27
!--      x-x-x      110909             a - a 0 0 0     28
!--
!
!corr      integer, intent(in)                :: newvet(192,3)
      integer, dimension(10), intent(in) :: key
      integer, intent(in)                :: jump
      integer, intent(in)                :: jsys
      type(spaceg_type), intent(in)      :: spg
!corr      integer, intent(in)                :: kpr
      character(len=6), intent(out)      :: string
      integer, intent(out)               :: k
!corr      character buff*80
!corr      character*124 cbuff
!corr      integer, dimension(:) :: lsqv(28),lterm(2,30)
!corr      character         string*6
!corr      character lsqt(28)*6,itrt(30)*12,tenp1(85)*6
!corr      integer temp(194),temp3(194),tempo2(25,2),mpvv(64),ksiti(64)
!corr      common /tab1/ temp,tempo2,temp3,ksiti,mpvv
!corr      common /tab2/ tenp1,lsqt,itrt
!corr      common /atpos/ cbuff
      integer, dimension(28), parameter :: lsqv = (/                      &
      111010,101210,101013,101213,111013,111210,101010,                   &  
      111110,110910,111113,110913,111213,113110,311110,113113,311113,     &
      101212,101208,111011,111009,111208,111212,111211,111209,111111,     &
      111109,110911,110909/)
      integer, dimension(2,30), parameter :: lterm = reshape((/           &
                 141516,101810,141516,101019,141516,171010,141516,101010  &
                ,141416,101019,141416,171719,141416,170319,141416,101010  &
                ,141515,171010,141515,171818,141515,171802,141515,101010  &
                ,141516,101825,141516,101025,141516,371725,141416,101024  &
                ,141414,101010,141414,171717,141516,171819,141516,173724  &
                ,141516,171024,141516,101024,141414,170317,141414,170303  &
                ,141414,171703,141514,171803,141514,171817,141514,101010  &
                ,141514,101810,101010,101010/), shape(lterm))
      integer :: i,i1,i2,kk,kcoll,icont,ks,jj,num,maxo,meno1,mm
      integer :: ilsq,it1,it2,ibuff,ilsqs,iterm,nm,ifin,msys,kkss,mult
!
      string = ' '
      mult=10000
      ilsq=0
      do i=1,3
         ilsq=ilsq+(key(i)+10)*mult
         mult=mult/100
      enddo
      k=0
      do 20 i=1,28
      if (ilsq.eq.lsqv(i)) k=i
   20 continue
      if (jump.eq.0) return
      mult=10000
      it1=0
      it2=0
      do i=1,3
         i1=i+3
         i2=i+6
         it1=it1+(key(i1)+10)*mult
         it2=it2+(key(i2)+10)*mult
         mult=mult/100
      enddo
      kk=0
      do 40 i=1,30
      if (it1.eq.lterm(1,i).and.it2.eq.lterm(2,i)) kk=i
   40 continue
      ibuff=0
      if (k.eq.0.or.kk.eq.0) then
          ibuff=1
!corr          write(buff,2000) key,ilsq,it1,it2,k,kk
          write(ERROR_UNIT,2000) key,ilsq,it1,it2,k,kk
 2000     format(' errore ',10i3,3i8,5x,2i4)
      endif
      ilsqs=k
      iterm=kk
      nm=key(10)
      k=nm*10000+ilsqs*100+iterm
      ifin=194
      msys=jsys
!corr      if (latt.eq.7) msys=7
      if (spg%lattyp == 'R') msys=7
!corr      kkk=msys*1000000+k
      kcoll=0
      icont=0
      ks=1
      jj=msys
      if (k.eq.11219.or.k.eq.20719) jj=1
      num=nm*8192+ilsqs*256+iterm*8+jj
      do 50 i=2,ifin
         if (num.eq.iabs(jsiti(i))) then
                             icont=icont+1
                             ks=i
                                    endif
   50 continue
      if (jsiti(ks).ge.0) then
                            kkss=cvet(ks)
                            string=sitin(kkss)
                          else
          maxo =0
          meno1=1
          mm   =0
! -- look for the maximum order operator, for -1 and for mirrors
          do 60 i=1,nm-1
             if (iabs(newvet(i,1)).gt.maxo) maxo=iabs(newvet(i,1))
             if (newvet(i,1).eq.-1) meno1=-1
             if (newvet(i,1).eq.-2) mm=mm+1
   60     continue
          kcoll=meno1*(msys*10000+maxo*1000+mm*100+nm)
          kkss=1
          do 70 i=1,25
             if (vcoll(i,1).eq.kcoll) kkss=vcoll(i,2)
   70     continue
          string=sitin(kkss)
      endif
!corr          cbuff(65:70) = string
!corr  if (ibuff.eq.1) then
!corr              write(kpr,'(a)') buff
!corr      write(kpr,'(1h )')
!corr          endif
      if (kkss.eq.1) k=-1
      return
      end subroutine site_new

end module kspec_mod
