!-----------------------------------------------------------------------
!     MATH ROUTINES
!-----------------------------------------------------------------------
      subroutine autov(a,eigen,t,icod)
      implicit real *8 (a-h,o-z)
      real *4 t(3,3),a(3,3),aik(3),eigen(3),aaa
!
      itmax=50
      n=3
      nm1=n-1
      eps1=.1e-10
      eps2=eps1
      eps3=.1e-9
      sigma1=0
      offdsq=0
      do i=1,3
         do j=1,3
            t(i,j)=0.
         enddo
      enddo
      do 5 i=1,n
      sigma1=sigma1+a(i,i)**2
      t(i,i)=1.0
      ip1=i+1
      if (i.ge.n) go to 6
      do 5 j=ip1,n
   5  offdsq=offdsq+a(i,j)**2
   6  s=2.*offdsq+sigma1
!
!  inizio iter. jacobi
!
      do 26 iter=1,itmax
      do 20 i=1,nm1
      ip1=i+1
      do 20 j=ip1,n
      q=abs(a(i,i)-a(j,j))
      if(q.le.eps1) goto 9
      if(abs(a(i,j)).le.eps2) goto 20
      p=2.*a(i,j)*q/(a(i,i)-a(j,j))
      spq=sqrt(p*p+q*q)
      csa=sqrt((1.+q/spq)/2.)
      sna=p/(2.*csa*spq)
      goto 10
    9 csa=1./sqrt(2.d0)
      sna=csa
   10 continue
!
      do k=1,n
         holdki=t(k,i)
         t(k,i)=holdki*csa+t(k,j)*sna
         t(k,j)=holdki*sna-t(k,j)*csa
      end do
!
      do k=i,n
         if(k.le.j) then
            aik(k)=a(i,k)
            a(i,k)=csa*aik(k)+sna*a(k,j)
            if(k.eq.j) then
               a(j,k)=sna*aik(k)-csa*a(j,k)
            end if
         else
            holdik=a(i,k)
            a(i,k)=csa*holdik+sna*a(j,k)
            a(j,k)=sna*holdik-csa*a(j,k)
         end if
      end do
!
      aik(j)=sna*aik(i)-csa*aik(j)
!
      do k=1,j
         if (k.le.i) then
            holdki=a(k,i)
            a(k,i)=csa*holdki+sna*a(k,j)
            a(k,j)=sna*holdki-csa*a(k,j)
         else
            a(k,j)=sna*aik(k)-csa*a(k,j)
         endif
      end do
   20 a(i,j)=0.0
!
      sigma2=0.0
      do i=1,n
         eigen(i)=a(i,i)
         sigma2=sigma2+eigen(i)**2
      end do
      if (abs(sigma2).le.1.e-07) then
         icod = 0
         return
      end if
      if (abs(sigma2).gt.1.e-07) then
         aaa = 1.0-sigma1/sigma2
         if(aaa.ge.eps3) goto 25
      end if
      icod=1
      return
   25 sigma1=sigma2
   26 continue
      icod=0
!
      return
      end
