module alphastrip_mod

implicit none

contains

   subroutine alphastrip(datas)
   use datasetmod, only: dataset_type
   use trig_constants, only: dtor, rtod
!
   type(dataset_type), intent(inout) :: datas 
   real, dimension(7,3) :: pweight = reshape((/-0.152766460,-0.268687600,-0.110173100,           &
                                                0.000000000,0.000000000,0.000000000,             &
                                                0.000000000,-0.055589500,-0.097177000,           &
                                               -0.268687600,-0.060190400,-0.049982700,           &
                                                0.000000000,0.000000000,-0.024976420,            &
                                               -0.054663300,-0.073126730,-0.268687600,           &
                                               -0.026679360,-0.049703870,-0.033789870/),(/7,3/))
   real, dimension(7,3) :: alevers = reshape((/1.002353500,1.002453600,1.002578830,              &
                                               0.000000000,0.000000000,0.000000000,              &
                                               0.000000000,1.002278320,1.002390960,              &
                                               1.002453600,1.002528660,1.002641430,              &
                                               0.000000000,0.000000000,1.002240830,              &
                                               1.002315940,1.002391060,1.002453600,              &
                                               1.002516240,1.002591350,1.002666470/),(/7,3/))
   integer              :: i,j
   real                 :: count
   integer              :: i1,i2
   integer              :: isc
   real                 :: s,st
   real                 :: th,thetaa,thle,thwe,ttheta,tthetaa
   real                 :: ymin
   integer              :: istog
   integer              :: ncounts
!
   ncounts = size(datas%x)
   istog = 3
   select case (istog)
      case (3)
        isc = 1
      case (5)
        isc = 2
      case default
        isc = 3
   end select
   ymin = minval(datas%y)
   datas%y = datas%y - ymin
   loop_out: do i=1,ncounts
        count=datas%y(i)
        if(count.lt.0.0)cycle
        tthetaa=0.5*datas%x(i)
        thetaa=0.5*datas%x(i)*dtor
        s=sin(thetaa)
        i2=i+1
        ttheta=0.5*datas%x(i2)
        loop_int: do j=1,istog
             thle=s*alevers(j,isc)
             th=asin(thle)*rtod
             thwe=pweight(j,isc)*count
12           if(i2.ge.ncounts)exit loop_out
             if(0.5*datas%x(i2)-th) 15,15,16
15           i2=i2+1
             go to 12
16           i1=i2-1
             ttheta=0.5*datas%x(i2)
             st=ttheta-0.5*datas%x(i1)
             datas%y(i1)=datas%y(i1)+(ttheta-th)/st*thwe
             datas%y(i2)=datas%y(i2)+(th-0.5*datas%x(i1))/st*thwe
        enddo loop_int
     enddo loop_out
     do i=1,ncounts
        datas%y(i)=datas%y(i)+ymin
     enddo
     end subroutine alphastrip

! -----------------------------------------------------------------------------------------------------

   subroutine kalpha2_stripping() bind(C,name="kalpha2_stripping")
   use variables
   use peak_mod
   use view
   use datamod
!
   call alphastrip(dataset(1))
!
   if (dataset(1)%has_back()) then
       call dataset(1)%make_background()
   endif
!
   if (numpeaks(pkind) > 0) then
       call run_peaksearch()
   else
       call vedinew(5, rescale=0)
   endif
!
   end subroutine kalpha2_stripping

end module alphastrip_mod
