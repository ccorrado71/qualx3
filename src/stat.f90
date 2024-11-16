 MODULE STAT
 USE type_constants, only: DP

 implicit none

 integer, parameter, private :: real_kind = DP

 type stat_type
   private
   integer         :: n
   real(real_kind) :: ex,ex2
   real(real_kind) :: k

 contains

   procedure, public :: init => init_stat
   procedure, public :: add => add_variable
   procedure, public :: mean => get_meanvalue
   procedure, public :: variance => get_variance
   procedure, public :: num => get_number

 end type stat_type

 CONTAINS


   subroutine init_stat(stat)
   class(stat_type), intent(inout) :: stat
   stat%n = 0
   stat%ex = 0
   stat%ex2 = 0
   end subroutine init_stat

  !--------------------------------------------------------------------

   subroutine add_variable(stat,x)
   class(stat_type), intent(inout) :: stat
   real(real_kind), intent(in)                :: x
   real(real_kind)                            :: diff
   if (stat%n == 0) stat%k = x
   stat%n = stat%n + 1
   diff = x-stat%k
   stat%ex = stat%ex + diff
   stat%ex2 = stat%ex2 + diff*diff
   end subroutine add_variable

  !--------------------------------------------------------------------

   real(real_kind) function get_meanvalue(stat)
   class(stat_type), intent(in) :: stat
   get_meanvalue = stat%k + stat%ex/stat%n 
   end function get_meanvalue

  !--------------------------------------------------------------------

   real(real_kind) function get_variance(stat)
   class(stat_type), intent(in) :: stat
   get_variance = (stat%ex2 - (stat%ex*stat%ex)/stat%n) / (stat%n - 1)
   end function get_variance

  !--------------------------------------------------------------------

   integer function get_number(stat)
   class(stat_type), intent(in) :: stat
   get_number = stat%n
   end function get_number

  !--------------------------------------------------------------------

   subroutine avevar(data,ave,var)
!
!  Calcola media e varianza
!
   real, dimension(:), intent(in) :: data
   real, intent(out)              :: ave,var
   integer                        :: n
   real, dimension(size(data))    :: s
!
   n=size(data)
   ave=sum(data(:))/n
   s(:)=data(:)-ave
   var=dot_product(s,s)
   var=(var-sum(s)**2/n)/(n-1)
!
   end subroutine avevar

  !--------------------------------------------------------------------

   subroutine avestd(data,ans,avef,nd)
!
!  Calcola la media eliminando i dati con |data(i) - ave| > ans*std
!
   real, dimension(:), intent(in) :: data
   real, intent(in)               :: ans
   real, intent(out)              :: avef
   integer, intent(out), optional :: nd
   real                           :: avei,var,stdn
   integer                        :: i,ni,nf
!
   call avevar(data,avei,var)
   stdn = ans*sqrt(var)
!
   ni = size(data)
   nf = 0
   avef = 0
   do i=1,ni
      if(abs(data(i) - avei) <= stdn) then
         avef = avef + data(i)
         nf = nf + 1
      endif
   enddo
   if (nf == 0) then  ! nf=0 se i dati sono tutti uguali e var = 0
       avef = avei
   else
       avef = avef / nf
   endif
   if (present(nd)) then
       nd = ni - nf
   endif
!
   end subroutine avestd

  !------------------------------------------------------------------------------------------

   real function Rvalue(fden,fval)
   real, dimension(:), intent(in) :: fden  ! grandezza a denominatore
   real, dimension(:), intent(in) :: fval
   real                           :: scaleR
   real                           :: sumFo
!
   sumFo = sum(fden)
   scaleR = sumFo / sum(fval)
   Rvalue = (sum(abs(fden-scaleR*fval)) / sumFo)*100.
!
   end function Rvalue

 END MODULE STAT
