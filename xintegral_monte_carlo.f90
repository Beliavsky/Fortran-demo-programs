program main
! 05/27/2021 08:20 AM estimate 1-dimension integrals using Monte Carlo simulation
implicit none
integer          , parameter     :: n = 10000001, dp = kind(1.0d0)
real(kind=dp)    , parameter     :: x1 = 2.0_dp, x2 = 5.0_dp, x12 = x2 - x1
character (len=*), parameter     :: fmt_cr = "(a20,':',f18.12)"
real(kind=dp)                    :: f_est,f_true
real(kind=dp)    , allocatable   :: x(:),y(:)
integer                          :: i,ifunc
character (len=10)               :: fname
allocate (x(n),y(n))
call random_seed()
call random_number(x)
x = x1 + x12*x
write (*,fmt_cr) "x1",x1
write (*,fmt_cr) "x2",x2
write (*,"(a20,':',i18)") "n",n
do ifunc=1,4
   select case (ifunc)
      case (1)
         fname  = "exp(x)"
         y      = exp(x)
         f_true = exp(x2) - exp(x1)
      case (2)
         fname  = "1/x"
         y      = 1/x
         f_true = log(x2) - log(x1)
      case (3)
         fname  = "x^2"
         y      = x**2
         f_true = (x2**3 - x1**3)/3
      case (4)
         fname  = "cos(x)"
         y      = cos(x)
         f_true = sin(x2) - sin(x1)
   end select
   f_est = sum(y)*x12/n
   write (*,"(/,'f(x) = ',a)") trim(fname)
   write (*,fmt_cr) "estimated",f_est
   write (*,fmt_cr) "true",f_true
   write (*,fmt_cr) "error",f_est - f_true
end do
end program main
