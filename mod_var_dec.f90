module mod_var_dec
  implicit none
  public
  integer, parameter :: n = 100000
  real, dimension(3,n) :: xyz,vel,acc,tempacc
  real, dimension(3), parameter :: boxlen = [50.0, 50.0, 50.0]
  real, parameter :: mnscl = 1.0e-3
  real, parameter :: eps = 1.0
  real, parameter :: sigma = 1.0
  real, parameter :: Kb = 1.0 !1.3806488e-23
  real, parameter :: Pi = 4.0*atan(1.0)
  real, parameter :: mindist = 1.0
  real, parameter :: rad =  mindist/2.0
  real, parameter :: m=1.0
  real, parameter :: tot_t = 1.0
  real, parameter :: dt = 0.01
  real, dimension(3), parameter :: cutdist = boxlen/2.0
  real :: ke =100.0
  real :: pe,te
end module mod_var_dec
