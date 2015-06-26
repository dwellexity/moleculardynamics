module mod_vel_init
  use mod_var_dec, only : vel,ke,m,n,Pi
  implicit none
  private
  real :: temp,x,y
  integer :: i
  integer :: t,seedsize
  integer, dimension(:), allocatable :: seed
  public :: boltz_vel,rand_vel
contains
  subroutine boltz_vel()
    call random_seed(size=seedsize)
    allocate(seed(seedsize))
    do i=1,seedsize
      call system_clock(t)
      seed(i)=t
    enddo
    call random_seed(put=seed)
    do i=1, n
      call random_number(x)
      call random_number(y)
      vel(1,i)=sqrt(-2*log(x)) *cos(2*Pi*y)
      call random_number(x)
      call random_number(y)
      vel(2,i) =sqrt(-2*log(x)) *cos(2*Pi*y)
      call random_number(x)
      call random_number(y)
      vel(3,i) = sqrt(-2*log(x)) *cos(2*Pi*y)
    enddo
    vel(1,:)=vel(1,:)-(sum(vel(1,:))/n)
    temp=sqrt((3.0*m*sum((vel(1,:))**2.0))/(2.0*ke))
    vel(1,:)=vel(1,:)/temp
    vel(2,:)=vel(2,:)-(sum(vel(2,:))/n)
    temp=sqrt((3.0*m*sum((vel(2,:))**2.0))/(2.0*ke))
    vel(2,:)=vel(2,:)/temp
    vel(3,:)=vel(3,:)-(sum(vel(3,:))/n)
    temp=sqrt((3.0*m*sum((vel(3,:))**2.0))/(2.0*ke))
    vel(3,:)=vel(3,:)/temp
  end subroutine boltz_vel
  subroutine rand_vel()
    call random_seed(size=seedsize)
    allocate(seed(seedsize))
    do i=1,seedsize
      call system_clock(t)
      seed(i)=t
    enddo
    call random_seed(put=seed)
    do i=1, n
      call random_number(x)
      vel(1,i) = x
      call random_number(x)
      vel(2,i) = x
      call random_number(x)
      vel(3,i) = x
    enddo
    vel(1,:)=vel(1,:)-(sum(vel(1,:))/n)
    temp=sqrt((3.0*m*sum((vel(1,:))**2.0))/(2.0*ke))
    vel(1,:)=vel(1,:)/temp
    vel(2,:)=vel(2,:)-(sum(vel(2,:))/n)
    temp=sqrt((3.0*m*sum((vel(2,:))**2.0))/(2.0*ke))
    vel(2,:)=vel(2,:)/temp
    vel(3,:)=vel(3,:)-(sum(vel(3,:))/n)
    temp=sqrt((3.0*m*sum((vel(3,:))**2.0))/(2.0*ke))
    vel(3,:)=vel(3,:)/temp
  end subroutine rand_vel
end module mod_vel_init
