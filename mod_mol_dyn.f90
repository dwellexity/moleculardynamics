module mod_mol_dyn
  use mod_var_dec, only : m,n,xyz,vel,acc,eps,sigma,dt,boxlen,cutdist
  implicit none
  private
  real, dimension(3,n) :: tempacc
  real, dimension(3) :: dist
  real, parameter :: sigma6=sigma**6
  real, parameter :: sigma62=2.0*sigma6
  real, parameter :: sigmaepsm=(24.0*eps*sigma6)/m
  public :: pos_upd,vel_upd,acc_upd
contains
  subroutine pos_upd()
    implicit none
    integer :: i,k
    do i=1,n
      do k=1,3
        xyz(k,i)=xyz(k,i) + vel(k,i)*dt + (acc(k,i)*(dt**2))/2
        if(xyz(k,i)>boxlen(k)) then
          xyz(k,i)=xyz(k,i)-boxlen(k)  !PBC
        endif
        if(xyz(k,i)<0) then
          xyz(k,i)=xyz(k,i)+boxlen(k)  !PBC
        endif
      enddo
    enddo
  end subroutine pos_upd
  !Updating Acceleration
  subroutine acc_upd()
    implicit none
    integer :: i,j,k
    real :: r,r6,temp
    do i=1,n
      tempacc(:,i)=acc(:,i)
      acc(:,i)=0.0
    enddo
    do i=1,n-1
      do j=i+1,n
        dist=xyz(:,i)-xyz(:,j)
        do k=1,3
          if(dist(k)>cutdist(k)) dist(k)=dist(k)-boxlen(k)    !Minimum Image Convention
          if(dist(k)<(-cutdist(k))) dist(k)=dist(k)+boxlen(k)  !What if it is equal to cutdist?
        enddo
        r=sqrt(sum(dist**2))
        r6=r**6
        temp=(sigmaepsm/(r6*(r**2)))*((sigma62*(1.0/r6))-1.0)
        do k=1,3
          acc(k,i)=acc(k,i)+ temp*dist(k)
          acc(k,j)=acc(k,j)- temp*dist(k)
        enddo
      enddo
    enddo
  end subroutine acc_upd
  !Updating Velocity
  subroutine vel_upd()
    implicit none
    integer :: i,k
    do i=1,n
      do k=1,3
        vel(k,i)=vel(k,i) + 0.5*(acc(k,i)+tempacc(k,i))*dt
      enddo
    enddo
  end subroutine vel_upd
end module mod_mol_dyn
