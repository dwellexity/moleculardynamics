module mod_energy
  use mod_var_dec, only: xyz,vel,m,ke,pe,eps,n,sigma,boxlen,cutdist
  implicit none
  private
  integer :: i,j,k
  real, dimension(3) ::dist
  real :: r
  public :: kinetic,potential
contains
  subroutine kinetic()
    implicit none
    ke=0.5*m*sum(vel*vel)
  end subroutine kinetic
  subroutine potential()
    real :: temp
    pe=0.0
    do i=1,n-1
      do j=i+1,n
        dist=xyz(:,i)-xyz(:,j)
        do k=1,3
          if(dist(k)>cutdist(k)) dist(k)=dist(k)-boxlen(k)    !Minimum Image Convention
          if(dist(k)<(-cutdist(k))) dist(k)=dist(k)+boxlen(k)  !What if it is equal to cutdist?
        enddo
        r=sqrt(sum(dist**2))
        temp=(sigma/r)**6
        pe= pe + 4.0*eps*temp*(temp-1)       !r is relative distance.
      enddo
    enddo
  end subroutine potential
end module mod_energy
