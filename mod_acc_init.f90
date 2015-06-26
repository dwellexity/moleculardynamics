module mod_acc_init
  use mod_var_dec, only: xyz,acc,eps,sigma,m,n,cutdist,boxlen
  implicit none

  private
  integer :: i,j,k
  real :: r
  real, dimension(3) :: dist

  public :: LJaccinit

contains
  subroutine LJaccinit()
    do i=1,n
       acc(:,i)=0.0
       do j=1,n
          if (j==i) cycle
          dist=xyz(:,i)-xyz(:,j)
          do k=1,3
             if(dist(k)>cutdist(k)) dist(k)=dist(k)-boxlen(k)    !Minimum Image Convention
             if(dist(k)<(-cutdist(k))) dist(k)=dist(k)+boxlen(k)  !What if it is equal to cutdist?
          enddo
          r=sqrt(sum(dist**2.0))
          do k=1,3
             acc(k,i)=acc(k,i)+ ((24.0*eps*(sigma**6.0))/(m*(r**8.0)))*((2.0*(sigma/r)**6.0)-1.0)*dist(k)
          enddo
       enddo
    enddo
  end subroutine LJaccinit


end module mod_acc_init
