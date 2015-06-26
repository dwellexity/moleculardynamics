module mod_pos_init
  use mod_var_dec, only : xyz,boxlen,n,rad,mindist,cutdist,Pi
  implicit none

  private
  real :: temp
  integer, dimension(3) :: temp1
  real, dimension(3) :: temp2,temp3,dist
  logical :: check
  integer :: i,j,k,t,cnt,seedsize
  integer, dimension(:), allocatable :: seed
  public :: rand_pos,lattcmpct_pos, lattfill_pos

contains
  subroutine rand_pos()
    implicit none
    temp=(n*(4.0*Pi*(mindist/2.0)**3.0))/3.0
    if(4*temp>product(boxlen)) then  !using 25% efficiency (random packing has highest efficiney around 64%)
       print*, "Too many atoms."     !after filling around 25%, further processing becomes very slow
       print*, "Consider using lattice ordering for initial position."
       stop
    endif

    call random_seed(size=seedsize)
    allocate(seed(seedsize))
    do i=1,seedsize
       call system_clock(t)
       seed(i)=t
    enddo
    call random_seed(put=seed)

    do i=1,3
       check = .true.
       do while (check .eqv. .true.)
          call random_number(temp)
          temp = boxlen(i)*temp
          if(temp>(boxlen(i)-rad)) then
             cycle
          elseif (temp<rad) then
             cycle
          else
             xyz(i,1) = temp
             check = .false.
          endif
       enddo
    enddo
    j=2
    do while (j<=n)
       do i=1,3
          check = .true.
          do while (check .eqv. .true.)
             call random_number(temp)
             temp = boxlen(i)*temp
             if(temp>(boxlen(i)-rad)) then
                cycle
             elseif (temp<rad) then
                cycle
             else
                xyz(i,j) = temp
                check = .false.
             endif
          enddo
       enddo
       do k=1,j-1
          dist=xyz(:,j)-xyz(:,k)
          do i=1,3
          if(dist(i)>cutdist(i)) dist(i)=dist(i)-boxlen(i)    !Minimum Image Convention
          if(dist(i)<(-cutdist(i))) dist(i)=dist(i)+boxlen(i)  !What if it is equal to cutdist?
          enddo
          if(sqrt(sum(dist**2.0))<mindist) exit
       enddo
       if(k==j) j=j+1
    enddo
  end subroutine rand_pos

  subroutine lattcmpct_pos()
    implicit none
    if(product(floor(boxlen/mindist))<n) then
       print*, "Too many atoms."
       stop
    endif
    

    temp1 = floor(((((n*(mindist**3))/product(boxlen))**(1.0/3.0))*boxlen)/mindist)
    
    do while(product(temp1)<n)
       do i=1,3
          if((float(temp1(i))/float(floor(boxlen(i)/mindist)))==minval(float(temp1)/float(floor(boxlen/mindist)))) then
             if(temp1(i)<floor(boxlen(i)/mindist)) then     !Is this redundant?
                temp1(i)=temp1(i)+1
                exit
             endif
          endif
       enddo
    enddo
    
    temp2=((boxlen-(temp1*mindist))/2.0)-rad
    cnt=1
    i=1
    outer: do
       do j=1,temp1(2)
          do k=1,temp1(3)
             xyz(1,cnt) = (i*mindist)+temp2(1)
             xyz(2,cnt) = (j*mindist)+temp2(2)
             xyz(3,cnt) = (k*mindist)+temp2(3)
             cnt = cnt+1
             if(cnt>n) exit outer
          enddo
       enddo
       i=i+1
    enddo outer
    
    if(i<temp1(1)) xyz(1,:)=xyz(1,:) + (mindist/2.0)
  end subroutine lattcmpct_pos

  subroutine lattfill_pos()
    implicit none
    if(product(floor(boxlen/mindist))<n) then
       print*, "Too many atoms."
       stop
    endif

    temp1 = floor(((((n*(mindist**3))/product(boxlen))**(1.0/3.0))*boxlen)/mindist)

    do while(product(temp1)<n)
       do i=1,3
          if((float(temp1(i))/float(floor(boxlen(i)/mindist)))==minval(float(temp1)/float(floor(boxlen/mindist)))) then
             if(temp1(i)<floor(boxlen(i)/mindist)) then     !Is this redundant?
                temp1(i)=temp1(i)+1
                exit
             endif
          endif
       enddo
    enddo

    temp3=(boxlen-(temp1*mindist))/temp1
    temp2= -(rad+(temp3/2.0))
    cnt=1
    i=1
    outer: do
       do j=1,temp1(2)
          do k=1,temp1(3)
             xyz(1,cnt) = (i*(mindist+temp3(1)))+temp2(1)
             xyz(2,cnt) = (j*(mindist+temp3(2)))+temp2(2)
             xyz(3,cnt) = (k*(mindist+temp3(3)))+temp2(3)
             cnt = cnt+1
             if(cnt>n) exit outer
          enddo
       enddo
       i=i+1
    enddo outer
    if(i<temp1(1)) xyz(1,:)=xyz(1,:) + ((mindist+temp3(1))/2.0)
  end subroutine lattfill_pos

end module mod_pos_init
