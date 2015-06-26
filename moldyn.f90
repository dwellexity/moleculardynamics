program mol_dyn
  use mod_exec_time, only : exec_time,tot_exec_time
  use mod_pos_init, only : rand_pos,lattcmpct_pos,lattfill_pos
  use mod_vel_init, only : boltz_vel,rand_vel
  use mod_acc_init, only : LJaccinit
  use mod_var_dec, only : vel,m,n,rad,boxlen,ke,pe,tot_t,dt,Kb
  use mod_energy, only : kinetic,potential
  use mod_mol_dyn, only : pos_upd,vel_upd,acc_upd
  implicit none
  integer :: i,t,iter
  character(len=1),dimension(3) :: choice
  real :: enertime,kelv
  character(len=100) :: cmd
  character(len=15) :: temp
  open(100,file="xyz.dat",status="replace")
  open(200,file="vel.dat",status="replace")
  open(300,file="acc.dat",status="replace")
  open(400,file="energy.dat",status="replace")
  open(500,file="kelvin.dat",status="replace")
  open(600,file="boxparams.dat",status="replace")
  open(700,file="distparams.dat",status="replace")
  write(*,*) "How would you like to arrange atoms initially?"
  write(*,*) "(a) Compact Simple Cubic"
  write(*,*) "(b) Filled Simple Cubic"
  write(*,*) "(c) Randomly Arranged"
  do
    read(*,*) choice(1)
    if(choice(1)=='a') then
      call lattcmpct_pos()
      exit
    elseif(choice(1)=='b') then
      call lattfill_pos()
      exit
    elseif(choice(1)=='c') then
      call rand_pos()
      exit
    else
      print*, "invalid Choice. Please Enter again."
    endif
  enddo
  write(*,*) "How would you like to assign atoms' initial velocity?"
  write(*,*) "(a) Random Velocity"
  write(*,*) "(b) Maxwell-Boltzmann Velocity"
  do
    read(*,*) choice(2)
    if(choice(2)=='a') then
      call rand_vel()
      exit
    elseif(choice(2)=='b') then
      call boltz_vel()
      exit
    else
      print*, "invalid Choice. Please Enter again."
    endif
  enddo
  write(*,*) "Which Potential Would you want to use?"
  write(*,*) "(a) Lennard-Jones Potential"
  do
    read(*,*) choice(2)
    if(choice(2)=='a') then
      call LJaccinit()
      exit
    else
      print*, "Invalid Choice. Please Enter again."
    endif
  enddo
  print*, "The Simulation is running."
  iter=int(tot_t/dt)
  call exec_time(1,"s")
  do t=1,iter
    if(t==1) then
      call exec_time(3,"s")
      call kinetic()
      call potential()
      kelv=(2*ke)/(3*n*Kb)
      write(400,*) t,ke,pe,ke+pe
      write(500,*) t,kelv
      call exec_time(3,"e")
      enertime=tot_exec_time
      call exec_time(2,"s")
    endif
    if(mod(t,100)==0) then
      call kinetic()
      call potential()
      kelv=(2*ke)/(3*n*Kb)
      write(400,*) t,ke,pe,ke+pe
      write(500,*) t,kelv
    endif
    !     do i=1,n
    !        write(100,*) xyz(:,i)
    !     enddo
    call pos_upd()
    call acc_upd()
    call vel_upd()
    if(t==1) then
      call exec_time(2,"e")
      write(*,"(A,I0,A)") "The simulation will take approximately ", int((tot_exec_time*iter)+((iter/100)*enertime)) ," seconds."
      write(temp,"(I0)") int((tot_exec_time*iter)+((iter/100)*enertime))
      cmd="start /b timeout /t " // temp // "/nobreak"
      call execute_command_line(cmd)
    endif
  enddo
  call exec_time(1,"e","y")
  do i=1,n
    write(200,*) vel(:,i),sqrt(sum((vel(:,i))**2))
  enddo
  call kinetic()
  print*, "The Kinetic Energy is ", ke
  call potential()
  print*, "The Potential Energy is ", pe
  write(600,"(I0,/,4(F10.5,/))") n,boxlen,rad
  write(700,*) sqrt((2*ke)/(3*m*n)),0.0
  do i=1,3
    write(700,*) minval(vel(i,:)),maxval(vel(i,:))
  enddo
  write(700,*) n,0.0
  call execute_command_line("start /b python plot.py")
end program mol_dyn
