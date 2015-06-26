program test
  !use mod_exec_time, only : tot_exec_time, exec_time
  implicit none

  integer :: a
  character(len=20) :: b

  !call exec_time(1)
  a=234
  b=char(a)
  print*, djdjb


  !call exec_time(2)
  !print*, tot_exec_time, "seconds"

end program test
