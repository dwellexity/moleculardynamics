module mod_exec_time
  implicit none
  private
  integer(kind=4) :: cnt,cntmx,cntrt      !make cntrt of type real(kind=8) in 5.1
  integer(kind=4), dimension(100) :: icnt
  real(kind=4) :: tot_exec_time
  public :: tot_exec_time, exec_time
contains
  subroutine exec_time(i,indctr,prnt)
    integer :: i
    character(len=1) :: indctr
    character(len=1), optional :: prnt
    if (indctr=="s") then
      call system_clock(cnt,cntrt,cntmx)
      icnt(i)=cnt
    elseif(indctr=="e") then
      call system_clock(cnt,cntrt,cntmx)
      tot_exec_time=(cnt-icnt(i))/float(cntrt)
      if(present(prnt)) then
        if(prnt=="y") print*, "Execution time is ", tot_exec_time, " seconds"
      endif
    else
      print*, "Wrong value passed to execution timer"
      stop
    endif
  end subroutine exec_time
end module mod_exec_time
