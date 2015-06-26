gfortran -Wall -std=f2008 -O3 mod_exec_time.f90 mod_var_dec.f90 mod_pos_init.f90 mod_vel_init.f90 mod_energy.f90 mod_acc_init.f90 mod_mol_dyn.f90 moldyn.f90

pause
a.exe

pause
exit
rem gfortran -g -fbounds-check -Wall -fbacktrace -finit-real=nan -std=f2008 -O3

