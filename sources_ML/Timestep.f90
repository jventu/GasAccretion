subroutine timestep(Menv,Mcore,tauZ,tau_env,dt_ref,dt)

include 'parameters.h'
include 'common2.h'


real*8, intent(IN) ::  tauZ, tau_env !in yr
real*8, intent(IN)::   Menv, Mcore
real*8, intent(IN) ::  dt_ref
real*8, intent(OUT)::  dt   !in seconds

real*8, parameter :: dt_MIN = 100.  
real*8 :: dt1, dt2, dt3



dt1 = 1.e-2* tauZ
dt2 = 1.e-2 * tau_env
dt =  min(dt1,dt2)

if(dt/an .le. dt_MIN) dt = dt_MIN *an ! I set a minimum possible timestep
If(dt_ref.gt.0. .and. dt/dt_ref .gt. 100) dt = 100 * dt_ref ! I don't want dt to increase too much with respect to previous dt

write(*,*) 
write(*,*) 'in Timestep:',  Menv/Mearth, tauZ/an, tau_env/an, dt/an, dt_ref/an
if(dt.lt.0) stop 'dt < 0 inside timestep sbr'

end subroutine

