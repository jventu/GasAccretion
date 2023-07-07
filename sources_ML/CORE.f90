subroutine MRcore(dt,Mcdot,Mcore_ref,Rcore,Mcore)

include 'parameters.h'
include 'common2.h'

real*8, intent(IN) ::   dt, Mcdot, Mcore_Ref
real*8,intent(OUT) ::   Mcore, Rcore
logical :: debug = .false.

Mcore = Mcdot*dt + Mcore_ref !cgs
Rcore = (3.*Mcore/(pi4*dens_core_p))**(1./3.)

if(debug) write(*,*) 'MRcore sbr:', Mcdot*an/Mearth, Rcore/Rearth, Mcore/Mearth
end subroutine


!=======================================================================================

subroutine core_lum(Mcdot,Mcore,Rcore,Lcore)

include 'parameters.h'
include 'common2.h'

real*8, intent(IN)  ::    Mcdot, Mcore, Rcore
real*8, intent(OUT) ::    Lcore
logical :: debug = .false.

Lcore = G*Mcdot*Mcore / Rcore

if(debug) write(*,*) 'core_lum sbr:', Lcore, Mcdot*an/Mearth
end subroutine
