subroutine GasAccRate_Limited_by_Disk(Mp,a_planet,Sigma_gas,Mdotgas)
! gas accretion onto protoplanet given by Lissauer et al 2009, Eq.2

include 'parameters.h'
include 'common2.h'


real*8, intent(IN) :: Mp,a_planet,Sigma_gas
real*8, intent(OUT) :: Mdotgas
real*8, parameter :: c0 = -18.67
real*8, parameter :: c1 = -8.97
real*8, parameter :: c2 = -1.23

real*8, parameter :: b = 1./0.15
real*8 :: period, norm_cte, f_norm_Jupiter
logical, parameter :: debug = .true.


period = 2.*pi / dsqrt(G*Msol/a_planet**3.)  !orbital period
norm_cte = Sigma_gas * a_planet**2. /period

Mdotgas = c0 + c1*(log10(Mp/Msol)) + c2*(log10(Mp/Msol))**2.
Mdotgas  = 10.**Mdotgas
Mdotgas =  0.1 * norm_cte * Mdotgas !0.1 *
f_norm_Jupiter = -b * (Mp/Mj) + b ! function to taper off gas accretion at Jupiter mass, as in L09
if(f_norm_Jupiter.lt.0) f_norm_Jupiter = 0.

Mdotgas = f_norm_Jupiter * Mdotgas !L09 reduced by 10

do while (Mdotgas*an/Mearth .gt. 0.01 )
Mdotgas = 0.99 * Mdotgas
end do

if(debug) write(*,*) 'in GasAccRate_Limited_by_Disk:', Mp/Mearth,a_planet/AU,Sigma_gas,period/an, Mdotgas*an/Mearth

if(Mp .gt. Mj) then
write(*,*)'Mass of Jupiter Reached (for larger masses, Mdotgas becomes negative)'
end if



end subroutine
