subroutine BoundaryConditions(Tout,Pout,aspect_ratio)
! BC depend on a_p, which is set in parameters.h

include 'parameters.h'
include 'common2.h'

!real*8, intent(IN) :: a_planet !only affects gas density, which affects Pout
real*8, intent(OUT) :: Tout, Pout,aspect_ratio
real*8 :: h_0, P0, q
logical :: debug = .false.
real*8 :: Mstar

Mstar = Mstar_p * Msol
! a_p, T0, p_disk, tau_dis are set in 'parameters.h'
Tout = T0 * a_p**(-0.5)
h_0 = sqrt(kb / (2.* uma * G * Mstar)) * sqrt(AU) * sqrt(T0)
aspect_ratio = h_0 * a_p**0.25 !comes from temperature profile (assumes T propto a^-1/2)

! pressure comes from gas density, which comes from rho_gas = Sigma_gas/H
P0 = sqrt((kb * G * Mstar) / (2.* uma * AU**3.)) * sqrt(T0) *  Sigma0 
q = p_disk +(7./4.)
Pout = P0 * a_p**(-q)

if(debug) then
    write(*,*) 'BoundaryConditions sbr:', a_p, Tout, Pout, aspect_ratio, T0, h_0, P0, q
end if

end subroutine BoundaryConditions
