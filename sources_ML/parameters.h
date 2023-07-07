            implicit none
!----------------------------------------
! semimajor axis, intial core mass, initial guess of Menv, core density, stellar mass
!----------------------------------------
        real*8 a_p
        parameter(a_p = 5.) !semimajor axis in AU

        real*8 Mcore_p
        parameter(Mcore_p = 1.) !guess of initial core mass (ME)

        real*8 Menv_p
        parameter(Menv_p = 1.e-4) !guess of initial envelope mass (ME), suited for Mcore=1 ME, high kappa

        real*8 dens_core_p !uniform density of the core (fixes the radius of the core for a given Mcore)
        parameter (dens_core_p = 5.)

        real*8 Mstar_p       !stellar mass in Msun (should not be changed, DNN trained for Sun-mass stars)
        parameter (Mstar_p = 1.)

!----------------------------------------
! constant accretion rate of solids:
!----------------------------------------
        real*8 cte_Mzdot 
        parameter (cte_Mzdot = 1.e-6) !in Mearth/yr

!----------------------------------------
! disk parameters for boundary conditions
!----------------------------------------
        real*8 T0
        parameter (T0 = 340.)

        real*8 Sigma0
        parameter (Sigma0 = 1700.) !1700 is for MMSN !to get 15 at 5.2 AU: factor 5.81 (to reproduce Fortier 2009)

        real*8 p_disk ! not used
        parameter (p_disk = 1.5)

!------------------------------------------
! opacity
!------------------------------------------
      real*8 f_opacity !factor to reduce dust opacity of BL94
      parameter (f_opacity= 1.)







