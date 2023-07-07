program Planetary_Growth
! ---------------------------------------------------
! Toy model of gas accretion 
! Copyright: J. Venturini 2023, University of Geneva
! ---------------------------------------------------

  use dnn_atmo, only : menve_fit, menve_cut_fit, mcrit_fit, mcrit_lowkappa_fit, init, dnn_path
  include 'parameters.h'
  include 'common2.h'

! Variables -----------------------------------
  integer :: indice_formacion, itime_Miso
  real*8 :: aplaneta, Tneb_aplaneta, Pneb_aplaneta, &
       aspect_ratio_aplaneta, densi_sup_gas_aplaneta, &
       rplanetesimal, time_yr

integer ::                  itime, istep
real*8 ::                   Pout, Tout
real*8 ::                   Ltot, Lcore, L_entropy, Lpost,Ltot_Ref
real*8 ::                   Menv, Menv_ref, Mcore_ref, Menv_ini, Menv_fin, Rcore, a_planet, a_10
real*8 ::                   R_Hill, Rtot,R_bondi,cs2,nconvec
real*8 ::                   Mstar, Sigma_solids, Mz_ref
real*8 ::                   Mcore, Mtot, Mtot_ini, Mtot_fin, dens_core
real*8 ::                   Rmin,Rmax, dr, x, Sigma

real*8, parameter ::        dt_ini = 1.0, dt_min =100., Myr_an=1.e6    !in yrs
integer, parameter ::       switch_ikoma = 0


!--------------------------------------------------------------------------
! ENVELOPE COMPOSITION
! ---------------------
real*8 :: mmw, MHHe, MHHe_ref, MdotHHe,  Mtot_ref
!---------------------------------------------------------------------------
! EVOLUTION
!-----------
integer, parameter ::       ntime = 1e7, iterZ_MAX = 100, imax_ini = 25, jmax_ini = 49
real*8, parameter  ::       Mcdot = 1.e-6 !Mearth/yr
real*8 ::                   Mzdot

real*8 :: time, dt, Myr
real*8, parameter :: kappa_dust_ikoma = f_opacity !defined in parameters.h
real*8 :: tau_env, tau_env_ref, Kenv,tauZ, Ldistr, Evap
real*8 :: dt_ref
real*8 :: time_prev
real*8 aspect_ratio

real*8 ::   Mdisk, t_cross, M_cross, Mdisk_gas, sigma0_solids, fice
character*100 :: name_output
character*1 :: second

real*8 :: tau_gas, kappa_dust, tau_gas_ikoma, MdotHHe_ikoma
real*8 :: Mcore_ini
logical, parameter :: debug = .true.
character(len=100),parameter :: input_dir = '../input_tables'

! Machine learning
real*8, dimension(5)  :: X_dnn
real*8, dimension(4)  :: Xcrit
real*8 :: Mcrit
!-------------------------------------------------------------------------------

!Name of output file
name_output = 'output_accretion.out'
open(1, file = name_output)

call UNITS

dnn_path = '../input_tables/networks'
call init(dnn_path)
time_yr = 0.
time = time_yr * an !cgs
Myr = Myr_an * an ! To get time in million-years

if(debug) write(*,*) 'out 1:', time/an, time_yr, itime, ntime
dt= dt_min * an ! yr
dt_ref= dt ! cgs

! Boundary conditions
Mzdot= cte_Mzdot * (Mearth /an) ! cgs
a_planet= a_p * au ! cgs
dens_core = dens_core_p
Rcore =(3.*Mcore_ref/(pi4*dens_core))**(1./3.)
call BoundaryConditions(Tout,Pout,aspect_ratio)
if(debug) write(*,*) a_planet/au, Tout, Pout, aspect_ratio
 
! definition of array on Mcore_ini
Mcore_ref = Mcore_p * Mearth 
Menv_ref = Menv_p * Mearth
MHHe_ref = Menv_ref 
Menv = Menv_ref

!accretion rate of solids
Mzdot= cte_Mzdot * (Mearth /an) ! cgs
Lcore = 0.
MdotHHe = 0.
tau_gas_ikoma = 0.
Mcrit = 0.
tau_env_ref = 0.

if(debug) write(*,*) 'initial time, Mcore:', time/an, Mcore_ref/Mearth
itime= istep

time = time_yr * an !cgs
if(debug) write(*,*) 'Times:', time/an, time_yr, an, time, istep, ntime
   Myr = 1.e6*an
    dt= dt_min * an ! cgs
    dt_ref = dt
   Mzdot= cte_Mzdot * (Mearth /an) ! cgs

! TIME EVOLUTION  ===================================================================
do itime = 1, ntime
  Mcore = Mcore_ref
  if(debug) write(*,*) 'start loop time:', Mcore/Mearth, dt/an, time/an

! Mcore grows by Mzdot*dt, here I get the new Mcore:
call MRcore(dt,Mzdot,Mcore_ref,Rcore,Mcore)

! Compute core luminosity:
call core_lum(Mzdot,Mcore,Rcore,Lcore)
Ltot = Lcore 

! Menv from Machine Learning (Alibert & Venturini 2019): achtung! should be used within training range, and for Mcore< Mcrit 
if (switch_ikoma == 0) then !===============================================================================
    if(debug) write(*,*) 'Gas Accretion from Ikoma+2000'   
    X_dnn = [a_planet/au, Tout, Pout, Lcore, Mcore/Mearth]
    Menv = menve_fit(X_dnn) !fit 2019
    Menv = Menv * Mearth

    ! computes the critcal mass from DNN (caveat: not very accurate).
    Xcrit = [a_planet/au, Tout, Pout, Ltot]
    Mcrit = mcrit_fit(Xcrit)
    ! ------------------------------------------------------------------------------------------
    if(debug) write(*,*) 'after, ML:', Mcore/Mearth, Menv/Mearth, a_planet/au, Tout, Pout, Lcore
    Mtot = Mcore + Menv 
    MHHe = Menv 
    MdotHHe = (MHHe-MHHe_ref)/dt 

    if(debug) write(*,*) 'main_Formation, ML:', MHHe/Mearth, MdotHHe*an/Mearth,  Mzdot*an/Mearth
end if ! switch_ikoma == 0 ===========================================================================

if (switch_ikoma == 1 ) then
  if(debug) write(*,*) 'Gas Accretion from Ikoma+2000'   
  if(debug) write(*,*) 'out 2:', Mcore/Mearth, Mzdot*an/Mearth, dt/an

! Ikoma 2000 gas accretion : pre-factor 6 suited when solid accretion is constant ----------------------------------
  tau_gas_ikoma = 6. * (10.)**8 * (Mcore/Mearth)**(-2.5) * kappa_dust_ikoma  * an ! in seconds
  MdotHHe_ikoma =  MHHe_ref / tau_gas_ikoma ! in cgs
  MdotHHe = MdotHHe_ikoma
  MHHe = MdotHHe*dt + MHHe_ref
  Menv = MHHe
  !update total mass
  Mtot = Mcore + Menv

if(debug) write(*,*) 'ikoma1:', (10.)**8 * (Mcore/Mearth)**(-2.5) * kappa_dust_ikoma, Mcore/Mearth
if(debug) write(*,*) 'ikoma2:', tau_gas_ikoma/an, MdotHHe*an/Mearth, Mcore_ref/Mearth, MHHe_ref/Mearth, MHHe/Mearth
end if ! switch_ikoma == 1

if(MdotHHe*an/Mearth > 0.1) then
    write(*,*) '...........................................................................................'
    write(*,*) 'Gas accretion rate insanely high!', MdotHHe*an/Mearth, 'should have switch to disk supply'
    write(*,*) '...........................................................................................'
    stop 'exit execution'
end if

write(*,*) '======================================================================================================'
write(*,*) '                  Inside main_Formation.f90                          '
write(*,*) '# timestep, dt[yr], time[Myr] =', itime, dt/an, time/Myr
write(*,*) 'Tout, Pout, a_planet :' , Tout, Pout, a_planet/AU
write(*,*) 'Ltot=', Ltot,'Lcore =', Lcore, 'Mcrit=', Mcrit
write(*,*) 'MdotZ, MdotHHe:' , Mzdot*an/Mearth,  MdotHHe*an/Mearth
write(*,*) 'Mcore, Menv = ', Mcore/mearth, Menv/Mearth, 'Mtot=', Mtot/Mearth
write(*,*) 'switch_ikoma=', switch_ikoma, 'kappa_dust_ikoma', kappa_dust_ikoma, 'tau_gas_ikoma', tau_gas_ikoma/an
write(*,*) '====================================================================================================='


tau_env = 0.5 * (MHHe/MdotHHe  + tau_env_ref) ! Menv/Mdotgas
tauZ = Mcore/Mzdot
! writing output ---------------------------------------------
if(mod(itime,50)==0.) then !write output growth every 50 timesteps
  WRITE(1,*) time/an, dt/an, Mcore/Mearth, Menv/Mearth, Mtot/Mearth, Mcrit, a_planet/AU, & !7 cols
  Lcore, Mzdot*an/Mearth, MdotHHe*an/Mearth, tauZ/an, tau_env/an, Rcore/Rearth,& !8-13
  Tout, Pout, aspect_ratio, switch_ikoma, itime, kappa_dust_ikoma !14-19
end if
call flush(1)

if(debug) WRITE(*,*) 'after writing output:', time/Myr, dt/an, Mcore/Mearth, Menv/Mearth, Mtot/Mearth,  Mzdot*an/Mearth
if(Mcore.ne.Mcore) stop 'NaN Mcore'
if(Menv.ne.Menv) stop 'NaN Menv'

! TIMESTEP calculation -----------------------------------
if( itime.gt.1) then !attached phase
   call timestep(Menv,Mcore,tauZ,tau_env,dt_ref,dt)
   if(dt .lt. dt_ini) dt = dt_ini
   dt_ref = dt     
end if 

! Save computed variables as reference for next timestep
Menv_ref = Menv
MHHe_ref = MHHe
Mcore_ref = Mcore
Mz_ref = Mcore 
Mtot_ref = Mtot
dt_ref = dt
Ltot_ref = Ltot
tau_env_ref = tau_env
time_prev = time

! New time
time = time + dt

if(time/Myr > 20.) then
     stop ' Time reached 20 Myr'
end if

if(debug) write(*,*) 'end loop time:', Mcore/Mearth, Menv/Mearth, dt/an, time/an, MdotHHe*an/Mearth
end do !loop in time
close(1)
end program

