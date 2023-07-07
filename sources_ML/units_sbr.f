
      subroutine UNITS
c
c
c Constantes et dimensions
c
       include           'parameters.h'
       include           'parameters_debug.h'
       include           'common2.h'



c  base cgs
      gramme      = 1.
      centimetre  = 1.
      seconde     = 1.
      degre       = 1.


c     -------------
      unit M = gramme
      unit L = centimetre
      unit T = seconde
c     -------------

c  cgs dans la nouvelle base
      gramme      = 1./unitM
      centimetre  = 1./unitL
      seconde     = 1./unitT
      degre       = 1.
      

      dyne        = gramme*centimetre/seconde**2
      erg         = gramme*centimetre**2/seconde**2

c  unites pratiques
      cm2     = 1.            * centimetre**2
      cm3     = 1.            * centimetre**3
      ms      = 1.       e+02 * centimetre/seconde
      kms     = 1.       e+05 * centimetre/seconde
      an      = 3.15576  e+07 * seconde
      Mans    = 3.15576  e+13 * seconde
      Gans    = 3.15576  e+16 * seconde
      Msol    = 1.9884138333164502e+33 * gramme
      Mj      = 1/1 047.5654  * Msol
      Mearth  = 5.97424  e+27 * gramme
      parsec  = 3.085678 e+18 * centimetre
      kpc     = 3.085678 e+21 * centimetre
      Mpc     = 3.085678 e+24 * centimetre
      AU      = 1.49597870691 e+13 * centimetre !149 597 871
      km      = 1.0      e+5  * centimetre
      Kelvin  = 1. * degre
      Bar     = 1.       e+6  * gramme / centimetre / seconde**2.
      Pa      = 10. * dyne/centimetre**2.

c  constantes physiques
      c       = 2.997925 e+10 * centimetre/seconde
      eV      = 1.6022   e-12 * erg
      G       = 6.6742867 e-08 *  dyne*centimetre**2/gramme**2
      hp      = 6.62620  e-27 * erg*seconde
      kb      = 1.38062  e-16 * erg/degre
      uma     = 1.660531 e-24 * gramme 
      a       = 7.57     e-15 * erg/cm3/Kelvin**4.
      Navo    = 6.022    e+23
      Rgp     = 8.315    e+07 * erg/Kelvin/gramme
      Rj      = 7.14     e+04  * km
      Rearth  = 6371.    *km
      Rsun    = 6.96     e+05  * km
      Lsun    = 3.827    e+33  * erg / seconde
      LJ      = 8.67     e-10  * Lsun
      a0      = 0.529    e-8   * centimetre
      

c  constantes physiques exprimees en unites derivees
      H0      = 50. * kms/Mpc

c autres constantes
      pi      = atan(1.)*4
      pi4     = 4.*pi
      sigmaPlanck   = a*c/4.   ! 5.67 e -5
      TeffJ     = (LJ   / (4.*pi*sigmaPlanck*Rj  **2.))**(1./4.) * Kelvin
      Teffsun   = (Lsun / (4.*pi*sigmaPlanck*Rsun**2.))**(1./4.) * Kelvin
      cfrad     = -3./(16.*pi*a*c)
      cp_silicate = 0.7e7*erg/kelvin/gramme


      return
      end
