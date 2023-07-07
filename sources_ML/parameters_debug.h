!
! DEBUG
!
!
      LOGICAL       writing_files
      PARAMETER     (writing_files=.false.)  ! writing all the .debug files for debugging

      LOGICAL       debug_newei
      PARAMETER     (debug_newei=.false.)   ! debugging new_planetesimal.f

      LOGICAL       WARNING
      PARAMETER     (WARNING=.true.)        ! print warnings

      LOGICAL       DEBUG_MIGRATION
      PARAMETER     (DEBUG_MIGRATION=.false.) ! debuging migration

      LOGICAL       DEBUG_MERGING
      PARAMETER     (DEBUG_MERGING=.false.)  ! debugging for the merging sbr

      LOGICAL       DEBUG_STARTING
      PARAMETER     (DEBUG_STARTING=.false.) ! debugging for the startingtime sbr

      LOGICAL       DEBUG_PASDETPS
      PARAMETER     (DEBUG_PASDETPS=.false.)  ! debugging of the timestep calculation

      LOGICAL       DEBUG
      PARAMETER     (DEBUG=.false.) ! debugging of planet

      LOGICAL       DEBUG_LOOP
      PARAMETER     (DEBUG_LOOP=.false.)  ! ?

      LOGICAL       DEBUG_EMPS
      PARAMETER     (DEBUG_EMPS=.false.)  ! debugging for c++ part

      LOGICAL       DEBUG_FILES
      PARAMETER     (DEBUG_FILES=.false.)  ! print the names of all used files

      LOGICAL       DEBUG_MASS
      PARAMETER     (DEBUG_MASS=.false.)  ! debugging for solid mass conservation

      LOGICAL       DEBUG_LCONT
      PARAMETER     (DEBUG_LCONT=.false.)  ! debugging for including Lcont

      LOGICAL       DEBUG_STR
      PARAMETER     (DEBUG_STR=.false.)  ! debugging for the MP_STRUCTURE_STAT_zone...

      LOGICAL       DEBUG_INTENV
      PARAMETER     (DEBUG_INTENV=.false.)  ! debugging for the INTERNAL_STRUCTURE

      LOGICAL       DEBUG_DER
      PARAMETER     (DEBUG_DER=.false.)  ! debugging for the DERIVEE sbr

      LOGICAL       DEBUG_ENTRO
      PARAMETER     (DEBUG_ENTRO=.false.)  ! debugging for calculating the entropy and contraction luminosity

      LOGICAL       verb
      PARAMETER     (verb=.false.)  ! for capture radius

      LOGICAL       err
      PARAMETER     (err=.false.)  ! for capcutre radius

      LOGICAL       affichage_nbody
      PARAMETER     (affichage_nbody=.false.)  ! print informations from the nbody

      character*3 affichage,affichage_sbr,affichage_disque
      PARAMETER (affichage='oui')   ! set on oui when you want to know where the code is
      PARAMETER (affichage_disque='non')  ! print a message for every supstep of the gas disk
      PARAMETER (affichage_sbr='non') ! print a message at the beginning of a lot of subroutines

      LOGICAL       DEBUGZONE
      PARAMETER     (DEBUGZONE=.false.)  ! debugging for the zones of planets

      LOGICAL       DEBUGZONE2
      PARAMETER     (DEBUGZONE2=.false.)  ! debugging for the zones of planets

      LOGICAL       DEBUGZONE3
      PARAMETER     (DEBUGZONE3=.false.)  ! debugging for the zones of planets

      LOGICAL       DEBUG_MAX
      PARAMETER     (DEBUG_MAX=.false.)  ! debugging for the structure in detached case

      LOGICAL       DEBUG_REMOVE
      PARAMETER     (DEBUG_REMOVE=.false.) ! debugging for removing the core mass

      LOGICAL       DEBUG_MISO
      PARAMETER     (DEBUG_MISO=.false.) ! debugging for Miso

      LOGICAL       DEBUG_REZONNING
      PARAMETER     (DEBUG_REZONNING=.false.) ! debugging for the rezonning

      LOGICAL       DEBUG_DRIFT
      PARAMETER     (DEBUG_DRIFT=.false.) ! debugging for the rezonning

      LOGICAL       DEBUG_OPAC
      PARAMETER     (DEBUG_OPAC=.false.) ! debugging for the new opacity

      LOGICAL       DEBUG_JULIA
      PARAMETER     (DEBUG_JULIA=.false.) ! debugging for the JULIA files 










