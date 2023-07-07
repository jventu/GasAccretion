module dnn_atmo
!******************************************************************************************************
!!$ Fortran implementation of the deep neural network (DNN) of Alibert and Venturini (2019)
!! Using neural-fortan
!! Written by Jonas Haldemann
!! (c) 2020-2023, Jonas Haldemann, University of Bern 
!******************************************************************************************************
use mod_kinds, only: ik, rk
use mod_network, only: network_type
IMPLICIT NONE
!******************************************************
!! Scaling parameters for the various fitted variables
!******************************************************
!******************************************************************************************************
!! Mcrit
INTEGER,PARAMETER :: n_X_mcrit = 4
REAL(8),DIMENSION(n_X_mcrit),PARAMETER :: mean_mcrit_x = [&
                2.535456772458802854e-01_8, 7.626984933910342761e+02_8, 5.927789213630855203e-01_8, 2.497384686918616126e+01_8],&
                                          std_mcrit_x  = [&
                7.117288501753405994e-01_8, 4.244577239527916390e+02_8, 1.500751113786840563e+00_8, 1.723995873058541628e+00_8]

REAL(8),PARAMETER ::              mean_mcrit_y = 1.003893721753783552e+00_8,&
                                  std_mcrit_y  = 2.652851404276058700e-01_8
!! Mcrit low kappa
INTEGER,PARAMETER :: n_X_mcrit_lowkappa = 4
REAL(8),DIMENSION(n_X_mcrit_lowkappa),PARAMETER :: mean_mcrit_lowkappa_x = [&
                2.508803905103767495e-01_8, 7.586579585296778987e+02_8, 5.918633647102968798e-01_8, 2.546255958164791977e+01_8],&
                                                   std_mcrit_lowkappa_x  = [&
                7.096136486436664947e-01_8, 4.230437618150253343e+02_8, 1.507156666582959215e+00_8, 1.997201920439825917e+00_8]
                                                           
REAL(8),PARAMETER ::              mean_mcrit_lowkappa_y = 8.913616748878813167e-01_8,&
                                  std_mcrit_lowkappa_y  = 3.597920747087929305e-01_8
!! Menve
INTEGER,PARAMETER :: n_X_menve = 5
REAL(8),DIMENSION(n_X_menve),PARAMETER :: mean_menve_x = [&
                2.340640168029828605e-01_8, 2.773281428206982202e+00_8, 5.920352901581708016e-01_8,&
                2.550603207244543924e+01_8, 8.370971210594866374e-01_8],&
                                            std_menve_x  = [&
                7.173917582486436517e-01_8, 3.603926882746055771e-01_8, 1.495208668787648465e+00_8,&
                2.011186985483957912e+00_8, 3.621669045943168297e-01_8]
REAL(8),PARAMETER ::              mean_menve_y =-6.625006224133132005e-01_8,&
                                  std_menve_y  = 1.395618681080658563e+00_8
!! Menve low kappa
INTEGER,PARAMETER :: n_X_menve_lowkappa = 5
REAL(8),DIMENSION(n_X_menve_lowkappa),PARAMETER :: mean_menve_lowkappa_x = [&
                2.370635818544996609e-01_8, 2.774131982700983823e+00_8, 5.834000212581879063e-01_8,&
                2.551171848512121443e+01_8, 6.954899742061307899e-01_8],&
                                                   std_menve_lowkappa_x  = [&
                7.192695949053735660e-01_8, 3.595057462060607389e-01_8, 1.494506244012776541e+00_8,&
                2.019872900365784485e+00_8, 3.851200157053586426e-01_8]
REAL(8),PARAMETER ::              mean_menve_lowkappa_y =-7.347137593300873126e-01_8,&
                                  std_menve_lowkappa_y  = 1.309368723754772201e+00_8
!! Menve_cut
INTEGER,PARAMETER :: n_X_menve_2 = 10
REAL(8),DIMENSION(n_X_menve_2),PARAMETER :: mean_menve_x_2 = [&
                -3.469946380312821654e-01_8,2.954454227894351526e+00_8,-1.318484554966601552e+00_8,&
                2.727431354956434006e+01_8,8.441779414301792128e-01_8,-3.469946380312821654e-01_8,&
                2.954454227894351526e+00_8,-1.318484554966601552e+00_8,2.727431354956434006e+01_8,&
                8.441779414301792128e-01_8],&
                                            std_menve_x_2  = [&
                9.414153672406021522e-01_8,3.311400064449916969e-01_8,2.851007109602055056e+00_8,&
                9.210589725053831556e-01_8,3.540329704192358151e-01_8,9.414153672406021522e-01_8,&
                3.311400064449916969e-01_8,2.851007109602055056e+00_8,9.210589725053831556e-01_8,&
                3.540329704192358151e-01_8]
REAL(8),PARAMETER ::              mean_menve_y_2 =-2.771203288623670158e+00_8,&
                                  std_menve_y_2  = 1.657681332503694982e+00_8

type(network_type) :: net_mcrit
type(network_type) :: net_mcrit_lowkappa
type(network_type) :: net_menve
type(network_type) :: net_menve_lowkappa
type(network_type) :: net_menve_cut

CHARACTER(LEN=200) :: dnn_path = '../input_tables/networks'
contains
!******************************************************************************************************
SUBROUTINE INIT(path)
	!! Load network weights into neural Fortran
    IMPLICIT NONE
    CHARACTER(LEN=200),INTENT(IN) :: path
    LOGICAL :: exist
    print*,'Load networks...'
    inquire(file=trim(path)//'/Mcrit_model.txt',exist=exist)
    if (exist) then
        print*,'Load Mcrit network from file:           ',trim(path)//'/Mcrit_model.txt'
        call net_mcrit % load(trim(path)//'/Mcrit_model.txt')
    else
        print*,'ERROR: Could not locate Mcrit network at path: ',trim(path)//'/Mcrit_model.txt'
        STOP
    endif
    inquire(file=trim(path)//'/Mcrit_lowkappa_model.txt',exist=exist)
    if (exist) then
        print*,'Load Mcrit low kappa network from file: ',trim(path)//'/Mcrit_lowkappa_model.txt'
        call net_mcrit_lowkappa % load(trim(path)//'/Mcrit_lowkappa_model.txt')
    else
        print*,'ERROR: Could not locate Mcrit low kappa network at path: ',trim(path)//'/Mcrit_lowkappa_model.txt'
        STOP
    endif
    inquire(file=trim(path)//'/Menve_model.txt',exist=exist)
    if (exist) then
        print*,'Load Menve network from file:           ',trim(path)//'/Menve_model.txt'
        call net_menve % load(trim(path)//'/Menve_model.txt')
    else
        print*,'ERROR: Could not locate Menve network at path: ',trim(path)//'/Menve_model.txt'
        STOP
    endif
    inquire(file=trim(path)//'/Menve_lowkappa_model.txt',exist=exist)
    if (exist) then
        print*,'Load Menve low kappa network from file: ',trim(path)//'/Menve_lowkappa_model.txt'
        call net_menve_lowkappa % load(trim(path)//'/Menve_lowkappa_model.txt')
    else
        print*,'ERROR: Could not locate Menve low kappa network at path: ',trim(path)//'/Menve_lowkappa_model.txt'
        STOP
    endif
    inquire(file=trim(path)//'/Menve_model_cut.txt',exist=exist)
    if (exist) then
        print*,'Load Menve network from file:           ',trim(path)//'/Menve_model_cut.txt'
        call net_menve_cut % load(trim(path)//'/Menve_model_cut.txt')
    else
        print*,'ERROR: Could not locate Menve network at path: ',trim(path)//'/Menve_model_cut.txt'
        STOP
    endif
END SUBROUTINE INIT
!******************************************************************************************************
REAL(8) FUNCTION mcrit_fit(X) RESULT(Y)
	!! Fit Critical Mass
	!! Input = [a,Tout,Pout,L] 
    IMPLICIT NONE
    REAL(8),DIMENSION(n_X_mcrit),INTENT(IN) :: X
    REAL(rk) :: X_scale(n_X_mcrit+1),Y_scale(1)
    REAL(8)  :: Y_out(1),X_transform(n_X_mcrit)

    X_transform(1)         = log10(X(1))
    X_transform(2)         = X(2)
    X_transform(3)         = log10(X(3))
    X_transform(4)         = log10(X(4))

    X_scale(1)             = 1.0_8                                               !! Pad input as in original jupyter notebook
    X_scale(2:1+n_X_mcrit) = REAL(StandardScaler(X_transform,mean_mcrit_x,std_mcrit_x),rk) !! Scale Input
    Y_scale = net_mcrit % output(X_scale)                               !! DNN_fit
    Y_out   = StandardScaler_invert(REAL(Y_scale,8),mean_mcrit_y,std_mcrit_y)   !! Rescale output
    Y       = 10._8**Y_out(1)
    RETURN
END FUNCTION mcrit_fit
!******************************************************************************************************
REAL(8) FUNCTION mcrit_lowkappa_fit(X) RESULT(Y)
	!! Fit Critical Mass (low opacity)
	!! Input = [a,Tout,Pout,L]
    IMPLICIT NONE
    REAL(8),DIMENSION(n_X_mcrit_lowkappa),INTENT(IN) :: X
    REAL(rk) :: X_scale(n_X_mcrit_lowkappa+1),Y_scale(1)
    REAL(8)  :: Y_out(1),X_transform(n_X_mcrit_lowkappa)

    X_transform(1)         = log10(X(1))
    X_transform(2)         = X(2)
    X_transform(3)         = log10(X(3))
    X_transform(4)         = log10(X(4))

    X_scale(1)             = 1.0_8                                                                 !! Pad input as in original jupyter notebook
    X_scale(2:1+n_X_mcrit_lowkappa) = REAL(StandardScaler(X_transform,mean_mcrit_lowkappa_x,std_mcrit_lowkappa_x),rk) !! Scale Input

    Y_scale = net_mcrit_lowkappa % output(X_scale)                                        !! DNN_fit
    Y_out   = StandardScaler_invert(REAL(Y_scale,8),mean_mcrit_lowkappa_y,std_mcrit_lowkappa_y)   !! Rescale output
    Y       = 10._8**Y_out(1)
    RETURN
END FUNCTION mcrit_lowkappa_fit
!******************************************************************************************************
REAL(8) FUNCTION menve_fit(X) RESULT(Y)
	!! Fit Envelope Mass
	!! Input = [a,Tout,Pout,L,Mcore]
    IMPLICIT NONE
    REAL(8),DIMENSION(n_X_menve),INTENT(IN) :: X
    REAL(rk) :: X_scale(n_X_menve+1),Y_scale(1)
    REAL(8)  :: Y_out(1),X_transform(n_X_menve)

    X_transform(1)         = log10(X(1))
    X_transform(2)         = log10(X(2))
    X_transform(3)         = log10(X(3))
    X_transform(4)         = log10(X(4))
    X_transform(5)         = log10(X(5))

    X_scale(1)             = 1.0_8                                      !! Pad input as in original jupyter notebook
    X_scale(2:1+n_X_menve) = REAL(StandardScaler(X_transform,mean_menve_x,std_menve_x),rk) !! Scale Input

    Y_scale = net_menve % output(X_scale)                               !! DNN_fit
    Y_out   = StandardScaler_invert(REAL(Y_scale,8),mean_menve_y,std_menve_y)   !! Rescale output
    Y       = 10._8**Y_out(1)
    RETURN
END FUNCTION menve_fit
!******************************************************************************************************
REAL(8) FUNCTION menve_lowkappa_fit(X) RESULT(Y)
	!! Fit Envelope Mass (low opacity)
	!! Input = [a,Tout,Pout,L,Mcore]
    IMPLICIT NONE
    REAL(8),DIMENSION(n_X_menve_lowkappa),INTENT(IN) :: X
    REAL(rk) :: X_scale(n_X_menve_lowkappa+1),Y_scale(1)
    REAL(8)  :: Y_out(1),X_transform(n_X_menve_lowkappa)
    
    X_transform(1)         = log10(X(1))
    X_transform(2)         = log10(X(2))
    X_transform(3)         = log10(X(3))
    X_transform(4)         = log10(X(4))
    X_transform(5)         = log10(X(5))
    X_scale(1)                      = 1.0_8                                                        !! Pad input as in original jupyter notebook
    X_scale(2:1+n_X_menve_lowkappa) = REAL(StandardScaler(X_transform,mean_menve_lowkappa_x,std_menve_lowkappa_x),rk) !! Scale Input

    Y_scale = net_menve_lowkappa % output(X_scale)                                        !! DNN_fit
    Y_out   = StandardScaler_invert(REAL(Y_scale,8),mean_menve_lowkappa_y,std_menve_lowkappa_y)   !! Rescale output
    Y       = 10._8**Y_out(1)
    RETURN
END FUNCTION menve_lowkappa_fit
!******************************************************************************************************
REAL(8) FUNCTION menve_cut_fit(X) RESULT(Y)
    !! Fit Envelope Mass
    !! Input = [a,Tout,Pout,L,Mcore]
    IMPLICIT NONE
    REAL(8),DIMENSION(n_X_menve),INTENT(IN) :: X
    REAL(rk) :: X_scale(n_X_menve_2),Y_scale(1)
    REAL(8)  :: Y_out(1),X_transform(n_X_menve_2)

    X_transform(1)         = log10(X(1))
    X_transform(2)         = log10(X(2))
    X_transform(3)         = log10(X(3))
    X_transform(4)         = log10(X(4))
    X_transform(5)         = log10(X(5))

    X_transform(6:10)      = X_transform(1:5)

    X_scale = REAL(StandardScaler(X_transform,mean_menve_x_2,std_menve_x_2),rk) !! Scale Input

    Y_scale = net_menve_cut % output(X_scale)                               !! DNN_fit
    Y_out   = StandardScaler_invert(REAL(Y_scale,8),mean_menve_y_2,std_menve_y_2)   !! Rescale output
    Y       = 10._8**Y_out(1)
    
    RETURN
END FUNCTION menve_cut_fit
!******************************************************************************************************
elemental function StandardScaler(x,u,s)
    !! z = (x - u) / s
    IMPLICIT NONE
    REAL(8),INTENT(IN) :: x  !! data
    REAL(8),INTENT(IN) :: u  !! mean
    REAL(8),INTENT(IN) :: s  !! standard deviation
    REAL(8) :: StandardScaler

    StandardScaler = (x - u) / s

    return
end function StandardScaler
!******************************************************************************************************
elemental function StandardScaler_invert(z,u,s)
    !! x = (z * s) + u
    REAL(8),INTENT(IN) :: z  !! scaled
    REAL(8),INTENT(IN) :: u  !! mean
    REAL(8),INTENT(IN) :: s  !! standard deviation
    REAL(8) :: StandardScaler_invert

    StandardScaler_invert = z*s + u

    return
end function StandardScaler_invert
!******************************************************************************************************
end module dnn_atmo
