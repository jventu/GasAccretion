gfortran -ffixed-line-length-none -O3 -c -w -g3 -g -Wall units_sbr.f
gfortran -ffixed-line-length-none -O3 -c -w -g3 -g -Wall CORE.f90
gfortran -ffixed-line-length-none -O3 -c -w -g3 -g -Wall boundary_conditions.f90
gfortran -ffixed-line-length-none -O3 -c -w -g3 -g -Wall Gas_Acc_Rate_from_Disk.f90
gfortran -ffixed-line-length-none -O3 -c -w -g3 -g -Wall Timestep.f90
gfortran -ffixed-line-length-none -O3 -c -w -g3 -g -Wall -Wsurprising -pedantic -fbounds-check -Wconversion -Wimplicit-interface -fimplicit-none  -I../FKB/build/include -I../DNN_atmo/Modules -L../FKB/build/lib main_Formation.f90
gfortran  -O3 -finit-real=nan  -I../FKB/build/include -I../DNN_atmo/Modules -L../FKB/build/lib -o GasAcc_exe  units_sbr.o CORE.f90 boundary_conditions.o Gas_Acc_Rate_from_Disk.o  Timestep.o main_Formation.o ../DNN_atmo/Modules/dnn_atmo.o -lneural
