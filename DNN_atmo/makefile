#compiler
FC= gfortran

FCOPTIONS= -ffixed-line-length-none -fcheck=all -fno-automatic -Wall -pedantic
PFLAGS       = -O3 -g
MODULESOURCE = ..
MODULEPATH = $(MODULESOURCE)/DNN_Atmo
MODDIR     = $(MODULEPATH)/Modules
OUTDIR     = $(MODDIR)
LIBDIR     = -L../FKB/build/lib
INC        = -I$(MODDIR) -I../FKB/build/include
LIB        = -lneural

dnn_path = $(MODULESOURCE)/DNN_Atmo
dnn_src  = $(dnn_path)/dnn_atmo.f90
dnn_ob   = $(MODDIR)/dnn_atmo.o

.PHONY : clean

default: $(MODDIR) $(dnn_ob)

$(dnn_ob): dnn_atmo.f90 $(OBJ)
	$(FC) $(INC) -J$(MODDIR) $(LIBDIR) $(FCOPTIONS) $(PFLAGS) -c $<  $(LIB) -o $@ 

test: test.f90 $(dnn_ob)
	$(FC) $(INC) $(LIBDIR) $(FCOPTIONS) $(PFLAGS) -o $@ $^ $(OBJ) $(LIB)

$(MODDIR):
	mkdir $(MODDIR)

clean :
	rm -rf $(MODDIR)
