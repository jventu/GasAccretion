echo "-> Build the Fortran Keras Bridge and the Neural Fortran Library"
cd FKB
sh build_steps.sh
cd ..
echo " "
echo "-> Build DNN_atmo Module"
cd DNN_atmo
make clean
make 
cd ..
echo " "
echo "Build Gas Accretion Executable"
cd sources_ML
sh compile_GasAcc.sh
cd ..
echo " "
echo "done!"
