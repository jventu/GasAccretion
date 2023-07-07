# GasAccretion

## Dependencies
This repository relies on the `Neural Fortran` library. To build the library you need `cmake`. `cmake` is already installed on many disributions by default, to check if this is the case open a terminal and type:
```
cmake --version
```
If `cmake` is not installed look for an installation guide online (e.g. [https://cmake.org/install/](https://cmake.org/install/)).

## Installation
To install simply run
```
sh build.sh
```
This will compile all sources including the `Neural Fortran` library.
If the build was successful, you will not have to repeat it and can start modifying the files in sources_ML. 
To generate the executable of the main Fortran routine in `sources_ML`, simply type:
```
sh compile_GasAcc.sh
```
## Run the simulation
The compilation should have created an executable called `GasAcc_exe` in the directory `sources_ML`. Go to said directory and run the executable:
```
./GasAcc_exe
```
