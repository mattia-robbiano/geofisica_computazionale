#!/bin/bash
# is the character for comments in bash (here we choose the bash interpreter to be used)

cd ./bin

# program compilation and linking
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ../sub_aust.F90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ../PROCESSING.F90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ../IOSTREAM.F90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace ../australia.F90 sub_aust.o PROCESSING.o IOSTREAM.o -o australia.exe

cd ..