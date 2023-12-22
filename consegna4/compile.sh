#!/bin/bash
# is the character for comments in bash (here we choose the bash interpreter to be used)

# program compilation and linking
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ./PROCESSING.f90
#gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ./SUB_AUST.F90
#gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ./MATH.f90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ./IOSTREAM.f90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace ./main.f90 IOSTREAM.o PROCESSING.o -o main.exe

#rm ./iostream.mod ./processing.mod ./sub_aust.mod ./IOSTREAM.o ./PROCESSING.o ./SUB_AUST.o