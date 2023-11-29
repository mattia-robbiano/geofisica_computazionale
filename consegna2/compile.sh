#!/bin/bash
# is the character for comments in bash (here we choose the bash interpreter to be used)

# program compilation and linking
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ./PROCESSING.F90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ./SUB_AUST.F90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ./IOSTREAM.F90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace ./australia.F90 SUB_AUST.o PROCESSING.o IOSTREAM.o -o australia.exe

#rm ./iostream.mod ./processing.mod ./sub_aust.mod ./IOSTREAM.o ./PROCESSING.o ./SUB_AUST.o