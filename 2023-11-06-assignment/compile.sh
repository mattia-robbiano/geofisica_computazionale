#!/bin/bash
# is the character for comments in bash (here we choose the bash interpreter to be used)

# program compilation and linking
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c sub_aust.F90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace australia.F90 sub_aust.o -o australia.exe
