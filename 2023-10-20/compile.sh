#!/bin/bash
# is the character for comments in bash (here we choose the bash interpreter to be used)

# program compilation and linking
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -o duesensori2.exe duesensori2.F90
