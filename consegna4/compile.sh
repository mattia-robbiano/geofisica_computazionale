#!/bin/bash

# program compilation and linking
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ./Robbiano_RadioSondaggi.f90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace -c ./Robbiano_IOSTREAM.f90
gfortran -Wextra -Wall -Wconversion -fimplicit-none -pedantic -fcheck=all -fbacktrace ./Robbiano_main.f90 Robbiano_IOSTREAM.o Robbiano_RadioSondaggi.o -o main.exe
