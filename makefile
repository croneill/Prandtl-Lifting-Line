SOURCES= Utilities.f90 ForSVG.f90 GaussElimination.f90 prl2.f90
EXE=prl2

all:
	gfortran $(SOURCES) -o $(EXE) -Og -g -fimplicit-none

strict:
	gfortran $(SOURCES) -o $(EXE) -Og -g -fimplicit-none -Wall -Wsurprising

fast:
	gfortran $(SOURCES) -o $(EXE) -static -Ofast -march=native -ffast-math -funroll-loops

run:
	make
	./$(EXE).exe
