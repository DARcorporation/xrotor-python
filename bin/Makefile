#*********************************************************
# Makefile for XROTOR Version 6.80
# M.Drela
# H.Youngren 11/28/98
#*********************************************************

SHELL = sh
BINDIR = /home/codes/bin/

###================================================
###  Default compilers and flags

## Gfortran
FC = gfortran
FFLAGS = -Ofast
FFLOPT = -Ofast
LIBS =
FTNLIB =
INSTALLCMD = install -s

###================================================


SRC = ../src

PROGS = xrotor

XROTOROBJS = xrotor.o xoper.o \
 			 xaero.o xio.o \
             xnoise.o xbend.o \
             xutils.o \
             spline.o userio.o vortex.o


all:	 $(PROGS)

install: 
	$(INSTALLCMD) $(PROGS) $(BINDIR)

clean:
	-/bin/rm *.o
	-/bin/rm $(PROGS)


### Make targets

xrotor: $(XROTOROBJS)
	$(FC) -o xrotor $(XROTOROBJS) $(LIBS)


xrotor.o: $(SRC)/xrotor.f90 $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xrotor.f90
xoper.o: $(SRC)/xoper.f90 $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xoper.f90
xio.o: $(SRC)/xio.f90 $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xio.f90
xaero.o: $(SRC)/xaero.f90 $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xaero.f90
xnoise.o: $(SRC)/xnoise.f90 $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xnoise.f90
xbend.o: $(SRC)/xbend.f90 $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xbend.f90
xutils.o: $(SRC)/xutils.f90
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xutils.f90
spline.o: $(SRC)/spline.f90
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/spline.f90
userio.o: $(SRC)/userio.f90
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/userio.f90
vortex.o: $(SRC)/vortex.f90
	$(FC) -c -I$(SRC) $(FFLOPT) $(SRC)/vortex.f90








