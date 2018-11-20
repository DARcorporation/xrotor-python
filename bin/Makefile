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
             xinte.o xutils.o \
             srclin.o spline.o userio.o vortex.o


all:	 $(PROGS)

install: 
	$(INSTALLCMD) $(PROGS) $(BINDIR)

clean:
	-/bin/rm *.o
	-/bin/rm $(PROGS)


### Make targets

xrotor: $(XROTOROBJS)
	$(FC) -o xrotor $(XROTOROBJS) $(LIBS)


xrotor.o: $(SRC)/xrotor.f $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xrotor.f
xoper.o: $(SRC)/xoper.f $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xoper.f
xio.o: $(SRC)/xio.f $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xio.f
xaero.o: $(SRC)/xaero.f $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xaero.f
xnoise.o: $(SRC)/xnoise.f $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xnoise.f
xbend.o: $(SRC)/xbend.f $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xbend.f
xinte.o: $(SRC)/xinte.f $(SRC)/XROTOR.INC
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xinte.f
xutils.o: $(SRC)/xutils.f
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/xutils.f
srclin.o: $(SRC)/srclin.f
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/srclin.f
spline.o: $(SRC)/spline.f
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/spline.f
userio.o: $(SRC)/userio.f
	$(FC) -c -I$(SRC) $(FFLAGS) $(SRC)/userio.f
vortex.o: $(SRC)/vortex.f
	$(FC) -c -I$(SRC) $(FFLOPT) $(SRC)/vortex.f








