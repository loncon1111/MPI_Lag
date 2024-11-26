# Configuration of include subroutine/functions
FC = gfortran
FFLAGS = -O -I$(NETCDF)/include
#LIBS   = -L$(NETCDF)/lib -lnetcdf -lnetcdff -L../lib/libcdfio.a -L../lib/libcdfplus.a
LIBS   = -L$(NETCDF)/lib -lnetcdf -lnetcdff
OBJS   =

.SUFFIXES: .f90 .f .o

.f90.o:
	$(FC) $(FFLAGS) $(OBJ) $(LDFLAGS) $(LIBS) -c $*.f90
.f.o:
	$(FC) $(FFLAGS) $(OBJ) $(LDFLAGS) $(LIBS) -c $*.f

all: libcdfplus.o libcdfio.o

libcdfplus.a : libcdfplus.f
libcdfio.a   : libcdfio.f

clean:
	rm *.a *.o 
