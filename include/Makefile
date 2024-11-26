# Configuration of include subroutine/functions
FC = mpif90
FFLAGS = -O -I$(NETCDF)/include
#LIBS   = -L$(NETCDF)/lib -lnetcdf -lnetcdff -L../lib/libcdfio.a -L../lib/libcdfplus.a
LIBS   = -L$(NETCDF)/lib -L$(LAPACK_PATH)/lib -llapack  -lnetcdf -lnetcdff
LIBDIR = ../lib
OBJS   = $(LIBDIR)/libcdfplus.o $(LIBDIR)/libcdfio.o

.SUFFIXES: .f90 .f .o

.f90.o:
	$(FC) $(FFLAGS) $(OBJ) $(LDFLAGS) $(LIBS) -c $*.f90
.f.o:
	$(FC) $(FFLAGS) $(OBJ) $(LDFLAGS) $(LIBS) -c $*.f

all: module_ncinput.o module_interp.o module_caltime.o module_iotra.o module_calvar.o module_common_mpi.o

module_caltime.o: module_caltime.f90
module_interp.o : module_interp.f90
module_ncinput.o: module_ncinput.f
module_iotra.o  : module_iotra.f90
module_calvar.o : module_calvar.f90
module_common_mpi.o: module_common_mpi.f90

clean:
	rm *.o *.mod 
distclean:
	rm *.o *.mod
