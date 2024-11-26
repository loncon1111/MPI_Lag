# Configuration of program startf
FC = mpif90
FFLAGS = -O -I$(NETCDF)/include
LIBS   = -L$(NETCDF)/lib -L$(LAPACK_PATH)/lib -llapack -lnetcdf -lnetcdff 
INCDIR = ../../include
LIBDIR = ../../lib
OBJS   = $(LIBDIR)/libcdfio.o $(LIBDIR)/libcdfplus.o $(INCDIR)/module_ncinput.o \
	 $(INCDIR)/module_interp.o $(INCDIR)/module_caltime.o $(INCDIR)/module_iotra.o \
	 $(INCDIR)/module_calvar.o $(INCDIR)/module_common_mpi.o
TARGET = ../../bin/

.SUFFIXES: .f90 .f .o

.f90.o:
	$(FC) $(FFLAGS) $(LIBS) -J$(INCDIR) -c $*.f90
.f.o:
	$(FC) $(FFLAGS) $(LIBS) -J$(INCDIR) -c $*.f

all: trace
	if [ ! -d $(TARGET) ]; then mkdir -p $(TARGET); fi
	cp trace $(TARGET)
	chmod +x $(TARGET)/trace

trace: $(OBJS) sub_trace.o trace.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -o trace trace.o sub_trace.o 

trace.o        : trace.f90 sub_trace.o
sub_trace.o    : sub_trace.f90

clean:
	rm *.o
distclean:
	rm *.o trace $(TARGET)/trace
