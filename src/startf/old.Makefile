# Configuration of program startf
FC = gfortran
FFLAGS = -O -I$(NETCDF)/include
INCDIR = ../../include
LIBDIR = ../../lib
LIBS   = -L$(NETCDF)/lib -lnetcdf -lnetcdff
OBJS   = $(LIBDIR)/libcdfio.a $(LIBDIR)/libcdfplus.a $(INCDIR)/module_ncinput.o \
	 $(INCDIR)/module_interp.o $(INCDIR)/module_caltime.o $(INCDIR)/module_iotra.o
TARGET = ../../bin/

.SUFFIXES: .f90 .f .o

.f90.o:
	$(FC) $(LIBS) $(FFLAGS) $(OBJS) -J$(INCDIR) -c $*.f90
.f.o:
	$(FC) $(LIBS) $(FFLAGS) $(OBJS) -J$(INCDIR) -c $*.f

all: startf
	if [ ! -d $(TARGET) ]; then mkdir -p $(TARGET); fi
	cp startf $(TARGET)
	chmod +x $(TARGET)/startf

startf: $(OBJS) sub_startf.o startf.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -o startf startf.o sub_startf.o

startf.o        : startf.f90 sub_startf.o
sub_startf.o    : sub_startf.f90

clean:
	rm *.o
distclean:
	rm *.o startf $(TARGET)/startf
