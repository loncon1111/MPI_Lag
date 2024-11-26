# Configuration of program caltra
FC = gfortran
FFLAGS = -O -I$(NETCDF)/include
LIBS   = -L$(NETCDF)/lib -lnetcdf -lnetcdff 
INCDIR = ../../include
LIBDIR = ../../lib
OBJS   = $(LIBDIR)/libcdfio.o $(LIBDIR)/libcdfplus.o $(INCDIR)/module_ncinput.o \
	 $(INCDIR)/module_interp.o $(INCDIR)/module_caltime.o $(INCDIR)/module_iotra.o
TARGET = ../../bin/

.SUFFIXES: .f90 .f .o

.f90.o:
	$(FC) $(FFLAGS) -J$(INCDIR) -c $*.f90
.f.o:
	$(FC) $(FFLAGS) -J$(INCDIR) -c $*.f

all: caltra
	if [ ! -d $(TARGET) ]; then mkdir -p $(TARGET); fi
	cp caltra $(TARGET)
	chmod +x $(TARGET)/caltra

caltra: $(OBJS) sub_caltra.o caltra.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -o caltra caltra.o sub_caltra.o

caltra.o        : caltra.f90 sub_caltra.o
sub_caltra.o    : sub_caltra.f90

clean:
	rm *.o
distclean:
	rm *.o caltra $(TARGET)/caltra
