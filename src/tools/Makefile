# Configuration of program caltra
FC = mpif90
FFLAGS = -O -I$(NETCDF)/include
LIBS   = -L$(NETCDF)/lib -lnetcdf -lnetcdff 
INCDIR = ../../include
LIBDIR = ../../lib
OBJS   = $(LIBDIR)/libcdfio.o $(LIBDIR)/libcdfplus.o $(INCDIR)/module_ncinput.o \
	 $(INCDIR)/module_interp.o $(INCDIR)/module_caltime.o $(INCDIR)/module_iotra.o
TARGET = ../../bin/
OBJS1  = gettidiff.o getmima.o newtime.o trainfo.o datelist.o extract.o lsl2list.o

.SUFFIXES: .f90 .f .o

.f90.o:
	$(FC) $(FFLAGS) $(LIBS) -J$(INCDIR) -c $*.f90
.f.o:
	$(FC) $(FFLAGS) $(LIBS) -J$(INCDIR) -c $*.f

install: getvars gettidiff getmima newtime trainfo datelist changet extract lsl2list
	if [ ! -d $(TARGET) ]; then mkdir -p $(TARGET); fi
	cp $^  $(TARGET)
	chmod +x $(TARGET)/$^

gettidiff: $(OBJS) gettidiff.o 
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -J$(INCDIR) -o gettidiff gettidiff.o
getmima: $(OBJS) getmima.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -J$(INCDIR) -o getmima getmima.o
newtime: $(OBJS) newtime.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -J$(INCDIR) -o newtime newtime.o
trainfo: $(OBJS) trainfo.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -J$(INCDIR) -o trainfo trainfo.o 
datelist: $(OBJS) datelist.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -J$(INCDIR) -o datelist datelist.o
getvars: $(OBJS) getvars.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -J$(INCDIR) -o getvars getvars.o
changet: $(OBJS) changet.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -J$(INCDIR) -o changet changet.o
extract: $(OBJS) extract.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -J$(INCDIR) -o extract extract.o
lsl2list: $(OBJS) lsl2list.o
	$(FC) $(FFLAGS) $(OBJS) $(LDFLAGS) $(LIBS) -J$(INCDIR) -o lsl2list lsl2list.o

gettidiff.o: gettidiff.f90
getmima.o: getmima.f
newtime.o: newtime.f
trainfo.o: trainfo.f
datelist.o: datelist.f
getvars.o: getvars.f
changet.o: changet.f
extract.o: extract.f
lsl2list.o: lsl2list.f

clean:
	rm *.o
distclean: gettidiff getmima newtime trainfo datelist extract lsl2list
	rm *.o $(TARGET)/$^
