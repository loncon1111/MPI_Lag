#!/bin/csh

# -------------------------------------------------
# Set some parameters
# -------------------------------------------------

# Input GRIB directory
set grbdir=/work/users/lanphuong/For_Hoa/lagranto/run/input

# Output netCDF directory
set cdfdir=/work/users/lanphuong/For_Hoa/lagranto/run/output

# Start and end date for conversion, and time step
set startdate = 20170722_00
set finaldate = 20170723_23
set timestep  = 1
#set timestep = 6

# -------------------------------------------------
# Do the conversion
# -------------------------------------------------

# Incrrement finaldate by one timestep - to include finaldate
set finaldate=`./newtime ${finaldate} ${timestep}` 

# Change to grib directory
#cd ${cdfdir}

# Start loop over all dates
set date=${startdate}
loop:

# Convert an${date}_uvwt
#\rm -f P${date}_tuvw
cdo -f nc -t ecmwf -invertlat -chname,t,T -chname,w,OMEGA -chname,u,U -chname,v,V an${date}_tuvw P${date}_tuvw

# Convert an${date}_q
#\rm -f P${date}_q
cdo -f nc -t ecmwf -invertlat -chname,q,Q an${date}_q P${date}_q

# Convert an${date}_ps
#\rm -f P${date}_ps
#cdo -f nc -t ecmwf -invertlat -chname,lnsp,LNSP ${grbdir}/an${date}_ps P${date}_ps_scratch
cdo -f nc -t ecmwf -invertlat -chname,sp,SP an${date}_ps P${date}_ps_scratch 

#ncap2   -O -s 'PS=0.01f*exp(LNSP)' P${date}_ps  P${date}_ps
cdo -O -f nc -b 64 -L -setattribute,PS@units=hPa -expr,'PS=0.01*SP' P${date}_ps_scratch P${date}_ps

# Merge all files
#\rm -f P${date}
cdo -f nc merge P${date}_tuvw P${date}_q P${date}_ps P${date}
\rm -f P${date}_tuvw
\rm -f P${date}_q
\rm -f P${date}_ps
\rm -f P${date}_ps_scratch
\rm -f P${date}_vo
\rm -f P${date}_pv
#\rm -f P${date}_t

# Proceed to next date
set date=`./newtime ${date} ${timestep}` 
if ( "${date}" != "${finaldate}" ) goto loop

exit 0
