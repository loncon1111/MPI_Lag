#!/bin/csh

# -------------------------------------------------
# Set some parameters
# -------------------------------------------------

# Input GRIB directory
set grbdir=/work/users/lanphuong/For_Hoa/lagranto/run/input

# Output netCDF directory
set cdfdir=/work/users/lanphuong/For_Hoa/lagranto/run/output

# Start and end date for conversion, and time step
set startdate = 20170718_13
set finaldate = 20170805_23
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

cdo -f nc -t ecmwf -invertlat an${date}_z P${date}_z

# Merge all files
#\rm -f P${date}
cdo -f nc merge P${date}_z old/P${date} P${date}

\rm -f P${date}_z

# Proceed to next date
set date=`./newtime ${date} ${timestep}` 
if ( "${date}" != "${finaldate}" ) goto loop

exit 0
