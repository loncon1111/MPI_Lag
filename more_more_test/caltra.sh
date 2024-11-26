#!/bin/csh

# ---------------------------------------------------------------------
# Usage, parameter settings
# ---------------------------------------------------------------------

# Write usage information
if ( (${#argv} == 0) | (${#argv} < 4) ) then
  echo 'USAGE: ./caltra.sh startdate enddate startfile filename [ optional arguments ]' 
  echo  
  exit 0
endif

# Write title
echo 
echo '========================================================='
echo '       *** START OF PREPROCESSOR CALTRA ***              '
echo

# Get the arguments
set startdate = $1
set enddate   = $2
set startf    = $3
set outfile   = $4
if ( ${#argv} > 4 ) then
   set flags = $5
else
   set flags=
endif 

# Set base directories (run+prog)
set cdfdir=${PWD}
set tradir=${PWD}

# Set program paths and filenames 
set nlfile = ${tradir}/namelist.caltra 

# Set the prefix of the primary and secondary data files
set charp = 'P'

echo '---- DIRECTORIES AND PROGRAMS ---------------------------'
echo    
echo "CDF directory         : ${cdfdir}"
echo "TRA directory         : ${tradir}"
echo

# ---------------------------------------------------------------------
# Set optional flags
# ---------------------------------------------------------------------

echo '---- OPTIONAL FLAGS -------------------------------------'
echo

# Set some default values ("nil" must be set according to input files)
set flag_j     = "nil"
set flag_i     = "nil"
set flag_t     = "nil"
set flag_o     = "nil"
set flag_p     = "nil"
set changet    = 'false'
set timecheck  = '.false.' 
set modlev     = '.false.'

while ( $#argv > 0 )

  switch ( $argv[1] )

   case -j
     set flag_j=1
     echo "Flag '-j'     -> ${flag_j} (user defined)"
   breaksw

   case -i
     set flag_i=$argv[2]
     echo "Flag '-i'     -> ${flag_i} (user defined)"
     shift;
   breaksw

   case -t
     set flag_t=$argv[2]
     echo "Flag '-t'     -> ${flag_t} (user defined)"
     shift;
   breaksw

   case -o
     set flag_o=$argv[2]
     echo "Flag '-o'     -> ${flag_o} (user defined)"
     shift;
   breaksw

   case -p
     set flag_p=1
     echo "Flag '-p'     -> ${flag_p} (user defined)"
   breaksw

   case -changet
     set changet = 'true'
     echo "changet       -> true (user defined)"
   breaksw

   case -timecheck
     set timecheck = '.true.'
     echo "timecheck               -> true (user defined)"
   breaksw
   
   case -2d
     set modlev = '.true.'
     echo "2d (model level)        -> true (user defined)"
   breaksw
  
  endsw
 
  shift;

end

# No change of times necessary if no check requested
if ( "${timecheck}" == ".false." ) then
   set  changet = 'false'
endif

# Set some defaults
if ( "${flag_j}"     == "nil" ) then
    set flag_j     = 0
    echo "Flag '-j'     -> 0 (default)"
endif
if ( "${flag_p}"     == "nil" ) then
    set flag_p     = 0
    echo "Flag '-p'     -> 0 (default)"
endif

# ---------------------------------------------------------------------
# Handle the time specifier - startdate, enddate
# ---------------------------------------------------------------------

echo
echo '---- TIME RANGE -----------------------------------------'
echo

# Check format of start and end date - must be the same
set ns=`echo $startdate | sed -e 's/_[0-9]*//' | wc -c`
set ne=`echo $enddate   | sed -e 's/_[0-9]*//' | wc -c`
if ( $ns != $ne ) then
  echo " ERROR: start and end date must be in the same format ***"
  exit 1
endif
if ( $ns != 9 ) then
  echo " ERROR: Date format must be yyyymmdd ***"
  exit 1
endif
set ns=`echo $startdate | sed -e 's/[0-9]*_//' | wc -c`
set ne=`echo $enddate   | sed -e 's/[0-9]*_//' | wc -c`
if ( $ns != $ne ) then
  echo " ERROR: start and end date must be in the same format ***"
  exit 1
endif
if ( ( $ns != 5 ) & ( $ns != 3 ) ) then
  echo " ERROR: Time format must be hh(mm) ***"
  exit 1
endif

# Split the start and end date into <yymmdd_hh and mm>
set startdate_ymdh = `echo $startdate | cut -c 1-11`
set startdate_min  = `echo $startdate | cut -c 12-13`
if ( $startdate_min == "" ) set startdate_min = 00
 
set enddate_ymdh = `echo $enddate | cut -c 1-11`
set enddate_min  = `echo $enddate | cut -c 12-13`
if ( $enddate_min == "" ) set enddate_min = 00

# Get the time difference between <start_ymdh> and <end_ymdh> date
# Decide whether trajectoriesare forward or backward
set timediff_hh = `./gettidiff ${enddate_ymdh} ${startdate_ymdh}`

if ( $timediff_hh == 0 ) then
  if ( $enddate_min > $startdate_min ) then
    set direction = f
    set idir      = 1
  else
    set direction = b
    set idir      = -1
  endif
else if ( $timediff_hh > 0 ) then
  set direction = f
  set idir      = 1
else
  set direction = b
  set idir      = -1
  @ timediff_hh = $idir * $timediff_hh
endif

# Get also minutes for time difference, if <start_min> or <end_min> != 0
set timediff_mm=

if ( $startdate_min != 00 || $enddate_min != 00 ) then
  @ min = ( $enddate_min - $startdate_min )
  if ( $min == 0 ) then
    set timediff_mm=
  else if ( $min > 0 ) then
    if ( $idir == 1 ) then
      set timediff_mm=$min
    else
      @ timediff_hh --
      @ timediff_mm = 60 - $min
    endif
  else
    if ( $idir == 1 ) then
      @ timediff_hh --
      @ timediff_mm = 60 + $min
    else
      @ timediff_mm = 0 - $min
    endif
  endif
endif

# Set the reference date equal to the satrtdate
set refdate=${startdate}

# Write status information
echo "Time range      : ${startdate} -> ${enddate}"
if ( ${timediff_mm} !=  "" ) then
   echo "Time difference : ${timediff_hh} h ${timediff_mm} min"
else
   echo "Time difference : ${timediff_hh} h"
endif
echo "Direction       : ${direction} (${idir})"
echo "Reference date  : ${refdate}"

# ---------------------------------------------------------------------
# Check availability of input data 
# ---------------------------------------------------------------------

echo
echo '---- INPUT FILES ----------------------------------------'
echo

# Take the time increment from flag list ('nil', if not defined)
set timeinc = ${flag_i}

# Find a first data file (if possible corresponding to start/end date
# If starttime is not a data time, take the first file in the direectory
if ( $direction == "f" ) then
  set file=${charp}${startdate_ymdh}
else
  set file=${charp}${enddate_ymdh}
endif
if ( ! -f $file ) then
  set file=`ls ${charp}[0-9_]*[0-9] | head -1 | sed -e 's/@//'`
endif

# Determine timeinc (the time difference in hours between two data file)
# if not already defined with option -i
if ( ${timeinc} == "nil" ) then
  set date1=`echo $file | cut -c 2-12`
  set n=`ls ${charp}[0-9_]*[0-9] | grep -n $date1 | awk -F: '{print $1}'`
  @ n ++
  set date2=`ls ${charp}[0-9_]*[0-9] | head -$n | tail -1 | cut -c 2-12`
  set timeinc=`./gettidiff $date2 $date1`
endif
if ( $timeinc == 0 ) then
    echo " ERROR: cannot set the time increment between input files ***"
    exit 1
endif

# Search the first file to use: We step through all P files and see whether they are
# good P files. Let's first do the test for the first data file found. If it's ok, we 
# take it; if not, we step through all P files and find the good one  
set flag=0
set td=

set date = `echo $file | cut -c 2-12`
set td1  = `./gettidiff ${startdate_ymdh} ${date}`
set td2  = `./gettidiff ${enddate_ymdh}   ${date}`


echo $date
if (( $td1 < $timeinc || $td2 < $timeinc ) && ( $td1 >= 0 || $td2 >= 0 )) then
   set datfiles=$date
   if ( $td1 < $timeinc    ) set td=$td1
   if ( $td2 < $timeinc    ) set td=$td2
   if ( ( $startdate_min > 0 ) || ( $enddate_min > 0 ) ) @ td ++
   goto label2      
endif

foreach i ( ${charp}????????_?? )

  set date = `echo $i | cut -c 2-12`
  set td1  = `./gettidiff ${startdate_ymdh} ${date}`
  set td2  = `./gettidiff ${enddate_ymdh}   ${date}`
  if (( $td1 < $timeinc || $td2 < $timeinc ) && ( $td1 >= 0 || $td2 >= 0 )) then
     set datfiles=$date
     if ( $td1 < $timeinc    ) set td=$td1
     if ( $td2 < $timeinc    ) set td=$td2
     if ( ( $startdate_min > 0 ) || ( $enddate_min > 0 ) ) @ td ++
     goto label2      
  endif

end

# if no P/T-files are available for the specified time period, then $td is
# still undefined
if ( $td == "" ) then
  echo " ERROR: no data files available for the specified time period"
  exit 1
endif

# Everything is fine so far: proceed
label2:

# Check whether first date is ok - before or at needed dates
if ( $direction == "f" ) then
  set tdiff0 = `./gettidiff ${startdate_ymdh} ${date}`
else
  set tdiff0 = `./gettidiff ${enddate_ymdh} ${date}`
endif
  if ( $tdiff0 < 0 ) then
  echo " ERROR: data files missing for the specified time period"
  exit 1
endif

echo $tdiff0
# Calculate the number of further files
@ num = ( $timediff_hh + $td ) / $timeinc + 1
@ dum1 = ( $num - 1 ) * $timeinc
@ dum2 = $timediff_hh + $td
if ( $dum1 != $dum2 ) @ num ++

# Get a list of all needed files
set numfiles=$num
set sfiles=1
echo $datfiles
echo ${charp}${datfiles} > input.lst
while ( $num > 1 )
  set date=`./newtime $date $timeinc`

  echo $date
  echo ${charp}${date} >> input.lst
  if ( ! -f ${charp}${date} ) then
    echo " ERROR: file with primary data is missing for $date"
    exit 1
  endif
    set datfiles=`echo $datfiles $date`
  @ num --
end

# Special handling for 2d trajectories - check whether P,PS are available
if ( "${modlev}" == "yes" ) then  
   set flonp = 1
   foreach date ( ${datfiles} )
       set ok = `./getvars ${charp}${date} | grep P` 
       if ( "${ok}" == "" ) set flonp = 0
       set ok = `./getvars ${charp}${date} | grep PS` 
       if ( "${ok}" == "" ) set flonp = 0
   end
   if ( ${flonp} == 0 ) then
      echo ' ERROR: P and PS must be available on P files for 2D mode'
      exit 1
   endif
endif
   
# Write some status information
echo "Primary file prefix            : ${charp}"
echo "Time increment for input files : ${timeinc}"
echo "# input files                  : ${numfiles}"  
echo "First input file               : $datfiles[1] " 
echo "Last input file                : $datfiles[$numfiles] " 

# ---------------------------------------------------------------------
# Handle vertical wind - scaling factor
# ---------------------------------------------------------------------

# No vertical wind scaling is needed for isentropic and 2d trajectories
set wfactor = 1
if ( "${modlev}" == "yes" ) goto next1

echo
echo '---- VERTICAL WIND SCALING ------------------------------'
echo

# Check whether OMEGA is avialbale on first file
set first=$datfiles[1]
set file="${charp}${first}"
set omflag=`./getvars $file | grep " OMEGA " | wc -l`
if ( $omflag != 1 ) then
   echo " ERROR: variable OMEGA is not on file $file"
   exit 1
endif

# Get the maximum and minimum wind on 850 hPa and decide the <wfactor>
set wmin  = `./getmima $file OMEGA P850 | tail -n 1 | awk '{print $1}'`
echo "`./getmima $file OMEGA P850 | awk '{print $1}`"
set wmax  = `./getmima $file OMEGA P850 | tail -n 1 | awk '{print $2}'`
set wmin  = `echo $wmin | sed -e 's/\.//'`
set wmax  = `echo $wmax | sed -e 's/\.//'`
set wdiff = `echo $wmax - $wmin | bc`
if ( $wdiff > 400 ) then
    set wfactor=1.
endif
if ( $wdiff < 400 ) then
    set wfactor=100.
endif

# Write status information
echo "Vertical scaling factor (wfactor)  : ${wfactor}"

next1:

# ---------------------------------------------------------------------
# Time step and output interval
# ---------------------------------------------------------------------

echo
echo '---- TIME STEPS -----------------------------------------'
echo

# Take the time step and output step from flag list ('nil', if not defined)
set timestep = ${flag_t}
set deltout  = ${flag_o}

# Calculate the time step
if ( $timestep == "nil" ) @ timestep = ( 60 * $timeinc ) / 12

# Take the output interval from time increment
if ( $deltout == "nil" ) @ deltout = 60 * ${timeinc}

# Check whether the timestep is an integer ratio of deltout
@ flag = $deltout / $timestep
@ flag = $deltout - ( $flag * $timestep )
if ( $flag != 0 ) then
  echo " ERROR: output time interval should be multiple of timestep"
  echo 
  echo "           $deltout min  : output time interval"
  echo "           $timestep min  : time step"
  exit 1
endif

# Calculate the start and the end time relative to the first datfile
if ( $direction == f ) then
  set tstart = `./gettidiff $startdate $datfiles[1]`
  set tend   = `./gettidiff $datfiles[$numfiles] $enddate`
else
  set tstart = `./gettidiff $datfiles[$numfiles] $startdate`
  set tend   = `./gettidiff $enddate $datfiles[1]`
endif

# Check whether tstart and tend are a multiple of the output time interval
if ( $tstart != 0 ) then
  if ( `echo $tstart | grep "\."` != "" ) then
    set dum=`echo $tstart | sed -e 's/[-0-9]*\.//'`
    @ flag = $dum / $deltout
    @ flag = $dum - ( $flag * $deltout )
    if ( $flag != 0 ) then
      echo " ERROR : the start time should be shifted relative to the first"
      echo "         datafile by a multiple of the output time interval"
      echo "         hint:  set the latter with the option -o"
      exit 1
    endif
  endif
endif
if ( `echo $tend | grep "\."` != "" ) then
  set dum=`echo $tend | sed -e 's/[-0-9]*\.//'`
  @ flag = $dum / $deltout
  @ flag = $dum - ( $flag * $deltout )
  if ( $flag != 0 ) then
    echo " ERROR : the end time should be shifted relative to the first"
    echo "         datafile by a multiple of the output time interval"
    echo "         hint:  set the latter with the option -o"
    exit 1
  endif
endif

# Write status information
echo "Trajectory calculation time step [min] : ${timestep}"
echo "Output time step [min]                 : ${deltout}"
if ( $direction == f ) then
  echo "Start time relative to first file      : $datfiles[1] + ${tstart} "
  echo "End time relative to first file        : $datfiles[$numfiles] - ${tend} "  
else
  echo "Start time relative to first file      : $datfiles[$numfiles] - ${tstart} "
  echo "End time relative to last file         : $datfiles[1] + ${tend} "
endif

# ---------------------------------------------------------------------
# Start file
# ---------------------------------------------------------------------

echo
echo '---- START FILE -----------------------------------------'
echo

# Check if start file is available
if ( ! -f ${startf} ) then
   echo " ERROR : start file ${startf} is missing"
   exit 1
endif

# Decide whether startfile has an explicit format specifier
set format = "0"
foreach app ( 1 2 3 4 5 6 7 8 9 )
  set flag = `echo ${startf} | grep "\.${app}"`
  if ( "${flag}" != "" ) set format = "${app}"
end

# If format is 0, it might nevertheless be a hidden format 1
if ( "${format}" == "0" ) then
    set ncol = `awk "{print NF}" ${startf} | tail -1` 
    if ( "${ncol}" != "3" ) then
        set format = "1"
        echo " WARNING: ${startf} is a hidden trajectory file of format 1"
        echo "          it will be renamed: ${startf} -> ${startf}.1"
        echo
        ln -sf ${startf} ${startf}.1
        set startf = "${startf}.1"
    endif
endif

# Get the number of trajectories
if ( "${format}" == "0" ) then
   set ntra = `wc -l ${startf} | awk '{print $1}' `
   set ncol = 3
else
   set ntra = `./trainfo.sh ${startf} ntra`
   set ncol = `./trainfo.sh ${startf} ncol`
   set ntim = `./trainfo.sh ${startf} ntim`
   
   if ( "${ntim}" != "1" ) then
      echo " ERROR: starting trajectory file must only have one time... Stop (${ntim})"
      exit 1
   endif

endif

# Write status information
echo "Start file                  : ${startf} "
if ( "${format}" == "0" ) then 
   echo "Format                      : (lon,lat,p) list"
else
   echo "Format                      : trajectory file (${format})"
endif
echo "# coordinates (lon,lat,lev) : ${ntra} "  
echo "# columns                   : ${ncol} "  

# ---------------------------------------------------------------------
# Prepare input file for caltra and run it
# ---------------------------------------------------------------------

# Set times relative to the reference date
if ( "${changet}" == "true" ) then
 echo
 echo '---- CHANGE TIMES ON DATA FILES  ------------------------'
 echo   
 foreach i ( $datfiles )
   ./changet.sh ${refdate} ${charp}${i}
 end
endif

# Split the reference date
set yyyy=`echo ${refdate}   | cut -c 1-4` 
set   mm=`echo ${refdate}   | cut -c 5-6` 
set   dd=`echo ${refdate}   | cut -c 7-8` 
set   hh=`echo ${refdate}   | cut -c 10-11` 
set  min=`echo ${refdate}00 | cut -c 12-13` 

# Get the total tiem range
if ( ${timediff_mm} != '' ) then
   @ timerange = 60 * ${timediff_hh} + ${timediff_mm}
else
   @ timerange = 60 * ${timediff_hh}
endif

# Write parameter file
\rm -f ${nlfile}
touch ${nlfile}

\echo "&params" >> ${nlfile}
\echo "   fbflag	= $idir	   ," >> ${nlfile}
\echo "   imethod	= 3	   ," >> ${nlfile}
\echo "   jflag	= ${flag_j}," >> ${nlfile}
\echo "   wfactor	= ${wfactor}," >> ${nlfile}
\echo "   timecheck	= ${timecheck}," >> ${nlfile}
\echo "   ts		= ${timestep}," >> ${nlfile}
\echo "   tst		= ${tstart}," >> ${nlfile}
\echo "   ten		= ${tend}," >> ${nlfile}
\echo "/" >> ${nlfile}
\echo "" >> ${nlfile}
\echo "&input" >> ${nlfile}
\echo "   year		= ${yyyy}  ," >> ${nlfile}
\echo "   month	= ${mm}	   ," >> ${nlfile}
\echo "   day		= ${dd}    ," >> ${nlfile}
\echo "   hour	        = ${hh}	   ," >> ${nlfile}
\echo "   minute	= ${min}   ," >> ${nlfile}
\echo "   time_range	= ${timerange}," >> ${nlfile}
\echo "   modlev	= ${modlev}," >> ${nlfile}
\echo "   numdat	= ${numfiles}," >> ${nlfile}
\echo "   timeinc	= ${timeinc}," >> ${nlfile}
\echo "   per		= ${flag_p}," >> ${nlfile}
\echo "   strname	= "\"${startf}\""," >> ${nlfile}
\echo "   ntra		= ${ntra}," >> ${nlfile}
\echo "   ncol		= ${ncol}," >> ${nlfile}
\echo "/" >> ${nlfile}
\echo "" >> ${nlfile}
\echo "&output" >> ${nlfile}
\echo "   cdfname	= "\"${outfile}\""," >> ${nlfile}
\echo "   deltout	= ${deltout}," >> ${nlfile}
\echo "/" >> ${nlfile}


# Finish the preprocessor
echo 
echo '       *** END OF PREPROCESSOR CALTRA ***              '
echo '========================================================='
echo

# Run caltra
#./caltra

if ( "${status}" != "0" ) then
  echo "ERROR:  Program <caltra> failed"
  exit 1
endif


exit 0 

