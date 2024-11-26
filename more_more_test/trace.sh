#!/bin/csh

# ---------------------------------------------------------------------
# Usage, parameter settings
# ---------------------------------------------------------------------

# Write usage information
if ( (${#argv} == 0) | (${#argv} < 2) ) then
  echo 
  echo "./trace inpfile outfile [ optional arguments ]"
  echo "**********************************************"
  echo " [ optional arguments ]                       "
  echo "   * - changet true/false                     "
  echo "   * - flag_i "
  echo "   * - flag_v "
  echo "   * - flag_f "
  echo "   * - flag_p "
  echo "   * - tvfile "
  
  echo  
  exit 0
endif

# Write title
echo 
echo '========================================================='
echo '       *** START OF PREPROCESSOR TRACE ***              '
echo

# Get the arguments
set inpfile   = $1
set outfile   = $2

# Set base directories (run+prog)
set cdfdir=${PWD}
set tradir=${PWD}

# Set program paths and filenames 
set parfile = ${tradir}/namelist.trace 
set prog    = ${tradir}/trace

# Set the prefix of the primary and secondary data files
set charp = 'P'

echo '---- DIRECTORIES AND PROGRAMS ---------------------------'
echo    
echo "CDF directory         : ${cdfdir}"
echo "TRA directory         : ${tradir}"
echo "PROGRAM TRACE         : ${prog}"
echo "PARAMETER file        : ${parfile}"
echo

# ---------------------------------------------------------------------
# Set optional flags
# ---------------------------------------------------------------------

echo '---- OPTIONAL FLAGS -------------------------------------'
echo

# Set some default values ("nil" must be set according to input files)
set flag_i     = "nil"
set flag_v     = "varlist"
set flag_f     = "nil"
set flag_p     = "nil"
set tvfile     = 'varlist'
set changet    = 'false'
set noclean    = 'false'
set timecheck  = 'false' 
set intmode    = 'normal'
set radius     = '0'        
set tropo_pv   = '2' 
set tropo_th   = '380'

# Set flag for consistency
set isok = 1

while ( $#argv > 0 )

  switch ( $argv[1] )

   case -i
     set flag_i=$argv[2]
     echo "Flag '-i'     -> ${flag_i} (user defined)"
     shift;
   breaksw

   case -v
     set flag_v="-v"
     set tvfile=$argv[2]
     echo "Flag '-v'     -> ${tvfile} (user defined)"
     shift;
     if ( $isok == 2 ) set isok = 0
     if ( $isok == 1 ) set isok = 2
   breaksw

   case -f
     set flag_f="-f"
     set tvfile="tvarlist.tmp"
     shift;
     set tvar="$argv[1]"
     shift;
     set tscale="$argv[1]"
     echo "Flag '-f'     -> ${tvar} ${tscale} (user defined)"
     if ( $isok == 2 ) set isok = 0
     if ( $isok == 1 ) set isok = 2
   breaksw

   case -p
     set flag_p=1
     echo "Flag '-p'     -> ${flag_p} (user defined)"
   breaksw

   case -changet
     set changet = 'true'
     echo "changet       -> true (user defined)"
   breaksw

   case -noclean
     set noclean = 'true'
     echo "noclean       -> true (user defined)"
   breaksw

   case -timecheck
     set timecheck = 'false'
     echo "timecheck               -> true (user defined)"
   breaksw

   case -nearest
     set intmode = 'nearest'
     echo "intmode                 -> nearest (user defined)"
   breaksw

   case -circle_avg
     set intmode = 'circle_avg'
     echo "intmode                 -> circle_avg (user defined)"
     shift;
     if ( "$1" == "" ) then
        echo "ERROR (circle_avg): specify radius in circle mode (km)! Example: -circle_avg 500"
        exit
     else
        set radius = $1
        echo 'intmode                 -> circle_avg radius =' ${radius} 'km'
     endif
   breaksw

   case -circle_min
     set intmode = 'circle_min'
     echo "intmode                 -> circle_min (user defined)"
     shift;
     if ( "$1" == "" ) then
        echo "ERROR (circle_min): specify radius in circle mode (km)! Example: -circle_min 400"
        exit
     else
        set radius = $1
        echo 'intmode                 -> circle_min radius =' ${radius} 'km'
     endif
   breaksw

   case -circle_max
     set intmode = 'circle_max'
     echo "intmode                 -> circle_max (user defined)"
     shift;
     if ( "$1" == "" ) then
        echo "ERROR (circle_max): specify radius in circle mode (km)! Example: -circle_max 600"
        exit
     else
        set radius = $1
        echo 'intmode                 -> circle_max radius =' ${radius} 'km'
     endif

   breaksw
   
  endsw
 
  shift;

end

# Set some defaults
if ( "${flag_p}"     == "nil" ) then
    set flag_p     = 0
    echo "Flag '-p'     -> 0 (default)"
endif

# No change of times necessary if no check requested
if ( "${timecheck}" == "false" ) then
   set  changet = 'false'
endif

# Check consitency of arguments
if ( $isok == 0 ) then
    echo
    echo " ERROR: Use either option '-v' or '-f', but not both..."
    exit 1
endif

# ---------------------------------------------------------------------
# Handle the input trajectory file
# ---------------------------------------------------------------------

echo
echo '---- TIME RANGE -----------------------------------------'
echo

# Check whether the input file can be found
if ( ! -f ${inpfile} ) then
    echo " ERROR : Input file ${inpfile} is missing"
    exit 1
endif

echo "Check 1"

# Get the start, end and reference date for the tracing
set startdate = `${tradir}/trainfo.sh ${inpfile} startdate` 
set enddate   = `${tradir}/trainfo.sh ${inpfile} enddate` 
set refdate   = `${tradir}/trainfo.sh ${inpfile} refdate` 
set ntra      = `${tradir}/trainfo.sh ${inpfile} ntra`
set ntim      = `${tradir}/trainfo.sh ${inpfile} ntim`
set ncol      = `${tradir}/trainfo.sh ${inpfile} ncol`

echo ${startdate} ${enddate}

# Check format of start and end date - must be the same
set ns=`echo $startdate | sed -e 's/_[0-9]*//' | wc -c`
echo $startdate | sed -e 's/_[0-9]*//'
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
set timediff_hh = `${tradir}/gettidiff ${enddate_ymdh} ${startdate_ymdh}`

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

# Write status information
echo "Time range      : ${startdate} -> ${enddate}"
if ( ${timediff_mm} != "" ) then
   echo "Time difference : ${timediff_hh} h ${timediff_mm} min"
else
   echo "Time difference : ${timediff_hh} h"
endif
echo "Direction       : ${direction} (${idir})"

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
  set timeinc=`${tradir}/gettidiff $date2 $date1`
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
set td1  = `${tradir}/gettidiff ${startdate_ymdh} ${date}`
set td2  = `${tradir}/gettidiff ${enddate_ymdh}   ${date}`

if (( $td1 < $timeinc || $td2 < $timeinc ) && ( $td1 >= 0 || $td2 >= 0 )) then
   set datfiles=$date
   if ( $td1 < $timeinc    ) set td=$td1
   if ( $td2 < $timeinc    ) set td=$td2
   if ( ( $startdate_min > 0 ) || ( $enddate_min > 0 ) ) @ td ++
   goto label2      
endif

foreach i ( ${charp}????????_?? )

  set date = `echo $i | cut -c 2-12`
  set td1  = `${tradir}/gettidiff ${startdate_ymdh} ${date}`
  set td2  = `${tradir}/gettidiff ${enddate_ymdh}   ${date}`

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
  set tdiff0 = `${tradir}/gettidiff ${startdate_ymdh} ${date}`
else
  set tdiff0 = `${tradir}/gettidiff ${enddate_ymdh} ${date}`
endif
  if ( $tdiff0 < 0 ) then
  echo " ERROR: data files missing for the specified time period"
  exit 1
endif

# Calculate the number of further files
@ num = ( $timediff_hh + $td ) / $timeinc + 1
@ dum1 = ( $num - 1 ) * $timeinc
@ dum2 = $timediff_hh + $td
if ( $dum1 != $dum2 ) @ num ++

# Get a list of all needed files
set numfiles=$num
while ( $num > 1 )

  set date=`${tradir}/newtime $date $timeinc`
  if ( ! -f ${charp}${date} ) then
    echo " ERROR: file with primary data is missing for $date"
    exit 1
  else
    set datfiles=`echo $datfiles $date`
  endif
  @ num --
end

# Calculate the start and the end time relative to the first datfile
if ( $direction == f ) then
  set tstart = `${tradir}/gettidiff $startdate $datfiles[1]`
  set tend   = `${tradir}/gettidiff $datfiles[$numfiles] $enddate`
else
  set tstart = `${tradir}/gettidiff $datfiles[$numfiles] $startdate`
  set tend   = `${tradir}/gettidiff $enddate $datfiles[1]`
endif


# Write some status information
echo "Primary file prefix               : ${charp}"
echo "Time increment for input files    : ${timeinc}"
echo "# input files                     : ${numfiles}"
echo "First input file                  : $datfiles[1] " 
echo "Last input file                   : $datfiles[$numfiles] " 
echo "${charp} files availability              : 1"  
if ( $direction == f ) then
echo "Start time relative to first file : $datfiles[1] + ${tstart} "
echo "End time relative to last file    : $datfiles[$numfiles] - ${tend} "  
else
echo "Start time relative to last file  : $datfiles[$numfiles] - ${tstart} "
echo "End time relative to first file   : $datfiles[1] + ${tend} "
endif

# ---------------------------------------------------------------------
# Check availability of input data 
# ---------------------------------------------------------------------

echo
echo '---- TRACEVAR FILE --------------------------------------'
echo    

# If "-f" option is used, create a temporary tracevar file
if ( "${flag_f}" == "-f" ) then

#   Preset values for <compfl> and <tprefix>
    set tcompfl=1
    
#   Check availability on P file
    foreach var ( `${tradir}/getvars ${charp}$datfiles[1]` )
       if ( "${var}" == "${tvar}" ) then
          set tcompfl=0
       endif
    end

#   Write the temporary <tracevars> file
    echo "${tvar} ${tscale} ${tcompfl}" >! ${tvfile}
    echo "Temporary tracervar file <${tvfile}> created"
    echo

endif


# Check if tracevars-file exists
if ( ! -f $tvfile ) then
  echo  " ERROR:  file $tvfile was not found ***"
  exit 1
endif

# check if the variables contained in the tracevars-file are available in the
# data file and check also if there are no empty lines in the tracevars-file
 
set nlines = `tail -n +4 $tvfile | wc -l`
set vars   = `tail -n +4 $tvfile | awk '{print $1}'`
set nvars  = `echo $vars | wc -w`
if ( $nlines != $nvars ) then
  echo " ERROR: tracevars-files must not contain empty lines ***"
  exit 1
endif
set calf=`tail -n +4 $tvfile | awk '{print $3}'`

# Write some status information
cat ${tvfile}
echo
echo "# Number of tracing variables        : ${nlines}"

# Loop over all variables - check availability
foreach v ( $vars )
  if ( $calf[1] == 0 ) then
    set v0 = `echo $v | awk 'BEGIN {FS = ":"}; {print $1}'`
 
    ${tradir}/getvars ${charp}$datfiles[1]

    set flag=`${tradir}/getvars ${charp}$datfiles[1] | grep " $v0 " | wc -l`
    set iscomment=`echo $v0 | cut -c 1` 
    
    if ( "$v0" == "PMIN" ) set flag = 1
    if ( "$v0" == "PMAX" ) set flag = 1
    
    if ( "${iscomment}" != "#" ) then 
	 if ( $flag == 0 ) then
           echo " ERROR: variable $v listed in $tvfile is not on the files ***"
           exit 1
	 endif
    endif
  endif
  shift calf
end
set ntrace=${nlines} 

# ---------------------------------------------------------------------
# Prepare input file for trace and run it
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

# ---------------------------------------------------------------------
# Prepare input file for caltra and run it
# ---------------------------------------------------------------------

# Check it before use
echo -n "Starting backup process in" 
echo -n "${vars}" | awk -v OFS="' '" '$1=$1'
echo "abc"

# Write parameter file
\rm -f ${parfile}
touch ${parfile}

\echo "&params" >> $parfile
\echo "   fbflag       = ${idir}     ," >> $parfile 
\echo "   timecheck    = .${timecheck}. ,">> $parfile
\echo "   tst          = ${tstart}     ," >> $parfile
\echo "   ten          = ${tend}       ," >>  $parfile
\echo "/" >> $parfile
\echo ""  >> $parfile
\echo "&input" >> $parfile
\echo "   startdate    = ${startdate}," >>  $parfile
\echo "   enddate      = ${enddate}," >>  $parfile
\echo "   inpfile      = "\"${inpfile}\""," >>  $parfile
\echo "   numdat       = ${numfiles}     ," >>  $parfile
\echo "   timeinc      = $timeinc     ," >>  $parfile
\echo "   per          = ${flag_p}" >>  $parfile
\echo "   ntra         = $ntra ," >>  $parfile
\echo "   ntim         = $ntim     ," >>  $parfile
\echo "   ncol         = $ncol     ," >>  $parfile
\echo "/" >>  $parfile
\echo "" >>  $parfile
\echo "&tracevars" >>  $parfile
\echo "   ntrace0      = $ntrace     ," >>  $parfile
\echo -n "   tvar         = '" >>  $parfile
\echo -n "${vars}'," | awk -v OFS="' '" '$1=$1' >>  $parfile
\echo "   ! Available diagnostics: TH,THE,PV,RH,DUDX,DUDY,DVDX,DVDY,DTDX,DTDY,..." >>  $parfile
\echo "   tfile        = '${tvfile}'" >>  $parfile
\echo "/" >>  $parfile
\echo "&interparams" >>  $parfile
\echo "   intmode      = "\"${intmode}\""," >>  $parfile
\echo "   radius       = ${radius}   ," >>  $parfile
\echo "   ! Availabe interpolation method:" >>  $parfile
\echo "   !'nearest','circle_avg','circle_min','circle_max','normal','default'" >>  $parfile
\echo "/" >>  $parfile
\echo "" >>  $parfile
\echo "&output" >>  $parfile
\echo "   outfile      = "\"${outfile}\""," >>  $parfile
\echo "/" >>  $parfile

# Finish the preprocessor
echo 
echo '       *** END OF PREPROCESSOR TRACE ***              '
echo '========================================================='
echo

# ---------------------------------------------------------------------
# Final tasks (make clean)
# ---------------------------------------------------------------------

exit 0 
