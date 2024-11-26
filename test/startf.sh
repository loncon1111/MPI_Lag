#!/bin/csh

# -----------------------------------------------------------------------------------------------------------------
# Usage and parameter handling
# -----------------------------------------------------------------------------------------------------------------

# Write usage information
if ( ${#argv} == 0 ) then
  echo 'Wrong format. Exit now...'
  echo
  echo 'USER GUIDE:'
  echo './startf.sh startdate ofile "@ specifier" [optional arguments]'
  echo '***********************************************************'
  echo '*  [optional arguments]                                   *'
  echo '*   -  changet                                            *'
  echo '*   -  regionf {region file}                              *'
  echo '*   -  timecheck                                          *'
  echo '***********************************************************'
  exit 1
endif

# -----------------------------------------------------------------------------------------------------------------
# Handle input parameters
# -----------------------------------------------------------------------------------------------------------------

# Write title
echo 
echo '========================================================='
echo '       *** START OF PREPROCESSOR CREATE_STARTF ***              '
echo

# Extract arguments and split reference date
set refdate   = $1
set ofile     = $2
set specifier = `echo $3` 
shift
shift
shift

# Check whether specifier is a file startf.criterion (default) or user explicit
set flag_criterion = 'file'
set test = `echo ${specifier} | grep '@' | wc -c`
echo $test
echo $specifier
echo ${specifier}| cut -d " " -f 2
if ( "${test}" == "0" ) then 
    set filename = 'startf.criterion'
    set specifier = `cat ${filename}`
else
    set filename  = `echo ${specifier}| cut -d " " -f 2`
    if ( -f ${filename} ) then
       set specifier = `cat ${filename}`
    else
       echo " ERROR: cannot read criterion from file ${specifier}... Stop"
       exit 1
    endif
endif

echo "---- INPUT PARAMETERS ----------------------------------"
echo 
echo "Reference date        : ${refdate}"
echo "Specifier             : [from file ${filename}]"
echo "Output file           : ${ofile}"
echo

# Handle optional arguments
set changet   = 'false'
set regionf   = 'regionf'
set timecheck = '.false.'

while ( $#argv > 0 )

  switch ( $argv[1] )

   case -t
     set tvfile = $argv[2]
     echo "tvfile                -> ${tvfile} (user defined)"
     shift;
   breaksw

   case -changet
     set changet = 'true'
     echo "changet               -> true (user defined)"
   breaksw

   case -timecheck
     set timecheck = '.true.'
     echo "timecheck               -> yes (user defined)"
   breaksw

   case -regionf
     set regionf = $argv[2]
     echo "regionf                -> ${regionf} (user defined)"
     shift;
   breaksw

   endsw
 
   shift;

end

# No change of times necessary if no check requested
if ( "${timecheck}" == "no" ) then
   set  changet = 'false'
endif

# Split the reference date
set yyyy=`echo ${refdate}   | cut -c 1-4` 
set   mm=`echo ${refdate}   | cut -c 5-6` 
set   dd=`echo ${refdate}   | cut -c 7-8`
set   hh=`echo ${refdate}   | cut -c 10-11` 
set  min=`echo ${refdate}00 | cut -c 12-13` 

# Set base directories (run+prog)
set tradir=${PWD}

echo "check aaaa"
# Set program paths and filenames 
set parfile=${tradir}/namelist.startf
set crifile=${tradir}/${filename}

# Write status information
echo
echo '---- DIRECTORIES AND PROGRAMS ---------------------------'
echo    
echo "PROGRAM CREATE_STARTF : startf"
echo "PARAMETER file        : ${parfile}"
echo "CRITERION file        : ${crifile}"
echo "RUN directory         : ${tradir}"
echo

# -----------------------------------------------------------------------------------------------------------------
# Set the primary and scecondary data files (necessary for interpolation if intermediate reference date)
# -----------------------------------------------------------------------------------------------------------------
echo 'check a'
# Find a first data file (if possible corresponding to reference date)
set file=P${yyyy}${mm}${dd}_${hh}
if ( ( -f ${file} ) && ( ${min} == 0 ) ) then
   set timeshift=0
   set date0=${yyyy}${mm}${dd}_${hh}
   set date1=${yyyy}${mm}${dd}_${hh}
   set pfile0=${file1}
   set pfile1=${file}
   goto label3
else
  set file=`ls P[0-9_]*[0-9] | head -1 | sed -e 's/@//'`
endif

# Determine time increment (in hours) between data files
set date1=`echo $file | cut -c 2-12`
set n=`ls P[0-9_]*[0-9] | grep -n $date1 | awk -F: '{print $1}'`
@ n ++
set date2=`ls P[0-9_]*[0-9] | head -$n | tail -1 | cut -c 2-12`
set timeinc=`./gettidiff $date2 $date1`
endif
if ( $timeinc == 0 ) then
    echo " ERROR: cannot set the time increment between input files ***"
    exit 1
endif

# Search the first file to use
set timeshift=
foreach i ( P????????_?? )

  set date0 = `echo $i | cut -c 2-12`
  set td1  = `./gettidiff ${yyyy}${mm}${dd}_${hh} ${date0}`
  echo 'check 1'
  if ( ( $td1 >= 0 ) && ( $td1 < $timeinc ) )  then
      set timeshift=$td1
      set pfile0=P${date0}
      goto label2
  endif

end

# Check if no P files are available for the specified time period
if ( $timeshift == "" ) then
  echo " ERROR: no data files available for the specified reference date"
  exit 1
endif

# Everything is fine so far: proceed
label2:

# Set the next date and check its availability
if ( ( ${timeshift} != 0 ) || ( ${min} > 0 ) ) then
    set date1=`./newtime $date0 $timeinc`
    if ( ! -f P${date1} ) then
       echo " ERROR: file with primary data is missing for $date1"
       exit 1
    else
       set pfile1=P${date1}
    endif
else
    set date1=${date0}
    set pfile1=${pfile0}
endif

# Set the final timeshift 
if ( ${min} != 00 ) then
    set timeshift=${timeshift}.${min}
endif

# Everything is fine!
label3:

# Write status information
echo '---- DATA FILES -----------------------------------------'
echo    
echo "Primary files                         : ${pfile0}"
echo "                                      : ${pfile1}"
echo "Timeshift to first data file (hh.mm)  : ${timeshift}"
echo "Time increment of data files          : ${timeinc}"
echo

# --------------------------------------------------------------------------------------------------------------
# Create the start positions (without selection)
# -----------------------------------------------------------------------------------------------------------------

# Set times relative to the reference date
if ( "${changet}" == "true" ) then
  echo '---- CHANGE TIMES ON DATA FILES  ------------------------'
  echo   
  ./changet.sh ${refdate} ${pfile0}
  ./changet.sh ${refdate} ${pfile1}
endif

# Write parameters to parameter file and create the starting positions
\rm -f ${parfile}

echo '&share'			   >! ${parfile}
echo '   year         = '${yyyy}','   >> ${parfile}
echo '   month        = '${mm}','     >> ${parfile}
echo '   day          = '${dd}','     >> ${parfile}
echo '   hour         = '${hh}','     >> ${parfile}
echo '   minute       = '${min}','    >> ${parfile}
echo '   time_range   = 00,'         >> ${parfile}
echo '/'			   >> ${parfile}
echo				   >> ${parfile}
echo '&input'			   >> ${parfile}
echo '   file0        = '\"${pfile0}\"','	>> ${parfile}
echo '   file1        = '\"${pfile1}\"','	>> ${parfile}
echo '   regionf      = '\"${regionf}\"','	>> ${parfile}
echo '   timeshift    = '${timeshift}','        >> ${parfile}   
echo '   timeinc      = '${timeinc}','		>> ${parfile}
echo '/'					>> ${parfile}
echo						>> ${parfile}
echo '&output'					>> ${parfile}

echo '   ofile        = '\"${ofile}\"','         >> ${parfile}
echo '   ! appendix: .1 (.ls) = mode 1 ascii, sorted by trajectory'		>> ${parfile}
echo '   !           .2 (.ti) = mode 2 ascii, sorted by time'			>> ${parfile}
echo '   !           .3 (.du) = mode 3 fortran (unformatted)'			>> ${parfile}
echo '   !           .4 (.nc) = mode 4 IVE netcdf (for compatibility reason)'	>> ${parfile}
echo '   !           .-1 = mode -1'						>> ${parfile}
echo '/'									>> ${parfile}
echo				>> ${parfile}
echo '&params'			>> ${parfile}
# Analyse the specifier and append to parameter file
cat  ${filename}				 >> ${parfile}
echo '/'                                                                        >> ${parfile}

# Write flag for no time check
echo '   timecheck    = '${timecheck}','    >> ${parfile}
echo '/'					>> ${parfile}

# Write title
echo 
echo '       *** END OF PREPROCESSOR CREATE_STARTF ***'        
echo '========================================================='
echo

exit 0 
  


