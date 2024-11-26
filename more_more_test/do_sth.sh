#!/bin/bash


echo ""
echo " Make sure you have changed attributions in USER CHANGE sub-section!"
echo ""
echo " #################################### PREFACE ####################################"
echo " This script is made for you to run trajectories program for multiple sequential days."
echo " So make sure that you have enough datasets to convenient this procedure..."
echo " Also, your trajectories program must be installed successfully..."
echo " GOOD LUCK!"
echo " #################################################################################"
echo 
echo -n " LOADING: Pretend like we are loading for 3 seconds..."
sleep 1
echo '[OKAY]'

# --------------------------------------------
#                 USER CHANGE
# --------------------------------------------

# I need to run python in several steps
# make sure your python has netCDF4
# in cases you have to module load python, then do it right away
module load anaconda3/5.1.0_gnu_64 lapack/3.5.0_gnu_64 netcdf/4.3.3.1_gnu_64
module load mvapich2/2.3a_hydra_gnu_64 

workdir="/work/users/hmo/truongnm/MPI_LCS"

startdate="20170720_12"    # Format: YYYYMMDD_HH
enddate="20170723_00"      # Format: YYYYMMDD_HH
interval=1                 # Interval running hours
increment=6                # it is different
hh_total=-72		   # Total running hours 

# Set flags for arguments

flag_p='no'  		   # Flag for periodic
changet='yes'             # Flag for change time
timecheck='yes'

# Startf arguments
specifier='startf.criterion' # Criterion file, 'default' if you don't set
regionf='none'	 	   # Region file, 'none' if you don't set

# Caltra arguments
flag_j='yes'		# If a trajectoy crosses the lower boundary (topo), 
			# it is just raised a little and then is allowed to move on  
flag_i='no'

# Trace arguments
flag_v='default'        # set varlist file, default set 'default'
intmode='circle_avg'    # set interpolation mode
radius='200'            # if interpolation mode = something related to circle, set radius (km), if no
			# leave it ''

# File output name specifier
startfile='startf_date_SONCA_clt.4' # Note: 1/2/3/4... prefixes are different file type
calfile='ofile_date_SONCA_clt.4'	   # 1/2 prefixes are for txt, 4 is for NetCDF file (recommend)
trafile='lsl_date_SONCA_clt.4'      # You must have 'date' string inside your file output name
			   # So we can replace it with your date 
 

# --------------------------------------------
#             END OF USER CHANGE
# --------------------------------------------

# Linking all binaries to this directory
echo "--------------------------------------------"
echo
echo " BIN: Check if your program has been installed successfully..."

bins='datelist getmima gettidiff startf caltra trace getvars trainfo newtime'
arr=($bins)

echo
ls -ll ${workdir}/bin/*
echo

nfil=${#arr[@]}

for ((ifil = 0 ; ifil < nfil ; ifil++));do
    if [ -f ${workdir}/bin/${arr[$ifil]} ]; then 
       echo " BIN: ${arr[$ifil]} PASSED..."
    else
       echo " BIN: ${arr[$ifil]} MISSING. Exit now..."
       exit 0
    fi
done

echo
echo " BIN: All clear. Linking..."
ln -fs ${workdir}/bin/* .

echo 
echo "--------------------------------------------"
echo

# Calculate running days
#ncases=`gettidiff $startdate $enddate`
HRS_TOTAL=`./gettidiff $enddate $startdate`
echo " TIME: total number of hours is $HRS_TOTAL with interval is $interval hours"
ncases=$(echo -e "scale=0; $HRS_TOTAL/$increment+1" | bc)
ncases=${ncases#-}
echo " TIME: Number of cases is $ncases"
if [ $HRS_TOTAL -lt 0 ]; then
   echo " TIME: Don't worry, your direction is backward. We captured it..."
   interval=-$interval
fi

echo
echo "--------------------------------------------"
echo


echo
echo "--------------------------------------------"
echo

isdate=$startdate

for ((icase = 0 ; icase < ncases ; icase++));do
    echo " RUNNING: Running for case $icase"
    echo
    rm -f namelist*

    # Find startdate and enddate of each case
    iedate=`./newtime $isdate $hh_total`
    
    echo " RUNNING: Startdate is $isdate"
    echo " RUNNING: Enddate is $iedate"    

    echo
    echo "****** START PROGRAM ******"
    echo 
    echo " RUNNING STARTF...: "
    echo
    # Arguments for startf program
    if [ "${changet}" == "yes" ]; then
       stargs+='-changet '
    fi
    if [ "${timecheck}" == "yes" ]; then
       stargs+='-timecheck '
    fi
    if [ "${regionf}" != "none" ]; then
       stargs+='-regionf ${regionf} '
    fi
    echo " STARTF: Startf program argument is: $stargs"
    sfile=`echo ${startfile} | sed "s/date/${isdate}/g"`
    echo " STARTF: Output filename is: ${sfile}"
    echo " STARTF: Running program..."
 
    echo " STARTF: Creating 'namelist.startf'..."

    echo
    echo " RUNNING CALTRA"
    echo

    # Arguments for caltra program
    if [ "${flag_p}" == "yes" ]; then
       cargs+='-p '
    fi
    if [ "${changet}" == "yes" ]; then
       cargs+='-changet '
    fi
    if [ "${timecheck}" == "yes" ]; then
       cargs+='-timecheck '
    fi
    if [ "${flag_j}" == "yes" ]; then
       cargs+='-j '
    fi    

    echo " CALTRA: caltra program argument is: $stargs"
    cfile=`echo ${calfile} | sed "s/date/${isdate}/g"`
    echo " CALTRA: Output filename is: ${cfile}"
    echo " CALTRA: Running program..."

    echo " CALTRA: Creating 'namelist.caltra'..."
    echo " ./caltra.sh ${isdate} ${iedate} ${sfile} ${cfile} ${cargs} "
    ./caltra.sh ${isdate} ${iedate} ${sfile} ${cfile} ${cargs} > log.caltra.${isdate}
    if [ -f ./namelist.caltra ]; then
       echo " CALTRA: Successfully created namelist.caltra"
    else
       echo " CALTRA: Error in creating namelist.caltra. Read 'log.caltra.${isdate}' for more details."
       exit 3
    fi

    ./caltra >> log.caltra.${isdate}

    tail -n 3 log.caltra.${isdate}

    if [ -f ${cfile} ]; then
       echo " CALTRA: SUCCESS. Read 'log.caltra.${isdate}' for more details before jumping to next step."
    else
       echo " CALTRA: Error file is stored in 'log.caltra.${isdate}' file"
       exit 4
    fi

    echo
    echo " RUNNING TRACE"
    echo

    # Arguments for caltra program
    if [ "${flag_p}" == "yes" ]; then
       targs+='-p '
    fi
    if [ "${changet}" == "yes" ]; then
       targs+='-changet '
    fi
    if [ "${flag_i}" == "yes" ]; then
       targs+='-i '
    fi
    if [ "${flag_v}" != "default" ]; then
       targs+="-v ${flag_v}"
    fi
    if [ "${timecheck}" == "yes" ]; then
       targs+='-timecheck '
    fi
    targs+="-${intmode} ${radius}"
     

    echo " TRACE: trace program argument is: $ttargs"
    tfile=`echo ${trafile} | sed "s/date/${isdate}/g"`
    echo " TRACE: Output filename is: ${tfile}"
    echo " TRACE: Running program..."

    echo " TRACE: Creating 'namelist.trace'..."
    echo " ./trace.sh ${cfile} ${tfile} ${targs} "
    ./trace.sh ${cfile} ${tfile} ${targs} > log.trace.${isdate}
    if [ -f ./namelist.trace ]; then
       echo " TRACE: Successfully created namelist.trace"
    else
       echo " TRACE: Error in creating namelist.trace. Read 'log.trace.${isdate}' for more details."
       exit 5
    fi

    mpirun -np 8 ./trace >> log.trace.${isdate}

    #mpirun -np $SLURM_NTASKS ./trace
    #sbatch submit_trace.pbs --wait
##    JOB_ID=$(sbatch --parsable submit_trace.pbs)

##    sleep 10s

##    sentence="$(squeue -j $JOB_ID)"            # read job's slurm status
##    stringarray=($sentence)
##    jobstatus=(${stringarray[12]})            # isolate the status of job number jobid


##    while [ "$jobstatus" = "R" ]
##    do
##        sleep 30s
##        sentence="$(squeue -j $JOB_ID)"            # read job's slurm status
##        stringarray=($sentence)
##        jobstatus=(${stringarray[12]})            # isolate the status of job number jobid
##        echo $jobstatus

##    done

##    mv LAG_std.out log.trace.${isdate}

    #tail -n 3 log.trace.${isdate}   
 
    if [ -f ${tfile} ]; then
       echo " TRACE: SUCCESS. Read 'log.trace.${isdate}' for more details before jumping to next step."
    else
       echo " TRACE: Error file is stored in 'log.trace.${isdate}' file"
       exit 6
    fi

    # Before doing anything, make sure to change time in lsl file to exactly like in ofile
    # Due to my laziness I don't really know how it fucked up like this
    sed -i "s/ofile_date.4/${cfile}/g" time_convert.py
    sed -i "s/lsl_date.4/${tfile}/g" time_convert.py
    python time_convert.py
    sed -i "s/${cfile}/ofile_date.4/g" time_convert.py
    sed -i "s/${tfile}/lsl_date.4/g" time_convert.py 

    # Clean arguments
    stargs=''
    cargs=''
    targs=''

    echo
    echo "--------------------------------------------"
    echo
    isdate=`./newtime $isdate $increment`
done

#for iday in {21..23}; do
    

#./caltra > caltra.log
#./trace > trace.log
