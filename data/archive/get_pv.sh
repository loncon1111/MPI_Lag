#!/bin/bash
#
# usage: ./rename.sh 20170502 20170701

if [ $# = 0 ]||[ $# = 1 ]; then
   echo "./rename.sh 20170502 20170701"
   exit 0
else

   indir=/work/users/lanphuong/For_Hoa/lagranto/run/input/
   dadir=/work/users/lanphuong/For_Hoa/lagranto/run

   syr=`echo ${1} | cut -c1-4`
   smo=`echo ${1} | cut -c5-6`
   sda=`echo ${1} | cut -c7-8`

   eyr=`echo ${2} | cut -c1-4`
   emo=`echo ${2} | cut -c5-6`
   eda=`echo ${2} | cut -c7-8`

   d1=`date -d $1 +%s`
   d2=`date -d $2 +%s`
  
   datedif=$(( ( $d2 - $d1 ) / (24*3600) ))
   echo $datedif

   sdate="$syr/$smo/$sda"
   for (( idate=0; idate<=$datedif; idate++ )); do
       ida=`date --date="$sdate+${idate}day" +"%d"`
       iyr=`date --date="$sdate+${idate}day" +"%Y"`
       imo=`date --date="$sdate+${idate}day" +"%m"`
       for ihr in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23; do
           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/t_${iyr}.nc temp_${iyr}${imo}${ida}_${ihr}_t.nc
           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/u_${iyr}.nc temp_${iyr}${imo}${ida}_${ihr}_u.nc
           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/v_${iyr}.nc temp_${iyr}${imo}${ida}_${ihr}_v.nc
           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/w_${iyr}.nc temp_${iyr}${imo}${ida}_${ihr}_w.nc
           cdo -f nc -t ecmwf merge temp_* an${iyr}${imo}${ida}_${ihr}_tuvw

           rm temp_*.nc
           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/q_${iyr}.nc an${iyr}${imo}${ida}_${ihr}_q
#           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/${iyr}_ps.nc an${iyr}${imo}${ida}_${ihr}_ps
           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/potential_vorticity_${iyr}.nc an${iyr}${imo}${ida}_${ihr}_pv
           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/vo_${iyr}.nc an${iyr}${imo}${ida}_${ihr}_vo

           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/specific_cloud_ice_water_content_${iyr}.nc an${iyr}${imo}${ida}_${ihr}_sp_cloud

           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/specific_cloud_liquid_water_content_${iyr}.nc an${iyr}${imo}${ida}_${ihr}_sp_cloud_wc

           cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/specific_rain_water_content_${iyr}.nc an${iyr}${imo}${ida}_${ihr}_sp_rain_wc

          cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/z_${iyr}.nc an${iyr}${imo}${ida}_${ihr}_z
          cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 ${iyr}/d_${iyr}.nc an${iyr}${imo}${ida}_${ihr}_d

           #cdo -f nc -t ecmwf -seldate,${iyr}-${imo}-${ida}T${ihr}:00:00 -select,name=lnsp ${dadir}/heat_example_ml.nc an${iyr}${imo}${ida}_${ihr}_ps
       done
   done

fi
