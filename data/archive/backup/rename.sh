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
#       for ihr in 00 06 12 18; do
       for ihr in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23; do
   #        cdo -f nc -t ecmwf -select,name=z,T,U,V,OMEGA,Q,ciwc,crwc,clwc,vo,pv,u10,v10,msl P${iyr}${imo}${ida}_${ihr} \
   #               P${iyr}${imo}${ida}_${ihr}_1
            mv P${iyr}${imo}${ida}_${ihr}_1 P${iyr}${imo}${ida}_${ihr}
       done
   done

fi
