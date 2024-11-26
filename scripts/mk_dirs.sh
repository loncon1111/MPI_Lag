#!/bin/bash

startdate="20170730_00"
enddate="20170730_06"

idate=${startdate}
while [ "$idate" != "$enddate" ]; do

    if [ ! -f $idate ]; then
       mkdir $idate
    fi

    mv clu_lag_*_${idate}.png ${idate}
 
    idate=`./newtime $idate 6`
done

