#!/bin/bash
years="2013 2011 2012 2019"

for iyear in $years; do
    sed -i "s/2010/$iyear/g" download_sfc.py
    echo "Downloading surface $iyear"
    python download_sfc.py
    echo "Download surface $iyear succesfully"
    sed -i "s/$iyear/2010/g" download_sfc.py
done
