#!/usr/bin/env python
# coding: utf-8

# In[14]:

# Set environments

import os
import sys

# immport libraries
import shapely.geometry as geometry
from shapely.geometry import Polygon
from descartes import PolygonPatch
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import netCDF4
import cartopy.crs as ccrs
import cartopy.feature as cfeature
from shapely.geometry import shape
from shapely.ops import transform
from datetime import datetime, timedelta





def next_time(startdate,days=0,hours=0,minutes=0,seconds=0):
    """Find next time for calculation"""
    sdate = datetime.strptime(startdate, '%Y%m%d_%H')
    date = sdate + timedelta(days=days, hours=hours)
    date = datetime.strftime(date, '%Y%m%d_%H')
    return date

def duration(startdate,enddate):
    """Time duration in hours"""
    sdate = datetime.strptime(startdate, '%Y%m%d_%H')
    edate = datetime.strptime(enddate  , '%Y%m%d_%H')
    delta = edate - sdate
#     if isinstance(delta, np.timedelta64):
#         return delta.astype(timedelta).total_hours() / 60.
    return delta.days, delta.seconds


# In[17]:

# Date
sdate = "20170715_00"
edate = "20170715_00"

dur_day, dur_sec = duration(sdate,edate)

dur_hr = dur_sec // (60*60) + dur_day * 24

ndate = dur_hr // 6

date = sdate

change_vars = ["lon","lat","p"]

for icase in range(ndate+1):
    src = netCDF4.Dataset("backup/ofile_%s.4" %date, mode = "r")
    dst = netCDF4.Dataset("startf_%s.4" %date, mode = "r+")

    for ivar,vname in enumerate(change_vars):
        dst[vname][0,:,:,:] = src[vname][-1,:,:,:]

    #print(src["BASEDATE"][0])
    int_mins = src["BASEDATE"][0,0,0,5]
    print(np.int32(int_mins))

    # Change BASEDATE
    base_date = datetime.strptime(date,'%Y%m%d_%H')+ timedelta(minutes=int(int_mins)) 
    dst["BASEDATE"][0,0,0,0] = np.float64(datetime.strftime(base_date,'%Y'))
    dst["BASEDATE"][0,0,0,1] = np.float64(datetime.strftime(base_date,'%m'))
    dst["BASEDATE"][0,0,0,2] = np.float64(datetime.strftime(base_date,'%d'))
    dst["BASEDATE"][0,0,0,3] = np.float64(datetime.strftime(base_date,'%H'))  

    src.close()
    dst.close()

    os.rename("startf_%s.4" %date,"results/startf_%s.4" %datetime.strftime(base_date,'%Y%m%d_%H')) 

    date = next_time(startdate=date, hours = 6)


