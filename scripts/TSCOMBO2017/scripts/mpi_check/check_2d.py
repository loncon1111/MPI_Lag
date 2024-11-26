#!/usr/bin/env python
# coding: utf-8

# In[14]:

# Set environments

import os
import sys
os.environ['PROJ_LIB'] = "/work/apps/gnu_4.8.5/anaconda3/5.1.0/share/proj/"

import alphashape
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
import pyproj
from functools import partial
from datetime import datetime, timedelta

## Define some functions
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

# Define projection to calculate area
proj = partial(pyproj.transform, pyproj.Proj(init='epsg:4326'),
               pyproj.Proj(init='epsg:3857'))

# Date
sdate = "20170729_00"
edate = "20170729_12"

dur_day, dur_sec = duration(sdate,edate)

dur_hr = dur_sec // (60*60) + dur_day * 24

ndate = dur_hr // 6

date = sdate

for icase in range(ndate+1):
    # Read label csv file
    df = pd.read_csv("../CLU_VORT/labels/labellist2d_%s.csv" %date)
    df.columns = [col.replace(' ', '') for col in df.columns]
    df["hull_centx"] = np.nan
    df["hull_centy"] = np.nan
    df["hull_area"]  = np.nan
    df["min_owlag"]  = np.nan

    ref_levs = df.groupby(["level"]).count().index.to_numpy()

    # Read netCDF cluster file
    f = netCDF4.Dataset("../CLU_VORT/nc_files/cc2d_%s.nc" %date,"r")

    lons = f.variables['longitude'][:]
    lats = f.variables['latitude'][:]
    levs = f.variables['level'][:]

    nlev = levs.shape[0]
    nlon = lons.shape[0]
    nlat = lats.shape[0]
    print(nlon,nlat,nlev)

    # Get OW_LAG
    ow_lag = ow_lag(date)

    nrows = len(df)
    for irow in range(nrows):
        print(irow)

        lev   = df["level"].iloc[irow]
        label = df["label"].iloc[irow]
        val   = df["value"].iloc[irow]        

        ilev  = np.where(levs == lev)[0]

        # Get variable
        var = np.squeeze(f.variables["labels_cc2d_%05.2f" %val][ilev,:,:])
        ilat,ilon = np.where(var == label)
        
        latind_1 = lats[ilat]
        lonind_1 = lons[ilon]
         
        points = np.zeros([len(latind_1),2])
        for ipt in range(len(latind_1)):
            points[ipt,0] = lonind_1[ipt]
            points[ipt,1] = latind_1[ipt]

        print(len(latind_1))

        check_equal_lon = all(elem == points[0,0] for elem in points[:,0])
        check_equal_lat = all(elem == points[0,1] for elem in points[:,1])

        if check_equal_lon == False and check_equal_lat == False :
            # get convex hull
            alpha = alphashape.optimizealpha(points)
            hull = alphashape.alphashape(points, alpha)

            # get hull boundaries, area, centroi,...
            hull_lons, hull_lats = hull.exterior.coords.xy
            hull_ilon = np.searchsorted(lons,hull_lons)
            hull_ilat = np.searchsorted(lats,hull_lats)
          
            # Output centroids and area
            hull_centx = hull.centroid.x
            hull_centy = hull.centroid.y

            s = shape(hull)
            hull_area = transform(proj, s).area

            try:
                min_owlag = np.nanmin([x for x in ow_lag[ilev,hull_ilat,hull_ilon] if x != 0])
            except ValueError:   # raised if min_owlag is empty
                min_owlag = 0

            #print(hull_centx, hull_centy)

            # Write this information into pandas
            df["hull_centx"].iloc[irow] = hull_centx
            df["hull_centy"].iloc[irow] = hull_centy
            df["hull_area"].iloc[irow] = hull_area
            df["min_owlag"].iloc[irow] = min_owlag

            # Get var
        # Flush
        lev,label,val = 3*[None]
        ilev,ilon,ilat = 3*[None]; var = None; latind_1,lonind_1 = 2*[None]
        check_equal_lon, check_equal_at = 2*[None]
        alpha = None; hull = None; points = None 
        hull_lons,hull_lats,hull_ilon,hull_ilat = 4*[None]
        hull_centx,hull_centy,hull_area,s = 4*[None]

    df.to_csv("label_list2d_%s.csv" %date)

    # Flush
    df = None; ref_levs = None; f = None
    lons,lats,levs = 3*[None]; nlon,nlat,nlev = 3*[None]
    ow_lag = None
