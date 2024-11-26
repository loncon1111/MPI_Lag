#!/usr/bin/env python
# coding: utf-8

# In[14]:

# Set environments

import os
import sys
os.environ['PROJ_LIB'] = "/work/apps/gnu_4.8.5/anaconda3/5.1.0/share/proj/"

# immport libraries
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
import cc3d
from shapely.geometry import shape
from shapely.ops import transform
import pyproj
from functools import partial
from datetime import datetime, timedelta


# ## Define some functions

# In[15]:


def ow_lag(date):
    # Read from forward
    f_fw = netCDF4.Dataset('../data/forward/new_%s.nc' %date, 'r')
    f_bw = netCDF4.Dataset('../data/backward/new_%s.nc' %date, 'r')
    
    f_ieig1 = f_fw.variables['ieig1']
    f_reig1 = f_fw.variables['reig1']
    b_ieig1 = f_bw.variables['ieig1']
    b_reig1 = f_bw.variables['reig1']
    
    ieig1 = f_ieig1[:,:,:] + b_ieig1[:,:,:]
    reig1 = f_reig1[:,:,:] + b_reig1[:,:,:]

    ow_lag = ieig1[:,:,:] - b_ieig1[:,:,:]
    # Flush
    f_fw = None; f_bw = None; f_ieig1 = None; f_reig1 = None; b_ieig1 = None; b_reig1 = None
    ieig1 = None; reig1 = None
    
    return ow_lag


# In[16]:


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


# Define projection to calculate area
proj = partial(pyproj.transform, pyproj.Proj(init='epsg:4326'),
               pyproj.Proj(init='epsg:3857'))


# Date
sdate = "20170713_06"
edate = "20170731_12"

dur_day, dur_sec = duration(sdate,edate)

dur_hr = dur_sec // (60*60) + dur_day * 24

ndate = dur_hr // 6

date = sdate


# In[18]:


for icase in range(ndate+1):
    df = pd.read_csv("../CLU_VORT/labels/labellist_%s.csv" %date, header = None, names = ["values","npoints","labels"])

    npoints = df.groupby(["values"])["npoints"].count().to_numpy()
    values = df.groupby(["values"]).count().index.to_numpy()

    # Check 

    # Check file 1
    f = netCDF4.Dataset("../CLU_VORT/nc_files/cc3d_%s.nc" %date,"r")

    lons = f.variables['longitude'][:]
    lats = f.variables['latitude'][:]
    levs = f.variables['level'][:]

    nlev = levs.shape[0]
    nlon = lons.shape[0]
    nlat = lats.shape[0]

    # Get OW_LAG
    ow_lag = ow_lag(date)

   # Sample var
    for ilev,lev in enumerate(levs):

        for ival,val in enumerate(values):
            label = df[df["values"] == val]["labels"].to_numpy()
            var = f.variables["labels_cc3d_%05.2f" %val][ilev,:,:]

            for ilabel in label:
                lab_arr = (var == ilabel).astype('int')
                labels_out = cc3d.connected_components(lab_arr,connectivity = 6)

####
                for j in range(1,np.max(labels_out) + 1):
        
                    check_equal_lon = False
                    check_equal_lat = False
            
                    ilat,ilon = np.where(labels_out == j)
      
                    SMALL_THRESHOLD = 6
                    count = len(ilat)
                    if count > SMALL_THRESHOLD:
                        latind_1 = lats[ilat]
                        lonind_1 = lons[ilon]
##

                        points = np.zeros([len(latind_1),2])
                        for ipt in range(len(latind_1)):
                            points[ipt,0] = lonind_1[ipt]
                            points[ipt,1] = latind_1[ipt]
                    
#                         fig, ax = plt.subplots()
#                         ax.scatter(points[:,0], points[:,1], color='red')
                        
                        check_equal_lon = all(elem == points[0,0] for elem in points[:,0])
                        check_equal_lat = all(elem == points[0,1] for elem in points[:,1])
        
                        if check_equal_lon == False and check_equal_lat == False :
                            alpha = 0.95 * alphashape.optimizealpha(points,max_iterations=100)
    
                            hull = alphashape.alphashape(points, alpha)
                            hull_lons,hull_lats = hull.exterior.coords.xy
                            hull_ilon = np.searchsorted(lons,hull_lons)
                            hull_ilat = np.searchsorted(lats,hull_lats)
                    
                        

#
                            # Output centroids and area
                            hull_centx = hull.centroid.x
                            hull_centy = hull.centroid.y
#             hull_area = hull.area # in square degrees
#
                            s = shape(hull)
                            hull_area = transform(proj, s).area
#
                            try:
                                min_owlag = np.nanmin([x for x in ow_lag[ilev,hull_ilat,hull_ilon] if x != 0])
                            except ValueError:   # raised if min_owlag is empty
                                min_owlag = 0            

                            print('{:>5.0f}\t{:>5.3f}\t{:>5d}\t{:<5d}\t{:>7.3f}\t{:>7.3f}\t{:>20.3f}\t{:e}'
                                  .format(lev,val,ilabel,j,hull_centx,hull_centy,hull_area,min_owlag)
                                    ,file = open(r"cluster_%s.dat" %date, "a+"))
            
#
                        # Flush
                        latind_1 = None; lonind_1 = None; ilat = None; ilon = None
                        points = None; hull = None; hull_pts = None; alpha = None;
                        hull_lons = None; hull_lats = None; hull_ilon = None; hull_ilat = None
                        fig,ax = 2*[None]; min_owlag = None; s = None; hull_area = None
        
                # Flush
                lab_arr = None; labels_out = None

####

            # Flush
            val = None; label = None

    # Flush
    df = None; npoints = None; values = None
    lons,lats,levs = 3*[None]; ow_lag = None


    date = next_time(startdate=date, hours = 6)


# In[ ]:





# In[ ]:




