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

#### MPI environments ####
from mpi4py import MPI

#### MPI functions ####
def para_range(n1, n2, nprocs, irank):

    iwork1 = (n2 - n1) // nprocs
    iwork2 = (n2 - n1) % nprocs
    ista   = irank * iwork1 + n1 + min(irank, iwork2)
    iend   = ista + iwork1
    if iwork2 > irank:
        iend = iend + 1
    return int(ista),int(iend)

def definetype(field_names, field_dtypes):
    num = nrows
    dtypes = list(zip(field_names, field_dtypes)) # zip to connect 2 arrays gradually
    a   = np.zeros(num, dtype = dtypes)

    struct_size = a.nbytes // num # // the floor division // rounds the result down to nearest int

    print(struct_size)
    offsets = [ a.dtype.fields[field][1] for field in field_names]

    mpitype_dict = {np.int32:MPI.INT,np.float64:MPI.DOUBLE,np.float32:MPI.REAL}
    field_mpitypes = [mpitype_dict[dtype] for dtype in field_dtypes]

    structtype = MPI.Datatype.Create_struct([1]*len(field_names), offsets, field_mpitypes)
    structtype = structtype.Create_resized(0, struct_size)
    structtype.Commit()
    return structtype


## Define some functions
def ow_lag(date):
    # Read from forward
    f_fw = netCDF4.Dataset('../../data/forward/new_%s.nc' %date, 'r')
    f_bw = netCDF4.Dataset('../../data/backward/new_%s.nc' %date, 'r')

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







########## initialize MPI communications ###########
comm = MPI.COMM_WORLD
myrank = comm.Get_rank()
nprocs = comm.Get_size()

assert nprocs > 1

# Define projection to calculate area
proj = partial(pyproj.transform, pyproj.Proj(init='epsg:4326'),
               pyproj.Proj(init='epsg:3857'))

# Date
sdate = "20170720_12"
edate = "20170720_12"

dur_day, dur_sec = duration(sdate,edate)

dur_hr = dur_sec // (60*60) + dur_day * 24

ndate = dur_hr // 6

date = sdate


ref_lev = 850   ## reference level


for icase in range(ndate+1):
    # Read label csv file
    df = pd.read_csv("../../CLU_VORT/labels/labellist2d_%s_%s.csv" %(date,int(ref_lev)))
#    df = pd.read_csv("labellist2d_%s.csv" %date)

    df.columns = [col.replace(' ', '') for col in df.columns]
    df["hull_centx"] = np.nan
    df["hull_centy"] = np.nan
    df["hull_area"]  = np.nan
    df["min_owlag"]  = np.nan

    ref_levs = df.groupby(["level"]).count().index.to_numpy()

    # Read netCDF cluster file
    f = netCDF4.Dataset("../../CLU_VORT/nc_files/cc2d_%s.nc" %date,"r")

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


    print(nrows)
    ######### Loop over all trajectories in MPI #############
    ista, iend = para_range(0,nrows,nprocs,myrank)
    iprev      = myrank - 1
    inext      = myrank + 1
    #if myrank == 0: iprev = MPI_PROC_NULL
    if myrank == 0: print(nrows)
    print ("I am process %d of %d running from %d to %d" %(myrank, nprocs, ista, iend))


    hull_field_names = ["hull_centx","hull_centy","hull_area","min_owlag"]
    #hull_field_types = [np.float64  , np.float64 , np.float64, np.float64]
    hull_field_types = 4*[np.float64]

    mytype = definetype(hull_field_names, hull_field_types)
    hull_data = np.zeros(nrows, dtype=(list(zip(hull_field_names, hull_field_types))))


    for irow in range(ista,iend):
        lev   = df["level"].iloc[irow]
        label = df["label"].iloc[irow]
        val   = df["value"].iloc[irow]        

        ilev  = np.where(levs == lev)[0]

        # Get variable
        var = np.squeeze(f.variables["labels_cc2d_%05.2f" %val][ilev,:,:])
        ilat,ilon = np.where(var == label)
        
        latind_1 = lats[ilat]
        lonind_1 = lons[ilon]

        npoints = len(latind_1)
         
        points = np.zeros([len(latind_1),2])
        for ipt in range(len(latind_1)):
            points[ipt,0] = lonind_1[ipt]
            points[ipt,1] = latind_1[ipt]

        check_equal_lon = all(elem == points[0,0] for elem in points[:,0])
        check_equal_lat = all(elem == points[0,1] for elem in points[:,1])


        ##### An example #######
        #hull_data[irow]["area"] = len(latind_1)

        if check_equal_lon == False and check_equal_lat == False and npoints < 1000 :
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

            print(myrank,irow,hull_centx, hull_centy)

            # Write this information into pandas
            hull_data[irow]["hull_centx"] = hull_centx
            hull_data[irow]["hull_centy"] = hull_centy
            hull_data[irow]["hull_area"] = hull_area
            hull_data[irow]["min_owlag"] = min_owlag

            # Get var
        # Flush
        lev,label,val = 3*[None]
        ilev,ilon,ilat = 3*[None]; var = None; latind_1,lonind_1 = 2*[None]
        check_equal_lon, check_equal_at, npoints = 3*[None]
        alpha = None; hull = None; points = None
        hull_lons,hull_lats,hull_ilon,hull_ilat = 4*[None]
        hull_centx,hull_centy,hull_area,s = 4*[None]


    ####### Send and receive in MPI ##########
    # intitiate rank communication for MPI
    ureq  = np.empty(nprocs,dtype = MPI.Request)
    tags  = np.arange(nprocs)

    if myrank == 0:
        data = np.zeros(nrows, dtype=(list(zip(hull_field_names, hull_field_types))))

        data[ista:iend] = hull_data[ista:iend-1]
        # gather updates from all cores (non-contiguous memory)
        for irank in range(1,nprocs):
            jsta, jend = para_range(0,nrows,nprocs,irank)
            comm.Recv([hull_data,mytype], source = irank)


            
            data[jsta:jend] = hull_data[jsta:jend]
            # Flush
            jsta,jend = 2*[None]

    else:
        comm.Send([hull_data,mytype], dest = 0)


#    df.to_csv("label_list2d_%s.csv" %date)
    if myrank == 0:
        df1 = pd.DataFrame(data)
        df1["level"] = df["level"]
        df1["label"] = df["label"]
        df1["value"] = df["value"]
        df1.to_csv("label_list2d_%s_%s.csv" %(date,int(ref_lev)))

    # Flush
    df = None; ref_levs = None; f = None
    lons,lats,levs = 3*[None]; nlon,nlat,nlev = 3*[None]
    ow_lag = None

    date = next_time(startdate=date, hours = 6)
