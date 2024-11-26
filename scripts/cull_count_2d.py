import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import metpy.calc as mpcalc
import numpy as np
import xarray as xr
import pandas as pd
from datetime import datetime, timedelta
import netCDF4
import cartopy.crs as ccrs
import cartopy.feature as cfeature
from matplotlib.offsetbox import AnchoredText

import shapely.geometry as geometry
from descartes import PolygonPatch
import cc3d


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

dat_core = pd.read_csv("data/best_track.csv")

sdate = "20170713_12"
edate = "20170729_00"

dur_day, dur_sec = duration(sdate,edate)

dur_hr = dur_sec // (60*60) + dur_day * 24

print(dur_hr)
ndate = dur_hr // 6

print(ndate)

rv_date = sdate

for icase in range(ndate+1):
    # Forward
    f_fw = netCDF4.Dataset('data/forward/vort_%s.nc' %rv_date)
    print(f_fw)
    print(f_fw.variables.keys())

    f_vort = f_fw.variables['relvort']

    # Backward
    f_bw = netCDF4.Dataset('data/backward/vort_%s.nc' %rv_date)
    print(f_bw)
    print(f_bw.variables.keys())

    b_vort = f_bw.variables['relvort']

    lons = f_bw.variables['longitude']
    lats = f_bw.variables['latitude']
    levs = f_bw.variables['level'][:]

    levs = levs[:]
    print(levs)

    nlev = levs.shape[0]
    nlon = lons.shape[0]
    nlat = lats.shape[0]
    print(nlon,nlat,nlev)
    print(lons)

    # Make sure if forward and backward vars fit to each other
    print(np.shape(f_vort))
    print(np.shape(b_vort))

    vort = f_vort[:,:,:] + b_vort[:,:,:]

    # Core
    dat_core['time']

    # require = dat_core['time'] == next_time()
    index = dat_core[dat_core['time'] == '%s' %rv_date].index.values
    print(index)
    # dat_core.latitude
    latc = dat_core["latitude"][index].to_numpy()
    lonc = dat_core.longitude[index].to_numpy()
    typc = dat_core.type[index].to_numpy()

    for icore in range(len(index)):
        print(latc[icore])

    # Start our new variables
    a = vort*1e4

    #df = pd.DataFrame(columns = ["values","counts","labels_list"])
    df  = open("labellist2d_%s.csv" %rv_date, "w+")
    df.write("level,value,number_of_labels,label \n")
    #print(df)

    def mask_array(b,value):
        mask = b[:,:]>value
        maskdat = mask.astype(int) # convert booleans to 0/1
        return maskdat

    max_val = np.nanmax(a)
    values = np.arange(0,max_val,0.1)

    for ival,val in enumerate(values):

        reg = np.zeros([nlev,nlat,nlon],dtype='int64')
        labels_out = np.zeros([nlev,nlat,nlon],dtype='int64')

        for ilev,lev in enumerate(levs):
        #
            reg[ilev,:,:] = mask_array(a[ilev,:,:],val)
            labels_out[ilev,:,:] = cc3d.connected_components(reg[ilev,:,:], \
                                   connectivity = 6)    # suggested for 2D

            #print("labels_out %s" %labels_out)
            # Flush


        #
        SMALL_THRESHOLD = 9
        cull_count = 0


        for ilev in range(labels_out.shape[0]):
            no_empty = []
            for j in range(1,np.max(labels_out[ilev,:,:])+1):

                whrows, whcols = np.where(labels_out[ilev,:,:] == j)
                count = len(whrows)
                if count <= SMALL_THRESHOLD:
                    # discard clusters that are too small
                    cull_count +=1

                    labels_out[ilev,whrows,whcols] = 0
                    # TODO: remap labels
                else:
                    if j not in no_empty:
                        no_empty.append(j)
                    #np.append(no_empty, j)

            nempt_count = len(no_empty)
     
            for ilabel,label in enumerate(no_empty):
                df.write( "%5.3f,%5.2f,%s,%s  \n" %(levs[ilev],val,nempt_count,label))


            # Flush
            nempt_count = None

        labels_out = None;nempt_count = None; no_empty = None
        reg = None

        print(df)
    # Flush
    f_fw = None; b_fw = None
    nlev = None; nlon = None; nlat = None
    lons = None; lats = None; levs = None
    index = None; lonc = None; latc = None; typc = None

    rv_date = next_time(startdate=rv_date, hours = 6)
