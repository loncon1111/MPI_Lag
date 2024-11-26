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

sdate = "20170720_12"
edate = "20170720_12"

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

    levs = levs
    print(levs)

    nlev = levs.shape[0]
    nlon = lons.shape[0]
    nlat = lats.shape[0]
    print(nlon,nlat,nlev)
    print(lons)

    bnd_ilons = [0,nlon-1]
    bnd_ilats = [0,nlat-1]

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

    # First, define a new file
    dst = netCDF4.Dataset('cc2d_%s.nc' %rv_date, "w", format = "NETCDF4" )
    src  = f_fw

    # Copy dimensions
    for dname, the_dim in src.dimensions.items():
        #print(dname, len(the_dim))
        dst.createDimension(dname, len(the_dim) if not the_dim.isunlimited() else None)

    # Copy some variables
    usl_var = ["reig1","ieig1","vort"]
    for v_name, varin in src.variables.items():
        if v_name not in usl_var:
            outVar = dst.createVariable(v_name, varin.dtype, varin.dimensions)
            #print(varin.datatype)

            outVar[:] = varin[:]

            # copy the variable attributes
            outVar.setncatts({k: varin.getncattr(k) for k in varin.ncattrs()})

    # Start our new variables
    a = vort*1e4



    def mask_array(b,value):
        mask = b[:,:]>value
        maskdat = mask.astype(int) # convert booleans to 0/1
        return maskdat

    max_val = np.nanmax(a)
    print("max_val %s" %max_val)
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

        no_empty = []
        for j in range(1,np.max(labels_out)+1):
            for ilev in range(labels_out.shape[0]):
                whrows, whcols = np.where(labels_out[ilev,:,:] == j)

                con_row = np.isin(whrows,bnd_ilats)
                con_col = np.isin(whcols,bnd_ilons)

                print(con_row)
                print(con_row.any())                

                count = len(whrows)
                #if count <= SMALL_THRESHOLD:
                if count <= SMALL_THRESHOLD or con_row.any() or con_col.any():
                    # discard clusters that are too small
                    cull_count +=1

                    labels_out[ilev,whrows,whcols] = 0
                    # TODO: remap labels
                else:
                    np.append(no_empty, j)
                # Flush
                con_row,con_col,whrows,whcols = 4*[None]

        print(cull_count)

        # Create new array, remove all cull
        #labels_stack = list(filter(lambda a: a != [0.,0.,0.], labels_out))
        #print(np.shape(labels_out))
        #print(np.shape(labels_stack))
  
        in_cc2d = dst.createVariable('labels_cc2d_%05.2f' %val, labels_out.dtype, ('level','latitude','longitude'))
        in_cc2d.long_name = 'Connected Component Labelling %5.2f' %val

        print(in_cc2d)
        print(np.shape(labels_out))

        in_cc2d[:,:,:] = labels_out[:,:,:]

        # Flush
        in_cc2d = None; labels_out = None
        reg = None

    src.close()

  

    src = None ; dst = None

    # Flush
    f_fw = None; b_fw = None
    nlev = None; nlon = None; nlat = None
    lons = None; lats = None; levs = None
    index = None; lonc = None; latc = None; typc = None

    rv_date = next_time(startdate=rv_date, hours = 6)
