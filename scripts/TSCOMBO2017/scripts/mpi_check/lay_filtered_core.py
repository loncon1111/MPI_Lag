#coding=utf-8
# %%
# import libraries
import os
import sys
os.environ['PROJ_LIB'] = "/work/apps/gnu_4.8.5/anaconda3/5.1.0/share/proj/"

from matplotlib.colors import ListedColormap
import netCDF4
import pandas as pd
import numpy as np
import cc3d
import alphashape
from function_copy import get_trajs
from function_copy import copy_trajs

# %%
rv_date = "20170713_06"
## Phase 2: Open netCDF file of label
#ncdir = "/Users/daohoa/Desktop/my-notebook/LAGRANGIAN/TSCOMBO2017/CLU_VORT/nc_files/"
#cdir  = "/Users/daohoa/Desktop/my-notebook/LAGRANGIAN/TSCOMBO2017/results/"
#data_dir =  "/Users/daohoa/Desktop/my-notebook/LAGRANGIAN/TSCOMBO2017/data/"
#
cdir  = "/work/users/hmo/truongnm/MPI_LCS/scripts/TSCOMBO2017/scripts/mpi_check/"
data_dir = "/work/users/hmo/truongnm/MPI_LCS/scripts/TSCOMBO2017/data/"
ncdir = "/work/users/hmo/truongnm/MPI_LCS/scripts/TSCOMBO2017/CLU_VORT/nc_files/"

# %%
# EXCEL file of first
df = pd.read_excel(cdir + "label_first_%s.xlsx" %rv_date, engine = 'openpyxl')
# %%
label_nc = netCDF4.Dataset(ncdir + "cc2d_%s.nc" %rv_date)

rlev  = label_nc.variables["level"][:]
rlon  = label_nc.variables["longitude"][:]
rlat  = label_nc.variables["latitude"][:]

nshape = (len(rlev),len(rlat),len(rlon))

#%%
# open reference startf storing
src_nc = netCDF4.Dataset(data_dir + "backward/startf_%s.4" %rv_date, mode = "r")
# %%
nrows = len(df)
print(nrows)

# %%
df
# %%
clt_arr = np.zeros(nshape)
bnd_arr = np.zeros(nshape)
mask    = np.zeros(nshape,dtype=np.int)

for irow in range(nrows):
    print(irow)
    level = df.level.iloc[irow]
    value = df.value.iloc[irow]
    label = df.label.iloc[irow]

    ilev  = np.squeeze(np.where(rlev == level))

    var   = label_nc.variables["labels_cc2d_%05.2f" %value][ilev,:,:]

    latind,lonind = np.where( var == label )

    mask[ilev,latind,lonind] = 1

    #Flush
    level,value,ilev = 3*[None]



labels_out = cc3d.connected_components(mask,connectivity = 18)
cull = np.arange(1,np.max(labels_out) + 1,dtype=np.int)
cull_counts = []
print(cull)
for icull in cull:
    cull_x,cull_y,cull_z = np.where(labels_out == icull)
    cull_counts.append(len(cull_x))

cull_max = cull[np.argmax(cull_counts)]
nlev = np.shape(labels_out)[0]

for ilev in range(nlev):
    ilat,ilon = np.where(labels_out[ilev,:,:] == cull_max)

    npoints = len(ilon)
    if npoints != 0:
        points = np.zeros([npoints,2])
        points[:,0] = np.take(rlon,ilon)
        points[:,1] = np.take(rlat,ilat)   

        ##### First, stores the cluster ######
        clt_arr[ilev,ilat,ilon] = 1

        ##### Take the alphashape #####
        alpha = alphashape.optimizealpha(points,max_iterations = 10000)
        hull  = alphashape.alphashape(points, alpha)
        hull_lons,hull_lats = hull.exterior.coords.xy
        lonind = [np.squeeze(np.where(rlon == x)) for x in hull_lons]
        latind = [np.squeeze(np.where(rlat == x)) for x in hull_lats]
        bnd_arr[ilev,latind,lonind] = 1

    # Flush
    level, label, value = 3*[None]
    ilev,ilon,ilat = 3*[None]; points = None
    mask = None; hull,alpha = 2*[None]
# %%
bnd_coords = get_trajs(bnd_arr,rlon,rlat,rlev)
clt_coords = get_trajs(clt_arr,rlon,rlat,rlev)
# %%
# %%
####### Extract into files #########
src_nc  = netCDF4.Dataset(data_dir + "backward/startf_%s.4" %rv_date, mode = "r")
dst_bnd = netCDF4.Dataset(cdir     + "startf_%s_bnd.4" %rv_date, mode = "w", format = "NETCDF4")
dst_clt = netCDF4.Dataset(cdir     + "startf_%s_clt.4" %rv_date, mode = "w", format = "NETCDF4")

bnd = copy_trajs(src_nc,dst_bnd,bnd_coords)
clt = copy_trajs(src_nc,dst_clt,clt_coords)

# %%