#coding=utf-8
# %%
# import libraries
import os
import sys
os.environ['PROJ_LIB'] = "/work/apps/gnu_4.8.5/anaconda3/5.1.0/share/proj/"

from matplotlib.colors import ListedColormap
import netCDF4
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np
import cartopy.feature as cfeature
import cartopy.crs as ccrs
import cc3d
import alphashape
from function_copy import get_trajs
from function_copy import copy_trajs

# %%
rv_date = "20170721_06"
## Phase 2: Open netCDF file of label
#ncdir = "/Users/daohoa/Desktop/my-notebook/LAGRANGIAN/TSCOMBO2017/CLU_VORT/nc_files/"
#cdir  = "/Users/daohoa/Desktop/my-notebook/LAGRANGIAN/TSCOMBO2017/results/"
#data_dir =  "/Users/daohoa/Desktop/my-notebook/LAGRANGIAN/TSCOMBO2017/data/"
#
cdir  = "/work/users/hmo/truongnm/MPI_LCS/scripts/TSCOMBO2017/scripts/mpi_check/"
data_dir = "/work/users/hmo/truongnm/MPI_LCS/scripts/TSCOMBO2017/data/"

# %%
# EXCEL file of first
df = pd.read_excel(cdir + "label_first_%s.xlsx" %rv_date, engine = 'openpyxl')
# %%
zeta_fw = netCDF4.Dataset(data_dir + "forward/" + "vort_%s.nc" %rv_date)
zeta_bw = netCDF4.Dataset(data_dir + "backward/" + "vort_%s.nc" %rv_date)

rlev  = zeta_bw.variables["level"][:]
rlon  = zeta_bw.variables["longitude"][:]
rlat  = zeta_bw.variables["latitude"][:]

ow_fw = netCDF4.Dataset(data_dir + "forward/" + "new_%s.nc" %rv_date)
ow_bw = netCDF4.Dataset(data_dir + "backward/" + "new_%s.nc" %rv_date)

#%%
# open reference startf storing
src_nc = netCDF4.Dataset(data_dir + "backward/startf_%s.4" %rv_date, mode = "r")

# %%
# calculating
vort_fw = zeta_fw.variables["relvort"][:]
vort_bw = zeta_bw.variables["relvort"][:]
ieig1_fw = ow_fw.variables["ieig1"][:]
ieig1_bw = ow_bw.variables["ieig1"][:]
reig1_fw = ow_fw.variables["reig1"][:]
reig1_bw = ow_bw.variables["reig1"][:]

vort    = vort_fw + vort_bw

ieig1 = ieig1_fw + ieig1_bw
reig1 = reig1_fw + reig1_bw
ow_lag = ieig1 - reig1
print(np.shape(vort),len(rlon),len(rlat))
# %%
nrows = len(df)
print(nrows)

# %%
df
# %%
clt_arr = np.zeros(np.shape(vort))
bnd_arr = np.zeros(np.shape(vort))

for irow in range(nrows):
#for irow in range(1):
    print(irow)
    level = df.level.iloc[irow]
    value = df.value.iloc[irow]

    ilev  = np.squeeze(np.where(rlev == level))

    mask = vort[ilev,:,:]*1e4 >= value
    labels_out = cc3d.connected_components(mask,connectivity = 6)
    plt.figure()
    plt.contourf(rlon,rlat,labels_out)
    plt.contour(rlon,rlat,ow_lag[ilev,:,:],levels=[0],colors = 'red')
    cull = range(1,np.max(labels_out) + 1)
    cull_counts = []
    for icull in cull:
        cull_x,cull_y = np.where(labels_out == icull)
        cull_counts.append(len(cull_x))

    cull_max = cull[np.argmax(cull_counts)]

    ilat,ilon = np.where(labels_out == cull_max)
    npoints = len(ilon)
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
