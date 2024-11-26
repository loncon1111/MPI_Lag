import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import metpy.calc as mpcalc
import numpy as np
import xarray as xr
import pandas as pd
import datetime
import netCDF4

# get dataset
ds = xr.open_dataset('lsl_outtrace1.nc')

lons = np.squeeze(ds['lon'].isel(time=0))
lats = np.squeeze(ds['lat'].isel(time=0))
levs = np.squeeze(ds['p'].isel(time=0))

ntim = len(np.squeeze(ds['time'])[1:])

print(np.squeeze(ds['time'])[1:])

r_ev1 = np.squeeze(ds['R_EIGVAL1'][1:,:,:,:]).sum(axis=0)
r_ev2 = np.squeeze(ds['R_EIGVAL2'][1:,:,:,:]).sum(axis=0)
i_ev1 = np.squeeze(ds['I_EIGVAL1'][1:,:,:,:])
i_ev2 = np.squeeze(ds['I_EIGVAL2'][1:,:,:,:])
rvort = np.squeeze(ds['RELVORT'][1:,:,:,:])
bdate = ds['BASEDATE'].isel(time=0)

ntra = r_ev1.shape[0]
#ntim = rvort.shape[0]

print(bdate)

ref_lat = np.arange(0. , 30.25, 0.25)
ref_lon = np.arange(90.,145.25, 0.25)
ref_lev = np.array([1000., 900., 850., 800., 750., 700.,650.,600., 500.,400., 300.])

nlon = ref_lon.shape[0]
nlat = ref_lat.shape[0]
nlev = ref_lev.shape[0]

#print(rvort)
print(ntra)
print(ntim)
#print( [[i_ev1[i][j] for i in range(0,ntim)] for j in range(4,5)])
#print( [[rvort[i][j] for i in range(0,ntim)] for j in range(4,5)])
spa_ieig1 = [[i_ev1[i][j] if rvort[i][j] > 0 else -i_ev1[i][j] for i in range(0,ntim)] for j in range(0,ntra)]
#print(spa_ieig1)
si_ev1 = np.sum(spa_ieig1,axis=1)
#print(si_ev1)
srvort = np.sum(rvort,axis=0)

#basedate = np.full([nlon,nlat,nlev],np.nan)
rvor = np.empty([nlev,nlat,nlon])
#print(srvort.

#for itra in range(0,ntra):
#    for ilev,rlev in enumerate(ref_lev):
#        if levs[itra] == rlev:
#            for ilat in range(0,nlat):
#                if lats[itra] == ref_lat[ilat]:
#                    for ilon in range(0,nlon):
#                        if lons[itra] == ref_lon[ilon]:
#                            rvor[ilev,ilat,ilon] = srvort[itra]

for ilev,rlev in enumerate(ref_lev):
    for ilat,rlat in enumerate(ref_lat):
        for ilon,rlon in enumerate(ref_lon):
            itra_lst = np.where((lats == rlat)&(lons == rlon)&(levs == rlev))
            #print(np.squeeze(r_ev1[itra].values))
            #print(np.squeeze(itra))
            #itra = filter(None,itra_lst)
            itra = np.squeeze(itra_lst)
            if itra.size > 0:
                rvor[ilev,ilat,ilon] = srvort[itra]
            itra = None
            itra_lst = None

#print(nlev,nlat,nlon,rvor.shape)
print(rvor[0,:,:])

# Writing netCDF4
ncfile = netCDF4.Dataset('vort.nc',mode='w',format='NETCDF4_CLASSIC')
print(ncfile)

# Creating Dimensions
lat_dim = ncfile.createDimension('latitude', nlat)  # latitude axis
lon_dim = ncfile.createDimension('longitude', nlon) # longitude axis
lev_dim = ncfile.createDimension('level', nlev)     # level axis
time_dim = ncfile.createDimension('time', None)     # unlimited axis (can be appended to)
dimx_BASEDATE = ncfile.createDimension('dimx_BASEDATE',6)

for dim in ncfile.dimensions.items():
    print(dim)

# Creating Attributes
ncfile.title='OWLAG value data'
print(ncfile.title)

ncfile.subtitle="My model data subtitle"
ncfile.anything="Write anything"

# Creating Variables
lat = ncfile.createVariable('latitude',np.float32, ('latitude',))
lat.units = 'degrees_north'
lat.long_name = 'latitude'

lon = ncfile.createVariable('longitude',np.float32, ('longitude',))
lon.units = 'degrees_east'
lon.long_name = 'longitude'

lev = ncfile.createVariable('level',np.float32, ('level',))
lev.units = 'hPa'
lev.long_name = 'level'

time = ncfile.createVariable('time',np.float64, ('time',))
time.units = 'hours since 1800-01-01'
time.long_name = 'Forward/Backward tracking time'

relvort = ncfile.createVariable('relvort',rvor.dtype,('level','latitude','longitude'))
relvort.long_name = 'Relative Vorticity'

BASEDATE = ncfile.createVariable('BASEDATE',np.int32,('dimx_BASEDATE',))

# Writing data

lat[:] = ref_lat
lon[:] = ref_lon
lev[:] = ref_lev

relvort[:,:,:] = rvor
BASEDATE[:]  = bdate

# first print the Dataset object to see what we've got
print(ncfile)

# close the Dataset
ncfile.close(); print('Dataset is closed!')
