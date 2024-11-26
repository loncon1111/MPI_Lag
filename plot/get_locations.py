
import netCDF4
import pandas as pd
import numpy as np

# Read netCDF4 connected component labeling 
date="20170714_06"

ccfile = netCDF4.Dataset("cc3d_%s.nc" %date)

thres = 2.40

var = ccfile.variables["labels_cc3d_%05.2f" %thres][:,:,:]

lons = ccfile.variables['longitude'][:]
lats = ccfile.variables['latitude'][:]
levs = ccfile.variables['level'][:]

nlon = len(lons)
nlat = len(lats)
nlev = len(levs)

ind = np.argwhere(var > 0)


lepts = levs[ind[:,0]]  # because it is netCDF4 variable
lopts = lons[ind[:,2]]
lapts = lats[ind[:,1]]


# Read reference lsl file
cdir = "/work/users/cefd_kc0912/WRF-LETKF/for_vic/for_paper_1/results/TSCOMBO2017/clustering_OWLAG/orig_files/"
opt  = "backward"

ncfile = netCDF4.Dataset(cdir + opt + "/lsl_%s.4" %date)

 

# READ ALL VARIABLES

time      = ncfile.variables["time"]
longitude = ncfile.variables["lon"]
latitude  = ncfile.variables["lat"]
level     = ncfile.variables["p"]
U         = ncfile.variables["U"]
V         = ncfile.variables["V"]
Q         = ncfile.variables["Q"]
T         = ncfile.variables["T"]
TH        = ncfile.variables["TH"]
THE       = ncfile.variables["THE"]
DIST      = ncfile.variables["DIST"]
DIST0     = ncfile.variables["DIST0"]
HEAD      = ncfile.variables["HEAD"]
DANGLE    = ncfile.variables["DANGLE"]
RELVORT   = ncfile.variables["RELVORT"]
R_EIGVAL1 = ncfile.variables["R_EIGVAL1"]
R_EIGVAL2 = ncfile.variables["R_EIGVAL2"]
I_EIGVAL1 = ncfile.variables["I_EIGVAL1"]
I_EIGVAL2 = ncfile.variables["I_EIGVAL2"]
BASEDATE  = ncfile.variables["BASEDATE"]
print(longitude.shape)
print(BASEDATE)


arr = np.array([],dtype = int)
for ipt,rpts in enumerate(lapts):
    new = np.where((longitude[0,0,0,:]==lopts[ipt])&(latitude[0,0,0,:]==lapts[ipt])&(level[0,0,0,:]==lepts[ipt]))
    arr = np.append(arr, new)

# Writing new netCDF4 file
oufile = netCDF4.Dataset(opt + "/lsl_filter_%s.4" %date, mode='w', format='NETCDF3_CLASSIC')

# copy global attributes all at once via dictionary
oufile.setncatts(ncfile.__dict__)

# copy dimensions except for dimx_lon
oufile.createDimension('dimx_lon',arr.shape[0])
for name, dimension in ncfile.dimensions.items():
    if name != 'dimx_lon':
        print(name, dimension)
        oufile.createDimension(name, len(dimension))
#                                (len(dimension)) if not dimension.isunlimited() else None)
    
# copy all file data except for the excluded

# time


for name, variable in ncfile.variables.items():
#         print(name, variable, variable.dimensions)
    oufile.createVariable(name, variable.datatype, variable.dimensions)
    print(oufile[name])
    print(variable)
    
    if (name != 'time') & (name != 'BASEDATE'):
        for iarr,rarr in enumerate(arr):
            oufile[name][:,:,:,iarr] = ncfile[name][:,:,:,rarr]
    elif name != 'time':
        oufile[name][:] = ncfile[name][:]
        

    # copy variable attributes all at once via dictionary
    oufile[name].setncatts(ncfile[name].__dict__)
    
print(oufile.variables['BASEDATE'])
# close the Dataset
ncfile.close(); oufile.close(); print('Dataset is closed!')
