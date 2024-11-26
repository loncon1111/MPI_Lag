#coding=utf-8
# %%
import netCDF4
import numpy as np

def copy_trajs(src,dst,traj_coords):

    # get source lon, lat ,p, level
    lon_src = np.squeeze(src.variables["lon"][:])
    lat_src = np.squeeze(src.variables["lat"][:])
    lev_src = np.squeeze(src.variables["p"][:])

    traj_bnd = np.array([],dtype = int)
    
    arr = np.array([],dtype = int)
    for ipt in range(traj_coords.ntra):
        new = np.where((lon_src == traj_coords.lons[ipt]) \
                      &(lat_src == traj_coords.lats[ipt])\
                      &(lev_src == traj_coords.levs[ipt]))
        arr = np.append(arr, new)
        # Flush
        new = None

    # Copy global attributes all at once via dict
    dst.setncatts(src.__dict__)

    # copy dimensions except for dimx_lon
    dst.createDimension('dimx_lon',arr.shape[0])
    for name, dimension in src.dimensions.items():
        if name != 'dimx_lon':
            dst.createDimension(name, len(dimension) if not dimension.isunlimited() else None)

    for name, variable in src.variables.items():
        dst.createVariable(name, variable.datatype, variable.dimensions)
    
        if (name != 'time') & (name != 'BASEDATE'):
            for iarr,rarr in enumerate(arr):
                dst[name][:,:,:,iarr] = src[name][:,:,:,rarr]
        else:
            dst[name][:] = src[name][:]

        # copy variable attributes all at once via dictionary
        dst[name].setncatts(src[name].__dict__)

    # close netCDF files
    dst.close()

class get_trajs():
    def __init__(self,mask_array,rx,ry,rz):
        iz,iy,ix = np.where(mask_array == 1)
        self.lons = np.take(rx,ix)
        self.lats = np.take(ry,iy)
        self.levs = np.take(rz,iz)
        self.ntra = len(self.lons)

# %%
