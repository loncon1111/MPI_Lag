# immport libraries
from descartes import PolygonPatch
import numpy as np
import pandas as pd
import netCDF4


# ## Define some functions

# In[15]:


def ow_lag(date):
    # Read from forward
    f_fw = netCDF4.Dataset('data/forward/new_%s.nc' %date, 'r')
    f_bw = netCDF4.Dataset('data/backward/new_%s.nc' %date, 'r')

    f_ieig1 = f_fw.variables['ieig1']
    f_reig1 = f_fw.variables['reig1']
    b_ieig1 = f_bw.variables['ieig1']
    b_reig1 = f_bw.variables['reig1']

    ieig1 = f_ieig1[:,:,:] + b_ieig1[:,:,:]
    reig1 = f_reig1[:,:,:] + b_reig1[:,:,:]

    ow_lag = ieig1[:,:,:] - reig1[:,:,:]
    # Flush
    f_fw = None; f_bw = None; f_ieig1 = None; f_reig1 = None; b_ieig1 = None; b_reig1 = None
    ieig1 = None; reig1 = None

    return ow_lag


