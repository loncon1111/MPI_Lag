
#!/usr/bin/env python
import cdsapi
import certifi
 
c = cdsapi.Client()

c.retrieve('reanalysis-era5-single-levels', {    # do not change this!
    'product_type': 'reanalysis',
    'variable'    : 'surface_pressure',
    'date'        : '2010-01-01/2010-12-31',
    'time'        : '00/to/23/by/6',
    'format'      : 'netcdf'
    }, '2010_ps.nc')
