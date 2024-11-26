
#!/usr/bin/env python
import cdsapi
import certifi
 
c = cdsapi.Client()

c.retrieve('reanalysis-era5-single-levels', {    # do not change this!
    'product_type': 'reanalysis',
    'variable': [ '10m_u_component_of_wind', 
                 '10m_v_component_of_wind', 'mean_sea_level_pressure',
                 'total_precipitation'],
    'date'        : '2017-07-14/2017-08-05',
    'time'        : '00/to/23/by/6',
    'format'      : 'netcdf'
    }, '2017_ps.nc')
