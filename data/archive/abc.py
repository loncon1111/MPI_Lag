#!/usr/bin/env python
import cdsapi
 
c = cdsapi.Client()
#params = ["w"]
#params = ["w","d","vo","q","u","v","t"]
params = ["w","q","u","v","t","potential_vorticity"]

#params = ["potential_vorticity"]
#params = ["q"]
#years = [2013]

for year in range(2019,2020):
#for year in years:
   for param in params:

      c.retrieve('reanalysis-era5-pressure-levels', {
        'variable'      : param,
        'pressure_level': '1000/900/850/800/750/700/650/600/500/400/300/200',
 	'stream'	: 'oper',
        'product_type'  : 'reanalysis',
        'date'          : str(year)+'-01-01/'+str(year)+'-12-31',
        'time'          : '00/to/23/by/6',
        #'time'          : '00/to/23/by/1',
        'format'        : 'netcdf' # Supported format: grib and netcdf. Default: grib
      #}, param+'_'+str(year)+'.nc')
      }, param+'_'+str(year)+'.nc')
