#!/usr/bin/env python
import cdsapi
 
c = cdsapi.Client()
params = ["w","q","z","u","t","v","d","vo","potential_vorticity",'specific_cloud_ice_water_content', 'specific_cloud_liquid_water_content', 'specific_rain_water_content']
#years = range(2010,2013)

#params = ["z"]

years = [2017]

for year in years:
#for year in years:
   for param in params:

      c.retrieve('reanalysis-era5-pressure-levels', {
        'variable'      : param,
        #'pressure_level': '1000/950/925/900/875/850/825/800/775/750/700/650/600/550/500/450/400/350/300/250/225/200/175/150/125/100',
        'pressure_level': '1000/950/925/900/875/850/825/800/775/750/700/650/600/550/500/450/400/350/300/250/225/200/175/150/125/100/70/50/30/20/10/7/5/3/2/1',
 	'stream'	: 'oper',
        'product_type'  : 'reanalysis',
        #'date'          : str(year)+'-01-01/'+str(year)+'-12-31',
        'date'          : str(year)+'-07-22/'+str(year)+'-07-23',
        #'time'          : '00/to/23/by/6',
        #'year'          : str(year),
        #'month'         : '09',
        #'day'           : [
        #            '05','06','07','08','09','10','11','12','13','14','15','16','17'
        #                  ],
        'time'          : '00/to/23/by/1',

        'format'        : 'netcdf' # Supported format: grib and netcdf. Default: grib
      }, param+'_'+str(year)+'.nc')
