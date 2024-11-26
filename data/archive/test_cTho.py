import cdsapi

c = cdsapi.Client()

c.retrieve(
    'reanalysis-era5-single-levels',
    {
        'product_type':'reanalysis',
        'variable':'10m_u_component_of_wind',
        'area'    :'52.00/2.00/40.00/20.00',
        'year':'2019',
        'month':'01',
        'day':'01',
        'time':'00:00',
        'format':'netcdf'
    },
    'download.nc')
