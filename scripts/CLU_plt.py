import cartopy.crs as ccrs
import cartopy.feature as cfeature
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import metpy.calc as mpcalc
import numpy as np
import xarray as xr
import pandas as pd
from datetime import datetime, timedelta
import netCDF4
import cartopy.crs as ccrs
import cartopy.feature as cfeature
from matplotlib.offsetbox import AnchoredText

import shapely.geometry as geometry
from descartes import PolygonPatch
import cc3d


def next_time(startdate,days=0,hours=0,minutes=0,seconds=0):
    """Find next time for calculation"""
    sdate = datetime.strptime(startdate, '%Y%m%d_%H')
    date = sdate + timedelta(days=days, hours=hours)
    date = datetime.strftime(date, '%Y%m%d_%H')
    return date

def duration(startdate,enddate):
    """Time duration in hours"""
    sdate = datetime.strptime(startdate, '%Y%m%d_%H')
    edate = datetime.strptime(enddate  , '%Y%m%d_%H')
    delta = edate - sdate
#     if isinstance(delta, np.timedelta64):
#         return delta.astype(timedelta).total_hours() / 60.
    return delta.days, delta.seconds

dat_core = pd.read_csv("data/best_track.csv")

sdate = "20170712_00"
edate = "20170730_18"

dur_day, dur_sec = duration(sdate,edate)

dur_hr = dur_sec // (60*60) + dur_day * 24

print(dur_hr)
ndate = dur_hr // 6

print(ndate)

rv_date = sdate

for icase in range(ndate+1):
    # Forward
    f_fw = netCDF4.Dataset('data/forward/vort_%s.nc' %rv_date)
    print(f_fw)
    print(f_fw.variables.keys())

    f_vort = f_fw.variables['relvort']

    # Backward
    f_bw = netCDF4.Dataset('data/backward/vort_%s.nc' %rv_date)
    print(f_bw)
    print(f_bw.variables.keys())

    b_vort = f_bw.variables['relvort']

    lons = f_bw.variables['longitude']
    lats = f_bw.variables['latitude']
    levs = f_bw.variables['level'][:]

    levs = levs[:-1]
    print(levs)

    nlev = levs.shape[0]
    nlon = lons.shape[0]
    nlat = lats.shape[0]
    print(nlon,nlat,nlev)
    print(lons)

    # Make sure if forward and backward vars fit to each other
    print(np.shape(f_vort))
    print(np.shape(b_vort))

    vort = f_vort[:,:,:] + b_vort[:,:,:]

    # Core
    dat_core['time']

    # require = dat_core['time'] == next_time()
    index = dat_core[dat_core['time'] == '%s' %rv_date].index.values
    print(index)
    # dat_core.latitude
    latc = dat_core["latitude"][index].to_numpy()
    lonc = dat_core.longitude[index].to_numpy()
    typc = dat_core.type[index].to_numpy()

    for icore in range(len(index)):
        print(latc[icore])

    # 
    columns = 2
    rows = nlev // columns
    rows += nlev % columns

    x_tick_labels = [u'95\N{DEGREE SIGN}E', u'105\N{DEGREE SIGN}E',
                    u'115\N{DEGREE SIGN}E', u'125\N{DEGREE SIGN}E',
                    u'135\N{DEGREE SIGN}E', u'145\N{DEGREE SIGN}E']
    y_tick_labels = [u'0\N{DEGREE SIGN}', u'5\N{DEGREE SIGN}N',
                    u'10\N{DEGREE SIGN}N', u'15\N{DEGREE SIGN}N',
                    u'20\N{DEGREE SIGN}N', u'25\N{DEGREE SIGN}N',
                    u'30\N{DEGREE SIGN}N']


    # Start our new variables
    a = vort*1e4



    def mask_array(value):
        mask = a[:,:,:]>value
        maskdat = mask.astype(int) # convert booleans to 0/1
        return maskdat

    max_val = np.nanmax(a)
    print(max_val)
    values = np.arange(0,max_val,0.1)
    print(len(values))

    for ival,val in enumerate(values):

        reg = mask_array(val)

        #
        labels_out = cc3d.connected_components(reg[:,:,:],connectivity = 18)


        #
        SMALL_THRESHOLD = 9
        cull_count = 0

        no_empty = []
        for j in range(1,np.max(labels_out)+1):
            for ilev in range(labels_out.shape[0]):
                whrows, whcols = np.where(labels_out[ilev,:,:] == j)
                count = len(whrows)
                if count <= SMALL_THRESHOLD:
                    # discard clusters that are too small
                    cull_count +=1

                    labels_out[ilev,whrows,whcols] = 0
                    # TODO: remap labels
                else:
                    if j not in no_empty:
                        no_empty.append(j)
                    print(j)

        print(no_empty)

        if len(no_empty) != 0:
    
            clevs = no_empty
            fig = plt.figure(figsize=(18,30))
            for irow,ilev in enumerate(levs):
                ax = fig.add_subplot(rows, columns, irow + 1, projection = ccrs.PlateCarree())
   
                #cf = ax.contourf( lons[:],lats[:],labels_out[irow,:,:],
                #                     clevs,
                #         cmap=plt.cm.Spectral_r, extend='both', transform = ccrs.PlateCarree() )
                cs = ax.contour(lons[:],lats[:],labels_out[irow,:,:],levels=clevs,colors='black')
                cs.monochrome = True


                ax.set_ylim(0,30)
                ax.set_yticks([0, 5, 10, 15, 20, 25, 30])
                ax.set_yticklabels(y_tick_labels,fontsize=18)
                ax.set_xticks([95, 105, 115, 125, 135, 145])
                ax.set_xticklabels(x_tick_labels,fontsize=18)
                ax.grid(linestyle='dotted', linewidth=3,color='black')
                for axis in ['top','bottom','left','right']:
                    ax.spines[axis].set_linewidth(2)

            # Add geographical features
#             axes[irow].set_extent([0, 20, 95, 125], ccrs.PlateCarree())
                ax.add_feature(cfeature.COASTLINE.with_scale('10m'))
                ax.add_feature(cfeature.LAKES.with_scale('10m'),
                                color = 'black', linewidths=0.05)
                ax.add_feature(cfeature.BORDERS.with_scale('10m'),
                                linestyle='-',  color = 'black',
                                linewidths=0.05)

                anchored_text = AnchoredText("%i hPa" % ilev, loc=2,
                                        prop=dict(fontweight="bold",fontsize=18))

                ax.add_artist(anchored_text)

                for icore in range(len(index)):
                    print(lonc[icore],latc[icore])
                    print(lonc[icore] < lons[-1] and latc[icore] < lats[-1] and latc[icore] > lats[0])
                    if lonc[icore] < lons[-1] and latc[icore] < lats[-1] and latc[icore] > lats[0]:

                        ax.plot( lonc[icore], latc[icore], marker = 'X', color = 'darkcyan',
                            markeredgewidth=1, markeredgecolor="black",markersize=15)


            fig.tight_layout(h_pad=None, w_pad=None)
            fig.subplots_adjust(right=0.8)
            #cbar_ax =  fig.add_axes([0.85, 0.15, 0.05, 0.7])
            #cbar = fig.colorbar(cf, orientation='vertical', pad=0.04, aspect=10,
            #                   extendrect = True, cax=cbar_ax)


            # fig.suptitle('%s' %rv_date,fontsize=20,y=0.98)
            # Tweak spacing between subplots to prevent labels from overlapping
            plt.subplots_adjust(hspace=0.05,wspace=0.2)

            plt.savefig('clu_%05.2f_%s.png' %(val, rv_date), dpi=300)


        # Flush
        labels_out = None
        reg = None


    # Flush
    f_fw = None; b_fw = None
    nlev = None; nlon = None; nlat = None
    lons = None; lats = None; levs = None
    index = None; lonc = None; latc = None; typc = None

    rv_date = next_time(startdate=rv_date, hours = 6)
