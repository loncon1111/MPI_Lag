'set grads off'
'set display color white'
'set mpdset hires'

*'set parea 0.6 9.5 0.2 8'
*'set parea 0.55 9.25 0.3 8.15'
'set parea 0.75 9.25 0.3 7.95'

'set xlopts 1 4 0.16'
'set ylopts 1 4 0.16'

'set xlint 5'
'set ylint 5'

'sdfopen P20170729_12'
'sdfopen P20170729_13'

*'set lon 230 255'
*'set lat 8.5 24.5'
'set lon 224 254'
'set lat 10 32'

'pi=3.14159'
'dtr=pi/180'
'a=6.37122e6'

'dy=cdiff(lat,y)*dtr*a'
'dx=cdiff(lon,x)*dtr*a*cos(lat*dtr)'

'dpsdx1=cdiff(msl.1(t=1),x)/dx'
'dpsdy1=cdiff(msl.1(t=1),y)/dy'

'define padv1=(-u10.1(t=1)*dpsdx1-v10.1(t=1)*dpsdy1)'

'dpsdx2=cdiff(msl.2(t=1),x)/dx'
'dpsdy2=cdiff(msl.2(t=1),y)/dy'

'define padv2=(-u10.2(t=1)*dpsdx2-v10.2(t=1)*dpsdy2)'

'define padv=(padv1+padv2)*1800.'

'set rgb  16    0    0  255'
'set rgb  17   55   55  255'
'set rgb  18  110  110  255'
'set rgb  19  165  165  255'
'set rgb  20  220  220  255'

'set rgb  21  255  220  220'
'set rgb  22  255  165  165'
'set rgb  23  255  110  110'
'set rgb  24  255   55   55'
'set rgb  25  255    0    0'

'set gxout shaded'
'set clevs -120 -90 -60 -30 -5 5 30 60 90 120'
'set ccols 16 17 18 19 20 0 21 22 23 24 25'

*'color -0.005 0.005 0.0005 -kind blue->white->white->red'
'd padv'
*'xcbar 10. 10.5'
'xcbar 9.5 10.25'

'define dp=msl.2(t=1)-msl.1(t=1)'
'set gxout contour'
'set cstyle 1'
'set ccolor 1'
*'set cint 15'
'set cint 40'
'set cmin 1'
'd dp'

'set cstyle 2'
'set ccolor 1'
*'set cint 15'
'set cint 40'
'set cmax -1'
'd dp'

*'set gxout vector'
*'set ccolor 1'
*'set arrowhead 0.08'
*'d skip(qubl,6,6);qvbl'
'basemap L 0 1 L'
'close 2'
'close 1'

'set strsiz 0.25'
'draw string 1 8.1 ADV_P & DP 29/07/17 12UTC'

*'gxprint filename.png x1024 y768'
'printim ADVP_DP_29-07-17_12.png x1024 y768'  
