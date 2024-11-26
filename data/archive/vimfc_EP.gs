'set grads off'
'set display color white'
'set mpdset hires'

*'set parea 0.6 9.5 0.2 8'
'set parea 0.55 9.25 0.3 8.15'

'set xlopts 1 4 0.16'
'set ylopts 1 4 0.16'
'set xlint 10'
'set ylint 10'

'sdfopen P20170728_00'
'set lon 220 265'
'set lat 5 35'

'define qs=q(lev=1000)+(q(lev=1000)-q(lev=950))/(z(lev=950)-z(lev=1000))*z(lev=1000)'

'define qubl=((u10*qs+u(lev=950)*q(lev=950))*(msl-95000)/2.+ave(u*q,lev=950,lev=925)*2500.+ave(u*q,lev=925,lev=900)*2500.+ave(u*q,lev=900,lev=875)*2500.+ave(u*q,lev=875,lev=850)*2500.)/9.81'

'define qvbl=((v10*qs+v(lev=950)*q(lev=950))*(msl-95000)/2.+ave(v*q,lev=950,lev=925)*2500.+ave(v*q,lev=925,lev=900)*2500.+ave(v*q,lev=900,lev=875)*2500.+ave(v*q,lev=875,lev=850)*2500.)/9.81'

'define qu=qubl+(ave(u*q,lev=850,lev=825)*2500.+ave(u*q,lev=825,lev=800)*2500.+ave(u*q,lev=800,lev=775)*2500.+ave(u*q,lev=775,lev=750)*2500.+ave(u*q,lev=750,lev=700)*5000.+ave(u*q,lev=700,lev=650)*5000.+ave(u*q,lev=650,lev=600)*5000.+ave(u*q,lev=600,lev=550)*5000.+ave(u*q,lev=550,lev=500)*5000.+ave(u*q,lev=500,lev=450)*5000.+ave(u*q,lev=450,lev=400)*5000.+ave(u*q,lev=400,lev=350)*5000.+ave(u*q,lev=350,lev=300)*5000.+ave(u*q,lev=300,lev=250)*5000.+ave(u*q,lev=250,lev=225)*2500.+ave(u*q,lev=225,lev=200)*2500.+ave(u*q,lev=200,lev=175)*2500.+ave(u*q,lev=175,lev=150)*2500.+ave(u*q,lev=150,lev=125)*2500.+ave(u*q,lev=125,lev=100)*2500.)/9.81'

'define qv=qvbl+(ave(v*q,lev=850,lev=825)*2500.+ave(v*q,lev=825,lev=800)*2500.+ave(v*q,lev=800,lev=775)*2500.+ave(v*q,lev=775,lev=750)*2500.+ave(v*q,lev=750,lev=700)*5000.+ave(v*q,lev=700,lev=650)*5000.+ave(v*q,lev=650,lev=600)*5000.+ave(v*q,lev=600,lev=550)*5000.+ave(v*q,lev=550,lev=500)*5000.+ave(v*q,lev=500,lev=450)*5000.+ave(v*q,lev=450,lev=400)*5000.+ave(v*q,lev=400,lev=350)*5000.+ave(v*q,lev=350,lev=300)*5000.+ave(v*q,lev=300,lev=250)*5000.+ave(v*q,lev=250,lev=225)*2500.+ave(v*q,lev=225,lev=200)*2500.+ave(v*q,lev=200,lev=175)*2500.+ave(v*q,lev=175,lev=150)*2500.+ave(v*q,lev=150,lev=125)*2500.+ave(v*q,lev=125,lev=100)*2500.)/9.81'

'vimfc=-hdivg(qu,qv)'

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
'set clevs -0.006 -0.0045 -0.003 -0.0015 -0.0001 0.0001 0.0015 0.003 0.0045 0.006'
'set ccols 16 17 18 19 20 0 21 22 23 24 25'

*'color -0.005 0.005 0.0005 -kind blue->white->white->red'
'd vimfc'
*'xcbar 10 10.5'
'xcbar 9.5 10.25'

'set gxout contour'
'd msl'

*'set z 3'
'set gxout vector'
'set ccolor 1'
'set arrowhead 0.08'
'd skip(qu,6,6);qv'
'basemap L 0 1 L'
'close 1'

'set strsiz 0.25'
'draw string 1 8 VIMFC 28/07/17 00UTC'

*'gxprint filename.png x1024 y768'
'printim VIMFC_28-07-17_00.png x1024 y768'  
