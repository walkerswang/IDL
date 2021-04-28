GEOPACK_RECALC,2017,251,0,0,0

mmlat=fltarr(361,181)
mmlon=fltarr(361,181)
glat=fltarr(361,181)
glon=fltarr(361,181)

for lat=-90, 90 do begin
  for lon=0, 360 do begin
    theta=(90-lat)/180.0*!pi
    phi=lon/180.0*!pi
    r=1
    GEOPACK_SPHCAR, r, theta, phi, geox, geoy, geoz, /to_rect
    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
    GEOPACK_SPHCAR, magx, magy, magz, r, theta , phi, /to_sphere
    glat[lon,lat+90]=lat
    glon[lon,lat+90]=lon
    mmlat[lon,lat+90]=90-theta/!pi*180
    mmlon[lon,lat+90]=phi/!pi*180
  endfor
endfor

;save, glat, glon, mmlat, mmlon, filename='/Users/wzihan/IDLWorkspace/Default/mag_grid.sav'
end