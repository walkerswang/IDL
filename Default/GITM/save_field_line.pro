wc=ask('root folder:',wc)
root=ask('figure root:',root)
ion=ask('ion file names:',ion)

ionFILELIST = FINDFILE(wc+ion)
nfiles = n_elements(ionfilelist)

npoint_all=dblarr(nfiles)
alon_all=dblarr(nfiles,41)
alat_all=dblarr(nfiles,41)
aalt_all=dblarr(nfiles,41)
;28 means altitude grids from 216 km to 615 km

for ifile=0,nfiles-1 do begin

  filenameion = ionfilelist(ifile)

  ;read 3dall (ne)
  read_thermosphere_file, filenameion, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  ;prepare the time for geopack
  strtime=strtrim(string(itime(0)),1)+'-'+strtrim(string(itime(1)),1)+'-'+strtrim(string(itime(2)),1)
  atime=fix(date_conv(strtime,'V'))

  print,filenameion

  ;GEOPACK does not work right now. 05-20-2021
  ;GEOPACK_RECALC,itime[0],atime[1],itime[3],itime[4],0

  lon = reform(data(0,*,0,0))*180/!pi
  lat=  reform(data(1,0,*,0))*180/!pi
  alt=  data(2,0,0,*)

  mlat= reform(data(22,*,*,*))
  mlon= reform(data(23,*,*,*))
  bmag= reform(data(27,*,*,*))
  dx=1e3 ;km
  re=6371e3

  ;find the lon for a fixed lt. Here is 19.5
  utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
  utime = utime(0)

  for fixlt=19,19 do begin
    localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0
    f=min(abs(localtime-fixlt),ilon)

    if ilon lt 2 then ilon=ilon+180
    if ilon gt 181 then ilon=ilon-180

    for apex=49, 49 do begin ;from 216 km
      ialt=apex ; altitude of apex
      h=apex
      feq=min(abs(mlat(ilon,*,ialt)),ilat) ;find the magnetic equator
      trace_field_line,ilon,ilat,ialt,data,l,npoint,alon,alat,aalt
      npoint_all[ifile,*]=npoint
      alon_all[ifile,*]=alon
      alat_all[ifile,*]=alat
      aalt_all[ifile,*]=aalt
    endfor
  endfor
endfor

save, lat, lon, alt, alat_all, alon_all, aalt_all, filename=root+'fl.sav'
end