itime=[2015,3,17,0,0,0]

restore, '/Users/wzihan/Dropbox (University of Michigan)/GDC_Ephemeris/gdc_idl_hires.sav'
tsize=n_elements(satut[*,0])

for tt =0, tsize-1 do begin
  diff=itime eq satut[tt,*]
  if total(diff) eq 6 then break
endfor

satlon=satlon[tt:tt+79,*]
satlat=satlat[tt:tt+79,*]
satut=satut[tt:tt+79,*]

salp=dblarr(80,6)
makect, 'all'

wc=ask('Root folder:',wc)
fig_folder=ask('Figure root:',root)
figname=ask('Figure name:',figname)
filelist=findfile(wc)

nfiles = n_elements(filelist)

if (nFiles eq 1 and strlen(filelist(0)) eq 0) then begin
  print, 'can not find file!'
  stop
endif

ialt=36

satp=dblarr(80,6)
ilon_g=dblarr(80,6)
ilat_g=dblarr(80,6)
plotdata=dblarr(184,184)

for iFile = 0, 7 do begin

  filename = filelist(iFile)

  print, 'Reading file ',filename

  read_thermosphere_file, filename, nvars, nalts, nlats, nlons, $
    vars, data, rb, cb, bl_cnt, iTime, Version

  utime = itime(3)*3600.0 + $
    itime(4)*60.0 + $
    itime(5)
  utime = utime(0)
  
  alts = reform(data(2,0,*,*)) / 1000.0
  lats = reform(data(1,0,*,0)) / !dtor
  lons = reform(data(0,*,0,0)) / !dtor
  
  alt = reform(data(2,*,*,*)) / 1000.0
  lat = reform(data(1,*,*,*)) / !dtor
  lon = reform(data(0,*,*,*)) / !dtor
  
  k=1.38e-23
  t=reform(data[15,*,*,ialt])
  n=dblarr(nlons,nlats)
  for s=4,14 do n=n+reform(data[s,*,*,ialt])
  p=n*k*t
  
  for s=0,5 do begin
    for tt=0,9 do begin
      if satlon[tt+ifile*10,s] lt 0 then slon=360+satlon[tt+ifile*10,s] else slon=satlon[tt+ifile*10,s]
      f1=min(abs(lons-slon),ilon)
      f2=min(abs(lats-satlat[tt+ifile*10,s]),ilat)
      ilat_g[tt+ifile*10,s]=ilat
      ilon_g[tt+ifile*10,s]=ilon
      satp[tt+ifile*10,s]=p[ilon,ilat]
      plotdata[ilon, ilat]=p[ilon,ilat]
    endfor
  endfor
  
endfor

save, satp, satlon, satlat, plotdata, filename=fig_folder+figname+'.sav'

end
  