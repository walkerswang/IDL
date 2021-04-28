foldername='/Users/wzihan/Simulations/'
FOLDER='/Users/wzihan/Simulations/data/'

allFILELIST = FINDFILE(FOLDER+'3DALL_t170907_2*.bin')

north = 1

nfiles = n_elements(allfilelist)

ff = 0
lf = nfiles-1

viup=fltarr(nfiles+1,1)
vin=fltarr(nfiles+1,1)
vie=fltarr(nfiles+1,1)

DEVICE, DECOMPOSED=0

loadct,34

ilat=150
ltime=14.5
ialt=35

for ifile = ff, lf do begin

  filename = allfilelist(iFile)

  p=strpos(filename,'.')
  day=strmid(filename,p-9,2)
  hour=strmid(filename,p-6,2)

  ;read 3dall (neutral wind)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  lon = (reform(data(0,*,ilat,0))*180/!pi + 360.0) mod 360.0

  utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
  utime = utime(0)
  
  localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0
  
  f=min(abs(localtime-ltime),ilon)

  ;16. V!Dn!N(east)
  ;17. V!Dn!N(north)
  ;18. V!Dn!N(up)
  ;ele_den[ifile,*,*]=reform(data(34,ilon,*,*))
  ;ti[ifile,*,*]=reform(data(36,ilon,*,*))
  ;vnup[ifile,*,*]=reform(data(18,ilon,*,*))
  ;viup[ifile,*,*]=reform(data(39,ilon,*,*))
  ;vie[ifile,*,*]=reform(data(37,ilon,*,*))
  ;vin[ifile,*,*]=reform(data(38,ilon,*,*))
  viup[ifile]=reform(data(39,ilon,ilat,ialt))
  vie[ifile]=reform(data(37,ilon,ilat,ialt))
  vin[ifile]=reform(data(38,ilon,ilat,ialt))
  
  ;print,sqrt(vie^2+vin^2+viup^2)
endfor

viup[48]=viup[47]
vie[48]=vie[47]
vin[48]=vin[47]

dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

time = TIMEGEN(START=JULDAY(9,7,2017,20,0,0), FINAL=JULDAY(9,7,2017,24,0,0),UNITS='Minutes', STEP_SIZE=5)

f=plot(time,vin,xstyle=1,ystyle=1,xtickformat='LABEL_DATE', xmajor=5,xminor=5,YTITLE='(m/s)')

end
