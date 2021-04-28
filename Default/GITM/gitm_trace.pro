
FOLDER='/USERS/WZIHAN/SIMULATIONS/'
allFILELIST = FINDFILE(FOLDER+'data/trace/3DALL_t170907_*.bin')

nfiles = n_elements(allfilelist)

ff = 0
lf = nfiles-1

latp=58.5
lonp=235
ialt=35

vin=fltarr(nfiles,1)
vie=fltarr(nfiles,1)
tii=fltarr(nfiles,1)
eden=fltarr(nfiles,1)

for ifile = lf, ff, -1 do begin

  filename = allfilelist(iFile)
  print,filename
  ;read 3dall (ne)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  lon = reform(data(0,*,0,0))*180/!pi
  lat=  reform(data(1,0,*,0))*180/!pi
  alt=  data(2,0,0,*)
  
  f1=min(abs(lonp-lon),ilon)
  f2=min(abs(latp-lat),ilat)

  ve= data(37,ilon,ilat,ialt)
  vn= data(38,ilon,ilat,ialt)
  vu=data(39,ilon,ilat,*)
  en= data(34,ilon,ilat,*)
  ti=data(36,ilon,ilat,*)
  te=data(35,ilon,ilat,*)
  tn=data(15,ilon,ilat,*)
  vne=data(16,ilon,ilat,*)
  vnn=data(17,ilon,ilat,*)
  vnu=data(18,ilon,ilat,*)
  n2=data(6,ilon,ilat,*)
  op=data(25,ilon,ilat,*)
  
  vie[ifile]=ve
  vin[ifile]=vn
  tii[ifile]=ti[ialt]
  eden[ifile]=en[ialt]
  
  urel=sqrt((ve-vne)^2+(vn-vnn)^2+(vu-vnu)^2)
  

  if (lf-ifile) eq 7 then begin
    p2=plot(alog10(en),alt/1e3,/overplot,color=[0,0,0],xrange=[10,12],name='2225',thick=3)
  endif
  
  if ifile eq 0 then begin
    p1=plot(alog10(en),alt/1e3,/overplot,color=[255,0,0],xrange=[10,12],name='2130',thick=3)
  endif
  
  if (lf-ifile) eq 0 then begin
    p3=plot(alog10(en),alt/1e3,/overplot,color=[0,255,0],xrange=[10,12],name='2300',xtitle='log(NE)',ytitle='Altitude (km)',thick=3)
  endif
  
  ;print,en[32]
  ;print,n2[32]
  ;print,op[32]
  
  for i=0,5*60-1 do begin
    lonp=lonp-ve/(6371*2*!pi*cos(latp*!pi/180))*360*1e-3
    latp=latp-vn/(6371*2*!pi)*360*1e-3
  endfor
  print,vn
  print,ve
  print,latp
  print,lonp
  utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
  utime = utime(0)

  localtime = (utime/3600.0 + lonp*12.0/180.0) mod 24.0
  print,localtime
endfor

leg = LEGEND(TARGET=[p1,p2,p3], POSITION=[12,200], /DATA, /AUTO_TEXT_COLOR)

p3.save,'/Users/wzihan/Simulations/plot_new/nw_effect_350km/trace_en_2130-2300_pub.ps'

time = TIMEGEN(START=JULDAY(9,7,2017,21,30,0), FINAL=JULDAY(9,7,2017,23,0,0),UNITS='Minutes', STEP_SIZE=5)
tic = TIMEGEN(START=JULDAY(9,7,2017,21,30,0), FINAL=JULDAY(9,7,2017,23,0,0),UNITS='Minutes', STEP_SIZE=10)

;p1=plot(time,tii,xtickformat='LABEL_DATE',xtickvalues=tic,ytitle='Ti (K)',xrange=[time[0],time[18]])
;p1=plot(time,alog10(eden),xtickformat='LABEL_DATE',xtickvalues=tic,ytitle='log(NE)',xrange=[time[0],time[18]])
p1=plot(time,sqrt(vie^2+vin^2),xtickformat='LABEL_DATE',xtickvalues=tic,ytitle='Eastward Ion Velocity (m/s)',xrange=[time[0],time[18]])
end
