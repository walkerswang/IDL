foldername='/Users/wzihan/Simulations/'
FOLDER='/Users/wzihan/Simulations/data/first_storm/'

allFILELIST = FINDFILE(FOLDER+'3DALL_*.bin')

north = 1

nfiles = n_elements(allfilelist)

ff = 0
lf = nfiles-1

viup=fltarr(nfiles,3)
ele_den=fltarr(nfiles,54)
vieast=fltarr(nfiles,3)
vinorth=fltarr(nfiles,3)
vneast=fltarr(nfiles,3)
vnnorth=fltarr(nfiles,3)
ti=fltarr(nfiles,54)
te=fltarr(nfiles,54)

DEVICE, DECOMPOSED=0

loadct,34

;for ifile = ff, lf do begin
;
;  filename = allfilelist(iFile)
;  
;  p=strpos(filename,'.')
;  day=strmid(filename,p-9,2)
;  hour=strmid(filename,p-6,2)
;
;  ;read 3dall (neutral wind)
;  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
;    nBLKlat, nBLKlon, nBLK, iTime, Version
;
;  ilat=[156,157,158]
;  ialt=32
;
;  lon = (reform(data(0,*,ilat,0))*180/!pi + 360.0) mod 360.0
;
;  f=min(abs(lon+147-360),ilon)
;
;  ;16. V!Dn!N(east)
;  ;17. V!Dn!N(north)
;  ;18. V!Dn!N(up)
;  viup[ifile,*] = reform(data(39,ilon,ilat,ialt))
;  ele_den[ifile,*]=reform(data(34,ilon,ilat[1],*))
;  te[ifile,*]=reform(data(35,ilon,ilat[1],*))
;  ti[ifile,*]=reform(data(36,ilon,ilat[1],*))
;  vieast[ifile,*]=reform(data(37,ilon,ilat,ialt))
;  vinorth[ifile,*]=reform(data(38,ilon,ilat,ialt))
;  vneast[ifile,*]=reform(data(16,ilon,ilat,ialt))
;  vnnorth[ifile,*]=reform(data(17,ilon,ilat,ialt))
;
;endfor
;
;alt=reform(data(2,ilon,ilat[1],*))
;
;save,viup,ele_den,te,ti,vieast,vinorth,vneast,vnnorth,alt,filename=foldername+'plot_new/pfisr_new.sav'
!p.multi=[0,1,6,0,0]
restore, foldername+'plot_new/pfisr_new.sav'
figfile= foldername+'plot_new/pfisr_new'

setdevice, figfile+'.ps','p',5

dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

;;;;;;;;;;;;;plot ne;;;;;;;;;;;;;
; Generate the Date/Time data
time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,0,0),UNITS='Minutes', STEP_SIZE=5)

ll=[66,67,68,69]

plot,time,alt,/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
  xminor=6,yminor=2,font=0,YTITLE='Altitude (km)',$
  xticklen=-0.03,yticklen=-0.01,xticks=11

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,5,0),UNITS='Minutes', STEP_SIZE=5)

FOR k=0, 133-2 DO BEGIN;  UT
  x1=time[k]
  x2=time[k+1]
  x3=time[k+1]
  x4=time[k]
  xx=[x1,x2,x3,x4];

  FOR i=0, 52 DO BEGIN         ; altitude
    yy=[alt[i],alt[i],alt[i+1],alt[i+1]]
    cc=alog10(ele_den(k,i))
    cl=(cc-10)/2*256.
    if (cl le 0) THEN cl=0
    if (cl ge 255) THEN cl=255
    POLYFILL,xx,yy,COLOR=cl,/data
  ENDFOR  ;i
ENDFOR ;k

post_ct=[.99,0.86,1.05,1]
; plot color table
nlable=11; number of lables
tickn=['10!U10.0','10!U10.2','10!U10.4','10!U10.6','10!U10.8',$
'10!U11.0','10!U11.2','10!U11.4','10!U11.6','10!U11.8','10!U12.0']
num_level=256
nvl=num_level
;!p.position=0.
;tickn='!18'+[strtrim(string(lev),1)]
plot,pos=post_ct,[0,1],[0,num_level-1],/noerase,/nodata,xcharsize=0.001,ycharsize=0.001,$
  xstyle=3,ystyle=1,xticklen=0,yticklen=0,thick=2
for b=0,num_level-1 do polyfill,[0.,0.,1.,1.],[b,b+1,b+1,b],color=b
xx=fltarr(nlable)
xx=post_ct[0]+findgen(nlable)*0+0.02
yy=fltarr(nlable)
yy(*)=(post_ct[3]-post_ct[1])/(nlable-1)*findgen(nlable)
for n=0,nlable-1 do begin
  xyouts,xx(n),post_ct[1]+yy(n),tickn(n),/normal,color=0,font=0,charsize=0.9
endfor
;xyouts,1.06,0.85,'Ne (m!U-3)',/normal,font=0,ORIENTATION=90,charsize=1.1

;;;;;;;;;;;;;plot te;;;;;;;;;;;;;
time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,0,0),UNITS='Minutes', STEP_SIZE=5)
plot,time,alt,/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
  xminor=6,yminor=2,font=0,YTITLE='Altitude (km)',$
  xticklen=-0.03,yticklen=-0.01,xticks=11

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,5,0),UNITS='Minutes', STEP_SIZE=5)

FOR k=0, 133-2 DO BEGIN;  UT
  x1=time[k]
  x2=time[k+1]
  x3=time[k+1]
  x4=time[k]
  xx=[x1,x2,x3,x4];

  FOR i=0, 52 DO BEGIN         ; altitude
    yy=[alt[i],alt[i],alt[i+1],alt[i+1]]
    cl=te[k,i]/4000.*256.
    if (cl le 0) THEN cl=0
    if (cl ge 255) THEN cl=255
    POLYFILL,xx,yy,COLOR=cl,/data
  ENDFOR  ;i
ENDFOR ;k

post_ct=[.99,0.69,1.05,.84]
; plot color table
nlable=8; number of lables
tickn=['0','500','1000','1500','2000','2500','3000','3500']
num_level=256
nvl=num_level
;!p.position=0.
;tickn='!18'+[strtrim(string(lev),1)]
plot,pos=post_ct,[0,1],[0,num_level-1],/noerase,/nodata,xcharsize=0.001,ycharsize=0.001,$
  xstyle=3,ystyle=1,xticklen=0,yticklen=0,thick=2
for b=0,num_level-1 do polyfill,[0.,0.,1.,1.],[b,b+1,b+1,b],color=b
xx=fltarr(nlable)
xx=post_ct[0]+findgen(nlable)*0+0.02
yy=fltarr(nlable)
yy(*)=(post_ct[3]-post_ct[1])/(nlable-1)*findgen(nlable)
for n=0,nlable-1 do begin
  xyouts,xx(n),post_ct[1]+yy(n),tickn(n),/normal,color=0,font=0,charsize=0.9
endfor
;xyouts,1.06,0.85,'Ne (m!U-3)',/normal,font=0,ORIENTATION=90,charsize=1.1

;;;;;;;;;;;;;plot ti;;;;;;;;;;;;;
time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,0,0),UNITS='Minutes', STEP_SIZE=5)

plot,time,alt,/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
  xminor=6,yminor=2,font=0,YTITLE='Altitude (km)',$
  xticklen=-0.03,yticklen=-0.01,xticks=11

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,5,0),UNITS='Minutes', STEP_SIZE=5)

FOR k=0, 133-2 DO BEGIN;  UT
  x1=time[k]
  x2=time[k+1]
  x3=time[k+1]
  x4=time[k]
  xx=[x1,x2,x3,x4];

  FOR i=0, 52 DO BEGIN         ; altitude
    yy=[alt[i],alt[i],alt[i+1],alt[i+1]]
    cl=ti[k,i]/3500.*256.
    if (cl le 0) THEN cl=0
    if (cl ge 255) THEN cl=255
    POLYFILL,xx,yy,COLOR=cl,/data
  ENDFOR  ;i
ENDFOR ;k

post_ct=[.99,0.52,1.05,.67]
; plot color table
nlable=8; number of lables
tickn=['0','500','1000','1500','2000','2500','3000','3500']
num_level=256
nvl=num_level
;!p.position=0.
;tickn='!18'+[strtrim(string(lev),1)]
plot,pos=post_ct,[0,1],[0,num_level-1],/noerase,/nodata,xcharsize=0.001,ycharsize=0.001,$
  xstyle=3,ystyle=1,xticklen=0,yticklen=0,thick=2
for b=0,num_level-1 do polyfill,[0.,0.,1.,1.],[b,b+1,b+1,b],color=b
xx=fltarr(nlable)
xx=post_ct[0]+findgen(nlable)*0+0.02
yy=fltarr(nlable)
yy(*)=(post_ct[3]-post_ct[1])/(nlable-1)*findgen(nlable)
for n=0,nlable-1 do begin
  xyouts,xx(n),post_ct[1]+yy(n),tickn(n),/normal,color=0,font=0,charsize=0.9
endfor
;xyouts,1.06,0.85,'Ne (m!U-3)',/normal,font=0,ORIENTATION=90,charsize=1.1

;;;;;;;;;;;;magnitude of vi_horizontal;;;;;;;;;;;;;;
time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,0,0),UNITS='Minutes', STEP_SIZE=5)

plot,time,ll,/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
  xminor=6,yminor=2,font=0,YTITLE='MagLatitude (deg.)',$
  xticklen=-0.03,yticklen=-0.01,xticks=11

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,5,0),UNITS='Minutes', STEP_SIZE=5)

FOR k=0, 133-2 DO BEGIN;  UT
  x1=time[k]
  x2=time[k+1]
  x3=time[k+1]
  x4=time[k]
  xx=[x1,x2,x3,x4];

  FOR i=0, 2 DO BEGIN         ; altitude
    yy=[ll[i],ll[i],ll[i+1],ll[i+1]]
    cl=sqrt(vieast(k,i)^2+vinorth(k,i)^2)/2000.*256.
    if (cl le 0) THEN cl=0
    if (cl ge 255) THEN cl=255
    POLYFILL,xx,yy,COLOR=cl,/data
  ENDFOR  ;i
ENDFOR ;k

post_ct=[.99,0.35,1.05,0.5]
; plot color table
nlable=5; number of lables
tickn=['0','500','1000','1500','2000']
num_level=256
nvl=num_level
;!p.position=0.
;tickn='!18'+[strtrim(string(lev),1)]
plot,pos=post_ct,[0,1],[0,num_level-1],/noerase,/nodata,xcharsize=0.001,ycharsize=0.001,$
  xstyle=3,ystyle=1,xticklen=0,yticklen=0,thick=2
for b=0,num_level-1 do polyfill,[0.,0.,1.,1.],[b,b+1,b+1,b],color=b
xx=fltarr(nlable)
xx=post_ct[0]+findgen(nlable)*0+0.02
yy=fltarr(nlable)
yy(*)=(post_ct[3]-post_ct[1])/(nlable-1)*findgen(nlable)
for n=0,nlable-1 do begin
  xyouts,xx(n),post_ct[1]+yy(n),tickn(n),/normal,color=0,font=0,charsize=0.9
endfor
;xyouts,1.06,0.6,'vi_up (m/s)',/normal,font=0,ORIENTATION=90,charsize=1.1

;;;;;;;;;;;;magnitude of vn_horizontal;;;;;;;;;;;;;;
time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,0,0),UNITS='Minutes', STEP_SIZE=5)

plot,time,ll,/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
  xminor=6,yminor=2,font=0,YTITLE='MagLatitude (deg.)',$
  xticklen=-0.03,yticklen=-0.01,xticks=11

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,5,0),UNITS='Minutes', STEP_SIZE=5)

FOR k=0, 133-2 DO BEGIN;  UT
  x1=time[k]
  x2=time[k+1]
  x3=time[k+1]
  x4=time[k]
  xx=[x1,x2,x3,x4];

  FOR i=0, 2 DO BEGIN         ; altitude
    yy=[ll[i],ll[i],ll[i+1],ll[i+1]]
    ;print,sqrt(vneast(k,i)^2+vnnorth(k,i)^2)
    cl=sqrt(vneast(k,i)^2+vnnorth(k,i)^2)/1000.*256.
    if (cl le 0) THEN cl=0
    if (cl ge 255) THEN cl=255
    POLYFILL,xx,yy,COLOR=cl,/data
  ENDFOR  ;i
ENDFOR ;k

post_ct=[.99,0.18,1.05,0.33]
; plot color table
nlable=6; number of lables
tickn=['0','200','400','600','800','1000']
num_level=256
nvl=num_level
;!p.position=0.
;tickn='!18'+[strtrim(string(lev),1)]
plot,pos=post_ct,[0,1],[0,num_level-1],/noerase,/nodata,xcharsize=0.001,ycharsize=0.001,$
  xstyle=3,ystyle=1,xticklen=0,yticklen=0,thick=2
for b=0,num_level-1 do polyfill,[0.,0.,1.,1.],[b,b+1,b+1,b],color=b
xx=fltarr(nlable)
xx=post_ct[0]+findgen(nlable)*0+0.02
yy=fltarr(nlable)
yy(*)=(post_ct[3]-post_ct[1])/(nlable-1)*findgen(nlable)
for n=0,nlable-1 do begin
  xyouts,xx(n),post_ct[1]+yy(n),tickn(n),/normal,color=0,font=0,charsize=0.9
endfor
;xyouts,1.06,0.6,'vi_up (m/s)',/normal,font=0,ORIENTATION=90,charsize=1.1


;;;;;;;;;;;;vertical flow;;;;;;;;;;;;;;
time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,0,0),UNITS='Minutes', STEP_SIZE=5)

plot,time,ll,/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='MagLatitude (deg.)',$
    xticklen=-0.03,yticklen=-0.01,xticks=11

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,5,0),UNITS='Minutes', STEP_SIZE=5)

FOR k=0, 133-2 DO BEGIN;  UT
  x1=time[k]
  x2=time[k+1]
  x3=time[k+1]
  x4=time[k]
  xx=[x1,x2,x3,x4];
  
  FOR i=0, 2 DO BEGIN         ; altitude
    yy=[ll[i],ll[i],ll[i+1],ll[i+1]]
    cl=(viup(k,i)/100)*128.+128.
    if (cl le 0) THEN cl=0
    if (cl ge 255) THEN cl=255
    POLYFILL,xx,yy,COLOR=cl,/data
  ENDFOR  ;i
ENDFOR ;k

post_ct=[.99,0.01,1.05,0.16]
; plot color table
nlable=11; number of lables
tickn=['-100','-80','-60','-40','-20','0','20','40','60','80','100']
num_level=256
nvl=num_level
;!p.position=0.
;tickn='!18'+[strtrim(string(lev),1)]
plot,pos=post_ct,[0,1],[0,num_level-1],/noerase,/nodata,xcharsize=0.001,ycharsize=0.001,$
  xstyle=3,ystyle=1,xticklen=0,yticklen=0,thick=2
for b=0,num_level-1 do polyfill,[0.,0.,1.,1.],[b,b+1,b+1,b],color=b
xx=fltarr(nlable)
xx=post_ct[0]+findgen(nlable)*0+0.02
yy=fltarr(nlable)
yy(*)=(post_ct[3]-post_ct[1])/(nlable-1)*findgen(nlable)
for n=0,nlable-1 do begin
  xyouts,xx(n),post_ct[1]+yy(n),tickn(n),/normal,color=0,font=0,charsize=0.9
endfor
;xyouts,1.06,0.6,'vi_up (m/s)',/normal,font=0,ORIENTATION=90,charsize=1.1

closedevice

end
