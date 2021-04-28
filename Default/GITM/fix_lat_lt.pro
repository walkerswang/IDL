foldername='/Users/wzihan/Simulations/'
FOLDER='/Users/wzihan/Simulations/data/'

allFILELIST = FINDFILE(FOLDER+'3DALL_*.bin')

north = 1

nfiles = n_elements(allfilelist)

ff = 0
lf = nfiles-1

ele_den=fltarr(nfiles,184,54)
vnup=fltarr(nfiles,184,54)
viup=fltarr(nfiles,184,54)
ti=fltarr(nfiles,184,54)

DEVICE, DECOMPOSED=0

loadct,34

ilat=131
ltime=20
ilon=150
for ifile = ff, lf do begin

  filename = allfilelist(iFile)

  p=strpos(filename,'.')
  day=strmid(filename,p-9,2)
  hour=strmid(filename,p-6,2)

  ;read 3dall (neutral wind)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version
  
  lon = (reform(data(0,*,ilat,0))*180/!pi + 360.0) mod 360.0

;  utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
;  utime = utime(0)
;
;  localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0
;
;  f=min(abs(localtime-ltime),ilon)
  
  ;16. V!Dn!N(east)
  ;17. V!Dn!N(north)
  ;18. V!Dn!N(up)
  ele_den[ifile,*,*]=reform(data(34,ilon,*,*))
  ti[ifile,*,*]=reform(data(36,ilon,*,*))
  vnup[ifile,*,*]=reform(data(18,ilon,*,*))
  viup[ifile,*,*]=reform(data(39,ilon,*,*))

endfor

alt=reform(data(2,ilon,ilat,*))

save,ele_den,ti,viup,vnup,alt,filename=foldername+'plot_new/tid_all_fix_lon.sav'

restore, foldername+'plot_new/tid_all_fix_lon.sav'

ele_den=reform(ele_den[*,ilat,*],[360,54])
viup=reform(viup[*,ilat,*],[360,54])

sm=smooth(ele_den,[31,1])

figfile= foldername+'plot_new/meridional/tid_ne'

setdevice, figfile+'.ps'

dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

;;;;;;;;;;;;;plot ne;;;;;;;;;;;;;
; Generate the Date/Time data
time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,23,55,0),UNITS='Minutes', STEP_SIZE=5)

plot,time,alt,/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
  xminor=6,yminor=2,font=0,YTITLE='Altitude (km)',$
  xticklen=-0.03,yticklen=-0.01,xticks=11

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,23,55,0),UNITS='Minutes', STEP_SIZE=5)

FOR k=0, lf-1 DO BEGIN;  UT
  x1=time[k]
  x2=time[k+1]
  x3=time[k+1]
  x4=time[k]
  xx=[x1,x2,x3,x4];

  FOR i=0, 52 DO BEGIN         ; altitude
    yy=[alt[i],alt[i],alt[i+1],alt[i+1]]
    ;cc=viup(k,i)
    ;cl=cc/100.0*127.+127
    cc=ele_den(k,i)-sm(k,i)
    cl=cc/1e9*127.+127
    if (cl le 0) THEN cl=0
    if (cl ge 255) THEN cl=255
    POLYFILL,xx,yy,COLOR=cl,/data
  ENDFOR  ;i
ENDFOR ;k

post_ct=[.99,0.86,1.05,1]
; plot color table
nlable=5; number of lables

;tickn=['-100','-50','0','50','100']
tickn=['-1x10!U9','-5x10!U8','0','5x10!U8','1x10!U9']
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
closedevice

end
