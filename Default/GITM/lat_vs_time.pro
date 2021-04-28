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

ilat=80
ilon=40 ;jacamarca 11.9523° S, 76.8758° W

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
;;  utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
;;  utime = utime(0)
;;
;;  localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0
;;
;;  f=min(abs(localtime-ltime),ilon)
;
;  ;16. V!Dn!N(east)
;  ;17. V!Dn!N(north)
;  ;18. V!Dn!N(up)
;  ele_den[ifile,*,*]=reform(data(34,ilon,*,*))
;  ti[ifile,*,*]=reform(data(36,ilon,*,*))
;  vnup[ifile,*,*]=reform(data(18,ilon,*,*))
;  viup[ifile,*,*]=reform(data(39,ilon,*,*))
;
;endfor
;
;alt=reform(data(2,ilon,ilat,*))
;lat =reform(data(1,ilon,*,0))*180/!pi
;save,lat,ele_den,ti,viup,vnup,alt,filename=foldername+'plot_new/tid_all_lon77.sav'

restore, foldername+'plot_new/tid_all_lon77.sav'

vnup=reform(vnup[*,*,35],[nfiles,nlats])
ele_den=ele_den[*,*,35]
ele_den=reform(ele_den,[nfiles,nlats])
viup=reform(viup[*,*,35],[nfiles,nlats])

sm=smooth(ele_den,[81,1])

figfile= foldername+'plot_new/meridional/tid_ne_ja'

setdevice, figfile+'.ps'

dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

;;;;;;;;;;;;;plot ne;;;;;;;;;;;;;
; Generate the Date/Time data
time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,23,55,0),UNITS='Minutes', STEP_SIZE=5)

plot,time[0:lf-23],lat[31:152],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
  xminor=6,yminor=2,font=0,YTITLE='Latitude',$
  xticklen=-0.03,yticklen=-0.01,xticks=7

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,23,55,0),UNITS='Minutes', STEP_SIZE=5)

FOR k=0, lf-23 DO BEGIN;  UT
  x1=time[k]
  x2=time[k+1]
  x3=time[k+1]
  x4=time[k]
  xx=[x1,x2,x3,x4];

  FOR i=31, 151 DO BEGIN         ; altitude
    yy=[lat[i],lat[i],lat[i+1],lat[i+1]]
    ;cc=vnup(k,i)
    ;cl=cc/30.0*127.+127
    cc=ele_den(k,i);-sm(k,i)
    cl=(alog10(cc)-11.5)*127.+127
    if (cl le 0) THEN cl=0
    if (cl ge 255) THEN cl=255
    POLYFILL,xx,yy,COLOR=cl,/data
  ENDFOR  ;i
ENDFOR ;k

post_ct=[.99,0.86,1.05,1]
; plot color table
nlable=5; number of lables

;tickn=['-30','-15','0','15','30']
;tickn=['-5x10!U10','-2.5x10!U10','0','2.5x10!U10','5x10!U10']
tickn=['10!U10.5','10!U11','10!U11.5','10!U12','10!U12.5']
num_level=256
nvl=num_level
;!p.position=0.
;tickn='!18'+[strtrim(string(lev),1)]
plot,pos=post_ct,[0,1],[0,num_level-1],/noerase,/nodata,xcharsize=0.001,ycharsize=0.001,$
  xstyle=3,ystyle=1,xticklen=0,yticklen=0,thick=2,title=''
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
