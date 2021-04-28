;wildcard = ask('files to plot tec (e.g., 3DALL*.bin)','3DALL*.bin')

foldername='/Users/wzihan/Simulations'
filelist = file_search(foldername+'/data/3DALL_t17*.bin')
file = filelist(0)
makect,'bwr'

gitm_read_header, file, Vars, Time, nLats, nLons, nAlts, version

IsDone = 0
iVar = [0,1,2,34]

nFiles = n_elements(filelist)

time = dblarr(nFiles)

ilon=37

;for iFile = 0, nFiles-1 do begin
;
;  file = filelist(iFile)
;  print, file
;  gitm_read_bin_1var,file, data, time1, nVars, Vars, version, VarsToGet = iVar
;
;  lats = reform(data(1,*,*,0))
;  lons = reform(data(0,*,*,0))
;
;  c_r_to_a, itime, time1
;  ut = float(itime(3)) + float(itime(4))/60.0 + float(itime(5))/3600.0
;
;  localtime = (lons/!dtor/15.0 + ut) mod 24.0
;
;  if (iFile eq 0) then begin
;
;    nLons = n_elements(lons(*,0))
;    nLats = n_elements(lats(0,*))
;    dlon = lons
;    dlon(0:nlons-2,*) = lons(1:nLons-1,*) - lons(0:nLons-2,*)
;    dlon(nlons-1,*) = dlon(0,*)
;
;    dlat = lats
;    dlat(*,1:nlats-2) = (lats(*,2:nLats-1) - lats(*,0:nLats-3))/2.0
;    dlat(*,0) = lats(*,1) - lats(*,0)
;    dlat(*,nLats-1) = lats(*,nLats-1) - lats(*,nLats-2)
;    area = 6372000.0*6372000.0 * cos(lats) * dlon * dlat
;
;    nAlts = n_elements(data(0,0,0,*))
;    dAlt = reform(data(2,*,*,*))
;
;    tec = fltarr(nFiles, nLons, nLats)
;
;  endif
;
;  time(iFile) = time1
;  for iAlt=0,nAlts-2 do begin
;    dAlt(*,*,iAlt) = reform(data(2,*,*,iAlt+1))-reform(data(2,*,*,iAlt))
;    tec(iFile,*,*) = tec(iFile,*,*) + dAlt(*,*,iAlt)*data(3,*,*,iAlt)
;  endfor
;
;endfor
;
;etime = max(time)
;stime = etime - 24.0*3600.0*3.0
;
;tmptime = time - stime
;
;tec = tec/1.0e16
;
;tec_smooth=smooth(tec,[25,1,1])
;tec_low=tec-tec_smooth
;tec_band=smooth(tec_low,[3,1,1])
;
;lat =reform(data(1,ilon,*,0))*180/!pi

restore,'tec_tid.sav'

figfile= foldername+'/plot_new/dtec_2h/tid_tec_lt18'

setdevice, figfile+'.ps'

dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,24,0,0),UNITS='Minutes', STEP_SIZE=5)

plot,time,lat[112:152],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
  xminor=6,yminor=2,font=0,YTITLE='Latitude',$
  xticklen=-0.03,yticklen=-0.01,xticks=5

time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,24,0,0),UNITS='Minutes', STEP_SIZE=5)

FOR k=0, nfiles-1 DO BEGIN;
  x1=time[k]
  x2=time[k+1]
  x3=time[k+1]
  x4=time[k]
  xx=[x1,x2,x3,x4]
  
  localtime = ((time[k]-time[0])*24+18 + lon/!pi*12.0) mod 24.0

  f=min(abs(localtime-18),ilon)

  print,ilon
    
  FOR i=112, 152 DO BEGIN         ; altitude
    yy=[lat[i],lat[i],lat[i+1],lat[i+1]]
    cc=tec_band(k,ilon,i)
    cl=(cc+1)/2.0*254.0
    if (cl le 0) THEN cl=0
    if (cl ge 254) THEN cl=254
    POLYFILL,xx,yy,COLOR=cl,/data
  ENDFOR  ;i
ENDFOR ;k

post_ct=[.99,0.86,1.05,1]
; plot color table
nlable=5; number of lables

;tickn=['-30','-15','0','15','30']
;tickn=['-5x10!U10','-2.5x10!U10','0','2.5x10!U10','5x10!U10']
tickn=['-1','-0.5','0','0.5','1']
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
