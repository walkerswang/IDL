
folder=ask('SAV file location:',folder)
restore,folder+'gamma.sav'

year=ask('year:')
month=ask('month:')
sday=ask('start day:')
shour=ask('start hour:')
eday=ask('end day:')
ehour=ask('end hour:')

nfiles=n_elements(EBDRIFT[*,0,0])

makect,'bwr'
for fixlt=0,23 do begin

  ;;;;;;;;plot gamma;;;;;;;;;
  setdevice, folder+'gamma_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

  time = TIMEGEN(START=JULDAY(month,sday,year,shour,0,0), FINAL=JULDAY(month,eday,year,ehour,0,0),UNITS='Minutes', STEP_SIZE=5)

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$ 
    xticklen=-0.03,yticklen=-0.01,xticks=11

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=gam[k,fixlt,i]/1e-5
      cl=(cc+600)/1200*255
      if (cl le 0) THEN cl=1
      if (cl ge 255) THEN cl=254
      POLYFILL,xx,yy,COLOR=cl,/data
    ENDFOR  ;i
  ENDFOR ;k

  post_ct=[.98,0.86,.995,1]
  ; plot color table
  nlable=5; number of lables

  ;tickn=['-30','-15','0','15','30']
  ;tickn=['-5x10!U10','-2.5x10!U10','0','2.5x10!U10','5x10!U10']
  ;tickn=['-50','-25','0','25','50']
  tickn=['-6x10!U-3','-3x10!U-3','0','3x10!U-3','6x10!U-3']
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
  xyouts,0.5,1,'Growth Rate s!U-1',/normal,font=0,charsize=1.1
  closedevice

  ;;;;;;;;plot ebdrift;;;;;;;;;
  setdevice, folder+'ebdrift_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=11

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=ebdrift[k,fixlt,i]
      cl=(cc+100)/200*255
      if (cl le 0) THEN cl=1
      if (cl ge 255) THEN cl=254
      POLYFILL,xx,yy,COLOR=cl,/data
    ENDFOR  ;i
  ENDFOR ;k

  ; plot color table
  nlable=5; number of lables

  tickn=['-100','-50','0','50','100']
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
  xyouts,0.5,1,'Drift m/s',/normal,font=0,charsize=1.1
  closedevice

  ;;;;;;;;plot neutralwind;;;;;;;;;

  setdevice, folder+'nw_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=11

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=-neutralwind[k,fixlt,i]
      cl=(cc+40)/80*255
      if (cl le 0) THEN cl=1
      if (cl ge 255) THEN cl=254
      POLYFILL,xx,yy,COLOR=cl,/data
    ENDFOR  ;i
  ENDFOR ;k

  ; plot color table
  nlable=5; number of lables

  tickn=['-40','-20','0','20','40']

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
  xyouts,0.5,1,'Neutral Wind m/s',/normal,font=0,charsize=1.1
  closedevice

  ;;;;;;;;plot grav;;;;;;;;;
  setdevice, folder+'grav_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=11

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=-grav[k,fixlt,i]
      cl=(cc+600)/1200*255
      if (cl le 0) THEN cl=1
      if (cl ge 255) THEN cl=254
      POLYFILL,xx,yy,COLOR=cl,/data
    ENDFOR  ;i
  ENDFOR ;k
 
  ; plot color table
  nlable=5; number of lables

  ;tickn=['-30','-15','0','15','30']
  ;tickn=['-5x10!U10','-2.5x10!U10','0','2.5x10!U10','5x10!U10']
  ;tickn=['-50','-25','0','25','50']
  tickn=['-600','-300','0','300','600']
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
  xyouts,0.5,1,'Gravity Term m/s',/normal,font=0,charsize=1.1
  closedevice

  ;;;;;;plot grad;;;;;;
  setdevice, folder+'grad_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=11

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=gradient[k,fixlt,i]/1e-5
      cl=(cc+20)/40*255
      if (cl le 0) THEN cl=1
      if (cl ge 255) THEN cl=254
      POLYFILL,xx,yy,COLOR=cl,/data
    ENDFOR  ;i
  ENDFOR ;k

  ; plot color table
  nlable=5; number of lables

  tickn=['-2x10!U-4','-1x10!U-4','0','1x10!U-4','2x10!U-4']
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
  xyouts,0.5,1,'KF m!U-1',/normal,font=0,charsize=1.1
  closedevice

  ;;;;;;;;plot recom;;;;;;;;;
  setdevice, folder+'recom_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=11

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=-recom[k,fixlt,i]/1e-5
      cl=(cc+600)/1200*255
      if (cl le 0) THEN cl=1
      if (cl ge 255) THEN cl=254
      POLYFILL,xx,yy,COLOR=cl,/data
    ENDFOR  ;i
  ENDFOR ;k

  ; plot color table
  nlable=5; number of lables

  ;tickn=['-30','-15','0','15','30']
  ;tickn=['-5x10!U10','-2.5x10!U10','0','2.5x10!U10','5x10!U10']
  ;tickn=['-50','-25','0','25','50']
  tickn=['-6x10!U-3','-3x10!U-3','0','3x10!U-3','6x10!U-3']
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
  xyouts,0.5,1,'Recombination s!U-1',/normal,font=0,charsize=1.1
  closedevice
endfor
end