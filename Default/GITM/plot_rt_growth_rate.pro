restore,folder+'gamma.sav'

makect,'bwr'
for fixlt=0,23 do begin

  ;;;;;;;;plot gamma;;;;;;;;;
  setdevice, folder+'plot_new/rt/gamma_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=5

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=gam[k,fixlt,i]/1e-5
      cl=(cc+400)/800*254
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
  ;tickn=['-50','-25','0','25','50']
  tickn=['-400','-200','0','200','400']
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

  ;;;;;;;;plot ebdrift;;;;;;;;;
  setdevice, folder+'plot_new/rt/ebdrift_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=5

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=ebdrift[k,fixlt,i]
      cl=(cc+100)/200*254
      if (cl le 0) THEN cl=0
      if (cl ge 255) THEN cl=255
      POLYFILL,xx,yy,COLOR=cl,/data
    ENDFOR  ;i
  ENDFOR ;k

  post_ct=[.99,0.86,1.05,1]
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
  ;xyouts,1.06,0.85,'Ne (m!U-3)',/normal,font=0,ORIENTATION=90,charsize=1.1
  closedevice

  ;;;;;;;;plot neutralwind;;;;;;;;;

  setdevice, folder+'plot_new/rt/nw_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=5

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=neutralwind[k,fixlt,i]
      cl=(cc+40)/80*254
      if (cl le 0) THEN cl=0
      if (cl ge 255) THEN cl=255
      POLYFILL,xx,yy,COLOR=cl,/data
    ENDFOR  ;i
  ENDFOR ;k

  post_ct=[.99,0.86,1.05,1]
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
  ;xyouts,1.06,0.85,'Ne (m!U-3)',/normal,font=0,ORIENTATION=90,charsize=1.1
  closedevice

  ;;;;;;;;plot grav;;;;;;;;;
  setdevice, folder+'plot_new/rt/grav_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=5

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=grav[k,fixlt,i]
      cl=(cc+400)/400*254
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
  ;tickn=['-50','-25','0','25','50']
  tickn=['-400','-300','-200','-100','0']
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

  ;;;;;;plot grad;;;;;;
  setdevice, folder+'plot_new/rt/grad_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=5

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=gradient[k,fixlt,i]/1e-5
      cl=(cc+10)/20*254
      if (cl le 0) THEN cl=0
      if (cl ge 255) THEN cl=255
      POLYFILL,xx,yy,COLOR=cl,/data
    ENDFOR  ;i
  ENDFOR ;k

  post_ct=[.99,0.86,1.05,1]
  ; plot color table
  nlable=5; number of lables

  tickn=['-10','-5','0','5','10']
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

  ;;;;;;;;plot recom;;;;;;;;;
  setdevice, folder+'plot_new/rt/recom_'+strtrim(fixlt,1)+'.ps'

  device,decomposed=0

  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  plot,time[0:nfiles],alt[25:52],/nodata,xstyle=1,ystyle=1,xcharsize=1.5,xtickformat='LABEL_DATE',$
    xminor=6,yminor=2,font=0,YTITLE='Altitude (m)',$
    xticklen=-0.03,yticklen=-0.01,xticks=5

  time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,9,2017,0,0,0),UNITS='Minutes', STEP_SIZE=5)

  FOR k=0, nfiles-1 DO BEGIN;  UT
    x1=time[k]
    x2=time[k+1]
    x3=time[k+1]
    x4=time[k]
    xx=[x1,x2,x3,x4];

    FOR i=0, 27  DO BEGIN         ; altitude
      yy=[alt[25+i],alt[25+i],alt[25+i+1],alt[25+i+1]]
      cc=recom[k,fixlt,i]/1e-5
      cl=(cc+400)/800*254
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
  ;tickn=['-50','-25','0','25','50']
  tickn=['-400','-200','0','200','400']
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
endfor
end