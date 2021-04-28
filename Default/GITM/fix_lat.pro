
FOLDER='/USERS/WZIHAN/SIMULATIONS/'
allFILELIST = FINDFILE(FOLDER+'data/3DALL_t17090*.bin')

north = 1

loadct,34

nfiles = n_elements(allfilelist)

ff = 0
lf = nfiles-1

nw=fltarr(nfiles,3,3)
conv=nw
tot=nw

device, decomposed=0

!p.multi=[0,1,1,0,0]

ilat=[80]

for ifile = ff, lf do begin

  filename = allfilelist(iFile)

  ;read 3dall (ne)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  for ii=0,n_elements(ilat)-1 do begin

    lon = reform(data(0,*,0,0))*180/!pi
    lat=  reform(data(1,0,*,0))*180/!pi
    alt= reform(data(2,0,0,*))/1e3

    n= reform(data(34,*,ilat,*))
    
    p = strpos(filename,'.bin')
    ;psfile = strmid(psfile,0,p)
    tp=strpos(filename,'_t')

    figfile= folder+'plot_new/meridional/ne_ja_'+strmid(filename,tp+2,13)

    setdevice, figfile+'.ps'

    plot,lon,alt,/nodata,xstyle=1,ystyle=1,xcharsize=1.5,YTITLE='Altitude (km)',xticklen=-0.03,xticks=12,title=strmid(filename,tp+2,13),xtitle='Longitude'

    FOR k=1, 181 DO BEGIN;lon  
      x1=lon[k]
      x2=lon[k+1]
      x3=lon[k+1]
      x4=lon[k]
      xx=[x1,x2,x3,x4];

      FOR i=0, 52 DO BEGIN         ; altitude
        yy=[alt[i],alt[i],alt[i+1],alt[i+1]]
        ;print,n(k,i)
        cc=alog10(n(k,i));n(k,i)
        ;print,cc
        cl=(cc-10)/2.5*255.;cc/100*127+127
        ;print,cl
        if (cl le 0) THEN cl=0
        if (cl ge 255) THEN cl=254
        POLYFILL,xx,yy,COLOR=cl,/data
      ENDFOR  ;i
    ENDFOR ;k

    post_ct=[.99,0.83,1.05,.98]
    ; plot color table
    nlable=6; number of lables
    ;tickn=['-100','-50','0','50','100']
    tickn=['10!U10.0','10!U10.5','10!U11.0','10!U11.5','10!U12.0','10!U12.5']
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

    closedevice
  endfor
endfor

end
