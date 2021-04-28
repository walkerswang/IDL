
FOLDER='/USERS/WZIHAN/SIMULATIONS/'
allFILELIST = FINDFILE(FOLDER+'data/3DALL_t17090*.bin')

north = 1

loadct,34

nfiles = n_elements(allfilelist)

ff = 36
lf = nfiles-1

nw=fltarr(nfiles,3,3)
conv=nw
tot=nw

device, decomposed=0

!p.multi=[0,1,1,0,0]

ltime=[20]

filename = allfilelist(ff)

;read 3dall (ne)
read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
  nBLKlat, nBLKlon, nBLK, iTime, Version

lon = (reform(data(0,*,0,0))*180/!pi + 360.0) mod 360.0

utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
utime = utime(0)

localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0

f=min(abs(localtime-ltime[0]),ilon)

n_prev= reform(data(34,ilon,*,*))

for ifile = ff+1, lf do begin

  filename = allfilelist(iFile)

  ;read 3dall (ne)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  for ii=0,n_elements(ltime)-1 do begin

    lon = (reform(data(0,*,0,0))*180/!pi + 360.0) mod 360.0
    lat=  reform(data(1,0,*,0))*180/!pi
    alt= reform(data(2,0,0,*))/1e3

    utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
    utime = utime(0)

    localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0

    f=min(abs(localtime-ltime[ii]),ilon)

    n= reform(data(34,ilon,*,*))

    p = strpos(filename,'.bin')
    ;psfile = strmid(psfile,0,p)
    tp=strpos(filename,'_t')

    figfile= folder+'plot_new/meridional/ne_lt_north_diff'+strtrim(ltime[ii],2)+'_'+strmid(filename,tp+2,13)

    setdevice, figfile+'.ps'

    plot,lat[91:182],alt,/nodata,xstyle=1,ystyle=1,xcharsize=1.5,YTITLE='Altitude (km)',xticklen=-0.03,xticks=18,title=strmid(filename,tp+2,13)

    FOR k=92, 181 DO BEGIN;  UT
      x1=lat[k]
      x2=lat[k+1]
      x3=lat[k+1]
      x4=lat[k]
      xx=[x1,x2,x3,x4];

      FOR i=0, 52 DO BEGIN         ; altitude
        yy=[alt[i],alt[i],alt[i+1],alt[i+1]]
        cc=n(k,i)-n_prev(k,i)
        cl=cc/2e10*127+127.
        if (cl le 0) THEN cl=0
        if (cl ge 255) THEN cl=254
        POLYFILL,xx,yy,COLOR=cl,/data
      ENDFOR  ;i
    ENDFOR ;k

    n_prev=n
    
    post_ct=[.99,0.83,1.05,.98]
    ; plot color table
    nlable=5; number of lables
    tickn=['-2x10!U10','-10!U10','0','10!U10','2x10!U10']
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
