
FOLDER='/USERS/WZIHAN/SIMULATIONS/'
allFILELIST = FINDFILE(FOLDER+'3DALL_t1709*.bin')
ionFILELIST = FINDFILE(FOLDER+'3DION_t1709*.bin')

north = 1

nfiles = n_elements(allfilelist)
g=9.8

ff = 0
lf = nfiles-1

nw=fltarr(nfiles,1,1)
conv=nw
tot=nw

!p.multi=[0,1,1,0,0]

ltime=[14]
lat=[67]
;for ifile = ff, lf do begin
;
;    filename = allfilelist(iFile)
;
;    ;read 3dall (neutral wind)
;    read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
;      nBLKlat, nBLKlon, nBLK, iTime, Version
;      
;    filename = ionfilelist(iFile)
;    read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,dataion, $
;      nBLKlat, nBLKlon, nBLK, iTime, Version
;    for ii=0,0 do begin
;      for jj=0,0 do begin
;        ilat=90+lat[jj]
;        ialt=24
;  
;        lon = (reform(data(0,*,ilat,0))*180/!pi + 360.0) mod 360.0
;        utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
;        utime = utime(0)
;
;        localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0
;
;        f=min(abs(localtime-ltime[ii]),ilon)
;
;        ;16. V!Dn!N(east)
;        ;17. V!Dn!N(north)
;        ;18. V!Dn!N(up)
;        vn_east = reform(data(16,ilon,ilat,ialt))
;        vn_north = reform(data(17,ilon,ilat,ialt))
;        vn_up = reform(data(18,ilon,ilat,ialt))
;        n= reform(data(34,ilon,ilat,*))
;        Alt=reform(data(2,0,0,*))
;        ;read 3dion (collisional freq and electric field and magnetic field)
;
;
;        ;24. B.F.East
;        ;25. B.F.North
;        ;26. B.F.Vertical
;        ;27. B.F.Magnitude
;        ;28. Potential
;        ;29. E.F.East
;        ;30. E.F.North
;        ;31. E.F.Vertical
;        ;32. E.F.Magnitude
;        ;33. INCollisionFreq
;        b_east = reform(dataion(24,ilon,ilat,ialt))
;        b_north = reform(dataion(25,ilon,ilat,ialt))
;        b_up = reform(dataion(26,ilon,ilat,ialt))
;        b_mag = reform(dataion(27,ilon,ilat,ialt))
;        e_east = reform(dataion(29,ilon,ilat,ialt))
;        e_north = reform(dataion(30,ilon,ilat,ialt))
;        e_up = reform(dataion(31,ilon,ilat,ialt))
;
;        mu_in = reform(data(33,ilon,ialt,*))
;
;        mu_in_ave=fltarr(nalts)
;        ;calculate height averaged mu_in
;        mu_in_total=0.0
;        n_total=0.0
;  
;        for iAlt= 0, nAlts-2 do begin
;          dAlt= Alt(iAlt+1)-Alt(iAlt)
;          n_total = n_total + dAlt*n(iAlt)
;          mu_in_total = mu_in_total + dAlt*n(iAlt)*mu_in(iAlt)
;        endfor
;  
;        mu_in_ave=mu_in_total/n_total
;
;        I=asin(-b_up/b_mag)
;        D=atan(b_east/b_north)
;        vn_dot_b=vn_north*b_north+vn_east*b_east+vn_up*b_up
;        vi_nw=sin(I)*(-vn_dot_b/b_mag-g/mu_in_ave*sin(I)) ;-g/mu_in[iLon,iLat]*sin(I)
;        vi_conv=(e_east*b_north-e_north*b_east)/b_mag^2
;
;        nw[ifile,ii,jj] =vi_nw
;        conv[ifile,ii,jj]=vi_conv
;        tot[ifile,ii,jj]=vi_conv+vi_nw
;        endfor
;    endfor
;endfor
;
;save,nw,conv,tot,filename='compare.sav'

loadct,40
restore,folder+'plot/nw_effect/compare.sav'
for ii=0,0 do begin
  for jj=0,0 do begin
    figfile= folder+'plot/nw_effect/nw_lat'+strtrim(lat[jj],2)+'_lt'+strtrim(ltime[ii],2)

    setdevice, figfile+'.ps','p',5

    device, decomposed=0
    
    dummy = LABEL_DATE(DATE_FORMAT=['%H:%I'])

    ; Generate the Date/Time data
    time = TIMEGEN(START=JULDAY(9,7,2017,18,0,0), FINAL=JULDAY(9,8,2017,5,0,0),UNITS='Minutes', STEP_SIZE=5)

    plot,time, nw[*,ii,jj], thick = 3, xstyle = 1, ystyle = 1,XTICKUNITS = ['Hour'], XTICKFORMAT='LABEL_DATE',yrange=[-50,100],color=1,ytitle='Ion vertival velocity (m/s)'
    oplot, time, conv[*,ii,jj], thick = 3, color=125
    oplot, time, tot[*,ii,jj], thick = 3, color=230

    closedevice
    endfor
endfor

end
