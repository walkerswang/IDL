wc=ask('Root folder:',wc)
root=ask('Figure root:',root)

allFILELIST = FINDFILE(wc+'3DALL*.bin')
ionFILELIST = FINDFILE(wc+'3DION*.bin')
nfiles = n_elements(allfilelist)

;28 means altitude grids from 216 km to 615 km
gam=fltarr(nfiles,24,28) ;growth rate
pedere=fltarr(nfiles,24,28) ;pederson conductance in e region
pederf=fltarr(nfiles,24,28) ;pederson conductance in e region
ebdrift=fltarr(nfiles,24,28) 
neutralwind=fltarr(nfiles,24,28)
grav=fltarr(nfiles,24,28) ;g/mu
gradient=fltarr(nfiles,24,28) 
recom=fltarr(nfiles,24,28); recombination rate

for ifile=0,nfiles-1 do begin

filenameion = ionfilelist(ifile)
filenameall = allfilelist(ifile)
;read 3dall (ne)
read_thermosphere_file, filenameion, nvars, nalts, nlats, nlons,vars,data, $
  nBLKlat, nBLKlon, nBLK, iTime, Version

read_thermosphere_file, filenameall, nvars, nalts, nlats, nlons,vars,dataall, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

;prepare the time for geopack
strtime=strtrim(string(itime(0)),1)+'-'+strtrim(string(itime(1)),1)+'-'+strtrim(string(itime(2)),1)
atime=fix(date_conv(strtime,'V'))

print,filenameion

;GEOPACK does not work right now. 05-20-2021 
;GEOPACK_RECALC,itime[0],atime[1],itime[3],itime[4],0

lon = reform(data(0,*,0,0))*180/!pi
lat=  reform(data(1,0,*,0))*180/!pi
alt=  data(2,0,0,*)

mlat= reform(data(22,*,*,*))
mlon= reform(data(23,*,*,*))
bmag= reform(data(27,*,*,*))
dx=1e3 ;km
re=6371e3

;find the lon for a fixed lt. Here is 19.5
utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
utime = utime(0)

for fixlt=0,23 do begin
localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0
f=min(abs(localtime-fixlt),ilon)

if ilon lt 2 then ilon=ilon+180
if ilon gt 181 then ilon=ilon-180

for apex=25, 52 do begin ;from 216 km
  ialt=apex ; altitude of apex
  h=apex
  feq=min(abs(mlat(ilon,*,ialt)),ilat) ;find the magnetic equator
  
  ;calculate vp
  ee=data(29,ilon,ilat,ialt)
  en=data(30,ilon,ilat,ialt)
  eu=data(31,ilon,ilat,ialt)
  be=data(24,ilon,ilat,ialt)
  bn=data(25,ilon,ilat,ialt)
  bu=data(26,ilon,ilat,ialt)

  edotb=(be*ee+bn*en+bu*eu)/bmag[ilon,ilat,ialt]
  ee_res=ee-edotb*be/bmag[ilon,ilat,ialt]
  en_res=en-edotb*bn/bmag[ilon,ilat,ialt]
  eu_res=eu-edotb*bu/bmag[ilon,ilat,ialt]
  ephi=sqrt(ee_res^2+en_res^2)*signum(ee_res)

  trace_field_line,ilon,ilat,ialt,data,l,npoint,alon,alat,aalt
  
  vol=0
  sigp_sf=0
  sigp_se=0
  sigp_stot=0
  ns=0
  b0=bmag[ilon,ilat,ialt];31100e-9
  n0f=0
  n0e=0
  ulp=0
  rt=0

  fregion=min(abs(alt[aalt]-2e5),fp1); find boundary of e and f region (200km)
  fp2=npoint-fp1
  
  if fp1 gt fp2 then begin
    tmp=fp1
    fp1=fp2
    fp2=tmp
  endif
  
  ;if alt[aalt[fp]] lt 200 then begin
  ;  fp=fp-1
  ;endif

  ;if fp lt 0 then begin
  ;  fp=0
  ;endif

  ;calculate parameters in f region
  for i=fp1,fp2 do begin
    den_e=data(12,alon[i],alat[i],aalt[i])
    b=bmag(alon[i],alat[i],aalt[i])
    den_n2=dataall(6,alon[i],alat[i],aalt[i])
    den_o2=dataall(5,alon[i],alat[i],aalt[i])
    den_o=dataall(4,alon[i],alat[i],aalt[i])
    ti=dataall(36,alon[i],alat[i],aalt[i])
    te=dataall(35,alon[i],alat[i],aalt[i])
    tn=dataall(15,alon[i],alat[i],aalt[i])
    op=data(3,alon[i],alat[i],aalt[i])
    o2p=data(4,alon[i],alat[i],aalt[i])
    nop=data(7,alon[i],alat[i],aalt[i])
    mi=(op*16+o2p*32+nop*30)/(op+o2p+nop)*1.67e-27
    tot=op+o2p+nop
    rion=[op/tot,o2p/tot,nop/tot] ;1:O+,2:O2+,3:NO+
    
    ;convert to geomagnetic coordinate
    ;GEOPACK_SPHCAR, 1+alt[aalt[i]]/re, !pi/2-lat[alat[i]]/180*!pi,lon[alon[i]]/180*!pi , geox, geoy, geoz, /to_rect
    ;geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
    ;GEOPACK_SPHCAR, magx, magy, magz ,r, theta , p, /to_sphere
    ;GEOPACK_SPHCAR, 1+alt[aalt[i-1]]/re, !pi/2-lat[alat[i-1]]/180*!pi,lon[alon[i-1]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta_p , p, /to_sphere ;sin(phi)=cos(theta)
;    
;    phi=abs(!pi/2-theta)
;    phi_p=abs(!pi/2-theta_p)

    magloc=geo2mag([lat[alat[i]],lon[alon[i]]])
    magloc_p=geo2mag([lat[alat[i-1]],lon[alon[i-1]]])

    phi=magloc[0]/180.0*!pi
    phi_p=magloc_p[0]/180.0*!pi
    
    ;calculate flux tube volume
    ll=lat[alat[i]] ;current geo latitude
    dlon=(6371*2*!pi*cos(ll*!pi/180))/360*abs(lon[alon[i]]-lon[alon[i-1]])
    dlat=(6371*2*!pi)/360*abs(lat[alat[i]]-lat[alat[i-1]])
    dalt=abs(alt[aalt[i]]-alt[aalt[i-1]])
    dl=sqrt(dalt^2+dlon^2+dlat^2)
    vol=vol+1e-9/b*dl
    
    ;calculate tube integrated conductance
    sigma=calculate_conductivity(den_e,den_N2,den_O2,den_O,Ti,Te,Tn,Rion,B)
    sigmah=sigma[1]
    sigmap=sigma[0]
    sigp_sf=sigp_sf+re*l*sigmap*abs(phi_p-phi)*cos(phi)
    
    ;sigp_int=sigp_int+dl*sigmap
    
    ;calculate n~
    ns=ns+re*l*den_e*(1-sin(phi)^2)^3/(1+3*sin(phi)^2)*abs(phi_p-phi)*cos(phi)
    
    ;calculate n0
    n0f=n0f+re*l*den_e*(1-sin(phi)^2)^3*abs(phi_p-phi)*cos(phi)
  endfor

  ;calculate mu_eff
  mi=16*1.67e-27
  veff=b0^2*sigp_sf/(mi*l^6*ns)

  ;calculate in e region
  for i=1, fp1-1 do begin
    den_e=data(12,alon[i],alat[i],aalt[i])
    b=bmag(alon[i],alat[i],aalt[i])
    den_n2=dataall(6,alon[i],alat[i],aalt[i])
    den_o2=dataall(5,alon[i],alat[i],aalt[i])
    den_o=dataall(4,alon[i],alat[i],aalt[i])
    ti=dataall(36,alon[i],alat[i],aalt[i])
    te=dataall(35,alon[i],alat[i],aalt[i])
    tn=dataall(15,alon[i],alat[i],aalt[i])
    op=data(3,alon[i],alat[i],aalt[i])
    o2p=data(4,alon[i],alat[i],aalt[i])
    nop=data(7,alon[i],alat[i],aalt[i])
    tot=op+o2p+nop
    rion=[op/tot,o2p/tot,nop/tot] ;1:O+,2:O2+,3:NO+

    ;convert to geomagnetic coordinate
;    GEOPACK_SPHCAR, 1+alt[aalt[i]]/re, !pi/2-lat[alat[i]]/180*!pi,lon[alon[i]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta , p, /to_sphere
;    GEOPACK_SPHCAR, 1+alt[aalt[i-1]]/re, !pi/2-lat[alat[i-1]]/180*!pi,lon[alon[i-1]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta_p , p, /to_sphere
;    phi=abs(!pi/2-theta)
;    phi_p=abs(!pi/2-theta_p)

    magloc=geo2mag([lat[alat[i]],lon[alon[i]]])
    magloc_p=geo2mag([lat[alat[i-1]],lon[alon[i-1]]])

    phi=magloc[0]/180.0*!pi
    phi_p=magloc_p[0]/180.0*!pi
  
    ;calculate tube integrated conductance
    sigma=calculate_conductivity(den_e,den_N2,den_O2,den_O,Ti,Te,Tn,Rion,B)
    sigmah=sigma[1]
    sigmap=sigma[0]
    sigp_se=sigp_se+re*l*sigmap*abs(phi_p-phi)*cos(phi)
    ;sigp_int=sigp_int+dl*sigmap
  
    ;calculate n0 in e region
    n0e=n0e+re*l*den_e*(1-sin(phi)^2)^3*abs(phi_p-phi)*cos(phi)
  endfor
  
  ;continue to calculate in e region
  for i=fp2+1, npoint-1 do begin
    den_e=data(12,alon[i],alat[i],aalt[i])
    b=bmag(alon[i],alat[i],aalt[i])
    den_n2=dataall(6,alon[i],alat[i],aalt[i])
    den_o2=dataall(5,alon[i],alat[i],aalt[i])
    den_o=dataall(4,alon[i],alat[i],aalt[i])
    ti=dataall(36,alon[i],alat[i],aalt[i])
    te=dataall(35,alon[i],alat[i],aalt[i])
    tn=dataall(15,alon[i],alat[i],aalt[i])
    op=data(3,alon[i],alat[i],aalt[i])
    o2p=data(4,alon[i],alat[i],aalt[i])
    nop=data(7,alon[i],alat[i],aalt[i])
    tot=op+o2p+nop
    rion=[op/tot,o2p/tot,nop/tot] ;1:O+,2:O2+,3:NO+

    ;convert to geomagnetic coordinate
;    GEOPACK_SPHCAR, 1+alt[aalt[i]]/re, !pi/2-lat[alat[i]]/180*!pi,lon[alon[i]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta , p, /to_sphere
;    GEOPACK_SPHCAR, 1+alt[aalt[i-1]]/re, !pi/2-lat[alat[i-1]]/180*!pi,lon[alon[i-1]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta_p , p, /to_sphere
;    phi=abs(!pi/2-theta)
;    phi_p=abs(!pi/2-theta_p)

    magloc=geo2mag([lat[alat[i]],lon[alon[i]]])
    magloc_p=geo2mag([lat[alat[i-1]],lon[alon[i-1]]])

    phi=magloc[0]/180.0*!pi
    phi_p=magloc_p[0]/180.0*!pi

    ;calculate tube integrated conductance
    sigma=calculate_conductivity(den_e,den_N2,den_O2,den_O,Ti,Te,Tn,Rion,B)
    sigmah=sigma[1]
    sigmap=sigma[0]
    sigp_se=sigp_se+re*l*sigmap*abs(phi_p-phi)*cos(phi)
    ;sigp_int=sigp_int+dl*sigmap

    ;calculate n0 in e region
    n0e=n0e+re*l*den_e*(1-sin(phi)^2)^3*abs(phi_p-phi)*cos(phi)
  endfor
  
  ;calculate in both e and f region
  n0_tot=n0e+n0f
  sigp_stot=sigp_se+sigp_sf
  
  for i=1,npoint-1 do begin
    den_e=data(12,alon[i],alat[i],aalt[i])
    be=data(24,alon[i],alat[i],aalt[i])
    bn=data(25,alon[i],alat[i],aalt[i])
    bu=data(26,alon[i],alat[i],aalt[i])
    ve=dataall(16,alon[i],alat[i],aalt[i])
    vn=dataall(17,alon[i],alat[i],aalt[i])
    vu=dataall(18,alon[i],alat[i],aalt[i])
    b=bmag(alon[i],alat[i],aalt[i])
    den_n2=dataall(6,alon[i],alat[i],aalt[i])
    den_o2=dataall(5,alon[i],alat[i],aalt[i])
    den_o=dataall(4,alon[i],alat[i],aalt[i])
    ti=dataall(36,alon[i],alat[i],aalt[i])
    te=dataall(35,alon[i],alat[i],aalt[i])
    tn=dataall(15,alon[i],alat[i],aalt[i])
    op=data(3,alon[i],alat[i],aalt[i])
    o2p=data(4,alon[i],alat[i],aalt[i])
    nop=data(7,alon[i],alat[i],aalt[i])
    n2p=data(5,alon[i],alat[i],aalt[i])
    tot=op+o2p+nop
    rion=[op/tot,o2p/tot,nop/tot] ;1:O+,2:O2+,3:NO+

    ;convert to geomagnetic coordinate
;    GEOPACK_SPHCAR, 1+alt[aalt[i]]/re, !pi/2-lat[alat[i]]/180*!pi,lon[alon[i]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta , p, /to_sphere
;    GEOPACK_SPHCAR, 1+alt[aalt[i-1]]/re, !pi/2-lat[alat[i-1]]/180*!pi,lon[alon[i-1]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta_p , p, /to_sphere
;    phi=abs(!pi/2-theta)
;    phi_p=abs(!pi/2-theta_p)

    magloc=geo2mag([lat[alat[i]],lon[alon[i]]])
    magloc_p=geo2mag([lat[alat[i-1]],lon[alon[i-1]]])

    phi=magloc[0]/180.0*!pi
    phi_p=magloc_p[0]/180.0*!pi
  
    ;calculate tube integrated conductance
    sigma=calculate_conductivity(den_e,den_N2,den_O2,den_O,Ti,Te,Tn,Rion,B)
    sigmah=sigma[1]
    sigmap=sigma[0]
  
    ;if alt[aalt[i]] gt 1.25e5 then begin
    ;calculate uq
    lhat=[be/b,bn/b,bu/b]
    vdotb=lhat(0)*ve+lhat(1)*vn+lhat(2)*vu
    
    shat=fltarr(3,1)
    shat[0]=sqrt(1/(1+lhat(0)^2/lhat(1)^2))
    shat[1]=-lhat(0)/lhat(1)*shat[0]
    shat[2]=0
    
    qhat=crossp(shat,lhat)
    
    uq=ve*qhat(0)+vn*qhat(1)+vu*qhat(2)
    ulp=ulp+sigmap*uq*(1+3*sin(phi)^2)^0.5/(1-sin(phi)^2)^1.5*abs(phi_p-phi)*cos(phi)*re*l/sigp_stot
    ;endif
    ;
    ;calculate recombination rate
    rt=rt+re*l/n0_tot*den_e*2e-13*(o2p+nop+n2p)*(1-sin(phi)^2)^3*abs(phi_p-phi)*cos(phi)
  endfor
  
  vp=ephi*l^3/b0 ;calculate vertical drift
  
  ;calculate kf gradient
  
  ;one flux tube below
  ialt=h
  trace_field_line,ilon,ilat,ialt-1,data,ll,npoint,alon,alat,aalt
  n0fl=0
  
  fregion=min(abs(alt[aalt]-2e5),fp1)
  fp2=npoint-fp1
  
  if fp1 gt fp2 then begin
    tmp=fp1
    fp1=fp2
    fp2=tmp
  endif

  for i=fp1,fp2 do begin
    den_e=data(12,alon[i],alat[i],aalt[i])
    b=bmag(alon[i],alat[i],aalt[i])
  
    ;convert to geomagnetic coordinate
;    GEOPACK_SPHCAR, 1+alt[aalt[i]]/re, !pi/2-lat[alat[i]]/180*!pi,lon[alon[i]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta , p, /to_sphere
;    GEOPACK_SPHCAR, 1+alt[aalt[i-1]]/re, !pi/2-lat[alat[i-1]]/180*!pi,lon[alon[i-1]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta_p , p, /to_sphere
;
;    phi=abs(!pi/2-theta)
;    phi_p=abs(!pi/2-theta_p)

    magloc=geo2mag([lat[alat[i]],lon[alon[i]]])
    magloc_p=geo2mag([lat[alat[i-1]],lon[alon[i-1]]])

    phi=magloc[0]/180.0*!pi
    phi_p=magloc_p[0]/180.0*!pi
  
    ;calculate n0f
    n0fl=n0fl+re*l*den_e*(1-sin(phi)^2)^3*abs(phi_p-phi)*cos(phi)
  endfor

   ;one flux tube above
  ialt=h
  trace_field_line,ilon,ilat,ialt+1,data,lh,npoint,alon,alat,aalt
  n0fh=0
  
  fregion=min(abs(alt[aalt]-2e5),fp1)
  fp2=npoint-fp1
  
  if fp1 gt fp2 then begin
    tmp=fp1
    fp1=fp2
    fp2=tmp
  endif
  
  for i=fp1,fp2 do begin
    den_e=data(12,alon[i],alat[i],aalt[i])
    b=bmag(alon[i],alat[i],aalt[i])

    ;convert to geomagnetic coordinate
;    GEOPACK_SPHCAR, 1+alt[aalt[i]]/re, !pi/2-lat[alat[i]]/180*!pi,lon[alon[i]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta , p, /to_sphere
;    GEOPACK_SPHCAR, 1+alt[aalt[i-1]]/re, !pi/2-lat[alat[i-1]]/180*!pi,lon[alon[i-1]]/180*!pi , geox, geoy, geoz, /to_rect
;    geopack_conv_coord, geox, geoy, geoz, magx, magy, magz, /FROM_GEO, /TO_MAG
;    GEOPACK_SPHCAR, magx, magy, magz ,r, theta_p , p, /to_sphere
;    phi=abs(!pi/2-theta)
;    phi_p=abs(!pi/2-theta_p)

    magloc=geo2mag([lat[alat[i]],lon[alon[i]]])
    magloc_p=geo2mag([lat[alat[i-1]],lon[alon[i-1]]])

    phi=magloc[0]/180.0*!pi/2
    phi_p=magloc_p[0]/180.0*!pi/2
  
    ;calculate n~
    n0fh=n0fh+re*l*den_e*(1-sin(phi)^2)^3*abs(phi_p-phi)*cos(phi)
  endfor

  ;calculate kf
  kf=1/(re*l^3*n0f)*(lh^3*n0fh-ll^3*n0fl)/(lh-ll)
  g=-9.8
  gee=g/l^2
  
  gam[ifile,fixlt,apex-25]=sigp_sf/(sigp_sf+sigp_se)*(vp-ulp-gee/veff)*kf-rt
  pedere[ifile,fixlt,apex-25]=sigp_se
  pederf[ifile,fixlt,apex-25]=sigp_sf
  ebdrift[ifile,fixlt,apex-25]=vp
  neutralwind[ifile,fixlt,apex-25]=ulp
  grav[ifile,fixlt,apex-25]=gee/veff
  gradient[ifile,fixlt,apex-25]=kf
  recom[ifile,fixlt,apex-25]=rt
endfor
endfor
endfor

save,gam,pedere,pederf,ebdrift,neutralwind,grav,gradient,recom,alt,filename=root+'gamma.sav'

end