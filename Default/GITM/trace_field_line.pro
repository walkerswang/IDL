pro trace_field_line,apexlon,apexlat,apexalt,data,l,npoint,alon,alat,aalt

  mlat=reform(data(22,*,*,*))
  mlon=reform(data(23,*,*,*))
  lon = reform(data(0,*,0,0))*180/!pi
  lat=  reform(data(1,0,*,0))*180/!pi
  alt=  data(2,0,0,*)
  
  lonp=mlon(apexlon,apexlat,apexalt)
  ;if sp eq 'n' then begin
  latp=abs(mlat(apexlon,apexlat,apexalt))
  ;endif else begin
  ;  latp=-abs(mlat(apexlon,apexlat,apexalt))
  ;endelse

  npoint=0
  alat=fltarr(184,1)
  alon=fltarr(184,1)
  aalt=fltarr(184,1)

  for jj=apexlat-15,apexlat+15 do begin
    altto=0
    lonto=0
    nnn=0
    for ii=apexlon-2,apexlon+2 do begin
      for kk=0,apexalt do begin
        if ((abs(mlat(ii,jj,kk)-latp) lt 1) || (abs(mlat(ii,jj,kk)+latp) lt 1)) && (abs(mlon(ii,jj,kk)-lonp) lt 1) then begin
          altto=altto+alt(kk)
          lonto=lonto+lon(ii)
          nnn=nnn+1
        endif
      endfor
    endfor
    if nnn gt 0 then begin
      ;p=plot([lat(jj),lat(jj)-0.01],[altto/nnn,altto/nnn-1],/overplot,symbol='+')
      feq=min(abs(altto/nnn-alt),indalt)
      feq=min(abs(lonto/nnn-lon),indlon)
      alat[npoint]=jj
      aalt[npoint]=indalt
      alon[npoint]=indlon
      npoint=npoint+1
    endif
  endfor
 
l=1/cos(latp/180*!pi)^2 ;l shell

end