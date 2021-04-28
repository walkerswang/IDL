GEOPACK_RECALC,2017,250,23,0,0

lat=58.5
theta=(90-lat)/180.0*!pi
lon=235.0
phi=lon/180.0*!pi
r=1+350/6371.0

GEOPACK_SPHCAR, r, theta , phi, geox, geoy, geoz, /to_rect

geopack_conv_coord, geox, geoy, geoz, gsmx, gsmy, gsmz, /FROM_GEO, /TO_GSM

IOPT=6
DIR=1

FLINE=MAKE_ARRAY(100,3)

GEOPACK_TRACE, gsmx, gsmy, gsmz, DIR, IOPT, xf, yf, zf,/T89,FLINE=FLINE

s=size(fline)
rr=MAKE_ARRAY(S[1],1)

FOR I=0,s[1]-1 do begin
    rr[i]=sqrt(fline[i,0]^2+fline[i,1]^2+fline[i,2]^2)
endfor

delr=rr-3

minr=min(abs(delr),index)

newr=sqrt(fline[index,0]^2+fline[index,1]^2+fline[index,2]^2)
print,newr

print,fline[index,*]/newr*3.0
end

;PATCH LOCATED AT 63.5 231. TRACE TO 3RE GSM=(2.0071285 0.59726887 2.1481865)
;
;BASE LOCATED AT 50.5 247. TRACE TO 3RE GSM=(2.0379669 1.8818920 1.1424418)
;
;Valley locaterd at 2.1493894 0.99089700 1.8434338