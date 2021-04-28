pro mlon_smp_towang

mydir='/Users/wzihan/rainbow/'
datestr='20130518'
mapdir='\pngmap\'

staname='sask'

aele={i:0,j:0,elev:0.}
elearr=replicate(aele,256,256)
grd={lat:0.,lon:0.}
grdarr=replicate(grd,257,257)
cord={cenlat:0.,cenlon:0.,minlat:0.,minlon:0.,maxlat:0.,maxlon:0.}

openr, 1, mydir+'\'+staname+'asiskymap.gm5' ; Skymap file made by me, containing the pixel AACGM coordinates using 2015 IRGF epoch
readu, 1, elearr
readu, 1, cord
readu, 1, grdarr
close, 1

; latitdunal range to sample 
min_lat=56.5
max_lat=64.

lon_smp=-50. ; MLON meridian to sample
lonw=0.25 ; Longitudinal half-width to sample

;start and end of time of keogram
shr=4
smn=30
ehr=5
emn=30

cnt=((ehr-shr)*60+emn-smn+1)*10
utimg=shr+smn/60.+indgen(cnt)/10./60.

smpkeoarr=fltarr(4000,3,cnt)
smplatarr=fltarr(4000)

inflag=replicate(0,256,256)

restore, mydir+'\saskrainbow01.sav' ; rainbow image file 0430-0530 UT

for hrmn=shr*60+smn,ehr*60+emn do begin
 
 hr=hrmn/60
 mn=hrmn mod 60
 
 for m=0, 9 do begin
  l=(hr-shr)*600+(mn-smn)*10+m
  cntsmp=0
  for j=0,255 do begin
  for i=0,255 do begin
   ii=elearr[i,j].i
   jj=elearr[i,j].j

   trr=rgb_struct_combined[(hr-4)*600+(mn-30)*10+m].red[ii,jj]
   trg=rgb_struct_combined[(hr-4)*600+(mn-30)*10+m].green[ii,jj]
   trb=rgb_struct_combined[(hr-4)*600+(mn-30)*10+m].blue[ii,jj]
   
   if l eq 0 then begin
    mlat0=grdarr[jj,ii].lat
    mlat1=grdarr[jj,ii+1].lat
    mlat2=grdarr[jj+1,ii+1].lat
    mlat3=grdarr[jj+1,ii].lat
    mlon0=grdarr[jj,ii].lon
    mlon1=grdarr[jj,ii+1].lon
    mlon2=grdarr[jj+1,ii+1].lon
    mlon3=grdarr[jj+1,ii].lon

    lat=(mlat0+mlat1+mlat2+mlat3)/4.
    lon=(mlon0+mlon1+mlon2+mlon3)/4.
   
    lonc=lon_smp
    if lat gt min_lat and lat lt max_lat and lon gt lonc-lonw and lon lt lonc+lonw then begin
     inflag[ii,jj]=1
     smplatarr[cntsmp]=lat
     smpkeoarr[cntsmp,0,l]=trr
     smpkeoarr[cntsmp,1,l]=trg
     smpkeoarr[cntsmp,2,l]=trb
     cntsmp++
    endif
   endif else begin
    if inflag[ii,jj] eq 1 then begin
      smpkeoarr[cntsmp,0,l]=trr
      smpkeoarr[cntsmp,1,l]=trg
      smpkeoarr[cntsmp,2,l]=trb
      cntsmp++
    endif
   endelse
  endfor
  endfor
 endfor
endfor

save, utimg,lon_smp,min_lat,max_lat,smplatarr,smpkeoarr,cntsmp,filename=mydir+'\keo_smp.sav'
print, 'finish'
end
