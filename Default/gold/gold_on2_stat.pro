ave_on2=MAKE_ARRAY(360, 180, 12, /float, VALUE = 0.0) ;lt(0,360)/15, theta(0,180)
cnt=MAKE_ARRAY(360, 180, 12, /integer, VALUE = 0)

;ave_on2_mag=MAKE_ARRAY(360, 180, 12, /float, VALUE = 0.0) ;mlt(0,360)/15, mag theta(0,180)
;cnt_mag=MAKE_ARRAY(360, 180, 12, /integer, VALUE = 0)

nmax=1000000
lati=MAKE_ARRAY(nmax, /float, VALUE = 0.0)
longi=MAKE_ARRAY(nmax, /float, VALUE = 0.0)
data=MAKE_ARRAY(nmax, /float, VALUE = 0.0)
datetime=MAKE_ARRAY(nmax, /string)
count=0UL
noutput=0

filelist = file_search('/Users/wzihan/data/gold/on2/*.nc')
nFiles = n_elements(filelist)

for iFile = 0, nfiles-1 do begin
  
  READ_GOLD_NCDF, filelist[ifile], test_data
  print,filelist[ifile]
  nscan=size(test_data.dqi,dimension=1)
  nscan=nscan[0]
  st=test_data.scan_start_time
  nsize=size(test_data.latitude,dimension=1)
  lat=test_data.latitude
  lon=test_data.longitude
  ratio=test_data.on2
  time=test_data.scan_start_time
  dqi=test_data.on2_dqi

  for s=0,nscan-1 do begin
    for i=0, nsize[0]-1 do begin
      for j=0, nsize[1]-1 do begin
        if ~FINITE(lat[i,j], /NAN) and ~FINITE(lon[i,j], /NAN) and ~FINITE(ratio[i,j,s], /NAN) then begin
          if dqi[i,j,s] eq 0 then begin
            year=fix(strmid(time[s],0,4))  
            month=fix(strmid(time[s],5,2))-1
            day=fix(strmid(time[s],8,2))
            second=fix(strmid(time[s],17,2))
            minute=fix(strmid(time[s],14,2))
            hour=fix(strmid(time[s],11,2))
            
            ; geographic
            theta=floor(90-lat[i,j])
            hour=hour+minute/60.0+second/3600.0
            localtime=lon[i,j]+hour*15 
            if localtime ge 360 then begin
              localtime=floor(localtime-360)
            endif else begin
              localtime=floor(localtime)
            endelse
            
            cnt[localtime,theta,month]=cnt[localtime,theta,month]+1
            ave_on2[localtime,theta,month]=ave_on2[localtime,theta,month]+ratio[i,j,s]
            
            lati[count]=lat[i,j]
            longi[count]=lon[i,j]
            data[count]=ratio[i,j,s]
            datetime[count]=time[s]
            count=count+1
            
            if count ge n_elements(data) then begin
              save, lati,longi,data,datetime,filename='/Users/wzihan/data/GOLD/on2_raw_'+strtrim(noutput,2)+'.sav'
              lati=MAKE_ARRAY(nmax, /float, VALUE = 0.0)
              longi=MAKE_ARRAY(nmax, /float, VALUE = 0.0)
              data=MAKE_ARRAY(nmax, /float, VALUE = 0.0)
              datetime=MAKE_ARRAY(nmax, /string)
              count=0UL
              noutput=noutput+1
            endif
            
;            ; geomagnetic
;            ret = AACGM_v2_SetDateTime(year,month,day,hour,minute,second)
;            p = cnvcoord_v2(lat[i,j],lon[i,j], 10)
;            m_theta=floor(90-p[0])
;            mlocaltime = floor(mlt_v2(p[1])*15)
;            
;            cnt_mag[mlocaltime,m_theta,month]=cnt_mag[mlocaltime,m_theta,month]+1
;            ave_on2_mag[mlocaltime,m_theta,month]=ave_on2[mlocaltime,m_theta,month]+ratio[i,j,s]
          endif  
        endif
      endfor
    endfor
  endfor
  
endfor

longi=longi[0:count-1]
lati=lati[0:count-1]
data=data[0:count-1]
datetime=datetime[0:count-1]

save, lati,longi,data,datetime,filename='/Users/wzihan/data/GOLD/on2_raw_'+strtrim(noutput,2)+'.sav'

tot_on2=ave_on2

save, tot_on2, cnt, filename='/Users/wzihan/data/GOLD/on2_stat.sav'

for i=0, 359 do begin
  for j=0, 179 do begin
    for k=0, 11 do begin
      if cnt[i,j,k] eq 0 then begin
        ave_on2[i,j,k]=!VALUES.F_NAN
      endif else begin
        ave_on2[i,j,k]=ave_on2[i,j,k]/cnt[i,j,k]
      endelse
    endfor
  endfor
endfor

save, ave_on2, cnt, filename='/Users/wzihan/data/GOLD/on2_ave.sav'

;tot_on2_mag=ave_on2_mag

;for i=0, 359 do begin
;  for j=0, 179 do begin
;    for k=0, 11 do begin
;      if cnt_mag[i,j,k] eq 0 then begin
;        ave_on2_mag[i,j,k]=!VALUES.F_NAN
;      endif else begin
;        ave_on2_mag[i,j,k]=ave_on2_mag[i,j,k]/cnt_mag[i,j,k]
;      endelse
;    endfor
;  endfor
;endfor
;
;save, ave_on2_mag, cnt_mag, filename='/Users/wzihan/data/GOLD/on2_ave_mag.sav'
end