pro read_sdarn_vt_v3,radar,bm
; find out superdarn beam locations using fitacf data downloaded form their web.
;,dir,date,rd,hour,mins,int,bm,data
  ;read superdarn radar range gate data downloaded from VT
  ;1 min time resolution and 24 beams for CVE
  dir='/Users/wzihan/Google\ Drive/My\ Paper/SAPS_jgr/'
  cd,dir
  file='20130518_'+radar+'_second.rec'
  bm_str=strtrim(string(bm),2)
  fileout='CVE/'+radar+'_bm'+bm_str+'second.txt' 
  fileout_new='CVE/'+radar+'_bm'+bm_str+'second_coord_new.txt'
  fileout_new_int='CVE/'+radar+'_bm'+bm_str+'second.sav'
  openw,unit1,fileout,/get_lun
;  tm_l=hour+mins/60. & tm_h=hour+(mins+int)/60.
;  day_str=strmid(date,6,2) & month_str=strmid(date,4,2) & year_str=strmid(date,0,4)
;  day=fix(day_str) & month=fix(month_str) & year=fix(year_str)
;  date_new=year_str+'-'+month_str+'-'+day_str
;  result=file_search(dir+'/'+date+'_18_24_'+rd+'*',count=ct)
  temp=' '
  openr,unit,file,/get_lun
      While ~eof(unit) Do begin
        readf,unit,temp
        ;print,temp
        readf,unit,temp
        ;print,temp
        rslt3=strsplit(temp,'  ',/extract)
        bmnum=fix(rslt3[2])
        readf,unit,temp
        ;print,temp
        rslt4=strsplit(temp,'  ',/extract)
        npnts=fix(rslt4[2])
        nrang=fix(rslt4[5])

          if(npnts ne 0 and bmnum eq bm) then begin
            readf,unit,temp; should be the last line of header
            For j=0,npnts-1 Do begin
              readf,unit,gate,range,pwr_0,pwr_l,vel,gsf,vel_err,width_l,$
                geo_lat,geo_lon,geo_azm,mag_lat,mag_lon,mag_azm,$
                format='(I4,1x,I5,1x,f5.1,3x,f4.1,1x,f9.1,3x,I1,1x,f8.1,1x,f8.1,7(1x,f8.2))'
              printf,unit1,gate, mag_lat,mag_lon,vel,vel_err
            Endfor 
            readf,unit,temp
          endif else begin
            skip_lun,unit,npnts+2,/lines
          endelse
          
      Endwhile
close,unit
free_lun,unit
close,unit1
free_lun,unit1

openr,unit,fileout,/get_lun
nlines=file_lines(fileout)
data=make_array(5,nlines,/float)
readf,unit,data
gate=data(0,*)
maglat=data(1,*)
maglon=data(2,*)
vel=data(3,*)
vel_err=data(4,*)
save,gate,geolat,geolon,maglat,maglon,vel,vel_err,filename=fileout_new_int
close,unit
free_lun,unit

;openr,unit,fileout,/get_lun
;nlines=file_lines(fileout)
;data=make_array(5,nlines,/float)
;readf,unit,data
;openw,unit1,fileout_new,/get_lun
;For i=0,max(data(0,*))-1 Do begin
;    sub=where(data(0,*) eq i)
;    print,sub
;    if(sub[0] ne -1) then begin
;        printf,unit1,data(0,sub[0]),data(1,sub[0]),data(2,sub[0]),data(3,sub[0]),data(4,sub[0])
;    endif  
;Endfor
;close,unit1
;free_lun,unit1
;close,unit
;free_lun,unit
;
;openr,unit,fileout_new,/get_lun
;nlines=file_lines(fileout_new)
;data=make_array(5,nlines,/float)
;readf,unit,data
;gate=findgen(100)
;geolat=interpol(data(1,*),data(0,*),findgen(100))
;geolon=interpol(data(2,*),data(0,*),findgen(100))
;maglat=interpol(data(3,*),data(0,*),findgen(100))
;maglon=interpol(data(4,*),data(0,*),findgen(100))
;save,gate,geolat,geolon,maglat,maglon,filename=fileout_new_int
;close,unit
;free_lun,unit
;p=plot(maglon,maglat,'+',/overplot)
End

;p=plot(findgen(360)-180,findgen(180)-90,yrange=[0,90],/nodata)
For i=0,19 Do begin
  read_sdarn_vt_v3,'cve',i
Endfor

End  


;