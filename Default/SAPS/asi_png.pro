pro asi_png
;plot THEMIS ASI data and save as png files
;can superpose other data on top as well
thm_init
!themis.local_data_dir='/Users/wzihan/data/themis/'
date='20130518'
hour=5 & mins=0 & sec=0
day_str=strmid(date,6,2) & month_str=strmid(date,4,2) & year_str=strmid(date,0,4)
day=fix(day_str) & month=fix(month_str) & year=fix(year_str) 

For i=0,0 Do begin
  hour_str=strtrim(string(hour),2) & mins_str=strtrim(string(mins),2) & sec_str=strtrim(string(sec),2)
  if(strlen(hour_str) eq 1) then hour_str='0'+hour_str
  if(strlen(mins_str) eq 1) then mins_str='0'+mins_str
  if(strlen(sec_str) eq 1) then sec_str='0'+sec_str
  time_str=hour_str+':'+mins_str+':'+sec_str
  date_str=year_str+'-'+month_str+'-'+day_str
  
  ;create themis asi mosaic
  thm_asi_create_mosaic,date_str+'/'+time_str,/verbose,/merge,show=['snkq','kapu'],$
  scale=2e7,central_lon=230, central_lat=60,xsize=1200,ysize=800,rotation=0;,/zbuffer;$;maxval=[8000],minval=[100],$
  ;no_grid=no_grid,gif_out=1,gif_dir='/Users/shashaz/Research/Umich/ASI/20150317/',/zbuffer
  ;,totalintensity;minimum_elevation=12,totalintensity
  
  
;;**********************************************************
;;can superpose PFISR beam locations on top of THEMIS ASI
;  loadct,39
;  dir='/Volumes/MyBookStudio/data/PFISR/2012/';'/Users/shashaz/Research/UCLA/PFISR/PFISR data/new/'
;  ;result=file_search(dir+date+'/data/'+date+'.00?_lp_1min-cal.h5',count=cnt)
;  result=file_search(dir+'20121119.001_lp_?min-cal.h5',count=cnt)
;  if(cnt eq 0) then begin
;    print,'No files found!'
;    goto,stopplot
;  endif else begin
;    file=result[0]
;    print,file
;    file_id=h5f_open(file)
;  endelse
;  dataset_id=H5d_open(file_id,'/Geomag/Latitude')
;  lat=H5d_read(dataset_id)
;  dataset_id1=H5d_open(file_id,'/Geomag/Longitude')
;  lon=H5d_read(dataset_id1)
;  dataset_id2=H5d_open(file_id,'/Geomag/Altitude')
;  alt=H5d_read(dataset_id2)
;  H5D_CLOSE, dataset_id
;  H5D_CLOSE, dataset_id1
;  H5D_CLOSE, dataset_id2
;  H5F_CLOSE, file_id
;
;  ;cl=[36,100,192,223,36,100,192,223,100,192,192,223,36];findgen(11)*20;
;  cl=[36,60,192,223,36,60,192,223,60,192,192,223,36]
;  for j=0,10 do begin
;    ;xyouts,0.1+0.05*j,0.9,j+1,color=cl[j],/normal,font=0
;    for kk=1,16,1 do begin
;      ; print,max(alt[*,j],max_sub,/NAN)
;      plots,lon[kk,j],lat[kk,j],psym=1,symsize=0.5,color=cl[j],thick=1.5;(i mod 7)*35
;      ; plots,lon[max_sub,j],lat[max_sub,j],psym=1,symsize=0.8,color=cl[j],thick=3;(i mod 7)*35
;    endfor
;  endfor
;  loadct,0
;  MAP_GRID, /Label, COLOR=150, font=0 ,Latlab=-100,Lonlab=63,Latdel=5,Londel=5, $
;    charsize=0.8,glinethick=1.3

  ;Superpose ground based magnetometer locations
  loadct,39    
 ; Plots,218.84,64.77,psym=4,symsize=1.3,color=250; Eagl
 ; Plots,212.51,65.11,psym=4,symsize=1.3,color=250; Pokr
  Plots,214.84,62.407,psym=4,symsize=1.3,color=250; Gako
 ; Plots,224.777,61.01,psym=4,symsize=1.3,color=250; whit
 ; Plots,214.786,66.56,psym=4,symsize=1.3,color=250; fykn
 ; Plots,204.404,62.935,psym=4,symsize=1.3,color=250; mcgr
 
  Plots,360-129.599,52.633,psym=2,symsize=1.3,color=200; VAP-A
  Plots,360-106.04,53.83,psym=2,symsize=1.3,color=200; VAP-B
  Plots,360-124.25,58.632,psym=5,symsize=1.3,color=100; GOES-15
  Plots,360-82.39,53.79,psym=5,symsize=1.3,color=100; GOES-13

  Plots,360-120.36,43.27,psym=4,symsize=1.3,color=250; CVW
  Plots,360-113.31,54.71,psym=4,symsize=1.3,color=250; T36
  Plots,360-112.97,53.35,psym=4,symsize=1.3,color=250; C06
  Plots,360-113.84,52.14,psym=4,symsize=1.3,color=250; RED
  Plots,360-103.8,49.69,psym=4,symsize=1.3,color=250; C12
  Plots,360-133.16,56.83,psym=4,symsize=1.3,color=250; T22
  Plots,360-135.33,57.07,psym=4,symsize=1.3,color=250; SIT
  
  ;Superpose SuperDARN beam looking directions
  dir_sdarn='/Users/wzihan/IDLWorkspace85/Default/CVW_beams/'
  For beam=4,23 do begin
    beam_str=strtrim(string(beam),2)
    restore,dir_sdarn + 'CVW_bm'+beam_str+'_coord_new_int.sav'
    geolon=360+geolon
    print,geolon[0],geolat[0],geolon[50],geolat[50]
    plots,[geolon[0],geolat[0]],color=200
    plots,[geolon[70],geolat[70]],color=200,/continue
    plots,[geolon[27],geolat[27]],color=200,psym=5
  Endfor
  
;  station='SAS'
;  restore,'/Users/shashaz/Research/UCLA/SDARN data/'+station+'_coordinate.sav'
;  beam=0 & gate=14 ; beam starts from 0 and gate starts from 1
;  glat=(coord[gate-1,beam,0]+coord[gate,beam,0]+coord[gate-1,beam+1,0]+coord[gate,beam+1,0])/4.
;  glon=(coord[gate-1,beam,1]+coord[gate,beam,1]+coord[gate-1,beam+1,1]+coord[gate,beam+1,1])/4.
;  print,glat,glon
;  Plots,glon,glat,psym=4,symsize=1.3,color=250; 
;  
;  beam=15 & gate=18 ; beam starts from 0 and gate starts from 1
;  glat=(coord[gate-1,beam,0]+coord[gate,beam,0]+coord[gate-1,beam+1,0]+coord[gate,beam+1,0])/4.
;  glon=(coord[gate-1,beam,1]+coord[gate,beam,1]+coord[gate-1,beam+1,1]+coord[gate,beam+1,1])/4.
;  print,glat,glon
;  Plots,glon,glat,psym=4,symsize=1.3,color=150; 
  
;  restore,'/Users/shashaz/Research/Umich/GITM/20120327/asi/asi_boundary/mag/GAKO_20120327_'+hour_str+mins_str+'_mag.sav'
;  plots,ave_lon_geo,ave_lat_geo,psym=2,symsize=1.,color=250
;  restore,'/Users/shashaz/Research/Umich/GITM/20120327/asi/asi_boundary/mag/MCGR_20120327_'+hour_str+mins_str+'_mag.sav'
;  plots,ave_lon_geo,ave_lat_geo,psym=2,symsize=1.,color=250
;  restore,'/Users/shashaz/Research/Umich/GITM/20120327/asi/asi_boundary/mag/GAKO_20120327_'+hour_str+mins_str+'_mag_arc.sav'
;  plots,lon_min_geo,lat_min_geo,psym=2,symsize=1.3,color=150
;  plots,lon_peak_geo,lat_peak_geo,psym=2,symsize=1.3,color=150
  ;plots,lon_max_geo,lat_max_geo,psym=2,symsize=1.3,color=150
;  restore,'/Users/shashaz/Research/Umich/GITM/20120327/asi/asi_boundary/mag/MCGR_20120327_'+hour_str+mins_str+'_mag_arc.sav'
;  plots,lon_min_geo,lat_min_geo,psym=2,symsize=1.3,color=150
;  plots,lon_peak_geo,lat_peak_geo,psym=2,symsize=1.3,color=150
  ;plots,lon_max_geo,lat_max_geo,psym=2,symsize=1.3,color=150
  
  ;xyouts,0.005,0.018,date_str+'/'+time_str+' UT',color=255,/normal,charsize=2,font=0
  ;xyouts,0.005,0.060,'THEMIS-GBO ASI',color=255,/normal,charsize=1.2
  
  ;xyouts,-140.2,63,'65',color=255,/data,charsize=1.2
  ;xyouts,-137.3,67.4,'70',color=255,/data,charsize=1.2
  
  xyouts,-152.5,65.5,'65',color=255,/data,charsize=2,font=0
  xyouts,-147.5,69.5,'70',color=255,/data,charsize=2,font=0
  
  goto,no_gps
  ;Superpose ground based GPS receiver location
  ;Plots,214.4,68.12,psym=2,symsize=1.3,color=250; ab46
  ;Plots,211.1,68.76,psym=2,symsize=1.3,color=250; ab45
  ;Plots,209.2,65.03,psym=2,symsize=1.3,color=250; ab36
  ;Plots,209.8,67.25,psym=2,symsize=1.3,color=250; ab33
  
  ;Plots,204.1,68.8,psym=4,symsize=1.3,color=0; 
  ;Plots,192.3,75,psym=4,symsize=1.3,color=0
  
  ;;plot GPS ground track
    nlines=file_lines('/Users/shashaz/Research/Umich/TOMO/processed/2008/068/cn_CR2008068.AB33')
    dayarr=make_array(nlines-3,/integer) & hrarr=make_array(nlines-3,/integer) & mnarr=make_array(nlines-3,/integer)
    secarr=make_array(nlines-3,/integer) & prnarr=make_array(nlines-3,/integer) & glatarr=make_array(nlines-3,/double)
    glonarr=make_array(nlines-3,/double) & elevarr=make_array(nlines-3,/double) & slatarr=make_array(nlines-3,/double)
    slonarr=make_array(nlines-3,/double) & rangearr=make_array(nlines-3,/double) & saltarr=make_array(nlines-3,/double)
    stecearr=make_array(nlines-3,/float) & vtecarr=make_array(nlines-3,/float)
    data=make_array(14,nlines-3,/double)
    openr,unit,'/Users/shashaz/Research/Umich/TOMO/processed/2008/068/cn_CR2008068.AB33',/get_lun
    skip_lun,unit,3,/lines
    readf,unit,data
    dayarr=transpose(data[0,*]) & hrarr=transpose(data[1,*]) & mnarr=transpose(data[2,*]) & secarr=transpose(data[3,*])
    prnarr=transpose(data[4,*]) & glatarr=transpose(data[5,*]) & glonarr=transpose(data[6,*]) & elevarr=transpose(data[7,*])
    slatarr=transpose(data[8,*]) & slonarr=transpose(data[9,*]) & rangearr=transpose(data[10,*])
    saltarr=transpose(data[11,*]) & stecarr=transpose(data[12,*]) & vtecarr=transpose(data[13,*])
    
    timearr=make_array(nlines-3,/double)
    timearr=hrarr+mnarr/60.+secarr/3600.
    loadct,39 
    elevlt=20 & normfactor=15
    for stnm=9,9 do begin
      sub=where(prnarr eq stnm and elevarr ge elevlt and timearr ge hour+mins/60. and timearr le hour+(mins+10)/60. and stecarr gt 0.0)
      ;if sub[0] lt 0 then goto,next2
     ; if(where(prns eq stnm) eq -1) then begin
     ;   km=km+1
     ;   prns[km]=stnm
        ;fsc_text,0.8,0.95-km*0.03,'prn: '+strtrim(string(stnm),2), font=0,color=stnm*250/32.,/normal 
      ;  xyouts,0.8,0.95-km*0.03,'prn: '+strtrim(string(stnm),2), font=0,color=stnm*250/32.,/normal 
     ; endif
       ; fsc_plots,glonarr[sub],glatarr[sub],symcolor=stnm*250/32.,psym=symcat(9),symsize=stecarr[sub]/normfactor
       For k=0,0 Do begin;n_elements(sub)-1 Do begin
         ll=sub[k]
         plots,glonarr[ll],glatarr[ll],color=fsc_color('magenta'),psym=symcat(9),symsize=1.3;stecarr[ll]/normfactor
       Endfor
    endfor
    
  no_gps:  
  
;  restore,'/users/wzihan/idlworkspace85/ob2.sav'
;  plots,ob2[*,2],ob2[*,1],color=100
;
;  restore,'/users/wzihan/idlworkspace85/ob1.sav'
;  plots,ob1[*,2],ob1[*,1],color=100

  filename='/Users/wzihan/plot/'+date+'_'+hour_str+mins_str+sec_str
  makepng,filename

  sec=sec+60
  if(sec ge 60) then begin
  mins=mins+1
  sec=sec-60
  endif
  if(mins ge 60) then begin
  mins=mins-60
  hour=hour+1
  endif

  
Endfor
;close,unit   
;free_lun,unit
stopplot:
End