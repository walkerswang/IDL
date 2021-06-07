root = ask('files to plot tec (e.g., 2DTEC*.bin)', root)
fig_folder=ask('figure folder: ', fig_folder)
figname=ask('figure name (no .ps): ', figname)
filelist = file_search(root)
file = filelist(0)
makect,'bwr'

isdone=0

gitm_read_header, file, time, nVars, Vars, nLons, nLats, nAlts, version

display,vars
sel=ask('which variable?')
iVar = [0,1,2,sel]

loc=fix(ask('which alt?'))

if not isDone then begin

  ; read all necessary data
  nFiles = n_elements(filelist)
  time = dblarr(nFiles)
  seldata = dblarr(nFiles, nLons, nLats)

  for iFile = 0, nFiles-1 do begin

    file = filelist(iFile)
    print, file
    gitm_read_bin_1var,file, data, time1, nVars, Vars, version, VarsToGet = iVar

    lats = reform(data(1,0,*,0))/!pi*180
    lons = reform(data(0,*,0,0))/!pi*180
    alts = reform(data(2,0,0,*))/1e3
    
    alt = reform(data(2,*,*,*)) / 1000.0
    lat = reform(data(1,*,*,*)) / !dtor
    lon = reform(data(0,*,*,*)) / !dtor

    c_r_to_a, itime, time1
    ut = float(itime(3)) + float(itime(4))/60.0 + float(itime(5))/3600.0

    localtime = (lons/15.0 + ut) mod 24.0

    time(iFile) = time1

    f=min(abs(loc-alts),ialt)
    selset=ialt
    seldata(ifile, *, *)= data(-1, *, *, ialt)

  endfor

  save, time, lats, alts, seldata, filename=fig_folder+figname+'.sav'
endif else begin
  restore, fig_folder+figname+'.sav'
endelse

; deal with the data

for i = 0 ,nLons-1 do begin
  for j = 0 , nLats-1 do begin
    seldata(*,i,j)=seldata(*,i,j)-smooth(seldata(*,i,j),13)
  endfor
endfor

smini_final = float(ask('minimum values for contour (0.0 means auto)'))
smaxi_final = float(ask('maximum values for contour (0.0 means auto)'))

; plot the results
for i=0, nfiles-1 do begin
  filename=filelist(i)
  gitm_read_bin_1var,filename, data, time1, nVars, Vars, version, VarsToGet = iVar
  c_r_to_a, itime, time1
  data[-1,*,*,ialt]=seldata[i,*,*]

  if (float(smini_final) lt 0.0) then colortable = 'mid'
  
  cursor_x=0.0
  cursor_y=0.0
  strx = '0.0'
  stry = '0.0'
  step = 3
  cursor_cnt = 0
  xrange = [0.0,0.0]
  yrange = [0.0,0.0]
  cnt1=1
  cnt2=0
  cnt3=0
  sel=-1
  yes = 0
  no  = 1  
  polar=0
  yes_writecnt=1
  showgridyes = 0
  plotvectoryes=0
  mini=smini_final
  maxi=smaxi_final
  psfile=fig_folder+figname
  tp=strpos(filename,'_t')
  psfile = psfile+'_'+strmid(filename,tp+2,13)+'.ps'
  thermo_plot_new,cursor_x,cursor_y,strx,stry,step,nvars,sel,nfiles,$
    cnt1,cnt2,cnt3,yes,no,yeslog,      $
    1-yeslog,nalts,nlats,nlons,yeswrite_cnt,$
    polar,npolar,MinLat,showgridyes,   $
    plotvectoryes,vi_cnt,vn_cnt,vector_factor,   $
    cursor_cnt,data,alt,lat,lon,   $
    xrange,yrange,selset,mini,maxi, $
    filename,vars, psfile, 0, colortable, itime, $
    iSecondVar=iSecondVar, IsZonalAverage=IsZonalAverage, $
    plotsquare = plotsquare, p0lon=0

endfor

end
