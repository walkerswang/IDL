root = ask('files to plot tec (e.g., 3DALL*.bin)', root)
fig_folder=ask('Figure folder: ', fig_folder)
figname=ask('Figure name (no .ps): ', figname)
filelist = file_search(root)
file = filelist(0)
makect,'bwr'

gitm_read_bin, file, data, time, nVars, Vars, version

flag1=ask('Fix lon or lt?', flag1)
loc1=fix(ask('Which lon/lt?'))
flag2=ask('Alt or lat?', flag2)
loc2=fix(ask('Which alt/lat?'))
year=ask('year:')
month=ask('month:')
sday=ask('start day:')
shour=ask('start hour:')
eday=ask('end day:')
ehour=ask('end hour:')
display,vars
sel=ask('Which variable?')
iVar = [0,1,2,sel]

if not isDone then begin

; read all necessary data
nFiles = n_elements(filelist)
time = dblarr(nFiles)
if flag2 eq 'lat' then begin
  seldata = dblarr(nFiles, nLats)
endif else begin
  seldata = dblarr(nFiles, nAlts)
endelse

for iFile = 0, nFiles-1 do begin
 
  file = filelist(iFile)
  print, file
  gitm_read_bin_1var,file, data, time1, nVars, Vars, version, VarsToGet = iVar

  lats = reform(data(1,*,*,0))
  lons = reform(data(0,*,*,0))
  alts = reform(data(2,*,*,0))/1e3
  
  c_r_to_a, itime, time1
  ut = float(itime(3)) + float(itime(4))/60.0 + float(itime(5))/3600.0

  localtime = (lons/!dtor/15.0 + ut) mod 24.0

  time(iFile) = time1
  
  if flag1 eq 'lon' then begin
    f=min(abs(loc1-lons),ilon)
  endif else begin
    f=min(abs(loc1-localtime),ilon)
  endelse
  
  if flag2 eq 'lat' then begin
    f=min(abs(loc2-alts),ialt)
    seldata(ifile, *)= data(sel, ilon, *, ialt)
  endif else begin
    f=min(abs(loc2-lats),ilat)
    seldata(ifile, *)= data(sel, ilon, ilat, *)
  endelse
  
endfor

save, time, lats, alts, seldata, filename=fig_folder+figname+'.sav'
endif else begin
  restore, fig_folder+figname+'.sav'
endelse

; deal with the data
flagsub=ask('Subtract average? (y or n)', 'n')
if flagsub eq 'n' then begin
  newdata=seldata
endif else begin
  if flag2 eq 'lat' then range=nLats else range=nAlts
  for i = 0 ,range-1 do begin
    seldata(*,i)=seldata(*,i)-smooth(seldata(*,i),13)
  endfor
endelse

;; plot the results
;figfile= fig_folder+figname
;setdevice, figfile+'.ps'
;
;FOR k=0, nfiles-1 DO BEGIN;
;  x1=time[k]
;  x2=time[k+1]
;  x3=time[k+1]
;  x4=time[k]
;  xx=[x1,x2,x3,x4]
;    
;if flag2 eq 'lat' then y=lats else y=alts
;
;for i=0,nPts-1 do time2d(i,*) = time(i)- time(0)
;
;contour, v, time2d(*,2:nalts-3), Alts(*,2:nalts-3), $
;  /follow, /fill, $
;  nlevels = 30, pos = pos, levels = levels, $
;  yrange = [0,max(alts(*,2:nalts-3))], ystyle = 1, $
;  ytitle = 'Altitude (km)', $
;  xtickname = xtickname, xtitle = xtitle, xtickv = xtickv, $
;  xminor = xminor, xticks = xtickn, xstyle = 1, charsize = 1.2
;  
;closedevice

end
