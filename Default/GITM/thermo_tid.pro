root = ask('files to plot tec (e.g., 3DALL*.bin)', root)
fig_folder=ask('figure folder: ', fig_folder)
figname=ask('figure name (no .ps): ', figname)
filelist = file_search(root)
file = filelist(0)
makect,'bwr'

isdone=0

gitm_read_header, file, time, nVars, Vars, nLons, nLats, nAlts, version

flag1=ask('fix lon or lt?', flag1)
loc1=fix(ask('which lon/lt?'))
flag2=ask('alt or lat?', flag2)
loc2=fix(ask('which lat/alt?'))
;year=ask('year:')
;month=ask('month:')
;sday=ask('start day:')
;shour=ask('start hour:')
;eday=ask('end day:')
;ehour=ask('end hour:')
display,vars
sel=ask('which variable?')
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

  lats = reform(data(1,0,*,0))/!pi*180
  lons = reform(data(0,*,0,0))/!pi*180
  alts = reform(data(2,0,0,*))/1e3
  
  c_r_to_a, itime, time1
  ut = float(itime(3)) + float(itime(4))/60.0 + float(itime(5))/3600.0

  localtime = (lons/15.0 + ut) mod 24.0

  time(iFile) = time1
  
  if flag1 eq 'lon' then begin
    f=min(abs(loc1-lons),ilon)
  endif else begin
    f=min(abs(loc1-localtime),ilon)
  endelse
  
  if flag2 eq 'lat' then begin
    f=min(abs(loc2-alts),ialt)
    seldata(ifile, *)= data(-1, ilon, *, ialt)
  endif else begin
    f=min(abs(loc2-lats),ilat)
    seldata(ifile, *)= data(-1, ilon, ilat, *)
  endelse
  
endfor

save, time, lats, alts, seldata, filename=fig_folder+figname+'.sav'
endif else begin
  restore, fig_folder+figname+'.sav'
endelse

; deal with the data
if flag2 eq 'lat' then range=nLats else range=nAlts

flagsub=ask('Subtract average? (y or n)', 'n')
if flagsub eq 'n' then begin
  newdata=seldata
endif else begin
  for i = 0 ,range-1 do begin
    seldata(*,i)=seldata(*,i)-smooth(seldata(*,i),13)
  endfor
endelse

; plot the results
figfile= fig_folder+figname
setdevice, figfile+'.ps'

ppp = 1
space = 0.01
pos_space, ppp, space, sizes, ny = ppp

get_position, ppp, space, sizes, 0, pos1, /rect
get_position, ppp, space, sizes, 1, pos2, /rect
pos = [pos1(0)+0.05,pos2(1), pos1(2)-0.07,pos1(3)]

if flag2 eq 'lat' then y=lats else y=alts

time2d = seldata
yy=seldata

for i=0,nfiles-1 do begin
  time2d(i,*) = time(i)- time(0)
endfor

for i=0,range-1 do begin
  yy(*,i)=y(i)
endfor

value=seldata
mini = min(value)
maxi = max(value)
vrange = (maxi-mini)
if (vrange eq 0.0) then vrange = 1.0
if (mini lt 0.0 or mini-0.1*vrange gt 0) then mini = mini - 0.1*vrange $
else mini = 0.0
maxi = maxi + 0.1*vrange

mini = float(ask('minimum values for contour',tostrf(mini)))
maxi = float(ask('maximum values for contour',tostrf(maxi)))

levels = findgen(31) * (maxi-mini) / 30 + mini

v = reform(value(*,2:range-3))
l = where(v gt maxi,c)
if (c gt 0) then v(l) = maxi
l = where(v lt mini,c)
if (c gt 0) then v(l) = mini

stime = time(0)
etime = max(time)
time_axis, stime, etime,btr,etr, xtickname, xtitle, xtickv, xminor, xtickn
xtitle = strmid(xtitle,0,12)

if flag2 eq 'alt' then begin
contour, v, time2d(*,2:range-3), yy(*,2:range-3), $
  /follow, /fill, pos=pos, $
  nlevels = 30, levels = levels, $
  yrange = [min(y(2:range-3)),max(y(2:range-3))], ystyle = 1, $
  ytitle = 'Altitude (km)', $
  xtickname = xtickname, xtitle = xtitle, xtickv = xtickv, $
  xminor = xminor, xticks = xtickn, xstyle = 1, charsize = 1.2
endif

if flag2 eq 'lat' then begin
  contour, v, time2d(*,2:range-3), yy(*,2:range-3), $
    /follow, /fill, pos=pos, $
    nlevels = 30, levels = levels, $
    yrange = [min(y(2:range-3)),max(y(2:range-3))], ystyle = 1, $
    ytitle = 'Latitude', $
    xtickname = xtickname, xtitle = xtitle, xtickv = xtickv, $
    xminor = xminor, xticks = xtickn, xstyle = 1, charsize = 1.2
endif

ctpos = pos
ctpos(0) = pos(2)+0.01
ctpos(2) = ctpos(0)+0.03
maxmin = [mini,maxi]
plotct, 255, ctpos, maxmin, title, /right
closedevice

end
