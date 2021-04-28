
;wildcard = ask('files to plot tec (e.g., 3DALL*.bin)','3DALL*.bin')

foldername='/Users/wzihan/Simulations'
filelist1 = file_search(foldername+'/data/3DALL_t170908*.bin')
filelist2 = file_search(foldername+'/data/3DALL_t170907*.bin')
file = filelist1(0)
makect,'bwr'

GITM_READ_HEADER, file, Time, nVars, Vars, nLons, nLats, nAlts, version

IsDone = 0
iVar = [0,1,2,34]
tec1=fltarr(nLons, nLats)
tec2=fltarr(nLons, nLats)
tec_res=fltarr(nLons, nLats)
dalt=fltarr(nLons, nLats, nAlts)

nFiles = n_elements(filelist1)

for iFile = 0, nFiles-1 do begin
  file = filelist1(iFile)
  print, file
  tp=strpos(file,'_t')
  setdevice, foldername+'/plot_new/dtec/dtec_'+strmid(file,tp+2,13)+'.ps', 'p', 5

  gitm_read_bin_1var,file, data, time1, nVars, Vars, version, VarsToGet = iVar

  lats = reform(data(1,*,*,0))
  lons = reform(data(0,*,*,0))
  lo = reform(lons(*,0))/!dtor
  
  c_r_to_a, itime, time1
  ut = float(itime(3)) + float(itime(4))/60.0 + float(itime(5))/3600.0
  utrot=ut*15.0
  localtime = (lons/!dtor/15.0 + ut) mod 24.0

  for iAlt=0,nAlts-2 do begin
    dAlt(*,*,iAlt) = reform(data(2,*,*,iAlt+1))-reform(data(2,*,*,iAlt))
    tec1(*,*) = tec1(*,*) + dAlt(*,*,iAlt)*data(3,*,*,iAlt)
  endfor

  ;;;;;;;;;;;;;;;;
  file = filelist2(iFile)
  print, file
  gitm_read_bin_1var,file, data, time1, nVars, Vars, version, VarsToGet = iVar

  lats = reform(data(1,*,*,0))
  lons = reform(data(0,*,*,0))

  c_r_to_a, itime, time1
  c_a_to_s, itime, stime
  ut = float(itime(3)) + float(itime(4))/60.0 + float(itime(5))/3600.0

  localtime = (lons/!dtor/15.0 + ut) mod 24.0

  for iAlt=0,nAlts-2 do begin
    dAlt(*,*,iAlt) = reform(data(2,*,*,iAlt+1))-reform(data(2,*,*,iAlt))
    tec2(*,*) = tec2(*,*) + dAlt(*,*,iAlt)*data(3,*,*,iAlt)
  endfor

  tec_res = (tec1-tec2)/1.0e16

  ppp = 1
  space = 0.01
  pos_space, ppp, space, sizes

  plotdumb

  ;--------------------------------------------------------------
  ; Figure out where on the page we should be
  ;--------------------------------------------------------------

  get_position, ppp, space, sizes, 0, pos

  nLevels = 31
  lon = lo+utrot

  !p.position = pos

  utime = itime(3)*3600.0 + $
    itime(4)*60.0 + $
    itime(5)
  utime = utime(0)
  p0lon = utime/3600.0 * 360.0 / 24.0

  map_set, 0.0, 180.0-p0lon, /noerase

  newrat = reform(tec_res(1:nLons-2,1:nLats-2))

  newlat = lats(1:nLons-2,1:nLats-2)/!dtor
  newlon = lons(1:nLons-2,1:nLats-2)/!dtor
  nLo  = nLons-2
  nLa  = nLats-2

  newrat(0,*)       = (newrat(1,*)+newrat(nLo-2,*))/2.0
  newrat(nLo-1,*) = (newrat(1,*)+newrat(nLo-2,*))/2.0
  newrat(*,0)       = mean(newrat(*,1))
  newrat(*,nLa-1) = mean(newrat(*,nLa-2))

  newlon(0,*)       = 0.0
  newlon(nLo-1,*) = 360.0
  newlat(*,0) = -90.0
  newlat(*,nLa-1) =  90.0

  maxi=10
  mini=-10
  levels = findgen(nlevels)*(maxi-mini)/(nlevels-1) + mini

  l = where(newrat lt levels(1),c)
  if (c gt 0) then newrat(l) = levels(1)

  contour, newrat, newlon, newlat, $
    /follow, /cell_fill, /over, $
    levels = levels

  nLevels = 11
  levels = findgen(nlevels)*(maxi-mini)/(nlevels-1) + mini
  contour, newrat, newlon, newlat, $
    /follow, /over, $
    levels = levels, thick = 2

  map_grid, lats = findgen(19)*10-90, glinethick=1

  map_continents

  xp = pos(0)
  yp = pos(3)+(pos(3)-pos(1))*1.1 ; pos(3)+yr/20.0
  xyouts, xp, yp, strmid(stime,0,9), $
    /norm, charsize = 1.1, align = 0.0

  xp = pos(2)
  yp = pos(3)+(pos(3)-pos(1))*1.1                  ; pos(3)+yr/20.0
  xyouts, xp, yp, strmid(stime,10,5)+' UT', $
      /norm, charsize = 1.1, align = 1.0

  ctpos = [pos(2)+0.005, pos(1), pos(2)+0.02, pos(3)]
  range = [mini,maxi]
  units = 'TEC (10!U16!N/m!U2!N)'
  ncolors = 255
  plotct, ncolors, ctpos, range, units, /right

  closedevice

endfor


end

