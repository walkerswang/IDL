FOLDER='/Users/wzihan/Simulations/'
FILELIST = FINDFILE(FOLDER+'data/'+'3DALL_t170908_*0000.bin')
FILELISTION = FINDFILE(FOLDER+'data/'+'3DION_t170907_050000.bin')
north = 1

nfiles = n_elements(filelist)

ff = 0
lf = nfiles-1

filei= filelistion(0)

read_thermosphere_file, filei, nvars, nalts, nlats, nlons,vars,datai, $
  nBLKlat, nBLKlon, nBLK, iTime, Version

mlat=reform(datai(22,*,*,32))
mlon=reform(datai(23,*,*,32))

for ifile = ff, lf do begin

  file = filelist(iFile)

  read_thermosphere_file, file, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  P = STRPOS(FILE, '3DALL')
  IF (P EQ 0) THEN P = STRLEN(FILE)
  on2file= folder+'plot_new/vi_vertical/vn_vertical_'+strmid(file,31,20)

  setdevice, on2file+'.ps',5

  k = 1.3807e-23

  n = reform(data(4,*,*,2:nalts-3)) + $
    reform(data(5,*,*,2:nalts-3)) + $
    reform(data(6,*,*,2:nalts-3))
  t = reform(data(15,*,*,2:nalts-3))
  Alt = reform(data(2,0,0,2:nalts-3))
  dAlt_150 = (reform(data(2,0,0,3:nalts-2))-reform(data(2,0,0,1:nalts-4)))/2.0
  loc_150 = where( alt lt 150000.0)

  p = alog(n*k*t)

  nLons = n_elements(t(*,0,0))
  nLats = n_elements(t(0,*,0))
  nAlts = n_elements(t(0,0,*))

  io_ = 4
  in2_ = 6
  ino_ = 8
  iviu=18
  
  o      = fltarr(nLons, nLats)
  n2     = fltarr(nLons, nLats)
  AltInt = fltarr(nLons, nLats)
  viu=fltarr(nLons, nLats)
  noInt = fltarr(nLons, nLats)

  ; The lowest two levels are ghost cells, so we want to start at level 2.

  nLevel = 8
  nLevel = 17
  if (ifile eq ff) then maxpressure = min(p(*,*,nLevel))

  MaxValN2 = 1.0e21

  ialt=32
  
  for iLon = 0, nLons-1 do begin
    for iLat = 0, nLats-1 do begin
      viu(iLon,iLat) = data(iviu,iLon,iLat,iAlt)
      n2(iLon,iLat) = data(in2_,iLon,iLat,iAlt)
      o(iLon,iLat)  =  data(io_,iLon,iLat,iAlt)

    endfor
  endfor

  ratio1 = n2*0.0
  loc = where(n2 gt 0.0,count)
  if (count gt 0) then ratio1(loc) =o(loc)
  loc = where(n2 eq 0.0,count)

  

  ratio=viu

  quantity = AltInt/1000.0

  if (nLats gt 1) then begin
    
    mini=-100
    maxi=100
    nLevels=31

    levels = findgen(nlevels)*(maxi-mini)/(nlevels-1) + mini

    lon = (reform(data(0,*,*,0))*180/!pi + 360.0) mod 360.0

    utime = itime(3)*3600.0 + $
      itime(4)*60.0 + $
      itime(5)
    utime = utime(0)

    ;        print, utime

    lat = reform(data(1,*,*,0))*180/!pi

    localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0

    reratio = ratio*0.0
    relocaltime = localtime*0.0
    for i=0,nLons-1 do begin
      loc = where(localtime(*,0) eq min(localtime(*,0)))
      relocaltime(i,*) = localtime(loc(0),*)
      reratio(i,*) = ratio(loc(0),*)
      localtime(loc(0),*) = 10000.0
    endfor

    ppp = 1
    space = 0.05
    pos_space, ppp, space, sizes, ny = ppp
    get_position, ppp, space, sizes, 0, pos, rect = 1
    ;print,pos

    ;xm = (pos(2)+pos(0))/2.0
    ;dx = pos(2)-pos(0)
    ;pos(0) = xm-dx
    ;pos(2) = pos(0)+dx*2

    ;print,pos
    ctpos = pos
    ctpos(0) = pos(2)+0.01
    ctpos(2) = ctpos(0)+0.03
    ;print,ctpos

    !p.position = pos

    p0lon = utime/3600.0 * 360.0 / 24.0
    makect,'bwr'
    map_set, 0.0, 180.0-p0lon, /noerase


    ;if (north) then map_set, 0.0, 180.0-p0lon, /cont $
    ;else map_set, 0.0, 180.0-p0lon, /cont
    ;        if (north) then map_set, 0.0, 180.0-p0lon, /orthographic, /cont $
    ;        else map_set, 0.0, 180.0-p0lon, /orthographic, /cont

    xyouts, (pos(0)+pos(2))/2.0, pos(3)+0.01, file, /norm, align=0.5
    !p.position = -1

    loc = where(ratio gt levels(nlevels-1),count)
    if (count gt 0) then ratio(loc) = levels(nlevels-1)

    newrat = ratio(1:nLons-2,1:nLats-2)
    newqua = quantity(1:nLons-2,1:nLats-2)
    newmlat = mlat(1:nLons-2,1:nLats-2)
    newmlon = mlon(1:nLons-2,1:nLats-2)

    newlat = lat(1:nLons-2,1:nLats-2)
    newlon = lon(1:nLons-2,1:nLats-2)
    nLons  = nLons-2
    nLats  = nLats-2

    newrat(0,*)       = (newrat(1,*)+newrat(nLons-2,*))/2.0
    newrat(nLons-1,*) = (newrat(1,*)+newrat(nLons-2,*))/2.0
    newrat(*,0)       = mean(newrat(*,1))
    newrat(*,nLats-1) = mean(newrat(*,nLats-2))

    newmlat(0,*)       = (newmlat(1,*)+newmlat(nLons-2,*))/2.0
    newmlat(nLons-1,*) = (newmlat(1,*)+newmlat(nLons-2,*))/2.0
    newmlat(*,0)       = mean(newmlat(*,1))
    newmlat(*,nLats-1) = mean(newmlat(*,nLats-2))
    
    newmlon(0,*)       = (newmlon(1,*)+newmlon(nLons-2,*))/2.0
    newmlon(nLons-1,*) = (newmlon(1,*)+newmlon(nLons-2,*))/2.0
    newmlon(*,0)       = mean(newmlon(*,1))
    newmlon(*,nLats-1) = mean(newmlon(*,nLats-2))

    newqua(0,*)       = (newqua(1,*)+newqua(nLons-2,*))/2.0
    newqua(nLons-1,*) = (newqua(1,*)+newqua(nLons-2,*))/2.0
    newqua(*,0)       = mean(newqua(*,1))
    newqua(*,nLats-1) = mean(newqua(*,nLats-2))

    newlon(0,*)       = 0.0
    newlon(nLons-1,*) = 360.0
    newlat(*,0) = -90.0
    newlat(*,nLats-1) =  90.0

    save, lat, lon, mlon, mlat, file = 'apex_grid.sav'

    l = where(newrat lt levels(1),c)
    if (c gt 0) then newrat(l) = levels(1)
    l = where(newrat gt levels(29),c)
    if (c gt 0) then newrat(l) = levels(29)

    contour, newrat, newlon, newlat, $
      /follow, /cell_fill, /over, $
      levels = levels

    lmini=-90.0
    lmaxi=90
    nLevels=19

    levels = findgen(nlevels)*(lmaxi-lmini)/(nlevels-1) + lmini
    contour, newmlat, newlon, newlat, $
      /follow, /over, $
      levels = levels, thick = 2
      
    lmini=-180.0
    lmaxi=180.0
    nLevels=19

    levels = findgen(nlevels)*(lmaxi-lmini)/(nlevels-1) + lmini+10
    contour, newmlon, newlon, newlat, $
      /follow, /over, $
      levels = levels, thick = 2

    map_grid, lats = findgen(19)*10-90, glinethick=1
    map_continents

    ctpos = [pos(2)+0.005, pos(1), pos(2)+0.02, pos(3)]
    range = [mini,maxi]
    units = 'V_i up (m/s)'
    ncolors = 255
    plotct, ncolors, ctpos, range, units, /right

    closedevice

  endif

endfor


end
