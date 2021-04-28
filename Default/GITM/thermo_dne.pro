FOLDER='/Users/wzihan/Simulations/'
FILELIST = FINDFILE(FOLDER+'data/'+'3DALL_t170908_*0000.bin')
FILELIST2 = FINDFILE(FOLDER+'data/'+'3DALL_t170907_*0000.bin')
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
  file2= filelist2(iFile)

  read_thermosphere_file, file, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  P = STRPOS(FILE, '3DALL')
  IF (P EQ 0) THEN P = STRLEN(FILE)
  on2file= folder+'plot_new/nw_meri/'+strmid(file,31,20)+'_dmeri_300km'

  setdevice, on2file+'.ps',5

  ialt=32
  
  ne1 = reform(data(17,*,*,ialt)) 

  nLons = n_elements(ne1(*,0,0))
  nLats = n_elements(ne1(0,*,0))
  nAlts = n_elements(ne1(0,0,*))

  read_thermosphere_file, file2, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  ne2 = reform(data(17,*,*,ialt)) 

  ratio=ne1-ne2

  if (nLats gt 1) then begin

    mini=-50
    maxi=50
    nLevels=31

    levels = findgen(nlevels)*(maxi-mini)/(nlevels-1) + mini

    lon = (reform(data(0,*,*,0))*180/!pi + 360.0) mod 360.0

    utime = itime(3)*3600.0 + $
      itime(4)*60.0 + $
      itime(5)
    utime = utime(0)

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
    units = 'Ne percent change'
    ncolors = 255
    plotct, ncolors, ctpos, range, units, /right

    closedevice

  endif

endfor


end
