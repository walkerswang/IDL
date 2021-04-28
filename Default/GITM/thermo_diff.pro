FOLDER='/Users/wzihan/Simulations/'
FILELIST = FINDFILE(FOLDER+'data/'+'3DALL_t170908_*0000.bin')
FILELIST2 = FINDFILE(FOLDER+'data/'+'3DALL_t170907_*0000.bin')
;FILELISTION = FINDFILE(FOLDER+'data/'+'3DION_t170907_050000.bin')
north = 1

nfiles = n_elements(filelist)

ff = 0
lf = nfiles-1

for ifile = ff, lf do begin

  file = filelist(iFile)
  file2= filelist2(iFile)

  read_thermosphere_file, file, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  P = STRPOS(FILE, '3DALL')
  IF (P EQ 0) THEN P = STRLEN(FILE)
  on2file= folder+'plot_new/nw_meri/'+strmid(file,31,20)+'_dmeri_200km'

  setdevice, on2file+'.ps',5

  ialt=24
  ivar=17
  
  ne1 = reform(data(ivar,*,*,ialt)) 

  nLons = n_elements(ne1(*,0,0))
  nLats = n_elements(ne1(0,*,0))
  nAlts = n_elements(ne1(0,0,*))

  read_thermosphere_file, file2, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  ne2 = reform(data(ivar,*,*,ialt)) 

  ratio=ne1-ne2

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

    newlat = lat(1:nLons-2,1:nLats-2)
    newlon = lon(1:nLons-2,1:nLats-2)
    nLons  = nLons-2
    nLats  = nLats-2

    newrat(0,*)       = (newrat(1,*)+newrat(nLons-2,*))/2.0
    newrat(nLons-1,*) = (newrat(1,*)+newrat(nLons-2,*))/2.0
    newrat(*,0)       = mean(newrat(*,1))
    newrat(*,nLats-1) = mean(newrat(*,nLats-2))

    newlon(0,*)       = 0.0
    newlon(nLons-1,*) = 360.0
    newlat(*,0) = -90.0
    newlat(*,nLats-1) =  90.0

    l = where(newrat lt levels(1),c)
    if (c gt 0) then newrat(l) = levels(1)
    l = where(newrat gt levels(29),c)
    if (c gt 0) then newrat(l) = levels(29)

    contour, newrat, newlon, newlat, $
      /follow, /cell_fill, /over, $
      levels = levels

    map_grid, lats = findgen(19)*10-90, glinethick=1
    map_continents

    ctpos = [pos(2)+0.005, pos(1), pos(2)+0.02, pos(3)]
    range = [mini,maxi]
    units = 'Un Difference (m/s)'
    ncolors = 255
    plotct, ncolors, ctpos, range, units, /right

    closedevice

  endif

endfor


end
