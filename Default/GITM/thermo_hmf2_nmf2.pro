;magnetic pole (83.2973     -85.2189)  (175,139)
FOLDER='/Users/wzihan/Simulations/data/'
figfolder='/Users/wzihan/Simulations/'
allFILELIST = FINDFILE(FOLDER+'3DALL_t170908*.bin')
ionFILELIST = FINDFILE(FOLDER+'3DION_t170908*.bin')

north = 1

nfiles = n_elements(allfilelist)
g=9.8

ff = 0
lf = nfiles-1
makect, 'all'

for ifile = ff, lf do begin

  filename = allfilelist(iFile)

  print,filename
  ;read 3dall (neutral wind)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  P = STRPOS(FILEname, '3DALL')
  IF (P EQ 0) THEN P = STRLEN(FILE)
  figfile= figfolder+'plot_new/hmf2_nmf2/'+strmid(filename,31,20)+'hmf2_nmf2'

  setdevice, figfile+'.ps','p',5

  nLons = n_elements(reform(data(0,*,0,0)))
  nLats = n_elements(reform(data(0,0,*,0)))
  nAlts=  n_elements(reform(data(0,0,0,*)))
  n= reform(data(34,*,*,24:nalts-1))
  Alt=reform(data(2,0,0,24:nalts-1))

  hmf2=fltarr(nlons,nlats)
  nmf2=fltarr(nlons,nlats)

  ;calculate height averaged mu_in
  for iLon = 0, nlons-1 do begin
    for iLat = 0, nlats-1 do begin
    nmf2(iLon,iLat)=max(n(iLon,iLat,*),index)
    hmf2(iLon,iLat)=index
    endfor
  endfor

  lon = (reform(data(0,*,*,0))*180/!pi + 360.0) mod 360.0
  utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
  utime = utime(0)
  p0lon = utime/3600.0 * 360.0 / 24.0
  
  lat = reform(data(1,*,*,0))*180/!pi
  localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0

  v1=hmf2(1:nLons-2,1:nLats-2)
  v2=nmf2(1:nLons-2,1:nLats-2)

  newlat = lat(1:nLons-2,1:nLats-2)
  newlon = lon(1:nLons-2,1:nLats-2)
  nLons  = nLons-2
  nLats  = nLats-2

  pv1=alt(v1)/1e3
  pv2=alog10(v2)
  nv1=pv1
  nv2=pv2
  nv1(0,*) = (pv1(1,*)+pv1(nLons-2,*))/2.0
  nv1(nLons-1,*) = (pv1(1,*)+pv1(nLons-2,*))/2.0
  nv1(*,0)       = mean(pv1(*,1))
  nv1(*,nLats-1) = mean(pv1(*,nLats-2))
  nv2(0,*)       = (pv2(1,*)+pv2(nLons-2,*))/2.0
  nv2(nLons-1,*) = (pv2(1,*)+pv2(nLons-2,*))/2.0
  nv2(*,0)       = mean(pv2(*,1))
  nv2(*,nLats-1) = mean(pv2(*,nLats-2))

  newlon(0,*)       = 0.0
  newlon(nLons-1,*) = 360.0
  newlat(*,0) = -90.0
  newlat(*,nLats-1) =  90.0

  mini=100
  maxi=600
  nLevels=21
  
  ppp = 2
  space = 0.05
  pos_space, ppp, space, sizes, ny = ppp
  get_position, ppp, space, sizes, 0, pos

  !p.position = pos

  levels = findgen(nlevels)*(maxi-mini)/(nlevels-1) + mini
    
  map_set, 0.0, 180.0-p0lon, /noerase
  contour, nv1, newlon, newlat, $
            /follow, /cell_fill, /over, $
            levels = levels

  contour, nv1, newlon, newlat, $
            /follow, /over, $
            levels = levels, thick = 2

   map_grid, lats = findgen(19)*10-90, glinethick=1
   map_continents

   ctpos = [pos(2)+0.005, pos(1), pos(2)+0.02, pos(3)]
   range = [mini,maxi]
   units = 'hmf2'
   ncolors = 255
   plotct, ncolors, ctpos, range, units, /right

  mini=10
  maxi=12.5
  nLevels=21
  levels = findgen(nlevels)*(maxi-mini)/(nlevels-1) + mini
  
  get_position, ppp, space, sizes, 1, pos
  !p.position = pos
  
  map_set, 0.0, 180.0-p0lon, /noerase
 
  contour, nv2, newlon, newlat, $
            /follow, /cell_fill, /over, $
            levels = levels

  contour, nv2, newlon, newlat, $
            /follow, /over, $
            levels = levels, thick = 2

   map_grid, lats = findgen(19)*10-90, glinethick=1
   map_continents

   ctpos = [pos(2)+0.005, pos(1), pos(2)+0.02, pos(3)]
   range = [mini,maxi]
   units = 'nmf2'
   ncolors = 255
   plotct, ncolors, ctpos, range, units, /right

  closedevice

endfor


end
