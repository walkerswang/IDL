;0.        96.567226
;1.        98.283613
;2.        100.00000
;3.        101.71639
;4.        103.46835
;5.        105.26575
;6.        107.12198
;7.        109.05566
;8.        111.09380
;9.        113.27758
;10.        115.66624
;11.        118.34096
;12.        121.41182
;13.        125.00337
;14.        129.15339
;15.        133.88501
;16.        139.23474
;17.        145.22976
;18.        151.88663
;19.        159.21105
;20.        167.19860
;21.        175.83624
;22.        185.10363
;23.        194.97557
;24.        205.42356
;25.        216.41716
;26.        227.92506
;27.        239.91554
;28.        252.35671
;29.        265.21654
;30.        278.46280
;31.        292.06310
;32.        305.98505
;33.        320.19649
;34.        334.66614
;35.        349.36381
;36.        364.26095
;37.        379.33108
;38.        394.54999
;39.        409.89596
;40.        425.34972
;41.        440.89446
;42.        456.51562
;43.        472.20071
;44.        487.93915
;45.        503.72199
;46.        519.54170
;47.        535.39200
;48.        551.26765
;49.        567.16429
;50.        583.07831
;51.        599.00671
;52.        614.94702
;53.        630.88733

;0. Longitude
;1. Latitude
;2. Altitude
;3. Rho
;4. [O(!U3!NP)]
;5. [O!D2!N]
;6. [N!D2!N]
;7. [N(!U4!NS)]
;8. [NO]
;9. [He]
;10. [N(!U2!ND)]
;11. [N(!U2!NP)]
;12. [H]
;13. [CO!D2!N]
;14. [O(!U1!ND)]
;15. Temperature
;16. V!Dn!N(east)
;17. V!Dn!N(north)
;18. V!Dn!N(up)
;19. V!Dn!N(up,O(!U3!NP))
;20. V!Dn!N(up,O!D2!N)
;21. V!Dn!N(up,N!D2!N)
;22. V!Dn!N(up,N(!U4!NS))
;23. V!Dn!N(up,NO)
;24. V!Dn!N(up,He)
;25. [O_4SP_!U+!N]
;26. [O!D2!U+!N]
;27. [N!D2!U+!N]
;28. [N!U+!N]
;29. [NO!U+!N]
;30. [O(!U2!ND)!U+!N]
;31. [O(!U2!NP)!U+!N]
;32. [H!U+!N]
;33. [He!U+!N]
;34. [e-]
;35. eTemperature
;36. iTemperature
;37. V!Di!N(east)
;38. V!Di!N(north)
;39. V!Di!N(up)

;0. Longitude
;1. Latitude
;2. Altitude
;3. [O_4SP_!U+!N]
;4. [O!D2!U+!N]
;5. [N!D2!U+!N]
;6. [N!U+!N]
;7. [NO!U+!N]
;8. [O(!U2!ND)!U+!N]
;9. [O(!U2!NP)!U+!N]
;10. [H!U+!N]
;11. [He!U+!N]
;12. [e-]
;13. eTemperature
;14. iTemperature
;15. V!Di!N(east)
;16. V!Di!N(north)
;17. V!Di!N(up)
;18. Ed1
;19. Ed2
;20. Je1
;21. Je2
;22. MagneticLatitude
;23. MagneticLongitude
;24. B.F.East
;25. B.F.North
;26. B.F.Vertical
;27. B.F.Magnitude
;28. Potential
;29. E.F.East
;30. E.F.North
;31. E.F.Vertical
;32. E.F.Magnitude
;33. INCollisionFreq
;34. PressGrad(east)
;35. PressGrad(north)
;36. PressGrad(up)

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
makect, 'bwr'
for ifile = ff, lf do begin

  filename = allfilelist(iFile)

  print,filename
  ;read 3dall (neutral wind)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  P = STRPOS(FILEname, '3DALL')
  IF (P EQ 0) THEN P = STRLEN(FILE)
  figfile= figfolder+'plot_new/nw_effect_200km/'+strmid(filename,31,20)+'nw_effect_v2_200km'

  setdevice, figfile+'.ps','p',5

  ialt=24

  ;16. V!Dn!N(east)
  ;17. V!Dn!N(north)
  ;18. V!Dn!N(up)
  vn_east = reform(data(16,*,*,ialt))
  vn_north = reform(data(17,*,*,ialt))
  vn_up = reform(data(18,*,*,ialt))
  n= reform(data(34,*,*,*))
  nLons = n_elements(vn_up(*,0))
  nLats = n_elements(vn_up(0,*))
  nAlts=  n_elements(n(0,0,*))
  Alt=reform(data(2,0,0,*))
  vi_up_gitm=reform(data(39,*,*,ialt))
  ;read 3dion (collisional freq and electric field and magnetic field)

  filename = ionfilelist(iFile)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  ;24. B.F.East
  ;25. B.F.North
  ;26. B.F.Vertical
  ;27. B.F.Magnitude
  ;28. Potential
  ;29. E.F.East
  ;30. E.F.North
  ;31. E.F.Vertical
  ;32. E.F.Magnitude
  ;33. INCollisionFreq
  b_east = reform(data(24,*,*,ialt))
  b_north = reform(data(25,*,*,ialt))
  b_up = reform(data(26,*,*,ialt))
  b_mag = reform(data(27,*,*,ialt))
  e_east = reform(data(29,*,*,ialt))
  e_north = reform(data(30,*,*,ialt))
  e_up = reform(data(31,*,*,ialt))

  vn_dot_b=fltarr(nlons,nlats)
  mu_in = reform(data(33,*,*,*))
  vi_nw =fltarr(nlons,nlats)
  rat =fltarr(nlons,nlats)
  vi_conv =fltarr(nlons,nlats)
  D =fltarr(nlons,nlats)
  I =fltarr(nlons,nlats)

  mu_in_ave=fltarr(nlons,nlats)
  ;calculate height averaged mu_in
  for iLon = 0, nlons-1 do begin
    for iLat = 0, nlats-1 do begin
      mu_in_total=0.0
      n_total=0.0
      for iAlt= 0, nAlts-2 do begin
        dAlt= Alt(iAlt+1)-Alt(iAlt)
        n_total = n_total + dAlt*n(iLon,iLat,iAlt)
        mu_in_total = mu_in_total + dAlt*n(iLon,iLat,iAlt)*mu_in(iLon,iLat,iAlt)
      endfor
      mu_in_ave(iLon,iLat)=mu_in_total/n_total
    endfor
  endfor

  for iLon = 0, nlons-1 do begin
    for iLat = 0, nlats-1 do begin
      I[ilon,ilat]=asin(-b_up[iLon,iLat]/b_mag[iLon,iLat])
      D[ilon,ilat]=atan(b_east[iLon,iLat]/b_north[iLon,iLat])
      vn_dot_b[ilon,ilat]=(vn_north[iLon,iLat]*b_north[iLon,iLat]+vn_east[iLon,iLat]*b_east[iLon,iLat]+vn_up[iLon,iLat]*b_up[ilon,iLat])
      vi_nw[iLon,iLat]=sin(I[ilon,ilat])*(-vn_dot_b[ilon,ilat]/b_mag[iLon,iLat]-g/mu_in_ave[iLon,iLat]*sin(I[ilon,ilat])) ;-g/mu_in[iLon,iLat]*sin(I)
      vi_conv[iLon,iLat]=(e_east[iLon,iLat]*b_north[iLon,iLat]-e_north[iLon,iLat]*b_east[iLon,iLat])/b_mag[iLon,iLat]^2
      if abs(vi_nw[iLon,iLat]) lt abs(vi_conv[iLon,iLat]) and vi_up_gitm[iLon,iLat] lt 0 then rat[iLon,iLat]=0
      if abs(vi_nw[iLon,iLat]) lt abs(vi_conv[iLon,iLat]) and vi_up_gitm[iLon,iLat] ge 0 then rat[iLon,iLat]=1
      if abs(vi_nw[iLon,iLat]) ge abs(vi_conv[iLon,iLat]) and vi_up_gitm[iLon,iLat] lt 0 then rat[iLon,iLat]=2
      if abs(vi_nw[iLon,iLat]) ge abs(vi_conv[iLon,iLat]) and vi_up_gitm[iLon,iLat] ge 0 then rat[iLon,iLat]=3
    endfor
  endfor

  lon = (reform(data(0,*,*,0))*180/!pi + 360.0) mod 360.0
  utime = itime(3)*3600.0 +itime(4)*60.0 + itime(5)
  utime = utime(0)

  lat = reform(data(1,*,*,0))*180/!pi
  localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0

  ppp = 3
  space = 0.05
  pos_space, ppp, space, sizes, ny = ppp
  get_position, ppp, space, sizes, 0, pos

  ctpos = pos
  ctpos(0) = pos(2)+0.01
  ctpos(2) = ctpos(0)+0.03

  v1=vi_nw(1:nLons-2,1:nLats-2)/vi_conv(1:nLons-2,1:nLats-2);vi_up_gitm(1:nLons-2,1:nLats-2);vi_nw(1:nLons-2,1:nLats-2)
  v2=vi_conv(1:nLons-2,1:nLats-2)/(vi_conv(1:nLons-2,1:nLats-2)+vi_nw(1:nLons-2,1:nLats-2));+vi_nw(1:nLons-2,1:nLats-2)
  v3=vi_nw(1:nLons-2,1:nLats-2)
  v4=vi_conv(1:nLons-2,1:nLats-2)
  v5=v3+v4
  v6=vi_up_gitm(1:nLons-2,1:nLats-2)
  v7=rat(1:nLons-2,1:nLats-2)
  v8=v6-v3-v4

  newlat = lat(1:nLons-2,1:nLats-2)
  newlon = lon(1:nLons-2,1:nLats-2)
  nLons  = nLons-2
  nLats  = nLats-2

  pv1=v3
  pv2=v4
  pv3=v8
  nv1=pv1
  nv2=pv2
  nv3=pv3
  
  nv1(0,*) = (pv1(1,*)+pv1(nLons-2,*))/2.0
  nv1(nLons-1,*) = (pv1(1,*)+pv1(nLons-2,*))/2.0
  nv1(*,0)       = mean(pv1(*,1))
  nv1(*,nLats-1) = mean(pv1(*,nLats-2))
  nv2(0,*)       = (pv2(1,*)+pv2(nLons-2,*))/2.0
  nv2(nLons-1,*) = (pv2(1,*)+pv2(nLons-2,*))/2.0
  nv2(*,0)       = mean(pv2(*,1))
  nv2(*,nLats-1) = mean(pv2(*,nLats-2))
  nv3(0,*)       = (pv3(1,*)+pv3(nLons-2,*))/2.0
  nv3(nLons-1,*) = (pv3(1,*)+pv3(nLons-2,*))/2.0
  nv3(*,0)       = mean(pv3(*,1))
  nv3(*,nLats-1) = mean(pv3(*,nLats-2))

  newlon(0,*)       = 0.0
  newlon(nLons-1,*) = 360.0
  newlat(*,0) = -90.0
  newlat(*,nLats-1) =  90.0

  mini=-100
  maxi=100
  nLevels=31
  no00 = 1
  no06 = 1
  no12 = 0
  no18 = 0
  maxrange=80

  contour_circle, nv1, newlon[*,0]+utime/3600*15, newlat[0,*], $
    mini = mini, maxi = maxi, $
    nLevels = nLevels, $
    no00 = no00, no06 = no06, no12 = no12, no18 = no18, $
    pos = pos, $
    maxrange = maxrange

  plotct, 255, ctpos, [-100,100],'vi_up_nw (m/s)', /right

  get_position, ppp, space, sizes, 1, pos

  ctpos = pos
  ctpos(0) = pos(2)+0.01
  ctpos(2) = ctpos(0)+0.03

  mini=-100
  maxi=100
  nLevels=31
  no00 = 1
  no06 = 1
  no12 = 0
  no18 = 0
  maxrange=80

  contour_circle, nv2, newlon[*,0]+utime/3600*15, newlat[0,*], $
    mini = mini, maxi = maxi, $
    nLevels = nLevels, $
    no00 = no00, no06 = no06, no12 = no12, no18 = no18, $
    pos = pos, $
    maxrange = maxrange

  plotct, 255, ctpos, [-100,100], 'vi_up_conv (m/s)', /right

  get_position, ppp, space, sizes, 2, pos

  ctpos = pos
  ctpos(0) = pos(2)+0.01
  ctpos(2) = ctpos(0)+0.03
  
  mini=-100
  maxi=100
  nLevels=31
  no00 = 1
  no06 = 1
  no12 = 0
  no18 = 0
  maxrange=80

  contour_circle, nv3, newlon[*,0]+utime/3600*15, newlat[0,*], $
    mini = mini, maxi = maxi, $
    nLevels = nLevels, $
    no00 = no00, no06 = no06, no12 = no12, no18 = no18, $
    pos = pos, $
    maxrange = maxrange

  plotct, 255, ctpos, [-100,100],'vi_up_diff (m/s)', /right

endfor


end
