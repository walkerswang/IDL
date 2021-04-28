;magnetic pole (83.2973     -85.2189)  (175,139)
FOLDER='/Users/wzihan/Simulations/data/trace/'
figfolder='/Users/wzihan/Simulations/'
allFILELIST = FINDFILE(FOLDER+'3DALL_t170907_230000.bin')
ionFILELIST = FINDFILE(FOLDER+'3DION_t170907_230000.bin')

north = 1

nfiles = n_elements(allfilelist)
g=9.8

ff = 0
lf = nfiles-1

for ifile = ff, lf do begin

  filename = allfilelist(iFile)

  print,filename
  ;read 3dall (neutral wind)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  P = STRPOS(FILEname, '3DALL')
  IF (P EQ 0) THEN P = STRLEN(FILE)
  figfile= figfolder+'plot_new/nw_effect_350km/'+strmid(filename,31,20)+'nwconv_comp_vs_alt'

  ;16. V!Dn!N(east)
  ;17. V!Dn!N(north)
  ;18. V!Dn!N(up)
  ilon=119 ;lon=235 231
  ilat=150 ;lat=58.5 63.5
  
  vn_east = reform(data(16,ilon,ilat,*))
  vn_north = reform(data(17,ilon,ilat,*))
  vn_up = reform(data(18,ilon,ilat,*))
  n= reform(data(34,ilon,ilat,*))
  Alt=reform(data(2,0,0,*))
  ;read 3dion (collisional freq and electric field and magnetic field)

  filename = ionfilelist(iFile)
  read_thermosphere_file, filename, nvars, nalts, nlats, nlons,vars,data, $
    nBLKlat, nBLKlon, nBLK, iTime, Version

  b_east = reform(data(24,ilon,ilat,*))
  b_north = reform(data(25,ilon,ilat,*))
  b_up = reform(data(26,ilon,ilat,*))
  b_mag = reform(data(27,ilon,ilat,*))
  e_east = reform(data(29,ilon,ilat,*))
  e_north = reform(data(30,ilon,ilat,*))
  e_up = reform(data(31,ilon,ilat,*))
  vi_up_gitm=reform(data(17,ilon,ilat,*))

  vn_dot_b=fltarr(nalts)
  mu_in = reform(data(33,ilon,ilat,*))
  vi_nw =fltarr(nalts)
  vi_conv =fltarr(nalts)
  D =fltarr(nalts)
  I =fltarr(nalts)

  mu_in_ave=fltarr(nlons,nlats)
  ;calculate height averaged mu_in
  
  mu_in_total=0.0
  n_total=0.0
  for iAlt= 0, nAlts-2 do begin
        dAlt= Alt(iAlt+1)-Alt(iAlt)
        n_total = n_total + dAlt*n(iAlt)
        mu_in_total = mu_in_total + dAlt*n(iAlt)*mu_in(iAlt)
   endfor
   mu_in_ave=mu_in_total/n_total

  for ialt = 0, nalts-1 do begin
      I[ialt]=asin(-b_up[ialt]/b_mag[ialt])
      D[ialt]=atan(b_east[ialt]/b_north[ialt])
      vn_dot_b[ialt]=(vn_north[ialt]*b_north[ialt]+vn_east[ialt]*b_east[ialt]+vn_up[ialt]*b_up[ialt])
      vi_nw[ialt]=sin(I[ialt])*(-vn_dot_b[ialt]/b_mag[ialt]-g/mu_in_ave*sin(I[ialt])) ;-g/mu_in[iLon,iLat]*sin(I)
      vi_conv[ialt]=(e_east[ialt]*b_north[ialt]-e_north[ialt]*b_east[ialt])/b_mag[ialt]^2
  endfor

  p1=plot(-vi_nw[2:51],alt[2:51],NAME='Neutral Wind',xtitle='Verticle (m/s)',ytitle='Alt (m)')
  p2=plot(vi_conv[2:51],alt[2:51],'-r',/overplot,Name='Convection')
  leg = LEGEND(TARGET=[p1,p2], POSITION=[800,2e5], /DATA, /AUTO_TEXT_COLOR)
  p2.save,figfolder+'plot_new/nw_effect_350km/'+strmid(filename,37,20)+'nw_effect_vs_alt.ps'
endfor


end
