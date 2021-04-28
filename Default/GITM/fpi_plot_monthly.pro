
iFPI = 1

FPINums = [1,2,3,4,5,6,7,8,9]
FPINames = ['car','caj','unk','unk','uao','par','eku','ann','vti']
nFpis = n_elements(FPINames)

FPILats =       [-7.28, -6.88, 0.0, 0.0, 40.1097, 35.2417, 37.7447, 42.40, 37.23]
FPILons = 360.0-[36.52, 38.56, 0.0, 0.0, 88.2042, 82.7286, 84.2936, 83.91, 80.42]



gitmfiles = findfile('data/3DALL*.save')
nFiles = n_elements(gitmfiles)

gitmNorth = fltarr(nFpis,nFiles)
gitmEast  = fltarr(nFpis,nFiles)
gitmTemp  = fltarr(nFpis,nFiles)

iT_ = 15
iE_ = 16
iN_ = 17

for iFile = 0,nFiles-1 do begin

   print, gitmfiles(iFile)
   restore, gitmfiles(iFile)

   if (iFile eq 0) then begin

      nVars = n_elements(avedata(*,0,0,0))
      nLons = n_elements(avedata(0,*,0,0))
      nLats = n_elements(avedata(0,0,*,0))
      nAlts = n_elements(avedata(0,0,0,*))

      Lons = reform(avedata(0,*,0,0))/!dtor
      Lats = reform(avedata(1,0,*,0))/!dtor
      Alts = reform(avedata(2,0,0,*))/1000.0

      l = where(Alts gt 240.0)
      iAlt = l(0)

      iLon = intarr(nFpis)
      xLon = fltarr(nFpis)
      iLat = intarr(nFpis)
      xLat = fltarr(nFpis)

      for iFpi = 0, nFpis-1 do begin

         l = where(Lons gt FPILons(iFPI))
         iLon(iFpi) = l(0)-1
         xLon(iFpi) = (FPILons(iFPI) - Lons(iLon(iFpi)))/(Lons(iLon(iFpi)+1)-Lons(iLon(iFpi)))

         l = where(Lats gt FPILats(iFPI))
         iLat(iFpi) = l(0)-1
         xLat(iFpi) = (FPILats(iFPI) - Lats(iLat(iFpi)))/(Lats(iLat(iFpi)+1)-Lats(iLat(iFpi)))

      endfor

   endif

   tempT = reform(avedata(iT_,*,*,iAlt))
   tempE = reform(avedata(iE_,*,*,iAlt))
   tempN = reform(avedata(iN_,*,*,iAlt))

   for iFpi = 0, nFpis-1 do begin

      gitmTemp(iFpi,iFile) = (1.0 - xLon(iFpi)) * (1.0 - xLat(iFpi)) * tempT(iLon(iFpi)  ,iLat(iFpi)  ) + $
                             (      xLon(iFpi)) * (1.0 - xLat(iFpi)) * tempT(iLon(iFpi)+1,iLat(iFpi)  ) + $
                             (1.0 - xLon(iFpi)) * (      xLat(iFpi)) * tempT(iLon(iFpi)  ,iLat(iFpi)+1) + $
                             (      xLon(iFpi)) * (      xLat(iFpi)) * tempT(iLon(iFpi)+1,iLat(iFpi)+1)
      
      gitmNorth(iFpi,iFile) = (1.0 - xLon(iFpi)) * (1.0 - xLat(iFpi)) * tempN(iLon(iFpi)  ,iLat(iFpi)  ) + $
                              (      xLon(iFpi)) * (1.0 - xLat(iFpi)) * tempN(iLon(iFpi)+1,iLat(iFpi)  ) + $
                              (1.0 - xLon(iFpi)) * (      xLat(iFpi)) * tempN(iLon(iFpi)  ,iLat(iFpi)+1) + $
                              (      xLon(iFpi)) * (      xLat(iFpi)) * tempN(iLon(iFpi)+1,iLat(iFpi)+1)
      
      gitmEast(iFpi,iFile) = (1.0 - xLon(iFpi)) * (1.0 - xLat(iFpi)) * tempE(iLon(iFpi)  ,iLat(iFpi)  ) + $
                             (      xLon(iFpi)) * (1.0 - xLat(iFpi)) * tempE(iLon(iFpi)+1,iLat(iFpi)  ) + $
                             (1.0 - xLon(iFpi)) * (      xLat(iFpi)) * tempE(iLon(iFpi)  ,iLat(iFpi)+1) + $
                             (      xLon(iFpi)) * (      xLat(iFpi)) * tempE(iLon(iFpi)+1,iLat(iFpi)+1)

   endfor

endfor

l = strpos(gitmfiles(0),'3DALL_')
ym = strmid(gitmfiles(0),l+6,4)
print, ym

for iFpi = 0, nFpis-1 do begin

   fpifile = FPINames(iFpi)+'_20'+ym+'.save' 
   f = findfile(fpifile)
   if (strlen(f) gt 6) then begin

      restore, fpifile

      nTimes = n_elements(temps(0,*))

      t = findgen(nTimes)/nTimes * 24.0

      gTemp = reform(gitmTemp(iFpi,*))
      gNorth = reform(gitmNorth(iFpi,*))
      gEast = reform(gitmEast(iFpi,*))

      l = where(temps gt 10.0)
      mini = min([min(temps(l)), min(gTemp)])*0.9
      maxi = max([max(temps(l)), max(gTemp)])*1.1

      print, FPINames(iFpi)+'_'+ym+'.ps'
      setdevice, FPINames(iFpi)+'_'+ym+'.ps','p',5

      ppp = 3
      space = 0.05
      pos_space, ppp, space, sizes, ny = ppp

      get_position, ppp, space, sizes, 0, pos, /rect
      pos(0) = pos(0) + 0.1

      plot, t, temps(0,*), yrange = [mini,maxi], min_val = mini, ystyle = 1, $
            ytitle = 'Temperature (K)', xstyle = 1,     $
            pos = pos, title = FPINames(iFpi)+' '+ym

      nT = n_elements(temps(0,*))
      tempAve = fltarr(nT)
      for i=0,nT-1 do begin
         tmp = reform(temps(*,i))
         l = where(tmp gt mini,c)
         if (c gt 0) then tempAve(i) = mean(tmp(l)) else tempAve(i) = -1.0e32
      endfor

      for i=0,3 do oplot, t, temps(i,*), min_val = mini
      oplot, t, tempAve, thick = 5, min_val = mini
      oplot, t, gTemp, thick = 5, linestyle = 2, min_val = mini

      l = where(TempAve gt mini)
      diff = TempAve(l)-gTemp(l)
      rms = sqrt(mean(diff^2))
      mdiff = mean(diff)
      xyouts, pos(2)+0.01, pos(3), 'Diff: '+tostr(mdiff)+'K; RMS: '+tostr(rms)+'K', orient = 270, $
              /norm
   
      l = where(vels gt -1000)
      maxi = max([max(abs(vels(l))), max(abs(gNorth)), max(abs(gEast))])*1.1
      mini = -maxi

      get_position, ppp, space, sizes, 1, pos, /rect
      pos(0) = pos(0) + 0.1

      plot, t, vels(0,*), yrange = [mini,maxi], min_val = mini, ystyle = 1, $
            ytitle = 'North Velocity (m/s)', xstyle = 1,     $
            pos = pos, /noerase

      velAve = fltarr(nT)
      for i=0,nT-1 do begin
         tmp = reform(vels(0:1,i))
         l = where(tmp gt mini,c)
         if (c gt 0) then velAve(i) = mean(tmp(l)) else velAve(i) = -1.0e32
      endfor

      for i=0,1 do oplot, t, vels(i,*), min_val = mini
      oplot, t, velAve, thick = 5, min_val = mini
      oplot, t, gNorth, thick = 5, linestyle = 2, min_val = mini

      l = where(VelAve gt mini)
      diff = VelAve(l)-gNorth(l)
      rms = sqrt(mean(diff^2))
      mdiff = mean(diff)
      xyouts, pos(2)+0.01, pos(3), 'Diff: '+tostr(mdiff)+'m/s; RMS: '+tostr(rms)+'m/s', $
              orient = 270, /norm

      get_position, ppp, space, sizes, 2, pos, /rect
      pos(0) = pos(0) + 0.1
      
      plot, t, vels(2,*), yrange = [mini,maxi], min_val = mini, ystyle = 1, $
            xtitle = 'UT Hours', ytitle = 'East Velocity (m/s)', xstyle = 1,     $
            pos = pos, /noerase

      velAve = fltarr(nT)
      for i=0,nT-1 do begin
         tmp = reform(vels(2:3,i))
         l = where(tmp gt mini,c)
         if (c gt 0) then velAve(i) = mean(tmp(l)) else velAve(i) = -1.0e32
      endfor

      for i=2,3 do oplot, t, vels(i,*), min_val = mini
      oplot, t, velAve, thick = 5, min_val = mini
      oplot, t, gEast, thick = 5, linestyle = 2, min_val = mini

      l = where(VelAve gt mini)
      diff = VelAve(l)-gEast(l)
      rms = sqrt(mean(diff^2))
      mdiff = mean(diff)
      xyouts, pos(2)+0.01, pos(3), 'Diff: '+tostr(mdiff)+'m/s; RMS: '+tostr(rms)+'m/s', $
              orient = 270, /norm

      closedevice

   endif

endfor


end
