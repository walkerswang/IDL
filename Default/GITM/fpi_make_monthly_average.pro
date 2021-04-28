
pro make_monthly, time, value, dt, meds, stds, npts

  second = time mod 86400.0
  nTimes = fix(86400.0 / dt)
  iter = round(time/dt) mod nTimes
  meds = fltarr(nTimes)-1.0e32
  stds = fltarr(nTimes)
  npts = intarr(nTimes)

  for i=0,nTimes-1 do begin

     l = where(iter eq i,c)
     if (c gt 0) then meds(i) = median(value(l))
     if (c gt 1) then stds(i) = stddev(value(l))
     npts(i) = c

  endfor

end

dt = 1800.0
dir='/raid3/Data/FPI/illinois/database/'
spawn,'pwd',pwd
l = strpos(pwd,'Runs')
yyyymm = strmid(pwd,l+5,6)
print, yyyymm

FPINames = ['car','caj','uao','par','eku','ann','vti']

nFpis = n_elements(FPINames)

for iFpi = 0, nFpis-1 do begin

   station = FPINames(iFpi)

   filelist = findfile(dir+'minime??_'+station+'_'+yyyymm+'*.txt')
   nFiles = n_elements(filelist)

   if (nFiles gt 1) then begin

      fpi_read_makela_all,filelist, fpi, all
      
      meantime = mean(all.time)
      c_r_to_a, itime, meantime
      c_a_to_ymd, itime, ymd
      outfile = station+'_'+strmid(ymd,0,6)+'.save'

      time = fpi.NorthTime
      value = fpi.northTemp
      flag = fpi.northTflag
      l = where(flag le 1)
      make_monthly, time(l), value(l), dt, northtemp, northtempstd, northtempnpts

      time = fpi.SouthTime
      value = fpi.southTemp
      flag = fpi.southTflag
      l = where(flag le 1)
      make_monthly, time(l), value(l), dt, southtemp, southtempstd, southtempnpts

      time = fpi.EastTime
      value = fpi.eastTemp
      flag = fpi.eastTflag
      l = where(flag le 1)
      make_monthly, time(l), value(l), dt, easttemp, easttempstd, easttempnPts

      time = fpi.WestTime
      value = fpi.westTemp
      flag = fpi.westTflag
      l = where(flag le 1)
      make_monthly, time(l), value(l), dt, westtemp, westtempstd, westtempnPts

      nPts = n_elements(westtemp)
      temps = fltarr(4,nPts)
      tempstd = fltarr(4,nPts)
      tempnPts = intarr(4,nPts)

      temps(0,*) = northtemp
      temps(1,*) = southtemp
      temps(2,*) = easttemp
      temps(3,*) = westtemp
      tempstd(0,*) = northtempstd
      tempstd(1,*) = southtempstd
      tempstd(2,*) = easttempstd
      tempstd(3,*) = westtempstd
      tempnPts(0,*) = northtempnPts
      tempnPts(1,*) = southtempnPts
      tempnPts(2,*) = easttempnPts
      tempnPts(3,*) = westtempnPts

      plot, northtemp, min_val = 0.0
      oplot, southtemp, min_val = 0.0
      oplot, easttemp, min_val = 0.0
      oplot, westtemp, min_val = 0.0

      time = fpi.NorthTime
      value = fpi.north
      flag = fpi.northVflag
      l = where(flag le 1)
      make_monthly, time(l), value(l), dt, north, northstd, northnPts

      time = fpi.SouthTime
      value = fpi.south
      flag = fpi.southVflag
      l = where(flag le 1)
      make_monthly, time(l), value(l), dt, south, southstd, southnPts

      plot, north, min_val = -1000.0
      oplot, south, min_val = -1000.0
      both = (north+south)/2.0
      oplot, both, min_val = -1000.0, thick = 4

      time = fpi.EastTime
      value = fpi.east
      flag = fpi.eastVflag
      l = where(flag le 1)
      make_monthly, time(l), value(l), dt, east, eaststd, eastnPts

      time = fpi.WestTime
      value = fpi.west
      flag = fpi.westVflag
      l = where(flag le 1)
      make_monthly, time(l), value(l), dt, west, weststd, westnPts

      vels = fltarr(4,nPts)
      velstd = fltarr(4,nPts)
      velnPts = intarr(4,nPts)
   
      vels(0,*) = north
      vels(1,*) = south
      vels(2,*) = east
      vels(3,*) = west
      velstd(0,*) = northstd
      velstd(1,*) = southstd
      velstd(2,*) = eaststd
      velstd(3,*) = weststd
      velnPts(0,*) = northnPts
      velnPts(1,*) = southnPts
      velnPts(2,*) = eastnPts
      velnPts(3,*) = westnPts

      save, file=outfile, temps, tempstd, tempnpts, vels, velstd, velnPts

      plot, east, min_val = -1000.0
      oplot, west, min_val = -1000.0
      both = (east+west)/2.0
      oplot, both, min_val = -1000.0, thick = 4

   endif

endfor

end
