

filelist = findfile('3DALL*.bin')
gitm_read_header, filelist, time, nVars, Vars, nLons, nLats, nAlts, version

nTimes = n_elements(time)
dt = time(1:nTimes-1) - time(0:nTimes-2)
dt = float(round(median(dt)))
nTimesPerDay = fix(86400.0 / dt)
nDays = round(float(nTimes)/float(nTimesPerDay))

for iT = 0, nTimesPerDay-1 do begin

   for iDay = 0,nDays-1 do begin

      t = time(0) + dt * iDay * nTimesPerDay + iT * dt
      d = abs(time-t)
      l = where(d eq min(d))
      file = filelist(l)
      print, iT, iDay, ': ',file

      gitm_read_bin, file, data1, time1, nVars, Vars, version

      if (iDay eq 0) then begin
         nFiles = 1
         avedata = data1
         avefiles = [file]
      endif else begin
         avedata = avedata + data1
         avefiles = [avefiles, file]
         nFiles++
      endelse

   endfor

   c_r_to_a, itime, t
   c_a_to_ymdhms, itime, ymdhms, /twoyear
   filebase = strmid(file,0,6)+strmid(ymdhms,0,4)+'_'+strmid(ymdhms,6,4)+'.monthly'
   print, filebase

   avedata = avedata/nFiles

   save, file = filebase+'.ave.save', avedata, avefiles, nFiles

endfor


end
