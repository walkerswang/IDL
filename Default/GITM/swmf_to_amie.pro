root=ask('Root folder: ', root)
IonosphereFiles = findfile(root)
nFilesIono   = n_elements(IonosphereFiles)
output=ask('Output folder: ', output)

for iFile=0,nFilesIono-1 do begin

   IonoFile =root+IonosphereFiles(iFile)
   print, 'Reading file....       ',IonoFile

   iono_read_file, IonoFile, nVarsIono, nLatsIono, nLonsIono, $
                   IonoVars, IonoTime, IonoData

   if (iFile eq 0) then begin

      nLats = nLatsIono
      nMlts = nLonsIono

      imf   = fltarr(nFilesIono,4)
      ae    = fltarr(nFilesIono,4)
      dst   = fltarr(nFilesIono,2)
      hp    = fltarr(nFilesIono,2)
      cpcp  = fltarr(nFilesIono)
      cpcpS = fltarr(nFilesIono)
      t     = dblarr(nFilesIono)

      Vars = ['Potential (V)', $
              'Energy Flux (ergs/cm2)','Mean Energy (eV)']

      nVars = n_elements(Vars)
      AmieData  = fltarr(nFilesIono, nVars, nmlts, nlats)
      AmieDataS = fltarr(nFilesIono, nVars, nmlts, nlats)

      lats  = 90.0 - reform(IonoData(0,0,0,*))
      latsS = 90.0 - reverse(reform(IonoData(1,0,0,*)) - 90.0)
      mlts = (reform(IonoData(0,1,*,0))/15.0 + 12.0) mod 24.0

      c_r_to_a, itime, ionotime
      c_a_to_ymd, itime, ymd

   endif

   t(iFile) = ionotime

   c_r_to_a, itime, t(iFile)
   print, itime

   for iMLT = 0, nMlts-1 do begin
      i = iMLT 

      AmieData(iFile,0,iMLT,*) = IonoData(0,5,i,*)*1000.0 ; kV -> V
      AmieData(iFile,1,iMLT,*) = IonoData(0,6,i,*)*1000.0 ; W/m2 -> ergs/cm2/s
      AmieData(iFile,2,iMLT,*) = IonoData(0,7,i,*)

      AmieDataS(iFile,0,iMLT,*) = reverse(IonoData(1,5,i,*))*1000.0
      AmieDataS(iFile,1,iMLT,*) = reverse(IonoData(1,6,i,*))*1000.0
      AmieDataS(iFile,2,iMLT,*) = reverse(IonoData(1,7,i,*))

   endfor

   cpcp(iFile)  = max(AmieData (iFile,0,*,*)) - min(AmieData (iFile,0,*,*))
   cpcpS(iFile) = max(AmieDataS(iFile,0,*,*)) - min(AmieDataS(iFile,0,*,*))

endfor

Version = 1.0

FileOut = output+'amie_'+ymd+'n.swmf'
amie_write_binary, fileout, Vars, lats, mlts, t, AmieData, $
                   imf = imf, ae = ae, dst = dst, hpi = hp, cpcp = cpcp, $
                   Version = Version

FileOut = output+'amie_'+ymd+'s.swmf'
amie_write_binary, fileout, Vars, latsS, mlts, t, AmieDataS, $
                   imf = imf, ae = ae, dst = dst, hpi = hp, cpcp = cpcpS, $
                   Version = Version


end
