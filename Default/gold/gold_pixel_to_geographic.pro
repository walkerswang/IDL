filelist = file_search('/Users/wzihan/data/gold/nmax/*.nc')
nFiles = n_elements(filelist)

for iFile = 0, nfiles-1 do begin
  READ_GOLD_NCDF, filelist[ifile], test_data
  nscan=n_elements(test_data.dqi)
  st=test_data.scan_start_time
  ;index=where(st eq '2018-10-24T22:10:33Z')
  nsize=size(test_data.latitude,dimension=1)
  dqi=test_data.nmax_dqi
  lat=test_data.latitude
  lon=test_data.longitude
  ir=test_data.RADIANCE_OI_1356
  nm=test_data.NMAX
  
  for ns=0,nscan-1 do begin
    
    la=[]
    lo=[]
    oi=[]
    nmax=[]
    time=st[ns]
    
    for i=0, nsize[0]-1 do begin
      for j=0, nsize[1]-1 do begin
        if ~FINITE(lat[i,j,ns], /NAN) and ~FINITE(lon[i,j,ns], /NAN) and ~FINITE(ir[i,j,ns], /NAN) then begin
            if dqi[i,j,ns] eq 0 then begin
              la=[la,lat[i,j,ns]]
              lo=[lo,lon[i,j,ns]]
              oi=[oi,ir[i,j,ns]]
              nmax=[nmax,nm[i,j,ns]]
            endif
        endif
      endfor
    endfor

    save,time,la,lo,oi,nmax,filename='/Users/wzihan/data/gold/nmax_sav/nmax'+st[ns]+'.sav'
  endfor
endfor
end