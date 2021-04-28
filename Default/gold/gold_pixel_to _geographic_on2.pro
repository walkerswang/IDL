filelist = file_search('/Users/wzihan/data/gold/on2/*.nc')
nFiles = n_elements(filelist)

for iFile = 0, nfiles-1 do begin
  if strpos(filelist[ifile], '2020_049') gt -1 then begin
    READ_GOLD_NCDF, filelist[ifile], test_data
    nscan=n_elements(test_data.dqi)
    st=test_data.scan_start_time
    nsize=size(test_data.latitude,dimension=1)
    dqi=test_data.on2_dqi
    lat=test_data.latitude
    lon=test_data.longitude
    on2=test_data.ON2

    for ns=0,nscan-1 do begin

      la=[]
      lo=[]
      ratio=[]
      time=st[ns]

      for i=0, nsize[0]-1 do begin
        for j=0, nsize[1]-1 do begin
          if ~FINITE(lat[i,j], /NAN) and ~FINITE(lon[i,j], /NAN) and ~FINITE(on2[i,j,ns], /NAN) then begin
            if dqi[i,j,ns] eq 0 then begin
              la=[la,lat[i,j]]
              lo=[lo,lon[i,j]]
              ratio=[ratio,on2[i,j,ns]]
            endif
          endif
        endfor
      endfor

      save,time,la,lo,ratio,filename='/Users/wzihan/data/gold/on2_sav/on2'+st[ns]+'.sav'
    endfor
  endif
endfor
end