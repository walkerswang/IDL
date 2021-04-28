if (n_elements(filelist) eq 0) then begin
  filelist = findfile("-t /*.save")
  if (strlen(filelist(0)) eq 0) then filelist = findfile("-t *.bin")
endif

foldername='/Users/wzihan/Simulations/data/'
wildcard = foldername+'3DION_t170907_210000.bin'
filelist=findfile(wildcard)

nfiles = n_elements(filelist)

if (nFiles eq 1 and strlen(filelist(0)) eq 0) then begin
  print, 'can not find file!'
  stop
endif

iszonalaverage = 0

for iFile = 0, nFiles-1 do begin

  filename = filelist(iFile)

  print, 'Reading file ',filename

  read_thermosphere_file, filename, nvars, nalts, nlats, nlons, $
    vars, data, rb, cb, bl_cnt, iTime, Version
  
  op=fltarr(4,32400)
  op(0,*) = reform(data(1,2:181,2:181,35),1,180*180) / !dtor
  op(1,*) = reform(data(0,2:181,2:181,35),1,180*180) / !dtor
  op(2,*)=reform(data(12,2:181,2:181,35),1,180*180) 
  op(3,*)=reform(data(28,2:181,2:181,35),1,180*180) 
  write_csv,'eden_2100.csv',op
  endfor
  end