foldername='/Users/wzihan/Simulations/data/'
filename = foldername+'3DALL_t170907_230000.bin'

print, 'Reading file ',filename

read_thermosphere_file, filename, nvars, nalts, nlats, nlons, $
    vars, data, rb, cb, bl_cnt, iTime, Version

lat_start=40

lat_end=60

eden=reform(data(34,105:125,lat_start+92:lat_end+92,35))

m=max(eden,index)

ind = ARRAY_INDICES(eden, index)

lat = reform(data(1,105+ind[0],lat_start+92+ind[1],35))/3.1415926*180

lon = reform(data(0,105+ind[0],lat_start+92+ind[1],35))/3.1415926*180

print,lat,lon
end

;PATCH LOCATED AT 63.5 231
;BASE LOCATED AT 50.5 247
;Interpolate 53.75 235
;Interpolate 57 239
;Interpolate 60.25 243
;valley 58.5 235