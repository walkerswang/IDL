pro cvrt_AE_HP_v2
; this code is written to convert the AE index to hemospheric power and hemispheric power index
;citation: Østgaard, N., et al. A relation between the energy deposition by electron precipitation 
;and geomagnetic indices during substorms, J. Geophys. Res., 107(A9), 1246, doi:10.1029/2001JA002003, 2002.
;http://onlinelibrary.wiley.com/doi/10.1029/GL013i007p00656/epdf for conversion between hp in GW to hpi
;UA[GW] = 4.4*sqrt(AL) − 7.6

date='2017-05-26'
read,date,prompt='Enter date(YYYY-MM-DD):'
year=strmid(date,0,4) & month=strmid(date,5,2) & day=strmid(date,8,2)
doy=julday(fix(month),fix(day),fix(year))-julday(1,0,fix(year))
restore,'/Users/wzihan/IDLWorkspace/Default/Geomagnetic_Indices/'+year+'_HP.sav'
outfile='/Users/wzihan/IDLWorkspace/Default/Geomagnetic_Indices/'+'power_'+date+'.txt'
openw,unit,outfile,/get_lun
printf,unit,':Data_list: power_2013.txt'
printf,unit,':Created: Tue Jan 27 20:00:00 UTC 2014'
printf,unit
printf,unit,'# Prepared by the U.S. Dept. of Commerce, NOAA, Space Environment Center.'
printf,unit,'# Please send comments and suggestions to sec@sec.noaa.gov'
printf,unit,'#'
printf,unit,'# Source: NOAA POES (Whatever is aloft)'
printf,unit,'# Units: gigawatts'
printf,unit
printf,unit,'# Format:'
printf,unit
printf,unit,'# Each line is formatted as in this example:'
printf,unit
printf,unit,'# 2006-09-05 00:54:25 NOAA-16 (S)  7  29.67   0.82'
printf,unit
printf,unit,'# A19   Date and UT at the center of the polar pass as YYYY-MM-DD hh:mm:ss'
printf,unit,'# 1X    (Space)'
printf,unit,'# A7    NOAA POES Satellite number'
printf,unit,'# 1X    (Space)'
printf,unit,'# A3    (S) or (N) - hemisphere'
printf,unit,'# I3    Hemispheric Power Index (activity level)'
printf,unit,'# F7.2  Estimated Hemispheric Power in gigawatts'
printf,unit,'# F7.2  Normalizing factor'
printf,unit
printf,unit

for i=0,23 do begin
  for j=0,59 do begin
    hour=strtrim(string(i),2)
    if(i le 9) then hour='0'+hour
    mins=strtrim(string(j),2)
    if(j le 9) then mins='0'+mins
    time=hour+':'+mins+':30'
    printf,unit,date,time,'NOAA-00 (N)  0',hp[doy,i,j],1.00,format='(a10,1x,a8,1x,a14,f7.2,f7.2)'
  endfor
endfor
close,unit
free_lun,unit
print
End