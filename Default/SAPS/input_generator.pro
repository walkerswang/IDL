list='/Users/wzihan/Desktop/input.csv'
sed_data = READ_CSV(list, HEADER=SedHeader,N_TABLE_HEADER=0, TABLE_HEADER=SedTableHeader)
yr=sed_data.FIELD01
month=sed_data.FIELD02
day=sed_data.FIELD03
hr=sed_data.FIELD04
mn=sed_data.FIELD05
sec=sed_data.FIELD06
msec=sed_data.FIELD07
bxgsm=sed_data.FIELD08
bygsm=sed_data.FIELD09
bzgsm=sed_data.FIELD10
vxgsm=sed_data.FIELD11
vygsm=sed_data.FIELD12
vzgsm=sed_data.FIELD13
den=sed_data.FIELD14
temp=sed_data.FIELD15
n=420

FNAME='/USERS/WZIHAN/DESKTOP/INPUT.TXT'
OPENW,1,FNAME 
for i=0,n-1 do begin
  printf,1,format='(7i4,3f10.5,4f10.3,f14.2)',$
    yr[i],month[i],day[i],hr[i],mn[i],sec[i],msec[i],bxgsm[i],bygsm[i],$
    bzgsm[i],vxgsm[i],vygsm[i],vzgsm[i],den[i],temp[i]
endfor
CLOSE,1
end