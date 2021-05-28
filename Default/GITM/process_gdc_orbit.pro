data=read_csv('/Users/wzihan/Dropbox (University of Michigan)/GDC_Ephemeris/gdc_idl.csv', HEADER=Header)

print, header

index=data.FIELD01
n=n_elements(index)

satut=dblarr(n,6)
satlat=dblarr(n,6)
satlon=dblarr(n,6)

satlat(*,0)=data.FIELD02
satlat(*,1)=data.FIELD04
satlat(*,2)=data.FIELD06
satlat(*,3)=data.FIELD08
satlat(*,4)=data.FIELD10
satlat(*,5)=data.FIELD12

satlon(*,0)=data.FIELD03
satlon(*,1)=data.FIELD05
satlon(*,2)=data.FIELD07
satlon(*,3)=data.FIELD09
satlon(*,4)=data.FIELD11
satlon(*,5)=data.FIELD13

satut(*,0)=data.FIELD14
satut(*,1)=data.FIELD15
satut(*,2)=data.FIELD16
satut(*,3)=data.FIELD17
satut(*,4)=data.FIELD18
satut(*,5)=data.FIELD19

save, satlat, satlon, satut, filename='/Users/wzihan/Dropbox (University of Michigan)/GDC_Ephemeris/gdc_idl.sav'
end
