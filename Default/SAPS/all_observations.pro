tilt=0
geopack_recalc,2013,138,5,0,0,TILT=tilt
probe='a'
timespan, '2013-05-18/04:15:00',1,/hour
RBSP_LOAD_EMFISIS,probe=probe,cadence='1sec',coord='gsm',level='l3'
get_data,'rbspa_emfisis_l3_1sec_gsm_coordinates',data=data
pos=data.y/6370

timespan,'2013-05-18',1,/day
omni_load_data,res5min='res5min'
get_data,'OMNI_HRO_5min_flow_speed',data=vc
get_data,'OMNI_HRO_5min_proton_density',data=nc
get_data,'OMNI_HRO_5min_BZ_GSM',data=bzc
get_data,'OMNI_HRO_5min_BY_GSM',data=byc
get_data,'OMNI_HRO_5min_Pressure',data=pc
get_data,'OMNI_HRO_5min_SYM_H',data=symhc

v=vc.Y
n=nc.Y
bz=bzc.Y
by=byc.Y
p=pc.Y
symh=symhc.Y

;GEOPACK_GETW,n,v,bz,w
lamda=[0.39,0.46,0.39,0.42,0.41,1.29]
beta=[0.8,0.18,2.32,1.25,1.6,2.4]
gamma=[0.87,0.67,1.32,1.29,0.69,0.53]
r=[0.39,0.7,0.031,0.58,1.15,0.88]

w = MAKE_ARRAY(6, 1, /DOUBLE, VALUE = 0)
s = MAKE_ARRAY(61, 1, /DOUBLE, VALUE = 0)


for i=0,5 do begin
  w[i]=0
  for j=13,60 do begin
    if (finite(n[j])+finite(v[j])+finite(bz[j]) EQ 3) then begin
    s[j]=(abs(n[j])/5)^lamda[i]*(abs(v[j])/400)^beta[i]*(abs(bz[j])/5)^gamma[i]
    w[i]=w[i]+r[i]/12*s[j]*exp(r[i]/60*(j*5-300))
    endif
  endfor
endfor


geopack_trace, pos[18000,0], pos[18000,1], pos[18000,2],-1,[p[60],symh[60],by[60],bz[60],w[0],w[1],w[2],w[3],w[4],w[5]], x, y, z, /TS04
geopack_conv_coord,x,y,z,d1,d2,d3,/TO_MAG,/FROM_GSM
print,90-acos(d3)/3.1415926*180,asin(d2/sqrt(d1^2+d2^2))/3.1415926*180
end