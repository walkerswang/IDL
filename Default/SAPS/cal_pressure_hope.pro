probe='a' ;b

date1='2013-05-18/04:30:00'

date2='2013-05-18/05:30:00'

timespan,date1,1,/hour

rbsp_load_ect_l3,probe,'hope',get_support_data=get_support_data

rbsp_load_mageis_l2,probe=probe,/get_mag_ephem

get_data,'rbsp'+probe+'_ect_mageis_L2_FPSA',data=mageis_flux

get_data,'rbsp'+probe+'_ect_hope_L3_Tperp_e_200',data=temp_e
get_data,'rbsp'+probe+'_ect_hope_L3_Tperp_he_30',data=temp_he
get_data,'rbsp'+probe+'_ect_hope_L3_Tperp_o_30',data=temp_o
get_data,'rbsp'+probe+'_ect_hope_L3_Tperp_p_30',data=temp_p

get_data,'rbsp'+probe+'_ect_hope_L3_Dens_e_200',data=d_e
get_data,'rbsp'+probe+'_ect_hope_L3_Dens_he_30',data=d_he
get_data,'rbsp'+probe+'_ect_hope_L3_Dens_o_30',data=d_o
get_data,'rbsp'+probe+'_ect_hope_L3_Dens_p_30',data=d_p

t_e=temp_e.Y
t_he=temp_he.Y
t_o=temp_o.Y
t_p=temp_p.Y

den_e=d_e.Y
den_he=d_he.Y
den_o=d_o.Y
den_p=d_p.Y

p=fltarr(n_elements(temp_p.Y))
pe=fltarr(n_elements(temp_e.Y))

for i=0,n_elements(temp_p.Y)-1 do begin
  p[i]=t_he[i]*den_he[i]*1e6+t_o[i]*den_o[i]*1e6+t_p[i]*den_p[i]*1e6
endfor

for i=0,n_elements(temp_e.Y)-1 do begin
  pe[i]= t_e[i]*den_e[i]*1e6
end

p=p*1e9*1.6*1e-19
pe=pe*1e9*1.6*1e-19

store_data,'Pressure',data={x:temp_p.X,y:p}
store_data,'Pressure_e',data={x:temp_e.X,y:pe}

tlimit,date1,date2
tplot,['Pressure','Pressure_e']
end
