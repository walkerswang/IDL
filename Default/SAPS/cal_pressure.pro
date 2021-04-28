probe='a'
timespan,'2013/07/06'

rbsp_load_mageis_l2,probe=probe,/get_mag_ephem

rbsp_load_ect_l3,probe,'hope'

get_data,'rbspa_ect_mageis_L2_FPSA',data=mageis_flux

get_data,'rbspa_ect_hope_L3_FPDO',data=hope_flux

mageis_energy=mageis_flux.v[1,*]

hope_energy=hope_flux.v[1,*]

list='/Users/wzihan/Google\ Drive/SAPS/hope_energy_width.csv'
sed_data = READ_CSV(list, HEADER=SedHeader,N_TABLE_HEADER=0, TABLE_HEADER=SedTableHeader)
hope_width=sed_data.field1

list='/Users/wzihan/Google\ Drive/SAPS/mageis_energy_width.csv'
sed_data = READ_CSV(list, HEADER=SedHeader,N_TABLE_HEADER=0, TABLE_HEADER=SedTableHeader)
mageis_width=sed_data.field1
   
mageis_j=mageis_flux.Y
hope_j=hope_flux.Y
hope_jj=fltarr(n_elements(mageis_j[*,1]),n_elements(hope_j[1,*]))
t=findgen(n_elements(mageis_j[*,1]))


for i=0,n_elements(hope_energy)-1 do begin
hope_jj[*,i]=interpolate(hope_j[*,i],t)
endfor

p=fltarr(n_elements(mageis_j[*,1]))

for i=0,n_elements(mageis_j[*,1])-1 do begin
  for j=0,1 do begin
    
    if j eq 0 then begin
      for k=0,n_elements(hope_energy)-1 do begin
        p[i]=p[i]+4*!const.pi*sqrt(2*1.67*1e-27*hope_energy[k]*1.6*1e-19)*hope_jj[i,k]*hope_width[k]*1e-3*1e4
        endfor
    endif
      
    if j eq 1 then begin
      for k=0,n_elements(mageis_width)-1 do begin
      p[i]=p[i]+4*!const.pi*sqrt(2*1.67*1e-27*mageis_energy[k]*1.6*1e-16)*mageis_j[i,k]*mageis_width[k]*1e4
      endfor
    endif
   endfor
endfor

p=p*1e9;

t=t/n_elements(t)*24
plot,t,p
end