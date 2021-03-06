probe='a' ;b

year='2013'

date='20130518'

date1_s='2013-05-18/04:50:00'

date2_s='2013-05-18/05:00:00'

date1_l='2013-05-18/04:45:00'

date2_l='2013-05-18/05:10:00'

rbspx = 'rbsp' + probe

timespan,date1_l,1,/hour
ql = 0
bp = '12'

;;;;;;;;;;;;load electric field;;;;;;;;;;;;;;

;Stuff to make plots pretty
rbsp_efw_init
!p.charsize = 1.2
tplot_options,'xmargin',[20.,15.]
tplot_options,'ymargin',[3,6]
tplot_options,'xticklen',0.08
tplot_options,'yticklen',0.02
tplot_options,'xthick',2
tplot_options,'ythick',2


;load eclipse times
rbsp_load_eclipse_predict,probe,date
get_data,'rbsp'+probe+'_umbra',data=eu
get_data,'rbsp'+probe+'_penumbra',data=ep


;; ;Load spice
;;  if ~keyword_set(no_spice_load) then rbsp_load_spice_kernels
;;  rbsp_load_spice_state,probe=probe,coord='gse',/no_spice_load
;;  store_data,'rbsp'+probe+'_state_pos_gse',newname='rbsp'+probe+'_pos_gse'
;;  store_data,'rbsp'+probe+'_state_vel_gse',newname='rbsp'+probe+'_vel_gse'


;Load spinfit MGSE Efield and Bfield

rbsp_efw_spinfit_vxb_subtract_crib,probe,/noplot,ql=ql,boom_pair=bp
evar = 'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit'

;tplot,'rbsp'+probe+'_' + ['vxb_mgse', 'sfit12_mgse']
; if ~keyword_set(noplot) then tplot,'rbsp'+probe+'_' + ['vxb_mgse','rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit']


;Grab the spinfit Ew and Bw data
split_vec,'rbsp'+probe+'_mag_mgse'
; get_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit',data=sfit
get_data,evar,data=edata

if is_struct(edata) then tinterpol_mxn,'rbsp'+probe+'_mag_mgse',edata.x,newname='rbsp'+probe+'_mag_mgse'


;smooth the background magnetic field
;over 30 min for the E*B=0 calculation
rbsp_detrend,'rbsp'+probe+'_mag_mgse',60.*30.


get_data,'rbsp'+probe+'_mag_mgse',data=magmgse
get_data,'rbsp'+probe+'_mag_mgse_smoothed',data=magmgse_smoothed

if ~is_struct(magmgse) then begin
  print,'NO MAG DATA FOR rbsp_efw_EdotB_to_zero_crib.pro TO USE...RETURNING'
  return
endif


bmag = sqrt(magmgse.y[*,0]^2 + magmgse.y[*,1]^2 + magmgse.y[*,2]^2)
bmag_smoothed = sqrt(magmgse_smoothed.y[*,0]^2 + magmgse_smoothed.y[*,1]^2 + magmgse_smoothed.y[*,2]^2)

;Replace axial measurement with E*B=0 version
edata.y[*,0] = -1*(edata.y[*,1]*magmgse_smoothed.y[*,1] + edata.y[*,2]*magmgse_smoothed.y[*,2])/magmgse_smoothed.y[*,0]
;; if ~keyword_set(suffix) then store_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit',data=edata
;; if keyword_set(suffix) then store_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit'+'_'+suffix,data=edata
if ~keyword_set(suffix) then store_data,evar,data=edata
if keyword_set(suffix) then store_data,evar+'_'+suffix,data=edata


;Find bad E*B=0 data (where the angle b/t spinplane MGSE and Bo is less than 15 deg)
;Good data has By/Bx < 3.732   and  Bz/Bx < 3.732

By2Bx = abs(magmgse_smoothed.y[*,1]/magmgse_smoothed.y[*,0])
Bz2Bx = abs(magmgse_smoothed.y[*,2]/magmgse_smoothed.y[*,0])
store_data,'B2Bx_ratio',data={x:edata.x,y:[[By2Bx],[Bz2Bx]]}
ylim,'B2Bx_ratio',0,10
options,'B2Bx_ratio','ytitle','By/Bx (black)!CBz/Bx (red)'
badyx = where(By2Bx gt 3.732)
badzx = where(Bz2Bx gt 3.732)


;calculate angles b/t despun spinplane antennas and Bo.
n = n_elements(edata.x)
ang_ey = fltarr(n)
ang_ez = fltarr(n)

for i=0L,n-1 do ang_ey[i] = acos(total([0,1,0]*magmgse_smoothed.y[i,*])/(bmag_smoothed[i]))/!dtor
for i=0L,n-1 do ang_ez[i] = acos(total([0,0,1]*magmgse_smoothed.y[i,*])/(bmag_smoothed[i]))/!dtor
store_data,'angles',data={x:edata.x,y:[[ang_ey],[ang_ez]]}



;Calculate ratio b/t spinaxis and spinplane components
e_sp = sqrt(edata.y[*,1]^2 + edata.y[*,2]^2)
rat = abs(edata.y[*,0])/e_sp
store_data,'rat',data={x:edata.x,y:rat}
store_data,'e_sp',data={x:edata.x,y:e_sp}
store_data,'e_sa',data={x:edata.x,y:abs(edata.y[*,0])}


;Check for Spinfit saturation
;; get_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit',data=tmpp
get_data,evar,data=tmpp
badsatx = where(abs(tmpp.y[*,0]) ge 195.)
badsaty = where(abs(tmpp.y[*,1]) ge 195.)
badsatz = where(abs(tmpp.y[*,2]) ge 195.)



;Remove bad Efield data
;....saturated data from the rest of the tplot variables
;....saturated data from Ex
;....Ex data when the E*B=0 calculation is unreliable

;; if ~keyword_set(suffix) then get_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit',data=tmpp
;; if keyword_set(suffix) then  get_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit_'+suffix,data=tmpp
if ~keyword_set(suffix) then get_data,evar,data=tmpp
if keyword_set(suffix) then  get_data,evar+'_'+suffix,data=tmpp
if badyx[0] ne -1 then tmpp.y[badyx,0] = !values.f_nan
if badzx[0] ne -1 then tmpp.y[badzx,0] = !values.f_nan
if badsatx[0] ne -1 then tmpp.y[badsatx,0] = !values.f_nan
if badsaty[0] ne -1 then tmpp.y[badsaty,1] = !values.f_nan
if badsatz[0] ne -1 then tmpp.y[badsatz,2] = !values.f_nan
;; if ~keyword_set(suffix) then store_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit',data=tmpp
;; if keyword_set(suffix) then  store_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit_'+suffix,data=tmpp
if ~keyword_set(suffix) then store_data,evar,data=tmpp
if keyword_set(suffix) then  store_data,evar+'_'+suffix,data=tmpp

get_data,'rat',data=tmpp
if badyx[0] ne -1 then tmpp.y[badyx] = !values.f_nan
if badzx[0] ne -1 then tmpp.y[badzx] = !values.f_nan
if badsatx[0] ne -1 then tmpp.y[badsatx] = !values.f_nan
if badsaty[0] ne -1 then tmpp.y[badsaty] = !values.f_nan
if badsatz[0] ne -1 then tmpp.y[badsatz] = !values.f_nan
store_data,'rat',data=tmpp

get_data,'e_sa',data=tmpp
if badyx[0] ne -1 then tmpp.y[badyx] = !values.f_nan
if badzx[0] ne -1 then tmpp.y[badzx] = !values.f_nan
if badsatx[0] ne -1 then tmpp.y[badsatx] = !values.f_nan
if badsaty[0] ne -1 then tmpp.y[badsaty] = !values.f_nan
if badsatz[0] ne -1 then tmpp.y[badsatz] = !values.f_nan
store_data,'e_sa',data=tmpp

get_data,'e_sp',data=tmpp
if badyx[0] ne -1 then tmpp.y[badyx] = !values.f_nan
if badzx[0] ne -1 then tmpp.y[badzx] = !values.f_nan
if badsatx[0] ne -1 then tmpp.y[badsatx] = !values.f_nan
if badsaty[0] ne -1 then tmpp.y[badsaty] = !values.f_nan
if badsatz[0] ne -1 then tmpp.y[badsatz] = !values.f_nan
store_data,'e_sp',data=tmpp




;Remove corotation field
;; dif_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit','rbsp'+probe+'_E_coro_mgse',newname='rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit'
;; if keyword_set(suffix) then dif_data,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit_'+suffix,'rbsp'+probe+'_E_coro_mgse',newname='rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit_'+suffix

if ~keyword_set(nospinfit) then begin
  dif_data,evar,'rbsp'+probe+'_E_coro_mgse',newname='rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit'
  if keyword_set(suffix) then dif_data,evar+'_'+suffix,'rbsp'+probe+'_E_coro_mgse',newname='rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit_'+suffix
endif else begin
  dif_data,evar,'rbsp'+probe+'_E_coro_mgse',newname='rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed'
  if keyword_set(suffix) then dif_data,evar+'_'+suffix,'rbsp'+probe+'_E_coro_mgse',newname='rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_'+suffix
endelse


cotrans,'rbsp'+probe+'_pos_gse','rbsp'+probe+'_pos_gei',/GSE2GEI

tsmooth2, 'rbsp'+probe+'_emfisis_l3_1sec_gse_Mag', 601, newname = 'rbsp'+probe+'_emfisis_l3_1sec_gse_Mag_sm601'

thm_fac_matrix_make, 'rbsp'+probe+'_emfisis_l3_1sec_gse_Mag_sm601',other_dim='Rgeo', pos_var_name='rbsp'+probe+'_pos_gei', newname = 'rbsp'+probe+'_emfisis_l3_1sec_gse_Mag_sm601_fac_matrix'

tvector_rotate,'rbsp'+probe+'_emfisis_l3_1sec_gse_Mag_sm601_fac_matrix','rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit',newname ='rbsp'+probe+'_esvy_fac'

rbsp_load_mageis_l2,probe=probe,/get_mag_ephem,/get_support_data

rbsp_load_ect_l3,probe,'hope',get_support_data=1

get_data,'rbsp'+probe+'_ect_mageis_L2_FPSA',data=mageis_flux

get_data,'rbsp'+probe+'_ect_mageis_L2_FESA',data=mageis_fluxe

get_data,'rbsp'+probe+'_ect_hope_L3_FPDO',data=hope_flux

get_data,'rbsp'+probe+'_ect_mageis_L2_FPDU_Energy_Widths',data=width

get_data,'rbsp'+probe+'_ect_mageis_L2_FEDU_Energy_Widths',data=widthe

mageis_width=width.Y[0,*]
mageis_widthe=widthe.Y[0,*]

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

store_data,'Pressure_hope_i',data={x:temp_p.X,y:p}
store_data,'Pressure_hope_e',data={x:temp_e.X,y:pe}

mageis_energy=mageis_flux.v[1,*]
mageis_energye=mageis_fluxe.v[1,*]

;hope_energy=hope_flux.v[1,*]

;list='/Users/wzihan/Google\ Drive/case/mageis_energy_width.csv'
;f2 = READ_CSV(list, HEADER=SedHeader,N_TABLE_HEADER=0, TABLE_HEADER=SedTableHeader)
;mageis_width=f2.field1

mageis_j=mageis_flux.Y
mageis_ej=mageis_fluxe.Y
;hope_j=hope_flux.Y
;hope_jj=fltarr(n_elements(mageis_j[*,1]),n_elements(hope_j[1,*]))
;t=findgen(n_elements(mageis_j[*,1]))

;for i=0,n_elements(hope_energy)-1 do begin
;  hope_jj[*,i]=interpolate(hope_j[*,i],t)
;endfor

p=fltarr(n_elements(mageis_j[*,1]))
;ph=fltarr(n_elements(hope_j[*,1]))

for i=0,n_elements(mageis_j[*,1])-1 do begin
  for k=0,n_elements(mageis_energy)-11 do begin
    p[i]=p[i]+8.0/3*!const.pi*sqrt(2*1.67*1e-27*mageis_energy[k]*1.6*1e-16)*mageis_j[i,k]*mageis_width[k]*1e4
  endfor
  for k=2,n_elements(mageis_energye)-1 do begin
    p[i]=p[i]+8.0/3*!const.pi*sqrt(2*9.11*1e-31*mageis_energye[k]*1.6*1e-16)*mageis_ej[i,k]*mageis_widthe[k]*1e4
  endfor
endfor

p=p*1e9


store_data,'Pressure_mageis_p',data={x:mageis_flux.X,y:p}



nodownload = 1b
nodownload = 0b


;...stuff to make plots pretty
rbsp_efw_init
!p.charsize = 1.2
tplot_options,'xmargin',[20.,15.]
tplot_options,'ymargin',[3,6]
tplot_options,'xticklen',0.08
tplot_options,'yticklen',0.02
tplot_options,'xthick',2
tplot_options,'ythick',2





;Get B1 and B2 availability
rbsp_load_efw_burst_times,probe=probe
options,['rbsp'+probe+'_efw_vb2_available',$
  'rbsp'+probe+'_efw_vb1_available'],'colors',0


rbsp_load_efw_waveform,probe=probe,type='calibrated',datatype='vsvy'
split_vec, 'rbsp'+probe+'_efw_vsvy', suffix='_V'+['1','2','3','4','5','6']




;-------------------------------------------------------------------------------------
;CREATE DENSITY PROXY (V1 + V2)/2
;-------------------------------------------------------------------------------------



get_data,'rbsp'+probe +'_efw_vsvy_V1',data=d1
get_data,'rbsp'+probe +'_efw_vsvy_V2',data=d2

datt = d1
sum = (d1.y + d2.y)/2.

datt.y = sum
store_data,'density_proxy',data=datt
options,'density_proxy','ytitle','(V1+V2)/2!C[volts]'


rbsp_detrend,['density_proxy'],60.*5.

dmin=10
dmax=10000

cal = rbsp_get_density_calibration(probe)


get_data,'density_proxy',data=pot

if is_struct(pot) then begin

  times = time_double(pot.x)
  v = pot.y
  den = fltarr(n_elements(pot.x))
  timesf = dblarr(n_elements(pot.x))

  for i=0,n_elements(cal.t0)-1 do begin

    ;check for roaming date
    ;(i.e. calibration that's
    ;currently being used)
    if cal.t1[i] eq 'xxxx-xx-xx/xx:xx:xx' then cal.t1[i] = time_string(systime(/seconds))

    tst = where(times ge time_double(cal.t0[i]) and times lt time_double(cal.t1[i]))
    if tst[0] ne -1 then begin
      dentmp = cal.A[i]*exp(v*cal.B[i]) + cal.C[i]*exp(v*cal.D[i])
      den[tst] = dentmp
      timesf[tst] = times[tst]
    endif
  endfor



  ;--------------------------------------------------
  ;If set, remove density values below and above dmin and dmax
  ;--------------------------------------------------

  if keyword_set(dmin) then begin
    goo = where(den lt dmin)
    if goo[0] ne -1 then begin
      if ~keyword_set(setval) then den[goo] = !values.f_nan else den[goo] = setval
    endif
  endif
  if keyword_set(dmax) then begin
    goo = where(den gt dmax)
    if goo[0] ne -1 then begin
      if ~keyword_set(setval) then den[goo] = !values.f_nan else den[goo] = setval
    endif
  endif


  if keyword_set(newname) then store_data,newname,data={x:timesf,y:den} else store_data,'density',data={x:timesf,y:den}


endif else print,'NO VALID TPLOT VARIABLE INPUTTED.....SKIPPING'

;Plot results

RBSP_LOAD_EMFISIS,probe=probe,cadence='1sec',coord='gsm',level='l3'

get_data,'rbsp'+probe+'_emfisis_l3_1sec_gsm_Mag',data=bbb

bx=bbb.y[*,0]-150
by=bbb.y[*,1]
bz=bbb.y[*,2]-600
theta=atan(bz/sqrt(bx^2+by^2))/3.1415926*180

store_data,'b',data={x:bbb.x,y:[[bx],[by],[bz]]}

;store_data,'by',data={x:bbb.x,y:by}
;store_data,'bz',data={x:bbb.x,y:bz}

options,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit','labels',['xMGSE','yMGSE','zMGSE']
options,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit',ytitle='E (mV/m)'
;options,'rbsp'+probe+'_esvy_fac',ytitle='E (mV/m)'
options,'density',ytitle='n (cm!E-3!N)'
options,'Pressure',ytitle='P (nPa)'
options,'rbsp'+probe+'_ect_mageis_L2_FPSA',ytitle='RC'
options,'rbsp'+probe+'_ect_mageis_L2_FESA',ytitle='RC'
options,'rbsp'+probe+'_ect_hope_L3_FPDO',ytitle='PSion'
options,'rbsp'+probe+'_ect_hope_L3_FEDO',ytitle='PSele'
tplot_options,'title','RBSP-'+probe + ' ' + date


;options,'by',ytitle='By GSE'
;options,'bz',ytitle='Bz GSE'
;ylim,'rbsp'+probe+'_emfisis_l3_1sec_gse_Magnitude',500,1000
;;220,290
;ylim,'bx',130,190
;;,180,220
;ylim,'by',-100,400
;;-200,-120
;ylim,'bz',500,900
;;60,150
;ylim,'theta',50,90
; ylim,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit',-10,10
; options,'rbsp'+probe+'_efw_esvy_mgse_vxb_removed_spinfit','labels',['xMGSE','yMGSE','zMGSE']

;rbsp_detrend,['b'],60.*2
;tplot,['bx_detrend','by_detrend','bz_detrend','rbsp'+probe+'_emfisis_l3_1sec_gse_Magnitude_detrend']

tlimit,date1_l,date2_l
popen, '/Users/wzihan/plot/'+probe+'_saps_all'+date
;'rbsp'+probe+'_esvy_fac'
options,'*L3_FPDO','zlog',1
options,'*FEDO','zlog',1
options,'density','ylog',1
options,'b','labels',['xGSM-150','yGSM','zGSM-600']
options,'b',ytitle='B (nT)'

ylim,'*L3_FPDO',0,50000
ylim,'*FEDO',0,10000
zlim,'*FPSA',1000,100000
zlim,'*FESA',1,1000000
zlim,'*FEDO',10000,100000000
zlim,'*FPDO',1000,10000000
ylim,'*FPSA',50,1000
ylim,'*FESA',50,1000
options,'*FPSA','zlog',1
options,'*FESA','zlog',1
options,'*FPSA','ylog',1
;'*FESA': electron differential fluxes averaged over one spin.
;'*FPSA':ion differential fluxes averaged over one spin.
tplot,['rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit','b','density','rbsp'+probe+'_ect_mageis_L2_FPSA','rbsp'+probe+'_ect_mageis_L2_FESA','rbsp'+probe+'_ect_hope_L3_FPDO','rbsp'+probe+'_ect_hope_L3_FEDO','Pressure_hope_i','Pressure_mageis_p'],var_label=['rbsp'+probe+'_ect_mageis_L2_MLT','rbsp'+probe+'_ect_mageis_L2_L']

pclose

;popen, '/Users/wzihan/plot/'+probe+'_saps_plasma'+'20130518'
;tplot,['density','Pressure','rbsp'+probe+'_ect_mageis_L2_FPSA','rbsp'+probe+'_ect_mageis_L2_FESA','rbsp'+probe+'_ect_hope_L3_FPDO','rbsp'+probe+'_ect_hope_L3_FEDO'],var_label=['rbsp'+probe+'_ect_mageis_L2_MLT','rbsp'+probe+'_ect_mageis_L2_L']
;pclose

get_data,'rbsp'+probe+'_ect_mageis_L2_FPSA',data=rcflux
get_data,'rbsp'+probe+'_ect_hope_L3_FPDO',data=psflux
en1=rcflux.v
en2=psflux.v
t1=rcflux.x
t2=psflux.x
f1=rcflux.y
f2=psflux.y
ff1=f1[*,0:16]
ff2=fltarr(7779,11)

t=indgen(7779);
for i=61,71 do begin
  ff2[*,i-61]=interpolate(f2[*,i],t)
endfor

ff=[ff1]
store_data,'flux',data={x:t1,y:ff1}
options,'flux','labels',['58.4 kev','69.3 kev','82.9 kev','99.1 kev','118.1 kev','139.8 kev','164.2 kev','194.1 kev','229.3 kev','267.3 kev','308.0 kev','356.8 kev','413.8 kev','478.9 kev','554.8 kev','636.2 kev','728.4 kev']
options,'flux','ylog',1
options,'b_detrend',ytitle='B_detrend (nT)'
options,'flux',ytitle='Differential Flux (cm!E-2!Ns!E-1!NkeV!E-1!N)'

tlimit,date1_s,date2_s

popen, '/Users/wzihan/plot/'+probe+'_saps_short'+date
tplot,['rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit','b','flux'],var_label=['rbsp'+probe+'_ect_mageis_L2_MLT','rbsp'+probe+'_ect_mageis_L2_L']
pclose

end

