  jtime=julday(1,1,2015)
for s=1,1 do begin
if s eq 0 then probe='a'
if s eq 1 then probe='b'
rbspx = 'rbsp' + probe
for dd=1001,1000 do begin  ;820
  print,dd
  caldat,jtime+dd,month,day,year
  year=strtrim(year,1)
  if month lt 10 then month='0'+strtrim(month,1) else month=strtrim(month,1)
  if day lt 10 then day='0'+strtrim(day,1) else day=strtrim(day,1)
  date=year+'-'+month+'-'+day
  datename=year+month+day
  timespan,date,1,/day

  RBSP_LOAD_EMFISIS,probe=probe,cadence='1sec',coord='gsm',level='l3',/GET_SUPPORT_DATA

  ;get_data,rbspx+'_emfisis_l3_1sec_gsm_coordinates',data=position

  ;pos=position.Y
  ;r=sqrt(pos[*,0]^2+pos[*,1]^2)

  ;load EFW;;;;;;;;;;;;;;;;;;; ;load EFW;;;;;;;;;;;;;;;;;;; ;load EFW;;;;;;;;;;;;;;;;;;; ;load EFW;;;;;;;;;;;;;;;;;;; ;load EFW;;;;;;;;;;;;;;;;;;; ;load EFW;;;;;;;;;;;;;;;;;;; ;load EFW;;;;;;;;;;;;;;;;;;; ;load EFW;;;;;;;;;;;;;;;;;;;
  ql = 0
  bp = '12'


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

  ;load EFW complete;;;;;;;;;;;;;;;;;;; ;load EFW complete;;;;;;;;;;;;;;;;;;; ;load EFW complete;;;;;;;;;;;;;;;;;;; ;load EFW complete;;;;;;;;;;;;;;;;;;; ;load EFW complete;;;;;;;;;;;;;;;;;;; ;load EFW complete;;;;;;;;;;;;;;;;;;; ;load EFW complete;;;;;;;;;;;;;;;;;;;

  ;load density begin;  ;load density begin;  ;load density begin;  ;load density begin;  ;load density begin;  ;load density begin;  ;load density begin;  ;load density begin;  ;load density begin;  ;load density begin;  ;load density begin;  ;load density begin;v
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

  ;load density end;;load density end;;load density end;;load density end;;load density end;;load density end;;load density end;;load density end;;load density end;;load density end;;load density end;;load density end;;load density end;;load density end;
  ;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hope;load hopev

  rbsp_load_ect_l3,probe,'hope',get_support_data='get'

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

;load hope end;;load hope end;;load hope end;;load hope end;;load hope end;;load hope end;;load hope end;;load hope end;;load hope end;;load hope end;;load hope end;

;load rbspice proton;;load rbspice;;load rbspice;;load rbspice;;load rbspice;;load rbspice;;load rbspice;;load rbspice;;load rbspice;;load rbspice;;load rbspice;;load rbspice;;load rbspice;;load rbspice;
 rbsp_load_rbspice, probe=probe, trange=trange, datatype='TOFxEH', level = 'l3',get_support_data=get_support_data
;load rbspice end;;load rbspice end;;load rbspice end;;load rbspice end;;load rbspice end;;load rbspice end;;load rbspice end;;load rbspice end;;load rbspice end;;load rbspice end;;load rbspice end;;load rbspice end;
;load mageis electron; ;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;
rbsp_load_mageis_l2,probe=probe,/get_mag_ephem,/get_support_data
;load mageis electron end ;;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;;load mageis electron;

tplot,['rbsp'+probe+'_efw_esvy_mgse_vxb_removed_coro_removed_spinfit','rbsp'+probe+'_emfisis_l3_1sec_gsm_Mag','density','flux','rbsp'+probe+'_rbspice_l3_TOFxEH_proton_omni_spin','rbsp'+probe+'_ect_mageis_L2_FESA','rbsp'+probe+'_ect_hope_L3_FPDO','rbsp'+probe+'_ect_hope_L3_FEDO'],var_label=['rbsp'+probe+'_ect_mageis_L2_MLT','rbsp'+probe+'_ect_mageis_L2_L']
zlim,'*FEDO',1000,10000000
zlim,'*FPDO',1000,10000000
options,'*FPDO','zlog',1
options,'*FEDO','zlog',1
options,'density','ylog',1

get_data, 'rbsp'+probe+'_ect_mageis_L2_MLT',data=mlt
get_data, 'rbsp'+probe+'_ect_mageis_L2_L',data=l

mlt=mlt.y
l=l.y
n=size(l)
n=n[1]
step=fix(n/24)
      for j=0,23 do begin
        lmin=min(l[long(j)*step:long(j)*step+step-1])
        lmax=max(l[long(j)*step:long(j)*step+step-1])
        mltavg=mean(mlt[long(j)*step:long(j)*step+step-1])
        if j lt 10 then hour='0'+strtrim(j,1) else hour=strtrim(j,1)
        if j+1 lt 10 then hour2='0'+strtrim(j+1,1) else hour2=strtrim(j+1,1)
        if  (mltavg gt 15 ) && (lmin lt 4) && (lmax gt 3) then begin
          time=date+'/'+hour+':00:00'
          time2=date+'/'+hour2+':00:00'
          timename=datename+hour
          popen, '/Users/wzihan/plot/'+probe+'_saps_'+timename
          tlimit,time,time2
          pclose
        endif
      endfor
endfor
endfor
end

