;+
; MMS EDP crib sheet
;
; do you have suggestions for this crib sheet?
;   please send them to egrimes@igpp.ucla.edu
;
; $LastChangedBy: egrimes $
; $LastChangedDate: 2016-07-18 08:37:51 -0700 (Mon, 18 Jul 2016) $
; $LastChangedRevision: 21476 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/trunk/projects/mms/examples/basic/mms_load_edp_crib.pro $
;-

list='/Users/wzihan/Google\ Drive/SAPS/case/storm_list.csv'
sed_data = READ_CSV(list, HEADER=SedHeader,N_TABLE_HEADER=1, TABLE_HEADER=SedTableHeader)

n=sed_data.FIELD01
number=size(n)
nn=number[1]-1
for ii=119,119 do begin

  year=strtrim(sed_data.FIELD01[ii],1)
  month=strtrim(sed_data.FIELD02[ii],1)
  if month lt 10 then month='0'+month
  day=strtrim(sed_data.FIELD03[ii],1)
  if day lt 10 then day='0'+day
  
  
  for jj=60,71 do begin
    hour=strtrim(sed_data.FIELD04[ii]+jj,1)
    hour2=strtrim(sed_data.FIELD04[ii]+jj+1,1)
    if hour lt 10 then hour='0'+hour
    if hour2 lt 10 then hour2='0'+hour2
    date=year+'-'+month+'-'+day+'/'+hour+':00:00'

    date2=year+'-'+month+'-'+day+'/'+hour2+':00:00'

    timespan, date, 1, /hour
    mms_load_edp, data_rate='slow', probes=[1], datatype='dce', level='l2'

    ; Display colors for parallel E (black) and error (pink)
    ; Large error bars signifies possible presence of cold plasma
    ; or spacecraft charging, which can make axial electric field
    ; measurements difficult. Please always use error bars on e-parallel!!
;    options, 'mms?_edp_dce_par_epar_srvy_l2', colors = [1, 0]
;    options, 'mms?_edp_dce_par_epar_srvy_l2', labels = ['Error', 'E!D||!N']

    ; Since the electric field is often close to zero in multiple components, label spacing tends to get bunched
    ; together
;    options, '*', 'labflag', -1
    
    mms_load_state,probes=[1],datatypes='pos'
    
    get_data,'mms1_mec_mlt',data=mlt
    get_data,'mms1_mec_l_dipole',data=lshell
    
    h=(sed_data.FIELD04[ii]+jj) mod 24
    mltt=mlt.y[h*120:h*120+120-1]
    ll=lshell.y[h*120:h*120+120-1]
    
    mltmin=min(mltt)
    mltmax=max(mltt)
    llmin=min(ll)
    llmax=max(ll)
    
    if ((llmin le 7) and (llmax ge 3)) and (((mltmin le 6) or (mltmin ge 15)) or ((mltmax le 6) or (mltmax ge 15))) then begin
; load ExTOF data:
      mms_load_eis, probes=[1], datatype='extof', level = 'l2' 

      mms_load_eis, probes=[1], datatype='phxtof', level = 'l2'
      
      mms_load_fpi, probes = [1], datatype = ['des-moms', 'dis-moms'] , level = 'l2', data_rate = 'fast', min_version='2.2.0'

      ; plot the PHxTOF proton spectra
      tplot, ['*_edp_dce_gse_slow_l2','*_des_numberdensity_fast','*_extof_proton_flux_omni_spin','*_phxtof_proton_flux_omni_spin','*_dis_energyspectr_omni_fast','*_mec_mlat','*_mec_mlt','*_mec_l_dipole']
      ;, 'mms?_edp_dce_par_epar_fast_l2']
      popen, '/Users/wzihan/plot/mms_saps_'+year+month+day+hour
      tplot
      pclose
    endif
    
  endfor

endfor
end
