;+
; $LastChangedBy: c0004hori $
; $LastChangedDate: 2017-09-10 18:57:42 +0900 (Sun, 10 Sep 2017) $
; $LastChangedRevision: 473 $
; $URL: https://ergsc-local.isee.nagoya-u.ac.jp/svn/ergsc/trunk/erg/satellite/erg/common/set_erg_var_label.pro $
;-
pro set_erg_var_label, predict=predict, erase=erase, no_download=no_download, $
   withr=withr
  
 get_timespan, tr 
   
 ;Load the definitive orbit data or predicted orbit
  if keyword_set(erase) then store_data, delete='erg_orb_*'
  if ~keyword_set(predict) then erg_load_orb, no_download=no_download
  
  if tnames('erg_orb_l2_pos_Lm') eq '' then begin ;No definitive orbit yet
    erg_load_orb_predict, no_download=no_download
    orb_prefix = 'erg_orb_pre_l2_pos_'
  endif else begin ;Definitive orbit exists, then check the available period
    get_data, 'erg_orb_l2_pos_Lm', time, data
    orbtr = minmax(time)
    if orbtr[0]-tr[0] le 60 and orbtr[1]-tr[1] ge -60 then begin
      orb_prefix='erg_orb_l2_pos_'
    endif else begin
      erg_load_orb_predict, no_download=no_download
      orb_prefix = 'erg_orb_pre_l2_pos_'
    endelse
  endelse

  split_vec, orb_prefix+'rmlatmlt'
  split_vec, orb_prefix+'Lm'
  if strpos(orb_prefix,'_pre_') ne -1 then pre = '(pre)' else pre = ''
  options, orb_prefix+'Lm_x', ytitle='Lm'+pre
  options, orb_prefix+'rmlatmlt_x', ytitle='R'+pre
  options, orb_prefix+'rmlatmlt_y', ytitle='MLAT'+pre
  options, orb_prefix+'rmlatmlt_z', ytitle='MLT'+pre
  
  if ~keyword_set(withr) then begin
    var_label =  orb_prefix+['rmlatmlt_y','rmlatmlt_z','Lm_x']
  endif else begin
    var_label =  orb_prefix+['rmlatmlt_y','rmlatmlt_z','Lm_x','rmlatmlt_x']
  endelse
  tplot_options, 'var_label', var_label
  
  return
end

  
