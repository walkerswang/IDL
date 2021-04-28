;+
;PROCEDURE: 
;	mvn_swe_kp
;PURPOSE:
;	Calculates SWEA key parameters.  The result is stored in tplot variables,
;   and as a save file.
;AUTHOR: 
;	David L. Mitchell
;CALLING SEQUENCE: 
;	mvn_swe_kp
;INPUTS:
;   None:      Uses data currently loaded into the SWEA common block.
;
;KEYWORDS:
;   PANS:      Named variable to return tplot variables created.
;
;   MOM:       Calculate density using a moment.  This is the default and
;              only option for now.
;
;   DDD:       Calculate density from 3D distributions (allows bin
;              masking).  Default is to use SPEC data.  This option fits
;              a Maxwell-Boltzmann distribution to the core and performs
;              a moment calculation for the halo.  This provides corrections
;              for both spacecraft potential and scattered photoelectrons.
;              (Currently disabled.)
;
;   ABINS:     Anode bin mask - 16-element byte array (0 = off, 1 = on)
;              Default = replicate(1B, 16).
;
;   DBINS:     Deflector bin mask - 6-element byte array (0 = off, 1 = on)
;              Default = replicate(1B, 6).
;
;   OBINS:     Solid angle bin mask - 96-element byte array (0 = off, 1 = on)
;              Default = reform(ABINS # DBINS, 96).
;
;   MASK_SC:   Mask PA bins that are blocked by the spacecraft.  This is in
;              addition to any masking specified by ABINS, DBINS, and OBINS.
;              Default = 1 (yes).
;
;   L2ONLY:    Only process data using L2 MAG data.
;
;   OUTPUT_PATH: An output_path for testing, the save file will be put into 
;                OUTPUT_PATH/yyyy/mm/.  Directories are created as needed.
;                Default = root_data_dir() + 'maven/data/sci/swe/kp'.
;
;OUTPUTS:
;
; $LastChangedBy: dmitchell $
; $LastChangedDate: 2016-04-09 08:16:18 -0700 (Sat, 09 Apr 2016) $
; $LastChangedRevision: 20770 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/maven/swea/mvn_swe_kp.pro $
;
;-

pro mvn_swe_kp, pans=pans, ddd=ddd, abins=abins, dbins=dbins, obins=obins, $
                mask_sc=mask_sc, mom=mom, l2only=l2only, output_path=output_path

  compile_opt idl2

  @mvn_swe_com

; Process inputs

  if (size(ddd,/type) eq 0) then ddd = 0
  if (size(mom,/type) eq 0) then mom = 1

; Make sure all needed data are available

  if (size(mvn_swe_engy,/type) ne 8) then begin
    print,"No SPEC data loaded!"
    print,"No KP data generated!"
    return
  endif

  if (size(a2,/type) ne 8) then begin
    print,"No PAD data loaded!"
    print,"No KP data generated!"
    return
  endif

  if keyword_set(l2only) then begin
    str_element, swe_mag1, 'level', level, success=ok
    if (ok) then if (max(level) lt 2B) then ok = 0
    if (not ok) then begin
      print,"No MAG L2 data loaded!"
      print,"No KP data generated!"
      return
    endif
  endif

; Set FOV masking

  if (n_elements(abins) ne 16) then abins = replicate(1B, 16)
  if (n_elements(dbins) ne  6) then dbins = replicate(1B, 6)
  if (n_elements(obins) ne 96) then begin
    obins = replicate(1B, 96, 2)
    obins[*,0] = reform(abins # dbins, 96)
    obins[*,1] = obins[*,0]
  endif else obins = byte(obins # [1B,1B])
  if (size(mask_sc,/type) eq 0) then mask_sc = 1
  if keyword_set(mask_sc) then obins = swe_sc_mask * obins

; Set output path and file root

  if keyword_set(output_path) then kp_path = output_path $
                              else kp_path = root_data_dir() + 'maven/data/sci/swe/kp'

  finfo = file_info(kp_path)
  if (not finfo.exists) then begin
    print,"KP root directory does not exist: ",kp_path
    return
  endif

  froot = 'mvn_swe_kp_'

; Load spacecraft ephemeris (used for filtering data)

  maven_orbit_tplot, /loadonly

; Calculate the energy shape parameter

  mvn_swe_shape_par, pans=pans

; Calculate the spacecraft potential

  mvn_swe_sc_pot

; Calculate the density and temperature
    
  mvn_swe_n1d, mom=mom, pans=more_pans

  pans = [pans, more_pans]

; Filter out poor solutions

  for i=0,(n_elements(more_pans)-1) do begin
    get_data,more_pans[i],data=dat
    indx = where(finite(dat.y) and ~finite(dat.dy), count)
    if (count gt 0L) then begin
      dat.y[indx] = !values.f_nan
      store_data,more_pans[i],data=dat
    endif
  endfor
  dat = 0

; Determine the parallel and anti-parallel energy fluxes
;   Exclude bins that straddle 90 degrees pitch angle
;   Apply FOV bin masking

  npts = n_elements(a2)
  t = dblarr(npts)
  eflux_pos_lo = fltarr(npts)
  eflux_pos_md = eflux_pos_lo
  eflux_pos_hi = eflux_pos_lo
  eflux_neg_lo = eflux_pos_lo
  eflux_neg_md = eflux_pos_lo
  eflux_neg_hi = eflux_pos_lo

  cnts_pos_lo = eflux_pos_lo
  cnts_pos_md = eflux_pos_lo
  cnts_pos_hi = eflux_pos_lo
  cnts_neg_lo = eflux_pos_lo
  cnts_neg_md = eflux_pos_lo
  cnts_neg_hi = eflux_pos_lo

  var_pos_lo = eflux_pos_lo
  var_pos_md = eflux_pos_lo
  var_pos_hi = eflux_pos_lo
  var_neg_lo = eflux_pos_lo
  var_neg_md = eflux_pos_lo
  var_neg_hi = eflux_pos_lo
 
  pad = mvn_swe_getpad(a2[0].time)
  energy = pad.energy[*,0]

  endx_lo = where((energy ge   5.) and (energy lt  100.), nlo)
  endx_md = where((energy ge 100.) and (energy lt  500.), nmd)
  endx_hi = where((energy ge 500.) and (energy lt 1000.), nhi)

  midpa = !pi/2.
  NaNs = replicate(!values.f_nan,64)
  
  for i=0L,(npts-1L) do begin
    pad = mvn_swe_getpad(a2[i].time, units='counts')

    if (pad.time gt t_mtx[2]) then boom = 1 else boom = 0
    indx = where(obins[pad.k3d,boom] eq 0B, count)
    if (count gt 0L) then pad.data[*,indx] = !values.f_nan

    cnts = pad.data
    sig2 = pad.var   ; variance with digitization noise
      
    mvn_swe_convert_units, pad, 'eflux'

    t[i] = pad.time
    
    ipos = where(pad.pa_max[63,*] lt midpa, npos)
    if (npos gt 0L) then begin
      eflux_pos = average(pad.data[*,ipos],2,/nan)
      cnts_pos = total(reform(cnts[*,ipos],64,npos),2,/nan)
      var_pos = total(reform(sig2[*,ipos],64,npos),2,/nan)
    endif else begin
      eflux_pos = NaNs
      cnts_pos = NaNs
      var_pos = NaNs
    endelse

    ineg = where(pad.pa_min[63,*] gt midpa, nneg)
    if (nneg gt 0L) then begin
      eflux_neg = average(pad.data[*,ineg],2,/nan)
      cnts_neg = total(reform(cnts[*,ineg],64,nneg),2,/nan)
      var_neg = total(reform(sig2[*,ineg],64,nneg),2,/nan)
    endif else begin
      eflux_neg = NaNs
      cnts_neg = NaNs
      var_neg = NaNs
    endelse

    eflux_pos_lo[i] = average(eflux_pos[endx_lo],/nan)
    eflux_pos_md[i] = average(eflux_pos[endx_md],/nan)
    eflux_pos_hi[i] = average(eflux_pos[endx_hi],/nan)
    cnts_pos_lo[i] = total(cnts_pos[endx_lo],/nan)
    cnts_pos_md[i] = total(cnts_pos[endx_md],/nan)
    cnts_pos_hi[i] = total(cnts_pos[endx_hi],/nan)
    var_pos_lo[i] = total(var_pos[endx_lo],/nan)
    var_pos_md[i] = total(var_pos[endx_md],/nan)
    var_pos_hi[i] = total(var_pos[endx_hi],/nan)

    eflux_neg_lo[i] = average(eflux_neg[endx_lo],/nan)
    eflux_neg_md[i] = average(eflux_neg[endx_md],/nan)
    eflux_neg_hi[i] = average(eflux_neg[endx_hi],/nan)
    cnts_neg_lo[i] = total(cnts_neg[endx_lo],/nan)
    cnts_neg_md[i] = total(cnts_neg[endx_md],/nan)
    cnts_neg_hi[i] = total(cnts_neg[endx_hi],/nan)
    var_neg_lo[i] = total(var_neg[endx_lo],/nan)
    var_neg_md[i] = total(var_neg[endx_md],/nan)
    var_neg_hi[i] = total(var_neg[endx_hi],/nan)
  endfor

  sdev_pos_lo = eflux_pos_lo * (sqrt(var_pos_lo)/cnts_pos_lo)
  sdev_pos_md = eflux_pos_md * (sqrt(var_pos_md)/cnts_pos_md)
  sdev_pos_hi = eflux_pos_hi * (sqrt(var_pos_hi)/cnts_pos_hi)
  sdev_neg_lo = eflux_neg_lo * (sqrt(var_neg_lo)/cnts_neg_lo)
  sdev_neg_md = eflux_neg_md * (sqrt(var_neg_md)/cnts_neg_md)
  sdev_neg_hi = eflux_neg_hi * (sqrt(var_neg_hi)/cnts_neg_hi)

; Filter out poor solutions

  indx = where(finite(eflux_pos_lo) and ~finite(sdev_pos_lo), count)
  if (count gt 0L) then eflux_pos_lo[indx] = !values.f_nan
  indx = where(finite(eflux_pos_md) and ~finite(sdev_pos_md), count)
  if (count gt 0L) then eflux_pos_md[indx] = !values.f_nan
  indx = where(finite(eflux_pos_hi) and ~finite(sdev_pos_hi), count)
  if (count gt 0L) then eflux_pos_hi[indx] = !values.f_nan

  indx = where(finite(eflux_neg_lo) and ~finite(sdev_neg_lo), count)
  if (count gt 0L) then eflux_neg_lo[indx] = !values.f_nan
  indx = where(finite(eflux_neg_md) and ~finite(sdev_neg_md), count)
  if (count gt 0L) then eflux_neg_md[indx] = !values.f_nan
  indx = where(finite(eflux_neg_hi) and ~finite(sdev_neg_hi), count)
  if (count gt 0L) then eflux_neg_hi[indx] = !values.f_nan

; Create TPLOT variables for save/restore file

  store_data,'mvn_swe_efpos_5_100',data={x:t, y:eflux_pos_lo, dy:sdev_pos_lo}
  store_data,'mvn_swe_efpos_100_500',data={x:t, y:eflux_pos_md, dy:sdev_pos_md}
  store_data,'mvn_swe_efpos_500_1000',data={x:t, y:eflux_pos_hi, dy:sdev_pos_hi}

  store_data,'mvn_swe_efneg_5_100',data={x:t, y:eflux_neg_lo, dy:sdev_neg_lo}
  store_data,'mvn_swe_efneg_100_500',data={x:t, y:eflux_neg_md, dy:sdev_neg_md}
  store_data,'mvn_swe_efneg_500_1000',data={x:t, y:eflux_neg_hi, dy:sdev_neg_hi}
  
  pans = [pans, 'mvn_swe_efpos_5_100', 'mvn_swe_efpos_100_500', 'mvn_swe_efpos_500_1000', $
                'mvn_swe_efneg_5_100', 'mvn_swe_efneg_100_500', 'mvn_swe_efneg_500_1000'   ]

; Create TPLOT variables for display only

  eflux_lo = fltarr(npts,2)
  eflux_lo[*,0] = eflux_pos_lo
  eflux_lo[*,1] = eflux_neg_lo
  vname = 'mvn_swe_ef_5_100'
  store_data,vname,data={x:t, y:eflux_lo, v:[0,1]}
  ylim,vname,0,0,1
  options,vname,'labels',['pos','neg']
  options,vname,'labflag',1
  
  eflux_md = fltarr(npts,2)
  eflux_md[*,0] = eflux_pos_md
  eflux_md[*,1] = eflux_neg_md
  vname = 'mvn_swe_ef_100_500'
  store_data,vname,data={x:t, y:eflux_md, v:[0,1]}
  ylim,vname,0,0,1  
  options,vname,'labels',['pos','neg']
  options,vname,'labflag',1
  
  eflux_hi = fltarr(npts,2)
  eflux_hi[*,0] = eflux_pos_hi
  eflux_hi[*,1] = eflux_neg_hi
  vname = 'mvn_swe_ef_500_1000'
  store_data,vname,data={x:t, y:eflux_hi, v:[0,1]}
  ylim,vname,0,0,1  
  options,vname,'labels',['pos','neg']
  options,vname,'labflag',1

; Store the results in tplot save/restore file(s)

  date = time_string(t[npts/2L],/date_only)
  yyyy = strmid(date,0,4)
  mm = strmid(date,5,2)
  dd = strmid(date,8,2)

  path = kp_path + '/' + yyyy
  finfo = file_info(path)
  if (~finfo.exists) then file_mkdir2, path, mode = '0775'o

  path = path + '/' + mm
  finfo = file_info(path)
  if (~finfo.exists) then file_mkdir2, path, mode = '0775'o

  tname = path + '/' + froot + yyyy + mm + dd
  fname = tname + '.tplot'
  finfo = file_info(fname)

; If the file already exists, then try to overwrite it;
; otherwise, create the file and make it group writable.

  if (finfo.exists) then begin
    if (~file_test(fname,/write)) then begin
      print,"Error: no write permission for: ",fname
      return
    endif
    tplot_save, pans, file=tname
  endif else begin
    tplot_save, pans, file=tname
    file_chmod, fname, '0664'o
  endelse

  return

end
