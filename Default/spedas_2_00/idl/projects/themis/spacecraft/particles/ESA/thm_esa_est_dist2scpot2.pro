;Multiscale smoothing
Function temp_multiscale_smooth, x, sm
  nsm = n_elements(sm)
  xout = x
  For j = 0, nsm-1 Do xout = smooth(xout, sm[j])
  Return, xout
End

;Interpolate onto a high resolution energy grid
Function temp_interp_hires, y, x, xout, xfactor = xfactor, _extra=_extra
  xout = -1
  If(keyword_set(xfactor)) Then xf = xfactor Else xf = 10.0
  ok = where(finite(x), nok)
  If(nok Eq 0) Then Return, -1
  xr = minmax(x[ok])
  nxout = long(xf*nok)
  xout = xr[0]+((xr[1]-xr[0])/(nxout-1))*indgen(nxout)
  yout = interpol(y[ok], x[ok], xout)
  Return, yout
End
  
;byte scale the input array x, vrange is the desired range to set to
;0 to 255. Log scaled, unless the linear keyword is set. The default
;value for vrange is [1.04e4, 1.0e8]
Function temp_tscale, x0, vrange = vrange, linear = linear, _extra=_extra


  If(keyword_set(vrange)) Then vr = vrange Else vr = [1.0e4, 1.0e8]
  vr = minmax(vr)

  If(~keyword_set(linear) And min(vr) Le 0) Then Begin
     dprint, 'Bad vrange, vr Le 0 is not allowed, vrange = ', vrange
     Return, -1
  Endif
  
  x = x0
  lo = where(~finite(x) or x Le vr[0], nlo)
  If(nlo Gt 0) Then x[lo] = vr[0]
  x = x < vr[1]

  If(~keyword_set(linear)) Then Begin
     x = bytescale(alog(x), range = alog(vr))
  Endif Else Begin
     x = bytescale(x, range = vr)
  Endelse

  Return, X
End
;+
;NAME:
; thm_esa_est_dist2scpot2
;PURPOSE:
; For a given probe and date, estimates the SC potential from PEER
; data, and plots it. This differs from the original
; thm_esa_est_dist2scpot.pro in that it uses the "eyeball test"; it
; only checks byte-scaled eflux variables.
;CALLING SEQUENCE:
; thm_esa_est_dist2scpot2, date, probe, no_init = no_init, $
;                          random_dp = random_dp, plot = plot
;INPUT:
; date = a date, e.g., '2008-01-05'
; probe = a probe, e.g., 'c'
;OUTPUT:
; a tplot variable 'th'+probe+'_est_scpot' is created
; If /random_dp is set, then date and probe are output 
;KEYWORDS:
; trange = a time range
; no_init = if set, do not read in a new set of data
; random_dp = if set, the input date and probe are randomized, note
;             that this keyword is unused if no_init is set.
; plot = if set, plot a comparison of the estimated sc_pot wht the
;        value obtained from the esa L2 cdf (originally from
;        thm_load_esa_pot)
; esa_datatype = 'peef', 'peer', or 'peeb'; the default is 'peer'
; yellow = the limit (0-255) where above this value, we assume that
;          there are photo electrons in the scaled eflux
;          spectrogram. This will give the potential; the default is 170.
;HISTORY:
; 3-mar-2016, jmm, jimm@ssl.berkeley.edu
; $LastChangedBy: jimm $
; $LastChangedDate: 2017-01-23 15:09:39 -0800 (Mon, 23 Jan 2017) $
; $LastChangedRevision: 22649 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/themis/spacecraft/particles/ESA/thm_esa_est_dist2scpot2.pro $
;-

Pro thm_esa_est_dist2scpot2, date, probe, trange=trange, yellow=yellow, $
                             no_init = no_init, random_dp = random_dp, $
                             plot = plot, esa_datatype = esa_datatype, $
                             load_all_esa = load_all_esa, _extra=_extra

;The default is to Use peer data for this
  If(is_string(esa_datatype)) Then Begin
     dtyp = strlowcase(strcompress(/remove_all, esa_datatype[0])) 
  Endif Else dtyp = 'peer'

  dtyp1 = '*'+'_en_eflux'

;Only load one type of data, unless you want it all for plotting or
;random_dp options
  If(~keyword_set(no_init)) Then Begin
     If(keyword_set(random_dp)) Then Begin
        probes = ['a', 'b', 'c', 'd', 'e']
        index = fix(5*randomu(seed))
        probe = probes[index]
;start in 2008
        t0 = time_double('2008-01-01')
        t1 = time_double(time_string(systime(/sec), /date))
        dt = t1-t0
        date = time_string(t0+dt*randomu(seed), /date)
     Endif
     sc = probe
     dprint, 'Probe: ', strupcase(sc)
     If(keyword_set(trange)) Then Begin
        dprint, 'Time_range:', time_string(trange) 
        thm_load_esa, level='l2', probe = sc, trange = trange, datatype = dtyp1
        thm_load_esa_pot, efi_datatype = 'mom', probe = sc, trange = trange
        date = time_string(trange[0], /date_only)
     Endif Else If(keyword_set(date)) Then Begin
        timespan, date
        dprint, 'date: ', date
        thm_load_esa, level='l2', probe = sc, datatype = dtyp1
        thm_load_esa_pot, efi_datatype = 'mom', probe = sc
     Endif Else Begin
;no date is set explicitly, use timerange
        ppp2 = time_string(timerange(),/date_only)
        date = ppp2[0]
        dprint, 'date: ', date
        thm_load_esa, level='l2', probe = sc, datatype = dtyp1
        thm_load_esa_pot, efi_datatype = 'mom', probe = sc
     Endelse
  Endif Else sc = probe

  thx = 'th'+sc
  get_data, thx+'_'+dtyp+'_en_eflux', data = dr

  If(~is_struct(dr)) Then Begin
     message, /info, 'No '+dtyp+' data'
     Return
  Endif

;bytescale in log space
  ok = where(finite(dr.y) And dr.y Gt 0, nok)
  If(nok Gt 0) Then vrange = minmax(dr.y[ok]) Else vrange = 0b
  yy = rotate(temp_tscale(dr.y, vrange = vrange, _extra=_extra), 7)
  vv = rotate(dr.v, 7)
  nchan = n_elements(vv[0,*])
  ntimes = n_elements(dr.x)
  scpot = fltarr(ntimes)+3.0 ;low limit
  If(keyword_set(yellow)) Then ylw = yellow Else yellow = 200
  For j = 0, ntimes-1 Do Begin
     i = 0
     maxv = max(yy[j,0:5], maxpt)
     do_this_j = 0b
     If(vv[j, 0] Lt 1.0) Then Begin ;fix for zero energy modes
        If(yy[j, 1] Ge yellow) Then do_this_j = 1b
     Endif Else Begin
        If(yy[j, 0] Ge yellow) Then do_this_j = 1b
     Endelse
     If(do_this_j) Then Begin
;interpolate to a higher resolution energy grid
        yyy0 = temp_interp_hires(reform(yy[j,*]), alog(reform(vv[j,*])), vvv, _extra=_extra)
        yyy = temp_multiscale_smooth(yyy0, [31, 21, 11])
        If(yyy[0] Ne -1) Then Begin
           Repeat Begin
              i=i+1
              i1 = i+1
              cc = yyy[i] lt yellow Or (yyy[i1] gt 1.1*yyy[i]) $
                   Or i1 Eq n_elements(yyy)-1
           Endrep Until cc
        Endif
        scpot[j] = exp(vvv[i]) < 100.0
     Endif
;     if(j eq 1340) then stop
  Endfor

  dlim = {ysubtitle:'[Volts]', units:'volts'}
  store_data, thx+'_est_scpot', data = {x:dr.x, y:scpot}, dlimits = dlim
  options, thx+'_est_scpot', 'yrange', [0.0, 100.0]

  If(keyword_set(plot) Or keyword_set(random_dp)) Then Begin
     thm_spec_lim4overplot, thx+'_'+dtyp+'_en_eflux', zlog = 1, ylog = 1, /overwrite, ymin = 2.0
     scpot_overlay1 = scpot_overlay(thx+'_esa_pot', thx+'_'+dtyp+'_en_eflux', sc_line_thick = 2.0, /use_yrange)
     scpot_overlay2 = scpot_overlay(thx+'_est_scpot', thx+'_'+dtyp+'_en_eflux', sc_line_thick = 2.0, suffix = '_EST', /use_yrange)
     If(~xregistered('tplot_window')) Then tplot_window, [scpot_overlay1, scpot_overlay2] $
     Else tplot, [scpot_overlay1, scpot_overlay2]
  Endif
End
