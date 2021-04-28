;+
;FUNCTION:   mvn_swe_padsum
;PURPOSE:
;  Sums multiple PAD data structures.  This is done by summing raw counts
;  corrected by deadtime and then setting dtc to unity.  Also, note that 
;  summed PAD's can be "blurred" by a changing magnetic field direction, 
;  so summing only makes sense for short intervals.  The theta, phi, and 
;  omega tags can be hopelessly confused if the MAG direction changes much.
;
;USAGE:
;  padsum = mvn_swe_padsum(pad)
;
;INPUTS:
;       pad:           An array of PAD structures to sum.
;
;KEYWORDS:
;
; $LastChangedBy: dmitchell $
; $LastChangedDate: 2017-01-16 11:52:29 -0800 (Mon, 16 Jan 2017) $
; $LastChangedRevision: 22604 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/maven/swea/mvn_swe_padsum.pro $
;
;CREATED BY:    David L. Mitchell  03-29-14
;FILE: mvn_swe_padsum.pro
;-
function mvn_swe_padsum, pad

  if (size(pad,/type) ne 8) then return, 0
  if (n_elements(pad) eq 1) then return, pad

  old_units = pad[0].units_name  
  mvn_swe_convert_units, pad, 'counts'            ; convert to raw counts
  padsum = pad[0]
  npts = n_elements(pad)

  padsum.met = mean(pad.met)
  padsum.time = mean(pad.time)
  padsum.end_time = max(pad.end_time)
  start_time = min(pad.time - (pad.delta_t/2D))
  padsum.delta_t = (padsum.end_time - start_time) > pad[0].delta_t
  padsum.dt_arr = total(pad.dt_arr,3)             ; normalization for the sum
    
  padsum.pa = total(pad.pa,3)/float(npts)         ; pitch angles can be blurred
  padsum.dpa = total(pad.dpa,3)/float(npts)
  padsum.pa_min = total(pad.pa_min,3)/float(npts)
  padsum.pa_max = total(pad.pa_max,3)/float(npts)

  padsum.sc_pot = mean(pad.sc_pot, /nan)
  padsum.Baz = mean(pad.Baz, /nan)
  padsum.Bel = mean(pad.Bel, /nan)

  padsum.magf[0] = mean(pad.magf[0], /nan)
  padsum.magf[1] = mean(pad.magf[1], /nan)
  padsum.magf[2] = mean(pad.magf[2], /nan)
  padsum.v_flow[0] = mean(pad.v_flow[0], /nan)
  padsum.v_flow[1] = mean(pad.v_flow[1], /nan)
  padsum.v_flow[2] = mean(pad.v_flow[2], /nan)

  padsum.data = total(pad.data/pad.dtc,3)  ; corrected counts
  padsum.var = total(pad.var/pad.dtc,3)    ; variance of sum
  padsum.dtc = 1.         ; summing corrected counts is not reversible
  padsum.bkg = total(pad.bkg,3)/float(npts)

  mvn_swe_convert_units, pad, old_units
  mvn_swe_convert_units, padsum, old_units

  return, padsum

end
