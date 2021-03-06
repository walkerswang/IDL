;-------------------------------------------------------------
;+
; NAME:
;       ENVELOPE
; PURPOSE:
;       Return the min/max envelope of a time series.
; CATEGORY:
; CALLING SEQUENCE:
;       envelope, y, w, mn, mx, ind
; INPUTS:
;       y = time series signal.                 in
;       w = window width in samples.            in
; KEYWORD PARAMETERS:
;       Keywords:
;         INMN=inmn  Returned array of minima indices.
;         INMX=inmx  Returned array of maxima indices.
; OUTPUTS:
;       mn = array of mins in each window.      out
;       mx = array of maxes in each window.     out
;       ind = array of window midpoint indices. out
; COMMON BLOCKS:
; NOTES:
;       Notes: data type of mn and mx are the same as
;         input array y.
;         To overplot envelope do:
;           oplot,ind,mn & oplot,ind,mx
;         To polyfill the envelope do:
;           polyfill, [ind,reverse(ind)],[mn,reverse(mx)]
;           May not fill ends.
;        Some minima or maxima may fall outside the envelope
;        depending on the window width.
; MODIFICATION HISTORY:
;       R. Sterner, 19 Sep, 1991
;       R. Sterner, 2010 May 04 --- Converted arrays from () to [].
;       R. Sterner, 2012 Mar 20 --- Added INMN=inmn, INMX=inmx.
;
; Copyright (C) 1991, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	pro envelope, y, w, mn, mx, ind, inmn=inmn, inmx=inmx, help=hlp
 
	if (n_params(0) lt 4) or keyword_set(hlp) then begin
	  print,' Return the min/max envelope of a time series.'
	  print,' envelope, y, w, mn, mx, ind'
	  print,'   y = time series signal.                 in'
	  print,'   w = window width in samples.            in'
	  print,'   mn = array of mins in each window.      out'
	  print,'   mx = array of maxes in each window.     out'
	  print,'   ind = array of window midpoint indices. out'
          print,' Keywords:'
	  print,'   INMN=inmn  Returned array of minima indices.'
	  print,'   INMX=inmx  Returned array of maxima indices.'
	  print,' Notes: data type of mn and mx are the same as'
	  print,'   input array y.'
	  print,'   To overplot envelope do:'
	  print,'     oplot,ind,mn & oplot,ind,mx'
	  print,'   To polyfill the envelope do:'
	  print,'     polyfill, [ind,reverse(ind)],[mn,reverse(mx)]'
          print,'     May not fill ends.'
          print,'  Some minima or maxima may fall outside the envelope'
          print,'  depending on the window width.'
	  return
	endif
 
	if n_elements(w) eq 0 then begin
	  print,' Error in envelope: window width is undefined.'
	  return
	endif
	if n_elements(w) gt 1 then begin
	  print,' Error in envelope: window width must be a scalar.'
	  return
	endif
 
	ny = n_elements(y)			; Number of points in signal.
	nw = fix(ny/w)				; Number of windows needed.
 
	s = size(y)				; Get data type of signal.
	mn = make_array(nw, type=s[s[0]+1])	; Make min/max arrays same type.
	mx = mn
	ind = lonarr(nw)			; Also need array of indices.
	inmn = lonarr(nw)			; Minima indices.
	inmx = lonarr(nw)			; Maxima indices.
 
	for i = 0L, nw-1 do begin		; Loop through each window.
	  lo = i*w				; Window start index.
	  hi = lo + w - 1			; Window end index.
	  ind[i] = (lo+hi)/2			; Window midpoint.
	  wmn = min(y[lo:hi], imn, max=wmx, subscript_max=imx) ; Min, Max, ...
	  mn[i] = wmn				; Save values.
	  mx[i] = wmx
          inmn[i] = imn + lo
          inmx[i] = imx + lo
	endfor
 
	return
	end
