;-------------------------------------------------------------
;+
; NAME:
;       NAXES
; PURPOSE:
;       Find nice axis tics.
; CATEGORY:
; CALLING SEQUENCE:
;       naxes, xmn, xmx ,nx, tx1, tx2, nt, xinc, ndec
; INPUTS:
;       xmn, xmx = Axis min and max.			in.
;       nx = Desired number of axis tics.			in.
; KEYWORD PARAMETERS:
;       Keywords:
;         /NO25  Means do not allow multiples of 2.5
;           (Attempt to better match IDL axes ticks. Use nx=5).
; OUTPUTS:
;       tx1, tx2 = Suggested first and last tic positions.	out.
;       nt = Suggested number of axis tics.			out.
;       xinc = Suggested tic spacing.			out.
;       ndec = Suggested number tic label decimal places.	out.
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner. 7 Nov, 1988 ---  Converted from FORTRAN.
;       R. Sterner, 1997 Feb 18 --- Added /NO25
;       R. Sterner, 2003 Nov 11 --- Fixed ndec to be int.
;       R. Sterner, 2007 Jul 05 --- Forced double to get start and end ticks.
;       R. Sterner, 2010 Jun 09 --- Added /B60 and /B24.
;
; Copyright (C) 1988, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	pro naxes, dx0,dx1,nx, drx0,drx1,nr,drinc,ndec,no25=no25,$
            b60=b60, b24=b24, help=hlp
 
	if (n_params(0) LT 8) or (keyword_set(hlp)) then begin
	  print,' Find nice axis tics.'
	  print,' naxes, xmn, xmx, nx, tx1, tx2, nt, xinc, ndec'
	  print,'   xmn, xmx = Axis min and max.			in.'
	  print,'   nx = Desired number of axis tics.			in.'
	  print,'   tx1, tx2 = Suggested first and last tic positions.	out.'
	  print,'   nt = Suggested number of axis tics.			out.'
	  print,'   xinc = Suggested tic spacing.			out.'
	  print,'   ndec = Suggested number tic label decimal places.	out.'
	  print,' Keywords:'
	  print,'   /NO25  Means do not allow multiples of 2.5'
	  print,'     (Attempt to better match IDL axes ticks. Use nx=5).'
          print,'   /B60 Base 60 friendly tic spacing for angles, or time in'
          print,'     minutes, or seconds.  Good for lon or lat tics.'
          print,'     For a spacing > 10 will give 15, 20, 30, 45, 60, or 90.'
          print,'   /B24 Base 24 friendly tic spacing for hours in the day.'
          print,'     For a spacing > 1 will give 2, 3, 6, or 12.'
	  return
	endif
 
        ;-----------------------------------------------------------------
        ;  Axis range and initial step size estimate
        ;-----------------------------------------------------------------
	dx = double(dx1 - dx0)	              ; Axis range.
	if dx gt 0 then begin	              ; Forward axis.
	  x0 = double(dx0)
	  x1 = double(dx1)
	endif else begin	              ; Reverse axis.
	  x0 = double(dx1)                    ; Keep difference
	  x1 = double(dx0)                    ; positive.
	endelse
	xinc = (x1-x0)/nx	              ; Approx. inc size.

        ;-----------------------------------------------------------------
        ;  Special case: angle step size.
        ;    Also useful for minutes in an hour and seconds in a minute.
        ;-----------------------------------------------------------------
        if keyword_set(b60) then begin
          spmin = 10.                         ; Min value to deal with.
          if xinc gt spmin then begin         ; Find nice n for grid.
            spdel = [90.,60.,45.,30.,20.,15.] ; Nice delta (for > spmin).
            dn = abs(xinc-spdel)              ; Dist from nice val.
            w = where(dn eq min(dn))          ; Find closest.
            xi = spdel[w[0]]                  ; Use nice grid num.
            ndec = 0                          ; No decimal places needed.
            goto, tics
          endif
        endif

        ;-----------------------------------------------------------------
        ;  Special case: hours in a day step size.
        ;-----------------------------------------------------------------
        if keyword_set(b24) then begin
          spmin = 1.                          ; Min value to deal with.
          if xinc gt spmin then begin         ; Find nice n for grid.
            spdel = [12.,6.,3.,2.]            ; Nice delta (for > spmin).
            dn = abs(xinc-spdel)              ; Dist from nice val.
            w = where(dn eq min(dn))          ; Find closest.
            xi = spdel[w[0]]                  ; Use nice grid num.
            ndec = 0                          ; No decimal places needed.
            goto, tics
          endif
        endif

        ;-----------------------------------------------------------------
        ;  Scale initial step size to between 1 and 10.
        ;-----------------------------------------------------------------
	p = alog10(xinc)	              ; Scale to 1 to 10.
	if p lt 0 then p = p-1.
	p = fix(p)
	pow = 10.D0^p
	xi = xinc/pow
	xinc = xi

        ;-----------------------------------------------------------------
        ;  Shift scaled initial step size to a value better for axis tics
        ;-----------------------------------------------------------------
	xi = 10.D0			      ; Filter scaled increment
	ndec = 0			      ;   to find nice increment.
	if xinc lt 7.07 then xi = 5.D0
	if keyword_set(no25) then begin	      ; No multiples of 2.5 allowed.
	  if xinc lt 3.16 then xi = 2.D0
	endif else begin		      ; Multiples of 2.5 ok.
	  if xinc lt 3.5 then xi = 2.5D0
	  if xinc lt 2.24 then xi = 2.D0
	endelse
	if xinc lt 1.4 then xi = 1.D0
	if xi eq 2.5 then ndec = 1
	if xi ge 10. then begin
	  xi = 1.D0
;	  p = p + 1.
	  p = p + 1  ; RES 2003 Nov 11
	  pow = pow*10.
	endif
	ndec = ndec - p
	if ndec lt 0 then ndec = 0
	xi = xi*pow			      ; xi = true increment.

        ;-----------------------------------------------------------------
        ;  Get first and last tics
        ;-----------------------------------------------------------------
tics:
	t = x0/xi			      ; Number of incs to X0.
	if t lt 0. then t = t - 0.05	      ; Adjust to be inside range.
	if t gt 0. then t = t + 0.05
	rx0 = double(long(t)*xi)
	t = x1/xi			      ; Number of incs to X1.
	if t lt 0. then t = t - 0.05
	if t gt 0. then t = t + 0.05
	rx1 = double(long(t)*xi)
	nr = fix((rx1-rx0)/xi + 1.5)	      ; Total number tics.
	rinc = double(xi)
 

        ;-----------------------------------------------------------------
        ;  Deal with axis direction
        ;-----------------------------------------------------------------
	if dx le 0. then begin		      ; Reverse axis.
	  if dx0 lt rx1-.1*rinc then begin    ; Force first tic inside range.
	    rx1 = rx1 - rinc
	    nr = nr - 1
	  endif
	  if dx1 gt rx0+.1*rinc then begin    ; Force last tic inside range.
	    rx0 = rx0 + rinc
	    nr = nr - 1
	  endif
	  drx0 = rx1			      ; Values to return.
	  drx1 = rx0
	  drinc = -rinc
	endif else begin		      ; Foward axis.
	  if dx0 gt rx0+.1*rinc then begin    ; Force first tic inside range.
	    rx0 = rx0 + rinc
	    nr = nr - 1
	  endif
	  if dx1 lt rx1-.1*rinc then begin    ; Force last tic inside range.
	    rx1 = rx1 - rinc
	    nr = nr - 1
	  endif
	  drx0 = rx0			      ; Values to return.
	  drx1 = rx1
	  drinc = rinc
	endelse
 
	return
 
	end
