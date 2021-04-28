; Copyright 2006 The Johns Hopkins University/Applied Physics Laboratory.
; All rights reserved.
;
;--- Brian Wolven, JHU/APL
;--- brian.wolven@jhuapl.edu
;
;-----
;----- scale2.pro
;-----
;----- Scales values between:
;-----    vmin and vmax (determined from data, if not specified in keywords)
;----- to the range:
;-----     cmin -> cmax (defaults to 0->255 if not specified)
;-----
;----- LOG keyword:
;-----  May be set to get a logarithmic rather than linear scale.
;-----
;----- FLOAT keyword:
;-----  May be set to force floating point arithmetic when all inputs are byte
;-----  or integer values.
;-----

FUNCTION scale2,qty,CMIN=cmin,CMAX=cmax,VMIN=vmin,VMAX=vmax,FLOAT=float,$
  LOG=log
  ;---

  IF (N_ELEMENTS(cmin) EQ 0) THEN cmin = 0
  IF (N_ELEMENTS(cmax) EQ 0) THEN cmax = 255

  ;--- Use local copies, don't alter original!

  IF (N_ELEMENTS(vmin) EQ 0) THEN smin = MIN(qty) ELSE smin = vmin
  IF (N_ELEMENTS(vmax) EQ 0) THEN smax = MAX(qty) ELSE smax = vmax

  ;--- Bounded normalization

  sqty = (((FLOAT(qty)>smin)<smax)-smin)/FLOAT(smax-smin)

  ;--- Convert to log stretch?
  ;--- 0->1 becomes 1->10, log gives back 0 to 1

  IF (KEYWORD_SET(log)) THEN sqty = ALOG10(sqty*9. + 1.)

  range = cmin+FLOAT(cmax-cmin)*sqty

  IF (NOT KEYWORD_SET(float)) THEN range = ROUND(range)

  d_err = CHECK_MATH(NOCLEAR=0)

  RETURN,range
END