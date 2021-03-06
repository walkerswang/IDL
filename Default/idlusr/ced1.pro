;-------------------------------------------------------------
;+
; NAME:
;       CED1
; PURPOSE:
;       Simple widget to edit a single color table entry.
; CATEGORY:
; CALLING SEQUENCE:
;       ced1, index
; INPUTS:
;       index = color table index to edit.   in
; KEYWORD PARAMETERS:
;       Keywords:
;         TITLE=txt  Title text to display.
;         /HSV  means work in Hue, Saturation, and Value
;           coordinates (def=Red, Green, Blue).
; OUTPUTS:
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner, 29 Oct, 1993
;       R. Sterner, 29 Oct, 1993
;       R. Sterner, 2010 Apr 30 --- Converted arrays from () to [].
;
; Copyright (C) 1993, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	pro ced1_event, ev
 
	widget_control, ev.id, get_uval=uval
	widget_control, ev.top, get_uval=d
 
	if uval eq 'DONE' then begin
	  widget_control, ev.top, /dest
	  return
	end
 
	if uval eq 'CANCEL' then begin
	  tvlct, d.r, d.g, d.b, d.index		; Restore original color.
	  widget_control, ev.top, /dest
	  return
	end
 
	if uval eq 'SLIDER' then begin		; Handle sliders.
	  widget_control, d.wid[0], get_val=s1	; Get current slider values.
	  widget_control, d.wid[1], get_val=s2
	  widget_control, d.wid[2], get_val=s3
	  if d.mode eq 0 then begin		; RGB mode.
	    tvlct, s1, s2, s3, d.index
	  endif else begin			; HSV mode.
	    tvlct, s1, s2/100., s3/100., d.index, /hsv
	  endelse
	  return
	endif
 
	end
 
;========================================================
;	ced1.pro = single entry color editor.
;	R. Sterner, 29 Oct, 1993
;========================================================
 
	pro ced1, index, title=title, hsv=hsv, help=hlp
 
	if (n_params(0) lt 1) or keyword_set(hlp) then begin
	  print,' Simple widget to edit a single color table entry.'
	  print,' ced1, index'
	  print,'   index = color table index to edit.   in'
	  print,' Keywords:'
	  print,'   TITLE=txt  Title text to display.'
	  print,'   /HSV  means work in Hue, Saturation, and Value'
	  print,'     coordinates (def=Red, Green, Blue).'
	  return
	endif
 
	;-------  Check that color index is in range  ----------
	if (index lt 0) or (index ge !d.n_colors) then begin
	  xmess,['Requested color table index, '+strtrim(index,2)+$
	    ', is out of the range','0 to '+strtrim(!d.n_colors-1,2)]
	  return
	endif
 
	;-------  Get intial color  -----------
	tvlct,rr,gg,bb,/get,index
	rr=rr[0]  &  gg=gg[0]  &  bb=bb[0]
	init = [rr,gg,bb]
 
	;-------  Get set up for chosen color system  ----------
	if keyword_set(hsv) then begin
	  lb = ['Hue (0 to 360)','Saturation (0 to 100%)','Value (0 to 100%)']
	  mx = [360,100,100]
	  mode = 1
	  rgb_to_hsv, rr, gg, bb, h, s, v
	  val = [h,s*100,v*100]
	endif else begin
	  lb = ['Red (0 to 255)','Green (0 to 255)','Blue (0 to 255)']
	  mx = [255,255,255]
	  mode = 0
	  val = init
	endelse
 
 
	;--------  Widget layout  ---------------
	top = widget_base(/column, title=' ')
	if n_elements(title) ne 0 then b = widget_label(top, val=title)
 
	wid = lonarr(3)
 
	for i = 0, 2 do begin
	  id_s = widget_slider(top, xsize=400, max=mx[i], val=val[i], $
	    uval='SLIDER', /drag)
	  wid[i] = id_s
	  id = widget_label(top, val=lb[i])
	endfor
 
	b = widget_base(top, /row)
	id = widget_button(b, value='Done', uval='DONE')
	id = widget_button(b, value='Cancel', uval='CANCEL')
 
	;--------  Set up and store needed global data  ---------
	data = {index:index, r:rr, g:gg, b:bb, wid:wid, mode:mode}
	widget_control, top, set_uval=data
 
	widget_control, /real, top
	xmanager, 'ced1', top
 
	return
	end
