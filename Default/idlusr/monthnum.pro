;-------------------------------------------------------------
;+
; NAME:
;       MONTHNUM
; PURPOSE:
;       Return month number given name.
; CATEGORY:
; CALLING SEQUENCE:
;       num = monthnum(name)
; INPUTS:
;       name = month name (at least 3 characters).  in
;         May be an array.  Case ignored.
; KEYWORD PARAMETERS:
;       Keywords:
;         /CHAR return results as 2-digit character strings.
; OUTPUTS:
;       num = month number (Jan=1, Feb=2, ...).     out
;         -1 means invalid input month name.
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner, 1999 Aug 2
;       R. Sterner, 2001 Jun 21 --- Allowed arrays.
;       R. Sterner, 2010 Jun 07 --- Converted arrays from () to [].
;       R. Sterner, 2012 Mar 09 --- Added /CHAR.
;
; Copyright (C) 1999, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
	function monthnum, name, char=char, help=hlp
 
	if (n_params(0) lt 1) or keyword_set(hlp) then begin
	  print,' Return month number given name.'
	  print,' num = monthnum(name)'
	  print,'   name = month name (at least 3 characters).  in'
          print,'     May be an array.  Case ignored.'
	  print,'   num = month number (Jan=1, Feb=2, ...).     out'
	  print,'     -1 means invalid input month name.'
          print,' Keywords:'
          print,'   /CHAR return results as 2-digit character strings.'
	  return,''
	endif
 
	a = strlowcase(strmid(monthnames(),0,3))	; List of 3 char names.
	n = strlowcase(strmid(name,0,3))		; Edited input name.
	num = n_elements(name)				; How many names?
	list = intarr(num)
	for i=0,num-1 do list[i]=(where(n[i] eq a))[0]	; Find number.
	if num eq 1 then list=list[0]
        if keyword_set(char) then list=string(list,form='(I2.2)')
	return, list
 
	end
