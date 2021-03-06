;-------------------------------------------------------------
;+
; NAME:
;       EQV2
; PURPOSE:
;       Interactively plot a function on current plot.
; CATEGORY:
; CALLING SEQUENCE:
;       eqv2, file
; INPUTS:
;       file = equation file.   in
; KEYWORD PARAMETERS:
;       Keywords:
;         COLOR=clr  Curve burn in color (def=!p.color).
; OUTPUTS:
; COMMON BLOCKS:
; NOTES:
;       Note: The QUIT button allows burning in the final curve and lists its
;         values.  The LIST button lists the current curve parameters.
;       
;       Equation file format: This file defines the equation, the x range
;       and range of each adjustable parameter. The file is a text file with
;       certain tags. Null and comment lines (* in first column) are allowed.
;       The tags are shown by an example:
;          eq: y = a + b*x + c*x^2
;          title:  Parabola
;          xrange:  -10 10
;          n_points:  100
;          par:  a -50 50 0
;          par:  b -50 50 0
;          par:  c -10 10 1
;       
;       The parameter tags are followed by 4 items:  Parameter name (as in the
;       equation), min value, max value, initial value.  An optional 5th item
;       may be the word int to force an integer value (for flags or harmonics).
; MODIFICATION HISTORY:
;       R. Sterner, 1998 Feb 24
;	R. Sterner, 1998 Mar 30 --- Added font_size = fntsz.
;       R. Sterner, 2010 May 04 --- Converted arrays from () to []. Just a bit.
; ????? What is this routine ?????
;
; Copyright (C) 1998, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
        pro eqv_plot, _d, erase=erase, burn=burn
 
	;-------  Erase last plot first  ---------------
	if _d.flag eq 1 then begin
	  device, set_graph=6		; XOR mode.
          oplot, *_d.x_ptr, *_d.y_ptr
	  device, set_graph=3		; COPY mode (normal).
	  if keyword_set(erase) then return
	  if keyword_set(burn) then begin
            oplot, *_d.x_ptr, *_d.y_ptr, color=_d.color
	    return
	  endif
	endif
 
        ;-------  Get Independent var range  ----------
        widget_control, _d.x1, get_val=_x1  &  _x1 = _x1[0]
        _err = execute('_x1='+_x1)
        widget_control, _d.x2, get_val=_x2  &  _x2 = _x2[0]
        _err = execute('_x2='+_x2)
 
        ;-------  Get number of points  ------
        widget_control, _d.n, get_val=_n & _n = _n[0]
        if _n eq '' then begin
          _n = '100'
          widget_control, _d.n, set_val=_n
        endif
 
	;------  Get x scale factor  ---------
	;---  Applied after calculation but before plotting  ---
	widget_control, _d.xs, get_val=_xs & _xs = _xs[0]
	if _xs eq 0. then _xs=1.0	; Avoid 0 case.
 
	;-----  Generate Independent var array  -----------
	if _d.var eq 'X' then begin	; Function of X.
	  x = maken(_x1, _x2, _n)
	endif else begin		; Function of T.
	  t = maken(_x1, _x2, _n)
	endelse
 
        ;-------  Read equation  -------------
        widget_control, _d.equat, get_val=_equat
        _equat = _equat(0)
 
        ;-----  Set parameters to their values ------
        for _i = 0, n_elements(_d.pname)-1 do begin
          _t = _d.pname[_i]+'='+string(_d.pval[_i])
          _err = execute(_t)
        endfor
 
        ;------  Compute Y(X) or X(T) & Y(T)  --------------------
        _err = execute(_equat)
        if _err ne 1 then begin
          xhelp,exit='OK',['Error executing:',_equat,!err_string,$
            ' ','Do the following to recover:',$
            '1. Correct any errors in the equation text',$
            '   the name of the independent variable.',$
            '2. Do retall.','3. Do xmanager.','4. Press the OK button below.']
          return
        endif
 
	;----------------------------------------------------------
	;  At this point the independent var array has been set up and the
	;  equation has been executed, so X and Y now both exist.
	;----------------------------------------------------------
 
	;---------  Apply x plot scale factor (usually 1)  -------
	if _xs ne 1. then x=x*_xs
 
        ;---------  Plot equation  -------------
	device, set_graph=6		; XOR mode.
        oplot, x, y
	device, set_graph=3		; COPY mode (normal).
	ptr_free, _d.x_ptr, _d.y_ptr	; Free old pointers.
	_d.x_ptr = ptr_new(x)		; Save X array.
	_d.y_ptr = ptr_new(y)		; Save Y array.
	_d.flag = 1			; Set plot flag.
 
        return
        end
 
 
;==============================================================n
;       sp2v = Convert slider position to value.
;===============================================================
 
        function sp2v, p, smax, pmin, pmax, int=int
	if keyword_set(int) then begin
          return, fix((p/float(smax))*(pmax-pmin) + pmin)
	endif else begin
          return, (p/float(smax))*(pmax-pmin) + pmin
	endelse
        end
 
;===============================================================
;       sv2p = Convert slider value to position.
;===============================================================
 
        function sv2p, v, smax, pmin, pmax
        p = fix(.5+float(smax)*(v-pmin)/(pmax-pmin))
        return, p>0<smax
        end
 
 
;==============================================================
;       eqv2_compile = Equation file parser
;==============================================================
 
        pro eqv2_compile, file, xrange=xran, title=title, $
          par=par, number=num, equation=equat, var=var, $
          error=err
 
	err = 0
 
        ;-------  Add default extension  -------
        filebreak, file, dir=dir, name=name, ext=ext
        if ext eq '' then ext='eqv'
        f = dir+name+'.'+ext
 
        ;-------  Get equation title  ----------
        txt = getfile(f)
        if (size(txt))(0) eq 0 then begin
          xmess,['Equation file not opened:',f]
          err = 1
          return
        endif
	title = txtgetkey(init=txt, 'title', del=':',/start)
	if title eq '' then title=' '
        ;------- indep Get X Range  ----------
	tmpx='' & tmpt=''
        tmpx = txtgetkey('xrange', /start)
        if tmpx eq '' then tmpt = txtgetkey('trange', /start)
	if (tmpx eq '') and (tmpt eq '') then begin
          xmess,['Error in equation file:',file,' No X or T Range found.']
          err = 1
          return
        endif
	if tmpx ne '' then begin
	  var = 'X'
          tmp = repchr(tmpx,',')
	endif
	if tmpt ne '' then begin
	  var = 'T'
          tmp = repchr(tmpt,',')
	endif
        xran = [getwrd(tmp,0),getwrd(tmp,1)]
        ;---------  Get number of points  -------
        tmp = txtgetkey('n_points', /start)
        if tmp eq '' then tmp = '100'
        num = tmp + 0
        ;---------  Get equation  ---------------
        w = where(strupcase(strmid(txt,0,3)) eq 'EQ:', cnt)
        if cnt eq 0 then begin
          xmess,['Error in equation file:',file,' No equation found.']
          err = 1
          return
        endif
        equat = ''
        i = w(0)
        tmp = txt(i)                    ; First line of equation.
        p = strpos(tmp,':')             ; Skip over eq:.
        tmp = strmid(tmp,p+1,999)
loop:   p = strpos(tmp,'$')             ; $? = continued?
        if p gt 0 then begin            ; Yes:
          tmp = strmid(tmp,0,p)         ;   1. Drop $.
          equat = equat + tmp           ;   2. Concat line.
          i = i + 1                     ;   3. Read next.
          tmp = txt(i)
          goto, loop                    ;   4. Process.
        endif else begin                ; No.
          equat = equat + tmp           ; Last line, concat.
        endelse
        ;--------  Get adjustable parameters  ----------
        w = where(strupcase(strmid(txt,0,4)) eq 'PAR:', cnt)    ; Any par lines?
        if cnt eq 0 then begin          ; No.
          par = {n:0}                   ; Set par count to 0 and return.
          err = 0
          return
        endif
        txt = txt(w)                    ; Extract parameter lines.
        pname = strarr(cnt)
        pmin = fltarr(cnt)
        pmax = fltarr(cnt)
        pdef = fltarr(cnt)
        pflag = bytarr(cnt)
        for i = 0, cnt-1 do begin
          tmp = getwrd(txt(i),delim=':',/last)  ; i'th parameter line.
          tmp = repchr(tmp,',')         ; Drop any commas.
          pname(i)= getwrd(tmp,0)       ; Get name.
          pmin(i) = getwrd(tmp,1) + 0.  ; Get parameter range min.
          pmax(i) = getwrd(tmp,2) + 0.  ; Get parameter range max.
          pdef(i) = getwrd(tmp,3) + 0.  ; Get parameter range def.
          pflag(i) = strlowcase(getwrd(tmp,4)) eq 'int'  ; Get parameter type.
        endfor
        par = {n:cnt, name:pname, min:pmin, max:pmax, def:pdef, flag:pflag}
 
        err = 0
 
        return
        end
 
 
;==============================================================
;	eqv2_event = Event handler
;==============================================================
 
	pro eqv2_event, ev
 
	widget_control, ev.id, get_uval=name0	; Get name of action.
        widget_control, ev.top, get_uval=d      ; Get data structure.
        name = strmid(name0,0,3)                ; First 3 chars.
 
	if name eq 'WIN' then begin
	  wshow
          return
        endif
 
        if name eq 'OFF' then begin
	  flag0 = d.flag
	  eqv_plot, d, /erase
	  d.flag = 1 - flag0			  ; Swap Flag.
          widget_control, ev.top, set_uval=d      ; Update parameter values.
          return
        endif
 
        if name eq 'QUI' then begin
	  menu = ['Quit and Erase curve',$
		'Quit and Burn in curve',$
		'Continue']
	  opt = xoption(menu)
	  if opt eq 2 then return
	  if opt eq 0 then begin
	    eqv_plot, d, /erase
	    ptr_free, d.x_ptr, d.y_ptr		; Free pointers.
            widget_control, /dest, ev.top
            return
	  endif
	  if opt eq 1 then begin
	    eqv_plot, d, /burn
	    ptr_free, d.x_ptr, d.y_ptr		; Free pointers.
	    widget_control, d.equat, get_val=equat
	    equat = equat(0)
            widget_control, /dest, ev.top
	    n = n_elements(d.pname)
	    txt = strarr(n)
	    for i=0,n-1 do txt(i)=strtrim(d.pname(i),2)+' = '+$
	      strtrim(d.pval(i),2)
	    xhelp,['Final parameter values for',equat,' ',txt], $
	      exit_text='Done'
            return
	  endif
        endif
 
        if name eq 'LIS' then begin
	  widget_control, d.equat, get_val=equat
	  equat = equat(0)
	  n = n_elements(d.pname)
	  txt = strarr(n)
	  for i=0,n-1 do txt(i)=strtrim(d.pname(i),2)+' = '+strtrim(d.pval(i),2)
	  xhelp,['Current parameter values for',equat,' ',txt], $
	    exit_text='Continue'
          return
        endif
 
        ;-------  Handle plot related items  ---------------
        if name eq 'PLT' then begin
          eqv_plot, d
	  goto, update_str
        endif
 
        ;-------  Handle parameter related items  ----------
        if name eq 'PAR' then begin
          act = strmid(name0,3,3)       ; Parameter action code.
          i = strmid(name0,6,2) + 0     ; Parameter index.
 
          ;-------  Process action code  --------
          case act of
'SLD':    begin         ;*** Moved slider. ***
            widget_control, d.id_slid(i), get_val=p             ; New pos.
          end
'MIN':    begin         ;*** Entered new range min. ***
            widget_control, d.id_pmin(i), get_val=t             ; Get ran min.
            d.pmin(i) = t+0.                                    ; Store.
            p = sv2p(d.pval(i), d.smax, d.pmin(i), d.pmax(i))   ; New pos.
            widget_control, d.id_slid(i), set_val=p             ; Update slider.
          end
'MAX':    begin         ;*** Entered new range max. ***
            widget_control, d.id_pmax(i), get_val=t             ; Get ran min.
            d.pmax(i) = t+0.                                    ; Store.
            p = sv2p(d.pval(i), d.smax, d.pmin(i), d.pmax(i))   ; New pos.
            widget_control, d.id_slid(i), set_val=p             ; Update slider.
          end
'STN':    begin         ;*** Set current value as new range min. ***
            d.pmin(i) = d.pval(i)       ; Update and display new range min.
            widget_control, d.id_pmin(i), set_val=strtrim(d.pmin(i),2)
            p = 0
            widget_control, d.id_slid(i), set_val=p             ; Update slider.
          end
'STX':    begin         ;*** Set current value as new range max. ***
            d.pmax(i) = d.pval(i)       ; Update and display new range max.
            widget_control, d.id_pmax(i), set_val=strtrim(d.pmax(i),2)
            p = d.smax
            widget_control, d.id_slid(i), set_val=p             ; Update slider.
          end
'DEF':    begin         ;*** Set current value back to default  ***
            widget_control,d.id_pval(i),set_val=strtrim(d.pdef(i),2) ; Val 2 Def
            p = sv2p(d.pdef(i), d.smax, d.pmin(i), d.pmax(i))
            widget_control, d.id_slid(i), set_val=p             ; Update slider.
          end
'VAL':    begin
            widget_control, d.id_pval(i), get_val=t             ; Get ran min.
            d.pval(i) = t+0.                                    ; Store.
            p = sv2p(d.pval(i), d.smax, d.pmin(i), d.pmax(i))   ; New pos.
            widget_control, d.id_slid(i), set_val=p             ; Update slider.
            widget_control, ev.top, set_uval=d    ; Update parameter values.
            eqv_plot, d                           ; Update plot.
	    goto, update_str
          end
'NAM':    begin
            widget_control, d.id_pnam(i), get_val=t             ; Get par name.
            d.pname(i) = t                                      ; Replace old.
            widget_control, ev.top, set_uval=d    ; Update parameter values.
            eqv_plot, d                           ; Update plot.
	    goto, update_str
          end
          endcase
          ;-------  Always: compute new val, display it, store it.
          v = sp2v(p, d.smax, d.pmin(i), d.pmax(i), int=d.pflag(i)) ; New val.
          widget_control,d.id_pval(i),set_val=strtrim(v,2)      ; Display..
          d.pval(i) = v                                         ; Store.
          widget_control, ev.top, set_uval=d      ; Update parameter values.
          eqv_plot, d                             ; Update plot.
	  goto, update_str
        endif
 
	print,' Unkown command: ',name0
	return
 
update_str:
	d.flag = 1				; Always on at this point.
        widget_control, ev.top, set_uval=d      ; Save data structure.
	return
 
	end
 
 
;==============================================================
;	eqv2 = Main Equation viewer version II routine	
;
;	Allows functions to be added to current plot
;	and function parameters to be interactively adjusted
;	using slider bars.  Useful for fitting purposes.
;==============================================================
 
	pro eqv2, file, color=clr, size=fntsz, help=hlp
 
	if keyword_set(hlp) then begin
help:	  print,' Interactively plot a function on current plot.'
	  print,' eqv2, file'
	  print,'   file = equation file.   in'
	  print,' Keywords:'
	  print,'   COLOR=clr  Curve burn in color (def=!p.color).'
	  print,'   SIZE=fsz   Font size to use (points).'
	  print,' Note: The QUIT button allows burning in the final curve and lists its'
	  print,'   values.  The LIST button lists the current curve parameters.'
	  print,' '
	  print,' Equation file format: This file defines the equation, the x range'
	  print,' and range of each adjustable parameter. The file is a text file with'
	  print,' certain tags. Null and comment lines (* in first column) are allowed.'
	  print,' The tags are shown by an example:'
	  print,'    eq: y = a + b*x + c*x^2'
	  print,'    title:  Parabola'
	  print,'    xrange:  -10 10'
	  print,'    n_points:  100'
	  print,'    par:  a -50 50 0'
	  print,'    par:  b -50 50 0'
	  print,'    par:  c -10 10 1'
	  print,' '
	  print,' The parameter tags are followed by 4 items:  Parameter name (as in the'
	  print,' equation), min value, max value, initial value.  An optional 5th item'
	  print,' may be the word int to force an integer value (for flags or harmonics).'
	  return
	endif
 
	if n_elements(clr) eq 0 then clr=!p.color

	;------  Deal with font size  --------------
	if n_elements(fntsz) ne 0 then begin
	  fnt = fontgen()
	  sz = strtrim(fntsz,2)
	  tmp = execute('fnrm = fnt.MONO'+sz+'M')
	  tmp = execute('fbld = fnt.PROP'+sz+'B')
	endif else fntsz=0
 
	;------  No equation file given  ------------
	opt = 0
	if n_elements(file) eq 0 then begin
	  menu = ['Specify an existing equation file',$
		  'Generate a new equation file',$
		  'Quit','Display help text']
	  opt = xoption(menu,title='Must use an equation file')
	  ;-------  Quit  --------
	  if opt eq 2 then return
	  ;-------  Help  --------
	  if opt eq 3 then goto, help
	  ;-------  Enter file name  --------
	  if opt eq 0 then begin
	    file = ''
	    read,' Enter name of equation file: ',file
	    if file eq '' then return
	  endif
	  ;-------  Generate new file  -------------
	  ;-------  Equation file wizard  -----------
	  if opt eq 1 then begin
	    ;------  Description  ----------
	    xhelp,exit='Next ->',$
		['Equation file wizard',' ',$
		'You will be prompted to enter values needed for',$
		'an equation file.  The entered values will then be',$
		'saved in a file and the equation viewer will come up.']
	    xtxtin,title,title='Enter short description of function (optional)'
	    ;------  Equation  --------
	    xhelp,exit='Next ->',$
		['The entered equation must be an explicit function',$
		'of x and return y as the result.',' ',$
		'Some examples:',' ','y = a + b*x + c*x^2 + d*x^3', $
		'y = iw_disp(k=x, phv=phv, dep=dep, bv=bv, cur=cur)',$
		' ','The entry must be a valid IDL equation or function call.',$
		' ','The equation may alternatively be an explicit function',$
		'of a parameter t instead of x.  Then x and y must be',$
		'functions of t.  Examples:',$
		' ','x = r*cos(t)+x0 & y = r*sin(t)+y0',' ',$
		'Note the & used to separate the two functions.']
	    var = xoption(['X','T','Cancel'],title='Select variable')
	    if var eq 2 then return
	    var = (['x','t'])(var)
	    xtxtin,equat,title='Enter equation:'
	    if equat eq '' then return
	    ;------  X range  -----------
	    xhelp,exit='Next ->',$
                ['The range in '+var+' covered by the function is specified by',$
		'giving the min and max values.  Some examples:',$
		' ','0, 0.5',' or','-10.0 10.0', $
		' ','This range is what will be covered by the plotted',$
		'function.  It may be changed at run time if desired.']
	    xtxtin,xran0,title='Enter x range (min max):'
	    if xran0 eq '' then return
	    xran = [getwrd(xran0,0),getwrd(xran0,1)]+0.
	    ;-------  Number of points  ----------
	    xhelp,exit='Next ->',$
                ['The number of points in x determines how smooth the',$
		'function plot will be.  Specify a reasonable number, it', $
		'may be changed at run time.']
	    xtxtin,num,def='100',title='Enter number of points in '+var+':'
	    if num eq '' then return
	    ;-------  Parameters  -------------
	    xhelp,exit='Next ->',$
                ['The next step is to enter a line of values for each',$
		'adjustable parameter in the function.  Each such',$
		'parameter will have a slider to vary its value',$
		'and plot the result.',' ',$
		'Four items are needed in each parameter entry line:',$
		'1. Parameter name: exactly as in the entered equation.',$
		'2. Minimum value.  The slider varies from min to max.',$
		'3. Maximum value.',$
		'4. Initial value.  The slider starts here.',$
		' ','Enter a new parameter line for each, a null entry',$
		'means all parameters have been entered.',' ',$
		'Do not enter a line for x, it is already done.']
	    tmp = ['']
	    i = 1
zloop:      itxt = 'Enter for adjustable parameter '+strtrim(i,2)+':'
	    xtxtin,txt,title=[equat,itxt,'Name  Min  Max  Init']
	    if txt eq '' then goto, zdone
	    if nwrds(txt) lt 4 then begin
	      xmess,['Error in entry.  Must have 4 or 5 items:', $
		'Parameter_name  min_value  max_value  init_value [int]', $
		' ','Try again']
	      goto, zloop
	    endif
	    tmp = [tmp,txt]
	    i = i+1
	    goto, zloop
	    ;-------  Pick apart parameter lines  --------
zdone:	    tmp = tmp(1:*)
	    n = n_elements(tmp)
	    name = strarr(n)
	    amin = fltarr(n)
	    amax = fltarr(n)
	    def = fltarr(n)
	    flag = bytarr(n)
	    for i=0,n-1 do begin
	      name(i) = getwrd(tmp(i),0)
	      amin(i) = getwrd(tmp(i),1)+0.
	      amax(i) = getwrd(tmp(i),2)+0.
	      def(i) = getwrd(tmp(i),3)+0.
	      flag(i) = strlowcase(getwrd(tmp(i),4)) eq 'int'
	    endfor
	    par = {n:n,name:name,min:amin,max:amax,def:def,flag:flag}
	    ;-------  Save new equation to file?  --------
	    ans = xyesno('Save newly entered equation to a file?')
	    if ans eq 'Y' then begin
	      xtxtin,file,def='temp',title='Name of file to save equation in:'
	      if file ne '' then begin
		filebreak, file, dir=dir, name=name, ext=ext
		if ext eq '' then ext='eqv'
		f = dir+name+'.'+ext
		openw,lun,f,/get_lun
		printf,lun,'*--- '+f+' = Equation file for '+title
		printf,lun,'*--- '+created()
		printf,lun,' '
		printf,lun,'eq:  '+equat
		printf,lun,'title:  '+title
		printf,lun,var+'range:  '+xran0
		printf,lun,'n_points:  '+num
		for i=0,n-1 do printf,lun,'par:  '+tmp(i)
		free_lun, lun
		xmess,'Equation saved in file '+f
		var = strupcase(var)
	      endif  ; Gave a non-null file name.
	    endif  ; Yes, save equation in file.
	  endif  ; Enter new file.
	endif	; No file given.
 
	;----  Deal with equation file  ----
	if opt eq 0 then begin
	  eqv2_compile, file, xrange=xran, number=num, equation=equat, $
            title=title, par=par, var=var, error=err
	  if err ne 0 then return
	endif
 
	;--------  Set up widget  ------------
	smax = 800
	if fntsz ne 0 then smax=round(800.*fntsz/16.)
	
	top = widget_base(/col, title=title)
 
	;--------  Equation window  -------
	id_eq = widget_text(top,val=equat,xsize=60,ysize=1,/edit, $
	  uval='PLT', font=fnrm)
 
	;--------  Sliders  ---------------
        id_slid   = lonarr(par.n)   ; Parameter slider related wids.
        id_parnam = strarr(par.n)
        id_parval = lonarr(par.n)
        id_parmin = lonarr(par.n)
        id_parmax = lonarr(par.n)
        for i = 0, par.n-1 do begin         ; Loop through parameters.
          b2 = widget_base(top,/row)
          id = widget_text(b2,val=par.name(i),xsize=10, /edit, $
            uval='PARNAM'+strtrim(i,2), font=fnrm)
          id_parnam(i) = id
	  tmp = par.def(i)
	  if par.flag(i) eq 1 then tmp=fix(tmp)
          id = widget_text(b2,val=strtrim(tmp,2),xsize=15,/edit,$
            uval='PARVAL'+strtrim(i,2), font=fnrm)
          id_parval(i) = id
          id = widget_label(b2,val='Range: ', font=fbld)
	  tmp = par.min(i)
	  if par.flag(i) eq 1 then tmp=fix(tmp)
          id = widget_text(b2,val=strtrim(tmp,2),/edit,$
            uval='PARMIN'+strtrim(i,2),xsize=15, font=fnrm)
          id_parmin(i) = id
          id = widget_label(b2,val=' to ', font=fbld)
	  tmp = par.max(i)
	  if par.flag(i) eq 1 then tmp=fix(tmp)
          id = widget_text(b2,val=strtrim(tmp,2),/edit,$
            uval='PARMAX'+strtrim(i,2),xsize=15, font=fnrm)
          id_parmax(i) = id
          id = widget_button(b2,val='Min',uval='PARSTN'+strtrim(i,2),font=fbld)
          id = widget_button(b2,val='Max',uval='PARSTX'+strtrim(i,2),font=fbld)
          id = widget_button(b2,val='Def',uval='PARDEF'+strtrim(i,2),font=fbld)
          s = widget_slider(top,uval='PARSLD'+strtrim(i,2),xsize=smax+1,$
            max=smax, /suppress,/drag)
          id_slid(i) = s
        endfor
 
        ;-------  Display widget and update plot  -------
        for i=0, par.n-1 do begin       ; Set parameter slider starting points.
          widget_control, id_slid(i),set_val=$
            sv2p(par.def(i),smax,par.min(i),par.max(i))
        endfor
 
	;-------  Function buttons  ----------------
	b = widget_base(top,/row)
	id = widget_button(b, value='Quit', uval='QUIT',font=fbld)
	id = widget_button(b, value='List', uval='LIST',font=fbld)
	id = widget_button(b, value='Off', uval='OFF',font=fbld)
	id = widget_button(b, value='Win', uval='WIN',font=fbld)
 
	;-------  Independent var  range and number  --------------
        id = widget_label(b,val='   ')
        id = widget_label(b,val=var+': Min ',font=fbld)
        id_x1 = widget_text(b,val=strtrim(xran(0),2),/edit,$
          xsize=10, uval='PLT', font=fnrm)
        id = widget_label(b,val='Max ',font=fbld)
        id_x2 = widget_text(b,val=strtrim(xran(1),2),/edit,$
          xsize=10, uval='PLT', font=fnrm)
	id = widget_label(b,val='Points ',font=fbld)
	id_n = widget_text(b,val=strtrim(num,2),/edit,$
          xsize=10, uval='PLT', font=fnrm)
	id = widget_label(b,val='Scale ',font=fbld)
	id_xs = widget_text(b,val='1.000',/edit,$
          xsize=10, uval='PLT', font=fnrm)
 
	;----  Set up info structure  -------
	info = {top:top,xran:xran,num:num,pflag:[par.flag],pname:[par.name], $
	  pdef:[par.def], pval:[par.def], pmin:[par.min], pmax:[par.max], $
	  x1:id_x1, x2:id_x2, n:id_n, xs:id_xs, equat:id_eq, smax:smax, $
	  id_slid:[id_slid], id_pval:[id_parval], id_pmin:[id_parmin], $
	  id_pmax:[id_parmax], id_pnam:[id_parnam], var:var, $
	  flag:0, x_ptr:ptr_new(), y_ptr:ptr_new(), color:clr }
	widget_control, top, set_uval=info
 
	;----  Do first plot here  --------
        eqv_plot, info
	widget_control, top, set_uval=info
 
        ;-------  xmanager  -------
        widget_control, top, /real
        xmanager, 'eqv2', top
 
	return
	end
