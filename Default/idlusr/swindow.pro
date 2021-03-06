;-------------------------------------------------------------
;+
; NAME:
;       SWINDOW
; PURPOSE:
;       Create a scrolling window.  Works much like window.
; CATEGORY:
; CALLING SEQUENCE:
;       swindow
; INPUTS:
; KEYWORD PARAMETERS:
;       keywords:
;         INDEX=ind Returned window index, base number, and
;           sw index, and draw widget ID:
;           ind = [indx, base, sw_ind, drw]. Use indx in wset, base
;           is not be needed directly, sw_ind is used in swdelete.
;           May use drw to set window lower left corner to ix,iy:
;             widget_control,drw,set_draw_view=[ix,iy]
;         COLORS=c  Set number of colors to use in windows.
;           Must be given for the first window created.
;         XSIZE=xs  Set total window X size in pixels.
;         YSIZE=ys  Set total window Y size in pixels.
;           Defaults = 500 x 400.
;         X_SCROLL_SIZE=xsc  Set visible window X size in pixels.
;         Y_SCROLL_SIZE=ysc  Set visible window Y size in pixels.
;           Defaults = total window size up to 2/3 screen size.
;         /RESIZE Allow window resize.
;           Can also set RESIZE to the name of a redradr routine:
;             RESIZE=rout, which must take the keyword /REDRAW to
;             redraw the window after a resize.
;         GET_SCROLL=scr returns [xscr,yscr], scroll sizes that would
;           be used.  No window is made with this option.
;         TITLE=txt  Set optional window title.
;         VIEW=[ix0,iy0] Set viewport into total scrolling window
;           by giving window pixel to show at lower left corner. IF
;           VIEW not given the viewport is at upper left of window.
;         /QUIET  means do not list window number when created.
;         RETAIN=r  Set backing store type (def=2, see manual).
;         /PIXMAP means use pixmap instead of screen window.  If
;          given then an ordinary window is used.
;         GROUP_LEADER=grp  Assign a group leader to this
;           widget.  When the widget with ID group is destroyed
;           this widget is also.
;         XOFFSET=xoff, YOFFSET=yoff  Window position on screen.
;           Pixels from screen upper left corner.
; OUTPUTS:
; COMMON BLOCKS:
;       swindow_com
; NOTES:
;       Notes: A draw widget is used to make a scrolling window.
;         Differences between windows and draw widgets prevent
;         exact emulation of the window command.
;         See also swdelete, and swlist.
;         Can use the arrow keys to move a scrolling window
;         around.  Shift moves faster, Control slower.
; MODIFICATION HISTORY:
;       R. Sterner, 14 Jun, 1993
;       R. Sterner, 29 Sep, 1993
;       R. Sterner, 30 Dec, 1993 --- added /QUIET.
;       R. Sterner, 1995 Dec 20 --- removed window size extension.
;       R. Sterner, 1997 Sep 24 --- Handled Win95 Y scroll bug.
;       R. Sterner, 2002 Jan 21 --- Added VIEW=vw.
;       R. Sterner, 2002 Aug 14 --- Added draw widget ID to INDEX.
;       R. Sterner, 2002 Aug 14 --- Dropped old unneeded code, save draw wid.
;       R. Sterner, 2002 Sep 22 --- Added XOFFSET, YOFFSET.
;       R. Sterner, 2003 Mar 17 --- If no scrolling needed, none used.
;       R. Sterner, 2003 Mar 17 --- Added GROUP_LEADER.
;       R. Sterner, 2003 Apr 02 --- Fixed window scroll sizes. Added GET_SCROLL.
;       R. Sterner, 2008 May 02 --- Added /resize keyword.
;       R. Sterner, 2008 Jun 20 --- Added /RESIZE to help text.
;       R. Sterner, 2008 Jun 20 --- Added Arrow key viewport moving.
;       R. Sterner, 2010 May 04 --- Converted arrays from () to [].
;
; Copyright (C) 1993, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	;-------------------------------------------------------------
	;  swindow event handler
	;-------------------------------------------------------------
	pro swindow_event, ev
 
	  evname = tag_names(ev,/struct)
 
	  ;-----------------------------------------------------
	  ;  Keyboard viewport move command event
	  ;
	  ;  Used widget_control in the main routine to set
	  ;  input_focus to the draw widget.  This gives events
	  ;  when a key is pressed when the mouse is in the window.
	  ;  For arrow keys the event type is 6.  The event key
	  ;  after offset by 5 is the following:
	  ;  LF:0,RT:1,UP:2,DN:3,PAGE_UP:4,PAGE_DN:5,HOME:6,END:7
	  ;  The event modifiers indices if Shift, Control, or Alt
	  ;  was used.  The arrow keys move by 10 pixels and
	  ;  shift arrow by 50 and control arrow by 1.
	  ;-----------------------------------------------------
	  if evname eq 'WIDGET_DRAW' then begin
	    if ev.release eq 0 then return	; Ignore key press.
	    if ev.type ne 6 then return		; Ignore non-motion keys.
	    key = ev.key - 5			; Key code.
	    mdc = (ev.modifiers<3)		; Modifier code.
	    dx = ([-1,1,0,0,0,0,0,0])[key]
	    dy = ([0,0,1,-1,500,-500,-10000,10000])[key]
	    if mdc eq 0 then fct=10		; Normal.
	    if mdc eq 1 then fct=50		; Shift.
	    if mdc eq 2 then fct=1		; Control.
	    widget_control, ev.id, get_draw_view=vw
	    x0 = vw[0]				; Current position.
	    y0 = vw[1]
	    x1 = x0 + fct*dx			; New position.
	    y1 = y0 + fct*dy
	    widget_control, ev.id, set_draw_view=[x1,y1]
	    return
	  endif ; ' then begin
 
	  ;-----------------------------------------------------
	  ;  Window resize event
	  ;-----------------------------------------------------
	  if evname eq 'WIDGET_BASE' then begin
	    widget_control, ev.top, get_uval=s
	    g = widget_info(ev.top, /geometry)
 
	    widget_control, ev.top, tlb_size_events = 0
 
	    widget_control, s.id_draw, /destroy
	    xs = s.xsize>(g.scr_xsize-29)
	    ys = s.ysize>(g.scr_ysize-29)
	    xss = g.scr_xsize-29
	    yss = g.scr_ysize-29
 
	    id_draw = widget_draw(ev.top,xs=xs,ys=ys,x_scr=xss, $
	      y_scr=yss, colors=s.colors, retain=s.retain)
	    widget_control, id_draw, draw_keyboard_events=1,/input_focus
 
	    s.id_draw = id_draw
	    widget_control, ev.top, set_uval=s
 
	    widget_control, ev.top, /tlb_size_events
 
	    if s.rout ne '' then call_procedure,s.rout,/redraw
	    return
	  endif ; WIDGET_BASE
 
	end
 
 
 
	;-------------------------------------------------------------
	;  swindow main routine
	;-------------------------------------------------------------
	pro swindow, index=indx, colors=colors, quiet=quiet, $
	  xsize=xsize, ysize=ysize, x_scroll_size=xscr0, $
	  y_scroll_size=yscr0, title=title, retain=retain, $
	  pixmap=pix, view=view, xoffset=xoff, yoffset=yoff, $
	  group_leader=grp, get_scroll=getscr, resize=resize, help=hlp
 
	common swindow_com, index_table, base_table, sw_ind, swcnt, $
	  sw_titl, sw_fx, sw_fy, sw_vx, sw_vy, drw_table
	;-------------------------------------------------------------
	;  Scrolling windows common (only for scrolling windows):
	;    index_table = array of window numbers to be used by wset.
	;    base_table = array of window widget base numbers.
	;    swcnt = Current count of scrolling windows.
	;    sw_titl = array of window titles.
	;    sw_ind = array of window numbers as seen by user (100+).
	;    sw_fx = array of window full x sizes.
	;    sw_fy = array of window full y sizes.
	;    sw_vx = array of window visible x sizes.
	;    sw_vy = array of window visible y sizes.
	;    drw_table = array of draw widget IDs.
	;--------------------------------------------------------------
 
	if keyword_set(hlp) then begin
	  print,' Create a scrolling window.  Works much like window.'
	  print,' swindow'
	  print,' keywords:'
	  print,'   INDEX=ind Returned window index, base number, and'
	  print,'     sw index, and draw widget ID:'
	  print,'     ind = [indx, base, sw_ind, drw]. Use indx in wset, base'
	  print,'     is not be needed directly, sw_ind is used in swdelete.'
	  print,'     May use drw to set window lower left corner to ix,iy:'
	  print,'       widget_control,drw,set_draw_view=[ix,iy]'
	  print,'   COLORS=c  Set number of colors to use in windows.'
	  print,'     Must be given for the first window created.'
	  print,'   XSIZE=xs  Set total window X size in pixels.'
	  print,'   YSIZE=ys  Set total window Y size in pixels.'
	  print,'     Defaults = 500 x 400.'
	  print,'   X_SCROLL_SIZE=xsc  Set visible window X size in pixels.'
	  print,'   Y_SCROLL_SIZE=ysc  Set visible window Y size in pixels.'
	  print,'     Defaults = total window size up to 2/3 screen size.'
	  print,'   /RESIZE Allow window resize.'
	  print,'     Can also set RESIZE to the name of a redradr routine:'
	  print,'       RESIZE=rout, which must take the keyword /REDRAW to'
	  print,'       redraw the window after a resize.'
	  print,'   GET_SCROLL=scr returns [xscr,yscr], scroll sizes that would'
	  print,'     be used.  No window is made with this option.'
	  print,'   TITLE=txt  Set optional window title.'
	  print,'   VIEW=[ix0,iy0] Set viewport into total scrolling window'
	  print,'     by giving window pixel to show at lower left corner. IF'
	  print,'     VIEW not given the viewport is at upper left of window.'
	  print,'   /QUIET  means do not list window number when created.'
	  print,'   RETAIN=r  Set backing store type (def=2, see manual).'
	  print,'   /PIXMAP means use pixmap instead of screen window.  If'
	  print,'    given then an ordinary window is used.'
	  print,'   GROUP_LEADER=grp  Assign a group leader to this'
	  print,'     widget.  When the widget with ID group is destroyed'
	  print,'     this widget is also.'
	  print,'   XOFFSET=xoff, YOFFSET=yoff  Window position on screen.'
	  print,'     Pixels from screen upper left corner.'
	  print,' Notes: A draw widget is used to make a scrolling window.'
	  print,'   Differences between windows and draw widgets prevent'
	  print,'   exact emulation of the window command.'
	  print,'   See also swdelete, and swlist.'
	  print,'   Can use the arrow keys to move a scrolling window'
	  print,'   around.  Shift moves faster, Control slower.'
	  return
	endif
 
	;--------  Set defaults  -----------
	if n_elements(xscr0) gt 0 then xscr=xscr0
	if n_elements(yscr0) gt 0 then yscr=yscr0
	if n_elements(ind) eq 0 then ind = 0		; Def window = 0.
	if n_elements(colors) eq 0 then colors = 0	; Use default # colors.
	if n_elements(xsize) eq 0 then xsize = 500	; Default window size.
	if n_elements(ysize) eq 0 then ysize = 400
	device, get_screen_size=ss			; Get screen size.
	xmx = ss[0]*2/3					; Max allowed default
	ymx = ss[1]*2/3					;   window size.
	;-----  X SCROLL  -----------------
	if n_elements(xscr) eq 0 then xscr=0
	;-----  Not Given  ----------------
	if xscr eq 0 then begin				; Default scroll size.
	  if xsize lt xmx then begin
	    if ysize lt ymx then xscr=0 else xscr=xsize+2
	  endif else begin
	    xscr = xmx
	  endelse
	;-----  Given  --------------------
	endif else xscr=xscr<(xsize+2)<(ss[0]-100)
	;-----  Y SCROLL  -----------------
	if n_elements(yscr) eq 0 then yscr=0
	;-----  Not Given  ----------------
	if yscr eq 0 then begin
	  if ysize lt ymx then begin
	    if xsize lt xmx then yscr=0 else yscr=ysize+2
	  endif else begin
	    yscr = ymx
	  endelse
	;-----  Given  --------------------
	endif else yscr=yscr<(ysize+2)<(ss[1]-100)
 
	;-------------------------------------------------------------
	;  Limitations on scroll sizes:
	;    Can give to the draw widget scroll sizes of 0, but
	;  can only have both x and y scroll sizes of 0 when
	;  both xsize and ysize are < screen size.  If one is bigger
	;  use a scroll size < screen on that dimension and
	;  a scroll size of window size + 2 on smaller dimension.
	;  For a window with both x and y size < screen cannot use
	;  scroll size = size+2 on both dimensions, use scroll size
	;  of 0 on both.
	;-------------------------------------------------------------
	;-----  can't have one 0 (none or both is ok) -------
	tmp = [xscr,yscr]
	if (min(tmp) eq 0) and (max(tmp) gt 0) then begin
	  if xscr eq 0 then xscr=(xsize+2)<(ss[0]-100)
	  if yscr eq 0 then yscr=(ysize+2)<(ss[1]-100)
	endif
 
	if arg_present(getscr) then begin
	  if xscr eq 0 then xscr2=xsize else xscr2=xscr
	  if yscr eq 0 then yscr2=ysize else yscr2=yscr
	  getscr = [xscr2,yscr2]
	  return
	endif
 
	if n_elements(retain) eq 0 then retain = 2	; Default backing store.
	;--------  Deal with pixmap  ---------------
	if keyword_set(pix) then begin
	  window,/free,xs=xsize,ys=ysize, $		; Ordinary window.
	    colors=colors, retain=retain, /pixmap
	  indx = !d.window
	  if not keyword_set(quiet) then print,' Pixmap '+strtrim(indx,2)
	  return
	endif
 
	;--------  Create scrolling window  --------
	;-------  Window number and title  ----------
	if n_elements(swcnt) eq 0 then swcnt = 99	; Init next window num.
	swcnt = swcnt + 1				; Next window num.
	if n_elements(title) eq 0 then begin		; Make a title.
	  if xsize ge 127 then begin			;   Big window.
	    title = 'swindow '+strtrim(swcnt,2)
	  endif else begin				;   Little window.
	    title = strtrim(swcnt,2)
	  endelse
	endif
 
	;-------  Make Scrolling window  ----------------
	top = widget_base(title=title,xoff=xoff,yoff=yoff)
	if n_elements(grp) ne 0 then widget_control, top, group=grp
	id_draw = widget_draw(top,xs=xsize,ys=ysize,x_scr=xscr,$
	  y_scr=yscr, colors=colors, retain=retain)
	widget_control, top, /real
	if n_elements(view) eq 2 then widget_control,id_draw,set_draw_view=view
 
	;-------  Window resize event  ---------
	if n_elements(resize) ne 0 then begin
	  widget_control, top, /tlb_size_events
;	  if datatype(resize) eq 'STR' then rout=resize else rout=''
;	  info = {id_draw:id_draw, xsize:xsize, ysize:ysize, xscr:xscr, $
;		yscr:yscr, colors:colors, retain:retain, rout:rout}
;	  widget_control, top, set_uval=info
	endif
	if datatype(resize) eq 'STR' then rout=resize else rout=''
	info = {id_draw:id_draw, xsize:xsize, ysize:ysize, xscr:xscr, $
	  scr:yscr, colors:colors, retain:retain, rout:rout}
	widget_control, top, set_uval=info
 
	;--------  Update common  ----------
	;---  Common is used to match window indices with widget bases.
	;---  This allows swdelete to find the widget to delete.
	;-----------------------------------
	if n_elements(base_table) eq 0 then begin
	  index_table = [-2L]
	  base_table = [-2L]
	  sw_titl = strarr(1)
	  sw_ind = intarr(1)
	  sw_fx = lonarr(1)
	  sw_fy = lonarr(1)
	  sw_vx = intarr(1)
	  sw_vy = intarr(1)
	  drw_table = intarr(1)
	endif
	w = where(index_table lt 0, cnt)
	if cnt gt 0 then begin		; Replace a deleted value.
	  index_table[w[0]] = !d.window
	  base_table[w[0]] = top
	  sw_titl[w[0]] = title
	  sw_ind[w[0]] = swcnt
	  sw_fx[w[0]] = xsize
	  sw_fy[w[0]] = ysize
	  sw_vx[w[0]] = xscr
	  sw_vy[w[0]] = yscr
	  drw_table[w[0]] = id_draw
	endif else begin		; Put new values at front.
	  index_table = [!d.window, index_table]
	  base_table = [top, base_table]
	  sw_titl = [title,sw_titl]
	  sw_ind = [swcnt,sw_ind]
	  sw_fx = [xsize,sw_fx]
	  sw_fy = [ysize,sw_fy]
	  sw_vx = [xscr,sw_vx]
	  sw_vy = [yscr,sw_vy]
	  drw_table = [id_draw,drw_table]
	endelse
 
	;--------  Return window index  -----------
	indx = [!d.window,top, swcnt,id_draw]
	if not keyword_set(quiet) then print,' Window '+strtrim(!d.window,2)
 
	;--------  Manage widget  --------
	xmanager, 'swindow', top, /no_block
	widget_control, id_draw, draw_keyboard_events=1,/input_focus
 
	return
	end
