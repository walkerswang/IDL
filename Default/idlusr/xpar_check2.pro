;--------------------------------------------------------------------
;  xpar_check.pro = test xpar routines
;  R. Sterner, 2008 Oct 07
;  Returns a text array to parse.
;--------------------------------------------------------------------

	pro xpar_check2, txt

	text_block, txt0, /q
;   init: print,"Hello"
;   init: window,/free
;   init: erase,128
;    
;   title: Interactive Globe
;    
;   win_redirect: 1
;   code_scroll: 
;   code_width: 120
;    
;   code: map_set,lat,lon,ang,cont=cont,usa=usa,$
;     iso=iso,hor=hor, /nobord,  $
;     ortho=ortho,merc=merc,goode=goode,cyl=cyl &$
;     rb2ll,lonc,latc,radc,/deg,maken(0,360,100),x,y &$
;     plots,x,y,col=12582847
;    
;   par_frame: 0, /row
;   par_frame: 1, /row
;    
;   par: lat, -90, 90, 0, col=13421823
;   par: lon, 180, -180, 0, col=13421823
;   par: ang, -180, 180, 0, col=13421823
;    
;   par: latc, -90, 90, 0, col=12582847, fr=1
;   par: lonc, -180, 180, 0, col=12582847, fr=1
;   par: radc, 0, 90, 10, col=12582847, fr=1
;    
;   sliders: 3
;   slider_len: 300
;   y_scroll: 0
;    
;   flag_frame: 0, /row
;   flag_frame: 1, /row, /exclusive
;    
;   flags: cont=1, usa=1, iso=1, hor=1
;   flags: ortho=1, merc=0, goode=0, cyl=0, frame=1
;    
;   user: pro=xpar_example_snap, lab=Window snapshot, /info
;   user: pro=list_doy, lab=List day number in year
;    
;   exit: wdelete
;   exit: print,"Good-bye"

	txt = txt0

	end
