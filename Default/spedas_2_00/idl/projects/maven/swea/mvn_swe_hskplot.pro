;+
;PROCEDURE:   mvn_swe_hskplot
;PURPOSE:
;  Plots time series summary plots of SWEA housekeeping data over arbitrarily
;  long time spans.  The result is stored in TPLOT variables.
;
;USAGE:
;  mvn_swe_hskplot, trange=trange, orbit=orbit, hsk=hsk
;
;INPUTS:
;
;KEYWORDS:
;
;       TRANGE:       Time range over which load data.  Must have at least two
;                     elements, in any format accepted by time_double().  If
;                     not specified, then load data using the current timespan.
;
;       ORBIT:        Load data by orbit number (overrides TRANGE and TIMESPAN
;                     methods).
;
;       HSK:          Restore housekeeping from this IDL save/restore file.
;                     (Full path and name required.)
;
; $LastChangedBy: dmitchell $
; $LastChangedDate: 2017-04-22 13:27:58 -0700 (Sat, 22 Apr 2017) $
; $LastChangedRevision: 23211 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/maven/swea/mvn_swe_hskplot.pro $
;
;CREATED BY:    David L. Mitchell  2017-04-06
;-
pro mvn_swe_hskplot, trange=trange, orbit=orbit, hsk=hsk

  @mvn_swe_com

  oneday = 86400D

  if (size(hsk,/type) eq 7) then begin
    finfo = file_info(hsk)
    if (finfo.exists) then begin
      rflg = 1
      hsk_save = swe_hsk
      restore, file=hsk
      trange = minmax(swe_hsk.time)
      timespan, time_string([trange[0],trange[1]+oneday],prec=-3)
    endif else begin
      rflg = 0
      print,"File not found: ",hsk
      return
    endelse
  endif else rflg = 0

  if (~rflg) then begin  
    if keyword_set(orbit) then begin
      imin = min(orbit, max=imax)
      trange = mvn_orbit_num(orbnum=[imin-0.5,imax+0.5])
    endif

    tplot_options, get_opt=topt
    tspan_exists = (max(topt.trange_full) gt time_double('2013-11-18'))
    if ((size(trange,/type) eq 0) and tspan_exists) then trange = topt.trange_full

    if (size(trange,/type) eq 0) then begin
      print,"You must specify a time range or an orbit range, or set a timespan."
      return
    endif
    
    start_day = time_double(time_string(min(trange), prec=-3))
    stop_day = time_double(time_string(max(trange), prec=-3))
    ndays = floor((stop_day - start_day)/oneday) + 1L
  
    tsp = [start_day, (start_day + oneday)]
    ok = 0
    while (not ok) do begin
      mvn_swe_clear
      mvn_swe_load_l0, tsp    
      if (size(swe_hsk,/type) eq 8) then begin
        all_hsk = swe_hsk
        ok = 1
      endif else begin
        tsp += oneday
        if (tsp[1] gt stop_day) then begin
          print,"No data within specified time range."
          return
        endif
      endelse
    endwhile
  
    while (tsp[1] lt stop_day) do begin
      tsp += oneday
      mvn_swe_clear
      mvn_swe_load_l0, tsp
      if (size(swe_hsk,/type) eq 8) then all_hsk = [temporary(all_hsk), swe_hsk]
    endwhile
  
    swe_hsk = temporary(all_hsk)
  endif

; Make tplot variables

  pdT = ['']
  pdC = ['']
  TClab = replicate('',8)
  TCcol = round(findgen(8)*(247./7.)) + 7
  Vlab = TClab
  Tlab = TClab[0:2]

  dTmax = 10.
  dCmax = 10.

    vflg = 1

    if (vflg) then vnorm = [28., 12., 5., 3.3, 2.5] else vnorm = replicate(0.,5)

    store_data,'SWE28I',data={x:pfp_hsk.time, y:pfp_hsk.SWE28I}
    options,'SWE28I','ynozero',1
    options,'SWE28I','ytitle','Current (mA)'

    store_data,'LVPST' ,data={x:swe_hsk.time, y:swe_hsk.LVPST}
    store_data,'MCPHV' ,data={x:swe_hsk.time, y:swe_hsk.MCPHV}
    store_data,'NRV'   ,data={x:swe_hsk.time, y:swe_hsk.NRV}
    store_data,'ANALV' ,data={x:swe_hsk.time, y:swe_hsk.ANALV}
    store_data,'DEF1V' ,data={x:swe_hsk.time, y:swe_hsk.DEF1V}
    store_data,'DEF2V' ,data={x:swe_hsk.time, y:swe_hsk.DEF2V}
    store_data,'V0V'   ,data={x:swe_hsk.time, y:swe_hsk.V0V}
    store_data,'ANALT' ,data={x:swe_hsk.time, y:swe_hsk.ANALT}
    store_data,'P12V'  ,data={x:swe_hsk.time, y:(swe_hsk.P12V-vnorm[1])}
    store_data,'N12V'  ,data={x:swe_hsk.time, y:(swe_hsk.N12V+vnorm[1])}
    store_data,'MCP28V',data={x:swe_hsk.time, y:(swe_hsk.MCP28V-vnorm[0])}
    store_data,'NR28V' ,data={x:swe_hsk.time, y:(swe_hsk.NR28V-vnorm[0])}
    store_data,'DIGT'  ,data={x:swe_hsk.time, y:swe_hsk.DIGT}
    store_data,'P2P5DV',data={x:swe_hsk.time, y:(swe_hsk.P2P5DV-vnorm[4])}
    store_data,'P5DV'  ,data={x:swe_hsk.time, y:(swe_hsk.P5DV-vnorm[2])}
    store_data,'P3P3DV',data={x:swe_hsk.time, y:(swe_hsk.P3P3DV-vnorm[3])}
    store_data,'P5AV'  ,data={x:swe_hsk.time, y:(swe_hsk.P5AV-vnorm[2])}
    store_data,'N5AV'  ,data={x:swe_hsk.time, y:(swe_hsk.N5AV+vnorm[2])}
    store_data,'P28V'  ,data={x:swe_hsk.time, y:(swe_hsk.P28V-vnorm[0])}
    store_data,'TV_frame',data={x:[0D], y:replicate(-100.,1,7), v:findgen(7)} 
    if (vflg) then begin
      options,'P28V',  'color',TCcol[0]   ; magenta
      options,'P12V',  'color',TCcol[1]   ; blue
      options,'N12V',  'color',TCcol[2]   ; cyan
      options,'P5AV',  'color',TCcol[3]   ; green
      options,'N5AV',  'color',TCcol[4]   ; yellow
      options,'P5DV',  'color',TCcol[5]   ; orange
      options,'P3P3DV','color',TCcol[6]   ; red

      store_data,'VoltsC',data=['TV_frame','P28V','P12V','N12V', $
                                'P5AV','N5AV','P5DV','P3P3DV']  ; skipping P2P5DV
      
      ylim,'VoltsC',-5,5,0
      options,'VoltsC','ytitle','Volts'
      options,'VoltsC','yticks',2
      options,'VoltsC','yminor',5
      options,'VoltsC','labflag',1
      options,'VoltsC','labels',['+28 V','+12 V','-12 V','+5 V','-5 V','5 DV','3.3 DV']
      vpans = ['VoltsC']
    endif else begin
      options,'P12V',  'color',TCcol[0]   ; magenta
      options,'N12V',  'color',TCcol[1]   ; blue
      options,'MCP28V','color',TCcol[2]   ; cyan
      options,'NR28V', 'color',TCcol[3]   ; green
      options,'P28V',  'color',TCcol[4]   ; yellow
      options,'P2P2DV','color',TCcol[0]   ; magenta
      
      options,'P3P3DV','color',TCcol[1]   ; blue
      options,'P5DV',  'color',TCcol[2]   ; cyan
      options,'P5AV',  'color',TCcol[3]   ; green
      options,'N5AV',  'color',TCcol[4]   ; yellow
      options,'NRV',   'color',TCcol[5]   ; orange
  
      store_data,'VoltsA',data=['TV_frame','P12V','N12V','MCP28V','NR28V','P28V']
      store_data,'VoltsB',data=['TV_frame','P2P5DV','P3P3DV','P5DV','P5AV','N5AV','NRV']

      ylim,'VoltsA',-15,35,0
      options,'VoltsA','yminor',5
      options,'VoltsA','labflag',1
      options,'VoltsA','labels',['+12 V','-12 V','MCP 28V','NR 28V','+28 V','','']
      ylim,'VoltsB',-6,6,0
      options,'VoltsB','yticks',2
      options,'VoltsB','yminor',6
      options,'VoltsB','labflag',1
      options,'VoltsB','labels',['+2.5 DV','+3.3 DV','+5 DV','+5 V','-5 V','NRV','']
      vpans = ['VoltsA','VoltsB']
    endelse

    store_data,'Temps',data=['TV_frame','LVPST','ANALT','DIGT']
    options,'ANALT','color',TCcol[0]  ; magenta
    options,'DIGT', 'color',TCcol[1]  ; blue
    options,'LVPST','color',TCcol[2]  ; cyan
;    ylim,'Temps',20,35,0
    ylim,'Temps',0,0,0
;    options,'Temps','yticks',3
;    options,'Temps','yminor',5
    options,'Temps','labflag',1
    options,'Temps','labels',['ANALT','DIGT','LVPST','','','','']
  
    store_data,'HSKREG',data={x:swe_hsk.time, y:transpose(swe_hsk.HSKREG), v:indgen(16)}
    options,'HSKREG','spec',1
    ylim,'HSKREG',0,12,0
    zlim,'HSKREG',0,1,0
    options,'HSKREG','yticks',3
    options,'HSKREG','yminor',4
    options,'HSKREG','ytitle','Dig HSK'
    options,'HSKREG','x_no_interp',1
    options,'HSKREG','y_no_interp',1
    options,'HSKREG','no_color_scale',1
    options,'HSKREG','panel_size',0.5

    dchsk = swe_hsk.npkt - shift(swe_hsk.npkt,1)
    dthsk = swe_hsk.time - shift(swe_hsk.time,1)
    store_data,'dchsk',data={x:swe_hsk[1:*].time, y:dchsk[1:*]}
    store_data,'dthsk',data={x:swe_hsk[1:*].time, y:dthsk[1:*]}
    options,'dchsk','ytitle','dN (28)'
    options,'dchsk','psym',5
    options,'dthsk','ytitle','dT (28)'
    options,'dthsk','psym',5
    options,'dthsk','ynozero',1
    options,'dchsk','color',TCcol[0]
    options,'dthsk','color',TCcol[0]

    dCmax = dCmax > max(dchsk,/nan)
    dTmax = dTmax > max(dthsk,/nan)

    pans = ['HSKREG',vpans,'Temps']
    pdC = [pdC,'dchsk']
    pdT = [pdT,'dthsk']
    TClab[0] = '28'

  if (rflg) then swe_hsk = hsk_save

  return

end
