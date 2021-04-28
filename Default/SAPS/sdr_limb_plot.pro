; Copyright 2006 The Johns Hopkins University/Applied Physics Laboratory.
; All rights reserved.
;+
; NAME:
;  SDRplotutilities
;
; PURPOSE:
;   Plotting procedures for the SDR Data, both Disk an Limb. The
;   Data is by default plotted on a Cylindrical Earth projection,
;   although there is an option for plotting an orthographic
;   projection. Various options exist to see different UV colors,
;   where the SAA is, Unrectified (as seen by SSUSI), with GAIM
;   sized pixels, etc.
;
; CATEGORY:
;   Utility.
;
; CALLING SEQUENCE:
;    -for disk plots:
;       sdr_disk_plot, SDRFILE=sdrfile, DAYNIGHT=daynight[, /NOINIT]
;       [, RMAX = rmax] [. COLR = color] [, /UNRECT][, /GAIM][, /ORTHO][, /NOSAT]
;       [, TITLE=title][, /SAA]
;
;    -for limb plots
;       sdr_limb_plot, SDRFILE=sdrfile [, /NOINIT] [, RMAX = rmax] [. COLR = color]
;       [, /UNRECT][, /GAIM][, /ORTHO][, /NOSAT] [, TITLE=title][, /SAA]
;
; DEFAULT VALUES:
;      plots:
;      - 1356 angstrom radiance,
;      - rectified for slant path (disk only)
;      - cylindrical projection plot,
;      - maximum radiance of 10000 Rayleighs
;      - title is the filename
;      - sdr (not coarser GAIM resolution) pixels
;
;
; INPUTS
;   sdr_disk_plot:
;     Required inputs
;     - SDRFILE = full path SDR disk file
;     - DAYNIGHT = 'day' or 'night' to select geolocation altitude
;     Optional inputs
;     - NOINIT - flag used for overplotting SDR data on an existing map plot
;     - RMAX   - maximum value to plot (e.g., in Rayleights to scale a UV image)
;     - COLR   - index of the UV band to plot, 1=1216, 2=1304, 3=1356,4=LBHS, 5=LBHL
;     - UNRECT - use unrectified radiance data in plot
;     - GAIM   - plot low resolution GAIM grid
;     - ORTHO  - use orthographic, not cylindrical map projection
;     - NOSAT  - do not plot satellite track
;     - TITLE  - use string in TITLE for plot title
;     - SAA    - plot red X over pixels contaminated by SAA (if any)
;
;   sdr_limb_plot:
;     Required inputs
;     - SDRFILE = full path SDR disk file
;     Optional inputs
;     - NOINIT - flag used for overplotting SDR data on an existing map plot
;     - RMAX   - maximum value to plot (e.g., in Rayleights to scale a UV image)
;     - COLR   - index of the UV band to plot, 1=1216, 2=1304, 3=1356,4=LBHS, 5=LBHL
;     - GAIM   - plot low resolution GAIM grid
;     - ORTHO  - use orthographic, not cylindrical map projection
;     - NOSAT  - do not plot satellite track
;     - TITLE  - use string in TITLE for plot title
;     - SAA    - plot red X over pixels contaminated by SAA (if any)

;
; COMMON BLOCKS:
; None
;
; PROCEDURE:
;
;
; MODIFICATION HISTORY:
;       RKS  2006-12-15 Moved plots from generateSDR_unix.pro.
;       RKS  2007-09-28 Added many options for plotting from disk by
;                        color, projections, etc.
;       RKS  2008-3-24  Added GAIM and SAA plotting options for SDR
;                        limb plots
;       RKS  2008-4-25  Plots SAA locations for day grid now as well
;                        as well as night.
;       RKS  2014-6-18  Made as standalone tool for plotting
; CVS
;   $Header: /cvsroot/ssusi/ssusi_gdas/sdr/idlplotutils.pro,v 1.22 2010/10/26 09:24:23 schaerk1 Exp $
;   $Name:  $
;+
;-------------------------------------------
PRO sdr_disk_plot, SDRFILE=sdrfile, DAYNIGHT=daynight, NOINIT = noinit, $
  RMAX=rmax, COLR=colr, UNRECT=unrect, GAIM=gaim, ORTHO=ortho, NOSAT=nosat, $
  TITLE=title, SAA=saa_flag
  ;  inputs:
  ;     - SDRFILE = full path SDR disk file
  ;     - DAYNIGHT = 'day' or 'night' to select geolocation altitude
  ;     - NOINIT  - flag used for overplotting multiple SDR files
  ;     - RMAX    - maximum radiance (in Rayleights) to scale the image color
  ;     - COLR - index of the bandwidthe to plot, 1=1216, 2=1304, 3=1356, etc.
  ;     - UNRECT - use unrectified radiance data in plot
  ;     - GAIM   - plot low resolution GAIM grid
  ;     - ORTHO  - use orthographic, not cylindrical map projection
  ;     - NOSAT  - do not plot satellite track
  ;     - TITLE  - use string in TITLE for plot title
  ;     - SAA    - plot red X over pixels contaminated by SAA

  UVcolor = ['1216','1304','1356','LBHS','LBHL']
  read_ncdf,sdr,file=sdrfile

  ; in case you opened the file with the dialog box.
  sdrfile = sdr.attributes.global.filename
  ; flag to indicate if sdr2 (or sdr)
  sdr2 = 0
  arr=strsplit(sdrfile,'SDR',/extract, /regex)

  ; do we have an SDR-DISK or SDR2-DISK file?
  arr2 = strsplit(arr[1],'-',/extract,/regex)
  if(arr2[0] eq '2') then sdr2=1


  if(not keyword_set(title)) then title = sdr.attributes.global.filename
  if(not keyword_set(RMAX)) then rmax = 10000.
  if(keyword_set(GAIM) and sdr2 eq 0) then begin
    print, 'No GAIM pixels in SDR-DISK files - you need to plot SDR2-DISK files for GAIM pixels'
    return
  endif

  if(keyword_set(noinit)) then begin
    print, 'will not initialize plot'
  endif else begin
    device, decompose=0
    loadct, 39
    if (keyword_set(ortho)) then map_set, 10., 330., /ORTHOGRAPHIC, /GRID, /CONT, $
      title=title else map_set, /cont, /grid, title= title
  endelse


  if(not keyword_set(daynight)) then daynight = 'day'
  if(keyword_set(colr)) then c_ind = colr-1 else c_ind = 2

  if(c_ind < 0 or c_ind >4) then begin
    print, 'ERROR: idl_plot_utils::sdr_disk_plot - Color must be between 1 and 5, not ', colr
    return
  endif

  if(keyword_set(ortho)) then begin
    ;       use rectangles
    sqx = 2.5 *(0.3 + sdr2 *0.7)
    sqy =  1.3 *(0.3 + sdr2*0.7)
    if(keyword_set(gaim)) then begin
      sqx = 3. * sqx
      sqy = 3. * sqy
    endif
    usersym, [-sqx,sqx,sqx,-sqx,-sqx],[sqy,sqy,-sqy,-sqy,sqy],/fill
  endif else begin
    dx = 1.0
    dy = 1.0
    if(keyword_set(gaim)) then begin
      dx = 2.
      dy = 2.
    endif
    usersym, [-dx,0,dx,0,-dx],[0,dy,0,-dy,0],/fill
  endelse


  ;  We choose variables grided for night or day - auroral zone
  ;  plotting is similar with the piercepoint_auroral_day coordinates
  ;
  if(STRLOWCASE(daynight) eq 'day') THEN BEGIN
    min_sza=0.
    max_sza=90.
    ; The Global Assimilation of
    ; Ionospheric Measurements or GAIM
    ; model uses a very coarse grid which
    ; is not very good for visualizing the
    ; ionosphere, but this utility can
    ; plot the data to check the SSUSI input

    if(keyword_set(gaim)) then begin

      longitude = sdr.PIERCEPOINT_GAIM_DAY_LONGITUDE
      latitude  = sdr.PIERCEPOINT_GAIM_DAY_LATITUDE
      saa       = sdr.in_saa_GAIM_day
      sza       = sdr.piercepoint_GAIM_day_SZA
      if(keyword_set(unrect)) then plotvar = sdr.disk_intensity_gaim_day[c_ind,*,*] $
      else Plotvar = sdr.disk_rectified_intensity_gaim_day[c_ind,*,*]
      if(keyword_set(counts)) then plotvar = sdr.diskcountsdata_GAIM_DAY[c_ind,*,*]
    endif else begin
      longitude = sdr.PIERCEPOINT_DAY_LONGITUDE
      latitude  = sdr.PIERCEPOINT_DAY_LATITUDE
      saa       = sdr.in_saa_day
      sza       = sdr.piercepoint_day_SZA
      if(keyword_set(unrect)) then plotvar = sdr.disk_intensity_day[c_ind,*,*] $
      else plotvar = sdr.disk_rectified_intensity_day[c_ind,*,*]
      if(keyword_set(counts)) then plotvar = sdr.diskcountsdata_DAY[c_ind,*,*]
    endelse
  endif

  if(STRLOWCASE(daynight) eq 'night') THEN BEGIN
    min_sza=100.
    max_sza=180.

    if(keyword_set(gaim)) then begin

      longitude = sdr.PIERCEPOINT_GAIM_NIGHT_LONGITUDE
      latitude  = sdr.PIERCEPOINT_GAIM_NIGHT_LATITUDE
      saa       = sdr.in_saa_GAIM_night
      sza       = sdr.piercepoint_GAIM_night_SZA
      if(keyword_set(unrect)) then plotvar = sdr.disk_intensity_gaim_night[c_ind,*,*] $
      else plotvar = sdr.disk_rectified_intensity_gaim_night[c_ind,*,*]
      if(keyword_set(counts)) then plotvar = sdr.diskcountsdata_GAIM_night[c_ind,*,*]
    endif else begin
      longitude = sdr.PIERCEPOINT_NIGHT_LONGITUDE
      latitude  = sdr.PIERCEPOINT_NIGHT_LATITUDE
      saa       = sdr.in_saa_night
      sza       = sdr.piercepoint_night_SZA
      if(keyword_set(unrect)) then plotvar = sdr.disk_intensity_night[c_ind,*,*] $
      else plotvar = sdr.disk_rectified_intensity_night[c_ind,*,*]
      if(keyword_set(counts)) then plotvar = sdr.diskcountsdata_night[c_ind,*,*]
    endelse
  endif


  nx = n_elements(longitude[*,0])
  ny = n_elements(LONGITUDE[0,*])

  plotvar = reform(plotvar,nx,ny)

  lon_mid = LONGITUDE[nx/2,2*ny/3]
  lat_mid = LATITUDE[nx/2,2*ny/3]

  ; if (keyword_set(ortho)) then map_set, lat_mid, lon_mid, /ORTHOGRAPHIC, /GRID, /CONT, $
  ;        title=title

  ; filter out NaNs
  ii = where(finite(plotvar) and sza gt min_sza and sza lt max_sza,dcnt)

  if(dcnt eq 0) then begin
    print, 'No data in the sza range ',min_sza,'< SZA < ',max_sza,', appropriate for ',daynight
    print,' quitting.'
    return
  endif

  ; plot radiance
  plots,LONGITUDE[ii],LATITUDE[ii],color=scale2(ALOG10(plotvar[ii]),VMIN=0.,VMAX=ALOG10(rmax)),psym=8

  cbartitle = 'LOG('+UVcolor[c_ind]+' RADIANCE in Rayleighs)'
  if(not keyword_set(noinit)) then colorbar_mod, MAXRANGE=Alog10(rmax), $
    title = cbartitle, position=[0.30, 0.07, 0.70, 0.12], /horizontal, $
    /bottom, divisions = fix(4), FORMAT='(F3.1)'

  if( keyword_set(saa_flag)) then begin

    ; for plotting the SAA
    jj = where(saa eq 1)

    if( jj[0] ne -1) then $
      oplot,LONGITUDE[jj],LATITUDE[jj], color = 240, psym=7

  endif


  if(not keyword_set(nosat)) then $
    oplot, sdr.longitude_day, sdr.latitude_day


  return
end

PRO sdr_limb_plot, SDRFILE=sdrfile, NOINIT = noinit, RMAX=rmax, $
  COLR=colr, GAIM=gaim, ORTHO=ortho, NOSAT=nosat, TITLE=title, SAA=saa
  ;  inputs:
  ;     - SDRFILE = full path SDR disk file
  ;     - NOINIT  - flag used for overplotting multiple SDR files
  ;     - RMAX    - maximum radiance (in Rayleights) to scale the image color
  ;     - COLR - index of the bandwidthe to plot, 1=1216, 2=1304, 3=1356, etc.
  ;     - GAIM   - plot low resolution GAIM grid
  ;     - ORTHO  - use orthographic, not cylindrical map projection
  ;     - NOSAT  - do not plot satellite track
  ;     - TITLE  - use string in TITLE for plot title
  ;     - SAA    - plot red X over pixels contaminated by SAA

  read_ncdf,sdr,file=sdrfile

  ; in case you opened the file with the dialog box.
  sdrfile = sdr.attributes.global.filename

  UVcolor = ['1216','1304','1356','LBHS','LBHL']
  ; what UV color are we plotting?
  if(keyword_set(colr)) then c_ind = colr-1 else c_ind = 2
  if(c_ind < 0 or c_ind >4) then begin
    print, 'ERROR: idl_plot_utils::sdr_disk_plot - Color must be between 1 and 5, not ', colr
    return
  endif

  if(not keyword_set(title)) then title = sdrfile

  if(keyword_set(noinit)) then begin
    print, 'will not initialize plot'
  endif else begin
    device, decompose=0
    loadct, 39
    if (keyword_set(ortho)) then map_set, 10., 330., /ORTHOGRAPHIC, /GRID, /CONT, $
      title=title else map_set, /cont, /grid, title= title
  endelse

  if(not keyword_set(RMAX)) then rmax = 10000.

  usersym, [-1,0,1,0,-1],[0,1,0,-1,0],/fill


  ;     map_set, lat_mid, lon_mid, /ORTHOGRAPHIC, /GRID, /CONT, $
  ;          title=sdr.attributes.global.filename
  if(keyword_set(gaim)) then begin
    nx = n_elements(sdr.TANGENTPOINT_LONGITUDE_GAIM[*,0])
    ny = n_elements(sdr.TANGENTPOINT_LONGITUDE_GAIM[0,*])
    lon_mid = sdr.TANGENTPOINT_LONGITUDE_GAIM[nx/2,2*ny/3]
    lat_mid = sdr.TANGENTPOINT_LATITUDE_GAIM[nx/2,2*ny/3]
    plotvar = reform(sdr.limb_intensity_gaim[c_ind,*,*],nx,ny)
    LONGITUDE = sdr.TANGENTPOINT_LONGITUDE_GAIM
    LATITUDE  = sdr.TANGENTPOINT_LATITUDE_GAIM
    SZA       = sdr.TANGENTPOINT_SZA_GAIM
  endif else begin
    nx = n_elements(sdr.TANGENTPOINT_LONGITUDE[*,0])
    ny = n_elements(sdr.TANGENTPOINT_LONGITUDE[0,*])
    lon_mid = sdr.TANGENTPOINT_LONGITUDE[nx/2,2*ny/3]
    lat_mid = sdr.TANGENTPOINT_LATITUDE[nx/2,2*ny/3]
    plotvar = reform(sdr.limb_intensity[c_ind,*,*],nx,ny)
    LONGITUDE = sdr.TANGENTPOINT_LONGITUDE
    LATITUDE  = sdr.TANGENTPOINT_LATITUDE
    SZA       = sdr.TANGENTPOINT_SZA
  endelse

  ;ii = where(plotvar gt -10000.)
  ii = where(finite(plotvar) and sza gt 100.)
  ; plot 1356 radiance
  plots,LONGITUDE[ii],LATITUDE[ii],color=scale2(plotvar[ii],VMIN=0.,VMAX=rmax),psym=8
  if( not keyword_set(nosat)) then $
    oplot, sdr.longitude, sdr.latitude

  cbartitle = 'LOG('+UVcolor[c_ind]+' RADIANCE in Rayleighs)'
  if(not keyword_set(noinit)) then colorbar_mod, MAXRANGE=Alog10(rmax), $
    title = cbartitle, position=[0.30, 0.07, 0.70, 0.12], /horizontal, $
    /bottom, divisions = fix(4), FORMAT='(F3.1)'


  ; to plot the locations of SAA contaminated pixels
  if(keyword_set(saa)) then begin
    ; for plotting the SAA
    if(keyword_set(gaim)) then $
      jj = where(sdr.in_saa_gaim eq 1) $
    else $
      jj = where(sdr.in_saa eq 1)

    if( jj[0] ne -1) then $
      oplot,LONGITUDE[jj],LATITUDE[jj], color = 240, psym=7

  endif


  return
end
