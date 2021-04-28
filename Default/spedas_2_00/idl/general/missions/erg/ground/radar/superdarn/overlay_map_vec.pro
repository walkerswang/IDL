;+
; PRO overlay_map_vec
;
; :Description:
;    Draw lines from a start point [lat0,lon0] with a direction [dlat,dlon] and arclength 
;    in degree on the plot window set up by sd_map_set. 
;
; :Params:
; lat0: latitude of the start point [deg]
; lon0: longitude of the start point [deg]
; dlat: the latitudinal component of the vector to be drawn (positive: north)
; dlon: the longitudinal component of the vector to be drawn (positive: east)
; arclength: length of the vector in degree with which the vector is drawn. 
;            dlat and dlon are normalized by this value. Thus the absolute 
;            values of dlat and dlon are ignored. Only the ratio is concerned.  
; 
; :Keywords:
; linethick: Set a value of line thickness 
; color:  Set a value of color table with which the lines are drawn 
; 
; :Examples:
; ex)   overlay_map_vec, 65., 270., 1.,-3, 18., linethick=1.5
; 
; :History:
; 2013/10/02: Initial release
;
; :Author:
;   Tomo Hori (E-mail: horit at isee.nagoya-u.ac.jp)
;
; $LastChangedBy: c0004hori $
; $LastChangedDate: 2017-09-28 11:34:35 +0900 (Thu, 28 Sep 2017) $
; $LastChangedRevision: 477 $
; $URL: https://ergsc-local.isee.nagoya-u.ac.jp/svn/ergsc/trunk/erg/ground/radar/superdarn/overlay_map_vec.pro $
;-

;-
PRO overlay_map_vec, lat0, lon0, dlat, dlon, arclength, $
  linethick=linethick, color=color, $
  psym=psym, symsize=symsize, $
  nooriginpoint=nooriginpoint
  
  ;Check the arguments
  npar = n_params()
  if npar ne 5 then return 
  
  if ~keyword_set(psym) then psym = 4 
  if ~keyword_set(symsize) then symsize = 0.8 
  
  ;Calculate the end point of a vector with given arclength
  the0 = 90. - lat0 & dthe = (-1.)*dlat
  get_end_point_in_sph, the0,lon0,dthe,dlon,arclength, $
    the1,phi1
  lat1 = 90.-the1 & lon1 = phi1
  
  
  ;Plot!
  if ~keyword_set(nooriginpoint) then plots, lon0,lat0, psym=psym, symsize=symsize, color=color
  plots, [lon0,lon1], [lat0,lat1], thick=linethick, color=color





  return

end
