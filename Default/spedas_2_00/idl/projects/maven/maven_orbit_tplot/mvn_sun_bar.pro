;+
;PROCEDURE:   mvn_sun_bar
;PURPOSE:
;  Creates a colored bar indicating sun/shadow.  Assumes that
;  you have run maven_orbit_tplot first.
;
;USAGE:
;  mvn_sun_bar
;
;INPUTS:
;
;KEYWORDS:
;
; $LastChangedBy: dmitchell $
; $LastChangedDate: 2017-02-02 10:08:35 -0800 (Thu, 02 Feb 2017) $
; $LastChangedRevision: 22717 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/maven/maven_orbit_tplot/mvn_sun_bar.pro $
;
;CREATED BY:    David L. Mitchell
;-
pro mvn_sun_bar, pans=bname

  bname = 'mvn_sun_bar'
  
  get_data,'wake',data=wake,index=i
  if (i eq 0) then begin
    print,'You must run maven_orbit_tplot first.'
    return
  endif

  y = replicate(1.,n_elements(wake.x),2)
  indx = where(finite(wake.y), count)
  if (count gt 0L) then y[indx,*] = 0.

  store_data,bname,data={x:wake.x, y:y, v:[0,1]}
  ylim,bname,0,1,0
  zlim,bname,0,3,0
  options,bname,'spec',1
  options,bname,'panel_size',0.05
  options,bname,'ytitle',''
  options,bname,'yticks',1
  options,bname,'yminor',1
  options,bname,'no_interp',1
  options,bname,'xstyle',4
  options,bname,'ystyle',4
  options,bname,'no_color_scale',1
  
  return

end
