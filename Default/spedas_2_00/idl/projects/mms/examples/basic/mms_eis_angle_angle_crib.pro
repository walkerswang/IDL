;+
; MMS EIS Polar-Azimuthal Plots
; 
; this crib sheet shows how to create angle-angle (polar versus azimuthal) 
; plots for EIS data with overlaid pitch angle contours
;
; do you have suggestions for this crib sheet?
;   please send them to egrimes@igpp.ucla.edu
;
; $LastChangedBy: egrimes $
; $LastChangedDate: 2016-09-20 14:02:38 -0700 (Tue, 20 Sep 2016) $
; $LastChangedRevision: 21887 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/mms/examples/basic/mms_eis_angle_angle_crib.pro $
;-

trange = ['2015-10-16', '2015-10-17']

; create the angle-angle plot for level 2 ExTOF data:
eis_ang_ang, trange=trange, level='l2', probe=3, datatype='extof'
stop

; the following shows how to choose a different species (oxygen)
eis_ang_ang, trange=trange, level='l2', probe=3, datatype='extof', species='oxygen'
stop

; create the angle-angle plot for level 2 PHxTOF data:
eis_ang_ang, trange=trange, level='l2', probe=3, datatype='phxtof'
stop

; same example as above, but for burst mode data
eis_ang_ang, trange=trange, level='l2', probe=3, datatype='phxtof', data_rate='brst'
stop

; the following shows how to choose different energy channels to plot:
eis_ang_ang, trange=trange, level='l2', probe=3, datatype='phxtof', energy_chan=[1, 2, 3, 4, 5]
stop

; save the plot to a PNG (saved to working directory as eis_angle_angle_plot.png)
eis_ang_ang, trange=trange, level='l2', probe=3, datatype='extof', png='eis_angle_angle_plot'
stop

; save the plot to a postscript file (saved to working directory as eis_angle_angle_plot.ps)
eis_ang_ang, trange=trange, level='l2', probe=3, datatype='extof', /i_print, p_filename='eis_angle_angle_plot'


end
