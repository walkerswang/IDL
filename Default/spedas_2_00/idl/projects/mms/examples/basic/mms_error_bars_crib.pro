;+
;
; This script shows how to create a plot of FPI density with error bars
;
; $LastChangedBy: egrimes $
; $LastChangedDate: 2017-01-05 18:48:53 -0800 (Thu, 05 Jan 2017) $
; $LastChangedRevision: 22518 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/mms/examples/basic/mms_error_bars_crib.pro $
;-
mms_load_fpi, probe=3, trange=['2015-10-16', '2015-10-17'], datatype='des-moms'

;get the data and errors
get_data, 'mms3_des_numberdensity_err_fast', data=errors
get_data, 'mms3_des_numberdensity_fast', data=data

; store the data/errors in a new tplot variable
new_data = {x: data.x, y: data.Y, dy: errors.Y*data.Y}
store_data, 'mms3_des_numberdensity_fast_with_errs', data=new_data

; set the ylimits to reduce whitespace on the plot (so the error bars are clearly visible in this example)
ylim, 'mms3_des_numberdensity_fast_with_errs', 8, 12, 0

tplot, 'mms3_des_numberdensity_fast_with_errs'
tlimit, '2015-10-16/13:06', '2015-10-16/13:06:50'

end
