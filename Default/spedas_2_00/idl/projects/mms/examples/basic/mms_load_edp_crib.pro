;+
; MMS EDP crib sheet
; 
; do you have suggestions for this crib sheet?  
;   please send them to egrimes@igpp.ucla.edu
; 
; $LastChangedBy: egrimes $
; $LastChangedDate: 2016-07-18 08:37:51 -0700 (Mon, 18 Jul 2016) $
; $LastChangedRevision: 21476 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/mms/examples/basic/mms_load_edp_crib.pro $
;-
timespan, '2015-10-16/13:00:00', 10, /minutes
mms_load_edp, data_rate='fast', probes=[1, 2, 3, 4], datatype='dce', level='l2'

; Display colors for parallel E (black) and error (pink)
; Large error bars signifies possible presence of cold plasma
; or spacecraft charging, which can make axial electric field
; measurements difficult. Please always use error bars on e-parallel!!
options, 'mms?_edp_dce_par_epar_fast_l2', colors = [1, 0]
options, 'mms?_edp_dce_par_epar_fast_l2', labels = ['Error', 'E!D||!N']

; Since the electric field is often close to zero in multiple components, label spacing tends to get bunched
; together
options, '*', 'labflag', -1

tplot, ['mms?_edp_dce_dsl_fast_l2', 'mms?_edp_dce_par_epar_fast_l2']


end