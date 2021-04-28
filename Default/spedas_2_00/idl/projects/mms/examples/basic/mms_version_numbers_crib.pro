;+
; MMS CDF version #s crib sheet
;
; do you have suggestions for this crib sheet?
;   please send them to egrimes@igpp.ucla.edu
;
; $LastChangedBy: egrimes $
; $LastChangedDate: 2016-09-20 14:21:38 -0700 (Tue, 20 Sep 2016) $
; $LastChangedRevision: 21889 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/mms/examples/basic/mms_version_numbers_crib.pro $
;-

timespan, '2015-10-16', 1, /day

; load some data for various instruments
mms_load_fgm, probe=1, versions=fgm_versions
mms_load_fpi, probe=1, versions=fpi_versions, datatype='des-moms'
mms_load_eis, probe=1, versions=eis_versions
mms_load_feeps, probe=1, versions=feeps_versions

; plot some useful stuff
tplot, ['mms1_fgm_b_gsm_srvy_l2_bvec', $
        'mms1_des_energyspectr_par_fast', $
        'mms1_des_energyspectr_perp_fast', $
        'mms1_des_energyspectr_anti_fast', $
        'mms1_epd_feeps_srvy_l2_electron_intensity_omni_spin', $
        'mms1_epd_eis_extof_proton_flux_omni']

; add the version #s to the plot
mms_add_cdf_versions, 'eis', eis_versions
mms_add_cdf_versions, 'fpi', fpi_versions, data_rate='fast' ; note usage of data_rate keyword
mms_add_cdf_versions, 'fgm', fgm_versions
mms_add_cdf_versions, 'feeps', feeps_versions

end