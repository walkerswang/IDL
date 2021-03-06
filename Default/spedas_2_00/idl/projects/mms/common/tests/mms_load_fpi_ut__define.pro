;+
;
; Unit tests for mms_load_fpi
;
; Requires both the SPEDAS QA folder (not distributed with SPEDAS) and mgunit
; in the local path
;
; warning: ACR tests in test_integration_time_get_dist require special, non-public CDFs
; to work / expect this test to fail if you don't have those files
; 
; 
; $LastChangedBy: egrimes $
; $LastChangedDate: 2017-06-02 13:41:25 -0700 (Fri, 02 Jun 2017) $
; $LastChangedRevision: 23393 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/mms/common/tests/mms_load_fpi_ut__define.pro $
;-

; regression tests ---------->
; same as below, except using mms_get_dist
function mms_load_fpi_ut::test_integration_time_get_dist
  mms_load_fpi, trange=['2015-10-16/13:00', '2015-10-16/13:10'], datatype=['dis-dist', 'des-dist'], data_rate='brst'
  mms_load_fpi, trange=['2016-12-09', '2016-12-10'], datatype=['dis-dist', 'des-dist'], data_rate='brst', level='acr', suffix='acr', probe=1
  mms_load_fpi, trange=['2015-10-16/13:00', '2015-10-16/13:10'], datatype=['dis-dist', 'des-dist'], data_rate='fast'
  fpi_dist = mms_get_dist('mms1_dis_dist_brstacr',trange=time_double(['2016-12-09', '2016-12-10']), probe = 1, species = 'i')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-0.0375) lt 0.001, 'Problem with integration time returned by mms_get_dist (ions, brst, ACR)'
  fpi_dist = mms_get_dist('mms1_des_dist_brstacr',trange=time_double(['2016-12-09', '2016-12-10']), probe = 1, species = 'e')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-0.0075) lt 0.001, 'Problem with integration time returned by mms_get_dist (electrons, brst, ACR)'
  
  ; with the level keyword
  fpi_dist = mms_get_dist('mms1_dis_dist_brstacr',trange=time_double(['2016-12-09', '2016-12-10']), probe = 1, species = 'i', level='acr', data_rate='brst')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-0.0375) lt 0.001, 'Problem with integration time returned by mms_get_dist (ions, brst, level=ACR)'
  fpi_dist = mms_get_dist('mms1_des_dist_brstacr',trange=time_double(['2016-12-09', '2016-12-10']), probe = 1, species = 'e', level='acr', data_rate='brst')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-0.0075) lt 0.001, 'Problem with integration time returned by mms_get_dist (electrons, brst, level=ACR)'

  
  fpi_dist = mms_get_dist('mms3_dis_dist_brst',trange=time_double(['2015-10-16/13:00', '2015-10-16/13:10']), probe = 3, species = 'i')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-0.15) lt 0.001, 'Problem with integration time returned by mms_get_dist (ions, brst)'
  fpi_dist = mms_get_dist('mms3_des_dist_brst',trange=time_double(['2015-10-16/13:00', '2015-10-16/13:10']), probe = 3, species = 'e')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-0.03) lt 0.001, 'Problem with integration time returned by mms_get_dist (electrons, brst)'
  fpi_dist = mms_get_dist('mms3_dis_dist_fast',trange=time_double(['2015-10-16/13:00', '2015-10-16/13:10']), probe = 3, species = 'i')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-4.5) lt 0.001, 'Problem with integration time returned by mms_get_dist (ions, fast)'
  fpi_dist = mms_get_dist('mms3_des_dist_fast',trange=time_double(['2015-10-16/13:00', '2015-10-16/13:10']), probe = 3, species = 'e')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-4.5) lt 0.001, 'Problem with integration time returned by mms_get_dist (electrons, fast)'
  return, 1
end

function mms_load_fpi_ut::test_integration_time_get_i_dist_brst
  mms_load_fpi, trange=['2015-10-16/13:00', '2015-10-16/13:10'], datatype='dis-dist', data_rate='brst'
  fpi_dist = mms_get_fpi_dist('mms3_dis_dist_brst',trange=time_double(['2015-10-16/13:00', '2015-10-16/13:10']), probe = 3, species = 'i')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-0.15) lt 0.001, 'Problem with integration time returned by mms_get_fpi_dist'
  return, 1
end

function mms_load_fpi_ut::test_integration_time_get_e_dist_brst
  mms_load_fpi, trange=['2015-10-16/13:00', '2015-10-16/13:10'], datatype='des-dist', data_rate='brst'
  fpi_dist = mms_get_fpi_dist('mms3_des_dist_brst',trange=time_double(['2015-10-16/13:00', '2015-10-16/13:10']), probe = 3, species = 'e')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-0.03) lt 0.001, 'Problem with integration time returned by mms_get_fpi_dist'
  return, 1
end

function mms_load_fpi_ut::test_integration_time_get_i_dist_fast
  mms_load_fpi, trange=['2015-10-16/13:00', '2015-10-16/13:10'], datatype='dis-dist', data_rate='fast'
  fpi_dist = mms_get_fpi_dist('mms3_dis_dist_fast',trange=time_double(['2015-10-16/13:00', '2015-10-16/13:10']), probe = 3, species = 'i')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-4.5) lt 0.001, 'Problem with integration time returned by mms_get_fpi_dist'
  return, 1
end

function mms_load_fpi_ut::test_integration_time_get_e_dist_fast
  mms_load_fpi, trange=['2015-10-16/13:00', '2015-10-16/13:10'], datatype='des-dist', data_rate='fast'
  fpi_dist = mms_get_fpi_dist('mms3_des_dist_fast',trange=time_double(['2015-10-16/13:00', '2015-10-16/13:10']), probe = 3, species = 'e')
  assert, abs(((*fpi_dist)[0].end_time-(*fpi_dist)[0].time)-4.5) lt 0.001, 'Problem with integration time returned by mms_get_fpi_dist'
  return, 1
end

; tplotnames regressions
function mms_load_fpi_ut::test_loading_tplotnames_desmoms
  mms_load_fpi, trange=['2015-10-15', '2015-10-16'], datatype='des-moms', tplotnames = tplotnames
  assert, n_elements(tplotnames) eq 39, '(potential) Problem with number of tplotnames returned from mms_load_fpi'
  return, 1
end

function mms_load_fpi_ut::test_loading_tplotnames_desdist
  mms_load_fpi, trange=['2015-10-15', '2015-10-16'], datatype='des-dist', tplotnames = tplotnames
  assert, n_elements(tplotnames) eq 12, '(potential) Problem with number of tplotnames returned from mms_load_fpi'
  return, 1
end

function mms_load_fpi_ut::test_loading_tplotnames_des
  mms_load_fpi, trange=['2015-10-15', '2015-10-16'], datatype=['des-moms', 'des-dist'], tplotnames = tplotnames
  assert, n_elements(tplotnames) eq 49, '(potential) Problem with number of tplotnames returned from mms_load_fpi'
  return, 1
end

; user requests a few seconds after file start time
function mms_load_fpi_ut::test_seconds_after_file_start_spdf
  mms_load_fpi, trange=['2015-10-15/6:45:21', '2015-10-15/6:51:21'], data_rate='brst', level='l2', /spdf
  assert, spd_data_exists('mms3_dis_bulkv_dbcs_brst','2015-10-15/06:47:23','2015-10-15/06:54:59'), $
    'Error! Not grabbing the correct data from the SPDF??? (1)'
  return, 1
end

; user requests a few seconds after file end time
function mms_load_fpi_ut::test_seconds_after_file_end_spdf
  mms_load_fpi, trange=['2015-10-15/6:49:21', '2015-10-15/6:54:01'], data_rate='brst', level='l2', /spdf
  assert, spd_data_exists('mms3_dis_bulkv_dbcs_brst','2015-10-15/06:47:23','2015-10-15/06:54:59'), $
    'Error! Not grabbing the correct data from the SPDF??? (2)'
  return, 1
end

; user requests a time interval without any CDF files inside
function mms_load_fpi_ut::test_empty_interval_spdf
  mms_load_fpi, trange=['2015-10-15/6:46:21', '2015-10-15/6:49:01'], data_rate='brst', level='l2', /spdf
  assert, spd_data_exists('mms3_dis_bulkv_dbcs_brst','2015-10-15/06:47:23','2015-10-15/06:49:59'), $
    'Error! Not grabbing the correct data from the SPDF??? (3)'
  return, 1
end

; user requests a time interval just beyond start time (but inside the interval)
; of last burst-mode file for the day
function mms_load_fpi_ut::test_weird_fpi_case_spdf
  mms_load_fpi, trange=['2015-10-16/13:07', '2015-10-16/13:09'], data_rate='brst', level='l2', /spdf
  assert, spd_data_exists('mms3_dis_bulkv_dbcs_brst','2015-10-16/13:07','2015-10-16/13:09'), $
    'Error! Not grabbing the correct data from the SPDF??? (4)'
  return, 1
end

; user downloads data, then tries to load the data using /no_update
function mms_load_fpi_ut::test_noupdate_actually_works_spdf
  ; load the data from the web
  mms_load_fpi, trange=['2015-10-15', '2015-10-18'], level='l2', probe=1, datatype='dis-moms', cdf_filenames=fn_sdc, /spdf
  del_data, '*'

  ; load the data locally
  mms_load_fpi, trange=['2015-10-15', '2015-10-18'], level='l2', probe=1, datatype='dis-moms', cdf_filenames=fn_local, /no_update, /spdf
  assert, spd_data_exists('mms1_dis_energyspectr_omni_fast', '2015-10-15', '2015-10-18'), $
    'Problem loading data from local drive'
  assert, array_equal(fn_sdc, fn_local), $
    'Problem loading data from local drive (different CDF filenames)'
  return, 1
end

; check that loading from local data doesn't load all data for the day
function mms_load_fpi_ut::test_noupdate_mem_spdf
  ; load the data from the web
  mms_load_fpi, trange=['2015-10-16/13:06:00', '2015-10-16/13:08:00'], level='l2', probe=1, datatype='dis-moms', cdf_filenames=fn_sdc, /spdf
  del_data, '*'

  ; load the data locally
  mms_load_fpi, trange=['2015-10-16/13:06:00', '2015-10-16/13:08:00'], level='l2', probe=1, datatype='dis-moms', cdf_filenames=fn_local, /no_update, /spdf
  assert, array_equal(fn_sdc, fn_local), $
    'Problem loading data from local drive (different CDF filenames)'
  return, 1
end

; user requests a few seconds after file start time
function mms_load_fpi_ut::test_seconds_after_file_start
  mms_load_fpi, trange=['2015-10-15/6:45:21', '2015-10-15/6:51:21'], data_rate='brst', level='l2'
  assert, spd_data_exists('mms3_dis_bulkv_dbcs_brst','2015-10-15/06:47:23','2015-10-15/06:54:59'), $
    'Error! Not grabbing the correct data from the SDC???'
  return, 1
end

; user requests a few seconds after file end time
function mms_load_fpi_ut::test_seconds_after_file_end
  mms_load_fpi, trange=['2015-10-15/6:49:21', '2015-10-15/6:54:01'], data_rate='brst', level='l2'
  assert, spd_data_exists('mms3_dis_bulkv_dbcs_brst','2015-10-15/06:47:23','2015-10-15/06:54:59'), $
    'Error! Not grabbing the correct data from the SDC???'
  return, 1
end

; user requests a time interval without any CDF files inside
function mms_load_fpi_ut::test_empty_interval
  mms_load_fpi, trange=['2015-10-15/6:46:21', '2015-10-15/6:49:01'], data_rate='brst', level='l2'
  assert, spd_data_exists('mms3_dis_bulkv_dbcs_brst','2015-10-15/06:47:23','2015-10-15/06:49:59'), $
    'Error! Not grabbing the correct data from the SDC???'
  return, 1
end

; user requests a time interval just beyond start time (but inside the interval)
; of last burst-mode file for the day
function mms_load_fpi_ut::test_weird_fpi_case
  mms_load_fpi, trange=['2015-10-16/13:07', '2015-10-16/13:09'], data_rate='brst', level='l2'
  assert, spd_data_exists('mms3_dis_bulkv_dbcs_brst','2015-10-16/13:07','2015-10-16/13:09'), $
    'Error! Not grabbing the correct data from the SDC???'
  return, 1
end

; user downloads data, then tries to load the data using /no_update
function mms_load_fpi_ut::test_noupdate_actually_works
  ; load the data from the web
  mms_load_fpi, trange=['2015-10-15', '2015-10-18'], level='l2', probe=1, datatype='dis-moms', cdf_filenames=fn_sdc
  del_data, '*'
  
  ; load the data locally
  mms_load_fpi, trange=['2015-10-15', '2015-10-18'], level='l2', probe=1, datatype='dis-moms', cdf_filenames=fn_local, /no_update
  assert, spd_data_exists('mms1_dis_energyspectr_omni_fast', '2015-10-15', '2015-10-18'), $
    'Problem loading data from local drive'
  assert, array_equal(strlowcase(fn_sdc), strlowcase(fn_local)), $
    'Problem loading data from local drive (different CDF filenames)'
  return, 1
end

; check that loading from local data doesn't load all data for the day
function mms_load_fpi_ut::test_noupdate_mem
  ; load the data from the web
  mms_load_fpi, trange=['2015-10-16/13:06:00', '2015-10-16/13:08:00'], level='l2', probe=1, datatype='dis-moms', cdf_filenames=fn_sdc
  del_data, '*'
  
  ; load the data locally
  mms_load_fpi, trange=['2015-10-16/13:06:00', '2015-10-16/13:08:00'], level='l2', probe=1, datatype='dis-moms', cdf_filenames=fn_local, /no_update
  assert, array_equal(strlowcase(fn_sdc), strlowcase(fn_local)), $
    'Problem loading data from local drive (different CDF filenames)'
  return, 1
end

; check that the errorflags variable can be loaded without being overwritten
; when the user requests datatype=['d?s-dist', 'd?s-moms']
function mms_load_fpi_ut::test_load_errorflags_moms_and_dist
  mms_load_fpi, datatype=['dis-moms', 'dis-dist'], data_rate='fast', trange=['2015-12-15', '2015-12-16'], probe=3
  assert, spd_data_exists('mms3_dis_errorflags_fast_moms mms3_dis_errorflags_fast_dist', '2015-12-15', '2015-12-16'), 'Problem loading errorflags variables'
  get_data, 'mms3_dis_errorflags_fast_moms', data=a
  get_data, 'mms3_dis_errorflags_fast_dist', data=b
  
  assert, ~array_equal(a.Y, b.Y), 'Problem loading errorflags variables'
  
  mms_load_fpi, datatype=['des-moms', 'des-dist'], data_rate='fast', trange=['2015-12-15', '2015-12-16'], probe=3
  assert, spd_data_exists('mms3_des_errorflags_fast_moms mms3_des_errorflags_fast_dist', '2015-12-15', '2015-12-16'), 'Problem loading errorflags variables'
  get_data, 'mms3_des_errorflags_fast_moms', data=a
  get_data, 'mms3_des_errorflags_fast_dist', data=b

  assert, ~array_equal(a.Y, b.Y), 'Problem loading errorflags variables'
  return, 1
end

; check that the above works with the suffix keyword
function mms_load_fpi_ut::test_load_errorflags_moms_and_dist_suffix
  mms_load_fpi, datatype=['dis-moms', 'dis-dist'], data_rate='fast', trange=['2015-12-15', '2015-12-16'], probe=3, suffix='TESTSUFFIX'
  assert, spd_data_exists('mms3_dis_errorflags_fastTESTSUFFIX_moms mms3_dis_errorflags_fastTESTSUFFIX_dist', '2015-12-15', '2015-12-16'), 'Problem loading errorflags variables'
  get_data, 'mms3_dis_errorflags_fastTESTSUFFIX_moms', data=a
  get_data, 'mms3_dis_errorflags_fastTESTSUFFIX_dist', data=b

  assert, ~array_equal(a.Y, b.Y), 'Problem loading errorflags variables'

  mms_load_fpi, datatype=['des-moms', 'des-dist'], data_rate='fast', trange=['2015-12-15', '2015-12-16'], probe=3, suffix='TESTSUFFIX2'
  assert, spd_data_exists('mms3_des_errorflags_fastTESTSUFFIX2_moms mms3_des_errorflags_fastTESTSUFFIX2_dist', '2015-12-15', '2015-12-16'), 'Problem loading errorflags variables'
  get_data, 'mms3_des_errorflags_fastTESTSUFFIX2_moms', data=a
  get_data, 'mms3_des_errorflags_fastTESTSUFFIX2_dist', data=b

  assert, ~array_equal(a.Y, b.Y), 'Problem loading errorflags variables'
  return, 1
end

; check that we don't crash when the version # isn't valid
function mms_load_fpi_ut::test_load_local_file_badversion
  mms_load_fpi, datatype='des-moms', trange=['2015-12-5', '2015-12-6'], cdf_version='2.32.0', /no_update
  return, 1
end

; end of regression tests <------

; check multiple data rates
;function mms_load_fpi_ut::test_load_datarate_array
;  mms_load_fpi, probe=3, data_rate=['fast', 'brst'], level='l2'
;  assert, spd_data_exists('', '2015-12-15', '2015-12-16'), $
;    'Problem loading FPI data with multiple data rates specified'
;  return, 1
;end

; the following tests loading the ACR burst mode data (that doesn't exist at the SDC yet)
; and tests /center_measurement functionality; these data files will need to be saved
; on your local machine before running these tests
function mms_load_fpi_ut::test_acr_brst_data
  mms_load_fpi, level='acr', trange=['2016-12-09', '2016-12-10'], probe=1, data_rate='brst' 
  mms_load_fpi, level='acr', trange=['2016-12-09', '2016-12-10'], probe=1, data_rate='brst', suffix='centered', /center_measurement
  get_data, 'mms1_des_bulkv_dbcs_brst', data=d
  get_data, 'mms1_dis_bulkv_dbcs_brstcentered', data=c
  assert, spd_data_exists('mms1_des_bulkv_dbcs_brst mms1_dis_bulkv_dbcs_brstcentered', '2016-12-09', '2016-12-10'), 'Problem checking /center_measurement with ACR brst files'
  assert, time_string(c.X[0], tformat='YYYY-MM-DD/hh:mm:ss.fff') eq '2016-12-09/08:58:54.110', 'Problem checking /center_measurement with ACR brst files'
  assert, time_string(d.X[0], tformat='YYYY-MM-DD/hh:mm:ss.fff') eq '2016-12-09/08:58:54.005', 'Problem checking /center_measurement with ACR brst files'
  return, 1
end

function mms_load_fpi_ut::test_ang_ang_elec
  @error_is_pass
  mms_fpi_ang_ang, '2015-10-15/13:06:30', species='elec'
  return, 1
end

function mms_load_fpi_ut::test_ang_ang_e
  mms_fpi_ang_ang, '2015-10-15/13:06:30', species='e'
  return, 1
end

function mms_load_fpi_ut::test_load
  mms_load_fpi, probe=4, level='l2', datatype='des-moms'
  assert, spd_data_exists('mms4_des_energyspectr_omni_fast mms4_des_pitchangdist_avg mms4_des_energyspectr_px_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data'
  return, 1
end

function mms_load_fpi_ut::test_load_multi_probes
  mms_load_fpi, probe=['1', '4'], level='l2', datatype='des-moms'
  assert, spd_data_exists('mms1_des_energyspectr_omni_fast mms4_des_pitchangdist_avg mms1_des_energyspectr_pz_fast', '2015-12-15', '2015-12-16'), 'Problem loading multiple probe fpi data'
  return, 1
end

function mms_load_fpi_ut::test_load_mixed_probe_type
  mms_load_fpi, probes=['1', 4], level='l2', datatype='des-moms'
  assert, spd_data_exists('mms1_des_energyspectr_omni_fast mms4_des_pitchangdist_avg mms1_des_energyspectr_pz_fast', '2015-12-15', '2015-12-16'), 'Problem loading mixed probe type fpi data'
  return, 1
end

function mms_load_fpi_ut::test_load_level_ql
  mms_load_fpi, probe=1, level='ql', min_version='3.0.0'
  assert, spd_data_exists('mms1_des_energyspectr_omni_fast mms1_des_energyspectr_py_fast','2015-12-15', '2015-12-16'), 'Problem loading quicklook fpi data'
  assert, ~spd_data_exists('mms2_dis_TempYY_err','2015-12-15', '2015-12-16'), 'Problem loading quicklook fpi data'
  return, 1
end

function mms_load_fpi_ut::test_load_level_sitl
  mms_load_fpi, probes=[4], level='sitl'
  assert, spd_data_exists('mms4_fpi_ePitchAngDist_avg', '2015-12-15', '2015-12-16'), 'Problem loading fpi data of type sitl'
  return, 1
end

function mms_load_fpi_ut::test_load_data_rate
  mms_load_fpi, probes=1, data_rate='fast', datatype='des-moms'
  assert, spd_data_exists('mms1_des_energyspectr_mz_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data with data rate'
  return, 1
end

function mms_load_fpi_ut::test_load_data_rate_caps
  mms_load_fpi, probes='1', data_rate='FAST', datatype='des-moms'
  assert, spd_data_exists('mms1_des_energyspectr_mz_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data with data rate with CAPS'
  return, 1
end

function mms_load_fpi_ut::test_load_data_rate_invalid
  mms_load_fpi, probes=['1'], datatype=1234
  assert, ~spd_data_exists('mms1_des_energyspectr_mz_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data with invalid data rate'
  return, 1
end

function mms_load_fpi_ut::test_load_dtypes
  mms_load_fpi, probe=1, datatype=['des-moms']
  assert, spd_data_exists('mms1_des_energyspectr_anti_fast mms1_des_pitchangdist_lowen_fast mms1_des_prestensor_gse_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data using data type'
  return, 1
end

function mms_load_fpi_ut::test_load_dtypes_multi
  mms_load_fpi, probe=1, datatype=['des-moms', 'dis-dist']
  assert, spd_data_exists('mms1_des_energyspectr_mx_fast mms1_des_pitchangdist_miden_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data type using multiple data types'
  return, 1
end

function mms_load_fpi_ut::test_load_dtypes_caps
  mms_load_fpi, probe=1, datatype='DIS', level='ql', min_version='3.0.0'
  assert, spd_data_exists('mms1_dis_startdelphi_angle_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data with data types in CAPS (1)'
  assert, ~spd_data_exists('mms1_DIS_startdelphi_angle_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data with data types in CAPS (2)'
  return, 1
end

function mms_load_fpi_ut::test_load_dtypes_star
  mms_load_fpi, probe=1, datatype='*'
  assert, spd_data_exists('mms1_des_pitchangdist_avg', '2015-12-15', '2015-12-16'), 'Problem loading fpi data with data types with star (1)'
  assert, ~spd_data_exists('mms3_des_pitchangdist_avg', '2015-12-15', '2015-12-16'), 'Problem loading fpi data with data types with star (2)'
  return, 1
end

function mms_load_fpi_ut::test_load_suffix
  mms_load_fpi, probe=4, level=['sitl'], suffix='_test'
  assert, spd_data_exists('mms4_fpi_startDelPhi_count_test mms4_fpi_eEnergySpectr_pY_test', '2015-12-15', '2015-12-16'), 'Problem loading fpi data using suffix keyword'
  assert, ~spd_data_exists('mms4_fpi_eEnergySpectr_pY', '2015-12-15', '2015-12-16'), 'Problem loading fpi data using suffix keyword'
  return, 1
end

function mms_load_fpi_ut::test_load_time_clip
  mms_load_fpi, probe=1, datatype='DIS', level='ql', trange=['2015-12-15 00:04:00', '2015-12-15 00:12:00'], /time_clip
  assert, spd_data_exists('mms1_dis_startdelphi_angle_fast', '2015-12-15/00:04:00', '2015-12-15/00:12:00'), 'Problem loading fpi data with time_clip'
  assert, ~spd_data_exists('mms1_dis_startdelphi_angle_fast', '2015-12-15/00:00:00', '2015-12-15/00:4:00'), 'Problem loading fpi data with time_clip'
  assert, ~spd_data_exists('mms1_dis_startdelphi_angle_fast', '2015-12-15/00:12:00', '2015-12-15/00:14:00'), 'Problem loading fpi data with time_clip'
  return, 1
end

function mms_load_fpi_ut::test_load_spdf
  mms_load_fpi, probe=1, datatype=['des-moms'], /spdf
  assert, spd_data_exists('mms1_des_energyspectr_mx_fast mms1_des_pitchangdist_miden_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data type using spdf'
  assert, ~spd_data_exists('mms2_des_energyspectr_mx_fast mms3_des_pitchangdist_miden_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data type using spdf'
  return, 1
end

function mms_load_fpi_ut::test_load_trange
  trange=timerange()
  mms_load_fpi, trange=trange, probe=1, datatype=['des-moms'], /spdf
  assert, spd_data_exists('mms1_des_energyspectr_mx_fast mms1_des_pitchangdist_miden_fast', '2015-12-15', '2015-12-16'), 'Problem loading fpi data type using trange'
  return, 1
end

function mms_load_fpi_ut::test_load_future_time
  start_date = systime(/seconds) + 86400.*10.
  stop_date = start_date + 86400.
  mms_load_fpi, trange=[start_date, stop_date], probe=1, datatype=['des-moms']
  assert, ~spd_data_exists('mms1_*', '2040-07-30', '2040-07-31'), 'Problem loading fpi data for date in future'
  return, 1
end

; regression test for bug fixed by updated CDFs (v3)
function mms_load_fpi_ut::test_load_energies_no_support
  mms_load_fpi, datatype='des-moms', varformat='*energys*', probe=1
  get_data, 'mms1_des_energyspectr_mx_fast', data=d
  assert, n_elements(d.V[0, *]) eq 32 and d.V[0, 31] ne 31, 'Problem with energy table in FPI energy spectra variables'
  return, 1
end

pro mms_load_fpi_ut::setup
  del_data, '*'
  timespan, '2015-12-15', 1, /day
  ; create a connection to the SDC (as a team member); ignore the 'FGM' part
  mms_load_data, login_info='test_auth_info_team.sav', instrument='fgm'
end

function mms_load_fpi_ut::init, _extra=e
  if (~self->MGutTestCase::init(_extra=e)) then return, 0
  ; the following adds code coverage % to the output
  self->addTestingRoutine, ['mms_load_fpi', 'mms_load_fpi_fix_spectra', 'mms_load_fpi_fix_dist', 'mms_load_fpi_fix_angles', $
      'mms_load_fpi_calc_omni', 'mms_load_fpi_calc_pad', 'mms_fpi_fix_metadata']
  return, 1
end

pro mms_load_fpi_ut__define
  define = { mms_load_fpi_ut, inherits MGutTestCase }
end