;+
;
; Unit tests for various tplot utilities
;
; Requires both the SPEDAS QA folder (not distributed with SPEDAS) and mgunit
; in the local path
;
; $LastChangedBy: egrimes $
; $LastChangedDate: 2017-01-03 09:10:21 -0800 (Tue, 03 Jan 2017) $
; $LastChangedRevision: 22474 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/mms/common/tests/tplot_stuff_ut__define.pro $
;-

; ------- the following are some regression tests -------
function tplot_stuff_ut::test_time_clip_multi_dimen_v3
  store_data, 'test_data', data={x: [1, 2, 3, 4, 5], y: findgen(5, 16, 32, 32), v1: findgen(5, 16), v2: findgen(5, 32), v3: findgen(32)}
  time_clip, 'test_data', 2, 4, /replace
  get_data, 'test_data', data=d
  assert, n_elements(d.X) eq n_elements(d.Y[*, 0]) && n_elements(d.X) eq n_elements(d.v1[*, 0]) && n_elements(d.X) eq n_elements(d.v2[*, 0]) && n_elements(d.v3) eq 32, $
    'Problem with time_clip on tplot variable with multi dimensional v tags'
  return, 1
end

function tplot_stuff_ut::test_time_clip_multi_v2
  store_data, 'test_data', data={x: [1, 2, 3, 4, 5], y: findgen(5, 16, 32), v1: findgen(5, 16), v2: findgen(5, 32)}
  time_clip, 'test_data', 2, 4, /replace
  get_data, 'test_data', data=d
  assert, n_elements(d.X) eq n_elements(d.Y[*, 0]) && n_elements(d.X) eq n_elements(d.v1[*, 0]) && n_elements(d.X) eq n_elements(d.v2[*, 0]), $
    'Problem with time_clip on tplot variable with multi dimensional v tags'
  return, 1
end

; ------- end of the regression tests -------

function tplot_stuff_ut::test_tt2000_2_unix
  unix_val = tt2000_2_unix([4.98e17, 4.99e17])
  assert, array_equal(time_string(unix_val), ['2015-10-13/09:19:06', '2015-10-24/23:05:18']), 'Problem with tt2000_2_unix routine'
  return, 1
end

function tplot_stuff_ut::test_mult_data
  store_data, 'test_data_to_multiply', data={x: time_double('2015-1-1')+indgen(15), y: indgen(15)+8}
  mult_data, 'test_data', 'test_data_to_multiply'
  get_data, 'test_data^test_data_to_multiply', data=multiplied
  assert, array_equal(multiplied.Y, (indgen(15)+8)*(indgen(15))), 'Problem with mult_data!'
  return, 1
end

function tplot_stuff_ut::test_add_data
  store_data, 'test_data_to_add', data={x: time_double('2015-1-1')+indgen(15), y: indgen(15)+5}
  add_data, 'test_data', 'test_data_to_add'
  get_data, 'test_data+test_data_to_add', data=added
  assert, array_equal(added.Y, indgen(15)+5+indgen(15)), 'Problem with add_data!'
  return, 1
end

function tplot_stuff_ut::test_save_restore
  tplot_save, 'test_data', filename='test_data_saved'
  get_data, 'test_data', data=orig
  del_data, '*'
  tplot_restore, filename='test_data_saved.tplot'
  get_data, 'test_data', data=saved
  assert, array_equal(orig.X, saved.X) && array_equal(orig.Y, saved.Y), 'Problem with tplot_save/tplot_restore!'
  return, 1
end

function tplot_stuff_ut::test_copy_data
  copy_data, 'test_data', 'test_data_copied'
  get_data, 'test_data', data=orig
  get_data, 'test_data_copied', data=copied
  assert, array_equal(orig.X, copied.X) && array_equal(orig.Y, copied.Y), 'Problem with copy_data!'
  return, 1
end

function tplot_stuff_ut::test_del_data_multi
  del_data, ['test_data', 'test_data_nonmonotonic', 'test_data_vector']
  assert, (tnames())[0] eq '', 'Problem with del_data!'
  return, 1
end

function tplot_stuff_ut::test_del_data
  del_data, 'test_data'
  assert, n_elements(tnames()) eq 2 && (tnames())[0] eq 'test_data_nonmonotonic', 'Problem with del_data!'
  return, 1
end

function tplot_stuff_ut::test_tclip
  tclip, 'test_data', 4, 6
  get_data, 'test_data_clip', data=d
  assert, array_equal(d.Y[0:3], [0, 0, 0, 0]) && array_equal(d.Y[4:6], [4, 5, 6]) && array_equal(d.Y[7:14], [0,0,0,0,0,0,0,0]), 'Problem with tclip!'
  return, 1
end

function tplot_stuff_ut::test_time_clip
  time_clip, 'test_data', time_double('2015-1-1')+4, time_double('2015-1-1')+6
  get_data, 'test_data_tclip', data=d
  assert, array_equal([4, 5, 6], d.Y), 'Problem with time_clip!'
  return, 1
end

function tplot_stuff_ut::test_zlim_zrange
  zlim, 'test_data', 10, 20, 0
  get_data, 'test_data', limits=l
  assert, array_equal(l.zrange, [10., 20.]) && l.zlog eq 0, 'Problem with using zlim to set zrange!'
  return, 1
end

function tplot_stuff_ut::test_zlim_zlog
  zlim, 'test_data', 20, 30, 1
  get_data, 'test_data', limits=l
  assert, array_equal(l.zrange, [20., 30.]) && l.zlog eq 1, 'Problem with using zlim to set zlog!'
  return, 1
end

function tplot_stuff_ut::test_ylim_yrange
  ylim, 'test_data', 10, 20, 0
  get_data, 'test_data', limits=l
  assert, array_equal(l.yrange, [10., 20.]) && l.ylog eq 0, 'Problem with using ylim to set yrange!'
  return, 1
end

function tplot_stuff_ut::test_ylim_ylog
  ylim, 'test_data', 20, 30, 1
  get_data, 'test_data', limits=l
  assert, array_equal(l.yrange, [20., 30.]) && l.ylog eq 1, 'Problem with using ylim to set ylog!'
  return, 1
end

function tplot_stuff_ut::test_data_cut_multi
  t = data_cut('test_data', time_double('2015-1-1')+indgen(3))
  assert, array_equal(t, findgen(3)), 'Problem with data_cut with multiple times!'
  return, 1
end

function tplot_stuff_ut::test_data_cut
  t = data_cut('test_data', time_double('2015-1-1')+2)
  assert, t eq 2.0, 'Problem with data_cut?'
  return, 1
end

function tplot_stuff_ut::test_tplot_rename
  tplot_rename, 'test_data', 'test_data_new'
  assert, tnames('test_data_new') ne '' && tnames('test_data') eq '', 'Problem with tplot_rename!'
  tplot_rename, 'test_data_new', 'test_data'
  return, 1
end

function tplot_stuff_ut::test_get_data
  get_data, 'test_data', data=testdata
  assert, array_equal(testdata.X, time_double('2015-1-1')+indgen(15))
  return, 1 
end

function tplot_stuff_ut::test_tplot_sort
  tplot_sort, 'test_data_nonmonotonic'
  get_data, 'test_data_nonmonotonic', data=sorted
  get_data, 'test_data', data=orig
  assert, array_equal(sorted.X, orig.X), 'Problem with tplot_sort!'
  return, 1
end

function tplot_stuff_ut::test_clean_spikes
  get_data, 'test_data', data=d
  d.Y[6] = 10000.0
  store_data, 'test_data_spike', data=d
  clean_spikes, 'test_data_spike'
  get_data, 'test_data_spike_cln', data=d
  assert, d.Y[6] eq 0, 'Problem with clean_spikes!'
  return, 1
end

function tplot_stuff_ut::test_split_vector
  split_vec, 'test_data_vector'
  assert, tnames('test_data_vector_z') ne '', 'Problem with split_vec!'
  get_data, 'test_data_vector_x', data=vec_x
  get_data, 'test_data_vector_y', data=vec_y
  get_data, 'test_data_vector_z', data=vec_z
  assert, array_equal(vec_x.Y, indgen(15)), 'Problem with split_vec!'
  assert, array_equal(vec_y.Y, indgen(15)*17), 'Problem with split_vec!'
  assert, array_equal(vec_z.Y, indgen(15)*14), 'Problem with split_vec!'
  return, 1
end

function tplot_stuff_ut::test_join_vec
  store_data, 'test_data_x', data={x: time_double('2015-1-1')+indgen(15), y: [indgen(15)]}
  store_data, 'test_data_y', data={x: time_double('2015-1-1')+indgen(15), y: [indgen(15)+16]}
  store_data, 'test_data_z', data={x: time_double('2015-1-1')+indgen(15), y: [indgen(15)*13]}
  join_vec, 'test_data_'+['x', 'y', 'z'], 'test_data_joinvec'
  get_data, 'test_data_joinvec', data=d
  assert, array_equal(d.Y[*, 0], indgen(15)), 'Problem with join_vec!'
  assert, array_equal(d.Y[*, 1], indgen(15)+16), 'Problem with join_vec!'
  assert, array_equal(d.Y[*, 2], indgen(15)*13), 'Problem with join_vec!'
  return, 1
end

pro tplot_stuff_ut::teardown
  if (tnames('*'))[0] ne '' then del_data, '*'
end

pro tplot_stuff_ut::setup
  if (tnames('*'))[0] ne '' then del_data, '*'
  test_data = {x: time_double('2015-1-1')+indgen(15), y: indgen(15)}
  store_data, 'test_data', data=test_data
  temp = test_data.X[10]
  test_data.X[10] = test_data.X[8]
  test_data.X[8] = temp
  store_data, 'test_data_nonmonotonic', data=test_data
  store_data, 'test_data_vector', data={x: time_double('2015-1-1')+indgen(15), y: [[indgen(15)], [indgen(15)*17], [indgen(15)*14]]}
end

function tplot_stuff_ut::init, _extra=e
  if (~self->MGutTestCase::init(_extra=e)) then return, 0
  ; the following adds code coverage % to the output
  self->addTestingRoutine, ['tplot_sort', 'tplot_rename']
  self->addTestingRoutine, ['data_cut'], /is_function
  return, 1
end

pro tplot_stuff_ut__define
  define = { tplot_stuff_ut, inherits MGutTestCase }
end