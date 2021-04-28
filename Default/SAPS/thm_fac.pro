timespan, '2013-05-18'
p='e'
thm_load_efi,probe=p,coord='gsm',level='l2'
thm_load_state,probe=p
thm_load_fgm,probe = p, coord = 'gsm', level = 'l2'

;smooth the Bfield data appropriately
tsmooth2, 'th'+p+'_fgs_gsm', 601, newname = 'th'+p+'_fgs_gsm_sm601'

;make transformation matrix
thm_fac_matrix_make, 'th'+p+'_fgs_gsm_sm601',other_dim='rgeo', pos_var_name='th'+p+'_state_pos', newname = 'th'+p+'_fgs_gsm_sm601_fac_mat'

;transform Bfield vector (or any other) vector into field aligned coordinates
tvector_rotate, 'th'+p+'_fgs_gsm_sm601_fac_mat', 'th'+p+'_efs_dot0_gsm', newname = 'th'+p+'_efs_dot0_fac'
tlimit,'2013-05-18/04:00:00','2013-05-18/05:00:00'
tplot_options,'title','THM'+p

popen, '/Users/wzihan/plot/thm'+p+'_saps_e'+'20130518'
tplot, 'th'+p+'_efs_dot0_fac'

pclose
end
