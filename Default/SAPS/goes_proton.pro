;setup
del_data, '*'
probe = '13'
datatype = 'magpd'
date='20130629'
timespan, '2013-06-29/02:00:00', 1, /hours
trange = timerange()

;load particle data
goes_load_data, probe=probe, datatype=datatype, trange=trange, $
  /noeph
goes_part_products, probe=probe, datatype=datatype, trange=trange, $
    uncorrected=1, g_interpolate=1, output='energy phi theta'
    
datatype='maged'
goes_load_data, probe=probe, datatype=datatype, trange=trange, $
    /noeph
goes_part_products, probe=probe, datatype=datatype, trange=trange, $
    uncorrected=1, g_interpolate=1, output='energy phi theta'
    
popen, '/Users/wzihan/plot/goes'+probe+date

;generate energy, phi, and theta spectra
;these will be in the spacecraft's coodinates
options,'g'+probe+'_magpd_dtc_uncor_flux_' +'energy','spec',0
options,'g'+probe+'_maged_dtc_uncor_flux_' +'energy','spec',0
options,'g'+probe+'_magpd_dtc_uncor_flux_' +'energy','labels',['95 kev','140 kev','210 kev','300 kev','575 kev']
options,'g'+probe+'_maged_dtc_uncor_flux_' +'energy','labels',['40 kev','75 kev','150 kev','275 kev','475 kev']
tplot, ['g'+probe+'_magpd_dtc_uncor_flux_' +'energy','g'+probe+'_maged_dtc_uncor_flux_' +'energy']
pclose
end