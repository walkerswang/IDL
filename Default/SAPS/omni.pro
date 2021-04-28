timespan,'2013-05-18',8,/hour
date='20130518'
omni_load_data
popen, '/Users/wzihan/plot/imf'+date
tplot,['OMNI_HRO_1min_BY_GSM','OMNI_HRO_1min_BZ_GSM','OMNI_HRO_1min_Pressure','OMNI_HRO_1min_SYM_H','OMNI_HRO_1min_AE_INDEX']
pclose

end
