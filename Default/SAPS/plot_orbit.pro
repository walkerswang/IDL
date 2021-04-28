date1_l='2013-06-29/02:00:00'

date2_l='2013-06-29/03:00:00'

timespan,date1_l,1,/hour

goes_pos=goes_load_pos(trange = [date1_l,date2_l], probe = 13, coord_sys = 'gsm')

print,goes_pos

goes_pos=goes_load_pos(trange = [date1_l,date2_l], probe = 15, coord_sys = 'gsm')

print,goes_pos
end