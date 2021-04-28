
function get_date

  spawn, 'date +%y-%m-%d-%H-%M-%S -u',date
  itime = fix(strsplit(date,'-',/extract))
  itime(5) = 0
  c_a_to_r, itime, now

  return, now

end
