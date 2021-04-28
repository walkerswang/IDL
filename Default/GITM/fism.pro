close,1

file='/Users/wzihan/data/FISM_Files/2017/fismflux20170905.dat'

openr,1,file

nBins = 59L
nTimesMax = 1440L*32L

nTimes = 0L

line = ''
while strpos(line,'#START') lt 0 do readf,1,line

tmp = fltarr(nBins)
iTime = intarr(6)
data = fltarr(nBins,nTimesMax)
time = dblarr(nTimesMax)

while not eof(1) do begin

  readf,1,iTime,tmp
  c_a_to_r, iTime, rTime
  time(nTimes) = rTime
  data(*,nTimes) = tmp

  nTimes = nTimes + 1

endwhile

time = time(0:nTimes-1)
data = data(*,0:nTimes-1)

close,1

stime = min(time)
etime = max(time)

stime = min(time)
etime = max(time)

time_axis, stime, etime, btr, etr, xtickname, xtitle, xtickv, xminor, xtickn

setdevice, 'fism.ps','p',5

for i=0,58 do begin

  xtn = strarr(10)+' '
  xt = ' '
  if (i mod 10 eq 0) then plotdumb
  if (i mod 10 eq 9) then begin
    xtn = xtickname
    xt = xtitle
  endif

  frac = (i mod 10)/10.0
  top = 1.0-frac
  bot = top-0.09
  pos = [0.1, bot, 0.95, top]

  plot, (time-stime), data(i,*), ystyle = 1, xstyle = 1, $
    xtickname=xtn, xtitle=xt, xtickv=xtickv, $
    xminor=xminor, xticks=xtickn, thick = 4, $
    pos = pos, /noerase

endfor

closedevice

end