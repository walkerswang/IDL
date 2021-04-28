pro rebinsmpkeo_towang

mydevice = !D.NAME

SET_PLOT,'PS'

mydir='/Users/wzihan/rainbow/'
datestr='20130518'

restore, filename=mydir+'\keo_smp.sav'

cnt=n_elements(utimg)

binsz=0.01 ; minimum bin size

lat=smplatarr[0:cntsmp-1]

res=histogram(lat,min=min_lat,max=max_lat,binsize=binsz,location=latarr)

cnt_arr=n_elements(latarr)

latkarrb=fltarr(cnt_arr)
cntkarrb=0

latkarrb[0]=min_lat
num=0
min_num=4 ; require that the pixel number to integrate in the latitudianl bin must be greater than 4

i=1
while i lt cnt_arr do begin
 while num lt min_num and i lt cnt_arr do begin
  num+=res[i]
  i++
 endwhile
 if i lt cnt_arr-1 then begin
  latkarrb[cntkarrb]=latarr[i]
  cntkarrb++
 endif else break
 
 num=0
endwhile  

latkarrb[cntkarrb]=max_lat
cntkarrb++

;print, latkarrb[0:cntkarrb-1],cntkarrb

numkarr=cntkarrb-1
keoarr=fltarr(numkarr,3,cnt)

ref_arr=reform(smpkeoarr[0:cntsmp-1,0,*],n_elements(smpkeoarr[0:cntsmp-1,0,*]))
res= percentiles(ref_arr,value=[0.01,0.99])
red_lowL=res[0]
red_hghL=res[1]

ref_arr=reform(smpkeoarr[0:cntsmp-1,1,*],n_elements(smpkeoarr[0:cntsmp-1,1,*]))
res= percentiles(ref_arr,value=[0.01,0.99])
grn_lowL=res[0]
grn_hghL=res[1]

ref_arr=reform(smpkeoarr[0:cntsmp-1,2,*],n_elements(smpkeoarr[0:cntsmp-1,2,*]))
res= percentiles(ref_arr,value=[0.01,0.99])
blu_lowL=res[0]
blu_hghL=res[1]

print, red_lowl,grn_lowl,blu_lowl
print, red_hghl,grn_hghl,blu_hghl

for i=0, cntkarrb-2 do begin

  qq=where(smplatarr[0:cntsmp-1] ge latkarrb[i] and smplatarr[0:cntsmp-1] lt latkarrb[i+1],num)
  keoarr[i,0:2,0:cnt-1]=mean(smpkeoarr[qq,0:2,0:cnt-1],dimension=1,/double)
  
  keoarr[i,0,*]=(keoarr[i,0,*]-red_lowl)*600.
  keoarr[i,1,*]=(keoarr[i,1,*]-grn_lowl)*600.
  keoarr[i,2,*]=(keoarr[i,2,*]-blu_lowl)*600.

endfor

save, utimg,numkarr,latkarrb,keoarr,filename=mydir+'\mlo50_keo.sav'

;WINDOW,/FREE,XSIZE=800,YSIZE=600
xsize=!D.x_size
ysize=!D.y_size

image24 = BytArr(3, xsize, ysize)

device, decomposed=1,FILENAME='saskkep50.ps'

;TVCRS, 0

st=4.+30./60.
et=5.+30./60.

xticnum=fix((et-st)*4+.01)
xtickstr=strarr(xticnum+1)
for i=0, xticnum do begin
  hr=fix(st+i/4.)
  mn=round((st+i/4.-hr)*60.)
  hrmn=hr*100+mn
  xtickstr[i]=string(format='(I4.4)',hrmn)
endfor

xticval=st+indgen(xticnum+1)*1./15.

latmin=57.
latmax=63.
cnt=n_elements(utimg)
plot, [8.],[60.],/nodata,/xstyle,/ystyle,xrange=[st,et],yrange=[latmin,latmax],pos=[.1,.5,.9,.9], ytitle='MLAT (deg)',title='Sask Rainbow 2013-05-18',$
  charsize=1.6,charthick=2.,xticks=xticnum, xminor=3, xticklen=.04, xtickn=xtickstr,yminor=2,yticks=2

for j=1, cnt-2 do begin
  x1=utimg[j]-3./3600.
  x2=utimg[j]+3./3600.

  if x1 lt st then continue
  if x2 gt et then break

  for i=0, numkarr-1 do begin

    y1=latkarrb[i]
    y2=latkarrb[i+1]

    if y2 lt latmin+.05 then continue
    if y1 gt latmax-.05 then break

    rr=fix(keoarr[i,0,j])>0
    rr=rr<255
    
    gg=fix(keoarr[i,1,j])>0
    gg=gg<255

    bb=fix(keoarr[i,2,j])>0
    bb=bb<255

    chp=bb*256UL*256UL+gg*256UL+rr
    polyfill,[x1,x1,x2,x2],[y1,y2,y2,y1],color=chp
  endfor
endfor

;image24=tvrd(/true)
;write_png, mydir+'\saskkeo50.png',image24

DEVICE,/CLOSE
;TVCRS, 1
print, 'finish'
SET_PLOT,mydevice

end

function percentiles,data,value=value

  result = -1
  n = n_elements(data)
  if (n le 0) then return,result   ; error : data not defined

  ; check if speficic percentiles requested - if not: set standard
  if(not keyword_set(value)) then value = [ 0., 0.25, 0.5, 0.75, 1.0 ]

  ; create a temporary copy of the data and sort
  ; tmp = data
  ; tmp = tmp(sort(tmp))
  ; NO: simply save the sorted index array
  ix = sort(data)
  
  ; loop through percentile values, get indices and add to result
  ; This is all we need since computing percentiles is nothing more
  ; than counting in a sorted array.
  for i=0L,n_elements(value)-1L do begin

    if(value(i) lt 0. OR value(i) gt 1.) then return,-1

    ;   if(value(i) le 0.5) then ind = fix(value(i)*n)    $
    ;   else ind = fix(value(i)*(n+1))
    if(value(i) le 0.5) then ind = long(value(i)*n)    $
    else ind = long(value(i)*(n+1))
    if (ind ge n) then ind = n-1    ; small fix for small n
    ; (or value eq 1.)

    ;  if(i eq 0) then result = tmp(ind)  $
    ;  else result = [result, tmp(ind) ]
    ; ## change number 2
    if(i eq 0) then result = data(ix(ind))  $
    else result = [result, data(ix(ind)) ]
  endfor

  return,result
end
