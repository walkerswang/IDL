read_ampere_ncdf,'/Users/wzihan/Downloads/2013/20130518Amp_invert.ncdf',time,pseudosvnum,plane_number,pos_eci,b_eci,pseudo_sv_quality,data_splice
pos_eci=transpose(pos_eci)
b_eci=transpose(b_eci)
jul=JULDAY(5,18,2013)

n=n_elements(pos_eci)/3
pos_geo=fltarr(n,3)
pos_aac=dblarr(n,3)
b_geo=dblarr(n,3)
mlt=fltarr(n,1)
sc=10
list=[]
for i=120000, n-1 do begin
  if i mod 1000 eq 0 then begin
  print,i
  end
  if i gt 0 then begin
    if time[i] lt time[i-1] then begin
      sc=sc+1
      if n_elements(list) gt 0 then begin
        ti='SC='+strcompress(string(sc))+'  BLACK: UPWARD BLUE: SOUTH, RED: EAST'
        p=plot(pos_geo[list,1],b_geo[list,0], xrange=[40,70], yrange=[-1000,1000],title=ti,ytitle='B (nT)')
        p=plot(pos_geo[list,1],b_geo[list,1],'-b',/overplot)
        p=plot(pos_geo[list,1],b_geo[list,2],'-r',/overplot)
        t=time[list]
        tickname=strtrim(time[list[0:*:10]])
        tickname=strmid(tickname,6,5)
        a_time = axis('x', location=[0,min(p.yrange)-200,0],tickname=tickname,ticklayout=1)
        t=mlt[list]
        tickname=strtrim(mlt[list[0:*:10]])
        tickname=strmid(tickname,6,6)
        a_mlt = axis('x', location=[0,min(p.yrange)-100,0],tickname=tickname,ticklayout=1)
        t1 = TEXT(0.05,0.095 , 'MLAT')
        t1 = TEXT(0.05,0.06 , 'MLT')
        t1 = TEXT(0.05,0.023 , 'UT')
        p.save,  strcompress(string(sc))+'SC_MAG.png'
        p.close
        
        ob=pos_aac[list,*]
        b=b_geo[list,*]
        t=time[list]
        save,ob,b,t,filename='ob'+strcompress(string(sc))+'mag.sav'
        list=[]
      endif
    endif
  endif
  hour=floor(time[i])
  minute=floor((time[i]-hour)*60)
  second=floor((time[i]-hour-float(minute/60.0))*3600)
  geopack_recalc,2013,5,18,hour,minute,second,/DATE
  geopack_conv_coord, pos_eci[i,0],  pos_eci[i,1], pos_eci[i,2],a,b,c, /from_gei,/to_geo
  geopack_sphcar, a, b, c, r, theta, phi, /to_sphere, /degree
  pos_geo[i,0]=r
  pos_geo[i,1]=90-theta
  pos_geo[i,2]=phi
  
  ret = AACGM_v2_SetDateTime(2013,5,18,hour,minute,second)
  p = cnvcoord_v2(90-theta,phi, (r-6370000)/1000)
  pos_aac[i,*]=p
  mlt[i] = mlt_v2(p[1])  
  geopack_conv_coord, b_eci[i,0],  b_eci[i,1], b_eci[i,2],d,e,f, /from_gei,/to_mag
  geopack_bcarsp,a,b,c,d,e,f,br,bt,bp
  b_geo[i,0]=br
  b_geo[i,1]=bt
  b_geo[i,2]=bp
  
  if (mlt[i] lt 21) and (mlt[i] gt 18) and (time[i] gt 4.5) and (time[i] lt 5.5) and (pos_geo[i,1] gt 40) and (pos_geo[i,1] lt 70) then begin
    list=[list,i]
  endif
endfor

sc=sc+1
if n_elements(list) gt 0 then begin
  ti='SC='+strcompress(string(sc))+'  BLACK: UPWARD BLUE: SOUTH, RED: EAST'
  p=plot(pos_geo[list,1],b_geo[list,0], xrange=[40,70], yrange=[-1000,1000],title=ti, xtitle='MLAT',ytitle='B (nT)')
  p=plot(pos_geo[list,1],b_geo[list,1],'-b',/overplot)
  p=plot(pos_geo[list,1],b_geo[list,2],'-r',/overplot)
  t=time[list]
  tickname=strtrim(time[list[0,*,5]])
  tickname=strmid(tickname,6,5)
  a_time = axis('x', location='top',tickname=tickname,ticklayout=1)
  t=mlt[list]
  tickname=strtrim(mlt[list[0:*,5]])
  tickname=strmid(tickname,6,6)
  a_mlt = axis('x', location=[0,min(p.yrange)-200,0],tickname=tickname,ticklayout=1)
  t1 = TEXT(0.05,0.095 , 'MLAT')
  t1 = TEXT(0.05,0.06 , 'MLT')
  t1 = TEXT(0.05,0.023 , 'UT')
  p.save,  strcompress(string(sc))+'SC_MAG.png'
  p.close
  list=[]
endif
end