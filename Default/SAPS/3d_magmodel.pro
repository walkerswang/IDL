geopack_recalc,2001,90

geopack_sphcar,1,0,0,x,y,z,/degree,/to_rect
geopack_trace,x,y,z,1,2,xf,yf,zf,fline=fline,/T89
g=plot3d(fline[*,0],fline[*,1],fline[*,2],xrange=[-40,20],yrange=[-30,30],zrange=[-30,30])

for p=0,360,20 do begin
  for i=0,90,10 do begin
    geopack_sphcar,1,i,p,x,y,z,/degree,/to_rect
    geopack_trace,x,y,z,1,2,xf,yf,zf,fline=fline,/T89
    g=plot3d(fline[*,0],fline[*,1],fline[*,2],/overplot)
   endfor

  for i=90,180,10 do begin
    geopack_sphcar,1,i,p,x,y,z,/degree,/to_rect
    geopack_trace,x,y,z,-1,2,xf,yf,zf,fline=fline,/T89
    g=plot3d(fline[*,0],fline[*,1],fline[*,2],/overplot)
   endfor

endfor

end