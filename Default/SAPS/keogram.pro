pro keogram

  mydir='/Users/wzihan/rainbow'
  datestr='20130518'
  mapdir='/pngmap/'

  staname='sask'

  aele={i:0,j:0,elev:0.}
  elearr=replicate(aele,256,256)
  grd={lat:0.,lon:0.}
  grdarr=replicate(grd,257,257)
  cord={cenlat:0.,cenlon:0.,minlat:0.,minlon:0.,maxlat:0.,maxlon:0.}

  l=make_array(20,1,/integer,value=0)

  openr, 1, mydir+'/'+staname+'asiskymap.gm5' ; Skymap file made by me, containing the pixel AACGM coordinates using 2015 IRGF epoch
  readu, 1, elearr
  readu, 1, cord
  readu, 1, grdarr
  close, 1

  ;WINDOW,/FREE,XSIZE=480,YSIZE=480, retain=2
  ;xsize=!D.x_size
  ;ysize=!D.y_size
  ;image24 = BytArr(3, xsize, ysize)
  
  device, decomposed=0
  TVCRS, 0

  minlat=56.
  maxlat=64.5
  minlon=-55.
  maxlon=-35

  clat=(minlat+maxlat)/2.
  clon=(minlon+maxlon)/2.
  maxelv=10. ;
  restore, mydir+'/saskrainbow01.sav' ; rainbow image file 0430-0530 UT

  loadct,0
  
  for hr=4,4 do begin
    for mn=30, 59 do begin
      for m=0,9 do begin

        for j=0,255 do begin
          for i=0,255 do begin
            if elearr[i,j].elev gt maxelv then begin
              ii=elearr[i,j].i
              jj=elearr[i,j].j
              mlat0=grdarr[jj,ii].lat
              mlat1=grdarr[jj,ii+1].lat
              mlat2=grdarr[jj+1,ii+1].lat
              mlat3=grdarr[jj+1,ii].lat
              mlon0=grdarr[jj,ii].lon
              mlon1=grdarr[jj,ii+1].lon
              mlon2=grdarr[jj+1,ii+1].lon
              mlon3=grdarr[jj+1,ii].lon

              cenlon=(mlon0+mlon1+mlon2+mlon3)/4
              cenlat=(mlat0+mlat1+mlat2+mlat3)/4
              
              trr=fix(rgb_struct_combined[(hr-4)*600+(mn-30)*10+m].red[ii,jj]*600.) >0
              trg=fix(rgb_struct_combined[(hr-4)*600+(mn-30)*10+m].green[ii,jj]*600.) >0
              trb=fix(rgb_struct_combined[(hr-4)*600+(mn-30)*10+m].blue[ii,jj]*600.) >0

              trr=trr<255
              trg=trg<255
              trb=trb<255
              chp=trb*65536L+trg*256L+trr
              
   
            endif
          endfor
        endfor

        image24=tvrd(/true)
        write_png, 'keogram.png',image24
        ;cgColorbar, Divisions=4, Minor=5, Format='(F0.2)', Range=[-800, 800]
        endfor
    endfor
  endfor

  ;set_plot, 'win'
  print, 'finish'

end
