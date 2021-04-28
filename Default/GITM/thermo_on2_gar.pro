
;filelist = findfile('3DALL_t????1[3-6]_00000?.bin')
filelist=findfile('/raid4/gitm_gar2/run_test/UA/data/2009_eddy_only_p_1/3DALL*.bin')
north = 1

nfiles = n_elements(filelist)


ff = 0
lf = nfiles-1

arr_col=fltarr(nfiles)

for ifile = ff, lf do begin

    file = filelist(iFile)

    read_thermosphere_file, file, nvars, nalts, nlats, nlons,vars,data, $
      nBLKlat, nBLKlon, nBLK, iTime, Version

    ;stop 
    p = strpos(file, '3DALL')
    if (p eq 0) then p = strlen(file)
    on2file = strmid(file,0,p)+'on2'

    setdevice, on2file+'.ps','p',5

    k = 1.3807e-23

    n = reform(data(4,*,*,2:nalts-3)) + $
        reform(data(5,*,*,2:nalts-3)) + $
        reform(data(6,*,*,2:nalts-3))
    t = reform(data(15,*,*,2:nalts-3))
    Alt = reform(data(2,0,0,2:nalts-3))
    dAlt_150 = (reform(data(2,0,0,3:nalts-2))-reform(data(2,0,0,1:nalts-4)))/2.0
    loc_150 = where( alt lt 150000.0)

    p = alog(n*k*t)

    nLons = n_elements(t(*,0,0))
    nLats = n_elements(t(0,*,0))
    nAlts = n_elements(t(0,0,*))

    io_ = 4
    in2_ = 6
    ino_ = 8

    o      = fltarr(nLons, nLats)
    n2     = fltarr(nLons, nLats)
    AltInt = fltarr(nLons, nLats)

    noInt = fltarr(nLons, nLats)

; The lowest two levels are ghost cells, so we want to start at level 2.

    nLevel = 8
    nLevel = 17
    if (ifile eq ff) then maxpressure = min(p(*,*,nLevel))

    MaxValN2 = 1.0e21

    for iLon = 0, nLons-1 do begin
        for iLat = 0, nLats-1 do begin

           noInt(iLon,iLat) = total(data(ino_,iLon,iLat,2+loc_150)*dAlt_150(loc_150))

            iAlt = nAlts-1
            Done = 0
            if (max(data(in2_,iLon,iLat,*)) eq 0.0) then Done = 1
            while (Done eq 0) do begin
                dAlt = (Alt(iAlt)-Alt(iAlt-1))
                n2Mid = (data(in2_,iLon,iLat,iAlt) + $
                         data(in2_,iLon,iLat,iAlt-1)) /2.0
                oMid  = (data(io_,iLon,iLat,iAlt) + $
                         data(io_,iLon,iLat,iAlt-1)) /2.0

                if (n2(iLon,iLat) + n2Mid*dAlt lt MaxValN2) then begin
                    n2(iLon,iLat) = n2(iLon,iLat) + n2Mid*dAlt
                    o(iLon,iLat)  =  o(iLon,iLat) +  oMid*dAlt
                    iAlt = iAlt - 1
                endif else begin
                    dAlt = (MaxValN2 - n2(iLon,iLat)) / n2Mid
                    n2(iLon,iLat) = n2(iLon,iLat) + n2Mid*dAlt
                    o(iLon,iLat)  =  o(iLon,iLat) +  oMid*dAlt
                    AltInt(iLon,iLat) = Alt(iAlt) - dAlt
                    Done = 1
                endelse
            endwhile

;            r = 1.0 - (maxpressure - p(iLon, iLat, iAlt)) / $
;              (p(iLon, iLat, iAlt-1) - p(iLon, iLat, iAlt))
;
;            tp =  r * p(iLon, iLat, iAlt) + (1-r) * p(iLon, iLat, iAlt-1)
;
;            a = r*Alt(iAlt) + (1-r)*Alt(iAlt-1)
;            ds = Alt(iAlt) - a
;
;            tob = exp(r * alog(data(io_,iLon,iLat,iAlt)) + $
;                      (1-r)* alog(data(io_,iLon,iLat,iAlt-1)))
;            tot = data(io_,iLon,iLat,iAlt)
;            to = (tob+tot)/2.0
;
;            tn2b = exp(r * alog(data(in2_,iLon,iLat,iAlt)) + $
;                       (1-r)* alog(data(in2_,iLon,iLat,iAlt-1)))
;            tn2t = data(in2_,iLon,iLat,iAlt)
;            tn2 = (tn2b+tn2t)/2.0
;
;            o(iLon, iLat)  = to*ds
;            n2(iLon, iLat) = tn2*ds
;
;            for i = iAlt+1, nAlts-1 do begin
;                ds = Alt(i) - Alt(i-1)
;                to = (data(io_, iLon, iLat, i) + data(io_, iLon, iLat, i-2))/2
;                o(iLon, iLat)  = o(iLon,iLat) + to*ds
;                tn2 = (data(in2_, iLon, iLat, i) + data(in2_, iLon, iLat, i-2))/2
;                n2(iLon, iLat)  = n2(iLon,iLat) + tn2*ds
;            endfor

        endfor
    endfor

    ratio = n2*0.0
    loc = where(n2 gt 0.0,count)
    if (count gt 0) then ratio(loc) = o(loc)/n2(loc)
    loc = where(n2 eq 0.0,count)

    quantity = AltInt/1000.0 

    if (nLats gt 1) then begin

        nl = 30
        levels = 0.4*findgen(nl+1)/nl + 0.0

        lon = (reform(data(0,*,*,0))*180/!pi + 360.0) mod 360.0

        utime = itime(3)*3600.0 + $
          itime(4)*60.0 + $
          itime(5)
        utime = utime(0)

;        print, utime

        lat = reform(data(1,*,*,0))*180/!pi

        localtime = (utime/3600.0 + lon*12.0/180.0) mod 24.0

        reratio = ratio*0.0
        relocaltime = localtime*0.0
        for i=0,nLons-1 do begin
            loc = where(localtime(*,0) eq min(localtime(*,0)))
            relocaltime(i,*) = localtime(loc(0),*)
            reratio(i,*) = ratio(loc(0),*)
            localtime(loc(0),*) = 10000.0
        endfor

        ppp = 3
        space = 0.05
        pos_space, ppp, space, sizes, ny = ppp
        get_position, ppp, space, sizes, 0, pos
        xm = (pos(2)+pos(0))/2.0
        dx = pos(2)-pos(0)
        pos(0) = xm-dx
        pos(2) = pos(0)+dx*2

        ctpos = pos
        ctpos(0) = pos(2)+0.01
        ctpos(2) = ctpos(0)+0.03

        !p.position = pos

        p0lon = utime/3600.0 * 360.0 / 24.0
        makect,'all'

        if (north) then map_set, 0.0, 180.0-p0lon, /cont $
        else map_set, 0.0, 180.0-p0lon, /cont
;        if (north) then map_set, 0.0, 180.0-p0lon, /orthographic, /cont $
;        else map_set, 0.0, 180.0-p0lon, /orthographic, /cont

        xyouts, (pos(0)+pos(2))/2.0, pos(3)+0.01, file, /norm, align=0.5
        !p.position = -1

        loc = where(ratio gt levels(nl-1),count)
        if (count gt 0) then ratio(loc) = levels(nl-1)

        newrat = ratio(1:nLons-2,1:nLats-2)
        newqua = quantity(1:nLons-2,1:nLats-2)

        newlat = lat(1:nLons-2,1:nLats-2)
        newlon = lon(1:nLons-2,1:nLats-2)
        nLons  = nLons-2
        nLats  = nLats-2

        newrat(0,*)       = (newrat(1,*)+newrat(nLons-2,*))/2.0
        newrat(nLons-1,*) = (newrat(1,*)+newrat(nLons-2,*))/2.0
        newrat(*,0)       = mean(newrat(*,1))
        newrat(*,nLats-1) = mean(newrat(*,nLats-2))

        newqua(0,*)       = (newqua(1,*)+newqua(nLons-2,*))/2.0
        newqua(nLons-1,*) = (newqua(1,*)+newqua(nLons-2,*))/2.0
        newqua(*,0)       = mean(newqua(*,1))
        newqua(*,nLats-1) = mean(newqua(*,nLats-2))

        newlon(0,*)       = 0.0
        newlon(nLons-1,*) = 360.0
        newlat(*,0) = -90.0
        newlat(*,nLats-1) =  90.0

        save, newrat, newlon, newlat, file = on2file+'.save'

        avg_globe_on2=mean(newrat)
        arr_col[ifile]=avg_globe_on2 ;garima changing - averaging the O/N2 globally
        

        l = where(newrat lt levels(1),c)
        if (c gt 0) then newrat(l) = levels(1)
        l = where(newrat gt levels(29),c)
        if (c gt 0) then newrat(l) = levels(29)

        contour, newrat, newlon, newlat, $
          /follow, nlevels = nl, /cell_fill, /over, $
          levels = levels, title = file

        map_continents
        map_grid, lats = findgen(19)*10-90, glinethick=3

        plotct, 255, ctpos, mm(levels), 'O/N2', /right

        get_position, ppp, space, sizes, 1, pos
        xm = (pos(2)+pos(0))/2.0
        dx = pos(2)-pos(0)
        pos(0) = xm-dx
        pos(2) = pos(0)+dx*2

        ctpos = pos
        ctpos(0) = pos(2)+0.01
        ctpos(2) = ctpos(0)+0.03

        !p.position = pos

        if (north) then $
          map_set, 0, 180.0-p0lon, /cont, /noerase $
        else map_set, -0, 180.0-p0lon, /cont, /noerase
;        if (north) then $
;          map_set, 0, 180.0-p0lon, /orthographic, /cont, /noerase $
;        else map_set, -0, 180.0-p0lon, /orthographic, /cont, /noerase
;    xyouts, (pos(0)+pos(2))/2.0, pos(3)+space, file, /norm
        !p.position = -1

        newqua = noInt(1:nLons,1:nLats)
        newqua(0,*)       = (newqua(1,*)+newqua(nLons-2,*))/2.0
        newqua(nLons-1,*) = (newqua(1,*)+newqua(nLons-2,*))/2.0
        newqua(*,0)       = mean(newqua(*,1))
        newqua(*,nLats-1) = mean(newqua(*,nLats-2))


        mini = min(newqua)
        maxi = max(newqua)
        range = (maxi-mini)
        if (range lt 1.0) then range = 30.0
        mini = mini - 0.1*range
        maxi = maxi + 0.1*range
        mini = 0e18
        maxi = 6e18

;mini = 220.0
;maxi = 400.0
        
        levels = findgen(31) * (maxi-mini) / 30 + mini

        loc = where(newqua gt levels(nl-1),count)
        if (count gt 0) then newqua(loc) = levels(nl-1)
        
        ; change garima - added xrange, yrange and zrange to the contour command,$
            ;otherwise was getting error about data range missing.
        contour, newqua, newlon, newlat, $
          /follow, nlevels = nl, /cell_fill, /over, $
        levels = levels, title = file,yrange=[-30,30],xrange=[-30,30], zrange=[0,100000]
          
         
              
        map_continents
        map_grid, lats = findgen(19)*10-90, glinethick=3

        plotct, 255, ctpos, mm(levels), 'Int NO (/m2)', /right

        closedevice

    endif

endfor

time=findgen(nfiles)
save, arr_col, file = on2file+'.save'

save, arr_col, filename='eddy_p_1.sav'

;set_plot, 'ps'
;DEVICE, FILE='myfile_800.ps', /LANDSCAPE
;PLOT, time, arr_col,linestyle=0,thick=4,$
;    TITLE='O/N2 ratio', xtitle='time'
;DEVICE, /CLOSE
;, TITLE='ON2', XTITLE='Time',thick=3, color='black')
;graph.save,'on2_line_plot.png'
;, border=10, resolution= 300, /transparent


;restore, 'eddy_200.sav',restored_objects='eddy_200'
;restore, 'eddy_500.sav', restored_objects='eddy_500'
;restore, 'eddy_800.sav', restored_objects='eddy_800'


;set_plot, 'ps'
;DEVICE, FILE='myfile_all.ps', /LANDSCAPE
;PLOT, time, eddy_200,linestyle=0,thick=4,$
;    TITLE='O/N2 ratio', xtitle='time', color='blue',$
;    name='Eddy 200'

;PLOT, time, eddy_500,linestyle=0,thick=4,$
;    color='blue', name='Eddy 500'


;PLOT, time, eddy_800,linestyle=0,thick=4,$
;    color='green', name='Eddy 800'



;DEVICE, /CLOSE






end
