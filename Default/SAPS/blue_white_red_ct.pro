pro blue_white_red_CT
steps = 128
scaleFactor = FINDGEN(steps) / (steps - 1)
; Do first 100 colors (yellow to blue).
redVector =0 + (255-0) * scaleFactor  ; Red vector: 0 -> 255
greenVector =0 + (255-0) * scaleFactor ; Green vector: 0 -> 255
blueVector = REPLICATE(255, steps) ; Blue vector: 255 -> 255
   
; Do second 100 colors (blue to red).
redVector = [redVector, REPLICATE(255, steps)]     ; Red vector: 255 -> 255
greenVector = [greenVector, 255 + (0 - 255) * scaleFactor]    ; Green vector: 255 -> 0
blueVector = [blueVector, 255 + (0 - 255) * scaleFactor] ; Blue vector: 255 -> 0
;print
;set_plot,'x'
;window,0,xsize=300,ysize=300
;device, decomposed=0
TVLCT, redVector, greenVector, blueVector  
;TVSCL, findgen(100,255);DIST(400)
end