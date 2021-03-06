;+
;Function: 
;
;Purpose: ;
; Author: Davin Larson  
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-
function spice_valid_times,et,objects=objects,tolerance=tol
if spice_test() eq 0 then message,'SPICE not installed'
if ~keyword_set(tol) then tol=120.
kinfo = spice_kernel_info(use_cache=1)
ok = et ne 0  ; get array of 1b     
for j=0,n_elements(objects)-1 do begin
  cspice_bods2c,objects[j],code,found
  nw = 0
  if found then  w = where(kinfo.obj_code eq code ,nw)
  if nw ne 0 then begin
    nvalid = replicate(0,n_elements(et))
    for i=0,nw-1 do begin
        s = kinfo[w[i]]
        etr = time_ephemeris(s.trange)
        nvalid +=  ( (et ge (etr[0]+tol)) and (et le (etr[1]-tol)) )
    endfor
    ok = ok and (nvalid ne 0)
  endif else  begin
      dprint,objects[j],' Not found. (Ignoring)'
  endelse
endfor
return,ok
end


