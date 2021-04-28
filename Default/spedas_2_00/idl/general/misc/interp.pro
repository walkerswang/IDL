;+
;FUNCTION:   interp(y,x,u)
;PURPOSE:
;  Linearly Interpolates vectors with an irregular grid.
;  INTERP is functionally the same as INTERPOL, however it is typically
;  much faster for most applications.
;USAGE:
;  result = interp(y,x,u)
;INPUTS:
;       Y:      The input vector can be any type except string.
;
;       X:      The absicissae values for Y.  This vector must have same # of
;               elements as Y.  The values MUST be monotonically ascending
;               or descending.
;
;       U:      The absicissae values for the result.  The result will have
;               the same number of elements as U.  U does not need to be
;               monotonic.
;KEYWORDS:
;  NO_CHECK_MONOTONIC:   set this keyword to skip the check for monotonic data.
;  INDEX:  Set to named variable to return the index of the closest x less than u.
;      (same dimensons as u)
;  NO_EXTRAPOLATE:  Set this keyword to prevent extrapolation.
;  INTERP_THRESHOLD:  Set to minimum allowed gap size.
;
;CREATED BY:	Davin Larson  4-30-96
;FILE:  interp.pro
;VERSION:  1.15
;LAST MODIFICATION:  02/04/17
;-
function interp,y,x,u,index=i,no_check_monotonic=ch_mon,no_extrapolate=no_extrap,interp_threshold=int_th,ignore_nan=ignore_nan

;on_error,2


ndimy = size(/n_dimension,y)
ndimx = size(/n_dimension,x)
if ndimy eq 2 then begin
    dimy= size(/dimension,y)
    dimv = dimy
    dimv[0]=n_elements(u)
    nv = make_array(dimv,type=size(/type,y))
    for j=0l,dimy[1]-1 do begin
       xx = (ndimx eq 2) ? x[*,j] : x
       nv[*,j] = interp(y[*,j],xx,u,no_extrapolate=no_extrap,interp_threshold=int_th,no_check_mono=ch_mon,index=i,ignore_nan=ignore_nan)
    endfor
    return,nv
endif

if n_params() eq 2 then begin
   nx = n_elements(y)
   return,interp(y,findgen(nx),findgen(x)/(x-1)*(nx-1),index=i,no_extrap=no_extrap,interp_thresh=int_th,ignore_nan=ignore_nan)
endif

;check for invalid x values:

nx = n_elements(x)

good = finite(x)
if keyword_set(ignore_nan) then good = good and finite(y)
good = where(good,c )
if c lt 1 then begin
;   message,/info,'Not enough valid data points to interpolate.'
   return,replicate(!values.f_nan,n_elements(u))
endif


; insure that all x points are valid
if c ne nx then return, interp(y[good],x[good],u,index=i,no_extrap=no_extrap,interp_thresh=int_th)

; insure that x is monotonically increasing
if x[0] gt x[nx-1] then return,interp(reverse(y),reverse(x),u,index=i,interp_thresh=int_th,no_extrap=no_extrap)


if not keyword_set(ch_mon) then begin
  dx = x-shift(x,1)
  dx[0] = 0
  bad = where(dx lt 0,c)
  if c ne 0 then dprint,dlevel=2,'Warning: Data not monotonic!'
endif

if keyword_set(int_th) then begin
   w = where( finite(y) ,c )
   if c eq 0 then w=[0]
   nv = interp(y[w],x[w],u,index=i,no_extrap=no_extrap)
   dx = (x[w])[i+1] - (x[w])[i]
   w = where(dx gt int_th,c)
   if c ne 0 then nv[w]=!values.f_nan
   return, nv
endif

mn = long(u)  &  mn[*] = 0l
mx = long(u)  &  mx[*] = nx-1

repeat begin           ; This loop should execute approximately log2(nx) times
   i = (mx+mn)/2
   tst = x[i] lt u
   ntst = tst eq 0
   mn =  tst*i + ntst*mn
   mx = ntst*i +  tst*mx
endrep  until max(mx-mn) le 1
i = (mx+mn)/2
nv = y[i] + (u-x[i])*(y[i+1]-y[i])/(x[i+1]-x[i])

if keyword_set(no_extrap) then begin
   mxmx = minmax(x)
   w = where( (u lt mxmx[0]) or (u gt mxmx[1]) , nbad)
   if nbad gt 0 then nv[w] = !values.f_nan
endif

return,nv
end
