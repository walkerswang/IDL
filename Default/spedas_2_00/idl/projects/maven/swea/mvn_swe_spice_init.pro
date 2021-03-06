;+
;PROCEDURE:   mvn_swe_spice_init
;PURPOSE:
;  Initializes SPICE.
;
;USAGE:
;  mvn_swe_spice_init
;
;INPUTS:
;
;KEYWORDS:
;
;    TRANGE:        Time range for MAVEN spacecraft spk and ck kernels.
;
;    LIST:          After loading, list the kernels in use.
;
;    FORCE:         If set, then clear all kernels and reload them based on TRANGE
;                   or the current value of trange_full.  Otherwise, ask the user
;                   for permission to clear and reload.
;
;    STATUS:        Don't load anything; just list kernels in use.
;
; $LastChangedBy: dmitchell $
; $LastChangedDate: 2015-05-11 11:32:53 -0700 (Mon, 11 May 2015) $
; $LastChangedRevision: 17551 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/maven/swea/mvn_swe_spice_init.pro $
;
;CREATED BY:    David L. Mitchell  09/18/13
;-
pro mvn_swe_spice_init, trange=trange, list=list, force=force, status=status

  @mvn_swe_com

  common mvn_spc_met_to_unixtime_com, cor_clkdrift, icy_installed, kernel_verified, $
         time_verified, sclk, tls

  if keyword_set(force) then noguff = 1 else noguff = 0

  if keyword_set(status) then begin
    mk = spice_test('*')
    indx = where(mk ne '', n_ker)
    if (n_ker eq 0) then begin
      print,"No kernels are loaded."
      return
    endif
    print,"Kernels in use:"
    for i=0,(n_ker-1) do print,"  ",file_basename(mk[i])
    return
  endif

  if (~noguff) then begin
    mk = spice_test('*')
    indx = where(mk ne '', n_ker)
    if (n_ker gt 0) then begin
      print,"SPICE kernels are already loaded."
      yn = 'N'
      read,"Reinitialize (y|n) ? ", yn
      if (strupcase(yn) ne 'Y') then return
    endif
  endif

  oneday = 86400D

  if (size(trange,/type) eq 0) then begin
    tplot_options, get_opt=topt
    if (max(topt.trange_full) gt time_double('2013-11-18')) then trange = topt.trange_full
    
    if (size(trange,/type) eq 0) then begin
      print,"You must supply a time range."
      return
    endif
  endif
  
  srange = minmax(time_double(trange)) + [-oneday, oneday]
  
  dprint, "Initializing SPICE ...", getdebug=old_dbug, setdebug=0

  if (noguff) then cspice_kclear ; remove any previously loaded kernels
  swe_kernels = mvn_spice_kernels(/all,/load,trange=srange,verbose=-1)
  swe_kernels = spice_test('*')  ; only loaded kernels, no wildcards
  n_ker = n_elements(swe_kernels)
  
  if keyword_set(list) then begin
    print, "Kernels in use: "
    for i=0,(n_ker-1) do print,"  ",file_basename(swe_kernels[i])
  endif

; Use common block settings to inform later routines that kernels have
; already been loaded, and they don't need to check again and print out
; a bunch of unnecessary diagnostics that can't be turned off.

  i = where(strpos(swe_kernels,'SCLK') ne -1, scnt)  ; spacecraft clock kernel
  j = where(strpos(swe_kernels,'tls') ne -1, tcnt)   ; leap seconds kernel
  
  if (scnt and tcnt) then begin
    kernel_verified = 1
    sclk = swe_kernels[i]
    tls = swe_kernels[j]
    time_verified = systime(1)
    msg = "Success"
  endif else begin
    kernel_verified = 0
    msg = "WARNING: no SPICE kernels!"
  endelse

  dprint, msg, setdebug=2

  return

end
