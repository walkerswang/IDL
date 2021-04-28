;+
; Function:  files = mvn_pfp_spd_download(PATHNAME)
; Purpose: Retrieve or Download MAVEN data files. (Can
;          be used to generate filenames too). Note that this is a
;          Hacked version of mvn_pfp_file_retrieve, to use for SPICE
;          kernels which are no longer able to be accessed via
;          file_retrieve, jmm, 2017-01-30
; INPUT:
; PATHNAME:  string specifying relative path to files. Default might change-  Currently:  'maven/pfp/l0/YYYY/MM/mvn_pfp_all_l0_YYYYMMDD_v???.dat'
;         PATHNAME must be relative to the LOCAL_DATA_DIR and REMOTE_DATA_DIR fields of the source keyword.
;         "globbed" filenames (*,?) are accepted.
; typical usage:
;   files = mvn_pfp_spd_download('maven/pfp/l0/YYYY/MM/mvn_pfp_all_l0_YYYYMMDD_v???.dat',/daily_names)   ; get L0 files for user defined time span
;   files = mvn_pfp_spd_download(pathname,/daily_names,trange=trange)  ; set time range
;Keywords:  (All are optional - none are recommended)
; L0:   set to 1 to return PFP L0 files
; DAILY_NAMES : resolution (in days) for generating file names. 
;         YYYY, yy, MM, DD,  hh,  mm, ss, .f, DOY, DOW, TDIFF are special characters that will be substituted with the appropriate date/time field
;         Be especially careful of extensions that begin with '.f' since these will be translated into a fractional second. 
;         See "time_string"  TFORMAT keyword for more info.
; TRANGE : two element vector containing start and end times (UNIX_TIME or UT string).  if not present then timerange() is called to obtain the limits.
; SOURCE:  alternate file source.   Default is whatever is return by the function:  mvn_file_source()    (see "mvn_file_source" for more info)
; FILES:  if provided these will be passed through as output.
; VALID_ONLY:  Set to 1 to prevent non existent files from being returned.
; CREATE_DIR:  Generates a filename and creates the directories needed to create the file without errors.  Will not check for file on remote server.
;
; KEYWORDS Passed on to "SPD_DOWNLOAD":
; LAST_VERSION : [0,1]  if set then only the last matching file is returned.  (Default is defined by source)
; VALID_ONLY:  [0,1]   If set then only existing files are returned.  (Default is defined by source keyword)
; VERBOSE:  set verbosity level (2 is typical)
; LIMITATIONS:
;   Beware of file pathnames that include the character sequences:
;   YY,  MM, DD, hh, mm, ss, .f  since these can be retranslated to
;   the time
; $LastChangedBy: jimm $
; $LastChangedDate: 2017-03-24 14:19:59 -0700 (Fri, 24 Mar 2017) $
; $LastChangedRevision: 23030 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/maven/general/mvn_pfp_spd_download.pro $
;-
function mvn_pfp_spd_download,pathname,trange=trange,verbose=verbose, source=src,files=files, $
   last_version=last_version,valid_only=valid_only,no_update=no_update,create_dir=create_dir,pos_start=pos_start, $
   remote_kp_cdf=remote_kp_cdf, $   
   insitu_kp_tab = insitu_kp_tab, $
   insitu_kp_cdf=insitu_kp_cdf, $
   daily_names=daily_names,hourly_names=hourly_names,resolution = res,shiftres=shiftres,  $
   no_server=no_server,user_pass=user_pass,L0=L0,recent=recent, $
   DPU=DPU,ATLO=ATLO,RT=RT,pformat=pformat,realtime=realtime,no_download=no_download,name=name

tstart = systime(1)

if keyword_set(recent) then trange = systime(1) - [recent,0] * 86400d ;    Obtain the last N*24 hours

if keyword_set(L0) || ~keyword_set(pathname) then begin   ; default location of L0 files
;   pathname = 'maven/pfp/l0/YYYY/MM/mvn_pfp_all_l0_YYYYMMDD_v???.dat'
   pathname = 'maven/data/sci/pfp/l0_all/YYYY/MM/mvn_pfp_all_l0_YYYYMMDD_v???.dat'
   daily_names=1
   last_version =1
endif

if keyword_set(insitu_kp_tab) then begin
   pathname = 'maven/data/sci/kp/insitu/YYYY/MM/mvn_kp_insitu_YYYYMMDD_v??_r??.tab'
   daily_names = 1
   last_version=1
endif

if keyword_set(remote_kp_cdf) then begin
   pathname = 'maven/kp/remote/sample_iuvs_kp/mvn_rs_kp_YYYYMMDDThh????_v???_r???.cdf'
   hourly_names = 1
   last_version=1
   valid_only=1
endif

if not keyword_set(shiftres) then shiftres =0
if keyword_set(daily_names) then begin 
   res = round(24*3600L * daily_names)
   sres= round(24*3600L * shiftres)
endif

if keyword_set(hourly_names) then begin
  res = round(3600L * hourly_names)
  sres= round(3600L * shiftres)
endif

;lv = n_elements(last_version) eq 0 ? 1 : last_version 
;vo = n_elements(valid_only) eq 0 ? 0 : valid_only

source = mvn_file_source(src,verbose=verbose,user_pass=user_pass,no_server=no_server,valid_only=valid_only,last_version=last_version,no_update=no_update)
if(keyword_set(no_download) or keyword_set(no_server)) then source.no_server = 1

pos_start = strlen(source.local_data_dir)

dprint,dlevel=5,verbose=verbose,phelp=1,source   ; display the options

if ~keyword_set(RT) then begin
  if ~keyword_set(files) then begin
    if keyword_set(res) then begin
      tr = timerange(trange)
      str = (tr-sres)/res
      dtr = (ceil(str[1]) - floor(str[0]) )  > 1           ; must have at least one file
      times = res * (floor(str[0]) + lindgen(dtr))+sres
      pathnames = time_string(times,tformat=pathname)
      pathnames = pathnames[uniq(pathnames)]   ; Remove duplicate filenames - assumes they are sorted
    endif else pathnames = pathname
    if keyword_set(create_dir) then begin
      files = source.local_data_dir + pathnames
      file_mkdir2,file_dirname( files ),_extra=source
      return,files
    endif
    nfiles = n_elements(pathnames)
    fc = 0
;Check for different remote paths, if all are the same, then we will
;not need a loop
    fdir = file_dirname(pathnames, /mark_directory)
    If(n_elements(fdir) gt 1) Then Begin
       If(n_elements(uniq(fdir)) Eq 1) Then same_dir = 1b $
       Else same_dir = 0b
    Endif Else same_dir = 1b
    If(same_dir) Then Begin
       files = spd_download_plus(remote_file = source.remote_data_dir+pathnames, $
                                 local_path = source.local_data_dir+fdir[0], $
                                 last_version = last_version, no_update = no_update, valid_only = valid_only, $
                                 no_server = source.no_server, file_mode = '666'o, dir_mode = '777'o)
    Endif Else Begin
       for j = 0, nfiles-1 do begin
          filesj = spd_download_plus(remote_file = source.remote_data_dir+pathnames[j], $
                                     local_path = source.local_data_dir+fdir[j], $
                                     last_version = last_version, no_update = no_update, valid_only = valid_only, $
                                     no_server = source.no_server, file_mode = '666'o, dir_mode = '777'o)
          if is_string(filesj) then begin
             if fc eq 0 then files = filesj else files = [files, filesj]
             fc = fc+1
          endif
       endfor
       if fc eq 0 then files = ''
    Endelse
    dprint,dlevel=3,verbose=verbose,systime(1)-tstart,' seconds to retrieve ',n_elements(files),' files'
  endif
  return,files
endif

return,files
end
