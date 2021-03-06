;+
;Procedure: ACE_MAG_SWEPAM_LOAD
;
;Purpose:  Loads ACE Magnetometer Values in GSM coordinates (1 minute averages) and/or
;          Solar Wind Electron Proton Alpha Monitor data
;
;keywords:
;   datatype = (Optional) type of data ('mag', 'swepam', 'all'), default is 'all' and includes
;              all the ascii type data files for ACE (mag and swepam)
;   trange = (Optional) Time range of interest (2 element array), default is tplot trange
;   downloadonly = (Optional) Set this to 1 to retrieve the data but not load it into
;                             tplot variables
;   verbose = (Optional) Set this keyword to get more messages, default is off
;   source = (Optional) for directories different from the default ACE data directory
;   tplotnames = (Optional) tplot variable names
;
;Example:
;   ace_mag_swepam_load
;Notes:
;
; Author: Cindy Goethel
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2009-05-19 11:14:44 -0700 (Tue, 19 May 2009) $
; $LastChangedRevision: 5884 $
; $URL $
;-

pro ace_mag_swepam_load, datatype=datatype, trange=trange, downloadonly=downloadonly, $
                         addmaster=addmaster, verbose=verbose, tplotnames=tn, source_options=source

  ace_init

  if not keyword_set(source) then source = !ace
  if not keyword_set(datatype) then datatype = 'all'
  if keyword_set(datatype) then Begin
      if datatype ne 'mag' and datatype ne 'swepam' and datatype ne 'all' then Begin
        print, 'Incorrect datatype specified.'
        print, "Valid datatypes for ACE ascii data include: ('mag', 'swepam', 'all')"
        if obj_valid(progobj) then progobj -> update, 0.0, $
             text = string("Incorrect datatype specified - valid datatypes are ('mag', 'swepam', 'all')")
        return
      endif
  endif
  
  datatype = strlowcase(datatype)
  if not keyword_set(trange) then trange = timerange(/current)

  if datatype ne 'all' then Begin
      pathformat = 'ace/'+datatype+'/k0/YYYY/YYYYMMDD_ace_'+datatype+'_1m.txt'
      relpathnames = file_dailynames(file_format=pathformat,trange=trange,addmaster=addmaster)
  endif else Begin
      pathformat = 'ace/mag/k0/YYYY/YYYYMMDD_ace_mag_1m.txt'
      magpathnames = file_dailynames(file_format=pathformat,trange=trange,addmaster=addmaster)
      pathformat = 'ace/swepam/k0/YYYY/YYYYMMDD_ace_swepam_1m.txt'
      swepathnames = file_dailynames(file_format=pathformat,trange=trange,addmaster=addmaster)
      relpathnames = [magpathnames, swepathnames]
  endelse

  files = file_retrieve(relpathnames, local_data_dir=!ace.local_data_dir, $
                            remote_data_dir=!ace.remote_data_dir)

  if keyword_set(downloadonly) then return

  n_files=n_elements(files)
  for i=0, n_files-1 do Begin
     if file_test(files(i)) eq 0 then Begin
         dprint,dlevel=1, verbose=verbose, string('ACE file not found: '+files(i))
         if obj_valid(progobj) then progobj -> update, 0.0, $
             text = string('ACE file not found: '+files(i))
         print, 'ACE file not found: '+files(i)
     endif else Begin
         print, 'ACE file '+files(i)+' retrieved.'
         if obj_valid(progobj) then progobj -> update, 0.0, $
              text = string('ACE file '+files(i)+' retrieved.')
         if (strpos(files(i), 'mag') ne -1) then Begin
            filetype='mag'
            data_start=20
         endif else Begin
            filetype='swepam'
            data_start=18
         endelse
         data=read_ascii(files(i), data_start=data_start)
         print, 'Loading ACE file '+files(i)
         if obj_valid(progobj) then progobj -> update, 0.0, $
              text = string('Loading ACE file '+files(i))         
         ace2tplot, data, filetype
     endelse
  endfor

  print, 'Done loading ACE files.'
  if obj_valid(progobj) then progobj -> update, 0.0, $
        text = string('Done loading ACE files.')         

end


