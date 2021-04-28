; docformat = 'rst'

;+
; This function takes in an array and extracts the first dimension
; into a list. It will handle 1, 2, 3 dimensional arrays.
;
; :Params:
;   in: in, required, type=array
;     This is the array input.
;-
function build_list, in
  compile_opt idl2
  rtn = list()

  if size(in, /N_DIMENSIONS) eq 2 then begin
    nOther = size(in, /DIMENSIONS)
    num_to_add = nOther[0]
    i_temp_second = indgen(nOther[1])
    for iToAdd = 0L, num_to_add-1 do rtn.add, reform(in[iToAdd, i_temp_second]) ; saving each time

  endif else if size(in, /N_DIMENSIONS) eq 3 then begin
    nOther = size(in, /DIMENSIONS)
    num_to_add = nOther[0]
    i_temp_second = indgen(nOther[1])
    i_temp_third = indgen(nOther[2])
    for iToAdd = 0L, num_to_add-1 do rtn.add, reform(in[iToAdd, i_temp_second, i_temp_third]) ; saving each time

  endif else begin
    rtn.add, in, /EXTRACT ; single array
  endelse

  return, rtn
end

;+
; This procedure reads a netCDF file with the GOLD format
; and returns a structure with the attributes and a
; separate structure with the data. If there is only one variable in the file
; then an array is returned.
;
; :Author:
;    Stephane Beland
;
; :Params:
;    infile: in, optional, type=string
;        Name of file to read in. The user will be prompted for
;        a file if not provided.
;    data: out, required, type=structure
;        Data structure generated from the content of the input file.
; :Keywords:
;    gattr: out, optional, type=structure
;        Structure returning the GLOBAL attributes from the netCDF file.
;    vattr: out, optional, type=LIST
;        LIST returning the VARIABLES attributes from the netCDF file for
;        each variable requested with the "items" parameters or all.
;    items: in, optional, type=strarr
;        Use this to specify a list of specific variables to return
;        ignoring all other variables. If empty, will return all variables.
;    ermsg: out, optional, type=string
;        Returned string with the error message (empty string if no errors).
;    nodata: in, optional, type=logical
;        If flag is set, only the global and variable attributes are returned.
;    list : in, optional, type=keyword
;        Use this keyword to return a structure of lists instead of a structure of
;        arrays. 
;
; :Example:
;    IDL> READ_GOLD_NCDF, 'test_file.nc', test_data, gattr=test_gattr
;
;- ---------------------------------------------------------------------------
PRO READ_GOLD_NCDF, infile, data, gattr=gattr, vattr=vattr, items=items, ermsg=ermsg, nodata=nodata, list=list


   CATCH, Error_status
   ;This statement begins the error handler:

   ermsg=''
   IF Error_status NE 0 THEN BEGIN
     mg_log, /error, /last_error
     mg_log, /error, ermsg=!ERROR_STATE.msg
     CATCH, /CANCEL
     return
   ENDIF

  if n_elements(infile) eq 0 then begin
    ; query for a file to read
    infile = dialog_pickfile(filter='*.nc')
    ; check if user hit the cancel button
    if strlen(infile) eq 0 then return
  endif

  infile=expand_path(infile)
  filebase = FILE_BASENAME(infile)

  ; make sure the file exists
  res=FILE_INFO(infile)
  if res.exists eq 0 then begin
    ermsg='File '+infile+' does not exist!'
    return
  endif
  if res.read eq 0 then begin
    ermsg='File '+infile+' can NOT be read!'
    return
  endif

  data=[]
  gattr=[]
  if H5F_IS_HDF5(infile) ne 1 then begin
    ; file is not ina netCDF format (which is HDF5)
    ermsg='File is not a netCDF format!'
    return
  endif
  nid = ncdf_open(infile, /nowrite)
  nid_inq = ncdf_inquire(nid)
  if n_elements(items) gt 0 then get_items=1 else get_items=0

  ; get the list of attributes and add them to the gattr structure
  for i=0, nid_inq.ngatts -1 do begin
    name = NCDF_ATTNAME(nid, /GLOBAL, i)
    NCDF_ATTGET, nid, /GLOBAL, name, value
    if n_elements(gattr) eq 0 then begin
      gattr = create_struct(gattr, name, temporary(value))
    endif else if (where(tag_names(gattr) eq STRUPCASE(name)))[0] eq -1 then begin
      ; verify we do not have duplicate names
      gattr = create_struct(gattr, name, temporary(value))
    endif
  endfor

  ; get the list of variables and add them to the data structure
  data=[]
  vattr=LIST()
  diminq=[]

  ; get the dimensions
  for i=0,nid_inq.ndims-1 do begin
    ncdf_diminq,nid,i,nn,ss
    diminq=[diminq, {name:nn, size:ss}]
  endfor

  ; if there is only one variable, then an array is returned instead of a structure.
  if nid_inq.nvars eq 1 then begin
    ; only one variable to return
    get_it=1
    res=ncdf_varinq(nid,0)
    if get_items then begin
      pos=where(strcmp(items, res.name, /fold_case) eq 1, count)
      if count eq 0 then get_it=0
    endif
    if get_it then begin
      if ~keyword_set(nodata) then begin
          ncdf_varget, nid, 0, data
          data=[data]
      endif
      ; get the attributes for this variable
      if res.natts gt 0 then begin
        attr = [] ; adding this line bv
        for i=0,res.natts-1 do begin
          attname=ncdf_attname(nid,0,i)
          ncdf_attget,nid,0,attname,value
          attr=create_struct(attr, attname,temporary(value))
        endfor
        vattr.ADD, create_struct(res, 'DIMENSIONS',diminq, 'ATTRIBUTES',attr)
      endif else vattr.ADD, !NULL
    endif
  endif else begin
    ; if more than one variable, organize the data in a structure
    for i=0, nid_inq.nvars-1 do begin
      get_it=1
      res=ncdf_varinq(nid,i)
      if get_items then begin
        pos=where(strcmp(items, res.name, /fold_case) eq 1, count)
        if count eq 0 then get_it=0
      endif
      if get_it then begin
        if ~keyword_set(nodata) then begin
          ; make sure we have data because ncdf_varget will crash with zero element array
          ; check all of the dimensions of the variable for a zero dimension
          !NULL = where( diminq[res.dim].SIZE eq 0, count )
          if count eq 0 then begin
            ncdf_varget, nid, i, value
            ; convert char arrays to strings
            if strcmp(res.DATATYPE, 'CHAR', /FOLD_CASE) then value = string(value)

            if keyword_set(list) then value = build_list(value)

            data=create_struct(data, res.name, temporary(value))
          endif else begin
            ; This will add an empty list for those varaibles that have a zero dimension
            if keyword_set(list) then data=create_struct(data, res.name, temporary(list()))
          endelse
        endif
        if res.natts gt 0 then begin
          attr=[]
          for j=0,res.natts-1 do begin
            attname=ncdf_attname(nid,i,j)
            ncdf_attget,nid,i,attname,value
            attr=create_struct(attr, attname,temporary(value))
          endfor
          vattr.ADD, create_struct(res, 'DIMENSIONS',diminq, 'ATTRIBUTES',attr)
        endif else vattr.ADD, !NULL
      endif
    endfor
  endelse

  ncdf_close, nid
  return

END
