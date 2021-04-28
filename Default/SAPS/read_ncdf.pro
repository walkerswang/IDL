; Copyright 2006 The Johns Hopkins University/Applied Physics Laboratory.
; All rights reserved.
;-------------------------------------------------------------
; $Id: read_ncdf.pro,v 1.5 2012/05/07 17:58:02 romeog1 Exp $
;+
; NAME:
;        READ_NCDF
;
; PURPOSE:
;        Open a netCDF file and read data from it. The data is
;        returned as a structure whose tag names are the names of
;        the variables with blanks etc. replaced. If no variables
;        are specified with the VARIABLES keyword, only dimensional
;        information is returned. All variables are loaded by default.
;        Other keyword options include OFFSET, COUNT, STRIDE,
;        NO_DIMENSIONS, NO_STRUCT, DIMNAMES, VARNAMES, VARDIMS, ATTRIBUTES.
;        Thus, this program includes ncdump functionality.
;
;        If no filename is given, a file selection dialog is opened with the
;        default mask '*.L*,*.nc*,*.NC*'. The name of the selected file is
;        returned in the TRUENAME keyword. A file selection dialog also appears
;        when the file cannot be found (see OPEN_FILE.PRO). This can be turned
;        off with the NO_DIALOG keyword. The VERBOSE keyword provides
;        information while analyzing and reading the file.
;
; AUTHOR:
;        Dr. Martin Schultz
;        Max-Planck-Institut fuer Meteorologie
;        Bundesstr. 55, D-20146 Hamburg
;        email: martin.schultz@dkrz.de
;
; CATEGORY:
;        File I/O
;
; CALLING SEQUENCE:
;        READ_NCDF, result, filename=<string>, truename=<string>,
;            variables=<stringarray>, all=<flag>, varnames=<stringarray>,
;            vardimid=<structure>, vardims=<structure>, attributes=<structure>,
;            count=<integerarray>, offset=<integerarray>, stride=<integerarray>,
;            dimnames=<stringarray>, dims=<longarray>, no_dimensions=<flag>,
;            no_struct=<flag>, no_dialog=<flag>, verbose=<flag>, title=<string>
;
; ARGUMENTS:
;        RESULT(out) -> a structure containing all the variable data
;             from the netCDF file. If only one variable is specified
;             and the NO_STRUCT keyword set, then RESULT will be an
;             array instead of a structure.
;             Note, that the COUNT, OFFSET,
;             and STRIDE keywords can affect the size of RESULT.
;             RESULT is set to -1L if an error occurs before the structure
;             has been built. You can use CHKSTRU.PRO to test for this.
;
; KEYWORD PARAMETERS:
;        FILENAME(in) -> the name of the netCDF file to be opened.
;             NCDF_READ uses OPEN_FILE to check the validity of
;             the file first. You can specify a search mask
;             instead of a filename in which case a file selection
;             dialog is displayed (unless you set the NO_DIALOG
;             keyword). The TRUENAME keyword contains the name
;             of the selected file or an empty string if the
;             file selection was canceled.
;
;        TRUENAME(out) -> the (fully qualified) name of the file selected
;             with the file selection dialog or an unaltered copy
;             of FILENAME if FILENAME is a valid filename.
;
;        VARIABLES(in) -> a string array containing the names of variables
;             for which data shall be read. Default is to read
;             only the dimensional information from the file.
;             (Currently) no warning is issued if a variable is not in the file.
;
;        ALL(in) -> set this keyword to load all variables stored in the
;             netCDF file. Generally, you cannot usethis keyword together with
;             COUNT, OFFSET, and STRIDE.
;
;        ASPOINTER(in) -> set this keyword to return the data and
;             attributes as structures of pointers rather than data
;             structures. This is more efficient if you read large
;             data sets (factor of 2 easily) and facilitates
;             manipulation of attribute values. If you use this
;             option, make sure to free the pointers at some point.
;
;        VARNAMES(out) -> a string array containing the names of all variables
;             as stored in the file. Note, that the tag names of e.g. the
;             result structure are filtered with the Valid_TagName function.
;
;        VARDIMID(out) -> a structure with integer arrays containing the
;             dimension ID's for all variables. See also VARDIMS which returns
;             the dimensions themselves.
;
;        VARDIMS(out) -> a structure with integer arrays containing the
;             dimensions for all variables in the netCDF file. These are not
;             kept in sync with potential COUNT, OFFSET, and STRIDE values,
;             but reflect the data sizes as stored in the file.
;
;        ATTRIBUTES(out) -> a structure holding the variable and global
;             attributes stored in the netCDF file (global attributes
;             are stored in tag name GLOBAL).
;
;        COUNT(in) -> an integer array containing the number of values to
;             be read for each dimension of the variables. Mapping of the
;             COUNT dimensions to the variable dimensions is achieved via
;             the first entry in the VARIABLES list and the COUNT parameter
;             will be applied to all variables that have that dimension.
;             Example: The first variable has dimensions LON, LAT, LEV,
;             the second variable has dimensions LON, LAT, and the third
;             variable has LAT, LEV. A COUNT of [40,20,10] would lead to
;             result dimensions of [40,20,10], [40,20], and [20,10].
;
;        OFFSET(in) -> an integer array containing the offsets for each
;             dimension of the variables to be read. Dimension mapping
;             is the same as for COUNT.
;
;        STRIDE(in) -> an integer array containing the stride for each
;             dimension of the variables to be read. Dimension mapping
;             is the same as for COUNT.
;
;        DIMNAMES(out) -> a string array containing the names of the
;             dimensions stored in the netCDF file.
;
;        DIMS(out) -> a long array containing the dimension values. Purely
;             for convenience. Use VARDIMS to retrieve the dimensions of
;             the variables.
;
;        TITLE(in) -> A title for the file selection dialog if an
;             incomplete or incorrect filename is specified. This
;             keyword is ignored if the no_dialog keyword is set.
;
;        NO_DIMENSIONS(in) -> set this keyword if you do not want to store
;             the dimensional variables from the file in the result structure.
;             DIMNAMES and DIMS will still be retrieved.
;
;        NO_STRUCT(in) -> if only one variable is selected with the
;             VARIABLE keyword, you can set this keyword to return only
;             the data for this variable as an array. This keyword implies
;             the functionality of NO_DIMENSIONS.
;
;        NO_DIALOG(in) -> set this keyword if you do not want interactive
;             behaviour when a file mask is given instead of a filename or
;             when the specified file does not exist.
;
;        VERBOSE(in) -> set this keyword to get detailed information while
;             reading the netCDF file.
;
; SUBROUTINES:
;        STRREPL (function)

;        Valid_TagName : replaces invalid characters in variable names so that
;            a structure can be built.
;
;        ncdf_attributes : retrieves global and variable attributes from netcdf
;            file and stores them as structure.
;
;        ncdf_dimensions : retrieves size and name for all dimensions in netcdf file.
;
;        ncdf_varnames : retrieves names and dimension information for all variables
;            in the netCDF file.
;
;        ncdf_mapdims : map dimension indices for COUNT, OFFSET, and STRIDE with
;            dimensions of first selected variable.
;
;        ncdf_TestDimensions : compute the COUNT, OFFSET, and STRIDE vectors that
;            must be applied for retrieving the data of one variable.

; REQUIREMENTS:
;        uses OPEN_FILE and STRREPL.
;
; NOTES:
;        Correct handling of dimensional information requires that the variables
;        storing the dimension values have the same name as the dimensions
;        themselves - a common feature in most netCDF standards.
;
;        I am still working on a netcdf file object which will be even
;        more powerful. At some point ncdf_read will only be a
;        procedure interface to this objec!
;
; EXAMPLE:
;        ncdf_read,result,/All
;        ; plot ozone vs. temperature
;        plot,result.temperature,result.ozone
;
; MODIFICATION HISTORY:
;        mgs, 18 Sep 1999: VERSION 1.00
;        mgs, 29 Feb 2000: - added variables keyword
;                          - added CATCH error handler
;        mgs, 21 Mar 2000: - bug fix for tag names
;        mgs, 09 May 2000: VERSION 2.00
;                          - now only reads dimensions as default
;                          - added ALL keyword to compensate
;                          - returns dimnames and attributes
;                            (makes ncdf_detail obsolete)
;                          - added COUNT, OFFSET and STRIDE keywords
;                          - added NO_DIMENSIONS and NO_DIALOG
;                            keywords and more
;        mgs, 22 Aug 2000: - added title keyword
;        mgs, 23 Nov 2001: - bug fix: attributes of type CHAR were
;                            returned as byte arrays.
;        mgs, 01 Dec 2001: - added aspointer keyword
;     06 Jun 2003: - fixed variable naming error to convert variables
;                            beginning with numbers D. Morrison
;
;-
;-------------------------------------------------------------
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright ï¿½ 1999-2000, Martin Schultz, Max-Planck Institut fuer
; Meteorologie, Hamburg
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;-------------------------------------------------------------
;        STRREPL (function)
;
; PURPOSE:
;        Replace all occurences of one character in a string
;        with another character. The character to be replaced
;        can either be given as string of length 1 or as an
;        index array containing the character positions
;        (see strwhere). This function also works for string arrays.
function strrepl,str,fromchar,tochar,   $
  IGNORECASE=ignorecase, FOLD_CASE=fold_CASE, $
  SILENT=silent

  ON_ERROR,2    ; return to caller

  ; argument testing
  if n_params() lt 3 then begin
    message,'Usage: strrepl,str,fromchar,tochar[,/IGNORECASE])'
  endif

  ; make working copy of string and convert to a byte array
  bstr = byte(string(str))

  ; fromchar is given as character
  if size(fromchar,/TYPE) eq 7 then begin
    ; ignore case?
    if keyword_set(ignorecase) OR keyword_set(fold_case) then begin
      ; call strrepl recursively w/o the IGNORE_CASE keyword
      res1 = strrepl(str,strupcase(fromchar),tochar)
      res2 = strrepl(res1,strlowcase(fromchar),tochar)
      return,res2
    endif else begin
      ; find all character occurences
      ; must be a single character - use the first
      bfc = (byte(fromchar))[0]
      ; go and search
      w = where(bstr eq bfc,count)
      ; if not found, return original string
      if count eq 0 then return,str
    endelse
  endif else begin
    ; fromchar is already an index array
    w = long(fromchar)
  endelse

  ; make sure index is in range
  test = where(w lt 0 OR w ge n_elements(bstr),tcount)
  if tcount gt 0 then begin
    IF (keyword_set(silent) EQ 0 ) THEN message,'WARNING: Index out of range!',/Continue
    ; restrict to valid index values
    test = where(w ge 0 AND w lt n_elements(bstr),tcount)
    if tcount gt 0 then begin
      w = w[test]
    endif else begin
      ; no valid indices: return original string
      return,str
    endelse
  endif

  ; convert tochar to a byte value
  btc  = (byte(tochar))[0]

  ; replace
  bstr[w] = btc

  ; return result as string
  return,string(bstr)

end


;=============================================================
; Valid_TagName: replace invalid characters in netCDF variable names
;   to allow building of a structure.

function Valid_TagName, arg


  ; If the name starts with a number, prepend it with an underscore.
  if (strpos(arg, '0') EQ 0) then arg = strcompress("_"+arg)
  if (strpos(arg, '1') EQ 0) then arg = strcompress("_"+arg)
  if (strpos(arg, '2') EQ 0) then arg = strcompress("_"+arg)
  if (strpos(arg, '3') EQ 0) then arg = strcompress("_"+arg)
  if (strpos(arg, '4') EQ 0) then arg = strcompress("_"+arg)
  if (strpos(arg, '5') EQ 0) then arg = strcompress("_"+arg)
  if (strpos(arg, '6') EQ 0) then arg = strcompress("_"+arg)
  if (strpos(arg, '7') EQ 0) then arg = strcompress("_"+arg)
  if (strpos(arg, '8') EQ 0) then arg = strcompress("_"+arg)
  if (strpos(arg, '9') EQ 0) then arg = strcompress("_"+arg)
  t = strrepl(arg,'-','_')
  t = strrepl(t,'+','P')
  t = strrepl(t,'$','D')
  t = strrepl(t,'*','S')
  t = strrepl(t,'&','_')
  t = strrepl(t,' ','_')
  t = strrepl(t,'@','_')
  t = strrepl(t,'.','_')
  return, t

end


;=============================================================
; ncdf_attributes: retrieve global or variable attributes from
; netCDF file. If keyword GLOBAL is not set, a variable ID must
; be supplied.
; The result is a structure with all global attributes or all
; attributes for one variable.

function ncdf_attributes, ncid, varid, global=global, verbose=verbose

  result = ''

  ; a little error checking
  if n_elements(varid) eq 0 and not keyword_set(global) then begin
    message,'Must supply VARID if keyword GLOBAL not set.',/Continue
    return, result
  endif

  ; get basic information about netCDF file
  nstru = NCDF_INQUIRE(ncid)

  ; determine number of attributes to be read
  if keyword_set(global) then begin
    natts = nstru.ngatts
  endif else begin
    vardesc = NCDF_VARINQ(ncid, varid)
    natts = vardesc.natts
  endelse

  if keyword_set(verbose) then begin
    if not keyword_set(global) then begin
      print, 'Attributes for variable ',vardesc.name
      prefix = '    '
    endif else begin
      prefix = 'Global attribute'
    endelse
  endif

  for i=0L,natts-1 do begin
    if keyword_set(global) then begin
      aname = NCDF_ATTNAME(ncid,/GLOBAL,i)
      NCDF_ATTGET,ncid, /GLOBAL, aname, avalue
      ainfo = NCDF_ATTINQ(ncid, /GLOBAL, aname)
      atype = ainfo.datatype
    endif else begin
      aname = NCDF_ATTNAME(ncid, varid, i)
      NCDF_ATTGET, ncid, varid, aname, avalue
      atype = size(avalue, /TNAME)
    endelse

    ; take care of IDL bug: CHAR is stored as BYTE
    ; assume all BYTES are CHARS (bug fixed in IDL5.3)
    if (atype eq 'BYTE' OR atype EQ 'CHAR') then $
      avalue = string(avalue)

    ; build or add to result structure
    newname = Valid_TagName(aname)
    if (i eq 0) then $
      result = create_struct( newname, avalue ) $
    else $
      result = create_struct( result, newname, avalue )

    if keyword_set(verbose) then begin
      print,prefix+aname+': '+avalue
    endif
  endfor

  return, result
end


;=============================================================
; ncdf_dimensions: retrieve dimension sizes and names from a
; netCDF file.

function ncdf_dimensions, ncid, names=names, verbose=verbose

  dims = -1L
  names = ''

  ; get basic information about netCDF file
  nstru = NCDF_INQUIRE(ncid)

  ; print,' ID of unlimited dimension : ',nstru.recdim
  ; print,'ID of time dimension : ',NCDF_DIMID(ncid, 'time')

  dims = lonarr(nstru.ndims)
  names = strarr(nstru.ndims)
  for i=0L,nstru.ndims-1 do begin
    NCDF_DIMINQ,ncid,i,dname,dsize
    names[i] = dname
    dims[i] = dsize
    if keyword_set(verbose) then $
      print,'Dimension '+strtrim(i,2)+': ',dname,dsize
  endfor

  return, dims
end


;=============================================================
; ncdf_varnames: retrieve variable names from a netCDF file.
; this function also returns the variable dimension id's.

function ncdf_varnames, ncid, VarDimId=vardimid

  dims = -1L
  names = ''

  ; get basic information about netCDF file
  nstru = NCDF_INQUIRE(ncid)

  for i=0L,nstru.nvars-1 do begin
    vardesc = NCDF_VARINQ(ncid, i)
    if keyword_set(verbose) then print,'Variable '+vardesc.name+'  Dim-IDs: '  $
      + string(vardesc.dim,format='(12i6)')

    if i eq 0 then begin
      names = vardesc.name
      vardimid = create_struct( Valid_TagName(vardesc.name), vardesc.dim )
    endif else begin
      names = [ names, vardesc.name ]
      vardimid = create_struct( vardimid, Valid_TagName(vardesc.name), vardesc.dim )
    endelse
  endfor

  return, names
end


;=============================================================
; ncdf_mapdims: check compatibility of variable dimensions
;    with count, offset, and stride values. The dimensionality
;    for each of count, offset, and stride must be same as for
;    the variable (the first one asked for) or it can be 0,
;    i.e. the respective parameter is undefined.
;    If these conditions are met, the variable's vardimid array
;    establishes the link between the parameter dimension and the
;    physical dimension, otherwise a scalar -1L is returned.

function ncdf_mapdims,varname, vardimid,  $
  count=count, offset=offset, stride=stride

  result = -1L

  nvdim = n_elements(vardimid)  ; dimensionality of variable
  nc = n_elements(count)
  no = n_elements(offset)
  ns = n_elements(stride)

  testc = (nc eq 0 OR nc eq nvdim)
  testo = (no eq 0 OR no eq nvdim)
  tests = (ns eq 0 OR ns eq nvdim)
  testnull = ( (nc > no > ns ) eq 0 )

  ok = ( testc AND testo AND tests ) AND not testnull
  if ok then result = vardimid

  return, result

end


;=============================================================
; ncdf_TestDimensions: check compatibility of variable dimensions
;    with count, offset, and stride values. The this... keyword
;    return valid entries for these parameters while the original
;    parameters remain unchanged.

function ncdf_TestDimensions, ncid, index, dims, mapdims, $
  count=count, offset=offset, stride=stride,  $
  thiscount=thiscount, thisoffset=thisoffset, thisstride=thisstride

  ; catch,/cancel
  result = 0    ; not compatible

  ; get variable information
  vardesc = NCDF_VARINQ(ncid, index)

  ; create default values
  ndims = n_elements(vardesc.dim)
  thiscount = dims[vardesc.dim]
  thisoffset = lonarr(ndims)
  thisstride = lonarr(ndims)+1L

  ; print,'variable ',vardesc.name
  ; print,'default thiscount=',thiscount
  ; print,'default thisoffset=',thisoffset
  ; print,'default thisstride=',thisstride

  for i=0L, n_elements(offset)-1 do begin
    w = where(vardesc.dim eq mapdims[i])
    ; print,'offset dimension ',i,' matches data dimension ',w
    if w[0] ge 0 then begin
      thisoffset[w] = offset[i] < (thiscount[w]-1)
      ; print,'new thisoffset=',thisoffset
      result = 1
    endif
  endfor

  for i=0L, n_elements(stride)-1 do begin
    w = where(vardesc.dim eq mapdims[i])
    ; print,'stride dimension ',i,' matches data dimension ',w
    if w[0] ge 0 then begin
      thisstride[w] = stride[i] > 1
      ; print,'new thisstride=',thisstride
      result = 1
    endif
  endfor

  for i=0L, n_elements(count)-1 do begin
    w = where(vardesc.dim eq mapdims[i])
    ; print,'count dimension ',i,' matches data dimension ',w
    if w[0] ge 0 then begin
      thiscount[w] = count[i] < ( (thiscount[w]-thisoffset[w])/thisstride[w] )
      thiscount[w] = thiscount[w] > 1
      ; print,'new thiscount=',thiscount
      result = 1
    endif
  endfor


  return, result

end

FUNCTION RSTRPOS, Expr, SubStr, Pos
  ON_ERROR, 2
  N = N_PARAMS()
  if (n lt 2) then message, 'Incorrect number of arguments.'

  ; Is expr an array or a scalar? In either case, make a result
  ; that matches.
  if (size(expr, /n_dimensions) eq 0) then result = 0 $
  else result = make_array(dimension=size(expr,/dimensions), /INT)

  RSubStr = STRING(REVERSE(BYTE(SubStr)))       ; Reverse the substring

  for i = 0L, n_elements(expr) - 1 do begin
    Len = STRLEN(Expr[i])
    IF (N_ELEMENTS(Pos) EQ 0) THEN Start=0 ELSE Start = Len - Pos

    RString = STRING(REVERSE(BYTE(Expr[i])))    ; Reverse the string

    SubPos = STRPOS(RString, RSubStr, Start)
    IF SubPos NE -1 THEN SubPos = Len - SubPos - STRLEN(SubStr)
    result[i] = SubPos
  endfor

  RETURN, result
END

;-------------------------------------------------------------

function extract_filename,fullname,filepath=thisfilepath

  ; determine path delimiter
  if (!version.os_family eq 'Windows') then sdel = '\' else sdel = '/'

  filename = ''
  thisfilepath = ''

  retry:
  ; look for last occurence of sdel and split string fullname
  p = rstrpos(fullname,sdel)

  ; extra Windows test: if p=-1 but fullname contains '/', retry
  if (p lt 0 AND strpos(fullname,'/') ge 0) then begin
    sdel = '/'
    goto,retry
  endif


  if (p ge 0) then begin
    thisfilepath = strmid(fullname,0,p+1)
    filename = strmid(fullname,p+1,strlen(fullname)-1)
  endif else $
    filename = fullname


  return,filename

end

;-------------------------------------------------------------

pro open_file,filemask,lun,filename=filename,  $
  write=write, update=update,  $
  result=result, $
  pickfile=pickfile,title=title,defaultmask=defaultmask, $
  no_pickfile=no_pickfile,verbose=verbose,  $
  F77_Unformatted=F77_Unformatted,   $
  Swap_Endian=Swap_endian,_EXTRA=e



  FORWARD_FUNCTION extract_filename


  ; fail safe : set result to error condition first
  result = -1
  ; reset error state
  message,/reset

  filename = ''
  if (n_elements(lun) eq 0) then lun = -1

  ; error check
  ON_ERROR,2
  if (n_params() lt 2) then begin
    message,'Procedure must be called with 2 parameters (FILENAME,ILUN)!'
  endif

  ; ============================================================
  ; set standard search mask and see if DIALOG_PICKFILE shall
  ; be called
  ; ============================================================

  if (n_elements(defaultmask) gt 0) then $
    stdmask = defaultmask[0] $
  else begin
    stdmask = '*'
    if (strupcase(!version.os_family) eq 'WINDOWS') then stdmask = '*.*'
  endelse

  if (n_elements(filemask) eq 0) then filemask = stdmask
  if (filemask eq '') then filemask = stdmask


  ; if filemask contains wildcards, always use pickfile dialog
  ; Abort if NO_PICKFILE is set
  if (strpos(filemask,'*') ge 0 OR strpos(filemask,'?') ge 0) then begin
    if (keyword_set(NO_PICKFILE)) then begin
      message,'Filename must not contain wildcards when '+ $
        'NO_PICKFILE option is set!',/CONT
      lun = -1   ; yet another error indicator
      return
    endif
    pickfile = 1
  endif

  ; make working copy of filemask (will be overwritten by PICKFILE dialog)
  thisfilename = filemask


  ; ============================================================
  ; set up parameters for DIALOG_PICKFILE
  ; ============================================================

  if (keyword_set(pickfile)) then begin
    ; seperate filename from filepath
    fname = extract_filename(filemask,filepath=path)

    ; if filename contains wildcards, put them to filemask and
    ; set filename to empty string
    ; if not (pickfile keyword set), then set standard search mask
    if (strpos(fname,'*') ge 0 OR strpos(fname,'?') ge 0) then begin
      fmask = fname
      fname = ''
    endif else begin
      fmask = stdmask
    endelse

    ; set dialog title
    if (n_elements(title) eq 0) then title = 'Choose a file'

    ; print,'### fname, path=>',fname,'<>',path,'<>',expand_path(path),'<'
    ; call pickfile dialog
    thisfilename = dialog_pickfile(file=fname,path=expand_path(path),  $
      filter=fmask, $
      title=title, $
      must_exist=(1 - keyword_set(WRITE)) )

    if (thisfilename eq '') then begin   ; cancel button pressed (?)
      lun = -1   ; yet another error indicator
      ; note that !error_state.code should be 0
      return
    endif
  endif


  ; print,'#DEBUG: little_endian = ',little_endian()

  ; Now try to open the file
  if (lun le 0) then get_lun,lun

  on_ioerror, openerr
  if (keyword_set(WRITE)) then $
    openw,lun,thisfilename,F77=F77_Unformatted, $
    Swap_Endian=Swap_Endian,_EXTRA=e   $
  else if keyword_set(update) then $
    openu,lun,thisfilename,F77=F77_Unformatted, $
    Swap_Endian=Swap_Endian,_EXTRA=e  $
  else  $
    openr,lun,thisfilename,F77=F77_Unformatted, $
    Swap_Endian=Swap_Endian,_EXTRA=e

  ; return parameters
  filename = expand_path(thisfilename)
  result = 0
  return

  openerr:
  result = !Error_State.Code
  lun = -1

  if (keyword_set(Verbose) OR not keyword_set(NO_PICKFILE)) then begin
    ; display error message
    dum=dialog_message(['Error opening file ',thisfilename+'!',  $
      !Error_State.sys_msg],/ERROR)
    ; try again
    if (not keyword_set(NO_PICKFILE) and not keyword_set(PICKFILE)) then $
      open_file,filemask,lun,filename=filename,result=result, $
      update=update,/pickfile,title=title,  $
      defaultmask=defaultmask,_EXTRA=e
  endif

  return

end

;=============================================================

pro READ_NCDF, result, filename=filename, truename=truename,    $
  variables=variables, all=all, attributes=attributes,        $
  varnames=varnames, vardimid=vardimid, vardims=vardims,      $
  dimnames=dimnames, dims=dims, title=title,                  $
  no_dimensions=no_dimensions, no_struct=no_struct,           $
  no_dialog=no_dialog, aspointer=aspointer, verbose=verbose,  $
  dimensions=dimensions
  ;               count=count, offset=offset, stride=stride,                  $


  ; initialize
  load_any = Arg_Present(result)   ;; avoid unnecessary pointers
  result = -1L
  ilun = -1
  truename = ''
  dimnames = ''
  dims = -1L
  ErrMsg = '<unknown error>'
  IF N_Elements(title) EQ 0 THEN title = 'Open NetCDF file:'
  ; attributes always read
  attributes = 0L
  ; error handler
  catch, theError
  if theError ne 0 then begin
    catch,/Cancel
    if ilun gt 0 then free_lun,ilun
    if n_elements(truename) eq 0 then begin
      if n_elements(filename) gt 0 then $
        truename = filename  $
      else $
        truename = '<unknown>'
    endif

    Message,ErrMsg,/Continue
    return
  endif

  ; argument and keyword checking
  if (keyword_set(no_struct) AND n_elements(variables) ne 1) then begin
    message,'Keyword NO_STRUCT only valid if 1 variable selected.',/Continue
    return
  endif

  ; if no filename is passed we set '*.nc' as file mask
  ;Change this to '*.nc*; *.L*' as default - D. Morrison 6/25/03
  ;Changed to '*.nc*;*.NC*;*.L*' as default - B. Wolven 2009-03-04
  if (n_elements(filename) eq 0) then $
    filename = '*.nc*;*.NC*;*.L*'
  ; for safety, we open the file first with OPEN_FILE
  ErrMsg = 'Error opening netCDF file '+filename
  open_file,filename,ilun,/BINARY,filename=truename,  $
    title=title,no_pickfile=no_dialog

  IF ilun le 0 THEN return

  if keyword_set(verbose) then $
    print,'Selected file ',truename
  free_lun,ilun

  ; now we know filename exists, so we can use NCDF_OPEN:
  ErrMsg = 'Error opening netCDF file '+truename
  id = NCDF_OPEN(truename)

  ErrMsg = 'Error reading netCDF file '+truename
  ; first find out about the file contents
  nstru = NCDF_INQUIRE(id)

  ; read dimensions from netCDF file
  dims = ncdf_dimensions(id, names=dimnames, verbose=verbose)

  ; retrieve the variable names and their dimension indices
  varnames = ncdf_varnames(id, vardimid=vardimid)

  ; return dimension names and sizes in a structure D. Morrison
  dimensions=create_struct("Names",dimnames,"Sizes",dims)

  ; service function: convert dimids to variable dimensions
  vardims = vardimid
  for i=0L, n_tags(vardimid)-1 do begin
    vardims.(i) = dims[vardimid.(i)]
    if keyword_set(verbose) then $
      print,'Variable '+varnames[i]+' : '+string(vardims.(i),format='(12i6)')
  endfor

  ; retrieve global and variable attributes (but only if requested)
  ;if arg_present(attributes) then begin
  if n_elements(attributes) gt 0L then begin
    gattr = ncdf_attributes(id, /Global, verbose=verbose)
    IF Keyword_Set(aspointer) THEN BEGIN
      attributes = create_struct( 'GLOBAL', Ptr_New(gattr) )
    ENDIF ELSE BEGIN
      attributes = create_struct( 'GLOBAL', gattr)
    ENDELSE

    for i=0L,nstru.nvars-1 do begin
      vattr = ncdf_attributes(id, i, verbose=verbose)
      IF Keyword_Set(aspointer) THEN BEGIN
        attributes = create_struct( attributes, Valid_TagName(varnames[i]), Ptr_New(vattr))
      ENDIF ELSE BEGIN
        attributes = create_struct( attributes, Valid_TagName(varnames[i]), vattr)
      ENDELSE
    endfor
  endif

  ; return variables names and sizes in a structure D. Morrison
  vars=create_struct("Names",varnames,"Dimensions",vardims,"Dimension_ID",vardimid)

  ; if at least one variable name is specified, use the first one to map
  ; potential offset, count, stride parameters to physical dimensions.
  ; This caused me some headache because I hadn't expected to see the dimensions
  ; shuffled around but they do ;-)
  ; set default (disallow use of offset, count, and stride)
  mapdims = -1L
  if n_elements(variables) gt 0 then begin
    w = where( StrUpCase(varnames) eq StrUpCase(variables[0]), wcnt )
    if wcnt eq 1 then begin   ; found it
      w = w[0]
      mapdims = ncdf_mapdims(varnames[w], vardimid.(w),  $
        count=count, offset=offset, stride=stride)
    endif
  endif

  ; initialize variable counter and result variable
  vcount = 0L
  result = { nothing : -1L }

  ; start building result structure with dimension variables
  ; unless NO_DIMENSIONS is set
  if keyword_set(no_dimensions) EQ 0 AND load_any then begin
    for i=0L,n_elements(dimnames)-1 do begin
      w = where(StrUpCase(varnames) eq StrUpCase(dimnames[i]), wcnt)
      if wcnt eq 1 then begin
        w = w[0]
        if keyword_set(verbose) then $
          print,'Loading data for variable '+varnames[w]+' ...'
        ; decide whether simple read is feasible
        if mapdims[0] lt 0 then begin
          NCDF_VARGET, id, w, data
        endif else begin
          ; need to find out how to offset, count, and stride the data
          ok = ncdf_TestDimensions(id, w, dims, mapdims, $
            count=count, offset=offset, stride=stride,   $
            thiscount=thiscount, thisoffset=thisoffset, thisstride=thisstride)
          NCDF_VARGET,id, w, data, count=thiscount, offset=thisoffset, stride=thisstride
        endelse
        ; create structure or add to it
        IF vcount EQ 0L THEN BEGIN
          IF Keyword_Set(aspointer) THEN BEGIN
            result = create_struct( Valid_Tagname(varnames[w]), Ptr_New(data) )
          ENDIF ELSE BEGIN
            result = create_struct( Valid_Tagname(varnames[w]), data )
          ENDELSE
        ENDIF ELSE BEGIN
          IF Keyword_Set(aspointer) THEN BEGIN
            result = create_struct( result, Valid_Tagname(varnames[w]), Ptr_New(data) )
          ENDIF ELSE BEGIN
            result = create_struct( result, Valid_Tagname(varnames[w]), data )
          ENDELSE
        ENDELSE
        data = 0
        vcount = vcount + 1L
        if keyword_set(verbose) then $
          print,'OK. Data dimensions are '+string(size(data,/Dimensions),format='(12i6)')
      endif
    endfor
  endif


  ; see which variables were requested and go through them

  if n_elements(variables) gt 0 AND load_any then dovars = variables else dovars = varnames
  ;if keyword_set(all) AND load_any then dovars = varnames

  for i=0L,n_elements(dovars)-1 do begin
    ; check if variable has already been added
    validname = Valid_TagName(dovars[i])
    test = where(StrUpCase(Tag_Names(result)) eq StrUpCase(validname))
    if test[0] lt 0 then begin
      w = where(StrUpCase(varnames) eq StrUpCase(dovars[i]), wcnt)
      if wcnt eq 1 then begin
        w = w[0]
        if keyword_set(verbose) then $
          print,'Loading data for variable '+varnames[w]+' ...'
        ; decide whether simple read is feasible
        if mapdims[0] lt 0 then begin
          NCDF_VARGET, id, w, data
        endif else begin
          ; need to find out how to offset, count, and stride the data
          ok = ncdf_TestDimensions(id, w, dims, mapdims, $
            count=count, offset=offset, stride=stride,   $
            thiscount=thiscount, thisoffset=thisoffset, thisstride=thisstride)
          NCDF_VARGET,id, w, data, count=thiscount, offset=thisoffset, stride=thisstride
        endelse
        ; create structure or add to it
        IF vcount EQ 0L THEN BEGIN
          IF Keyword_Set(aspointer) THEN BEGIN
            result = create_struct( validname, Ptr_New(data) )
          ENDIF ELSE BEGIN
            result = create_struct( validname, data )
          ENDELSE
        ENDIF ELSE BEGIN
          IF Keyword_Set(aspointer) THEN BEGIN
            result = create_struct( result, validname, Ptr_New(data) )
          ENDIF ELSE BEGIN
            result = create_struct( result, validname, data )
          ENDELSE
        ENDELSE
        vcount = vcount + 1L
        data = 0
        if keyword_set(verbose) then $
          print,'OK. Data dimensions are '+string(size(data,/Dimensions),format='(12i6)')
      endif
    endif
  endfor

  ; add attributes structure and dimension names and variables onto result
  IF Keyword_Set(aspointer) THEN BEGIN
    result = create_struct( result, 'Attributes', Ptr_New(attributes), 'Dimensions', Ptr_New(dimensions), 'Variables', Ptr_New(vars))
  ENDIF ELSE BEGIN
    result = create_struct( result, 'Attributes', attributes,  'Dimensions',dimensions, 'Variables', vars )
  ENDELSE


  ; close netCDF file
  NCDF_CLOSE,id

  if keyword_set(verbose) then $
    print, "File read"

  ; extract the only requested variable if the no_struct keyword is set
  if keyword_set(no_struct) then begin
    nt = tag_names(result)
    test = strupcase(Valid_TagName(variables[0]))
    w = where(nt eq test,wcnt)
    if wcnt eq 1 then begin
      result = result.(w[0])
    endif else begin
      result = -999.
    endelse
  endif

  return
end
