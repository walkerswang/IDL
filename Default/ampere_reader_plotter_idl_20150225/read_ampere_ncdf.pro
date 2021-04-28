pro read_ampere_ncdf,ncdfname,x_axis_frac_hour,pseudosvnum_total,plane_number_total,pos_eci_total,b_eci,pseudo_sv_quality,data_splice

  ; Haje Korth
  ; JHU/APL

  ; error handling.
  catch, theerror
  if (theerror ne 0) then begin
    catch, /cancel
    void = error_message()
    obj_destroy, fileobj
    return
  endif

;  ; initial definition of objects.
;  fileobj = obj_new()
;
;  ; open the source file in read-only mode.
;  fileobj = obj_new('ncdf_file', ncdfname)
;  if (obj_valid(fileobj) eq 0) then $
;    message, 'Error: Invalid file object returned from ncdf_file init.'
;
;  ; load variables
;  x_axis_frac_hour = fileobj -> getvardata('time')
;  pseudosvnum_total = fileobj -> getvardata('pseudo_sv_num')
;  plane_number_total = fileobj -> getvardata('plane_num')
;  pos_eci_total = fileobj -> getvardata('pos_eci')
;  b_eci = fileobj -> getvardata('b_eci')
;  pseudo_sv_quality = fileobj -> getvardata('pseudo_sv_quality')
;  data_splice = fileobj -> getvardata('data_splice')
;
;  ; destroy the file object.
;  obj_destroy, fileobj

  fileid=ncdf_open(ncdfname,/nowrite)
  varid=ncdf_varid(fileid,'time')
  ncdf_varget,fileid,varid,x_axis_frac_hour
  varid=ncdf_varid(fileid,'pseudo_sv_num')
  ncdf_varget,fileid,varid,pseudosvnum_total
  varid=ncdf_varid(fileid,'plane_num')
  ncdf_varget,fileid,varid,plane_number_total
  varid=ncdf_varid(fileid,'pos_eci')
  ncdf_varget,fileid,varid,pos_eci_total
  varid=ncdf_varid(fileid,'b_eci')
  ncdf_varget,fileid,varid,b_eci
  varid=ncdf_varid(fileid,'pseudo_sv_quality')
  ncdf_varget,fileid,varid,pseudo_sv_quality
  varid=ncdf_varid(fileid,'data_splice')
  ncdf_varget,fileid,varid,data_splice
 
  
  return
end