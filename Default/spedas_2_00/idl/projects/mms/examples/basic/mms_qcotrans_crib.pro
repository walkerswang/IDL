;+
;Procedure:
;  mms_qcotrans_crib
;
;Purpose:
;  Demonstrate usage of mms_qcotrans.
;
;Notes:
;  See also: mms_cotrans_crib
;  
;  Supported coordinate systems:
;    -BCS
;    -DBCS
;    -DMPA
;    -SMPA
;    -DSL
;    -SSL
;    -GSE
;    -GSE2000
;    -GSM
;    -SM
;    -GEO
;    -ECI
;    -J2000 (identical to ECI)
;    
;$LastChangedBy: egrimes $
;$LastChangedDate: 2016-11-07 10:53:48 -0800 (Mon, 07 Nov 2016) $
;$LastChangedRevision: 22326 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_2_00/projects/mms/examples/basic/mms_qcotrans_crib.pro $
;-

;------------------------------------------------------
; Load data & setup tplot variables 
;------------------------------------------------------
probe = '1'
level = 'l2'

timespan, '2015-10-16/12', 2, /hour
trange = timerange()

; load quaternions
mms_load_mec, probe=probe, trange=trange, varformat='*_quat_*'

; load data to be transformed
mms_load_fgm, probe=probe, trange=trange, level=level, varformat='*_b_*'
mms_load_fpi, probe=probe, trange=trange, level=level, datatype=['dis-moms'], varformat='*_bulk?_*'

; example variables to be transformed
v_name = 'mms'+probe+'_dis_bulkv_dbcs_fast'
b_name = 'mms'+probe+'_fgm_b_dmpa_srvy_l2_bvec'

;------------------------------------------------------
; Implicit transformations
;  -Input/output coordinates can be specified with IN/OUT_SUFFIX keywords
;  -Input coordinates can be omitted when metadata is present
;------------------------------------------------------

; transform to GSE
mms_qcotrans, [v_name, b_name], out_suffix='_gse'

tplot, [v_name, v_name, b_name, b_name] + ['','_gse','','_gse']

stop

;------------------------------------------------------
; Specify input suffix
;  -Replaces current suffix with that of new coordinates
;  -If any inputs' metadata do not match explicit input coordinates
;   then the transformation will be skipped
;------------------------------------------------------

; transform both _GSE variables to _SM
; note that input names do not include the _gse suffix
mms_qcotrans, [v_name, b_name], in_suffix='_gse', out_suffix='_sm'

tplot, [v_name, v_name, b_name, b_name] + ['_gse','_sm','_gse','_sm']

stop

;------------------------------------------------------
; Explicit transformations
;  -input/output coordinates can be specified independent of suffixes
;  -if metadata is incorrect or not present then use /IGNORE_DLIMITS to ignore
;------------------------------------------------------

; transform both original variables as though they are both in DMPA coordinates
; use /ignore_dlimits to ignore metadata for dbcs velocity
mms_qcotrans, [v_name,b_name], in_coord='dmpa', out_coord='gse', $
              out_suffix='_pseudo_gse', /ignore_dlimits

; plot against real GSE
; b field will be identical, velocity should be nearly identical
tplot, [v_name, v_name, b_name, b_name] + ['_gse','_pseudo_gse','_gse','_pseudo_gse']

stop


;------------------------------------------------------
; Specify full output name
;  -a second argument can be used to specify a full name for the output variable
;------------------------------------------------------

mms_qcotrans, v_name, 'v_gse2k', out_coord='gse2000'
mms_qcotrans, b_name, 'b_gse2k', out_coord='gse2000'

tplot, [v_name,'v_gse2k',b_name,'b_gse2k']


end