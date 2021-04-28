;+
; PROGRAM: erg_crib_gmag_mm210
;   This is an example crib sheet that will load 210 MM magnetometer data.
;   Open this file in a text editor and then use copy and paste to copy
;   selected lines into an idl window.
;   Or alternatively compile and run using the command:
;     .run erg_crib_gmag_mm210
;
; NOTE: See the rules of the road.
;       For more information, see http://stdb2.isee.nagoya-u.ac.jp/mm210/
;
; Written by: Y. Miyashita, Jun 16, 2010
;             ERG-Science Center, ISEE, Nagoya Univ.
;             erg-sc-core at isee.nagoya-u.ac.jp
;
;   $LastChangedBy: c0005miyashita $
;   $LastChangedDate: 2016-11-24 15:02:58 +0900 (Thu, 24 Nov 2016) $
;   $LastChangedRevision: 365 $
;   $URL: https://ergsc-local.isee.nagoya-u.ac.jp/svn/ergsc/trunk/erg/examples/erg_crib_gmag_mm210.pro $
;-

; initialize
thm_init

; set the date and duration (in days)
timespan, '2006-11-20'


; load 1 min resolution data
erg_load_gmag_mm210, site='msr rik', datatype='1min'

; view the loaded data names
tplot_names

; plot the H, D, and Z components
tplot, ['mm210_mag_*_1min_hdz']
stop

; load 1 sec resolution data
erg_load_gmag_mm210, site='msr rik', datatype='1sec'

; view the loaded data names
tplot_names

; plot the H, D, and Z components
tplot, ['mm210_mag_*_1sec_hdz']

end
