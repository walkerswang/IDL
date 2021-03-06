;***********************************************************************************
; THEMIS ESA CRIB
; Created by J. McFadden
; 07-04-24
; Comments: 
;	The bad ion data at energy ~1000 eV on "tha" and "thc" starting ~15:15 on 3-23-07 
;		are due to errors in the ETC table loads
;	Similar table load errors are present until 3-30-07
;	The discontinuity in counts at ~400 eV on all s/c at 11:10-15:15 on 3-23-07, especially
;		visible in "thc" is due to a 24 energy step sweep with twice the accumulation time
;		on the lower energy samples.


; select a time span

	startdate = '2007-03-23/0:00'
	ndays=1
	timespan,startdate,ndays

; load esa data

	thm_load_esa_pkt

; plot ESA ions reduced data

	tplot,['tha_peir_en_counts','thb_peir_en_counts','thc_peir_en_counts','thd_peir_en_counts','the_peir_en_counts'],title='THEMIS Reduced Ions'
	; or
	tplot,['th?_peir*'],title='THEMIS Reduced Ions'

; plot ESA ion full data

	tplot,['tha_peif_en_counts','thb_peif_en_counts','thc_peif_en_counts','thd_peif_en_counts','the_peif_en_counts'],title='THEMIS Full Ions'
	;or
	tplot,['th?_peif*'],title='THEMIS Full Ions'

; to get a single 3-d ion distribution

	window,1
	window,2
	ctime,t
	dat=get_tha_peif(t)
	wset,1 & spec3d,dat
	;switched plot3d to plot3d_new to avoid name conflict in IDL 8.1
	wset,2 & plot3d_new,dat
	print,'ion density  1/cc  = ',n_3d_new(dat)
	print,'ion velocity km/s  = ',v_3d_new(dat)
	print,'ion temperature eV = ',t_3d_new(dat)

; plot ESA electron reduced data

	tplot,['tha_peer_en_counts','thb_peer_en_counts','thc_peer_en_counts','thd_peer_en_counts','the_peer_en_counts'],title='THEMIS Reduced Electrons'
	;or
	tplot,['th?_peer*'],title='THEMIS Reduced Electrons'

; plot ESA electron full data

	tplot,['tha_peef_en_counts','thb_peef_en_counts','thc_peef_en_counts','thd_peef_en_counts','the_peef_en_counts'],title='THEMIS Full Electrons'
	;or
	tplot,['th?_peef*'],title='THEMIS Full Electrons'

; to get a single 3-d electron distribution

	ctime,t
	dat=get_tha_peef(t)
	wset,1 & spec3d,dat
	;switched plot3d to plot3d_new to avoid name conflict in IDL 8.1
	wset,2 & plot3d_new,dat
	print,'electron density  1/cc  = ',n_3d_new(dat)
	print,'electron velocity km/s  = ',v_3d_new(dat)
	print,'electron temperature eV = ',t_3d_new(dat)

end
