;------------------------------------------------------------------------------
;  load_jetct.pro = Load the Matlab Jet color table.
;  R. Sterner, 2013 Feb 28 --- Fromn data emailed by Shawn Johnson.
;------------------------------------------------------------------------------

        pro load_jetct, smooth=smth, help=hlp

        if keyword_set(hlp) then begin
          print,' Load the Matlab Jet color table.'
          print,' load_jetct'
          print,' Keywords:'
          print,'   /SMOOTH Optionally smooth the table a bit.'
          print,' Notes:'
          print,'   Does device,decomp=0'
          return
        endif

        text_block, txt, /quiet
;      r0       g0        b0
;      f        f         f
;  --------  --------  --------
;         0         0    0.5625
;         0         0    0.6250
;         0         0    0.6875
;         0         0    0.7500
;         0         0    0.8125
;         0         0    0.8750
;         0         0    0.9375
;         0         0    1.0000
;         0    0.0625    1.0000
;         0    0.1250    1.0000
;         0    0.1875    1.0000
;         0    0.2500    1.0000
;         0    0.3125    1.0000
;         0    0.3750    1.0000
;         0    0.4375    1.0000
;         0    0.5000    1.0000
;         0    0.5625    1.0000
;         0    0.6250    1.0000
;         0    0.6875    1.0000
;         0    0.7500    1.0000
;         0    0.8125    1.0000
;         0    0.8750    1.0000
;         0    0.9375    1.0000
;         0    1.0000    1.0000
;    0.0625    1.0000    0.9375
;    0.1250    1.0000    0.8750
;    0.1875    1.0000    0.8125
;    0.2500    1.0000    0.7500
;    0.3125    1.0000    0.6875
;    0.3750    1.0000    0.6250
;    0.4375    1.0000    0.5625
;    0.5000    1.0000    0.5000
;    0.5625    1.0000    0.4375
;    0.6250    1.0000    0.3750
;    0.6875    1.0000    0.3125
;    0.7500    1.0000    0.2500
;    0.8125    1.0000    0.1875
;    0.8750    1.0000    0.1250
;    0.9375    1.0000    0.0625
;    1.0000    1.0000         0
;    1.0000    0.9375         0
;    1.0000    0.8750         0
;    1.0000    0.8125         0
;    1.0000    0.7500         0
;    1.0000    0.6875         0
;    1.0000    0.6250         0
;    1.0000    0.5625         0
;    1.0000    0.5000         0
;    1.0000    0.4375         0
;    1.0000    0.3750         0
;    1.0000    0.3125         0
;    1.0000    0.2500         0
;    1.0000    0.1875         0
;    1.0000    0.1250         0
;    1.0000    0.0625         0
;    1.0000         0         0
;    0.9375         0         0
;    0.8750         0         0
;    0.8125         0         0
;    0.7500         0         0
;    0.6875         0         0
;    0.6250         0         0
;    0.5625         0         0
;    0.5000         0         0

        s = txtdb_rd(txt)
        r = byte(rebin(s.r0,256)*255)
        g = byte(rebin(s.g0,256)*255)
        b = byte(rebin(s.b0,256)*255)

        if keyword_set(smth) then begin
          r = smooth(r,31)
          g = smooth(g,31)
          b = smooth(b,31)
        endif

        device, decomp=0
        tvlct,r,g,b

        end
