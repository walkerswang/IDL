; docformat = 'rst'
;+
; This is an example program to demonstrate how to create a colored line plot
; with Coyote Graphics routines.
;
; :Categories:
;    Graphics
;    
; :Examples:
;    Save the program as "colored_line_plot.pro" and run it like this::
;       IDL> .RUN colored_line_plot
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;        Written, 25 January 2013 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
PRO Colored_Line_Plot
 
   ; Example random data. Normally passed into the program as a positional parameter.
   data = cgDemoData(17)
   time = cgScaleVector(Findgen(N_Elements(data)), 0, 6)
   thick = (!D.Name EQ 'PS') ? 6 : 3
   
   ; Create a graphics window.
   cgDisplay
   
   ; Two plots in a column. 
   !P.Multi=[0,1,2]
   
   ; The top plot is colored in time. The bottom plot is
   ; colored in elevation. Here is how we set up the time colors.
   timeColors = Byte(Round(cgScaleVector(Findgen(N_Elements(data)), 0, 255)))
   
   ; Here is how we set up the elevation colors.   
   colors = cgScaleVector(Findgen(N_Elements(data)), Min(data), Max(data))
   elevColors = Value_Locate(colors, data)
   elevColors = Byte(Round(cgScaleVector(elevColors, 0, 255)))
   
   ; Set up the color table.
   cgLoadCT, 34
   
   ; Draw the first plot.
   cgPlot, time, data, /NoData, XTitle='Time', YTitle='Elevation', Label='Colored in Time'
   FOR j=0,N_Elements(data)-2 DO cgPlotS, [time[j], time[j+1]], [data[j], data[j+1]], $
       Color=timeColors[j], Thick=thick
   FOR j=0,N_Elements(data)-1,2 DO cgPlotS, time[j], data[j], PSym=2, $
       SymSize=1.5, Color=timeColors[j]

   ; Draw the second plot.
   cgPlot, time, data, /NoData, XTitle='Time', YTitle='Elevation', Label='Colored in Elevation'
   FOR j=0,N_Elements(data)-2 DO cgPlotS, [time[j], time[j+1]], [data[j], data[j+1]], $
       Color=elevColors[j], Thick=thick
   FOR j=0,N_Elements(data)-1,2 DO cgPlotS, time[j], data[j], PSym=2, $
       SymSize=1.5, Color=elevColors[j]
      
   ; Clean up.
   !P.Multi = 0
   
END ;*****************************************************************

; This main program shows how to call the program and produce
; various types of output.

  ; Display the plot in a graphics window.
  Colored_Line_Plot
  
  ; Display the plot in a resizeable graphics window.
  cgWindow, 'Colored_Line_Plot', WBackground='White', $
     WTitle='Colored Line Plots in a Resizeable Graphics Window'
  
  ; Create a PostScript file.
  cgPS_Open, 'colored_line_plot.ps'
  Colored_Line_Plot
  cgPS_Close
  
  ; Create a PNG file with a width of 600 pixels.
  cgPS2Raster, 'colored_line_plot.ps', /PNG, Width=600

END