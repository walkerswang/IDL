pro convert_plot,filename,new_dir,den,format1,format2
    temp = '/usr/local/bin/convert -density '+strcompress(string(den))+' '+filename+' '+new_dir+file_basename(filename,format1)+format2
    spawn,temp
end
;***************************main************************************

filename='/Users/jiaenren/Desktop/Lab/plots/20160114/20160114.003_lp_1min_RISR_beam11_18_24_lp_upb_flux.ps'
new_dir='/Users/jiaenren/Desktop/Lab/plots_jpg/20160114/'
convert_plot,filename,new_dir,600,'.ps','.jpg'
end







