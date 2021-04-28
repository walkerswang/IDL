list='/Users/wzihan/Google\ Drive/MATLAB/e_gse_model.csv'
e = READ_CSV(list, HEADER=SedHeader,N_TABLE_HEADER=0, TABLE_HEADER=SedTableHeader)
geopack_conv_coord, e.field1, e.field2, e.field3, d1, d2, d3,/from_gsm,/to_gse
end