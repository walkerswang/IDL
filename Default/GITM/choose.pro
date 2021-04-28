
pro find_next, from, number, array

  j = number-1
  maxi = from-1
  while (array(j) ge maxi and j gt 0) do begin
     j--
     maxi--
  endwhile
  if (j gt -1) then begin
     array(j)++
     for i=j+1,number-1 do array(i)=array(i-1)+1
  endif
  
end

pro choose, from, number, array

  if (number eq from) then begin
     array = intarr(1,number)
     array(0,*) = indgen(number)
  endif else begin

     c = factorial(from)/factorial(number)/factorial(from-number)
     array = intarr(c,number)

     a = indgen(number)
     array(0,*) = a
     for i=1,c-1 do begin
        find_next, from, number, a
        array(i,*) = a
     endfor

  endelse
  
end

