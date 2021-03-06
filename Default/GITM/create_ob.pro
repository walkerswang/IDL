pro crawl,head1,head2,body1,body2,body3,body4,y,w

	for i=!d.x_size,-180,-20 do begin
	  erase
	  tv,[head1,body2,body1,body2,body1,body2,body1,body4],i,y
	  wait,w
	  erase
	  tv,[head2,body1,body2,body1,body2,body1,body2,body3],i-10,y
	  wait,w
	endfor
	return
end


pro create_ob

	common objects, ob

	style	= 0
; Create the man :

	xs	= 11
	ys	= 21

	case(style)of
	  0:begin
	    color
	    image	= [			$
	      [ 0,6,7,0,0,0,0,0,6,7,0 ],	$
	      [ 6,3,6,7,0,0,0,6,3,6,7 ],	$
	      [ 6,3,6,7,0,0,0,6,3,6,7 ],	$
	      [ 6,3,6,7,0,0,0,6,3,6,7 ],	$
	      [ 6,3,6,7,0,0,0,6,3,6,7 ],	$
	      [ 6,3,6,7,0,0,0,6,3,6,7 ],	$
	      [ 6,3,6,7,5,0,5,6,3,6,7 ],	$
	      [ 6,3,6,7,5,0,5,6,3,6,7 ],	$
	      [ 6,3,6,7,5,5,5,6,3,6,7 ],	$
	      [ 6,3,6,7,0,5,0,6,3,6,7 ],	$
	      [ 0,6,7,0,0,5,0,0,6,7,0 ],	$
	      [ 0,0,0,0,0,5,0,0,0,0,0 ],	$
	      [ 0,0,0,0,5,5,5,0,0,0,0 ],	$
	      [ 0,0,0,5,5,5,5,5,0,0,0 ],	$
	      [ 2,2,1,1,2,2,2,2,2,2,2 ],	$
	      [ 2,1,1,1,2,2,2,2,2,2,2 ],	$
	      [ 1,1,1,1,1,2,2,2,2,2,2 ],	$
	      [ 0,1,1,1,3,2,3,2,2,2,0 ],	$
	      [ 0,0,1,1,3,3,3,2,2,0,0 ],	$
	      [ 0,0,0,1,1,3,2,2,0,0,0 ],	$
	      [ 0,0,0,0,1,3,2,0,0,0,0 ]]
	  endcase
	  1:begin

	    image	= intarr(xs,ys)

	    man_color	= 200

	    for i=0,4 do begin
	      image(i,ys-1-4+i)		= man_color
	      image(xs-1-i,ys-1-4+i)	= man_color
	    endfor
	    image(5,ys-1)		= man_color
	    for j=ys-1-10,ys-1-5 do begin
	      image(0,j)		= man_color
	      image(xs-1,j)		= man_color
	    endfor

	    for i=0,2 do begin
	      image(i,ys-1-10)		= man_color
	      image(xs-1-i,ys-1-10)	= man_color
	    endfor

	    for j=0, ys-1-11 do begin
	      image(2,j)		= man_color
	      image(8,j)		= man_color
	    endfor

	    for i=3,7 do begin
	      image(i,0)		= man_color
	    endfor

	  endcase
	endcase

	oldman = {image : intarr(xs,ys), xoff : xs/2, yoff : ys/2,	$
	  x : !d.x_size/2, y : !d.y_size/2, xvel : 0, yvel : 0}
	newman = {image : image, xoff : xs/2, yoff : ys/2}

; Create the whole mushroom

	case(style)of
	  0:begin
	    shroom1	= [						$
	    [ 0, 0, 0, 0, 0, 0,11,11,11,11,11,11,12,12,12, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0,11,10,10,10,11,11,11,12,12, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0,11,10,10,10,11,12,12, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0,11,10,11,12,12, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0,11,10,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,11,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,11,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,11,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,11,10,12,15,15,15,15,16,16,16,13, 0], $
	    [ 0, 0, 0, 0, 0,14,14,14,14,11,10,12,15,15,15,16,16,13,13,13,13], $
	    [ 0,14,14,14,14,14,14,14,14,11,10,12,15,15,16,12,12,13,13,13,13], $
	    [12,12,12,12,12,14,14,14,14,15,15,13,13,13,13,13,13,13,13,13,13], $
	    [12,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13], $
	    [ 0,12,10,10,10,10,10,11,11,11,12,12,12,12,12,12,12,13,13,13, 0], $
	    [ 0, 0,11,10,10,10,10,11,11,11,12,12,12,12,12,12,12,13, 0, 0, 0], $
	    [ 0, 0, 0,11,11,10,10,11,11,11,11,12,12,12,12,12,13, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0,11,11,11,11,11,11,11,12,12,12,13, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0,12,12,11,12,12,12,12,13, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0,12,12,12,12,12,12,13, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,12,12,12,12,13, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,12,12,12, 0, 0, 0, 0, 0, 0, 0, 0]]
	    shroom2	= [						$
	    [ 0, 0, 0, 0, 0, 0, 0, 0,22,21,21,23, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0,22,21,21,23, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,22,21,23,23, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,22,21,21,23, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,22,21,21,23, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,21,21,23,23, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,21,21,21,23, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,22,21,21,23, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,22,21,21,23, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,22,21,23, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [29,28,28,25,24,25, 0, 0, 0, 0,21,22,23, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [27,27,26,29,29,24,24,25,24,22,22,22,23, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [26,27,27,26,29,29,29,25,24,22,22,22,23,25, 0, 0, 0, 0, 0, 0, 0], $
	    [26,26,26,26,26,26,28,30,24,22,23,23,23,24,25,25, 0, 0, 0, 0, 0], $
	    [ 0,26,26,26,27,27,28,28,30,30,30,25,25,25,24,24,24, 0, 0, 0, 0], $
	    [ 0,26,26,27,27,27,27,28,28,28,28,30,30,30,30,24,25,24,25, 0, 0], $
	    [ 0, 0,26,26,27,27,26,27,29,29,28,28,28,29,29,29,30,30,30,30, 0], $
	    [ 0, 0, 0,26,26,26,26,26,27,27,27,26,29,29,29,29,29,30,30,29,29], $
	    [ 0, 0, 0, 0, 0,27,27,26,26,27,27,26,26,27,27,27,28,28,30,29,29], $
	    [ 0, 0, 0, 0, 0, 0, 0,26,26,26,26,26,26,26,27,28,28,30,30,30, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,26,26,27,27,26,26,26,28,30, 0, 0, 0]]
	  endcase
	  1:begin
	    shroom1		= intarr(21,21)

	    m_color		= 200

	    shroom1(6:13,0)	= m_color
	    shroom1(6,0:9)	= m_color
	    shroom1(13,0:9)	= m_color
	    shroom1(0:5,9)	= m_color
	    shroom1(14:19,9)	= m_color
	    a=findgen(30)*!pi/29
	    shroom1(10+10*cos(a), 10+10*sin(a))	= m_color

	    shroom2	= shroom1
	  endcase
	endcase

	dark_shroom	= intarr(21,21)

; Create the half mushroom

	case(style)of
	  0:begin
	    hshroom1	= [						$
	    [ 0, 0, 0, 0, 0, 0,11,11,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0,11,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0,11,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0,11,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0,11,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,11,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,11,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,11,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,11,10,12,15, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0,14,14,14,14,11,10,12,15, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0,14,14,14,14,14,14,14,14,11,10,12,15, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [12,12,12,12,12,14,14,14,14,15,15,13,13,13, 0, 0, 0, 0, 0, 0, 0], $
	    [12,11,11,11,11,12,12,12,12,12,12,12,12,12, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0,12,10,10,10,10,10,11,11,11,12,12,12,12,12, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0,11,10,10,10,10,11,11,11,12,12,12,12,12, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0,11,11,10,10,11,11,11,11,12,12,12,12, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0,11,11,11,11,11,11,11,12,12,12, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0,12,12,11,12,12,12,12, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0,12,12,12,12,12,12, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,12,12,12,12,13, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,12,12,12, 0, 0, 0, 0, 0, 0, 0, 0]]
	    hshroom2	= [						$
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,22,21, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,22,21, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [29,28,28,25,24,25, 0, 0, 0, 0,21,22, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [27,27,26,29,29,24,24,25,24,22,22,22, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [26,27,27,26,29,29,29,25,24,22,22,22,23, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [26,26,26,26,26,26,28,30,24,22,23,23,23, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0,26,26,26,27,27,28,28,30,30,30,25,25,25, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0,26,26,27,27,27,27,28,28,28,28,30,30,30, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0,26,26,27,27,26,27,29,29,28,28,28,29, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0,26,26,26,26,26,27,27,27,26,29,29, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0,27,27,26,26,27,27,26,26, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0,26,26,26,26,26,26, 0, 0, 0, 0, 0, 0, 0, 0], $
	    [ 0, 0, 0, 0, 0, 0, 0, 0, 0,26,26,27, 0, 0, 0, 0, 0, 0, 0, 0, 0]]
	  endcase
	  1:begin
	    hshroom1	= intarr(21,21)

	    hshroom1(6,4:9)	= m_color
	    hshroom1(0:5,9)	= m_color
	    hshroom1(7,4)	= m_color
	    hshroom1(8,5)	= m_color
	    hshroom1(9,6)	= m_color
	    hshroom1(10,7)	= m_color
	    hshroom1(11,6)	= m_color
	    hshroom1(12,7)	= m_color
	    hshroom1(13,8)	= m_color
	    hshroom1(14,9)	= m_color
	    hshroom1(14,10)	= m_color
	    hshroom1(14,11)	= m_color
	    hshroom1(15,12)	= m_color
	    hshroom1(16,13)	= m_color
	    hshroom1(17,14)	= m_color
	    hshroom1(17,15)	= m_color
	    a=findgen(30)*!pi/29
	    a	= a(6:29)
	    hshroom(10+10*cos(a), 10+10*sin(a))	= m_color
	  endcase
	endcase

; create bullet

	bullet	= intarr(3,7)
	dark_bullet	= bullet

	case(style)of
	  0:begin
	    bullet = [	$
	    [ 0,19, 0], $
	    [19,18,20], $
	    [19,18,20], $
	    [19,18,20], $
	    [17,18,17], $
	    [17,17,17], $
	    [ 0,17, 0]]
	  endcase
	  1:begin
	    bul_color	= 200

	    bullet(0,0:4)	= bul_color
	    bullet(2,0:4)	= bul_color
	    bullet(1,5:6)	= bul_color
	  endcase
	endcase

; body
	body1w = [	$
	  [ 0, 0, 0,40,41,41,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39, 0, 0,38,38, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39,37,37,37,37,37,38,38, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0,40,41,41,36,36,36,36,36,37,37,37, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,36,36,35,35,36,36,36,36,37,37,38, 0, 0, 0, 0], $
	  [ 0, 0,38,37,36,35,35,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0], $
	  [ 0,38,37,36,35,35,34,34,34,34,34,34,35,35,36,36,37,38, 0, 0, 0], $
	  [ 0,38,37,36,35,34,33,33,33,33,33,34,34,35,35,36,37,37, 0, 0, 0], $
	  [39,39,36,35,34,33,33,32,32,32,32,33,34,34,35,36,36,37,39,39,39], $
	  [41,41,36,35,34,33,32,31,31,31,32,32,33,34,35,36,36,37,41,41,41], $
	  [41,41,36,35,33,32,31,31,31,31,31,32,33,34,35,35,36,37,41,41,41], $
	  [41,41,36,35,33,32,31,31,31,31,31,32,33,34,35,35,36,37,41,41,41], $
	  [40,40,36,35,33,32,31,31,31,31,31,32,33,34,35,36,36,37,40,40,40], $
	  [ 0,38,37,35,34,33,32,31,31,31,32,33,33,34,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,37,36,35,34,33,32,32,32,33,33,34,35,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,38,37,36,35,34,33,33,33,34,34,35,35,36,37,38, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0,38,37,37,36,36,36,36,36,37,37,41,41,39, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0,38,37,37,37,37,37,38, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,40,41,41,39, 0, 0, 0]]
	body2w = [	$
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0,38,38, 0, 0, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0,38,37,37,37,37,37,37,38,38,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0,38,37,37,36,36,36,36,36,37,37,41,41,39, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,36,36,35,35,36,36,36,36,37,37,38, 0, 0, 0, 0], $
	  [ 0, 0,38,37,36,35,35,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0], $
	  [ 0,38,37,36,35,35,34,34,34,34,34,34,35,35,36,36,37,38, 0, 0, 0], $
	  [ 0,38,37,36,35,34,33,33,33,33,33,34,34,35,35,36,37,37, 0, 0, 0], $
	  [39,39,36,35,34,33,33,32,32,32,32,33,34,34,35,36,36,37,39,39,39], $
	  [41,41,36,35,34,33,32,31,31,31,32,32,33,34,35,36,36,37,41,41,41], $
	  [41,41,36,35,33,32,31,31,31,31,31,32,33,34,35,35,36,37,41,41,41], $
	  [41,41,36,35,33,32,31,31,31,31,31,32,33,34,35,35,36,37,41,41,41], $
	  [40,40,36,35,33,32,31,31,31,31,31,32,33,34,35,36,36,37,40,40,40], $
	  [ 0,38,37,35,34,33,32,31,31,31,32,33,33,34,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,37,36,35,34,33,32,32,32,33,33,34,35,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,38,37,36,35,34,33,33,33,34,34,35,35,36,37,38, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0,40,41,41,36,36,36,36,36,37,37,38, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39,37,37,37,37,37,38, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]
	body3w = [	$
	  [ 0, 0, 0,40,41,41,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39, 0, 0,38,38, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39,37,37,37,37,37,38,38, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0,40,41,41,36,36,36,36,36,37,37,37, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,36,36,35,35,36,36,36,36,37,37,38, 0, 0, 0, 0], $
	  [ 0, 0,38,37,36,35,35,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0], $
	  [ 0,38,37,36,35,35,34,34,34,34,34,34,35,35,36,36,37,38, 0, 0, 0], $
	  [ 0,38,37,36,35,34,33,33,33,33,33,34,34,35,35,36,37,37, 0, 0, 0], $
	  [39,39,36,35,34,33,33,32,32,32,32,33,34,34,35,36,36,37,0 ,0 ,0 ], $
	  [41,41,36,35,34,33,32,31,31,31,32,32,33,34,35,36,36,37,38, 0,0 ], $
	  [41,41,36,35,33,32,31,31,31,31,31,32,33,34,35,35,36,37,38, 0,0 ], $
	  [41,41,36,35,33,32,31,31,31,31,31,32,33,34,35,35,36,37,38, 0,0 ], $
	  [40,40,36,35,33,32,31,31,31,31,31,32,33,34,35,36,36,37, 0, 0, 0], $
	  [ 0,38,37,35,34,33,32,31,31,31,32,33,33,34,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,37,36,35,34,33,32,32,32,33,33,34,35,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,38,37,36,35,34,33,33,33,34,34,35,35,36,37,38, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0,38,37,37,36,36,36,36,36,37,37,41,41,39, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0,38,37,37,37,37,37,38, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,40,41,41,39, 0, 0, 0]]
	body4w = [	$
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0,38,38, 0, 0, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0,38,37,37,37,37,37,37,38,38,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0,38,37,37,36,36,36,36,36,37,37,41,41,39, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,36,36,35,35,36,36,36,36,37,37,38, 0, 0, 0, 0], $
	  [ 0, 0,38,37,36,35,35,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0], $
	  [ 0,38,37,36,35,35,34,34,34,34,34,34,35,35,36,36,37,38, 0, 0, 0], $
	  [ 0,38,37,36,35,34,33,33,33,33,33,34,34,35,35,36,37,37, 0, 0, 0], $
	  [39,39,36,35,34,33,33,32,32,32,32,33,34,34,35,36,36,37, 0, 0, 0], $
	  [41,41,36,35,34,33,32,31,31,31,32,32,33,34,35,36,36,37,38, 0, 0], $
	  [41,41,36,35,33,32,31,31,31,31,31,32,33,34,35,35,36,37,38, 0, 0], $
	  [41,41,36,35,33,32,31,31,31,31,31,32,33,34,35,35,36,37,38, 0, 0], $
	  [40,40,36,35,33,32,31,31,31,31,31,32,33,34,35,36,36,37, 0, 0, 0], $
	  [ 0,38,37,35,34,33,32,31,31,31,32,33,33,34,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,37,36,35,34,33,32,32,32,33,33,34,35,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,38,37,36,35,34,33,33,33,34,34,35,35,36,37,38, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0,40,41,41,36,36,36,36,36,37,37,38, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39,37,37,37,37,37,38, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]
	head1w = [	$
	  [ 0, 0, 0,40,41,41,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39, 0, 0,38,38, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39,37,37,37,37,37,38,38, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0,40,41,41,36,36,36,36,36,37,37,37, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,36,36,35,35,36,36,36,36,37,37,38, 0, 0, 0, 0], $
	  [ 0, 0,38,37,42,42,42,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0], $
	  [ 0,38,37,42, 0, 0,42,42,34,34,34,34,35,35,36,36,37,38, 0, 0, 0], $
	  [ 0,38,37,42, 0, 0,42,42,33,33,33,34,34,35,35,36,37,37, 0, 0, 0], $
	  [ 0,37,36,42, 0, 0,42,42,32,32,32,33,34,34,35,36,36,37,39,39,39], $
	  [ 0,37,36,35,42,42,42,31,31,31,32,32,33,34,35,36,36,37,41,41,41], $
	  [ 0,37,36,35,33,32,31,31,31,31,31,32,33,34,35,35,36,37,41,41,41], $
	  [ 0,37,36,35,42,42,42,31,31,31,31,32,33,34,35,35,36,37,41,41,41], $
	  [ 0,37,36,42, 0, 0,42,42,31,31,31,32,33,34,35,36,36,37,40,40,40], $
	  [ 0,38,37,42, 0, 0,42,42,31,31,32,33,33,34,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,37,42, 0, 0,42,42,32,32,33,33,34,35,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,38,37,42,42,42,33,33,33,34,34,35,35,36,37,38, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0,38,37,37,36,36,36,36,36,37,37,41,41,39, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0,38,37,37,37,37,37,38, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,40,41,41,39, 0, 0, 0]]
	head2w = [	$
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0, 0, 0, 0, 0,38,38, 0, 0, 0,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0, 0,38,37,37,37,37,37,37,38,38,40,41,41,39, 0, 0, 0], $
	  [ 0, 0, 0, 0,38,37,37,36,36,36,36,36,37,37,41,41,39, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,36,36,35,35,36,36,36,36,37,37,38, 0, 0, 0, 0], $
	  [ 0, 0,38,37,42,42,42,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0], $
	  [ 0,38,37,42, 0, 0,42,42,34,34,34,34,35,35,36,36,37,38, 0, 0, 0], $
	  [ 0,38,37,42, 0, 0,42,42,33,33,33,34,34,35,35,36,37,37, 0, 0, 0], $
	  [ 0,37,36,42, 0, 0,42,42,32,32,32,33,34,34,35,36,36,37,39,39,39], $
	  [ 0,37,36,35,42,42,42,31,31,31,32,32,33,34,35,36,36,37,41,41,41], $
	  [ 0,37,36,35,35,32,31,31,31,31,31,32,33,34,35,35,36,37,41,41,41], $
	  [ 0,37,36,35,42,42,42,31,31,31,31,32,33,34,35,35,36,37,41,41,41], $
	  [ 0,37,36,42, 0, 0,42,42,31,31,31,32,33,34,35,36,36,37,40,40,40], $
	  [ 0,38,37,42, 0, 0,42,42,31,31,32,33,33,34,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,37,42, 0, 0,42,42,32,32,33,33,34,35,35,36,37,38, 0, 0, 0], $
	  [ 0, 0,38,37,42,42,42,33,33,33,34,34,35,35,36,37,38, 0, 0, 0, 0], $
	  [ 0, 0, 0,38,37,36,35,35,35,35,35,35,36,36,37,38, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0, 0,40,41,41,36,36,36,36,36,37,37,38, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39,37,37,37,37,37,38, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], $
	  [ 0, 0, 0,40,41,41,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]

        nex = n_elements(head1w(*,0))
        ney = n_elements(head1w(0,*))
	body1e = body1w
	body2e = body2w
	body3e = body3w
	body4e = body4w
	head1e = head1w
	head2e = head2w
	body1s = body1w
	body2s = body2w
	body3s = body3w
	body4s = body4w
	head1s = head1w
	head2s = head2w

        for i=0,nex-1 do for j=0,ney-1 do begin

	  body1e(nex-1-i,j) = body1w(i,j)
	  body2e(nex-1-i,j) = body2w(i,j)
	  body3e(nex-1-i,j) = body3w(i,j)
	  body4e(nex-1-i,j) = body4w(i,j)
	  head1e(nex-1-i,j) = head1w(i,j)
	  head2e(nex-1-i,j) = head2w(i,j)
	  body1s(j,i) = body1w(i,j)
	  body2s(j,i) = body2w(i,j)
	  body3s(j,i) = body3w(i,j)
	  body4s(j,i) = body4w(i,j)
	  head1s(j,i) = head1w(i,j)
	  head2s(j,i) = head2w(i,j)

	endfor

        cent = {head1e : head1e,		$
		head2e : head2e,		$
		head1w : head1w,		$
		head2w : head2w,		$
		body1e : body1e,		$
		body2e : body2e,		$
		body1w : body1w,		$
		body2w : body2w}


	s_shape1 = [							$
  [42, 0, 0, 0,42, 0, 0,42, 0,42, 0, 0, 0, 0, 0,42, 0,42, 0, 0,42, 0, 0, 0,42],$
  [ 0,42, 0, 0,42, 0, 0,42, 0, 0,42, 0, 0, 0,42, 0, 0,42, 0, 0,42, 0, 0,42, 0],$
  [ 0,42, 0, 0,42, 0, 0,42, 0, 0,42, 0, 0, 0,42, 0, 0,42, 0, 0,42, 0, 0,42, 0],$
  [ 0,42, 0, 0,42, 0, 0, 0,42, 0,42, 0, 0, 0,42, 0,42, 0, 0, 0,42, 0, 0,42, 0],$
  [ 0, 0,42, 0, 0,42, 0, 0,42, 0,42, 0, 0, 0,42, 0,42, 0, 0,42, 0, 0,42, 0, 0],$
  [ 0, 0, 0,42, 0, 0,42, 0,42, 0,42, 0, 0, 0,42, 0,42, 0,42, 0, 0,42, 0, 0, 0],$
  [ 0, 0, 0, 0,42, 0,42, 0,42, 0,42, 0, 0, 0,42, 0,42, 0,42, 0,42, 0, 0, 0, 0],$
  [ 0, 0, 0, 0,42, 0,42, 0,42, 0,42, 0, 0, 0,42, 0,42, 0,42, 0,42, 0, 0, 0, 0],$
  [ 0, 0, 0, 0, 0,42, 0,42, 0,42, 0,42, 0,42, 0,42, 0,42, 0,42, 0, 0, 0, 0, 0],$
  [ 0, 0, 0, 0,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44, 0, 0, 0, 0],$
  [ 0, 0, 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0, 0, 0],$
  [ 0, 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0, 0],$
  [ 0, 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0, 0],$
  [ 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0],$
  [ 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0],$
  [ 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0],$
  [ 0, 0,44,44,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44,44,44, 0, 0],$
  [ 0, 0, 0, 0,43,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44,43, 0, 0, 0, 0],$
  [ 0, 0, 0, 0,43,43,44,44, 0, 0, 0, 0, 0, 0, 0, 0, 0,44,44,43,43, 0, 0, 0, 0],$
  [ 0, 0, 0, 0,43,43, 0, 0,44,44,44, 0, 0, 0,44,44,44, 0, 0,43,43, 0, 0, 0, 0],$
  [ 0, 0, 0,42,42,43, 0, 0, 0, 0, 0,44,44,44, 0, 0, 0, 0, 0,43,42,42, 0, 0, 0],$
  [ 0, 0,42,45,45,42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,42, 0, 0,42, 0, 0],$
  [ 0,42, 0, 0, 0, 0,42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,42, 0,45,45, 0,42, 0],$
  [ 0, 0,42, 0, 0,42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,42, 0, 0,42, 0, 0],$
  [ 0, 0, 0,42,42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,42,42, 0, 0, 0]]

	s_shape2 = [							$
  [42, 0, 0, 0,42, 0, 0,42, 0,42, 0, 0, 0, 0, 0,42, 0,42, 0, 0,42, 0, 0, 0,42],$
  [ 0,42, 0, 0,42, 0, 0,42, 0, 0,42, 0, 0, 0,42, 0, 0,42, 0, 0,42, 0, 0,42, 0],$
  [ 0,42, 0, 0,42, 0, 0,42, 0, 0,42, 0, 0, 0,42, 0, 0,42, 0, 0,42, 0, 0,42, 0],$
  [ 0,42, 0, 0,42, 0, 0, 0,42, 0,42, 0, 0, 0,42, 0,42, 0, 0, 0,42, 0, 0,42, 0],$
  [ 0, 0,42, 0, 0,42, 0, 0,42, 0,42, 0, 0, 0,42, 0,42, 0, 0,42, 0, 0,42, 0, 0],$
  [ 0, 0, 0,42, 0, 0,42, 0,42, 0,42, 0, 0, 0,42, 0,42, 0,42, 0, 0,42, 0, 0, 0],$
  [ 0, 0, 0, 0,42, 0,42, 0,42, 0,42, 0, 0, 0,42, 0,42, 0,42, 0,42, 0, 0, 0, 0],$
  [ 0, 0, 0, 0,42, 0,42, 0,42, 0,42, 0, 0, 0,42, 0,42, 0,42, 0,42, 0, 0, 0, 0],$
  [ 0, 0, 0, 0, 0,42, 0,42, 0,42, 0,42, 0,42, 0,42, 0,42, 0,42, 0, 0, 0, 0, 0],$
  [ 0, 0, 0, 0,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44, 0, 0, 0, 0],$
  [ 0, 0, 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0, 0, 0],$
  [ 0, 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0, 0],$
  [ 0, 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0, 0],$
  [ 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0],$
  [ 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0],$
  [ 0,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44, 0],$
  [ 0, 0,44,44,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44,44,44, 0, 0],$
  [ 0, 0, 0, 0,43,44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,44,43, 0, 0, 0, 0],$
  [ 0, 0, 0, 0,43,43,44,44, 0, 0, 0, 0, 0, 0, 0, 0, 0,44,44,43,43, 0, 0, 0, 0],$
  [ 0, 0, 0, 0,43,43, 0, 0,44,44,44, 0, 0, 0,44,44,44, 0, 0,43,43, 0, 0, 0, 0],$
  [ 0, 0, 0,42,42,43, 0, 0, 0, 0, 0,44,44,44, 0, 0, 0, 0, 0,43,42,42, 0, 0, 0],$
  [ 0, 0,42,45,45,42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,42, 0, 0,42, 0, 0],$
  [ 0,42, 0, 0, 0, 0,42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,42, 0,45,45, 0,42, 0],$
  [ 0, 0,42, 0, 0,42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,42, 0, 0,42, 0, 0],$
  [ 0, 0, 0,42,42, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,42,42, 0, 0, 0]]

  xs = 25
  ys = 25
	spider = {shape1 : s_shape1, shape2 : s_shape2, on : 0, 	$
		  xpos : -1, ypos : -1, 	$
		  xsize : xs, ysize : ys, slope : 0.0, velocity : 0.0,	$
		  overspider : intarr(xs,ys), lastx : 0, lasty : 0,	$
		  loc1 : where(s_shape1 ne 0), 				$
		  loc2 : where(s_shape2 ne 0), body : 1}

; put to structure


	ob	= { 			$
	  oldman : oldman, 		$
	  newman : newman, 		$
	  mushroom1 : shroom1,		$
	  mushroom2 : shroom2,		$
	  half_shroom1 : hshroom1, 	$
	  half_shroom2 : hshroom2, 	$
	  dark_shroom : dark_shroom, 	$
	  bullet : bullet, 		$
	  dark_bullet : dark_bullet,	$
	  cent : cent,			$
	  spider : spider}

	return

end
