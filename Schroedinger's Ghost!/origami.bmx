Global triangles:TList = New TList
Global tricount=0
Type triangle
	Field lines:line[3] 'edges
	Field x1#,y1#,x2#,y2#,x3#,y3# 'vertex positions
	Field poly#[] 'polygon for drawing
	Field incisions:tlist 'incisions into this triangle
	Field red , green , blue 'colour
	Field links:tlist[3] 'links on each side to other triangles (joins)
	Field area# 'duh
	Field cx#,cy# 'centroid position
	Field id
	Field folds:tlist
	
	Method New()
		tricount:+ 1
		id=tricount
		triangles.addlast Self
		incisions = New tlist
		For i = 0 To 2
			links[i] = New tlist
		Next
		folds=New tlist
	End Method
	
	Function create:triangle(x1# , y1# , x2# , y2# , x3# , y3# , red = 255 , green = 255 , blue = 255)
		t:triangle = New triangle
		t.x1=x1
		t.y1=y1
		t.x2=x2
		t.y2=y2
		t.x3=x3
		t.y3 = y3
		t.red = red
		t.green = green
		t.blue = blue
		
		Local poly#[]=[x1,y1,x2,y2,x3,y3]
		t.poly=poly
		t.lines[0]=line.create(x1,y1,x2,y2)
		t.lines[1]=line.create(x2,y2,x3,y3)
		t.lines[2]=line.create(x3,y3,x1,y1)
		
		t.doarea()
		t.docentroid()
		
		Return t
	End Function
	
	Method rejiggle(nx1#,ny1#,nx2#,ny2#,nx3#,ny3#)
		x1=nx1
		y1=ny1
		x2=nx2
		y2=ny2
		x3=nx3
		y3=ny3
		poly=[x1,y1,x2,y2,x3,y3]
		lines[0]=line.create(x1,y1,x2,y2,lines[0].red,lines[0].green,lines[0].blue,lines[0].alpha)
		lines[1]=line.create(x2,y2,x3,y3,lines[1].red,lines[1].green,lines[1].blue,lines[1].alpha)
		lines[2]=line.create(x3,y3,x1,y1,lines[2].red,lines[2].green,lines[2].blue,lines[2].alpha)
		
		doarea()
		docentroid()
	End Method
	
	Method doarea()
		a# = lines[0].length
		b# = lines[1].length
		c# = lines[2].length
		cosc# = (a * a + b * b - c * c) / (2 * a * b)
		sinc# = Sqr(1 - cosc * cosc)
		area# = a * b * sinc / 2
	End Method
	
	Method makelink(tri:triangle , myside, theirside, f:fold = Null) 
		For li:link = EachIn links[myside]
			If li.tri = tri And li.f=f Return
		Next
		li:link=link.create(tri , myside , theirside, f)
		links[myside].addlast(li)
		
	End Method
	
	Method draw(x# , y#)
		Local poly2#[6]
		For i = 0 To 2
			poly2[2 * i] = poly[2 * i] + x
			poly2[2 * i + 1] = poly[2 * i + 1] + y
		Next
		'If pointinside(mx,my)
		'	SetAlpha .8
		'Else
			SetAlpha .4
		'EndIf
		SetColor red,green,blue
		DrawPoly poly2
		SetColor 255,255,255
		SetAlpha 1
		For i=0 To 2
			lines[i].draw(x , y)
		Rem
			SetColor 255,0,0
			For li:link = EachIn links[i]
				DrawLine cx + x , cy + y , li.tri.cx + x , li.tri.cy + y
				an# = ATan2(li.tri.cy - cy , li.tri.cx - cx )
				
				DrawLine li.tri.cx+x,li.tri.cy+y,li.tri.cx+x+Cos(an+150)*15,li.tri.cy+y+Sin(an+150)*15
				DrawLine li.tri.cx+x,li.tri.cy+y,li.tri.cx+x+Cos(an-150)*15,li.tri.cy+y+Sin(an-150)*15
			Next
			SetColor 255,255,255
		EndRem
		Next
		'DrawRect cx + x , cy + y , 2 , 2
		'DrawText String(id) , cx + x , cy + y
		'For i = 0 To 2
		'	DrawText Int(lines[i].alpha) , cx + x , cy + y + (i + 1) * 13
		'Next
	End Method
	
	Method cut(l:line,f:fold=Null)
		otherinc:incision=Null
		For inc:incision = EachIn incisions
			dx# = l.ex - inc.l.ex
			dy# = l.ey - inc.l.ey
			d# = dx * dx + dy * dy
			If d < 100
				l = line.create(l.x , l.y , inc.l.ex , inc.l.ey) 
				otherinc:incision = inc
				'Print "WHAMMO"
			EndIf
		Next
		Local hits#[3]
		n = 0
		'Print "CUT"
		For i = 0 To 2
			hits[i] = lines[i].intersect(l)
			If hits[i] <> - 1 n:+ 1
			'Print hits[i]
		Next
		'Print n
		Select n
		Case 0 'misses triangle altogether
		Case 1 'stops inside triangle
			For i = 0 To 2
				If hits[i] <> - 1 side = i
			Next
			l.red = 255
			l.green = 0
			l.blue = 0
			
			inc:incision = incision.create(l , side , hits[side]) 
			If otherinc
				For tri:triangle = EachIn triangles
					tri.removelinksto Self
				Next
				incisions.remove otherinc
				If inc.side = otherinc.side
					If otherinc.mu > inc.mu
						beinc:incision = inc
						inc = otherinc
						otherinc = beinc
					EndIf
					x5# = l.ex
					y5# = l.ey
					x3# = lines[side].x + lines[side].dx * otherinc.mu
					y3# = lines[side].y + lines[side].dy * otherinc.mu
					x4# = lines[side].x + lines[side].dx * inc.mu
					y4# = lines[side].y + lines[side].dy * inc.mu
					x0# = lines[side].x
					y0# = lines[side].y
					x1# = lines[(side + 1) Mod 3].x
					y1# = lines[(side + 1) Mod 3].y
					x2# = lines[(side + 2) Mod 3].x
					y2# = lines[(side + 2) Mod 3].y
					t1:triangle = triangle.create(x0 , y0 , x3 , y3 , x5 , y5)
					t2:triangle = triangle.create(x5 , y5 , x0 , y0 , x2 , y2)
					t3:triangle = triangle.create(x5 , y5 , x4 , y4 , x1 , y1)
					t4:triangle = triangle.create(x1 , y1 , x2 , y2 , x5 , y5)
					t5:triangle = triangle.create(x3 , y3 , x4 , y4 , x5 , y5)
					t1.inherit(Self , [side , - 2 , - 1])
					t2.inherit(Self , [ - 1 , (side + 2) Mod 3 , - 1])
					t3.inherit(Self , [ - 2 , side , - 1])
					t4.inherit(Self , [ (side + 1) Mod 3 , - 1 , - 1])
					t5.inherit(Self , [side , - 2 , - 2])
										
					t1.checklinks(0 , Self , side)
					linktriangles(t1 , 2 , t2 , 0)
					
					t2.checklinks(1 , Self , (side + 2) Mod 3)
					linktriangles(t2 , 2 , t4 , 1)
					
					t3.checklinks(1 , Self , side)
					linktriangles(t3 , 2 , t4 , 2)
					
					t4.checklinks(0 , Self , (side + 1) Mod 3)
					
					t5.checklinks(0 , Self , side)
										
				Else
					sidea = inc.side
					sideb = otherinc.side
					sidec = 3 - sidea - sideb
					
					corner = (sidec + 2) Mod 3
					x0# = lines[corner].x
					y0# = lines[corner].y
					x1# = lines[sidea].x + lines[sidea].dx * inc.mu
					y1# = lines[sidea].y + lines[sidea].dy * inc.mu
					x2# = l.ex
					y2# = l.ey
					x3# = lines[sideb].x + lines[sideb].dx * otherinc.mu
					y3# = lines[sideb].y + lines[sideb].dy * otherinc.mu
					iside=(sideb+2) Mod 3
					x4# = lines[iside].x
					y4# = lines[iside].y
					oiside = (sidea + 2) Mod 3
					x5# = lines[oiside].x
					y5# = lines[oiside].y
					
					t1:triangle = triangle.create(x0 , y0 , x1 , y1 , x2 , y2)
					t2:triangle = triangle.create(x0 , y0 , x2 , y2 , x3 , y3)
					t3:triangle = triangle.create(x1 , y1 , x4 , y4 , x2 , y2)
					t4:triangle = triangle.create(x2 , y2 , x4 , y4 , x5 , y5)
					t5:triangle = triangle.create(x2 , y2 , x5 , y5 , x3 , y3)
					t1.inherit(Self, [sidea,-2,-1])
					t2.inherit(Self, [-1,-2,sideb])
					t3.inherit(Self, [sidea,-1,-2])
					t4.inherit(Self, [-1,sidec,-1])
					t5.inherit(Self, [-1,sideb,-2])
					
					t1.checklinks(0 , Self , sidea)
					linktriangles(t1 , 2 , t2 , 0) 
					
					t2.checklinks(2 , Self , sideb)
					
					t3.checklinks(0 , Self , sidea)
					linktriangles(t3 , 1 , t4 , 0)
					
					t4.checklinks(1 , Self , sidec)
					linktriangles(t4 , 2 , t5 , 0) 
					
					t5.checklinks(1 , Self , sideb)
					
				EndIf
				die()
				inc.die()
				otherinc.die()
				'incision.create(line.create(x5 , y5 , x3 , y3) , 0 , 0) 
				'incision.create(line.create(x5 , y5 , x4 , y4) , 0 , 0)
			Else
				incisions.addlast inc
			EndIf
		Case 2 'passes through triangle
			For i=0 To 2
				'Print "---"+String(i)
				For li:link = EachIn links[i]
					'Print li.tri.id
				Next
			Next
			For tri:triangle = EachIn triangles
				tri.removelinksto Self
			Next
			For i = 0 To 2
				If hits[i] = - 1 'this is the side that wasn't crossed, ie the one that will end up quadrilateral
					corner = (i + 2) Mod 3 'point which is on the bit that will end up as a triangle
					other=(i+1) Mod 3 'the other side which isn't this one
					
					tx1# = lines[corner].x
					ty1# = lines[corner].y
					tx2# = lines[corner].x + hits[corner] * lines[corner].dx
					ty2# = lines[corner].y + hits[corner] * lines[corner].dy
					tx3# = lines[other].x + hits[other] * lines[other].dx
					ty3# = lines[other].y + hits[other] * lines[other].dy
					tx4# = lines[i].x
					ty4# = lines[i].y
					tx5# = lines[other].x
					ty5# = lines[other].y
					t1:triangle = triangle.create(tx1 , ty1 , tx2 , ty2 , tx3 , ty3)
					t2:triangle=triangle.create(tx2 , ty2 , tx4 , ty4 , tx5 , ty5)
					t3:triangle = triangle.create(tx5 , ty5 , tx3 , ty3 , tx2 , ty2)
					t1.inherit(Self,[corner,-2,other])
					t2.inherit(Self,[corner,i,-1])
					t3.inherit(Self , [other , - 2 , - 1])
					hoopy=t1
					
					t1.checklinks(0 , Self , corner)
					t1.checklinks(2 , Self , other)
					
					t2.checklinks(0 , Self , corner)
					t2.checklinks(1 , Self , i)
					
					t3.checklinks(0 , Self , other) 
					
					linktriangles(t3 , 2 , t2 , 2) 

					If f 'folding
						linktriangles(t1 , 1 , t3 , 1, f)
					EndIf
					
					die()
				EndIf
			Next
		Case 3 'Euclid is unhappy
		End Select
	End Method
	
	Method die()
		For inc:incision = EachIn incisions
			inc.die()
		Next
		For f:fold = EachIn folds
			f.foldees.remove Self
		Next
		triangles.remove Self
	End Method
	
	Method inherit(tri:triangle , alphas[]) 
		Print ""
		Print String(id)+"--"+String(tri.id)
		For i = 0 To 2
			Print tri.lines[i].alpha
		Next
		For f:fold = EachIn tri.folds
			f.foldees.addlast Self
			folds.addlast f
		Next
		For i = 0 To 2
			'Print i
			'Print alphas[i]
			Select alphas[i]
			Case - 2
				lines[i].alpha = 1
			Case - 1
				lines[i].alpha = 0
			Default
				'Print tri.lines[alphas[i]].alpha
				lines[i].alpha = tri.lines[alphas[i]].alpha
			End Select
		Next
		For i=0 To 2
			Print String(alphas[i])+"-->"+String(lines[i].alpha)
		Next
	End Method
	
	Method findjoins(list:tlist , followfolds = 1)
		list.addlast Self
		For i = 0 To 2
			For li:link = EachIn links[i]
				If li.f=Null Or followfolds
					If Not list.contains(li.tri)
						li.tri.findjoins(list,followfolds)
					EndIf
				EndIf
			Next
		Next
	End Method
	
	Method checklinks(side , otri:triangle , oside)
		'Print "!!!!"
		'Print "checking "+String(id)+"'s "+String(side)+" side against "+String(otri.id)+"'s "+String(oside)+" side"
		For li:link = EachIn otri.links[oside]
			l:line = lines[side]
			tri:triangle = li.tri
			'Print "checking " + String(id) + " against " + String(tri.id)
			l2:line = tri.lines[li.theirside]
			'Print String(l.id)+"   "+String(l.x) + "," + String(l.y)+" --> "+String(l.ex)+","+String(l.ey)
			'Print String(li.theirside)+".."+String(l2.id)+"   "+String(l2.x)+","+String(l2.y)+" --> "+String(l2.ex)+","+String(l2.ey)
			If l2.length > l.length
				l3:line = l2
				l2 = l
				l = l3
				'Print "swap"
			EndIf
			If l.dx = 0
				mu1# = (l2.ey - l.y) / l.dy
				mu2# = (l2.y - l.y) / l.dy
				
			Else
				mu1# = (l2.ex - l.x) / l.dx
				mu2# = (l2.x - l.x) / l.dx
				
			EndIf
			'Print mu1 / l.length
			'Print mu2 / l.length
			overlaplimit#=5
			If (mu1 >= 0 And mu1 <= l.length) Or (mu2 >= 0 And mu2 <= l.length)
				'Print "ok!"
				go=0
				If mu1 > mu2
					If mu1 > l.length
						overlap# = l.length - mu2
						'Print "overlap by mu2 = " + String(overlap)
					Else
						If mu2 >= 0 And mu2 <= l.length
							'Print "totally inside (1)"
							go=1
						Else
							If mu2<0
								overlap# = mu1
							Else
								overlap# = l.length - mu1
							EndIf
							'Print "overlap by mu1 = " + String(overlap)
						EndIf
					EndIf
					If Not go
						If overlap > overlaplimit go = 1
					EndIf
				Else
					If mu2 > l.length
						overlap# = l.length - mu1
						'Print "overlap by mu1 = " + String(overlap)
					Else
						If mu1 >= 0 And mu1 <= l.length
							'Print "totally inside (2)"
							go=1
						Else
							If mu1<0
								overlap# = mu2
							Else
								overlap# = l.length - mu2
							EndIf
							'Print "overlap by mu2 = " + String(overlap)
						EndIf
					EndIf
					If Not go
						If overlap > overlaplimit go = 1
					EndIf
				EndIf
				If go
					linktriangles(Self , side , tri , li.theirside)
				EndIf
			EndIf
			
			Rem
			dir = -Sgn(l2.dx * l.dx + l2.dy * l.dy)
			If l.dx = 0
				mu1# = (l2.ey - l.y) / l.dy
				mu2# = (l2.y - l.y) / l.dy
				
			Else
				mu1# = (l2.ex - l.x) / l.dx
				mu2# = (l2.x - l.x) / l.dx
				
			EndIf
			Print dir
			Print mu1/l.length
			Print mu2/l.length
			If (mu1 >= 0 And mu1 <= l.length) Or (mu2 >= 0 And mu2 <= l.length)
				Print "ok!"
				linktriangles(Self , side , tri , li.theirside) 
			EndIf
			EndRem
		Next
	End Method
	
	Method removelinksto(tri:triangle)
		For i = 0 To 2
			For li:link = EachIn links[i]
				If li.tri = tri
					links[i].remove li
				EndIf
			Next
		Next
	End Method
	
	Method fold(f:fold)
		folds.addlast f
		f.foldees.addlast Self
		reflect(f.l)
	End Method
	
	Method unfold(f:fold)
		reflect(f.l)
		folds.remove f
		For i = 0 To 2
			For li:link = EachIn links[i]
				If li.f = f li.f = Null
			Next
		Next
	End Method
	
	Method reflect(l:line)
		pan#=l.an+90
		c# = Cos(pan)
		s# = Sin(pan)
		l2:line = line.create(x1 , y1 , x1 + c , y1 + s)
		mu# = l2.intersect(l , 0) 
		'Print mu
		x1:+ 2 * mu * l2.dx
		y1:+ 2 * mu * l2.dy
		l2:line = line.create(x2 , y2 , x2 + c , y2 + s)
		mu# = l2.intersect(l , 0)
		'Print mu
		x2:+ 2 * mu * l2.dx
		y2:+ 2 * mu * l2.dy
		l2:line = line.create(x3 , y3 , x3 + c , y3 + s)
		mu# = l2.intersect(l , 0)
		'Print mu
		x3:+ 2 * mu * l2.dx
		y3:+ 2 * mu * l2.dy
		
		rejiggle(x1,y1,x2,y2,x3,y3)
	End Method
	
	Method sideofline(l:line)
		pan#=l.an + 90
		l2:line = line.create(cx , cy , cx + Cos(pan) , cy + Sin(pan) )
		mu# = l2.intersect(l , 0) 
		'Print mu
		Return Sgn(mu)
	End Method
	
	Method docentroid()
		cx1# = lines[0].x + lines[0].dx * lines[0].length / 2
		cy1# = lines[0].y + lines[0].dy * lines[0].length / 2
		cx2# = lines[1].x + lines[1].dx * lines[1].length / 2
		cy2# = lines[1].y + lines[1].dy * lines[1].length / 2
		l1:line = line.create(lines[2].x , lines[2].y , cx1 , cy1)
		l2:line = line.create(lines[0].x , lines[0].y , cx2 , cy2) 
		'l1.draw(400 , 400)
		'l2.draw(400,400)
		mu# = l1.intersect(l2)
		Local pos#[2]
		cx# = l1.x + mu * l1.dx
		cy# = l1.y + mu * l1.dy
		
	End Method
	
	Method pointinside(px# , py#)
		For i = 0 To 2
			If lines[i].pointintersect(px , py) = - 1
				Return 0
			EndIf
		Next
		Return 1
	End Method 
End Type

Type link
	Field tri:triangle , myside , theirside , f:fold
	
	Function create:link(tri:triangle , myside , theirside , f:fold = Null)
		li:link = New link
		li.tri = tri
		li.myside = myside
		li.theirside = theirside
		li.f=f
		
		Return li
	End Function
End Type

Global allincisions:tlist = New tlist
Type incision
	Field l:line
	Field side , mu#
	
	Method New()
		allincisions.addlast Self
	End Method
	
	Function create:incision(l:line , side , mu#)
		inc:incision = New incision
		inc.l = l
		inc.side = side
		inc.mu = mu
		Return inc
	End Function
	
	Method die()
		allincisions.remove Self
	End Method
End Type

Global allfolds:tlist = New tlist
Type fold
	Field l:line
	Field foldees:tlist
	
	Method New()
		allfolds.addlast Self
		foldees = New tlist
	End Method
	
	Function create:fold(l:line)
		f:fold = New fold
		f.l = l
		
		Return f
	End Function
	
	Method unfold()
		'Print "UNFOLD"
		For tri:triangle = EachIn foldees
			tri.unfold(Self)
		Next
		allfolds.remove Self
	End Method
End Type

Global linecount=0
Type line
	Field x#,y#,ex#,ey#,dx#,dy#,length#,an#
	Field id
	Field red,green,blue,alpha#
	
	Function create:line(x1# , y1# , x2# , y2#,red=255,green=255,blue=255,alpha#=1)
		l:line = New line
		l.id = linecount
		linecount:+1
		l.x = x1
		l.y = y1
		dx# = x2 - x1
		dy# = y2 - y1
		l.length=Sqr(dx*dx+dy*dy)
		l.dx=dx/l.length
		l.dy=dy/l.length
		l.an=ATan2(dy,dx)
		l.ex = x1+l.dx*l.length
		l.ey = y1 + l.dy * l.length
		l.red = red
		l.green = green
		l.blue = blue
		l.alpha = alpha
		
		Return l
	End Function
	
	Method draw(ox# , oy#)
		SetAlpha alpha'*.8+.2
		SetColor red,green,blue
		DrawLine x + ox , y + oy , ox + x + dx * length , oy + y + dy * length
		SetAlpha 1
		SetColor 255 , 255 , 255
		'DrawText Int(alpha),ox+x+dx*length/2,oy+y+dy*length/2
		'endx# = ox + x + dx * (length-5)
		'endy# = oy + y + dy * (length-5)
		'DrawLine endx,endy,endx+Cos(an+150)*10,endy+Sin(an+150)*10
		'DrawLine endx,endy,endx+Cos(an-150)*10,endy+Sin(an-150)*10
		'DrawText an,x+ox,y+oy
	End Method
	
	Method intersect#(l:line,fit=2)
		If l.dx = 0
			mu# = (l.x - x) / dx
		Else
			mu# = (l.y - y + (l.dy / l.dx) * (x - l.x) ) / (dy - l.dy * dx / l.dx)
		EndIf
		'DrawRect x + 400 + dx * mu , y + 400 + dy * mu , 5 , 5
		
		If fit = 0
			Return mu
		EndIf
		
		If l.dx=0
			lambda# = (y - l.y + mu * dy) / l.dy
		Else
			lambda# = (x - l.x + mu * dx) / l.dx
		EndIf
		
		'DrawText lambda,x+400+mu*dx,y+400+mu*dy
		If mu >= 0 And mu <= length
			Select fit
			Case 2
				If lambda <= l.length And lambda >= 0
					hit = mu
				Else
					hit = - 1
				EndIf
			Case 1
				If lambda >= 0
					hit = mu
				Else
					hit = - 1
				EndIf
			End Select
		Else
			hit = - 1
		EndIf
		Return hit
	End Method
	
	Method pointintersect#(px# , py#) 
		pdx# = Cos(an - 90)*5
		pdy# = Sin(an - 90)*5
		l:line = line.create(px , py , px + pdx , py + pdy)
		'l.draw(400 , 400)
		mu# = intersect(l , 1)
		Return mu
	End Method
	
	Method pointdistance#(px#,py#)
		pdx# = Cos(an - 90)*5
		pdy# = Sin(an - 90)*5
		l:line = line.create(px , py , px + pdx , py + pdy)
		'l.draw(400 , 400)
		mu# = l.intersect(Self , 0)
		Return mu
	End Method	
End Type

Function linktriangles(t1:triangle , side1 , t2:triangle , side2, f:fold = Null)
	t1.makelink(t2 , side1, side2, f) 
	t2.makelink(t1 , side2, side1, f) 
End Function

Function recentre()
	tarea# = 0
	tx# = 0
	ty# = 0
	For tri:triangle = EachIn triangles
		tarea:+ tri.area
		tx:+ tri.cx * tri.area
		ty:+ tri.cy * tri.area
	Next
	tx:/ tarea
	ty:/ tarea
	scrollx = - tx + 400
	scrolly = - ty + 400
End Function


t1:triangle=triangle.create( - 300 , - 300 , 300 , - 300 , - 300 , 300)
t2:triangle = triangle.create( - 300 , 300 , 300 , - 300 , 300 , 300)
t1.lines[1].alpha = 0
t2.lines[0].alpha = 0
linktriangles(t1,1,t2,0)

AppTitle="Folding and cutting but no sticking because I am not allowed the glue any more"
Graphics 800 , 800 , 0
SetBlend ALPHABLEND

sx# = - 1
sy# = - 1
state = 0
Global mx , my
n = 0
frame = 0
Global tscrollx# = 400 , tscrolly# = 400
Global scrollx# = tscrollx , scrolly# = tscrolly
osx# = 0
osy# = 0

Global hoopy:triangle=Null

While Not KeyHit(KEY_ESCAPE)
	'scrollx#:+ (tscrollx - scrollx) * .1
	'scrolly#:+ (tscrolly - scrolly) * .1
	mx = MouseX()-scrollx
	my = MouseY()-scrolly
	mhit1 = MouseHit(1)
	mhit2 = MouseHit(2)
	Select state
	Case 0
		If mhit1
			go=1
			For tri:triangle = EachIn triangles
				If tri.pointinside(mx , my)
					go = 0
				EndIf
			Next
			If go
				sx = mx
				sy = my
				state = 1
			EndIf
		EndIf
	Case 1
		SetColor 255 , 0 , 0
		DrawLine sx + scrollx , sy + scrolly, mx + scrollx, my + scrolly
		SetColor 255 , 255 , 255
		
		If mhit1 Or mhit2
			'Print "hit "+String(mhit1+2*mhit2)
			l:line = line.create(sx , sy , mx , my,255,0,0)
			If mhit2
				f:fold = fold.create(l)
			Else
				f:fold = Null
			EndIf
			checklist:tlist=triangles.copy()
			For tri:triangle = EachIn checklist
				tri.cut(l,f)
			Next
			
			checklist = triangles.copy()
			biglist:tlist = Null
			bigarea = - 1
			lists:tlist=New tlist
			While checklist.count()
				list:tlist = New tlist
				lists.addlast list
				triangle(checklist.first() ).findjoins(list,mhit1)
				area#=0
				For tri:triangle = EachIn list
					checklist.remove tri
					area:+ tri.area
				Next
				'Print area
				If area > bigarea Or bigarea = - 1
					bigarea = area
					biglist = list
				EndIf
			Wend
			If mhit1
				'Print "CUT!"
				If biglist
					For tri:triangle = EachIn triangles
						If Not biglist.contains(tri)
							tri.die()
						EndIf
					Next
				EndIf
				recentre()
				
			ElseIf mhit2
				'Print "FOLD!"
				For list:tlist = EachIn lists
					tri:triangle = triangle(list.first() )
					side = tri.sideofline(l)
					'Print side
					If side = 1
						For tri = EachIn list
							tri.fold(f)
						Next
					EndIf
				Next
				If hoopy
					For i = 0 To 2
						Print hoopy.lines[i].alpha
					Next
				EndIf
				recentre()
				mhit2=0
			EndIf
			lists=Null
			
			state = 0
			
		EndIf
	End Select
	
	If MouseDown(3)
		scrollx = osx + MouseX() - startdragx
		scrolly = osy + MouseY() - startdragy
	Else
		startdragx = MouseX()
		startdragy = MouseY()
		osx# = scrollx
		osy# = scrolly
	EndIf
	
	frame:+ 1
	If frame Mod 20=0
		n = n + 1
	EndIf
	n=n Mod triangles.count()
	c = 0
	area#=0
	For tri:triangle = EachIn triangles
		area:+tri.area
		'If c=n
			'For i = 0 To 2
			'	tri.lines[i].pointintersect(mx , my)
			'Next
			'If tri.pointinside(mx , my)
			'	DrawText "BONZA" , 400 , 0
			'EndIf
			tri.draw(scrollx , scrolly)
		'EndIf
		c:+ 1
	Next
	DrawText "Total area: "+String(Int(area)),0,0
	DrawText mx , 0 , 15
	DrawText my,30,15
	For inc:incision = EachIn allincisions
		inc.l.draw(scrollx , scrolly)
	Next
	For f:fold = EachIn allfolds
		If f=allfolds.last()
			d# = f.l.pointdistance(mx , my)
			DrawText d,f.l.x+scrollx,f.l.y+scrolly
			If Abs(d) < 5
				For tri:triangle = EachIn f.foldees
					tri.draw(scrollx , scrolly)
				Next
				f.l.green = 255
				If state = 0 And mhit2
					f.unfold()
					recentre()
				EndIf
			Else
				f.l.green = 0
			EndIf
		EndIf
		f.l.draw(scrollx , scrolly)
	Next
	
	Flip
	Cls
Wend