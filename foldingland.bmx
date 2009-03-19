Global lines:tlist=New tlist
Type line
	Field x1# , y1# , x2# , y2#
	Field length# , dx# , dy#
	Method New()
		lines.addlast Self
	End Method
	
	Function create:line(x1# , y1# , x2# , y2#)
		l:line = New line
		l.x1 = x1
		l.x2 = x2
		l.y1 = y1
		l.y2 = y2
		l.dostats()
		Return l
	End Function
	
	Method dostats()
		dx# = x2 - x1
		dy# = y2 - y1
		length# = Sqr(dx * dx + dy * dy)
		dx:/ length
		dy:/ length
	End Method
	
	Method distancefrom#(x# , y#)
		px# = dy
		py# = - dx
		
		mu# = ((x-x1) * py - (y-y1) * px) / (py * dx - px * dy)
		If mu >= 0 And mu <= length
			If px = 0
				lambda# = (y - y1 - mu * dy) / py
			Else
				lambda# = (x - x1 - mu * dx) / px
			EndIf
			Return Abs(lambda)
		ElseIf mu < 0
			pdx# = x - x1
			pdy# = y - y1
			d# = Sqr(pdx * pdx + pdy * pdy) 
			Return d
		Else
			pdx# = x - x2
			pdy# = y - y2
			d# = Sqr(pdx * pdx + pdy * pdy) 
			Return d
		EndIf
	End Method
		
	Method draw()
		SetAlpha 1
		DrawLine x1 , y1 , x2 , y2
	End Method
End Type

Type arc Extends line
	Field x3# , y3#
	
	Field cx# , cy# , radius# , startan# , arclength# , anstep#
	
	Method draw()
		SetColor 255,255,255
		SetAlpha 1
		DrawRect cx , cy , 1 , 1
		an# = startan
		steps = Int(arclength / anstep)
		ox# = cx + radius * Cos(an)
		oy# = cy + radius * Sin(an)
		For i = 1 To steps
			an:+anstep
			x# = cx + radius * Cos(an)
			y# = cy + radius * Sin(an) 
			DrawLine ox , oy , x , y
			ox = x
			oy = y
		Next
		x# = cx + radius * Cos(startan + arclength)
		y# = cy + radius * Sin(startan + arclength)
		DrawLine ox , oy , x , y
		
		SetColor 255 , 0 , 0
		DrawRect x1 - 2 , y1 - 2 , 4 , 4
		SetColor 0,255,0
		DrawRect x2 - 2 , y2 - 2 , 4 , 4
		SetColor 0,0,255
		DrawRect x3 - 2 , y3 - 2 , 4 , 4
		SetColor 255 , 255 , 255
		
		DrawText cx , 0 , 0
		DrawText cy , 0 , 13
		DrawText startan,0,26
	End Method 
		
	Method distancefrom#(x# , y#)
		DrawRect 400,0,10,10
		dx# = x - cx
		dy# = y - cy
		an# = ATan2(dy , dx)
		ad# = andiff(startan , an)
		If ad < 0 ad:+ 360
		If ad < arclength
			d# = Sqr(dx * dx + dy * dy) - radius
			'DrawText d,x,y
			Return Abs(d)
		Else
			dx1# = x - x1
			dy1# = y - y1
			dx2# = x - x2
			dy2# = y - y2
			d1# = Sqr(dx1 * dx1 + dy1 * dy1)
			d2# = Sqr(dx2 * dx2 + dy2 * dy2)
			If d1 < d2
				Return d1
			Else
				Return d2
			EndIf
		EndIf
	End Method
		
End Type

Function createarc:arc(x1# , y1# , x2# , y2# , x3# , y3#)
	ma# = (y2 - y1) / (x2 - x1)
	mb# = (y3 - y2) / (x3 - x2)
	x# = (ma * mb * (y1 - y3) + mb * (x1 + x2) - ma * (x2 + x3) ) / (2 * (mb - ma) )
	y#=-(x-(x1+x2)/2)/ma+(y1+y2)/2
	
	an1# = ATan2(y1 - y , x1 - x)
	an2# = ATan2(y2 - y , x2 - x)
	an3# = ATan2(y3 - y , x3 - x)
	diff1# = andiff(an1 , an2)
	diff2# = andiff(an1 , an3)
	diff3# = andiff(an2 , an3)

	Print diff1
	Print diff2
	Print diff3

	d1# = Abs(diff1)
	d2# = Abs(diff2)
	d3# = Abs(diff3)

	arclength# = diff1
	If arclength < 0 arclength:+ 360
	If diff3<0
		an# = an1
	Else
		an# = an2
		arclength = 360 - arclength
	EndIf
	Rem			
	Local ds#[3]
	Local shorts[3]
	shorts=[0,1,2]
	ds = [d1 , d2 , d3]
	For i = 0 To 2
		n = i
		oi=i-1
		While n < 2
			If ds[shorts[n]] > ds[shorts[n + 1]]
				b = shorts[n]
				shorts[n] = shorts[n + 1]
				shorts[n + 1] = b
				n:+ 1
				i=oi
			Else
				n = 2
			EndIf
		Wend
	Next
	got = 0
	i=-1
	While Not got
		i:+1
		Select shorts[i]
		Case 0 ' point 3 in middle
			If diff1 * diff2 > 0 And d2 < d1 got = 1
		Case 1 'point 2 in middle
			If diff2 * diff3 > 0 And d3 < d1 got = 1
		Case 2 'point 1 in middle
			If diff3 * diff1 > 0 And d1 < d3 got = 1
		End Select
	Wend
	Select shorts[i]
	Case 0 '1 and 2 on line
		arclength#=d1
		If diff1 < 0
			an# = an2
		Else
			an# = an1
		EndIf
	Case 1 '1 and 3 on line
		arclength# = d2
		If diff2 < 0
			an# = an3
		Else
			an# = an1
		EndIf
	Case 2 '2 and 3 on line
		arclength# = d3
		If diff3 < 0
			an# = an3
		Else
			an# = an2
		EndIf
	End Select

	If d1 < d2
		If d1 < d3
			arclength=d1	
			If diff1 < 0
				an = an2
			Else
				an = an1
			EndIf
		Else
			arclength = d3
			If diff3 < 0
				an = an3
			Else
				an = an2
			EndIf
		EndIf
	Else
		If d2 < d3
			arclength = d2
			If diff2 < 0
				an = an3
			Else
				an = an1
			EndIf
		Else
			arclength = d3
			If diff3 < 0
				an = an3
			Else
				an = an2
			EndIf
		EndIf
	EndIf
	
	
	If diff1 * diff2 < 0 'point 1 is the middle one
		Print "1"
		If diff1 < 0 'point 2 is the start
			an# = an2
			arclength# = diff3
		Else 'point 3 is the start
			an# = an3
			arclength# = -diff3
		EndIf
	ElseIf diff1 * diff2 > 0 'point 2 is the middle one
		Print "2"
		If diff1 > 0 'point 1 is the start
			an# = an1
			arclength# = diff2
		Else 'point 3 is the start
			an# = an3
			arclength = - diff2
		EndIf
	Else 'point 3 is the middle one
		Print "3"
		If diff2 > 0 'point 1 is the start
			an# = an1
			arclength = diff1
		Else 'point 2 is the start
			an# = an2
			arclength = - diff1
		EndIf
	EndIf			
	If arclength < 0 arclength:+ 360
	EndRem
	
	dx# = x1 - x
	dy# = y1 - y
	radius#=Sqr(dx*dx+dy*dy)
	anstep#=ACos(1-2.0/radius)
	
	a:arc = New arc
	a.cx = x
	a.cy = y
	a.startan = an
	a.arclength = arclength
	a.radius = radius
	a.anstep = anstep
	a.x1 = x1
	a.y1 = y1
	a.x2 = x2
	a.y2 = y2
	a.x3 = x3
	a.y3 = y3
	
	Return a
End Function

Function andiff#(an1 , an2#)
	d# = (an2 - an1) Mod 360
	If d > 180 d:- 360
	If d < - 180 d:+ 360
	Return d
End Function

AppTitle = "folding land?"
Graphics 800 , 800 , 0
'SetBlend ALPHABLEND

mstate = 0

Global sproing#[80,80]
While Not KeyHit(KEY_ESCAPE)
	
	For ix = 0 To 79
	For iy = 0 To 79
		x# = ix * 10 + 5
		y# = iy * 10 + 5
		td# = 0
		cd# = 0
		
		For l:line = EachIn lines
			d# = l.distancefrom(x , y) + 1
			'td:+ 1 / (d * d)
			f# = 1 * Sqr(d) 
			'If arc(l)
			'	td:+ f
			'Else
				td:- f
			'EndIf
			If f>cd cd=f
		Next
		'SetAlpha td*10
		'DrawText Int(td * 10) , x , y
		diff# = td - sproing[ix , iy]
		sproing[ix,iy]:+diff*.1
		shade# = 255 * sproing[ix,iy] * 5
		If shade > 255 shade = 255
		'SetColor shade,0,0'shade,shade
		'DrawRect ix*10,iy*10,10,10
		px# = (ix * 1.5 + iy * .8) * 2 + 200
		py# = (ix * .8 + iy * ( - 1.5)) * 2 + 400 - sproing[ix,iy]*2
		DrawRect px,py,1,1
	Next
	Next
	
	For l:line = EachIn lines
		l.draw()
	Next
	
	mx# = MouseX()
	my# = MouseY()
		
	SetColor 255,255,255
	Select mstate
	Case 0 'doing nowt
		If MouseHit(1)
			x1# = mx
			y1# = my
			mstate = 1
		EndIf
		If MouseHit(2)
			x1# = mx
			y1# = my
			mstate = 2
		EndIf
	
			
	Case 1 'drawing line
		DrawLine x1,y1,mx,my
		If MouseHit(1)
			x2# = mx
			y2# = my
			line.create(x1 , y1 , x2 , y2)
			mstate = 0
		EndIf
	Case 2 'drawing arc (placing point 2)
		DrawRect x1 - 2 , y1 - 2 , 4 , 4
		If MouseHit(2)
			x2# = mx
			y2# = my
			mstate = 3
		EndIf
	Case 3 'drawing arc (placing point 3)
		DrawRect x1 - 2 , y1 - 2 , 4 , 4
		DrawRect x2 - 2 , y2 - 2 , 4 , 4
		If MouseHit(2)
			x3# = mx
			y3# = my
			createarc(x1 , y1 , x2 , y2 , x3 , y3)
			mstate = 0
		EndIf
	
	End Select
	
	Flip
	Cls
Wend