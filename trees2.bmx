Type segment
	Field ll# , rl# , width#
	Field mytree:tree
	Field lbranch:tree , rbranch:tree
	Field age#
	Field tilt#
	
	Method New()
		age = 1
	End Method
	
	Function create:segment(ll# , rl# , width#, mytree:tree)
		s:segment = New segment
		s.ll = ll
		s.rl = rl
		s.width = width
		s.mytree=mytree
		
		Return s
	End Function
End Type

Global plof=0

Global trees:tlist=New tlist
Type tree
	Field segments:tlist
	Field x#
	Field growth#
	Field branched
	
	Method New()
		trees.addlast Self
		segments = New tlist
	End Method
	
	Function create:tree(x# , basewidth#)
		t:tree = New tree
		t.x = x
		
		s:segment = segment.create(0,0,basewidth,t)
		t.segments.addlast s
		growth=1
		
		Return t
	End Function
	
	Method newsegment(branch)
		os:segment = segment(segments.last() )
		If Rand(2) = 1 And trees.contains(Self) And segments.count() > 5 And Not branched And Not branch
			branched=1
			If Rand(1,2)=2
				Print "lbranch"
				os.lbranch = tree.create( - 1 , os.ll)
				trees.remove os.lbranch
			Else
				Print "rbranch"
				os.rbranch = tree.create(-1 , os.rl)
				trees.remove os.rbranch
			EndIf
		Else
			branched=0
		EndIf
		If branch
			width#=os.width * Rnd(0.6 / Sqr(segments.count() ) , 1)
		Else
			width# = os.width * Rnd(0.9 / Sqr(segments.count() ) , 1)
		EndIf
		ratio# = Cos(os.tilt) ^ (Sgn(tilt) * .2)
		Print ratio
		Print os.tilt
		s:segment = segment.create(Rnd(.5 , 1.5)*os.width*ratio , Rnd(.5 , 1.5)*os.width*ratio, width , Self)
		segments.addlast s
	End Method
	
	Method update(grow# = .29,branch=0,branchwidth#=0)
		g# = grow * Rnd(.5 , 1) / (segments.count()+1)
		growth:+ g


		'DrawText g , 0 , plof * 13
		'If branch
		'	DrawText branchwidth,120,plof*13
		'EndIf

		plof:+1


		n=branch+1
		For s:segment = EachIn segments
			grow# = g / (Sqr(n) * s.width)
			s.width:+ 100 * grow * (1-Sgn(branch)*.8)
			s.age:*(1-g*.09)
			If branch
				If s.width > branchwidth 
					s.width = branchwidth
					'g = 0
				EndIf
				'branchwidth:* .7
			EndIf
			If s.lbranch
				ss:segment=segment(s.lbranch.segments.first())
				s.lbranch.update(grow*250 , n , s.ll)
			EndIf
			If s.rbranch
				ss:segment=segment(s.rbranch.segments.first())
				s.rbranch.update(grow*250 , n , s.rl)
			EndIf
			's.ll:+ grow'*.1
			's.rl:+ grow'*.1
			n:+1
		Next
		
		If growth >= 1
			newsegment(branch)
			growth = 0
		EndIf
	End Method
	
	Method draw()
		l:TLink=segments.firstlink()
		os:segment=segment(l.value())
		ox1# = x - os.width / 2
		oy1# = gheight
		ox2# = x + os.width / 2
		oy2# = gheight
		DrawLine ox1,oy1,ox2,oy2
		l = l.nextlink()
		
		If l drawsegment(l,ox1,oy1,ox2,oy2,os.width)
		
		Rem
		
		While l <> Null
			s:segment = segment(l.value() )
			l = l.nextlink()
			If l = Null
				ll# = s.ll * growth
				rl# = s.rl * growth
			Else
				ll# = s.ll
				rl# = s.rl
			EndIf
			dx# = (ox2 - ox1) / os.width
			dy# = (oy2 - oy1) / os.width
			nx# = dy
			ny# = - dx
			mx# = (ox2 + ox1) / 2
			my# = (oy2 + oy1) / 2
			
			
			x1# = mx + nx * ll - dx * s.width / 2 
			y1# = my + ny * ll - dy * s.width / 2
			x2# = mx + nx * rl + dx * s.width / 2 
			y2# = my + ny * rl + dy * s.width / 2
			
			DrawLine ox1 , oy1 , x1 , y1
			DrawLine ox2 , oy2 , x2 , y2
			DrawLine x1,y1,x2,y2
			
			ox1 = x1
			oy1 = y1
			ox2 = x2
			oy2 = y2
			
			os=s
		Wend
		
		DrawLine x1 , y1 , x2 , y2
		EndRem
	End Method
End Type

Function drawsegment(l:TLink , ox1# , oy1# , ox2# , oy2# , owidth#)
	'DrawText Int(owidth),ox1,oy1
	s:segment = segment(l.value() )
	l = l.nextlink()
	If l = Null
		growth#=s.mytree.growth
		width# = s.width * growth
		ll# = s.ll * growth
		rl# = s.rl * growth
	Else
		width# = s.width
		ll# = s.ll
		rl# = s.rl
	EndIf
	dx# = (ox2 - ox1) / owidth
	dy# = (oy2 - oy1) / owidth
	nx# = dy
	ny# = - dx
	mx# = (ox2 + ox1) / 2
	my# = (oy2 + oy1) / 2
	
	
	x1# = mx + nx * ll - dx * width / 2 
	y1# = my + ny * ll - dy * width / 2
	x2# = mx + nx * rl + dx * width / 2 
	y2# = my + ny * rl + dy * width / 2
	
	Local poly#[]
	poly = [ox1 , oy1 , x1 , y1 , x2 , y2 , ox2 , oy2]
	
	'160,200,0
	'100,50,10
	age#=s.age
	SetColor 160*age+100*(1-age),200*age+50*(1-age),10*(1-age)
	DrawPoly poly
	SetColor 255,255,255
	'DrawLine x1,y1,x2,y2
	
	s.tilt = ATan2(ny , nx) + 90
	
	If s.lbranch
		bl:TLink=s.lbranch.segments.firstlink()
		drawsegment(bl , ox1 , oy1 , x1 , y1 , ll)
	Else
		DrawLine ox1 , oy1 , x1 , y1
	EndIf
	If s.rbranch
		bl:TLink=s.rbranch.segments.firstlink()
		drawsegment(bl , x2 , y2 , ox2 , oy2 , rl)
	Else
		DrawLine ox2 , oy2 , x2 , y2
	EndIf
	
	If l
		drawsegment(l , x1 , y1 , x2 , y2 , s.width)
	Else
		DrawLine x1 , y1 , x2 , y2
		DrawText s.tilt,x1,y1
	EndIf
End Function

Global winds:tlist=New tlist
Type wind
	Field pos#[10 , 2]
	Field direction
	Field vy#,ay#
	
	Method New()
		winds.addlast Self
	End Method
	
	Function create:wind(y# , direction)
		w:wind = New wind
		For i = 0 To 9
			w.pos[i , 1] = y
			w.pos[i , 0] = (1 - direction) * gwidth / 2
		Next
		w.vy#=Rnd(-10,10)
		w.direction = direction
	End Function
	
	Method update()
		tot# = 0
		For i = 9 To 1 Step -1
			tot:+pos[i,1]
			pos[i , 0] = pos[i - 1 , 0]
			pos[i , 1] = pos[i - 1 , 1]
		Next
		tot:/9
		pos[0 , 0]:+ direction * 7
		f# = - (pos[0 , 1] - tot) * .00001 - Sgn(vy)*vy*vy*.01 + Rnd(-1,1)*windiness
		ay :+ f
		ay:*.9
		vy :+ ay 

		If Abs(vy) > 5
		'	vy = Sgn(vy) * 5
		EndIf
		pos[0 , 1]:+ vy
		
		If pos[0 , 0] > gwidth + 10 Or pos[0 , 0] < - 10
			winds.remove Self
		EndIf
	End Method
	
	Method draw()
		SetColor 255 , 255 , 255
		SetAlpha .5
		For i = 0 To 8
			DrawLine pos[i , 0] , pos[i , 1] , pos[i + 1 , 0] , pos[i + 1 , 1]
		Next
		SetAlpha 1
	End Method
End Type

Const gwidth = 400 , gheight = 800
Graphics gwidth , gheight , 0
SetBlend ALPHABLEND
SetClsColor 0 , 0 , 255
SeedRnd MilliSecs()

t:tree = tree.create(gwidth / 2 , 10)

wind.create(400 , 1)

Global windiness# = 0
tick=0

While Not KeyHit(KEY_ESCAPE)
	plof = 0
	
	tick:+1
	
	windiness#=(Sin(tick*.001)*.5+.5)*.2
	
	If Rand(3) = 1
		wind.create(Rnd(20 , gheight - 20) , Rand(0 , 1) * 2 - 1) 
	EndIf
	
	For w:wind = EachIn winds
		w.update()
		w.draw()
	Next
	
	For t:tree = EachIn trees
		t.update()
		t.draw()
	Next
	
	Flip
	Cls
Wend