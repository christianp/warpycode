Global planets:tlist=New tlist
Type planet
	Field x# , y#
	Field vx#,vy#
	Field r#
	Field dir#
	Field spin#
	
	Method New()
		planets.addlast Self
	End Method
	
	Function create:planet(x# , y# , r#)
		p:planet = New planet
		p.x = x
		p.y = y
		p.r = r
		p.dir = 1
		Return p
	End Function
	
	Method control()
		dx# = mx - x
		dy# = my - y
		an# = ATan2(dy , dx)
		vx:+ Cos(an) * .1
		vy:+ Sin(an) * .1
		
		If MouseDown(2)
			If dir > - 1
				dir:- (1 + dir) * .01
			EndIf
		EndIf
		If MouseDown(1)
			If dir < 1
				dir:+ (1 - dir) * .01
			EndIf
		EndIf
	End Method
	
	Method update()
		vx:* .92
		vy:* .92
		x:+ vx
		y:+ vy
		spin:+ dir * 5
		dir:*.99
	End Method
	
	Method draw()
		DrawOval x - r , y - r , r * 2 , r * 2
		
		SetColor 0,0,0
		For an# = 0 To 360 Step 60
			DrawLine x , y , x + Cos(an + spin) * r , y + Sin(an + spin) * r
		Next
		SetColor 255,255,255
	End Method
End Type

Global spots:tlist=New tlist
Type spot
	Field x# , y#
	Field vx#,vy#
	Field speed#
	
	Method New()
		spots.addlast Self
	End Method
	
	Function create:spot(x# , y#,speed#)
		s:spot = New spot
		s.x = x
		s.y = y
		s.speed=speed
		Return s
	End Function
	
	Method update()
		t# = 0
		avx# = 0
		avy# = 0
		v#=Sqr(vx*vx+vy*vy)
		For p:planet = EachIn planets
			dx# = x - p.x
			dy# = y - p.y
			d#=Sqr(dx*dx+dy*dy)
			an# = ATan2(dy , dx)
			d2#=(1.2*p.r-d)
			avx:+ Cos(an+90) * p.r * p.dir / d + Cos(an)*.0001*d2*Abs(p.dir)
			avy:+ Sin(an+90) * p.r * p.dir / d + Sin(an)*.0001*d2*Abs(p.dir)
			t:+ p.r / d
			If d < p.r
				dotprod# = (Cos(an + 90) * vx + Sin(an + 90) * vy)/d
				p.dir:+ dotprod * 1.0/p.r
			EndIf
		Next
		avx:*speed/ t
		avy:* speed / t
		
		
		vx = vx * .9 + avx+inx+Rnd(-.1,.1)
		vy = vy * .9 + avy+iny+Rnd(-.1,.1)
		x:+ vx
		y:+ vy
	End Method
	
	Method draw()
		DrawRect x , y , 1 , 1
	End Method
End Type


For c = 1 To 1000
	spot.create(Rand(800) , Rand(800), Rnd(.5,2.5) ) 
Next

myplanet:planet = planet.create(200 , 200 , 50)
planet.create(600,600,25)


Global mx,my

Graphics 800 , 800 , 0
SetBlend ALPHABLEND

While Not KeyHit(KEY_ESCAPE)
	
	mx = MouseX()
	my = MouseY()
	
	
	myplanet.control
	
	For p:planet = EachIn planets
		p.draw()
		p.update()
	Next
	
	For s:spot = EachIn spots
		s.update()
		s.draw()
	Next
	
	Flip
	Cls
Wend