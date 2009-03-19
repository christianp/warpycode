Global things:tlist=New tlist
Type thing
	Field x# , y# , vx# , vy#
	Field red,green,blue
	Field oldpos#[20]
	
	Method New()
		things.addlast Self
		colour = Rand(1 , 7)
		red = 50
		green = 50
		blue = 50
		If colour & 1 red=150
		If colour & 2 green=150
		If colour & 4 blue=150
	End Method
	
	Function create:thing(x# , y# , vx# , vy#)
		t:thing = New thing
		t.x = x
		t.y = y
		t.vx = vx
		t.vy = vy
		For i = 0 To 9
			t.oldpos[i * 2] = t.x
			t.oldpos[i * 2 + 1] = t.y
		Next
		Return t
	End Function
	
	Method update()
		x:+ vx
		y:+ vy
		For i = 0 To 17
			oldpos[i] = oldpos[i + 2]
		Next
		oldpos[18] = x
		oldpos[19] = y
	End Method
	
	Method draw()
		SetColor red,green,blue
		v# = Sqr(vx * vx + vy * vy) 
		'DrawText v,x,y
		SetLineWidth 3
		For i = 0 To 8
			DrawLine oldpos[i * 2] , oldpos[i * 2 + 1] , oldpos[i * 2 + 2] , oldpos[i * 2 + 3]
		Next
		'DrawOval x-4,y-4,8,8
	End Method
	
	Method die()
		things.remove Self
	End Method
End Type

Global attractors:tlist=New tlist
Type attractor
	Field x# , y# , mass#
	
	Method New()
		attractors.addlast Self
	End Method
	
	Function create:attractor(x# , y# , mass#)
		a:attractor = New attractor
		a.x = x
		a.y = y
		a.mass = mass
		Return a
	End Function
	
	Method update()
		For t:thing = EachIn things
			dx# = x - t.x
			dy# = y - t.y
			d# = Sqr(dx * dx + dy * dy)
			dx:/ d
			dy:/ d
			iv# = t.vx * dx + t.vy * dy
			'DrawText v,t.x,t.y
			f# = - Sgn(iv) * mass * 100 / d
			v# = Sqr(t.vx * t.vx + t.vy * t.vy)
			'If Abs(f)>v f=0
			t.vx:+ f * dx
			t.vy:+ f * dy
			nv#=Sqr(t.vx*t.vx+t.vy*t.vy)
			t.vx:* v / nv
			t.vy:* v / nv
			If d < 15
				't.die()
				'an# = Rand(360)
				't.create(400+Cos(an)*600,400+Sin(an)*600,Cos(-an)*9,Sin(-an)*9)
			EndIf
		Next
	End Method
	
	Method draw()
		SetColor 100,100,100
		DrawOval x - 10*mass , y - 10*mass , 20*mass , 20*mass
	End Method
End Type

SeedRnd MilliSecs()


Function setup()
	things=New tlist
	For c = 1 To 30
		t:thing = thing.create(Rand(800) , Rand(800) , Rnd( - 3 , 3) , Rnd( - 3 , 3))
	Next
	
	attractors=New tlist
	n=Rand(3 , 5)
	For c = 1 To n
		attractor.create(Rand(100 , 700) , Rand(100 , 700) , Rand(.2 , 2) )
	Next
End Function

AppTitle="Where did you learn physics?"
Graphics 800 , 800 , 0
SetBlend ALPHABLEND
SetClsColor 255 , 255 , 255
Cls
setup()

selected:attractor = Null
ox# = 0
oy# = 0

		colour = Rand(1 , 7)
		If colour & 1 cred = 255 Else cred = 150
		If colour & 2 cgreen = 255 Else cgreen = 150
		If colour & 4 cblue = 255 Else cblue = 150

filltime = 999
filldir = -1

While Not KeyHit(KEY_ESCAPE) 
	mx = MouseX()
	my = MouseY()
	
	For t:thing = EachIn things
		t.update()
		t.draw()
	Next
	
	For a:attractor = EachIn attractors
		a.update()
		a.draw()
	Next
	
	Rem
	If selected
		If Not MouseDown(1)
			selected = Null
		EndIf
		selected.x = mx + ox
		selected.y = my + oy
	Else
		If MouseDown(1)
			mindist#=-1
			For a:attractor=EachIn attractors
				dx# = a.x - mx
				dy# = a.y - my
				d# = dx * dx + dy * dy
				'DrawText d,x,y+10
				If d < mindist Or mindist = - 1
					mindist=d
					selected = a
					ox# = dx
					oy# = dy
				EndIf
			Next
		EndIf
	EndIf
	EndRem
	
	If MouseHit(2) Or MouseHit(1)
		filldir = - 1
		filltime = 0
	EndIf
	
	If Rand(500)=1 And filltime>0
		colour = Rand(1 , 7)
		If colour & 1 cred = 255 Else cred = 150
		If colour & 2 cgreen = 255 Else cgreen = 150
		If colour & 4 cblue = 255 Else cblue = 150
	EndIf
	
	SetBlend ALPHABLEND
	
	filltime:+filldir
	If filltime>0
		SetAlpha .01
	Else
		SetAlpha -filltime*.01
	EndIf
	If filltime = - 100
		filldir = 1
		setup()
	EndIf
	If filltime=1000
		filldir=-1
	EndIf
	SetColor cred,cgreen,cblue
	DrawRect 0 , 0 , 800 , 800
	SetAlpha 1
	'DrawRect 0,0,10,10
	SetBlend ALPHABLEND
	SetColor 255,255,255
	
	Flip
	'Cls
Wend