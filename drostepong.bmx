Global balls:tlist = New tlist
Type ball
	Field an# , z# , van# , vz#
	Method New()
		balls.addlast Self
		an=Rand(360)
	End Method
	
	Function create:ball()
		b:ball = New ball
		'fill in, maybe?
		Return b
	End Function
	
	Method update()
		an:+ van
		z:+ vz
		If Abs(Floor(z)-z)<.05
			For p:bat = EachIn bats
				If Abs(fixangle(an - p.an) ) < p.width
					vz = - vz
					Return
				EndIf
			Next
		EndIf
	End Method
	
	Method draw()
		Local pos#[2]
		'For pz = Floor(zoom) - 5 To Floor(zoom) + 1
			drostedline(an - 2 , z - .05 , an + 2 , z - .05)
			drostedline(an - 2 , z - .05 , an - 2 , z + .05)
			drostedline(an - 2 , z + .05 , an + 2 , z + .05)
			drostedline(an + 2 , z + .05 , an + 2 , z - .05)
		'Next
	End Method
End Type

Global bats:tlist=New tlist
Type bat
	Field an# , z#
	Field width#
	Field ai
	Field red , green , blue
	Field score
	
	Method New()
		bats.addlast Self
		width=10
	End Method
	
	Function create:bat(ai , red , green , blue)
		p:bat = New bat
		p.ai = ai
		p.red = red
		p.green = green
		p.blue = blue
		Return p
	End Function
	
	Method draw()
		drosteline(an - width , z , an + width , z)
	End Method
End Type

Global zoom#=0

Function drostedpoint:Float[](an# , z#)
	Local pos#[2]
	z:- zoom
	If z < 0
		r# = 300 / (1 - z)
	Else
		r# = 300 * (z + 1)
	EndIf
	DrawText z,0,0
	DrawText r , 0 , 15
	
	x# = 400 + Cos(an) * r
	y# = 400 + Sin(an) * r
	pos = [x , y]
	Return pos
End Function

Function drostedline(an1# , z1# , an2# , z2#)
	Local pos1#[2] , pos2#[2]
	pos1 = drostedpoint(an1 , z1)
	pos2 = drostedpoint(an2 , z2)
	dx# = pos2[0] - pos1[0]
	dy# = pos2[1] - pos1[1]
	d# = dx * dx + dy * dy
	If d > 1600
		t# = 0
		d = Sqr(d)
		inc# = 10.0 / d
		inc#=.25
		While t < 1
			t:+ inc
			If t > 1 t = 1
			pos2 = drostedpoint(an1 * (1 - t) + t * an2 , z1 * (1 - t) + t * z2)
			DrawLine pos1[0] , pos1[1] , pos2[0] , pos2[1]
			pos1=pos2
		Wend
	Else
		DrawLine pos1[0] , pos1[1] , pos2[0] , pos2[1]
	EndIf
End Function

Function drosteline(an1# , z1# , an2# , z2#)
	pz#=0
	For pz = Floor(zoom)- 10 To Floor(zoom)+1
		drostedline(an1 , z1 + pz , an2 , z2 + pz)
	Next
End Function

Function fixangle#(an#)
	an = an Mod 360
	If an > 180 an:- 360
	If an < - 180 an:+ 360
	Return an
End Function

Graphics 800 , 800 , 0
SetBlend ALPHABLEND

p1:bat = bat.create(0 , 0 , 0 , 255)
b:ball = ball.create()
b.z = .5
b.vz = - .05
b.van=1
While Not KeyHit(KEY_ESCAPE)
	mx=MouseX()-400
	MoveMouse 400,400
	p1.an:+ mx
	DrawOval 360,360,80,80

	For p:bat = EachIn bats
		p.draw()
	Next
	
	b.update()
	b.draw()
	'zoom:-.01
	zoom:+(b.z-zoom)*.1
	
	Flip
	Cls
Wend