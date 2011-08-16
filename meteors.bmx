Global width=800
Global height=600
Const burnrate#=1

Graphics width,height,0

Type meteor
	Field x#,y#,vx#,vy#
	Field size#
End Type

SetColor 255,255,255
SetClsColor 0,0,255
meteorlist:TList=New TList
While Not KeyHit(KEY_ESCAPE)
	DrawText "!",0,0
	If Rand(10)=1
		m:meteor=New meteor
		m.x=Rand(width)
		m.y=40
		m.vx=Rnd(-2,2)
		m.vy=Rnd(10,30)
		m.size=Rnd(5,20)
		meteorlist.addlast(m)
	EndIf
	
	SetBlend ALPHABLEND
	SetAlpha .5
	For m:meteor=EachIn meteorlist
		DrawText m.x,m.x,m.y
		m.x=m.x+m.vx
		m.y=m.y+m.vy
		
		pressure#=m.y:Float/height
		m.size=m.size-pressure*burnrate
		
		SetAlpha 1
		SetColor 100,0,0
		DrawOval m.x-m.size,m.y-m.size,m.size*2,m.size*2
		SetAlpha pressure
		SetColor 200+Rand(55),200+Rand(55),0
		burnheight#=pressure*m.size*Rnd(1.9,2.1)
		flamewidth#=m.size*(1+pressure)
		DrawOval m.x-flamewidth,m.y-m.size-burnheight,flamewidth*2,m.size*2+burnheight
		If m.y>height Or m.size<=0
			meteorlist.remove m
		EndIf
	Next
	
	Flip
	Cls
Wend