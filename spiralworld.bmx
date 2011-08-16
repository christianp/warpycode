Const gwidth = 800
Const gheight = 600
Graphics gwidth,gheight,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

Global centrex#,centrey#

Const planeth# = 100
Const layerdiff=150
Global planetr# = (gwidth*gwidth/4 - planeth * planeth) / (2 * planeth) + planeth
Global theta# = ASin(gwidth / (2 * planetr) )
Global thetaU# = ASin( (gwidth / 2) / (planetr + layerdiff) )
Global thetaD# = ASin( (gwidth / 2) / (planetr - layerdiff) )
Global drawan#=0

Print thetad
Print r



Function drawworld()
	centrex# = gwidth/2
	centrey# = gheight/2 + planetr - planeth/2
	
	an# = 0
	ox# = gwidth/2
	oy# = gheight/2-planeth/2
	While an <= theta
		px# = screenx(an)
		py# = screeny(an)
		DrawLine ox , oy , px , py
		DrawLine gwidth-ox,oy,gwidth-px,py
	'	DrawLine ox,oy+layerdiff,px,py+layerdiff
	'	DrawLine ox,oy-layerdiff,px,py-layerdiff
		ox = px
		oy = py
		If an = theta
			an:+ 1
		Else
			an:+ 1
			If an > theta an = theta
		EndIf
	Wend

	an# = 0
	ox = gwidth / 2
	oy# = gheight / 2 - planeth / 2 + layerdiff
	While an <= thetaD
		px# = screenx(an-360)
		py# = screeny(an-360)
		DrawLine ox , oy , px , py
		DrawLine gwidth-ox,oy,gwidth-px,py
		ox = px
		oy = py
		If an = thetaD
			an:+ 1
		Else
			an:+ 1
			If an > thetaD an = thetaD
		EndIf
		'Print an
	Wend

	an# = 0
	ox = gwidth / 2
	oy# = gheight / 2 - planeth / 2 - layerdiff
	While an <= thetaU
		px# = screenx(an+360)
		py# = screeny(an+360)
		DrawLine ox , oy , px , py
		DrawLine gwidth-ox,oy,gwidth-px,py
		ox = px
		oy = py
		If an = thetaU
			an:+ 1
		Else
			an:+ 1
			If an > thetaU an = thetaU
		EndIf
		'Print an
	Wend
End Function

Function correctr#(pos#)
	If pos >= - theta 
		If pos <= theta
			r = planetr
		Else
			r = planetr + layerdiff
		EndIf
	Else
		r = planetr - layerdiff
	EndIf
	Return r
End Function

Function screenx(pos#)
	r#=correctr(pos)
	Return centrex + Cos(pos - 90) * r
End Function

Function screeny(pos#)
	r#=correctr(pos)
	Return centrey + Sin(pos - 90) * r
End Function

Function onscreen(pos#)
	If pos >= - theta
		If pos <= theta
			Return 1
		Else
			pos:- 360
			If pos >= - thetaU And pos <= thetaU
				Return 1
			Else
				Return 0
			EndIf
		EndIf
	Else
		pos:+ 360
		If pos >= - thetaD And pos <= thetaD
			Return 1
		Else
			Return 0
		EndIf
	EndIf
End Function

Global trees:tlist=New tlist
Type tree
	Field pos#
	Field height#
	Field name$
	
	Method New()
		trees.addlast Self
		name=Chr(Rand(Asc("a"),Asc("Z")))
	End Method
	
	Function create:tree(pos# , height#)
		t:tree = New tree
		t.pos = pos
		t.height = height
		
		t.name:+String(t.pos)
		Return t
	End Function
	
	Method draw()
		diff# = (pos - drawan)*1
		If onscreen(diff)
			totdrawn:+1
			x# = screenx(diff)
			y# = screeny(diff)
			
			DrawRect x - 3 , y - 3 , 6 , 6
			SetRotation diff
			DrawText name , x , y
			SetRotation 0
		EndIf
	End Method
End Type

Global totdrawn

For c = 1 To 100
	tree.create(Rnd( - 3600 , 3600) , Rnd(30 , 100) )
Next

While Not KeyHit(KEY_ESCAPE)

	mx = MouseX() - gwidth / 2
	my = MouseY() - gheight / 2
	
	drawan:+mx*.002

	drawworld()
	
	totdrawn=0
	For t:tree = EachIn trees
		t.draw()
	Next
	DrawText totdrawn,0,30

	Flip
	Cls
Wend