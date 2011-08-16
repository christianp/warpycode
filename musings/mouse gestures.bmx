Global mousegestures:TList=New TList
Type tmousegesture
	Field ox,oy
	Field done
	
	Method New()
		mousegestures.addlast Self
	End Method
	
	Function updateall()
		x=MouseX()
		y=MouseY()
		For mg:tmousegesture=EachIn mousegestures
			mg.update x,y
		Next
	End Function

	Method update(x,y) Abstract
	
	Method performed()
		odone=done
		done=0
		Return odone
	End Method
End Type

Global mouseshake:tmouseshake=New tmouseshake
Type tmouseshake Extends tmousegesture
	Field dir,shakes,notshake,lastshake
	
	Method update(x,y)
		dx=x-ox
		dy=y-oy
		If Abs(dy)<Abs(dx) And Abs(dx)>5
			If Sgn(dx)<>dir
				dir=Sgn(dx)
				shakes:+1
				If shakes>3
					done=1
				EndIf
				notshake=0
				lastshake=MilliSecs()
			EndIf
		Else
			notshake:+1
			If MilliSecs()-lastshake>100
				shakes=0
				'dir=0
			EndIf
		EndIf
		ox=x
		oy=y
	End Method
	
	Method performed()
		If Super.performed()
			shakes=0
			dir=0
			Return 1
		EndIf
	End Method
End Type


'demo

Global particles:TList=New TList
Type particle
	Field x#,y#,vx#,vy#,ox#,oy#
	Function Create:particle()
		p:particle=New particle
		p.x=300
		p.y=600
		p.vx=Rnd(-2,2)
		p.vy=Rnd(-25,-15)
		particles.addlast p
		Return p
	End Function
	
	Method update()
		ox=x
		oy=y
		x:+vx
		y:+vy
		vy:+.5
		If y>600
			particles.remove Self
		EndIf
	End Method
	
	Method draw()
		DrawLine ox,oy,x,y
	End Method
End Type



Graphics 600,600,0

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	tmousegesture.updateall
	
	DrawText mouseshake.shakes,0,0
	
	If mouseshake.shakes>3
			particle.Create
	EndIf
	
	For p:particle=EachIn particles
		p.update
		p.draw
	Next

	Flip
	Cls
Wend
