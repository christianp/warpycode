Global boxes:TList=New TList
'this is a box. It will accelerate under gravity, spin at a constant rate,
'and fade away as it gets lower
Type box
	Field ox#,oy#,x#,y#
	Field oan#,an#
	Field vx#,vy#,van#
	
	Function Create:box(x#,y#)
		b:box=New box
		b.x=x
		b.y=y
		b.vx=Rnd(-3,3)
		b.van=Rnd(-20,20)
		boxes.addlast b
		Return b
	End Function
	
	Method update()
		'remember previous state
		ox=x
		oy=y
		oan=an
		
		If oy>600
			'if was off screen *last* frame, delete this box
			'shouldn't use current frame to delete box, because on render it might be interpolated back onto the screen
			boxes.remove Self
		EndIf
		
		'move to new state
		vy:+1
		x:+vx
		y:+vy
		an:+van
	End Method
	
	Method draw(alpha#)
		'work out position and rotation at *apparent* time
		cx#=ox+(x-ox)*alpha
		cy#=oy+(y-oy)*alpha
		can#=oan+andiff(an,oan)*alpha	'can't just do (an-oan)*alpha because what if oan=170 and an=-170?
		
		'fade depends on y-position so can use cy instead of keeping track of old fade and new fade
		fade#=1-(cy/600.0)
		
		SetAlpha fade
		SetRotation can
		DrawRect cx,cy,20,20
	End Method
End Type

'this just gives the difference between two angles
Function andiff#(an1#,an2#)
	an1=(an1-an2) Mod 360
	If an1<-180 an1:+180
	If an1>180 an1:-180
	Return an1
End Function



'init graphics
Graphics 800,600,0
SetBlend ALPHABLEND


'set up timing
Global dt#=.01
Global t#=0
Global ctime#=MilliSecs() 'must set ctime to current time because otherwise you do millions of logic steps in the first frame!
Global accumulator#=0

Global deltatime#
Global steps

While 1

	'work out time elapsed since last frame started
	newtime#=MilliSecs()
	deltaTime#=(newtime-ctime)/1000.0
	ctime=newtime
	accumulator:+deltatime
	
	steps=0	'keep track of how many steps done this frame, just for curiosity's sake
	
	While accumulator>dt And MilliSecs()-newtime<1000.0/20
		update
		
		t:+dt
		accumulator:-dt
		steps:+1
	Wend
	
	'render stage
	Local alpha#=accumulator/dt
	
	
	draw alpha
	
	If KeyHit(KEY_ESCAPE) Or AppTerminate()
		End
	EndIf
Wend


Function update()
	Delay 1000*(Cos(t*20)+1)*dt*1.1
End Function

Function draw(alpha#)
	Global ox#
	x#=t*1000 Mod 800
	If x<ox ox=x
	SetColor 0,0,0
	DrawRect 0,0,200,100
	DrawRect x+1,0,2*x-ox,600
	'show some information about the speed of the simulation
	SetColor 255,255,255
	DrawText "dt: "+dt,0,0
	DrawText "logic FPS: "+1/dt,0,15
	DrawText "steps: this frame: "+steps,0,30
	DrawText "display FPS: "+1/deltatime,0,45
	DrawText "accumulator: "+accumulator,0,60
	DrawText "t: "+t,0,75
	
	y#=600-(Cos(t*20)*.5+.5)*600
	DrawLine ox,y,x,y
	
	'draw the boxes
	For b:box=EachIn boxes
		b.draw alpha
	Next
	ox=x

	Flip
	'Cls
End Function