Global gwidth=800,gheight=600

Global balls:TList=New TList
Type ball
	Field x#,y#		'position
	Field vx#,vy#		'velocity
	Field radius#
	Field mass#
	
	Method New()
		balls.addlast Self
	End Method
	
	Function Create:ball(x#,y#,radius#,mass#)
		b:ball=New ball
		b.x=x
		b.y=y
		b.radius=radius
		b.mass=mass
		b.vx=(gwidth/2-x)*.01
		b.vy=(gheight/2-y)*.01
		Return b
	End Function
	
	Method update()
		x:+vx
		y:+vy
		
		'wrap edges of screen around
		If x<0
			x:+gwidth
		EndIf
		If x>gwidth
			x:-gwidth
		EndIf
		If y<0
			y:+gheight
		EndIf
		If y>gheight
			y:-gheight
		EndIf
	End Method
	
	Function collideAll()
		l:TList=balls.Copy()
		While l.Count()>1
			b:ball=ball(l.removefirst())
			For b2:ball=EachIn l
			
				dx#=b2.x-b.x		'get relative positions of b2 with respect to b
				dy#=b2.y-b.y
				d#=Sqr(dx*dx+dy*dy)	'get square of distance between b and b2
				
				dd#=b.radius+b2.radius-d	'get amount balls overlap
				
				If dd>0	'if distance between balls is less than their radii added together, then they are overlapping
				
					dx:/d	'by dividing the difference between the two balls' positions, we get a unit vector pointing from b to b2
					dy:/d
					
					dvx#=b2.vx-b.vx	'get relative velocity of b2 with respect to b
					dvy#=b2.vy-b.vy
					
					f#=dvx*dx+dvy*dy		'calculate force of impact - project relative velocity vector onto line from b to b2
					f:*2/(b.mass+b2.mass)	
					
					b2.vx:-dx*f*b.mass	'push b2 away from b
					b2.vy:-dy*f*b.mass
					
					b.vx:+dx*f*b2.mass	'push b in the opposite direction
					b.vy:+dy*f*b2.mass
					
					b.x:-dd*dx*b2.mass/(b.mass+b2.mass)	'position the balls so they are just touching and no longer overlap
					b.y:-dd*dy*b2.mass/(b.mass+b2.mass)
					b2.x:+dd*dx*b.mass/(b.mass+b2.mass)
					b2.y:+dd*dy*b.mass/(b.mass+b2.mass)
					
					
				EndIf
				
			Next
		Wend
	End Function
	
	
	Method draw()
		shade#=mass*5
		SetColor shade,shade,shade
		DrawOval x-radius,y-radius,radius*2,radius*2
		
		'because the edges of the screen wrap round, draw again shifted by a screen's width/height to maintain the illusion
		DrawOval gwidth+x-radius,y-radius,radius*2,radius*2
		DrawOval x-radius,gheight+y-radius,radius*2,radius*2
		DrawOval -gwidth+x-radius,y-radius,radius*2,radius*2
		DrawOval x-radius,-gheight+y-radius,radius*2,radius*2
	End Method
	
End Type

Function changeState()
	balls=New TList
	state=(state+1) Mod 5
	Select state
	Case 0
		b1:ball=ball.Create(400,300,50,50)
		b1.vx=0
		b1.vy=0
		b2:ball=ball.Create(200,300,50,50)
		b2.vx=1
		b2.vy=0
	Case 1
		b1:ball=ball.Create(400,300,50,50)
		b1.vx=0
		b1.vy=0
		b2:ball=ball.Create(200,300,20,20)
		b2.vx=1
		b2.vy=0
	Case 2
		b1:ball=ball.Create(400,300,50,50)
		b1.vx=1
		b1.vy=0
		b2:ball=ball.Create(200,300,50,50)
		b2.vx=2
		b2.vy=0
		
		b3:ball=ball.Create(400,450,50,50)
		b3.vx=0
		b3.vy=0
		b4:ball=ball.Create(200,450,50,50)
		b4.vx=1
		b4.vy=0
	Case 3
		b1:ball=ball.Create(400,300,50,50)
		b1.vx=-1
		b1.vy=0
		b2:ball=ball.Create(200,250,50,50)
		b2.vx=1
		b2.vy=0
		
	Case 4
		For c=1 To 5
			radius#=Rnd(10,50)
			mass=Rnd(.5,1)*radius
			ball.Create Rand(radius,gwidth-radius),Rand(radius,gheight-radius),radius,mass
		Next
	End Select
End Function

Function drawlines(txt$,x,y)
	Local lines$[]=txt.split("~n")
	For i=0 To Len(lines)-1
		DrawText lines[i],0,y
		y:+13
	Next
End Function

Graphics gwidth,gheight,0
SeedRnd MilliSecs()

Global state=-1
Global statetxt$[]=[	"Ball 1 stationary~nBall 2 heading directly at ball 1~nEqual masses",..
					"Ball 1 stationary~nBall 2 heading directly at ball 1~nDifferent masses",..
					"Both balls moving~nBall 2 heading directly at ball 1~nEqual masses~nBall 1's frame of reference shown below",..
					"Ball 1 stationary~nBall 2 hits ball 1 at an angle~nEqual masses",..
					"Lots of balls all over the place!"]

changeState

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())

	If KeyHit(KEY_SPACE)
		changeState
	EndIf

	ball.collideAll

	For b:ball=EachIn balls
		b.update
	Next
	
	For b:ball=EachIn balls
		b.draw
	Next
	
	SetColor 255,255,255
	Drawlines statetxt[state],0,0
	
	Flip
	Cls
Wend