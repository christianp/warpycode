
Global thrusts:TList=New TList
Type thrust
	Field x1#,y1#,x2,y2#,x3#,y3#,x4#,y4#
	Field x#,y#,vx#,vy#
	Field maxthrust
	Field red,green,blue
	Field life#
	
	Method New()
		thrusts.addlast Self
		life=1
	End Method
	
	Function create:thrust(x#,y#,vx#,vy#,x1#,y1#,x2#,y2#,maxthrust#)
		t:thrust=New thrust
		t.x=x
		t.y=y

		t.vx=vx
		t.vy=vy
		
		t.x1=x1
		t.y1=y1
		t.x2=x2
		t.y2=y2
		
		t.x3=t.x+t.x1*Rnd(1,1.5)
		t.y3=t.y+t.y1
		t.x4=t.x+t.x2*Rnd(1,1.5)
		t.y4=t.y+t.y2
		
		n=Rand(1,7)
		If n & 1 t.red=255
		If n & 2 t.green=255
		If n & 4 t.blue=255
		
		t.maxthrust=maxthrust
		
		Return t
	End Function
	
	Method update()
		If y>maxthrust
			y:+vy
			x:+vx
		Else
			life:-(1-life)*.01+.000001
			If life<=0
				thrusts.remove Self
			EndIf
		EndIf
	End Method
	
	Method draw()
		Local poly#[]
		'poly=[sx+x1-3,sy+y1+3,x+x1-3,y+y1-3,x+x2+3,y+y2-3,sx+x2+3,sy+y2+3]
		'SetColor 150*life,150*life,150*life
		'SetBlend SHADEBLEND
		'DrawPoly poly
		poly=[x3,y3,x+x1,y+y1,x+x2,y+y2,x4,y4]
		SetColor red,green,blue
		SetBlend ALPHABLEND
		SetAlpha life
		DrawPoly poly

		'DrawLine x+x1,y+y1,x,y
		'DrawLine x+x2,y+y2,x,y

		SetAlpha 1
		SetColor 255,255,255
		DrawText life,x,y
	End Method
End Type

Global balls:TList
Type ball

	Field x#,y#,vx#,vy#,an#,van#
	Field radius#,circ#
	Field state
	
	Field front:TList
	Field back:TList

	Method New()
		balls.addlast Self
		front=New TList
		back=New TList
	End Method
	
	Function create:ball(x#,y#,radius#)
		b:ball=New ball
		b.x=x
		b.y=y
		b.radius=radius
		b.circ=2*Pi*b.radius
		
		Return b
	End Function
	
	Method update()
	
	End Method
End Type

AppTitle="Crazy shape escape"
Const gwidth=800,gheight=600
Graphics 800,600,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

While Not KeyHit(KEY_ESCAPE)

	If Rand(thrusts.count()*50)=1
		x#=Rand(-100,gwidth+100)
		vx#=-(x-gwidth/2)*.003
		y#=gheight+100
		vy#=-Rnd(1,5)
		x1#=Rnd(-200,-100)
		y1#=Rnd(-1,1)*5000/x1
		thrust.create(x,y,vx,vy,x1,y1,-x1,-y1,Rnd(50,gheight*.7))
	EndIf
	
	For t:thrust=EachIn thrusts
		t.update()
		t.draw()
	Next

	Flip
	Cls
	
Wend