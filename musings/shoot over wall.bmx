Graphics 600,600,0
Global bullets:TList=New TList
Type bullet
	Field x#,y#
	Field vx#,vy#
	Field path#[]

	Function Create:bullet(x#,y#,vx#,vy#)
		b:bullet=New bullet
		bullets.addlast b
		b.x=x
		b.y=y
		b.vx=vx
		b.vy=vy
		Return b
	End Function

	Method update()
		path:+[x,y]

		x:+vx
		y:+vy
		vy:+g
		DrawRect x-5,y-5,10,10

		For i=0 To Len(path)-1 Step 2
			DrawRect path[i],path[i+1],1,1
		Next

		If x>600 Or y>600
			bullets.remove Self
		EndIf
	End Method
End Type

Const g#=1,x1#=300

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	x2#=MouseX()
	y#=MouseY()-300
	
	DrawLine 300,300,300,300+y
	DrawLine 0,300,x2,300

	If MouseHit(1)
		If x2>x1 And y<0
			theta# = ATan(-y*x2/(x2*x1-x1*x1))
			v=Sqr(g*x2/(Sin(2*theta)))
			Print theta
			Print v
			bullet.Create(0,300,v*Cos(-theta),v*Sin(-theta))
		EndIf
	EndIf
	
	For bu:bullet=EachIn bullets
		bu.update
	Next

	Flip
	Cls
Wend
