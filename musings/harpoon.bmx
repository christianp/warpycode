Global ropes:TList=New TList
Type rope
	Field ox#,oy#
	Field x#,y#
	Field nx#,ny#
	Field weight#
	Field up:rope,down:rope
	Field length#
	
	Method New()
		length=10
	End Method
	
	Function Create:rope(x#,y#,vx#=0,vy#=0)
		r:rope=New rope
		r.x=x
		r.y=y
		r.ox=x-vx
		r.oy=y-vy
		Return r
	End Function
	
	Method update()
		vx#=x-ox
		vy#=y-oy+weight
		ox=x
		oy=y
		x:+vx
		y:+vy
	End Method
	
	Method solve()
		If Not weight Return
		If up fix(up)
		If down fix(down)
	End Method
	
	Method fix(r:rope)
		dx#=r.x-x
		dy#=r.y-y
		d#=dx*dx+dy*dy
		If d>length*length
			d=Sqr(d)
			mix#=.99*weight/(r.weight+weight)
			f#=mix*(d-length)/(d)
			nx:+f*dx
			ny:+f*dy
		EndIf
	End Method
	
	Method draw()
		SetAlpha 1
		DrawRect x-1,y-1,2,2
		If up
			DrawLine x,y,up.x,up.y
		EndIf
	End Method
End Type

Type ropeend Extends rope
	Field fixed:rock
	
	Function Create:rope(x#,y#,vx#=0,vy#=0)
		r:ropeend=New ropeend
		r.x=x
		r.y=y
		r.ox=x-vx
		r.oy=y-vy
		Return r
	End Function

	Method update()
		Super.update
		
		If fixed
			fixed.x=x
			fixed.y=y
			fixed.vx=0
			fixed.vy=0
		Else
			affix
		EndIf
	End Method

	Method affix()
		For s:rock=EachIn rocks
			dx#=s.x-x
			dy#=s.y-y
			d#=dx*dx+dy*dy
			If d<100
				fixed=s
				weight=1
				Return
			EndIf
		Next
	End Method	
End Type

Type base
	Field x#,y#
	Field state
	Field root:rope
	Field last:rope
	Field endr:ropeend
	Field target:rock
	
	Method update()
		Select state
		Case 0	'nowt
			aim
		Case 1	'shooting
			unravel
			If endr.fixed Or dist(x,y,endr.x,endr.y)>300
				endr.weight=1
				state=2
			EndIf
		Case 2
			reel
		End Select
	End Method
	
	Method aim()
		c:rock=Null
		mindist#=-1
		For s:rock=EachIn rocks
			dx#=s.x-x
			dy#=s.y-y
			d#=dx*dx+dy*dy
			d#=dx
			If (d<mindist Or mindist=-1) And d<90000
				mindist=d
				c=s
			EndIf
		Next
		If Not c Return
		
		shoot c
		
	End Method
	
	Method shoot(c:rock)
		dx#=c.x-x
		dy#=c.y-y
		v#=9
		a#=dx*dx+dy*dy
		b#=2*(c.vx*dx+c.vy*dy)
		d#=c.vx*c.vx+c.vy*c.vy-v*v
		If b*b<4*a*d
			Print "no soln"
			Return
		EndIf
		tInv#=(-b+Sqr(b*b-4*a*d))/(2*a)
		vx#=dx*tInv+c.vx
		vy#=dy*tInv+c.vy
		Print a
		Print Sqr(vx*vx+vy*vy)
		state=1
		last=ropeend.Create(x,y,vx,vy)
		endr=ropeend(last)
		root=rope.Create(x,y)
		ropes.addlast last
		last.up=root
	End Method
	
	Method unravel()
		dx#=last.x-x
		dy#=last.y-y
		d#=Sqr(dx*dx+dy*dy)
		If d>10
			r:rope=last
			last=rope.Create(x,y)
			last.weight=.5
			last.up=root
			last.down=r
			r.up=last
			ropes.addlast last
		EndIf
	End Method
	
	Method reel()
		If dist(endr.x-endr.ox,endr.y-endr.oy)>5 Return
		dx#=last.x-x
		dy#=last.y-y
		d#=Sqr(dx*dx+dy*dy)
		If d<2
			ropes.remove last
			If last=endr
				rocks.remove ropeend(last).fixed
				state=0
			Else
				last=last.down
				last.up=root
			EndIf
		Else
			If last.down
				last.weight=last.down.weight*.1
			EndIf
			last.length:*.8
			DrawText last.length,0,15
		EndIf
	End Method
				
	
	Method draw()
		SetAlpha .5
		DrawOval x-10,y-10,20,20
	End Method
End Type

Global rocks:TList=New TList
Type rock
	Field x#,y#
	Field vx#,vy#
	
	Method New()
		rocks.addlast Self
		Rem
		an#=Rnd(360)
		x=300+500*Cos(an)
		y=300+500*Sin(an)
		an:+Rnd(-10,10)
		vx=-1*Cos(an)
		vy=-1*Sin(an)
		EndRem
		x=-100
		y=Rnd(50,200)
		vx=Rnd(5,8)
		vy=Rnd(-1,1)
	End Method
	
	Method update()
		x:+vx
		y:+vy
		If (x<0 And vx<0) x:+600
		If (x>600 And vx>0) x:-600
		If (y<0 And vy<0) y:+600
		If (y>600 And vy>0) y:-600	
	End Method
	
	Method draw()
		SetAlpha 1
		SetColor 150,150,150
		DrawRect x-5,y-5,10,10
	End Method
End Type


Function dist#(x1#,y1#,x2#=0,y2#=0)
	x2:-x1
	y2:-y1
	Return Sqr(x2*x2+y2*y2)
End Function



Graphics 600,600,0
SetBlend ALPHABLEND


b:base=New base
b.x=300
b.y=300

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())

	b.update
	For r:rope=EachIn ropes
		r.update
	Next
	For c=1 To 30
		For r:rope=EachIn ropes
			r.solve
		Next
		For r:rope=EachIn ropes
			r.x:+r.nx
			r.y:+r.ny
			r.nx=0
			r.ny=0
		Next
	Next
	
	If Rand(rocks.count()*10+1)=1 Or KeyHit(KEY_SPACE)
		New rock
	EndIf
	For s:rock=EachIn rocks
		s.update
	Next
	
	
	b.draw
	
	For s:rock=EachIn rocks
		s.draw
	Next
	
	For r:rope=EachIn ropes
		r.draw
	Next
	
	DrawText rocks.count(),0,0
	
	Flip
	Cls
Wend