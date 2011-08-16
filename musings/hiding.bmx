Type light
	Field x#,y#
	Field lit
	
	Method hidden(px#,py#)
		an#=ATan2(py-p.y,px-p.x)
		For w:wall=EachIn walls
			If Not sameside(p.x,p.y,px,py,w.x1,w.y1,w.x2,w.y2)
				an1#=ATan2(w.y1-p.y,w.x1-p.x)
				an2#=ATan2(w.y2-p.y,w.x2-p.x)
				If an2<an1
					b#=an2
					an2=an1
					an1=b
				EndIf
				d1#=andiff(an2,an1)
				If d1<0 Then d1:+360
				d2#=andiff(an,an1)
				If d2<0 Then d2:+360
				If d2<d1 Then Return 1
			EndIf			
		Next
		Return 0
	End Method
End Type


Type line
	Field	x#,y#,dx#,dy#,nx#,ny#,length#
	
	Function Create:line(x1#,y1#,x2#,y2#)
		l:line=New line
		dx#=x2-x1
		dy#=y2-y1
		an#=ATan2(dy,dx)
		l.dx=Cos(an)
		l.dy=Sin(an)
		l.nx=-Sin(an)
		l.ny=Cos(an)
		l.x=x1
		l.y=y1
		l.length=-1
		Return l
	End Function
	
	Method intersect#[](px#,py#)	'distances along tangent and normal, not x/y co-ords!
		If dx=0
			mu#=(py-y+(x-px)*ny/nx)/(dy-dx*ny/nx)
			lambda#=(py+mu*ny-y)/dy
		Else
			mu#=(y-py+(px-x)*dy/dx)/(ny-nx*dy/dx)
			lambda#=(px+mu*nx-x)/dx
		EndIf
		Return [lambda,mu]
	End Method
	
	Method draw()
		SetColor 255,255,0
		If length=-1
			DrawLine x,y,x+dx*400,y+dy*400
		Else
			DrawLine x,y,x+dx*length,y+dy*length
		EndIf
	End Method
End Type

Global walls:TList=New TList
Type wall Extends line
	Field x1#,y1#,x2#,y2#
	
	Method New()
		walls.addlast Self
	End Method
	
	Function Create:wall(x1#,y1#,x2#,y2#)
		'wall stuff
		w:wall=New wall
		w.x1=x1
		w.y1=y1
		w.x2=x2
		w.y2=y2
		
		'generic line stuff
		dx#=x2-x1
		dy#=y2-y1
		an#=ATan2(dy,dx)
		w.x=x1
		w.y=y1
		w.dx=Cos(an)
		w.dy=Sin(an)
		w.nx=-Sin(an)
		w.ny=Cos(an)
		w.length=Sqr(dx*dx+dy*dy)
		Return w
	End Function
	
	Method draw()
		SetColor 0,0,0
		SetLineWidth 3
		DrawLine x1,y1,x2,y2
		SetLineWidth 1
	End Method
	
	Function shadows:TList(x#,y#)
		l:TList=New TList
		For w:wall=EachIn walls
			l.addlast w
			l.addlast line.Create(w.x1,w.y1,2*w.x1-x,2*w.y1-y)
			l.addlast line.Create(w.x2,w.y2,2*w.x2-x,2*w.y2-y)
		Next
		Return l
	End Function
End Type



Global dudes:TList=New TList
Type dude
	Field x#,y#
	Field tx#,ty#
	
	Method New()
		dudes.addlast Self
	End Method
	
	Function Create:dude(x#,y#)
		d:dude=New dude
		d.x=x
		d.y=y
		d.tx=d.x
		d.ty=d.y
		Return d
	End Function
	
	Method update()
		If p.lit And Not hidden()
			findcover
			tx=x
			ty=y
		Else
			dx#=tx-x
			dy#=ty-y
			d#=Sqr(dx*dx+dy*dy)
			If d>5
				an#=ATan2(ty-y,tx-x)
				x:+Cos(an)
				y:+Sin(an)
				DrawLine x,y,tx,ty
			Else
				tx=x+Rnd(-20,20)
				ty=y+Rnd(-20,20)
				c=0
				While Not p.hidden(tx,ty)
					tx=x+Rnd(-20,20)
					ty=y+Rnd(-20,20)
					c:+1
					If c>10 Exit
				Wend
			EndIf
		EndIf
	End Method
	
	Method findcover()
		Local tx#,ty#
		Local d#,mindist#=-1
		Local an#,can#
		For w:wall=EachIn walls
			If sameside(x,y,p.x,p.y,w.x1,w.y1,w.x2,w.y2)
				d1#=(x-w.x1)*(x-w.x1)+(y-w.y1)*(y-w.y1)
				d2#=(x-w.x2)*(x-w.x2)+(y-w.y2)*(y-w.y2)
				If d1<d2
					tx=w.x1-w.dx*10
					ty=w.y1-w.dy*10
					d=d1
				Else
					tx=w.x2+w.dx*10
					ty=w.y2+w.dy*10
					d=d2
				EndIf
				an#=ATan2(ty-y,tx-x)
			Else
				an1#=ATan2(y-p.y,x-p.x)
				an2#=ATan2((w.y1+w.y2)/2-p.y,(w.x1+w.x2)/2-p.x)
				dan#=andiff(an1,an2)
				If dan<0
					san#=ATan2(w.y1-p.y,w.x1-p.x)
					an=san+80
				Else
					san#=ATan2(w.y2-p.y,w.x2-p.x)
					an=san-80
				EndIf
				s#=Sin(san)
				c#=Cos(san)
				d#=Abs((w.y1-y+(x-w.x1)*s/c)/(c+s*s/c))

			EndIf
			If d<mindist Or mindist=-1
				mindist=d
				can=an
			EndIf
			
		Next
		
		If mindist<>-1
			DrawLine x,y,x+Cos(can)*100,y+Sin(can)*100
			x:+Cos(can)*2
			y:+Sin(can)*2
		EndIf
	End Method
	
	Method hidden()
		Return p.hidden(x,y)
	End Method
	
	Method draw()
		If hidden()
			SetColor 50,50,150
		Else
			SetColor 0,0,200
		EndIf
		DrawOval x-5,y-5,10,10
	End Method
End Type

Function andiff#(an1#,an2#)
	'debugo String(an1)+" , "+String(an2)
	If an2=0 And an1=360 Then Return 360
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

'returns True if p1 and p2 are on the same side of the line a->b
Function sameside(p1x#,p1y#,p2x#,p2y#,ax#,ay#,bx#,by#)
	cp1# = (bx-ax)*(p1y-ay)-(p1x-ax)*(by-ay)
	cp2# = (bx-ax)*(p2y-ay)-(p2x-ax)*(by-ay)
	If cp1*cp2 >= 0 Then Return True
End Function


Graphics 600,600,0
SetBlend ALPHABLEND
SetClsColor 200,200,200


Local mx#,my#
Local wx#=-1,wy#

Global shadows:TList

Global p:light=New light

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	'input
	mx=MouseX()
	my=MouseY()
	
	p.x=mx
	p.y=my
	p.lit=KeyDown(KEY_SPACE)
	
	If MouseHit(1)
		If wx<>-1
			wall.Create wx,wy,mx,my
			wx=-1
		Else
			wx=mx
			wy=my
		EndIf
	EndIf
	
	If MouseHit(2)
		dude.Create mx,my
	EndIf
	
	'logic
	shadows=wall.shadows(p.x,p.y)
	For d:dude=EachIn dudes
		d.update
	Next
		
	'draw
	If wx<>-1
		SetColor 0,0,0
		SetAlpha .5
		SetLineWidth 3
		DrawLine wx,wy,mx,my
		SetAlpha 1
		SetLineWidth 1
	EndIf

	For w:wall=EachIn walls
		w.draw
	Next
	
	For l:line=EachIn shadows
		l.draw
	Next
	
	For d:dude=EachIn dudes
		d.draw
	Next

	Flip
	Cls
Wend
