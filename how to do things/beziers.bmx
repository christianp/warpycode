Graphics 800,800,0

Type point
	Field x#,y#
	Function create:point(x#,y#)
		p:point=New point
		p.x=x
		p.y=y
		Return p
	End Function
End Type

Type bezspline
	Field points:TList
	Field startpoint#
	
	Method New()
		points=New TList
	End Method
	
	Method addpoint(p:point)
		points.addlast p
	End Method
	
	Method bezier(ax#,ay#,bx#,by#,cx#,cy#,dx#,dy#,start#=0)
		'DrawLine ax,ay,bx,by
		'DrawLine cx,cy,dx,dy
		a#=start
		b#=1-a
		ox#=ax*b*b*b + 3*bx*b*b*a + 3*cx*b*a*a + dx*a*a*a
		oy#=ay*b*b*b + 3*by*b*b*a + 3*cy*b*a*a + dy*a*a*a
		For t#=start To 1 Step .05
			a#=t
			b#=1-t
			x#=ax*b*b*b + 3*bx*b*b*a + 3*cx*b*a*a + dx*a*a*a
			y#=ay*b*b*b + 3*by*b*b*a + 3*cy*b*a*a + dy*a*a*a
			DrawLine ox,oy,x,y
			ox=x
			oy=y
		Next
		a#=1
		b#=0
		x#=ax*b*b*b + 3*bx*b*b*a + 3*cx*b*a*a + dx*a*a*a
		y#=ay*b*b*b + 3*by*b*b*a + 3*cy*b*a*a + dy*a*a*a
		DrawLine ox,oy,x,y
		
	End Method
	
	Method traversebezier()
		'For p:point=EachIn points
		'	DrawRect p.x-2,p.y-2,5,5
		'Next
		
		If points.count()<4 Then Return
		al:TLink=points.firstlink()
		bl:TLink=al.nextlink()
		cl:TLink=bl.nextlink()
		dl:TLink=cl.nextlink()
		While dl<>Null
			a:point=point(al.value())
			b:point=point(bl.value())
			c:point=point(cl.value())
			d:point=point(dl.value())
			If al=points.firstlink() Then start#=startpoint Else start=0
			ax#=a.x
			ay#=a.y
			bx#=b.x
			by#=b.y
			dx#=c.x
			dy#=c.y
			cx#=2*dx-d.x
			cy#=2*dy-d.y
			bezier(ax,ay,bx,by,cx,cy,dx,dy,start)
			al=cl
			bl=dl
			cl=dl.nextlink()
			If cl<>Null Then dl=cl.nextlink() Else dl=Null
		Wend
	End Method
	
	Method tangentlength#(a:point,b:point,c:point,d:point,t#)
		ta#=t
		tb#=1-t

		ax#=a.x
		ay#=a.y
		bx#=b.x
		by#=b.y
		dx#=c.x
		dy#=c.y
		cx#=2*dx-d.x
		cy#=2*dy-d.y
		
		vx#=-3*ax*tb*tb + 3*bx*tb*(tb-2*ta) + 3*cx*ta*(2*tb-ta) + dx*3*ta*ta
		vy#=-3*ay*tb*tb + 3*by*tb*(tb-2*ta) + 3*cy*ta*(2*tb-ta) + dy*3*ta*ta
		td#=Sqr(vx*vx+vy*vy)
		Return td
	End Method
		
	Method bezierx#(a:point,b:point,c:point,d:point,t#)
		ax#=a.x
		bx#=b.x
		dx#=c.x
		cx#=2*dx-d.x
		
		ta#=t
		tb#=1-t
		
		x#=ax*tb*tb*tb + 3*bx*tb*tb*ta + 3*cx*tb*ta*ta + dx*ta*ta*ta
		Return x
	End Method

	Method beziery#(a:point,b:point,c:point,d:point,t#)
		ay#=a.y
		by#=b.y
		dy#=c.y
		cy#=2*dy-d.y
		
		ta#=t
		tb#=1-t
		
		y#=ay*tb*tb*tb + 3*by*tb*tb*ta + 3*cy*tb*ta*ta + dy*ta*ta*ta
		Return y
	End Method
End Type

Type ball
	Field myspline:bezspline
	Field segment:TLink
	Field v#
	Field t#
	Field ox#,oy#
	
	Function create:ball(s:bezspline,v#)
		b:ball=New ball
		b.myspline=s
		b.segment=s.points.firstlink()
		b.t=0
		b.v=v
		Return b
	End Function
	
	Method update()
		Local points:point[4]
		l:TLink=segment
		For c=0 To 3
			If l<>Null
				points[c]=point(l.value())
				l=l.nextlink()
			EndIf
		Next
		If points[3]<>Null
			speed#=v/myspline.tangentlength(points[0],points[1],points[2],points[3],t)
			If speed>1-t Then speed=1-t
			t:+speed
			myspline.startpoint=t
			DrawText 1/speed,0,15
			x#=myspline.bezierx(points[0],points[1],points[2],points[3],t)
			y#=myspline.beziery(points[0],points[1],points[2],points[3],t)
			DrawRect x-5,y-5,11,11

			'Checkwhat the actual speed is
			movedist1#=Sqr((ox-x)^2+(oy-y)^2)
			DrawText movedist1,0,0
			ox=x
			oy=y
			
			If t>=1
				segment=segment.nextlink()
				If segment<>Null Then segment=segment.nextlink()
				If segment=Null Then segment=myspline.points.firstlink()
				t:-1
			EndIf
		Else
			segment=myspline.points.firstlink()
		EndIf
	End Method
End Type

t1#=0
t2#=0
speed#=0 'Speed of clever speed changing version
x1#=ax
y1#=ay
x2#=ax
y2#=ay
ballspeed#=10

tick=0
mx=MouseX()
my=MouseY()

s:bezspline=New bezspline
b:ball=ball.create(s,5)
While Not KeyHit(KEY_ESCAPE)

	omx=mx
	omy=my
	mx=MouseX()
	my=MouseY()
	vx=mx-omx
	vy=my-omy
	
	tick:+1
	If MouseDown(1)
		If tick Mod 3 = 0
			s.addpoint(point.create(mx,my))
			s.addpoint(point.create(mx+vx,my+vy))
		EndIf
	EndIf
	If s.points.count()>2
		If s.points.first()<>b.segment.value()
			s.points.removefirst()
			s.points.removefirst()
		EndIf
	EndIf

	SetColor 100,100,100
	s.traversebezier()
	SetColor 0,0,255
	b.update()

	Flip
	Cls
Wend

'DON'T BE DECEIVED! This is just a plain old bezier-drawing function, to show the curve the balls are moving along
Function bezier(ax#,ay#,bx#,by#,cx#,cy#,dx#,dy#)
	DrawLine ax,ay,bx,by
	DrawLine cx,cy,dx,dy
	ox#=ax
	oy#=ay
	For t#=0 To 1 Step .01
		a#=t
		b#=1-t
		x#=ax*b*b*b + 3*bx*b*b*a + 3*cx*b*a*a + dx*a*a*a
		y#=ay*b*b*b + 3*by*b*b*a + 3*cy*b*a*a + dy*a*a*a
		DrawRect x-1,y-1,3,3
		DrawLine ox,oy,x,y
		ox=x
		oy=y
	Next
End Function

