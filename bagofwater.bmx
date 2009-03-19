Function quickhull:tlist(s:TList)
	If s.count()<=3 Return s
	out:tlist=New tlist
	l:point=Null
	r:point=Null
	For p:point=EachIn s
		If l=Null
			l=p
		ElseIf p.x<l.x
			l=p
		EndIf
		If r=Null
			r=p
		ElseIf p.x>r.x
			r=p
		EndIf
	Next
	l.n=0
	out.addlast l
	
	an#=ATan2(r.y-l.y,r.x-l.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=Cos(an+90)
	sy#=Sin(an+90)
	
	s1:tlist=New tlist
	s2:tlist=New tlist
	For p:point=EachIn s
		If p<>l And p<>r
			mu#=(l.y-p.y+(ry/rx)*(p.x-l.x))/(sy-sx*ry/rx)
			If mu<0 
				s1.addlast p 
			ElseIf mu>0
				s2.addlast p
			EndIf
		EndIf
	Next
	
	out1:tlist = findhull(s1 , l , r , 1)
	If out1.count()
		r.n = point(out1.last() ).n+1
	Else
		r.n=1
	EndIf
	out2:tlist=findhull(s2,r,l,r.n+1)
	If out1
		For o:Object=EachIn out1
			out.addlast o
		Next
	EndIf
	out.addlast r
	DrawRect r.x-3,r.y-3,6,6
	If out2
		For o:Object=EachIn out2
			out.addlast o
		Next
	EndIf
	
	Return out	
End Function

Function findhull:tlist(sk:tlist , p:point , q:point , n)
	'Print n
	If Not sk.count() Return Null
	c:point=Null
	out:tlist=New tlist
	maxdist#=-1
	an#=ATan2(q.y-p.y,q.x-p.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=-ry
	sy#=rx
	For tp:point=EachIn sk
		If tp<>p And tp<>q
			mu#=(p.y-tp.y+(ry/rx)*(tp.x-p.x))/(sy-sx*ry/rx)
			If maxdist=-1 Or Abs(mu)>maxdist
				c=tp
				maxdist=Abs(mu)
			EndIf
		EndIf
	Next
	'DrawLine p.x , p.y , c.x , c.y
	'DrawLine q.x , q.y , c.x , c.y
	
	an#=ATan2(c.y-p.y,c.x-p.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=Cos(an+90)
	sy#=Sin(an+90)
	s1:tlist=New tlist
	s2:tlist=New tlist
	For tp:point=EachIn sk
		If tp<>c
			If Not pointintriangle(tp.x,tp.y,p.x,p.y,q.x,q.y,c.x,c.y)
				mu#=(p.y-tp.y+(ry/rx)*(tp.x-p.x))/(sy-sx*ry/rx)
				If mu<0 s1.addlast tp ElseIf mu>0 s2.addlast tp
			EndIf
		EndIf
	Next
	If s1.count()
		out1:tlist = findhull(s1 , p , c , n)
		If out1.count()
			c.n = point(out1.last() ).n + 1
		Else
			c.n=n+1
		EndIf
	Else
		c.n=n
			'Print "?"+String(c.n)
	EndIf
	out2:tlist = findhull(s2 , c , q , c.n + 1)
	If out1
		For o:Object=EachIn out1
			out.addlast o
		Next
	EndIf
	out.addlast c
	If out2
		For o:Object=EachIn out2
			out.addlast o
		Next
	EndIf
	Return out
End Function

Function pointintriangle(px#,py#,ax#,ay#,bx#,by#,cx#,cy#)
	If sameside(px,py,ax,ay,bx,by,cx,cy) And sameside(px,py,bx,by,ax,ay,cx,cy) And sameside(px,py,cx,cy,ax,ay,bx,by)
		Return True
	Else
		Return False
	EndIf
End Function
Function sameside(p1x#,p1y#,p2x#,p2y#,ax#,ay#,bx#,by#)
	cp1# = (bx-ax)*(p1y-ay)-(p1x-ax)*(by-ay)
	cp2# = (bx-ax)*(p2y-ay)-(p2x-ax)*(by-ay)
	If cp1*cp2 >= 0 Then Return True
End Function	
	


Type point
	Field x# , y#
	Field vx#,vy#
	Field n
End Type

Type stick
	Field x# , length#
End Type

Global points:tlist = New tlist
Global sticks:tlist = New tlist

Graphics 800 , 800 , 0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

For c = 1 To 200
	p:point = New point
	an = Rand(360)
	length#=Rand(200)
	p.x = 400 + length * Cos(an)
	p.y = 400 + length * Sin(an)
	p.vx = Rnd( - 3 , 3)
	p.vy=Rnd(-3,3)
	points.addlast p
Next

radius# = 100
bradius#=150

For c = 1 To 3
	s:stick = New stick
	s.x = Rand(300 , 500)
	s.length#=Rand(200,300)
Next

cx# = 400
cy#=400
While Not KeyHit(KEY_ESCAPE)
	hull:tlist = quickhull(points)
	maxn=hull.count()
	For p:point = EachIn hull
		'DrawRect p.x - 3 , p.y - 3 , 6 , 6
		'DrawText p.n, p.x,p.y
		n:+ 1
		'Print p.n
		ln = p.n + 1
		If ln=maxn ln=0
		rn = p.n - 1
		If rn < 0 rn = maxn-1
		l:point = point(hull.valueatindex(ln))
		r:point = point(hull.valueatindex(rn))
		dx3# = cx - p.x
		dy3# = cy - p.y
		d3# = Sqr(dx3 * dx3 + dy3 * dy3) 
		
		dx1# = l.x - p.x
		dy1# = l.y - p.y
		'dx1:/ 50
		'dy1:/ 50
		dx2# = r.x - p.x
		dy2# = r.y - p.y
		'dx2:/ 50
		'dy2:/ 50
		inward#=.01*(d3-radius)/radius
		p.vx:+ (dx1 + dx2) * .00001*d3+inward*dx3
		p.vy:+ (dy1 + dy2) * .00001*d3+inward*dy3
		
		'p.vx:- .1
		DrawLine l.x , l.y , p.x , p.y
		DrawLine r.x , r.y , p.x , p.y

	Next
	
	cx# = 0
	cy# = 0
	
	For p:point = EachIn points
		p.vx:* .99
		p.vy:* .99
		p.vy:+.1
		p.x:+ p.vx
		p.y:+ p.vy
		
		dx# = p.x - 400
		If Abs(dx)<bradius
			y = Sqr(bradius * bradius - dx * dx) + 600
		Else
			y = 600
		EndIf
		If p.y > y
			p.y = y
			If Abs(dx) < bradius
				v#=Sqr(p.vx*p.vx+p.vy*p.vy)
				p.vx:-v*(dx/bradius)
				p.vy=- v * ( (y - 600) / bradius)
			Else
				p.vy = - p.vy * .99
			EndIf
				
		EndIf
		DrawRect p.x , p.y , 1 , 1
		cx:+ p.x
		cy:+ p.y
	Next
	numpoints=points.count()
	cx:/ numpoints
	cy:/ numpoints
	Flip
	Cls
Wend