Type element
	Field x#,y#


	Method distance#(ox#,oy#)
	End Method

	Method intersections:TList(o:Object)
	End Method
	
	Method closestx#(nx#,ny#)
		Return nx
	End Method
	
	Method closesty#(nx#,ny#)
		Return ny
	End Method
	
	Method move(newx#,newy#)
		x=newx
		y=newy
	End Method
	
	Method addextrapoints()
	End Method
	
	Method sameside(x1#,y1#,x2#,y2#)
		Return 1
	End Method
	
	
	Method draw()
	End Method
End Type

Type point Extends element
	Field drawing

	Function Create:point(x#,y#,drawing=0)
		p:point=New point
		p.x=x
		p.y=y
		p.drawing=drawing
		Return p
	End Function

	Method closestx#(nx#,ny#)
		Return x
	End Method
	
	Method closesty#(nx#,ny#)
		Return y
	End Method

	Method distance#(ox#,oy#)
		dx#=x-ox
		dy#=y-oy
		d#=Sqr(dx*dx+dy*dy)
		Return d
	End Method
	
	Method intersections:TList(o:Object)
		Return New TList
	End Method
	
	Method draw()
		If Not drawing Return
		DrawOval x-5,y-5,10,10
	End Method
End Type

Type line Extends element
	Field x1#,y1#,x2#,y2#
	Field nx#,ny#,dx#,dy#
	Field angle#,length#
	Function Create:line(x1#,y1#,x2#,y2#)
		l:line=New line
		l.x1=x1
		l.y1=y1
		l.x2=x2
		l.y2=y2
		l.calcthings
		Return l
	End Function

	Method calcthings()
		dx#=x2-x1
		dy#=y2-y1
		length#=Sqr(dx*dx+dy*dy)
		dx:/length
		dy:/length
		angle#=ATan2(dy,dx)
		nx#=Cos(angle+90)
		ny#=Sin(angle+90)
		x=x1+dx*length/2
		y=y1+dy*length/2
	End Method
	
	Method closestx#(ox#,oy#)
		lambda#=ldistance(ox,oy)
		If dx<>0
			ix#=ox+lambda*nx
			mu#=(ix-x1)/dx
		Else
			iy#=oy+lambda*ny
			mu#=(iy-y1)/dy
		EndIf
		If mu<0
			Return x1
		ElseIf mu>length
			Return x2
		Else
			Return x1+mu*dx
		EndIf
	End Method
	
	Method closesty#(ox#,oy#)
		lambda#=ldistance(ox,oy)
		If dx<>0
			ix#=ox+lambda*nx
			mu#=(ix-x1)/dx
		Else
			iy#=oy+lambda*ny
			mu#=(iy-y1)/dy
		EndIf
		If mu<0
			Return y1
		ElseIf mu>length
			Return y2
		Else
			Return y1+mu*dy
		EndIf
	End Method

	Method distance#(ox#,oy#)
		lambda#=ldistance(ox,oy)
		If dx<>0
			ix#=ox+lambda*nx
			mu#=(ix-x1)/dx
		Else
			iy#=oy+lambda*ny
			mu#=(iy-y1)/dy
		EndIf
		If mu<0
			ddx#=ox-x1
			ddy#=oy-y1
			d#=Sqr(ddx*ddx+ddy*ddy)
			Return d
		ElseIf mu>length
			ddx#=ox-x2
			ddy#=oy-y2
			d#=Sqr(ddx*ddx+ddy*ddy)
			Return d
		Else
			Return Abs(lambda)
		EndIf
	End Method
	
	Method ldistance#(ox#,oy#)
		lambda#=(oy-y1+(x1-ox)*dy/dx)/(nx*dy/dx-ny)
		Return lambda#
	End Method
	
	Method intersections:TList(o:Object)
		If line(o)
			ol:line=line(o)
			Return linelineintersect(Self,ol)
		ElseIf arc(o)
			c:arc=arc(o)
			Return linearcintersect(c,Self)
		Else
			Return New TList
		EndIf
	End Method
	
	Method sameside(p1x#,p1y#,p2x#,p2y#)
		cp1# = (x2-x1)*(p1y-y1)-(p1x-x1)*(y2-y1)
		cp2# = (x2-x1)*(p2y-y1)-(p2x-x1)*(y2-y1)
		If cp1*cp2 >= 0 Then Return True
	End Method

	Method move(newx#,newy#)
		x=newx
		y=newy
		x1=newx-dx*length/2
		y1=newy-dy*length/2
		x2=newx+dx*length/2
		y2=newy+dy*length/2
	End Method
	
	Method contains(p:point)
		pdx#=p.x-x1
		pdy#=p.y-y1
		d#=pdx*pdx+pdy*pdy
		If d=0 Return 1
		an#=ATan2(pdy,pdx)
		If Abs(andiff(an,angle))<3 And d<=length*length Return 1
	End Method
	
	Method draw()
		DrawLine x1,y1,x2,y2
	End Method
End Type

Type arc Extends element
	Field r#
	Field startan#,endan#

	Function Create:arc(x#,y#,r#,startan#=0,endan#=360)
		c:arc=New arc
		c.x=x
		c.y=y
		c.r=r
		c.startan=startan
		c.endan=endan
		Return c
	End Function
	
	Method distance#(ox#,oy#)
		dx#=ox-x
		dy#=oy-y
		an#=ATan2(dy,dx)
		If inarc(an,startan,endan)
			d#=Sqr(dx*dx+dy*dy)
			Return Abs(d-r)
		Else
			If Abs(andiff(an,startan))<Abs(andiff(an,endan))
				x1#=x+Cos(startan)*r
				y1#=y+Sin(startan)*r
				dx=ox-x1
				dy=oy-y1
				d=Sqr(dx*dx+dy*dy)
				Return d
			Else
				x2#=x+Cos(endan)*r
				y2#=y+Sin(endan)*r
				dx=ox-x2
				dy=oy-y2
				d=Sqr(dx*dx+dy*dy)
				Return d
			EndIf
		EndIf
	End Method
	
	Method intersections:TList(o:Object)
		If line(o)
			ol:line=line(o)
			Return linearcintersect(Self,ol)
		ElseIf arc(o)
			c:arc=arc(o)
			Return arcarcintersect(Self,c)
		Else
			Return New TList
		EndIf
	End Method
	
	Method closestx#(ox#,oy#)
		dx#=ox-x
		dy#=oy-y
		an#=ATan2(dy,dx)
		If inarc(an,startan,endan)
			Return x+Cos(an)*r
		Else
			If Abs(andiff(an,startan))<Abs(andiff(an,endan))
				Return x+Cos(startan)*r
			Else
				Return x+Cos(endan)*r
			EndIf
		EndIf
	End Method
	
	Method closesty#(ox#,oy#)
		dx#=ox-x
		dy#=oy-y
		an#=ATan2(dy,dx)
		If inarc(an,startan,endan)
			Return y+Sin(an)*r
		Else
			If Abs(andiff(an,startan))<Abs(andiff(an,endan))
				Return y+Sin(startan)*r
			Else
				Return y+Sin(endan)*r
			EndIf
		EndIf
	End Method
	
	Method addextrapoints()
		addpoint point.Create(x,y,1)
	End Method
	
	Method draw()
		drawshell x,y,r,startan,endan
	End Method
End Type


Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

Function inarc(an,an1#,an2#)
	'If an1>180 Then an1:-360
	'If an2<180 Then an2:+360
	d1#=andiff(an2,an1)
	If d1=0 Then d1=360
	If d1>0
		d2#=andiff(an,an1)
		d3#=-andiff(an,an2)
		If (d2>=0 And d2<=d1) Or (d3>=0 And d3<=d1)
			Return 1
		Else
		EndIf
	Else
		Return 1-inarc(an,an2,an1)
	EndIf
End Function


Function sameside(p1x#,p1y#,p2x#,p2y#,ax#,ay#,bx#,by#)
	cp1# = (bx-ax)*(p1y-ay)-(p1x-ax)*(by-ay)
	cp2# = (bx-ax)*(p2y-ay)-(p2x-ax)*(by-ay)
	If cp1*cp2 >= 0 Then Return True
End Function	
Function pointintriangle(px#,py#,ax#,ay#,bx#,by#,cx#,cy#)
	If sameside(px,py,ax,ay,bx,by,cx,cy) And sameside(px,py,bx,by,ax,ay,cx,cy) And sameside(px,py,cx,cy,ax,ay,bx,by)
		Return True
	Else
		Return False
	EndIf
End Function


Function drawshell(x#,y#,r#,startan#=0,endan#=360)
	If endan<startan Then endan:+360
	endan:-startan
	numsegs=2*Pi*r/10
	If numsegs<12 Then numsegs=12
	anstep#=360.0/numsegs
	an#=0
	ox#=x+Cos(startan)*r
	oy#=y+Sin(startan)*r
	While an<endan
		an:+anstep
		If an>endan Then an=endan
		px#=x+Cos(an+startan)*r
		py#=y+Sin(an+startan)*r
		DrawLine ox,oy,px,py
		ox=px
		oy=py
	Wend
End Function

Function linelineintersect:TList(l1:line,l2:line)
	l:TList=New TList
	If l2.dx<>0
		lambda#=(l2.y1-l1.y1+(l1.x1-l2.x1)*l2.dy/l2.dx)/(l1.dy-l1.dx*l2.dy/l2.dx)
	Else
		lambda#=(l2.x1-l1.x1+(l1.y1-l2.y1)*l2.dx/l2.dy)/(l1.dx-l1.dy*l2.dx/l2.dy)
	EndIf
	If l1.dx<>0
		mu#=(l1.y1-l2.y1+(l2.x1-l1.x1)*l1.dy/l1.dx)/(l2.dy-l2.dx*l1.dy/l1.dx)
	Else
		mu#=(l1.x1-l2.x1+(l2.y1-l1.y1)*l1.dx/l1.dy)/(l2.x1-l2.y1*l1.dx/l1.dy)
	EndIf
	
	If (lambda#>=0 And lambda<=l1.length)
		If (mu#>=0 And mu<=l2.length)
			l.addlast point.Create(l1.x1+lambda*l1.dx,l1.y1+lambda*l1.dy)
		EndIf
	EndIf
	Return l
End Function

Function linearcintersect:TList(c:arc,ol:line)
	l:TList=New TList
	d#=ol.ldistance(c.x,c.y)
	If d<=c.r
		lx#=c.x+d*ol.nx
		ly#=c.y+d*ol.ny
		If d<c.r
			If ol.dx<>0
				mlambda#=(lx-ol.x1)/ol.dx
			Else
				mlambda#=(ly-ol.y1)/ol.dy
			EndIf
			lambda#=Sqr(c.r*c.r-d*d)
			If mlambda+lambda>=0 And mlambda+lambda<=ol.length
				x1#=lx+lambda*ol.dx
				y1#=ly+lambda*ol.dy
				an1#=ATan2(y1-c.y,x1-c.x)
				If inarc(an1,c.startan,c.endan)
					l.addlast point.Create(x1,y1)
				EndIf
			EndIf
			If mlambda-lambda>=0 And mlambda-lambda<=ol.length
				x2#=lx-lambda*ol.dx
				y2#=ly-lambda*ol.dy
				an2#=ATan2(y2-c.y,x2-c.x)
				If inarc(an2,c.startan,c.endan)
					l.addlast point.Create(x2,y2)
				EndIf
			EndIf
		Else
			l.addlast point.Create(lx,ly)
		EndIf
	EndIf
	Return l
End Function

Function arcarcintersect:TList(c1:arc,c2:arc)
	l:TList=New TList
	dx#=c2.x-c1.x
	dy#=c2.y-c1.y
	d#=Sqr(dx*dx+dy*dy)
	fac#=(d*d+c1.r*c1.r-c2.r*c2.r)/(2*d*d)
	mx#=c1.x+dx*fac
	my#=c1.y+dy*fac
	If d=c1.r+c2.r
		l.addlast point.Create(mx,my)
	ElseIf d<c1.r+c2.r
		angle#=ATan2(dy,dx)
		nx#=Cos(angle+90)
		ny#=Sin(angle+90)
		d:*fac
		lambda#=Sqr(c1.r*c1.r-d*d)
		x1#=mx+lambda*nx
		y1#=my+lambda*ny
		x2#=mx-lambda*nx
		y2#=my-lambda*ny
		l.addlast point.Create(x1,y1)
		l.addlast point.Create(x2,y2)
	EndIf
	For p:point=EachIn l
		an1#=ATan2(p.y-c1.y,p.x-c1.x)
		an2#=ATan2(p.y-c2.y,p.x-c2.x)
		If Not (inarc(an1,c1.startan,c1.endan) And inarc(an2,c2.startan,c2.endan))
			l.remove p
		EndIf
	Next
	Return l
End Function

Type tool
	Field state
	Field name$
	
	Method New()
		FlushMouse
		FlushKeys
	End Method

	Method update(x#,y#)
	End Method

	Method draw()
	End Method
End Type

Type drag Extends tool
	Field offx#,offy#
	Field ce:element
	
	Method New()
		name="Drag objects"
	End Method
	
	Method update(x#,y#)
		Select state
		Case 0
			If MouseDown(1)
				mindist#=-1
				ce:element=Null
				For e:element=EachIn elements
					d#=e.distance(x,y)
					If d<10 And (d<mindist Or mindist=-1)
						mindist=d
						ce=e
					EndIf
				Next
				If ce
					state=1
					offx#=x-ce.x
					offy#=y-ce.y
				EndIf
			EndIf
		Case 1
			If MouseDown(1)
				ce.move(x-offx,y-offy)
				redopoints
			Else
				state=0
			EndIf
		End Select
	End Method
	
End Type

Type ruler Extends tool
	Field x1#,y1#,x2#,y2#
	
	Method New()
		name="Ruler"
	End Method
	
	Method update(x#,y#)
		Select state
		Case 0 'nothing doing
			If MouseDown(1)
				x1=x
				y1=y
				x2=x1
				y2=y1
				state=1
			EndIf
		Case 1 'got one point
			x2=x
			y2=y
			If Not MouseDown(1)
				finish
			EndIf
		End Select
	End Method
	
	Method draw()
		Select state
		Case 1
			DrawLine x1,y1,x2,y2
		End Select
	End Method
	
	Method finish()
		addelement line.Create(x1,y1,x2,y2)
		curtool=New ruler
	End Method
End Type

Type compass Extends tool
	Field cx#,cy#,r#
	Field startan#,endan#
	Field an#
	Field dir
	
	Method New()
		name$="Compass"
	End Method
	
	Method update(x#,y#)
		Select state
		Case 0
			cx=x
			cy=y
			If MouseHit(1)
				state=1
			EndIf
		Case 1
			dx#=x-cx
			dy#=y-cy
			r=Sqr(dx*dx+dy*dy)
			If MouseHit(1)
				state=7
			EndIf
		Case 2
			cx=x
			cy=y
			If MouseHit(1)
				state=7
			EndIf
		Case 3,6
			state=2
		Case 7
			If Not MouseDown(1)
				state=4
			EndIf
		Case 4
			dx#=x-cx
			dy#=y-cy
			startan#=ATan2(dy,dx)
			'If startan<0 Then startan:+360
			endan=startan
			If MouseDown(1)
				state=5
				dir=0
			EndIf
		Case 5
			dx#=x-cx
			dy#=y-cy
			an#=ATan2(dy,dx)
			d1#=andiff(an,startan)
			d2#=andiff(an,endan)
			odiff#=andiff(endan,startan)
			If Abs(d1)<Abs(d2) Or (d1=d2 And d1<0)
				If d1<0
					startan=an
				EndIf
			Else
				If d2>0
					endan=an
				EndIf
			EndIf
			If Abs(andiff(endan,startan))<3 And Abs(odiff)>3
				startan=0
				endan=360
				state=50
			EndIf
			If Not MouseDown(1)
				finish
			EndIf
		Case 50
			If Not MouseDown(1)
				finish
			EndIf
		Case 9
			If Not MouseDown(1)
				state=2
				FlushMouse
			EndIf
		End Select
	End Method
	
	Method finish()
		addelement arc.Create(cx,cy,r,startan,endan)
		state=7
		
	End Method
	
	Method draw()
		Select state
		Case 0
			point.Create(cx,cy).draw
		Case 1,2
			drawshell cx,cy,r
			SetAlpha .3
			DrawOval cx-r,cy-r,r*2,r*2
			SetAlpha 1
			point.Create(cx,cy).draw
		Case 4
			SetAlpha .3
			DrawOval cx-r,cy-r,r*2,r*2
			SetAlpha 1
			DrawOval cx-3,cy-3,6,6
			px#=cx+Cos(startan)*r
			py#=cy+Sin(startan)*r
			point.Create(cx,cy).draw
			DrawLine cx,cy,px,py
		Case 5,50
			DrawLine cx,cy,cx+Cos(an)*r,cy+Sin(an)*r
			drawshell cx,cy,r,startan,endan
		End Select
	End Method
End Type

Type eraser Extends tool

	Method New()
		name="Eraser"
	End Method

	Method update(x#,y#)
		If MouseDown(1)
			For e:element=EachIn elements
				If e.distance(x,y)<5
					elements.remove e
				EndIf
			Next
			redopoints
		EndIf
	End Method
End Type

Type labeller Extends tool
	Field lx#,ly#
	Field txt$
	Field nextdelete
	Method New()
		name="Labeller"
	End Method
	
	Method update(x#,y#)
		ms=MilliSecs()
		Select state
		Case 0
			lx=x
			ly=y
			If MouseHit(1)
				state=1
				txt=""
				FlushKeys
			EndIf
		Case 1
			If KeyDown(KEY_BACKSPACE)
				If ms>nextdelete
					txt=txt[..Len(txt)-1]
					nextdelete=ms+100
				EndIf
			EndIf
			cr=GetChar()
			If cr>31
				txt:+Chr(cr)
			EndIf
			If KeyHit(KEY_ENTER)
				finish
			EndIf
		End Select
	End Method
	
	Method finish()
		labels.addlast label.Create(txt,lx,ly)
		state=0
	End Method
	
	Method draw()
		Select state
		Case 1
			DrawText txt,lx,ly
			w=TextWidth(txt)
			DrawLine lx+w,ly,lx+w,ly+12
		End Select
	End Method
End Type

Type areameasurer Extends tool
	Method update(x#,y#)
		findarea x,y
	End Method
End Type

Function findarea(x#,y#)
	belements:TList=New TList
	bpoints:TList=New TList
	inlist:TList=New TList
	
	For l:line=EachIn elements
		l.x1:-5*l.dx
		l.y1:-5*l.dy
		l.x2:+5*l.dx
		l.y2:+5*l.dy
		l.calcthings
		inlist.addlast l
	Next
	
	While inlist.count()
		e:element=element(inlist.removefirst())
		For e2:element=EachIn inlist
			For p:point=EachIn e.intersections(e2)
				bpoints.addlast p
			Next
		Next
	Wend
	
	For l:line=EachIn elements
		l.x1:+5*l.dx
		l.y1:+5*l.dy
		l.x2:-5*l.dx
		l.y2:-5*l.dy
		l.calcthings
	Next
	
	tris:TList=New TList
	intris:TList=New TList
	Local tri:point[3]
	Local poly#[]
	While bpoints.count()
		p1:point=point(bpoints.removefirst())
		pl:TList=bpoints.copy()
		While pl.count()
			p2:point=point(pl.removefirst())
			For p3:point=EachIn pl
				go=1
				poly=[p1.x,p1.y,p2.x,p2.y,p3.x,p3.y]
				For p4:point=EachIn bpoints
					If p4<>p3 And p4<>p1 And p4<>p2
						If pointintriangle(p4.x,p4.y,p1.x,p1.y,p2.x,p2.y,p3.x,p3.y)
							go=0
						EndIf
					EndIf
				Next



				l1:line=line.Create(p1.x,p1.y,p2.x,p2.y)
				l2:line=line.Create(p2.x,p2.y,p3.x,p3.y)
				l3:line=line.Create(p3.x,p3.y,p1.x,p1.y)
				For l:line=EachIn elements
					If Not (l.contains(p1) Or l.contains(p2))
					For p:point=EachIn l1.intersections(l)
						go=0
					Next
					EndIf
					If Not (l.contains(p3) Or l.contains(p2))
					For p:point=EachIn l2.intersections(l)
						go=0
					Next
					EndIf
					If Not (l.contains(p1) Or l.contains(p3))
					For p:point=EachIn l3.intersections(l)
						go=0
					Next
					EndIf
				Next
				If go
					tri=[p1,p2,p3]
					If pointintriangle(x,y,p1.x,p1.y,p2.x,p2.y,p3.x,p3.y)
						intris.addlast tri
					Else
						tris.addlast tri
					EndIf
				EndIf
			Next
		Wend
	Wend
	
	Local itri:point[3]
	caught=1
	While caught
		caught=0
		For tri=EachIn tris
			go=0
			For itri=EachIn intris
				num=0
				p1=Null
				p2=Null
				For i=0 To 2
					For ii=0 To 2
						If tri[i]=itri[ii] 
							If p1
								p2=tri[i]
							Else
								p1=tri[i]
							EndIf
							num:+1
						EndIf
					Next
				Next
				If num >=2
					ngo=1
					If p1 And p2
						For l:line=EachIn elements
							If l.contains(p1) And l.contains(p2)
								ngo=0
							EndIf
						Next
					EndIf
					If ngo go=1
				EndIf
			Next
			If go 
				intris.addlast tri
				caught:+1
				tris.remove tri
			EndIf
		Next
	Wend
	
	op:TList=New TList
	SetColor 0,255,0
	For tri=EachIn intris
		p1=tri[0]
		p2=tri[1]
		p3=tri[2]
		If Not op.contains(p1) op.addlast p1
		If Not op.contains(p2) op.addlast p2
		If Not op.contains(p3) op.addlast p3
		poly=[p1.x,p1.y,p2.x,p2.y,p3.x,p3.y]
		'SetAlpha .1
		DrawPoly poly
		'SetAlpha 1
	Next
	SetColor 0,0,0
	
	Function findadjacent:point(p1:point,op:TList)
		For l:line=EachIn elements
			For p2:point=EachIn op
				If l.contains(p1) And l.contains(p2) Return p2
			Next
		Next
	End Function
	
	bounds:TList=New TList
	bound:TList=New TList
	While op.count()
		p1:point=point(op.removefirst())
		bound.addlast p1
		caught=1
		While caught
			op.remove p1
			caught=0
			p1=findadjacent(p1,op)
			If p1
				bound.addlast p1
				caught=1
			EndIf
		Wend
		bounds.addlast bound
		bound=New TList
	Wend
	If Not bounds.count() Return 0
	
	tarea#=0
	bigarea#=-1
	For bound:TList=EachIn bounds
		p2:point=Null
		For p:point=EachIn bound
			If p2
				DrawLine p2.x,p2.y,p.x,p.y
			EndIf
			p2=p
		Next
		
		area#=0
		ptris:TList=triangulate(bound)
		If ptris
			For t:tri=EachIn ptris
				't.draw
				area:+t.area()
			Next
		EndIf
		If area>bigarea Or bigarea=-1
			bigarea=area
		EndIf
		tarea:+area
	Next


	DrawText bigarea-(tarea-bigarea),0,30

		
	Return
	
End Function





Function triangulate:TList(points:TList) 
'this function takes a list of the vertices of a polygon (in order) as input, and returns a list of triangles.

	points = points.Copy()   'this algorithm works by removing points from the list, so we make a copy of it and leave the original intact
	
	c = points.count()  'we keep track of how many points are in our working polygon so that we know when to stop!
	If c < 3 Return New TList 'error-checking: fewer than 3 points doesn't make a polygon
	
	l:TList = New TList 'this list will store all our triangles
	While c>3	
		Local array:point[]
		array = point[] (points.toarray())  'make an array from the list of points, for easier referencing
	
		
		i = 0
		go = 0
		While Not go
			p1:point=array[i]
			p2:point=array[(i+1) Mod c]
			p3:point = array[(i + 2) Mod c] 
			
			'p1,p2,p3 are consecutive points on the boundary of the polygon.
			'consider the triangle p1->p2->p3
			
			midx:Float = (p1.x + p2.x + p3.x) / 3.0	'(midx,midy) is a point inside the candidate triangle
			midy:Float = (p1.y + p2.y + p3.y) / 3.0
			
			'here we check if (midx,midy) is inside the polygon. An 'S'-bend in the polygon can cause the candidate triangle
			'to actually be on the outside of the polygon, making it useless in a triangulation.
			'This check works by counting the number of times a horizontal ray originating from (midx,midy) crosses the boundary of the polygon
			'if hits is odd, then (midx,midy) is inside the polygon.
			hits=0
			For ii = 0 To c - 1
				x1#=array[ii].x
				y1#=array[ii].y
				x2#=array[(ii+1) Mod c].x
				y2#=array[(ii+1) Mod c].y
				If (y1-midy)*(y2-midy)<0
					ix#=x1+(x2-x1)*(midy-y1)/(y2-y1)
					If ix<midx hits:+1
				EndIf
			Next

			If (hits Mod 2) 'tri is inside polygon
			
				'We now know the triangle is inside the polygon, so the last thing we need to check is that the line p3->p1
				'doesn't cross the boundary at any point.
				
				x1#=p1.x
				y1#=p1.y
				x2#=p3.x
				y2#=p3.y
				dx1#=x2-x1
				dy1#=y2-y1
				
				go=1
				n=(i+3) Mod c
				While n<>i
					x3#=array[n].x
					y3#=array[n].y
					dx2#=x3-x2
					dy2#=y3-y2
					
					If dx1<>dx2 Or x1<>x2 Or dy1<>dy2 Or y1<>y2
						lambda#=(y2-y1+dy2*(x1-x2)/dx2)/(dy1-dx1*dy2/dx2)
						mu#=(x1-x2+lambda*dx1)/dx2
						If lambda>0 And lambda<1
							If mu>=0 And mu<=1
								go=0
							EndIf
						EndIf
					EndIf
					x2=x3
					y2=y3
					n=(n+1) Mod c
				Wend
			EndIf
			
			If Not go 'if go=0, then our line crossed the boundary at some point, so this triangle isn't an ear.
				i=(i+1) Mod c
				If i=0 Return Null
			EndIf
		Wend

		'by the time we get out of that while loop, we know that the triangle p1->p2->p3 is an ear, so can be clipped
		t:tri = tri.Create(p1, p2, p3) 
		
		
		'remove p2 from the list of points - this is the same as removing the whole ear from the polygon - now there is no way 
		'p1->p2->p3 will be considered again.
		points.remove p2
		
		l.addlast t	'add the triangle to our list of ears
		c:-1	'we've removed a point

	Wend
	
	'we're left with a single triangle, but it's not in our list of ears yet, so we need to add it
	array=point[](points.toarray())
	t:tri=tri.Create(array[0],array[1],array[2])
	l.addlast t
	
	'done! return the list of triangles
	Return l
	
End Function


Type tri
	Field p1:point,p2:point,p3:point
	
	Function Create:tri(p1:point,p2:point,p3:point)
		t:tri=New tri
		t.p1=p1
		t.p2=p2
		t.p3=p3
		Return t
	End Function
	
	Method draw()
		Local poly:Float[] 
		SetAlpha.5
		poly =[p1.x, p1.y, p2.x, p2.y, p3.x, p3.y] 
		DrawPoly poly
		SetAlpha 1
		DrawLine p1.x, p1.y, p2.x, p2.y
		DrawLine p2.x, p2.y, p3.x, p3.y
		DrawLine p3.x, p3.y, p1.x, p1.y
	End Method
	
	Method area#()
		dx1#=p2.x-p1.x
		dy1#=p2.y-p1.y
		dx2#=p3.x-p1.x
		dy2#=p3.y-p1.y
		d1#=Sqr(dx1*dx1+dy1*dy1)
		d2#=Sqr(dx2*dx2+dy2*dy2)
		an#=ACos((dx1*dx2+dy1*dy2)/(d1*d2))
		Return d1*d2*Sin(an)/2
	
	End Method
End Type


Function addelement(e:element)
	For e2:element=EachIn elements
		For p:point=EachIn e.intersections(e2)
			addpoint p
		Next
	Next
	e.addextrapoints
	elements.addlast e
End Function

Function redopoints()
	points=New TList
	l:TList=elements.copy()
	'y=0
	While l.count()
		e:element=element(l.removefirst())
		For e2:element=EachIn l
			il:TList=e.intersections(e2)
			'DrawText il.count(),200,y
			'y:+15
			For p:point=EachIn il
				addpoint p
			Next
		Next
		e.addextrapoints
	Wend
End Function

Function addpoint(p:point)
	For p2:point=EachIn points
		If p2.distance(p.x,p.y)<5 Return
	Next
	points.addlast p
End Function

Type label
	Field x#,y#
	Field txt$
	
	Function Create:label(txt$,x#,y#)
		la:label=New label
		la.x=x
		la.y=y
		la.txt=txt
		Return la
	End Function
	
	Method draw()
		DrawText txt,x,y
	End Method
End Type

Global elements:TList
Global labels:TList
Global points:TList
Global gwidth=800
Global gheight=800
Global state
'Global mx,my
Global curtool:tool

AppTitle="Eugottabeclidding"
Graphics gwidth,gheight,0
SetClsColor 248,236,194
SetColor 0,0,0
SetBlend ALPHABLEND


elements=New TList
points=New TList
labels=New TList

curtool=Null
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())

	mx=MouseX()
	my=MouseY()
	
	If curtool
		DrawText curtool.name+" (right click to go back)",0,0
		
		mindist#=-1
		cx#=mx
		cy#=my
		For p:point=EachIn points
			d#=p.distance(mx,my)
			If d<5 And (d<mindist Or mindist=-1)
				mindist=d
				cx=p.x
				cy=p.y
			EndIf
		Next
		If mindist=-1
			For e:element=EachIn elements
				d#=e.distance(mx,my)
				If d<5 And (d<mindist Or mindist=-1)
					mindist=d
					cx=e.closestx(mx,my)
					cy=e.closesty(mx,my)
				EndIf
			Next
		EndIf
		
		DrawRect cx,cy,5,5
		
		curtool.update cx,cy
		If MouseHit(2)
			curtool.state:-1
		EndIf
		If curtool.state=-1
			curtool=Null
			FlushMouse
			FlushKeys
		EndIf
	Else
		DrawText "Press a key to select a tool",0,0
		DrawText "R: Ruler",0,15
		DrawText "C: Compass",0,30
		DrawText "E: Eraser",0,45
		DrawText "L: Labeller",0,60
		DrawText "A: Polygon area calculator",0,75
		cr$=Chr(GetChar())
		Select cr
		Case "d"
			curtool=New drag
		Case "r"
			curtool=New ruler
		Case "c"
			curtool=New compass
		Case "e"
			curtool=New eraser
		Case "l"
			curtool=New labeller
		Case "a"
			curtool=New areameasurer
		End Select
	EndIf
	
	
	For e:element=EachIn elements
		e.draw
	Next
	
	For p:point=EachIn points
		p.draw
	Next
	
	For la:label=EachIn labels
		la.draw
	Next
	
	If curtool
		curtool.draw
	EndIf
	
	Flip
	Cls
Wend