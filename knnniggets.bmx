'This is just a little type to keep track of individual control points
Type point
	Field x#,y#
	
	Function create:point(x#,y#)
		p:point=New point
		p.x=x
		p.y=y
		Return p
	End Function
End Type



Type line
	Field x# , y# , ex# , ey# , dx# , dy# , length# , an#
	Field nx#,ny#
	
	Function create:line(x1# , y1# , x2# , y2#)
		l:line = New line
		l.x = x1
		l.y = y1
		dx# = x2 - x1
		dy# = y2 - y1
		l.length=Sqr(dx*dx+dy*dy)
		l.dx=dx/l.length
		l.dy=dy/l.length
		l.an=ATan2(dy,dx)
		l.ex = x1+l.dx*l.length
		l.ey = y1 + l.dy * l.length
		l.nx = Cos(an - 90)
		l.ny=Sin(an-90)
		
		Return l
	End Function

	
	Method intersect#(l:line,fit=2) ' returns point on line where it intersects given line, or -1 if no intersection (on segment). fit=2 requires intersection point to be on second line segment, fit=1 just requires it to be on second line
		If l.dx = 0
			mu# = (l.x - x) / dx
		Else
			mu# = (l.y - y + (l.dy / l.dx) * (x - l.x) ) / (dy - l.dy * dx / l.dx)
		EndIf
		'DrawRect x + 400 + dx * mu , y + 400 + dy * mu , 5 , 5
		
		If fit = 0
			Return mu
		EndIf
		
		If l.dx=0
			lambda# = (y - l.y + mu * dy) / l.dy
		Else
			lambda# = (x - l.x + mu * dx) / l.dx
		EndIf
		
		'DrawText lambda,x+400+mu*dx,y+400+mu*dy
		If mu >= 0 And mu <= length
			Select fit
			Case 2
				If lambda <= l.length And lambda >= 0
					hit = mu
				Else
					hit = - 1
				EndIf
			Case 1
				If lambda >= 0
					hit = mu
				Else
					hit = - 1
				EndIf
			End Select
		Else
			hit = - 1
		EndIf
		Return hit
	End Method
	
	Method pointintersect#(px# , py#) 'returns point on line where normal to line contains given point
		pdx# = nx*5
		pdy# = ny*5
		l:line = line.create(px , py , px + pdx , py + pdy)
		'l.draw(400 , 400)
		mu# = intersect(l , 1)
		Return mu
	End Method
	
	Method pointdistance#(px#,py#,fit=0) 'returns (perpendicular) distance from line to point
		pdx# = nx*5
		pdy# = ny*5
		l:line = line.create(px , py , px + pdx , py + pdy)
		'l.draw(400 , 400)
		mu# = l.intersect(Self , fit)
		Return mu
	End Method	
	
	Method draw()
		drawzoomline x , y , ex , ey
	End Method
End Type




Function ZoomX#(x#)
	Return (x - panx) * zoom + gwidth / 2
End Function
Function ZoomY#(y#)
	Return (y - pany) * zoom + gheight / 2
End Function

Function UnzoomX#(x#)
	Return (x - gwidth / 2) / zoom + panx
End Function
Function UnzoomY#(y#)
	Return (y - gheight / 2) / zoom + pany
End Function

Function DrawZoomPoly(poly#[],outline=False)
	poly=poly[..]
	While i < Len(poly)
		poly[i] = zoomx(poly[i])
		poly[i + 1] = zoomy(poly[i + 1]) 
		i:+ 2
	Wend
	If outline
		ox# = poly[0]
		oy# = poly[1]
		i = 2
		While i < Len(poly)
			DrawLine ox , oy , poly[i] , poly[i + 1]
			ox = poly[i]
			oy=poly[i+1]
			i:+ 2
			DrawLine poly[0],poly[1],ox,oy
		Wend
	Else
		DrawPoly poly
	EndIf
End Function

Function DrawZoomLine(ax# , ay# , bx# , by#)
	ax = zoomx(ax)
	ay = zoomy(ay)
	bx = zoomx(bx)
	by = zoomy(by)
	DrawLine ax,ay,bx,by
End Function

Function DrawZoomRect(x# , y# , width# , height#,zoomdimensions=1,filled=1)
	x = zoomx(x)
	y = zoomy(y)
	If zoomdimensions
		width:* zoom
		height:* zoom
	EndIf
	If filled
		DrawRect x , y , width , height
	Else
		DrawLine x , y , x + width , y
		DrawLine x + width , y , x + width , y + height
		DrawLine x , y , x , y + height
		DrawLine x , y + height , x + width , y + height
	EndIf
End Function

Function DrawZoomText(txt$ , x# , y#)
	x = ZoomX(x)
	y = ZoomY(y)
	DrawText txt , x , y
End Function
		

'Catmull Rom romspline type - the points list is a list of the control points to use in the romspline
Type romspline

	Field points:TList
	
	Method New() '(This is called when the romspline object is created, it just initialises the points list)
		points = New TList
	End Method
	
	Function create:romspline()
		s:romspline = New romspline
		Return s
	End Function
	
	Method addpoint(p:point) 'Call this to add a point to the end of the list
		points.addlast p
	End Method
	
	Method somepoints:tlist(n)
		stp# = 1.0 / n
		
		num=points.count()
		If num<4 Then Return 'Check there are enough points to draw a romspline

		outlists:tlist=New tlist

		'Get the first four TLinks in the list of points. This algorithm is going to work by crawling along the list, then getting the point objects from the TLinks. Yet more irrelevant stuff to the actual Catmull Rom.
		pl0:TLink=points.firstlink()
		pl1:TLink=pl0.nextlink()
		pl2:TLink=pl1.nextlink()
		pl3:TLink=pl2.nextlink()
		
		While pl3 <> Null 'pl3 will be null when we've reached the end of the list
			list:tlist = New tlist
			outlists.addlast list
			'get the point objects from the TLinks
			p0:point=point(pl0.value())
			p1:point=point(pl1.value())
			p2:point=point(pl2.value())
			p3:point = point(pl3.value() )
			ox# = p1.x
			oy# = p1.y
			Local tri#[6]
			For i = 0 To n'THE MEAT And BONES! Oddly, there isn't much to explain here, just copy the code.
				t#=Float(i)/n
				x# = .5 * ( (2 * p1.x) + (p2.x - p0.x) * t + (2 * p0.x - 5 * p1.x + 4 * p2.x - p3.x) * t * t + (3 * p1.x - p0.x - 3 * p2.x + p3.x) * t * t * t)
				y# = .5 * ( (2 * p1.y) + (p2.y - p0.y) * t + (2 * p0.y - 5 * p1.y + 4 * p2.y - p3.y) * t * t + (3 * p1.y - p0.y - 3 * p2.y + p3.y) * t * t * t)
				'DrawRect x , y , 1 , 1
				list.addlast point.create(x , y)
			Next
			If filled
				tri = [p1.x , p1.y , ox , oy , p2.x , p2.y]
				DrawZoomPoly tri
			EndIf
			
			'Move one place along the list
			pl0=pl1
			pl1=pl2
			pl2=pl3
			pl3=pl3.nextlink()
		Wend
		
		Return outlists
	End Method
	
	Method draw(filled=0) 'Do the actual drawing!
	
		'Just marking out the control points here
		'For p:point=EachIn points
		'	DrawRect p.x-1,p.y-1,2,2
		'Next
		
		num=points.count()
		If num<4 Then Return 'Check there are enough points to draw a romspline

		'Get the first four TLinks in the list of points. This algorithm is going to work by crawling along the list, then getting the point objects from the TLinks. Yet more irrelevant stuff to the actual Catmull Rom.
		pl0:TLink=points.firstlink()
		pl1:TLink=pl0.nextlink()
		pl2:TLink=pl1.nextlink()
		pl3:TLink=pl2.nextlink()
		
		While pl3<>Null 'pl3 will be null when we've reached the end of the list
			'get the point objects from the TLinks
			p0:point=point(pl0.value())
			p1:point=point(pl1.value())
			p2:point=point(pl2.value())
			p3:point = point(pl3.value() )
			ox# = p1.x
			oy# = p1.y
			Local tri#[6]
			For t#=0 To 1 Step .1 'THE MEAT And BONES! Oddly, there isn't much to explain here, just copy the code.
				x# = .5 * ( (2 * p1.x) + (p2.x - p0.x) * t + (2 * p0.x - 5 * p1.x + 4 * p2.x - p3.x) * t * t + (3 * p1.x - p0.x - 3 * p2.x + p3.x) * t * t * t)
				y# = .5 * ( (2 * p1.y) + (p2.y - p0.y) * t + (2 * p0.y - 5 * p1.y + 4 * p2.y - p3.y) * t * t + (3 * p1.y - p0.y - 3 * p2.y + p3.y) * t * t * t)
				'DrawRect x , y , 1 , 1
				If filled
					tri = [p1.x , p1.y , ox , oy , x , y]
					DrawZoomPoly tri
				Else
					DrawZoomLine ox , oy , x , y
				EndIf
				ox = x
				oy = y
			Next
			If filled
				tri = [p1.x , p1.y , ox , oy , p2.x , p2.y]
				DrawZoomPoly tri
			EndIf
			
			'Move one place along the list
			pl0=pl1
			pl1=pl2
			pl2=pl3
			pl3=pl3.nextlink()
		Wend
	End Method
End Type



Function sameside(p1x#,p1y#,p2x#,p2y#,ax#,ay#,bx#,by#)
	cp1# = (bx-ax)*(p1y-ay)-(p1x-ax)*(by-ay)
	cp2# = (bx-ax)*(p2y-ay)-(p2x-ax)*(by-ay)
	If cp1*cp2 >= 0 Then Return True
End Function	
	
Function pointintriangle(px#,py#,ax#,ay#,bx#,by#,cx#,cy#)
	If sameside(px , py , ax , ay , bx , by , cx , cy) And sameside(px , py , bx , by , ax , ay , cx , cy) And sameside(px , py , cx , cy , ax , ay , bx , by)
		'Local tri#[6]
		'tri = [ax , ay , bx , by , cx , cy]
		'SetColor 255,255,255
		'DrawPoly tri
		'Print String(ax)+","+String(ay)+","+String(bx)+","+String(by)+","+String(cx)+","+String(cy)
		'Flip
		'DebugStop
		Return True
	Else
		Return False
	EndIf
End Function

Function quickhull:TList(s:TList)
	If s.count()<=3 Return s
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
	
	an#=ATan2(r.y-l.y,r.x-l.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=Cos(an+90)
	sy#=Sin(an+90)
	
	s1:TList=New TList
	s2:TList=New TList
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
	
	out1:TList=findhull(s1,l,r)
	out2:TList=findhull(s2,r,l)
	out:TList = New TList
	out.addlast l
	If out1
		For o:Object=EachIn out1
			out.addlast o
		Next
	EndIf
	out.addlast r
	If out2
		For o:Object=EachIn out2
			out.addlast o
		Next
	EndIf

	Return out
End Function

Function findhull:TList(sk:TList,p:point,q:point)
	If Not sk.count() Return Null
	c:point=Null
	out:TList=New TList
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
	an#=ATan2(c.y-p.y,c.x-p.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=Cos(an+90)
	sy#=Sin(an+90)
	s1:TList=New TList
	s2:TList=New TList
	For tp:point=EachIn sk
		If tp<>c
			If Not pointintriangle(tp.x,tp.y,p.x,p.y,q.x,q.y,c.x,c.y)
				mu#=(p.y-tp.y+(ry/rx)*(tp.x-p.x))/(sy-sx*ry/rx)
				If mu<0 s1.addlast tp ElseIf mu>0 s2.addlast tp
			EndIf
		EndIf
	Next
	out1:TList=findhull(s1,p,c)
	out2:TList=findhull(s2,c,q)
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

Type shape
	Field points:tlist
	Field cx# , cy#
	Field edges:tlist
	Field mypoly#[]
	
	Method New()
		edges = New tlist
	End Method
	
	Method makeshape(yapoints:tlist)
		points = yapoints
		Local poly#[points.count() * 2]
		i = 0
		op:point = Null
		For p:point = EachIn points
			poly[i] = p.x
			poly[i + 1] = p.y
			i:+ 2
			
			cx:+ p.x
			cy:+ p.y
			
			If op
				edges.addlast(line.create(op.x , op.y , p.x , p.y) )
			EndIf
			op=p
		Next
		cx:/ points.count()
		cy:/ points.count()
		
		fp:point=point(points.first())
		edges.addlast(line.create(p.x , p.y , fp.x , fp.y) )
		
		mypoly = poly		
	End Method
	
	Method draw()
		SetColor 150 , 150 , 150
		drawzoompoly mypoly
	End Method
End Type

Global islands:tlist = New tlist
Global numlevels=0
Type level Extends shape
	Field ups:tlist
	Field down:level
	Field height
	Field subs:tlist
	Field number
	
	Method New()
		'levels.addlast Self
		ups = New tlist
		subs = New tlist
	End Method
	
	Method reposition(x# , y#)
		For p:point = EachIn points
			p.x:- x
			p.y:- y
		Next
		For sub:level = EachIn subs
			sub.reposition(x , y)
		Next
		For up:level = EachIn ups
			up.reposition(x , y)
		Next
	End Method
	
	Method save(f:tstream)
		WriteInt f,points.count()
		For p:point = EachIn points
			WriteFloat f,p.x
			WriteFloat f,p.y
		Next
		For sub:level = EachIn subs
			WriteLine f,"sub"
			sub.save(f)
		Next
		For up:level = EachIn ups
			WriteLine f,"up"
			up.save(f)
		Next
		WriteLine f,"end"
	End Method
	
	Function load:level(f:tstream , down:level = Null)
		numpoints = ReadInt(f)
		points:tlist = New tlist
		For n = 1 To numpoints
			x# = ReadFloat(f)
			y# = ReadFloat(f)
			points.addlast(point.create(x , y) )
		Next
		l:level = level.create(points , down)
		
		in$ = ReadLine(f)
		While in <> "end"
			Select in
			Case "sub"
				sub:level = level.load(f , Null) 
				sub.height = l.height
				islands.remove sub
				l.subs.addlast sub
			Case "up"
				up:level = level.load(f , l)
			End Select
			in=ReadLine(f)
		Wend
		Return l
	End Function
	
	Function create:level(points:tlist , down:level , issub = 0)
		l:level = New level

		numlevels:+ 1
		l.number=numlevels
		
		If down
			down.ups.addlast l
			l.down = down
			l.height = l.down.height + 1
			If l.height > maxheight
				maxheight = l.height
			EndIf
		Else
			If issub
				l.height=issub
			Else
				islands.addlast l
				l.height=1
			EndIf
		EndIf
		
		l.makeshape(points)
		
		Return l
	End Function
	
	Method draw()
		hshade#=Sqr(height/maxheight)
		'SetColor 255 * hshade , 255 * hshade , 255 * hshade
		SetColor 0,204*hshade,0
		'spl.draw(True) 
		DrawZoomPoly mypoly
		'SetColor 255 , 255 , 255
		'drawzoompoly mypoly,True
		For sub:level = EachIn subs
			sub.draw()
		Next
		For up:level=EachIn ups
			up.draw()
		Next
		'SetColor 255,255,255
		'DrawZoomText String(Int(cx))+","+String(Int(cy)),cx,cy
	End Method
	
	Method pointinside(x# , y#)
		p1:point = Null
		p2:point = Null
		For p:point = EachIn points
			If p1
				If p2
					If pointintriangle(x , y , p1.x , p1.y , p2.x , p2.y , p.x , p.y)
						Return 1
					EndIf
				EndIf
				p2=p
			Else
				p1 = p
			EndIf
		Next
		For sub:level = EachIn subs
			If sub.pointinside(x , y) Return 1
		Next
		Return 0
	End Method
	
	Method drawoutline()
		SetColor 255 , 255 , 255
		For l:line = EachIn edges
			l.draw()
		Next
		'spl.draw(False)
	End Method
	
	Method highestcollision:level(x# , y#)
		If pointinside(x , y)
			For up:level = EachIn ups
				l:level = up.highestcollision(x , y)
				If l Return l
			Next
			Return Self
		Else
			Return Null
		EndIf
	End Method
	
	Method distancetoedge#(x# , y#)
		mindist#=-1
		For l:line = EachIn edges
			d# = l.pointdistance(x , y , 2) 
			If d >= 0 And (d < mindist Or mindist = - 1)
				mindist = d
			EndIf
		Next
		If mindist = - 1
			For p:point = EachIn points
				dx# = p.x - x
				dy# = p.y - y
				d# = Sqr(dx * dx + dy * dy)
				If d < mindist Or mindist = - 1
					mindist = d
				EndIf
			Next
		EndIf
		Return mindist
	End Method
	
	Method heightatpoint#(x# , y#)
		ddown# = distancetoedge(x , y) 
		If ddown = 0 Return height
		If Not ups.count() Return height
		mindup# = - 1
		For up:level = EachIn ups
			dup# = up.distancetoedge(x , y) 
			If dup < mindup Or mindup = - 1
				mindup = dup
			EndIf
		Next
		r# = mindup/ddown
		h#=2.0^(-r)+height
		Return h
	End Method
	
	Method lineintersect:line(l:line)
		For l:line = EachIn edges
			
		Next
	End Method
End Type

Global entities:tlist=New tlist
Type entity
	Field x# , y#, radius#
	
	Method New()
		entities.addlast Self
		radius=3
	End Method
	
	Method click(button = 1)
		Return
	End Method
End Type

Global castles:tlist = New tlist
Type castle Extends entity
	Field knights
	Field r:road
	
	Method New()
		castles.addlast Self
		'super.new()
		radius = 5
	End Method
	
	Method save(f:tstream)
		WriteFloat f , x
		WriteFloat f , y
	End Method
	
	Function load:castle(f:tstream)
		x# = ReadFloat(f)
		y# = ReadFloat(f)
		c:castle = castle.create(x , y) 
		Return c
	End Function
	
	Function create:castle(x# , y#)
		c:castle = New castle
		c.x = x
		c.y = y
		c.r = road.create(x , y)
		roads.addlast c.r
		Return c
	End Function
	
	Method draw()
		SetColor 100,100,100
		drawzoomrect x-5,y-5,10,10,1
	End Method
	
	Method click(button)
		Select button
		Case 1 'draw road
			gamestate = 1
			oldr = r
		Case 2 'send knights
			gamestate = 2
			oldc=Self
		End Select
	End Method
	
	Method sally(target:road) 
		route:tlist = r.dijkstra(target) 
		knight.create(route , r)
	End Method
End Type

Global knights:tlist=New tlist
Type knight Extends entity
	Field route:tlist
	Field myp:path , dir,omyp:path
	Field r:road,target:road
	Field mu#,v#
	Field state
	
	Method New()
		knights.addlast Self
	End Method
	
	Function create:knight(route:tlist , r:road)
		k:knight = New knight
		k.route = route
		k.r = r
		Return k
	End Function
	
	Method update()
		drawzoomtext "KNIGHT" , x , y
		drawzoomtext v,x,y+5
		Select state
		Case 0 'got route
			If Not myp
				If route.count()
					target:road=road(route.removelast())
					For p:path = EachIn r.paths
						If p.other(r) = target
							myp = p
						EndIf
					Next
					If myp.r1 = r
						dir = -1
					Else
						dir =  1
					EndIf
					mu = 0
					If omyp
						dotprod# = omyp.l.dx * myp.l.dx + omyp.l.dy * myp.l.dy
						scale# = (dotprod + 1) * .5
						v:* scale
					EndIf
				Else
					state = 1
				EndIf
			Else
				s# = Sin(myp.slope * 135 * dir + 45)
				s:*Abs(s)
				maxspeed# = (s + 1.1) * .1
				v:+(maxspeed-v)*.1
				along#=mu/myp.l.length
				x = r.x * (1-along) + target.x * along
				y = r.y * (1-along) + target.y * along
				mu:+ v
				If mu > myp.l.length
					r = target
					omyp=myp
					myp = Null
					mu=0
				EndIf
			EndIf
		Case 1 'waiting at target
			knights.remove Self
		End Select
	End Method
	
	Method draw()
		'If target
		'	SetLineWidth 3
		'	drawzoomline r.x , r.y , target.x , target.y
		'	SetLineWidth 1
		'EndIf
		SetColor 255,255,255
		drawzoomrect x - 1 , y - 1 , 2 , 2 , 1 , 0
	End Method
End Type

Global roads:tlist=New tlist
Type road Extends entity
	Field lvl:level , paths:tlist,links:tlist
	Field height#
	Field tot#,prv:road
	
	Method New()
		links = New tlist
		paths = New tlist
		'super.new()
		radius=3
	End Method
	
	Function create:road(x#,y#)
		r:road = New road
		r.x = x
		r.y = y
		r.lvl = highestin(x , y)
		If r.lvl
			r.height = r.lvl.heightatpoint(x , y)
		EndIf
		Return r
	End Function
	
	Method link(r:road)
		If Not links.contains(r)
			p:path = path.create(r , Self)
			paths.addlast p
			r.paths.addlast p
			links.addlast r
			r.links.addlast Self
		EndIf	
	End Method
	
	Method draw()
		'SetColor 255 , 255 , 255
		'drawzoomrect x,y,5,5,0
		'DrawZoomText Int(height*10),x,y
	End Method
	
	Method dijkstra:tlist(target:road)
		q:tlist=roads.copy()
		For r:road = EachIn q
			r.tot = - 1
			r.prv = Null
		Next
		tot = 0
		mintot#=-1
		While q.count() 
			mintot# = - 1
			u:road=Null
			For r:road = EachIn q
				If r.tot >= 0 And (r.tot < mintot Or mintot = - 1)
					u = r
					mintot = r.tot
				EndIf
			Next
			Print mintot
			q.remove u
			For p:path = EachIn u.paths
				r:road = p.other(u)
				alt# = u.tot + p.l.length
				If alt < r.tot Or r.tot=-1
					r.tot = alt
					r.prv = u
				EndIf
			Next
		Wend
		out:tlist = New tlist
		While target.prv
			out.addlast(target)
			target = target.prv
		Wend
		Return out
	End Method
	
	Method click(button)
		Select button
		Case 1 'extend road
			gamestate = 1
			oldr = Self
		End Select
	End Method
End Type


Global paths:tlist=New tlist
Type path
	Field r1:road , r2:road
	Field x1# , y1# , x2# , y2#
	Field l:line
	Field slope#
	
	Method New()
		paths.addlast Self
	End Method
	
	Method other:road(r:road)
		If r1 = r Return r2 Else Return r1
	End Method
	
	Function create:path(r1:road , r2:road)
		p:path = New path
		p.r1 = r1
		p.r2 = r2
		p.x1 = r1.x
		p.y1 = r1.y
		p.x2 = r2.x
		p.y2 = r2.y
		p.l = line.create(p.x1 , p.y1 , p.x2 , p.y2)
		p.slope = calcslope(r1.height , r2.height , p.l.length)
		Return p
	End Function
	
	Function calcslope#(h1# , h2# , dist#)
		s# = 50 * (h2 - h1) / dist
		If s > 1 s = 1
		If s < - 1 s = - 1
		Return s
	End Function
	
	Method draw()
		s#=Abs(slope)
		col# = (1 - s) * 255
		SetColor 255,col,col
		SetLineWidth 3
		l.draw()
		SetLineWidth 1
		'drawzoomtext l.length,x1*.5+x2*.5,y1*.5+y2*.5
		'drawzoomtext slope,x1*.5+x2*.5,y1*.5+y2*.5
	End Method
End Type

Function highestin:level(x# , y#)
	out:level=Null
	For l:level = EachIn islands
		highest:level = l.highestcollision(x , y)
		If highest Then out = highest
	Next
	Return out
End Function


Function loadmap(filename$)
	f:tstream = ReadFile(filename)
	initworld()
	maxheight=1
	While Not Eof(f) 
		in$ = ReadLine(f)
		Select in
		Case "level"
			level.load(f)
		Case "castle"
			castle.load(f)
		End Select
	Wend
	CloseFile f
End Function

Function savemap(filename$) 
	f:tstream=WriteFile(filename)
	cx# = 0
	cy# = 0
	For l:level = EachIn islands
		cx:+ l.cx
		cy:+l.cy
	Next
	cx:/ islands.count()
	cy:/ islands.count()
	
	For l:level = EachIn islands
		WriteLine f,"level"
		l.reposition(cx,cy)
		l.save(f)
	Next
	For c:castle = EachIn castles
		c.x:- cx
		c.y:-cy
		WriteLine f , "castle"
		c.save(f)
	Next
	CloseFile f
End Function

Function initzoom()
	panx = 0
	pany = 0
	zoom = 1
	mzoom = 0
	omz# = MouseZ() 	
End Function

Function initworld()
	islands = New tlist
	roads=New tlist
	paths=New tlist
	castles = New tlist
	entities = New tlist
	knights=New tlist
	initzoom()
	gamestate = 0
End Function

Function editmap(filename$)
	maxheight=1
	If FileType(filename)
		loadmap(filename)
	Else
		initworld()
	EndIf

	initzoom()

	curlevel:level = Null
	surlevel:level=Null
	
	oldms = MilliSecs()
	
	done=0
	While Not done
		tick:+1
	
		For l:level = EachIn islands
			l.draw()
		Next
		
		For r:road = EachIn roads
			r.draw()
		Next
		
		For c:castle = EachIn castles
			c.draw()
		Next
		
		mousecontrol()	
		
		Select gamestate
		Case 0 'doing nothing
			SetColor 255,255,255
			DrawText "Left click to start drawing upwards, right click to extend level" , 0 , 0
			DrawText "C to place castle",0,15
			DrawText "Backspace to clear map, Space to save and quit" , 0 , 30
			
			
			DrawText String(mx)+","+String(my),0,gheight-30
			l:level = highestin(mx , my)
			If l
				h# = l.heightatpoint(mx , my)
				DrawText h,zoomx(mx),zoomy(my)
			EndIf
			
			If mhit[1]
				curlevel=highestin(mx,my)
				gamestate = 1
				points:tlist = New tlist
				If curlevel
					If curlevel.height = maxheight Then maxheight:+ 1
				EndIf
			EndIf
			If mhit[2]
				surlevel:level=highestin(mx,my)
				If surlevel
					curlevel = surlevel.down
					gamestate = 2
					points:tlist = New tlist
				EndIf
			EndIf
			
			If KeyHit(KEY_C)
				l:level = highestin(mx , my)
				If l
					castle.create(mx , my)
				EndIf
			EndIf
			
			If KeyHit(KEY_BACKSPACE) 
				islands = New tlist
				castles=New tlist
				curlevel = Null
				surlevel=Null
			EndIf				
	
			If KeyHit(KEY_SPACE)
				done=1
			EndIf
		Case 1 , 2 'drawing level
			SetColor 255,255,255
			DrawText "move mouse to draw area, right click to finish",0,0
			If curlevel
				curlevel.drawoutline()
			EndIf
			For p:point = EachIn points
				SetColor 255,255,255
				'DrawZoomLine ox , oy , p.x , p.y
				DrawZoomRect p.x,p.y,4,4,0
				ox# = p.x
				oy# = p.y
			Next
			'If MouseHit(1)
			'If tick Mod 3 =1
				p:point = point.create(mx , my) 
				go=1
				If curlevel
					If curlevel.pointinside(p.x , p.y)
						For up:level = EachIn curlevel.ups
							If up.pointinside(p.x , p.y) And up<>surlevel go = 0
						Next
					Else
						go=0
					EndIf
				Else
					For l:level = EachIn islands
						If l<>surlevel And l.pointinside(p.x , p.y)
							go = 0
						EndIf
					Next
				EndIf
				If go Then points.addlast p
			'EndIf
			If points.count() > 3
				outline:tlist = quickhull(points)
				s:shape = New shape
				s.makeshape(outline)
				s.draw()
			EndIf
			
			If mhit[2]
				If points.count()>3
					points = quickhull(points)
					Select gamestate
					Case 1
						l:level = level.create(points , curlevel)
						curlevel = l
					Case 2
						l:level = level.create(points , Null,surlevel.height)
						surlevel.subs.addlast l
					End Select
				EndIf
				gamestate=0
			EndIf
		End Select
		
		ms = MilliSecs()
		fps# = 1000.0 / (ms - oldms)
		oldms=ms
		DrawText Int(fps),0,gheight-15
		
		Flip
		Cls
	Wend
	
	savemap(filename)
	
End Function

Function mousecontrol()
	mx# = MouseX()
	my# = MouseY()
	For i = 1 To 3
		mhit[i] = MouseHit(i)
	Next 
	scrollspeedo#=.1/zoom
	If mx < 100
		panx:- (100 - mx) * scrollspeedo
	EndIf
	If mx > gwidth - 100
		panx:+ (100-gwidth + mx) * scrollspeedo
	EndIf
	If my < 100
		pany:- (100 - my) * scrollspeedo
	EndIf
	If my > gheight - 100
		pany:+ (100-gheight + my) * scrollspeedo
	EndIf
	mx = UnzoomX(mx)
	my = UnzoomY(my)

	mz = MouseZ()
	mzoom:+ (mz - omz) * .1
	If mzoom<0 mzoom=0
	zoom=1.2^mzoom
	If zoom<1 zoom=1
	omz=mz
End Function

Function pickclosest:entity(list:tlist , x# , y#, inradius=1)
	mindist# = - 1
	pickede:entity=Null
	For e:entity = EachIn list
		dx# = e.x - x
		dy# = e.y - y
		d# = dx * dx + dy * dy
		If d < e.radius * e.radius Or inradius=0
			If d < mindist Or mindist = - 1
				mindist = d
				pickede = e
			EndIf
		EndIf
	Next
	If pickede
		SetColor 255,255,255
		drawzoomrect pickede.x - pickede.radius , pickede.y - pickede.radius , 2 * pickede.radius , 2 * pickede.radius , 1 , 0
	EndIf
	Return pickede
End Function

Function rungame()
	initzoom()
	done = 0
	gamestate = 0
	oldr = Null
	oldc=Null
	
	While Not done
		tick:+1
		
		mousecontrol()
		
		For l:level = EachIn islands
			l.draw() 
		Next
		
		For r:road = EachIn roads
			r.draw()
		Next
		
		For p:path = EachIn paths
			p.draw()
		Next

		For c:castle = EachIn castles
			c.draw()
		Next
		
		For k:knight = EachIn knights
			k.update()
			k.draw()
		Next
		
		
		If KeyHit(KEY_ESCAPE)
			done = 1
		EndIf
		
		DrawText String(Int(mx))+","+String(Int(my)),0,gheight-15
		
		Select gamestate
			Case 0 'default
				pickede:entity = pickclosest(entities , mx , my)
				If pickede
					For i=1 To 3
						If mhit[i]
							pickede.click(i)
						EndIf
					Next
				Else
				
				EndIf
			Case 1 'drawing road
				SetColor 255,255,255
				DrawText "left click to place road, right click to end" , 0 , 0
				If oldr
					dx# = mx - oldr.x
					dy# = my - oldr.y
					d# = Sqr(dx * dx + dy * dy)
					If d > 20
						d = d
						dx:* 20 / d
						dy:* 20 / d
						mx = oldr.x + dx
						my = oldr.y + dy
						d=20
					EndIf
					
					l:level = highestin(mx , my)
					If l
						h# = l.heightatpoint(mx , my)
						s# = Abs(path.calcslope(oldr.height , h , d))
						If s > 1
							col# = 0
						Else
							col# = (1 - s) * 255
						EndIf
						SetColor 255,col,col
						DrawzoomText s , mx , my
					EndIf
					drawzoomline oldr.x , oldr.y , mx , my
					SetColor 255,255,255
				EndIf
				
				
				pickedr:road = road(pickclosest(roads , mx , my))
				If mhit[1]
					If pickedr
						If oldr<>pickedr
							oldr.link(pickedr)
							oldr = pickedr
						EndIf
					Else
						r:road = road.create(mx , my)
						If r.lvl
							roads.addlast r
							If oldr
								oldr.link(r)
							EndIf
							oldr=r
						EndIf
					EndIf
				EndIf
				DrawText roads.count() , 0 , 15
				If oldr Then DrawText("!!!" , 0 , 30)
		
				If oldr
				EndIf		
		
				If mhit[2]
					gamestate=0
				EndIf
		
		Case 2 'sally forth!
			l:level = highestin(mx , my)
			If l
				If mhit[1]
					oldc.sally(mx , my)
				EndIf
			EndIf
			'pickedr:road = road(pickclosest(roads , mx , my) )
			'If pickedr
			'	If mhit[1]
			'		oldc.sally(pickedr)
			'		gamestate=0
			'	EndIf
			'EndIf
		End Select
		
		DrawText knights.count(),400,0
		
		Flip
		Cls
	Wend
EndFunction

AppTitle = "Knnnnniggets!"
Global gwidth=800,gheight=800
Graphics gwidth , gheight, 0
SetBlend ALPHABLEND

Global mx#, my#,mhit[4]

Global panx# = 0 , pany# = 0 , zoom# = 1 , mzoom# = 1 , omz#=MouseZ()

Global maxheight# = 1

Global gamestate=0

Global oldr:road = Null
Global oldc:castle=Null

Global tick=0

editmap("map.txt")

loadmap("map.txt")
rungame()

Rem
	Case 3 'placing roads
		SetColor 255,255,255
		DrawText "left click to place road, right click to end",0,0
		If MouseHit(1)
			r:road = road.create(mx , my) 
			If r.lvl
				roads.addlast r
				If oldr
					oldr.link(r)
				EndIf
				oldr=r
			EndIf
		EndIf

		If oldr
			drawzoomline oldr.x , oldr.y , mx , my
		EndIf		

		If MouseHit(2)
			gamestate=0
		EndIf
endrem