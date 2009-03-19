Global roadpoints:tlist=New tlist
Global roadsegments:tlist=New tlist
Global roads:tlist=New tlist
Global intersections:tlist=New tlist


Type roadpoint
	Field x#,y#
	Field mysegment:roadsegment
	
	Method New()
		roadpoints.addlast Self
	EndMethod
	
	Function create:roadpoint(x#,y#)
		rp:roadpoint=New roadpoint
		rp.x=x
		rp.y=y
		Return rp
	End Function
	
	Method draw()
		DrawOval x-2,y-2,4,4
	EndMethod
End Type

Type roadsegment
	Field points:tlist
	Field myroad:road
	
	Method New()
		points=New tlist
		roadsegments.addlast Self
	End Method
	
	Method addpoint(rp:roadpoint)
		If points.count()>1
			l:TLink=points.lastlink()
			rp1:roadpoint=roadpoint(l.value())
			rp2:roadpoint=roadpoint(l.prevlink().value())
			dx#=rp.x-rp1.x
			dy#=rp.y-rp1.y
			d#=dx*dx+dy*dy
			If d<500
				c#=dotprodbetween(rp.x,rp.y,rp1.x,rp1.y,rp2.x,rp2.y)
				If c>.97
					removepoint rp1
				EndIf
			EndIf
		EndIf
		points.addlast(rp)
		rp.mysegment=Self
	End Method
	
	Method removepoint(rp:roadpoint)
		points.remove rp
		roadpoints.remove rp
	EndMethod
	
	Method replacepoint:roadpoint(rp:roadpoint)
		l:TLink=points.findlink(rp)
		If Not l Then Return rp
		irp:intersection=intersection.create(rp.x,rp.y)
		points.insertafterlink(irp,l)
		removepoint rp
		
		rs:roadsegment=New roadsegment
		While points.last()<>irp
			rs.points.addfirst points.last()
			points.remove points.last()
		Wend
		rs.points.addfirst irp
		l=myroad.segments.findlink(Self)
		myroad.segments.insertafterlink rs,l
		
		Return irp
	EndMethod		
	
	Method draw()
		l:TLink=points.firstlink()
		While l.nextlink()<>Null
			a:roadpoint=roadpoint(l.value())
			l=l.nextlink()
			b:roadpoint=roadpoint(l.value())
			DrawLine a.x,a.y,b.x,b.y
			a.draw()
		Wend
		If b<>Null Then b.draw()
	EndMethod
End Type

Type road
	Field segments:tlist
	Field r,g,b
	
	Method New()
		segments=New tlist
		roads.addlast Self

		an=Rand(360)
		radius#=Sqr(Rnd())
		r=redness(an,r)
		g=greenness(an,r)
		b=blueness(an,r)
	End Method
	
	Method addsegment(rs:roadsegment)
		segments.addlast(rs)
		rs.myroad=Self
	End Method
	
	Method newsegment:roadsegment(startpoint:roadpoint)
		rs:roadsegment=New roadsegment
		addsegment(rs)
		rs.addpoint(startpoint)
		Return rs
	End Method
	
	Method draw()
		SetColor r,g,b
		For rs:roadsegment=EachIn segments
			rs.draw()
		Next
	End Method
End Type

Type intersection Extends roadpoint
	Field roads:tlist
	
	Method New()
		roads=New tlist
		intersections.addlast Self
	End Method
	
	Function create:intersection(x#,y#)
		irp:intersection=New intersection
		irp.x=x
		irp.y=y
		Return irp
	EndFunction
	
	Method addroad(r:road)
		If Not roads.contains(r)
			roads.addlast r
		EndIf
	EndMethod
	
	Method draw()
		GetColor r,g,b
		SetColor 255,255,255
		DrawOval x-4,y-4,8,8
		SetColor r,g,b
	EndMethod
		
End Type

Function dotprodbetween#(x#,y#,x1#,y1#,x2#,y2#)
	dx#=x-x1
	dy#=y-y1
	ddx#=x1-x2
	ddy#=y1-y2
	an1#=ATan2(dy,dx)
	an2#=ATan2(ddy,ddx)
	c#=Cos(an1-an2)
	Return c
End Function

''''''Colour picker functions
Function redness#(an#,r#)
	If r>1 Then r=1
	c#=Cos(an-90)
	If c<0 c=0
	Return 255*(r*c+(1-r)*(1-c))
End Function

Function greenness#(an#,r#)
	If r>1 Then r=1
	c#=Cos(an+30)
	If c<0 c=0
	Return 255*(r*c+(1-r)*(1-c))
End Function

Function blueness#(an#,r#)
	If r>1 Then r=1
	c#=Cos(an+150)
	If c<0 c=0
	Return 255*(r*c+(1-r)*(1-c))
End Function
'''''''''''''


Graphics 800,800,0
SeedRnd MilliSecs()

Const STATE_NULL=0,STATE_DRAWING=1
Global state=STATE_NULL
omx=0
omy=0
rs:roadsegment=Null
While Not KeyHit(KEY_ESCAPE)
	mx=MouseX()
	my=MouseY()
	
	hrp:roadpoint=Null
	closestrp:roadpoint=Null
	mindist#=-1
	For irp:intersection=EachIn intersections
		dx#=mx-irp.x
		dy#=my-irp.y
		d#=mx*mx+my*my
		If d<mindist Or mindist=-1
			closestrp=irp
			mindist=d
			If d<122
				hrp=irp
			EndIf
		EndIf
	Next
	
	'If Not hrp
		For rp:roadpoint=EachIn roadpoints
			dx#=mx-rp.x
			dy#=my-rp.y
			d#=dx*dx+dy*dy
			If rs<>Null
				If rp.mysegment=rs And rp<>rs.points.first() Then d=mindist+1
			EndIf
			If d<mindist Or mindist=-1
				closestrp=rp
				mindist=d
				If d<122
					hrp=rp
				EndIf
			EndIf
		Next
	'EndIf
	
	Select state
	Case STATE_NULL
		If MouseHit(1)
			state=STATE_DRAWING
			If hrp
				rp:roadpoint=hrp
				If intersection(rp)
					r:road=New road
				Else
					If rp=rp.mysegment.points.first() Or rp=rp.mysegment.points.last()
						r:road=rp.mysegment.myroad
					Else
						r:road=New road
					EndIf
					rp.mysegment.replacepoint(rp)
				EndIf
			Else
				r:road=New road
				rp:roadpoint=roadpoint.create(mx,my)
			EndIf
			rs:roadsegment=r.newsegment(rp)
		EndIf
	Case STATE_DRAWING
		If Not MouseDown(1)
			state=STATE_NULL
			If hrp
				'rs.removepoint(roadpoint(rs.points.last()))
				If Not intersection(hrp)
					hrp=hrp.mysegment.replacepoint(hrp)
				EndIf
				wakkawakka=1
				rs.addpoint(hrp)
			EndIf
			wakkawakka=0
			rs=Null
		Else
			link:TLink=rs.points.lastlink()
			rp1:roadpoint=roadpoint(link.value())
			dx#=mx-rp1.x
			dy#=my-rp1.y
			d#=dx*dx+dy*dy
			DrawText d,0,0
			If d>400
				rs.addpoint(roadpoint.create(mx,my))
			ElseIf d>9
				DrawText rs.points.count(),0,30
				If rs.points.count()>1
					rp2:roadpoint=roadpoint(link.prevlink().value())
					c#=dotprodbetween(mx,my,rp1.x,rp1.y,rp2.x,rp2.y)
					DrawText c,0,15
					If c<.9
						rs.addpoint(roadpoint.create(mx,my))
					EndIf
				EndIf
			EndIf
		EndIf
	End Select
	DrawText state,200,0
	
	For r:road=EachIn roads
		r.draw()
	Next
	
	If closestrp<>Null
		SetColor 255,255,255
		DrawOval closestrp.x-5,closestrp.y-5,10,10
	EndIf
	If hrp<>Null
		SetColor 255,0,0
		DrawOval hrp.x-5,hrp.y-5,10,10
	EndIf
	
	Flip
	Cls			
	omx=mx
	omy=my
Wend