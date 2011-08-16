Global points:TList=New TList
Type point
	Field x#,y#
	Field net:network
	Field links:TList
	Field pop
	Field d#,pred:point
	
	Function generate(points:TList,x#,y#,r#,depth)
		If Not depth Return 0
		p:point=New point
		p.x=x
		p.y=y
		points.addlast p
		sum=0
		For c=1 To Rand(2,3)
			pop=point.generate(points,x+Rnd(-1,1)*r,y+Rnd(-1,1)*r,r^.8,depth-1)
			sum:+pop*pop
		Next
		p.pop=Sqr(sum)+Rand(50,100)
		Return p.pop
	End Function
	
	Method New()
		net=New network
		net.addlast Self
		links=New TList
	End Method
	
	Method link(p2:point)
		links.addlast p2
		p2.links.addlast Self
		If Not net.contains(p2) net.merge(p2.net)
	End Method
	
	Method route:TList(p2:point)
		If p2=Self Return New TList
		
		For p:point=EachIn net
			p.d=-1
		Next
		
		d=0
		
		unchecked:TList=New TList
		unchecked.addlast Self
		
		While unchecked.count()
			mindist#=-1
			close:point=Null
			For p:point=EachIn unchecked
				If p.d<mindist Or mindist=-1
					mindist=p.d
					close=p
				EndIf
			Next
			
			For p:point=EachIn close.links
				dx#=p.x-close.x
				dy#=p.y-close.y
				nd#=close.d+Sqr(dx*dx+dy*dy)
				If nd<p.d Or p.d=-1
					If p.d=-1 unchecked.addlast(p)
					p.d=nd
					p.pred=close
				EndIf
			Next
			unchecked.remove close
			If close=p2
				unchecked=New TList
			EndIf
		Wend
		r:TList=New TList
		While p2<>Self
			r.addfirst p2
			p2=p2.pred
		Wend
		Return r
	End Method
End Type

Type network Extends TList	
	Function generate:network(x#,y#,r#,depth)
		points=New TList
		point.generate(points,x,y,r,depth)
		
		root:point=point(points.first())	'we'll need this later to get its network

		minx#=600
		maxx#=0
		miny#=600
		maxy#=0
		For p:point=EachIn points
			If p.x<minx minx=p.x
			If p.x>maxx maxx=p.x
			If p.y<miny miny=p.y
			If p.y>maxy maxy=p.y
		Next
		maxx:-minx
		maxy:-miny
		scale#=580/Max(maxx,maxy)
		For p:point=EachIn points
			p.x=(p.x-minx)*scale+10
			p.y=(p.y-miny)*scale+10
		Next
		
		Local pairs:TList=New TList
		points.sort
		While points.count()
			p:point=point(points.removefirst())
			For p2:point=EachIn points
				pairs.addlast(point[]([p,p2]))
			Next
		Wend
		
		Function sortpairs(o1:Object,o2:Object)
			Local pair1:point[]=point[](o1)
			Local pair2:point[]=point[](o2)
			dx1#=pair1[0].x-pair1[1].x
			dy1#=pair1[0].y-pair1[1].y
			dx2#=pair2[0].x-pair2[1].x
			dy2#=pair2[0].y-pair2[1].y
			If dx1*dx1+dy1*dy1<dx2*dx2+dy2*dy2
				Return -1
			Else
				Return 1
			EndIf
		End Function
		
		pairs.sort True,sortpairs
		
		While pairs.count()
			Local pair:point[]=point[](pairs.removefirst())
			If Not pair[0].net.contains(pair[1])
				pair[0].link(pair[1])
			EndIf
		Wend
		
		Return root.net
		
	End Function
	
	Method merge(n2:network)
		For p:point=EachIn n2
			p.net=Self
			addlast p
		Next
	End Method
End Type

Graphics 600,600,0
SeedRnd MilliSecs()

net:network=network.generate(300,300,250,4)

ms=MilliSecs()
While Not (AppTerminate() Or KeyHit(KEY_ESCAPE))
	If KeyHit(KEY_SPACE)
		net:network=network.generate(300,300,250,4)
	EndIf
	
	mx=MouseX()
	my=MouseY()
	close:point=Null
	mindist#=-1
	For p:point=EachIn net
		dx#=p.x-mx
		dy#=p.y-my
		d#=dx*dx+dy*dy
		If d<mindist Or mindist=-1
			mindist=d
			close=p
		EndIf
	Next
	
	SetColor 255,255,255
	For p:point=EachIn net
		DrawOval p.x-2,p.y-2,5,5
		For p2:point=EachIn p.links
			DrawLine p.x,p.y,p2.x,p2.y
		Next
	Next

	p:point=point(net.first())
	SetColor 255,0,0
	For p2:point=EachIn p.route(close)
		DrawLine p.x,p.y,p2.x,p2.y
		p=p2
	Next

	oldms=ms
	ms=MilliSecs()
	SetColor 255,255,255
	fps#=1000.0/(ms-oldms)
	DrawText Int(fps),0,0

	Flip
	Cls
Wend