Global letters:TList
Type letter
	Field address$[]
	Field x#,y#
	Field bag:postbag
	Field tw# , th#
	Field w#,h#
	
	Method New()
		bag = New postbag
		bag.addlast Self
	End Method
	
	Function Create:letter(x# , y# , address$[])
		l:letter = New letter
		l.x = x
		l.y = y
		l.address = address
		l.tw = 0
		For line$ = EachIn address
			l.tw = Max(l.tw , TextWidth(line) )
		Next
		l.th=TextHeight(address[0])
		l.w = 140
		l.h= 90
		Return l
	End Function
		
	Function pick:letter(x# , y#)
		For l:letter = EachIn letters
			'dx# = x - l.x
			'dy# = y - l.y
			If x>l.x And y>l.y And x<l.x+l.w And y<l.y+l.h
			'If dx * dx + dy * dy < l.size*l.size
				Return l
			EndIf
		Next
	End Function
	
	Method overlap(l:letter)
		If x < l.x + l.w And x + w > l.x And y < l.y + l.h And y + h > l.y
			Return True
		EndIf
	End Method
	
	Method drop(avoidbag:postbag = Null)
		bags:TList = New TList
		bags.addlast bag
		For l:letter = EachIn letters
			If l <> Self And l.bag<>avoidbag
				'dx# = l.x - x
				'dy# = l.y - y
				'd# = Sqr(dx * dx + dy * dy)
				If overlap(l)
				'If d < l.size + size
					If Not bags.contains(l.bag)
						bags.addlast l.bag
					EndIf
				EndIf
			EndIf
		Next
		'Print "bags: "+bags.count()
		
		check:TList = New TList
		For b:postbag = EachIn bags
			For l:letter = EachIn b
				check.addlast l
			Next
		Next
		'Print "check: "+check.count()
		
		For l:letter = EachIn check
			close:letter=Null
			For l2:letter = EachIn letters
				If l2 <> l
					'dx# = l2.x - l.x
					'dy# = l2.y - l.y
					'd# = Sqr(dx * dx + dy * dy)
					If l.overlap(l2)
					'If d < l.size + l2.size
						close = l2
						Exit
					EndIf
				EndIf
			Next		
			If close
				l.bag.remove l
				l.bag = close.bag	
				l.bag.addlast l
			ElseIf l.bag.count() > 1
				l.bag.remove l
				l.bag = New postbag
				l.bag.addlast l
			EndIf
		Next
	End Method
		
	Method draw()
		If bag.count()>1
			SetColor bag.red , bag.green , bag.blue
		Else
			SetColor 150 , 150 , 150
		EndIf
		DrawRect x  , y,w,h
		SetColor 0 , 0 , 0
		tx# = x - tw/2+w/2
		ty# = y - Len(address)*th / 2+h/2
		For line$ = EachIn address
			DrawText line , tx , ty
			ty:+ th
		Next
			
	End Method
End Type

Global postbags:TList=New TList
Type postbag Extends TList
	Field red , green , blue
	
	Method New()
		postbags.addlast Self
		Local col#[3]
		t# = 1
		For i = 0 To 1
			col[i] = Rnd(0 , t)
			t:- col[i]
		Next
		red = Sqr(col[0])*255
		green = Sqr(col[1])*255
		blue=Sqr(t)*255
	End Method
	Method remove(o:Object)
		Super.remove o
		If Not count()
			postbags.remove Self
		EndIf
	End Method
End Type

Global labels:TList=New TList
Type label
	Field x#,y#
	Field txt$
	
	Method New()
		labels.addlast Self
	End Method
	
	Method draw()
		GetColor r,g,b
		SetColor 0,0,0
		DrawRect x,y,TextWidth(txt),TextHeight(txt)
		SetColor r,g,b
		DrawText txt,x,y
	End Method
End Type

Function drawlabel(txt$,x,y)
	l:label=New label
	l.x=x
	l.y=y
	l.txt=txt
End Function

'load place name bits
Global prefixes$[],suffixes$[],forenames$[],surnames$[],streetnames$[]
For bit$=EachIn LoadText("prefix.txt").split("~n")
	If bit
		prefixes:+[bit]
	EndIf
Next
For bit$=EachIn LoadText("suffix.txt").split("~n")
	If bit
		suffixes:+[bit]
	EndIf
Next
For bit$=EachIn LoadText("any.txt").split("~n")
	If bit
		prefixes:+[bit]
		suffixes:+[bit]
	EndIf
Next
For bit$=EachIn LoadText("streetname.txt").split("~n")
	If bit
		streetnames:+[bit]
	EndIf
Next
'For bit$=EachIn LoadText("surname.txt").split("~n")
'	If bit
'		surnames:+[bit]
'	EndIf
'Next
'For bit$=EachIn LoadText("forename.txt").split("~n")
'	If bit
'		forenames:+[bit]
'	EndIf
'Next

Global points:TList=New TList
Type point
	Field streetname$,townname$
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
		streetname=streetnames[Rand(0,Len(streetnames)-1)]
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
	
	Method draw()
		SetAlpha 1
		SetColor 255,255,255
		DrawOval x - 5 , y - 5 , 10 , 10
		SetLineWidth 3
		For p2:point=EachIn links
			DrawLine x,y,p2.x,p2.y
		Next
		SetLineWidth 1
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
		scale# = 550 / Max(maxx , maxy)
		offx# = (600 - scale * maxx) / 2
		offy#=(600-scale*maxy)/2
		For p:point=EachIn points
			p.x=(p.x-minx)*scale+offx
			p.y=(p.y-miny)*scale+offy
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
		
		points = root.net.copy()
		Function linksort(o1:Object , o2:Object)
			p1:point = point(o1)
			p2:point = point(o2)
			Return p1.links.count() - p2.links.count()
		End function
		points.sort True,linksort
		While points.count()
			p:point=point(points.removefirst())
			p.townname$ = prefixes[Rand(0 , Len(prefixes) - 1)] + suffixes[Rand(0 , Len(suffixes) - 1)]
			p.townname=Upper(p.townname[..1])+Lower(p.townname[1..])
			For p2:point = EachIn points.copy()
				dx# = p2.x - p.x
				dy# = p2.y - p.y
				d# = dx * dx + dy * dy
				If d < 10000
					p2.townname = p.townname
					points.remove p2
				endif
			Next
		wend
		
		Return root.net
		
	End Function
	
	Method merge(n2:network)
		For p:point=EachIn n2
			p.net=Self
			addlast p
		Next
	End Method
	
	Method pick:point(x# , y# , r#)
		mindist# = - 1
		close:point=Null
		For p:point=EachIn Self
			dx#=p.x-x
			dy#=p.y-y
			d#=dx*dx+dy*dy
			If (d<mindist Or mindist=-1) And (d<r*r Or r=-1)
				mindist=d
				close=p
			EndIf
		Next
		Return close
	End method

	
	Method draw()
		For p:point = EachIn Self
			p.draw
		Next
	End Method

End Type

Function poisson(lambda!)
	If lambda>500 Return poisson(lambda/2)+poisson(lambda/2)
	k=0
	u!=Rnd(0,1)
	fact=1
	p!=Exp(-lambda)
	u:-p
	While u>0
		k:+1
		fact:*k
		p:*lambda/k
		u:-p
	Wend
	Return k
End Function


Graphics 600,600,0
SeedRnd MilliSecs()
SetBlend ALPHABLEND

net:network=network.generate(300,300,250,4)

ms = MilliSecs()

While Not (AppTerminate() Or KeyHit(KEY_ESCAPE))
	
	mx=MouseX()
	my=MouseY()

	close:point=net.pick(mx,my,30)
	
	
	net.draw
	
	'Rem
	If close
		p:point=point(net.first())
		SetColor 255,0,0
		For p2:point=EachIn p.route(close)
			DrawLine p.x,p.y,p2.x,p2.y
			p=p2
		Next
		SetColor 255,255,255
		DrawText close.streetname + ", " + close.townname , close.x , close.y
	endif
	'EndRem
	
	oldms=ms
	ms=MilliSecs()
	SetColor 255,255,255
	fps#=1000.0/(ms-oldms)
	DrawText Int(fps),0,0

	Flip
	Cls
Wend
	
