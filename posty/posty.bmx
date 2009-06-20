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
	
	Method drop()
		bags:TList = New TList
		bags.addlast bag
		For l:letter = EachIn letters
			If l <> Self
				If overlap(l)
					If Not bags.contains(l.bag)
						bags.addlast l.bag
					EndIf
				EndIf
			EndIf
		Next
		
		check:TList = New TList
		For b:postbag = EachIn bags
			For l:letter = EachIn b
				check.addlast l
			Next
		Next
		
		For l:letter = EachIn check
			close:letter=Null
			For l2:letter = EachIn letters
				If l2 <> l
					If l.overlap(l2)
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
Type tlabel
	Field x#,y#
	Field nx#,ny#,ox#,oy#
	Field w#,h#
	Field txt$
	
	Method New()
		labels.addlast Self
	End Method
	
	Method draw()
		GetColor r,g,b
		SetColor 100,0,0
		DrawRect x,y,w,h
		SetColor r,g,b
		DrawText txt,x,y
	End Method
	
	Function arrange()
		move=1
		c=0
		While move And c<20
			c:+1
		'For c=1 To 10
			move=0
			For l:tlabel=EachIn labels
				'l.x:+(l.ox-l.x)*.5
				'l.y:+(l.oy-l.y)*.5
				l.nx=0
				l.ny=0
			Next
			For l:tlabel=EachIn labels
				dx#=Min(600-l.w,Max(0,l.x))
				dy#=Min(600-l.h,Max(0,l.y))
				l.nx:+dx-l.x
				l.ny:+dy-l.y
				For l2:tlabel=EachIn labels
					If l<>l2 And l2.x<l.x+l.w And l2.x+l2.w>l.x And l2.y<l.y+l.h And l2.y+l2.h>l.y
						move:+1
						dx#=(l2.x+l2.w/2)-(l.x+l.w/2)
						dx=Sgn(dx)*(Abs(dx)-(l.w+l2.w)/2)
						dy#=(l2.y+l2.h/2)-(l.y+l.h/2)
						dy=Sgn(dy)*(Abs(dy)-(l.h+l2.h)/2)
						'l2.nx:+dx/2
						l.nx:+dx/2
						'l2.ny:+dy/2
						l.ny:+dy/2
						
						Rem
						DrawLine 0,0,l.x,l.y
						move:+1
						Local moves#[]=[l2.x+l2.w-l.x, l2.y+l2.h-l.y, l.x+l.w-l2.x, l.y+l.h-l2.y]
						big=0
						For i=1 To 3
							If Abs(moves[i])<Abs(moves[big]) big=i
						Next
						'l.txt=big+l.txt
						Select big
						Case 0
							l2.nx:-moves[0]*.3
							'l.nx:+moves[0]*.3
						Case 1
							l2.ny:-moves[1]*.3
							'l.ny:+moves[1]*.3
						Case 2
							l2.nx:+moves[2]*.3
							'l.nx:-moves[2]*.3
						Case 3
							l2.ny:+moves[3]*.3
							'l.ny:-moves[3]*.3
						End Select
						EndRem
					EndIf
				Next
			Next
			For l:tlabel=EachIn labels
				l.x:+l.nx
				l.y:+l.ny
			Next
		Wend
		'Next
		
		For l:tlabel=EachIn labels
			l.draw
		Next
		
		labels=New TList
	End Function
End Type

Function label(txt$,x,y)
	l:tlabel=New tlabel
	l.x=x
	l.y=y
	l.ox=x
	l.oy=y
	l.w=TextWidth(txt)
	l.h=TextHeight(txt)
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
For bit$=EachIn LoadText("surname.txt").split("~n")
	If bit
		surnames:+[bit]
	EndIf
Next
For bit$=EachIn LoadText("forename.txt").split("~n")
	If bit
		forenames:+[bit]
	EndIf
Next

Global points:TList=New TList
Type point
	Field streetname$,townname$
	Field x#,y#
	Field net:network
	Field links:TList
	Field pop
	Field mailbox:TList
	
	'pathfinding
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
		'streetname=streetnames[Rand(0,Len(streetnames)-1)]
		mailbox=New TList
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
	
	Method makeletter()
		p:point=Self
		While p=Self Or Rand(5)>1
			i=Rand(1,p.links.count())-1
			p=point(p.links.valueatindex(i))
		Wend

		name$=forenames[Rand(0,Len(forenames)-1)]+" "+surnames[Rand(0,Len(surnames)-1)]
		Local address$[]=[name,p.streetname,p.townname]
		For line$=EachIn address
			Print line
		Next
		Print "FROM: "+streetname+", "+townname
		l:letter=letter.Create(Rand(0,450),Rand(300,500),address)
		mailbox.addlast l
	End Method
	
	Method update(delta#)
		prob#=delta*pop*.0002
		'label poisson(prob),x,y
		For c=1 To poisson(prob)
			makeletter
		Next
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
		End Function
		points.sort True,linksort
		While points.count()
			p:point=point(points.removefirst())
			p.townname$ = prefixes[Rand(0 , Len(prefixes) - 1)] + suffixes[Rand(0 , Len(suffixes) - 1)]
			p.townname=Upper(p.townname[..1])+Lower(p.townname[1..])
			checko:TList=New TList
			checko.addlast p
			Local pstreetnames$[]=streetnames[..]
			While checko.count()
				p:point=point(checko.removefirst())
				i=Rand(0,Len(pstreetnames)-1)
				p.streetname=pstreetnames[i]
				pstreetnames=pstreetnames[..i]+pstreetnames[i+1..]
				For p2:point = EachIn points.copy()
					dx# = p2.x - p.x
					dy# = p2.y - p.y
					d# = Sqr(dx * dx + dy * dy)
					If d < 50
						p2.townname = p.townname
						points.remove p2
						checko.addlast p2
					EndIf
				Next
			Wend
		Wend
		
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
	End Method

	Method update(delta#)
		For p:point=EachIn Self
			p.update delta
		Next
	End Method
	
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

mode=0

look:point=Null

Local held:letter ,heldbag:postbag, offx# , offy#

While Not (AppTerminate() Or KeyHit(KEY_ESCAPE))
	
	mx=MouseX()
	my=MouseY()
	
	oldms=ms
	ms=MilliSecs()
	delta#=(ms-oldms)/1000.0
	
	net.update delta

	Select mode
	Case 0	'nothing
		If MouseHit(1)
			look=net.pick(mx,my,30)
			If look 
				mode=1
				letters=look.mailbox
			EndIf
		EndIf
	Case 1	'look at place's letters
		letters=look.mailbox
		
	
		If MouseDown(1)
			If Not held
				held = letter.pick(mx , my)
				If held
					offx = held.x - mx
					offy = held.y - my
					letters.remove held
					letters.addfirst held
				EndIf
			EndIf
			If held
				held.x = mx + offx
				held.y = my + offy
			EndIf
		Else
			If held
				held.drop
				held=Null
			EndIf
		EndIf
		
		If MouseDown(2)
			If Not heldbag
				l:letter = letter.pick(mx , my)
				If l
					heldbag=l.bag
					offx = mx
					offy = my
				EndIf
			EndIf
			If heldbag
				For l:letter = EachIn heldbag
					l.x:+ mx - offx
					l.y:+ my - offy
				Next
				offx = mx
				offy = my
			EndIf
		Else
			If heldbag
				For l:letter = EachIn heldbag.copy()
					l.drop
				Next
				heldbag=Null
			EndIf
		EndIf


		
		If KeyHit(KEY_SPACE)
			mode=0
		EndIf
	End Select
	
	net.draw

	Select mode
	Case 0	
		close:point=net.pick(mx,my,30)
		If close
			p:point=point(net.first())
			SetColor 255,0,0
			For p2:point=EachIn p.route(close)
				DrawLine p.x,p.y,p2.x,p2.y
				p=p2
			Next
			SetColor 255,255,255
			label close.streetname + ", " + close.townname , close.x , close.y
		EndIf
	Case 1
		SetAlpha .7
		For l:letter = EachIn letters.reversed()
			l.draw
		Next
		SetAlpha 1
		
		SetColor 255,255,255
		DrawText postbags.count(),0,0

	End Select
		
	tlabel.arrange
	
	SetColor 255,255,255
	fps#=1/delta
	DrawText Int(fps),0,0
	DrawText mode,0,15

	Flip
	Cls
Wend
	
