Include "convexhull.bmx"

Global lettertemplates:lettertemplate[]
Type lettertemplate
	Field img:TImage,w,h
	Field tx,ty,tw,th
	Method New()
		lettertemplates:+[Self]
	End Method
	
	Function Create:lettertemplate(filename$,tx,ty,tw,th)
		lt:lettertemplate = New lettertemplate
		lt.img=LoadImage(filename)
		lt.w=ImageWidth(lt.img)*.5
		lt.h=ImageHeight(lt.img)*.5
		lt.tx=tx*.5
		lt.ty=ty*.5
		lt.tw=tw*.5
		lt.th=th*.5
		Return lt
	End Function
	
	Method draw(address$[],x#,y#)
		SetScale .5,.5
		SetAlpha 1
		DrawImage img,x,y
		SetColor 0,0,0
		SetScale .3,.3
		fittext address,x+tx,y+ty,tw,th
		SetScale 1,1
	End Method
End Type

lettertemplate.Create("letter1.png",7,101,389,158)
lettertemplate.Create("letter2.png",5,97,390,150)
lettertemplate.Create("letter3.png",123,15,306,212)

Function fittext(lines$[],x#,y#,w#,h#)
	GetScale sx#,sy#
	
	th#=TextHeight(lines[0])*sx
	bw#=0
	For i=0 To Len(lines)-1
		tw=TextWidth(lines[i])*sy
		If tw>bw bw=tw
	Next
	bw=(w-bw)/2
	bh=(h-Len(lines)*th)/2
	For i=0 To Len(lines)-1
		DrawText lines[i],x+bw,y+bh
		y:+th
	Next
	SetScale 1,1
End Function


Type letter
	Field address$[]
	Field destination:place
	Field x#,y#
	Field bag:postbag
	Field tw# , th#
	Field w#,h#
	Field template:lettertemplate
	
	Method New()
		template = lettertemplates[Rand(0,Len(lettertemplates)-1)]
	End Method
	
	Function Create:letter(address$[], destination:place)
		l:letter = New letter
		l.address = address
		l.destination = destination
		l.tw = 0
		For line$ = EachIn address
			l.tw = Max(l.tw , TextWidth(line) )
		Next
		l.th=TextHeight(address[0])
		l.w = l.template.w
		l.h= l.template.h
		Return l
	End Function
		
	Method overlap(l:letter)
		If x < l.x + l.w And x + w > l.x And y < l.y + l.h And y + h > l.y
			Return True
		EndIf
	End Method
	
	Method drop(p:place)
		bags:TList = New TList
		bags.addlast bag
		For l:letter = EachIn p.mailbox
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
			For l2:letter = EachIn p.mailbox
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
				p.bags.addlast l.bag
				l.bag.addlast l
			EndIf
		Next
		
		For b:postbag=EachIn bags
			If b.count()=0
				p.bags.remove b
			EndIf
		Next
	End Method
		
	Method draw()
		If bag.count()>1
			SetColor bag.red , bag.green , bag.blue
		ElseIf y<gheight/2
			SetColor 255,255,255
		Else
			SetColor 150 , 150 , 150
		EndIf
		template.draw address,x,y
			
	End Method
End Type

Global postbags:TList=New TList
Type postbag Extends TList
	Field red , green , blue
	
	Method New()
		postbags.addlast Self
		pickcolour red,green,blue
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

Global vans:TList=New TList
Type van
	Field bag:postbag
	Field route:place[]
	Field p:place,p2:place
	Field x#,y#,t#
	Field dx#,dy#,d#
	
	Method New()
		vans.addlast Self
	End Method
	
	Function Create:van(bag:postbag,route:place[],start:place)
		v:van=New van
		v.route = route
		v.bag = bag
		v.drive start,route[0]
		Return v
	End Function 
	
	Method drive(start:place,finish:place)
		t:-d
		p=start
		p2=finish
		
		dx=p2.x-p.x
		dy=p2.y-p.y
		d=Sqr(dx*dx+dy*dy)
		dx:/d
		dy:/d
	End Method
	
	Method update(delta#)
		t:+delta*15
		x=p.x+dx*t
		y=p.y+dy*t
		
		If t>=d
			For l:letter=EachIn bag
				If l.destination = p2
					bag.remove l
					p2.deliverletter l
				EndIf
			Next
			
			If Len(route)>1
				route=route[1..]
				drive p2,route[0]
			Else
				vans.remove Self
				For l:letter=EachIn bag
					p2.deliverletter l
				Next
			EndIf
		EndIf
	End Method
	
	Method draw()
		SetColor 255,0,0
		SetAlpha 1
		DrawRect x-5,y-5,10,10
		label bag.count(), x+3,y-15, True, .1, .3, 0,255,0
	End Method
End Type

Global labels:TList=New TList
Type tlabel
	Field x#,y#
	Field nx#,ny#,ox#,oy#
	Field w#,h#,scale#
	Field txt$
	Field weight#
	Field r,g,b
	Field bg
	
	Method New()
		labels.addlast Self
	End Method
	
	Method draw()
		If bg
			SetScale 1,1
			SetColor 100,0,0
			DrawRect x,y,w,h
		EndIf
		SetColor r,g,b
		SetScale scale,scale
		DrawText txt,x+10*scale,y+6*scale
		SetScale 1,1
	End Method
	
	Function arrange()
		move=1
		c=0
		While move And c<20
			c:+1
			move=0
			For l:tlabel=EachIn labels
				l.nx=0
				l.ny=0
			Next
			For l:tlabel=EachIn labels
				dx#=Min(gwidth-l.w,Max(0,l.x))
				dy#=Min(gheight-l.h,Max(0,l.y))
				l.nx:+dx-l.x
				l.ny:+dy-l.y
				For l2:tlabel=EachIn labels
					If l<>l2 And l2.x<l.x+l.w And l2.x+l2.w>l.x And l2.y<l.y+l.h And l2.y+l2.h>l.y
						move:+1
						dx1#=l2.x+l2.w-l.x
						dx2#=l.x+l.w-l2.x
						dy1#=l2.y+l2.h-l.y
						dy2#=l.y+l.h-l2.y
						md#=Min(Abs(dx1),Min(Abs(dx2),Min(Abs(dy1),Abs(dy2))))
						
						If md=Abs(dx1)
							dx#=dx1
							dy#=0
						ElseIf md=Abs(dx2)
							dx#=-dx2
							dy#=0
						ElseIf md=Abs(dy1)
							dx#=0
							dy#=dy1
						ElseIf md=Abs(dy2)
							dx#=0
							dy#=-dy2
						EndIf
						
						f1#=l2.weight/(l.weight+l2.weight)
						f2#=l.weight/(l.weight+l2.weight)
						
						l.x:+dx*f1
						l.y:+dy*f1
						l2.x:-dx*f2
						l2.y:-dy*f2
						
						Rem
						dx#=(l2.x+l2.w/2)-(l.x+l.w/2)
						dx=Sgn(dx)*(Abs(dx)-(l.w+l2.w)/2)
						dy#=(l2.y+l2.h/2)-(l.y+l.h/2)
						dy=Sgn(dy)*(Abs(dy)-(l.h+l2.h)/2)
						f#=l2.weight/(l2.weight+l.weight)
						f2#=l.weight/(l2.weight+l.weight)
						l.nx:+f*dx/2
						l.ny:+f*dy/2
						l2.nx:-f2*dx/2
						l2.ny:-f2*dy/2
						EndRem
						
						
					EndIf
				Next
			Next
			For l:tlabel=EachIn labels
				l.x:+l.nx
				l.y:+l.ny
			Next
		Wend
		
		For l:tlabel=EachIn labels
			l.draw
		Next
		
		labels=New TList
	End Function
End Type

Function label(txt$,x,y,bg=True,weight#=1,scale#=1,r=255,g=255,b=255)
	l:tlabel=New tlabel
	l.x=x
	l.y=y
	l.bg=bg
	l.ox=x
	l.oy=y
	l.scale=scale
	l.w=TextWidth(txt)*scale+20*scale
	l.h=TextHeight(txt)*scale+12*scale
	l.txt=txt
	l.weight=weight
	l.r=r
	l.g=g
	l.b=b
End Function

Function capitalise$(word$)
	word=Trim(word)
	Return Upper(word[..1])+Lower(word[1..])
End Function

'load place name bits
Global prefixes$[],suffixes$[],forenames$[],surnames$[],streetnames$[]
For bit$=EachIn LoadText("prefix.txt").split("~n")
	bit=Trim(bit)
	If bit
		prefixes:+[bit]
	EndIf
Next
For bit$=EachIn LoadText("suffix.txt").split("~n")
	bit=Trim(bit)
	If bit
		suffixes:+[bit]
	EndIf
Next
For bit$=EachIn LoadText("any.txt").split("~n")
	bit=Trim(bit)
	If bit
		prefixes:+[bit]
		suffixes:+[bit]
	EndIf
Next
For bit$=EachIn LoadText("streetname.txt").split("~n")
	bit=Trim(bit)
	If bit
		streetnames:+[bit]
	EndIf
Next
For bit$=EachIn LoadText("surname.txt").split("~n")
	bit=Trim(bit)
	If bit
		surnames:+[capitalise(bit)]
	EndIf
Next
For bit$=EachIn LoadText("forename.txt").split("~n")
	bit=Trim(bit)
	If bit
		forenames:+[capitalise(bit)]
	EndIf
Next

Global places:TList=New TList
Type place
	Field streetname$
	Field town:ttown
	Field x#,y#,r#
	Field net:network
	Field links:TList
	Field pop
	Field mailbox:TList
	Field bags:TList
	
	'pathfinding
	Field d#,pred:place,dsteps
	
	Function generate(places:TList,x#,y#,r#,depth)
		If depth<=0 Return 0
		p:place=New place
		p.x=x
		p.y=y
		places.addlast p
		
		sum=0
		For c=1 To Max(1,poisson(2))
			pop=place.generate(places,x+Rnd(-1,1)*r,y+Rnd(-1,1)*r,r^.8,depth-1)
			sum:+pop*pop
		Next
		p.pop=Sqr(sum)+Rand(50,100)
		
		p.r = Sqr(p.pop)/4+3
		
		Return p.pop
	End Function
	
	Method New()
		net=New network
		net.addlast Self
		links=New TList
		mailbox=New TList
		bags = New TList

		townname$ = prefixes[Rand(0 , Len(prefixes) - 1)] + suffixes[Rand(0 , Len(suffixes) - 1)]
		
		town = ttown.Create(capitalise(townname))
		town.places.addlast Self
		
	End Method
	
	Method link(p2:place)
		links.addlast p2
		p2.links.addlast Self
		If Not net.contains(p2) net.merge(p2.net)
	End Method
	
	Method route:place[](p2:place)
		If p2=Self 
			Return(New place[0])
		EndIf
		
		For p:place=EachIn net
			p.d=-1
			p.dsteps=-1
		Next
		
		d=0
		dsteps=0
		
		unchecked:TList=New TList
		unchecked.addlast Self
		
		While unchecked And unchecked.count()
			mindist#=-1
			close:place=Null
			For p:place=EachIn unchecked
				If p.d<mindist Or mindist=-1
					mindist=p.d
					close=p
				EndIf
			Next
			
			For p:place=EachIn close.links
				dx#=p.x-close.x
				dy#=p.y-close.y
				nd#=close.d+Sqr(dx*dx+dy*dy)
				If nd<p.d Or p.d=-1
					If p.d=-1 unchecked.addlast(p)
					p.d=nd
					p.pred=close
					p.dsteps=close.dsteps+1
				EndIf
			Next
			unchecked.remove close
			If close=p2
				unchecked=Null
			EndIf
		Wend
		Local r:place[p2.dsteps]
		While p2<>Self
			r[p2.dsteps-1]=p2
			p2=p2.pred
		Wend
		Return r
	End Method
	
	Method makeletter()
		p:place=Self
		While p=Self Or Rand(5)>1
			i=Rand(1,p.links.count())-1
			p=place(p.links.valueatindex(i))
		Wend

		name$=forenames[Rand(0,Len(forenames)-1)]+" "+surnames[Rand(0,Len(surnames)-1)]
		Local address$[]=[name,p.streetname,Upper(p.town.name)]
		l:letter=letter.Create(address,p)
		deliverletter l
		
	End Method
	
	Method deliverletter(l:letter)
		If l.destination=Self 
			Return
		EndIf
		
		l.bag=New postbag
		bags.addlast l.bag
		l.bag.addlast l
		
		mailbox.addlast l
		
		l.x=Rand(0,gwidth-l.w)
		l.y=Rand(gheight/2,gheight-l.h)
	End Method

	Method pickletter:letter(x# , y#)
		For l:letter = EachIn mailbox
			If x>l.x And y>l.y And x<l.x+l.w And y<l.y+l.h
				Return l
			EndIf
		Next
	End Method
		
	Method sendbags()
		If bags.count()=0 Return
		outbags:TList=New TList
		For b:postbag=EachIn bags
			ok=True
			For l:letter=EachIn b
				If l.y>=300
					ok=False
					Exit
				EndIf
			Next
			If ok
				outbags.addlast b
			EndIf
			If b.count()=0
				DebugStop
			EndIf
		Next
		For b:postbag=EachIn outbags
			Local sr:place[]=Null
			For l:letter=EachIn b
				Local r:place[] = route(l.destination)
				If Not sr
					sr=r
				Else
					i=0
					While i<Len(sr) And i<Len(r) And sr[i]=r[i]
						i:+1
					Wend
					sr=sr[..i]
				EndIf
			Next
			If Len(sr)>0
				dest:place=sr[Len(sr)-1]
				van.Create b,sr,Self
				For l:letter=EachIn b
					mailbox.remove l
				Next
				bags.remove b
			EndIf
		Next
		
	End Method
	
	Method update(delta#)
		prob#=delta*pop*.0002
		For c=1 To poisson(prob)
			makeletter
		Next
	End Method
	
	Method draw()
		SetScale 1,1
		SetAlpha 1
		SetColor 255,255,255
		DrawOval x - r , y - r , r*2,r*2
		SetLineWidth 3
		For p2:place=EachIn links
			DrawLine x,y,p2.x,p2.y
		Next
		SetLineWidth 1
		If mailbox.count()>0
			label mailbox.count(), x+r,y-15, False, 1, .3, 255,255,0
		EndIf
	End Method
End Type

Type ttown
	Field border#[]
	Field name$
	Field rightest:point
	Field places:TList
	
	Method New()
		places=New TList
	End Method
	
	Function Create:ttown(name$)
		t:ttown=New ttown
		t.name=name
		Return t
	End Function
	
	Method makeborder()
		points:TList=New TList
		For pl:place=EachIn places
			For c=1 To 6
				an#=Rnd(60)+c*60
				d#=Rnd(15,30)
				points.addlast point.Create(pl.x+d*Cos(an),pl.y+d*Sin(an))
			Next
		Next
		points = quickhull(points)

		border = New Float[points.count()*2]
		
		i=0
		For p:point=EachIn points
			border[i]=p.x
			border[i+1]=p.y
			If rightest=Null Or p.x>rightest.x
				rightest=p
			EndIf
			i:+2
		Next
	End Method
	
	Method draw()
		SetColor 255,255,255
		SetAlpha .2
		DrawPoly border
'		SetAlpha 1
'		SetScale .7,.7
'		DrawText name,rightest.x+3,rightest.y
'		SetScale 1,1
		label name,rightest.x+3,rightest.y, False, 0.01, 1, 255,255,255
	End Method
End Type

Type network Extends TList	
	Field towns:TList
	
	Function generate:network(x#,y#,r#,depth)
		towngap#=r/5
		places=New TList
		place.generate(places,x,y,r,depth)
		
		'make sure no two places too close together
		For c=1 To 10
			For p:place=EachIn places
				For p2:place=EachIn places
					If p2<>p
						dx# = p2.x - p.x
						dy# = p2.y - p.y
						d# = Sqr(dx * dx + dy * dy)
						If d=0
							dx=Rnd(-1,1)
							dy=Sqr(1-dx*dx)
							d=1
						EndIf
						dx:/d
						dy:/d
						td=Rnd(20,40)
						If d < td
							f#=td-d
							p2.x :+ f*dx/2
							p2.y :+ f*dy/2
							p.x :- f*dx/2
							p.y :- f*dy/2
						EndIf
					EndIf
				Next
			Next
'			For p:place=EachIn places
''				If p.x<0 p.x=0
'				If p.x>gwidth p.x=gwidth-10
'				If p.y<0 p.y=0
'				If p.y>gheight-10 p.y=gheight-10
'			Next
		Next


		root:place=place(places.first())	'we'll need this later to get its network

		
		'make up all pairs of places
		Local pairs:TList=New TList
		places.sort
		While places.count()
			p:place=place(places.removefirst())
			For p2:place=EachIn places
				pairs.addlast(place[]([p,p2]))
			Next
		Wend
		
		'sort pairs by distance 
		Function sortpairs(o1:Object,o2:Object)
			Local pair1:place[]=place[](o1)
			Local pair2:place[]=place[](o2)
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
		
		'link pairs, starting at closest, so everything belongs to one network
		While pairs.count()
			Local pair:place[]=place[](pairs.removefirst())
			If Not pair[0].net.contains(pair[1])
				pair[0].link(pair[1])
			EndIf
		Wend
		
		places = root.net.copy()
		
		Function linksort(o1:Object , o2:Object)
			p1:place = place(o1)
			p2:place = place(o2)
			Return p1.links.count() - p2.links.count()
		End Function
		
		places.sort True,linksort
		
		While places.count()
			p:place=place(places.removefirst())
			checko:TList=New TList
			checko.addlast p
			Local pstreetnames$[]=streetnames[..]
			While checko.count()
				p:place=place(checko.removefirst())
				i=Rand(0,Len(pstreetnames)-1)
				p.streetname=pstreetnames[i]
				pstreetnames=pstreetnames[..i]+pstreetnames[i+1..]
				For p2:place = EachIn places.copy()
					dx# = p2.x - p.x
					dy# = p2.y - p.y
					d# = Sqr(dx * dx + dy * dy)
					If d < towngap
						p2.town = p.town
						p.town.places.addlast p2
						places.remove p2
						checko.addlast p2
					EndIf
				Next
			Wend
		Wend

		places=root.net
		minx#=0
		maxx#=0
		miny#=0
		maxy#=0
		For p:place=EachIn places
			If p.x<minx Or minx=0 minx=p.x
			If p.x>maxx Or maxx=0 maxx=p.x
			If p.y<miny Or miny=0 miny=p.y
			If p.y>maxy Or maxy=0 maxy=p.y
		Next
		maxx:-minx
		maxy:-miny
		
		scale# = r / Max(maxx , maxy)
		offx# = (gwidth-scale*maxx) / 2
		offy#=(gheight-scale*maxy)/2
		For p:place=EachIn places
			p.x=(p.x-minx)*scale+offx
			p.y=(p.y-miny)*scale+offy
		Next
		
		
		towns:TList=New TList
		For p=EachIn root.net
			If Not towns.contains(p.town)
				towns.addlast p.town
			EndIf
		Next
		For t:ttown=EachIn towns
			t.makeborder
		Next
		root.net.towns = towns
		
		
		For p=EachIn root.net
			p.update 50
		Next
		
		Return root.net
		
	End Function
	
	Method merge(n2:network)
		For p:place=EachIn n2
			p.net=Self
			addlast p
		Next
	End Method
	
	Method pick:place(x# , y# , r#)
		mindist# = - 1
		close:place=Null
		For p:place=EachIn Self
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
		For p:place=EachIn Self
			p.update delta
		Next
	End Method
	
	Method draw()
		For t:ttown=EachIn towns
			t.draw
		Next
		
		For p:place = EachIn Self
			p.draw
		Next
	End Method

End Type

Function pickcolour(r Var, g Var, b Var)
	Local t#
	r=Rnd()
	g=Rnd()
	b=Rnd()
	t = (r+g+b)/2
	r :* 255/t
	g :* 255/t
	b :* 255/t
End Function

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




Global gwidth=1200, gheight=800
Graphics gwidth,gheight,0
SeedRnd MilliSecs()
SetBlend ALPHABLEND

Global printfont:timagefont=LoadImageFont("londonmm.ttf",50)
SetImageFont printfont



net:network=network.generate(gwidth/2,gheight/2,gheight/2,4)

ms = MilliSecs()

mode=0

look:place=Null

Local held:letter, heldbag:postbag, offx#, offy#

While Not (AppTerminate())
	
	mx=MouseX()
	my=MouseY()
	
	oldms=ms
	ms=MilliSecs()
	delta#=(ms-oldms)/1000.0
	
	net.update delta
	For v:van=EachIn vans
		v.update delta
	Next

	Select mode
	Case 0	'nothing
		If MouseHit(1)
			look=net.pick(mx,my,30)
			If look 
				mode=1
			EndIf
		EndIf
		
		If KeyHit(KEY_ESCAPE)
			End
		EndIf
	Case 1	'look at place's letters
		hletter:letter=look.pickletter(mx,my)
		If MouseDown(1)
			If Not held
				held = hletter
				If held
					offx = held.x - mx
					offy = held.y - my
					look.mailbox.remove held
					look.mailbox.addfirst held
				EndIf
			EndIf
			If held
				held.x = mx + offx
				held.y = my + offy
			EndIf
		Else
			If held
				held.drop look
				held=Null
			EndIf
		EndIf
		
		If MouseDown(2)
			If Not heldbag
				If hletter
					heldbag=hletter.bag
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
					l.drop look
				Next
				heldbag=Null
			EndIf
		EndIf


		If KeyHit(KEY_SPACE)
			look.sendbags
			If look.bags.count()=0
				mode=0
				FlushMouse
			EndIf
		ElseIf KeyHit(KEY_ESCAPE)
			mode=0
			FlushMouse
		EndIf
	End Select
	
	
	'''''''''''''''''''''''''' 
	'DRAWING
	
	net.draw
	For v:van=EachIn vans
		v.draw
	Next

	Select mode
	Case 0	
		close:place=net.pick(mx,my,30)
		If close
			label close.streetname + ", " + close.town.name , close.x , close.y, True, 10, .4
		EndIf
		tlabel.arrange
	Case 1
		SetColor 255,255,0
		SetAlpha 1
		DrawOval look.x-8,look.y-8,16,16

		tlabel.arrange
		

		SetColor 255,255,255
		SetAlpha .1
		DrawRect 0,0,gwidth,gheight

		If hletter
			p:place=close
			SetColor 255,0,0
			SetAlpha 1
			SetLineWidth 3
			For p2:place=EachIn close.route(hletter.destination)
				DrawLine p.x,p.y,p2.x,p2.y
				p=p2
			Next
			SetLineWidth 1
		EndIf

		
		For l:letter = EachIn look.mailbox.reversed()
			l.draw
		Next
		
		SetColor 255,255,255
		DrawLine 0,gheight/2,gwidth,gheight/2
		
		DrawText "bags: "+look.bags.count(),0,15
		
	End Select
		
	SetColor 255,255,255
	fps#=1/delta
	DrawText Int(fps),0,0

	Flip
	Cls
Wend
	
