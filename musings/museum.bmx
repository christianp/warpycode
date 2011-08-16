Type room
	Field u#,l#,r#,d#
	Field links:TList
	Field doors:TList
	Field arts:TList
	Field things:TList
	Field distance,pathprv:room
	Field red,green,blue
	
	Method New()
		links=New TList
		doors=New TList
		arts=New TList
		things=New TList
		red=Rnd(0,1)*80
		green=Rnd(0,1)*80
		blue=Rnd(0,1)*80
	End Method
	
	Function Create:room(u#,l#,r#,d#)
		c:room=New room
		c.l=l
		c.u=u
		c.r=r
		c.d=d
		c.populate
		Return c
	End Function
	
	Method populate()
		For c=1 To 5
			a:art=art.Create(Self)
			If a
				arts.addlast a
				things.addlast a
				allthings.addlast a
			EndIf
		Next
		'For c=1 To 3
		'	f:fella=fella.Create(Self)
		'	If f
		'		things.addlast f
		'	EndIf
		'Next
	End Method
	
	Function pathto:door(start:room,dest:room,rooms:TList)
		If start=dest Return Null
		Function distcmp(o1:Object,o2:Object)
			If room(o1).distance>room(o2).distance
				Return 1
			Else
				Return -1
			EndIf
		End Function
		
		For c:room=EachIn rooms
			c.distance=0
			c.pathprv=Null
		Next
		
		l:TList=New TList
		l.addlast start
		While l.count()
			l.sort True,distcmp
			c:room=room(l.removefirst())
			For c2:room=EachIn c.links
				If c2.distance>c.distance+1 Or c2.pathprv=Null
					l.addlast c2
					c2.pathprv=c
					c2.distance=c.distance+1
				EndIf
			Next
		Wend
		oc:room=dest
		c:room=dest.pathprv
		While oc<>start
			p:door=oc.doorto(c)
			'DrawLine (oc.l+oc.r)/2,(oc.u+oc.d)/2,p.x,p.y
			'DrawLine p.x,p.y,(c.l+c.r)/2,(c.u+c.d)/2
			oc=c
			c=c.pathprv
		Wend
		Return p
	End Function
		
	Method doorto:door(dest:room)
		For p:door=EachIn doors
			If p.c1=dest Or p.c2=dest
				Return p
			EndIf
		Next
	End Method
		
	
	Method update()
		'For t:thing=EachIn things
		'	t.update
		'Next
	End Method
	
	Method draw()
		SetLineWidth 1
		SetColor red,green,blue
		SetAlpha .4
		DrawRect l,u,r-l,d-u
		SetAlpha 1
		SetColor 255,255,255
		DrawLine l,u,l,d
		DrawLine l,u,r,u
		DrawLine l,d,r,d
		DrawLine r,u,r,d
		SetColor 255,0,0
	'	For c:room=EachIn links
	'		DrawLine (l+r)/2,(u+d)/2,(c.l+c.r)/2,(c.u+c.d)/2
	'	Next
		'For t:thing=EachIn things
		'	t.draw
		'Next
	End Method
End Type

Function makerooms:TList(u#,l#,r#,d#,depth=0)
	rooms:TList=New TList
	If Rnd(0,1)>2.0/(depth+1) Or Min(r-l,d-u)<80
		rooms.addlast room.Create(u,l,r,d)
		Return rooms
	EndIf

	If d-u>r-l
		middle#=u+(d-u)*Rnd(.3,.7)
		l1:TList=makerooms(u,l,r,middle,depth+1)
		l2:TList=makerooms(middle,l,r,d,depth+1)
		For c:room=EachIn l1
			For c2:room=EachIn l2
				If (Not (c.r<c2.l Or c.l>c2.r)) And c.d=c2.u
					minr#=Min(c.r,c2.r)
					maxl#=Max(c.l,c2.l)
					If minr-maxl>doorsize
						p:door=door.Create(c,c2,(minr+maxl)/2,middle,0)
						c.links.addlast c2
						c2.links.addlast c
						rooms.addlast p
						c.doors.addlast p
						c2.doors.addlast p
					EndIf
				EndIf
			Next
		Next
		
	Else
		middle#=l+(r-l)*Rnd(.3,.7)
		l1:TList=makerooms(u,l,middle,d,depth+1)
		l2:TList=makerooms(u,middle,r,d,depth+1)
		For c:room=EachIn l1
			For c2:room=EachIn l2
				If (Not (c.d<c2.u Or c.u>c2.d)) And c.r=c2.l
					mind#=Min(c.d,c2.d)
					maxu#=Max(c.u,c2.u)
					If mind-maxu>doorsize
						c.links.addlast c2
						c2.links.addlast c
						p:door=door.Create(c,c2,middle,(mind+maxu)/2,90)
						rooms.addlast p
						c.doors.addlast p
						c2.doors.addlast p
					EndIf
				EndIf
			Next
		Next
	EndIf
	For o:Object=EachIn l1
		rooms.addlast o
	Next
	For o:Object=EachIn l2
		rooms.addlast o
	Next
	If depth=0
		makeentrance rooms
	EndIf
	Return rooms
End Function

Function makeentrance(rooms:TList)
	l:TList=New TList
	For c:room=EachIn rooms
		If c.l=0 l.addlast(c)
	Next
	c=room(picklist(l))
	l.remove c
	y#=c.u+Rnd(.3,.7)*(c.d-c.u)
	entrance:door=door.Create(c,c,0,y,90)
	
	c=room(picklist(l))
	y#=c.u+Rnd(.3,.7)*(c.d-c.u)
	sortie:door=door.Create(c,c,0,y,90)
End Function

Type door
	Field x#,y#
	Field an#
	Field c1:room,c2:room
	Field busy
	
	Function Create:door(c1:room,c2:room,x#,y#,an#)
		p:door=New door
		p.c1=c1
		p.c2=c2
		p.x=x
		p.y=y
		p.an=an
		Return p
	End Function
	
	Method draw()
		'If busy
		'	SetColor 255,0,0
		'Else
			SetColor 0,255,0
		'EndIf
		SetAlpha 1
		SetLineWidth 4
		DrawLine x-Cos(an)*doorsize/2,y-Sin(an)*doorsize/2,x+Cos(an)*doorsize/2,y+Sin(an)*doorsize/2
		SetLineWidth 1
		'DrawLine x,y,(c1.l+c1.r)/2,(c1.u+c1.d)/2
		'DrawLine x,y,(c2.l+c2.r)/2,(c2.u+c2.d)/2
	End Method
End Type

Global allthings:TList=New TList
Type thing
	Field x#,y#,size#
	
	Method update() Abstract
	Method draw() Abstract
End Type

Type fella Extends thing
	Field c:room,oldc:room
	Field tp:door
	Field ta:art
	Field v#,vx#,vy#
	Field tvx#,tvy#
	Field tx#,ty#
	Field state
	Field seen:TList
	Field boredom#
	Field maxspeed#
	Field headan#
	
	Method New()
		seen=New TList
		size=5
		maxspeed=Rnd(.3,1)
	End Method
	
	Function Create:fella(c:room)
		f:fella=New fella
		f.c=c
		If f.position()
			Return f
		EndIf
	End Function
	
	Method position()
		valid=0
		steps=0
		While Not valid
			x=Rnd(c.l+10,c.r-10)
			y=Rnd(c.u+10,c.d-10)
			steps:+1
			valid=validateposition()
			If steps=10
				Print "?"
				Return 0
			EndIf
			'draw
			'Flip
		Wend
		Return 1
	End Method
	
	Method validateposition()
		For t:thing=EachIn c.things
			dx#=t.x-x
			dy#=t.y-y
			d#=Sqr(dx*dx+dy*dy)
			If d<size+t.size
				Return 0
			EndIf
		Next
		Return 1
	End Method
		
	Method update()
		think
		
		'boredom:*1.0001
		
		avoid
		steer
	End Method
	
	Method think()
		Select state
		Case 0	'nothing happening
			decide
		Case 1	'looking at art
			lookart
		Case 2	'moving to door
			movetodoor
		Case 3	'waiting for door to be free
			waitfordoor
		Case 4	'walking through door
			usedoor
		Case 5
			exitdoor
		End Select
	End Method
	
	Method decide()
		If Rand(20)>1 Return
		
		If boredom>10
			findexit
		ElseIf Rand(5)=1
			pickdoor
		Else
			pickart
		EndIf
	End Method
	
	Method findexit()
		If c=sortie.c1
			state=2
			tp=sortie
		Else
			tp=room.pathto(c,sortie.c1,allrooms)
			tx=tp.x
			ty=tp.y
			state=2
		EndIf
	End Method
		
	Method pickart()
		Function cmpart(o1:Object,o2:Object)
			a1:art=art(o1)
			a2:art=art(o2)
			If a1.viewers>a2.viewers Return 1 Else Return -1
		End Function
		
		l:TList=New TList
		'most=-1
		For a:art=EachIn c.arts
			If (Not seen.contains(a))' And (a.viewers>=most Or most=-1)
			'	If a.viewers>most
			'		most=a.viewers
			'		l=New TList
			'	EndIf
				l.addlast a
			EndIf
		Next
		l.sort True,cmpart
		If Not l.count()
			pickdoor
			Return
		EndIf
		
		ta:art=art(picklist(l,sqrrnd))
		ta.viewers:+1
		state=1
		picklookart
		
		Rem
		midx#=(c.l+c.r)/2
		midy#=(c.u+c.d)/2
		w#=c.r-c.l
		h#=c.d-c.u
		wander#=50
		tx=x+Rnd(-1,1)*wander
		ty=y+Rnd(-1,1)*wander
		If Abs(tx-midx)>Abs(ty-midy)
			If tx<midx tx=c.l Else tx=c.r
			If ty<c.u+20 ty=c.u+20
			If ty>c.d-20 ty=c.d-20
		Else
			If ty<midy ty=c.u Else ty=c.d
			If tx<c.l+20 tx=c.l+20
			If tx>c.r-20 tx=c.r-20
		EndIf
		state=1
		EndRem
	End Method
	
	Method picklookart()
		r#=Rnd(0,1)*ta.size
		an#=Rnd(360)
		tx=ta.x+r*Cos(an)
		ty=ta.y+r*Sin(an)
	End Method
	
	Method lookart()
		If moveto(ta.x,ta.y,ta.size+20)
			If Rand(100)=1
				picklookart
			EndIf
			v=0
			lookat tx-x,ty-y,.1
			
			boredom:-ta.interest
			If boredom<0 boredom=0
			If Rand(200)=1
				state=0
				seen.addlast ta
				ta.viewers:-1
				ta=Null
			EndIf
		EndIf
	End Method
	
	Method lookat(dx#,dy#,rate#)
		If v<.5
			rate:*(v+.5)
		EndIf
		dan#=ATan2(dy,dx)
		dan=andiff(dan,headan)
		headan:+dan*rate
		'DrawLine x,y,x+dx,y+dy
	End Method

	
	Method moveto(dx#,dy#,stopr#=20)
		
		dx:-x
		dy:-y
		d#=Sqr(dx*dx+dy*dy)

		
		If d>stopr
			boredom:+.003
			lookat dx,dy,.2
			If d<stopr+5
				tv#=((d-stopr)/5+.2)*maxspeed
				v:+(tv-v)*.1
			Else
				v:+.05
				If v>maxspeed v=maxspeed
			EndIf
			'x:+dx*v/d
			'y:+dy*v/d
			walk dx*5/d,dy*5/d
			
			

		Else
			Return 1
		EndIf
	End Method
	
	Method walk(dx#,dy#)
		vx:+dx
		vy:+dy
		tvx:+Abs(dx)
		tvy:+Abs(dy)
	End Method
	
	Method avoid()
		nvx#=0
		nvy#=0
		For t:thing=EachIn c.things
		If t<>Self
			dx#=t.x-x
			dy#=t.y-y
			d#=Sqr(dx*dx+dy*dy)
			maxd#=t.size+size+10
			If d<maxd
				'd=Sqr(d)
				ad#=(maxd-d)
				fx#=dx*ad/d+dy*ad*.1/d
				fy#=dy*ad/d+dx*ad*.1/d
				nvx:-fx
				nvy:-fy
				If fella(t)
					f:fella=fella(t)
					f.vx:+fx*.1
					f.vy:+fy*.1
				EndIf
				'DrawLine x,y,t.x,t.y
			EndIf
		EndIf
		Next
		If nvx*nvx+nvy*nvy>10 And v<maxspeed*.1
			v:+.01
			If v>maxspeed v=maxspeed
		EndIf
		walk nvx*.5,nvy*.5
		
		If Not (state>=3 And state<=5)
		If x-c.l<10 
			vx:+5
			'x=c.l+10
		EndIf
		If c.r-x<10
			vx:-5
			'x=c.r-10
		EndIf
		If y-c.u<10
			vy:+5
			'y=c.u+10
		EndIf
		If c.d-y<10
			vy:-5
			'y=c.d-10
		EndIf
		EndIf
	End Method
	
	Method steer()
		If Sqr(tvx*tvx+tvy*tvy)>10
			v:*.8
			vx:+Rnd(-1,1)*1
			vy:+Rnd(-1,1)*1
		EndIf
		
		nv#=Sqr(vx*vx+vy*vy)
		If nv>0
			vx:*v/nv
			vy:*v/nv
		EndIf
		x:+vx
		y:+vy

		If v>maxspeed*.1
			lookat vx,vy,.1
		EndIf
		
		vx=0
		vy=0
		tvx=0
		tvy=0
		If v>.1
			v:-.01
		EndIf
	End Method
	
	Method pickdoor()
		tp=door(picklist(c.doors))
		state=2
		tx=tp.x
		ty=tp.y
	End Method
	
	Method movetodoor()
		If moveto(tp.x,tp.y,20)
			state=3
		EndIf
	End Method
	
	Method waitfordoor()
		If tp.busy>3' And tp<>entrance
			Rem
			If tx=tp.x
				dp1#=(tp.x-x)*(-Sin(tp.an))+(tp.y-y)*Cos(tp.an)
				dp2#=(tp.x-x)*Cos(tp.an)+(tp.y-y)*Sin(tp.an)
				dan#=Rnd(60,80)
				an#=tp.an-Sgn(dp1)*90+dan*Sgn(dp2)
				tx=tp.x+Cos(an)*20
				ty=tp.y+Sin(an)*20
			EndIf
			moveto tx,ty,0
			EndRem
			boredom:+.01
			If Rand(100)=1
				pickdoor
			EndIf
		Else
			state=4
			tp.busy:+1
		EndIf
	End Method
	
	Method usedoor()
		If moveto(tp.x,tp.y,10)
			state=5
			oldc=c
			If c=tp.c1
				c=tp.c2
			Else
				c=tp.c1
			EndIf
			If tp<>sortie
				c.things.addlast Self
			Else
				sortie.busy:-1
				finishtrip
			EndIf
		EndIf
	End Method	
	
	Method exitdoor()
		moveto (c.l+c.r)/2,(c.u+c.d)/2,0
		dx#=tp.x-x
		dy#=tp.y-y
		If dx*dx+dy*dy>doorsize*doorsize/4
			state=0
			oldc.things.remove Self
			tp.busy:-1
		EndIf
	End Method		
	
	Method finishtrip()
		'Print seen.count()
		allthings.remove Self
		c.things.remove Self
	End Method
	
	
	Method draw()
		SetColor 0,0,255-boredom*10
		SetAlpha 1
		DrawOval x-3,y-3,6,6
		'DrawLine x,y,x+Cos(headan)*8,y+Sin(headan)*8
		
		x1#=x+Cos(headan)*8
		y1#=y+Sin(headan)*8
		x2#=x+Cos(headan+120)*3
		y2#=y+Sin(headan+120)*3
		x3#=x+Cos(headan-120)*3
		y3#=y+Sin(headan-120)*3
		Local poly#[]=[x1,y1,x2,y2,x3,y3]
		DrawPoly poly
		
		'Select state
		'Case 1
		'	DrawLine x,y,ta.x,ta.y
		'Case 3
		'	SetColor 255,255,0
		'	DrawOval x-3,y-3,6,6
		'End Select
		'
		'If tp=sortie
		'	SetColor 255,255,255
		'	DrawText state,x,y
		'EndIf
	End Method
End Type

Type child Extends fella
	Field mother:fella
	
	Method think()
		Select state
		Case 1
			If mother.c<>c Or ((mother.state<>1 Or mother.ta<>ta) And Rand(20)=1)
				state=0
				If ta
					ta.viewers:-1
					ta=Null
				EndIf
			EndIf
		End Select
		Super.think
	End Method
	
	Method decide()
		If Not allthings.contains(mother)
			findexit
		EndIf
		If mother.c=c
			If mother.state=1
				ta=mother.ta
				state=1
				ta.viewers:+1
				picklookart
			Else
				moveto mother.x,mother.y,30
			EndIf
			'If tp And state>2
			'	tp.busy:-1
			'	tp=Null
			'EndIf
		Else
			p:door=room.pathto(c,mother.c,allrooms)
			'If p<>tp
				state=2
				tp=p
				tx=tp.x
				ty=tp.y
			'	If tp And state>2
			'		tp.busy:-1
			'	EndIf
			'EndIf
			boredom=0
			Super.think
		EndIf
	End Method
	
	'Method draw()
	'	SetColor 0,255,0
	'	DrawLine x,y,mother.x,mother.y
	'	Super.draw
	'End Method
End Type

Type art Extends thing
	Field c:room
	Field viewers
	Field interest#
	
	Function Create:art(c:room)
		a:art=New art
		a.c=c
		a.size=Rnd(10,30)
		interest#=Rnd(0.01,0.05)
		If a.position()
			Return a
		EndIf
	End Function
	
	Method position()
		valid=0
		w#=(c.r-c.l)/2
		h#=(c.d-c.u)/2
		midx#=c.l+w
		midy#=c.u+h
		w:-size+30
		h:-size+30
		If w<0 Or h<0
			'Print "room too small"
			size:+Min(w,h)
			If size<10
				Return 0
			EndIf
		EndIf
		steps=0
		While Not valid
			valid=1
			steps:+1
			If steps=10 Return 0
			x=midx+Rnd(-1,1)*w
			y=midy+Rnd(-1,1)*h
			For a:art=EachIn c.arts
				dx#=a.x-x
				dy#=a.y-y
				d#=Sqr(dx*dx+dy*dy)-size-a.size
				If d<20
					valid=0
				EndIf
			Next
			'draw
			'Flip
		Wend
		Return 1
	End Method
	
	Method update()
	End Method
	
	Method draw()
		SetColor 255,255,255
		SetAlpha .2
		DrawOval x-size,y-size,size*2,size*2
		r#=size*(1-1.0/(viewers+1))
		DrawOval x-r,y-r,r*2,r*2
		'SetAlpha 1
		'DrawText viewers,x-4,y-6
	End Method
End Type

Function picklist:Object(l:TList,f!(min_value!,max_value!)=Rnd)
	n=f(0,1)*l.count()
	If n=l.count() n:-1
	Return l.valueatindex(n)
End Function

Function sqrrnd!(min_value!,max_value!)
	t#=Rnd(0,1)
	Return min_value+t*t*(max_value-min_value)
End Function

Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan<-180 dan:+360
	If dan>180 dan:-360
	Return dan
End Function

Const doorsize=30
Graphics 600,600,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

Global allrooms:TList=makerooms(0,0,600,600)
Global entrance:door,sortie:door

n=0
For a:art=EachIn allthings
	n:+1
Next
Print n+" arts"
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	If KeyHit(KEY_SPACE)
		allthings=New TList
		allrooms:TList=makerooms(0,0,600,600)
	EndIf
	If Rand(100)=1
		f:fella=fella.Create(entrance.c1)
		If f
			f.x=entrance.x+Rnd(3,10)
			f.y=entrance.y+Rnd(-1,1)*doorsize
			entrance.c1.things.addlast f
			allthings.addlast f

			If Rand(10)=1
				of:fella=f
				For i=1 To Rand(1,3)
					ch:child=New child
					ch.x=f.x+Rand(-20,20)
					ch.y=f.y+Rand(-20,20)
					ch.mother=f
					ch.maxspeed=Max(maxspeed,ch.mother.maxspeed)
					ch.c=f.c
					allthings.addlast ch
					entrance.c1.things.addlast ch
					of=ch
				Next
			EndIf
		EndIf
	EndIf
	entrance.busy=0

	For c:room=EachIn allrooms
		c.update
	Next
	For t:thing=EachIn allthings
		t.update
	Next
	
	For c:room=EachIn allrooms
		c.draw
	Next
	
	For t:thing=EachIn allthings
		t.draw
	Next
	
	For p:door=EachIn allrooms
		p.draw
	Next
	
	'room.pathto(entrance.c1,sortie.c1,allrooms)

	Flip
	Cls
Wend