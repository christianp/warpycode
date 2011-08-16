Global points:tlist=New tlist
Global links:tlist=New tlist
Global booms:tlist=New tlist

Const sproing#=.001,springlength#=50

Type point
	Field x#,y#
	Field vx#,vy#
	Field mylinks:tlist
	Field state
	Field lit,inpath,inside
	Field fade#
	
	Method New()
		mylinks=New tlist
		points.addlast Self
	EndMethod	

	Function create:point(x#,y#)
		p:point=New point
		p.x=x
		p.y=y
		Return p
	EndFunction
	
	Method isvisible(from:point)
		If from=Self Return 0
		dx#=from.x-x
		dy#=from.y-y
		
		For l:link=EachIn links
			If Not (l.a=Self Or l.a=from Or l.b=Self Or l.b=from)
				dx1#=l.b.x-l.a.x
				dy1#=l.b.y-l.a.y
				lambda#=(l.a.y-y+(dy1/dx1)*(x-l.a.x))/(dy-dy1*dx/dx1)
				If lambda>0 And lambda<1
					mu#=(y-l.a.y+(dy/dx)*(l.a.x-x))/(dy1-dy*dx1/dx)
					If mu>0 And mu<1
						visible=0
						Return 0
						'DrawText lambda,x+lambda*dx,y+lambda*dy
						'DrawLine l.a.x,l.a.y,x+lambda*dx,y+lambda*dy
					EndIf
				EndIf
			EndIf
		Next
		Rem
		If visible
			SetColor 255,0,0
			DrawLine x,y,from.x,from.y
		Else
			SetColor 0,0,255
		EndIf
		DrawRect x-2,y-2,4,4
		EndRem
		'WaitKey()
		Return 1
	EndMethod	
	
	Method filllinks(n)
		If n<mylinks.count() Return
		visibles:tlist=New tlist
		For p:point=EachIn points
			If p.isvisible(Self)
				visibles.addlast p
			EndIf
		Next
		'SetColor 255,255,255
		
		For p:point=EachIn visibles
			If p.mylinks.count()>5 And visibles.count()>2
				visibles.remove p
			EndIf
		Next
				
		While mylinks.count()<n And visibles.count()>0
			i=Rand(visibles.count())-1
			addlink(point(visibles.valueatindex(i)))
			visibles.remove(visibles.valueatindex(i))
		Wend
	EndMethod
	
	Method addlink(p:point)
		mylinks.addlast p
		p.mylinks.addlast Self
		link.create(Self,p)
	End Method
	
	Method update()
	
		Rem
		For p:point=EachIn mylinks
			dx#=p.x-x
			dy#=p.y-y
			d#=Sqr(dx*dx+dy*dy)
			If d<>springlength
				'f#=sproing*(d-springlength)
				f#=Sgn(d-springlength)*.01
				fx#=f*dx/d
				fy#=f*dy/d
				vx:+fx
				vy:+fy
			EndIf
		Next
		EndRem
		
		fade:+(lit-fade)*.1
		If fade<.1 fade=0
	End Method
	
	Method draw()
		If Self=path.first()
			SetColor 0,255,0
		ElseIf path.contains(Self)
			SetColor 255,0,0
		Else
			SetColor 0,0,255
		EndIf
		SetAlpha fade
		If Self=path.last()
			size=15
		Else
			size=5
		EndIf
		DrawOval x-size,y-size,size*2,size*2
	End Method
	
	Method joinpath()
		lit=1
		inpath=1
		If path.count()>0
			For l:link=EachIn links
				If (l.a=Self Or l.a=path.last()) And (l.b=Self Or l.b=path.last())
					l.inpath=1
				EndIf
			Next
		EndIf
		path.addlast Self
	End Method
	
	Method isinside()
		If inside<>0 Return inside
		
		For p:point=EachIn mylinks
			If p.isinside()=-1
				inside=-1
				Return -1
			EndIf
		Next
		Return 1
	EndMethod
	
	Method die()
		points.remove Self
		For p:point=EachIn mylinks
			p.mylinks.remove Self
		Next
		For l:link=EachIn links
			If l.a=Self Or l.b=Self
				links.remove l
			EndIf
		Next
		boom.create(x,y)
	EndMethod
EndType

Type link
	Field a:point,b:point
	Field inpath
	
	Method New()
		links.addlast Self
	EndMethod
	
	Function create:link(a:point,b:point)
		l:link=New link
		l.a=a
		l.b=b
		Return l
	EndFunction
	
	Method draw()
		If (path.contains(a) And path.contains(b)) Or a=path.last() Or b=path.last()
			If a.fade And b.fade
				If a.fade<b.fade
					fade#=a.fade
				Else
					fade#=b.fade
				EndIf
				SetAlpha fade
				If inpath
					SetColor 255,0,0
				Else
					SetColor 255,255,255
				EndIf
				DrawLine a.x,a.y,b.x,b.y
			EndIf
		EndIf
	EndMethod
EndType

Type boom
	Field x#,y#,life#,speed#

	Method New()
		life=1
		speed=Rnd(.01,.03)
		booms.addlast Self
	EndMethod
	
	Function create:boom(x#,y#)
		b:boom=New boom
		b.x=x
		b.y=y
		Return b
	EndFunction
	
	Method draw()
		SetColor 255,0,0
		size#=30.0/Sqr(life)
		SetAlpha life*.5
		DrawOval x-size,y-size,size*2,size*2
		life:-speed
		If life<=0
			booms.remove Self
		EndIf
	End Method
End Type

Function anglediff#(a#,b#)
	diff#=(b-a)
	While diff>180
		diff:-360
	Wend
	While diff<-180
		diff:+360
	Wend
	Return diff
EndFunction

Function dostring(s$)
	DrawText s,5,textpos*15+5
	textpos:+1
EndFunction

Function fact(n)
	If n=1 Return 1
	Return n*fact(n-1)
End Function

Global textpos=0

Global max_points=20

Graphics 800,800,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

dostring("OK, because nobody reads READMEs I'm taking drastic measures.")
dostring("In this game, you have to create a path along the network of points.")
dostring(" ")
dostring("You do this by left-clicking on the points connected to your current")
dostring("point (THE BIG RED ONE), to move to that point.")
dostring(" ")
dostring("BUT: You can't visit a point twice, apart from the one you started at.")
dostring("SO: If you get stuck, right-click to destroy your path and start again.")
dostring(" ")
dostring("The aim of the game is to get back to the point you started at (THE GREEN ONE).")
dostring("Doing this gets you points. You get more points for a longer path,")
dostring("And lots more points for enclosing other points within your path.")
dostring("")
dostring("Thank you.")
dostring("Now press A KEY to start the game.")
Flip
WaitKey()
Cls

Global pos:point=point.create(400,400)
Global path:tlist=New tlist
pos.joinpath()
For c=1 To max_points
	p:point=point.create(Rand(800),Rand(800))
	p.filllinks(Rand(3))
Next
pos.filllinks(3)

Global realscore,displayscore

While Not KeyHit(KEY_ESCAPE)

	mx=MouseX()
	my=MouseY()
	
	closestp:point=Null
	closestan#=0
	man#=ATan2(my-pos.y,mx-pos.x)
	
	For p:point=EachIn points
		p.draw()
		p.update()

		If ((Not path.contains(p)) Or (p=path.first() And p<>pos And path.count()>2)) And p.mylinks.contains(pos)
			an#=ATan2(p.y-pos.y,p.x-pos.x)
			diff#=Abs(anglediff(man,an))
			If (diff<closestan Or closestp=Null) And diff<120
				closestp=p
				closestan=diff
			EndIf
		EndIf
	Next
	
	If closestp
		SetAlpha .2
		DrawOval closestp.x-15,closestp.y-15,30,30
		SetAlpha 1
	EndIf
	
	For p:point=EachIn pos.mylinks
		p.lit=1
	Next
	
	For l:link=EachIn links
		l.draw()
	Next
	
	For b:boom=EachIn booms
		b.draw()
	Next
	
	If displayscore<>realscore
		'this is simulation of a poisson distribution. It's very clever. It makes the displayed score tick up more quickly when you score more points, but doesn't use floats (lies, all lies)
		rate#=Abs(realscore-displayscore)/50.0
		orate#=rate
		n=1
		i=0
		f#=0
		r#=Rnd()
		expo#=Exp(-rate)
		While f<r And i<10
			prob#=(rate/orate)*expo/n
			rate:*orate
			f:+prob
			i:+1
			n:*i
		Wend
		i:-1
		'Print i
		displayscore:+i*Sgn(realscore-displayscore)
	EndIf
	SetAlpha 1
	DrawText displayscore,0,0
	DrawText realscore,0,15
	
	If MouseHit(1)
		If closestp
			If closestp=path.first()
				insides:tlist=New tlist
				outs:tlist=New tlist
				'x#=0
				'y#=0
				score=path.count()*100
				For p:point=EachIn points
					If path.contains(p)
						inside=1
					Else
						sum#=0
						i=0
						p1:point=point(path.first())
						n=path.count()
						For i=1 To n
							p2:point=point(path.valueatindex(i Mod n))
							an1#=ATan2(p1.y-p.y,p1.x-p.x)
							an2#=ATan2(p2.y-p.y,p2.x-p.x)
							an#=anglediff(an1,an2)
							sum:+an
							p1=p2
						Next
						If Abs(sum)>180
							inside=1
						Else
							inside=0
						EndIf
					EndIf
					If inside
						'x:+p.x
						'y:+p.y
						score:+200
						insides.addlast p
					EndIf
				Next
				'Print score
				realscore:+score/10
				'x:/score
				'y:/score
				start:point=point(path.first())
				pos=point.create(start.x,start.y)
				path=New tlist
				pos.lit=1
				pos.inpath=1
				path=New tlist
				path.addlast pos
				For p:point=EachIn insides
					p.die()
					For po:point=EachIn p.mylinks
						po.mylinks.remove(p)
						If Not (outs.contains(po) Or insides.contains(po))
							outs.addlast po
						EndIf
					Next
				Next
				For p:point=EachIn outs
					pos.addlink(p)
				Next
				
				insides=Null
				outs=Null
			Else
				'For p:point=EachIn points
				'	If Not path.contains(p) p.lit=0
				'Next
				closestp.joinpath()
				pos=closestp
			EndIf
		EndIf
	EndIf
	
	If MouseHit(2)
		For p:point=EachIn path
			p.die()
		Next
		Print path.count()
		realscore:-path.count()*50
		pos=point(points.first())
		pos.filllinks(3)
		path=New tlist
		pos.joinpath()
		For p:point=EachIn points
			p.filllinks(Rand(3))
		Next
	EndIf
	
	If points.count()<max_points
		p:point=point.create(Rand(800),Rand(800))
		p.filllinks(Rand(3))
	EndIf
	
						
	Flip
	Cls
Wend