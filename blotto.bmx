Const shipradius#=50,shellradius#=25
Const fightingforce#=.1,healingforce#=.0000
Const chasedistance#=40000
Global teams:tlist=New tlist
Global allships:tlist=New tlist
Global allgroups:tlist=New tlist
Global deadgroups:tlist=New tlist

Type ship
	Field x#,y#
	Field vx#,vy#
	Field group:group
	Field team:team
	Field neighbours:tlist
	Field life#
	Field shield#
	
	Function create:ship(x#,y#,t:team)
		s:ship=New ship
		allships.addlast s
		s.x=x
		s.y=y
		s.team=t
		s.neighbours=New tlist
		s.life=1
		s.shield=1
		Return s
	End Function
	
	Method depthsearch(checked:tlist=Null)
		If checked=Null checked=New tlist
		n=1
		checked.addlast Self
		For s:ship=EachIn neighbours
			If Not checked.contains(s)
				n:+s.depthsearch(checked)
			EndIf
		Next
		Return n
	End Method
	
	Method joingroup(g:group)
		If group group.members.remove Self
		group=g
		If Not g Return
		group.members.addlast Self
		For s:ship=EachIn neighbours
			If s.group<>g s.joingroup(g)
		Next
	End Method
	
	Method unlink(s:ship)
		s.neighbours.remove Self
		neighbours.remove s
	End Method
	
	Method link(s:ship)
		s.neighbours.addlast Self
		neighbours.addlast s
	End Method
	
	Method domove()
		'movement			
		'vx:+Rnd(-1,1)*.1
		'vy:+Rnd(-1,1)*.1
		
		tx=group.targx
		ty=group.targy
		dx#=tx-x
		dy#=ty-y
		d#=Sqr(dx*dx+dy*dy)
		If d=0 d=1
		dx:/d
		dy:/d
		rad#=shellradius*(1-1.0/group.size)
		rad#=shellradius*Log(group.size-1)/2
		rad:*life
		force#=.1*Sgn(d-rad)
		vx:+dx*force
		vy:+dy*force
		
		If neighbours.count()
			ax#=0
			ay#=0
			For s:ship=EachIn neighbours
				dx#=x-s.x
				dy#=y-s.y
				d#=dx*dx+dy*dy
				force#=Sgn(d-10)
				ax:+dx*force
				ay:+dy*force
			Next
			d#=Sqr(ax*ax+ay*ay)
			ax:/d
			ay:/d
			vx:+ax*.01
			vy:+ay*.01
		EndIf
	
		vx:*.95
		vy:*.95
		
		x:+vx
		y:+vy
	End Method
		
	Method dofight()
		'fighting
		life:+group.size*healingforce
		For g:group=EachIn allgroups
			dx#=g.x-x
			dy#=g.y-y
			d#=dx*dx+dy*dy
			If d<g.radius*g.radius
				d=Sqr(d)
				If d<1 d=1
				force#=fightingforce*g.size/(d*shield)
				If g.team<>team
					life:-force
					g.team.juice:+force
				EndIf
			EndIf
		Next
		If life>1 life=1
		If life<0 die()
	End Method
			
	Method update()
		domove()	
		dofight()		
	End Method
	
	Method die()
		For s:ship=EachIn neighbours
			unlink(s)
		Next
		If group
			If group.size=1
				team.deletegroup(group)
			EndIf
		EndIf
		joingroup(Null)
		allships.remove Self
		team.ships.remove Self
	End Method
	
	Method draw()
		SetColor team.red,team.green,team.blue
		SetAlpha life
		DrawOval x-4,y-4,8,8
		
		'SetColor 150,150,150
		'For s:ship=EachIn neighbours
		'	DrawLine s.x,s.y,x,y
		'Next
	End Method
End Type

Type group
	Field members:tlist
	Field team:team
	Field x#,y#,size,radius#
	Field name$
	Field targx#,targy#
	Field targgroup:group,state
	Field dead
	
	Method New()
		members=New tlist
		allgroups.addlast Self
	End Method
	
	Method calcstats()
		size=members.count()
		x#=0
		y#=0
		For s:ship=EachIn members
			x:+s.x
			y:+s.y
		Next
		x:/size
		y:/size
		newradius=-1
		For s:ship=EachIn members
			dx#=s.x-x
			dy#=s.y-y
			d#=dx*dx+dy*dy
			If d>newradius Or newradius=-1 newradius=d
		Next
		newradius=Sqr(newradius)+3
		radius:+(newradius-radius)*.5
		
	End Method
	
	Method draw()
		If radius>1
			SetColor team.red,team.green,team.blue
			SetAlpha .2
			DrawOval x-radius,y-radius,2*radius,2*radius
			SetAlpha 1
		EndIf
		DrawText name+" ("+String(state)+","+String(size)+","+String(dead)+")",x,y
	End Method
	
	Method update()
		Select state
		Case 0
		'doing nothing - search for something to do
			groupdists:tlist=New tlist
			For g:group=EachIn allgroups
				If g<>Self And g.members.count()
					dx#=g.x-x
					dy#=g.y-y
					d#=dx*dx+dy*dy
					If d<chasedistance
						gd:groupdist=New groupdist
						gd.g=g
						gd.dist=d
						groupdists.addlast gd
					EndIf
				EndIf
			Next
			If Not groupdists.count() 
				state=2
			Else
				groupdists.sort()
				l:TLink=groupdists.firstlink()
				targgroup=Null
				While l
					g:group=groupdist(l.value()).g
					'DrawText g.name,x,y+30
					If (g.team<>team And g.size<size) Or (g.team=team And g.size>=size)
						targgroup=g
						state=1
						Exit
					EndIf
					l=l.nextlink()
				Wend
				If Not state state=2
			EndIf
		Case 1
		'moving towards something
			If targgroup.members.count()=0
				targgroup=Null
				state=0
			Else
				targx=targgroup.x
				targy=targgroup.y
				If targgroup.team<>team And targgroup.size>size
					targgroup=Null
					state=0
				EndIf
			EndIf
		Case 2
		'nothing happening, move to closest factory
			closest:flag=Null
			mindist#=-1
			For fl:flag=EachIn team.flags
				dx#=fl.x-x
				dy#=fl.y-y
				d#=dx*dx+dy*dy
				If d<mindist Or mindist=-1
					mindist=d
					closest=fl
				EndIf
			Next
			If closest
				targx=closest.x
				targy=closest.y
			Else
				targx=team.x
				targy=team.y
			EndIf
			state=0
		End Select
		'SetColor 255,255,255
		'SetLineWidth 2
		'DrawLine x,y,targx,targy
		'SetLineWidth 1
	End Method
End Type

Type groupdist
	Field g:group
	Field dist#
	
	Method compare(o:Object)
		og:groupdist=groupdist(o)
		If dist<og.dist
			Return -1
		ElseIf og.dist<dist
			Return 1
		Else
			Return 0
		EndIf
	End Method
End Type

Type factory Extends ship

	Function create:factory(x#,y#,t:team)
		f:factory=New factory
		allships.addlast f
		f.x=x
		f.y=y
		f.team=t
		f.neighbours=New tlist
		f.life=1
		f.shield=20
		Return f
	End Function
	
	Method update()
		
		If Rand(100)=1
			an=Rand(360)
			r#=Rnd(50,100)
			team.newship(x+Cos(an)*r,y+Sin(an)*r)
		EndIf
	End Method
	
	Method draw()
		SetColor team.red,team.green,team.blue
		SetAlpha .5
		DrawOval x-15,y-15,30,30
		drawspiral(x,y,15,team.juice)
		SetAlpha 1
	End Method

	Method die()
		super.die()
		team.factories.remove Self
	End Method
End Type

Type flag Extends ship
	Field an#
	
	Function create:flag(x#,y#,t:team)
		fl:flag=New flag
		fl.x=x
		fl.y=y
		fl.team=t
		fl.neighbours=New tlist
		Return fl
	End Function
	
	Method update()
		an:+4.7
	End Method
	
	Method draw()
		rot#=Cos(an)*30
		SetColor team.red,team.green,team.blue
		DrawLine x,y,x+Sin(rot+180)*20,y+Cos(rot+180)*20
	End Method
	
	Method die()
		super.die()
		team.flags.remove Self
	End Method
End Type			

Function drawspiral(x#,y#,maxwidth#,limit#)
	an#=0
	x3#=x
	y3#=y
	x4#=x
	y4#=y
	c#=0
	For c=1 To limit
		sc#=Sqr(c)
		lc#=Log(c)
		an#=130*sc
		sx=Cos(an)*lc*7+x
		sy=Sin(an)*lc*7+y
		width#=maxwidth/sc
		x1#=x3
		y1#=y3
		x2#=x4
		y2#=y4
		x3#=sx+Cos(an)*width
		y3#=sy+Sin(an)*width
		x4#=sx+Cos(an+180)*width
		y4#=sy+Sin(an+180)*width
		'DrawLine x1,y1,x2,y2
		'DrawLine x1,y1,x3,y3
		'DrawLine x2,y2,x4,y4
		'DrawLine x3,y3,x4,y4
		Local points#[]=[x1,y1,x2,y2,x4,y4,x3,y3]
		DrawPoly points
	Next
	
	c#=limit
	sc#=Sqr(c)
	lc#=Log(c)
	an#=130*sc
	sx=Cos(an)*lc*7+x
	sy=Sin(an)*lc*7+y
	width#=maxwidth/sc
	If width>10 width=10
	x1#=x3
	y1#=y3
	x2#=x4
	y2#=y4
	x3#=sx+Cos(an)*width
	y3#=sy+Sin(an)*width
	x4#=sx+Cos(an+180)*width
	y4#=sy+Sin(an+180)*width
	'DrawLine x1,y1,x2,y2
	'DrawLine x1,y1,x3,y3
	'DrawLine x2,y2,x4,y4
	'DrawLine x3,y3,x4,y4
	points=[x1,y1,x2,y2,x4,y4,x3,y3]
	DrawPoly points
End Function


Type team
	Field name$
	Field ships:tlist,groups:tlist,factories:tlist,flags:tlist
	Field red,green,blue
	Field x#,y#
	Field juice#
	Field charge#
	
	Field groupticker

	Function create:team(name$,red,green,blue)
		t:team=New team
		teams.addlast t
		t.name=name
		t.red=red
		t.green=green
		t.blue=blue
		t.ships=New tlist
		t.groups=New tlist
		t.factories=New tlist
		t.flags=New tlist
		Return t
	End Function

	Method makegroups()
		If Not ships.count() Return
		'DebugStop
		
		ungrouped:tlist=New tlist
		For s:ship=EachIn ships
			For os:ship=EachIn ships
				If os<>s
					dx#=os.x-s.x
					dy#=os.y-s.y
					d#=dx*dx+dy*dy
						
					If d<shipradius*shipradius*.9
					'DebugStop
					'these two are close enough to link
					
						If Not s.neighbours.contains(os)
						'make sure they're linked
							s.link(os)
						EndIf
						
						If os.group
							If os.group<>s.group
							'if other ship is in a group which is not mine, join theirs
								s.joingroup(os.group)
							EndIf
						Else
						'if other ship is not in a group
							If s.group
							'if I'm in a group, make it join my group
								os.joingroup(s.group)
							'Else
							'if I'm not in a group, create a new one and add both ships to it
							'	g:group=newgroup()
							'	s.joingroup(g)
							EndIf
						EndIf
						
					Else
					'these two aren't close enough to link, so make sure they're not linked
						If s.neighbours.contains(os)
							s.unlink(os)
						EndIf
					EndIf
				EndIf				
			Next
			
			If Not s.group
			'if this ship isn't in a group (ie it's still all on its own) create a new one and join it
				g:group=newgroup()
				s.joingroup(g)
			EndIf		
		Next
		
		For g:group=EachIn groups
		'Clean up groups
			If g.members.count()
			'Find all the splits in the group
				firstship:ship=Null
				oldships:tlist=g.members.copy()
				While g.members.first()<>firstship
					firstship=ship(g.members.first())
					n1=g.members.count()
					n2=firstship.depthsearch()
					If firstship.depthsearch()<>g.members.count()
					'If this ship's neighbours-group isn't the same size as the group, this group has been split
						ng:group=newgroup()
						firstship.joingroup(ng)
						If Not g.members.count()
							deletegroup(g)
							Exit
						EndIf
						firstship=ship(g.members.first())
					EndIf
				Wend
				For s:ship=EachIn oldships
					If Not groups.contains(s.group)
						DebugStop
					EndIf
				Next
			Else
			'empty group, delete it
				deletegroup(g)
			EndIf
		Next
		
		For g:group=EachIn groups
			g.calcstats()
		Next
		
	End Method
	
	Method newflag:flag(x#,y#)
		fl:flag=flag.create(x,y,Self)
		'ships.addlast fl
		flags.addlast fl
		Return fl
	End Method
	
	Method newfactory:factory(x#,y#)
		f:factory=factory.create(x,y,Self)
		'ships.addlast f
		factories.addlast f
		Return f
	End Method
	
	Method newship:ship(x#,y#)
		s:ship=ship.create(x,y,Self)
		ships.addlast s
		s.joingroup(newgroup())
		Return s
	End Method
	
	Method newgroup:group()
		g:group=New group
		groups.addlast g
		'allgroups.addlast g
		g.team=Self
		groupticker:+1
		g.name=name+String(groupticker)
		Return g
	End Method
	
	Method deletegroup(g:group)
		g.dead=1
		groups.remove g
		allgroups.remove g
		deadgroups.addlast g
	End Method
	
	Method update()
		juice:+.01
		If Self=teama 
			SetColor red,green,blue
			SetAlpha 1
			DrawText juice,0,15
		EndIf
		
		'If Rand(500)
		'	ships.addlast ship.create(Rand(800),Rand(800),Self)
		'EndIf
		For g:group=EachIn groups
			g.update()
			g.draw()
		Next
		
		For s:ship=EachIn ships
			s.update()
			s.draw()
		Next
		
		For f:factory=EachIn factories
			f.update()
			f.draw()
		Next
		
		For fl:flag=EachIn flags
			fl.update()
			fl.draw()
		Next
	End Method
End Type

Graphics 800,800,0
SetBlend ALPHABLEND

Global teama:team=team.create("A",255,0,0)
teamb:team=team.create("B",0,0,255)
teama.newfactory(100,100)
teama.x=100
teama.y=100

teamb.newfactory(700,700)
teamb.x=700
teamb.y=700


teama.juice=10

While Not KeyHit(KEY_ESCAPE)
	mx=MouseX()
	my=MouseY()
	
	If MouseDown(1)
		If teama.juice>.1
			teama.charge:+.1
			teama.juice:-.1
		EndIf
		SetColor teama.red,teama.green,teama.blue
		SetAlpha .5
		drawspiral(mx,my,15,teama.charge)
		SetAlpha 1
	Else
		If teama.charge>10
			teama.juice:+teama.charge-10
			teama.charge=0
			teama.newfactory(mx,my)
		ElseIf teama.charge>1
			p#=Rnd(0,1)
			n#=0
			c=0
			Print teama.charge
			Print p
			While n<p
				gn=Abs(c-teama.charge)
				prob#=.7*(.3^gn)
				n:+prob
				Print n
				c:+1
			Wend
			Print c
			For i=1 To c
				an=Rand(360)
				v#=Rnd(1,3)
				s:ship=teama.newship(mx+Cos(an)*v*10,my+Sin(an)*v*10)
				s.vx=Cos(an)*v
				s.vy=Sin(an)*v
				Print "!"
			Next
			teama.charge=0
		Else
			teama.juice:+teama.charge
			teama.charge=0
		EndIf
	EndIf

	If MouseHit(2)
		deleted=0
		For fl:flag=EachIn teama.flags
			dx#=fl.x-mx
			dy#=fl.y-my
			d=dx*dx+dy*dy
			If d<400
				fl.die()
				deleted:+1
			EndIf
		Next
			
		If Not deleted	
			teama.newflag(mx,my)
		EndIf
	EndIf
	
	'teama.x=mx
	'teama.y=my

	For t:team=EachIn teams
		t.makegroups()
	Next

	For t:team=EachIn teams
		t.update()
	Next
	
	DrawText teama.groups.count(),0,0
	
	Flip
	Cls
Wend