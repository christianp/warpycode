Type cog
	Field notches,pattern[],opattern[]
	Field radius#
	Field x#,y#
	Field angle#,anstep#,ticks
	Field links:tlist,parent:cog
	Field depth,upnotch
	Field shade#
	Field shooters:tlist
	Field health#
	Field inplay
	
	Function create:cog(pattern[],notches,radius#,x#,y#)
		c:cog=New cog
		c.notches=notches
		c.radius=radius
		c.x=x
		c.y=y
		c.anstep=360.0/notches
		c.links=New tlist
		c.shade=.4
		c.shooters=New tlist
		c.health=10
		i=0
		c.pattern=New Int[notches]
		c.opattern=New Int[notches]
		For n=0 To notches-1
			c.pattern[n]=pattern[i]
			c.opattern[n]=c.pattern[i]
			i=(i+1) Mod Len(pattern)
		Next
		cogs.addlast c
		Return c
	End Function
	
	Method link(c:cog)
		dx#=x-c.x
		dy#=y-c.y
		an#=ATan2(dy,dx)
		oan#=an
		If an<0 an=360+an
		upnotch=(Int(an/c.anstep)+1) Mod c.notches
		an=(c.anstep+1)*upnotch
		d#=Sqr(dx*dx+dy*dy)
		r#=radius+c.radius
		x=c.x+r*Cos(oan)
		y=c.y+r*Sin(oan)
		c.links.addlast Self
		depth=c.depth+1
		shade=.8-.4^depth
		parent=c
		inplay=1
	End Method
	
	Method draw()
		SetAlpha shade*.8
		SetColor 255,25.5*health,25.5*health
		DrawOval x-radius+4,y-radius+4,radius*2-8,radius*2-8
		SetAlpha 1
		SetColor 255,255,255
		For i=0 To notches-1
			an#=(i+ticks)*anstep+angle
			c#=Cos(an)
			s#=Sin(an)
			sx#=x+c*(radius-4)
			sy#=y+s*(radius-4)
			If pattern[i]
				ex#=x+c*(radius+4)
				ey#=y+s*(radius+4)
			Else
				ex#=x+c*(radius-2)
				ey#=y+s*(radius-2)
			EndIf
			DrawLine sx,sy,ex,ey
		Next
		For sh:shooter=EachIn shooters
			sh.draw()
		Next
	End Method
	
	Method turn(an#)
		health:+Abs(an)*.005
		If health>10 
			health=10
			If Rand(1000)
				i=Rand(0,notches-1)
				pattern[i]=opattern[i]
			EndIf
		EndIf
		angle:+an
		While Abs(angle)>anstep
			dir=Sgn(angle)
			tick(dir)
			angle:-anstep*dir
		Wend
		For c:cog=EachIn links
			notch=(c.upnotch + ticks) Mod notches
			If notch<0 Then notch=notches+notch
			If pattern[notch]
				c.turn(-an*radius/c.radius)
			EndIf
		Next
	End Method
	
	Method tick(dir)
		ticks:+dir
		For s:shooter=EachIn shooters
			s.shoot()
		Next
		
		i=Rand(0,3)
		notch=ticks Mod notches
		If notch<0 notch:+notches
		If pattern[notch]
			channel=CueSound(ticknoises[i])
		Else
			channel=CueSound(tocknoises[i])
		EndIf
		SetChannelRate channel,50/radius
		SetChannelVolume channel,1.0/notches
		ResumeChannel channel
	End Method
	
	Method place()
		l:cog=Null
		For c:cog=EachIn cogs
			If c<>Self
				dx#=c.x-x
				dy#=c.y-y
				d#=dx*dx+dy*dy
				tr=c.radius+radius
				If d<tr*tr+100
					If l<>Null
						Return 0
					Else
						l=c
					EndIf
				EndIf
			EndIf
		Next
		If l
			link(l)
			Return 1
		Else
			Return 0
		EndIf
	End Method
	
	Method hit(s:shot)
		dx#=s.x-x
		dy#=s.y-y
		an#=ATan2(dy,dx)
		If an<0 an=360+an
		upnotch=(Int(an/anstep)+1-ticks) Mod notches
		If upnotch<0 Then upnotch:+notches
		If pattern[upnotch]
			pattern[upnotch]=0
		Else
			health:-1
			If health<=0 die()
		EndIf
	End Method
	
	Method die()
		If parent
			parent.links.remove Self
			cogs.remove Self
			For c:cog=EachIn links
				c.die()
			Next
		EndIf
		explode(x,y,Rand(80,100))
	End Method
End Type

Type shooter
	Field tick
	Field shotpattern[],anglepattern[]
	Field mycog:cog
	
	Function create:shooter(c:cog,shotpattern[],anglepattern[])
		s:shooter=New shooter
		s.shotpattern=shotpattern
		s.anglepattern=anglepattern
		c.shooters.addlast s
		s.mycog=c
		Return s
	End Function
	
	Method shoot()
		tick:+1
		shotn=tick Mod Len(shotpattern)
		angle=tick Mod Len(anglepattern)
		If shotpattern[shotn]
			shot.create(mycog.x,mycog.y,anglepattern[angle],1)
		EndIf
	End Method
	
	Method draw()
		SetColor 255,0,0
		For i=0 To Len(anglepattern)-1
			DrawLine mycog.x,mycog.y,mycog.x+Cos(anglepattern[i])*mycog.radius,mycog.y+Sin(anglepattern[i])*mycog.radius
		Next
		SetColor 255,255,255
	End Method
EndType

Type shot
	Field x#,y#,vx#,vy#
	Field side
	
	Function create:shot(x#,y#,angle#,side)
		b:shot=New shot
		b.x=x
		b.y=y
		b.vx=Cos(angle)*10
		b.vy=Sin(angle)*10
		b.side=side
		shots.addlast b
		Return b
	End Function
	
	Method update(delta#)
		ox#=x
		oy#=y
		x:+vx*delta
		y:+vy*delta
		If x<0 Or x>800 Or y<0 Or y>800
			shots.remove Self
		EndIf
		
		If side=1 'Cogs
			hitship:ship=Null
			For s:ship=EachIn ships
				If oy<s.y-10 And y>s.y-10
					If (ox<s.x-10 And x>s.x-10) Or (ox>s.x+10 And x<s.x+10)
						hitship=s
					EndIf
				EndIf
				If x>s.x-10 And x<s.x+10 And y>s.y-10 And y<s.y+10
					hitship=s
				EndIf
			Next
			If hitship
				hitship.hit()
				shots.remove Self
			EndIf
		Else 'Ships
			hitcog:cog=Null
			mindist=-1
			For c:cog=EachIn cogs
				If c.inplay
					dx#=c.x-x
					dy#=c.y-y
					d#=dx*dx+dy*dy
					If d<c.radius*c.radius And (d<mindist Or mindist=-1)
						hitcog=c
						mindist=d
					EndIf
				EndIf
			Next
			If hitcog
				shots.remove Self
				hitcog.hit(Self)
			EndIf
		EndIf
	End Method
	
	Method draw()
		DrawRect x-2,y-2,4,4
	End Method
End Type

Type ship
	Field x#,y#
	Field vx#
	Field rate
	Field tx#,dir
	Field speed#
	Field angle#
	
	Function create:ship(x#,y#)
		g:ship=New ship
		g.x=x
		g.y=y
		g.rate=Rand(10,50)
		g.speed#=Rnd(.1,1)
		ships.addlast g
		g.picktarget()
		Return g
	End Function
	
	Method picktarget()
		d=0
		For c=0 To 3
			d:+Rnd(.1,3)*100*speed
		Next
		minx=x-d
		maxx=x+d
		If minx<0 minx=0
		If maxx>800 maxx=800
		tx=Rnd(minx,maxx)
		dir=Sgn(tx-x)
	End Method
	
	Method draw()
		l#=Abs(vx)*5
		x1#=x+l*Cos(-angle+135)
		y1#=y+5+l*Sin(-angle+135)
		x2#=x+l*Cos(-angle+45)
		y2#=y+5+l*Sin(-angle+45)
		DrawLine x,y+5,x1,y1
		DrawLine x,y+5,x2,y2
		DrawLine x1,y1,x2,y2
		Local poly#[]=[x,y+5,x1,y1,x2,y2]
		a#=Abs(vx)/5+.5
		If a>1 a=1
		SetColor 255,200*(a+Rnd(-.2,.2)),0
		SetAlpha a
		DrawPoly poly
		SetAlpha .5
		SetColor 255,255,255
		'DrawLine x2,y2,x3,y3
		'DrawLine x3,y3,x1,y1
		poly=[x-10,y+10,x+10,y+10,x,y-10]
		DrawPoly poly
		SetAlpha 1
		DrawLine x-10,y+10,x+10,y+10
		DrawLine x-10,y+10,x,y-10
		DrawLine x+10,y+10,x,y-10
	End Method
	
	Method update(delta#)
		vx:+dir*.5*delta*speed
		vx:*.95
		x:+vx*delta
		If Sgn(tx-x)<>dir Then picktarget()
		
		angle#=-vx*5
		
		If y>780
			y:-.5
		Else
			If Rand(rate)=1
				shot.create(x,y,-90-angle,0)
			EndIf
		EndIf
	End Method

	Method hit()
		ships.remove Self
		explode(x,y,Rand(10,30))
	End Method
End Type

Type spark
	Field x#,y#,vx#,vy#,life#
	
	Function create:spark(x#,y#,vx#,vy#,life)
		e:spark=New spark
		e.x=x
		e.y=y
		e.vx=vx
		e.vy=vy
		e.life=life
		sparks.addlast e
		Return e
	End Function
	
	Method update(delta#)
		slow#=(1-.1*delta)
		vx:*slow
		vy:*slow
		x:+vx*delta
		y:+vy*delta
		life:-1
		If life=0
			sparks.remove Self
		EndIf
	End Method
	
	Method draw()
		SetColor 25*Rand(5,10),25*Rand(5,10),0
		DrawRect x+Rand(-2,2),y+Rand(-2,2),1,1
		SetColor 255,255,255
	End Method
End Type

Function explode(x#,y#,oomf)
	For c=1 To oomf
		an#=Rnd(360)
		v#=Rnd(.8,1)*oomf*.1
		spark.create(x,y,Cos(an)*v,Sin(an)*v,oomf)
	Next
End Function

Global cogs:tlist=New tlist
Global shots:tlist=New tlist
Global ships:tlist=New tlist
Global sparks:tlist=New tlist

Global ticknoises:TSound[4]
Global tocknoises:TSound[4]
For i=1 To 4
	ticknoises[i-1]=LoadSound("tick"+String(i)+".wav")
	tocknoises[i-1]=LoadSound("tock"+String(i)+".wav")
Next

Graphics 800,800,0
SetBlend ALPHABLEND

Global maincog:cog=cog.create([1],16,50,400,100)
maincog.inplay=1
'c:cog=cog.create([1,1,1,0,1,1,0,0],16,25,500,400)
'c.link(maincog)
'c2:cog=cog.create([1,0,1,1,1,0],24,75,600,400)
'c2.link(c)
'c3:cog=cog.create([1],6,25,550,450)
'c3.link(c2)
's:shooter=shooter.create(c3,[1],[80,100])
'c3:cog=cog.create([1],18,25,650,450)
'c3.link(c2)
's:shooter=shooter.create(c3,[1,1,0],[80,100])
'c:cog=cog.create([1,0,1,1],24,100,300,400)
'c.link(maincog)
'c2:cog=cog.create([1],24,25,300,450)
'c2.link(c)
's:shooter=shooter.create(c2,[1],[90,75,90,115])

ship.create(400,810)

placecog:cog=Null
placecogr#=0

ms=MilliSecs()
lasttick=ms

Global shiprate#=100

While Not KeyHit(KEY_ESCAPE)
	DrawText "Left click to create and place cogs! Right click to reject a cog",0,0

	mx=MouseX()
	my=MouseY()
	
	oldms=ms	
	ms=MilliSecs()
	delta#=(ms-oldms)/20.0
	
	maincog.turn(delta)'*.5)
	
	For c:cog=EachIn cogs
		c.draw()
	Next
	
	For g:ship=EachIn ships
		g.update(delta)
		g.draw()
	Next
	
	For b:shot=EachIn shots
		b.update(delta)
		b.draw()
	Next
	
	For e:spark=EachIn sparks
		e.update(delta)
		e.draw()
	Next
	
	'If Len(ships)=0 Then shiprate:*.8
	
	If Rand(shiprate)=1
		ship.create(Rand(800),810)
	EndIf
	shiprate:*.9999
	
	If placecog
		placecog.x=mx
		placecog.y=my
		placecog.radius:*1.05
		If placecog.radius>placecogr# placecog.radius=placecogr
		SetAlpha .5
		ox#=mx+placecogr
		oy#=my
		For an=0 To 360 Step 15
			x#=mx+Cos(an)*(placecogr-4)
			y#=my+Sin(an)*(placecogr-4)
			DrawLine ox,oy,x,y
			ox=x
			oy=y
		Next
		SetAlpha 1
	Else
		plen=Rand(1,8)
		Local pattern[]=New Int[plen]
		pattern[0]=1
		For i=1 To plen-1
			pattern[i]=Rand(0,1)
		Next
		target=Rand(plen,48)
		If target=1 target=2
		notches=plen
		While notches<target
			d=target/notches
			If d=1 Then d=2
			notches:*Rnd(2,d)
		Wend
		placecogr#=Rand(3,15)*5
		placecog=cog.create(pattern,notches,5,Rnd(800),Rnd(800))
		If Rand(3)=1
			splen=Rand(1,5)
			aplen=Rand(1,5)
			Local spattern[]=New Int[splen]
			Local apattern[]=New Int[aplen]
			spattern[0]=1
			For i=1 To splen-1
				spattern[i]=Rand(0,1)
			Next
			apattern[0]=90
			For i=1 To aplen-1
				an=90
				For n=0 To 4
					an:+Rnd(-3,3)*5
				Next
				apattern[i]=an
			Next
			s:shooter=shooter.create(placecog,spattern,apattern)
		EndIf
	EndIf
	
	If MouseHit(1)
		If placecog
			If placecog.radius=placecogr
				If placecog.place() Then placecog=Null
			EndIf
		EndIf
	EndIf
	If MouseHit(2)
		If placecog
			cogs.remove placecog
			placecog=Null
		EndIf
	EndIf
	Flip
	Cls
Wend