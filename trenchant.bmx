SetGraphicsDriver GLMax2DDriver()

Global trenches:TList = New TList
Global areas:TList = New TList
Global soldiers:TList = New TList
Global shots:TList=New TList
Global artilleries:TList=New TList
Global shells:TList=New TList
Global messages:TList=New TList
Global tanks:TList=New TList
Global icecreamvans:TList=New TList
Global noises:TList=New TList
Global mrwhippee:icecreamvan

Global mx,my

Global deathtoll[2],roundsfired[2],shellsdropped[2]
Global areasowned[2]
Global earnedcash[2],displaycash[2]

Incbin "rifle1.wav"
Incbin "explosion1.wav"
Incbin "mrsoftee.wav"
Incbin "hand8.wav"
Global riflenoise:TSound=LoadSound("incbin::rifle1.wav")
Global explosionnoise:TSound=LoadSound("incbin::explosion1.wav")
Global icecreamnoise:TSound=LoadSound("incbin::mrsoftee.wav")
Global handbellnoise:TSound=LoadSound("incbin::hand8.wav")

Global names$[]=["JENKINS","SMITH","JONES","WATSON","JOOLS","CARRUTHERS","SMYTHE"]
Global titles$[]=["PRIVATE","CORPORAL","SERGEANT"]

Global scroll#
Global kerchingspeed#[2]

Type trench
	Field points:TList
	Field l:trench,r:trench
	
	Method New()
		points = New TList
		trenches.addlast Self
	End Method
	
	Function Create:trench(x#,l:trench) 
		t:trench = New trench
		If l <> Null
			l.r = t
			a:area=area.Create(l , t)
			a.owner=Rand(0,1)
		EndIf
		t.l = l
		t.points.addlast(point.Create(x,0))
		ox# = x
		numpoints=gheight/40
		For c=0 To numpoints
			'y:+ Rnd(10 , 50)
			y=c*40+Rnd(-10,10)
			x:+ Rnd( - 15 , 15)+(ox-x)*.4
			If l <> Null
				minx = l.getx(y)+20
			Else
				minx = 10
			EndIf
			If x < minx x = minx
			t.points.addlast(point.Create(x , y) )
		Next
		p:point = point(t.points.last() ) 
		p.y = gheight
		Return t
	End Function
	
	Method getx#(y#)
		If y>gheight Or y<0 Return 0
		link:TLink = points.firstlink() 
		p:point=point(link.value())
		While p.y < y
			ox = p.x
			oy = p.y
			link = link.nextlink()
			p=point(link.value())
		Wend
		m# = (p.x - ox) / (p.y - oy)
		x# = ox + (y - oy) * m
		Return x
	End Method
	
	Method draw()
		link:TLink = points.firstlink()
		p:point = point(link.value() )
		While link <> Null
			ox# = p.x
			oy# = p.y
			link = link.nextlink()
			If link <> Null
				p = point(link.value() )
				SetLineWidth 20
				SetBlend SHADEBLEND
				SetColor 150,150,150
				SetAlpha .5
				DrawLine ox-10,oy,p.x-10,p.y
				DrawLine ox+10,oy,p.x+10,p.y
				SetBlend ALPHABLEND
				SetColor 255,255,255
				SetLineWidth 22
				SetAlpha .3
				DrawLine ox,oy,p.x,p.y
				SetAlpha 1
				SetLineWidth 2
				y# = oy
				x# = ox
				m#=(p.x-ox)/(p.y-oy)
				col=1
				While y < p.y
					y:+ 4
					If y > p.y
						y = p.y
						diff# = p.y - oy
					Else
						diff# = 4
					EndIf
					x#:+ m * diff
					If col
						DrawLine ox , oy , x , y
					EndIf
					col = 1 - col
					ox = x
					oy = y
				Wend
			EndIf
		Wend
	End Method
			
End Type

Type point
	Field x# , y#
	
	Function Create:point(x# , y#)
		p:point = New point
		p.x = x
		p.y = y
		Return p
	End Function
End Type

Type area
	Field l:trench , r:trench
	Field la:area , ra:area
	Field owner
	Field soldiers:TList[2]
	Field segments[16,2]
	Field control#[16 , 2]
	Field numsegments
	
	Method New()
		areas.addlast Self
		soldiers[0] = New TList
		soldiers[1] = New TList
	End Method
	
	Function Create:area(l:trench , r:trench)
		a:area = New area
		a.l = l
		a.r = r
		a.owner = - 1
		Return a
	End Function
	
	Method update()
		For c=0 To 15
			segments[c,0]=0
			segments[c,1]=0
		Next
		For s:soldier=EachIn soldiers[0]
			If tank(s) n=3 Else n=1
			segments[Int(s.y/50),0]:+n
		Next
		For s:soldier=EachIn soldiers[1]
			If tank(s) n=3 Else n=1
			segments[Int(s.y/50),1]:+n
		Next
		
		If soldiers[0].count()
			If soldiers[1].count()
				owner=-1
			Else
				owner=0
				areasowned[0]:+1
			EndIf
		ElseIf soldiers[1].count()
			owner=1
			areasowned[1]:+1
		Else
			owner=-1
		EndIf

		For c=0 To 15
			aseg=segments[c,0]
			bseg=segments[c,1]
			controller#=owner
			If aseg
				If bseg
					controller=bseg/(aseg+bseg)
				Else
					controller=0
				EndIf
			ElseIf bseg
				controller=1
			ElseIf owner=-1
				control[c,1]:-.1
				If control[c,1]<0 control[c,1]=0
			EndIf
			
			If aseg Or bseg Or owner>-1
				control[c,1]:+.1
				If control[c,1]>1 control[c,1]=1
			EndIf
			control[c,0]:+(controller-control[c,0])*.1
			If control[c,0]<0 control[c,0]=0
			If control[c,0]>1 control[c,0]=1
			
		Next

	
	End Method
	
	Method draw()
		llink:TLink = l.points.firstlink()
		rlink:TLink = r.points.firstlink()
		p1:point = point(llink.value() )
		p2:point = point(rlink.value() )
		SetAlpha .3
		sidecolors(owner)
		While llink <> Null Or  rlink <> Null
			If llink llink = llink.nextlink()
			If rlink rlink = rlink.nextlink()
			If llink p3:point = point(llink.value() ) Else p3=Null
			If rlink p4:point = point(rlink.value() ) Else p4=Null
			Local poly#[]
			If p3
				If p4
					poly# = [p1.x , p1.y , p2.x , p2.y , p4.x , p4.y , p3.x , p3.y]
				Else
					poly# = [p1.x , p1.y , p2.x , p2.y , p3.x , p3.y]
				EndIf
			Else
				If p4
					poly# = [p1.x , p1.y , p2.x , p2.y , p4.x , p4.y]
				EndIf
			EndIf
			If p3 And p4
				c=Int((p1.y+p2.y+p3.y+p4.y)/200)
			EndIf
			DrawPoly poly
			SetAlpha .1
			SetColor 255*(1-control[c,0])*control[c,1]+100*(1-control[c,1]),100*(1-control[c,1]),255*(control[c,0])*control[c,1]+100*(1-control[c,1])
			DrawPoly poly
			SetAlpha .3
			If p3 p1 = p3
			If p4 p2 = p4
		Wend
		SetAlpha 1
		SetColor 255,255,255
	End Method
End Type

Type soldier
	Field x# , y# , an#
	Field state
	Field myarea:area
	Field side
	Field nexttrench:trench
	Field nextarea:area
	Field target:soldier
	Field ammoload
	Field speed#
	Field tx#,ty#
	Field drawan#
	Field maxspeed#
	Field kills
	Field anmod#
	Field health#
	Field vendor:icecreamvan,ticket
	Field peckish
	Field shelter:tank,shelteroffset#
	
	Method New()
		soldiers.addlast Self
		state = 0
		maxspeed#=Rnd(1,1.2)
		health=1
		'If Rand(10)=1 peckish=1
		peckish=1
	End Method
	
	Function Create:soldier(side , y#)
		s:soldier = New soldier
		s.side = side
		s.y = y
		s.init()
		Return s
	End Function
	
	Method init()
		Select side
			Case 0
				x = -20
				an = 0
				nexttrench = trench(trenches.first())
				nextarea=area(areas.first())
			Case 1
				x = gwidth+20
				an = 180
				nexttrench = trench(trenches.last())
				nextarea=area(areas.last())
		End Select
		If Not nextarea Throw "??????"
		drawan=an
	End Method
	
	Method update()
		'Print state
		'If myarea Print "in area"
		'If nextarea Print "next area"
		ammoload:+1
		
		speed:-.1
		If speed<0 speed=0
		
		trx=nexttrench.getx(y)
		If Abs(trx-x)<10
			If state=2 prob=50 Else prob=20
			If Rand(prob)=1
				If state=3 'staying on line
					seg=Int(y/50)
					myseg=myarea.segments[seg,side]
					If seg>0
						upseg=myarea.segments[seg-1,side]
					Else
						upseg=myseg
					EndIf
					If seg<15
						downseg=myarea.segments[seg+1,side]
					Else
						downseg=myseg
					EndIf
					If upseg<downseg
						dir=-1
						theirseg=upseg
					Else
						dir=1
						theirseg=downseg
					EndIf
					If state=2 factor#=2 Else factor#=1.2
					If myseg > theirseg * factor And myseg > 1
						segsize#=gheight/16
						If dir=-1 And seg>0
							state=6
							ty=(seg-1)*segsize+Rand(segsize-1)
						ElseIf dir=1 And seg<myarea.numsegments
							state=6
							ty=(seg+1)*segsize+Rand(segsize-1)
						EndIf
						tx=nexttrench.getx(ty)
						If side=0 tx:-10 Else tx:+10
					EndIf
				EndIf
			EndIf
		EndIf
		
		If state<>3 Or Rand(20)=1
			anmod=Rnd(-40,40)
		EndIf
		
		If (state=3 Or state=1)
			If peckish
				closestwhippee:icecreamvan=Null
				mindist#=-1
				If health<=1
					For w:icecreamvan=EachIn icecreamvans
						If w.side=side And w.speed<.1 And w.serving 'And w.queue.count()<10
							dx#=w.queuex-x
							dy#=w.queuey-y
							d#=dx*Dx+dy*dy
							If d<2500 And (d<mindist Or mindist=-1) 
								closestwhippee=w
								mindist=d
							EndIf
						EndIf
					Next
				EndIf
				If closestwhippee
					state=8
					vendor=closestwhippee
				EndIf
			EndIf
		EndIf
		If (state=3 Or state=1 Or state=6)
			If (Not vendor) 'And Rand(50)=1
				closesttank:tank=Null
				mindist#=-1
				For t:tank=EachIn tanks
					If t.side=side
						dx#=t.queuex-x
						dy#=t.queuey-y
						d#=dx*dx+dy*dy
						If d<2500 And (d<mindist Or mindist=-1)
							closesttank=t
							mindist=d
						EndIf
					EndIf
				Next
				If myarea
					If myarea.owner<>side closesttank=Null
				EndIf
				If closesttank
					state=9
					shelter=closesttank
				EndIf
			EndIf
		EndIf
		
		If nextarea=Null finito=1		
		
		getarea()
		
		Select state
		Case 0 'just made - run to trench
			move()
			If (x > trx And side=0) Or (x<trx And side=1)
				state = 1
				'getarea()
			EndIf
		Case 1 'in area, doing nowt
			If myarea.soldiers[1 - side].count() 
				picktarget(myarea,100)
				If state=1 state=3
			Else
				'picktarget(nextarea,100) 
				If state=1 state = 3
			EndIf
		Case 2 'shooting
			If Not soldiers.contains(target) 'dude is dead
				state = 1
				target=Null
			Else
				dx# = target.x - x
				dy# = target.y - y
				d# = Sqr(dx * dx + dy * dy) 
				an=ATan2(dy,dx)
				If ammoload>=60 And Rand(10)=1
					ammoload=0
					p# = 10 / d
					If p > 1 p = 1
					'p:*.9
					sx#=target.x
					sy#=target.y
					If target.myarea<>myarea
						tdiff#=target.x-target.nexttrench.getx(target.y)
						If Abs(tdiff)<15 p:*.3
						If Abs(trx-x)<15 p:*3
					EndIf
					If Rnd(1) < p
						If target.hit()
							kills:+1
						EndIf
					Else
						sx:+Rnd(-7,7)
						sy:+Rnd(-7,7)
					EndIf
					roundsfired[side]:+1
					shot.Create(x,y,sx,sy)
					If Rand(2)=1 riflesound()
				Else
					If d>60 Or Rand(3)=1
						If target.myarea=myarea
							move()
						Else
							'move()
							state=3
						EndIf
					EndIf
				EndIf
			EndIf
		Case 3 'walking to front line
			Select side
			Case 0
				If Not Target an=anmod
				If trx-x>10
					move()
				ElseIf trx-x<0
					'getarea()
				EndIf
			Case 1
				If Not Target an=180+anmod
				If x-trx>10
					move()
				ElseIf x-trx<0
					'getarea()
				EndIf
			End Select
			If Abs(x-trx)<=10
				If Rand(20)=1
					If nextarea
						seg=Int(y/50)
						myguys=myarea.segments[seg,side]
						theirguys=nextarea.segments[seg,1-side]
						If seg>0
							myguys:+myarea.segments[seg-1,side]
							theirguys:+nextarea.segments[seg-1,1-side]
						EndIf
						If seg<15
							myguys:+myarea.segments[seg+1,side]
							theirguys:+nextarea.segments[seg+1,1-side]
						EndIf
						'myguys=myarea.soldiers[side].count()
						'theirguys=nextarea.soldiers[1-side].count()
						If (theirguys<3 And myguys>3) Or (theirguys=0 And myguys>=2)
							state=5
						EndIf
					Else
						state=5
					EndIf
				EndIf
			EndIf
			picktarget(myarea,100)
			If state=3 picktarget(nextarea,50)
		Case 4 'walking to target
			If Not soldiers.contains(target)
				state = 1
				target = Null
			Else
				dx# = target.x - x
				dy# = target.y - y
				d#=dx*dx+dy*dy
				If (Sqr(d) < 80) Or Abs(dy)<3
					state = 2
				Else
					If target.myarea=myarea
						an# = ATan2(dy , dx)
					Else
						an# = ATan2(dy , 0)
					EndIf
					move()
				EndIf
			EndIf
		Case 5 'walking to next area
			picktarget(myarea,100)
			Select side
			Case 0
				an = 0
			Case 1
				an = 180
			End Select
			move()
			tx = nexttrench.getx(y)
			If (x > tx And side = 0) Or (x < tx And side = 1) 
				'getarea()
				state=3
				'If myarea = Null
				'	End
				'EndIf
			EndIf
		Case 6 'running up the line
			'picktarget(myarea,100)
			'If state=6 picktarget(nextarea,100)
			dy#=ty-y
			dx#=tx-x
			d#=dx*dx+dy*dy
			an#=ATan2(dy,dx)
			move()
			If d<9 state=3
		Case 8 'running to ice cream van
			'DrawText Int(health),x,y
			leave=0
			If ticket>10 And Rand(100)=1
				peckish=0
			EndIf
			If vendor.serving=0 Or soldiers.contains(vendor)=False Or health>=2 Or peckish=0
				fedup()
			Else
				If vendor.queue.contains(Self)
					pow#=(10*ticket)
					qan#=vendor.an+90
					tx=vendor.x+Cos(qan)*pow
					ty=vendor.y+Sin(qan)*pow
					dx#=tx-x
					dy#=ty-y
					d#=dx*dx+dy*dy
					If d<8
						an=qan+180
					Else
						an#=ATan2(dy,dx)
						move()
					EndIf
				Else
					dx#=vendor.queuex-x
					dy#=vendor.queuey-y
					d#=dx*dx+dy*dy
					If d<2500
						vendor.queue.addlast Self
						ticket=vendor.queue.count()
						speed:*.8
					Else
						an=ATan2(dy,dx)
						move()
					EndIf
				EndIf
			EndIf
		Case 9 'following tank
			If tanks.contains(shelter)=False Or myarea.owner<>side 'Or (shelter.speed<.2 And ticket<5) 
				outshelter()
			Else
				If shelter.queue.contains(Self)
					pow#=(10*ticket)
					tx=shelter.x+(2*side-1)*(ticket*6+5)
					ty=shelter.y+shelteroffset
					dx#=tx-x
					dy#=ty-y
					d#=dx*dx+dy*dy
					If Rand(50)=1
						shelteroffset=Rnd(-25,25)
					EndIf
					If d<16
						If side=0
							an=anmod
						Else
							an=anmod+180
						EndIf
					Else
						an#=ATan2(dy,dx)
						move()
					EndIf
				Else
					dx#=shelter.queuex-x
					dy#=shelter.queuey-y
					d#=dx*dx+dy*dy
					If d<2500
						shelter.queue.addlast Self
						ticket=shelter.queue.count()
						speed:*.8
						shelteroffset=Rnd(-25,25)
					Else
						an=ATan2(dy,dx)
						move()
					EndIf
				EndIf
				picktarget(myarea,100)
			EndIf
		End Select
		
		dan#=andiff(an,drawan)
		drawan:+Sgn(dan)*9
	End Method
	
	Method outshelter()
		state=1
		shelter.queue.remove Self
		shelter=Null
	End Method
	
	Method fedup()
		state=1
		vendor.leavequeue Self
		vendor=Null
	End Method

	Method riflesound()
		'If noises.count()>60 Return
		r:noise=noise.Create(x,y,riflenoise)
		SetChannelRate r.ch,Rnd(.6,1.2)
	End Method
		
	Method getarea()
		If Not nextarea Throw "!!???"
		If myarea
			myarea.soldiers[side].remove Self
		EndIf
		lx#=0
		For a:area=EachIn areas
			rx#=a.r.getx(y)
			If x>lx And x<rx
				myarea=a
				If side=0
					nexttrench=a.r
					nextarea=a.ra
				Else
					nexttrench=a.l
					nextarea=a.la
				EndIf
				a.soldiers[side].addlast Self
				Return
			EndIf
			lx=rx
		Next
		'myarea = nextarea
		'If myarea myarea.soldiers[side].addlast(Self)
		'If Not myarea Return
		'Select side
		'Case 0
		'	nexttrench = myarea.r
		'	nextarea = myarea.ra
		'Case 1
		'	nexttrench = myarea.l
		'	nextarea = myarea.la
		'End Select
		'If Not nextarea Throw "!!!!!!!!!"
	End Method
	
	Method picktarget(a:area,dist)
		closest:soldier = Null
		mindist=-1
		If target
			dx#=target.x-x
			dy#=target.y-y
			d#=dx*dx+dy*dy
			If d<dist*dist
				state=2
				Return
			EndIf
		EndIf
		If Not a Return
		For s:soldier = EachIn a.soldiers[1 - side]
			dx# = s.x - x
			dy# = s.y - y
			d# = dx * dx + dy * dy
			If (d<mindist Or mindist=-1) And d<dist*dist And Rand(5)=1
				closest=s
				mindist = d
			EndIf
		Next
		If closest
			target = closest
			If Sqr(mindist) > 100
				state = 4
			Else
				state = 2
				'Print closest.x
			EndIf
			Return 1
		EndIf
	End Method
	
	Method hit(amount#=1)
		health:-amount
		If health>0 Return 0
		
		'Print "BLAM"
		If myarea
			myarea.soldiers[side].remove Self
		EndIf
		soldiers.remove Self
		deathtoll[side]:+1
		If kills>5
			name$=names[Rand(1,Len(names))-1]
			p#=Rnd(1)
			p=p*p
			n#=1
			While n/Len(titles)<p
				n:+1
			Wend
			title$=titles[n-1]
			If side=0
				enemy$="BLUE"
			Else
				enemy$="RED"
			EndIf
			If kills>10
				star$="PLATINUM"
			ElseIf kills>7
				star$="GOLD"
			Else
				star$="BRONZE"
			EndIf
			message.Create(title+" "+name+" WAS AWARDED A "+star+" STAR FOR  KILLING "+String(kills)+" "+enemy+" SOLDIERS")
		EndIf
		Return 1
	End Method
	
	Method move(dir=0)
		speed:+Rnd(.11,.13)
		If speed>maxspeed speed=maxspeed
		x:+ Cos(an) * speed
		y:+ Sin(an) * speed
		If y<5 y=5
		If y>gheight-5 y=gheight-5
	End Method
	
	Method draw()
		sidecolors(side)
		Local poly#[]
		x1#=x+Cos(drawan)*(6+2*speed)
		y1#=y+Sin(drawan)*(6+2*speed)
		x2#=x+Cos(drawan+120)*4
		y2#=y+Sin(drawan+120)*4
		x3#=x+Cos(drawan+240)*4
		y3#=y+Sin(drawan+240)*4
		poly=[x1,y1,x2,y2,x3,y3]
		drawoutline poly,side
		'DrawPoly poly
		'DrawRect x - 2 , y - 2 , 4 , 4
		SetColor 255,255,255
		'SetScale .5,.5
		'DrawText ammoload,x,y
		'SetScale 1,1
	End Method
End Type

Type shot
	Field ox#,oy#,x#,y#,life
	
	Method New()
		shots.addlast Self
	End Method
	
	Function Create:shot(ox#,oy#,x#,y#,life=0)
		b:shot=New shot
		b.ox=ox
		b.oy=oy
		b.x=x
		b.y=y
		b.life=life
		Return b
	End Function
	
	Method draw()
		If life<5
			SetLineWidth 2
			SetAlpha .2
			SetColor 255,255,0
			DrawLine ox,oy,ox+(x-ox)*.2,oy+(y-oy)*.2
			SetAlpha 1
			
			If life=4
				an#=ATan2(y-oy,x-ox)
				For c=1 To 6
					ban=an+Rand(-30,30)
					pow#=Rnd(2,5)
					b:shot=shot.Create(Cos(ban)*pow,Sin(ban)*pow,x,y,5)
				Next
				shots.remove Self
			EndIf
		Else
			x:+ox
			y:+oy
			ox:*.8
			oy:*.8
			SetColor 100,150,0
			SetAlpha 5.0/life
			DrawRect x,y,Rand(1,2),Rand(1,2)
			SetAlpha 1
		EndIf
		life:+1
		If life>20 shots.remove Self
	End Method
End Type

Type artillery
	Field side
	Field x#,y#
	Field ammoload#
	
	Method New()
		artilleries.addlast Self
		ammoload=Rnd(100)
	End Method

	Function Create:artillery(side)',x#,y#)
		g:artillery=New artillery
		g.side=side
		'g.x=x
		'g.y=y
		Return g
	End Function
	
	Method update()
		ammoload:+Rnd(.1,.5)
		If ammoload>100
			most=-1
			x=0
			For a:area=EachIn areas
				For s=0 To 15
					diff=a.segments[s,1-side]-a.segments[s,side]
					If a.owner=-1 prob=2 Else prob=8
					If (diff>most Or most=-1) And diff>0 And Rand(prob)=1 
						most=diff
						py#=s*50+25
						px#=(a.l.getx(py)+a.r.getx(py))/2
					EndIf
				Next
				x:+80
			Next
			If most>0
				ammoload=0
				px:+Rnd(-30,30)
				py:+Rnd(-30,30)
				shell.Create(side,px,py)
				shellsdropped[side]:+1
			EndIf
		EndIf
	End Method
End Type

Type shell
	Field x#,y#
	Field side
	Field time#
	
	Method New()
		shells.addlast Self
	End Method
	
	Function Create:shell(side,x#,y#)
		k:shell=New shell
		k.side=side
		k.x=x
		k.y=y
		Return k
	End Function
	
	Method update()
		time:+Rnd(.01,.03)
		If time>=1
			For c=0 To Rnd(300,400)
				an#=Rnd(360)
				pow#=Rnd(0,10)
				shot.Create(Cos(an)*pow,Sin(an)*pow,x,y,5)
			Next
			For s:soldier=EachIn soldiers
				dx#=s.x-x
				dy#=s.y-y
				d#=dx*dx+dy*dy
				If d<1800
					p#=50/Sqr(d)
					If p>1 p=1
					p:*.4
					If Rnd(0,1)<p
						'Print "!"
						s.hit(3)
					EndIf
				EndIf
			Next
			shells.remove Self
			explosionsound()
		EndIf
	End Method
	
	Method explosionsound()
		r:noise=noise.Create(x,y,explosionnoise)
		SetChannelRate r.ch,Rnd(.4,.6)
	End Method
	
	Method draw()
		sidecolors(side)
		SetLineWidth 2
		r#=Sqr(1-time)*40
		ox#=x+r
		oy#=y
		Local poly#[6]
		For an=0 To 360 Step 30
			sx#=Cos(an)*r+x
			sy#=Sin(an)*r+y
			dx#=sx-ox
			dy#=sy-oy
			ex#=ox+dx*time
			ey#=oy+dy*time
			SetAlpha .3
			DrawLine ox,oy,ex,ey
			poly=[ox,oy,ex,ey,x,y]
			SetAlpha .2
			DrawPoly poly
			ox=sx
			oy=sy
		Next
		SetLineWidth 1
		SetAlpha 1
	End Method
End Type

Type tank Extends soldier
	Field an#,van#
	Field turretan#,ttx#,tty#
	Field ltalong#,rtalong#
	Field queue:TList,queuex#,queuey#
	
	Method New()
		'soldiers.addlast Self
		tanks.addlast Self
		ammoload=50
		queue=New TList
	End Method

	Function Create:tank(side,y#)
		c:tank=New tank
		c.side = side
		c.y = y
		c.init()
		If side=0
			c.tx=200
		Else
			c.tx=gwidth-200
		EndIf
		c.ty=y+Rnd(-50,50)
		Return c
	End Function

	Method update()
		
		If health<=5
			'movement decisions
			dx#=tx-x
			dy#=ty-y
			d#=dx*dx+dy*dy
			wantan#=ATan2(dy,dx)
			dan#=andiff(an,wantan)
			If d>400 turn(-Sgn(dan))
			
			If Abs(dan)<10
				If d>Rand(700,900)
					move(1)
				Else
					move(-1)
				EndIf
				van:*.9
			EndIf
			
			'tactical decisions
			If myarea
				'If myarea.owner=side p=400 Else p=50
				p=2000.0/(queue.count()+1)
				If d<=1000 And Rand(p)=1 'close to target
					seg=Int(y/50)
					myguys=myarea.segments[seg,side]
					theirguys#=myarea.segments[seg,1-side]
					diff=theirguys-myguys
					If theirguys<3 'move somewhere else
						most#=0
						For c=0 To 15
							segdiff=Abs(c-seg)
							If segdiff=0 segdiff=1
							If c<>seg
								theirguys#=myarea.segments[c,1-side]/segdiff
								If theirguys>most Or most=-1
									most=theirguys
									ty=c*50+Rnd(10,40)
									tx=(myarea.l.getx(ty)+myarea.r.getx(ty))/2
								EndIf
							EndIf
							If nextarea And myarea.owner=side
								theirguys#=nextarea.segments[c,1-side]/segdiff
								If theirguys>most Or most=-1
									most=theirguys
									ty=c*50+Rnd(10,40)
									tx=(nextarea.l.getx(ty)+nextarea.r.getx(ty))/2
								EndIf
							EndIf
						Next
						If most=0' And Rand(10)=1
							tx=(nextarea.l.getx(y)+nextarea.r.getx(y))/2
							ty=y+Rand(-50,50)
							If ty<25 ty=25
							If ty>gheight-25 ty=gheight-25
						EndIf
					EndIf
				EndIf
			EndIf
			
			'shooting decisions
			If ammoload<>100 ammoload:+1
			'If ammoload>400 ammoload=400
			wantan#=ATan2(tty-y,ttx-x)
			tdan#=andiff(turretan,wantan)
			If tty>0 turretan:-Sgn(tdan)*.5
			If (ammoload>=300 And Abs(tdan)<10) Or tty=0
			
				If tty>0
					k:shell=shell.Create(side,ttx+Rnd(-10,10),tty+Rnd(-10,10))
					k.time=1
					ammoload=0
					speed:-.5*Cos(andiff(an,turretan))
					For c=0 To 20
						shotan#=turretan+Rand(-20,20)
						pow#=Rnd(2,12)
						shot.Create(Cos(shotan)*pow,Sin(shotan)*pow,x+Cos(turretan)*16,y+Sin(turretan)*16,5)
						SetLineWidth 5
						SetAlpha .5
						SetColor 255,255,0
						DrawLine x+Cos(turretan)*16,y+Sin(turretan)*16,ttx,tty
						SetAlpha 1
						SetLineWidth 1
					Next
					ttx=0
					tty=0
				EndIf
			EndIf
			
			If ammoload=100				
				most#=0
				For a:area=EachIn areas
					For c=0 To 15
						If Abs(c-seg)>1
							py#=c*50+25
							px#=(a.l.getx(py)+a.r.getx(py))/2+Rand(-20,10)
							dy#=py-y
							dx#=px-x
							d#=Sqr(dx*dx+dy*dy)
							rating#=a.segments[c,1-side]/Sqr(d)
							If rating>most And rating>0 And d<200
								most=rating
								ttx=px
								tty=py
							EndIf
						EndIf
					Next
					If most>0
					EndIf
				Next
				
				closest:tank=Null
				mindist#=-1
				For oc:tank=EachIn tanks
					If oc<>Self And oc.health<=5 And oc.side<>side
						dx#=oc.x-x
						dy#=oc.y-y
						d#=Sqr(dx*dx+dy*dy)
						If (d<mindist Or mindist=-1) And d<200
							closest=oc
							mindist=d
						EndIf
					EndIf
				Next
				If closest
					ttx=closest.x
					tty=closest.y
				EndIf
				
				If tty ammoload:+1
			EndIf
			
			'movement update
			an:+van
			turretan:+van
			ltalong:+van+speed
			rtalong:+(-van)+speed
			van:*.99
			speed:*.99
			x:+Cos(an)*speed
			y:+Sin(an)*speed
			
			For oc:tank=EachIn tanks
				If oc<>Self
					dx#=oc.x-x
					dy#=oc.y-y
					d#=Sqr(dx*dx+dy*dy)
					If d<40
						midx#=(oc.x+x)/2
						midy#=(oc.y+y)/2
						x=midx-20*dx/d
						y=midy-20*dy/d
						oc.x=midx+20*dx/d
						oc.y=midy+20*dy/d
					EndIf
				EndIf
			Next
					
			
			If (x>nexttrench.getx(y) And side=0) Or (x<nexttrench.getx(y) And side=1)
				getarea()
			EndIf
			
			If y<5 
				y=5
				If ty<25 ty=25
			EndIf
			If y>gheight-5 
				y=gheight-5
				If ty>gheight-25 ty=gheight-25
			EndIf
		
		Else
			health:+.1
			If health>20 
				tanks.remove Self
				soldiers.remove Self
			EndIf
		EndIf
		
		'manage queue
		n=0
		For s:soldier=EachIn queue
			If soldiers.contains(s)
				n:+1
				s.ticket=n
			Else
				queue.remove s
			EndIf
		Next
		queuex=x+(2*side-1)*(queue.count()*12+5)
		queuey=y
	End Method
	
	Method turn(dir)
		van:+dir*.02
	End Method
	
	Method move(dir)
		If dir=-1
			speed:*.98
		Else
			speed:+dir*.01
			If speed>3 speed=3
		EndIf
	End Method
	
	Method draw()
		Local body#[]=[-13.0,-6.0,-8.0,-9.0,8.0,-9.0,13.0,-6.0,13.0,6.0,8.0,9.0,-8.0,9.0,-13.0,6.0]
		Local ltrack#[]=[-10.0,-8.0,-10.0,-12.0,10.0,-12.0,10.0,-8.0]
		Local rtrack#[]=[-10.0,8.0,-10.0,12.0,10.0,12.0,10.0,8.0]
		al=ammoload
		If al>100 al=100
		gunrecoil#=al/20.0
		Local gun#[]=[-5.0+gunrecoil,-3.0,-5.0+gunrecoil,3.0,16.0+gunrecoil,2.0,16.0+gunrecoil,-2.0]
		rotatedpoly(body,x,y,an,1.5)
		rotatedpoly(ltrack,x,y,an,1.5)
		rotatedpoly(rtrack,x,y,an,1.5)
		rotatedpoly(gun,x,y,turretan,1.5)
		If health>5
			'SetAlpha (20-health)/20.0
			n#=255*health/20.0
			SetColor n,n,n
			SetBlend SHADEBLEND
			drawside=-1
		Else		
			SetAlpha 1
			drawside=side
		EndIf
		drawoutline ltrack,drawside
		drawoutline rtrack,drawside
	
		along#=ltalong*.2 Mod 20
		along:-(Int(along)/5+1)*5
		While along<32
			If along<32 And along>0
				ralong#=along
				If ralong#<9 
					ralong:/3
				ElseIf ralong#>23
					ralong=ralong-23
					ralong=ralong/3+17
				Else
					ralong:-6
				EndIf
				drawrotatedline ralong-10,-12,ralong-10,-8,x,y,an,1.5
			EndIf
			along:+5
		Wend

		along#=rtalong*.2 Mod 20
		along:-(Int(along)/5+1)*5
		While along<32
			If along<32 And along>0
				ralong#=along
				If ralong#<9 
					ralong:/3
				ElseIf ralong#>23
					ralong=ralong-23
					ralong=ralong/3+17
				Else
					ralong:-6
				EndIf
				drawrotatedline ralong-10,8,ralong-10,12,x,y,an,1.5
			EndIf
			along:+5
		Wend
		
		drawoutline body,drawside
		drawoutline gun,drawside
		SetBlend ALPHABLEND
	End Method
	
	Method hit(amount#=1)
		health:+amount
		If health>5
			Super.hit()
		EndIf
	End Method
	
End Type

Type icecreamvan Extends soldier
	Field serving,stock
	Field jinglenoise:noise
	Field queue:TList
	Field cash,earned,price
	Field controlled
	Field queuex#,queuey#
	
	Method New()
		icecreamvans.addlast Self
		soundchannel=AllocChannel()
		queue=New TList
		controlled=0
	End Method
	
	Function Create:icecreamvan(side,y#)
		w:icecreamvan=New icecreamvan
		w.side=side
		w.restock()
		w.y=y
		Return w
	End Function
	
	Method restock()
		If serving jingle()
		If side=0
			x=-20
			an=0
		ElseIf side=1
			x=gwidth+20
			an=180
		EndIf
		stock=20
		earnedcash[side]:+cash
		earned:+cash
		cash=0
	End Method
	
	Method getprice(px)
		If side=1 px=gwidth-px
		Return px*px*2/gwidth
	End Method
	
	Method update()

		x:+Cos(an)*speed
		y:+Sin(an)*speed
		speed:*.99
		
		price=getprice(x)
		
		If side=0 And x<-20
			restock()
		ElseIf side=1 And x>gwidth+20
			restock()
		EndIf
		
		If controlled
			tx=mx
			ty=my
			If MouseHit(1) jingle()
		Else
			Select state
			Case 0 'starting
				tx=x
				ty=y
				bestprice=-1
				bestarea:area=Null
				bestx#=0
				ty#=Rand(50,gheight-50)
				For a:area=EachIn areas
					If a.owner=side
						trx#=(a.l.getx(ty)+a.r.getx(ty))/2
						theirprice=getprice(trx)
						If theirprice>bestprice Or bestprice=-1
							bestprice=theirprice
							bestarea=a
							bestx#=trx
						EndIf
					EndIf
				Next
				If bestarea
					r#=Rnd(40)
					tx=bestx+r*(1-2*side)
					state=1
				EndIf
			Case 1 'selling
				dx#=tx-x
				dy#=ty-y
				d#=dx*dx+dy*dy
				If d<4000 And Not serving
					jingle()
				EndIf
				If stock<Rand(3,6)
					If serving jingle()
					tx=side*(gwidth+40)-20
					state=2
				EndIf
			Case 2 'restocking
				If stock=20 state=0
			End Select
		EndIf
			
		dx#=tx-x
		dy#=ty-y
		If tx<40 And side=0
			If stock<20 
				dx=-20-x
			ElseIf x<40
				dx=40-x
			EndIf
			'dy=0
		ElseIf tx>gwidth-40 And side=1
			If stock<20
				dx=gwidth+20-x
			ElseIf x>gwidth-40
				dx=gwidth-40-x
			EndIf
		EndIf
		wantan#=ATan2(dy,dx)
		d#=Sqr(dx*dx+dy*dy)
		dan=andiff(wantan,an)
		drawdan#=0
		If d>10
			If Abs(dan)>10
				an:+Sgn(dan)*1.5
				drawdan#=dan
			Else
				If d>50
					speed:+.015
				Else
					If speed<.5
						speed:+.01
					Else
						speed:*.98
					EndIf
				EndIf
			EndIf
		Else
			speed:*.98				
		EndIf
		If serving And speed<.1
			drawdan=andiff(drawan,70)
			drawan:-Sgn(drawdan)*.5
		Else
			drawan:-(90*Sgn(drawdan)-drawan)*.05
			drawan:*.8
		EndIf
		speed:*.99

		If serving And Rand(50)=1
			If queue.count()
				s:soldier=soldier(queue.first())
				dx#=s.x-x
				dy#=s.y-y
				d#=dx*dx+dy*dy
				If d<180
					s.peckish=0
					queue.remove s
					For s:soldier=EachIn queue
						s.ticket:-1
					Next
					stock:-1
					cash:+price
					If stock<=0 
						PlaySound handbellnoise
						jingle()
						'Print "OUT OF ICE CREAM!"
					EndIf
				EndIf
			EndIf
		EndIf
		
		n=0
		p=1
		For s:soldier=EachIn queue
			s.ticket=p
			p:+1
			If Not soldiers.contains(s)
				queue.remove s
				n:+1
			EndIf
			s.ticket:-n
		Next
		If queue.count() 
			s:soldier=soldier(queue.last())
			queuex=s.x
			queuey=s.y
		Else
			queuex=x
			queuey=y
		EndIf
			
		
		If jinglenoise
			jinglenoise.x=x
			jinglenoise.y=y
		EndIf
		
		getarea()
	End Method
	
	Method jingle()
		If serving
			jinglenoise.stop()
			jinglenoise=Null
			serving=0
			queue=New TList
		ElseIf stock>0
			serving=1
			jinglenoise=noise.Create(x,y,icecreamnoise,1)
		EndIf
	End Method 
	
	Method control()
		
		
	End Method
	
	Method hit(amount#)
		stock:-amount*Rand(3)
		If stock<0
			cash=0
			restock()
		EndIf
	End Method
	
	Method leavequeue(s:soldier)
		n=0
		For os:soldier=EachIn queue
			If os=s
				n:+1
				queue.remove s
			EndIf
			s.ticket:-n
		Next
	End Method
	
	Method getarea()
		If myarea
			myarea.soldiers[side].remove Self
		EndIf
		lx#=0
		For a:area=EachIn areas
			rx#=a.r.getx(y)
			If x>lx And x<rx
				myarea=a
				If side=0
					nexttrench=a.r
				Else
					nexttrench=a.l
				EndIf
				a.soldiers[side].addlast Self
				Return
			EndIf
			lx=rx
		Next
	End Method
	
	Method draw()
		x1#=6
		y1#=0
		x2#=Cos(120)*4
		y2#=Sin(120)*4
		x3#=Cos(240)*4
		y3#=Sin(240)*4
		Local seller#[]=[x1,y1,x2,y2,x3,y3]
		Local wagon#[]=[-6.0,-4.0,10.0,-6.0,10.0,6.0,-6.0,4.0]
		icex#=9-(stock/20.0)*13
		Local icecream#[]=[icex,-3.0,9.0,-5.0,9.0,5.0,icex,3.0]
		'If speed>.1
		'	oomf#=Abs(drawan)/10.0
		'Else
		'	oomf#=0
		'EndIf
		rotatedpoly(seller,-16+oomf,0,drawan,1)
		rotatedpoly(seller,x,y,an,1)
		rotatedpoly(wagon,x,y,an,1)
		rotatedpoly(icecream,x,y,an,.9)
		drawoutline(wagon,side)
		drawoutline(seller,side)
		DrawPoly icecream
		
		If x<50 And side=0
			SetAlpha x/50
		ElseIf x>gwidth-50 And side=0
			SetAlpha (gwidth-x)/50
		EndIf
		pricetext$=moneytext(price)
		SetColor 255,255,0
		DrawText pricetext,x-TextWidth(pricetext)/2,y-65
		total$=moneytext(cash)
		SetColor 255,255,255
		DrawText total,x-TextWidth(total)/2,y-40
		SetAlpha 1
	End Method
	
	
End Type

Function moneytext$(amount)
	pennies=amount Mod 100
	If pennies<10
		penniest$="0"+String(pennies)
	Else
		penniest$=String(pennies)
	EndIf
	pounds=(amount-pennies)/100
	total$="£"+String(pounds)+"."+penniest
	Return total
End Function

Type noise
	Field x#,y#
	Field ch:TChannel,sound:TSound
	Field rep
	
	Method New()
		noises.addlast Self
	End Method
	
	Function Create:noise(x#,y#,sound:TSound,rep=0)
		r:noise=New noise
		r.sound=sound
		r.ch=PlaySound(sound)
		r.x=x
		r.y=y
		r.rep=rep
		Return r
	End Function
	
	Method update()
		dx#=mrwhippee.x-x
		dy#=mrwhippee.y-y
		d#=Sqr(dx*dx+dy*dy)
		v#=50/d
		If v>1 v=1
		If v<.05 v=.05
		SetChannelVolume ch,v
		If Not ChannelPlaying(ch)
			If rep
				ch=PlaySound(sound)
			Else
				noises.remove Self
			EndIf
		EndIf
	End Method
	
	Method stop()
		StopChannel ch
		noises.remove Self
	End Method
End Type

Type message
	Field text$
	
	Method New()
		messages.addlast Self
	End Method
	
	Function Create:message(text$)
		m:message=New message
		'Print text
		m.text=text+" . . . . . . . . . ."
		Return m
	End Function
End Type

Function sidecolors(side)	
	Select side
		Case - 1
			SetColor 100 , 100 , 100
		Case 0
			SetColor 255 , 0 , 0
		Case 1
			SetColor 0 , 0 , 255
	End Select
End Function

Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

Function rotatedpoly#[](poly#[],px#,py#,an#,scale#=1)
	For n=0 To Len(poly)-1 Step 2
		x#=poly[n]*scale
		y#=poly[n+1]*scale
		poly[n]=x*Cos(an)-y*Sin(an)+px
		poly[n+1]=x*Sin(an)+y*Cos(an)+py
	Next
	Return poly
End Function

Function drawrotatedline#(x1#,y1#,x2#,y2#,px#,py#,an#,scale#=1)
	nx1#=(x1*Cos(an)-y1*Sin(an))*scale+px
	ny1#=(x1*Sin(an)+y1*Cos(an))*scale+py
	nx2#=(x2*Cos(an)-y2*Sin(an))*scale+px
	ny2#=(x2*Sin(an)+y2*Cos(an))*scale+py
	DrawLine nx1,ny1,nx2,ny2
End Function

Function drawoutline(poly#[],side,thickness=1)
	If side>=0 sidecolors(side)
	DrawPoly poly
	SetLineWidth thickness
	If side>=0 SetColor 200,200,200
	l=Len(poly)
	For n=0 To l-1 Step 2
		x1#=poly[n]
		y1#=poly[n+1]
		x2#=poly[(n+2) Mod l]
		y2#=poly[(n+3) Mod l]
		DrawLine x1,y1,x2,y2
	Next
	SetLineWidth 1
End Function

Function initgame()
	trenches:TList = New TList
	areas:TList = New TList
	soldiers:TList = New TList
	shots:TList=New TList
	artilleries:TList=New TList
	shells:TList=New TList
	messages:TList=New TList
	tanks:TList=New TList
	icecreamvans:TList=New TList
	noises:TList=New TList

	For i=0 To 1
		deathtoll[i]=0
		roundsfired[i]=0
		shellsdropped[i]=0
	Next

	t:trench = Null
	notrenches=gwidth/90+1
	For c = 1 To notrenches
		t = trench.Create(c * 90-50 , t) 
	Next
	oa:area = Null
	For a:area = EachIn areas
		a.la = oa
		If oa oa.ra = a
		oa=a
	Next
	
	For c=1 To 5
		artillery.Create(0)
		artillery.Create(1)
	Next
	
	mrwhippee=icecreamvan.Create(0,Rand(50,gheight-50))
	mrwhippee.controlled=1
	icecreamvan.Create(0,Rand(50,gheight-50))
	icecreamvan.Create(1,Rand(50,gheight-50))
	icecreamvan.Create(1,Rand(50,gheight-50))
	
	message.Create("FOR KING AND COUNTRY!")
	message.Create(". . . . . . . . . .")
	message.Create(". . . . . . . . . .")
	
	
	scroll#=-gwidth
End Function

Function game()
	rifleshot=0
	time=MilliSecs()
	mx=MouseX()
	my=MouseY()
	
	For i=0 To 1
		areasowned[i]=0
	Next
	
	For a:area = EachIn areas
		a.update()
		a.draw()
	Next
	
	'a:area=area(areas.last())
	'For c=0 To 15
	'	DrawText a.segments[c,0],0,c*15
	'	DrawText a.segments[c,1],100,c*15
	'Next

	sidecolors(0)
	t:trench=area(areas.first()).l
	l:TLink=t.points.firstlink()
	p1:point=point(l.value())
	l=l.nextlink()
	Local poly#[8]
	SetAlpha .37
	While l<>Null
		p2:point=point(l.value())
		poly=[0.0,p1.y,p1.x,p1.y,p2.x,p2.y,0.0,p2.y]
		DrawPoly poly
		p1=p2
		l=l.nextlink()
	Wend
	
	sidecolors(1)
	t=area(areas.last()).r
	l:TLink=t.points.firstlink()
	p1:point=point(l.value())
	l=l.nextlink()
	While l<>Null
		p2:point=point(l.value())
		poly=[p1.x,p1.y,Float(gwidth),p1.y,Float(gwidth),p2.y,p2.x,p2.y]
		DrawPoly poly
		p1=p2
		l=l.nextlink()
	Wend
	
	For t:trench = EachIn trenches
		If Not (t=trenches.last() Or t=trenches.first())
			t.draw()
		EndIf
	Next
	
	For s:soldier = EachIn soldiers
		s.update()
		If Not tank(s)
			s.draw()
		EndIf
	Next
	
	For c:tank=EachIn tanks
		'c.update()
		c.draw()
	Next
	
	For w:icecreamvan=EachIn icecreamvans
		w.control()
		w.draw()
	Next
	
	For g:artillery=EachIn artilleries
		g.update()
	Next
	
	For b:shot=EachIn shots
		b.draw()
	Next
	
	For k:shell=EachIn shells
		k.update()
		k.draw()
	Next
	
	For r:noise=EachIn noises
		r.update()
	Next
	
	l:TLink=messages.firstlink()
	x=-scroll
	scroll:+2
	SetAlpha 1
	SetColor 200,200,200
	While x<gwidth
		m:message=message(l.value())
		DrawText m.text,x,gheight-35
		x:+TextWidth(m.text)+20
		l=l.nextlink()
	Wend
	If scroll>TextWidth(message(messages.first()).text)
		messages.removefirst()
		scroll=-20
	EndIf
	If l=Null And x<gwidth+20
		n=Rand(0,5)
		Select n
		Case 0
			text$="THE DEATH TOLL IS "+String(deathtoll[0]+deathtoll[1])+" SOULS"
		Case 1
			i=Rand(0,1)
			If i=0
				text$="THE BLUES HAVE LOST "
			Else
				text$="THE REDS HAVE LOST "
			EndIf
			text:+String(deathtoll[i])+" MEN"
		Case 2
			text$=String(roundsfired[0]+roundsfired[1])+" ROUNDS HAVE BEEN FIRED"
		Case 3
			i=Rand(0,1)
			If i=0
				text$="THE BLUES HAVE FIRED "
			Else
				text$="THE REDS HAVE FIRED "
			EndIf
			text:+String(roundsfired[i])+" ROUNDS"
		Case 4
			text$=String(shellsdropped[0]+shellsdropped[1])+" SHELLS HAVE BEEN DROPPED"
		Case 5
			i=Rand(0,1)
			If i=0
				text$="THE BLUES HAVE DROPPED "
			Else
				text$="THE REDS HAVE DROPPED "
			EndIf
			text:+String(shellsdropped[i])+" SHELLS"
		End Select
		message.Create(text)
	EndIf
	
	p#=Sin(time/50.0)*.5+.5'+Rnd(-.1,.1)
	p=p*30+2
	If Rnd(p)<2.2
		'p#=Float(areasowned[0])/(areasowned[0]+areasowned[1])
		p#=.5
		sr#=Rnd(0,1)
		If sr<p side=1 Else side=0
		y=Rand(10,gheight-10)
		If Rand(150)=1
			tank.Create(side,y)
		Else
			soldier.Create(side,y)
		EndIf
	EndIf
	
	For i=0 To 1
		diff=earnedcash[i]-displaycash[i]
		If kerchingspeed[i]<diff*.1
			kerchingspeed[i]:+1
		ElseIf kerchingspeed[i]
			kerchingspeed[i]=diff*.1
		EndIf
		If diff>0 And kerchingspeed[i]=0 Then kerchingspeed[i]=1
		displaycash[i]:+kerchingspeed[i]
	Next
	
	sidecolors(0)
	earnedtext$="Red have earnt: "+moneytext(displaycash[0])
	DrawText earnedtext,0,0
	sidecolors(1)
	earnedtext$="Blue have earnt: "+moneytext(displaycash[1])
	DrawText earnedtext,gwidth-TextWidth(earnedtext),0
	
	SetAlpha .7
	SetColor 0,0,0
	DrawRect 0,gheight-40,gwidth,40
	SetAlpha 1
	
End Function




Const gwidth=800,gheight=600
Graphics gwidth, gheight , 0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

'Incbin "c:\windows\fonts\times.ttf"
'SetImageFont(LoadImageFont("incbin::c:\windows\fonts\times.ttf",30,SMOOTHFONT))

Global finito=1

While Not KeyHit(KEY_ESCAPE)
	If Not finito
		game()
	Else
		If finito<50
			SetColor 255,0,0
			SetAlpha 1.0-finito/50.0
			DrawRect 0,0,gwidth,gheight
			finito:+1
		Else
			initgame()
			finito=0
		EndIf
	EndIf
	
	Flip
	Cls
Wend
	
