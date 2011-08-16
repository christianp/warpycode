Const gwidth=600,gheight=600

Global drawables:TList=New TList
Type drawable
	Field x#,y#
	Field vx#,vy#
	Field r#
	
	Method New()
		drawables.addlast Self
	End Method
	
	Method die()
		drawables.remove Self
	End Method
	
	Method update() Abstract

	Method move()
		x:+vx
		y:+vy
		While x<0
			x:+gwidth
		Wend
		While x>gwidth 
			x:-gwidth
		Wend
		While y<0 
			y:+gheight
		Wend
		While y>gheight 
			y:-gheight
		Wend
	End Method
	
	Method draw()
		drawat x,y
		If x<r
			drawat x+gwidth,y
			If y<r
				drawat x+gwidth,y+gheight
			ElseIf y>gheight-r
				drawat x+gwidth,y-gheight
			EndIf
		ElseIf x>gwidth-r
			drawat x-gwidth,y
			If y<r
				drawat x-gwidth,y+gheight
			ElseIf y>gheight-r
				drawat x-gwidth,y-gheight
			EndIf
		EndIf
		If y<r
			drawat x,y+gheight
		ElseIf y>gheight-r
			drawat x,y-gheight
		EndIf
	End Method
	
	Method drawat(px#,py#) Abstract
End Type

Global things:TList=New TList
Type thing Extends drawable
	Field mass#
	
	Function pick:thing(px#,py#)
		For t:thing=EachIn things
			distanceto t.x,t.y,px,py,dx#,dy#
			d#=Sqr(dx*dx+dy*dy)
			If d<t.r
				Return t
			EndIf
		Next
	End Function
	
	Method New()
		things.addlast Self
		r=1
	End Method
	
	Method die()
		Super.die
		things.remove Self
	End Method
	
	
	Method collide(t:thing)
		distanceto(x,y,t.x,t.y,dx#,dy#)
		d#=Sqr(dx*dx+dy*dy)
		dd#=r+t.r-d
		If dd>0
			dx:/d
			dy:/d
			dvx#=t.vx-vx
			dvy#=t.vy-vy
			f#=dvx*dx+dvy*dy
			f:*2/(mass+t.mass)
			t.push(-dx*f*mass,-dy*f*mass, Self)
			push(dx*f*t.mass,dy*f*t.mass, t)
			
			x:-dd*dx*t.mass/(mass+t.mass)
			y:-dd*dy*t.mass/(mass+t.mass)
			t.x:+dd*dx*mass/(mass+t.mass)
			t.y:+dd*dy*mass/(mass+t.mass)
		EndIf
	End Method
	
	Method push(fx#,fy#,t:thing)
		vx:+fx
		vy:+fy
		
		f#=Sqr(fx*fx+fy*fy)
		hit f,t
	End Method
	
	Method hit(f#,t:thing) Abstract
	
	
	Function findobstacle:thing(x#,y#,vx#,vy#,s:thing,allow#)
		mintime#=-1
		closest:thing=Null
		For t:thing=EachIn things
			If t<>s
				distanceto(x,y,t.x,t.y,dx#,dy#)
				dvx#=vx-t.vx
				dvy#=vy-t.vy
				dr#=t.r+allow
				
				a#=dvx*dvx+dvy*dvy
				b#=-2*(dx*dvx+dy*dvy)
				c#=dx*dx+dy*dy-dr*dr
				det#=b*b-4*a*c
				
				If det>=0
					det=Sqr(det)
					t1#=(-b+det)/(2*a)
					t2#=(-b-det)/(2*a)
					t1#=Min(t1,t2)
					dv#=Sqr(dvx*dvx+dvy*dvy)
					t1:/dv
					'SetColor 0,0,0
					
					If t1>0' And t1<300
						If t1<mintime Or mintime=-1
							closest=t
							mintime=t1
						EndIf
					EndIf
				EndIf
			EndIf
		Next
		Return closest
	End Function
			
End Type

Type asteroid Extends thing
	Field an#,anv#
	Field poly#[]

	Function Create:asteroid(x#,y#,vx#,vy#,r#)
		a:asteroid=New asteroid
		a.x=x
		a.y=y
		a.vx=vx
		a.vy=vy
		a.r=r
		a.mass=2*r
		numbits=10
		an#=0
		a.anv=Rnd(-.3,.3)
		a.poly=New Float[numbits*2]
		For i=0 To numbits-1
			pr#=Rnd(.9,1.1)*r
			a.poly[i*2]=Cos(an)*pr
			a.poly[i*2+1]=Sin(an)*pr
			an:+360.0/numbits
		Next
		Return a
	End Function
	
	Method die()
		Super.die

		For c=1 To poisson(20)
			an#=Rnd(360)
			v#=Rnd(.1,.5)
			nr#=Rnd(r)
			smoke.Create x+Cos(an)*nr,y+Sin(an)*nr,vx+Cos(an)*v,vy+Sin(an)*v,poisson(30)
		Next
		If r>15
			an#=0
			numkids=Rand(2,3)
			tr#=r
			For c=1 To numkids
				an=Rnd(360)
				v#=Rnd(.1,.5)
				nr#=Rnd(1.0/(1+numkids-c),1)*tr
				tr:-nr
				asteroid.Create x+Cos(an)*r*1.2,y+Sin(an)*r*1.2,vx+Cos(an)*v,vy+Sin(an)*v,r*Rnd(.4,.7)
			Next
		EndIf
	End Method
	
	Method update()
		an:+anv
	End Method
	
	Method drawat(px#,py#)
		SetColor 80,20,20
		'DrawOval px-r,py-r,r*2,r*2
		Local p#[]=New Float[Len(poly)]
		For c=0 To Len(poly)-1
			p[c]=poly[c]
		Next
		rotate p,an
		translate p,px,py
		DrawPoly p
		SetColor 255,255,255
	End Method
	
	Method hit(f#,t:thing)
	End Method
End Type

Type trail Extends drawable
	Field dx#,dy#
	Field age
	
	Function Create:trail(x#,y#,ox#,oy#)
		t:trail=New trail
		distanceto x,y,ox,oy,t.dx,t.dy
		t.x=x+t.dx/2
		t.y=y+t.dy/2
		t.dx:/2
		t.dy:/2
		t.r=Sqr(t.dx*t.dx+t.dy*t.dy)
		t.age=100
		Return t
	End Function
	
	Method update()
		age:-1
		If age<=0 die()
	End Method
	
	Method drawat(px#,py#)
		age:-1
		SetAlpha Rnd(.5,1)*age/500.0
		SetColor 255,150,0
		DrawLine px-dx,py-dy,px+dx,py+dy
		SetAlpha 1
		SetColor 255,255,255
	End Method
End Type	

Type spoot Extends drawable
	Field age
	
	Function Create:spoot(x#,y#,vx#,vy#)
		s:spoot=New spoot
		s.x=x
		s.y=y
		s.vx=vx
		s.vy=vy
		s.age=20
		Return s
	End Function	
	
	Method update()
		age:-1
		If age<=0 die()
	End Method
	
	Method drawat(px#,py#)
		SetColor Rand(150,255),Rand(150,255),0
		SetAlpha Rnd(.5,1)*Sqr(age/20.0)
		DrawRect x,y,1,1
		SetAlpha 1
		SetColor 255,255,255
	End Method
End Type

Type smoke Extends drawable
	Field age
	Field maxage#
	
	Function Create:smoke(x#,y#,vx#,vy#,age)
		s:smoke=New smoke
		s.x=x
		s.y=y
		s.vx=vx
		s.vy=vy
		s.age=age
		s.maxage=age
		Return s
	End Function
	
	Method update()
		age:-1
		If age<=0 die()
		r:+Rnd(0,.3)
	End Method
	
	Method drawat(px#,py#)
		SetColor 100,100,100
		fade#=age/maxage
		If fade>1 fade=1
		SetAlpha Rnd(.3,.8)*fade*fade
		DrawOval x-r,y-r,r*2,r*2
		SetAlpha 1
		SetColor 255,255,255
	End Method
End Type

Type dude Extends thing
	Field an#,anv#
	Field damage#
	Field shooto
	
	Field ltrail:trail
	
	'Field tx#,ty#
	
	Method New()
		r=5
		mass=5
		tx=300
		ty=300
		trails=New TList
	End Method
	
	Function Create:dude(x#,y#)
		d:dude=New dude
		d.x=x
		d.y=y
		d.ltrail=trail.Create(x,y,x,y)
		Return d
	End Function
	
	Method hit(f#,t:thing)
		If asteroid(t) f=Max(f,1)
		damage:+f
		If damage>10
			die
		EndIf
	End Method
	
	Method die()
		For c=1 To poisson(10)
			san#=Rnd(360)
			v#=Rnd(0,1)
			smoke.Create x,y, vx*.5+v*Cos(san),vy*.5+v*Sin(san), Rnd(20,60)
		Next
		For c=1 To poisson(20)
			san#=Rnd(360)
			v#=Rnd(0,2)
			spoot.Create x,y, vx*.5+v*Cos(san),vy*.5+v*Sin(san)
		Next
		Super.die
	End Method
	
	Method update()
	
		shooto:-1
		
		'ltrail=trail.Create(x,y,ltrail.x,ltrail.y)

		bvx#=x+Cos(an)*5
		bvy#=y+Sin(an)*5
		target:thing=thing.findobstacle(x,y,bvx,bvy,Self,0)
		If target
			If asteroid(target) And shooto<=0
				bullet.Create x+Cos(an)*5,y+Sin(an)*5,vx+Cos(an)*5,vy+Sin(an)*5
				shooto=Rand(30,100)
			EndIf
		EndIf
	
		wx#=0
		wy#=0
		
		closest:thing=thing.findobstacle(x,y,vx,vy,Self,r+5)	
		
		If closest

			distanceto(x,y,closest.x,closest.y,dx#,dy#)
			
			ran#=ATan2(dy,dx)
			d#=Sqr(dx*dx+dy*dy)
			dr#=closest.r+r+5
			swing#=ASin(dr/d)
			van#=ATan2(vy,vx)
			If vy=0 And vx=0 Then van=ran
			dan#=andiff(ran,van)
			nvan#=ran+Sgn(dan)*swing
			
			nx#=Cos(nvan+Sgn(dan)*90)
			ny#=Sin(nvan+Sgn(dan)*90)
			ix#=dx/d
			iy#=dy/d
			
			If vx*vx+vy*vy=0
				tt#=mintime
			Else
				tt#=d/(vx*ix+vy*iy)
			EndIf
			a#=(dr-(vx*nx+vy*ny)*tt)/(0.5*tt*tt)
			a:*1.5
			
			dvx#=a*nx
			dvy#=a*ny
			
			dv#=Sqr(dvx*dvx+dvy*dvy)
			wx:+dvx
			wy:+dvy
		Else
			v#=Sqr(vx*vx+vy*vy)
			If v>2
				wx:-0.0015*vx/v
				wy:-0.0015*vy/v
			ElseIf v>0.1
				wx:+.0015*vx/v
				wy:+.0015*vy/v
			Else
				wx:+Rnd(-.1,.1)
				wy:+Rnd(-.1,.1)
			EndIf
		
		EndIf
		
		'	distanceto(tx,ty,dx#,dy#)
		'	d#=Sqr(dx*dx+dy*dy)
		'	If d>0
		'		vx:+dx*.01/d
		'		vy:+dy*.01/d
		'	EndIf
			wx:+(KeyDown(key_right)-KeyDown(key_left))*.02
			wy:+(KeyDown(key_down)-KeyDown(key_up))*.02
		
		
		w#=Sqr(wx*wx+wy*wy)
		'DrawText Int(w*1000),x,y
		wan#=ATan2(wy,wx)
		
		rate#=w*50
		If rate>2 rate=2
		spoots=poisson(rate)
		For c=1 To spoots
			v#=Rnd(.2,1.5)
			san#=wan+Rnd(-20,20)+180
			spoot.Create x,y,vx+v*Cos(san),vy+v*Sin(san)
		Next
		
		dan#=andiff(an,wan)
		If Abs(dan)>.5 dan=Sgn(dan)*.5
		anv:+dan
		anv:*.9
		an:+anv
		
		vx:+wx
		vy:+wy
		
		
		For c=1 To poisson(damage/4)
			san#=an+180+Rnd(-1,1)*(damage*3+5)
			f#=Rnd(.1,1)
			smoke.Create(x,y,vx*.5+f*Cos(san),vy*.5+f*Sin(san),Sqr(damage)*10)
		Next
			
	End Method
	
	Method drawat(px#,py#)
		SetColor 0,0,255
		Local poly#[]
		poly=[8.0,0.0,-4.0,-4.0,-4.0,4.0]
		rotate poly,an
		translate poly,px,py
		DrawPoly poly
		'DrawOval px-5,py-5,10,10
		SetColor 255,255,255
	End Method
	
End Type

Type bullet Extends thing
	Field life
	Method New()
		mass=.10
		r=1
		life=300
	End Method
	
	Function Create:bullet(x#,y#,vx#,vy#)
		b:bullet=New bullet
		b.x=x+vx
		b.y=y+vy
		b.vx=vx
		b.vy=vy
		Return b
	End Function
	
	Method hit(f#,t:thing)
		t.die
		die
	End Method
	
	Method update()
		life:-1
		If life<=0 die()
	End Method
	
	Method drawat(px#,py#)
		v#=Sqr(vx*vx+vy*vy)
		SetColor 0,0,255
		DrawLine px,py,px-vx*5/v,py-vy*5/v
		SetColor 255,255,255
	End Method
End Type

Function rotate(p#[],an#)
	c#=Cos(an)
	s#=Sin(an)
	For i=0 To Len(p)-1 Step 2
		x#=p[i]
		y#=p[i+1]
		p[i]=x*c-y*s
		p[i+1]=y*c+x*s
	Next
End Function

Function translate(p#[],x#,y#)
	For i=0 To Len(p)-1 Step 2
		p[i]:+x
		p[i+1]:+y
	Next
End Function

Function andiff#(an1#,an2#)
	an2:-an1
	If an2<-180 an2:+360
	If an2>180 an2:-360
	Return an2
End Function

Function distanceto(x#,y#,px#,py#,dx# Var,dy# Var)
	dx=px-x
	dy=py-y
	While Abs(dx)>gwidth/2
		 dx:-Sgn(dx)*gwidth
	Wend
	While Abs(dy)>gheight/2 
		dy:-Sgn(dy)*gheight
	Wend
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

AppTitle="Anti-Asteroids"
Graphics gwidth,gheight,0
SeedRnd MilliSecs()
SetBlend ALPHABLEND

an#=0
For c=1 To 6
	an#:+40
	r#=Rand(100,290)
	asteroid.Create 300+r*Cos(an), 300+r*Sin(an), Rnd(-.5,.5), Rnd(-.5,.5), Rnd(40,60)
Next

'asteroid.Create 50,50,1,0,50
'asteroid.Create 300,50,0,0,50

For c=1 To 3
	an:+72
	du:dude=dude.Create(300+Cos(an)*60,300+Sin(an)*60)
	du.vx=Rnd(-2.5,2.5)
	du.vy=Rnd(-2.5,2.5)
Next

picked:thing=Null
Local pickx#,picky#
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())

	If Rand(1000)=1
		du:dude=dude.Create(Rnd(250,350),Rnd(250,250))
		du.vx=Rnd(-.1,.1)
		du.vy=Rnd(-.1,.1)
	EndIf

	For dr:drawable=EachIn drawables
		dr.draw
	Next
	
	If MouseDown(1)
		If Not picked
			pickx=MouseX()
			picky=MouseY()
			picked=thing.pick(pickx,picky)
		EndIf
		If asteroid(picked)
			distanceto picked.x,picked.y,MouseX(),MouseY(),dx#,dy#
			d#=Sqr(dx*dx+dy*dy)
			If d>0
				f#=.001/Sqr(d)
				dx:*f
				dy:*f
				DrawLine picked.x,picked.y,picked.x+dx*1000,picked.y+dy*1000
				picked.vx:+dx
				picked.vy:+dy
			EndIf
		EndIf
	Else
		picked=Null
	EndIf
	
	obstacles:TList=things.copy()
	While obstacles.count()
		t1:thing=thing(obstacles.removefirst())
		For t2:thing=EachIn obstacles
			t1.collide t2
		Next
	Wend
	
	For dr:drawable=EachIn drawables
		dr.move
		dr.update
	Next
	
	Flip
	Cls
Wend