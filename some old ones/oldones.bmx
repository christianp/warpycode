Import "graphics.bmx"

Global things:TList=New TList
Type thing
	Field x#,y#,vx#,vy#,r#,m#,ox#,oy#
	Field x2#,y2#,vx2#,vy2#
	Field cor#,cof#	'coefficient of restitution, friction
	Field red,green,blue	'colour
	
	Method New()
		things.addlast Self
		m=1
		r=1
		cor=1
		cof=0
	End Method
	
	Method checkcollide(t:thing)
	
		'quick bounding box check
		If Not (x-r<t.x+t.r And x+r>t.x-t.r And y-r<t.y+t.r And y+r>t.y-t.r)
			Return False
		EndIf

		'find difference between new positions
		dx#=x-t.x
		dy#=y-t.y
		d#=dx*dx+dy*dy
		tr#=r+t.r
		
		If d=0 Return False
		
		'if radii overlap
		If d<tr*tr
		
			'find difference in old positions
			odx#=ox-t.ox
			ody#=oy-t.oy
			dvx#=vx-t.vx
			dvy#=vy-t.vy
			
			'find time of collision
			a#=dvx*dvx+dvy*dvy
			b#=2*dvx*odx+2*dvy*ody
			c#=odx*odx+ody*ody-tr*tr
			If a=0 Return False
			If (b*b-4*a*c)<0 Return False
			time#=(-b-Sqr(b*b-4*a*c))/(2*a)
			
			If time<0 time=(-b+Sqr(b*b-4*a*c))/(2*a)
			If Abs(time)>1	'something mad has gone on, such as both objects beginning the frame overlapping
				d=Sqr(d)
				diff#=tr-d
				dx:/d
				dy:/d
				f#=diff*t.m/(m+t.m)
				f2#=diff*m/(m+t.m)
				mx#=dx*f
				my#=dy*f
				mx2#=-dx*f2
				my2#=-dy*f2
				x2:+mx
				y2:+my
				vx2:+mx
				vy2:+my
				t.x2:+mx2
				t.y2:+my2
				t.vx2:+mx2
				t.vy2:+my2
				Return True
			EndIf
			
			'find positions of objects at time of collision
			nx#=ox+time*vx
			ny#=oy+time*vy
			nx2#=t.ox+time*t.vx
			ny2#=t.oy+time*t.vy
			
			'find vector between objects at collision
			dx=nx-nx2
			dy=ny-ny2
			d=tr
			
			
			If d=0 Return False	'just in case#
			dx:/d
			dy:/d
			
			'work out COR of collision (guess)
			rcor#=(m*cor+t.m*t.cor)/(m+t.m)
			
			'find components of velocity towards collision normal and tangential
			in#=vx*dx+vy*dy
			in2#=t.vx*dx+t.vy*dy
			n#=-vx*dy+vy*dx
			n2#=-t.vx*dy+t.vy*dx
			
			'find new velocities in direction of normal after collision
			nin#=(m*in+t.m*in2+t.m*rcor*(in2-in))/(m+t.m)
			nin2#=(m*in+t.m*in2+m*rcor*(in-in2))/(m+t.m)
			
			Rem
			'work out friction and apply to tangential velocity
			rcof#=(cof+t.cof)/(m+t.m)
			rcof=0
			f#=(in2-in)*rcof
			nn#=-Sgn(n)*f/m
			nn:+n
			If nn*n<0 nn=0
			
			nn2#=-Sgn(n2)*f/t.m
			nn2:+n2
			If nn2*n2<0 nn2=0
			EndRem
			
			'combine to give new velocities
			nvx#=nin*dx-n*dy
			nvy#=nin*dy+n*dx
			
			nvx2#=nin2*dx-n2*dy
			nvy2#=nin2*dy+n2*dx
			
			'add remaining time's movement using new velocities
			nx:+(1-time)*nvx
			ny:+(1-time)*nvy
			nx2:+(1-time)*nvx2
			ny2:+(1-time)*nvy2
			
			
			
			x2:+nx-x
			y2:+ny-y
			vx2:+nvx-vx
			vy2:+nvy-vy
			
			t.x2:+nx2-t.x
			t.y2:+ny2-t.y
			t.vx2:+nvx2-t.vx
			t.vy2:+nvy2-t.vy
			
			Rem
			x=nx
			y=ny
			t.x=nx2
			t.y=ny2
			vx=nvx
			vy=nvy
			t.vx=nvx2
			t.vy=nvy2
			EndRem
			
			f#=Abs(in-in2)
			hit(t,f)
			t.hit(Self,f)
			
			Return True
		EndIf
	End Method
	
	Method supdate()
	
		x=x2
		y=y2
		vx=vx2
		vy=vy2
		
		update	'inheritor-specific updates
	
	
		'drag back into origin, to avoid escape velocity type affairs
		vx:-x*.000001
		vy:-y*.000001
	
		ox=x
		oy=y
		x:+vx
		y:+vy
		
		Rem
		If x<r
			time#=(r-ox)/vx
			vx=-vx*cor
			x=r+(1-time)*vx
		EndIf
		If x>600-r
			time#=(600-r-ox)/vx
			vx=-vx*cor
			x=600-r+(1-time)*vx
		EndIf
		If y<r
			time#=(r-oy)/vy
			vy=-vy*cor
			y=r+(1-time)*vy
		EndIf
		If y>600-r
			time#=(600-r-oy)/vy
			vy=-vy*cor
			y=600-r+(1-time)*vy
		EndIf
		EndRem
		
		x2=x
		y2=y
		vx2=vx
		vy2=vy
	End Method
	
	Method update()
	End Method
	
	Method sdraw()
		SetColor red,green,blue

		SetAlpha .4
		DrawZoomCircle x,y,r
		
		SetAlpha 1
		draw	'inheritor-specific draw
		
	End Method
	
	Method draw()
	End Method
	
	
	
	Method hit(t:thing,f#)
	End Method
	
End Type

Type dust Extends thing
	Field col#

	Method New()
		col=Rand(360)
	End Method

	Function Create:dust(x#,y#,m#,r#)
		d:dust=New dust
		d.x2=x
		d.y2=y
		d.m=m
		d.r=r
		d.cor=.3
		d.cof=cof
		Return d
	End Function
	
	Method update()
		col:+.5
		red=Sin(col)*100+100
		green=Cos(col*2)*100+100
		blue=Cos(col*3)*100+100
	End Method
	
	Method draw()
		DrawZoomCircle x,y,r*.5
	End Method
End Type

Type ship Extends thing
	Field an#,van#
	'Field h:hook
	
	Method New()
		r=10
		m=3
		cor=.7
		red=255
		green=255
		blue=255
		
		'h=New hook
		'h.t=Self
		'h.l=30
	End Method
	
	Function Create:thing(x#,y#)
		s:ship=New ship
		s.x2=x
		s.y2=y
		Return s
	End Function
	
	Method update()
		If MouseHit(1)
			sink.Create unzoomx(MouseX()),unzoomy(MouseY())
		EndIf

		dx#=(KeyDown(KEY_RIGHT)-KeyDown(KEY_LEFT))
		dy#=(KeyDown(KEY_DOWN)-KeyDown(KEY_UP))
		drawzoomline x,y,x+dx*50,y+dy*50
		d#=dx*dx+dy*dy
		wan#=ATan2(dy,dx)
		dan#=andiff(wan,an)
		DrawText dan,0,45
		If d van:+dan*.01
		thrust#=Cos(dan)*.1*Sgn(d)
	
		an:+van
		'van:+(KeyDown(KEY_RIGHT)-KeyDown(KEY_LEFT))*1
		van:*.9
		'an:+van
		
		'thrust#=(KeyDown(KEY_UP)-KeyDown(KEY_DOWN))*.1
		
		vx:+Cos(an)*thrust
		vy:+Sin(an)*thrust
		
		
		'h.on=KeyDown(KEY_SPACE)
		'h.update
		
	End Method
	
	Method draw()
		Local poly#[6]
		poly=[Float(x+Cos(an)*10),Float(y+Sin(an)*10),Float(x+Cos(an+150)*10),Float(y+Sin(an+150)*10),Float(x+Cos(an-150)*10),Float(y+Sin(an-150)*10)]
		DrawZoomPoly poly
		
		p#=Sqr(x*x+y*y)
		drawzoomline x,y,x-50*x/p,y-50*y/p
		
		'h.draw
	End Method
	
	Method hit(t:thing,f#)
		If dust(t)
			Rem
				things.remove t
				SetColor 255,255,255
				DrawOval t.x-20,t.y-20,40,40
				
				t.r:*.7
				If t.r>5
					rr#=1
					While rr>0
						s#=Min(rr,Rnd(.2,.5))
						rr:-s
						nan#=Rand(360)
						nr#=Sqr(s)*t.r
						d2:dust=dust.Create(t.x+Cos(nan)*2*nr,t.y+Sin(nan)*2*nr,s*t.m,nr)
						'f#=Rnd(.01,.02)
						'd2.vx2=t.vx2+Cos(nan)*f
						'd2.vy2=t.vy2+Sin(nan)*f
					Wend
				EndIf
			EndRem
		EndIf
	End Method
		
End Type


Type world Extends thing
	Field hx#,hy#

	Method New()
		col=Rand(1,7)
		If (col & 1) red=255
		If (col & 2) green=255
		If (col & 4) blue=255
		cor=.5
	End Method

	Function Create:world(x#,y#,m#,r#)
		w:world=New world
		w.x2=x
		w.y2=y
		w.hx=x
		w.hy=y
		w.r=r
		w.m=m
		Return w
	End Function
	
	Method update()
		'vx:+(hx-x)*.001
		'vy:+(hy-y)*.001
		For w:world=EachIn things
			If w<>Self
				dx#=w.x2-x2
				dy#=w.y2-y2
				d#=Sqr(dx*dx+dy*dy)
				dx:/d
				dy:/d
				d=d-r-w.r
				If d<500
					f#=.3/d
					vx:-dx*f
					vy:-dy*f
				EndIf
			EndIf
		Next
	End Method
	
	Method draw()
		DrawZoomRect x-r/2,y-r/2,r,r
		For w:world=EachIn things
			If w<>Self
				drawzoomline x,y,w.x,w.y
			EndIf
		Next
	End Method
End Type

Type bag Extends thing
	Field owner:thing
	Field contents:TList
	
	Method New()
		r=10
		m=5
		cor=0
	End Method
End Type

Type sink Extends thing
	Field up
	
	Method New()
		up=1
		r=1
		m=1
		cor=0
	End Method
	
	
	Function Create:sink(x#,y#)
		s:sink=New sink
		s.x2=x
		s.y2=y
		Return s
	End Function
	
	Method update()
		If up
			r:+1
			m:+2
			If r=30 up=0
		Else
			r:*.99
			m:*.99
			If r<5
				things.remove Self
			EndIf
		EndIf
		
		b#=r/30
		red=Rand(255)*b
		green=Rand(255)*b
		blue=Rand(255)*b
	End Method
End Type

Rem
Type hook
	Field t:thing
	Field kids:TList,owner:hook
	Field on
	Field l#
	Field age
	
	Method New()
		kids=New TList
		owner=Self
		l=30
	End Method
	
	Method grab()
		If kids.count()>=2 Return
		For du:dust=EachIn things
			If Not owner.contains(du)
				
				dx#=du.x-t.x
				dy#=du.y-t.y
				d#=Sqr(dx*dx+dy*dy)
				If d-t.r-du.r<l
					h:hook=New hook
					h.owner=owner
					h.l=l
					kids.addlast h
					h.t=du
					If kids.count()=2 Return
				EndIf
			EndIf
		Next
	End Method
	
	Method contains(o:thing)
		If t=o Return True
		For h:hook=EachIn kids
			If h.contains(o) Return True
		Next
		Return False
	End Method
	
	Method update()
		If age=120
			If on grab
		Else
			age:+1
		EndIf
		
		For h:hook=EachIn kids
			dx#=t.x-h.t.x
			dy#=t.y-h.t.y
			d#=Sqr(dx*dx+dy*dy)
			'If d-t.r-h.t.r>l
				dx:/d
				dy:/d
				f#=(d-t.r-h.t.r-l)
				f=.001*f*Abs(f)/l
				h.t.vx2:+f*dx*t.m/(t.m+h.t.m)
				h.t.vy2:+f*dy*t.m/(t.m+h.t.m)
				t.vx2:-f*dx*h.t.m/(t.m+h.t.m)
				t.vy2:-f*dy*h.t.m/(t.m+h.t.m)
			'EndIf		
			
			h.on=on
			h.update
		Next
			
	End Method
	
	Method draw()
		SetColor 255,255,255
		SetAlpha .5*age/120.0
		For h:hook=EachIn kids
			DrawzoomLine t.x,t.y,h.t.x,h.t.y
			h.draw
		Next
		If on
			SetAlpha .3
			drawzoomcircle t.x,t.y,t.r+30
		EndIf
	End Method
End Type
EndRem


Global me:thing
Function setup()
	things=New TList
	me:thing=ship.Create(100,40)
	'thing.Create(300,300,9,30,.5,0)
	'dust.Create(100,500,.1,50)
	
	For c=1 To 5
		pr=Rand(500,1000)
		an=c*360/5
		r#=Rand(50,100)
	'	world.Create Cos(an)*pr,Sin(an)*pr,r/4,r
	Next
	'world.Create 0,0,15,60
	
	
	For c=1 To 100
		makedust
	Next
	
End Function

Function makedust()
	in=1
	While in
		pr=Rand(100,1000)
		an=Rand(360)
		x=Cos(an)*pr
		y=Sin(an)*pr
		in=0
		r=Rnd(5,10)
		For t:thing=EachIn things
			dx#=x-t.x
			dy#=y-t.y
			d#=dx*dx+dy*dy
			If d<=(t.r+r)*(t.r+r)*1.1
				in=1
				Exit
			EndIf
		Next
	Wend
	dust.Create(x,y,.01*r,r)
End Function

AppTitle="some old ones"
gwidth=600
gheight=600
Graphics 600,600,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()
'SetClsColor 255,255,255
setup

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())

	px#=zoomx(me.x)
	py#=zoomy(me.y)
	frame=200
	If px<frame
		panx:+(px-frame)*.1
	EndIf
	If px>gwidth-frame
		panx:+(px-(gwidth-frame))*.1
	EndIf
	If py<frame
		pany:+(py-frame)*.1
	EndIf
	If py>gheight-frame
		pany:+(py-(gheight-frame))*.1
	EndIf
	
	'If KeyHit(KEY_SPACE) setup
	
	'If Rand(50)=1
	'	'makething
	'EndIf

	For t:thing=EachIn things
		t.supdate
	Next
	
	
	ts:TList=things.copy()
	While ts.count()>1
		t:thing=thing(ts.removefirst())
		For t2:thing=EachIn ts
			If t.checkcollide(t2)
				'
			Else
				'
			EndIf
		Next
	Wend
	
	ts=things.copy()
	While ts.count()>1
		t:thing=thing(ts.removefirst())
		For t2:thing=EachIn ts
			dx#=t2.x-t.x
			dy#=t2.y-t.y
			d#=Sqr(dx*dx+dy*dy)
			If d<>0
				f#=4/(d*d)
				t.vx2:+t2.m*f*dx/d
				t.vy2:+t2.m*f*dy/d
				t2.vx2:-t.m*f*dx/d
				t2.vy2:-t.m*f*dy/d
			EndIf
		Next
	Wend
		
	For t:thing=EachIn things
		t.sdraw
	Next
	
	DrawText things.count(),0,0

	Flip
	Cls
Wend