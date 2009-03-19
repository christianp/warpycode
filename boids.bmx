Import warpy.gfx

Function dist#(x1# , y1# , x2# , y2#) 
	dx# = x2 - x1
	dy# = y2 - y1
	Return Sqr(dx * dx + dy * dy) 
End Function

Global things:TList
Type thing
	Field x# , y# , vx# , vy#
	Field radius#
	
	Function init() 
		things = New TList
	End Function
	
	Method New() 
		things.addlast Self
	End Method
	
	Method die() 
		things.remove Self
	End Method

	Method update() Abstract
	Method draw() Abstract
End Type

Global boids:TList
Type boid Extends thing
	Field nvx# , nvy#
	Field tspeed#
	Field red,green,blue
	Field neighbours
	Field dlim#

	Function init() 
		boids = New TList
	End Function
	
	Method New() 
		boids.addlast Self
		dlim=60
	End Method
	

	
	Method die()
		v# = Sqr(vx * vx + vy * vy) 
		an# = ATan2(vy , vx) 
		
		For c = 1 To Rand(2 , 5) 
			ean# = an + Rnd( - 50 , 50) 
			ev#=v*Rnd(0,1)
			evx# = Cos(ean) * ev
			evy#=Sin(ean)*ev
			explosion.Create x , y , evx , evy
		Next
		boids.remove Self
		Super.die() 
		
	End Method

	Method draw()
		Global bsize#=3
		angle# = ATan2(vy , vx) 
		x1# = x + Cos(angle) * bsize
		y1# = y + Sin(angle) * bsize
		x2# = x + Cos(angle + 120) * bsize
		y2# = y + Sin(angle + 120) * bsize
		x3# = x + Cos(angle - 120) * bsize
		y3# = y + Sin(angle - 120) * bsize
		
		SetColor red,green,blue
		
		drawzoomLine x1 , y1 , x2 , y2
		drawzoomLine x2 , y2 , x3 , y3
		drawzoomLine x3 , y3 , x1 , y1
		
		'DrawzoomText dlim,x,y
	End Method
	
	Method update() 
		x:+ vx
		y:+ vy
		'vx:* .95
		'vy:* .95
		
		nvx = vx
		nvy = vy
		tspeed#=3
		
		avoid
		
		dojob
		
		steer
	End Method
	
	Method dojob() Abstract

	
	Method steer() 
		oan# = ATan2(vy , vx) 
		nan# = ATan2(nvy , nvx)
		dp# = vy * nvy + vx * nvx
		
		ov# = Sqr(vx * vx + vy * vy)
		nv# = Sqr(nvx * nvx + nvy * nvy) 
		dp:/ (nv * ov) 
		If ov = 0 Then dp = 1
		'dp:*dp
		
		tspeed# :* (1 + dp)/2
		If ov > tspeed
			ov:+ (tspeed-ov)*.7
		Else
			ov:+ (tspeed - ov) * .01
			If Rand(50/(tspeed*tspeed)) = 1
				exhaust.Create x , y , - vx*.1 , - vy*.1
			endif
		EndIf
		'ov:* (1 + dp) * .5
		dan# = (nan - oan) Mod 360
		If dan < - 180 dan:+ 360
		If dan > 180 dan:- 360
		dan:*.1
		If Abs(dan) > 15 Then dan = Sgn(dan) * 15
		dan:+.1
		an# = oan + dan
		vx = Cos(an) * ov
		vy = Sin(an) * ov
		
		
	End Method
	
	Method avoid()
		Local ax# , ay#
		
		neighbours=0
		
		For t:thing = EachIn things
			If t <> Self And boid(t)
				dx# = t.x - x
				dy# = t.y - y
				d# = Sqr(dx * dx + dy * dy)-radius-t.radius
				If d < dlim
					neighbours:+1
					If d < 4 And boid(t)
						'Print("crash") 
						die
						If boid(t) boid(t).die
					EndIf
					'tspeed:*.8
					ad# = dlim-d
					
					ax:- dx*ad/d
					ay:-dy*ad/d
				EndIf
			EndIf
		Next
		
		v#=Sqr(vx*vx+vy*vy)
		nvx:+ ax*.15*v
		nvy:+ ay*.15*v
	End Method
	
	Method clump() 
		Local ax# , ay#, hx#,hy#
		
		If boids.count() < 2 Return
		
		tspeed:*.7
		
		Local clumpers
		
		For b:boid = EachIn boids
			dx# = b.x - x
			dy# = b.y - y
			d# = dx * dx + dy * dy
			If b <> Self And d<30000
				ax:+ b.x
				ay:+ b.y
				hx:+ b.vx
				hy:+b.vy
				clumpers:+1
			EndIf
		Next
		If clumpers
			ax:/ clumpers
			ay:/ clumpers
			ax:- x
			ay:- y
			a#=Sqr(ax*ax+ay*ay)*3
			hx:/ clumpers
			hy:/ clumpers
			h# = Sqr(hx * hx + hy * hy) * 3
			nvx:+ ax /a
			nvy:+ ay / a
			If h > 0
				nvx:+ hx / h
				nvy:+ hy / h
			endif
		Else
			tspeed=0
		EndIf
				
		If neighbours = 1
			v#=Sqr(vx*vx+vy*vy)
			tspeed = v * .9
			If tspeed<.5 tspeed=.5
		EndIf
	End Method
End Type

Type fetcher Extends boid
	Field targs:TList
	Field gotcargo
	Field jobfor:place
	
	
	Method New()
		targs = New TList
		radius = 3
		red = 255
		green = 255
		blue=255
	End Method
	
	Function Create:fetcher(x# , y#) 
		f:fetcher=New fetcher
		f.x = x
		f.y = y
		Return f
	End Function
	
	Method die()
		If jobfor
			jobfor.needs:+ 1
		EndIf
		Super.die
	End Method
	
		
	Method dojob() 
		If Not jobfor And Rand(500)=1 picktarget
		If targs.count()
			gotarget
			dlim=60
		Else
			dlim=100
			v# = vx * vx + vy * vy
			'tspeed=Sqr(v)*.8
			clump
			If Rand(10)=1 picktarget
		EndIf
	End Method
	
		
	Method gotarget()
		
		targ:place=place(targs.last())
		
		dx# = targ.x - x
		dy# = targ.y - y
		angle# = ATan2(dy , dx)
		aggro#=6
		nvx:+ Cos(angle) *aggro
		nvy:+ Sin(angle)*aggro
		
		d# = dx * dx + dy * dy
		If d < 2500
			tspeed:* Sqr(d / 2500)
			
			If d < 100
				reachtarget
			EndIf
		EndIf
		

	End Method
	
	Method picktarget()
		l:TList = New TList
		For p:place = EachIn needers
			If dist(x,y,p.x,p.y)<=p.needr l.addlast p
		Next
		If Not l.count() Return
		n = Rand(0 , l.count() - 1)
		targ:place = place(l.valueatindex(n) ) 
		
		targs.addlast targ
	End Method
	
	Method addtarget(ptargs:TList) 
		mindist# = - 1
		closest:place = Null
		For p:place = EachIn ptargs
			dx# = p.x - x
			dy# = p.y - y
			d# = dx * dx + dy * Dy
			If d < mindist Or mindist = - 1
				closest = p
				mindist=d
			EndIf
		Next
		If closest
			targs.addlast closest
		EndIf
	End Method
	
	Method reachtarget() 
		targ:place = place(targs.removelast() ) 
		
		If quarry(targ) 
			'got resource
			gotcargo=1
		ElseIf fort(targ)
			If gotcargo
				fort(targ).build
				gotcargo = 0
				jobfor=Null
			ElseIf takejob(targ)
				targs.addlast targ
				addtarget quarries
			EndIf
		EndIf
	End Method
	
	Method takejob(targ:place) 
		If targ.jobtaken() 
			jobfor = targ
			Return 1
		EndIf
	End Method

End Type

Global places:TList
Global needers:TList
Type place Extends thing
	Field x# , y#
	Field needs,needr#
	
	Function init() 
		places = New TList
		needers = New TList
		
		quarry.init
		fort.init
	End Function
	
	Method New() 
		places.addlast Self
		radius=7
	End Method
	
	Function doneeders() 
		needers = New TList
		For p:place = EachIn places
			If p.needs > 0
				needers.addlast p
				p.needr:+.8
			EndIf
		Next
	End Function
	
	Method jobtaken()
		If needs
			needs:- 1
			Return 1
		EndIf
	End Method
	
End Type

Global quarries:TList
Type quarry Extends place
	Field an#,speed#
	
	Function init() 
		quarries = New TList
	End Function
	
	Method New() 
		quarries.addlast Self
		an = Rand(360) 
		speed=Rnd(.1,1)
	End Method
	
	Function Create:quarry(x# , y#) 
		q:quarry = New quarry
		q.x = x
		q.y = y
		Return q
	End Function
	
	Method update()
		an:+ Rnd( - 1 , 2) 
		x:+ Cos(an) * speed
		y:+ Sin(an) * speed
		
	End Method
	
	Method draw()
		SetColor 100,50,50
		drawzoomRect x - 5 , y - 5 , 10 , 10
	End Method
End Type

Global forts:TList=New TList
Type fort Extends place
	Field built#
	
	Function init() 
		forts = New TList
	End Function
	
	Method New() 
		forts.addlast Self
		needs=5
	End Method
	
	Function Create:fort(x# , y#) 
		f:fort=New fort
		f.x = x
		f.y = y
		Return f
	End Function
	
	Method update()
	End Method
	
	Method build() 
		built:+ .2
	End Method
	
	Method draw()
		SetColor 0 , 0 , 100
		SetAlpha built
		drawzoomRect x - 5 , y - 5 , 10 , 10
		SetAlpha 1 - built
		SetColor 100 , 0 , 0
		drawzoomrect x-5,y-5,10,10,1,0
		'drawzoomLine x - 5 , y - 5 , x - 5 , y + 5
		'drawzoomLine x - 5 , y - 5 , x + 5 , y - 5
		'drawzoomLine x - 5 , y + 5 , x + 5 , y + 5
		'drawzoomLine x + 5 , y - 5 , x + 5 , y + 5
		SetAlpha 1
		
		If needs
			SetAlpha .3
			drawzoomshell x , y , needr, needr/10
			SetAlpha 1
		EndIf
	End Method
	
End Type

Global puffs:TList
Type puff Extends thing
	Field life#
	Field radius#
	
	Function init() 
		puffs = New TList
	End Function
	
	Method New() 
		puffs.addlast Self
	End Method
	
	Function Create:puff(x# , y# , vx# , vy#) 
		e:puff = New puff
		e.x = x
		e.y = y
		e.vx = vx
		e.vy = vy
		e.radius = Rnd(1 , 5) 
		Return e
	End Function
	
	Method update() 
		x:+ vx
		y:+ vy
		life:+ .03
		If life > 1
			die
		EndIf
	End Method
	
	Method die() 
		puffs.remove Self
		Super.die
	End Method
	
	Method draw() 
		SetColor 255 , 0 , 0
		SetAlpha 1-life
		r# = Sin(180 * life) * radius
		drawzoomcircle x,y,r
		SetAlpha 1
	End Method
End Type

Type explosion Extends puff
	Function Create:explosion(x# , y# , vx# , vy#) 
		e:explosion = New explosion
		e.x = x
		e.y = y
		e.vx = vx
		e.vy = vy
		e.radius = Rnd(1 , 5) 
		Return e
	End Function
	Method draw() 
		SetColor 255 , 0 , 0
		SetAlpha 1-life
		r# = Sin(180 * life) * radius
		drawzoomcircle x,y,r
		SetAlpha 1
	End Method
End Type

Type exhaust Extends puff
	Function Create:exhaust(x# , y# , vx# , vy#) 
		e:exhaust = New exhaust
		e.x = x
		e.y = y
		e.vx = vx
		e.vy = vy
		e.radius = Rnd(.5 , 3) 
		Return e
	End Function
	Method draw() 
		SetColor 245 , 245 , 245
		SetAlpha (1-life)*.5
		r# = Sin(180 * life) * radius
		drawzoomcircle x,y,r
		SetAlpha 1
	End Method
End Type

thing.init
place.init
boid.init
puff.init
AppTitle = "Twoot"
initgfx 600,600
SetClsColor 132,112,255

For c = 1 To 0
	fetcher.Create(Rand(600) , Rand(600) ) 
Next

fort.Create 300 , 300
quarry.Create 50 , 50
quarry.Create 550 , 550
quarry.Create - 200 , 800
quarry.Create 800 , - 200


Global holdx# , holdy#
Global mx,my,zmx,zmy,mz=MouseZ()

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate() ) 
	
	omz = mz
	mz = MouseZ() 
	dmz = mz - omz
	DrawText mz,0,15
	zoom:*(1+dmz*.1)
	
	mx = MouseX()
	my = MouseY() 
	zmx = unzoomx(mx) 
	zmy=unzoomy(my)
	
	
	If boids.count()<200 And Rand(50) = 1
		an = Rand(360) 
		r=Rand(600,1000)
		fetcher.Create 300+Cos(an)*r,300+Sin(an)*r
	EndIf
	
	If MouseHit(1) 
		fort.Create(zmx , zmy ) 
	EndIf
	
	If MouseDown(2) 
		panx = holdx-mx
		pany = holdy- my 
	Else
		holdx = panx+mx
		holdy = pany+my
	EndIf
	
	place.doneeders
	
	For p:place = EachIn places
		p.update
	Next
	
	For b:boid = EachIn boids
		b.update
	Next
	
	For e:puff = EachIn puffs
		e.update
	Next

	For p:place = EachIn places
		p.draw
	Next
	
	For b:boid = EachIn boids
		b.draw
	Next
	
	For e:puff = EachIn puffs
		e.draw
	Next
	
	DrawText boids.count() + " boids" , 0 , 0
	DrawText "left click to construct new fort",0,585
	
	Flip
	Cls
Wend