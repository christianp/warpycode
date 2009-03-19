Const gravity#=.001


Global grandcircles:tlist=New tlist
Type grandcircle
	Field radius#
	Field an#
	Field van#
	Field weights:tlist
	Field cx# , cy#
	Field upramps:tlist
	Field downramps:tlist
	
	Method New()
		grandcircles.addlast Self
		weights = New tlist
		upramps = New tlist
		downramps=New tlist
	End Method
	
	Function create:grandcircle(radius#)
		gc:grandcircle = New grandcircle
		gc.radius = radius
		Return gc
	End Function
	
	Method update()
		an:+ van
		van:* .99
		
		For w:weight = EachIn weights
			f# = w.mass * gravity * Sin(w.an - an+180)
			van:+f
		Next
		
		For h:hamster = EachIn hamsters
			If h.mycircle=Self
				f# = h.mass * gravity * Sin(h.an - an) * h.pradius/radius
				van:+ f
			EndIf
		Next
		
	End Method
	
	Method Draw()
		cx# = gwidth / 2
		cy# = gheight/2
		SetColor 0 , 0 , 0
		SetAlpha .2
		DrawOval cx - radius , cy - radius , 2 * radius , 2 * radius
		For spokean = 0 To 360 Step 60
			ex# = cx + radius * Cos(spokean + an)
			ey# = cy + radius * Sin(spokean + an)
			DrawLine ex , ey , cx , cy
		Next
		SetColor 255,255,255
		SetAlpha 1
		
		For w:weight = EachIn weights
			x# = cx + radius*Sin(w.an - an)
			y# = cy + radius * Cos(w.an - an)
			w.draw(x,y)
		Next
		
		Rem
		x# = cx
		y# = cy + radius
		For a# = 0 To 180
			r#=radius*(.25*Cos(a)+.75)
			ex# = cx + r * Sin(a)
			ey# = cy + r * Cos(a)
			DrawLine x , y , ex , ey
			x = ex
			y = ey
		Next
		EndRem
	
	End Method
End Type

Global ramps:tlist=New tlist
Type ramp
	Field mycircle:grandcircle
	Field endcircle:grandcircle
	Field an# , anwide#
	Field rdiff#
	
	Method New()
		ramps.addlast Self
	End Method
	
	Function create:ramp(gc:grandcircle , egc:grandcircle , an1# , an2#)
		r:ramp = New ramp
		r.mycircle = gc
		gc.upramps.addlast r
		r.endcircle = egc
		egc.downramps.addlast r
		r.an = an1
		r.anwide = an2
		
		r.rdiff# = r.mycircle.radius - r.endcircle.radius
		
		Return r
	End Function
	
	Method Draw()
		SetAlpha .3
		For h:hamster = EachIn hamsters
			If h.mycircle = mycircle Or h.mycircle=endcircle
				SetAlpha 1
			EndIf
		Next

		x# = mycircle.cx + mycircle.radius*Sin(an-mycircle.an)
		y# = mycircle.cy + mycircle.radius*Cos(an-mycircle.an)
		
		For t# = 0 To 1 Step .01
			r#=radius(t)
			ex# = mycircle.cx + r * Sin(an+t*anwide-mycircle.an)
			ey# = mycircle.cy + r * Cos(an+t*anwide-mycircle.an)
			DrawLine x , y , ex , ey
			x = ex
			y = ey
		Next
	End Method	
	
	Method radius#(t#)
		r# = endcircle.radius + rdiff * .5 * (Cos(t * 180) + 1)
		Return r
	End Method
	
	Method endan#()
		Return an + anwide - mycircle.an + endcircle.an
	End Method
End Type

Global allweights:tlist=New tlist
Type weight
	Field mass#
	Field an#
	Field x#,y#
	
	Method New()
		allweights.addlast Self
	End Method
	
	Function create:weight(mass#,an#)
		w:weight = New weight
		w.mass = mass
		w.an=an
		Return w
	End Function
	
	Method Draw(dx# , dy#)
		x = dx
		y = dy
		size#=mass*2
		DrawRect x - size / 2 , y - size / 2 , size , size
	End Method
End Type

Global hamsters:tlist=New tlist
Type hamster
	Field mycircle:grandcircle
	Field myramp:ramp
	Field radius#
	Field penheight#
	Field an#
	Field spin#
	Field v#
	Field mass#
	Field runspeed#
	Field weights:tlist
	Field otan#,otr#,otwist#
	Field van#
	Field pradius#
	Field getonrampan#
	
	Method New()
		hamsters.addlast Self
		weights=New tlist
	End Method
	
	Function create:hamster(gc:grandcircle , radius#,an#=0,runspeed#=2)
		h:hamster = New hamster
		h.mycircle = gc
		h.radius = radius
		h.an = an
		h.runspeed = runspeed
		h.otan = h.an
		h.otr=gc.radius-h.radius*.5
		Return h
	End Function
	
	Method Draw()
		drawan#=an-mycircle.an
		x# = mycircle.cx + (pradius-radius)*Sin(drawan)
		y# = mycircle.cy + (pradius - radius) * Cos(drawan)
		'DrawText v,x,y
		SetColor 0 , 0 , 255
		SetAlpha .5
		DrawOval x - radius , y - radius , 2 * radius , 2 * radius
		SetAlpha .7
		For spokean = 0 To 360 Step 60
			ex# = x + Sin(spokean+spin) * radius
			ey# = y + Cos(spokean + spin) * radius
			'DrawText spokean , ex , ey
			'DrawText Int(niceangle(spokean+spin)),ex,ey+12
			DrawLine x,y,ex,ey
		Next
		SetAlpha 1
		SetColor 255 , 255 , 255
		
		For w:weight = EachIn weights
			wx# = x + Sin(w.an + spin) * radius
			wy# = y + Cos(w.an + spin) * radius
			w.draw(wx , wy)
		Next
		
	End Method
	
	Method update()
		
		If KeyDown(KEY_LEFT)
			v:+ runspeed/mass
		ElseIf KeyDown(KEY_RIGHT)
			v:- runspeed/mass
		EndIf
		
		If myramp
			SetColor 255 , 0 , 0
			myramp.draw()
			SetColor 255 , 255 , 255
			t# = (an - getonrampan) / myramp.anwide
			opradius# = pradius
			pradius# = myramp.radius(t)
			v:* opradius / pradius
			DrawText t,0,24
			If t >= 1
				mycircle = myramp.endcircle
				an=an-myramp.mycircle.an+myramp.endcircle.an
				myramp = Null
				otan=an
			EndIf
			If t <= 0
				myramp = Null
			EndIf
		EndIf
		If myramp=Null
			
			pradius# = mycircle.radius
			
			If KeyDown(KEY_UP)
				For r:ramp = EachIn mycircle.upramps
					If rollover(niceangle(r.an-an)) And v*r.anwide>0
						myramp = r
						getonrampan#=an
					EndIf
				Next						
			EndIf
			
			If KeyDown(KEY_DOWN)
				For r:ramp = EachIn mycircle.downramps
					If rollover(niceangle(r.endan() - an) ) And v * r.anwide < 0
						myramp = r
						mycircle = r.mycircle
						an# = an + mycircle.an - r.endcircle.an
						getonrampan# = an - r.anwide
					EndIf
				Next
			EndIf
			
			
			If KeyDown(KEY_SPACE)
				For w:weight = EachIn mycircle.weights
					wan# = niceangle(w.an - an)
					If rollover(wan)
						mycircle.weights.remove w
						w.an# = -spin + wan * radius / mycircle.radius
						weights.addlast w
					EndIf
				Next
			Else
				For w:weight = EachIn weights
					wan# = niceangle(w.an + spin)
					If rollunder(wan)
						weights.remove w
						mycircle.weights.addlast w
						w.an = an
					EndIf
				Next
			EndIf
	
		EndIf
		
		mass# = radius
		For w:weight = EachIn weights
			mass:+ w.mass
		Next
		
		v:-gravity*radius*Sin(an-mycircle.an)
		v:*.99
		an:+v
		
		van# = - v * pradius / radius
		spin#:+ van
		
		penheight# = radius * .5
		ty# = penheight * Cos(spin)
		tx# = penheight * Sin(spin)
		s1# = pradius - radius + ty
		andiff# = ATan2(tx , s1)
		DrawText s1,0,12
		trail.create(mycircle , otan , otr , otwist , andiff + an , s1 , spin)
		otan = andiff + an
		otr = s1
		otwist = spin
		
		
		
		DrawText allweights.count()-weights.count(),0,0
	End Method
	
	Method rollover(wan#)
		If wan / v < 1 And wan/v>=0
			Return 1
		EndIf
		Return 0
	End Method
	
	Method rollunder(wan#)
		'DrawText Int(wan),w.x,w.y
		If wan / van < 1 And wan / van >= 0
			Return 1
		EndIf
		Return 0
	End Method
End Type

Global trails:tlist=New tlist
Type trail
	Field mycircle:grandcircle
	Field an1#,an2#
	Field r1#,r2#
	Field nxt:trail
	Field life#
	Field twist1# , twist2#
	Field width#
		
	Method New()
		trails.addlast Self
		life#=1
	End Method
	
	Function create:trail(gc:grandcircle , an1# , r1# , twist1# , an2# , r2# , twist2# , width#=10)
		t:trail = New trail
		t.mycircle = gc
		t.an1 = an1
		t.r1 = r1
		t.twist1 = twist1
		t.an2 = an2
		t.r2 = r2
		t.twist2 = twist2
		t.width=width
		Return t
	End Function
	
	Method Draw()
		dan1# = an1 - mycircle.an
		dan2# = an2 - mycircle.an
		SetAlpha life*.5
		x1# = mycircle.cx + r1 * Sin(dan1)
		y1# = mycircle.cy + r1 * Cos(dan1)
		x2# = mycircle.cx + r2 * Sin(dan2)
		y2# = mycircle.cy + r2 * Cos(dan2)
		
		'dtwist1#=twist1-mycircle.an
		'dtwist2#=twist2-mycircle.an
		dtwist1# =-twist1 - dan1
		dtwist2#=-twist2-dan2
		
		x1a# = x1 + width * Cos(dtwist1)
		y1a# = y1 + width * Sin(dtwist1)
		x1b# = x1 - width * Cos(dtwist1)
		y1b# = y1 - width * Sin(dtwist1)
		x2a# = x2 + width * Cos(dtwist2)
		y2a# = y2 + width * Sin(dtwist2)
		x2b# = x2 - width * Cos(dtwist2)
		y2b# = y2 - width * Sin(dtwist2)
		
		Local poly#[6]
		poly = [x1a , y1a , x1b , y1b , x2b , y2b]
		'DrawPoly poly
		poly = [x1a , y1a , x2a , y2a , x2b , y2b]
		'DrawPoly poly
		DrawLine x1a , y1a , x1b , y1b
		DrawLine x1a , y1a , x2a , y2a
		DrawLine x1b , y1b , x2b , y2b
		
	End Method
	
	Method update()
		life:- .01
		If life <= 0
			trails.remove Self
		EndIf
	End Method
End Type
	

Function niceangle#(an#)
	an = an Mod 360
	If an > 180 an:- 360
	If an < - 180 an:+ 360
	Return an
End Function

AppTitle="Hamstrograph"
Const gheight = 800
Const gwidth = 800
Graphics gheight , gwidth , 0
SetBlend ALPHABLEND
SetClsColor 180 , 180 , 180
Cls

gc:grandcircle = grandcircle.create(350)
For i = 1 To 15
	gc.weights.addlast(weight.create(Rnd(4,10),Rand(180,210)))
Next

egc:grandcircle = grandcircle.create(180)

For i = 1 To 5
	egc.weights.addlast(weight.create(5 , i * 72) )
Next


ramp.create(gc , egc , 180 , 180) 
ramp.create(gc,egc,0,180)

ham:hamster=hamster.create(gc,60)

While Not KeyHit(KEY_ESCAPE)
	For gc:grandcircle = EachIn grandcircles
		gc.update()
		gc.draw()
	Next
	
	For r:ramp = EachIn ramps
		r.draw()
	Next
	
	For t:trail = EachIn trails
		t.draw()
		t.update()
	Next
	SetAlpha 1 'from trails

	For h:hamster = EachIn hamsters
		h.update()
		h.draw()
	Next
	
	
	gc=grandcircle(grandcircles.first())
	
	Flip
	Cls
Wend