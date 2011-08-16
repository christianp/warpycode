Const screenheight=800,screenwidth=800
Graphics screenwidth,screenheight,0
SetBlend ALPHABLEND
SetClsColor 100,100,100
AutoMidHandle True

Global santas:TList=New TList
Global swings:TList=New TList
Global stretches:TList=New TList
Global polys:TList=New TList

Global santabody=LoadImage("santabody.png")
Global santaleg=LoadImage("santaleg.png")
Global santaleftarm=LoadImage("santaleftarm.png")
Global santarightarm=LoadImage("santarightarm.png")
Global santahead=LoadImage("santahead.png")
Global santahat=LoadImage("santahat.png")
Global santabobble=LoadImage("santabobble.png")

SetImageHandle santaleg,19,10
SetImageHandle santaleftarm,19,8
SetImageHandle santarightarm,19,8
'SetImageHandle santahat,36,2

Type santa
	Field x#,y#,vx#,vy#
	Field targx#,targy#
	Field van#,an#
	Field myswings:TList
	Field leftleg:swing,rightleg:swing,leftarm:swing,rightarm:swing,head:swing
	Field hat:swing
	Field beard:stretch[7]
		
	Function create:santa(x#,y#)
		'create santa
		s:santa=New santa
		santas.addlast s
		s.myswings=New TList
		s.x=x
		s.y=y
		
		'make limbs
		s.leftleg=s.addswing(santaleg,-20,30,70)
		s.rightleg=s.addswing(santaleg,20,30,70)
		s.leftarm=s.addswing(santaleftarm,-35,-20,70)
		s.rightarm=s.addswing(santarightarm,35,-20,70)
		
		'make hat
		bobble:swing=swing.create(santabobble,0,0,9,.6,180,.5)
		s.hat=swing.create(santahat,0,-30,15,1,180)
		s.hat.targan=180
		scale#=1
		last:swing=s.hat
		For c=1 To 2
			scale=scale*.8
			w:swing=swing.create(santahat,0,0,scale*15,scale,180,1-scale)
			w.prv=last
			last=w
		Next
		s.head=s.addswing(santahead,0,-50,100)
		bobble.prv=last
		
		'make beard
		For c=0 To 2
			s.beard[c]=stretch.create(380+c*20,400,String(c))
			s.beard[c+3]=stretch.create(380+c*20,420,String(c+3))
		Next
		s.beard[6]=stretch.create(400,440,"6")
	
		s.beard[0].addlink(s.beard[1],20)
		s.beard[0].addlink(s.beard[3],20)
		s.beard[1].addlink(s.beard[2],20)
		s.beard[1].addlink(s.beard[4],20)
		s.beard[2].addlink(s.beard[5],20)
		s.beard[3].addlink(s.beard[4],20)
		s.beard[4].addlink(s.beard[5],20)
		s.beard[3].addlink(s.beard[6],25)
		s.beard[4].addlink(s.beard[6],20)
		s.beard[5].addlink(s.beard[6],25)

		poly.create([s.beard[0],s.beard[1],s.beard[4],s.beard[3]])
		poly.create([s.beard[1],s.beard[2],s.beard[5],s.beard[4]])
		poly.create([s.beard[3],s.beard[4],s.beard[6]])
		poly.create([s.beard[4],s.beard[5],s.beard[6]])
		
		
		Return s
	End Function
	
	Method addswing:swing(image,x,y,length)
		w:swing=swing.create(image,x,y,length)
		myswings.addlast w
		placeswing(w)
		Return w
	End Method
		
	Method placeswing(w:swing)
		w.px=transformx(w.ox,w.oy,x,y,an)
		w.py=transformy(w.ox,w.oy,x,y,an)
	End Method
End Type


Function transformx#(tx#,ty#,x#,y#,an#)
	Return x+Cos(an)*tx+Cos(an+90)*ty
End Function
	
Function transformy#(tx#,ty#,x#,y#,an#)
	Return y+Sin(an)*tx+Sin(an+90)*ty
End Function

Type swing
	Field ox#,oy#
	Field x#,y#,px#,py#
	Field an#,van#,length#,targan#,stiffness#
	Field image,scale#
	Field prv:swing
	
	Function create:swing(image,ox#,oy#,length,scale#=1,targan#=0,stiffness#=1)
		w:swing=New swing
		w.image=image
		w.length=length
		w.scale=scale
		w.targan=targan
		w.stiffness=stiffness
		w.ox=ox
		w.oy=oy
		swings.addlast w
		Return w
	End Function
End Type

Type stretch
	Field x#,y#,vx#,vy#
	Field name:String
	Field links:TList
	
	Function create:stretch(x#,y#,name:String)
		s:stretch=New stretch
		s.links=New TList
		s.x=x
		s.y=y
		s.name=name
		stretches.addlast s
		Return s
	End Function
	
	Method addlink(a:stretch,length#,stiffness#=.2)
		l:link=New link
		l.a=a
		links.addlast l
		l.length=length
		l.stiffness=stiffness
		
		l=New link
		l.a=Self
		a.links.addlast l
		l.length=length
		l.stiffness=stiffness
	End Method
End Type

Type link
	Field a:stretch
	Field length#
	Field stiffness#
End Type

Type poly
	Field points:TList
	
	Function create:poly(points:stretch[])
		p:poly=New poly
		p.points=New TList
		For s:stretch=EachIn points
			p.points.addlast s
		Next
		polys.addlast p
		Return p
	End Function
	
	Method draw()
		Local p:Float[]=New Float[points.count()*2]
		c=0
		For s:stretch=EachIn points
			p[c*2]=s.x
			p[c*2+1]=s.y
			c=c+1
		Next
		DrawPoly p
	End Method
End Type



mysanta:santa=santa.create(screenwidth/2,screenheight/2)
theirsanta:santa=santa.create(0,screenheight/2)

time=0
While Not KeyHit(KEY_ESCAPE)
	time=time+1
	If MouseDown(1)
		mysanta.leftarm.targan=Sin(time*20)*60-90
	Else
		mysanta.leftarm.targan=0
	EndIf
	mx=MouseX()
	my=MouseY()
	
	mysanta.targx=mx
	mysanta.targy=my
	theirsanta.targx=screenwidth-mx
	theirsanta.targy=my
	
	For s:santa=EachIn santas	
		an#=(s.targx-s.x)/5.0
		s.van=s.van*.9+Sin(an-s.an)*5
		s.an=s.an+s.van
		
		dx#=s.targx-s.x
		dy#=s.targy-s.y
		d#=Sqr(dx*dx+dy*dy)
		s.vx=s.vx+dx*.01
		s.vy=s.vy+dy*.01

		s.vx=s.vx*.95
		s.vy=s.vy*.95
		s.x=s.x+s.vx
		s.y=s.y+s.vy
	
		's.an=90
		SetRotation s.an
		SetScale 1,1
		DrawImage santabody,s.x,s.y
		
		For w:swing=EachIn s.myswings
			s.placeswing(w)
		Next
		
		s.hat.px=transformx(0,-30,s.head.px,s.head.py,-s.head.an)
		s.hat.py=transformy(0,-30,s.head.px,s.head.py,-s.head.an)

		s.beard[0].x=transformx(-30,30,s.head.px,s.head.py,-s.head.an)
		s.beard[0].y=transformy(-30,30,s.head.px,s.head.py,-s.head.an)
		s.beard[1].x=transformx(0,39,s.head.px,s.head.py,-s.head.an)
		s.beard[1].y=transformy(0,39,s.head.px,s.head.py,-s.head.an)
		s.beard[2].x=transformx(30,30,s.head.px,s.head.py,-s.head.an)
		s.beard[2].y=transformy(30,30,s.head.px,s.head.py,-s.head.an)
		For c=0 To 2
			s.beard[c].vx=0
			s.beard[c].vy=0
		Next
	Next
	
	For w:swing=EachIn swings
		If w.prv<>Null
			w.px=w.prv.x
			w.py=w.prv.y
		EndIf
		w.an#=ATan2(w.x-w.px,w.y-w.py)
		'RuntimeError w.an
		'DrawRect w.x,w.y,5,5
		'SetRotation 0
		'DrawText w.an,w.px,w.py
		w.van=w.van*.97*w.stiffness-Sin(w.an-w.targan)*1.5'*w.stiffness
		w.an=w.an+w.van
		w.x=w.px+Sin(w.an)*w.length
		w.y=w.py+Cos(w.an)*w.length
		SetRotation -w.an
		SetScale w.scale,w.scale
		DrawImage w.image,w.px,w.py
	Next

	SetScale 1,1
	SetRotation 0
	
	For t:stretch=EachIn stretches
		t.x=t.x+t.vx
		t.y=t.y+t.vy
	Next

	For t:stretch=EachIn stretches
		t.vx=t.vx*.9
		t.vy=t.vy*.9+1
		Local points:Float[]=New Float[t.links.count()*2]
		c=0
		For l:link=EachIn t.links
			points[c*2]=l.a.x
			points[c*2+1]=l.a.y
			c=c+1
			dx#=l.a.x-t.x
			dy#=l.a.y-t.y
			d#=Sqr(dx*dx+dy*dy)
			dx=dx/d
			dy=dy/d
			f#=(l.length-d)*l.stiffness
			t.vx=t.vx-f*dx
			t.vy=t.vy-f*dy
		Next
	Next
	
	SetAlpha .9
	For p:poly=EachIn polys
		p.draw()
	Next
	SetAlpha 1

	Flip
	Cls
Wend