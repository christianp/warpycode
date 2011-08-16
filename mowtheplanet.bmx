Framework BRL.GLMax2D

Import BRL.Math
Import BRL.Random
Import BRL.StandardIO


Global projsize#=380
Global fsize#=3
Global gsize#=2

Function projx#(pos#[])
	Local z#=(pos[3]+2)/Sqr(3)
	z#=1	'actually I prefer orthographic projection
	Return projsize*pos[1]/z+400
End Function

Function projy#(pos#[])
	Local z#=(pos[3]+2)/Sqr(3)
	z=1
	Return projsize*pos[2]/z+400
End Function

Function inclip(pos#[])
	Return pos[3]<0
End Function

Global things:TList=New TList
Type thing
	Field pos#[]
	Field t:trixel
	
	Method New()
		things.addlast Self
	End Method

	Method displaypos#[]()
		Return rotate(pos,rot)
	End Method
	
	Method place()
		t=trixel.findcontainer(pos)
		t.insert Self
	End Method
	
	
	Method die()
		things.remove Self
		If t
			t.remove Self
		EndIf
	End Method
	
	Method update() Abstract
	
	Method draw() Abstract
End Type

Global numgrasses=0
Global grasses:TList=New TList
Type grass Extends thing
	Field life#
	Field n
	Method New()
		numgrasses:+1
		grasses.addlast Self
		n=grasses.count()
	End Method
	
	Method die()
		grasses.remove Self
		numgrasses:-1
		Super.die
	End Method
	
	Function Create:grass(pos#[])
		g:grass=New grass
		g.pos=pos
		g.place
		Return g
	End Function
	
	Method update()
		'Return
		life:+.0001
		'If life>1 life=1

		If life>2
			Local v#[]
			v=halfspacerandom(pos,3)
			If trixel.thingsinhalfspace(v,4).count()<10
				grass.Create(v)
			EndIf
			life:-1
		EndIf
	End Method
	
	Method draw()
		'Return
		Local v#[]
		v=displaypos()
		ll#=Min(life,1)
		SetAlpha .4*ll+.2
		shade#=-v[3]*250
		SetColor 0,255,0
		'SetColor 0,shade,0
		If inclip(v)
			grassesdrawn:+1
			x#=projx(v)
			y#=projy(v)
			size#=1-v[3]*2
			'DrawRect x-size,y-size,size*2,size*2
			drawhalfspace v,.5*ll,4
			'd#=Sqr(x*x+y*y)
			'DrawLine x+400,y+400,400+x*(d+7)/d,y*(d+7)/d+400
		EndIf		
	End Method		
	
End Type

Global flags:TList=New TList
Type flag Extends thing
	
	Method New()
		flags.addlast Self
	End Method
	
	Function Create:flag(pos#[])
		f:flag=New flag
		f.pos=pos
		f.place
		For c=1 To 5
			grass.Create(halfspacerandom(pos,15))
		Next
		Return f
	End Function
	
	Method update()
		For g:grass=EachIn trixel.thingsinhalfspace(pos,15)
			g.life:+.002
		Next
	End Method
	
	Method draw()
		SetColor 0,150,0
		SetAlpha .1
		drawhalfspace displaypos(),15
	End Method
End Type

Global sheeps:TList=New TList
Type sheep Extends thing
	Field thought
	Global radius#=1.5
	Field speed#

	Field dest#[]
	
	Field follow:sheep
	
	Method New()
		sheeps.addlast Self
	End Method
	
	Function Create:sheep(pos#[])
		s:sheep=New sheep
		s.pos=pos
		s.place
		Return s
	End Function
	
	Method update()
		If Rand(3000)=1
			thought=0
		EndIf
		
		Select thought
		Case 0	'default
			speed=0
			If Rand(100)>1 Return

			l:TList=trixel.thingsinhalfspace(pos,radius+.5)
			eg:grass=Null
			For g:grass=EachIn l
				If g.life>.5
					eg=g				
					Exit
				EndIf
			Next
			If eg
				eg.die
			Else
				l:TList=trixel.thingsinhalfspace(pos,30)
				mind#=-1
				ngrasses=0
				For g:grass=EachIn l
					If g.life>.5
						ngrasses:+1
					EndIf
				Next
				If ngrasses>10
					cg:grass=Null
					l2:TList=New TList
					td#=0
					For g:grass=EachIn l	
						If g.life>.5
							l2.addlast g
						EndIf
					Next
					Local ds#[l2.count()]
					i=0
					For g:grass=EachIn l2
						d#=anglebetween(pos,g.pos)
						ds[i]=1/(d*d)
						td:+ds[i]
						i:+1
					Next
					r#=Rnd(td)
					acc#=0
					cg=grass(l2.first())
					i=0
					For g:grass=EachIn l2
						acc:+ds[i]
						If acc>r
							cg=g
							Exit
						EndIf
						i:+1
					Next
					If cg
						dest=cg.pos
						thought=1
					EndIf
				Else
					cs:sheep=Null
					For s:sheep=EachIn l
						d#=anglebetween(pos,s.pos)
						If s.follow<>Self And (d<mind Or mind=-1) And d>radius*2+6
							mind=d
							cs=s
						EndIf
					Next
					If cs
						follow=cs
						thought=2
					Else
						dest=halfspacerandom(pos,10)
						thought=1
					EndIf
				EndIf
			EndIf
		Case 1	'walking
			d#=anglebetween(pos,dest)
			If d<radius+.5
				thought=0
			Else	
				If d<radius+3
					speed:-Rnd(0.01)
				EndIf
				moveto dest
			EndIf
		Case 2	'following
			moveto follow.pos
			If anglebetween(pos,follow.pos)<radius*2+4
				follow=Null
				thought=0
			EndIf
		End Select
		
		If Not t.contains(pos)
			t.remove Self
			place
		EndIf
		speed:-.005
		If speed<0 speed=0
	End Method
	
	Method moveto(p#[])
		p=halfspacerandom(p,1)
		d#=anglebetween(pos,p)
		speed#:+.01
		If speed>.1 speed=.1
		pos=slerp(pos,p,speed/d)
		normalise pos
	End Method
	
	Function collide()
		l:TList=sheeps.copy()
		n=l.count()
		For i=1 To n
			s1:sheep=sheep(l.removefirst())
			For s2:sheep=EachIn l
				d#=anglebetween(s1.pos,s2.pos)
				If d<radius*2
					move#=(radius*2-d)/2
					Local v#[]
					v=crossprod(s1.pos,s2.pos)
					normalise v
					s1.pos=rotate(s1.pos,rotaround(v,-move))
					s2.pos=rotate(s2.pos,rotaround(v,move))
					normalise s1.pos
					normalise s2.pos
				EndIf
			Next
		Next
	End Function
	
	Method draw()
		Local v#[]
		v=displaypos()
		If Not inclip(v) Return

		SetColor 255,255,255
		SetAlpha .51
		drawhalfspace v,radius

		Select thought
		Case 0
		'	SetAlpha .02
		'	drawhalfspace v,30
		Case 1
		'	slerpline v,rotate(dest,rot),1
		End Select

		SetColor 0,0,0
		'DrawText thought,projx(v),projy(v)
	End Method
End Type

Type man Extends thing
	Field mown
	
	Method update()
		Local pick#[]
		pick = pickpoint(MouseX(),MouseY())
		
		If pick
			pick=rotate(pick,inverse(rot))
			If MouseDown(1)
				dp#=dotprod(pos,pick)
				an#=ACos(dp)
				If an>0
					dist#=Min(an,.7/an)
					pos=slerp(me.pos,pick,dist)
					normalise pos
				EndIf
			EndIf
		EndIf
		
		If KeyHit(KEY_F)
			If pick
				flag.Create pick
			EndIf
		EndIf
		
		If KeyHit(KEY_S)
			If pick
				For c=1 To 3
					sheep.Create halfspacerandom(pick,10)
				Next
			EndIf
		EndIf
		

		For g:grass=EachIn trixel.thingsinhalfspace(pos,15)
			'g.die
			'mown:+1
		Next
	End Method

	Method draw()
		Local manv#[]
		manv=displaypos()
		If inclip(manv)
			x#=projx(manv)
			y#=projy(manv)
			SetAlpha .2
			SetColor 255,255,255
			'drawhalfspace(manv,15)
			SetColor 255,0,0
			SetAlpha 1
			drawhalfspace manv,2
		EndIf
		
		Local v#[]
		SetColor 255,255,255
	End Method
End Type

Global trixels:TList=New TList
Type trixel
	Field p1#[],p2#[],p3#[]
	Field centre#[4]
	Field children:trixel[],parent:trixel
	Field contents:TList,numcontents
	Field name$
	
	Method New()
		contents=New TList
	End Method
	
	Function Create:trixel(name$,p1#[],p2#[],p3#[],parent:trixel=Null)
		t:trixel=New trixel
		t.name=name
		t.p1=p1
		t.p2=p2
		t.p3=p3
		t.centre[1]=(p1[1]+p2[1]+p3[1])/3
		t.centre[2]=(p1[2]+p2[2]+p3[2])/3
		t.centre[3]=(p1[3]+p2[3]+p3[3])/3
		normalise(t.centre)
		t.parent=parent
		Return t
	End Function
	
	Method contains(p#[])
		Return intriangle(p,p1,p2,p3)
	End Method
	
	Function findcontainer:trixel(p#[])
		For t:trixel=EachIn trixels
			If t.contains(p) Return t.container(p)
		Next
	End Function
	
	Method container:trixel(p#[])
		If Not contains(p) Return Null
		If children
			For i=0 To 3
				t:trixel=children[i].container(p)
				If t Return t
			Next
		Else
			Return Self
		EndIf
	End Method
	
	Method insert(th:thing)
		t:trixel=Self
		While t
			t.numcontents:+1
			t=t.parent
		Wend
		contents.addlast th
		th.t=Self
		If contents.count()>10
			subdivide()
			t:trixel=Self
			n=numcontents
			While t
				t.numcontents:-n
				t=t.parent
			Wend
			nc:TList=New TList
			For th:thing=EachIn contents
				t2:trixel=container(th.pos)
				If t2
					t2.insert th
				Else
					nc.addlast th
				EndIf
			Next
			contents=nc
		EndIf
	End Method
	
	Method remove(th:thing)
		If Not contents.contains(th) Return
		numcontents:-1
		contents.remove th
		t:trixel=parent
		While t
			t.numcontents:-1
			t=t.parent
		Wend
		If parent And parent.numcontents<=10
			parent.merge
		EndIf
	End Method
	
	
	Method subdivide()
		children=New trixel[4]
		Local p4#[4],p5#[4],p6#[4]
		p4[1]=(p1[1]+p2[1])/2
		p4[2]=(p1[2]+p2[2])/2
		p4[3]=(p1[3]+p2[3])/2
		p5[1]=(p3[1]+p2[1])/2
		p5[2]=(p3[2]+p2[2])/2
		p5[3]=(p3[3]+p2[3])/2
		p6[1]=(p1[1]+p3[1])/2
		p6[2]=(p1[2]+p3[2])/2
		p6[3]=(p1[3]+p3[3])/2
		normalise(p4)
		normalise(p5)
		normalise(p6)
		
		children[0]=trixel.Create(name+"0",p1,p4,p6,Self)
		children[1]=trixel.Create(name+"1",p4,p2,p5,Self)
		children[2]=trixel.Create(name+"2",p5,p3,p6,Self)
		children[3]=trixel.Create(name+"3",p4,p5,p6,Self)
	End Method
	
	Method merge()
		'contents=New TList
		For i=0 To 3
			For th:thing=EachIn children[i].contents
				contents.addlast th
				th.t=Self
			Next
		Next
		children=Null
	End Method
	
	Method draw()
		Local pp1#[],pp2#[],pp3#[]
		pp1=rotate(p1,rot)
		pp2=rotate(p2,rot)
		pp3=rotate(p3,rot)
		If Not (inclip(pp1) Or inclip(pp2) Or inclip(pp3)) Return
		
		If children
			For i=0 To 3
				children[i].draw()
			Next
		Else
			'SetAlpha .5
			'SetColor 0,0,255
			'drawhalfspace rotate(centre,rot),2.0/Len(name),5
			'slerpline pp1,pp2,5
			'slerpline pp2,pp3,5
			'slerpline pp3,pp1,5
			
			For th:thing=EachIn contents
				th.draw
			Next

			Rem
			If intersectshalfspace(me.pos,10)
				DrawRect projx(rotate(centre,rot)),projy(rotate(centre,rot)),10,10
			EndIf
			EndRem
		EndIf
	End Method
	
	Method intersectshalfspace(p#[],an#)
		'find if any corners inside halfspace
		If inhalfspace(p1,p,an) Or inhalfspace(p2,p,an) Or inhalfspace(p3,p,an) Return True	'all or some points in halfspace means yes
		
		'check if bounding circle intersects halfspace
		Local v1#[4],v2#[4],v#[]
		v1=quatsub(p2,p1)
		v2=quatsub(p3,p1)
		v=crossprod(v1,v2)
		normalise v
		db#=ACos(dotprod(p1,v))
		dp#=dotprod(p,v)
		anb#=ACos(dp)
		
		If anb>90 anb=180-anb
		If anb>an+db Return False	'bounding circle doesn't intersect means no
				
		If edgeinhalfspace(p1,p2,p,an) Or edgeinhalfspace(p2,p3,p,an) Or edgeinhalfspace(p1,p3,p,an) Return True
		
		If contains(p) Return True	'if centre of halfspace is inside triangle then yes
	End Method
	
	Function findinhalfspace:trixel[](p#[],an#)
		Local ins:trixel[0]
		For t:trixel=EachIn trixels
			ins:+t.kidsinhalfspace(p,an)
		Next
		Return ins
	End Function
	
	Function thingsinhalfspace:TList(p#[],an#)
		Local ins:trixel[]
		ins=trixel.findinhalfspace(p,an)
		ts:TList=New TList
		For t:trixel=EachIn ins
			For th:thing=EachIn t.contents
				If inhalfspace(th.pos,p,an)
					ts.addlast th
				EndIf
			Next
		Next
		Return ts
	End Function
	
	Method kidsinhalfspace:trixel[](p#[],an#)
		If Not intersectshalfspace(p,an) Return
		If children
			Local ins:trixel[]
			For i=0 To 3
				ins:+children[i].kidsinhalfspace(p,an)
			Next
			Return ins
		Else
			Return [Self]
		EndIf
	End Method
End Type

Function edgeinhalfspace(p1#[],p2#[],p#[],an#)
	g1#=dotprod(p,p1)
	g2#=dotprod(p,p2)
	an=Cos(an)
	theta#=ACos(dotprod(p1,p2))
	u#=Tan(theta/2)
	a#=-u*u*(g1+an)
	b#=g1*(u*u-1)+g2*(u*u+1)
	c#=g1-an
	If b*b<4*a*c Return False
	s#=Sqr(b*b-4*a*c)
	s1#=(-b+s)/(2*a)
	s2#=(-b-s)/(2*a)
	If (s1>0 And s1<1) Or (s2>0 And s1<1) 
		Return True
	EndIf
EndFunction

Function halfspacesintersect(p1#[],an1#,p2#[],an2#)
	dp#=dotprod(p1,p2)
	an#=ACos(dp)
	Return an<an1+an2
End Function

Function quatmult#[](q1#[],q2#[])
	Local a# = q1[0], b# = q1[1], c# = q1[2], d# = q1[3]
	Local w# = q2[0], x# = q2[1], y# = q2[2], z# = q2[3]
	
	Return [ a*w - b*x - c*y - d*z,		a*x + w*b + c*z - d*y,		a*y + w*c + d*x - b*z,		a*z + w*d + b*y - c*x ]
End Function
	
Function quatsub#[](q1#[],q2#[])
	Local q#[4]
	For i=0 To 3
		q[i]=q1[i]-q2[i]
	Next
	Return q
End Function

Function rotate#[](v#[],r#[])
	Return quatmult(r,quatmult(v,conj(r)))
End Function

Function rotaround#[](p#[],an#,inplace=False)
	an:/2
	Local q#[]
	If inplace q=p Else q=New Float[4]
	q[0]=Cos(an)
	q[1]=p[1]*Sin(an)
	q[2]=p[2]*Sin(an)
	q[3]=p[3]*Sin(an)
	Return q
End Function

Function conj#[](q#[])
	Local c#[4]
	c[0]=q[0]
	c[1]=-q[1]
	c[2]=-q[2]
	c[3]=-q[3]
	Return c
End Function

Function inverse#[](q#[])
	Local i#[4]
	n#=q[0]*q[0]+q[1]*q[1]+q[2]*q[2]+q[3]*q[3]
	i[0]=q[0]/n
	i[1]=-q[1]/n
	i[2]=-q[2]/n
	i[3]=-q[3]/n
	Return i
End Function

Function dotprod#(q1#[],q2#[])
	Return q1[1]*q2[1]+q1[2]*q2[2]+q1[3]*q2[3]
End Function

Function anglebetween#(q1#[],q2#[])
	dp#=dotprod(q1,q2)
	Return ACos(dp)
End Function

Function crossprod#[](q1#[],q2#[])
	Local q#[4]
	q[0]=0
	q[1]=q1[2]*q2[3]-q1[3]*q2[2]
	q[2]=q1[3]*q2[1]-q1[1]*q2[3]
	q[3]=q1[1]*q2[2]-q1[2]*q2[1]
	Return q
End Function

Function modulus#(q#[])
	Return Sqr(q[1]*q[1]+q[2]*q[2]+q[3]*q[3])
End Function

Function slerp#[](q1#[],q2#[],t#)
	Local q#[4]
	dp#=dotprod(q1,q2)
	an#=ACos(dp)
	If Sin(an)=0
		q[0]=q1[0]
		q[1]=q1[1]
		q[2]=q1[2]
		q[3]=q1[3]
		Return q
	EndIf
	s1#=Sin((1-t)*an)/Sin(an)
	s2#=Sin(t*an)/Sin(an)
	q[0]=s1*q1[0]+s2*q2[0]
	q[1]=s1*q1[1]+s2*q2[1]
	q[2]=s1*q1[2]+s2*q2[2]
	q[3]=s1*q1[3]+s2*q2[3]
	Return q
End Function



Function sphererandom#[]()
	x1#=1
	x2#=1
	While x1*x1+x2*x2>1
		x1=Rnd(-1,1)
		x2=Rnd(-1,1)
	Wend
	t#=Sqr(1-x1*x1-x2*x2)
	x#=2*x1*t
	y#=2*x2*t
	z#=1-2*(x1*x1+x2*x2)
	Return [0.0,x,y,z]
End Function

Function halfspacerandom#[](pos#[],an#)
	s#=Sin(Rnd(90))
	fan#=Sqr(s)*an
	Local v#[]
	v=rotaround(sphererandom(),fan,True)
	v=rotate(pos,v)
	normalise v
	Return v
End Function

Function slerpline(p1#[],p2#[],s#=1)
	If Not (inclip(p1) Or inclip(p2)) Return
	Local p#[],op#[]
	
	an#=anglebetween(p1,p2)
	s:/an
	
	If s=0 Return
	
	op=p1
	ox#=projx(p1)
	oy#=projy(p1)
	t#=0
	While t<1
		p=slerp(p1,p2,t)
		If inclip(p)
			x#=projx(p)
			y#=projy(p)
			
			If inclip(op)
				DrawLine ox,oy,x,y
			EndIf
			
			ox=x
			oy=y
		EndIf
		op=p
		t:+s
	Wend

	If inclip(p2) And inclip(op)
		x=projx(p2)
		y=projy(p2)
		ox=projx(op)
		oy=projy(op)
		DrawLine ox,oy,x,y
	EndIf
End Function

Function drawhalfspace(p#[],an#,bits=30)
	If Not inclip(p) Return
	Local v#[],ov#[]
	v=[p[0],p[2],p[3],p[1]]
	v=crossprod(v,p)
	normalise(v)
	v=rotate(p,rotaround(v,an))
	Local rr#[]
	anstep#=360.0/bits
	rr=rotaround(p,anstep)
	px#=projx(p)
	py#=projy(p)
	Local poly#[]
	ov=v
	For c=0 To bits
		poly=[px,py,projx(v),projy(v),projx(ov),projy(ov)]
		DrawPoly poly
		ov=v
		v=rotate(v,rr)
	Next
End Function

Function subdivide#[][](p1#[],p2#[],p3#[],steps=2)
	If Rand(steps)>1
		Return [p1,p2,p3]
	EndIf
	
	Local tris#[][]
	
	Local p4#[4],p5#[4],p6#[4]
	p4[1]=(p1[1]+p2[1])/2
	p4[2]=(p1[2]+p2[2])/2
	p4[3]=(p1[3]+p2[3])/2
	p5[1]=(p3[1]+p2[1])/2
	p5[2]=(p3[2]+p2[2])/2
	p5[3]=(p3[3]+p2[3])/2
	p6[1]=(p1[1]+p3[1])/2
	p6[2]=(p1[2]+p3[2])/2
	p6[3]=(p1[3]+p3[3])/2
	normalise(p4)
	normalise(p5)
	normalise(p6)
	
	tris:+subdivide(p1,p4,p6,steps+1)+subdivide(p4,p2,p5,steps+1)+subdivide(p5,p3,p6,steps+1)+subdivide(p4,p5,p6,steps+1)
	Return tris
End Function

Function normalise(p#[])
	d#=Sqr(p[1]*p[1]+p[2]*p[2]+p[3]*p[3])
	p[1]:/d
	p[2]:/d
	p[3]:/d
End Function

Function intriangle(p#[],p1#[],p2#[],p3#[])
	Local v#[4]
	Local diff#[4]
	v=crossprod(p1,p2)
	diff[1]=p[1]-p1[1]
	diff[2]=p[2]-p1[2]
	diff[3]=p[3]-p1[3]
	dp1#=dotprod(v,diff)
	
	v=crossprod(p2,p3)
	normalise(v)
	diff[1]=p[1]-p2[1]
	diff[2]=p[2]-p2[2]
	diff[3]=p[3]-p2[3]
	dp2#=dotprod(v,diff)
	
	v=crossprod(p3,p1)
	normalise(v)
	diff[1]=p[1]-p3[1]
	diff[2]=p[2]-p3[2]
	diff[3]=p[3]-p3[3]
	dp3#=dotprod(v,diff)
	
	Return dotprod(p,p1)>0 And ((Sgn(dp1)=Sgn(dp2) And Sgn(dp2)=Sgn(dp3)) Or dp1*dp2*dp3=0)
End Function

Function inhalfspace(p#[],s#[],an#)
	dp#=dotprod(p,s)
	If dp>Cos(an) Return True
End Function

Function pickpoint#[](px#,py#)
	x#=(px-400)/projsize
	y#=(py-400)/projsize
	If x*x+y*y<1
		z#=-Sqr(1-x*x-y*y)
		Return [0.0,x,y,z]
	Else
		Return Null
	EndIf
End Function

Function listrandom:Object(l:TList)
	i=Rand(0,l.count()-1)
	Return l.valueatindex(i)
End Function

AppTitle="Go outside and mow the planet"
Graphics 800,800,0
SetBlend ALPHABLEND
SetClsColor 0,0,0

Local v#[]

'make geodesic grid
d#=Sqr((10+2*Sqr(5))/4)
a#=1/d
b#=(1+Sqr(5))/(2*d)
n=0
Local ico#[][]
ico=[ [0.0,0.0,a,b],[0.0,0.0,-a,b],[0.0,0.0,a,-b],[0.0,0.0,-a,-b],[0.0,a,b,0.0],[0.0,-a,b,0.0],[0.0,a,-b,0.0],[0.0,-a,-b,0.0],[0.0,b,0.0,a],[0.0,-b,0.0,a],[0.0,b,0.0,-a],[0.0,-b,0.0,-a]]
Local tris[]
tris=[0,1,8,0,4,8,4,8,10,6,8,10,1,6,8,1,6,7,3,6,7,3,7,11,2,3,11,2,3,10,2,4,10,2,4,5,0,4,5,0,5,9,0,1,9,1,7,9,7,9,11,5,9,11,2,5,11,3,6,10]
For i=0 To 59 Step 3
	trixels.addlast trixel.Create((i/3)+"T",ico[tris[i]],ico[tris[i+1]],ico[tris[i+2]])
Next

Global rot#[]
rot=Null
rot=[0.0,1.0,0.0,0.0]


Local source#[]
For j=1 To 5
	source=sphererandom()
	For i=1 To 100
		an#=Rnd(2,15)
		v=rotate(source,rotaround(sphererandom(),an))
'		g:grass=grass.Create(v)
'		g.life=Rnd(1)
	Next
Next

For c=1 To 100
	g:grass=grass.Create(sphererandom())
	g.life=Rnd(1)
Next

Global me:man=New man
me.pos=[0.0,0.0,0.0,1.0]

Global grassesdrawn=0


Local clickx,clicky,omz=MouseZ()
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	If Rnd(15)<1
'		grass.Create(sphererandom())
	EndIf
	
	mz=MouseZ()
	projsize:*(1+(KeyDown(KEY_DOWN)-KeyDown(KEY_UP)+(mz-omz)*10)*.01)
	omz=mz

	If MouseDown(2)
		lturn#=(MouseX()-clickx)*1.0/400
		rot = quatmult([Float(Cos(lturn)),0.0,-Float(Sin(lturn)),0.0],rot)
		uturn#=(MouseY()-clicky)*1.0/400
		rot = quatmult([Float(Cos(uturn)),Float(Sin(uturn)),0.0,0.0],rot)
	Else
		clickx=MouseX()
		clicky=MouseY()
		Local forwards#[]
		forwards=[0.0,0.0,0.0,-1.0]
		v=me.displaypos()
		dp#=dotprod(v,forwards)
		v=crossprod(forwards,v)
		m#=modulus(v)
		If m>0
			v[1]:/m
			v[2]:/m
			v[3]:/m
			an#=ACos(dp)
			If an>5
				an=-(an-5)/30
				v=[Float(Cos(an)),Float(Sin(an))*v[1],Float(Sin(an))*v[2],Float(Sin(an))*v[3]]
				rot=quatmult(v,rot)
			EndIf
		EndIf
	EndIf
	
		
	'update
	
	For th:thing=EachIn things
		th.update
	Next
	
	sheep.collide

	'draw
	
	SetColor 255,255,255
	SetAlpha .1
	DrawOval 400-projsize-10,400-projsize-10,projsize*2+20,projsize*2+20
	SetColor 60,36,0
	SetAlpha 1
	DrawOval 400-projsize,400-projsize,projsize*2,projsize*2


	grassesdrawn=0
	
	For f:flag=EachIn flags
		f.draw
	Next
	
	For t:trixel=EachIn trixels
		t.draw()
	Next
	
	For s:sheep=EachIn sheeps
		s.draw
	Next
	
	me.draw
	
	
	SetColor 255,255,255
	SetAlpha 1
	SetScale 3,3
	DrawText me.mown,0,0
	SetScale 1,1
	
	DrawText numgrasses,0,45

	oldms=ms
	ms=MilliSecs()
	fps#=1000.0/(ms-oldms)
	DrawText fps,0,785
	DrawText grassesdrawn,0,60
	
	Flip
	Cls
Wend