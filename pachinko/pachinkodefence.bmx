Global gwidth=600,gheight=800

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

Function pointlinedistance#(x1#,y1#,x2#,y2#)
	dx#=x2-x1
	dy#=y2-y1
	d#=Sqr(dx*dx+dy*dy)
	dx:/d
	dy:/d
	
	m#=(x-x1)*dx+(y-y1)*dy		'dot product of unit vector in direction of line, and vector towards point
	
	If m>=0 And m<=d
		Return m
	EndIf
End Function

Function drawshell(x#,y#,r#)
	segs=2*Pi*r/5
	ox#=x+r
	oy#=y
	For c#=1 To segs
		an=360*c/segs
		px#=x+Cos(an)*r
		py#=y+Sin(an)*r
		DrawLine px,py,ox,oy
		ox=px
		oy=py
	Next
End Function

Type tboard
	Field pegs:TList
	
	Method New()
		pegs=New TList
	End Method
	
	Function grid:tboard(w,h)
		board:tboard=New tboard
		For j#=0 To h-1
			y#=((j+.5)/h)*(gheight-100)+50
			For i#=0 To w-1
				x#=((i+.5+(j Mod 2)*.5)/(w+.5))*gwidth
				board.pegs.addlast peg.Create(x,y)
			Next
		Next
		Return board
	End Function
	
	Method pick:peg(x#,y#)
		mindist#=-1
		picked:peg=Null
		For p:peg=EachIn pegs
			dx#=p.x-x
			dy#=p.y-y
			d#=dx*dx+dy*dy
			If d<mindist Or picked=Null
				picked=p
				mindist=d
			EndIf
		Next
		Return picked
	End Method
	
	Method draw()
		For p:peg=EachIn pegs
			p.draw
		Next
	End Method
End Type

Type peg
	Field x#,y#
	Field b:block
	
	Function Create:peg(x#,y#)
		p:peg=New peg
		p.x=x
		p.y=y
		Return p
	End Function

	Method draw()
		If Not b
			SetColor 50,50,50
			DrawOval x-2,y-2,4,4
		Else
			b.draw
		EndIf
	End Method
End Type

Type block
	Field health#
	Field p:peg
	Field level
	
	Method New()
		health=1
		level=1
	End Method
	
	Method update() Abstract
	
	Method collide(b:ball) Abstract
	
	Method hit(amount#)
		health:-amount
		
		If health<=0
			game.removeblock Self
		EndIf
	End Method
	
	Method upgrade()
		level:+1
	End Method
	
	Method draw() Abstract
End Type

Type repelblock Extends block
	Field strength#,r#
	Field fuzz#
	
	Method New()
		strength=.02
		r=30
	End Method
	
	Method upgrade()
		Super.upgrade
		strength:+level*.05
		r:+Sqr(level)*4
	End Method
	
	Method update()
		fuzz:*.7
		If fuzz<.1 fuzz=0
	End Method
	
	Method collide(b:ball)
		dx#=b.x-p.x
		dy#=b.y-p.y
		d#=dx*dx+dy*dy
		If d<r*r
			dx=b.x-gwidth/2
			dy=b.y-gheight/2
			d=Sqr(dx*dx+dy*dy)
			dx:/d
			dy:/d
			b.x:+strength*dx
			b.y:+strength*dy
			fuzz:+Rnd(.3)
			hit .01
		EndIf
		
	End Method
	
	Method draw()
		If fuzz
			SetAlpha fuzz
			SetColor 125,249,255
			DrawOval p.x-r,p.y-r,r*2,r*2
			SetAlpha 1
		EndIf
		SetColor 0,0,0
		DrawOval p.x-r/4,p.y-r/4,r/2,r/2
		SetColor 255,255,255
		drawshell p.x,p.y,r/4
	End Method
End Type

Type squareblock Extends block
	Field w#,h#
	Field cr#
	
	Method New()
		Select Rand(0,1)
		Case 0
			w=20
			h=5
		Case 1
			w=5
			h=20
		End Select
		
		cr=Rnd(.1,1)
	End Method
	
	Method upgrade()
		Super.upgrade
		w:*1.1
		h:*1.1
	End Method
	
	Method update()
	End Method
	
	Method collide(b:ball)
		vx#=b.x-b.ox
		vy#=b.y-b.oy
		
		Local t#[8]
		
		'edges
		
		If vy<>0
			t[0]=(p.y-h-b.r-b.oy)/vy
			px#=b.ox+t[0]*vx
			If px<p.x-w Or px>p.x+w Or vy<0 t[0]=2
			t[1]=(p.y+h+b.r-b.oy)/vy
			px#=b.ox+t[1]*vx
			If px<p.x-w Or px>p.x+w Or vy>0 t[1]=2
		EndIf
		If vx<>0
			t[2]=(p.x-w-b.r-b.ox)/vx
			py#=b.oy+t[2]*vy
			If py<p.y-h Or py>p.y+h Or vx<0 t[2]=2
			t[3]=(p.x+w+b.r-b.ox)/vx
			py#=b.oy+t[3]*vy
			If py<p.y-h Or py>p.y+h Or vx>0 t[3]=2
		EndIf
		
		'corners
		px#=p.x-w
		py#=p.y-h
		dx#=b.ox-px
		dy#=b.oy-py
		qa#=vx*vx+vy*vy
		qb#=2*(dx*vx+dy*vy)
		qc#=dx*dx+dy*dy-b.r*b.r
		If qb*qb>=4*qa*qc
			t[4]=(-qb-Sqr(qb*qb-4*qa*qc))/(2*qa)
		EndIf
		
		px#=p.x+w
		py#=p.y-h
		dx#=b.ox-px
		dy#=b.oy-py
		qa#=vx*vx+vy*vy
		qb#=2*(dx*vx+dy*vy)
		qc#=dx*dx+dy*dy-b.r*b.r
		If qb*qb>=4*qa*qc
			t[5]=(-qb-Sqr(qb*qb-4*qa*qc))/(2*qa)
		EndIf

		px#=p.x-w
		py#=p.y+h
		dx#=b.ox-px
		dy#=b.oy-py
		qa#=vx*vx+vy*vy
		qb#=2*(dx*vx+dy*vy)
		qc#=dx*dx+dy*dy-b.r*b.r
		If qb*qb>=4*qa*qc
			t[6]=(-qb-Sqr(qb*qb-4*qa*qc))/(2*qa)
		EndIf

		px#=p.x-w
		py#=p.y+h
		dx#=b.ox-px
		dy#=b.oy-py
		qa#=vx*vx+vy*vy
		qb#=2*(dx*vx+dy*vy)
		qc#=dx*dx+dy*dy-b.r*b.r
		If qb*qb>=4*qa*qc
			t[7]=(-qb-Sqr(qb*qb-4*qa*qc))/(2*qa)
		EndIf
		
		Local t2#[]=t[..]
		t2.sort
		
		i=0
		While i<8 And t2[i]<=0
			i:+1
		Wend
		If i=8 Return
		Local time#=t2[i]
		If time<=1
			Select time
			Case t[0]
				py#=p.y-h-b.r
				vy:-(1+cr)*vy
				b.y=py+(1-time)*vy
				b.oy=b.y-vy
			Case t[1]
				py#=p.y+h+b.r
				vy:-(1+cr)*vy
				b.y=py+(1-time)*vy
				b.oy=b.y-vy
			Case t[2]
				px#=p.x-w-b.r
				vx:-(1+cr)*vx
				b.x=px+(1-time)*vx
				b.ox=b.x-vx
			Case t[3]
				px#=p.x+w+b.r
				vx:-(1+cr)*vx
				b.x=px+(1-time)*vx
				b.ox=b.x-vx
			Case t[4]
				cx#=b.ox+time*vx
				cy#=b.oy+time*vy
				px#=p.x-w
				py#=p.y-h
				dx#=cx-px
				dy#=cy-py
				d#=Sqr(dx*dx+dy*dy)
				dx:/d
				dy:/d
				dp#=vx*dx+vy*dy
				vx:-(1+cr)*dp*dx
				vy:-(1+cr)*dp*dy
				b.x=cx+(1-time)*vx
				b.y=cy+(1-time)*vy
				b.ox=b.x-vx
				b.oy=b.y-vy
			Case t[5]
				cx#=b.ox+time*vx
				cy#=b.oy+time*vy
				px#=p.x+w
				py#=p.y-h
				dx#=cx-px
				dy#=cy-py
				d#=Sqr(dx*dx+dy*dy)
				dx:/d
				dy:/d
				dp#=vx*dx+vy*dy
				vx:-(1+cr)*dp*dx
				vy:-(1+cr)*dp*dy
				b.x=cx+(1-time)*vx
				b.y=cy+(1-time)*vy
				b.ox=b.x-vx
				b.oy=b.y-vy
			Case t[6]
				cx#=b.ox+time*vx
				cy#=b.oy+time*vy
				px#=p.x-w
				py#=p.y+h
				dx#=cx-px
				dy#=cy-py
				d#=Sqr(dx*dx+dy*dy)
				dx:/d
				dy:/d
				dp#=vx*dx+vy*dy
				vx:-(1+cr)*dp*dx
				vy:-(1+cr)*dp*dy
				b.x=cx+(1-time)*vx
				b.y=cy+(1-time)*vy
				b.ox=b.x-vx
				b.oy=b.y-vy
			Case t[7]
				cx#=b.ox+time*vx
				cy#=b.oy+time*vy
				px#=p.x+w
				py#=p.y+h
				dx#=cx-px
				dy#=cy-py
				d#=Sqr(dx*dx+dy*dy)
				dx:/d
				dy:/d
				dp#=vx*dx+vy*dy
				vx:-(1+cr)*dp*dx
				vy:-(1+cr)*dp*dy
				b.x=cx+(1-time)*vx
				b.y=cy+(1-time)*vy
				b.ox=b.x-vx
				b.oy=b.y-vy
			End Select

			hit .01
		Else
			
		EndIf
	End Method
	
	Method draw()
		SetColor 100,255*cr,100
		SetAlpha health
		DrawRect p.x-w,p.y-h,w*2,h*2
		SetAlpha 1
		DrawLine p.x-w,p.y-h,p.x+w,p.y-h
		DrawLine p.x-w,p.y-h,p.x-w,p.y+h
		DrawLine p.x-w,p.y+h,p.x+w,p.y+h
		DrawLine p.x+w,p.y-h,p.x+w,p.y+h
	End Method
End Type

Type roundblock Extends block
	Field r#
	Field cr#
	
	Method New()
		r=20
		
		cr=Rnd(.1,1)
	End Method
	
	Method upgrade()
		r:+level*2
	End Method
	
	Method update()
	End Method
	
	Method collide(b:ball)
		vx#=b.x-b.ox
		vy#=b.y-b.oy
		ov#=Sqr(vx*vx+vy*vy)
		
		dx#=b.ox-p.x
		dy#=b.oy-p.y
		
		dr#=r+b.r
		
		qa#=vx*vx+vy*vy
		qb#=2*dx*vx+2*dy*vy
		qc#=dx*dx+dy*dy-dr*dr
		
		If qb*qb>4*qa*qc
		
			lambda#=(-qb-Sqr(qb*qb-4*qa*qc))/(2*qa)
			
			If lambda>=0 And lambda<=1
				px#=b.ox+lambda*vx
				py#=b.oy+lambda*vy
				
				v#=Sqr(vx*vx+vy*vy)
				t#=lambda/v
				
				dx3#=px-p.x
				dy3#=py-p.y
				d3#=Sqr(dx3*dx3+dy3*dy3)
				dx3:/d3
				dy3:/d3
				
				px=p.x+dx3*(dr+.01)
				py=p.y+dy3*(dr+.01)
				
				dp#=Abs(vx*dx3+vy*dy3)
				vx:+dp*(1+cr)*dx3
				vy:+dp*(1+cr)*dy3
									
				b.x=px+(1-t)*vx
				b.y=py+(1-t)*vy
				b.ox=b.x-vx
				b.oy=b.y-vy
				
				dx=b.x-p.x
				dy=b.y-p.y
				d#=dx*dx+dy*dy
				While d<dr*dr
					d=Sqr(d)
					dx=dx3*(.001+dr-d)
					dy=dy3*(.001+dr-d)
					b.x:+dx
					b.y:+dy
					b.ox:+dx
					b.oy:+dy
					dx=b.x-p.x
					dy=b.y-p.y
					d#=dx*dx+dy*dy
				Wend
				
				hit .01
				Return
			EndIf
		EndIf
		dx#=b.x-p.x
		dy#=b.y-p.y
		d#=dx*dx+dy*dy
		If d<dr*dr
			d=Sqr(d)
			dx:/d
			dy:/d
			px#=p.x+dr*dx
			py#=p.y+dr*dy
			b.ox:+px-b.x
			b.oy:+py-b.y
			b.x=px
			b.y=py
			hit .01
		EndIf
	End Method
	
	Method draw()
		SetColor 100,100,cr*255
		SetAlpha health
		DrawOval p.x-r,p.y-r,2*r,2*r
		SetAlpha 1
		drawshell p.x,p.y,r
	End Method
End Type

Type ball
	Field x#,y#,ox#,oy#
	Field r#
	Field red#,green#,blue#
	
	Method New()
		Select 1
		Case 1
			x=Rnd(.3,.7)*gwidth
			y=0
		Case 2
			x=Rnd(.3,.7)*gwidth
			y=gheight
		Case 3
			x=0
			y=Rnd(.3,.7)*gheight
		Case 4
			x=gwidth
			y=Rnd(.3,.7)*gheight
		End Select
		
		ox=x
		oy=y
		
		Rem
		red=Rnd()
		green=Rnd()
		blue=Rnd()
		t#=red+green+blue
		red:*255/t
		green:*255/t
		blue:*255/t
		EndRem
		shade#=Rnd(.5,1)*255
		red=shade;green=shade;blue=shade
		
		r=Rnd(5,10)
	End Method
	
	Method update()
		x2#=x
		y2#=y
		x:+x-ox
		y:+y-oy
		ox=x2
		oy=y2
	End Method
	
	Function collide(balls:TList)
		l:TList=balls.copy()
		nballs=l.count()
		For c=1 To nballs
			b1:ball=ball(l.removefirst())
			For b2:ball=EachIn l
				vx1#=b1.x-b1.ox
				vy1#=b1.y-b1.oy
				vx2#=b2.x-b2.ox
				vy2#=b2.y-b2.oy
				vx#=vx2-vx1
				vy#=vy2-vy1
				
				dr#=b1.r+b2.r
				
				dx#=b2.ox-b1.ox
				dy#=b2.oy-b1.oy
				
				qa#=vx*vx+vy*vy
				qb#=2*vx*dx+2*vy*dy
				qc#=dx*dx+dy*dy-dr*dr
				
				If qb*qb>=4*qa*qc
					t#=(-qb-Sqr(qb*qb-4*qa*qc))/(2*qa)
					If t>=0 And t<=1
						x1#=b1.ox+t*vx1
						y1#=b1.oy+t*vy1
						x2#=b2.ox+t*vx2
						y2#=b2.oy+t*vy2
						
						dx#=x2-x1
						dy#=y2-y1
						d#=Sqr(dx*dx+dy*dy)
						dx:/d
						dy:/d
						dp#=vx*dx+vy*dy
						
						v#=Sqr(vx*vx+vy*vy)
						
						f#=2*v/dr
						
						f1#=b2.r*f
						vx1:-f1*dx
						vy1:-f1*dy
						
						b1.x=x1+(1-t)*vx1
						b1.y=y1+(1-t)*vy1
						b1.ox=b1.x-vx1
						b1.oy=b1.y-vy1
						
						f2#=b1.r*f
						vx2:+f2*dx
						vy2:+f2*dy
						
						b2.x=x2+(1-t)*vx2
						b2.y=y2+(1-t)*vy2
						b2.ox=b2.x-vx2
						b2.oy=b2.y-vy2
					EndIf
				EndIf
				
			Next
		Next
	End Function
	
	Method draw()
		SetColor red,green,blue
		DrawOval x-r,y-r,2*r,2*r
	End Method
End Type

Type button
	Field txt$
	Field x#,y#
	Field w#,h#
	Field red,green,blue
	
	Field f()
	
	Method New()
	End Method
	
	Function Create:button(txt$,x#,y#,f(),red=255,green=255,blue=255)
		b:button=New button
		b.txt=txt
		b.f=f
		b.x=x
		b.y=y
		b.w=TextWidth(b.txt)+4
		b.h=TextHeight(b.txt)+4
		b.red=red
		b.green=green
		b.blue=blue
		Return b
	End Function
	
	Function pick:button(buttons:TList,x#,y#)
		For b:button=EachIn buttons.reversed()
			If x>b.x And x<b.x+b.w And y>b.y And y<b.y+b.h
				Return b
			EndIf
		Next
	End Function
	
	Method draw()
		SetColor red,green,blue
		DrawLine x,y,x+w,y
		DrawLine x,y,x,y+h
		DrawLine x+w,y,x+w,y+h
		DrawLine x,y+h,x+w,y+h
		DrawText txt,x+2,y+2
	End Method
End Type

Type tgame
	Field board:tboard
	Field blocks:TList
	Field balls:TList
	
	Field buttons:TList
	
	Field gravity#=.1
	Field basket#=20
	
	Field placemode
	
	Method New()
		initgfx
		
		blocks=New TList
		balls=New TList

		board=tboard.grid(9,12)
		
		
		buttons=New TList
		
		Function placeround()
			game.placemode=1
		End Function
		addbutton button.Create("Round Block",10,gheight-40,placeround)
		Function placesquare()
			game.placemode=2
		End Function
		addbutton button.Create("Square Block",10,gheight-20,placesquare)
		Function placerepel()
			game.placemode=3
		End Function
		addbutton button.Create("Repel Block",120,gheight-40,placerepel)
		
		Function upgradeblock()
			game.placemode=0
		End Function
		addbutton button.Create("Upgrade Block",120,gheight-20,upgradeblock)
		
		placemode=1
		
	End Method
	
	Method addbutton(b:button)
		buttons.addlast b
	End Method
	
	
	Method update()
		If KeyHit(KEY_ESCAPE) Or AppTerminate()
			End
		EndIf
		
		mx=MouseX()
		my=MouseY()
		
		pbutton:button=button.pick(buttons,mx,my)
		ppeg:peg=board.pick(mx,my)
		
		If MouseHit(1)
			If pbutton
				pbutton.f()
			ElseIf ppeg And placemode>0
				If Not ppeg.b
					Select placemode
					Case 1
						bl:block=New roundblock
					Case 2
						bl:block=New squareblock
					Case 3
						bl:block=New repelblock
					End Select
					addblock ppeg,bl
				EndIf
			ElseIf ppeg And placemode=0
				If ppeg.b
					ppeg.b.upgrade
				EndIf
			EndIf
		EndIf
		If MouseHit(2)
			If ppeg And ppeg.b
				removeblock ppeg.b
			EndIf
		EndIf
		
		For i=1 To 3
			If KeyHit(i+48)
				placemode=i
			EndIf
		Next
		
			
		
		If KeyHit(KEY_SPACE)
			b:ball=New ball
			b.x=mx
			b.y=my
			b.ox=mx
			b.oy=my
			balls.addlast b
		EndIf
		
		For bl:block=EachIn blocks
			bl.update
		Next
		
		For c=1 To poisson(0.01)
			balls.addlast(New ball)
		Next
		
		For b:ball=EachIn balls
			dx#=gwidth/2-b.x
			dy#=gheight/2-b.y
			d#=Sqr(dx*dx+dy*dy)
			If d<basket-b.r
				balls.remove b
			Else
				dx:/d
				dy:/d
				b.x:+dx*gravity
				b.y:+dy*gravity
			EndIf
		Next
		
		For b:ball=EachIn balls
			b.update
		Next
		
		ball.collide balls
		
		For c=1 To 3
			For b:ball=EachIn balls
				For bl:block=EachIn blocks
					bl.collide b
				Next
			Next
		Next
	End Method
	
	Method addblock(p:peg,bl:block)
		blocks.addlast bl
		bl.p=p
		p.b=bl
	End Method
	
	Method removeblock(bl:block)
		blocks.remove bl
		bl.p.b=Null
	End Method
	
	Method draw()
		SetAlpha .2
		SetColor 255,0,0
		DrawOval gwidth/2-basket,gheight/2-basket,basket*2,basket*2
		SetAlpha 1
	
		board.draw
		
		For b:ball=EachIn balls
			b.draw
		Next
		
		For bu:button=EachIn buttons
			bu.draw
		Next
		
		Flip
		Cls
	End Method
End Type

Global game:tgame
Function initgfx()
	Graphics gwidth,gheight,0
	SetBlend ALPHABLEND
End Function


game=New tgame

While True
	game.update
	
	game.draw
Wend