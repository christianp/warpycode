'--------------------
'       maths

Function linesintersect#(ax#,ay#,bx#,by#,cx#,cy#,dx#,dy#,fit=0)
	'fit, bitmask, set:
	' 1: doesn't need to be on first segment
	' 2: doesn't need to be on second segment
	bx:-ax
	by:-ay
	dx:-cx
	dy:-cy
	
	If dx<>0
		lambda#=(cy-ay+(ax-cx)*dy/dx)/(by-bx*dy/dx)
	Else
		lambda#=(cx-ax+(ay-cy)*dx/dy)/(bx-by*dx/dy)
	EndIf
	If bx<>0
		mu#=(ay-cy+(cx-ax)*by/bx)/(dy-dx*by/bx)
	Else
		mu#=(ax-cx+(cy-ay)*bx/by)/(dx-dy*bx/by)
	EndIf
	
	Rem
	Print String(ax)+"  ,  "+String(ay)
	Print String(bx)+"  ,  "+String(by)
	Print String(cx)+"  ,  "+String(cy)
	Print String(dx)+"  ,  "+String(dy)
	Print lambda
	Print mu
	WaitKey
	EndRem
	If (lambda#>=0 And lambda<=1) Or (fit & 1)
		If (mu#>=0 And mu<=1) Or (fit & 2)
			Return lambda
		EndIf
	EndIf
	Return -1
End Function


'---------------------
'     game things


Type grabbable
	Field x#,y#
	
	Method New()
		grabbables.addlast Self
	End Method
	
	Function Create:grabbable(x#,y#)
		g:grabbable=New grabbable
		g.x=x
		g.y=y
		Return g
	End Function
End Type



Type dot
	Field ox#,oy#
	Field oox#,ooy#
	Field newx#,newy#
	Field g:grabbable
	Field vx#,vy#
	Field radius#
	Field magnify#
	Field image:timage
	Field an#,van#
	Field reflect
	Field reflan#
	Field bounced
	
	Field parent:dot
	
	Method New()
		reflect=1
	End Method
	
	Function Create:dot(x#,y#,radius#,image:timage)
		d:dot=New dot
		d.g=grabbable.Create(x,y)
		d.newx=x
		d.newy=y
		d.ox=x
		d.oy=y
		d.radius=radius
		d.image=image
		
		d.jiggle()
		Return d
	End Function
	
	Method draw()
		SetRotation an
		sx#=magnify*radius/ImageWidth(image)
		sy#=magnify*radius/ImageHeight(image)
		SetScale sx,sy*reflect
		DrawImage image,g.x,g.y
		SetRotation 0
		SetScale 1,1
		DrawRect g.x-2,g.y-2,4,4
	End Method
	
	Method update()
		DrawLine g.x,g.y,ox,oy
		vx=g.x-ox
		vy=g.y-oy
		an:+van
		van:*.99
		ox=g.x
		oy=g.y
		g.x=g.x+vx
		g.y=g.y+vy+gravity*magnify
		newx=g.x
		newy=g.y
		budgex=0
		budgey=0
		oox#=ox
		ooy#=oy
		bounced=0
		
		For w:wall=EachIn walls
			lambda#=linesintersect(ox,oy,g.x,g.y,w.x1,w.y1,w.x2,w.y2)
			If lambda#>=0
				bounced=1
				vx#=newx-ox
				vy#=newy-oy
				newx#=ox+lambda*vx
				newy#=oy+lambda*vy
				'side=Sgn(w.nx*(ox-w.x1)+w.ny*(oy-w.y1))
				side=1
				ndp#=(w.nx*vx+w.ny*vy)*side
				fdp#=(w.dx*vx+w.dy*vy)*friction
				vx=(vx-2*w.nx*ndp-w.dx*fdp)*bounce
				vy=(vy-2*w.ny*ndp-w.dy*fdp)*bounce
				van:+360*fdp/(2*Pi*radius)
				newx=newx+vx*(1-lambda)
				newy=newy+vy*(1-lambda)
				ox=newx-vx
				oy=newy-vy
			EndIf
		Next
	End Method
	
	Method collapse()
		If parent And bounced
			dx#=newx-ox
			dy#=newy-oy
			nx#=dx*Cos(2*reflan)+dy*Sin(2*reflan)
			ny#=dy*Sin(2*reflan)-dy*Cos(2*reflan)
			pvx#=parent.newx-parent.ox
			pvy#=parent.newy-parent.oy
			parent.newx:+(nx-pvx)
			parent.newy:+(ny-pvy)
		EndIf
	End Method
	
	Method jiggle()
		g.x=newx
		g.y=newy
		magnify=1
	End Method
End Type





Type wall
	Field x1#,y1#,x2#,y2#
	Field nx#,ny#
	Field dx#,dy#
	Field length#
	Field an#
	
	Method New()
		walls.addlast Self
	End Method
	
	Function Create:wall(x1#,y1#,x2#,y2#)
		w:wall=New wall
		w.x1=x1
		w.y1=y1
		w.x2=x2
		w.y2=y2
		w.jiggle()
		
		Return w
	End Function	
	
	Method jiggle()
		dx=x2-x1
		dy=y2-y1
		length=Sqr(dx*dx+dy*dy)
		dx:/length
		dy:/length
		an=ATan2(dy,dx)
		nx=Cos(an-90)
		ny=Sin(an-90)
	End Method
	
	Method draw()
		SetLineWidth 2
		DrawLine x1,y1,x2,y2
		SetLineWidth 1
	End Method
End Type


Type filter
	Method New()
		filters.addlast Self
	End Method
	
	Method apply()
	End Method
	
	Method update()
	End Method
	
	Method draw()
	End Method
End Type

Type mirror Extends filter
	Field g1:grabbable,g2:grabbable
	Field length#
	
	Function Create:mirror(g1:grabbable,g2:grabbable)
		m:mirror=New mirror
		m.g1=g1
		m.g2=g2
		dx#=g2.x-g1.x
		dy#=g2.y-g1.y
		m.length=Sqr(dx*dx+dy*dy)
		Return m
	End Function
	
	Method update()
		dx#=g2.x-g1.x
		dy#=g2.y-g1.y
		d#=Sqr(Dx*dx+dy*dy)
		f#=(length-d)/d
		g1.x:-dx*f/2
		g1.y:-dy*f/2
		g2.x:+dx*f/2
		g2.y:+dy*f/2
	End Method
	
	Method apply()
		dx#=g2.x-g1.x
		dy#=g2.y-g1.y
		an#=ATan2(dy,dx)
		nx#=Cos(an+90)
		ny#=Sin(an+90)
		
		
		For d:dot=EachIn realdots
			lambda#=linesintersect(g1.x,g1.y,g2.x,g2.y,d.g.x,d.g.y,d.g.x+nx,d.g.y+ny,2)
			If lambda>=0
				x#=g1.x+dx*lambda
				y#=g1.y+dy*lambda
				dx=x-d.g.x
				dy=y-d.g.y
				x=d.g.x+2*dx
				y=d.g.y+2*dy
				nd:dot=dot.Create(x,y,d.radius,d.image)
				nd.parent=d
				nd.an=-d.an+2*an
				nd.reflect=-1*d.reflect
				dx=d.ox-d.g.x
				dy=d.oy-d.g.y
				vx#=dx*Cos(2*an)+dy*Sin(2*an)
				vy#=dy*Sin(2*an)-dy*Cos(2*an)
				nd.ox=x+vx
				nd.oy=y+vy
				nd.reflan=an
				seendots.addlast nd
			EndIf
		Next
	End Method
	
	Method draw()
		DrawLine g1.x,g1.y,g2.x,g2.y
	End Method
End Type

Type magnify Extends filter
	Field g:grabbable
	Field radius#
	Field zoom#
	Field hitlist:TList
	
	Function Create:magnify(g:grabbable,radius#,zoom#)
		ma:magnify=New magnify
		ma.g=g
		ma.radius=radius
		ma.zoom=zoom
		Return ma
	End Function
	
	Method update()
	End Method
	
	Method apply()
		For d:dot=EachIn seendots
			dx#=d.g.x-g.x
			dy#=d.g.y-g.y
			dist#=dx*dx+dy*dy
			If dist<radius*radius
				d.magnify:*(zoom^(1-dist/(radius*radius)))
			EndIf
		Next
		
	End Method
	
	Method draw()
		x=g.x+radius
		y=g.y
		For an=0 To 360 Step 10
			nx=g.x+Cos(an)*radius
			ny=g.y+Sin(an)*radius
			DrawLine x,y,nx,ny
			x=nx
			y=ny
		Next
	End Method
End Type


'----------------------------
'          globals

Global gravity#
Global bounce#
Global friction#

Global done

Global realdots:TList
Global seendots:TList
Global walls:TList
Global grabbables:TList
Global filters:TList

Global grabbed:grabbable


Function initgame()
	realdots=New TList
	walls=New TList
	grabbables=New TList
	filters=New TList
	
	grabbed=Null
	
	gravity=.2
	bounce=.95
	friction#=.1
	
	Graphics 800,800,0
	SetBlend ALPHABLEND
	AutoMidHandle True
	
	d:dot=dot.Create(100,300,20,LoadImage("nose.png"))
	'd.ox:-3
	realdots.addlast d
	
	wall.Create(0,700,800,700)
	wall.Create(400,800,800,400)
	'wall.Create(0,400,400,800)
	wall.Create(0,0,0,800)
	wall.Create(0,0,800,0)
	wall.Create(0,800,800,800)
	wall.Create(800,0,800,800)
	
	mirror.Create(grabbable.Create(400,300),grabbable.Create(400,700))
	magnify.Create(grabbable.Create(200,500),150,2)
	
	done=0
	
End Function

Function updateworld()
	mx=MouseX()
	my=MouseY()

	'rejiggle dots
	For d:dot=EachIn realdots
		d.jiggle()
	Next


	'make seen dots
	seendots=realdots.copy()
	For f:filter=EachIn filters
		f.update()
		f.apply()
	Next
	
	If MouseDown(1)
		If grabbed
			grabbed.x=mx
			grabbed.y=my
		Else
			mindist#=-1
			For g:grabbable=EachIn grabbables
				dx#=g.x-mx
				dy#=g.y-my
				dist#=dx*dx+dy*dy
				If (dist<mindist Or mindist=-1) And dist<50 
					mindist=dist
					grabbed=g
				EndIf
			Next
		EndIf
	Else
		grabbed=Null
	EndIf

	'update dots
	For d:dot=EachIn seendots
		d.update()
	Next
	
	'collapse seen dots
	For d:dot=EachIn seendots
		d.collapse()
	Next
End Function

Function drawworld()
	For w:wall=EachIn walls
		w.draw()
	Next
	
	For d:dot=EachIn seendots
		d.draw()
	Next
	
	For f:filter=EachIn filters
		f.draw()
	Next

	Flip
	Cls
End Function

initgame()
While Not done
	updateworld()
	drawworld()
	'Delay 20
	
	If KeyHit(KEY_ESCAPE) Or AppTerminate()
		done=1
	EndIf
Wend