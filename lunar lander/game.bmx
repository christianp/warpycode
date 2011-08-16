Const gravity#=0.01



Function newgame()
	lander.init
	flame.init
	wall.init
	
	player:lander=lander.Create(100,-400)
	player.controls=deflandercontrols


	x#=-1000
	y#=0
	While x<1000
		ox#=x
		oy#=y
		y#=oy+(Rnd(200)-y)*.5
		x:+Rnd(100,200)
		If x>=1000
			x=1000
			y=0
		EndIf
		wall.Create ox,oy,x,y
	Wend
	
	'wall.Create(-300,0,0,-100)
	'wall.Create(0,-100,300,0)
	'wall.Create(-300,0,300,0)
End Function

Function updategame()
	For l:lander=EachIn landers
		l.update
	Next
	
	For f:flame=EachIn flames
		f.update
	Next
End Function

Function drawgame()
	DrawText "LANDER "+landers.count(),400,0
	For l:lander=EachIn landers
		l.draw
	Next
	For f:flame=EachIn flames
		f.draw
	Next
	
	For w:wall=EachIn walls
		w.draw
	Next
	
	'drawzoomline -1000,0,1000,0
End Function



Global landers:TList
Global landerimage:timage
Type lander
	Field x#,y#
	Field an#
	Field vx#,vy#,van#
	Field controls:landercontrols
	Field rockets:TList
	Field corners:TList

	Function init()
		landers=New TList
		landercontrols.init
		landerimage=LoadImage("lander.png")
	End Function
	
	Method New()
		controls=New landercontrols
		rockets=New TList
		corners=New TList
		
		landers.addlast Self
	End Method
	
	Function Create:lander(x#,y#)
		l:lander=New lander
		l.x=x
		l.y=y
		
		l.addrocket 45,5,90,.01,landercontrol_left
		l.addrocket -45,5,90,.01,landercontrol_right
		
		l.addcorner 35,50
		l.addcorner -35,50
		
		Return l
	End Function

	
	Method addrocket(dx#,dy#,an#,strength#,control)
		d#=Sqr(dx*dx+dy*dy)
		theta#=ATan2(dy,dx)
		
		rot#=Sin(theta-an)*1000
		'push#=-Cos(theta-an)
		px#=-Cos(an)
		py#=-Sin(an)
		'Print "px: "+px+"~npy: "+py
		
		r:rocket=New rocket
		r.x=dx
		r.y=dy
		r.an=an
		r.d=d
		r.strength=strength
		'r.van#=strength*rot
		r.van=strength*rot/(2*Pi*d)
		r.vx=strength*px
		r.vy=strength*py
		r.control=control
		rockets.addlast r
	End Method
	
	Method addcorner(dx#,dy#)
		co:corner=New corner
		co.x=dx
		co.y=dy
		co.ox=x+dx*Cos(an)-dy*Sin(an)
		co.oy=y+dx*Sin(an)+dy*Cos(an)
		corners.addlast co
	End Method
	
	Method update()
		c#=Cos(an)
		s#=Sin(an)

		ox#=x
		oy#=y
		x:+vx
		y:+vy
		vy:+gravity
		an:+van
		
		nx#=x
		ny#=y
		For co:corner=EachIn corners
			cx#=x+c*co.x-s*co.y
			cy#=y+s*co.x+c*co.y
			For w:wall=EachIn walls
				ovx#=cx-co.ox
				ovy#=cy-co.oy
				Local fx#,fy#
				If w.bounce(co.ox,co.oy,cx,cy,fx,fy)
					nvx#=cx-co.ox
					nvy#=cy-co.oy
					'x:+nvx
					'y:+nvy
					ov#=Sqr(vx*vx+vy*vy)
					nx:+(x+fx)-nx
					ny:+(y+fy)-ny
					nv#=Sqr(vx*vx+vy*vy)
					'Print "diff"+(nv-ov)
					
					fx:*.25
					fy:*.25
					push co.x,co.y,fx,fy
				EndIf
			Next
			co.ox=cx
			co.oy=cy
		Next
		dx#=nx-x
		dy#=ny-y
		'Print "accel"+dx+","+dy
		vx:+dx
		vy:+dy
		If dx Or dy
			vx:*.2
			vy:*.2
		EndIf
		x:+dx
		y:+dy
		
		c#=Cos(an)
		s#=Sin(an)

		For r:rocket=EachIn rockets
			v#=controls.value(r.control)
			sx#=x+r.x*c-r.y*s
			sy#=y+r.x*s+r.y*c
			fvx#=vx+Sin(an)*r.d*2*Pi*van*.001
			fvy#=vy-Cos(an)*r.d*2*Pi*van*.001
			For i=1 To poisson(1*v)
				fan#=an+r.an+Rnd(-10,10)*v
				fv#=Rnd(.9,1.1)*r.strength*100
				flame.Create(sx,sy,fvx+Cos(fan)*fv,fvy+Sin(fan)*fv)
			Next
			'dan#=r.van*v
			'dx#=v*(r.vx*c-r.vy*s)
			'dy#=v*(r.vx*s+r.vy*c)
			f#=v*r.strength
			fx#=-f*Cos(r.an+an)
			fy#=-f*Sin(r.an+an)
			vx:+fx
			vy:+fy
			push r.x,r.y,fx,fy
		Next
		Rem
		'controls
		If controls.u.value()
			vx:+Sin(an)*.1
			vy:-Cos(an)*.1
		EndIf
		turn#=controls.r.value()-controls.l.value()
		van:+turn*.1
		EndRem
		
		dx#=x-panx
		dy#=y-pany
		d#=Sqr(dx*dx+dy*dy)
		dx:/d
		dy:/d
		d:-gwidth/4
		If d>0
			panx:+dx*.1*d
			pany:+dy*.1*d
		EndIf
	End Method
	
	Method push(dx#,dy#,fx#,fy#)
		d#=Sqr(dx*dx+dy*dy)
		theta#=ATan2(dy,dx)+an
		rot#=(-Sin(theta)*fx+Cos(theta)*fy)*1000
		van:+rot/(2*Pi*d)
		
	End Method
	
	Method draw()
		drawzoomline 0,0,x,y
		c#=Cos(an)
		s#=Sin(an)
		DrawzoomLine x-c*50,y-s*50,x+c*50,y+s*50
		SetRotation an
		drawzoomimage landerimage,x,y,100
		SetRotation 0
		
		SetColor 255,0,0
		For co:corner=EachIn corners
			cx#=x+Cos(an)*co.x-Sin(an)*co.y
			cy#=y+Sin(an)*co.x+Cos(an)*co.y
			drawzoomcircle cx,cy,5
		Next
		SetColor 255,255,255
		
		v#=vx*vx+vy*vy
		DrawText v,400,15
	End Method
End Type

Type corner
	Field x#,y#
	Field ox#,oy#
End Type

Type rocket
	Field x#,y#,an#,d#
	Field strength#
	Field van#
	Field vx#,vy#
	Field control
End Type

Global flames:TList
Type flame
	Field x#,y#,vx#,vy#
	Field life#
	
	Function init()
		flames=New TList
	End Function
	
	Method New()
		flames.addlast Self
	End Method
	
	Function Create:flame(x#,y#,vx#,vy#)
		f:flame=New flame
		f.x=x
		f.y=y
		f.vx=vx
		f.vy=vy
		f.life=1
		Return f
	End Function
	
	Method update()
		life:-.01
		If life<=0
			flames.remove Self
		EndIf
		x:+vx
		y:+vy
	End Method
	
	Method draw()
		SetAlpha Sqr(life)
		drawzoomrect x,y,1,1
		SetAlpha 1
	End Method
End Type

Global deflandercontrols:landercontrols
Const landercontrol_up=0, landercontrol_down=1, landercontrol_left=2, landercontrol_right=3
Type landercontrols
	Field inputs:manyInput[4]
	
	Function init()
		deflandercontrols=New landercontrols
		deflandercontrols.addinput landercontrol_up,tinput.Create(input_keyboard,KEY_UP)
		deflandercontrols.addinput landercontrol_down,tinput.Create(input_keyboard,KEY_DOWN)
		deflandercontrols.addinput landercontrol_left,tinput.Create(input_keyboard,KEY_LEFT)
		deflandercontrols.addinput landercontrol_right,tinput.Create(input_keyboard,KEY_RIGHT)
	End Function
	
	Method New()
		For i=0 To 3
			inputs[i]=New manyinput
		Next
	End Method
	
	Method addinput(control,ti:tinput)
		Print "add input "+control
		inputs[control].addinput ti
	End Method
	
	Method value#(i)
		Return inputs[i].value()
	End Method
End Type




Global walls:TList
Type wall
	Field sx#,sy#,ex#,ey#
	Field dx#,dy#
	Field nx#,ny#
	Field an#,length#
	
	Function init()
		walls=New TList
	End Function
	
	Method New()
		walls.addlast Self
	End Method
	
	Function Create:wall(sx#,sy#,ex#,ey#)
		w:wall=New wall
		w.sx=sx
		w.sy=sy
		w.ex=ex
		w.ey=ey
		
		dx#=ex-sx
		dy#=ey-sy
		d#=Sqr(dx*dx+dy*dy)
		w.an#=ATan2(dy,dx)
		w.length=d
		w.dx=dx/d
		w.dy=dy/d
		w.nx=-Sin(w.an)
		w.ny=Cos(w.an)
		Return w
	End Function
	
	Method bounce(ox# Var,oy# Var,x# Var,y# Var,fx# Var,fy# Var)
		lambda#=linesintersect(ox,oy,x,y,sx,sy,ex,ey)
		If lambda=-1 Return
		
		vx#=x-ox
		vy#=y-oy
		
		ix#=ox+vx*lambda
		iy#=oy+vy*lambda
		
		dp#=vx*nx+vy*ny
		fx#=-2*nx*dp
		fy#=-2*ny*dp
		v#=Sqr(vx*vx+vy*vy)
		vx:+fx
		vy:+fy
		x=ix+vx*(1-lambda)
		y=iy+vy*(1-lambda)
		ox=x-vx
		oy=y-vy

		'SetColor 255,0,0
		'DrawlongerLine x,y,ox,oy
		'drawzoomcircle ix,iy,3
		'SetColor 0,255,0
		'DrawlongerLine ix,iy,x,y
		'SetColor 0,0,255
		'DrawlongerLine x,y,x+vx*10,y+vy*10
		'SetColor 255,255,255
		'drawlongerline ix,iy,ix+nx,iy+ny
		'Flip
		'Delay 2000

		Return 1
	End Method
	
	Method draw()
		drawzoomline sx,sy,ex,ey
	End Method
End Type

Global planets:TList
Type planet
	Field x#,y#,r#
	
	Function Create:planet(x#,y#,r#)
	End Function
End Type



Function drawlongerline(x1,y1,x2,y2)
	dx=x2-x1
	dy=y2-y1
	drawzoomline x1,y1,x1+dx*10,y1+dy*10
End Function


