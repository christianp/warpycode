Const floatiness# = .001
Const thrust# = .1
Const blow# = .1
Const drawscale# = 5
Const spinniness#=.4
Const joythreshold# = .5
Const infflow#=.009

Type balloon
	Field x# , y# , vx# , vy#
	Field an# , van#
	Field linf# , rinf# , vinf#
	Field controller
	
	Method New()
		balloons.addlast Self
	End Method
	
	Function create:balloon(controller , x# , y# , inf#)
		b:balloon = New balloon
		b.controller=controller
		b.x = x
		b.y = y
		b.linf = inf
		b.rinf = inf
		Return b
	End Function
	
	Method update()
		vx:* .99
		vy:* .99

		spin(linf * Cos(an) * floatiness)
		spin(-rinf * Cos(an) * floatiness)
		vy:-(linf+rinf)*floatiness
		
		x:+ vx
		y:+ vy
		van:* .99
		an:+ van
		
		diff# = (rinf - linf) * infflow
		vinf:+ diff
		vinf:*.99
		rinf:- vinf
		linf:+vinf
		
		DrawText linf , 0 , 0
		DrawText rinf,0,15
	End Method
	
	Method control()
		jy# = JoyY(controller)
		If Abs(jy)<joythreshold jy=0
		leftthrust# = jy * thrust
		spin(-leftthrust)
		push(0 , leftthrust)
		jr# = JoyR(controller)
		If Abs(jr)<joythreshold jr=0
		rightthrust# = jr * thrust
		spin(rightthrust)
		push(0,rightthrust)
		
		jx# = JoyX(controller)
		If Abs(jx)<joythreshold jx=0
		leftblow# = jx * blow
		push(leftblow , 0)
		linf:+ leftblow
		ju# = JoyU(controller)
		If Abs(ju)<joythreshold ju=0
		rightblow# = ju * blow
		push( rightblow , 0)
		rinf:- rightblow
	End Method
	
	Method spin(amount#)
		van:+ amount*spinniness
	End Method
	
	Method push(px# , py#)
		vx:+ Cos(an) * px + Sin(an) * py
		vy:- Cos(an) * py - Sin(an) * px
	End Method
	
	Method draw()
		ox# = x + Cos(an+180)*(linf * drawscale)
		oy#=y+Sin(an+180)*(linf*drawscale)
		For t# = -1 To 1 Step .1
			r# = (linf * Abs(t) + rinf * (1 - Abs(t) ) ) * drawscale
			r#=(linf*Abs(t*t)+rinf*(1-Abs(t*t)))*drawscale
			sx# = x + Cos(an + t*180) * r
			sy# = y + Sin(an + t*180) * r
			DrawLine ox , oy , sx , sy
			ox = sx
			oy = sy
			DrawRect 400 + t * 50 , r + 100 , 1 , 1
			DrawRect 400+t*50,+200,1,1
		Next
		DrawRect x+linf*drawscale*Cos(an+180),y+linf*drawscale*Sin(an+180),3,3
		DrawRect x + rinf * drawscale * Cos(an) , y + rinf * drawscale * Sin(an) , 3 , 3
		
		DrawLine x,y,x+Cos(an+90)*drawscale*(linf+rinf)/2,y+Sin(an+90)*drawscale*(linf+rinf)/2
	End Method
		
End Type

Global balloons:tlist=New tlist

Graphics 800 , 800 , 0
SetBlend ALPHABLEND

dude:balloon=balloon.create(0,400,400,5)

While Not KeyHit(KEY_ESCAPE)
	
	For b:balloon = EachIn balloons
		b.control()
		b.update()
		b.draw() 
	Next
	
	Flip
	Cls
Wend