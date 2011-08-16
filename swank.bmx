Const bounce#=1,sproing#=.02

Global lines:TList=New TList
Type line
	Field ox#,oy#
	Field dx#,dy#,nx#,ny#
	Field an#
	
	Method New()
		lines.addlast Self
	End Method
	
	Function create:line(ox#,oy#,an#)
		l:line=New line
		l.ox=ox
		l.oy=oy
		l.an=an
		l.dx=Cos(an)
		l.dy=Sin(an)
		l.nx=-l.dy
		l.ny=l.dx
		Return l
	End Function
	
	Method travel()
		oan#=an
		px#=ox
		py#=oy
		While px>=0 And px<=800 And py>=0 And py<=800
			closest:circle=Null
			mindist#=-1
			For c:circle=EachIn circles
				mu#=(py-c.y+(dy/dx)*(c.x-px))/(ny-dy*nx/dx)
				lambda#=(c.x-px+mu*nx)/dx
				If Abs(mu)<c.r
					oomph#=sproing/mu
					If oomph>c.v oomph=c.v
					c.vx:-oomph*nx
					c.vy:-oomph*ny
					If lambda>0 
						man#=ACos(mu/c.r)
						an1#=an+man+90
						olx#=c.x+Cos(an1)*c.r
						oly#=c.y+Sin(an1)*c.r
						dfx#=olx-ox
						dfy#=oly-oy
						d#=dfx*dfx+dfy*dfy
						If (d<mindist Or mindist=-1)
							closest=c
							mindist=d
						EndIf
					EndIf
				EndIf
			Next
			If closest 'wrap around a circle
				c=closest
				mu#=(py-c.y+(dy/dx)*(c.x-px))/(ny-dy*nx/dx)
				lambda#=(c.x-ox+mu*nx)/dx
				man#=ACos(mu/c.r)
				an1#=an+90*Sgn(mu)
				an2#=an-man+90
				'SetColor 255,0,0
				'DrawLine c.x,c.y,c.x+Cos(an1)*c.r,c.y+Sin(an1)*c.r
				'SetColor 0,0,255
				'DrawLine c.x,c.y,c.x+Cos(an2)*c.r,c.y+Sin(an2)*c.r
				'SetColor 255,255,255
				diff#=andiff(an1,an2)
				anadd#=0
				olx#=c.x+Cos(an1)*c.r
				oly#=c.y+Sin(an1)*c.r
				DrawLine px,py,olx,oly
				While anadd*diff<diff*diff
					anadd:+5*Sgn(diff)
					If anadd*diff>diff*diff anadd=diff
					px#=c.x+Cos(an1+anadd)*c.r
					py#=c.y+Sin(an1+anadd)*c.r
					DrawLine olx,oly,px,py
					olx=px
					oly=py
				Wend
				recalcan(an2+90*Sgn(diff))
				'Return
			Else 'no more circles to hit - head for edge
				diff#=andiff(an,oan)
				recalcan(an+diff*.01)
				opx#=px
				opy#=py
				px:+dx*5
				py:+dy*5
				DrawLine opx,opy,px,py
			EndIf
		Wend
		an=oan
		dx=Cos(an)
		dy=Sin(an)
		nx=-dy
		ny=dx
	End Method
	
	Method recalcan(pan#)
		an=pan
		dx=Cos(an)
		dy=Sin(an)
		nx=-dy
		ny=dx
	End Method
End Type

Global circles:TList=New TList
Type circle
	Field x#,y#,r#
	Field vx#,vy#,v#
	
	Method New()
		circles.addlast Self
	End Method
	
	Function create:circle(x#,y#,r#)
		c:circle=New circle
		If x<r x=r
		If x>800-r x=800-r
		If y<r y=r
		If y>800-r y=800-r
		c.x=x
		c.y=y
		c.r=r
		an=Rand(360)
		c.vx=Cos(an)*.5'50/c.r
		c.vy=Sin(an)*.5'50/c.r
		Return c
	End Function
	
	Method update()
		v#=vx*vx+vy*vy
		If v>5
			vx=5*vx/v
			vy=5*vy/y
		EndIf
		x:+vx
		y:+vy
		If x<r Or x>800-r
			vx=-vx
			x:+vx
		EndIf
		If y<r Or y>800-r
			vy=-vy
			y:+vy
		EndIf
		
		For c:circle=EachIn circles
			If c<>Self
				dx#=c.x-x
				dy#=c.y-y
				d#=dx*dx+dy*dy
				If d<(c.r+r)*(c.r+r)
					d=Sqr(d)
					dx:/d
					dy:/d
					diffd#=-(c.r+r-d)/2
					x:+diffd*dx
					y:+diffd*dy
					dotprod#=(vx-c.vx)*dx+(vy-c.vy)*dy
					oomph#=c.r/(r+c.r)
					vx:-2*dx*dotprod*oomph*bounce
					vy:-2*dy*dotprod*oomph*bounce
					c.x:-diffd*dx
					c.y:-diffd*dy
					oomph#=r/(r+c.r)
					c.vx:+2*dx*dotprod*oomph*bounce
					c.vy:+2*dy*dotprod*oomph*bounce
				EndIf
			EndIf
		Next
	End Method
End Type

Function lineintersectcircle(l:line,c:circle)
End Function

Function andiff#(an1#,an2#)
	diff#=(an2-an1) Mod 360
	If diff<-180 diff:+360
	If diff>180 diff:-360
	Return diff
End Function


Graphics 800,800,0
SeedRnd MilliSecs()
SetBlend ALPHABLEND

l:line=line.create(0,400,30)
l2:line=line.create(800,400,200)
For n=1 To 9
	circle.create(Rand(800),Rand(800),Rand(30,100))
Next
While Not KeyHit(KEY_ESCAPE)
	For c:circle=EachIn circles
		c.update()
		SetAlpha .4
		DrawOval c.x-c.r,c.y-c.r,c.r*2,c.r*2
		SetAlpha 1
	Next	
	
	c.x=MouseX()
	c.y=MouseY()
	c.vx=0
	c.vy=0
	
	For l:line=EachIn lines
		l.travel()
	Next
		
	Flip
	Cls
Wend