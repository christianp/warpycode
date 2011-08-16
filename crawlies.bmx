Global width=800,height=800
Global linelist:TList=New TList
Global crawlylist:TList=New TList

Const sproing#=.1

Graphics width,height,0
SetBlend SOLIDSHADE
SetColor 255,255,255

Type vector
	Field x#,y#
End Type

Type line
	Field x1#,y1#,x2#,y2#
	Field dx#,dy#
	Field an#,length#
	
	Method distancefrompoint:vector(sx#,sy#)
		u#=((sx-x1)*(x2-x1)+(y3-y1)*(y2-y1))/(length*length)
		a#=y1
		v:vector=New vector
		v.x=x1+u*(x2-x1)
		v.y=y1+u*(y2-y1)
		'RuntimeError "boo"
		Return v
	End Method
	
End Type

Type crawly
	Field x#,y#,vx#,vy#
	Field numlegs,legs:TList
End Type

Type leg
	Field x#,y#,length#,life
End Type


Function newline:line(x1#,y1#,x2#,y2#)
	l:line=New line
	l.x1=x1
	l.x2=x2
	l.y1=y1
	l.y2=y2
	dx#=l.x2-l.x1
	dy#=l.y2-l.y1
	l.length#=Sqr(dx*dx+dy*dy)
	l.dx=dx/l.length
	l.dy=dy/l.length
	l.an=ATan2(dy,dx)
	linelist.addlast l
	Return l
End Function

Function newcrawly:crawly(x#,y#)
	c:crawly=New crawly
	c.x=x
	c.y=y
	c.numlegs=0
	c.legs=New TList
	crawlylist.addlast c
	Return c
End Function

'newline(0,height/2+100,width,height/2+150)
'newline(0,height/2-50,width,height/2)
newline(width/2-250,0,width/2,height)
newcrawly(width/2,height/2)

While Not KeyHit(KEY_ESCAPE)
	For l:line=EachIn linelist
		DrawLine l.x1,l.y1,l.x2,l.y2
	Next

	For c:crawly=EachIn crawlylist
		c.x=c.x+c.vx
		c.y=c.y+c.vy
		c.vx=c.vx*.99
		c.vy=c.vy*.99+.1
		If c.numlegs<4 And Rand(10)=1
			mindist#=-1
			closestpoint:vector=Null
			closestline:line=Null
			For l:line=EachIn linelist
				v:vector=l.distancefrompoint(c.x,c.y)
				dx#=v.x-c.x
				dy#=v.y-c.y
				d#=Sqr(dx*dx+dy*dy)
				If d<mindist Or mindist=-1
					mindist=d
					'RuntimeError mindist
					closestpoint=v
					closestline=l
				EndIf
			Next
			If closestpoint<>Null
				u#=Rnd(1,1)*(closestpoint.x-closestline.x1)/closestline.dx
				l:line=closestline
				x#=l.x1+u*l.dx
				y#=l.y1+u*l.dy
				'RuntimeError v.x
				g:leg=New leg
				g.x=x
				g.y=y
				g.length=50
				g.life=Rand(20,100)
				c.numlegs=c.numlegs+1
				c.legs.addlast g
			EndIf
		EndIf
		For g:leg=EachIn c.legs
			dx#=c.x-g.x
			dy#=c.y-g.y
			d#=Sqr(dx*dx+dy*dy)
			f#=sproing*(d-g.length)/g.length
			dx=dx/d
			dy=dy/d
			c.vx=c.vx-f*dx
			c.vy=c.vy-f*dy
			DrawLine c.x-dx*5,c.y-dy*5,g.x,g.y
			g.life=g.life-1
			If g.life=0
				c.legs.remove g
				c.numlegs=c.numlegs-1
			EndIf
			an#=ATan2(dy,dx)-90
			DrawText an,g.x,g.y
		Next
	Next

	Flip
	Cls
Wend