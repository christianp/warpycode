Global width=800
Global height=600

Graphics width,height,0

Function bezier(ax#,ay#,v1x#,v1y#,v2x#,v2y#,dx#,dy#,length#,r,g,blue,thickness#)
	'SetAlpha .5
	'SetBlend SHADEBLEND
	mx#=(dx-ax)/length
	my#=(dy-ay)/length
	bx#=ax+v1x
	by#=ay+v1y
	cx#=dx+v2x
	cy#=dy+v2y
	For t=0 To length*2
		a#=t/(length*2)
		b#=1-t/(length*2)
		x#=ax*b*b*b+3*bx*b*b*a+3*cx*b*a*a+dx*a*a*a
		y#=ay*b*b*b+3*by*b*b*a+3*cy*b*a*a+dy*a*a*a
		SetBlend SOLIDBLEND
		SetColor r,g,blue
		DrawLine x,y,x-thickness*my*(a+1),y+thickness*mx*(a+1)
		'SetColor 255,255,255
		'DrawLine ax,ay,dx,dy
		'DrawLine ax,ay,bx,by
	Next
	DrawText v1x,0,0
	DrawText m,0,15
End Function

Type swoosh
	Field a1#,p1#
	Field a2#,p2#
	Field dir
	Field r,g,b
End Type

swooshlist:TList=New TList

For c=1 To 20
	s:swoosh=New swoosh
	s.dir=Rand(0,1)
	s.a1=Rand(360)
	s.p1=Rnd(1,2)
	s.a2=Rand(360)
	s.p2=Rnd(.1,1)
	s.r=Rand(100,255)
	s.g=Rand(100,255)
	s.b=Rand(100,255)
	swooshlist.addlast(s)
Next

SetClsColor 255,255,255
Cls

ms=MilliSecs()
While Not KeyHit(KEY_ESCAPE)
	For s:swoosh=EachIn swooshlist
		s.a1=s.a1+1
		s.a2=s.a2+1
		x1#=s.dir*(width+200)-100
		y1#=height*.5*(Sin(s.a1*s.p1)+1)
		v1x#=(s.dir*2-1)*100
		v1y#=100*Cos(s.a1*s.p1)
		x2#=(1-s.dir)*(width+200)-100
		y2#=height*.5*(Sin(s.a2*s.p2)+1)
		v2x#=(1-s.dir*2)*100
		v2y#=100*Cos(s.a2*s.p2)
		l#=Sqr((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
		
		bezier(x1,y1,v1x,v1y,v2x,v2y,x2,y2,l,s.r,s.g,s.b,150)
	Next

	oldms=ms
	ms=MilliSecs()
	fps#=1000.0/(ms-oldms)
	DrawText fps,0,30
	
	Flip
	Cls 
	'WaitKey()
	'FlushMem
Wend
