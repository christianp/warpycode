Const screenwidth=800,screenheight=800
Graphics screenwidth,screenheight,0

Function bezier(ax#,ay#,v1x#,v1y#,v2x#,v2y#,dx#,dy#,length#,r,g,blue,fx#,fy#,shade#)
	SetAlpha shade
	SetBlend ALPHABLEND
	SetColor r,g,blue
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
		'SetBlend SOLIDBLEND
		DrawRect x,y,1,1
		'DrawLine fx,fy,x,y
		'DrawLine x,y,x-thickness*my*(a+1),y+thickness*mx*(a+1)
		'SetColor 255,255,255
		'DrawLine ax,ay,dx,dy
		'DrawLine ax,ay,bx,by
	Next
		
	DrawText v1x,0,0
	DrawText m,0,15
End Function

Global splots:TList=New TList

Type splot
	Field x#,y#
	Field life
	Field prongs:TList
	
	Function Create:splot(numprongs)
		s:splot=New splot
		s.life=Rand(100,300)
		s.prongs=New TList
		s.x=Rand(screenwidth)
		s.y=Rand(screenheight)
		space#=360.0/numprongs
		For c=1 To numprongs
			p:prong=New prong
			p.an=space*(c+Rnd(-.5,.5))
			p.v=Rnd(3,10)
			p.l=0
			p.slow#=Rnd(.9,.95)
			s.prongs.addlast(p)
		Next
		splots.addlast(s)
		Return s
	End Function
End Type

Type prong
	Field l#,v#,an#,slow#
End Type

splot.create(15)
While Not KeyHit(KEY_ESCAPE)
	If Rand(100)=1 Then splot.Create(Rand(8,20))

	For s:splot=EachIn splots
		lastp:prong=prong(s.prongs.Last())
		ox#=s.x+lastp.l*Sin(lastp.an)
		oy#=s.y+lastp.l*Cos(lastp.an)
		For p:prong=EachIn s.prongs
			x#=s.x+p.l*Sin(p.an)
			y#=s.y+p.l*Cos(p.an)
			'SetColor 255,255,255
			'DrawLine s.x,s.y,x,y
			bezier(ox,oy,Sin(lastp.an+90)*lastp.l/5,Cos(lastp.an+90)*lastp.l/5,-Sin(p.an+90)*p.l/5,-Cos(p.an+90)*p.l/5,x,y,100,255,255,255,s.x,s.y,s.life/300.0)
			ox=x
			oy=y
			p.l=p.l+p.v
			p.v=p.v*p.slow
			lastp=p
		Next
		's.life=s.life-1
		If s.life=0
			splots.remove(s)
		EndIf
	Next
	
	Flip
	Cls
Wend