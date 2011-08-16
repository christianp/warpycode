Global junctions:tlist
Global roads:tlist

Type area
	Field junctions:junction[3]
	Field roads:road[3]
	Field edged[3]
	
	Method New()
		areas.addlast Self
	End Method
	
	Function create:triangle(junctions:junction[3])
		a:area=New area
		a.junctions=junctions
		For i=0 to

Type junction
	Field root:junction
	Field x#,y#
	Field myroads:tlist
	
	Method New()
		junctions.addlast Self
		myroads=New tlist
	End Method
	
	Function create:junction(x#,y#)
		j:junction=New junction
		j.x=x
		j.y=y
		Return j
	End Function
	
	Method link:road(b:junction)
		r:road=roadto(b)
		If r Return r
		r:road=road.create(Self,b)
		myroads.addlast r
		b.myroads.addlast r
		Return r
	End Method
	
	Method roadto:road(b:junction)
		For r:road=EachIn myroads
			If (r.a=Self And r.b=b) Or (r.a=b And r.b=Self)
				Return r
			EndIf
		Next
	End Method
	
	Method grow()
		DrawRect x,y,7,7
		If myroads.count()>=3 Return
		DrawRect x-7,y-7,7,7
		totan#=0
		n=0
		For r:road=EachIn myroads
			If r.a=Self
				an#=ATan2(r.b.y-y,r.b.x-x)
			Else
				an#=ATan2(r.a.y-y,r.a.x-x)
			EndIf
			n:+1
			totan:+an
		Next
		'Print totan
		If n=0
			an=Rand(360)
		Else
			an#=(totan/n)+180 Mod 360
			If Rand(2)=1
				an:+(Rnd(0,1)*2-1)*90
			EndIf
		EndIf
		an:+Rand(-30,30)
		l#=Rnd(40,100)
		j:junction=junction.create(x+Cos(an)*l,y+Sin(an)*l)
		While Not cansee(j)
			DrawLine x,y,j.x,j.y
			an:+Rnd(-10,10)
			l:*.8
			j.x=x+Cos(an)*l
			j.y=y+Sin(an)*l
			If l<40
				junctions.remove j
				Return
			EndIf
		Wend
		j.root=root
		'link(j)
	End Method
	
	Method linkable()
		If myroads.count()<2 Return 1
	End Method
	
	Method cansee(b:junction)
		For r:road=EachIn roads
			If Not (r.a=b Or r.b=b Or r.a=Self Or r.b=Self)
				lambda#=lineintersect(r.b.x,r.b.y,r.a.x,r.a.y,x,y,b.x,b.y)
				If lambda>=0 And lambda<=1
					'SetColor 0,0,2555
					'DrawLine r.a.x,r.a.y,r.b.x,r.b.y
					'SetColor 0,255,0
					'DrawLine x,y,b.x,b.y
					'SetColor 255,255,255
					'DrawRect r.a.x+lambda*(r.b.x-r.a.x),r.a.y+lambda*(r.b.y-r.a.y),5,5
					'Flip
					'Delay 1000
					Return 0
				EndIf
			EndIf
		Next
		Return 1
	End Method
	
	Method joinup()
		If Not linkable() Return
		closest:junction=Null
		mindist=-1
		For j:junction=EachIn junctions
			If j.myroads.count()<=2 And j<>Self And Not roadto(j) And cansee(j)
				dx#=j.x-x
				dy#=j.y-y
				d#=dx*dx+dy*dy
				If d<mindist Or mindist=-1
					closest=j
					mindist=d
				EndIf
			EndIf
		Next
		If closest
			link(closest)
			Return 1
		EndIf
	End Method
	
	Method update()
		dx#=root.x-x
		dy#=root.y-y
		d#=dx*dx+dy*dy
		p#=Sqr(d)
		If p<50 p=50
		If Rand(p)<20
			If myroads.count()<3 grow()
			joinup()
		EndIf
	End Method
	
	Method draw()
		DrawOval x-3,y-3,6,6
		DrawText myroads.count(),x,y+5
	End Method
End Type

Type road
	Field a:junction,b:junction
	Field length#
	
	Method New()
		roads.addlast Self
	End Method
	
	Function create:road(a:junction,b:junction)
		r:road=New road
		r.a=a
		r.b=b
		dx#=r.a.x-r.b.x
		dy#=r.a.y-r.b.y
		r.length=Sqr(dx*dx+dy*dy)
		Return r
	End Function
	
	Method draw()
		DrawLine a.x,a.y,b.x,b.y
	End Method
End Type

Function lineintersect#(ax#,ay#,x2#,y2#,bx#,by#,x4#,y4#)
	avx#=x2-ax
	avy#=y2-ay
	bvx#=x4-bx
	bvy#=y4-by
	
	lambda#=(by-ay+bvy*(ax-bx)/bvx)/(avy-avx*bvy/bvx)
	Return lambda
End Function

Graphics 800,800,0
SeedRnd MilliSecs()
SetBlend ALPHABLEND

Function initgame()
	junctions=New tlist
	roads=New tlist
	root:junction=junction.create(400,400)
	root.root=root
	finito=0
	timer=0
	fishingline=junction.create(0,0)
	junctions.remove fishingline
End Function


Function game()
	mx=MouseX()
	my=MouseY()
	fishingline.x=mx
	fishingline.y=my
	fishingline.draw()

	For j:junction=EachIn junctions
		j.update()
		'If j.cansee(fishingline)
		'	SetColor 255,0,0
		'Else
		'	SetColor 255,0,0
		'EndIf
		j.draw()
	Next
	
	
	

	For r:road=EachIn roads
		r.draw()
	Next
	
	timer:+1
	DrawText timer,400,0
	
	Flip
	Cls
End Function

Global finito=1
Global timer
Global fishingline:junction
Global mx,my

While Not KeyHit(KEY_ESCAPE)
	If finito
		initgame()
	Else
		game()
	EndIf
Wend