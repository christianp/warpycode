Global stopwatches:TList,numwatches

Global joyN
Global debugging
Global drawnonarcs
Global level#

Global clinks:TList
Global pops:TList

Global circles:TList
Global walls:TList
Global things:TList
Global jewels:TList
Global p:player
Global numabove

Global ms



'sound items

Global sounditems:TList

Type sounditem
	Field chan:TChannel
	Field loops
	Field sound:TSound
	
	Function Create:sounditem(sound:TSound,rate#=1,pan#=0,volume#=1,loops=0)
		si:sounditem=New sounditem
		si.chan=AllocChannel()
		SetChannelRate(si.chan,rate)
		SetChannelPan(si.chan,pan)
		SetChannelVolume si.chan , volume
		si.sound = sound
		si.loops=loops
		PlaySound(si.sound,si.chan)
		sounditems.AddLast si
		Return si
	End Function
	
	Method Delete()
		StopChannel chan
	End Method

	Method update()	
		If Not ChannelPlaying(chan)
			If loops
				PlaySound sound , chan
			Else
				sounditems.Remove Self
			EndIf
		EndIf
	End Method
	
End Type

Function loadsounds:TList(name$,n,ext$)
	l:TList=New TList
	For i=1 To n
		s:TSound=LoadSound(name+String(i)+ext)
		l.addlast s
	Next
	Return l
End Function

Function choosesound:TSound(l:TList)
	n=Rand(0,l.count()-1)
	Return TSound(l.valueatindex(n))
End Function


'blah blah graphics rigmarole
Global gwidth#,gheight#
Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom

Function ZoomX#(x#)
	Return (x - panx) * zoom + gwidth / 2
End Function
Function ZoomY#(y#)
	Return (y - pany) * zoom + gheight / 2
End Function

Function UnzoomX#(x#)
	Return (x - gwidth / 2) / zoom + panx
End Function
Function UnzoomY#(y#)
	Return (y - gheight / 2) / zoom + pany
End Function

Function DrawZoomPoly(poly#[],outline=False)
	'poly=poly[..]
	While i < Len(poly)
		poly[i] = zoomx(poly[i])
		poly[i + 1] = zoomy(poly[i + 1]) 
		i:+ 2
	Wend
	If outline
		ox# = poly[0]
		oy# = poly[1]
		i = 2
		While i < Len(poly)
			DrawLine ox , oy , poly[i] , poly[i + 1]
			ox = poly[i]
			oy=poly[i+1]
			i:+ 2
			DrawLine poly[0],poly[1],ox,oy
		Wend
	Else
		DrawPoly poly
	EndIf
End Function

Function DrawZoomLine(ax# , ay# , bx# , by#)
	ax = zoomx(ax)
	ay = zoomy(ay)
	bx = zoomx(bx)
	by = zoomy(by)
	DrawLine ax,ay,bx,by
End Function

Function DrawZoomRect(x# , y# , width# , height#,zoomdimensions=1,filled=1)
	x = zoomx(x)
	y = zoomy(y)
	If zoomdimensions
		width:* zoom
		height:* zoom
	EndIf
	If filled
		DrawRect x , y , width , height
	Else
		DrawLine x , y , x + width , y
		DrawLine x + width , y , x + width , y + height
		DrawLine x , y , x , y + height
		DrawLine x , y + height , x + width , y + height
	EndIf
End Function

Function DrawZoomCircle(x# , y# , radius#)
	x = zoomx(x) 
	y = zoomy(y)
	radius:* zoom
	DrawOval x - radius , y - radius , 2 * radius , 2 * radius
End Function

Function DrawZoomText(txt$ , x# , y#)
	x = ZoomX(x)
	y = ZoomY(y)
	DrawText txt , x , y
End Function

Function DrawZoomImage(image:TImage , x# , y#,width#,heighto=0)
	If heighto
		w# = width / ImageHeight(image)
	Else
		w# = width / ImageWidth(image)
	EndIf
	SetScale w*zoom , w*zoom
	DrawImage image , zoomx(x) , zoomy(y)
	SetScale 1,1
End Function




'------------------------
'------------------------
'------------------------
'------------------------
'----real game stuff-----
'------------------------
'------------------------
'------------------------
'------------------------


Type circle
	Field x#,y#,r#
	Field nonarcs:TList
	Field allnonarcs:TList
	Field vx#,vy#
	Field connects:TList
	Field checked
	Field hidden
	Field occupied
	Field pushx#,pushy#
	
	Field colourcode
	Field red,green,blue
	
	Field maxr#,numjewels
	

	Method New()
		circles.addlast Self
		nonarcs=New TList
		allnonarcs=New TList
		connects=New TList
		
		Local probs[6]
		probs=[1,1,1,1,1,1]
		prob=Rand(1,6)
		t=0
		colourcode=0
		While t<prob
			t:+probs[colourcode]
			colourcode:+1
		Wend
		
		red=(colourcode & 4)*255
		green=(colourcode & 2)*255
		blue=(colourcode & 1)*255


		mr#=Rnd(.1,1)*gwidth*Sqr(2)
		an#=Rnd(360)
		x=panx+mr*Cos(an)
		y=pany+mr*Sin(an)
		
		sf#=Rnd(.5,1.5)
		maxr=50*sf

		
		van#=Rnd(-40,40)+an+180
		v#=Rnd(.25,1)
		vx=v*Cos(van)
		vy=v*Sin(van)

		numjewels=Rand(2,10)*sf
		For n=0 To numjewels
		Next
	End Method
	
	Method addjewel()
		j:jewel=New jewel
		an=Rnd(360)
		d#=Rnd(0,r-1)
		j.x=x+d*Cos(an)
		j.y=y+d*Sin(an)
		j.strength=1
		j.red=red
		j.green=green
		j.blue=blue
		numjewels:-1
	End Method
	
	Method testmadness(txt$,ending=1)
		If x=1/0.0 Or y=1/0.0 Or vx=1/0.0 Or vy=1/0.0
			Print "madness at: "+txt
			Print(x)
			Print(y)
			Print vx
			Print vy
			If ending End
		EndIf
	End Method
	
	Method update()
		testmadness "start"
		

		v#=Sqr(vx*vx+vy*vy)
		testmadness "velocity "+String(v),0
		If v>5
			vx:*5/v
			vy:*5/v
			testmadness "too fast "+String(v)
		EndIf
		vx:-.001*v*vx
		vy:-.001*v*vy
		testmadness "friction "+String(v)

		x:+pushx
		y:+pushy
		vx:+pushx
		vy:+pushy
		testmadness "pushed"
		pushx=0
		pushy=0
		x:+vx
		y:+vy
		testmadness "moving"
		
		
		
		'move into view
		dx#=x-panx
		dy#=y-pany
		d#=dx*dx+dy*dy
		If d>gwidth*gwidth/(zoom*zoom*4)
			vx:-dx*.000007
			vy:-dy*.000007
			testmadness "move into view"
		EndIf
		
		If numjewels
			r:+1
			If r>maxr Then r=maxr
			If r=maxr And Rand(5)=1 Then addjewel
		Else
			If (Not occupied) Or hidden
				r:-.1
			EndIf
			If r<=5
				die
			EndIf
		EndIf
		
		If y>pany+gheight*1.5
			die
		EndIf
		
		hidden=0
		occupied=0

		connects=New TList
		connects.addlast Self
		
	End Method
	
	Method die()
		circles.remove Self
		'sounditem.Create(choosesound(pops),.9)
		
		'New circle
	End Method
	
	Method connectedto(c2:circle)
		Return connects.contains(c2)
		For c:circle=EachIn circles
			c.checked=0
		Next
		Return lookfor(c2)
	End Method
	
	Method lookfor(c2:circle)
		checked=1
		If Self=c2 Then Return 1
		For c:circle=EachIn connects
			If Not c.checked
				If c.lookfor(c2) Then Return 1
			EndIf
		Next
		Return 0
	End Method
	
	Method intersect(c2:circle)
		If r=0 Or c2.r=0 Then Print "r=0???"
		'debugo "<<<<<"
		dx#=c2.x-x
		dy#=c2.y-y
		d=Sqr(dx*dx+dy*dy)
		If d=0 Return
		dx:/d
		dy:/d
		If d>=r+c2.r Then Return
		
		For c:circle=EachIn c2.connects
			If Not connects.contains(c)
				connects.addlast c
			EndIf
		Next
		For c:circle=EachIn connects
			If Not c2.connects.contains(c)
				c2.connects.addlast c
			EndIf
		Next
		
		
		overlap#=r+c2.r-d

		wallnum=colourcode & c2.colourcode
		If wallnum=0
			If c2.r+r=0 Then Print "OOPS"
			push1#=overlap*c2.r/(c2.r+r)
			pushx:-dx*push1
			pushy:-dy*push1
			push2#=overlap*r/(c2.r+r)
			c2.pushx:+dx*push2
			c2.pushy:+dy*push2
			Return
		EndIf


		f#=.00008
		f1#=overlap*c2.r/r
		vx:-dx*f1*f
		vy:-dy*f1*f
		f2#=overlap*r/c2.r
		c2.vx:+dx*f2*f
		c2.vy:+dy*f2*f
		testmadness "overlap"
		
		
		fullcircle:nonarc=nonarc.Create(0,360)
		If r>c2.r
			If d+c2.r<=r
				c2.nonarcs=New TList
				c2.nonarcs.addlast fullcircle
				c2.hidden=1
				'debugo "removed second circle!"
				Return
			EndIf
		Else
			If d+r<=c2.r
				nonarcs=New TList
				nonarcs.addlast fullcircle
				hidden=1
				'debugo "removed first circle"
				Return
			EndIf
		EndIf
		
		b#=(r*r-c2.r*c2.r+d*d)/(2*d)
		theta#=ACos(b/r)
		phi#=ATan2(dy,dx)
		strength#=Sqr(theta/180)
		addnonarc(phi-theta,phi+theta,c2.red,c2.green,c2.blue,strength,c2.red,c2.green,c2.blue,strength,1)
		
		
		theta2#=ACos((d-b)/c2.r)
		phi2#=phi+180
		strength2#=Sqr(theta2/180)
		c2.addnonarc(phi2-theta2,phi2+theta2,red,green,blue,strength2,red,green,blue,strength2,1)
		
		
		Rem
		wallnum=colourcode & c2.colourcode
		If wallnum=0
			x1#=x+Cos(phi-theta)*r
			y1#=y+Sin(phi-theta)*r
			x2#=x+Cos(phi+theta)*r
			y2#=y+Sin(phi+theta)*r
			w:wall=wall.Create(x1,y1,x2,y2,(vx+c.vx)/2,(vy+c.vy)/2)
			DrawText "wall",400,12
		EndIf

		wallred=wallnum & 4
		wallgreen=wallnum & 2
		wallblue=wallnum & 1
		
		SetColor wallred*255,wallgreen*255,wallblue*255
		
		
		
		'drawzoomline x1,y1,x2,y2
		EndRem
		
	End Method
	
	Method addnonarc(b1#,b2#,red1,green1,blue1,s1#,red2,green2,blue2,s2#,orig=0)
		newna:nonarc=nonarc.Create(b1,b2,red1,green1,blue1,s1,red2,green2,blue2,s2)
		If orig Then allnonarcs.addlast newna
		For na:nonarc=EachIn nonarcs
			'debugo "compare "+newna.repr()+" with "+na.repr()
			If na.contains(b1)
				If na.contains(b2)
					If newna.contains(na.an1) And newna.contains(na.an2)
						'debugo("both arcs contained in each other - full circle")
						nonarcs=New TList
						nonarcs.addlast nonarc.Create(0,360)
						hidden=1
						Return
					EndIf
					'debugo("new arc wholly contained in old arc")
					Return
				Else
					'debugo("a1->b2")
					nonarcs.remove na
					addnonarc(na.an1,b2,na.red1,na.green1,na.blue1,na.s1,red2,green2,blue2,s2)
					Return
				EndIf
			Else
				If na.contains(b2)
					'debugo("b1->a2")
					nonarcs.remove na
					addnonarc(b1,na.an2,red1,green1,blue1,s1,na.red2,na.green2,na.blue2,na.s2)
					Return
				Else
					If newna.contains(na.an1)
						'debugo("old arc wholly contained in new arc")
						nonarcs.remove na
					Else
						'debugo("arcs don't intersect")
					EndIf
				EndIf
			EndIf
		Next
		nonarcs.addlast newna
	End Method
	
	
	Method onscreen()
		zx=zoomx(x)
		zy=zoomy(y)
		If zx>-r And zx<gwidth+r And zy>-r And zy<gheight+r Then Return 1 Else Return 0
	End Method
	
	Method draw()
		If Not onscreen() Return
		
		SetColor 0,0,0
		'Drawzoomcircle x,y,r
		
		SetColor red,green,blue
		Select nonarcs.count()
		Case 0 'this circle doesn't intersect any other, so just draw all of it
			drawarc(0,360,red,green,blue,1,red,green,blue,1)
		Case 1 'this circle intersects one other, so only need to look at one nonarc
			na:nonarc=nonarc(nonarcs.first())
			If Not (na.an1=0 And na.an2=360)
				drawarc(na.an2,na.an1,na.red2,na.green2,na.blue2,na.s2,na.red1,na.green1,na.blue1,na.s1)
			EndIf
		Default 'this circle intersects several others, so work through list of nonarcs, drawing 
				'from end of previous one to start of next one
			SortList nonarcs,True,nonarc.comparestarts
			ona:nonarc=nonarc(nonarcs.last())
			For na:nonarc=EachIn nonarcs
				drawarc(ona.an2,na.an1,ona.red2,ona.green2,ona.blue2,ona.s2,na.red1,na.green1,na.blue1,na.s1)
				ona=na
			Next
		End Select
		
		n=3
		For na:nonarc=EachIn nonarcs
			If Not (na.an1=0 And na.an2=360)
				'drawnonarc(na)
				'SetColor n*50,0,0
				'drawarc(na.an1,na.an2,-1,0,0,0,255,0,0,0)
				'n:+1

				'midan#=na.an1+andiff(na.an2,na.an1)/2
				'ex#=x+r*Cos(midan)
				'ey#=y+r*Sin(midan)
				'DrawzoomLine x,y,ex,ey
			EndIf
		Next
		
		SetColor 255,255,255
	End Method
		
	Method drawarc(a#,b#,red1=-1,green1=-1,blue1=-1,s1#=1,red2=-1,green2=-1,blue2=-1,s2#=1)
		SetLineWidth 3
		If b<a Then b:+360	
		steps=2*Pi*r/10
		If r<15 Then steps=9
		s#=360.0/steps
		theta#=a
		ox#=x+r*Cos(theta)
		oy#=y+r*Sin(theta)
		ox1#=x+(r+1)*Cos(theta)
		oy1#=y+(r+1)*Sin(theta)
		ox2#=x+(r-1)*Cos(theta)
		oy2#=y+(r-1)*Sin(theta)
		Local poly#[8]
		While theta<b
			If red1<>-1
				pos#=(theta-a)/(b-a)
				If pos<.5*s1
					pos:/s1
					SetColor red1*(.5-pos)+red*(.5+pos),green1*(.5-pos)+green*(.5+pos),blue1*(.5-pos)+blue*(.5+pos)
				ElseIf pos>1-.5*s2
					pos=(1-pos)/s2
					SetColor red2*(.5-pos)+red*(.5+pos),green2*(.5-pos)+green*(.5+pos),blue2*(.5-pos)+blue*(.5+pos)
				Else
					SetColor red,green,blue
				EndIf
			EndIf
			dx#=x+r*Cos(theta)
			dy#=y+r*Sin(theta)
			dx1#=x+(r+1)*Cos(theta)
			dy1#=y+(r+1)*Sin(theta)
			dx2#=x+(r-1)*Cos(theta)
			dy2#=y+(r-1)*Sin(theta)
			poly=[zoomx(ox1),zoomy(oy1),zoomx(dx1),zoomy(dy1),zoomx(dx2),zoomy(dy2),zoomx(ox2),zoomy(oy2)]
			DrawPoly poly
			'DrawzoomLine ox,oy,dx,dy
			
			'poly=[x,y,ox,oy,dx,dy]
			'SetAlpha .1
			'DrawzoomPoly poly
			'SetAlpha 1
			
			ox1=dx1
			oy1=dy1
			ox2=dx2
			oy2=dy2
			ox=dx
			oy=dy
			theta:+s
		Wend
		dx=x+r*Cos(b)
		dy=y+r*Sin(b)
		dx1#=x+(r+1)*Cos(b)
		dy1#=y+(r+1)*Sin(b)
		dx2#=x+(r-1)*Cos(b)
		dy2#=y+(r-1)*Sin(b)
		'DrawzoomLine ox,oy,dx,dy
		poly=[zoomx(ox1),zoomy(oy1),zoomx(dx1),zoomy(dy1),zoomx(dx2),zoomy(dy2),zoomx(ox2),zoomy(oy2)]
		DrawPoly poly
		SetLineWidth 1
	End Method
	
	Method drawnonarc(na:nonarc)
		x1#=x+r*Cos(na.an1)
		y1#=y+r*Sin(na.an1)
		x2#=x+r*Cos(na.an2)
		y2#=y+r*Sin(na.an2)
		d#=2*r*Sin(andiff(na.an1,na.an2))
		pos#=0
		ox#=x1
		oy#=y1
		Local poly#[6]
		SetAlpha .1
		While pos<d
			t#=pos/d
			dx#=x1+t*(x2-x1)
			dy#=y1+t*(y2-y1)
			arcred=na.red1*(1-t)+na.red2*t
			arcgreen=na.green1*(1-t)+na.green2*t
			arcblue=na.blue1*(1-t)+na.blue2*t
			SetColor (arcred+red)*.5,(arcgreen+green)*.5,(arcblue+blue)*.5
			poly=[x,y,ox,oy,dx,dy]
			'drawzoompoly poly
			ox=dx
			oy=dy
			pos:+10
		Wend
		poly=[x,y,ox,oy,x2,y2]
		SetColor na.red2,na.green2,na.blue2
		'DrawzoomPoly poly
		SetAlpha 1
	End Method
	
End Type

Type nonarc
	Field an1#,an2#
	Field red1,green1,blue1,red2,green2,blue2
	Field s1#,s2#
	
	Function Create:nonarc(an1#,an2#,red1=255,green1=255,blue1=255,s1#=1,red2=255,green2=255,blue2=255,s2#=1)
		na:nonarc=New nonarc
		an1=an1 Mod 360
		If an1<0 Then an1:+360
		If an2<>360 Then an2=an2 Mod 360
		If an2<0 Then an2:+360
		na.an1=an1
		na.an2=an2
		na.red1=red1
		na.green1=green1
		na.blue1=blue1
		na.s1=s1
		na.red2=red2
		na.green2=green2
		na.blue2=blue2
		na.s2=s2
		Return na
	End Function
	
	Method contains(an#)
		d1#=andiff(an2,an1)
		If d1<0 Then d1:+360
		d2#=andiff(an,an1)
		If d2=0 Then Return 1
		If d2<0 Then d2:+360
		'debugo "     "+d1
		'debugo "     "+d2
		If d2<d1 Then Return 1 Else Return 0
	End Method
	
	Function comparestarts(o1:Object,o2:Object)
		na1:nonarc=nonarc(o1)
		na2:nonarc=nonarc(o2)
		If nicean(na1.an1)<nicean(na2.an1) Then Return -1 Else Return 1
	End Function
	
	Method repr$()
		Return "("+String(Int(an1))+","+String(Int(an2))+")"
	End Method
End Type

Type wall
	Field x1#,y1#,x2#,y2#
	Field vx#,vy#,nx#,ny#
	
	Method New()
		walls.addlast Self
	End Method
	
	Function Create:wall(x1#,y1#,x2#,y2#,vx#,vy#)
		w:wall=New wall
		w.x1=x1
		w.y1=y1
		w.x2=x2
		w.y2=y2
		w.vx=vx
		w.vy=vy
		an#=ATan2(y2-y1,x2-x1)
		w.nx=Cos(an+90)
		w.ny=Sin(an+90)
		Return w
	End Function
	
	Method draw()
		SetColor 255,255,255
		SetLineWidth 3
		Drawzoomline x1,y1,x2,y2
		drawzoomline (x1+x2)/2,(y1+y2)/2,(x1+x2)/2+vx,(y1+y2)/2+vy
		SetLineWidth 1
	End Method
End Type

Function nicean#(an#)
	an=an Mod 360
	If an<=-180 Then an:+360
	Return an
End Function

Function andiff#(an1#,an2#)
	'debugo String(an1)+" , "+String(an2)
	If an2=0 And an1=360 Then Return 360
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

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
	Return Nan
End Function

Function pointlinedistance#(px#,py#,ax#,ay#,bx#,by#,nx#=1/0.0,ny#=1/0.0)
	dx#=bx-ax
	dy#=by-ay
	If nx=Nan
		an#=ATan2(dy,dx)
		nx#=Cos(an+90)
		ny#=Sin(an+90)
	EndIf
	If dx<>0
		lambda#=(py-ay+(ax-px)*dy/dx)/(nx*dy/dx-ny)
	Else
		lambda#=px-ax
	EndIf
	Return lambda#
End Function

Function intersectcircles(circles:TList)
	For c:circle=EachIn circles
		c.nonarcs=New TList
		c.allnonarcs=New TList
	Next
	
	
	l:TList=circles.copy()
	For c:circle=EachIn l
		If Not c.onscreen() Then l.remove c
	Next
	
	While l.count()>1
		'debugo "?????"
		c1:circle=circle(l.removefirst())
		For c2:circle=EachIn l
			c1.intersect(c2)
		Next
	Wend
End Function

Type thing
	Field x#,y#
	Field vx#,vy#
	Field ox#,oy#
	Field insides:TList,inside:circle
	Field radius#
	
	Method New()
		things.addlast Self
		insides=New TList
		radius=1
	End Method

	Method update()
		If x=1/0.0 Or y=1/0.0
			Print "thing nan!"
		EndIf
		'vx=x-ox
		'vy=y-oy
		ox=x
		oy=y
		x:+vx
		y:+vy
		vx:*.99
		vy:*.99
		
		Rem
		For w:wall=EachIn walls
			cd#=Sgn(pointlinedistance(ox,oy,w.x1,w.y1,w.x2,w.y2,w.nx,w.ny))
			offx#=radius*cd*w.nx
			offy#=radius*cd*w.ny
			
			x1#=ox+offx+w.vx
			y1#=oy+offy+w.vy
			x2#=x+offx+w.vx
			y2#=y+offy+w.vy
			
			DrawzoomLine x1,y1,x2,y2
			DrawzoomLine x1,y1,x,y
			
			lambda#=linesintersect(x1,y1,x2,y2,w.x1,w.y1,w.x2,w.y2)
			'DrawText lambda,0,0
			If lambda<>Nan
				DrawText "hit!",400,12
				
				x#=ox+lambda*vx
				y#=oy+lambda*vy
				'DrawRect x+offx-2,y+offy-2,4,4
				DrawText w.vx*w.vx+w.vy*w.vy,400,45
				dvx#=vx-w.vx
				dvy#=vy-w.vy
				dp#=dvx*w.nx+dvy*w.ny
				'Print dp
				'SetColor 255,0,0
				vx=vx-2*dp*w.nx
				vy=vy-2*dp*w.ny
				DrawzoomLine x+offx,y+offy,x+offx+vx*30*(1-lambda),y+offy+vy*30*(1-lambda)
				x:+(1-lambda)*vx
				y:+(1-lambda)*vy
				ox=x-vx
				oy=y-vy
			EndIf
		Next
		EndRem

		insides:TList=New TList
		inside=Null
		closest:circle=Null
		mindist#=-1
		isinside=0
		For c:circle=EachIn circles
			dx#=c.x-x
			dy#=c.y-y
			d#=dx*dx+dy*dy
			If d<c.r*c.r
				an#=ATan2(dy,dx)+180
				innonarc=0
				For na:nonarc=EachIn c.nonarcs
					If na.contains(an) Then innonarc=1
				Next
				If innonarc
					maxr#=c.r
				Else
					maxr#=c.r-radius
				EndIf
			Else
				maxr#=c.r
			EndIf
			If d<=maxr*maxr
				If Not inside
					inside=c
				Else
					If Not insides.count() Then insides.addfirst inside
					insides.addfirst c
				EndIf
				isinside=1
				c.occupied=1
				
			ElseIf Not isinside
				compd=mindist+c.r
				If mindist=-1 Or d<compd*compd
					closest=c
					mindist=Sqr(d)-c.r
				EndIf
			EndIf
		Next
		If Not isinside
			closest.occupied=1
			dx#=closest.x-x
			dy#=closest.y-y
			d=Sqr(dx*dx+dy*dy)
			nx#=x-closest.x
			ny#=y-closest.y
			d#=Sqr(nx*nx+ny*ny)
			nx:/d
			ny:/d
			dvx#=vx-closest.vx
			dvy#=vy-closest.vy
			ndp#=(nx*dvx+ny*dvy)
			x#=closest.x-dx*(closest.r-radius)/d
			y#=closest.y-dy*(closest.r-radius)/d
			vx=(vx-2*nx*ndp)
			vy=(vy-2*ny*ndp)
			inside=closest
		EndIf
	End Method
	
	Method pushcircle(c:circle,f#)
		'Return
		dx#=x-c.x
		dy#=y-c.y
		d#=Sqr(dx*dx+dy*dy)
		If d=0 Return
		If c.r=0 Return
		f#=(d/(c.r*c.r))*f
		dx:/d
		dy:/d
		c.vx:+f*dx
		c.vy:+f*dy
		c.testmadness "push circle"
	End Method

	Method draw()
	End Method
	
	Method islocal(t:thing)
		If (Not inside) Or (Not t.inside) Then Return
		If inside.connectedto(t.inside) Then Return 1 Else Return 0
	End Method
End Type

Type player Extends thing
	Field score#
	Field joy
	
	Method New()
		joy=assignjoy()
		radius=15
	End Method
	
	Method control()
		pushx#=KeyDown(KEY_RIGHT)-KeyDown(KEY_LEFT)
		pushy#=KeyDown(KEY_DOWN)-KeyDown(KEY_UP)
		If joy>=0
			pushx:+safejoyx(joy)
			pushy:+safejoyy(joy)
		EndIf
		vx:+(pushx)*.05
		vy:+(pushy)*.05
	End Method
	
		
	Method update()
		Super.update()
			
		f#=.1
		If insides.count()
			For c:circle=EachIn insides
				pushcircle(c,f)
			Next
		ElseIf inside
			pushcircle(inside,f)
		EndIf
		
		
		totalf#=0
		totalscore#=0
		
		For j:jewel=EachIn jewels
			If islocal(j)
				totalscore:+j.strength*j.strength
				dx#=x-j.x
				dy#=y-j.y
				d#=Sqr(dx*dx+dy*dy)
				f#=.5/(d*j.size)'*j.size)
				If f>1 Then f=1
				totalf:+f
				dx:/d
				dy:/d
				slowing#=1-f*.9
				j.vx:*slowing
				j.vy:*slowing
				f:*1.5
				If d>5
					j.vx:+dx*f
					j.vy:+dy*f
				EndIf
				
			EndIf
		Next
		scorediff=totalscore-score
		score:+Sqr(Abs(scorediff))*Sgn(scorediff)
		
		'DrawText totalscore,0,60
	End Method
	
	Method draw()
		SetColor 255,255,255

		border#=5
		Local poly#[4]
		ox1#=zoomx(x+radius)
		oy1#=zoomy(y)
		ox2#=zoomx(x+radius-border)
		oy2#=zoomy(y)
		an#=0
		While an<=360
			an:+15
			x1#=zoomx(x+Cos(an)*radius)
			y1#=zoomy(y+Sin(an)*radius)
			x2#=zoomx(x+Cos(an)*(radius-border))
			y2#=zoomy(y+Sin(an)*(radius-border))
			poly=[ox1,oy1,x1,y1,x2,y2,ox2,oy2]
			DrawPoly poly
			ox1=x1
			oy1=y1
			ox2=x2
			oy2=y2
		Wend
			
		'drawzoomcircle x,y,radius
		SetColor 0,0,0
		SetAlpha .7
		drawzoomcircle x,y,radius-border
		SetAlpha 1
		'DrawText Int(score),0,75
		SetColor 255,255,255
	End Method
End Type

Type jewel Extends thing	
	Field strength#,size#
	Field red#,green#,blue#

	Method New()
		'Super.New()
		jewels.addlast Self
		strength#=1
		size=1
	End Method
	
	Method die()
		things.remove Self
		jewels.remove Self
	End Method
	
	Method update()
		Super.update()
		
		vx:+Rnd(-1,1)*.04/size
		vy:+Rnd(-1,1)*.04/size
		
		If vx=1/0.0
			Print "jewel nan??"
			End
		EndIf
		
		size#=Log(strength)+1
		If size<1
			Print "size = "+String(size)
			Print strength
			size=1
		EndIf
		radius=size*3+2
		
		f#=.08*size
		If insides.count()
			For c:circle=EachIn insides
				pushcircle(c,f)
			Next
		ElseIf inside
			pushcircle(inside,f)
		EndIf
	End Method

	Method draw()
		SetColor 255,255,255
		drawzoomcircle x,y,radius
		SetColor red,green,blue
		drawzoomcircle x,y,radius-2
	End Method
End Type

Function cleanjewels()
	l:TList=jewels.copy()
	For j:jewel=EachIn jewels
		If Not j.islocal(p) l.remove j
	Next
	
	While l.count()>1
		j:jewel=jewel(l.removefirst())
		For j2:jewel=EachIn l
			If j.islocal(j2)
				dx#=j2.x-j.x
				dy#=j2.y-j.y
				d#=dx*dx+dy*dy
				limit#=(j.size+j2.size)*1.2
				If d<limit*limit
					j2.die
					l.remove j2
					frac#=j.strength/(j.strength+j2.strength)
					j.x:+dx*(1-frac)
					j.y:+dy*(1-frac)
					j.strength:+j2.strength
					j.red=j.red*frac+j2.red*(1-frac)
					j.green=j.green*frac+j2.green*(1-frac)
					j.blue=j.blue*frac+j2.blue*(1-frac)
					rate#=Sqr(1/(j.size+j2.size))

					sounditem.Create(choosesound(clinks),rate)
				EndIf
			EndIf
		Next
	Wend
End Function

Type bgline
	Field x#,y#,an#
	Field red,green,blue
	Field nonlines:TList
	Field minlambda#,maxlambda#

	Method New()
		nonlines=New TList
	End Method
	
	Function Create:bgline(x#,y#,an#,red,green,blue,tohit:TList)
		bgl:bgline=New bgline
		bgl.x=x
		bgl.y=y
		bgl.an=an
		bgl.red=red
		bgl.green=green
		bgl.blue=blue
		
		bgl.makenonlines(tohit)
		
		Return bgl
	End Function
	
	Method makenonlines(tohit:TList)
		dx#=Cos(an)
		dy#=Sin(an)
		nx#=Cos(an+90)
		ny#=Sin(an+90)
		nonlines=New TList

		If dx=0
			minxlambda#=unzoomx(0)-x
			maxxlambda#=unzoomx(gwidth)-x
		Else
			minxlambda#=(unzoomx(0)-x)/dx
			maxxlambda#=(unzoomx(gwidth)-x)/dx
		EndIf
		
		If dy=0
			minylambda#=unzoomy(0)-y
			maxylambda#=unzoomy(gheight)-y
		Else
			minylambda#=(unzoomy(0)-y)/dy
			maxylambda#=(unzoomy(gheight)-y)/dy
		EndIf
		
		If minxlambda<minylambda Then minlambda=minxlambda Else minlambda=minylambda
		If maxxlambda>maxylambda Then maxlambda=maxxlambda Else maxlambda=maxylambda

		For c:circle=EachIn tohit
			If Not c.hidden
				If dx=0
					mu#=(x-c.x)/nx
				Else
					mu#=(y-c.y+(c.x-x)*dy/dx)/(ny-nx*dy/dx)
				EndIf
				If mu<c.r
					d#=Sqr(c.r*c.r-mu*mu)
					lambda#=(c.x-x+mu*nx)/dx
					
					lowlambda#=lambda-d
					highlambda#=lambda+d

				
						'debugo "lowlambda is "+String(lowlambda)
						'debugo "highlambda is "+String(highlambda)
						
						
						debugo "____"
						For na:nonarc=EachIn c.allnonarcs
							x1#=c.x+c.r*Cos(na.an1)
							y1#=c.y+c.r*Sin(na.an1)
							x2#=c.x+c.r*Cos(na.an2)
							y2#=c.y+c.r*Sin(na.an2)
							'drawzoomline x1,y1,x2,y2
							ldx#=x2-x1
							ldy#=y2-y1
							dp#=ldx*nx+ldy*ny
							'debugo "ld: ("+String(ldx)+","+String(ldy)+")"
							'debugo "n: ("+String(nx)+","+String(ny)+")"
							'debugo "dp: "+String(dp)
							nllambda#=linesintersect(x,y,x+dx,y+dy,x1,y1,x2,y2,1)
							If nllambda<>Nan And nllambda>lowlambda And nllambda<highlambda
								If dp<0 
									debugo "lowlambda increased from "+String(lowlambda)+" to "+String(nllambda)
									lowlambda=nllambda
								ElseIf dp>0 
									debugo "highlambda decreased from "+String(highlambda)+" to "+String(nllambda)
									highlambda=nllambda
								EndIf
							EndIf
							If debugging
								Delay 2000
							EndIf
						Next
						
					sx#=x+lowlambda*dx
					sy#=y+lowlambda*dy
					ex#=x+highlambda*dx
					ey#=y+highlambda*dy
					
					adding=1
					For na:nonarc=EachIn c.allnonarcs
						diffo#=andiff(na.an2,na.an1)
						midan#=na.an1+diffo/2
						If diffo<0 Then midan:+180
						mnx#=Cos(midan)
						mny#=Sin(midan)
						x1#=c.x+c.r*Cos(na.an1)
						y1#=c.y+c.r*Sin(na.an1)
						
						'drawzoomline c.x,c.y,x1,y1
						'drawzoomline x1,y1,x1+20*mnx,y1+20*mny
						
						dp1#=mnx*(sx-x1)+mny*(sy-y1)
						dp2#=mnx*(ex-x1)+mny*(ey-y1)
						
						'debugo "dp1 is "+String(dp1)
						'debugo "dp2 is "+String(dp2)
						
						'If dp1>0 Then debugo("starton")
						'If dp2>0 Then debugo("endon")
						
						If dp1>0 And dp2>0 Then adding=0
					
					Next
					If adding
						If highlambda>minlambda And lowlambda<maxlambda
								addnonline(lowlambda,highlambda)
						EndIf
					EndIf
				EndIf
			EndIf
		Next
	End Method
	
	Method addnonline(lambda1#,lambda2#)
		newnl:nonline=nonline.Create(lambda1,lambda2)
		For nl:nonline=EachIn nonlines
			debugo "comparing "+newnl.repr()+" with "+nl.repr()
			If lambda1>nl.lambda1
				If lambda1<nl.lambda2
					If lambda2<nl.lambda2
						debugo "wholly contained in old line"
						Return
					Else
						debugo "nl.lambda1 -> lambda2"
						nonlines.remove nl
						addnonline nl.lambda1,lambda2
						Return
					EndIf
				Else
					debugo "line doesn't intersect"
				EndIf
			Else
				If lambda2>nl.lambda1
					If lambda2<nl.lambda2
						debugo "lambda1 -> nl.lambda2"
						nonlines.remove nl
						addnonline lambda1,nl.lambda2
						Return
					Else
						debugo "wholly contained in new line"
						nonlines.remove nl
					EndIf
				Else
					debugo "line doesn't intersect"
				EndIf
			EndIf
		Next
		newnl.lambda1:-.5
		newnl.lambda2:+.5
		nonlines.addlast newnl
	End Method
					
	
	Method draw(drawin,drawout)
		SetColor red,green,blue
		SetLineWidth 3
		
		dx#=Cos(an)
		dy#=Sin(an)
		
		
		sx#=x+minlambda*dx
		sy#=y+minlambda*dy
		ex#=x+maxlambda*dx
		ey#=y+maxlambda*dy
		
		'drawzoomtext nonlines.count(),x,y
		
		If drawin
			For nl:nonline=EachIn nonlines
				x1#=x+nl.lambda1*dx
				y1#=y+nl.lambda1*dy
				x2#=x+nl.lambda2*dx
				y2#=y+nl.lambda2*dy
				'debugo "?"
				drawzoomline x1,y1,x2,y2
			Next
		EndIf

		If drawout
			SortList nonlines,True,nonline.comparestarts
			SetColor 0,0,0
			Select nonlines.count()
			Case 0
				drawzoomline sx,sy,ex,ey
			Case 1
				onl:nonline=nonline(nonlines.first())
	
				x1#=x+onl.lambda1*dx
				y1#=y+onl.lambda1*dy
				x2#=x+onl.lambda2*dx
				y2#=y+onl.lambda2*dy
				
				drawzoomline sx,sy,x1,y1
				drawzoomline x2,y2,ex,ey
			Default
				'debugo "---"
				'debugo minlambda
				onl:nonline=Null
				For nl:nonline=EachIn nonlines
					'debugo nl.lambda1
	
					If Not onl
						x1#=x+nl.lambda1*dx
						y1#=y+nl.lambda1*dy
						drawzoomline sx,sy,x1,y1
					Else
						x1#=x+onl.lambda2*dx
						y1#=y+onl.lambda2*dy
						x2#=x+nl.lambda1*dx
						y2#=y+nl.lambda1*dy
						drawzoomline x1,y1,x2,y2
					EndIf
					
					onl=nl
				Next
				
				x2#=x+nl.lambda2*dx
				y2#=y+nl.lambda2*dy
				drawzoomline x2,y2,ex,ey
				'debugo maxlambda
			End Select
		EndIf
		
		SetLineWidth 1
		SetColor 255,255,255
	End Method
End Type

Type nonline
	Field lambda1#,lambda2#
	
	Function Create:nonline(lambda1#,lambda2#)
		nl:nonline=New nonline
		nl.lambda1=lambda1
		nl.lambda2=lambda2
		Return nl
	End Function
	
	Function comparestarts(o1:Object,o2:Object)
		nl1:nonline=nonline(o1)
		nl2:nonline=nonline(o2)
		If nl1.lambda1<nl2.lambda1 Then Return -1 Else Return 1
	End Function
	
	Method repr$()
		Return "("+String(lambda1)+","+String(lambda2)+")"
	End Method
End Type

Function makebg()
	reds:TList=New TList
	greens:TList=New TList
	blues:TList=New TList

	For c:circle=EachIn circles
		If c.red Then reds.addlast c
		If c.green Then greens.addlast c
		If c.blue Then blues.addlast c
	Next
	
	
	'bgl:bgline=bgline.Create(-400,400,-45,0,255,0,greens)
	'bgl.draw(1,1)
	'Return
	
	
	bgspace=20

	ox#=panx-(panx Mod bgspace)
	oy#=pany-(pany Mod bgspace)

	y#=-bgspace
	'n=(oy+ox)/bgspace Mod 7
	'If n<0 Then n:+7
	While y<gheight+bgspace*2
		'n=(n Mod 7)+1
		'debugo n
		lx#=ox-gwidth/2
		ly#=oy+y-gheight/2
		'bgl:bgline=bgline.Create(lx,ly,-45,255*(n&4),255*(n&2),255*(n&1),circles)
		'bgl.draw(0,1)
		redbgl:bgline=bgline.Create(lx,ly,-45,255,0,0,reds)
		redbgl.draw(1,0)
		greenbgl:bgline=bgline.Create(lx,ly,45,0,255,0,greens)
		greenbgl.draw(1,0)
		bluebgl:bgline=bgline.Create(lx,ly,0,0,0,255,blues)
		bluebgl.draw(1,0)
		y:+bgspace
	Wend
	
	x#=-bgspace
	'n=((oy+ox)/bgspace - 1) Mod 7
	'If n<0 Then n:+7
	While x<gwidth+bgspace*2
		'n=(n Mod 7)+1
		'debugo n
		lx#=ox+x-gwidth/2
		ly#=oy+gheight/2
		'bgl:bgline=bgline.Create(lx,ly,-45,255*(n&4),255*(n&2),255*(n&1),circles)
		'bgl.draw(0,1)
		redbgl:bgline=bgline.Create(lx,ly,-45,255,0,0,reds)
		redbgl.draw(1,0)
		greenbgl:bgline=bgline.Create(lx,ly-gheight,45,0,255,0,greens)
		greenbgl.draw(1,0)
		x:+bgspace
	Wend
	
End Function

Function assignjoy()
	If joyN>=JoyCount() Return -1
	joyN:+1
	Return joyN
End Function

Function SafeJoyX#(joy)
	jx#=JoyX(joy)
	If Abs(jx)<.2 Return 0
	Return jx
End Function

Function SafeJoyY#(joy)
	jy#=JoyY(joy)
	If Abs(jy)<.2 Return 0
	Return jy
End Function


Function debugo(txt$)
	If debugging
		Print txt
	EndIf
End Function

Type stopwatch
	Field ms1,ms2,n
	
	Method New()
		stopwatches.addlast Self
		ms1=MilliSecs()
		n=numwatches
		numwatches:+1
	End Method
End Type

Function startwatch()
	New stopwatch
End Function

Function endwatch(txt$)
	sw:stopwatch=stopwatch(stopwatches.removelast())
	Return
	sw.ms2=MilliSecs()
	diff=sw.ms2-sw.ms1
	If diff>1000/60
		SetColor 255,0,0
	Else
		SetColor 255,255,255
	EndIf
	DrawRect 0,sw.n*22,diff*10,20
	DrawText txt,250,sw.n*22
End Function


Incbin "clink1.ogg"
Incbin "clink2.ogg"
Incbin "clink3.ogg"
Incbin "clink4.ogg"
Incbin "clink5.ogg"




SeedRnd MilliSecs()


'graphics init
Function initgfx()
	AppTitle="Bubblyworlds"
	'?Win32 SetGraphicsDriver D3D7Max2DDriver()
	Graphics gwidth , gheight
	SetBlend ALPHABLEND
	'SetClsColor 100,100,100
End Function

Function initgame()
	joyN=0
	debugging=1
	drawnonarcs=0
	level=1
	
	sounditems=New TList

	clinks=loadsounds("incbin::clink",5,".ogg")
	'pops=loadsounds("pop",3,".ogg")
	
	circles:TList=New TList
	things:TList=New TList
	jewels:TList=New TList
	walls=New TList
	
	SeedRnd MilliSecs()
	
	For n=1 To 20
		New circle
	Next

	p:player=New player
	c:circle=New circle
	c.x=0.1
	c.y=0
	c.vx=0
	c.vy=0
	
	
	Rem
	c.colourcode=4
	c.red=255
	c.green=0
	c.blue=0
	c.r=80
	an=Rand(360)
	an=190
	For n=1 To 1
		c:circle=New circle
		c.x=Cos(an)*300
		c.y=Sin(an)*300
		c.vx=-Cos(an)*.25
		c.vy=-Sin(an)*.25
		c.r=150
		c.colourcode=2
		c.red=0
		c.green=255
		c.blue=0
		j:jewel=New jewel
		j.x=c.x+1
		j.y=c.y
		an:+70
	Next
	'For j:jewel=EachIn jewels
	'	j.die
	'Next
	EndRem
End Function

Function updategame()
	numwatches=0
	stopwatches=New TList
	
	dx#=zoomx(p.x)-gwidth/2
	dy#=zoomy(p.y)-gheight/2
	d=dx*dx+dy*dy-gwidth*gheight/9
	If d>0
		dist#=Sqr(dx*dx+dy*dy)
		dx:/dist
		dy:/dist
		panx:+d*dx*.00003
		pany:+d*dy*.00003
	EndIf
	
	'DrawLine gwidth*1/4,gheight*1/4,gwidth*3/4,gheight*1/4
	'DrawLine gwidth*1/4,gheight*1/4,gwidth*1/4,gheight*3/4
	'DrawLine gwidth*3/4,gheight*1/4,gwidth*3/4,gheight*3/4
	'DrawLine gwidth*1/4,gheight*3/4,gwidth*3/4,gheight*3/4


	startwatch
	For c:circle=EachIn circles
		c.update()
	Next
	endwatch "update circles ("+String(circles.count())+")"
	
	walls=New TList
	startwatch
	intersectcircles(circles)
	endwatch "intersect circles"
	
	startwatch
	cleanjewels()
	endwatch "clean jewels ("+String(jewels.count())+")"
	
	startwatch
	For t:thing=EachIn things
		t.update()
	Next
	endwatch "update things ("+String(things.count())+")"
	

End Function

Function drawgame()

	startwatch
	For si:sounditem=EachIn sounditems
		si.update()
	Next
	endwatch "update sounds ("+String(sounditems.count())+")"

	startwatch
	makebg
	endwatch "make bg"

	'DrawText "panx "+String(panx),0,0
	'DrawText "pany "+String(pany),0,12
	'DrawText things.count(),0,24
	'DrawText circles.count(),0,36

	startwatch
	For c:circle=EachIn circles
		c.draw()
	Next	
	endwatch "draw circles"
	
	startwatch
	For w:wall=EachIn walls
		w.draw
	Next
	endwatch "draw walls ("+String(walls.count())+")"
	
	For j:jewel=EachIn jewels
		j.draw()
	Next
	
	startwatch
	p.draw()
	endwatch "draw player"
	
	'For t:thing=EachIn things
	'	t.draw()
	'Next
	
	oldms=ms
	ms=MilliSecs()
	fps#=1000.0/(ms-oldms)
	DrawText Int(fps),gwidth-50,0
	
	Flip
	Cls
End Function

gwidth=800
gheight=800
initgfx()
initgame()

While Not KeyHit(KEY_ESCAPE) Or AppTerminate()

	''''' CONTROLS
	
	If MouseHit(2)
		debugging=1
		debugo "------------------"
	Else
		debugging=0
	EndIf
	
	If MouseHit(1)
		drawnonarcs=1-drawnonarcs
	EndIf
	
	p.control
	
	''''' UPDATES
	updategame()
	
	'''''' DRAWING
	drawgame()
	
Wend

