Global gwidth#,gheight#
Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom


Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

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
	poly=poly[..]
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



Type pip
	Field x#,y#
End Type


Type segment
	Field number
	Field angle#
	Field r#
	Field width#
	Field pips:TList
	
	Function Create:segment(number,angle#,r#,width#)
		se:segment=New segment
		se.number=number
		se.angle=angle
		se.r=r
		se.width=width
		
		se.pips=New TList
		For c=1 To 20
			p:pip=New pip
			se.pips.addlast p
			an#=angle+Rnd(0,1)*width
			pr#=Rnd(r,r+shellsize)
			p.x=Cos(an)*pr
			p.y=Sin(an)*pr
		Next
		Return se
	End Function
	
	Method draw()
		r1#=r
		r2#=r+shellsize
		ox1#=Cos(angle)*r1
		oy1#=Sin(angle)*r1
		ox2#=Cos(angle)*r2
		oy2#=Sin(angle)*r2
		drawzoomline ox1,oy1,ox2,oy2
		
		'return
		an#=angle
		Local poly#[]
		While an<angle+width-1
			an:+10
			If an>angle+width an=angle+width
			x1#=Cos(an)*r1
			y1#=Sin(an)*r1
			x2#=Cos(an)*r2
			y2#=Sin(an)*r2
			drawzoomline ox1,oy1,x1,y1
			drawzoomline ox2,oy2,x2,y2
			poly=[ox1,oy1,ox2,oy2,x2,y2,x1,y1]
			SetAlpha .6
			drawzoompoly poly
			SetAlpha 1
			ox1=x1
			oy1=y1
			ox2=x2
			oy2=y2
		Wend
		drawzoomline x1,y1,x2,y2
		
		SetColor 0,0,255
		For p:pip=EachIn pips
			drawzoomrect p.x,p.y,3,3
		Next
		SetColor 255,255,255
		
	End Method
End Type

Global shellsize#=50
Global curshell:shell
Type shell
	Field level
	Field segments:TList
	Field up:shell,down:shell
	
	Method New()
		segments=New TList
	End Method
	
	Function Create:shell(level)
		sh:shell=New shell
		sh.level=level
		Return sh
	End Function
	
	Method getsegment:segment(n,angle#)
		If angle<0 angle:+360
		If n<level
			Return up.getsegment(n,angle)
		ElseIf n>level
			If Not down
				down=shell.Create(level+1)
				down.up=Self
			EndIf
			Return down.getsegment(n,angle)
		Else
			segwidth#=360.0/(2*level+1)
			segnum=angle/segwidth
			getan#=segnum*segwidth
			For se:segment=EachIn segments
				If se.number=segnum Return se
			Next
			se:segment=segment.Create(segnum,getan,shellsize*level,segwidth)
			segments.addlast se
			Return se
		EndIf
	End Method
	
	Method draw()
		r#=level*shellsize
		ox#=r
		oy#=0
		an#=0
		SetColor 255,255,255
		SetAlpha 1
		While an<360
			an:+30/(level+1)
			If an>360 an=360
			x#=Cos(an)*r
			y#=Sin(an)*r
			DrawzoomLine ox,oy,x,y
			ox=x
			oy=y
		Wend
		SetAlpha .5
		For se:segment=EachIn segments
		'	se.draw
		Next
		SetAlpha 1
	End Method
	
	Method getvisible:TList(minlevel,maxlevel,minan#,maxan#,dir=0,l:TList=Null)
		hops:+1
		curshell=Self
		If Not l l=New TList
		If dir>=0 And level>=minlevel
			If up up.getvisible(minlevel,maxlevel,minan,maxan,1,l)
		EndIf
		If dir<=0 And level<=maxlevel
			If Not down
				down=shell.Create(level+1)
				down.up=Self
			EndIf
			down.getvisible(minlevel,maxlevel,minan,maxan,-1,l)
		EndIf
		If level<minlevel Or level>maxlevel Return l
		draw
		segwidth#=360.0/(2*level+1)
		an#=minan
		l.addlast getsegment(level,minan)
		While an<maxan
			an:+segwidth
			If an>maxan an=maxan
			se:segment=getsegment(level,an)
			If Not l.contains(se) l.addlast se
		Wend
		Return l
	End Method	
End Type

Global hops

Function dorect(x1#,y1#,x2#,y2#)
	midx#=(x1+x2)/2
	midy#=(y1+y2)/2
	midan#=ATan2(midy,midx)
	Local points#[]=[x1,y1,x2,y1,x2,y2,x1,y2]
	minr=-1
	maxr=-1
	minan#=0
	maxan#=0
	For n=0 To 6 Step 2
		dx#=points[n]
		dy#=points[n+1]
		r#=Sqr(dx*dx+dy*dy)
		If r<minr Or minr=-1 minr=r
		If r>maxr Or maxr=-1 maxr=r
		an#=ATan2(dy,dx)
		DrawText an,100,n*15
		diff#=andiff(an,midan)
		If diff<minan minan=diff
		If diff>maxan maxan=diff
	Next
	DrawText minan,0,30
	DrawText maxan,0,45
	
	minlevel=minr/shellsize
	maxlevel=maxr/shellsize
	DrawText minlevel,0,0
	DrawText maxlevel,0,15
	hops=0
	segs:TList=curshell.getvisible(minlevel,maxlevel,minan+midan,maxan+midan)
	DrawText hops,600,0
	For se:segment=EachIn segs
		se.draw
	Next
	
	
	SetColor 255,0,0
	drawzoomline 0,0,Cos(midan+minan)*maxr,Sin(midan+minan)*maxr
	drawzoomline 0,0,Cos(midan+maxan)*maxr,Sin(midan+maxan)*maxr
	SetColor 255,255,255
End Function


gwidth=800
gheight=800
Graphics 800,800,0
SetBlend ALPHABLEND
MoveMouse 400,400

curshell=shell.Create(0)
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	
	mx=unzoomx(MouseX())
	my=unzoomy(MouseY())
	an#=ATan2(my,mx)
	r#=Sqr(mx*mx+my*my)
	level=Int(r/shellsize)
	
	'se:segment=curshell.getsegment(level,an)
	'curshell.draw
	'se.draw
	
	dorect mx-shellsize/2,my-shellsize/2,mx+shellsize/2,my+shellsize/2
	
	SetColor 255,0,0
	SetAlpha .5
	DrawzoomRect mx-shellsize/2,my-shellsize/2,shellsize,shellsize
	SetAlpha 1
	SetColor 255,255,255
	

	Flip
	Cls

Wend