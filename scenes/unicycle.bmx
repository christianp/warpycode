Global gwidth#,gheight#
Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom


'returns True if p1 and p2 are on the same side of the line a->b
Function sameside(p1x#,p1y#,p2x#,p2y#,ax#,ay#,bx#,by#)
	cp1# = (bx-ax)*(p1y-ay)-(p1x-ax)*(by-ay)
	cp2# = (bx-ax)*(p2y-ay)-(p2x-ax)*(by-ay)
	If cp1*cp2 >= 0 Then Return True
End Function	
	
'Clever little trick for telling if a point is inside a given triangle
'If for each pair of points AB in the triangle, P is on the same side of AB as 
'the other point in the triangle, then P is in the triangle. 
Function pointintriangle(px#,py#,ax#,ay#,bx#,by#,cx#,cy#)
	If sameside(px,py,ax,ay,bx,by,cx,cy) And sameside(px,py,bx,by,ax,ay,cx,cy) And sameside(px,py,cx,cy,ax,ay,bx,by)
		Return True
	Else
		Return False
	EndIf
End Function

Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

Function transformpoly#[](poly#[] , px# , py# , an# , scale# = 1) 
	'px, py: translate
	'an: rotate
	'scale: duhhh thicko
	
	Local opoly#[Len(poly)]
	
	can# = Cos(an)
	san#=Sin(an)
	For n=0 To Len(poly)-1 Step 2
		x#=poly[n]*scale
		y#=poly[n+1]*scale
		opoly[n]=x*Can-y*San+px
		opoly[n+1]=x*San+y*Can+py
	Next
	Return opoly
End Function

Function zoompoly#[](poly#[])
	Local opoly#[Len(poly)]
	For n=0 To Len(poly)-1 Step 2
		opoly[n]=zoomx(poly[n])
		opoly[n+1]=zoomy(poly[n+1])
	Next
	Return opoly
End Function

Function drawrotatedline#(x1# , y1# , x2# , y2# , px# , py# , an# , scale# = 1)
	'px,py: translate
	'an: rotate
	'scale: duhhh thicko
	
	can# = Cos(an)
	san# = Sin(an)
	
	nx1#=(x1*can-y1*san)*scale+px
	ny1#=(x1*san+y1*can)*scale+py
	nx2#=(x2*can-y2*san)*scale+px
	ny2#=(x2*san+y2*can)*scale+py
	DrawLine nx1,ny1,nx2,ny2
End Function

Function drawoutline(poly#[],thickness=1)
	'DrawPoly poly
	SetLineWidth thickness
	l = Len(poly)
	For n=0 To l-3 Step 2
		x1#=poly[n]
		y1#=poly[n+1]
		x2#=poly[(n+2) Mod l]
		y2#=poly[(n+3) Mod l]
		DrawZoomLine x1,y1,x2,y2
	Next
	SetLineWidth 1
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

Function DrawZoomCircle(x# , y# , radius#)
	x = zoomx(x) 
	y = zoomy(y)
	radius:* zoom
	DrawOval x - radius , y - radius , 2 * radius , 2 * radius
End Function

Function DrawZoomShell(x#,y#,radius#,segsize#=9)
	anstep#=360.0*segsize/(2*Pi*radius)
	an#=anstep
	ox#=x+radius
	oy#=y
	While an<360
		newx#=x+Cos(an)*radius
		newy#=y+Sin(an)*radius
		DrawzoomLine ox,oy,newx,newy
		ox=newx
		oy=newy
		an:+anstep
	Wend
	DrawzoomLine ox,oy,x+radius,y
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


'graphics init
Function initgfx()
	SetGraphicsDriver D3D7Max2DDriver()
	Graphics gwidth , gheight
	SetBlend ALPHABLEND
End Function




Type unicycle
	Field x#,y#,r#,vx#
	Field an#,pedalan#,vpedal#
	Field leglength#,seatheight#
	
	Method update()
	
		mx=unzoomx(MouseX())
		vpedal:*.96
		vpedal:+(mx-x)*.001*Abs(Cos(an+pedalan))
	
		pedalan:+vpedal
		vx#=2*Pi*r*vpedal/360
		x:+vx
	End Method

	Method draw()
		seatx#=x-Cos(an)*seatheight
		seaty#=y-Sin(an)*seatheight
		
		fx1#=x+Cos(an+pedalan)*r*.8
		fy1#=y+Sin(an+pedalan)*r*.8
		
		
		fx2#=x+Cos(an+pedalan+180)*r*.8
		fy2#=y+Sin(an+pedalan+180)*r*.8
		
		For spokean=0 To 360 Step 60
			totalan#=spokean+pedalan+an
			DrawzoomLine x,y,x+Cos(totalan)*r,y+Sin(totalan)*r
		Next
		drawzoomline x,y,seatx,seaty

		SetLineWidth 3
		drawzoomshell x,y,r
		drawleg(seatx,seaty,fx1,fy1)
		drawleg(seatx,seaty,fx2,fy2)
		drawzoomline fx1-3,fy1,fx1+3,fy1
		drawzoomline fx2-3,fy2,fx2+3,fy2
		SetLineWidth 1
		
		
		
		
	End Method
	
	Method drawleg(seatx#,seaty#,footx#,footy#)
		dx#=footx-seatx
		dy#=footy-seaty
		d#=Sqr(dx*dx+dy*dy)
		theta#=ACos(d/leglength)
		legan#=ATan2(dy,dx)
		kneex#=seatx#+Cos(legan-theta)*leglength/2
		kneey#=seaty#+Sin(legan-theta)*leglength/2
		
		DrawzoomLine seatx,seaty,kneex,kneey
		drawzoomline kneex,kneey,footx,footy
	End Method
End Type

Function updateworld()
	u.update
	dx=zoomx(u.x)-gwidth/2
	If Abs(dx)>gwidth*.4
	'	panx:+dx*.01
	EndIf
End Function

Function drawworld()
	u.draw
	
	DrawText panx,0,0
	DrawText u.vx,0,12
	
	Flip
	Cls
End Function


gwidth=800
gheight=800

initgfx()
Global u:unicycle=New unicycle
u.an=90
u.r=50
u.leglength=150
u.seatheight=90
u.vpedal#=.5
pany=-200

While Not KeyHit(KEY_ESCAPE) Or AppTerminate()
	updateworld()

	drawworld()
Wend