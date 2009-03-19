

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

Function inarc(an,an1#,an2#)
	'If an1>180 Then an1:-360
	'If an2<180 Then an2:+360
	d1#=andiff(an2,an1)
	If d1=0 Then d1=360
	If d1>0
		d2#=andiff(an,an1)
		d3#=-andiff(an,an2)
		If (d2>=0 And d2<=d1) Or (d3>=0 And d3<=d1)
			Return 1
		Else
		EndIf
	Else
		Return 1-inarc(an,an2,an1)
	EndIf
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
	SetScale 1,1
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

Function DrawZoomText(txt$ , x# , y#,scale#=1)
	x = ZoomX(x)
	y = ZoomY(y)
	SetScale zoom*scale,zoom*scale
	DrawText txt , x , y
	SetScale 1,1
End Function

Function DrawZoomImage(image:TImage , x# , y#,width#=-1,heighto=0)
	If width=-1
		w#=1
	Else
		If heighto
			w# = width / ImageHeight(image)
		Else
			w# = width / ImageWidth(image)
		EndIf
	EndIf
	SetScale w*zoom , w*zoom
	DrawImage image , zoomx(x) , zoomy(y)
	SetScale 1,1
End Function

Function onscreen(x1#,y1#,x2#,y2#)
	x1#=zoomx(x1)
	y1#=zoomy(y1)
	x2#=zoomx(x2)
	y2#=zoomy(y2)
	Return (x1<gwidth And x2>0) And (y1<gheight And y2>0)
End Function

'graphics init
Function initgfx()
	'SetGraphicsDriver D3D7Max2DDriver()
	Graphics gwidth , gheight
	SetBlend ALPHABLEND
	'panx#=0
	'pany#=0
	'zoom#=1
	'tzoom#=zoom
End Function