Function DrawTexturedPoly(image:TImage, xyuv:Float[] , frame = 0, vertex = -1) 
	Local handle_x#,  handle_y#
	GetHandle handle_x#,  handle_y#
	Local origin_x#,  origin_y#
	GetOrigin origin_x#,  origin_y#	
	
	Local D3DDriver:TD3D7Max2DDriver = TD3D7Max2DDriver(_max2dDriver)
	
	Assert Image, "Image not found"
	
	If D3DDriver Then 
		DrawTexturedPolyD3D ..
			D3DDriver,..
			 TD3D7ImageFrame(image.Frame(frame)), ..
			 xyuv, handle_x, handle_y, origin_x,origin_y, vertex*4
		Return
	End If
	Local  OGLDriver:TGLMax2DDriver = TGLMax2DDriver(_max2dDriver)
	If OGLDriver Then
			DrawTexturedPolyOGL ..
				OGLDriver,..
				 TGLImageFrame(image.Frame(frame)), ..
				 xyuv, handle_x, handle_y, origin_x,origin_y,  vertex*4
		Return
	End If
End Function


Function DrawTexturedPolyD3D( Driver:TD3D7Max2DDriver,  Frame:TD3D7ImageFrame,xyuv#[],handlex#,handley#,tx#,ty# , vertex)
	'If Driver.islost Return
	If xyuv.length<6 Return
	Local segs=xyuv.length/4
	Local len_ = Len(xyuv)
	
	If vertex > - 1 Then
		segs = vertex / 4
		len_ = vertex
	End If
	Local uv#[] = New Float[segs*6] ' 6
	

	Local c:Int Ptr=Int Ptr(Float Ptr(uv))
	
	Local ii:Int = 0
	For Local i=0 Until len_ Step 4
		Local x# =  xyuv[i+0]+handlex
		Local y# =  xyuv[i+1]+handley
		uv[ii+0] =  x*Driver.ix+y*Driver.iy+tx
		uv[ii+1] =  x*Driver.jx+y*Driver.jy+ty 
		uv[ii+2] =  0  ' *********** THIS IS THE Z-COORDINATE
		c[ii+3] = Driver.DrawColor
		uv[ii+4] =  xyuv[i+2]
		uv[ii+5] =  xyuv[i+3]
		ii:+6
	Next
	Driver.SetActiveFrame Frame
	Driver.device.DrawPrimitive(D3DPT_TRIANGLEFAN,D3DFVF_XYZ| D3DFVF_DIFFUSE | D3DFVF_TEX1,uv,segs,0)
End Function

Function DrawTexturedPolyOGL (Driver:TGLMax2DDriver, Frame:TGLImageFrame, xy#[],handle_x#,handle_y#,origin_x#,origin_y#, vertex) 
	Private
	Global TmpImage:TImage
	Public
	
	If xy.length<6 Return
	
	Local rot#  = GetRotation()
	Local tform_scale_x#, tform_scale_y#
	GetScale tform_scale_x, tform_scale_y
	
	Local s#=Sin(rot)
	Local c#=Cos(rot)
	Local ix= c*tform_scale_x
	Local iy=-s*tform_scale_y
	Local jx= s*tform_scale_x
	Local jy= c*tform_scale_y
	
	glBindTexture GL_TEXTURE_2D, Frame.name
	glEnable GL_TEXTURE_2D
	
	glBegin GL_POLYGON
	For Local i=0 Until Len xy Step 4
		If vertex > -1 And i >= vertex Then Exit
		Local x#=xy[i+0]+handle_x
		Local y#=xy[i+1]+handle_y
		Local u#=xy[i+2]
		Local v#=xy[i+3]
		glTexCoord2f u,v
		glVertex2f x*ix+y*iy+origin_x,x*jx+y*jy+origin_y
	Next
	glEnd
	If Not tmpImage Then tmpImage = CreateImage(1,1)
	DrawImage tmpImage, -100, - 100 ' Chtob zbit' flag texturi
End Function




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
	Return -1
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




Type gelnode
	Field ox#,oy#
	Field x#,y#,vx#,vy#
	Field newx#,newy#
	Field stuck
	Field u#,v#
	
	Method New()
		gelnodes.addlast Self
	End Method
	
	Function Create:gelnode(x#,y#,u#=0,v#=0)
		g:gelnode=New gelnode
		g.newx=x
		g.newy=y
		g.ox=x
		g.oy=y
		g.x=x
		g.y=y
		g.u=u
		g.v=v
		Return g
	End Function
	
	Method startframe()
		If stuck
			dx#=newx-x
			dy#=newy-y
			newx=x
			newy=y
			d#=Sqr(dx*dx+dy*dy)
			'If d>1
				stuck=0
			'EndIf
		EndIf
		
		If Not stuck
			vx=newx-ox
			vy=newy-oy+gravity
			vx:*.99
			vy:*.99
			x=newx
			y=newy
			ox=x
			oy=y
			newx=x+vx
			newy=y+vy
						
			If newy>0
				'newy=0
				'newx=x
				'stuck=1
			EndIf
		EndIf
	End Method
	
	Method bounceoff(x1#,y1#,x2#,y2#,lambda#)
		an#=ATan2(y2-y1,x2-x1)
		nx#=Cos(an+90)
		ny#=Sin(an+90)
		dx#=Cos(an)
		dy#=Sin(an)
		
		


		vx#=newx-ox
		vy#=newy-oy
		vel#=vx*vx+vy*Vy
		
		newx#=ox+lambda*vx
		newy#=oy+lambda*vy

		side=Sgn(nx*(ox-x1)+ny*(oy-y1))
		ndp#=(nx*vx+ny*vy)'*side
		fdp#=(dx*vx+dy*vy)*friction
		vx=(vx-2*nx*ndp-dx*fdp)*bounce
		vy=(vy-2*ny*ndp-dy*fdp)*bounce

		If vel<.1
			'newx:-1*nx*Sgn(ndp)
			'newy:-1*ny*Sgn(ndp)
		EndIf
		
		newx=newx+vx*(1-lambda)
		newy=newy+vy*(1-lambda)
		ox=newx
		oy=newy
		ox=newx-vx
		oy=newy-vy
	End Method
		
	Method update()
		minlambda#=-1
		
		
		For w:wall=EachIn walls
			lambda#=linesintersect(ox,oy,newx,newy,w.x1,w.y1,w.x2,w.y2)
			If lambda>=0 And (lambda<minlambda Or minlambda=-1)
				minlambda=lambda
				x1#=w.x1
				y1#=w.y1
				x2#=w.x2
				y2#=w.y2
			EndIf
		Next
		
		For gt:geltri=EachIn geltris
			If Self<>gt.g1 And Self<>gt.g2 And Self<>gt.g3
				If pointintriangle(newx,newy,gt.g1.x,gt.g1.y,gt.g2.x,gt.g2.y,gt.g3.x,gt.g3.y)
					lambda1#=linesintersect(ox,oy,newx,newy,gt.g1.x,gt.g1.y,gt.g2.x,gt.g2.y)
					lambda2#=linesintersect(ox,oy,newx,newy,gt.g2.x,gt.g2.y,gt.g3.x,gt.g3.y)
					lambda3#=linesintersect(ox,oy,newx,newy,gt.g3.x,gt.g3.y,gt.g1.x,gt.g1.y)
					If lambda1 >=0 'on line p1->p2
						If lambda1<minlambda Or minlambda=-1
							minlambda=lambda1
							n1=gt.g1
							n2=gt.g2
						EndIf
					ElseIf lambda2>=0 'on line p2->p3
						If lambda2<minlambda Or minlambda=-1
							minlambda=lambda2
							n1=gt.g2
							n2=gt.g3
						EndIf
					ElseIf lambda3>=0 'on line p3->p1
						If lambda3<minlambda Or minlambda=-1
							minlambda=lambda2
							n1=gt.g3
							n2=gt.g1
						EndIf
					Else
					EndIf
				EndIf
			EndIf
		Next
		
		If minlambda>=0
			bounceoff(x1,y1,x2,y2,minlambda)
		EndIf
	End Method
	
	Method draw()
		If stuck
			SetColor 255,0,0
		Else
			SetColor 255,255,255
		EndIf
			
		drawzoomrect x-1,y-1,3,3
		
	End Method		
End Type

Type geltri
	Field g1:gelnode,g2:gelnode,g3:gelnode
	Field d1#,d2#,d3#
	Field ocx#,ocy#
	Field lambda#
	Field img:timage
	Field links:TList
	
	Method New()
		links=New TList
		geltris.addlast Self
		If Not specialtri Then specialtri=Self
		lambda#=.1
	End Method
	
	Method calcarea#()
		Return Abs(g1.x*(g2.y-g3.y)+g2.x*(g3.y-g1.y)+g3.x*(g1.y-g2.y))/2
	End Method
	
	Function Create:geltri(g1:gelnode,g2:gelnode,g3:gelnode,img:timage)
		gt:geltri=New geltri
		gt.g1=g1
		gt.g2=g2
		gt.g3=g3
		gt.img=img
		
		gt.ocx#=(g1.x+g2.x+g3.x)/3
		gt.ocy#=(g1.y+g2.y+g3.y)/3
		dx1#=g2.x-g1.x
		dy1#=g2.y-g1.y
		dx2#=g3.x-g2.x
		dy2#=g3.y-g2.y
		dx3#=g1.x-g3.x
		dy3#=g1.y-g3.y
		gt.d1#=Sqr(dx1*dx1+dy1*dy1)
		gt.d2#=Sqr(dx2*dx2+dy2*dy2)
		gt.d3#=Sqr(dx3*dx3+dy3*dy3)
		
		
		Return gt
	End Function
	
	Method makelinks(tris:TList)
		For gt2:geltri=EachIn tris
			If gt2<>Self And incommon(gt2)
				linkto(gt2)
			EndIf
		Next
	End Method
	
	Method linkto(gt2:geltri)
		dx#=gt2.ocx-ocx
		dy#=gt2.ocy-ocy
		d#=Sqr(dx*dx+dy*dy)
		links.addlast geltrilink.Create(d,gt2)
	End Method
	
	Method incommon(gt2:geltri)
		num=0
		If g1=gt2.g1 Or g1=gt2.g2 Or g1=gt2.g3
			num:+1
		EndIf
		If g2=gt2.g1 Or g2=gt2.g2 Or g2=gt2.g3
			num:+1
		EndIf
		If g3=gt2.g1 Or g3=gt2.g2 Or g3=gt2.g3
			num:+1
		EndIf
		Return num
	End Method
	
	Method solve()
		newarea#=calcarea()
		
		scale#=Sqr(area/newarea)
		'Print scale
		
		cx#=(g1.x+g2.x+g3.x)/3
		cy#=(g1.y+g2.y+g3.y)/3
		
		vx#=cx-ocx
		vy#=cy-ocy
		movex#=0
		movey#=0
		
		For gtl:geltrilink=EachIn links
			dx#=gtl.gt2.ocx-cx
			dy#=gtl.gt2.ocy-cy
			d#=Sqr(dx*dx+dy*dy)
			dx:/d
			dy:/d
			diff#=(gtl.d-d)*lambda*.95/2
			g1.newx:-dx*diff
			g1.newy:-dy*diff
			g2.newx:-dx*diff
			g2.newy:-dy*diff
			g3.newx:-dx*diff
			g3.newy:-dy*diff
			gtl.gt2.g1.newx:+dx*diff
			gtl.gt2.g1.newy:+dy*diff
			gtl.gt2.g2.newx:+dx*diff
			gtl.gt2.g2.newy:+dy*diff
			gtl.gt2.g3.newx:+dx*diff
			gtl.gt2.g3.newy:+dy*diff
		Next
		
			
		
		If specialtri=Self
			'SetColor 255,255,0
			'drawzoomrect cx,cy,1,1
			'drawzoomtext scale,cx,cy
		EndIf
		'Return
		
		dx1#=g2.x-g1.x
		dy1#=g2.y-g1.y
		dx2#=g3.x-g2.x
		dy2#=g3.y-g2.y
		dx3#=g1.x-g3.x
		dy3#=g1.y-g3.y
		nd1#=Sqr(dx1*dx1+dy1*dy1)
		nd2#=Sqr(dx2*dx2+dy2*dy2)
		nd3#=Sqr(dx3*dx3+dy3*dy3)
		dx1:/nd1
		dy1:/nd1
		dx2:/nd2
		dy2:/nd2
		dx3:/nd3
		dy3:/nd3
		
		diff1#=d1-nd1
		diff2#=d2-nd2
		diff3#=d3-nd3
		fx1#=dx1*diff1/2
		fy1#=dy1*diff1/2
		fx2#=dx2*diff2/2
		fy2#=dy2*diff2/2
		fx3#=dx3*diff3/2
		fy3#=dy3*diff3/2

		g1.newx:+(fx3-fx1)*lambda
		g1.newy:+(fy3-fy1)*lambda
		g2.newx:+(fx1-fx2)*lambda
		g2.newy:+(fy1-fy2)*lambda
		g3.newx:+(fx2-fx3)*lambda
		g3.newy:+(fx2-fy3)*lambda

		nx1#=g1.x+fx1
		ny1#=g1.y+fy1
		nx2#=g2.x+fx2
		ny2#=g2.y+fy2
		nx3#=g3.x+fx3
		ny3#=g3.y+fy3
		If specialtri=Self
			'narea#=Abs(nx1*(ny2-ny3)+nx2*(ny3-ny1)+nx3*(ny1-ny2))/2
			'DrawText narea,400,0
	
		EndIf

		'drawzoomline g1.x,g1.y,nx1,ny1
		'drawzoomline g2.x,g2.y,nx2,ny2
		'drawzoomline g3.x,g3.y,nx3,ny3
		
		ocx=cx
		ocy=cy
	End Method
	
	Method draw()
		
		SetAlpha .2
		SetColor 255,255,255
		Local poly#[]
		poly=[zoomx(g1.x),zoomy(g1.y),g1.u,g1.v,zoomx(g2.x),zoomy(g2.y),g2.u,g2.v,zoomx(g3.x),zoomy(g3.y),g3.u,g3.v]
		drawtexturedpoly img,poly
		
		poly=[zoomx(g1.x),zoomy(g1.y),zoomx(g2.x),zoomy(g2.y),zoomx(g3.x),zoomy(g3.y)]
		DrawPoly poly


		SetAlpha 1
	End Method
	
End Type

Type geltrilink
	Field d#
	Field gt2:geltri
	
	Function Create:geltrilink(d#,gt2:geltri)
		gtl:geltrilink=New geltrilink
		gtl.d=d
		gtl.gt2=gt2
		Return gtl
	End Function
End Type


Type wall
	Field x1#,y1#,x2#,y2#
	
	Method New()
		walls.addlast Self
	End Method
	
	Function Create:wall(x1#,y1#,x2#,y2#)
		w:wall=New wall
		w.x1=x1
		w.y1=y1
		w.x2=x2
		w.y2=y2
		Return w
	End Function
	
	Method draw()
		SetColor 255,255,255
		SetLineWidth 3
		drawzoomline x1,y1,x2,y2
		SetLineWidth 1
	End Method
End Type

Function makeshape:gelnode[,](img:timage,width#,hsegs,x#,y#)
	Print ImageWidth(img)
	segwidth#=width/hsegs
	height#=width*ImageHeight(img)/ImageWidth(img)
	vsegs=Int(height/segwidth)
	If vsegs<2 Then vsegs=2
	segheight#=height/vsegs
	
	Local shape:gelnode[hsegs+1,vsegs+1]
	For sx=0 To hsegs
	For sy=0 To vsegs
		u#=Float(sx)/hsegs
		v#=Float(sy)/vsegs
		u:*.75
		v:*.85
		shape[sx,sy]=gelnode.Create(sx*segwidth+x-width/2,sy*segheight+y-height/2,u,v)
	Next
	Next
	
	tris:TList=New TList
	
	For sx=1 To hsegs
	For sy=1 To vsegs
		tris.addlast geltri.Create(shape[sx-1,sy-1],shape[sx,sy-1],shape[sx-1,sy],img)
		tris.addlast geltri.Create(shape[sx-1,sy],shape[sx,sy-1],shape[sx,sy],img)
	Next
	Next
	
	For gt:geltri=EachIn tris
		gt.makelinks tris
	Next
	
	Return shape
End Function	


Global gravity#=0.3
Global friction#=.5
Global bounce#=1

Global gelnodes:TList
Global walls:TList
Global geltris:TList

Global specialtri:geltri
Global closest:gelnode
Incbin "sausage.png"
Global sausage:timage=LoadImage("incbin::sausage.png")


gwidth=800
gheight=800

initgfx()
initgame()



While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	updateworld()
	
	drawworld()
Wend

Function initgame()
	gelnodes=New TList
	walls=New TList
	geltris=New TList
	
	pany=-400
	
	
	
	makeshape(sausage,200,9,-200,-450)
	makeshape(sausage,200,9,120,-450)
	
	For g:gelnode=EachIn gelnodes
		g.x:+Rnd(-.1,.1)
		g.y:+Rnd(-.1,.1)
	Next
	
	wall.Create(-300,-300,-100,0)
	wall.Create(-400,0,400,0)
	wall.Create(100,0,300,-300)
	wall.Create(-300,-300,300,-300)
	
End Function


Function updateworld()
	For g:gelnode=EachIn gelnodes
		g.startframe
	Next

	mx=unzoomx(MouseX())
	my=unzoomy(MouseY())
	If MouseDown(1)
		g=closest
		dx#=mx-g.x
		dy#=my-g.y
		d#=Sqr(dx*dx+dy*dy)
		g.newx:+dx*.2
		g.newy:+dy*.2
	Else
		mindist#=-1
		closest:gelnode=Null
		For g:gelnode=EachIn gelnodes
			dx#=g.x-mx
			dy#=g.y-my
			d#=dx*dx+dy*Dy
			If d<mindist Or mindist=-1
				closest=g
				mindist=d
			EndIf
		Next
	
	EndIf
	
	For i=1 To 10
		For gt:geltri=EachIn geltris
			gt.solve
		Next
		
		SetColor 0,0,255
		For g:gelnode=EachIn gelnodes
			'drawzoomline g.x,g.y,g.newx,g.newy
			g.x=g.newx
			g.y=g.newy
		Next
	Next
	
	For g:gelnode=EachIn gelnodes	
		g.update
	Next
	
End Function

Function drawworld()
	Flip
	Cls
	
	For w:wall=EachIn walls
		w.draw
	Next

	For gt:geltri=EachIn geltris
		gt.draw
	Next
	For g:gelnode=EachIn gelnodes
		g.draw
	Next
	
	Rem
	SetScale .2,.2
	DrawRect 0,0,ImageWidth(sausage),ImageHeight(sausage)
	DrawImage sausage,0,0
	SetScale 1,1
	
	Local poly#[12]
	w#=ImageWidth(sausage)
	h#=ImageHeight(sausage)
	DrawRect 0,0,400,100
	sx#=MouseX()/Float(gwidth)
	sy#=MouseY()/Float(gheight)
	poly=[0.0,0.0,0.0,0.0,w,0.0,sx,0.0,0.0,h,0.0,sy]
	drawtexturedpoly sausage,poly
	poly=[w,0.0,sx,0.0,w,h,sx,sy,0.0,h,0.0,sy]
	drawtexturedpoly sausage,poly
	DrawImage sausage,0,h
	DrawText sx,0,700
	DrawText sy,0,712
	EndRem
	
	DrawText gelnodes.count(),0,0
	
End Function

