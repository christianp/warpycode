Global gwidth:Float = 800, gheight:Float = 800
Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom

Const gsection#=137.50776

Global state=-1
Global dude:shape
Global ox#=0
Global oy#=0
Global mx#,my#
Global limblimit#=.6
Const drawgap=10

Global yellowpaper:TImage
Global bluepaper:TImage
Global brownpaper:TImage
Global greenpaper:TImage


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

Function DrawZoomTexturedPoly(image:TImage, poly:Float[] ) 
	poly = poly[..] 
	While i < Len(poly)
		poly[i] = ZoomX(poly[i] ) 
		poly[i + 1] = zoomy(poly[i + 1]) 
		i:+4
	Wend
	DrawTexturedPoly image, poly
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



Function DrawTexturedPoly(image:TImage, xyuv:Float[] , frame = 0, vertex = -1) 
	Local handle_x#,  handle_y#
	GetHandle handle_x#,  handle_y#
	Local origin_x#,  origin_y#
	GetOrigin origin_x#,  origin_y#	
	
	'Local D3DDriver:TD3D7Max2DDriver = TD3D7Max2DDriver(_max2dDriver)
	
	Assert Image, "Image not found"
	
	Rem
	If D3DDriver Then 
		DrawTexturedPolyD3D ..
			D3DDriver,..
			 TD3D7ImageFrame(image.Frame(frame)), ..
			 xyuv, handle_x, handle_y, origin_x,origin_y, vertex*4
		Return
	End If
	EndRem
	Local  OGLDriver:TGLMax2DDriver = TGLMax2DDriver(_max2dDriver)
	If OGLDriver Then
			DrawTexturedPolyOGL ..
				OGLDriver,..
				 TGLImageFrame(image.Frame(frame)), ..
				 xyuv, handle_x, handle_y, origin_x,origin_y,  vertex*4
		Return
	End If
End Function


Rem
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
EndRem

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

'Quickhull function - call this one with a set of points.
Function quickhull:TList(s:TList)
	If s.count()<=3 Return s
	l:inpoint=Null
	r:inpoint=Null
	For p:inpoint=EachIn s
		If l=Null
			l=p
		ElseIf p.x<l.x
			l=p
		EndIf
		If r=Null
			r=p
		ElseIf p.x>r.x
			r=p
		EndIf
	Next
	
	an#=ATan2(r.y-l.y,r.x-l.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=Cos(an+90)
	sy#=Sin(an+90)
	
	s1:TList=New TList
	s2:TList=New TList
	For p:inpoint=EachIn s
		If p<>l And p<>r
			mu#=(l.y-p.y+(ry/rx)*(p.x-l.x))/(sy-sx*ry/rx)
			If mu<0 
				s1.addlast p 
			ElseIf mu>0
				s2.addlast p
			EndIf
		EndIf
	Next
	
	out1:TList=findhull(s1,l,r)
	out2:TList=findhull(s2,r,l)
	out:TList = New TList
	out.addlast l
	If out1
		For o:Object=EachIn out1
			out.addlast o
		Next
	EndIf
	out.addlast r
	If out2
		For o:Object=EachIn out2
			out.addlast o
		Next
	EndIf
	
	Return out
End Function

'Findhull helper function - you never need to call this
Function findhull:TList(sk:TList,p:inpoint,q:inpoint)
	If Not sk.count() Return Null
	c:inpoint=Null
	out:TList=New TList
	maxdist#=-1
	an#=ATan2(q.y-p.y,q.x-p.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=-ry
	sy#=rx
	For tp:inpoint=EachIn sk
		If tp<>p And tp<>q
			mu#=(p.y-tp.y+(ry/rx)*(tp.x-p.x))/(sy-sx*ry/rx)
			If maxdist=-1 Or Abs(mu)>maxdist
				c=tp
				maxdist=Abs(mu)
			EndIf
		EndIf
	Next
	an#=ATan2(c.y-p.y,c.x-p.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=Cos(an+90)
	sy#=Sin(an+90)
	s1:TList=New TList
	s2:TList=New TList
	For tp:inpoint=EachIn sk
		If tp<>c
			If Not pointintriangle(tp.x,tp.y,p.x,p.y,q.x,q.y,c.x,c.y)
				mu#=(p.y-tp.y+(ry/rx)*(tp.x-p.x))/(sy-sx*ry/rx)
				If mu<0 s1.addlast tp ElseIf mu>0 s2.addlast tp
			EndIf
		EndIf
	Next
	out1:TList=findhull(s1,p,c)
	out2:TList=findhull(s2,c,q)
	If out1
		For o:Object=EachIn out1
			out.addlast o
		Next
	EndIf
	out.addlast c
	If out2
		For o:Object=EachIn out2
			out.addlast o
		Next
	EndIf
	Return out
End Function







Global shapes:TList=New TList
Type shape
	Field points:TList
	Field lines:TList
	Field inpoints:TList
	Field sets:TList,sets2:TList
	Field joints:TList
	
	Method New()
		points=New TList
		lines=New TList
		inpoints=New TList
		joints=New TList
		shapes.addlast Self
	End Method
	
	Method addpoint(x#,y#)
		p:point=point.Create(x,y)
		If points.count()
			p1:point=point(points.first())
			dx#=x-p1.x
			dy#=y-p1.y
			d#=dx*dx+dy*dy
			pl:point=point(points.last())
			If d<drawgap*drawgap And points.count()>5
				p:point=p1
			Else
				points.addlast p
			EndIf
			l:line=line.Create(pl,p)
			lines.addlast l
			
			If p=p1
				changestate(1)
			EndIf
		Else
			points.addlast p
		EndIf
	End Method
	
	Method analyse()
		tx#=0
		ty#=0
		minx#=-1
		miny#=-1
		maxx#=-1
		maxy#=-1
		For p:point=EachIn points
			tx:+p.x
			ty:+p.y
			If p.x<minx Or minx=-1 Then minx=p.x
			If p.x>maxx Or maxx=-1 Then maxx=p.x
			If p.y<miny Or miny=-1 Then miny=p.y
			If p.y>maxy Or maxy=-1 Then maxy=p.y
		Next
		
		If maxx-minx>maxy-miny
			targr#=maxx-minx
		Else
			targr#=maxy-miny
		EndIf
		targr:/Sqr(2) 'because we want half the diagonal of a square with sides length targr
		
		inpoints:TList=New TList
		
		n=points.count()
		ax#=tx/n
		ay#=ty/n
		
		r#=0
		n=0
		While r<targr
			r#=Sqr(n)*5
			an#=gsection*n
			x#=ax+Cos(an)*r
			y#=ay+Sin(an)*r
			If inside(x,y)
				inpoints.addlast(inpoint.Create(x,y))
			EndIf
			n:+1
		Wend
		
		For p:point=EachIn points
			ip:inpoint=inpoint(inpoint.Create(p.x,p.y))
			p.ip=ip
			inpoints.addlast(ip)
		Next
		
		tnclose=0
		For ip:inpoint=EachIn inpoints
			For ip2:inpoint=EachIn inpoints
				If ip<>ip2
					If ip2.close(ip.x,ip.y)
						ip.numclose:+1
						tnclose:+1
					EndIf
				EndIf
			Next
		Next
		
		makesets()
	End Method
	
	Method buildsets:TList(l:TList,closer#)
		osets:TList=New TList
		While l.count()
			ip:inpoint=inpoint(l.removefirst())
			jsets:TList=New TList
			For s:set=EachIn osets
				If s.close(ip.x,ip.y,closer)
					jsets.addlast s
				EndIf
			Next
			Select jsets.count()
			Case 0 'no close sets, make new one
				s:set=New set
				s.points.addlast ip
				osets.addlast s
			Case 1 'one close set, join this one
				s:set=set(jsets.first())
				s.points.addlast ip
			Default 'more than one! join sets
				s1:set=set(jsets.removefirst())
				For s:set=EachIn jsets
					s1.join(s)
					osets.remove s
				Next
				s1.points.addlast ip
			End Select
		Wend
		Return osets
	End Method	
	
	Method makesets()
		closer#=Sqr(limblimit*400)
		wantn#=limblimit*100

		l:TList=inpoints.copy()
		l2:TList=inpoints.copy()
		sets:TList=New TList
		For ip:inpoint=EachIn l
			If ip.numclose>wantn
				l.remove ip
			EndIf
		Next
		
		sets=buildsets(l,closer)
		
		'cull tiny sets
		For s:set=EachIn sets
			If s.points.count()<15 
				sets.remove(s)
			EndIf
		Next
		
		'take convex hull of each set, find number of inpoints inside hull, compare to
		'number of points originally in set
		For s:set=EachIn sets
			hull:TList=quickhull(s.points)
			sh:shape=New shape
			shapes.remove sh
			oip:inpoint=Null
			For ip:inpoint=EachIn hull
				sh.points.addlast ip
				If oip
					sh.lines.addlast line.Create(oip,ip)
				EndIf
				oip=ip
			Next
			sh.lines.addlast line.Create(ip,inpoint(hull.first()))
			For ip:inpoint=EachIn inpoints
				If sh.inside(ip.x,ip.y)
					s.hullpoints:+1
				EndIf
			Next
			
			If s.points.count()/Float(s.hullpoints) < .8
				sets.remove s
			EndIf
		Next
		
		For s:set=EachIn sets
			For ip:inpoint=EachIn s.points
				l2.remove ip
			Next
		Next
		sets2:TList=buildsets(l2,closer)
			
		joints=New TList
		For s:set=EachIn sets
			
			For s2:set=EachIn sets2
				hitpoints:TList=New TList
				For p:point=EachIn s.points
					hito=0
					For p2:point=EachIn s2.points
						dx#=p2.x-p.x
						dy#=p2.y-p.y
						d#=dx*dx+dy*dy
						If d<=closer*closer
							If Not hitpoints.contains(p2)
								hitpoints.addlast p2
							EndIf
							hito=1
						EndIf
					Next
					If hito
						hitpoints.addlast p
					EndIf
				Next
				
				tx#=0
				ty#=0
				For p:point=EachIn hitpoints
					tx:+p.x
					ty:+p.y
				Next
				tx:/hitpoints.count()
				ty:/hitpoints.count()
				joints.addlast joint.Create(tx,ty,s,s2)
			Next
				
			
		Next
		
		For s2:set=EachIn sets2
			sets.addlast s2
		Next
		
		'put lost points in sets
		For ip:inpoint=EachIn inpoints
			found=0
			mindist#=-1
			closeset:set=Null
			For s:set=EachIn sets
				mindist2#=-1
				For p:point=EachIn s.points
					dx#=p.x-ip.x
					dy#=p.y-ip.y
					d#=dx*dx+dy*dy
					If d<mindist2 Or mindist2=-1
						mindist2=d
					EndIf
				Next
				If mindist2<mindist
					mindist=mindist2
					closeset=s
				EndIf
				If s.points.contains(ip)
					found=1
				EndIf
			Next
			If Not found
				closeset.points.addlast ip
			EndIf
		Next
		
		For s:set=EachIn sets
			For p:point=EachIn points
				If s.points.contains(p.ip)
					s.border.addlast p
				EndIf
			Next

			s.tris:TList=triangulate(s.border)
		Next

		
		
	End Method
	
	
	Method finish()
		maxsize=-1
		bigset:set=Null
		For s:set=EachIn sets
			If s.points.count()>maxsize Or maxsize=-1
				maxsize=s.points.count()
				bigset=s
			EndIf
			
			s.enmiddle()
		Next
		
		
		
	End Method
	
	Method inside(x#,y#)
		'fiendishly clever? count the number of lines which intersect the line y=y#.
		'if it's odd, point is inside polygon
		n=0
		For l:line=EachIn lines
			ix#=l.xaty(y)
			If ix>=0 And ix<x
				n:+1
			EndIf
			If ix=x Return 1
		Next
		Return n Mod 2
	End Method
	
	Method draw()
		If state<1
			For l:line=EachIn lines
				DrawLine l.p1.x,l.p1.y,l.p2.x,l.p2.y
			Next
			For p:point=EachIn points
				p.draw()
			Next
			
			For ip:inpoint=EachIn inpoints
			'	ip.draw()
			Next
		Else
		
			If sets
				n=0
				For s:set=EachIn sets
					n:+1
					s.draw(n)
				Next
			EndIf
			
			SetColor 255,255,0
			For j:joint=EachIn joints
				DrawOval j.x-5,j.y-5,10,10
			Next
		EndIf
		
	End Method
End Type

Function biton(n,i)
	Return (n Shr i) Mod 2
End Function

Type point
	Field x#,y#
	Field ip:inpoint
	
	Function Create:point(x#,y#)
		p:point=New point
		p.x=x
		p.y=y
		Return p
	End Function
	
	Method draw()
		DrawRect x-1,y-1,3,3
	End Method
End Type

Const closelimit#=50
Type inpoint Extends point
	Field numclose
	
	Function Create:point(x#,y#)
		ip:inpoint=New inpoint
		ip.x=x
		ip.y=y
		Return ip
	End Function
	
	Method close(tx#,ty#)
		dx#=x-tx
		dy#=y-ty
		If Abs(dx)>closelimit Or Abs(dy)>closelimit Return 0
		d#=dx*dx+dy*dy
		If d<=closelimit*closelimit Then Return 1 Else Return 0
	End Method
	
	Method draw()
		s#=numclose/100.0
		If s<limblimit s=0 Else s=1
		SetColor 255*s,0,255*(1-s)
		DrawRect x,y,1,1
		SetColor 255,255,255
	End Method
End Type

Type line
	Field p1:point,p2:point
	
	Function Create:line(p1:point,p2:point)
		l:line=New line
		l.p1=p1
		l.p2=p2
		Return l
	End Function
	
	Method xaty#(y#)
		If (p1.y<y And p2.y<y) Or (p1.y>y And p2.y>y) Or p1.y=p2.y
			Return -1
		EndIf
		
		Return p1.x+(p2.x-p1.x)*(y-p1.y)/(p2.y-p1.y)
	End Method
End Type

Function triangulate:TList(points:TList)

	c=points.count()
	If c<3 Return New TList
	l:TList=New TList
	While c>3	
		Local array:point[]
		array=point[](points.toarray())
	
		
		i=0
		go=0
		While Not go
			p1:point=array[i]
			p2:point=array[(i+1) Mod c]
			p3:point=array[(i+2) Mod c]
			
			midx#=(p1.x+p2.x+p3.x)/3.0
			midy#=(p1.y+p2.y+p3.y)/3.0
			
			hits=0
			For ii=0 To c-1
				x1#=array[ii].x
				y1#=array[ii].y
				x2#=array[(ii+1) Mod c].x
				y2#=array[(ii+1) Mod c].y
				If (y1-midy)*(y2-midy)<0
					ix#=x1+(x2-x1)*(midy-y1)/(y2-y1)
					If ix<midx hits:+1
				EndIf
			Next

			If (hits Mod 2) 'tri is inside polygon
			
				x1#=p1.x
				y1#=p1.y
				x2#=p3.x
				y2#=p3.y
				dx1#=x2-x1
				dy1#=y2-y1
				
				go=1
				n=(i+3) Mod c
				While n<>i
					x3#=array[n].x
					y3#=array[n].y
					dx2#=x3-x2
					dy2#=y3-y2
					
					If dx1<>dx2 Or x1<>x2 Or dy1<>dy2 Or y1<>y2
						lambda#=(y2-y1+dy2*(x1-x2)/dx2)/(dy1-dx1*dy2/dx2)
						mu#=(x1-x2+lambda*dx1)/dx2
						If lambda>0 And lambda<1
							If mu>=0 And mu<=1
								go=0
							EndIf
						EndIf
					EndIf
					x2=x3
					y2=y3
					n=(n+1) Mod c
				Wend
			EndIf
			
			If Not go
				i=(i+1) Mod c
				If i=0 Return Null
			EndIf
		Wend

		t:tri=tri.Create(p1,p2,p3)
		
		points.remove p2
		l.addlast t
		c:-1
	Wend
	
	array=point[](points.toarray())
	t:tri=tri.Create(array[0],array[1],array[2])
	l.addlast t
	
	Return l
	
End Function


Type tri
	Field p1:point,p2:point,p3:point
	
	Function Create:tri(p1:point,p2:point,p3:point)
		t:tri=New tri
		t.p1=p1
		t.p2=p2
		t.p3=p3
		Return t
	End Function
	
	Method draw()
		'DrawLine p1.x,p1.y,p2.x,p2.y
		'DrawLine p2.x,p2.y,p3.x,p3.y
		'DrawLine p3.x,p3.y,p1.x,p1.y
		
		'SetAlpha .5
		Local poly#[]
		SetColor 255,255,255
		poly=panuv([p1.x,p1.y,p2.x,p2.y,p3.x,p3.y])
		DrawtexturedPoly bluepaper,poly
		SetAlpha 1
	End Method
End Type
	
Type set
	Field points:TList
	Field border:TList
	Field hullpoints
	Field tris:TList
	Field midx#,midy#
	
	Method New()
		points=New TList
		border=New TList
		tris=New TList
	End Method
	
	Method close(x#,y#,closer#)
		For p:point=EachIn points
			dx#=x-p.x
			dy#=y-p.y
			d#=dx*dx+dy*dy
			If d<=closer*closer
				Return 1
			EndIf
		Next
		Return 0
	End Method
	
	Method join(s:set)
		For ip2:inpoint=EachIn s.points
			If Not points.contains(ip2)
				points.addlast ip2
			EndIf
		Next
	End Method
	
	Method intersect:TList(s:set)
		l:TList=New TList
		For p:point=EachIn points
			If s.points.contains(p)
				l.addlast p
			EndIf
		Next
		Return l
	End Method
	
	
	Method wrap(n)
		c=Len(array)
		While n<0
			n:+c
		Wend
		Return n Mod c
	End Method
	
	
	Method enmiddle()
		midx#=0
		midy#=0
		For p:point=EachIn points
			midx:+p.x
			midy:+p.y
		Next
		midx:/points.count()
		midy:/points.count()
		For p=EachIn points
			p.x:-midx
			p.y:-midy
		Next
	End Method	
	
	Method draw(n)
		'red=biton(n,0)+biton(n,3)+biton(n,6)
		'green=biton(n,1)+biton(n,4)+biton(n,7)
		'blue=biton(n,2)+biton(n,5)+biton(n,8)
		's#=85*(1-n/10.0)
		'SetColor red*s,green*s,blue*s
		
		For t:tri=EachIn tris
			t.draw()
		Next
	End Method
End Type	

Type joint
	Field s1:set,s2:set
	Field x#,y#
	
	Function Create:joint(x#,y#,s1:set,s2:set)
		j:joint=New joint
		j.x=x
		j.y=y
		j.s1=s1
		j.s2=s2
		Return j
	End Function
	
End Type
	
Function changestate(i)
	Select i
	Case -1
		shapes.remove dude
		state=-1
	Case 0
		dude=New shape
		state=0
		ox=mx
		oy=my
	Case 1
		dude.analyse()
		state=1
	Case 2
		state=2
	End Select
End Function





Function panuv:Float[] (poly:Float[] ) 
	Local opoly:Float[Len(poly) * 2] 
	i = 0
	While i < Len(poly) 
		opoly[i * 2] = poly[i] 
		opoly[i * 2 + 1] = poly[i + 1] 
		u:Float = ZoomX(poly[i] ) / gwidth
		'If u < 0 u:+1
		v:Float = ZoomY(poly[i + 1] ) / gheight
		'If v < 0 v:+1
		opoly[i * 2 + 2] = u
		opoly[i * 2 + 3] = v
		i:+2
	Wend
	Return opoly
End Function

'graphics init
Incbin "yellowpaper.jpg"
Incbin "bluepaper.jpg"
Incbin "greenpaper.jpg"
Incbin "brownpaper.jpg"
Function initgfx()
	'SetGraphicsDriver d3d7max2ddriver() 
	AppTitle = "Petri Purho can shake my wobbly flagella."
	Graphics gwidth, gheight
	SetBlend ALPHABLEND
	SetClsColor 100, 100, 100
	
	yellowpaper = LoadImage("incbin::yellowpaper.jpg") 
	bluepaper = LoadImage("incbin::bluepaper.jpg") 
	greenpaper = LoadImage("incbin::greenpaper.jpg") 
	brownpaper = LoadImage("incbin::brownpaper.jpg") 
End Function




Graphics 800,800,0
SetBlend ALPHABLEND


Function drawdude:shape()
	dude=New shape
	state=-1
	ox#=0
	oy#=0
	limblimit#=.6

	panx=400
	pany=400
	While state<>2 And Not (KeyHit(KEY_ESCAPE) Or AppTerminate())

		mx#=MouseX()
		my#=MouseY()
	
		Select state
		Case -1
			If MouseHit(1)
				changestate(0)
			EndIf
		Case 0 'drawing
			dx#=mx-ox
			dy#=my-oy
			d#=dx*dx+dy*dy
			If d>=drawgap*drawgap
				d=Sqr(d)
				dx:*drawgap/d
				dy:*drawgap/d
				dude.addpoint(ox+dx,oy+dy)
				ox=mx
				oy=my
			EndIf
		Case 1
			DrawLine mx,0,mx,my
			If dude.inside(mx,my)
				DrawLine 0,my,mx,my
			EndIf
			If MouseHit(2)
				changestate(-1)
			EndIf
			If KeyDown(KEY_DOWN) Or KeyDown(KEY_UP)
				limblimit:+(KeyDown(KEY_UP)-KeyDown(KEY_DOWN))*.01
				dude.makesets()
			EndIf
			DrawText limblimit,0,0
			
			If KeyHit(KEY_SPACE)
				changestate(2)
			EndIf
		End Select
		
		For s:shape=EachIn shapes
			s.draw()
		Next

		
		Flip
		Cls
	Wend
	
	dude.finish()
	Return dude
End Function

initgfx()
drawdude()
'rungame()