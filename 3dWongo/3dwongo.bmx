Function ZoomX:Float(x:Float) 
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

Function DrawTexturedPoly(image:TImage, xyuv:Float[] , frame = 0, vertex = -1) 
	Local handle_x#,  handle_y#
	GetHandle handle_x#,  handle_y#
	Local origin_x#,  origin_y#
	GetOrigin origin_x#,  origin_y#	
	
'	Local D3DDriver:TD3D7Max2DDriver = TD3D7Max2DDriver(_max2dDriver)
	
	Assert Image, "Image not found"
	
	'If D3DDriver Then 
	'	DrawTexturedPolyD3D ..
	'		D3DDriver,..
	'		 TD3D7ImageFrame(image.Frame(frame)), ..
	'		 xyuv, handle_x, handle_y, origin_x,origin_y, vertex*4
	'	Return
	'End If
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

Function DrawZoomTexturedPoly(image:TImage, poly:Float[] ) 
	poly = poly[..] 
	While i < Len(poly)
		poly[i] = ZoomX(poly[i] ) 
		poly[i + 1] = zoomy(poly[i + 1]) 
		i:+4
	Wend
	DrawTexturedPoly image, poly
End Function




Function rotationquat:Float[] (pitch:Float, yaw:Float, roll:Float) 
	a:Float = Cos(pitch / 2) 
	b:Float = Sin(pitch / 2) 
	c:Float = Cos(yaw / 2) 
	d:Float = Sin(yaw / 2) 
	e:Float = Cos(roll / 2) 
	f:Float = Sin(roll / 2) 
	Return[a * c * e - b * d * f, b * c * e + a * d * f, a * d * e - b * c * f, b * d * e + a * c * f] 
End Function

Function quatproduct:Float[] (q1:Float[] , q2:Float[] ) 
	a:Float = q1[0] * q2[0] - q1[1] * q2[1] - q1[2] * q2[2] - q1[3] * q2[3] 
	b:Float = q1[1] * q2[0] + q1[0] * q2[1] + q1[2] * q2[2] - q1[3] * q2[3] 
	c:Float = q1[2] * q2[0] + q1[3] * q2[1] + q1[0] * q2[2] - q1[1] * q2[3] 
	d:Float = q1[3] * q2[0] - q1[2] * q2[1] + q1[1] * q2[2] + q1[0] * q2[3] 
	Return[a, b, c, d] 
End Function

Function rotatevector:Float[] (v:Float[] , q:Float[] ) 
	x:Float = (q[0] * q[0] + q[1] * q[1] - q[2] * q[2] - q[3] * q[3] ) * v[0] + (2 * q[1] * q[2] - 2 * q[0] * q[3] ) * v[1] + (2 * q[0] * q[2] + 2 * q[1] * q[3] ) * v[2] 
	y:Float = (2 * q[0] * q[3] + 2 * q[1] * q[2] ) * v[0] + (q[0] * q[0] - q[1] * q[1] + q[2] * q[2] - q[3] * q[3] ) * v[1] + (2 * q[2] * q[3] - 2 * q[0] * q[1] ) * v[2] 
	z:Float = (2 * q[1] * q[3] - 2 * q[0] * q[2] ) * v[0] + (2 * q[0] * q[1] + 2 * q[2] * q[3] ) * v[1] + (q[0] * q[0] - q[1] * q[1] - q[2] * q[2] + q[3] * q[3] ) * v[2] 
	
	Return[x, y, z] 
End Function

Function mulvec:Float[] (s:Float, v:Float[] ) 
	Return[s * v[0] , s * v[1] , s * v[2] ] 
End Function

Function addvec:Float[] (v1:Float[] , v2:Float[] ) 
	Return[v1[0] + v2[0] , v1[1] + v2[1] , v1[2] + v2[2] ] 
End Function

Function project3d:Float[] (v:Float[] , ox:Float = 0, oy:Float = 0) 
	x:Float = v[0] / (v[2] *.01) + ox
	y:Float = v[1] / (v[2] *.01) + oy
	Return[x, y] 
End Function



Type shape
	Field vertices:Float[,] 
	Field tvertices:Float[,] 
	Field pvertices:Float[,] 
	Field tris[,] 
	Field tex:TImage[] 
	Field q:Float[4] 
	Field x:Float, y:Float
	
	Method New() 
		q =[1.0, 0.0, 0.0, 0.0] 
		shapes.addlast Self
		x = 0
		y = 0
	End Method
	
	Function Create:shape(vertices:Float[] , tris[] , img:timage) 
		s:shape = New shape
		i = 0
		s.vertices = New Float[Len(vertices) / 3, 3] 
		s.tvertices = New Float[Len(vertices) / 3, 3] 
		s.pvertices = New Float[Len(vertices) / 3, 2] 
		While i * 3 < Len(vertices) 
			s.vertices[i, 0] = vertices[i * 3] 
			s.vertices[i, 1] = vertices[i * 3 + 1] 
			s.vertices[i, 2] = vertices[i * 3 + 2] 
			i:+1
		Wend
		i = 0
		s.tris = New Int[Len(tris) / 3, 3] 
		s.tex = New TImage[Len(s.tris)] 
		While i * 3 < Len(tris) 
			s.tris[i,0] = tris[i * 3] 
			s.tris[i,1] = tris[i * 3 + 1] 
			s.tris[i, 2] = tris[i * 3 + 2] 
			s.tex[i] = img
			i:+1
		Wend
		
		Return s
	End Function
	
	Method transform() 
		Local p:Float[2] 
		Local v:Float[3] 
		For i = 0 To Len(vertices) / 3 - 1
			vx:Float = vertices[i, 0] 
			vy:Float = vertices[i, 1] 
			vz:Float = vertices[i, 2] 
			v =[vx, vy, vz] 
			v = rotatevector(v, q) 
			v = addvec(v, [0.0, 0.0, 100.0] ) 
			tvertices[i, 0] = v[0] 
			tvertices[i, 1] = v[1] 
			tvertices[i, 2] = v[2] 
			p = project3d(v, x, y) 
			pvertices[i, 0] = p[0] 
			pvertices[i, 1] = p[1] 
		Next
	End Method
	
	Method draw() 
		For i = 0 To Len(pvertices) / 2 - 1
			DrawRect pvertices[i, 0] + 400, pvertices[i, 1] + 400, 1, 1
		Next
		Local poly:Float[] 
		For i = 0 To Len(tris) / 3 - 1
			a = tris[i, 0] 
			b = tris[i, 1] 
			c = tris[i, 2] 
			poly =[tvertices[a, 0] , tvertices[a, 1] , tvertices[a, 2] , tvertices[b, 0] , tvertices[b, 1] , tvertices[b, 2] , tvertices[c, 0] , tvertices[c, 1] , tvertices[c, 2] ] 
			dot:Float = dotnormal(poly) 
			'DrawLine pvertices[a, 0] + 400, pvertices[a, 1] + 400, pvertices[b, 0] + 400, pvertices[b, 1] + 400
			'DrawLine pvertices[b, 0] + 400, pvertices[b, 1] + 400, pvertices[c, 0] + 400, pvertices[c, 1] + 400
			'DrawLine pvertices[c, 0] + 400, pvertices[c, 1] + 400, pvertices[a, 0] + 400, pvertices[a, 1] + 400
			If dot > 0
				poly =[pvertices[a, 0] , pvertices[a, 1] , pvertices[b, 0] , pvertices[b, 1] , pvertices[c, 0] , pvertices[c, 1] ] 
				DrawZoomTexturedPoly tex[i] , panuv(poly) 
			EndIf
		Next
	End Method
End Type

Type sphere Extends shape
	Field rowsegs[] 
	Field numrows, segsize:Float
	Field numsegs
	Method makesphere:sphere(r:Float, nrows, ssize:Float) 
		numrows = nrows
		segsize = ssize
		rowsegs = New Int[numrows] 
		rowsegs[0] = 1
		rowsegs[numrows - 1] = 1
		numsegs = 2
		For i = 1 To numrows - 2
			theta:Float = 180 * Float(i) / (numrows - 1) 
			n = 2 * Pi * r * Sin(theta) / segsize
			If n < 3 Then n = 3
			rowsegs[i] = n
			numsegs:+n
		Next
		vertices = New Float[numsegs, 3] 
		tvertices = New Float[numsegs, 3] 
		pvertices = New Float[numsegs, 2] 
		Local phis:Float[] 
		phis = New Float[numsegs] 
		j = 0
		For i = 0 To numrows - 1
			theta:Float = 180 * Float(i) / (numrows - 1) 
			rowr:Float = r * Sin(theta) 
			For k = 0 To rowsegs[i] - 1
				phi:Float = k * 360.0 / rowsegs[i] 
				vertices[j, 0] = Cos(phi) * rowr
				vertices[j, 1] = - Cos(theta) * r
				vertices[j, 2] = - Sin(phi) * rowr
				phis[j] = phi
				j:+1
			Next
		Next
		numtris = 2 * (numsegs - 2) 
		tris = New Int[numtris, 3] 
		tex = New TImage[numtris] 
		v1 = 0
		tri = 0
		For i = 0 To numrows - 2
			v2 = v1 + rowsegs[i] 
			j1 = 0
			j2 = 0
			otri = tri
			While j1 < rowsegs[i]-1 Or j2 < rowsegs[i + 1] -1
				If j1 = rowsegs[i] - 1  'at end of top row; finish off bottom row
					tris[tri, 0] = v1 + j1
					tris[tri, 1] = v2 + j2
					tris[tri, 2] = v2 + j2 + 1
					j2:+1
				Else
					If j2 = rowsegs[i + 1] - 1 'at end of bottom row; finish off top row
						tris[tri, 0] = v1 + j1 + 1
						tris[tri, 1] = v1 + j1
						tris[tri, 2] = v2 + j2
						j1:+1
					Else
						nphi1:Float = phis[v1 + j1 + 1] 
						nphi2:Float = phis[v2 + j2 + 1] 
						If nphi1 < nphi2 'top point is next
							tris[tri, 0] = v1 + j1 + 1
							tris[tri, 1] = v1 + j1
							tris[tri, 2] = v2 + j2
							j1:+1
						Else
							tris[tri, 0] = v1 + j1
							tris[tri, 1] = v2 + j2
							tris[tri, 2] = v2 + j2 + 1
							j2:+1
						End If
					End If
				End If
				tri:+1
			Wend
			If rowsegs[i] = 1
				tris[tri, 0] = v1
				tris[tri, 1] = v2 + j2
				tris[tri, 2] = v2
				tri:+1
			ElseIf rowsegs[i + 1] = 1
				tris[tri, 0] = v1
				tris[tri, 1] = v1 + j1
				tris[tri, 2] = v2
				tri:+1
			Else
				tris[tri, 0] = v1
				tris[tri, 1] = v1 + j1
				tris[tri, 2] = v2 + j2
				tri:+1
				tris[tri, 0] = v1
				tris[tri, 1] = v2 + j2
				tris[tri, 2] = v2
				tri:+1
			End If
			If i Mod 2 = 0
				img:TImage = brownpaper
			Else
				img:TImage = yellowpaper
			End If
			For t = otri To tri - 1
				tex[t] = img
			Next
			v1 = v2
		Next
		Print tri
	End Method
End Type

Function dotnormal:Float(poly:Float[] ) 
	dx:Float = poly[3] - poly[0] 
	dy:Float = poly[4] - poly[1] 
	dz:Float = poly[5] - poly[2] 
	d:Float = Sqr(dx * dx + dy * dy + dz * dz) 
	dx:/d
	dy:/d
	dz:/d
	ex:Float = poly[6] - poly[0] 
	ey:Float = poly[7] - poly[1] 
	ez:Float = poly[8] - poly[2] 
	e:Float = Sqr(ex * ex + ey * ey + ez * ez) 
	ex:/d
	ey:/d
	ez:/d
	nx:Float = dy * ez - dz * ey
	ny:Float = dz * ex - dx * ez
	nz:Float = dx * ey - dy * ex
	vx:Float = (poly[0] + poly[3] + poly[6] ) / 3
	vy:Float = (poly[1] + poly[4] + poly[7] ) / 3
	vz:Float = (poly[2] + poly[5] + poly[8] ) / 3
	dot:Float = vx * nx + vy * ny + vz * nz
	Return dot
End Function

Global gwidth:Float, gheight:Float
Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom
Global tpanx:Float = 0, tpany:Float = 0
Global yellowpaper:TImage
Global bluepaper:TImage
Global brownpaper:TImage
Global greenpaper:TImage

Global shapes:TList

Incbin "yellowpaper.jpg"
Incbin "bluepaper.jpg"
Incbin "greenpaper.jpg"
Incbin "brownpaper.jpg"

'graphics init
Function initgfx() 
	'SetGraphicsDriver d3d7max2ddriver() 
	AppTitle = "3d Wongo!"
	Graphics gwidth, gheight
	SetBlend ALPHABLEND
	
	yellowpaper = LoadImage("incbin::yellowpaper.jpg") 
	bluepaper = LoadImage("incbin::bluepaper.jpg") 
	greenpaper = LoadImage("incbin::greenpaper.jpg") 
	brownpaper = LoadImage("incbin::brownpaper.jpg") 
End Function

Function initgame() 
	shapes = New TList
End Function

'START!
gwidth = 800
gheight = 800
initgfx() 
SeedRnd MilliSecs()

initgame() 

Local vertices:Float[] 
size:Float = 25
vertices =[- size, - size, - size, size, - size, - size, - size, size, - size, size, size, - size, - size, - size, size, - size, size, size, size, size, size, size, - size, size, - size *.4, - size *.4, - size, size *.4, - size *.4, - size, - size *.4, size *.4, - size, size *.4, size *.4, - size] 
Local tris[] 
'																							   |
tris =[1, 7, 3, 3, 7, 6, 7, 4, 5, 7, 5, 6, 4, 0, 2, 4, 2, 5, 0, 4, 7, 0, 7, 1, 2, 3, 5, 3, 6, 5, 0, 1, 8, 1, 9, 8, 9, 1, 3, 9, 3, 11, 2, 10, 11, 2, 11, 3, 2, 0, 8, 2, 8, 10] 
'																							   |
'tris =[0, 1, 2] 
		
cube:shape = shape.Create(vertices, tris,yellowpaper) 
'shapes.Remove cube
sp:sphere = New sphere
sp.makesphere(70, 20, 20) 

Local q:Float[4] , unitx:Float[4] , unity:Float[4] , unitz:Float[4] 
t = 0
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate()) 
	t:+1
	mx:Float = MouseX() - 400
	my:Float = MouseY() - 400

	dx:Float = mx - sp.x
	dy:Float = my - sp.y
	sp.q = rotationquat(dy * 9 / 40.0 - 90, - dx * 18 / 40.0, 0) 
	
	
	d:Float = Sqr(mx * mx + my * my) 
	x:Float = d / 30 + 5
	cube.vertices[8, 0] = - x
	cube.vertices[8, 1] = - x
	cube.vertices[9, 0] = x
	cube.vertices[9, 1] = - x
	cube.vertices[10, 0] = - x
	cube.vertices[10, 1] = x
	cube.vertices[11, 0] = x
	cube.vertices[11, 1] = x
	cube.q = rotationquat(my * 9 / 40.0, - mx * 9 / 40.0, 0) 
	
	sp.x:+(mx - sp.x) *.01
	sp.y:+(my - sp.y) *.01
	
	
	For s:shape = EachIn shapes
		s.transform() 
	Next
	
	SetColor 255, 255, 255

	x:Float = gwidth / 2
	y:Float = gheight / 2
	Local bpoly:Float[] 
	bpoly =[- x, - y, x, - y, x, y, - x, y] 
	DrawZoomTexturedPoly bluepaper, panuv(bpoly) 

	For s:shape = EachIn shapes
		s.draw() 
	Next
	
	
	
	DrawText dx, 0, 0
	DrawText dy, 0, 15
	
	
	

	
	Flip
	Cls
Wend