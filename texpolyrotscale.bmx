Graphics 640,480,0

img:timage=LoadImage("fatgiraffe.jpg")

While Not KeyHit(KEY_ESCAPE)
	mx=MouseX()-320
	my=MouseY()-240
	
	d#=Sqr(mx*mx+my*my)*.01
	an#=ATan2(my,mx)
	
	SetScale 1,1
	SetRotation 0
	
	DrawText "scale = "+String(d),0,0
	DrawText "rotation = "+String(an),0,15
	
	SetRotation an
	SetScale d,d

	Local ix#, iy#, jx#, jy#
	GetTransformVars( ix, iy, jx, jy )
	
	'draw a rectangle
	SetOrigin 200,240
	DrawRect 0,0,100,100
	
	'draw a normal polygon in the shape of a rectangle
	SetOrigin 400,240
	Local poly2#[8]
	poly2=[	0.0,0.0, 100.0,0.0, 100.0,100.0, 0.0,100.0 ]
	DrawPoly poly2
	
	'draw a textured polygon in the shape of a rectangle
	SetOrigin 320,240
	Local poly#[16]
	poly=[	0.0,0.0,0.0,0.0, 100.0,0.0,1.0,0.0, 100.0,100.0,1.0,1.0, 0.0,100.0,0.0,1.0 ]
	drawtexturedpoly( img, poly )
	
	SetOrigin 0,0
	SetScale 1,1
	SetRotation 0

	DrawText "ix = "+String(ix),0,60
	DrawText "iy = "+String(iy),0,75
	DrawText "jx = "+String(jx),0,90
	DrawText "jy = "+String(jy),0,105
	
	
	Flip
	Cls
Wend



'Proger's DrawTexturedPoly code


Function DrawTexturedPoly( image:TImage,xyuv#[],frame=0, vertex = -1)
	Local handle_x#,  handle_y#
	GetHandle handle_x#,  handle_y#
	Local origin_x#,  origin_y#
	GetOrigin origin_x#,  origin_y#	
	
	Assert Image, "Image not found"
	
	Local  OGLDriver:TGLMax2DDriver = TGLMax2DDriver(_max2dDriver)
	If OGLDriver Then
			DrawTexturedPolyOGL ..
				OGLDriver,..
				 TGLImageFrame(image.Frame(frame)), ..
				 xyuv, handle_x, handle_y, origin_x,origin_y,  vertex*4
		Return
	End If
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
	Const scale#=1.28
	Local ix#= c*tform_scale_x*scale
	Local iy#= -s*tform_scale_y*scale
	Local jx#= s*tform_scale_x*scale
	Local jy#= c*tform_scale_y*scale
	
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