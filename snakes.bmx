Type snake
	Field x#,y#
	Field an#,width#=10
	Field maxlength=200
	Field segments#[]
	
	Function Create:snake(x#,y#)
		s:snake=New snake
		s.x=x
		s.y=y
		Return s
	End Function
	
	Method update()
		speed#=4
		x:+Cos(an)*speed
		y:+Sin(an)*speed
		
		numsegs=Len(segments)
		segments=segments[..numsegs+4]
		segments[numsegs]=x+Cos(an+90)*width
		segments[numsegs+1]=y+Sin(an+90)*width
		segments[numsegs+2]=x+Cos(an-90)*width
		segments[numsegs+3]=y+Sin(an-90)*width
		If Len(segments)>maxlength
			segments=segments[4..]
		EndIf
	End Method
	
	Method draw()
		If Len(segments)>=8
			Local ox1#,oy1#,ox2#,oy2#,x2#,y2#,x1#,y1#
			Local poly#[]
			ox1=segments[0]
			oy1=segments[1]
			ox2=segments[2]
			oy2=segments[3]
			tailan#=ATan2(oy2-oy1,ox2-ox1)-90
			tailx#=(ox1+ox2)/2
			taily#=(oy1+oy2)/2
			DrawLine tailx,taily,tailx+20*Cos(tailan),taily+20*Sin(tailan)
			For n=1 To Len(segments)/4 - 1
				x1=segments[n*4]
				y1=segments[n*4+1]
				x2=segments[n*4+2]
				y2=segments[n*4+3]
				poly#=[ox1,oy1,ox2,oy2,x2,y2,x1,y1]
				'SetAlpha .5
				'DrawPoly poly
				drawtexturedpoly bluepaper,panuv(poly)
				'SetAlpha 1
				DrawLine ox1,oy1,x1,y1
				DrawLine ox2,oy2,x2,y2
				ox1=x1
				oy1=y1
				ox2=x2
				oy2=y2
			Next
			headan#=ATan2(y2-y1,x2-x1)+90
			headx#=(x1+x2)/2
			heady#=(y1+y2)/2
		EndIf
	End Method
End Type

Function andiff#(an1,an2)
	an#=an1-an2
	If an<-180 an:+360
	If an>180 an:+360
	Return an
End Function


Graphics 800,800,0
SetBlend ALPHABLEND

Global bluepaper:timage=LoadImage("fatgiraffe.jpg")

s:snake=snake.Create(400,400)
s.width=50

done=0
While Not done
	
	mx=MouseX()
	my=MouseY()
	
	an#=ATan2(my-s.y,mx-s.x)
	diff#=andiff(an,s.an)
	If Abs(diff)>3 diff=Sgn(diff)*3
	s.an:+diff
	's.an:+3

	s.update
	s.draw

	Flip
	Cls
	
	If KeyHit(KEY_ESCAPE) Or AppTerminate()
		done=1
	EndIf
Wend



Function panuv:Float[] (poly:Float[] ) 
	Local opoly:Float[Len(poly) * 2] 
	i = 0
	While i < Len(poly) 
		opoly[i * 2] = poly[i] 
		opoly[i * 2 + 1] = poly[i + 1] 
		u:Float = poly[i] / gwidth
		'If u < 0 u:+1
		v:Float = poly[i + 1] / gheight
		'If v < 0 v:+1
		opoly[i * 2 + 2] = u
		opoly[i * 2 + 3] = v
		i:+2
	Wend
	Return opoly
End Function

Function DrawTexturedPoly(image:TImage, xyuv:Float[] , frame = 0, vertex = -1) 
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
	Const scale#=1
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
