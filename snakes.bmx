Global snakes:TList
Type snake
	Field x#,y#
	Field an#,width#=10
	Field maxlength=100
	Field segments#[]
	Field numsegs
	Field t
	
	Method New()
		snakes.addlast Self
	End method
	
	Function Create:snake(x#,y#)
		s:snake=New snake
		s.x=x
		s.y=y
		s.segments=New Float[8]
		Return s
	End Function
	
	Method chase(tx#,ty#)
		If ty=y And tx=x Return
		chasean#=ATan2(ty-y,tx-x)
		diff#=andiff(chasean,an)
		diff#=Sgn(diff)*5
		an:+diff
	End Method
	
	Method update()
		speed#=4
		x:+Cos(an)*speed
		y:+Sin(an)*speed
		
		t:+1
		If t Mod 3 = 0
			t=0
			numsegs:+1
			segments=segments[..numsegs*4]
		EndIf
		If numsegs>0
			segments[numsegs*4-4]=x+Cos(an+90)*width
			segments[numsegs*4-3]=y+Sin(an+90)*width
			segments[numsegs*4-2]=x+Cos(an-90)*width
			segments[numsegs*4-1]=y+Sin(an-90)*width
		EndIf
		If numsegs>maxlength
			segments=segments[4..]
			numsegs:-1
		EndIf
	End Method
	
	Method draw()
		If Len(segments)>=8
			Local ox1#,oy1#,ox2#,oy2#,x2#,y2#,x1#,y1#
			Local poly#[]
			If numsegs=maxlength frac#=t/3.0 Else frac#=0
			ox1=segments[0]*(1-frac)+segments[4]*frac
			oy1=segments[1]*(1-frac)+segments[5]*frac
			ox2=segments[2]*(1-frac)+segments[6]*frac
			oy2=segments[3]*(1-frac)+segments[7]*frac
			tailan#=ATan2(oy2-oy1,ox2-ox1)-90
			tailx#=(ox1+ox2)/2
			taily#=(oy1+oy2)/2
			ox#=ox1
			oy#=oy1
			For n=0 To 6
				endan#=tailan+n*30-90
				tx#=tailx+Cos(endan)*width
				ty#=taily+Sin(endan)*width
				poly=[tailx,taily,ox,oy,tx,ty]
				drawtexturedpoly bluepaper,panuv(poly)
				DrawLine ox,oy,tx,ty
				ox=tx
				oy=ty
			Next
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
			ox#=x2
			oy#=y2
			For n=0 To 6
				endan#=headan+n*30-90
				tx#=headx+Cos(endan)*width
				ty#=heady+Sin(endan)*width
				poly=[headx,heady,ox,oy,tx,ty]
				drawtexturedpoly bluepaper,panuv(poly)
				DrawLine ox,oy,tx,ty
				ox=tx
				oy=ty
			Next
		EndIf
	End Method
End Type

Function andiff#(an1,an2)
	an#=an1-an2
	While an<-180
		an:+360
	Wend
	While an>180
		an:-360
	Wend
	Return an
End Function


Global gwidth,gheight
gwidth=800
gheight=800
Graphics 800,800,0
SetBlend ALPHABLEND

Global bluepaper:timage=LoadImage("rockstar fencing/bluepaper.jpg")

snakes=New TList

s1:snake=snake.Create(400,400)
s1.width=20

s2:snake=snake.Create(100,100)

done=0
While Not done
	
	mx=MouseX()
	my=MouseY()
	
	s1.chase mx,my
	s2.chase 800-mx,800-my

	For s:snake=EachIn snakes
		s.update
	Next
	
	For s:snake=EachIn snakes
		s.draw
	Next

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
