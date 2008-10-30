Global gwidth#,gheight#
Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom


Global sounditems:TList
Type sounditem
	Field chan:TChannel
	Field loops
	Field sound:TSound
	Field rate#,pan#,volume#
	
	Function Create:sounditem(sound:TSound,rate#,pan#,volume#,loops=0)
		si:sounditem=New sounditem
		si.chan=AllocChannel()
		si.sound = sound
		si.loops=loops
		si.rate=rate
		si.pan=pan
		si.volume=volume
		si.update
		PlaySound(si.sound,si.chan)
		sounditems.AddLast si
		Return si
	End Function
	
	Method Delete()
		StopChannel chan
	End Method

	Method update()	
		SetChannelRate chan,rate
		SetChannelPan chan,pan
		SetChannelVolume chan , volume
		If Not ChannelPlaying(chan)
			If loops
				PlaySound sound , chan
			Else
				sounditems.Remove sound
			EndIf
		EndIf
	End Method
	
End Type



Global snakes:TList
Type snake
	Field x#,y#
	Field chasex#,chasey#
	Field an#,width#=10,normalwidth#
	Field maxlength=20
	Field speed#=4
	Field segments#[]
	Field numsegs
	Field t
	Field mouthx#,mouthy#
	Field tongue:TList
	Field tonguestretch#,tlength#
	Field tonguex#,tonguey#
	Field tonguestate
	Field caught:fruit
	Field eyean1#,eyean2#,veyean1#,veyean2#
	Field lumps:TList
	Field lastcatch
	
	Method New()
		snakes.addlast Self
		tongue=New TList
		length#=15
		For c=1 To 10
			l:licker=New licker
			l.length=length
			tlength:+length
			length:*.95
			tongue.addlast l
		Next
		lumps=New TList
		
	End Method
	
	Function Create:snake(x#,y#)
		s:snake=New snake
		s.x=x
		s.y=y
		s.normalwidth=10
		s.maxlength=Rand(15,50)
		s.speed=Rnd(2,4)
		s.segments=New Float[8]
		Return s
	End Function
	
	Method chase(tx#,ty#)
		chasex=tx
		chasey=ty
	End Method
	
	Method update()
		If chasex<>x Or chasey<>y
			chasean#=ATan2(chasey-y,chasex-x)
			diff#=andiff(chasean,an)
			an:+Sgn(diff)*3
		Else
			diff=0
		EndIf

		If Abs(diff)>20 Then diff=20*Sgn(diff)
		tspeed#=(1.5-Cos(diff*4.5))*speed
		x:+Cos(an)*speed
		y:+Sin(an)*speed
		
		width:+(normalwidth-width)*.3
		t:+1
		If t Mod 6= 0
			t=0
			numsegs:+1
			segments=segments[..numsegs*4]
		EndIf
		If numsegs>0
			segments[numsegs*4-4]=x
			segments[numsegs*4-3]=y
			segments[numsegs*4-2]=width
			segments[numsegs*4-1]=an
		EndIf
		If numsegs>maxlength
			segments=segments[4..]
			numsegs:-1
		EndIf
		
		'lumps
		For a:lump=EachIn lumps
			a.seg:-.5
			If a.seg<=0
				lumps.remove a
			EndIf
		Next
		
		'tongue
		mouthx#=x+Cos(an)*width*.5
		mouthy#=y+Sin(an)*width*.5

		tonguex=x+Cos(an)*width*2
		tonguey=y+Sin(an)*width*2
		mindist#=-1
		mindiff#=0
		For f:fruit=EachIn fruits
			dx#=f.x-x
			dy#=f.y-y
			d#=Sqr(dx*dx+dy*dy)
			diff#=Abs(andiff(ATan2(dy,dx),an))
			If (diff<mindiff Or mindist=-1) And Abs(diff)<90 And d<tlength
				mindist=d
				mindiff=diff
				tonguex=f.x
				tonguey=f.y
			EndIf
		Next

		Select tonguestate
		Case 0 ' no fruit

			ostretch#=tonguestretch
			targstretch#=mindist/tlength
			tonguestretch:+(targstretch-tonguestretch)*.1
			
			tonguediff#=tonguestretch-ostretch
			
			If (MilliSecs()-lastcatch>1000) And Rand(100)=1
				tonguestate=2
				lastcatch=MilliSecs()
			EndIf
		Case 1
			tonguex=caught.x
			tonguey=caught.y
			For l:licker=EachIn tongue
				l.an:*.8
			Next
			tonguestretch:-.05
			If tonguestretch<=0
				tonguestate=0
				normalwidth:+1/normalwidth
				tlength:+1/normalwidth
				If Rand(5)=1 maxlength:+1
				a:lump=New lump
				a.seg=numsegs
				lumps.addlast a
				caught.dead=1
				caught:fruit=Null
				lastcatch=MilliSecs()
			EndIf
		Case 2 'hissing
			tonguestretch:+(.2-tonguestretch)*.1
			wobble#=MilliSecs()*10
			For l:licker=EachIn tongue
				l.an:+Cos(wobble)*5
				wobble:+45
			Next
			If MilliSecs()-lastcatch>500
				tonguestate=0
				lastcatch=MilliSecs()
			EndIf
		End Select

		ox#=mouthx
		oy#=mouthy
		oan#=an
		limit#=45
		wobble#=MilliSecs()
		For l:licker=EachIn tongue
			chasean#=ATan2(tonguey-oy,tonguex-ox)
			diff#=andiff(chasean,l.an+oan)
			l.an:+Sgn(diff)*2
			l.an:*.9
			If Abs(l.an)>limit l.an=Sgn(l.an)*limit
			wobble:+30
			oan:+l.an+Cos(wobble)*3
			nx#=ox+Cos(oan)*l.length*tonguestretch
			ny#=oy+Sin(oan)*l.length*tonguestretch
			ox=nx
			oy=ny
			limit:*.8
		Next
		
		Select tonguestate
		Case 0
			For f:fruit=EachIn fruits
				dx#=f.x-ox
				dy#=f.y-oy
				d#=dx*dx+dy*dy
				If d<90
					tonguestate=1
					caught=f
				EndIf
			Next
		Case 1
			caught.x=ox
			caught.y=oy
		End Select
	
	End Method
	
	Method draw()
		Local ox1#,oy1#,ox2#,oy2#,x2#,y2#,x1#,y1#
		Local poly#[]

		bodypaper:timage=getpaper("blue")
		tonguepaper:timage=getpaper("lightgrey")

		'draw tongue
		ox#=mouthx
		oy#=mouthy
		oan#=an
		ox1#=ox+Cos(oan+90)*width*.1
		oy1#=oy+Sin(oan+90)*width*.1
		ox2#=ox-Cos(oan+90)*width*.1
		oy2#=oy-Sin(oan+90)*width*.1
		wobble#=MilliSecs()
		For l:licker=EachIn tongue
			oan:+l.an+Cos(wobble)*3
			wobble:+30
			nx#=ox+Cos(oan)*l.length*tonguestretch
			ny#=oy+Sin(oan)*l.length*tonguestretch
			x1#=ox+Cos(oan+90)*width*.1
			y1#=oy+Sin(oan+90)*width*.1
			x2#=ox-Cos(oan+90)*width*.1
			y2#=oy-Sin(oan+90)*width*.1
			poly=[x1,y1,x2,y2,ox2,oy2,ox1,oy1]
			SetColor 255,255,255
			drawzoomtexturedpoly tonguepaper,panuv(poly)
			ox=nx
			oy=ny
			ox1=x1
			oy1=y1
			ox2=x2
			oy2=y2
		Next
		x1#=ox+Cos(oan-30)*width*.5
		y1#=oy+Sin(oan-30)*width*.5
		poly=[ox1, oy1, x1,y1, ox2, oy2]
		drawzoomtexturedpoly tonguepaper,panuv(poly)
		x1#=ox+Cos(oan+30)*width*.5
		y1#=oy+Sin(oan+30)*width*.5
		poly=[ox1, oy1, x1,y1, ox2, oy2]
		drawzoomtexturedpoly tonguepaper,panuv(poly)


		SetLineWidth 3
		SetColor 255,255,255
		If Len(segments)>=8

			If numsegs>=maxlength frac#=t/6.0 Else frac#=0
			tailx#=segments[0]*(1-frac)+segments[4]*frac
			taily#=segments[1]*(1-frac)+segments[5]*frac
			tailwidth#=segments[2]
			tailan#=segments[3]+180
			ox1#=tailx+Cos(tailan-90)*tailwidth
			oy1#=taily+Sin(tailan-90)*tailwidth
			ox2#=tailx+Cos(tailan+90)*tailwidth
			oy2#=taily+Sin(tailan+90)*tailwidth

			ox#=ox1
			oy#=oy1
			For n=0 To 6
				endan#=tailan+n*30-90
				tx#=tailx+Cos(endan)*tailwidth
				ty#=taily+Sin(endan)*tailwidth
				poly=[tailx,taily,ox,oy,tx,ty]
				dp#=Cos(tailan*2)*.2+.8
				SetColor 255*dp,255*dp,255*dp
				drawzoomtexturedpoly bodypaper,panuv(poly)
				SetColor 0,0,0
				SetAlpha .1
				DrawzoomLine ox,oy,tx,ty
				SetAlpha 1
				ox=tx
				oy=ty
			Next

			lumpsize#=.1*width
			lumpsize#=5
			For n=1 To Len(segments)/4 - 1
				sx#=segments[n*4]
				sy#=segments[n*4+1]
				swidth#=segments[n*4+2]
				For a:lump=EachIn lumps
					diff=Abs(a.seg-n)
					diff:*diff
					If diff=0
						swidth:+lumpsize
					ElseIf diff<5
						swidth:+lumpsize/diff
					EndIf
				Next
				san#=segments[n*4+3]
				x1#=sx+Cos(san+90)*swidth
				y1#=sy+Sin(san+90)*swidth
				x2#=sx+Cos(san-90)*swidth
				y2#=sy+Sin(san-90)*swidth
				poly#=[ox1,oy1,ox2,oy2,x2,y2,x1,y1]
				dp#=Cos(san*2)*.2+.8
				SetColor 255*dp,255*dp,255*dp
				drawzoomtexturedpoly bodypaper,panuv(poly)
				SetAlpha .1
				SetColor 0,0,0
				DrawzoomLine ox1,oy1,x1,y1
				DrawzoomLine ox2,oy2,x2,y2
				DrawzoomLine ox1,oy1,x2,y2
				DrawzoomLine ox2,oy2,x1,y1
				SetAlpha 1
				ox1=x1
				oy1=y1
				ox2=x2
				oy2=y2
			Next
			
			
			SetColor 255,255,255

			headan#=an'ATan2(y2-y1,x2-x1)+90
			headx#=x
			heady#=y
			headwidth#=width
			ox#=x+Cos(headan)*headwidth
			oy#=y+Sin(headan)*headwidth
			For n=0 To 6
				endan#=headan+n*30-90
				tx#=headx+Cos(endan)*headwidth
				ty#=heady+Sin(endan)*headwidth
				poly=[headx,heady,ox,oy,tx,ty]
				dp#=Cos(headan*2)*.2+.8
				SetColor 255*dp,255*dp,255*dp
				drawzoomtexturedpoly bodypaper,panuv(poly)
				SetColor 0,0,0
				SetAlpha .1
				DrawzoomLine ox,oy,tx,ty
				SetAlpha 1
				ox=tx
				oy=ty
			Next
			eyex1#=headx+(ox2-headx)*.7
			eyey1#=heady+(oy2-heady)*.7
			eyex2#=headx+(ox1-headx)*.7
			eyey2#=heady+(oy1-heady)*.7
			veyean1#:+andiff(ATan2(tonguey-eyey1,tonguex-eyex1),eyean1)*.5
			veyean2#:+andiff(ATan2(tonguey-eyey2,tonguex-eyex2),eyean2)*.5
			eyean1:+veyean1
			eyean2:+veyean2
			veyean1:*.9
			veyean2:*.9
			SetColor 255,255,255
			eyer#=width*.4
			Drawzoomcircle eyex1,eyey1,eyer
			Drawzoomcircle eyex2,eyey2,eyer
			SetColor 0,0,0
			Drawzoomcircle eyex1+Cos(eyean1)*eyer/2,eyey1+Sin(eyean1)*eyer/2,eyer/2
			Drawzoomcircle eyex2+Cos(eyean2)*eyer/2,eyey2+Sin(eyean2)*eyer/2,eyer/2
		EndIf
		
		'DrawText whistle.volume,0,0
		'DrawText whistle.rate,0,15
		
	End Method
End Type

Function getpaper:TImage(name$)
	Return timage(papers.valueforkey(name))
End Function

Global texes$[]=["red","yellow","green"]
Global fruits:TList
Type fruit
	Field x#,y#
	Field tex:TImage
	Field dead
	
	Method New()
		x=panx+Rnd(-1,1)*gwidth
		y=pany+Rnd(-1,1)*gheight
		'fruits.addlast Self
		tex=getpaper(texes[Rand(0,2)])
	End Method
	
	Method draw()
		Local poly#[18]
		n=0
		For an=0 To 359 Step 40
			px#=x+Cos(an)*9
			py#=y+Sin(an)*9
			poly[n]=px
			poly[n+1]=py
			n:+2
		Next
		drawzoomtexturedpoly tex,panuv(poly)
	End Method
End Type

Global onitems:TList
Global numscreens=0
Type screen
	Field sx,sy
	Field x#,y#
	Field neighbours:screen[3,3]
	Field items:TList
	Field number
	
	Method New()
		items=New TList
		neighbours[1,1]=Self
		number=numscreens
		numscreens:+1
	End Method
	
	Function Create:screen(sx,sy)
		s:screen=New screen
		s.sx=sx
		s.sy=sy
		s.x=sx*gwidth
		s.y=sy*gheight
		'numfruits=Rand(0,3)*10+Rand(10)
		'For c=1 To numfruits
		'	f:fruit=New fruit
		'	f.x=s.x+Rnd(-.5,.5)*gwidth
		'	f.y=s.y+Rnd(-.5,.5)*gheight
		'	s.items.addlast f
		'Next
		
		numclumps=Rand(3)
		For c=1 To numclumps
			x#=s.x+Rnd(-.5,.5)*gwidth
			y#=s.y+Rnd(-.5,.5)*gheight
			size=Rand(1,10)
			r#=30*Sqr(size)
			For n=1 To size
				f:fruit=New fruit
				an#=Rand(360)
				ro#=Rnd(0,1)*r
				f.x=x+Cos(an)*r
				f.y=y+Sin(an)*r
				s.items.addlast f
			Next
		Next
		Return s
	End Function
	
	Method update()
		If curscreen=Self
			fruits=New TList
			For nx=0 To 2
			For ny=0 To 2
			If nx<>1 Or ny<>1
				If Not neighbours[nx,ny]
					s2:screen=screen.Create(sx+nx-1,sy+ny-1)
					neighbours[nx,ny]=s2
				EndIf
				s2:screen=neighbours[nx,ny]
				s2.neighbours[2-nx,2-ny]=Self
				If nx=1
					s2.neighbours[0,2-ny]=neighbours[0,1]
					s2.neighbours[2,2-ny]=neighbours[2,1]
					s2.neighbours[0,1]=neighbours[0,ny]
					s2.neighbours[2,1]=neighbours[2,ny]
				EndIf
				If ny=1
					s2.neighbours[2-nx,0]=neighbours[1,0]
					s2.neighbours[2-nx,2]=neighbours[1,2]
					s2.neighbours[1,0]=neighbours[nx,0]
					s2.neighbours[1,2]=neighbours[nx,2]
				EndIf
				s2.update
			EndIf
			Next
			Next
		EndIf
		For f:fruit=EachIn items
			If f.dead
				items.remove f
			Else
				fruits.addlast f
			EndIf
		Next
	End Method
		
	Method draw(main=1)
		Rem
		drawzoomline x-gwidth/2,y-gheight/2,x+gwidth/2,y-gheight/2
		drawzoomline x-gwidth/2,y-gheight/2,x-gwidth/2,y+gheight/2

		co=0
		For nx=0 To 2
		For ny=0 To 2
			If neighbours[nx,ny] co:+1
		Next
		Next

		DrawText number+":"+sx+","+sy+" ("+co+")",zoomx(x),zoomy(y)
		EndRem
		
		For f:fruit=EachIn items
			f.draw
		Next
		If main
			For nx=0 To 2
			For ny=0 To 2
				If nx<>1 Or ny<>1
					neighbours[nx,ny].draw False
					'drawzoomline x,y,neighbours[nx,ny].x,neighbours[nx,ny].y
				EndIf
			Next
			Next
		EndIf
	End Method
	
End Type

Type licker
	Field x#,y#,length#,an#
End Type
Type lump
	Field seg#
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


gwidth=800
gheight=400
AppTitle="Snakey ssss"
SetGraphicsDriver GLMax2DDriver()
Graphics gwidth,gheight,0
SetBlend ALPHABLEND

Global papers:tmap=New tmap
dirhandle=ReadDir("paper")
fname$=NextFile(dirhandle)
While fname
	If fname[Len(fname)-9..]="paper.jpg"
		papers.insert fname[..Len(fname)-9],LoadImage("paper/"+fname)
	EndIf
	fname=NextFile(dirhandle)
Wend

sounditems=New TList
Global whistleupsound:TSound=LoadSound("whistleup.ogg")
Global whistledownsound:TSound=LoadSound("whistledown.ogg")

snakes=New TList

s1:snake=snake.Create(0,0)

Global curscreen:screen=screen.Create(0,0)

For c=1 To 100
	New fruit
Next

done=0
oldms=MilliSecs()
'zoom=.2
While Not done

	panx=s1.x
	pany=s1.y
	
	mx=unzoomx(MouseX())
	my=unzoomy(MouseY())
	
	s1.chase mx,my
	
	curscreen.update

	For s:snake=EachIn snakes
		s.update
	Next
	
	For si:sounditem=EachIn sounditems
		si.update
	Next
	
	SetColor 255,255,255
	Local poly#[]
	x1#=unzoomx(0)
	y1#=unzoomy(0)
	x2#=unzoomx(gwidth)
	y2#=unzoomy(gheight)
	poly=[x1,y1,x2,y1,x2,y2,x1,y2]
	drawzoomtexturedpoly getpaper("brown"),panuv(poly)
	
	curscreen.draw

	For s:snake=EachIn snakes
		s.draw
	Next
	screenx=Floor(panx/gwidth+.5)
	screeny=Floor(pany/gheight+.5)
	dx=screenx-curscreen.sx
	dy=screeny-curscreen.sy
	curscreen=curscreen.neighbours(dx+1,dy+1)
'	DrawText dx+","+dy,0,0
	
	
	
	ms=MilliSecs()
	fps#=1000.0/(ms-oldms)
	'DrawText fps,700,0
	oldms=ms

	Flip
	Cls
	
	If KeyHit(KEY_ESCAPE) Or AppTerminate()
		done=1
	EndIf
Wend

Function dist#(x1#,y1#,x2#,y2#)
	dx#=x2-x1
	dy#=y2-y1
	Return Sqr(dx*dx+dy*dy)
End Function



Function DrawZoomCircle(x# , y# , radius#)
	x = zoomx(x) 
	y = zoomy(y)
	radius:* zoom
	DrawOval x - radius , y - radius , 2 * radius , 2 * radius
End Function

Function DrawZoomLine(ax# , ay# , bx# , by#)
	ax = zoomx(ax)
	ay = zoomy(ay)
	bx = zoomx(bx)
	by = zoomy(by)
	DrawLine ax,ay,bx,by
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

