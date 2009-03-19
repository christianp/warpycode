Type moebius
	Field w,h
	Field image:TImage
	
	Function create:moebius(w,h)
		m:moebius=New moebius
		m.w=w
		m.h=h
		m.image=CreateImage(m.w,m.h)
		Return m
	End Function
		
	Method fixpos(x Var,y Var)
		multiples=x/w & 1
		x=x Mod w
		y=y Mod h
		If x<0
			x:+w
			multiples:*-1
		EndIf
		If y<0 y:+h
		If multiples y=(h-y) Mod h
	End Method
	
	Method setpixel(x,y,rgb)
		p:TPixmap=LockImage(image)
		fixpos(x,y)
		p.writepixel(x,y,rgb | (255 Shl 24))
		UnlockImage image
	End Method
	
End Type

Graphics 580,450,0

Const xsize=410,ysize=270
m:moebius=moebius.create(xsize,ysize)
white=255
bgimg=LoadImage("etchasketch.jpg")

scrollx=0
scrolly=0

time=MilliSecs()
While Not KeyHit(KEY_ESCAPE)
	SetScale 1,1
	DrawImage bgimg,0,0

	mx=MouseX()
	my=MouseY()
	
	scrollx:+(KeyDown(KEY_LEFT)-KeyDown(KEY_RIGHT))*2
	scrolly:+(KeyDown(KEY_UP)-KeyDown(KEY_DOWN))*2*flipdir
	dscrollx=scrollx Mod m.w
	If dscrollx<0 dscrollx=m.w+dscrollx
	dscrolly=scrolly Mod m.h
	If dscrolly<0 dscrolly=m.h+dscrolly
	flippo=(scrollx/m.w & 1)
	flipdir=1-flippo*2
	If scrollx<0 flipdir:*-1
	If KeyDown(KEY_SPACE)
		x=xsize/2-scrollx
		y=ysize/2-scrolly
		If dscrollx>xsize/2
			y=ysize-y
			DrawRect 0,0,50,50
		EndIf
		m.setpixel(x,y,white)
	EndIf
	
	SetColor 255,255,255
	DrawLine 80,dscrolly+80,490,dscrolly+80
	DrawLine dscrollx+80,80,dscrollx+80,340
	DrawText dscrollx,0,0
	DrawText dscrolly,0,15
	
	'SetViewport dscrollx+80,dscrolly+80,490,340
	DrawImage m.image,dscrollx+80,dscrolly+80
	DrawImage m.image,dscrollx+80,dscrolly+80-ysize
	SetScale 1,-1
	DrawImage m.image,dscrollx+80-m.w,m.h+dscrolly+80
	DrawImage m.image,dscrollx+80-m.w,m.h+dscrolly+80-ysize
	'SetViewport 0,0,580,450
	
	SetScale 1,1
	DrawText flippo,0,30

	otime=time
	time=MilliSecs()
	fps#=1000.0/(time-otime)
	DrawText fps,0,50
	Flip
	Cls
Wend
