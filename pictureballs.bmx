Const springforce#=15,friction#=.9
Global blots:tlist=New tlist

Type blot
	Field x#,y#,vx#,vy#
	Field r#,maxr#
	Field startx#,starty#
	
	Function create:blot(x#,y#,r#)
		b:blot=New blot
		b.x=x
		b.y=y
		b.startx=b.x
		b.starty=b.y
		b.maxr=r
		b.r=b.maxr
		blots.addlast b
		Return b
	End Function
	
	Method move()
		ox#=x
		oy#=y
		x:+vx
		y:+vy
		vx:*friction
		vy:*friction
		'vy:+.01
		
		If x<0 Or x>799
			vx=-vx
			x=ox
			
		EndIf
		
		If y<0 Or y>799
			vy=-vy
			y=oy
		EndIf
		
		r:+(maxr-r)*.5
	End Method
	
	Method collide()
		For b:blot=EachIn blots
			If b<>Self
				dx#=b.x-x
				dy#=b.y-y
				d#=dx*dx+dy*dy
				If d<25 d=25
				tr#=r+b.r
				If d<tr*tr
					d=Sqr(d)
					r=d*r/tr
					b.r=d*b.r/tr
					f#=springforce/d
					dx:/d
					dy:/d
					vx:-f*dx'/r
					vy:-f*dy'/r
					b.vx:+f*dx/b.r
					b.vy:+f*dy/b.r
				EndIf
			EndIf
		Next
		
	End Method
	
	Method draw()
		If x>0 And x<799 And y>0 And y<799
			dx#=x-startx
			dy#=y-starty
			d#=dx*dx+dy*dy
			If d<50 d=50
			light#=50/Sqr(d)
			rgb=pix.readpixel(x,y)
			red=(rgb Shr 16) & 255
			green=(rgb Shr 8) & 255
			blue=rgb & 255
			SetAlpha light*.5+.5
			SetColor red,green,blue
			DrawOval x-r,y-r,r*2,r*2
		EndIf
	End Method
End Type

Graphics 800,800,0
SeedRnd MilliSecs()
SetBlend ALPHABLEND
Global pix:tpixmap=LoadPixmap("nicepattern.jpg")

p#=0.5
time=MilliSecs()

While Not KeyHit(KEY_ESCAPE)
	mx=MouseX()
	my=MouseY()
	

	otime=time
	time=MilliSecs()
	fps#=1000.0/(time-otime)

	If MouseHit(1)
		blot.create(mx,my,Rand(50,150))
	EndIf
	
	If fps>40
		If Rnd(0,1)<p
			'p:+.001
			blot.create(Rand(800),Rand(800),100)'Rand(50,150))
		EndIf
	EndIf
	
	totr#=0
	
	'SetAlpha .5
	For b:blot=EachIn blots
		b.move()
	'Next
	'For b:blot=EachIn blots
		b.collide()
	'Next
	'For b:blot=EachIn blots
		b.draw()
		totr:+b.r
	Next
	SetAlpha 1
	
	SetColor 255,255,255
	'DrawText Int(fps),0,0
	blotscount=blots.count()
	totalpix#=800*800
	'DrawText Sqr(blotscount/totalpix),0,15
	'DrawText blotscount,0,30
	'DrawText totr/blotscount,0,45

	Flip
	Cls
Wend