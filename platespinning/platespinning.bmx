Const gravity#=.1
Const thrust#=.2
Const spinthrust#=1000
Const turnspeed#=6
Const plateslow#=.000005
Const wallbounce#=.5

Type ship
	Field x#,y#,vx#,vy#
	Field angle#
	Field myplate:plate,attachangle#
	Field spindir,jumpangle#
	Field thrusting
	
	Function create:ship(x#,y#)
		s:ship=New ship
		s.x=x
		s.y=y
		s.angle=-90
		ships.addlast s
		Return s
	End Function
	
	Method disconnect(jan#)
		v#=2*Pi*myplate.radius*Abs(myplate.van)/360
		vx=Cos(jan+90*Sgn(myplate.van))*v
		vy=Sin(jan+90*Sgn(myplate.van))*v
		x=myplate.x+Cos(jan)*myplate.radius+vx
		y=myplate.y+Sin(jan)*myplate.radius+vy
		myplate=Null
	End Method
	
	Method update()
	
		If myplate
			an#=attachangle+myplate.an
			x#=myplate.x+Cos(an)*myplate.radius
			y#=myplate.y+Sin(an)*myplate.radius
			angle=an+spindir*90
			
			myplate.van:+Cos(an)*gravity
			
			If Abs(jumpangle)<=180
				spoiler=-90*Sgn(myplate.van)
				diff#=correctangle(jumpangle-an+spoiler)
				
				If Abs(diff)<Abs(myplate.van)'And Sgn(myplate.van)=Sgn(Sin(jumpangle))
					disconnect(jumpangle+spoiler)
					Return
				EndIf
			EndIf
			If Abs(myplate.van)>30
				disconnect(myplate.an+attachangle)
				Return
			EndIf
			
		Else
			vx:*.999
			vy:*.999
			vy:+gravity
			x:+vx
			y:+vy

			If x<leftwall
				Print x
				x=leftwall
				vx=-vx*wallbounce
				If Abs(angle)>90
					angle=180-angle
				EndIf
			EndIf
			
			If x>rightwall
				x=rightwall
				vx=-vx*wallbounce
				If Abs(angle)<90
					angle=180-angle
				EndIf
			EndIf
			
			If y<topwall
				y=topwall
				vy=-vy*wallbounce
				If angle<0
					angle=-angle
				EndIf
			EndIf
			
			If y>bottomwall
				y=bottomwall
				vy=-vy*wallbounce
				If angle>0
					angle=-angle
				EndIf
			EndIf
			
			For p:plate=EachIn plates
				dx#=x-p.x
				dy#=y-p.y
				d#=dx*dx+dy*dy
				If d<p.radius*p.radius
					myplate=p
					an#=ATan2(dy,dx)
					attachangle#=an-p.an
					v#=Sqr(vx*vx+vy*vy)
					van#=ATan2(vy,vx)
					theta#=(van+an) Mod 360
					
					dotprod#=(vx*Cos(an+90)+vy*Sin(an+90))/v
					theta=ACos(dotprod)-90
					'DrawText theta,x,y
					'zoomline p.x,p.y,x,y
					'zoomline x,y,x+Cos(an+90)*15,y+Sin(an+90)*15
					'zoomline x,y,x+vx*5,y+vy*5
					'Flip
					'WaitKey()
					spindir=-Sgn(theta)
					p.van:+(360/(2*Pi))*Abs(v-Abs(2*Pi*p.radius*p.van/360))/p.radius*spindir
					jumpangle=181
				EndIf
			Next
		EndIf
		
		angle=correctangle(angle)
		'DrawText angle,x,y
		'DrawText attachangle,x,y+15
		
		If thrusting
			v#=Sqr(vx*vx+vy*vy)
			For n=0 To Rand(5)
				t#=Rnd(3,10)+v
				an#=Rnd(170,190)+180+angle
				svx#=Cos(an)*t
				svy#=Sin(an)*t
				spark.create(x-Cos(angle)*8,y+Sin(angle)*8,svx,svy,Rnd(10,30))
			Next
		EndIf
		thrusting=0
	End Method
	
	Method control()		
		If myplate
			If KeyDown(KEY_UP)
				myplate.spin(spindir*spinthrust/myplate.radius)
				thrusting=1
			EndIf
			
			v#=Abs(2*Pi*myplate.radius*myplate.van/360.0)*.1
			
			targzoom#=1000/(myplate.radius*(Abs(myplate.van)+10))
			zoom:+(targzoom-zoom)*.01
			
			leftscroll=myplate.x-5*myplate.radius
			rightscroll=myplate.x+5*myplate.radius
			topscroll=myplate.y-5*myplate.radius
			bottomscroll=myplate.y+5*myplate.radius

			If MouseHit(1)
				jumpangle#=ATan2(my-myplate.y,mx-myplate.x)
			EndIf
			If MouseHit(2)
				disconnect(myplate.an+attachangle)
			EndIf
			
		Else
			If KeyDown(KEY_UP)
				vx:+Cos(angle)*thrust
				vy:+Sin(angle)*thrust
				thrusting=1
			EndIf
			If KeyDown(KEY_LEFT)
				angle:-turnspeed
			EndIf
			If KeyDown(KEY_RIGHT)
				angle:+turnspeed
			EndIf
			
			v#=Sqr(vx*vx+vy*vy)
			targzoom#=1.0/(v*.05+1)
			zoom:+(targzoom-zoom)*.05
			
			leftscroll=leftwall
			rightscroll=rightwall
			topscroll=topwall
			bottomscroll=bottomwall
			


		EndIf

		DrawText String(leftscroll)+","+String(rightscroll)+","+String(topscroll)+","+String(rightscroll),0,0
		
		If zoomx(leftscroll)<0
			zx#=zoomx(x)
			If zx<200
				scrollx:+(zx-200)*.012*v
			EndIf
		EndIf
		If zoomx(rightscroll)>800
			zx#=zoomx(x)
			If zx>600
				scrollx:+(zx-600)*.012*v
			EndIf
		EndIf
	
		If zoomy(topscroll)<0
			zy#=zoomy(y)
			If zy<200
				scrolly:+(zy-200)*.012*v
			EndIf
		EndIf
		If zoomy(bottomscroll)>800
			zy#=zoomy(y)
			If zy>600
				scrolly:+(zy-600)*.012*v
			EndIf
		EndIf
	End Method
	
	Method draw()
		x1#=x+Cos(angle)*15
		y1#=y+Sin(angle)*15
		x2#=x+Cos(angle+120)*10
		y2#=y+Sin(angle+120)*10
		x3#=x+Cos(angle+240)*10
		y3#=y+Sin(angle+240)*10
		zoomline x1,y1,x2,y2
		zoomline x2,y2,x3,y3
		zoomline x3,y3,x1,y1
		zoompoly([x1,y1,x2,y2,x3,y3])
		
		If myplate
			an#=ATan2(my-myplate.y,mx-myplate.x)
			spoiler=-90*Sgn(myplate.van)
			sx#=myplate.x+Cos(an+spoiler)*myplate.radius
			sy#=myplate.y+Sin(an+spoiler)*myplate.radius
			zoomline sx,sy,sx+Cos(an)*myplate.radius,sy+Sin(an)*myplate.radius
		EndIf
	End Method
End Type

Type plate
	Field x#,y#
	Field radius#
	Field an#,van#
	
	Function create:plate(x#,y#,radius#)
		p:plate=New plate
		p.x=x
		p.y=y
		p.radius=radius
		plates.addlast p
		Return p
	End Function
	
	Method update()
		van:*(1-plateslow*Abs(van)*radius)
		an:+van
	End Method
	
	Method spin(amount#)
		van:+amount/radius
	End Method
	
	Method draw()
		zx#=zoomx(x)
		zy#=zoomy(y)
		If (zx<-radius*zoom Or zx>800+radius*zoom) Or (zy<-radius*zoom Or zy>800+radius*zoom)
			pan#=ATan2(zy-400,zx-400)
			dx#=x-dude.x
			dy#=y-dude.y
			d#=Sqr(dx*dx+dy*dy)
			tail#=d*.1*zoom
			If tail>100 tail=100
			SetLineWidth 3
			If pan>-45 And pan<135
				If pan<45
					py=Tan(pan)*400+400
					'DrawRect 780,py-10,20,20
					DrawLine 800,py,800+Cos(pan+180)*tail,py+Sin(pan+180)*tail
				Else
					px=400/Tan(pan)+400
					DrawLine px,800,px+Cos(pan+180)*tail,800+Sin(pan+180)*tail
				EndIf
			Else
				If Abs(pan)>135
					py=-Tan(pan)*400+400
					DrawLine 0,py,Cos(pan+180)*tail,py+Sin(pan+180)*tail
				Else
					px=-400/Tan(pan)+400
					DrawLine px,0,px+Cos(pan+180)*tail,Sin(pan+180)*tail
				EndIf
			EndIf
			SetLineWidth 1
		EndIf
			
		For n=0 To 5
			ox#=x+Cos(n*60+an)*radius
			oy#=y+Sin(n*60+an)*radius
			zoomline x,y,ox,oy
			For i=0 To 60 Step 10
				sx#=x+Cos(n*60+i+an)*radius
				sy#=y+Sin(n*60+i+an)*radius
				zoomline ox,oy,sx,sy
				SetAlpha .5
				fizz#=Abs(van)/10.0
				If fizz>1 fizz=1
				If Abs(van)>10
					gfizz#=(Abs(van)-10)/20.0
					If gfizz>1 gfizz=1
				EndIf
				SetColor 255*fizz,255*gfizz,0
				zoompoly([x,y,ox,oy,sx,sy])
				SetColor 255,255,255
				SetAlpha 1
				ox=sx
				oy=sy
			Next
		Next
		'DrawText an,x,y
		'DrawText van,x,y+15
	End Method
End Type

Type spark
	Field x#,y#,vx#,vy#,life#
	
	Function create:spark(x#,y#,vx#,vy#,life#)
		e:spark=New spark
		e.x=x
		e.y=y
		e.vx=vx
		e.vy=vy
		e.life=life
		sparks.addlast e
		Return e
	End Function
	
	Method update()
		vx:*.99
		vy:*.99
		life:-1
		If life<=0
			sparks.remove Self
		EndIf
	End Method
	
	Method draw()
		SetColor 255,200+Rnd(-55,55),0
		zoomrect x+Rand(-3,3),y+Rand(-3,3),1,1
		SetColor 255,255,255
	End Method
End Type

Type level
	Field pieces:tlist
	Field topwall,bottomwall,leftwall,rightwall
	
	Function create:level(t,b,lf,r)
		l:level=New level
		l.pieces=New tlist
		l.topwall=t
		l.bottomwall=b
		l.leftwall=lf
		l.rightwall=r
		levels.addlast l
		Return l
	End Function
	
	Method addpiece:levelpiece()
		piece:levelpiece=New levelpiece
		pieces.addlast(piece)
		Return piece
	End Method
End Type

Type levelpiece
	Field x#,y#,radius#
End Type

Function correctangle(angle#)
	angle=angle Mod 360
	If angle>180 angle:-360
	If angle<-180 angle:+360
	Return angle
End Function

Function dezoomx#(x#)
	Return (x-400)/zoom+scrollx
End Function

Function dezoomy#(y#)
	Return (y-400)/zoom+scrolly
End Function

Function zoomx#(x#)
	Return (x-scrollx)*zoom+400
End Function

Function zoomy#(y#)
	Return (y-scrolly)*zoom+400
End Function

Function zoomline(x1#,y1#,x2#,y2#)
	x1=zoomx(x1)
	y1=zoomy(y1)
	x2=zoomx(x2)
	y2=zoomy(y2)
	DrawLine x1,y1,x2,y2
End Function

Function zoomrect(x#,y#,width#,height#)
	x=zoomx(x)
	y=zoomy(y)
	DrawRect x,y,width*zoom,height*zoom
End Function

Function zoompoly(points#[])
	Local poly#[]=New Float[Len(points)]
	For n=0 To Len(points)/2-1
		poly[n*2]=zoomx(points[n*2])
		poly[n*2+1]=zoomy(points[n*2+1])
	Next
	DrawPoly poly
End Function

Function editor()
	'topwall=l.topwall
	'bottomwall=l.bottomwall
	'leftwall=l.leftwall
	'rightwall=l.rightwall
	'For piece:levelpiece=EachIn l.pieces
	'	plate.create(piece.x,piece.y,piece.radius)
	'Next
	
	quit=0
	mode=0
	zoom=1
	scrollx=0
	scrolly=0
	
	'plates=New tlist
	curplate:plate=Null
	
	While Not quit
	
		DrawText "Left-click to make a plate, grab things to move them, press S to save, escape to quit back to game.",0,0
	
		For p:plate=EachIn plates
			p.draw()
		Next

		mx=MouseX()
		my=MouseY()
		zmx=dezoomx(mx)
		zmy=dezoomy(my)
		
		zoomline leftwall,topwall,leftwall,bottomwall
		zoomline rightwall,topwall,rightwall,bottomwall
		zoomline leftwall,topwall,rightwall,topwall
		zoomline leftwall,bottomwall,rightwall,bottomwall
		
		If mx<40
			scrollx:+(mx-40)*.15/zoom
		EndIf
		If mx>760
			scrollx:+(mx-760)*.15/zoom
		EndIf
		If my<40
			scrolly:+(my-40)*.15/zoom
		EndIf
		If my>760
			scrolly:+(my-760)*.15/zoom
		EndIf
		
		DrawText mode,0,15
		
		zoomrect -3,-3,6,6
		
		Select mode
		Case 0 'nothin' doing
			If MouseHit(1)
				mindist=-1
				For p:plate=EachIn plates
					dx#=zmx-p.x
					dy#=zmy-p.y
					d#=Sqr(dx*dx+dy*dy)
					If d<p.radius+10 And (p.radius+10-d<mindist Or mindist=-1)
						curplate=p
						mindist=p.radius-d
					EndIf
				Next
				If curplate
					If mindist>20
						mode=2
						offx=curplate.x-zmx
						offy=curplate.y-zmy
					Else
						mode=3
					EndIf
				Else
					If zmx>=leftwall And zmx<=rightwall
						If Abs(zmy-topwall)<10
							mode=5
						ElseIf Abs(zmy-bottomwall)<10
							mode=6
						EndIf
					EndIf
					If zmy>=topwall And zmy<=bottomwall
						If Abs(zmx-leftwall)<10
							mode=7
						ElseIf Abs(zmx-rightwall)<10
							mode=8
						EndIf
					EndIf
				EndIf
				
				If Not mode
					curplate=plate.create(zmx,zmy,80)
					mode=1
					offx=0
					offy=0
				EndIf
			EndIf
			
			If MouseHit(2)
				closest:plate=Null
				mindist=-1
				For p:plate=EachIn plates
					dx#=zmx-p.x
					dy#=zmy-p.y
					d#=dx*dx+dy*dy
					If d<p.radius*p.radius And (d<mindist Or mindist=-1)
						closest=p
						mindist=d
					EndIf
				Next
				If closest
					plates.remove closest
				EndIf
			EndIf
		Case 1,2 'plate movement
			curplate.x=zmx+offx
			curplate.y=zmy+offy
			If (mode=1 And MouseHit(1)) Or (mode=2 And Not MouseDown(1))
				curplate=Null
				mode=0
			EndIf
		Case 3 'resize plate
			dx#=zmx-curplate.x
			dy#=zmy-curplate.y
			d#=Sqr(dx*dx+dy*dy)
			If d>200 d=200
			curplate.radius=d
			If Not MouseDown(1)
				curplate=Null
				mode=0
			EndIf
		Case 5 'move topwall
			topwall=zmy
			If Not MouseDown(1) mode=0
		Case 6 'move bottomwall
			bottomwall=zmy
			If Not MouseDown(1) mode=0
		Case 7 'move leftwall
			leftwall=zmx
			If Not MouseDown(1) mode=0
		Case 8 'move rightwall
			rightwall=zmx
			If Not MouseDown(1) mode=0
			
		Default
			mode=0
			curplate=Null
		End Select
		Flip
		Cls
		
		If KeyHit(KEY_ESCAPE)
			quit=1
		ElseIf KeyHit(KEY_S)
			editorsave()
		EndIf
	Wend
End Function

Function editorsave()
	Print "saved"
	f:tstream=OpenFile("levels.txt",False,True)
	WriteLine f,"l"
	WriteLine f,topwall
	WriteLine f,bottomwall
	WriteLine f,leftwall
	WriteLine f,rightwall
	For p:plate=EachIn plates
		WriteLine f,p.x
		WriteLine f,p.y
		WriteLine f,p.radius
	Next
	CloseStream f
End Function

Graphics 800,800,0
SetBlend ALPHABLEND

Global levels:tlist=New tlist

Global dude:ship
Global mx,my
Global scrollx#=0,scrolly#=0,zoom#=1
Global topwall,bottomwall,rightwall,leftwall

'editor()

f:tstream=OpenFile("levels.txt")
curlevel:level=Null
curpiece:levelpiece=Null
While Not Eof(f)
	line$=ReadLine(f)
	If line="l"
		t=Int(ReadLine(f))
		b=Int(ReadLine(f))
		lf=Int(ReadLine(f))
		r=Int(ReadLine(f))
		curlevel=level.create(t,b,lf,r)
		state=0
	Else
		Select state
		Case 0
			curpiece=curlevel.addpiece()
			curpiece.x=Int(line)
		Case 1
			curpiece.y=Int(line)
		Case 2
			curpiece.radius=Int(line)
			state=-1
		End Select
		state:+1
	EndIf
Wend
CloseFile f

levelno=1

Global ships:tlist
Global plates:tlist
Global sparks:tlist
quit=0
While Not quit
	
	ships=New tlist
	plates=New tlist
	sparks=New tlist
	
	dude:ship=ship.create(0,0)
	
	l:level=level(levels.valueatindex(levelno))
	topwall=l.topwall
	bottomwall=l.bottomwall
	leftwall=l.leftwall
	rightwall=l.rightwall
	For piece:levelpiece=EachIn l.pieces
		plate.create(piece.x,piece.y,piece.radius)
	Next
	
	scrollx=0
	scrolly=0
	zoom=.5
	
	While Not done
	
		If KeyHit(KEY_E)
			editor()
			done=1
			levelno=-1
		EndIf
	
		If completed
			SetScale 2,2
			DrawText "LEVEL COMPLETED! PRESS A BUTTON",200,400
			SetScale 1,1
			If MouseHit(1) Or MouseHit(2) Or GetChar()
				done=1
			EndIf
		EndIf
						
		mx=dezoomx(MouseX())
		my=dezoomy(MouseY())
		
		dude.control()
		
		zoomline leftwall,topwall,leftwall,bottomwall
		zoomline rightwall,topwall,rightwall,bottomwall
		zoomline leftwall,topwall,rightwall,topwall
		zoomline leftwall,bottomwall,rightwall,bottomwall
		
		failed=0
		For p:plate=EachIn plates
			p.update()
			p.draw()
			If Abs(p.van)<10 failed:+1
		Next
		If failed=0 
			completed=1
			FlushKeys()
			FlushMouse()
		EndIf
		
		For e:spark=EachIn sparks
			e.update()
			e.draw()
		Next
		
		For s:ship=EachIn ships
			s.update()
			s.draw()
		Next
		
		Flip
		Cls
		
		If KeyHit(KEY_ESCAPE)
			done=1
			quit=1
		EndIf
	Wend
	
	levelno:+1
	Print levelno
	done=0
	completed=0
	If levelno=levels.count() quit=1
Wend