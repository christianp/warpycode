Global balls:TList=New TList
Global tracks:TList=New TList
Global mx,my,vmx,vmy,zmx#,zmy#,mz#,vmz#,omx,omy,omz

Const bounce#=.8
Global panx#,pany#,zoom#=1

Incbin "TheBananaSplitsTitle(TheTraLaLaSong).ogg"
themesong:TSound=LoadSound("incbin::TheBananaSplitsTitle(TheTraLaLaSong).ogg")

Const gwidth=500,gheight=500

AppTitle="Brass monkey, that funky monkey!"
Graphics gwidth,gheight,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()
HideMouse

Type monkey
	Field x#,y#
	Field vx#,vy#
	Field grab:ball,graban#,hugan#
	Field van#,an#
	Field otaillength#
	Field taillength#,anchorx#,anchory#
	Field grabx#,graby#,mgrabx#,mgraby#
	Field wraps,oschwing#,winddir
	Field tailx#,taily#,tailvx#,tailvy#,shoottail
	Field leftover#
	
	Method update()
		ox#=x
		oy#=y
		If grab
			van:-Sin(an-90)*10/taillength
			'van:*.99
			an:+van
			anchorx#=grab.x+Cos(hugan)*grab.r
			anchory#=grab.y+Sin(hugan)*grab.r
			ox#=x
			oy#=y
			x#=anchorx+Cos(an)*taillength
			y#=anchory+Sin(an)*taillength
			vx=x-ox
			vy=y-oy

			For b:ball=EachIn balls
				If b<>grab
					dx#=b.x-x
					dy#=b.y-y
					d#=dx*dx+dy*dy
					If d<b.r*b.r
						van=-van*bounce
						an:+van
						Return
					EndIf
				EndIf
			Next

			swingan#=ATan2(y-anchory,x-anchorx)
			dan#=andiff(swingan,hugan)
				mgraban=graban+180*wraps
				mgrabx=grab.x+grab.r*Cos(mgraban)
				mgraby=grab.y+grab.r*Sin(mgraban)
				ohugan#=hugan
				dx#=x-grab.x
				dy#=y-grab.y
				d#=Sqr(dx*dx+dy*dy)
				man#=ATan2(dy,dx)
				If d<grab.r d=grab.r
				nhugan#=man-Sgn(dan)*ACos(grab.r/d)
				diff4#=andiff(swingan,graban)

				schwingan#=ATan2(y-graby,x-grabx)
				schwing#=Cos(schwingan+graban+90)
				schwingdiff#=andiff(ATan2(y-mgraby,x-mgrabx),mgraban)
				
				diff#=andiff(mgraban,nhugan)
				diff2#=andiff(ohugan,nhugan)

				l#=2*Pi*diff*(-winddir)*grab.r/360.0+Abs(wraps)*Pi*grab.r

				Rem
				DrawText "wraps: "+String(wraps),0,15
				DrawText "schwing: "+String(schwing),0,0
				DrawText "winddir: "+String(winddir),0,30
				DrawText "diff4: "+String(diff4),0,45
				DrawText "schwingdiff: "+String(schwingdiff),0,60
				DrawText "diff: "+String(diff),0,75
				DrawText "hugan: "+String(hugan),0,120
				DrawText "van: "+String(van),0,150
				DrawText "l: "+String(l),0,90
				DrawText "l+: "+String(l+Abs(wraps*Pi*grab.r)),0,135
				DrawText "otaillength: "+String(otaillength),0,105
				DrawText "taillength: "+String(taillength),0,165

				DrawLine x,y,grab.x+grab.r*Cos(mgraban),grab.y+grab.r*Sin(mgraban)
				SetColor 0,0,255
				DrawLine x,y,grab.x+grab.r*Cos(nhugan),grab.y+grab.r*Sin(nhugan)
				SetColor 255,255,255
				EndRem
				
			If Abs(diff4)<90 And wraps=0
				'an#=graban+diff4
				hugan=graban
				van:*taillength/otaillength
				taillength=otaillength
				
				If wraps=0 
					winddir=Sgn(van)
				ElseIf (winddir=1 And schwingdiff<0) Or (winddir=-1 And schwingdiff>0)
					'Print "??"
					wraps:+Sgn(van)
				EndIf
				oschwing=schwing
				
			Else'If Abs(dan)>90
				If l>otaillength
					van=-van*bounce
					If Abs(van)<.2
						x=grab.x+(x-grab.x)*1.01
						y=grab.y+(y-grab.y)*1.01
						detach()
						Return
					EndIf
					'Print ">>>"
					'Print l
					'Print diff
					Return
				EndIf
				If (winddir=1 And schwingdiff<0) Or (winddir=-1 And schwingdiff>0)
					Print "??"
					wraps:+Sgn(van)
				EndIf
				'dx#=x-grab.x+grab.r*Cos(hugan)
				'dy#=y-grab.y+grab.r*Sin(hugan)
				'an#=hugan-ATan2(dy,dx)
				ptaillength=taillength
				taillength=otaillength-l
				If taillength<0 
					'Print "OOK"
					'Print van
					'Print taillength
					'Print l
					'Print diff
					'Print nhugan
					'Print d
					'Print 100.0/d
					'Print grab.r
					detach()
					Return
				EndIf
				van:*ptaillength/taillength
				x1#=grab.x+grab.r*Cos(nhugan)
				y1#=grab.y+grab.r*Sin(nhugan)
				dx#=x-x1
				dy#=y-y1
				an#=ATan2(dy,dx)
				
				Rem
				dx#=x-grab.x
				dy#=y-grab.y
				d#=Sqr(dx*dx+dy*dy)
				If d<grab.r
					x=grab.x+dx*grab.r/d
					y=grab.y+dy*grab.r/d
					detach()
					Print "!!!!!"
					'DrawRect x,y,5,5
					'Flip
					'Delay 1000
					Return
				EndIf
				EndRem
				hugan=nhugan
				x1#=grab.x+grab.r*Cos(nhugan)
				y1#=grab.y+grab.r*Sin(nhugan)
				'm.draw()
				'Flip
				'Delay 100000			
			EndIf	
			olddiff=diff
			diff1#=andiff(hugan,graban)
			
		Else
			v#=Sqr(vx*vx+vy*vy)
			If v>50
				vx=0
				vy=0
			EndIf
			
			For b:ball=EachIn balls
				dx#=x-b.x
				dy#=y-b.y
				d#=dx*dx+dy*dy
				If d<b.r*b.r
					an#=ATan2(dy,dx)
					c#=Cos(an)
					s#=Sin(an)
					dotprod#=vx*c+vy*s
					nvx#=vx-2*c*dotprod*bounce
					nvy#=vy-2*s*dotprod*bounce
					vx=nvx
					vy=nvy
					x=b.x+c*(b.r+1)
					y=b.y+s*(b.r+1)
				EndIf
			Next
			
			vy:+.1

			x:+vx
			y:+vy
			'If y>800
			'	y=800
			'	vy=-vy*bounce
			'EndIf
		EndIf
		
		dx=x-ox
		dy=y-oy
		d#=Sqr(dx*dx+dy*dy)
		dx:/d
		dy:/d
		ft#=-leftover
		n=0
		While ft<d
			ft:+30
			If ft<d
				n:+1
				px#=ox+dx*ft
				py#=oy+dy*ft
				t:track=track.create(px,py)
				If grab t.life=1
			EndIf
		Wend
		leftover=30-(ft-d)
			
			
	End Method
	
	Method detach()
		mx=zoomx(grab.x)
		my=zoomy(grab.y)
		omx=mx
		omy=my
		grab=Null
		v#=2*Pi*van*taillength/360.0
		vx=-Sin(an)*v
		vy=Cos(an)*v
		van=0
		wraps=0
	End Method
	
	Method control()
		If grab 
			van:-(vmx*.03+(KeyDown(KEY_RIGHT)-KeyDown(KEY_LEFT)))*5/otaillength
			ptaillength=otaillength
			otaillength:-vmz*.5+(KeyDown(KEY_UP)-KeyDown(KEY_DOWN))*3
			van:*ptaillength/otaillength
		ElseIf shoottail
			tailx:+tailvx
			taily:+tailvy
			dx#=tailx-x
			dy#=taily-y
			d#=Sqr(dx*dx+dy*dy)
			If d>600
				shoottail=0
				Return
			EndIf
			closest:ball=Null
			mindist=-1
			For b:ball=EachIn balls
				dx#=b.x-tailx
				dy#=b.y-taily
				d#=Sqr(dx*dx+dy*dy)
				diff#=b.r-d
				If d<=b.r And (diff<mindist Or mindist=-1)
					closest=b
					mindist=diff
				EndIf
			Next
			If closest
				attach(closest)
				shoottail=0
			EndIf
		EndIf
		If MouseHit(2)
			If Not grab
				vy:-1
			EndIf
		EndIf
		If KeyDown(KEY_SPACE) Delay 500
		If MouseHit(1)
			If grab
				detach()
			Else
				shoottail=1
				dx#=zmx-x
				dy#=zmy-y
				d#=Sqr(dx*dx+dy*dy)
				tailvx=dx*30/d
				tailvy=dy*30/d
				tailx=x
				taily=y
			EndIf
		EndIf
	End Method
	
	Method attach(b:ball)
			grab=b
			graban#=ATan2(y-b.y,x-b.x)
			an=graban
			hugan=graban
			dx#=grab.x-x
			dy#=grab.y-y
			d#=Sqr(dx*dx+dy*dy)
			grabx=grab.x-dx*grab.r/d
			graby=grab.y-dy*grab.r/d
			otaillength=d-grab.r
			taillength=otaillength
			van=360*(-vx*Sin(an)+vy*Cos(an))/(2*Pi*taillength)
			
			For t:track=EachIn tracks
				t.life:+1
			Next
	End Method
	
	Method draw()
		SetColor 255,255,255
		If Not grab
			DrawzoomLine zmx-5,zmy,zmx+5,zmy
			DrawzoomLine zmx,zmy-5,zmx,zmy+5
		EndIf
		Drawzoomcircle x,y,5
		If grab
			If winddir=0 Return
			SetLineWidth 3
			DrawzoomLine x,y,anchorx,anchory
			diff#=andiff(graban+180*wraps,hugan)
			windan#=0
			ox#=grabx
			oy#=graby
			pootle=Abs(wraps)
			If diff*winddir>0 pootle:-1
			For c=1 To pootle
				For n=1 To 36
					windan:+Sgn(winddir)*5
					px#=grab.x+grab.r*Cos(graban+windan)
					py#=grab.y+grab.r*Sin(graban+windan)
					DrawzoomLine ox,oy,px,py
					ox=px
					oy=py
				Next
			Next
			While andiff(hugan,windan+graban)*winddir>0
				pan#=windan+graban
				windan:+Sgn(winddir)*5
				px#=grab.x+grab.r*Cos(pan)'-180*wraps)
				py#=grab.y+grab.r*Sin(pan)'-180*wraps)
				DrawzoomLine ox,oy,px,py
				ox=px
				oy=py
			Wend
			SetLineWidth 1
		Else
			If shoottail
				SetLineWidth 3
				DrawzoomLine x,y,tailx,taily
				SetLineWidth 1
			EndIf
		EndIf
	End Method
End Type	

Type ball
	Field x#,y#,r#
	Field red,green,blue
	
	Method New()
		balls.addlast Self
		n=Rand(1,6)
		red=(n & 1)*255
		green=(n & 2)*255
		blue=(n & 4)*255
	End Method
	
	Function create:ball(x#,y#,r#)
		b:ball=New ball
		b.x=x
		b.y=y
		b.r=r
		Return b
	End Function
	
	Method draw()
		zx=zoomx(x)
		zy=zoomy(y)
		If zx<-r Or zx>gwidth+r Or zy<-r Or zy>gheight+r Return
		SetColor red,green,blue
		SetAlpha .5
		Drawzoomcircle x,y,r
		
		SetAlpha 1
		SetLineWidth 3
		segments#=r/2
		'If segments<5 segments=5
		'If segments>100 segments=100
		stp#=360.0/segments
		an#=0
		ox#=x+r
		oy#=y
		While an<360
			an:+stp
			px#=x+Cos(an)*r
			py#=y+Sin(an)*r
			DrawzoomLine ox,oy,px,py
			ox=px
			oy=py
		Wend
		SetLineWidth 1
	End Method
End Type

Type track
	Field x#,y#,life
	
	Method New()
		tracks.addlast Self
	End Method
	
	Function create:track(x#,y#)
		t:track=New track
		t.x=x
		t.y=y
		Return t
	End Function
	
	Method update()
		If life
			life:+1
			If life=50 tracks.remove Self
		EndIf
	End Method
	
	Method draw()
		SetColor 255,255,255
		SetAlpha 1-life/50.0
		drawzoomcircle(x,y,3)
		SetAlpha 1
	End Method
End Type

Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

Function zoomx#(x#)
	Return (x-panx)*zoom+gwidth/2
End Function

Function zoomy#(y#)
	Return (y-pany)*zoom+gheight/2
End Function

Function dezoomx#(x#)
	Return (x-gwidth/2)/zoom+panx
End Function

Function dezoomy#(y#)
	Return (y-gheight/2)/zoom+pany
End Function

Function drawzoomline(x1#,y1#,x2#,y2#)
	x1=zoomx(x1)
	y1=zoomy(y1)
	x2=zoomx(x2)
	y2=zoomy(y2)
	DrawLine x1,y1,x2,y2
End Function

Function drawzoomcircle(x#,y#,w#)
	x=zoomx(x)
	y=zoomy(y)
	w:*zoom
	DrawOval x-w,y-w,w*2,w*2
End Function

Function drawzoompoly(poly#[])
	For n=0 To Len(poly)-1 Step 2
		poly[n]=zoomx(poly[n])
		poly[n+1]=zoomy(poly[n+1])
	Next
	DrawPoly poly
End Function


Global m:monkey=New monkey
m.x=30
m.y=-300

For c=1 To 500
	ball.create(Rand(-10000,10000),Rand(-20000,0),Rand(50,100))
Next
For c=1 To 5
	ball.create(Rand(-2000,2000),Rand(-2000,-400),Rand(50,100))
Next
ball.create(Rand(-gwidth/2,gwidth/2),-500,Rand(50,100))

Global timer=0

ch:TChannel=PlaySound(themesong)

mx=gwidth/2
my=gheight/2
While Not KeyHit(KEY_ESCAPE)
	If Not ChannelPlaying(ch)
		ch=PlaySound(themesong)
	EndIf
	timer:+1
	omx=mx
	omy=my
	omz=mz
	mx:+MouseX()-gwidth/2
	my:+MouseY()-gheight/2
	mz=MouseZ()
	MoveMouse gwidth/2,gheight/2
	zmx#=dezoomx(mx)
	zmy#=dezoomy(my)
	vmx=mx-omx
	vmy=my-omy
	DrawText mx,0,0
	DrawText my,0,15
	vmz:+mz-omz
	vmz:*.8
	
	v#=Sqr(m.vx*m.vx+m.vy*m.vy)
	dx#=m.x-panx
	dy#=m.y-pany-gheight/4
	panx:+dx*.9
	pany:+dy*.9
	If pany>-gheight/2 pany=-gheight/2
	tzoom#=(gheight/800.0)*5.0/v
	'If tzoom<1 tzoom=1
	If tzoom<.1 tzoom=.1
	If tzoom>1 tzoom=1
	zoom:+(tzoom-zoom)*.1
	
	For t:track=EachIn tracks
		t.update()
		t.draw()
	Next
	
	For b:ball=EachIn balls
		'b.update()
		b.draw()
	Next
	
	m.update()
	m.control()
	m.draw()
	
	SetColor 0,0,255
	SetAlpha .4
	ox#=panx-(gwidth/2)/zoom
	oy#=Sin(timer*7)*50'+400
	If pany>-500/zoom
		For x#=0 To (gwidth+100)/zoom Step 10
			px#=x+panx-(gwidth/2)/zoom
			y#=Sin(timer*7+x)*10'+400
			drawzoompoly([ox,oy,px,y,px,dezoomy(gheight),ox,dezoomy(gheight)])
			ox=px
			oy=y
		Next
	EndIf
	SetAlpha 1
	Flip
	Cls
Wend