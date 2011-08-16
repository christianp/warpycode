Incbin "clop1.ogg"
Incbin "clop2.ogg"
Incbin "clop3.ogg"
Incbin "neigh.ogg"
Incbin "horse.png"
Incbin "bg.png"
Incbin "fg.png"
Incbin "bg.ogg"

Global panx#,pany#

Function drawzoomimage(i:TImage,x#,y#)
	DrawImage i,x-panx+300,y-pany+350
End Function

Type horse
	Field foot,trot,dir,clop
	Field speed#,steps,laststop
	Field x#,y#
	Field img:TImage
	Global clops:TSound[]=[LoadSound("incbin::clop1.ogg"),LoadSound("incbin::clop2.ogg"),LoadSound("incbin::clop3.ogg")]
	Global neigh:TSound=LoadSound("incbin::neigh.ogg")
	Global bgsound:TSound=LoadSound("incbin::bg.ogg"),bgchannel:TChannel
	Field neighing:TChannel
	
	Method New()
		trot=ms
		img=LoadImage("incbin::horse.png")
		dir=1
	End Method
	
	Method update()
		speed=(KeyDown(KEY_RIGHT)-KeyDown(KEY_LEFT))*10
		If speed<>0
			dir=Sgn(speed)
			If ms>trot
				If neighing
					SetChannelVolume neighing,1-steps*.2
					If steps>=5
						StopChannel neighing
						neighing=Null
					EndIf
				EndIf
				trot:+100
				x:+speed
				foot=(foot Mod 3)+1
				steps:+1
				If foot=1
					PlaySound clops[clop]
					clop=(clop Mod 2)+1
				EndIf
			EndIf
			SetChannelVolume bgchannel,0
			laststop=ms
		Else
			If ms>trot
				trot=ms
				foot=0
				clop=0
				If steps>10 And (Not neighing) And Rand(150)<steps
					neighing=PlaySound(neigh)
				EndIf
				steps=0
			EndIf
			
			If Not (bgchannel And ChannelPlaying(bgchannel))
				bgchannel=PlaySound(bgsound)
			EndIf
			
			If ms-laststop<2000
				SetChannelVolume bgchannel,(ms-laststop)/2000.0
			Else
				SetChannelVolume bgchannel,1
			EndIf
		EndIf
	End Method
	
	Method draw()
		Local xoff#,yoff#
		xoff=-20
		Select foot
		Case 0
			yoff=-33
			SetRotation 0
		Case 1
			xoff:-10
			yoff=-Cos(20)*33
			SetRotation -20
		Case 3
			xoff:+10
			yoff=-Cos(20)*33-Sin(20)*40
			SetRotation 20
		Case 2
			yoff=-35
			SetRotation 0
		End Select

		SetScale .2,.2
		DrawZoomImage img,x+xoff,y+yoff
		'DrawRect x+xoff,y+yoff,40,15

		SetRotation 0
		SetScale 1,1
		'DrawText speed,0,0
		'DrawText steps,0,15
	End Method
End Type


AppTitle="I love horses!"
Graphics 600,600,0
SetBlend ALPHABLEND

bg:TImage=LoadImage("incbin::bg.png")
fg:TImage=LoadImage("incbin::fg.png")

h:horse=New horse
'h.x=100
'h.y=360
Global ms

Global vpanx#,vpany#

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	ms=MilliSecs()
	
	h.update
	
	
	dpan#=(h.x-panx)*.01
	vpanx:+dpan+Rnd(-.1,.1)
	vpany:+Rnd(-1,1)*Abs(dpan)*.2+Rnd(-.1,.1)
	vpanx:*.8
	vpany:*.8
	panx:+vpanx
	pany:+vpany
	
	scroll#=(-panx) Mod 600
	DrawImage bg,scroll,-pany-25
	DrawImage bg,scroll-600,-pany-25
	DrawImage bg,scroll+600,-pany-25
	
	h.draw
	
	DrawImage fg,scroll,-pany-25
	DrawImage fg,scroll-600,-pany-25
	DrawImage fg,scroll+600,-pany-25
	
	SetRotation 0

	Flip
	Cls
Wend
