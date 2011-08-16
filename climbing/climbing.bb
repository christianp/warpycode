Graphics 640,480,32,2
SetBuffer BackBuffer()

Type climber
	Field name$,id$
	Field lhx,lhy,lhxp#,lhyp#
	Field rhx,rhy,rhxp#,rhyp#
	Field llx,lly,llxp#,llyp#
	Field rlx,rly,rlxp#,rlyp#
	Field midx#,midy#,midxp#,midyp#
	Field hipx#,hipy#,hipxp#,hipyp#
	Field lsxp#,lsyp#,rsxp#,rsyp#
	Field an#
End Type

Global server
Global rare=2
Global numholds=4
Global xsize=32
Global ysize=24
Global xscale#=640.0/xsize
Global yscale#=480.0/ysize
Dim wall(xsize,ysize)


Global wallimg=LoadImage("rock114.bmp")
Global bodyimg=LoadImage("body.bmp")
HandleImage bodyimg,15,15
Global headimg=LoadImage("head.bmp")
HandleImage headimg,15,15
Global hipimg=LoadImage("hips.bmp")
Global uparm=LoadImage("uparm.bmp")
Global lowarm=LoadImage("lowarm.bmp")
Global handimg=LoadImage("hand.bmp")
HandleImage handimg,7,7
Global upleg=LoadImage("upleg.bmp")
Global lowleg=LoadImage("upleg.bmp")

Global reach=5
Global legreach#=11
Global scroll,grok,off


								;;NET StOOF!
Global me.climber
netplayq$=Input("Net Play?")
If Lower(netplayq)="y" Then netplay=1
If netplay
	name$=Input("Name:")
	ip$=Input("IP: ")
	server=OpenTCPStream(ip,921)
	Print "connected!"
	If Not server
		Print "Could not connect :("
		Print "Press a key to exit"
		WaitKey()
		End
	EndIf
	out("n"+name)
	in$=""
	While Not ingame
		If ReadAvail(server)
			byte=ReadByte(server)
			If byte=10
				DebugLog "in: "+in
				Select Left(in,1)
					Case "s"
						seed=Mid(in,2)
						DebugLog "seed is "+seed
						SeedRnd MilliSecs()
					Case "n"	;joining game!
						myid$=Mid(in,2)
						DebugLog "my id is "+myid
					Case "j"
						id$=Mid(in,2,2)
						name$=Mid(in,4)
						him.climber=newclimber(id,name)
						If id=myid
							me=him
							ingame=1
							DebugLog "ingame!"
						EndIf
					Default
						handlenet(in)
				End Select
				in=""
			Else
				in=in+Chr(byte)
			EndIf
		EndIf
	Wend
Else
	name$=Input("Your Name? ")
	me=newclimber("01",name)
EndIf


Dim holds(4)
For c=1 To numholds
	holds(c)=LoadImage("hold"+c+".bmp")
	hold=holds(c)
Next

For x=1 To xsize
For y=1 To ysize
	If Rand(rare) = 1
		wall(x,y) = Rand(1,numholds)
	EndIf
Next
Next

wall(xsize/2-2,ysize/2)=1
wall(ysize/2+1,ysize/2)=1
wall(xsize/2-2,ysize/2-5)=1
wall(xsize/2+1,ysize/2-5)=1
time#=250

in$=""
While Not KeyHit(1)

	If netplay
		;Get messages
		While ReadAvail(server)
			byte=ReadByte(server)
			If byte=10
				handlenet(in)
				in=""
			Else
				in=in+Chr(byte)
			EndIf
		Wend
	EndIf
	
	x=0
	y=0-ImageHeight(wallimg)
	While x<640
	While y<480
		DrawImage wallimg,x,walloff+y
		y=y+ImageHeight(wallimg)
	Wend
	y=0-ImageHeight(wallimg)
	x=x+ImageWidth(wallimg)
	Wend
	
	For x=0 To xsize
		For y=0 To ysize
			If wall(x,y)
				hold=holds(wall(x,y))
				DrawImage hold,x*xscale,off+480-y*yscale-yscale
			EndIf
		Next
	Next

	For climber.climber=Each climber
		transformclimber(climber,off)	
		renderclimber(climber,off)
	Next
	
	ms=MilliSecs()
	If ms-oldms > time
		walloff=walloff+1
		If walloff > ImageHeight(wallimg)
			walloff=0
		EndIf

		off=off+1
		oldms=ms
	EndIf
	
	If off>yscale
		scrolldown()
	EndIf
	
	mx=MouseX()
	my=(480-MouseY())+off
	x=(mx-(mx Mod xscale))/xscale
	y=(my-(my Mod yscale))/yscale
	lshift=KeyDown(42)
	mhit=MouseHit(1) Or (MouseHit(2) Shl 1)
	If mhit
		moveclimber(me,x,y,lshift,mhit)
		If netplay
			sendmove(x,y+grok,lshift,mhit)
		EndIf
	EndIf
	
	If me\rhy>me\lhy
		height=me\rhy
	Else
		height=me\lhy
	EndIf
	Text 0,0,height+grok
	
	Flip
	Cls
Wend

Function handlenet(in$)
	DebugLog "got "+in
	Select Left(in,1)
		Case "j"	;New player
			id$=Mid(in,2,2)
			name$=Mid(in,4)
			newclimber(id,name)
		Case "m"	;Movement
			DebugLog "movement"
			id$=Mid(in,2,2)
			x$=Mid(in,4,2)
			y$=Mid(in,6,2)
			legs$=Mid(in,8,1)
			side$=Mid(in,9,1)
			DebugLog id+" to "+x+","+y+"  "+legs+"  "+side
			him.climber=findclimber(id)
			If him<>Null
				moveclimber(him,x,Int(y)-grok,legs,side)
			Else
				DebugLog "could not find "+id
				For c.climber=Each climber
					DebugLog c\id+":"+c\name
				Next
			EndIf
	End Select
End Function

Function out(out$)
	DebugLog "out: "+out
	For c=1 To Len(out)
		WriteByte server,Asc(Mid(out,c,1))
	Next
	WriteByte server,10
End Function

Function pad$(in$,length)
	If Len(in)<length
		Print "padding"
		pad$=""
		For c=1 To length-Len(in)
			pad$=pad+"0"
		Next
		Print "padded to "+pad+in
		Return pad+in
	EndIf
	Return in
End Function

Function scrolldown()
		grok=grok+1
		scroll=scroll+1
		If scroll>5
			rare=rare+1
			scroll=0
		EndIf
		If time > 100 Then time=time*.9
		off=0
		For y=0 To ysize-1
			For x=0 To xsize
				wall(x,y)=wall(x,y+1)
			Next
		Next
		For x=0 To xsize
			If Rand(rare)=1
				wall(x,ysize)=Rand(1,numholds)
			Else
				wall(x,ysize)=0
			EndIf
		Next
		For climber.climber=Each climber
			climber\lhy=climber\lhy-1
			climber\rhy=climber\rhy-1
			climber\lly=climber\lly-1
			climber\rly=climber\rly-1
			climber\midy=climber\midy-1
			climber\hipy=climber\hipy-1
		Next
End Function

Function newclimber.climber(id$,name$)
	him.climber=New climber
	him\name=name
	him\id=id
	him\lhx=xsize/2-2
	him\lhy=ysize/2-grok
	him\rhx=xsize/2+1
	him\rhy=ysize/2-grok
	him\llx=xsize/2-2
	him\lly=ysize/2-5-grok
	him\rlx=xsize/2+1
	him\rly=ysize/2-5-grok
	
	Return him
End Function

Function findclimber.climber(id$)
	For c.climber=Each climber
		If c\id=id
			Return c
		EndIf
	Next
End Function

Function killclimber(id)
	For climber.climber=Each climber
		If climber\id=id Then Delete climber
	Next
End Function

Function sendmove(x$,y$,legs$,side$)
	x=pad(x,2)
	y=pad(y,2)
	DebugLog x+","+y+"  "+legs+"  "+side
	out("m"+x+y+legs+side)
End Function

Function moveclimber(him.climber,x,y,legs,side)
	If legs
		If y<him\hipy
			If (side And 1)
				If wall(x,y)
					If x<him\rlx
						dist#=Sqr((x-him\hipx)^2+(y-him\hipy)^2)
						If dist<reach
							him\llx=x
							him\lly=y
						EndIf
					EndIf
				EndIf
			EndIf
			
			If (side And 2)
				If wall(x,y)>0 And x>him\llx
					dist#=Sqr((x-him\hipx)^2+(y-him\hipy)^2)
					If dist<reach
						him\rlx=x
						him\rly=y
					EndIf
				EndIf
			EndIf
		EndIf
	
	Else
	
		If (side And 1)
			If wall(x,y)
				If x<him\rhx
					dist#=Sqr((x-him\midx)^2+(y-him\midy)^2)
					ldist=Sqr((x-him\llx)^2+(y-him\lly)^2)
					rdist=Sqr((x-him\rlx)^2+(y-him\rly)^2)
					If dist<reach And ldist<legreach And rdist<legreach
						him\lhx=x
						him\lhy=y
					EndIf
				EndIf
			EndIf
		EndIf
		
		If (side And 2)
			If wall(x,y)>0 And x>him\lhx
				dist#=Sqr((x-him\midx)^2+(y-him\midy)^2)
				ldist=Sqr((x-him\llx)^2+(y-him\lly)^2)
				rdist=Sqr((x-him\rlx)^2+(y-him\rly)^2)
				If dist<reach And ldist<legreach And rdist<legreach
					him\rhx=x
					him\rhy=y
				EndIf
			EndIf
		EndIf
	EndIf
End Function

Function transformclimber(him.climber,off)
	lhx=him\lhx
	lhy=him\lhy
	rhx=him\rhx
	rhy=him\rhy
	llx=him\llx
	lly=him\lly
	rlx=him\rlx
	rly=him\rly
	
	an#=ATan2(rhx-lhx,lhy-rhy)+90
	him\an=an
	
	him\midx#=(him\lhx*2+him\rhx*2+him\llx+him\rlx)/6.0
	him\midy#=(him\lhy*2+him\rhy*2+him\lly+him\rly)/6.0
	him\midxp#=him\midx*xscale+xscale/2
	him\midyp#=off+480-him\midy*yscale-yscale/2
	
	him\lhxp#=lhx*xscale+xscale/2
	him\lhyp#=off+480-lhy*yscale-yscale/2-3
	him\rhxp#=rhx*xscale+xscale/2
	him\rhyp#=off+480-rhy*yscale-yscale/2-3
	xdiff#=him\midxp-him\lhxp
	ydiff#=him\midyp-him\lhyp
	length#=Sqr(xdiff^2+ydiff^2)
	lsp#=length*.8
;	him\lsxp#=him\lhxp+(xdiff/length)*lsp
;	him\lsyp#=him\lhyp+(ydiff/length)*lsp
	him\lsxp=him\midxp-Sin(an-90)*15
	him\lsyp=him\midyp-Cos(an-90)*15
	xdiff=him\midxp-him\rhxp
	ydiff=him\midyp-him\rhyp
	length#=Sqr(xdiff^2+ydiff^2)
	rsp#=length*.8
	;him\rsxp#=him\rhxp+(xdiff/length)*rsp
	;him\rsyp#=him\rhyp+(ydiff/length)*rsp
	him\rsxp=him\midxp+Sin(an-90)*15
	him\rsyp=him\midyp+Cos(an-90)*15
	
	him\hipxp#=him\midxp-Sin(an)*50
	him\hipyp#=him\midyp-Cos(an)*50

	him\hipx#=(him\hipxp-xscale/2)/xscale
	him\hipy#=(480-(him\hipyp-off)+yscale)/yscale
	
	him\llxp=llx*xscale+xscale/2
	him\llyp=off+480-lly*yscale-yscale/2-3
	him\rlxp=rlx*xscale+xscale/2
	him\rlyp=off+480-rly*yscale-yscale/2-3

End Function

Function renderclimber(him.climber,off)

	Oval him\midxp-reach*xscale,him\midyp-reach*yscale,reach*xscale*2,reach*yscale*2,0
	
	turnimage(bodyimg,BackBuffer(),him\an,him\midx*xscale+xscale/2,off+480-him\midy*yscale-yscale/2,0,0,120,120)
	DrawImage headimg,him\midxp+Sin(him\an)*20,him\midyp+Cos(him\an)*20
	DrawImage handimg,him\lhxp,him\lhyp
	DrawImage handimg,him\rhxp,him\rhyp

	Oval him\hipxp-reach*xscale,him\hipyp-reach*yscale,reach*xscale*2,reach*yscale*2,0
	
	If him\llyp>him\rlyp
		Line 0,him\llyp-yscale*legreach,640,him\llyp-yscale*legreach
	Else
		Line 0,him\rlyp-yscale*legreach,640,him\rlyp-yscale*legreach
	EndIf
	
	turnimage(hipimg,BackBuffer(),him\an,him\hipxp,him\hipyp,0,0,60,60)

	x1#=him\lhxp
	y1#=him\lhyp
	r1#=reach*xscale/2.0
	x2#=him\lsxp
	y2#=him\lsyp
	r2#=reach*xscale/2.0
	limb(x1,y1,r1,x2,y2,r2,BackBuffer(),lowarm,uparm,50,50)
	
	x1#=him\rhx*xscale+xscale/2
	y1#=off+480-him\rhy*yscale-yscale/2-3
	r1#=reach*xscale/2.0
	x2#=him\rsxp
	y2#=him\rsyp
	r2#=reach*xscale/2.0

	limb(x1,y1,r1,x2,y2,r2,BackBuffer(),uparm,lowarm,50,50)

	x1#=him\hipxp
	y1#=him\hipyp
	r1#=reach*xscale/2.0
	x2#=him\llxp
	y2#=him\llyp
	r2#=reach*xscale/2.0
	limb(x1,y1,r1,x2,y2,r2,BackBuffer(),upleg,lowleg,50,50,-1)

	x2#=him\rlxp
	y2#=him\rlyp

	limb(x1,y1,r1,x2,y2,r2,BackBuffer(),upleg,lowleg,50,50,-1)

	Rect him\midxp,him\midyp,5,5
	Rect him\lsxp,him\lsyp,5,5
	Rect him\rsxp,him\rsyp,5,5
	Rect him\lhxp,him\lhyp,5,5
	Rect him\rhxp,him\rhyp,5,5
	Rect him\hipxp,him\hipyp,5,5
	Rect him\llxp,him\llyp,5,5
	Rect him\rlxp,him\rlyp,5,5
	
	Line him\midxp,him\midyp,him\lsxp,him\lsyp
	Line him\midxp,him\midyp,him\rsxp,him\rsyp
	Line him\lsxp,him\lsyp,him\lhxp,him\lhyp
	Line him\rsxp,him\rsyp,him\rhxp,him\rhyp
	Line him\midxp,him\midyp,him\hipxp,him\hipyp
	Line him\hipxp,him\hipyp,him\llxp,him\llyp
	Line him\hipxp,him\hipyp,him\rlxp,him\rlyp
	
	Text him\midxp,him\midyp,him\name+"("+him\id+")"
End Function

Function turnimage(image,tobuffer,an#,addx,addy,minx,miny,maxx,maxy)

	imgbuffer=ImageBuffer(image)
	LockBuffer(tobuffer)
	LockBuffer(imgbuffer)

	xm#=Sin(-an)
	ym#=Cos(-an)

	width=maxx-minx
	height=maxy-miny	
	
	imgwidth=ImageWidth(image)
	imgheight=ImageHeight(image)
	
	midx#=width/2.0
	midy#=height/2.0

	For x=0 To width-1
	For y=0 To height-1
		diffx=midx-x
		diffy=midy-y
		oldx#=diffx*ym+diffy*xm+ImageWidth(image)/2
		oldy#=diffy*ym-diffx*xm+ImageHeight(image)/2
		If oldx>0 And oldy>0 And oldx<imgwidth-1 And oldy<imgheight-1
			argb=ReadPixelFast(oldx,oldy,imgbuffer)
			red=argb Shr 16 And %11111111
			green=argb Shr 8 And %11111111
			blue=argb And %11111111
			If red Or green Or blue
				WritePixel addx-diffx,addy-diffy,argb,tobuffer
			EndIf
		EndIf
	Next
	Next

	UnlockBuffer(imgbuffer)
	UnlockBuffer(tobuffer)

End Function

Function limb(x1#,y1#,r1#,x2#,y2#,r2#,tobuffer,image1,image2,clipx,clipy,dir=1)
	dist=Sqr((x1-x2)^2+(y1-y2)^2)
	If dist>r1+r2
		an#=ATan2(x2-x1,y2-y1)
		turnimage(image1,tobuffer,an,x1,y1,0,0,clipx,clipy)
		turnimage(image1,tobuffer,an,x1+Sin(an)*r1,y1+Cos(an)*r1,0,0,clipx,clipy)
		Return
	EndIf
	If dir=1
		If x1>x2
			x3#=x1
			y3#=y1
			r3#=r1
			x1=x2
			y1=y2
			r1=r2
			x2=x3
			y2=y3
			r2=r3
		EndIf
	ElseIf dir=-1
		If x1<x2
			x3#=x1
			y3#=y1
			r3#=r1
			x1=x2
			y1=y2
			r1=r2
			x2=x3
			y2=y3
			r2=r3
		EndIf
	EndIf
	
	cmidx#=(x2-x1)/2.0
	cmidy#=(y2-y1)/2.0
	
	midlen#=Sqr(cmidx^2+cmidy^2)
	
	h=Sqr(r1^2-midlen^2)
	an#=ATan2(cmidx,cmidy)
	
	elbowx#=x1+cmidx+Sin(an-90)*h
	elbowy#=y1+cmidy+Cos(an-90)*h
	
	an#=ATan2(elbowx-x1,elbowy-y1)
	turnimage(image1,tobuffer,an,x1+(elbowx-x1)/2.0,y1+(elbowy-y1)/2.0,0,0,clipx,clipy)
	an#=ATan2(elbowx-x2,elbowy-y2)
	turnimage(image2,tobuffer,an,x2+(elbowx-x2)/2.0,y2+(elbowy-y2)/2.0,0,0,clipx,clipy)
End Function