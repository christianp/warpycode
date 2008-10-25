Const screenheight=800,screenwidth=800
Graphics screenwidth,screenheight,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

Const splash#=.1
Const splashres#=2
Const dropsize#=25
Const droppull#=10
Const pipeoof#=.3
Const pipedropslow#=.1
Const scorerate#=.1
Const backlevelgain#=10

Global tanks:TList=New TList
Global drops:TList=New TList
Global pipedrops:TList=New TList
Global pipes:TList=New TList

Global backwater1#[screenwidth/5+1]
Global backwater2#[screenwidth/5+1]
Global backwaterlevel#=0

Type tank
	Field homex#,homey#
	Field x#,y#,width#,height#
	Field waterlevel#
	Field splosh1#[],splosh2#[],sploshsize
	Field life,maxlife#,targheight
	
	Function create:tank(x#,y#,width#,height#,life)
		t:tank=New tank
		t.x=x
		t.y=y
		t.homex=t.x
		t.homey=t.y
		t.width=width
		t.targheight=height
		t.height=1
		t.sploshsize=Int(2*width/splashres)
		t.splosh1=New Float[t.sploshsize+2]
		t.splosh2=New Float[t.sploshsize+2]
		t.life=life
		t.maxlife=t.life
		tanks.addlast t
		Return t
	End Function
	
	Method update()
		If height<>targheight Then height:+Sgn(targheight-height)*.5
		If homey>screenheight-backwaterlevel+100 Then life=0
		If life>0
			life:-1
		Else
			targheight=0
			If height<=0
				tanks.remove Self
				Return
			EndIf
		EndIf
		
		ox#=x-width
		oy#=y-waterlevel
		For c=2 To sploshsize-2
			splosh2[c]=((splosh1[c-1]+splosh1[c+1]+splosh1[c-2]+splosh1[c+2])/2.0-splosh2[c])*.9
		Next
		SetColor 0,0,255
		For c=1 To sploshsize-1
			a#=splosh1[c]
			splosh1[c]=splosh2[c]
			splosh2[c]=a
			nx#=ox+splashres
			ny#=y-waterlevel-(splosh1[c]+splosh1[c-1]+splosh1[c+1])/3.0
			For off#=0 To splashres
				DrawLine ox+off,y,ox+off,oy*(1-off/splashres)+ny*off/splashres
			Next
			ox=nx
			oy=ny
		Next
		
		bright#=life/maxlife
		SetColor 255*bright,255*bright,255*bright
		SetAlpha .1
		DrawOval homex-100,homey-100,200,200
		SetAlpha 1
		SetColor 255,255,255
		SetLineWidth 3
		DrawLine x-width,y,x-width,y-height-3
		DrawLine x+width,y,x+width,y-height-3
		DrawLine x-width,y,x+width,y
		SetLineWidth 1
		SetAlpha .4
		DrawOval x-10,y-10,20,20
		SetAlpha 1
		
		If waterlevel>height
			diff#=waterlevel-height
			While waterlevel>height
				side=Rand(0,1)*2-1
				drop.create(x+side*width,y-height,Rnd(2)*side,Rnd(-.5,.5))
				waterlevel:-dropsize/width
			Wend
		EndIf
	End Method
End Type

Type drop
	Field x#,y#,vx#,vy#
	
	Function create:drop(x#,y#,vx#,vy#)
		d:drop=New drop
		d.x=x
		d.y=y
		d.vx=vx
		d.vy=vy
		drops.addlast d
		Return d
	End Function
	
	Method update()
		ox#=x
		oy#=y
		x:+vx
		y:+vy
		vy:+1
		For t:tank=EachIn tanks
			If ((ox>t.x-t.width And x<t.x-t.width) Or (ox<t.x+t.width And x>t.x+t.width)) And y<t.y And y>t.y-t.height Then vx=-vx
			If x>=t.x-t.width And x<=t.x+t.width And ((oy<=t.y-t.waterlevel And y>=t.y-t.waterlevel) Or (y>=t.y-t.waterlevel And y<=t.y) Or (oy>=t.y-t.waterlevel And oy<=t.y))
				t.waterlevel:+dropsize/t.width
				c=(x-t.x+t.width)/splashres
				If c<2 Then c=2
				If c>t.sploshsize-2 Then c=t.sploshsize-2
				t.splosh1[c]:-vy*.3*t.waterlevel/t.height
				drops.remove Self
				Return
			EndIf
		Next
		SetAlpha .5
		SetColor 0,0,255
		DrawOval x-3,y-3,6,6
		SetAlpha 1
		
		If y>screenheight-backwaterlevel
			backwaterlevel:+backlevelgain*dropsize/screenwidth
			If x<screenwidth-5 And x>5
				backwater1[Int(x/5)]:-vy*.1
			EndIf
			drops.remove Self
		EndIf
	End Method
End Type

Type pipedrop
	Field mypipe:pipe,pos#,v#
	
	Function create:pipedrop(p:pipe,pos#,v#)
		pd:pipedrop=New pipedrop
		pipedrops.addlast pd
		p.mydrops.addlast pd
		pd.mypipe=p
		pd.pos=pos
		pd.v=v
		Return pd
	End Function
	
	Method update()
		pos:+v
		v=v*.9+Sin(mypipe.an)
		If pos<0 Or pos>mypipe.length
			d:drop=drop.create(mypipe.x+pos*mypipe.c,mypipe.y+pos*mypipe.s,v*mypipe.c*pipedropslow,v*mypipe.s*pipedropslow)
			mypipe.mydrops.remove Self
			pipedrops.remove Self
			Return
		EndIf
		
		SetColor 0,0,255
		SetAlpha .5
		x#=mypipe.x+pos*mypipe.c
		y#=mypipe.y+pos*mypipe.s
		DrawOval x-10,y-10,20,20
		SetAlpha 1
	End Method
End Type

Type pipe
	Field x#,y#,length#
	Field endx#,endy#
	Field an#
	Field s#,c#,ps#,pc#
	Field crankan#,vcrankan#
	Field crankx#,cranky#
	Field mydrops:TList
	Field pull#
	
	Function create:pipe(x#,y#,length#,an#)
		p:pipe=New pipe
		pipes.addlast p
		p.x=x
		p.y=y
		p.length=length
		p.an=an
		p.s=Sin(p.an)
		p.c=Cos(p.an)
		p.ps=Sin(p.an+90)
		p.pc=Cos(p.an+90)
		p.endx=p.x+p.c*p.length
		p.endy=p.y+p.s*p.length
		p.crankx=p.x+p.c*p.length/2
		p.cranky=p.y+p.s*p.length/2
		p.mydrops=New TList
		Return p
	End Function
	
	Method calc()
		an=ATan2(endy-y,endx-x)
		length#=Sqr((endy-y)^2+(endx-x)^2)
		s=Sin(an)
		c=Cos(an)
		ps=Sin(an+90)
		pc=Cos(an+90)
		crankx=x+c*length/2
		cranky=y+s*length/2
	End Method
	
	Method update()
		
		If Self<>picked
			vcrankan:+Cos(crankan)*.5
			vcrankan:*.95
			crankan:+vcrankan
		EndIf
		
		If (crankan+90) Mod 360>180
			pull:+Abs(vcrankan)
		EndIf
		If pull<0 Then pull=0
		
		If Sgn(vcrankan)=1
			pullx#=x
			pully#=y
		Else
			pullx#=endx
			pully#=endy
		EndIf
			
		intank:tank=Null
		For t:tank=EachIn tanks
			If pullx>t.x-t.width And pullx<t.x+t.width And pully<t.y And pully>t.y-t.height
				intank=t
			EndIf
		Next
		If intank<>Null
			While pull>=droppull And intank.waterlevel>0
				If Sgn(vcrankan)=1 Then pos#=5 Else pos#=length-5
				pd:pipedrop=pipedrop.create(Self,pos,0)
				pull:-droppull
				intank.waterlevel:-dropsize/intank.width
			Wend
		EndIf
	
		For pd:pipedrop=EachIn mydrops
			pd.v:+vcrankan*pipeoof
		Next
		
		SetColor 255,255,255
		SetLineWidth 2
		DrawLine x-pc*10,y-ps*10,endx-pc*10,endy-ps*10
		DrawLine x+pc*10,y+ps*10,endx+pc*10,endy+ps*10
		If mousestate=2 And picked=Self
			SetColor 255,255,0
		EndIf
		DrawLine crankx,cranky,crankx+Cos(crankan)*40,cranky+Sin(crankan)*40
		SetColor 255,255,255
		SetLineWidth 1
		SetAlpha .3
		DrawOval x-10,y-10,21,21
		DrawOval endx-10,endy-10,21,21
		SetAlpha 1
	End Method
End Type

mytank:tank=tank.create(screenwidth/2,screenheight/2,100,100,2000)
mytank.waterlevel=50
mytank.height=100

Global mousestate=0
Global startx#=0,starty#=0
Global picked:pipe=Null
Global pickedt:tank=Null
Global score#=0

While Not KeyHit(KEY_ESCAPE) And backwaterlevel<screenheight
	If Rand(3)=1
		drop.create(Rand(25,screenwidth-25),0,0,Rnd(-.5,.5))
	EndIf
	
	mx#=MouseX()
	my#=MouseY()
	DrawText mousestate,0,15
	Select mousestate
	Case -1 'waiting for unclick
		If Not MouseDown(1) Then mousestate=0
	Case 0 'default
		If MouseDown(1)
			picked:pipe=Null
			pickedend1:pipe=Null
			pickedend2:pipe=Null
			For p:pipe=EachIn pipes
				dx#=mx-p.crankx-Cos(p.crankan)*40
				dy#=my-p.cranky-Sin(p.crankan)*40
				dist#=dx*dx+dy*dy
				If dist<900
					picked:pipe=p
				EndIf
				dx#=mx-p.x
				dy#=my-p.y
				dist#=dx*dx+dy*dy
				If dist<400
					pickedend1=p
				EndIf
				dx#=mx-p.endx
				dy#=my-p.endy
				dist#=dx*dx+dy*dy
				If dist<400
					pickedend2=p
				EndIf
			Next
			If picked<>Null
				mousestate=2
			ElseIf pickedend1<>Null
				mousestate=4
			ElseIf pickedend2<>Null
				mousestate=5
			Else
				pickedt:tank=Null
				For t:tank=EachIn tanks
					dx#=mx-t.x
					dy#=my-t.y
					dist#=dx*dx+dy*dy
					If dist<100
						pickedt=t
					EndIf
				Next
				If pickedt<>Null
					mousestate=3
				Else
					mousestate=1
					startx=mx
					starty=my
				EndIf
			EndIf
		EndIf
		If MouseHit(2)
			For p:pipe=EachIn pipes
				lambda#=(my-p.y+(p.ps/p.pc)*(p.x-mx))/(p.s-p.ps*p.c/p.pc)
				If lambda>=0 And lambda<=p.length
					mu#=(p.x+p.c*lambda-mx)/p.pc
					If Abs(mu)<10
						For pd:pipedrop=EachIn p.mydrops
							pipedrops.remove pd
						Next
						pipes.remove p
					EndIf
				EndIf
			Next
		EndIf
	Case 1	'placing pipe
		SetLineWidth 8
		SetColor 255,255,255
		SetAlpha .5
		DrawLine startx,starty,mx,my
		SetAlpha 1
		SetLineWidth 1
		
		If Not MouseDown(1)
			mousestate=-1
			dx#=mx-startx
			dy#=my-starty
			an#=ATan2(dy,dx)
			length#=Sqr(dx*dx+dy*dy)
			If length>100
				p:pipe=pipe.create(startx,starty,length,an)
			EndIf
		EndIf
	Case 2 'turning crank
		dx#=mx-picked.crankx
		dy#=my-picked.cranky
		oan#=picked.crankan
		picked.crankan=ATan2(dy,dx)
		diff#=picked.crankan-oan Mod 360
		'If diff<0 Then diff=360-diff
		If diff>180 Then diff:-360
		picked.vcrankan=Abs(Sin(diff))*50
		
		If Not MouseDown(1)
			mousestate=0
			picked=Null
		EndIf
	Case 3 'moving tank
		ox#=pickedt.x
		
		pickedt.x=mx
		pickedt.y=my
		dist#=(mx-pickedt.homex)^2+(my-pickedt.homey)^2
		If dist>10000
			dist=Sqr(dist)
			pickedt.x=pickedt.homex+100*(mx-pickedt.homex)/dist
			pickedt.y=pickedt.homey+100*(my-pickedt.homey)/dist
		EndIf

		wobble#=ATan(pickedt.x-ox)/90.0
		For c#=2 To pickedt.sploshsize-2
			pickedt.splosh1[c]:+wobble*.025*(2*c/pickedt.sploshsize-1)*pickedt.waterlevel
		Next
		
		If Not MouseDown(1)
			mousestate=0
			pickedt=Null
		EndIf
	Case 4 'move end 1
		pickedend1.x=mx
		pickedend1.y=my
		pickedend1.calc()
		If Not MouseDown(1)
			mousestate=0
			pickedend1=Null
		EndIf
	Case 5
		pickedend2.endx=mx
		pickedend2.endy=my
		pickedend2.calc()
		If Not MouseDown(1)
			mousestate=0
			pickedend2=Null
		EndIf
	End Select
	
	For c=2 To screenwidth/5-3
		backwater2[c]=((backwater1[c-1]+backwater1[c+1]+backwater1[c-2]+backwater1[c+2])/2.0-backwater2[c])*.99
	Next
	SetColor 0,0,100
	For c=0 To screenwidth/5-1
		a#=backwater1[c]
		backwater1[c]=backwater2[c]
		backwater2[c]=a
		Local poly#[]=[c*5.0,Float(screenheight),(c+1)*5.0,Float(screenheight),(c+1)*5.0,screenheight-backwaterlevel-backwater2[c+1],c*5.0,screenheight-backwaterlevel-backwater1[c]]
		DrawPoly poly
		'DrawLine c*5,screenheight-backwaterlevel-backwater2[c],(c+1)*5,screenheight-backwaterlevel-backwater1[c+1]
		'DrawLine c*5,screenheight-backwaterlevel,(c+1)*5,screenheight-backwaterlevel
	Next
	
	totalwater#=0
	capacity#=0
	For t:tank=EachIn tanks
		t.update()
		totalwater:+t.waterlevel*t.width
		capacity:+t.height*t.width*t.life/t.maxlife
		score:+scorerate*t.waterlevel/t.width
	Next
	
	If totalwater/capacity>.8
		If Rand(250)=1
			tank.create(Rand(100,screenwidth-100),Rand(100,screenheight-backwaterlevel-100),Rand(50,125),Rand(30,100),Rand(1000,4000))
		EndIf
	EndIf
	
	For d:drop=EachIn drops
		d.update()
	Next
	
	For p:pipe=EachIn pipes
		p.update()
	Next
	
	For pd:pipedrop=EachIn pipedrops
		pd.update()
	Next
	DrawText "Score: "+String(Int(score)),0,0
	
	'FlushMem
	Flip
	Cls
Wend

If backwaterlevel>=screenheight
	Cls
	DrawText "Your score was: "+String(Int(score)),0,screenheight/2
	Flip
	WaitKey()
EndIf