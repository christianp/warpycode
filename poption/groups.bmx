Const version:String = "hunkahunka"

Const screenheight=800,screenwidth=800

Global gfxscale#,width,height,depth,originx#,originy#
Global tabletmode=0,poponrelease=0,alwayspush=0,fullscreen=0

Global popsounds:TSound[]=[LoadSound("pop1.wav"),LoadSound("pop2.wav"),LoadSound("pop3.wav")]
Global cannonsounds:TSound[]=[LoadSound("cannon1.wav"),LoadSound("cannon2.wav"),LoadSound("cannon3.wav")]
Global airhorn:TSound=LoadSound("airhorn.wav")
Global clocktick:TSound=LoadSound("clock-tick1.wav")
Global explosion:TSound=LoadSound("explosion.wav")
Global clink:TSound=LoadSound("thudbox1.wav")
Global sweepsound:TSound=LoadSound("bigwhitebroom.wav",True)

Global radius=80
Const oomf#=.45
Const bounce#=.9
Const popchance=5000
Const minepopchance=4000
Const mineblast=1000
Const magnetzoom#=10
Const magnetpull#=.02
'Global magnetradius=radius*1.01
Const wormholepull#=5000000
Const wormholelife=600
Const sweepswishlength=3

Global thingprobs#[]=[100.0,5.0,1.0,1.0]
Global sweeps:TList=New TList
Global things:TList=New TList
Global groups:TList=New TList
Global ungrouped:TList=New TList
Global flashes:TList=New TList
Global scoreflashes:TList=New TList
Global walls:TList=New TList
Global sounditems:TList=New TList
Global netclients:TList=New TList
Global bots:TList=New TList
Global displayscore
Global mousestate,mstartx#,mstarty#,mx#=screenwidth/2,my#=screenheight/2
Global mysweep:sweep
Global roundlength
Global tick,repoptick,repop#,minrepop#,repopdecay#,gamespeed
Global starttime,timeleft#
Global netmode,controlmode
Global server:netserver,myclient:localclient
Global results:TList
Global ingamemenustate,finished,finishing=0
Global lockmouse=0

Const netfrequency=5
Const HELLO=101,CLIENTNAME=102,WRONGVERSION=103,RIGHTVERSION=104,GOODBYE=105,TICKTIME=106,GAMESTART=107,RESETGAME=108,ENDROUND=109
Const NEWCLIENT=201,NEWSWEEP=202,NEWWALL=203,NEWTHING=204
Const SWEEPPOS=301,THINGPOS=302,THINGDEAD=303,CLIENTDEAD=304,CLIENTSCORE=305

'stupid winAPI stuff
'Extern "win32"
'	Function GetActiveWindow%()
'	Function GetDesktopWindow%()
'	Function GetWindowRect%(hWnd%, lpRect:Byte Ptr)
'	Function SetWindowText%(hWnd%, lpString$z) = "SetWindowTextA@8"
'	Function SetWindowPos%(hWnd%, after%, x%, y%, w%, h%, flags%)
'	Function GetCursorPos(lpPoint:Byte Ptr)
'End Extern

Type lpRECT
	Field l%, t%, r%, b%
End Type

Type POINTAPI
	Field X,Y
End Type
'relative sanity after this point

Function initgfx()
	If fullscreen
		gmodes=CountGraphicsModes()
		width=0
		c=0
		While (height<screenheight Or width<height) And c<gmodes
			GetGraphicsMode(c,width,height,depth,hertz)
			Print width
			c:+1
		Wend
		If c>=gmodes
			gfxscale=Float(height)/screenheight
		Else
			gfxscale=1.0
		EndIf
		depth=32
	Else
		width=screenwidth
		height=screenheight
		depth=0
		gfxscale=1
	EndIf
	AppTitle="Poption"
	Graphics width,height,depth
	originx#=(width-screenwidth)/2.0
	originy#=(height-screenheight)/2.0
	SetOrigin originx,originy

	'just a bit more winAPI rubbish
	If Not fullscreen
	'	centrewindow()
	EndIf
	
	SetBlend ALPHABLEND
	SetImageFont(LoadImageFont("c:\windows\fonts\Verdana.ttf", 30, SMOOTHFONT)) 
	SeedRnd MilliSecs()
	AutoMidHandle True
	MoveMouse width/2,height/2

End Function

Rem
Function centrewindow()
	Global hWnd% = GetActiveWindow()
	Local desk_hWnd% = GetDesktopWindow(), l:lpRect = New lpRECT

	GetWindowRect desk_hWnd, l:lpRECT ' Get Desktop Dimensions
	
	' Center Window
	SetWindowPos hWnd, -2, (l.r / 2) - (width / 2), (l.b / 2) - (height / 2), 0, 0, 1

	l:lpRECT = Null ' Mark for garbage collection
	wl:lpRECT = Null
End Function
EndRem

Type thing
	Field netid
	Field powerup
	Field x#,y#
	Field ox#,oy#
	Field vx#,vy#
	Field friction#
	Field group:group
	Field neighbours:TList
	Field distancefrommouse#
	Field distancefromepicentre
	Field Size#
	Field Wait,maxwait
	Field scores
	Field hit
	Field pushable,clickable
	Field thudchan:TChannel
	Field mywinner:sweep
	
	Method New()
		friction=.9
		pushable=1
		clickable=1
	End Method
	
	Function Create:thing(x#,y#,powerup=-1)
		Local t:thing
		If powerup=-1 Then powerup=pickint(thingprobs,[0,1,2,3])
		Select powerup
		Case 0
			t=New thing
		Case 1
			t=New mine
		Case 2
			t=New magnet
		Case 3
			t=New wormhole
		End Select
		t.powerup=powerup
		t.x=x
		t.y=y
		t.ox=t.x
		t.oy=t.y
		things.AddLast t
		t.distancefromepicentre=-1
		t.Size=0
		t.Wait=-1
		t.thudchan=AllocChannel()
		Return t
	End Function
	
	Method check(mygroup:group)
		group=mygroup
		mygroup.members.AddLast Self
		ungrouped.Remove Self
		neighbours=New TList				
		For t:thing=EachIn ungrouped
			dx#=t.x-x
			dy#=t.y-y
			d#=dx*dx+dy*dy
			If d<t.Size*t.Size
				t.check(mygroup)
				neighbours.AddLast t
				t.neighbours.AddLast Self
			EndIf
		Next
	End Method
	
	Method update()
		hit=0
		If Size<radius Then Size:+5
		If Wait>0
			If Wait<=5 Then Size=radius*Float(Wait)/5
			Wait:-1
		EndIf
		If friction=0
			x=ox
			y=oy
		EndIf
		vx=vx*friction
		vy=vy*friction
		x=x+vx
		y=y+vy
		
		'boundaries 
		If x<0 Or x>screenwidth
			If x<0 Then x=0;playbouncesound()
			If x>screenwidth Then x=screenwidth;playbouncesound()
			For w:wall=EachIn walls
				If w.inside(x,y)
					x=ox
					y=oy
				EndIf
			Next
			vx=-vx*bounce
		EndIf
		If Y<0 Or Y>screenheight
			If Y<0 Then y=0;playbouncesound()
			If Y>screenheight Then y=screenheight;playbouncesound()
			For w:wall=EachIn walls
				If w.inside(x,y)
					x=ox
					y=oy
				EndIf
			Next
			vy=-vy*bounce
		EndIf
		
		'collide with walls
		For w:wall=EachIn walls
			If w.inside(x,y)
				hitwall(w)
				x=ox
				y=oy
			EndIf
		Next

		'set old position
		ox=x
		oy=y
		
		'random poppage
		If controlmode
			p#=1.0*group.members.Count()
			If Rand(800)<p
				distancefromepicentre=1
				die()
			EndIf
		EndIf
		
		'dying stuff
		If Wait=0
			If mywinner
				mywinner.score:+scores
				If mywinner=mysweep And myclient
					myclient.tellscore()
				ElseIf mywinner.isbot And server
					server.bottellscore(mywinner.owner)
				EndIf
			EndIf
			die()
		EndIf
	End Method
	
	Method hitwall(w:wall)
		SetColor 255,0,0
		sx#=x-ox
		sy#=y-oy
		
		For i=0 To 2
			a=i*2
			b=i*2+1
			c=(i*2+2) Mod 6
			d=(i*2+3) Mod 6

			lambda#=(y-w.points[b]+(sy/sx)*(w.points[a]-x))/(w.diffs[b]-sy*w.diffs[a]/sx)
			vlength#=Sqr(vy*vy+vx*vx)
			
			If lambda>=0 And lambda<=w.lengths[i]
				mu#=-(w.points[a]+w.diffs[a]*lambda-x)/sx
				If mu>0 And mu<1
					dotprod#=-vx*w.pdiffs[a]-vy*w.pdiffs[b]
					vx=(vx+2*w.pdiffs[a]*dotprod)*bounce
					vy=(vy+2*w.pdiffs[b]*dotprod)*bounce
					x=w.points[a]+lambda*w.diffs[a]-.1*w.pdiffs[a]
					y=w.points[b]+lambda*w.diffs[b]-.1*w.pdiffs[b]
					hit=1
					playbouncesound()
				EndIf
			EndIf
		Next
	End Method
	
	Method playbouncesound()
		If ChannelPlaying(thudchan) Then Return
		speed#=((x-ox)*(x-ox)+(y-oy)*(y-oy))
		If speed>5
			volume#=(speed-5)/speed
			If volume<0 Then volume=0
			SetChannelVolume thudchan,volume
			SetChannelPan thudchan,2*x/screenwidth-1
			PlaySound clink,thudchan
			'sounditem.create(clink,1,2*x/screenwidth-1,volume)
		EndIf
	End Method	
	
	Method setcolours()
		bright#=group.members.Count()*.05
		If bright>1 Then bright=1
		SetColor bright*255,bright*255,(1-bright)*255
		SetAlpha .1
	End Method
	
	Method Draw()
		setcolours()
		DrawOval x-Size/2,y-Size/2,Size,Size
		SetAlpha 1
		DrawOval x-5,y-5,10,10
		For ot:thing=EachIn neighbours
			DrawLine ot.x,ot.y,x,y
		Next
	End Method


	Method pop(distance=0)
		If distance>distancefromepicentre And distancefromepicentre>=0 Then Return
		distancefromepicentre=distance
		If neighbours=Null Then Return
		For t:thing=EachIn neighbours
			If t.distancefromepicentre=-1 Or t.distancefromepicentre>distance
				t.pop(distance+1)
			EndIf
		Next
	End Method
	
	Method makepop(myscore,winner:sweep)
		If myscore
			If distancefromepicentre<5
				multiplier=50
			Else
				multiplier=100
			EndIf
			If mine(Self) Then multiplier:*2
			scores=(distancefromepicentre+1)*multiplier
			mywinner=winner
		EndIf
		Wait=distancefromepicentre*5
		maxwait=Wait+Rand(4)
	End Method
		
	Method popsound()
		rate#=1.0/(Sqr(Sqr(distancefromepicentre))+Rnd(.3))
		If rate<0 Then rate=0
		If rate>1 Then rate=1
		pan#=2*x/screenwidth-1.0
		If distancefromepicentre<5
			sound=popsounds[Rand(2)]
		Else
			sound=cannonsounds[Rand(2)]
		EndIf
		sounditem.Create(sound,rate,pan,1)
	End Method

	Method die()
		flash.Create(x,y,Sqr(distancefromepicentre+1)*15,0)
		If scores Then scoreflash.Create(x,y,scores)
		popsound()
		things.Remove Self
		
		Select netmode
		Case 1
			server.obitthing(Self)
		Case 2
			myclient.obitthing(Self)
		End Select
	End Method
End Type

Type mine Extends thing
	Field popchance#
	Method New()
		friction=.9
		pushable=1
		clickable=0
	End Method
	
	Method update()
		Super.update()
		
		If controlmode
			popchance#=(10*group.members.Count()+1)/Float(minepopchance)
			If Rand(minepopchance)<=(10*group.members.Count()+1) Then die()
		EndIf
	End Method
	
	Method setcolours()
		SetColor 255,0,0
		SetAlpha Sin(tick*(popchance*500)^2)*.3+.5
	End Method
	
	Method Draw()
		skitter#=(popchance*200)^2
		rx#=Rnd(-skitter,skitter)
		ry#=Rnd(-skitter,skitter)
		x:+rx
		y:+ry
		Super.draw()
		x:-rx
		y:-ry
	End Method
	
	Method popsound()
		rate#=1.0/Sqr(Sqr(distancefromepicentre))
		If rate<0 Then rate=0
		If rate>1 Then rate=1
		pan#=2*x/screenwidth-1.0
		sounditem.Create(explosion,rate,pan,.7)
	End Method
	
	Method die()
		For t:thing=EachIn things
			If t<>Self
				dx#=t.x-x
				dy#=t.y-y
				d#=dx*dx+dy*dy
				If d<90000
					d=Sqr(d)
					dx:/d
					dy:/d
					f#=mineblast/d
					t.vx:+f*dx
					t.vy:+f*dy
				EndIf
			EndIf
		Next
	
		pop()
		If Not scores Then group.pop(0,Null)
		Super.die()
	End Method
End Type

Type magnet Extends thing
	Field stickers:TList
	
	Method New()
		friction=1
		pushable=0
		clickable=0
		stickers=New TList
		vx=Rnd(-magnetzoom,magnetzoom)
		vy=Rnd(-magnetzoom,magnetzoom)
	End Method
	
	Method update()
		Super.update()
		
		For t:thing=EachIn neighbours
			If Not (stickers.findlink(t) Or magnet(t) Or t=Self)
				stickers.addlast t
			EndIf
		Next
		
		For t:thing=EachIn stickers
			If Not things.findlink(t)
				stickers.remove t
			Else
				SetColor 255,255,255
				SetAlpha Rnd(.3)
				DrawLine x,y,t.x,t.y
				SetAlpha 1
				dx#=x-t.x
				dy#=y-t.y
				d#=Sqr(dx*dx+dy*dy)
				f#=(d-magnetradius)*magnetpull
				t.vx:+dx*f/d
				t.vy:+dy*f/d
				If d>300 Then stickers.remove t
			EndIf
		Next
	End Method
	
	Method setcolours()
		SetColor 0,0,255
		SetAlpha Sin(tick*10)*.3+.4
	End Method
End Type

Type wormhole Extends thing
	Field red#,green#,blue#
	Method New()
		friction=0
		pushable=0
		clickable=0
	End Method
	
	Method update()
		Super.update()
		
		For t:thing=EachIn things
			If t<>Self
				dx#=x-t.x
				dy#=y-t.y
				d#=dx*dx+dy*dy
				sqrd#=Sqr(d)
				dx:/d
				dy:/d
				t.vx:+dx*wormholepull/d
				t.vy:+dy*wormholepull/d
				If sqrd<radius
					t.pop()
					If mine(t) Then wait=Rand(3,10)
					g:group=t.group
					If g<>Null
						g.pop(0,Null)
					EndIf
				EndIf
			EndIf
		Next
		If controlmode
			If Rand(wormholelife)<=1 Then die()
		EndIf
	End Method
	
	Method makepop(myscore,winner:sweep)
		Return
	End Method
	
	Method setcolours()
		red#=red*.9+(Rnd(5)*50)*.1
		green#=green*.9+(Rnd(5)*50)*.1
		blue#=blue*.9+(Rnd(5)*50)*.1
		
		SetColor red,green,blue
		'SetAlpha Sin(tick*.2)*.5+.3
		SetAlpha .5
	End Method
	
	Method Draw()
		SetColor 100,100,100
		phase#=Sin(tick*10)
		SetAlpha phase*.02+.04
		r#=phase*20+200
		DrawOval x-r,y-r,r*2,r*2
		SetAlpha 1
		
		Super.draw()
	End Method
End Type
	
Type group
	Field members:TList
	Field r,g,b
	
	Function Create:group()
		g:group=New group
		g.members=New TList
		g.r=Rand(5)*50
		g.g=Rand(5)*50
		g.b=Rand(5)*50
		groups.AddLast g
		Return g
	End Function
	
	Method pop(scores,winner:sweep)
		total=0
		For t:thing=EachIn members
			t.makepop(scores,winner)
			total:+t.scores
		Next
		If winner=mysweep
			scoreflash.Create(0,screenheight,total)
		EndIf
		'quickhull(members)			
	End Method
	
	Method Compare(o:Object)
		og:group=group(o)
		If og.members.count()>members.count() Then Return -1 Else Return 1
	End Method
End Type

Type sweep
	Field netid
	Field chan,volume#
	Field red,green,blue
	Field x#,y#
	Field catchup#
	Field ox#,oy#
	Field an#,van#,ovan#
	Field length#
	Field sweeppoints:vector[(sweepswishlength+1)*2]
	Field targx#,targy#
	Field ovx#,ovy#
	Field smallmove#              
	Field sweepan#
	Field score,displayscore
	Field name$
	Field isbot,owner:ai
	
	Method New()
		For c=0 To sweepswishlength*2+1
			sweeppoints[c]=New vector
		Next
		chan=AllocChannel()
		volume=0
		SetChannelVolume chan,0
		PlaySound sweepsound,chan
		sweeps.AddLast Self
		catchup=1
	End Method
	
	Function Create:sweep(length#,red,green,blue,netid=0,name$="")
		s:sweep=New sweep
		s.length=length
		s.red=red
		s.green=green
		s.blue=blue
		s.name=name
		s.netid=netid
		s.score=0
		Return s
	End Function
		
	
	Method update()
		If catchup>1 catchup=1
		If catchup<0 catchup=0
		'horrendous sweeper-moving stuff
		ox#=x
		oy#=y
		ovan#=van
		x:+(targx-x)*catchup
		y:+(targy-y)*catchup
		dx#=x-ox
		dy#=y-oy
		ovx#=ovx*.5+dx*.5
		ovy#=ovy*.5+dy*.5
		smallmove#=Sqr(ovx*ovx+ovy*ovy)
		dx#=x-ox
		dy#=y-oy
		mousemove#=dx*dx+dy*dy
		sweepan#=ATan2(ovy,ovx)
		ox=x
		oy=y
		swish#=Sin(sweepan-90-an)
		van=van*.5+swish*10
		an:+van
		
		If an Mod 90<1 an:+.1

		'end points of sweeper
		x1#=x-Cos(an)*length
		y1#=y-Sin(an)*length
		x2#=x+Cos(an)*length
		y2#=y+Sin(an)*length
		
		'update sweeppoints quadrangle
		For i=sweepswishlength To 1 Step -1
			sweeppoints[i*2]=sweeppoints[i*2-2]
			sweeppoints[i*2+1]=sweeppoints[i*2-1]
		Next
		'sweeppoints[2]=sweeppoints[0]
		'sweeppoints[3]=sweeppoints[1]
		sweeppoints[0]=vector.Create(x1,y1)
		sweeppoints[1]=vector.Create(x2,y2)
		'ov:vector=sweeppoints[0]
		
		'q:vector=sweeppoints[0]
		'w:vector=sweeppoints[1]
		'e:vector=sweeppoints[2]
		'e:vector=sweeppoints[3]
		
		'update displayscore
		scorerate#=(score-displayscore)*.03
		If scorerate>0 And scorerate<1 Then scorerate=1
		displayscore:+scorerate
		
		sounds()
		
	End Method
	
	Method sounds()
		volume:*.8
		SetChannelVolume chan,volume*.3
	End Method
	
	Method Draw()
		SetLineWidth 5
		SetColor red,green,blue
		DrawLine sweeppoints[0].x,sweeppoints[0].y,sweeppoints[1].x,sweeppoints[1].y
		For i=0 To sweepswishlength-1
			SetAlpha 1.0-Float(i)/sweepswishlength
			DrawLine sweeppoints[i*2+0].x,sweeppoints[i*2+0].y,sweeppoints[i*2+2].x,sweeppoints[i*2+2].y
			DrawLine sweeppoints[i*2+1].x,sweeppoints[i*2+1].y,sweeppoints[i*2+3].x,sweeppoints[i*2+3].y
			Local poly#[]=[sweeppoints[i*2+0].x,sweeppoints[i*2+0].y,sweeppoints[i*2+1].x,sweeppoints[i*2+1].y,sweeppoints[i*2+3].x,sweeppoints[i*2+3].y,sweeppoints[i*2+2].x,sweeppoints[i*2+2].y]
			'SetAlpha .5
			'SetColor 255*(1.0-Float(i)/sweepswishlength),0,255*Float(i)/sweepswishlength
			DrawPoly poly
			'SetAlpha 1
		Next
		DrawLine sweeppoints[sweepswishlength*2].x,sweeppoints[sweepswishlength*2].y,sweeppoints[sweepswishlength*2+1].x,sweeppoints[sweepswishlength*2+1].y
		
		SetLineWidth 1
		SetAlpha .5
		DrawOval x-10,y-10,20,20
		SetAlpha 1
		If an<-90 Or an>90 Then SetRotation an+180 Else SetRotation an
		SetScale .5,.5
		DrawText name,x,y
		SetScale 1,1
		SetRotation 0
		
		
		SetColor 255,255,255
	End Method
		
	Method push()
		report=0
		If netmode=2 'if client, need to report pushes
			If Self=myclient.mynetclient.theirsweep Then report=1
		EndIf
		volume:+(smallmove/1000.0)
	
		swipe#=Cos(sweepan-90-an)*.5+.5
		swipe#=.5
		If Abs(swipe)=1 Then Return
		dx#=Cos(sweepan+180)
		dy#=Sin(sweepan+180)
		For t:thing=EachIn things
			If t.pushable
				in1=pointintriangle(t.x,t.y,sweeppoints[0].x,sweeppoints[0].y,sweeppoints[1].x,sweeppoints[1].y,sweeppoints[2].x,sweeppoints[2].y)
				in2=pointintriangle(t.x,t.y,sweeppoints[1].x,sweeppoints[1].y,sweeppoints[3].x,sweeppoints[3].y,sweeppoints[2].x,sweeppoints[2].y)
				If (in1 Or in2) And Not t.hit
					t.x=t.x-dx*(smallmove)
					t.y=t.y-dy*(smallmove)
					t.vx=t.vx-dx*swipe*oomf*smallmove
					t.vy=t.vy-dy*swipe*oomf*smallmove
					If report=1
						myclient.tellthing(t)
					ElseIf isbot And server
						server.tellthingpos(t)
					EndIf
				EndIf
			EndIf
		Next
	End Method

	Method clickypop()
		closest:thing=Null
		For t:thing=EachIn things
			distancefrommouse=(x-t.x)*(x-t.x)+(y-t.y)*(y-t.y)
			If distancefrommouse<radius*radius/4 And t.clickable
				If closest=Null Then closest=t
				'ElseIf t.distancefrommouse<closest.distancefrommouse Then closest=t
			EndIf
		Next
		If closest<>Null
			closest.pop()
			g:group=closest.group
			If g=Null Then Return
			g.pop(1,Self)
		EndIf
	End Method

	Method Delete()
		StopChannel chan
	End Method
End Type

Type wall
	Field points#[6]
	Field diffs#[6],pdiffs#[6],lengths#[3]
	Field drawedge[3]
	
	Function Create:wall(points#[],drawedge[])
		w:wall=New wall
		walls.addlast w
		For i=0 To 5
			w.points[i]=points[i]
		Next
		w.calcdiffs()
		For i=0 To 2
			w.drawedge[i]=drawedge[i]
		Next
		Return w
	End Function
	
	Method calcdiffs()
		dx#=points[2]-points[0]
		dy#=points[3]-points[1]
		l#=Sqr(dx*dx+dy*dy)
		dx=dx/l
		dy=dy/l
		an#=ATan2(dy,dx)
		If dy=0 Then an#=Sgn(dx)*90-90
		lengths[0]=l
		diffs[0]=dx
		diffs[1]=dy
		pdiffs[0]=Cos(an+90)
		pdiffs[1]=Sin(an+90)
		
		dx#=points[4]-points[2]
		dy#=points[5]-points[3]
		l#=Sqr(dx*dx+dy*dy)
		dx=dx/l
		dy=dy/l
		an#=ATan2(dy,dx)
		If dy=0 Then an#=Sgn(dx)*90-90
		lengths[1]=l
		diffs[2]=dx
		diffs[3]=dy
		pdiffs[2]=Cos(an+90)
		pdiffs[3]=Sin(an+90)
		
		dx#=points[0]-points[4]
		dy#=points[1]-points[5]
		l#=Sqr(dx*dx+dy*dy)
		dx=dx/l
		dy=dy/l
		an#=ATan2(dy,dx)
		If dy=0 Then an#=Sgn(dx)*90-90
		lengths[2]=l
		diffs[4]=dx
		diffs[5]=dy
		pdiffs[4]=Cos(an+90)
		pdiffs[5]=Sin(an+90)
	End Method
	
	Method Draw()
		SetColor 50,0,150
		SetLineWidth 3
		For i=0 To 2
			If drawedge[i]
				DrawLine points[i*2],points[i*2+1],points[(i*2+2) Mod 6],points[(i*2+3) Mod 6]
			EndIf
		Next
		SetAlpha .3
		DrawPoly points
		SetAlpha 1
		SetLineWidth 1
		
	End Method
	
	Method inside(x#,y#)
		in=pointintriangle(x,y,points[0],points[1],points[2],points[3],points[4],points[5])
		Return in
	End Method
End Type

Type flash
	Field x#,y#
	Field Size#,maxsize#
	Field Wait
	
	Function Create:flash(x#,y#,Size#,Wait=0)
		f:flash=New flash
		f.x=x
		f.y=y
		f.Size=Size
		f.maxsize=f.Size
		f.Wait=Wait
		flashes.AddLast f
		Return f
	End Function
	
	Method update()
		If Wait>0
			Wait:-1
		Else
			bright#=Size/maxsize
			SetAlpha bright*.5
			SetColor bright*255,(1-bright)*255,0
			DrawOval x-Size,y-Size,2*Size,2*Size
			Size:-1
			If Size<=0
				flashes.Remove Self
			EndIf
		EndIf
	End Method
End Type

Type scoreflash
	Field x#,y#
	Field life,maxlife,bigscale#
	Field value
	
	Function Create:scoreflash(x#,y#,value#)
		sf:scoreflash=New scoreflash
		sf.x=x
		sf.y=y
		sf.life=30+value*.1
		If sf.life>100 Then sf.life=100
		sf.maxlife=sf.life
		sf.bigscale=value*.005+.5
		If sf.bigscale>3 Then sf.bigscale=3
		sf.value=value
		scoreflashes.addlast sf
		Return sf
	End Function
	
	Method update()
		SetColor 100,100,100
		y:-1
		life:-1
		scale#=Sin(180*Float(life)/maxlife)
		SetAlpha Float(life)/maxlife
		SetScale bigscale*scale,bigscale*scale
		DrawText value,x,y-30*scale*bigscale
		SetScale 1,1
		
		If life<=0
			scoreflashes.remove Self
		EndIf
	End Method
End Type

Type sounditem
	Field chan:TChannel
	
	Function Create:sounditem(sound:TSound,rate#,pan#,volume#)
		s:sounditem=New sounditem
		s.chan=AllocChannel()
		SetChannelRate(s.chan,rate)
		SetChannelPan(s.chan,pan)
		SetChannelVolume s.chan,volume
		PlaySound(sound,s.chan)
		sounditems.AddLast s
		Return s
	End Function
	
	Method Delete()
		StopChannel chan
	End Method
End Type

Type vector
	Field x#,y#
	Function Create:vector(x#,y#)
		v:vector=New vector
		v.x=x
		v.y=y
		Return v
	End Function
End Type

Type netclient
	Field state
	Field socket:TSocket
	Field stream:tsocketstream
	Field netid
	Field name$
	Field theirsweep:sweep
	
	Method New()
		netclients.addlast Self
	End Method
	
	Method addsweep(red,green,blue,netid)
		theirsweep=sweep.Create(120,red,green,blue,netid)
		theirsweep.catchup=.5
	End Method
	
	Method die()
		If theirsweep
			sweeps.remove theirsweep
		EndIf
		netclients.remove Self
	End Method	
End Type

Type netserver
	Field socket:tsocket
	Field netids
	Field clients:TList
	Field state
	
	Function Create:netserver(port=6791)
		netclients:TList=New TList

		s:netserver=New netserver
		s.socket=CreateTCPSocket()
		s.clients=New TList
		BindSocket s.socket,port
		SocketListen s.socket
		Print "server running on port "+String(port)
		Return s
	End Function

	Method update()
		csocket:tsocket=SocketAccept(socket)
		If csocket
			acceptclient(csocket)
		EndIf

		For c:netclient=EachIn clients
			If Not SocketConnected(c.socket)
				'Arg, it's dead!
				clients.remove c
				c.die()
				obitclient(c)
			EndIf
		Next
		
		For c:netclient=EachIn clients
			If SocketReadAvail(c.socket)
				command=ReadInt(c.stream)
				'Print String(c.netid)+" says something"
				Select command
				Case HELLO
					Print "HELLO!"
					theirversion$=ReadLine(c.stream)
					If theirversion<>version
						netwriteInt c.stream,WRONGVERSION
						CloseSocket(c.socket)
						clients.remove c
					Else
						netwriteInt c.stream,RIGHTVERSION
						announceclient(c)
					EndIf
				Case GOODBYE
					clients.remove c
					c.die()
					obitclient(c)
					netwriteInt c.stream,GOODBYE
				Case CLIENTNAME
					Print "NAME!"
					name$=ReadLine(c.stream)
					setname(c,name)
					announcename(c)
					
					initclient(c)					

					'now send details of all clients, sweeps, walls, things, etc.
					For oc:netclient=EachIn clients
						If c<>oc
							tellclient(c,oc)
							tellname(c,oc)
							If oc.theirsweep
								tellsweep(c,oc)
							EndIf
						EndIf
					Next
					
					For bot:ai=EachIn bots
						tellclient(c,bot.mynetclient)
						tellname(c,bot.mynetclient)
						tellsweep(c,bot.mynetclient)
					Next
					
					If state=1
						Print "telling to start"
						tellstart(c)
					EndIf
					
				Case NEWSWEEP
					Print "Setting sweep colours, creating sweep"
					red=ReadInt(c.stream)
					green=ReadInt(c.stream)
					blue=ReadInt(c.stream)
					c.addsweep(red,green,blue,getnetid())
					announcesweep(c)
					announcename(c)
				Case SWEEPPOS
					If c.theirsweep
						c.theirsweep.targx#=ReadFloat(c.stream)
						c.theirsweep.targy#=ReadFloat(c.stream)
						tellsweeppos(c)
					EndIf
				Case THINGPOS
					netid=ReadInt(c.stream)
					x#=ReadFloat(c.stream)
					y#=ReadFloat(c.stream)
					vx#=ReadFloat(c.stream)
					vy#=ReadFloat(c.stream)
					For t:thing=EachIn things
						If t.netid=netid
							t.x=x
							t.y=y
							t.vx=vx
							t.vy=vy
						EndIf
					Next
					tellthingpos(t,c)
				Case THINGDEAD
					netid=ReadInt(c.stream)
					distancefromepicentre=ReadInt(c.stream)
					scores=ReadInt(c.stream)
					For t:thing=EachIn things
						If t.netid=netid
							t.distancefromepicentre=distancefromepicentre
							t.scores=scores
							t.die()
							obitthing(t)
						EndIf
					Next
				Case CLIENTSCORE
					If c.theirsweep
						score=ReadInt(c.stream)
						c.theirsweep.score=score
						tellscore(c)
					EndIf
				End Select
			EndIf
		Next

		Rem		
		If tick Mod 20 = 0
			If things.count()
				t:thing=thing(choice(things))
				tellthingpos(t)
			EndIf
		EndIf
		EndRem
		
		If tick Mod netfrequency = 0
			For bot:ai=EachIn bots
				tellsweeppos(bot.mynetclient)
			Next
		EndIf
		
		Select state
		Case 0 'not started
			DrawText "Press S to start the game",0,0
			If KeyHit(KEY_S)
				gamespeed=1
				state=1
				starttime=MilliSecs()
				announcestart()
			EndIf
		Case 1
			DrawText "Game running",0,0
		End Select
	End Method
	
	Method initgame()
		For c:netclient=EachIn clients
			initclient(c)
		Next
		
		For bot:ai=EachIn bots
			If Not bot.mynetclient
				Print "announcing bot "+bot.itssweep.name
				bot.mynetclient=New netclient
				bot.mynetclient.theirsweep=bot.itssweep
				bot.mynetclient.name=bot.itssweep.name
				bot.mynetclient.netid=getnetid()
				announceclient(bot.mynetclient)
				announcesweep(bot.mynetclient)
				announcename(bot.mynetclient)
			EndIf
		Next
		
		For c:netclient=EachIn clients
			If c.theirsweep
				Print "reannouncing "+c.theirsweep.name
				osweep:sweep=c.theirsweep
				c.addsweep(osweep.red,osweep.green,osweep.blue,osweep.netid)
				c.theirsweep.name=osweep.name
				announcesweep(c)
				sweeps.remove osweep
			EndIf
		Next
	End Method
	
	Method initclient(c:netclient)
		netwriteInt c.stream,RESETGAME
		For w:wall=EachIn walls
			tellwall(c,w)
		Next
		
		For t:thing=EachIn things
			tellthing(c,t)
		Next
	End Method
		
	Method telltime()
		For c:netclient=EachIn clients
			netwriteInt c.stream,TICKTIME
			netwriteFloat c.stream,timeleft
		Next
	End Method
	
	Method announcestart()
		For c:netclient=EachIn clients
			tellstart(c)
		Next
	End Method
	
	Method tellstart(c:netclient)
		netwriteInt c.stream,GAMESTART	
	End Method
	
	Method acceptclient(socket:tsocket)
		c:netclient=New netclient
		c.socket=socket
		c.stream=CreateSocketStream(c.socket)
		c.netid=getnetid()
		clients.addlast c
		Print "client (netid "+String(c.netid)+") connected"
		netwriteInt c.stream,HELLO
		netwriteInt c.stream,c.netid
		netwriteLine c.stream,version
	End Method

	Method getnetid()
		netids:+1
		Return netids
	End Method

	Method tellclient(c:netclient,oc:netclient)
		Print "telling "+String(c.netid)+" about client "+String(oc.netid)
		netwriteInt c.stream,NEWCLIENT
		netwriteInt c.stream,oc.netid
	End Method
	
	Method announceclient(newc:netclient)
		Print "Announcing client "+String(newc.netid)+" ("+newc.name+")"

		For c:netclient=EachIn clients
			If c<>newc
				tellclient(c,newc)
			EndIf
		Next
	End Method
	
	Method obitclient(c:netclient)
		For oc:netclient=EachIn clients
			netwriteInt oc.stream,CLIENTDEAD
			netwriteInt oc.stream,c.netid
		Next
	End Method
	
	Method setname(c:netclient,oname$)
		clashcount=0
		newname$=oname
		For oc:netclient=EachIn clients
			If newname=oc.name
				clashcount:+1
				newname=oname+" ("+String(clashcount)+")"
			EndIf
		Next
		c.name=newname
		If c.theirsweep
			c.theirsweep.name=newname
		EndIf
	End Method

	Method announcename(oc:netclient)
		Print "Announcing new name "+String(oc.netid)+": "+oc.name
		For c:netclient=EachIn clients
			stream:tsocketstream=c.stream
			tellname(c,oc)
		Next
	End Method
	
	Method tellname(c:netclient,oc:netclient)
		netwriteInt c.stream,CLIENTNAME
		netwriteInt c.stream,oc.netid
		netwriteLine c.stream,oc.name
	End Method

	Method tellsweep(c:netclient,oc:netclient)
		netwriteInt c.stream,NEWSWEEP
		netwriteInt c.stream,oc.netid
		netwriteInt c.stream,oc.theirsweep.netid
		netwriteInt c.stream,oc.theirsweep.red
		netwriteInt c.stream,oc.theirsweep.green
		netwriteInt c.stream,oc.theirsweep.blue
	End Method
	
	Method announcesweep(oc:netclient)
		For c:netclient=EachIn clients
			tellsweep(c,oc)
		Next
	End Method
	
	Method tellsweeppos(oc:netclient)
		For c:netclient=EachIn clients
			If c<>oc
				netwriteInt c.stream,SWEEPPOS
				netwriteInt c.stream,oc.theirsweep.netid
				netwriteFloat c.stream,oc.theirsweep.targx
				netwriteFloat c.stream,oc.theirsweep.targy
			EndIf
		Next
	End Method
	
	Method tellwall(c:netclient,w:wall)
		netwriteInt c.stream,NEWWALL
		For i=0 To 5
			netwriteFloat c.stream,w.points[i]
		Next
		For i=0 To 2
			netwriteInt c.stream,w.drawedge[i]
		Next
	End Method
	
	Method tellthing(c:netclient,t:thing)
		netwriteInt c.stream,NEWTHING
		netwriteInt c.stream,t.netid
		netwriteInt c.stream,t.powerup
		netwriteFloat c.stream,t.x
		netwriteFloat c.stream,t.y
		netwriteFloat c.stream,t.vx
		netwriteFloat c.stream,t.vy
	End Method
	
	Method announcething(t:thing)
		t.netid=getnetid()
		
		For c:netclient=EachIn clients
			tellthing(c,t)
		Next
	End Method
	
	Method tellthingpos(t:thing,oc:netclient=Null)
		For c:netclient=EachIn clients
			If c<>oc
				netwriteInt c.stream,THINGPOS
				netwriteInt c.stream,t.netid
				netwriteFloat c.stream,t.x
				netwriteFloat c.stream,t.y
				netwriteFloat c.stream,t.vx
				netwriteFloat c.stream,t.vy
			EndIf
		Next
	End Method
	
	Method obitthing(t:thing)
		For c:netclient=EachIn clients
			netwriteInt c.stream,THINGDEAD
			netwriteInt c.stream,t.netid
			netwriteInt c.stream,t.distancefromepicentre
			netwriteInt c.stream,t.scores
		Next
	End Method
	
	Method tellscore(oc:netclient)
		For c:netclient=EachIn clients
			If c<>oc
				netwriteInt c.stream,CLIENTSCORE
				netwriteInt c.stream,c.netid
				netwriteInt c.stream,c.theirsweep.score
			EndIf
		Next
	End Method
	
	Method tellendround()
		For c:netclient=EachIn clients
			netwriteInt c.stream,ENDROUND
		Next
	End Method
	
	Method bottellscore(bot:ai)
		For c:netclient=EachIn clients
			netwriteInt c.stream,CLIENTSCORE
			netwriteInt c.stream,bot.mynetclient.netid
			netwriteInt c.stream,bot.itssweep.score
		Next				
	End Method
	
	Method reset()
		state=0
		For bot:ai=EachIn bots
			sweeps.remove bot.itssweep
			netclients.remove bot.mynetclient
		Next
		bots=New TList
	End Method
	
	Method disconnect()
		st=MilliSecs()
		For c:netclient=EachIn clients
			netwriteInt c.stream,GOODBYE
		Next
		
		While 1
			For c=EachIn clients
				If SocketReadAvail(c.socket)
					command=ReadInt(c.stream)
					If command=GOODBYE
						clients.remove c
						c.die()
						netwriteInt c.stream,GOODBYE
					EndIf
				EndIf
			Next
			If Not clients.count() Then Return
			If MilliSecs()-st>5000 Then Return
			DrawText "Ending Server...",0,0
			Flip
			Cls
		Wend
		netclients=New TList

		bots=New TList
		sweeps=New TList
		things=New TList
		groups=New TList
		flashes=New TList
		scoreflashes=New TList
		booms=New TList
		walls=New TList
		sounditems=New TList
	End Method
End Type

Type localclient
	Field socket:tsocket
	Field stream:tsocketstream
	Field mynetclient:netclient
	Field state
	Field name$,red,green,blue
	
	Function Create:localclient(ip$,port,name$,red,green,blue)
		netclients:TList=New TList
	
		Print "connecting to "+ip+":"+String(port)
		address=dotted_to_integer(ip)
		lc:localclient=New localclient
		lc.socket=CreateTCPSocket()
		ConnectSocket lc.socket,address,port
		
		If lc.socket
			lc.stream=CreateSocketStream(lc.socket)
			Print "connected!"
			
			lc.name=name
			lc.red=red
			lc.green=green
			lc.blue=blue
			Return lc
		Else
			Print "couldn't connect"
			Return Null
		EndIf
	End Function
	
	Method update()
		If Not SocketConnected(socket)
			state=-1
			Return
		EndIf
		
		If SocketReadAvail(socket)
			command=ReadInt(stream)
			Select command
			Case HELLO
				c:netclient=New netclient
				mynetclient=c
				c.netid=ReadInt(stream)
				theirversion$=ReadLine(stream)
				Print "Connected to a server running version "+theirversion
				
				netwriteInt stream,HELLO
				netwriteLine stream,version
			Case GOODBYE
				disconnect()
			Case WRONGVERSION
				Print "Wrong version!"
				state=-1
			Case RIGHTVERSION
				Print "Correct version"
				netwriteInt stream,CLIENTNAME
				netwriteLine stream,name
				
				netwriteInt stream,NEWSWEEP
				netwriteInt stream,red
				netwriteInt stream,green
				netwriteInt stream,blue
			Case NEWCLIENT
				netid=ReadInt(stream)
				Print "New client ("+String(netid)+") has joined"
				c:netclient=New netclient
				c.netid=netid
			Case CLIENTNAME
				netid=ReadInt(stream)
				name$=ReadLine(stream)
				Print "Client "+String(netid)+" changed name to "+name
				For c:netclient=EachIn netclients
					If c.netid=netid
						c.name=name
						If c.theirsweep
							c.theirsweep.name=name
						EndIf
					EndIf
				Next
			Case NEWSWEEP
				clientid=ReadInt(stream)
				sweepid=ReadInt(stream)
				Print "New sweep for "+String(clientid)+" with id "+sweepid
				red=ReadInt(stream)
				green=ReadInt(stream)
				blue=ReadInt(stream)
				
				For c:netclient=EachIn netclients
					Print String(c.netid)+" - "+String(clientid)
					If c.netid=clientid
						c.addsweep(red,green,blue,sweepid)
						c.theirsweep.name=c.name
						Print c.theirsweep.netid
					EndIf
				Next
				If clientid=mynetclient.netid
					'this is my sweep
					mysweep=mynetclient.theirsweep
				EndIf
			Case NEWWALL
				Print "New wall"
				Local points#[6]
				For i=0 To 5
					points[i]=ReadFloat(stream)
				Next
				Local drawedge[3]
				For i=0 To 2
					drawedge[i]=ReadInt(stream)
				Next
				wall.Create(points,drawedge)
			Case NEWTHING
				netid=ReadInt(stream)
				powerup=ReadInt(stream)
				x#=ReadFloat(stream)
				y#=ReadFloat(stream)
				vx#=ReadFloat(stream)
				vy#=ReadFloat(stream)
				t:thing=thing.Create(x,y,powerup)
				t.netid=netid
				t.vx=vx
				t.vy=vy
			Case SWEEPPOS
				netid=ReadInt(stream)
				x#=ReadFloat(stream)
				y#=ReadFloat(stream)
				For s:sweep=EachIn sweeps
					If s.netid=netid
						s.targx=x
						s.targy=y
					EndIf
				Next
			Case THINGPOS
				netid=ReadInt(stream)
				x#=ReadFloat(stream)
				y#=ReadFloat(stream)
				vx#=ReadFloat(stream)
				vy#=ReadFloat(stream)
				For t:thing=EachIn things
					If t.netid=netid
						t.x=x
						t.y=y
						t.vx=vx
						t.vy=vy
					EndIf
				Next
			Case THINGDEAD
				netid=ReadInt(stream)
				distancefromepicentre=ReadInt(stream)
				scores=ReadInt(stream)
				For t:thing=EachIn things
					If t.netid=netid
						t.distancefromepicentre=distancefromepicentre
						t.scores=scores
						t.die()
					EndIf
				Next
			Case CLIENTDEAD
				netid=ReadInt(stream)
				Print "Client "+String(netid)+" disconnected"
				For c:netclient=EachIn netclients
					If c.netid=netid
						c.die()
					EndIf
				Next
			Case CLIENTSCORE
				netid=ReadInt(stream)
				score=ReadInt(stream)
				For c:netclient=EachIn netclients
					If c.netid=netid
						If c.theirsweep
							c.theirsweep.score=score
						EndIf
					EndIf
				Next
			Case TICKTIME
				timeleft=ReadFloat(stream)
			Case GAMESTART
				starttime=MilliSecs()
				gamespeed=1
			Case RESETGAME 'game restarting
				Print "running game"
				sweeps=New TList
				rungame()
				Print "showing scores"
				mysweep=Null
				mymenu.reset()
				mymenu=clientscoresmenu
				mymenu.open()
				prevmenu=mainmenu
			Case ENDROUND 'round ended
				timeleft=-1
			End Select			
		EndIf
		
		If mysweep
			If tick Mod netfrequency = 0
				netwriteInt stream,SWEEPPOS
				netwriteFloat stream,mysweep.targx
				netwriteFloat stream,mysweep.targy
			EndIf
		EndIf
	End Method
	
	Method tellthing(t:thing)
		netwriteInt stream,THINGPOS
		netwriteInt stream,t.netid
		netwriteFloat stream,t.x
		netwriteFloat stream,t.y
		netwriteFloat stream,t.vx
		netwriteFloat stream,t.vy
	End Method
	
	Method obitthing(t:thing)
		netwriteInt stream,THINGDEAD
		netwriteInt stream,t.netid
		netwriteInt stream,t.distancefromepicentre
		netwriteInt stream,t.scores
	End Method
	
	Method tellscore()
		netwriteInt stream,CLIENTSCORE
		netwriteInt stream,mynetclient.theirsweep.score
	End Method
	
	Method disconnect()
		netwriteInt stream,GOODBYE
		Print "disconnecting"
		st=MilliSecs()
		While 1
			If Not SocketConnected(socket)
				Exit
			EndIf
			If SocketReadAvail(socket)
				command=ReadInt(stream)
				If command=GOODBYE
					Exit
				EndIf
			EndIf
			If MilliSecs()-st>5000 Then Exit
			DrawText "Waiting to die...",0,0
			Flip
			Cls
		Wend
		netclients=Null

		bots=New TList
		sweeps=New TList
		things=New TList
		groups=New TList
		flashes=New TList
		scoreflashes=New TList
		booms=New TList
		walls=New TList
		sounditems=New TList
	End Method
End Type

Type ai
	Field itssweep:sweep
	Field state
	Field speed#
	'Field netid
	Field startx#,starty#,cx#,cy#,tx#,ty#,dx#,dy#,d#
	Field a:thing,b:thing
	Field mynetclient:netclient
	
	Function Create:ai(name$,red,green,blue,speed#)
		bot:ai=New ai
		bots.addlast bot
		If server Then netid=server.getnetid() Else netid=0
		bot.itssweep=sweep.Create(120,red,green,blue,netid,name)
		bot.itssweep.isbot=1
		bot.itssweep.targx=Rand(screenwidth)
		bot.itssweep.targy=Rand(screenheight)
		bot.itssweep.owner=bot
		bot.speed=speed
		Return bot
	End Function
	
	Method update()
		
		If state
			Rem
			tx#=0
			ty#=0
			For t:thing=EachIn tg.members
				tx:+t.x
				ty:+t.y
			Next
			tx:/tg.members.count()
			ty:/tg.members.count()
			cx#=0
			cy#=0
			For t:thing=EachIn g.members
				cx:+t.x
				cy:+t.y
			Next
			cx:/g.members.count()
			cy:/g.members.count()
			EndRem
			
			tx=a.x
			ty=a.y
			cx=b.x
			cy=b.y
			If Not (things.contains(a) And things.contains(b)) Then state=0
			
			'DrawLine cx,cy,tx,ty
			
			dx#=tx-cx
			dy#=ty-cy
			d#=Sqr(dx*dx+dy*dy)
			dx:/d
			dy:/d
			startx#=cx-dx*120+Rand(-10,10)
			starty#=cy-dy*120+Rand(-10,10)
		EndIf	
	
		Select state
		Case 0 'not sweeping
			If groups.count()<2 Then Return
			glist:TList=groups.copy()
			glist.sort()
			
			Rem
			inlist:TList=New TList
			c=glist.count()-1
			start=c
			maxi=group(glist.last()).members.count()
			num=group(glist.valueatindex(c)).members.count()
			While num=maxi
				inlist.addlast glist.valueatindex(c)
				num=group(glist.valueatindex(c)).members.count()
				If num=0
					c:+1
					Exit
				EndIf
				c:-1
				If c<=0 Then Exit
			Wend
			If c<0 Then Return
			'g:group=group(glist.valueatindex(Rand(c,start)))
			'glist.remove g
			'tg:group=group(glist.valueatindex(Rand(c,start-1)))
			For g:group=EachIn inlist
				DrawText g.members.count(),thing(g.members.first()).x,thing(g.members.first()).y
			Next
			g:group=group(choice(inlist))
			inlist.remove g
			tg:group=group(choice(inlist))
			End Rem
			
			total=0
			For og:group=EachIn glist
				out=0
				For t:thing=EachIn og.members
					If wormhole(t)
						glist.remove og
						out=1
					EndIf
				Next
				If Not out
					num=og.members.count()
					total:+num*num
				EndIf
			Next
			p1=Rand(1,total)
			p2=Rand(1,total)
			c=0
			g:group=Null
			tg:group=Null
			For og:group=EachIn glist
				num=og.members.count()
				c:+num*num
				If p1<=c And g=Null Then g=og
				If p2<=c And tg=Null Then tg=og
			Next
			
			If g=tg Then Return

			a:thing=thing(choice(g.members))
			b:thing=thing(choice(tg.members))
			state=1
			itssweep.catchup=0
		Case 1 'moving to start
			If Rand(1000)=1 Then state=1
			'itssweep.catchup=.5*speed
			itssweep.catchup:+.05*speed
			itssweep.targx=startx
			itssweep.targy=starty
			dist#=((itssweep.x-startx)^2+(itssweep.y-starty)^2)
			If dist<25
				state=2
				itssweep.catchup=.12'*speed
				itssweep.targx:+dx*80+Rnd(-5,5)
				itssweep.targy:+dy*80+Rnd(-5,5)
			EndIf
		Case 2 'getting ready
			dist#=((itssweep.x-itssweep.targx)^2+(itssweep.y-itssweep.targy)^2)
			If dist<25
				state=3
				itssweep.catchup=0
			EndIf
		Case 3 'sweeping
			itssweep.targx=tx+Rand(-10,10)
			itssweep.targy=ty+Rand(-10,10)
			itssweep.catchup:+.02*speed
			itssweep.push()
			dist#=((itssweep.x-tx)^2+(itssweep.y-ty)^2)
			If dist<25
				state=0
				smalldist#=-1
				closest:group=Null
				For g:group=EachIn groups
					gx#=0
					gy#=0
					For t:thing=EachIn g.members
						gx:+t.x
						gy:+t.y
					Next
					gx:/g.members.count()
					gy:/g.members.count()
					dist#=((gx-itssweep.x)^2+(gy-itssweep.y)^2)
					If dist<smalldist Or smalldist=-1
						closest=g
						smalldist=dist
					EndIf
				Next
				If Not closest Then Return
				'If closest.members.count()>4 Or groups.count()<4 Or Rand(5)=1
					itssweep.clickypop()
				'EndIf
			EndIf
		End Select
		
		'SetLineWidth 3
		'DrawLine cx,cy,tx,ty
		'DrawLine itssweep.x,itssweep.y,itssweep.targx,itssweep.targy
		'SetLineWidth 1
	End Method
End Type



Type triangle
	Field points:thing[]
	
	Function Create:triangle(points:thing[])
		t:triangle=New triangle
		t.points=points
		Return t
	End Function
End Type

Function quickhull:TList(s:TList)
	If s.count()<=3 Return s
	l:thing=Null
	r:thing=Null
	For p:thing=EachIn s
		If l=Null
			l=p
		ElseIf p.x<l.x
			l=p
		EndIf
		If r=Null
			r=p
		ElseIf p.x>r.x
			r=p
		EndIf
	Next
	
	an#=ATan2(r.y-l.y,r.x-l.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=Cos(an+90)
	sy#=Sin(an+90)
	
	s1:TList=New TList
	s2:TList=New TList
	For p:thing=EachIn s
		If p<>l And p<>r
			mu#=(l.y-p.y+(ry/rx)*(p.x-l.x))/(sy-sx*ry/rx)
			If mu<0 
				s1.addlast p 
			ElseIf mu>0
				s2.addlast p
			EndIf
		EndIf
	Next
	
	out1:TList=findhull(s1,l,r)
	out2:TList=findhull(s2,r,l)
	out:TList=New TList
	If out1
		For o:Object=EachIn out1
			out.addlast o
		Next
	EndIf
	If out2
		For o:Object=EachIn out2
			out.addlast o
		Next
	EndIf
	
	outwalls:TList=New TList
	For t:triangle=EachIn out
		Local drawedge[3]
		For i=0 To 2
			a:thing=t.points[i]
			b:thing=t.points[(i+1) Mod 3]
			go=1
			For ot:triangle=EachIn out
				goa=1
				gob=1
				If ot<>t
					For ii=0 To 2
						If ot.points[ii]=a goa=0
						If ot.points[ii]=b gob=0
					Next
				EndIf
				If Not (goa Or gob) go=0
			Next
			If go
				drawedge[i]=1 
			Else 
				drawedge[i]=0
			EndIf
		Next
		Local wallpoints#[]=[t.points[0].x,t.points[0].y,t.points[1].x,t.points[1].y,t.points[2].x,t.points[2].y]
		outwalls.addlast wall.Create(wallpoints,drawedge)
	Next
	Return outwalls
End Function

Function findhull:TList(sk:TList,p:thing,q:thing)
	If Not sk.count() Return Null
	c:thing=Null
	out:TList=New TList
	maxdist#=-1
	an#=ATan2(q.y-p.y,q.x-p.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=-ry
	sy#=rx
	For tp:thing=EachIn sk
		If tp<>p And tp<>q
			mu#=(p.y-tp.y+(ry/rx)*(tp.x-p.x))/(sy-sx*ry/rx)
			If maxdist=-1 Or Abs(mu)>maxdist
				c=tp
				maxdist=Abs(mu)
			EndIf
		EndIf
	Next
	out.addlast triangle.Create([p,q,c])
	an#=ATan2(c.y-p.y,c.x-p.x)
	rx#=Cos(an)
	ry#=Sin(an)
	sx#=Cos(an+90)
	sy#=Sin(an+90)
	s1:TList=New TList
	s2:TList=New TList
	For tp:thing=EachIn sk
		If tp<>c
			If Not pointintriangle(tp.x,tp.y,p.x,p.y,q.x,q.y,c.x,c.y)
				mu#=(p.y-tp.y+(ry/rx)*(tp.x-p.x))/(sy-sx*ry/rx)
				If mu<0 s1.addlast tp ElseIf mu>0 s2.addlast tp
			EndIf
		EndIf
	Next
	out1:TList=findhull(s1,p,c)
	out2:TList=findhull(s2,c,q)
	If out1
		For o:Object=EachIn out1
			out.addlast o
		Next
	EndIf
	If out2
		For o:Object=EachIn out2
			out.addlast o
		Next
	EndIf
	Return out
End Function



Function Dotted_to_integer(Dotted_ip$)
		Dotted_ip$:+"."
		Local octet:String[4]
		For i = 0 To 3
			dot =  Dotted_ip$.find(".") ; octet[i] =  Dotted_ip$[0..dot]
			Dotted_ip$ = Dotted_ip$[dot+1..]
		Next
	Return (octet[0].toint() Shl 24) + (octet[1].toint() Shl 16) + (octet[2].toint() Shl 8) + octet[3].toint()
End Function 

Function updatemouse()
	If tabletmode Or lockmouse=0
		mx=MouseX()-originx
		my=MouseY()-originy
	Else
		mx:+MouseX()-width/2
		my:+MouseY()-height/2
		MoveMouse width/2,height/2
	EndIf
	If mx<0 Then mx=0
	If mx>screenwidth Then mx=screenwidth
	If my<0 Then my=0
	If my>screenheight Then my=screenheight
End Function

Function getmousex#()
	Return MouseX()-originx
End Function

Function getmousey#()
	Return MouseY()-originy
End Function

Function pick:Object(probs#[],values:Object[])
	total#=0
	For p#=EachIn probs
		total:+p
	Next
	r#=Rnd(total)
	sum#=0
	c=0
	While sum<r
		value:Object=values[c]
		sum:+probs[c]
		c:+1
	Wend
	Return value
End Function

Function pickint(probs#[],values[])
	total#=0
	For p#=EachIn probs
		total:+p
	Next
	r#=Rnd(total)
	sum#=0
	c=0
	While sum<r
		value=values[c]
		sum:+probs[c]
		c:+1
	Wend
	Return value
End Function


Function sameside(p1x#,p1y#,p2x#,p2y#,ax#,ay#,bx#,by#)
	cp1# = (bx-ax)*(p1y-ay)-(p1x-ax)*(by-ay)
	cp2# = (bx-ax)*(p2y-ay)-(p2x-ax)*(by-ay)
	If cp1*cp2 >= 0 Then Return True
End Function	
	
Function pointintriangle(px#,py#,ax#,ay#,bx#,by#,cx#,cy#)
	If sameside(px,py,ax,ay,bx,by,cx,cy) And sameside(px,py,bx,by,ax,ay,cx,cy) And sameside(px,py,cx,cy,ax,ay,bx,by)
		Return True
	Else
		Return False
	EndIf
End Function

Function choice:Object(l:TList)
	n=l.Count()
	If n=1 Then Return l.first()
	p=Rand(n-1)
	Return l.valueatindex(p)
End Function

Function netwriteint(s:TStream,val)
	If s=Null Then Return
	If Not Eof(s)
		WriteInt s,val
	EndIf
End Function

Function netwritefloat(s:TStream,val#)
	If Not Eof(s)
		WriteFloat s,val
	EndIf
End Function

Function netwriteline(s:TStream,val$)
	If Not Eof(s)
		WriteLine s,val
	EndIf
End Function

Function initgame(filename$)
	repop=50
	minrepop=20
	repopdecay=.99
	roundlength=60
	radius=80

	loadfile(filename)

	tick=0
	repoptick=0
	gamespeed=1
End Function

Function levelfile$(name$)
	Return "levels\"+name+".txt"
End Function

Function loadfile(filename$)
	filename=levelfile(filename)
	If Not FileType(filename) Return
	f:TStream=ReadFile(filename)
	While Not Eof(f)
		Line:TList=splitstring(ReadLine(f))
		If String(line.first())[0] <> "'"
			command$=String(line.first())
			Select command
			Case "roundlength"
				roundlength=Int(String(line.valueatindex(1)))
			Case "repop"
				repop=Float(String(line.valueatindex(1)))
				minrepop=Float(String(line.valueatindex(2)))
				repopdecay=Float(String(line.valueatindex(3)))
			Case "radius"
				radius=Int(String(line.valueatindex(1)))
			Case "wall"
				Local wallpoints#[6]
				Local drawedge[3]
				For c=0 To 5
					wallpoints[c]=Float(String(line.valueatindex(c+1)))
				Next
				For c=0 To 2
					drawedge[c]=Int(String(line.valueatindex(c+7)))
				Next
				wall.Create(wallpoints,drawedge)
			Case "bot"
				name$=String(line.valueatindex(1))
				red=Int(String(line.valueatindex(2)))
				green=Int(String(line.valueatindex(3)))
				blue=Int(String(line.valueatindex(4)))
				speed#=Float(String(line.valueatindex(5)))
				ai.Create(name,red,green,blue,speed)
			End Select
		EndIf
	Wend
	CloseStream f
End Function

Function splitstring:TList(in$)
	c=0
	in.Trim()
	out:TList=New TList
	While c<in.length
		If in[c]=32
			out.addlast in[0..c]
			in=in[c+1..in.length]
			c=0
		EndIf
		c:+1
	Wend
	If in.length Then out.addlast in
	Return out
End Function

Function rungame(levelfile$="test",playername$="Player",difficulty#=1.0)
	'bots:TList=New TList
	'sweeps:TList=New TList
	'things:TList=New TList
	'groups:TList=New TList
	'flashes:TList=New TList
	'scoreflashes:TList=New TList
	'booms:TList=New TList
	'walls:TList=New TList
	'sounditems:TList=New TList
	
	ingamemenustate=0
	
	mousestate=0
	mstartx#=0
	mstarty#=0
	mx=screenwidth/2
	my=screenheight/2
	
	controlmode=1 '1 means run stuff locally, 0 means stuff is run by server
	
	gamespeed=0
	
	mysweep:sweep=Null

	Select netmode
	Case 0 'local game
		mysweep:sweep=sweep.Create(120,0,255,0,0,playername)
		HideMouse
		lockmouse=True
	
		initgame(levelfile)
	Case 1 'server - no local player
		ShowMouse
		initgame(levelfile)
		server.initgame()
		gamespeed=0
	Case 2 'client
		controlmode=0
		HideMouse
		lockmouse=True
	End Select

	For bot:ai=EachIn bots
		bot.speed:*difficulty
	Next
	
	'ovx#=0
	'ovy#=0


	starttime=MilliSecs()
	time=starttime
	tickmod=starttime
	timegap=0
	oldtimeleft#=0
	timeleft#=0
	finished=0
	fps#=1000.0/50.0
	nextframe=time+fps

	While Not finished
		Cls
		
	'////////////UPDATE NET STUFF///////////
		Select netmode
		Case 1
			server.update()
		Case 2
			myclient.update()
			If myclient.state<0 
				Print "Finished "+String(myclient.state)
				Return myclient.state			EndIf
		End Select

	'//////////////SERVER STUFF////////////
		If controlmode
			
			'timer for creating new things
			repoptick:+gamespeed
			If repoptick>=repop/sweeps.count()
				repop:*repopdecay
				If repop<minrepop Then repop=minrepop
				inside=1
				While inside
					x#=Rand(10,screenwidth-10)
					y#=Rand(10,screenheight-10)
					inside=0
					For w:wall=EachIn walls
						If w.inside(x,y) Then inside=1
					Next
				Wend
				t:thing=thing.Create(x,y)
				If netmode=1
					server.announcething(t)
				EndIf
				repoptick=0
			EndIf
		EndIf


	'//////////////CLIENT STUFF////////////
		updatemouse()

		If mysweep And ingamemenustate=0
			mysweep.targx=mx
			mysweep.targy=my
			
			If MouseHit(2) Or (MouseHit(1) And alwayspush)
				mysweep.clickypop()
			EndIf
			
			If alwayspush
				mysweep.push()
			Else
				If MouseDown(1)
					If mousestate=0
						mousestate=1
						mstartx=mx
						mstarty=my
					EndIf
					mysweep.push()
				Else
					If mousestate=1
						d#=(mx-mstartx)^2+(my-mstarty)^2        
						If d<200 Or poponrelease
							mysweep.clickypop()
						EndIf
						mousestate=0
					EndIf
				EndIf
			EndIf
		EndIf
				
		
	'//////////////GAME STUFF//////////////	
		'timer for all sorts of stuff
		tick:+gamespeed

		If gamespeed
			For bot:ai=EachIn bots
				bot.update()
			Next
		EndIf
		
		ungrouped=things.Copy()
		For t:thing=EachIn things
			t.group=Null
		Next
		groups=New TList
		While ungrouped.Count()
			t:thing=thing(ungrouped.First())
			t.check(group.Create())
		Wend
		
		For w:wall=EachIn walls
			w.draw()
		Next
		
		For s:sweep=EachIn sweeps
			If gamespeed Then s.update()
			s.draw()
		Next
	
		For t:thing=EachIn things
			If gamespeed Then t.update()
			t.draw()
		Next
		
		For f:flash=EachIn flashes
			f.update()
		Next
		
		For sf:scoreflash=EachIn scoreflashes
			sf.update()
		Next
		
		SetAlpha 1
		
		y=30
		SetScale .6,.6
		For s:sweep=EachIn sweeps
			SetColor s.red,s.green,s.blue
			DrawText s.name,screenwidth-200,y
			DrawText s.displayscore,screenwidth-60,y
			y:+18
		Next
		SetScale 1,1
	
		For sound:sounditem=EachIn sounditems
			If Not ChannelPlaying(sound.chan)
				sounditems.Remove sound
			EndIf
		Next
		
		'time
		otime=time
		time=MilliSecs()
		'DrawText Int(1000.0/(time-otime)),700,0
		If time<nextframe
			Delay nextframe-time
			nextframe:+fps
		EndIf
		
		If gamespeed
			Select netmode
			Case 0
				timeleft=roundlength-(time-starttime)/1000.0
			Case 1
				otimeleft#=timeleft
				timeleft=roundlength-(time-starttime)/1000.0
				If Int(timeleft)<Int(otimeleft)
					server.telltime()
				EndIf
			End Select
		EndIf
		
		'end of round stuff
		If timeleft<0
			If oldtimeleft>0
				PlaySound airhorn
				If server
					server.tellendround()
				EndIf
				For t:thing=EachIn things
					If t.distancefromepicentre>=0
						t.die()
					EndIf
				 Next
			EndIf
			repoptick=0
			c=0
			For t:thing=EachIn things
				If t.distancefromepicentre=-1
					t.pop()
					t.group.pop(0,Null)
					t.maxwait=c
					t.Wait=c
					c:+Rand(5,10)
				EndIf
				If wormhole(t) Then t.die()
			Next
			If things.Count()=0 Then finished=1
		EndIf
		oldtimeleft=timeleft
		
		'ticking, time display
		If timeleft>0 And gamespeed
			If timegap
				timegap=0
				While time>tickmod+1000
					tickmod:+1000
				Wend
			Else
				While time>tickmod+1000
					chan:TChannel=AllocChannel()
					SetChannelVolume(chan,timeleft/60.0)
					PlaySound(clocktick,chan)
					tickmod:+1000
				Wend
			EndIf
			
			If timeleft>5
				scale#=Sin((timeleft Mod 1)*180)*.2+1
			Else
				scale#=Sin((timeleft Mod 1)*180)*(5-Int(timeleft))/3.0+1
				EndIf
			SetScale gfxscale*scale,gfxscale*scale
			DrawText Int(timeleft),400,0
			SetScale gfxscale*1,gfxscale*1
		EndIf 

	'////////////INGAME MENU////////////////
		Select ingamemenustate
		Case 0 'menu not showing
			If KeyHit(KEY_ESCAPE)
				ingamemenustate=1
				changemenu(ingamemenu,0)
				prevmenu=Null
				ShowMouse
				lockmouse=False
				If netmode=0
					gamespeed=0
				EndIf
				
				For s:sweep=EachIn sweeps
					PauseChannel s.chan
				Next
				If controlmode
					timegap=time-starttime
				EndIf
			EndIf
		Case 1 'menu showing
			domenus()
			If quitmenu
				If controlmode
					starttime=time-timegap
				EndIf
				quitmenu=0
				ingamemenustate=0
				
				For s:sweep=EachIn sweeps
					ResumeChannel s.chan
				Next
				
				If finishing
					finished=1
					finishing=0
					lockmouse=0
				Else
					If netmode=0
						gamespeed=1
					EndIf
					HideMouse
					If mysweep Then lockmouse=True
				EndIf
			EndIf
		End Select

		
		'FlushMem 
		Flip
	Wend
	
	numresults=sweeps.count()
	results:TList=New TList
	Local nameslist$[sweeps.count()]
	Local scoreslist%[sweeps.count()]
	Local rgblist%[sweeps.count(),3]
	results.addlast nameslist
	results.addlast scoreslist
	results.addlast rgblist
	c=0
	For s:sweep=EachIn sweeps
		nameslist[c]=s.name
		scoreslist[c]=s.score
		rgblist[c,0]=s.red
		rgblist[c,1]=s.green
		rgblist[c,2]=s.blue
		c:+1
	Next
	
	ShowMouse
	FlushKeys

	For c=1 To 10
		SetAlpha c/25.0
		SetColor 0,0,0
		DrawRect 0,0,width,height
		Flip
		'Delay 100
	Next
	For s:sweep=EachIn sweeps
		StopChannel s.chan
	Next
	For si:sounditem=EachIn sounditems
		StopChannel si.chan
	Next
	
	If netmode=0 Or netmode=2
		mysweep=Null
		bots=New TList
		sweeps=New TList
		things=New TList
		groups=New TList
		flashes=New TList
		scoreflashes=New TList
		booms=New TList
		walls=New TList
		sounditems=New TList
	EndIf
	'FlushMem

	lockmouse=0
	finished=0
End Function