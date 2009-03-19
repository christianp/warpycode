'SCHROEDINGER'S GHOST!!
'What a cool idea!!!
'It has BLOWN my MIND!!!

Const lightscale#=3000,maxlight=150
Const maxp#=.005

Global ghosts:TList=New TList

Type mover
	Field x#,y#
	Field vx#,vy#
	Field tick
	Field an
	
	Method update()
		nx=x+vx
		ny=y+vy
		If nx<0 nx=27
		If nx>27 nx=0
		If ny<0 ny=29
		If ny>29 ny=0
		If Not map[nx,ny]
			vx=0
			vy=0
		EndIf

		tick:+1
		If tick>10
			advance()
		EndIf
	End Method
	
	Method move(dx,dy)
		If dx=vx And dy=vy Return
		vx=dx
		vy=dy
		tick=0
		an=((vx-1)*Abs(vx)+vy)*90
	End Method
	
	Method makemove()
	End Method
	
	Method advance()
		ox=x
		oy=y
		x:+vx
		y:+vy
		If x<0 x=27
		If x>27 x=0
		If y<0 y=29
		If y>29 y=0
		If map[x,y]
			tick=-10
			makemove()
		Else
			x=ox
			y=oy
		EndIf
	End Method
End Type
	
Type ghost Extends mover
	Field ox#,oy#,tx#,ty#
	Field state
	Field red,green,blue
	Field prob#[28,30]
	Field coalesce#
	Field poss[200,2],observed
	Field fade#
	Field ovx,ovy
	
	Method New()
		num=Rand(1,7)
		'red=(num & 1)*255
		'blue=(num & 2)*255
		'green=(num & 4)*255
		x#=Rnd(0,27)
		y#=Rnd(0,29)
		vx#=Rnd(-.1,.1)
		vy#=Rnd(-.1,.1)
		state=0
		coalesce#=1
		ghosts.addlast Self
	End Method
	
	Function create:ghost(red,green,blue)
		g:ghost=New ghost
		g.red=red
		g.green=green
		g.blue=blue
		Return g
	End Function
	
	Method update()
		Select state
		Case 0 'unobserved, floating
			If coalesce<1
				coalesce:+.05
			Else
				coalesce=1
			EndIf
			x:+vx
			If x<0
				x=0
				vx=-vx
			ElseIf x>27
				x=27
				vx=-vx
			EndIf
			y:+vy
			If y<0
				y=0
				vy=-vy
			ElseIf y>29
				y=29
				vy=-vy
			EndIf
			
			
			If observed
				num=Rand(0,observed-1)
				state=1
				ox=x
				oy=y
				tx=poss[num,0]
				ty=poss[num,1]
				For pm:pacman=EachIn pacmen
					If Abs(pm.x-tx)+Abs(pm.y-ty)<5
						state=0
					EndIf
				Next
			EndIf
			
		Case 1 'coalescing
			x=ox*coalesce+tx*(1-coalesce)
			y=oy*coalesce+ty*(1-coalesce)
			coalesce:-.02
			If coalesce<=0
				state=2
				vx=0
				vy=0
				coalesce=0
				x=tx
				y=ty
				fade=0
			EndIf
		Case 2 'formed
			closest:pacman=Null
			mindist=-1
			For pm:pacman=EachIn pacmen
				d#=Abs(pm.x-x)+Abs(pm.y-y)
				If d<mindist Or mindist=-1
					closest=pm
				EndIf
			Next
			If closest
				dx#=Sgn(closest.x-x)
				dy#=Sgn(closest.y-y)
				If Abs(closest.x-x)>Abs(closest.y-y)
					dy=0
				Else
					dx=0
				EndIf
				px=x+dx
				py=y+dy
				If px<0 px=27
				If px>27 px=0
				If py<0 py=29
				If py>29 py=0
				If map[px,py]
					move(dx,dy)
				ElseIf vx=0 And vy=0
					Select Rand(0,1)
					Case 0
						vx=Rand(0,1)*2-1
					Case 1
						vy=Rand(0,1)*2-1
					End Select
				EndIf
			EndIf
			tick:+.0001
			super.update()
			
			If observed
				fade:-.02
				If fade<0 fade=0
			Else
				fade:+.02
			EndIf
			If fade>1
				state=0
				vx=Rnd(-.1,.1)
				vy=Rnd(-.1,.1)
				fade=0
			EndIf
			
		End Select
		observed=0
		
		If state<>2
			For px=0 To 27
			For py=0 To 29
				dx#=px-x
				dy#=py-y
				d#=dx*dx+dy*dy
				'If d<100 d=100
				p#=maxp/d
				If p>maxp p=maxp
				prob[px,py]=p*coalesce
			Next
			Next
		EndIf
	End Method
	
	Method observe(px,py)
		Select state
		Case 0
			If Rnd(0,1)<prob[px,py]
				poss[observed,0]=px
				poss[observed,1]=py
				observed:+1
			EndIf
		Case 2
			If x=px And y=py
				observed:+1
			EndIf
		End Select
	End Method
	
	Method makemove()
		For pm:pacman=EachIn pacmen
			If pm.x=x And pm.y=y
				finito=1
			EndIf
		Next
	End Method
		
	Method Draw()
		Select state
		Case 0,1
			If state<>2
				SetBlend LIGHTBLEND
				For px=0 To 27
				For py=0 To 29
					If map[px,py]
						p#=prob[px,py]
						lightup#=p*255*lightscale
						If lightup>maxlight lightup=maxlight
						SetColor red*lightup,green*lightup,blue*lightup
						DrawRect px*20,py*20,20,20
					EndIf
				Next
				Next
			EndIf
		Case 2
			px=x*20+tick*vx
			py=y*20+tick*vy
			SetBlend ALPHABLEND
			SetAlpha 1-fade
			SetColor red*255,green*255,blue*255
			DrawOval px+3,py+3,15,15
			SetAlpha 1
			SetColor 255,255,255
			DrawOval px+5,py+6,4,4
			DrawOval px+12,py+6,4,4
			SetColor 0,0,0
			DrawRect px+7+vx,py+7+vy,1,1
			DrawRect px+14+vx,py+7+vy,1,1
			
		End Select
	End Method
End Type

Global pacmen:TList=New TList
Type pacman Extends mover

	Field score,mouthopen,mouthdir

	Method New()
		x=15
		y=17
		pacmen.addlast Self
		mouthdir=1
	End Method
	
	Method update()
		'tick:+.1
		super.update()
		
		px=x
		py=y
		'If vx Or vy
		'	While (px>0 And px<28 And py>0 And py<30 And map[px,py]=1)
		'		observe(px,py)
		'		px:+vx
		'		py:+vy
		'	Wend
		'Else
			While map[px,py]
				observe(px,py)
				px:-1
				If px<0 px=27
			Wend
			px=x+1
			If px>27 px=0
			While map[px,py]
				observe(px,py)
				px:+1
				If px>27 px=0
			Wend
			px=x
			py=y-1
			If py<0 py=29
			While map[px,py]
				observe(px,py)
				py:-1
				If py<0 py=29
			Wend
			py=y+1
			If py>29 py=0
			While map[px,py]
				observe(px,py)
				py:+1
				If py>29 py=0
			Wend
		'EndIf
		
		If vx Or vy
			mouthopen:+5*mouthdir
			If mouthopen<0 Or mouthopen>70
				mouthdir=-mouthdir
				mouthopen:+mouthdir*5
			EndIf
		EndIf
	End Method
	
	Method observe(px,py)
		observedmap[px,py]:+1
		For g:ghost=EachIn ghosts
			g.observe(px,py)
		Next
	End Method
	
	Method makemove()
		If pills[x,y]
			pills[x,y]=0
			score:+100
			totalpills:-1
			If totalpills=0
				finito=1
			EndIf
		EndIf
	End Method
	
	Method Draw()
		SetBlend ALPHABLEND
		cx#=x*20+10+tick*vx
		cy#=y*20+10+tick*vy
		ox#=cx+Cos(mouthopen+an)*8
		oy#=cy+Sin(mouthopen+an)*8
		SetColor 0,0,0
		DrawLine cx,cy,ox,oy
		For pan=mouthopen To 360-mouthopen Step 10
			px#=cx+Cos(pan+an)*8
			py#=cy+Sin(pan+an)*8
			Local poly#[]=[cx,cy,ox,oy,px,py]
			SetColor 255,255,0
			DrawPoly poly
			SetColor 0,0,0
			DrawLine ox,oy,px,py
			ox=px
			oy=py
		Next
		SetColor 0,0,0
		DrawLine cx,cy,px,py
	End Method
End Type

file:TStream=OpenFile("schroedingersmap.txt")
Global map[28,30]
For y=0 To 29
	Line$=ReadLine(file)
	For x=0 To 27
		If Line[x]-48 Then map[x,y]=1
	Next
Next

AppTitle="Schroedinger's Ghost!"
Graphics 560,600,0

Global finito=0
Global restart=1

Global observedmap[28,30]
Global pills[28,30]
Global totalpills

Global player:pacman
Global musicch:TChannel
Global musicogg:TSound=LoadSound("bambasamba.ogg")

Function initgame()
	totalpills=0
	For x=0 To 27
	For y=0 To 29
		If map[x,y]
			pills[x,y]=1
			totalpills:+1
		EndIf
	Next
	Next
	ghosts=New TList
	pacmen=New TList

	ghost.create(0,0,1)
	ghost.create(1,0,0)
	ghost.create(0,1,0)
	
	player:pacman=New pacman
	
	finito=0
	musicch=PlaySound(musicogg)
End Function	

Function rungame()
	If Not ChannelPlaying(musicch)
		musicch=PlaySound(musicogg)
	EndIf
	
	While Not finito
		mx=MouseX()
		my=MouseY()
		
	
		'draw map
		For x=0 To 27
		For y=0 To 29
			If map[x,y]
				If observedmap[x,y]
					SetBlend LIGHTBLEND
					SetColor 100,100,0
					DrawRect x*20,y*20,20,20
					observedmap[x,y]=0
				EndIf
			Else
				SetBlend ALPHABLEND
				SetColor 100,100,100
				DrawRect x*20,y*20,20,20
			EndIf
			If pills[x,y]
				SetBlend ALPHABLEND
				SetColor 255,255,255
				DrawOval x*20+8,y*20+8,5,5
			EndIf
		Next
		Next
		
		If KeyHit(KEY_LEFT)
			player.move(-1,0)
		ElseIf KeyHit(KEY_RIGHT)
			player.move(1,0)
		ElseIf KeyHit(KEY_UP)
			player.move(0,-1)
		ElseIf KeyHit(KEY_DOWN)
			player.move(0,1)
		EndIf
		
		
		'do ghosts
		yoff=0
		For g:ghost=EachIn ghosts
			g.update()
			g.draw()
			x=mx/20
			y=my/20
			'SetColor 255,255,0
			'DrawText g.prob[x,y],x*20,y*20+yoff
			'yoff:+15
		Next
		
		'do pacmen
		For p:pacman=EachIn pacmen
			p.update()
			p.draw()
		Next
		
		SetBlend LIGHTBLEND
		SetColor 255,255,255
		txt$=String(player.score)
		DrawText txt,280-TextWidth(txt)/2,0
		
		'quitting
		If KeyHit(KEY_ESCAPE)
			finito=1
			restart=0
		EndIf
		
		Flip
		Cls
	Wend
End Function

  
While restart
	initgame()
	rungame()
	
	Cls
	If totalpills
		txt$="You were eaten! Your score was "+String(player.score)
	Else
		txt$="YOU WON!"
	EndIf
	StopChannel musicch
	If restart
		DrawText txt,280-TextWidth(txt)/2,300-TextHeight(txt)/2
		txt$="Press a key to play again"
		DrawText txt,280-TextWidth(txt)/2,330-TextHeight(txt)/2
		Flip
		Delay 500
		WaitKey()
	EndIf
Wend

