Extern "win32"
	Function ShowCursor(flag%)
End Extern


If appargs.length>1
	If AppArgs[1]="/p" Or AppArgs[1]="/c" Then End
EndIf
Const screenwidth=1024,screenheight=768
Const numcats=5
SeedRnd MilliSecs()
Graphics screenwidth,screenheight,32
SetBlend ALPHABLEND
SetImageFont(LoadImageFont("c:\windows\fonts\Verdana.ttf",100,SMOOTHFONT))

Type cat
	Field x#,y#,vx#,vy#,state,ticker
	Field targ:cat
	Field speed#
	Field tx#,ty#
	
	Method New()
		cats.addlast Self
	End Method
	
	Method update()
		Select state
		Case -1 'waiting
			If Rand(200)=1 Then state=0
		Case 0 'doing nothing
			If Rand(100)=1
				state=Rand(5)
			EndIf
		Case 1 'miaow
			If Rand(200)
				texto.create(x,y-10,Rnd(-15,15),Rnd(.5,1),.2,.5,"miaow",Rnd(50,150))
				state=-1
			EndIf
		Case 2 'walk
			tx=Rand(screenwidth)
			ty=Rand(screenheight)
			vx#=tx-x
			vy#=ty-y
			d#=Sqr(vx*vx+vy*vy)
			vx:/d
			vy:/d
			speed=Rnd(.6,1.4)
			state=6
		Case 3 'snore
			If Rand(25)=1 Then texto.create(x,y,Rnd(-15,15),Rnd(1,1.2),Rnd(.1,.3),.2,"z",Rnd(30,80))
			If Rand(1000)=1
				state=-1
			EndIf
		Case 4 'stalk
			tcat:cat=Self
			While tcat=Self
				tcat=cat(choice(cats))
			Wend
			tx=tcat.x
			ty=tcat.y
			vx#=tx-x
			vy#=ty-y
			d#=Sqr(vx*vx+vy*vy)
			vx:/d
			vy:/d
			speed=Rnd(2,3)
			state=6
		Case 6 'walking
			x:+vx*speed
			y:+vy*speed
			If Abs(x-tx)<3 And Abs(y-ty)<3
				If targ
					state=8
				Else
					state=-1
				EndIf
			EndIf
		Case 7 'fighting
			If Rand(5)=1 Then texto.create(x,y,Rnd(-45,45),Rnd(1,3),Rnd(.2,.3),.7,String(choice(fightingwords)),Rnd(10,20))
			If Rand(50)=1
				targ.state=8
				tx=Rand(screenwidth)
				ty=Rand(screenheight)
				vx#=tx-x
				vy#=ty-y
				d#=Sqr(vx*vx+vy*vy)
				vx:/d
				vy:/d
				speed=Rnd(2,5)
				state=6
				ticker=10
			EndIf
		Case 8 'licking
			If Rand(50)=1 Then texto.create(x,y,Rnd(-30,30),Rnd(1,1.5),Rnd(.1,.2),.4,"lick",Rnd(20,50))
			If Rand(500)=1 Then texto.create(x,y,Rnd(-30,30),Rnd(1,1.5),Rnd(.2,.4),.9,String(choice(coughs)),Rnd(20,50))
			If Rand(500)=1
				state=-1
				targ=Null
			EndIf
		Default
			state=0
		End Select
		
		If ticker=0
			For c:cat=EachIn cats
				If c<>Self And state<>7
					dx#=c.x-x
					dy#=c.y-y
					d#=dx*dx+dy*dy
					If d<2500
						state=7
						targ=c
					EndIf
				EndIf
			Next
		Else
			ticker:-1
		EndIf
		
		SetAlpha 1
		SetScale 1,1
		SetRotation 0
		'DrawRect x,y,1,1
		SetScale .1,.1
		'DrawText state,x,y
	End Method
End Type

Type thing
	Field x#,y#,action$
	
	Method New()
		things.addlast Self
	End Method
End Type

Type texto
	Field x#,y#,an#,v#,scale#,alpha#,text$,life,maxlife
	
	Function create:texto(x#,y#,an#,v#,scale#,alpha#,text$,life)
		t:texto=New texto
		textos.addlast t
		t.x=x
		t.y=y
		t.an=an
		t.v=v
		t.scale=scale
		t.alpha=alpha
		t.text=text
		t.life=life
		t.maxlife=life
		Return t
	End Function
	
	Method update()
		c#=Cos(an-90)
		s#=Sin(an-90)
		x:+c*v
		y:+s*v
		SetScale scale,scale
		SetRotation an
		SetAlpha alpha*Float(life)/maxlife
		DrawText text,x+s*TextWidth(text)*scale/2+c*TextHeight(text)*scale/2,y-c*TextWidth(text)*scale/2+s*TextHeight(text)*scale/2
		life:-1
		If life=0
			textos.remove Self
		EndIf
	End Method
End Type

Function choice:Object(l:TList)
	n=l.count()
	If Not n Then Return
	p=Rand(0,n-1)
	Return l.valueatindex(p)
End Function

Global fightingwords:TList=ListFromArray(["hiss","scratch","bite","swipe","claws"])
Global coughs:TList=ListFromArray(["cough","hack","wheeze"])

Global cats:TList=New TList
Global things:TList=New TList
Global textos:TList=New TList

For i=1 To numcats
	c:cat=New cat
	c.x=Rand(screenwidth)
	c.y=Rand(screenheight)
Next
mx=MouseX()
my=MouseY()
showcursor False
While Not KeyHit(KEY_ESCAPE)
	
	If MouseX()<>mx Or MouseY()<>my
		End
	EndIf

	For c:cat=EachIn cats
		c.update()
	Next
	
	For t:texto=EachIn textos
		t.update()
	Next

	Flip
	Cls
Wend