Rem
Framework BRL.GLMax2D
Import BRL.Math
Import BRL.FileSystem
Import Pub.Win32
Import BRL.FreeAudioAudio
Import BRL.WAVLoader
Import BRL.StandardIO
Import BRL.FreeTypeFont
Import BRL.Random
Import BRL.Socket
Import BRL.SocketStream
EndRem

SetGraphicsDriver GLMax2DDriver()

Incbin "c:\windows\fonts\Verdana.ttf"
Global mainfont:timagefont=LoadImageFont("incbin::c:\windows\fonts\Verdana.ttf",30,SMOOTHFONT)

Include "groups.bmx"
Include "editor.bmx"

Type menu
	Field tendrils:TList
	Field first:tendril,last:tendril
	Field hover:tendril
	
	Method New()
		tendrils=New TList
	End Method
	
	Method addtendril(t:tendril)
		tendrils.addlast t
		t.z=tendrils.count()

		If Not first Then first=t
		If last
			last.nxt=t
			t.prv=last
		EndIf
		last=t
	End Method
	
	Method open()
		If Not first Then Return
		first.speed=menuspeed
	End Method
	
	Method close()
		If Not first Then Return
		t:tendril=first
		prv:tendril=t
		While t.targtick
			prv=t
			t=t.nxt
			If t=Null Then Exit
		Wend
		prv.speed=-menuspeed
	End Method
	
	Method toggle()
		If Not first Then Return
		If first.targtick>0 Then close() Else open()
	End Method
	
	Method pickhover()
		hover:tendril=Null
		For t:tendril=EachIn tendrils
			If t.endtick=1
				If mx>=t.lb And mx<=t.rb And my>=t.tb And my<=t.bb
					hover=t
				EndIf
			EndIf
			t.ealpha=.2
			t.mywidth=t.realwidth
		Next
		tendrils.sort()
		If hover And hover.pickable
			tendrils.remove hover
			tendrils.addlast hover
			hover.mywidth=80+Sin(menutick*9)*5+5
			hover.ealpha=.8
		EndIf
	End Method
	
	Method update()
		pickhover()
		For t:tendril=EachIn tendrils
			t.update()
			t.draw()
		Next
	End Method
	
	Method reset()
		For t:tendril=EachIn tendrils
			t.targtick=0
			t.endtick=0
			t.active=0
			t.speed=0
		Next
	End Method
	
End Type

Type tendril
	Field sx#,sy#,ex#,ey#
	Field vx1#,vy1#,vx2#,vy2#
	Field targx#,targy#,directionlength#,speed#,targtick#,endtick#
	Field sred,sgreen,sblue
	Field ered,egreen,eblue
	Field salpha#,ealpha#
	Field mywidth#,realwidth#
	Field endlength#
	Field segments,endsegments
	Field z,nxt:tendril,prv:tendril
	Field lb,rb,tb,bb
	Field text$,tred,tgreen,tblue,tsize#
	Field entry,active,label$,entrytext$
	Field fullimage:timage
	Field pickable
	Field mymenuitem:menuitem
	Field textend
	
	Function create:tendril(sx#,sy#,ex#,ey#,mywidth#,segments,endsegments,endlength#,sred,sgreen,sblue,salpha#,ered,egreen,eblue,ealpha#,text$,tred,tgreen,tblue,tsize#,pickable=1)
		t:tendril=New tendril
		t.sx=sx
		t.sy=sy
		t.ex=ex
		t.ey=ey
		t.mywidth=mywidth
		t.realwidth=mywidth
		t.segments=segments
		t.endsegments=endsegments
		t.endlength=endlength
		t.sred=sred
		t.sgreen=sgreen
		t.sblue=sblue
		t.salpha=salpha
		t.ered=ered
		t.egreen=egreen
		t.eblue=eblue
		t.ealpha=ealpha
		t.pickable=pickable

		t.text=text
		t.tred=tred
		t.tgreen=tgreen
		t.tblue=tblue
		t.tsize=tsize
		Return t
	End Function
	
	Method Compare(o:Object)
		t:tendril=tendril(o)
		If z>t.z
			Return 1
		ElseIf z<t.z 
			Return -1
		Else
			Return 0
		EndIf
	End Method
	
	Method update()
		ox#=ox
		oy#=ey
		If targtick>=1 
			targtick=1
			endtick:+speed
			If endtick>1 Then endtick=1
			If endtick<0
				targtick:+speed
				endtick=0
			EndIf
		Else
			targtick:+speed
			If targtick<0 Then targtick=0
		EndIf


		If nxt
			If targtick=1 And nxt.speed=0 And speed>0
				nxt.speed=menuspeed
			EndIf
		EndIf
		If prv
			If endtick=0 And speed<0 And prv.endtick 
				prv.speed=-menuspeed
				If targtick=0 Then speed=0
			EndIf
		EndIf
		
		If Sgn(endlength)>0
			lb=ex
			rb=ex+endlength
		Else
			lb=ex+endlength
			rb=ex
		EndIf
		tb=ey-realwidth/2
		bb=ey+realwidth/2
		
		If mymenuitem
			mymenuitem.update()
			If active Then mymenuitem.active()
		EndIf
		Rem
		If entry
			If active
				doentry()
			EndIf
		Else
			If active
				If nxt Then nxt.active=1
				active=0
			EndIf
		EndIf
		EndRem
	End Method
	
	Method draw()
		If targtick=0 Then Return
		
		'If endtick=1 And fullimage
		'	DrawImage fullimage,0,0
		'Else
		numpoints=segments*4
		Local points#[numpoints]
		dir=Sgn(endlength)
		length#=Sqr((ex-sx)*(ex-sx)+(ey-sy)*(ey-sy))
		'mx#=(ex-sx)/length
		'my#=(ey-sy)/length
		bx#=sx+vx1*targtick
		by#=sy+vy1*targtick
		cx#=ex+vx2*targtick
		cy#=ey+vy2*targtick
		x#=sx
		y#=sy
		For c=0 To segments-1
			a#=Float(c)/segments
			b#=1-a
			ox#=x
			oy#=y
			x#=sx*b*b*b+3*bx*b*b*a+3*cx*b*a*a+ex*a*a*a
			y#=sy*b*b*b+3*by*b*b*a+3*cy*b*a*a+ey*a*a*a
			an#=ATan2(y-oy,x-ox)
			If dir>0 Then an:+90 Else an:-90
			wx#=Cos(an)*(mywidth/2)'*(b*.5+1)
			wy#=Sin(an)*(mywidth/2)'*(b*.5+1)
			points[c*2]=x-wx
			points[c*2+1]=y-wy
			points[numpoints-c*2-2]=x+wx
			points[numpoints-c*2-1]=y+wy
		Next
		
		For i=0 To (segments-1)*targtick
			Local drawpoints#[]=[points[i*2],points[i*2+1],points[i*2+2],points[i*2+3],points[numpoints-i*2-4],points[numpoints-i*2-3],points[numpoints-i*2-2],points[numpoints-i*2-1]]
			a#=Float(i)/segments
			b#=1-a
			SetColor sred*b+ered*a,sgreen*b+egreen*a,sblue*b+eblue*a
			SetAlpha salpha*b+ealpha*a
			If dir<0 And i=0
				a#=drawpoints[0]
				b#=drawpoints[1]
				drawpoints[0]=drawpoints[6]
				drawpoints[1]=drawpoints[7]
				drawpoints[6]=a
				drawpoints[7]=b
			EndIf
			DrawscalePoly drawpoints
			If i<segments-1
				SetAlpha 1
				DrawscaleLine drawpoints[0],drawpoints[1],drawpoints[2],drawpoints[3]
				DrawscaleLine drawpoints[6],drawpoints[7],drawpoints[4],drawpoints[5]
			EndIf
		Next

		If targtick=1
			SetAlpha ealpha
			SetColor ered,egreen,eblue
			Local enddrawpoints#[]=[points[segments*2-2],points[segments*2-1],points[segments*2-2]+endlength*endtick,points[segments*2-1],points[segments*2-2]+endlength*endtick,points[segments*2-1]+mywidth,points[numpoints-segments*2],points[numpoints-segments*2+1]]
			DrawscalePoly enddrawpoints
			SetAlpha 1
			DrawscaleLine enddrawpoints[0],enddrawpoints[1],enddrawpoints[2],enddrawpoints[3]
			'drawscaleline enddrawpoints[2],enddrawpoints[3],enddrawpoints[4],enddrawpoints[5]
			drawscaleline enddrawpoints[6],enddrawpoints[7],enddrawpoints[4],enddrawpoints[5]
	
			circlestep#=180.0/endsegments
			Local endcircledraw#[endsegments*2+2]
			cx#=(enddrawpoints[2]+enddrawpoints[4])/2.0
			cy#=enddrawpoints[3]+mywidth/2.0
			ox#=enddrawpoints[2]
			If dir>0
				oy#=enddrawpoints[3]
			Else
				oy#=enddrawpoints[5]
			EndIf
			SetAlpha 1
			For i=0 To endsegments
				an#=circlestep*i
				If dir>0 Then an:-90 Else an:+90
				x#=mywidth*Cos(an)/2
				y#=mywidth*Sin(an)/2
				endcircledraw[i*2]=cx+x
				endcircledraw[i*2+1]=cy+y
				drawscaleline ox,oy,cx+x,cy+y
				ox=cx+x
				oy=cy+y
			Next
			SetAlpha ealpha
			DrawscalePoly endcircledraw
			
		EndIf
		'EndIf
		
		If endtick=1 
			If active And textentry(mymenuitem)
				Local entrypoints#[]=[enddrawpoints[0]+5,enddrawpoints[1]+5,enddrawpoints[2]-5,enddrawpoints[3]+5,enddrawpoints[2]-5,enddrawpoints[5]-5,enddrawpoints[6]+6,enddrawpoints[7]-5]
				SetColor 100,100,100
				SetAlpha .6
				DrawscalePoly entrypoints
			EndIf
			
			If mymenuitem 
				mymenuitem.draw()
			EndIf
		EndIf
		
		SetColor tred,tgreen,tblue
		SetAlpha 1
		tw#=TextWidth(text)*tsize
		If dir>0
			endpoint#=endtick/(tw/endlength)
			If endpoint>1 Then endpoint=1
			bitlength=Int(endpoint*text.length)
			textbit$=text[0..bitlength]
			x=lb+10
		Else
			endpoint#=1+tw/endlength
			If endtick>=endpoint
				bitlength=Int((endtick-endpoint)*text.length/(1-endpoint))
			Else
				bitlength=0
			EndIf
			textbit$=text[text.length-bitlength..text.length]
			x=lb+10+tw-TextWidth(textbit)*tsize
		EndIf
		DrawscaleText textbit,x,ey-15,tsize,tsize
		
		textend=x+TextWidth(textbit)*tsize
		
		'SetColor 255,255,255
		'SetScale .4,.4
		'For i=0 To numpoints/2-1
		'	DrawRect points[i*2],points[i*2+1],5,5
		'	DrawText i,points[i*2],points[i*2+1]
		'Next
		SetScale 1,1
	End Method
	
	Method doentry()
		key=GetChar()
		Select key
		Case 0
			'do nothing
		Case KEY_BACKSPACE
			entrytext = entrytext[0..entrytext.length-1]
		Case KEY_RETURN 
			active=0
			If nxt Then nxt.active=1
		Default entrytext:+Chr(key)
		End Select
		text=label+entrytext
	End Method
	
	Method addmenuitem(mi:menuitem)
		mymenuitem=mi
		mi.parent=Self
		entrytext=mi.entrytext
	End Method
End Type

Type menuitem
	Field entrytext$,label$
	Field parent:tendril

	Method active()
		Return

	End Method

	Method update()
		Return
	End Method
	
	Method draw()
		Return
	End Method
End Type

Type scorelist Extends menuitem
	Method draw()
		SetAlpha 1
		SetColor 255,255,255
		title$="Scores"
		w#=TextWidth(title)
		DrawscaleText title,400-w/2,5,1,1

		If Not results Then Return

		y#=60
		Local nameslist$[]=String[](results.first())
		Local scoreslist[]=Int[](results.valueatindex(1))
		Local rgblist[nameslist.length-1,3]
		rgblist=Int[,](results.last())
		For c=0 To nameslist.length-1
			SetColor rgblist[c,0],rgblist[c,1],rgblist[c,2]
			DrawscaleText nameslist[c],200,y,.8,.8
			DrawscaleText scoreslist[c],600,y,.8,.8
			y:+28
		Next
	End Method
End Type

Type textentry Extends menuitem
	
	Function create:textentry(label$,entrytext$)
		te:textentry=New textentry
		te.label=label
		te.entrytext=entrytext
		
		Return te
	End Function
	
	Method update()
		parent.entrytext=entrytext
		parent.text=label+entrytext
	End Method
	
	Method active()
		Select key
		Case 0
			'do nothing
		Case KEY_BACKSPACE
			entrytext = entrytext[0..entrytext.length-1]
		Case KEY_RETURN 
			parent.active=0
		Default entrytext:+Chr(key)
		End Select
		
	End Method
	
End Type

Type listchoice Extends menuitem
	Field list:TList
	Field pos
	
	Method active()
		'If KeyHit(KEY_LEFT)
		'	pos:-1
		'	If pos<=-1 Then pos=list.count()-1
		'EndIf
		'If KeyHit(KEY_RIGHT)
		'	pos:+1
		'	If pos>=list.count() Then pos=0
		'EndIf
		
		pos:+1
		If pos>=list.count() Then pos=0
		
		parent.active=0
	End Method
	
	Method update()
		entrytext=String(list.valueatindex(pos))
		parent.entrytext=entrytext
		parent.text=label+entrytext
	End Method	
End Type

Type botchoice Extends menuitem
	Field mybot:ai
	
	Method update()
		If Not mybot Return
		If mybot
			mybot.itssweep.name=ebnamebutton.entrytext
			mybot.speed=slider(ebskillbutton.mymenuitem).getvalue()/10.0
			mybot.itssweep.red=slider(ebredbutton.mymenuitem).getvalue()
			mybot.itssweep.green=slider(ebgreenbutton.mymenuitem).getvalue()
			mybot.itssweep.blue=slider(ebbluebutton.mymenuitem).getvalue()
		EndIf
		
	End Method		
	
	Method active()
		parent.active=0
		
		link:TLink=bots.findlink(mybot)
		If link=Null
			If bots.count()	setchoice(ai(bots.first()))
		ElseIf link.nextlink()
			setchoice(ai(link.nextlink().value()))
		Else
			setchoice(ai(bots.first()))
		EndIf			
		
	End Method
	
	Method draw()
		If Not mybot Return

		SetAlpha 1
		SetColor 255,255,255
		DrawscaleText mybot.itssweep.name,parent.textend,parent.ey-TextHeight(mybot.itssweep.name)/2,parent.tsize,parent.tsize
	End Method
		
	Method setchoice(bot:ai)
		mybot=bot
		If mybot
			textentry(ebnamebutton.mymenuitem).entrytext=mybot.itssweep.name
			slider(ebskillbutton.mymenuitem).setvaluefrom mybot.speed*10
			slider(ebredbutton.mymenuitem).setvaluefrom mybot.itssweep.red
			slider(ebgreenbutton.mymenuitem).setvaluefrom mybot.itssweep.green
			slider(ebbluebutton.mymenuitem).setvaluefrom mybot.itssweep.blue
		EndIf
	End Method
End Type

Type toggle Extends menuitem
	Field yes
	
	Method active()
		yes=1-yes
		parent.active=0
	End Method
	
	Method draw()
		SetColor 255,255,255
		SetAlpha .2+.8*yes
		x=parent.textend+30
		DrawscaleOval x-20,parent.ey-20,40,40
	End Method
End Type

Type slider Extends menuitem
	Field number#
	Field minnumber#,maxnumber#,range#
	Field isint
	Field l,r
	Field value#
	
	Function create:slider(minnumber,maxnumber,isint)
		sl:slider=New slider
		sl.minnumber=minnumber
		sl.maxnumber=maxnumber
		sl.range#=sl.maxnumber-sl.minnumber
		sl.isint=isint
		sl.value=.5
		
		Return sl
	End Function
	
	Method active()
		If Not MouseDown(1) parent.active=0
		setvalue(Float(mx-l)/(r-l))
	End Method

	Method setvalue(v#)
		If v>1 v=1
		If v<0 v=0
		If isint
			ivalue=Int(v*range)
			If v*range-ivalue>.5
				ivalue:+1
			EndIf
			value=ivalue/range
		EndIf
	End Method
	
	Method setvaluefrom(v#)
		setvalue((v-minnumber)/range)
	End Method
	
	Method draw()
		range#=maxnumber-minnumber
		l=parent.textend+5
		'If parent.endlength<0
			r=parent.rb
		'Else
		'	r=
		'EndIf
		y=parent.ey
		SetLineWidth 2
		SetColor 255,255,255
		SetAlpha 1
		DrawscaleLine l,y,r,y
		SetLineWidth 1
		drawscaleoval l+value*(r-l)-3,y-3,7,7
		SetAlpha .5
		drawscaleoval l+value*(r-l)-7,y-7,15,15
		SetAlpha 1
		If isint
			DrawscaleText Int(getvalue()),l,y,parent.tsize,parent.tsize
		Else
			DrawscaleText getvalue(),l,y,parent.tsize,parent.tsize
		EndIf
	End Method
	
	Method getvalue#()
		Return value*range+minnumber
	End Method
End Type

Function loadoptions()
	If Not FileType("options.dat")
		saveoptions()
		Return
	EndIf
	f:TStream=OpenFile("options.dat")
	fullscreen=ReadInt(f)
	tabletmode=ReadInt(f)
	poponrelease=ReadInt(f)
	alwayspush=ReadInt(f)
	CloseFile f
End Function

Function saveoptions()
	f:TStream=OpenFile("options.dat",0,1)
	WriteInt f,fullscreen
	WriteInt f,tabletmode
	WriteInt f,poponrelease
	WriteInt f,alwayspush
	CloseFile f
End Function

Function initmenu()
	'FlushMem()
	SetBlend ALPHABLEND

	'////MAIN MENU
	mainmenu=New menu

	singleplayerbutton:tendril=tendril.create(0,200,300,100,80,30,8,300,100,0,0,.5,255,0,0,.5,"Singleplayer",255,255,255,1)
	singleplayerbutton.vx1=150
	singleplayerbutton.vx2=-200
	mainmenu.addtendril singleplayerbutton

	'servermenubutton:tendril=tendril.create(800,200,580,170,80,30,8,-280,0,0,100,.5,0,0,255,.5,"Start Server",255,255,255,1)
	'servermenubutton.vx1=-150
	'servermenubutton.vx2=150
	'mainmenu.addtendril servermenubutton

	'joingamebutton:tendril=tendril.create(0,280,300,240,80,30,8,300,0,100,0,.5,0,255,0,.5,"Join Game",255,255,255,1)
	'joingamebutton.vx1=150
	'joingamebutton.vx2=-200
	'mainmenu.addtendril joingamebutton
	
	mainmenuoptionsbutton:tendril=tendril.create(800,280,580,310,80,30,8,-280,100,0,100,.5,255,0,255,.5,"Options",255,255,255,1)
	mainmenuoptionsbutton.vx1=-150
	mainmenuoptionsbutton.vx2=150
	mainmenu.addtendril mainmenuoptionsbutton
	
	editorbutton:tendril=tendril.create(0,360,300,380,80,30,8,300,100,100,0,.5,255,255,0,.5,"Level Editor",255,255,255,1)
	editorbutton.vx1=150
	editorbutton.vx2=-200
	mainmenu.addtendril editorbutton
	
	'quitbutton:tendril=tendril.create(100,800,300,590,80,30,8,300,0,0,0,.5,100,100,100,.5,"Quit",255,255,255,1)
	'quitbutton.vy1=-150
	'quitbutton.vx2=-200
	'mainmenu.addtendril quitbutton
	
	'///OPTIONS MENU
	optionsmenu:menu=New menu
	
	optionsmenuokbutton:tendril=tendril.create(50,800,220,590,80,30,8,160,0,0,0,.5,0,0,200,.5,"OK",255,255,255,1)
	optionsmenuokbutton.vy1=-150
	optionsmenuokbutton.vx2=-200
	optionsmenu.addtendril optionsmenuokbutton
	
	optionsmenucancelbutton:tendril=tendril.create(750,800,580,590,80,30,8,-160,0,0,0,.5,200,0,0,.5,"Cancel",255,255,255,1)
	optionsmenucancelbutton.vy1=-150
	optionsmenucancelbutton.vx2=200
	optionsmenu.addtendril optionsmenucancelbutton
	
	optionsfullscreenbutton:tendril=tendril.create(0,200,300,100,80,30,8,300,100,0,0,.5,100,100,100,.5,"Fullscreen:",255,255,255,.75)
	optionsfullscreenbutton.vx1=150
	optionsfullscreenbutton.vx2=-200
	fullscreentoggle:toggle=New toggle
	optionsfullscreenbutton.addmenuitem fullscreentoggle
	'optionsmenu.addtendril optionsfullscreenbutton

	optionstabletmodebutton:tendril=tendril.create(800,200,580,170,80,30,8,-280,0,0,0,.5,100,100,100,.5,"Tablet Mode:",255,255,255,.75)
	optionstabletmodebutton.vx1=-150
	optionstabletmodebutton.vx2=150
	tabletmodetoggle:toggle=New toggle
	optionstabletmodebutton.addmenuitem tabletmodetoggle
	optionsmenu.addtendril optionstabletmodebutton
		
	optionspoponreleasebutton:tendril=tendril.create(0,280,300,240,80,30,8,300,100,0,0,.5,100,100,100,.5,"Pop on Release:",255,255,255,.75)
	optionspoponreleasebutton.vx1=150
	optionspoponreleasebutton.vx2=-200
	poponreleasetogle:toggle=New toggle
	optionspoponreleasebutton.addmenuitem poponreleasetogle
	optionsmenu.addtendril optionspoponreleasebutton

	optionsalwayspushbutton:tendril=tendril.create(800,280,580,310,80,30,8,-280,0,0,0,.5,100,100,100,.5,"Always Push:",255,255,255,.75)
	optionsalwayspushbutton.vx1=-150
	optionsalwayspushbutton.vx2=150
	alwayspushtoggle:toggle=New toggle
	optionsalwayspushbutton.addmenuitem alwayspushtoggle
	optionsmenu.addtendril optionsalwayspushbutton

	'///SINGLEPLAYER SETUP MENU
	spmenu:menu=New menu

	spgobutton:tendril=tendril.create(0,200,300,100,80,30,8,300,100,0,0,.5,255,0,0,.5,"Start Game",255,255,255,1)
	spgobutton.vx1=150
	spgobutton.vx2=-200
	spmenu.addtendril spgobutton
	
	spnamebutton:tendril=tendril.create(800,200,580,170,80,30,8,-280,0,0,0,.5,0,0,150,.5,"Name: ",255,255,255,.75)
	spnamebutton.vx1=-150
	spnamebutton.vx2=150
	nameentry:textentry=textentry.create("Name: ","Player")
	spnamebutton.addmenuitem nameentry
	spmenu.addtendril spnamebutton

	splevelbutton:tendril=tendril.create(0,280,280,240,80,30,8,320,0,0,0,.5,100,100,100,.5,"Level: ",255,255,255,.75)
	splevelbutton.vx1=150
	splevelbutton.vx2=-200
	splevelchoice:listchoice=New listchoice
	splevelchoice.label="Level: "
	splevelchoice.list=levelslist(1,0)
	splevelbutton.addmenuitem splevelchoice
	
	spdifficultybutton:tendril=tendril.create(800,280,580,310,80,30,8,-280,0,0,0,.5,0,150,0,.5,"Difficulty",255,255,255,.75,1)
	spdifficultybutton.vx1=-150
	spdifficultybutton.vx2=150
	difficultyslider:slider=slider.create(1,10,1)
	spdifficultybutton.addmenuitem difficultyslider
	spmenu.addtendril spdifficultybutton

	spmenu.addtendril splevelbutton

	Rem

	'///SERVER SETUP MENU
	servermenu:menu=New menu
	
	servergobutton:tendril=tendril.create(0,200,300,100,80,30,8,300,100,0,0,.5,255,0,0,.5,"Start Server",255,255,255,1)
	servergobutton.vx1=150
	servergobutton.vx2=-200
	servermenu.addtendril servergobutton

	serverportbutton:tendril=tendril.create(800,200,580,170,80,30,8,-280,0,0,0,.5,0,0,150,.5,"Server Port",255,255,255,.75)
	serverportbutton.vx1=-150
	serverportbutton.vx2=150
	serverportentry:textentry=textentry.create("Server Port: ","6791")
	serverportbutton.addmenuitem serverportentry
	servermenu.addtendril serverportbutton
	
	serverlevelbutton:tendril=tendril.create(0,280,280,240,80,30,8,320,0,0,0,.5,100,100,100,.5,"Level: ",255,255,255,.75,1)
	serverlevelbutton.vx1=150
	serverlevelbutton.vx2=-200	
	mplevelchoice:listchoice=New listchoice
	mplevelchoice.label="Level: "
	mplevelchoice.list=levelslist(0,1)
	serverlevelbutton.addmenuitem mplevelchoice
	servermenu.addtendril serverlevelbutton

	servermenubackbutton:tendril=tendril.create(100,800,300,590,80,30,8,300,0,0,0,.5,100,100,100,.5,"Back to Main Menu",255,255,255,1)
	servermenubackbutton.vy1=-150
	servermenubackbutton.vx2=-200
	servermenu.addtendril servermenubackbutton
	
	'///CLIENT SETUP MENU
	clientmenu:menu=New menu
	
	clientgobutton:tendril=tendril.create(0,200,300,100,80,20,8,300,100,0,0,.5,255,0,0,.5,"Connect to Server",255,255,255,1)
	clientgobutton.vx1=150
	clientgobutton.vx2=-200
	clientmenu.addtendril clientgobutton

	clientipbutton:tendril=tendril.create(800,200,580,170,80,20,8,-280,0,0,0,.5,0,0,150,.5,"Server IP: ",255,255,255,.65)
	clientipbutton.vx1=-150
	clientipbutton.vx2=150
	clientipentry:textentry=textentry.create("Server IP: ","127.0.0.1")
	clientipbutton.addmenuitem clientipentry
	clientmenu.addtendril clientipbutton

	clientportbutton:tendril=tendril.create(0,280,300,240,80,20,8,300,0,0,0,.5,0,0,150,.5,"Server Port: ",255,255,255,.75)
	clientportbutton.vx1=150
	clientportbutton.vx2=-200
	clientportentry:textentry=textentry.create("Server Port: ","6791")
	clientportbutton.addmenuitem clientportentry
	clientmenu.addtendril clientportbutton

	clientnamebutton:tendril=tendril.create(800,280,580,310,80,20,8,-280,150,150,0,.5,255,255,0,.5,"Name: ",255,255,255,.6)
	clientnamebutton.vx1=-150
	clientnamebutton.vx2=150
	clientnameentry:textentry=textentry.create("Name: ","Player")
	clientnamebutton.addmenuitem clientnameentry
	clientmenu.addtendril clientnamebutton

	clientredbutton:tendril=tendril.create(0,360,300,380,80,20,8,300,100,0,0,.5,255,0,0,.5,"Red: ",255,255,255,.75)
	clientredbutton.vx1=150
	clientredbutton.vx2=-200
	clientredentry:textentry=textentry.create("Red: ","255")
	clientredbutton.addmenuitem clientredentry
	clientmenu.addtendril clientredbutton

	clientgreenbutton:tendril=tendril.create(800,360,580,450,80,20,8,-280,0,100,0,.5,0,255,0,.5,"Green: ",255,255,255,.75)
	clientgreenbutton.vx1=-150
	clientgreenbutton.vx2=150
	clientgreenentry:textentry=textentry.create("Green: ","255")
	clientgreenbutton.addmenuitem clientgreenentry
	clientmenu.addtendril clientgreenbutton

	clientbluebutton:tendril=tendril.create(0,440,300,520,80,20,8,300,0,0,100,.5,0,0,255,.5,"Blue: ",255,255,255,.75)
	clientbluebutton.vx1=150
	clientbluebutton.vx2=-200
	clientblueentry:textentry=textentry.create("Blue: ","255")
	clientbluebutton.addmenuitem clientblueentry
	clientmenu.addtendril clientbluebutton

	clientmenubackbutton:tendril=tendril.create(100,800,300,590,80,20,8,300,0,0,0,.5,100,100,100,.5,"Back to Main Menu",255,255,255,1)
	clientmenubackbutton.vy1=-150
	clientmenubackbutton.vx2=-200
	clientmenu.addtendril clientmenubackbutton
	
	EndRem
	
	'///SINGLEPLAYER SCORES MENU
	spscoresmenu:menu=New menu
	spscoresbutton:tendril=tendril.create(0,185,200,185,360,20,30,380,100,100,100,.5,100,100,255,.5,"",255,255,255,1,0)
	spscoresbutton.vx1=100
	spscoresbutton.vx2=-100
	spscoresbutton.addmenuitem (New scorelist)
	spscoresmenu.addtendril spscoresbutton
	
	spscoresplayagainbutton:tendril=tendril.create(800,380,580,420,80,30,8,-280,100,0,0,.5,255,0,0,.5,"Play Again",255,255,255,1)
	spscoresplayagainbutton.vx1=-150
	spscoresplayagainbutton.vx2=150
	spscoresmenu.addtendril spscoresplayagainbutton
	
	spchangelevelbutton:tendril=tendril.create(0,380,300,490,80,20,8,300,0,100,0,.5,255,0,0,.5,"Change Level",255,255,255,1)
	spchangelevelbutton.vx1=150
	spchangelevelbutton.vx2=-200
	spscoresmenu.addtendril spchangelevelbutton

	spscoresmenubackbutton:tendril=tendril.create(100,800,300,590,80,20,8,300,0,0,0,.5,100,100,100,.5,"Back to Main Menu",255,255,255,1)
	spscoresmenubackbutton.vy1=-150
	spscoresmenubackbutton.vx2=-200
	spscoresmenu.addtendril spscoresmenubackbutton
	
	Rem
	'///SERVER SCORES MENU
	serverscoresmenu:menu=New menu
	serverscoresbutton:tendril=tendril.create(0,225,200,225,440,20,30,380,100,100,100,.5,100,100,255,.5,"",255,255,255,1,0)
	serverscoresbutton.vx1=100
	serverscoresbutton.vx2=-100
	serverscoresbutton.addmenuitem (New scorelist)
	serverscoresmenu.addtendril serverscoresbutton
	
	serverscoresplayagainbutton:tendril=tendril.create(800,460,580,500,80,30,8,-280,100,0,0,.5,255,0,0,.5,"Play Again",255,255,255,1)
	serverscoresplayagainbutton.vx1=-150
	serverscoresplayagainbutton.vx2=150
	serverscoresmenu.addtendril serverscoresplayagainbutton

	serverscoresmenubackbutton:tendril=tendril.create(100,800,300,590,80,20,8,300,0,0,0,.5,100,100,100,.5,"Back to Main Menu",255,255,255,1)
	serverscoresmenubackbutton.vy1=-150
	serverscoresmenubackbutton.vx2=-200
	serverscoresmenu.addtendril serverscoresmenubackbutton
	
	'///CLIENT SCORES MENU
	clientscoresmenu:menu=New menu
	clientscoresbutton:tendril=tendril.create(0,225,200,225,440,20,30,380,100,100,100,.5,100,100,255,.5,"",255,255,255,1,0)
	clientscoresbutton.vx1=100
	clientscoresbutton.vx2=-100
	clientscoresbutton.addmenuitem (New scorelist)
	clientscoresmenu.addtendril clientscoresbutton
	
	clientcoresmenubackbutton:tendril=tendril.create(100,800,300,590,80,20,8,300,0,0,0,.5,100,100,100,.5,"Back to Main Menu",255,255,255,1)
	clientcoresmenubackbutton.vy1=-150
	clientcoresmenubackbutton.vx2=-200
	clientscoresmenu.addtendril clientcoresmenubackbutton
	EndRem
	
	'///INGAME MENU
	ingamemenu:menu=New menu

	returntogamebutton:tendril=tendril.create(0,200,300,100,80,20,8,300,100,0,0,.8,255,0,0,.8,"Return to Game",255,255,255,1)
	returntogamebutton.vx1=150
	returntogamebutton.vx2=-200
	ingamemenu.addtendril returntogamebutton
	
	ingameoptionsbutton:tendril=tendril.create(800,280,580,310,80,30,8,-280,100,0,100,.5,255,0,255,.5,"Options",255,255,255,1)
	ingameoptionsbutton.vx1=-150
	ingameoptionsbutton.vx2=150
	ingamemenu.addtendril ingameoptionsbutton

	ingameendbutton:tendril=tendril.create(800,560,580,590,80,30,8,-280,0,0,0,.8,150,150,150,.8,"End Game",255,255,255,1)
	ingameendbutton.vx1=-150
	ingameendbutton.vx2=150
	ingamemenu.addtendril ingameendbutton
	
	'///EDITOR MENU
	editormenu:menu=New menu
	
	returntoeditorbutton:tendril=tendril.create(0,200,300,100,80,20,8,300,100,0,0,.8,255,0,0,.8,"Return to Editor",255,255,255,1)
	returntoeditorbutton.vx1=150
	returntoeditorbutton.vx2=-200
	editormenu.addtendril returntoeditorbutton
	
	editorfilenamebutton:tendril=tendril.create(800,200,580,170,80,30,8,-280,100,0,100,.5,255,0,255,.5,"Filename: ",255,255,255,.75)
	editorfilenamebutton.vx1=-150
	editorfilenamebutton.vx2=150
	editorfilenameentry:textentry=textentry.create("Filename: ","editor")
	editorfilenamebutton.addmenuitem editorfilenameentry
	editormenu.addtendril editorfilenamebutton

	editortestbutton:tendril=tendril.create(0,200,300,240,80,20,8,300,0,0,100,.8,0,0,255,.8,"Test Level",255,255,255,1)
	editortestbutton.vx1=150
	editortestbutton.vx2=-200
	editormenu.addtendril editortestbutton

	editorloadfilebutton:tendril=tendril.create(800,280,580,310,80,30,8,-280,100,0,100,.5,255,0,255,.5,"Load Level",255,255,255,1)
	editorloadfilebutton.vx1=-150
	editorloadfilebutton.vx2=150
	editormenu.addtendril editorloadfilebutton
	
	editorsettingsbutton:tendril=tendril.create(0,280,300,380,80,20,8,300,0,0,100,.8,0,0,255,.8,"Level Settings",255,255,255,1)
	editorsettingsbutton.vx1=150
	editorsettingsbutton.vx2=-200
	editormenu.addtendril editorsettingsbutton
	
	editorbotsmenubutton:tendril=tendril.create(800,360,580,450,80,30,8,-280,100,0,100,.5,255,0,255,.5,"Bot Settings",255,255,255,1)
	editorbotsmenubutton.vx1=-150
	editorbotsmenubutton.vx2=150
	editormenu.addtendril editorbotsmenubutton

	editorquitbutton:tendril=tendril.create(800,560,580,590,80,30,8,-280,0,0,0,.8,150,150,150,.8,"Quit Editor",255,255,255,1)
	editorquitbutton.vx1=-150
	editorquitbutton.vx2=150
	editormenu.addtendril editorquitbutton
	
	
	'///EDITOR SETTINGS MENU
	editorsettingsmenu:menu=New menu
	
	esmenuokbutton:tendril=tendril.create(50,800,220,590,80,30,8,160,0,0,0,.5,0,0,200,.5,"OK",255,255,255,1)
	esmenuokbutton.vy1=-150
	esmenuokbutton.vx2=-200
	editorsettingsmenu.addtendril esmenuokbutton
	
	esmenucancelbutton:tendril=tendril.create(750,800,580,590,80,30,8,-160,0,0,0,.5,200,0,0,.5,"Cancel",255,255,255,1)
	esmenucancelbutton.vy1=-150
	esmenucancelbutton.vx2=200
	editorsettingsmenu.addtendril esmenucancelbutton

	esradiusbutton:tendril=tendril.create(0,200,300,100,80,20,8,300,0,0,100,.8,0,0,255,.8,"Radius",255,255,255,.6,1)
	esradiusbutton.vx1=150
	esradiusbutton.vx2=-200
	radiusslider:slider=slider.create(30,150,1)
	esradiusbutton.addmenuitem radiusslider
	editorsettingsmenu.addtendril esradiusbutton
	
	esroundlengthbutton=tendril.create(800,200,580,170,80,30,8,-280,100,0,100,.5,255,0,255,.5,"Round Length",255,255,255,.6,1)
	esroundlengthbutton.vx1=-150
	esroundlengthbutton.vx2=150
	roundlengthslider:slider=slider.create(10,120,1)
	esroundlengthbutton.addmenuitem roundlengthslider
	editorsettingsmenu.addtendril esroundlengthbutton
	
	esrepopbutton:tendril=tendril.create(0,280,300,240,80,20,8,300,0,0,100,.8,0,0,255,.8,"Pop Rate",255,255,255,.6,1)
	esrepopbutton.vx1=150
	esrepopbutton.vx2=-200
	repopslider:slider=slider.create(5,50,1)
	esrepopbutton.addmenuitem repopslider
	editorsettingsmenu.addtendril esrepopbutton

	esminrepopbutton:tendril=tendril.create(800,280,580,310,80,30,8,-280,100,0,100,.5,255,0,255,.5,"Min. Rate",255,255,255,.6,1)
	esminrepopbutton.vx1=-150
	esminrepopbutton.vx2=150
	minrepopslider:slider=slider.create(5,50,1)
	esminrepopbutton.addmenuitem minrepopslider
	editorsettingsmenu.addtendril esminrepopbutton

	'///EDITOR BOTS MENU
	editorbotsmenu:menu=New menu
	
	ebaddbotbutton:tendril=tendril.create(0,200,300,100,80,20,8,300,0,0,100,.8,0,0,255,.8,"Add Bot",255,255,255,.6,1)
	ebaddbotbutton.vx1=150
	ebaddbotbutton.vx2=-200
	editorbotsmenu.addtendril ebaddbotbutton
	
	ebpickerbutton:tendril=tendril.create(800,200,580,170,80,30,8,-280,100,0,100,.5,255,0,255,.5,"Bot: ",255,255,255,.6)
	ebpickerbutton.vx1=-150
	ebpickerbutton.vx2=150
	botpicker:botchoice=New botchoice
	ebpickerbutton.addmenuitem botpicker
	editorbotsmenu.addtendril ebpickerbutton
	
	ebnamebutton:tendril=tendril.create(0,280,300,240,80,20,8,300,0,0,0,.5,0,0,150,.5,"Name: ",255,255,255,.75)
	ebnamebutton.vx1=150
	ebnamebutton.vx2=-200
	ebnameentry:textentry=textentry.create("Name: ","")
	ebnamebutton.addmenuitem ebnameentry
	editorbotsmenu.addtendril ebnamebutton
	
	ebskillbutton:tendril=tendril.create(800,280,580,310,80,30,8,-280,100,0,100,.5,255,0,255,.5,"Skill: ",255,255,255,.6)
	ebskillbutton.vx1=-150
	ebskillbutton.vx2=150
	skillslider:slider=slider.create(1,10,1)
	ebskillbutton.addmenuitem skillslider
	editorbotsmenu.addtendril ebskillbutton

	ebredbutton:tendril=tendril.create(0,360,300,380,80,20,8,300,0,0,0,.5,150,0,0,.5,"Red: ",255,255,255,.75)
	ebredbutton.vx1=150
	ebredbutton.vx2=-200
	ebredslider:slider=slider.create(0,255,1)
	ebredbutton.addmenuitem ebredslider
	editorbotsmenu.addtendril ebredbutton
				
	ebgreenbutton:tendril=tendril.create(800,360,580,450,80,30,8,-280,0,0,0,.5,0,255,0,.5,"Green: ",255,255,255,.6)
	ebgreenbutton.vx1=-150
	ebgreenbutton.vx2=150
	ebgreenslider:slider=slider.create(0,255,1)
	ebgreenbutton.addmenuitem ebgreenslider
	editorbotsmenu.addtendril ebgreenbutton

	ebbluebutton:tendril=tendril.create(0,440,300,520,80,20,8,300,0,0,0,.5,0,0,150,.5,"Blue: ",255,255,255,.75)
	ebbluebutton.vx1=150
	ebbluebutton.vx2=-200
	ebblueslider:slider=slider.create(0,255,1)
	ebbluebutton.addmenuitem ebblueslider
	editorbotsmenu.addtendril ebbluebutton

	ebmenubackbutton:tendril=tendril.create(100,800,300,590,80,20,8,300,0,0,0,.5,100,100,100,.5,"Back to Menu",255,255,255,1)
	ebmenubackbutton.vy1=-150
	ebmenubackbutton.vx2=-200
	editorbotsmenu.addtendril ebmenubackbutton
	
	mymenu=mainmenu
End Function

Function levelslist:TList(sp=1,mp=1)
	levels:TList=New TList
	Local files$[]
	files=LoadDir("levels")
	For filename$=EachIn files
		If filename[filename.length-4..filename.length]=".txt"
			shortname$=filename[0..filename.length-4]
			Select filename[0..3]
			Case "sp_"
				If sp Then levels.addlast shortname
			Case "mp_"
				If mp Then levels.addlast shortname
			Default
				levels.addlast shortname
			End Select
		EndIf
	Next
	Return levels
End Function

Function changemenu(m:menu,smooth=1)
	If m=mainmenu Then prevmenu=Null Else prevmenu=mymenu
	mymenu.close()
	If smooth
		nextmenu=m
	Else
		mymenu.reset()
		mymenu=m
		mymenu.open()
	EndIf
End Function

Function getinput$(res$,def$="")
	If res="" Then Return def Else Return res
End Function

Function domenus()
	If KeyHit(KEY_ESCAPE)
		If prevmenu
			changemenu(prevmenu)
			prevmenu=Null
		Else
			doquittingmenu()
		EndIf
	EndIf	
	
	If quittingmenu
		If tendril(mymenu.tendrils.first()).targtick=0
			quitmenu=1
			quittingmenu=0
		EndIf
	EndIf
	
	updatemouse()
	'mx=MouseX()
	'my=MouseY()
	key=GetChar()
	
	mymenu.update()
	
	If server Then server.update
	If myclient Then myclient.update()
	
	If MouseHit(1)
		For t:tendril=EachIn mymenu.tendrils
			t.active=0
		Next
		If mymenu.hover
			Select mymenu.hover
			Case mainmenuoptionsbutton,ingameoptionsbutton
				changemenu(optionsmenu)
			Case singleplayerbutton,spchangelevelbutton
				changemenu(spmenu)
			Case spgobutton,spscoresplayagainbutton
				netmode=0
				playername$=getinput(spnamebutton.entrytext,"Player")
				level$=getinput(splevelbutton.entrytext,"test")
				difficulty#=slider(spdifficultybutton.mymenuitem).getvalue()
				difficulty=difficulty/10.0
				FlushKeys
				rungame(level,playername,difficulty)
				changemenu(spscoresmenu)
				prevmenu=mainmenu
				mymenu.reset()
			Case servermenubutton
				changemenu(servermenu)
			Case joingamebutton
				changemenu(clientmenu)
			Case editorbutton
				runeditor()
			Case quitbutton
				quitgame=1
			Case optionsmenuokbutton
				updateoptions()
				changemenu(prevmenu)
			Case servergobutton
				port=Int(getinput(serverportbutton.entrytext,6791))
				level$=getinput(serverlevelbutton.entrytext,"test")
				server=netserver.create(port)
				netmode=1
				rungame(level)
				changemenu(serverscoresmenu)
				prevmenu=servermenu
				server.reset()
				mymenu.reset()
				'server.disconnect()
			Case clientgobutton
				ip$=getInput(clientipbutton.entrytext,"127.0.0.1")
				port=Int(getInput(clientportbutton.entrytext,6791))
				name$=getInput(clientnamebutton.entrytext,"Player")
				red=Int(getInput(clientredbutton.entrytext,255))
				green=Int(getInput(clientgreenbutton.entrytext,255))
				blue=Int(getInput(clientbluebutton.entrytext,255))
				myclient:localclient=localclient.create(ip,port,name,red,green,blue)
				If myclient
					netmode=2
					'rungame()
					'myclient.disconnect()
				EndIf
			Case serverscoresplayagainbutton
				rungame(level)
				changemenu(serverscoresmenu,0)
				server.reset()
				prevmenu=servermenu
				'mymenu.reset()
			Case clientmenubackbutton,spscoresmenubackbutton,serverscoresmenubackbutton,clientscoresmenubackbutton,optionsmenucancelbutton
				changemenu(prevmenu)
			Case servermenubackbutton
				changemenu(mainmenu)
			Case returntogamebutton,returntoeditorbutton
				doquittingmenu()
			Case ingameendbutton
				finishing=1
				doquittingmenu()
			Case editorloadfilebutton
				bots=New tlist
				walls=New tlist
				selectedwall=Null
				initgame(editorfilenamebutton.entrytext)
			Case editorquitbutton
				finishing=1
				doquittingmenu()
				changemenu(mainmenu)
			Case editortestbutton
				savelevel()
				netmode=0
				playername$="Player"
				level$=editorfilenamebutton.entrytext
				FlushKeys
				bots:tlist=New tlist
				walls:tlist=New tlist
				sweeps:tlist=New tlist
				rungame(level,playername)
				changemenu(editormenu)
				prevmenu=Null
				mymenu.reset()
				'runeditor()
				initgame(level)
			Case editorsettingsbutton
				changemenu(editorsettingsmenu)
			Case esmenuokbutton
				updateeditorsettings()
				changemenu(prevmenu)
			Case esmenucancelbutton
				changemenu(prevmenu)
			Case editorbotsmenubutton
				changemenu(editorbotsmenu)
			Case ebaddbotbutton
				bot:ai=ai.create("Bot "+String(bots.count()+1),100,100,100,1)
				botchoice(ebpickerbutton.mymenuitem).setchoice(bot)
			Case ebmenubackbutton
				changemenu(prevmenu)
			Default
				selected=mymenu.hover
				selected.active=1
			End Select
		EndIf
	EndIf
	
	If nextmenu
		If mymenu.first.targtick=0
			mymenu.reset()
			leavemenu()
			mymenu=nextmenu
			mymenu.open()
			entermenu()
			nextmenu=Null
		EndIf
	EndIf
	
	omenutime=menutime
	menutime=MilliSecs()
	menufps#=1000.0/(menutime-omenutime)
	'SetColor 255,255,255
	'DrawText "FPS: "+String(fps),0,30
	
	menutick:+1

End Function

Function doquittingmenu()
	quittingmenu=1
	mymenu.close()
End Function

Function leavemenu()
	Select mymenu
	Case serverscoresmenu
		server.disconnect()
	Case clientscoresmenu
		myclient.disconnect()
	End Select
End Function

Function entermenu()
	Select mymenu
	Case spmenu
		splevelchoice.list=levelslist(1,0)
	Case servermenu
		mplevelchoice.list=levelslist(0,1)
	Case optionsmenu
		'fill in
		toggle(optionsfullscreenbutton.mymenuitem).yes=fullscreen
		toggle(optionstabletmodebutton.mymenuitem).yes=tabletmode
		toggle(optionspoponreleasebutton.mymenuitem).yes=poponrelease
		toggle(optionsalwayspushbutton.mymenuitem).yes=alwayspush
	Case editorsettingsmenu
		slider(esradiusbutton.mymenuitem).setvaluefrom(radius)
		slider(esroundlengthbutton.mymenuitem).setvaluefrom(roundlength)
		slider(esrepopbutton.mymenuitem).setvaluefrom(repop)
		slider(esminrepopbutton.mymenuitem).setvaluefrom(minrepop)
	Case editorbotsmenu
		If bots.count()
			botchoice(ebpickerbutton.mymenuitem).setchoice(ai(bots.first()))
		Else
			botchoice(ebpickerbutton.mymenuitem).setchoice(Null)
		EndIf
	End Select
End Function

Function updateoptions()
	ofullscreen=fullscreen
	fullscreen=toggle(optionsfullscreenbutton.mymenuitem).yes
	If fullscreen<>ofullscreen Then initgfx()

	tabletmode=toggle(optionstabletmodebutton.mymenuitem).yes
	poponrelease=toggle(optionspoponreleasebutton.mymenuitem).yes
	alwayspush=toggle(optionsalwayspushbutton.mymenuitem).yes
	
	saveoptions()
End Function

Function updateeditorsettings()
	radius=slider(esradiusbutton.mymenuitem).getvalue()
	roundlength=slider(esroundlengthbutton.mymenuitem).getvalue()
	repop=slider(esrepopbutton.mymenuitem).getvalue()
	minrepop=slider(esminrepopbutton.mymenuitem).getvalue()
	If repop<minrepop repop=minrepop
End Function

'singleplayerbutton.nxt=servermenubutton
'servermenubutton.nxt=joingamebutton
'servermenubutton.prv=singleplayerbutton
'joingamebutton.prv=servermenubutton

'////CONSTANTS
Const menuspeed#=.2

'///MENU BITS
Global mymenu:menu,nextmenu:menu,prevmenu:menu
Global mainmenu:menu,spmenu:menu,servermenu:menu,clientmenu:menu,spscoresmenu:menu,serverscoresmenu:menu,clientscoresmenu:menu,ingamemenu:menu,optionsmenu:menu,editormenu:menu,editorsettingsmenu:menu,editorbotsmenu:menu
Global splevelchoice:listchoice,mplevelchoice:listchoice
Global singleplayerbutton:tendril,servermenubutton:tendril,joingamebutton:tendril,mainmenuoptionsbutton:tendril,editorbutton:tendril,quitbutton:tendril
Global spgobutton:tendril,spnamebutton:tendril,splevelbutton:tendril,spdifficultybutton:tendril
Global servergobutton:tendril,serverportbutton:tendril,serverlevelbutton:tendril,servermenubackbutton:tendril
Global clientgobutton:tendril,clientipbutton:tendril,clientportbutton:tendril,clientmenubackbutton:tendril,clientnamebutton:tendril,clientredbutton:tendril,clientgreenbutton:tendril,clientbluebutton:tendril
Global spscoresbutton:tendril,spscoresmenubackbutton:tendril,spscoresplayagainbutton:tendril,spchangelevelbutton:tendril
Global serverscoresbutton:tendril,serverscoresmenubackbutton:tendril,serverscoresplayagainbutton:tendril
Global clientscoresbutton:tendril,clientscoresmenubackbutton:tendril
Global returntogamebutton:tendril,ingameendbutton:tendril,ingameoptionsbutton:tendril
Global optionsfullscreenbutton:tendril,optionstabletmodebutton:tendril,optionspoponreleasebutton:tendril,optionsalwayspushbutton:tendril,optionsmenucancelbutton:tendril,optionsmenuokbutton:tendril
Global returntoeditorbutton:tendril,editorfilenamebutton:tendril,editorloadfilebutton:tendril,editortestbutton:tendril,editorsettingsbutton:tendril,editorbotsmenubutton:tendril,editorquitbutton:tendril
Global esmenuokbutton:tendril,esmenucancelbutton:tendril,esradiusbutton:tendril,esroundlengthbutton:tendril,esrepopbutton:tendril,esminrepopbutton:tendril
Global ebmenubackbutton:tendril,ebaddbotbutton:tendril,ebpickerbutton:tendril,ebnamebutton:tendril,ebskillbutton:tendril,ebredbutton:tendril,ebgreenbutton:tendril,ebbluebutton:tendril

Global selected:tendril

'///TIMING
Global menutick,menutime,quitgame,quitmenu,quittingmenu
Global key

loadoptions()

initgfx()
quitgame=0
initmenu()
SetBlend ALPHABLEND
While Not quitgame
	menutick=0
	menutime=MilliSecs()
	done=0
	mymenu.reset()
	mymenu.open()
	While Not quitgame
	
		domenus()
		
		If quitmenu Then quitgame=1
		
		drawscaleline mx-2,my-2,mx+2,my+2
		
		'SetClsColor 0,Sin(tick-90)*25+25,0
		Flip
		Cls
		'FlushMem
	Wend
	ShowMouse
Wend