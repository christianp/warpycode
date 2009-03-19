Global dudes:TList
Global possessions:TList
Global possessionid
Global gwidth,gheight
Global done
Global time

Type possession
	Field x#,y#
	Field team
	Field kind$
	Field id
	
	Method New()
		possessions.addlast Self
		id=possessionid
		possessionid:+1
	End Method
	
	Method update()
	End Method
	Method draw()
	End Method
	
	Function find:possession(id)
		For p:possession=EachIn possessions
			If p.id=id Return p
		Next
	End Function
End Type

Type dude Extends possession

	Field superior:dude
	Field inferiors:TList
	Field facts:knowledge
	Field newfacts:knowledge
	Field vision#
	
	Method New()
		kind="dude"
		dudes.addlast Self
		
		inferiors=New TList
		
		facts=New knowledge
	End Method
	
	Method lookaround:knowledge()
		k:knowledge=New knowledge
		For p:possession=EachIn possessions
			If p<>Self
				dx#=p.x - x
				dy#=p.y - y
				dist#=Sqr(dx*dx+dy*dy)
				If dist<vision
					k.addlast fact.Create("$ seen seen "+p.kind+" id "+String(p.id)+" x "+String(p.x)+" y "+String(p.y),"seen id",-1)
				EndIf
			EndIf
		Next
		Return k
	End Method

	Method update()
		If newfacts
			facts.merge newfacts
		EndIf
		newfacts=New knowledge
		
		newfacts.merge lookaround()

		For f:fact=EachIn facts
			If time>f.expire And f.expire<>-1
				facts.remove f
			EndIf
		Next
	End Method
	
	Method draw()
		SetAlpha .5
		DrawOval x-vision,y-vision,vision*2,vision*2
		SetAlpha 1


		DrawRect x-2,y-2,5,5
		DrawText id,x,y
		
		ty=0
		For f:fact=EachIn facts
			Drawlines f.txt,id*120,ty
			ty:+Len(f.txt.split("~n"))*12
		Next
	End Method
	
	
	Method recruit(d:dude)
		inferiors.addlast d
		d.superior=Self
	End Method
End Type

Type soldier Extends dude
	
	Method New()
		orders=New TList
		
		vision=100
	End Method
	
	Function Create:soldier(x#,y#,team)
		s:soldier=New soldier
		s.x=x
		s.y=y
		s.team=team
		Return s
	End Function

	Method update()
		Super.update

		orders:knowledge=facts.match("$ order")
		DrawText orders.count(),x,y
		DrawText facts.match("$ seen").count(),id*30,250
		For f:fact=EachIn facts.match("$ seen")
			p:possession=possession.find(Int(f.getkey("id")))
			DrawLine x,y,p.x,p.y
		Next
		If Not orders.count()
			initiative()
			orders:knowledge=facts.match("$ order")
		EndIf
		
		If orders.count()
			orders.sort(True,ordercmp)
			dby=300
			order:fact=fact(orders.first())
			Select order.getkey("order")
			Case "move"
				tx#=Float(order.getkey("x"))
				ty#=Float(order.getkey("y"))
				
				dx#=tx-x
				dy#=ty-y
				d#=dx*dx+dy*dy
				
				If d<20
					For f:fact=EachIn facts.match("$ order order move")
						facts.remove f
					Next
				Else
					an#=ATan2(dy,dx)
					move(an)
				EndIf
			End Select
		EndIf
		
	End Method
	
	Function ordercmp(o1:Object,o2:Object)
		f1:fact=fact(o1)
		f2:fact=fact(o2)
		p1#=Float(f1.getkey("priority"))
		p2#=Float(f2.getkey("priority"))
		If p1>p2
			Return -1
		Else
			Return 1
		EndIf
	End Function

	Method initiative:TList()
		bigf:fact=Null
		For f:fact=EachIn facts.match("$ seen")
			p:possession=possession.find(Int(f.getkey("id")))
			dx#=p.x-x
			dy#=p.y-y
			d#=dx*dx+dy*dy
				
			Select f.getkey("seen")
			Case "flag"
				If p.team=team
					priority$=1/d
				Else
					priority$=2/d
				EndIf
				facts.addlast fact.Create("$ order order move x "+String(p.x)+" y "+String(p.y)+" priority "+priority, "order")
			End Select
		Next
	End Method
	
	Method move(an#)
		speed#=1
		dx#=Cos(an)*speed
		dy#=Sin(an)*speed
		
		x:+dx
		y:+dy
		
	End Method
	
	Method draw()
		Super.draw
	End Method
End Type

Type officer Extends dude
	Method New()
		vision=200
	End Method
	
	Function Create:officer(x#,y#,team)
		o:officer=New officer
		o.x=x
		o.y=y
		o.team=team
		Return o
	End Function
End Type

Type fact
	Field txt$
	Field words:tmap
	Field keys$[]
	Field expire
	
	Method New()
		words=New tmap
	End Method
	
	Function Create:fact(wordstxt$,keystxt$, expire=-1)
		Local words$[],keys$[]
		words=wordstxt.split(" ")
		keys=keystxt.split(" ")
		f:fact=New fact
		For i=0 To Len(words)-1 Step 2
			f.words.insert words[i], words[i+1]
			f.txt:+words[i]+": "+words[i+1]+"  ~n"
		Next
		f.keys=keys
		f.expire=expire
		Return f
	End Function
	
	Method same(f2:fact)
		If getkey("$")<>f2.getkey("$") Return 0
		For key$=EachIn keys
			k1$=getkey(key)
			k2$=f2.getkey(key)
			If k2 <> k1 Return 0
		Next
		Return 1
	End Method
	
	Method getkey$(key$)
		If Not words.contains(key) Return ""
		Return String(words.valueforkey(key))
	End Method
	
End Type

Type knowledge Extends TList
	
	Method contains(o:Object)
		If fact(o)
			f:fact=fact(o)
			For f2:fact=EachIn Self
				If f2.same(f) Return 1
			Next
			Return 0		
		Else
			Return Super.contains(o)
		EndIf
	End Method
	
	Method merge(k:knowledge)
		l3:TList=New TList
		For f:fact=EachIn k
			If Not contains(f) l3.addlast f
		Next
		For f:fact=EachIn l3
			addlast f
		Next
	End Method
	
	Method match:knowledge( txt$ )
		k:knowledge=New knowledge
		If Not count() Return k

		Local keys$[]
		keys=txt.split(" ")
		
		Local matchkeys$[Len(keys)/2]
		For i=0 To Len(keys)-1 Step 2
			matchkeys[i/2]=keys[i]
		Next
		cf:fact=fact.Create(txt, " ".join(matchkeys))
		
		For f:fact=EachIn Self
			If cf.same(f)
				k.addlast f
			EndIf
		Next
		Return k
	End Method
End Type

Type flag Extends possession
	Method New()
		kind="flag"
	End Method
	
	Function Create:flag(x#,y#,team)
		fl:flag=New flag
		fl.x=x
		fl.y=y
		fl.team=team
		Return fl
	End Function
	
	Method draw()
		an#=Sin(time*3)*30-90
		ex#=x+Cos(an)*20
		ey#=y+Sin(an)*20
		DrawLine x,y,ex,ey
		SetRotation an
		DrawRect ex,ey,15,20
		SetRotation 0
	End Method
End Type

Function drawlines(txt$,x,y)
	Local lines$[]
	lines=txt.split("~n")
	For line$=EachIn lines
		DrawText line,x,y
		y:+12
	Next
End Function

Function initgfx()
	gwidth=800
	gheight=800
	Graphics gwidth,gheight,0
	SetBlend ALPHABLEND
End Function

Global player:officer
Function initgame()
	done=0

	possessions=New TList

	dudes=New TList
	possessionid=0
	
	player=officer.Create(50,50,0)
	
	soldier.Create(100,100,0)
	soldier.Create(700,700,1)
End Function

initgfx()
initgame()

While Not done
	time:+1
	
	If MouseHit(2)
		player.recruit( soldier.Create( MouseX(),MouseY(), player.team) )
	EndIf
	
	If MouseHit(1)
		flag.Create(MouseX(),MouseY(),player.team)
	EndIf
	
	For p:possession=EachIn possessions
		p.update
	Next
	
	For p:possession=EachIn possessions
		p.draw
	Next
	
	Flip
	Cls

	If KeyHit(KEY_ESCAPE) Or AppTerminate()
		done=1
	EndIf
Wend