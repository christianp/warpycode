Import warpy.zoomgraphics
Import warpy.jsondecoder
Include "fileio.bmx"

Global constructors:tmap=New tmap

Function addconstructor(name$,func:TActor(j:jsonobject))
	TFuncObject.Create name,func
End Function

Function constructactor(kind$,ao:jsonobject)
	f:TFuncObject=TFuncObject(constructors.valueforkey(kind))
	f.func ao
End Function

Type TFuncObject
	Field func:TActor(j:jsonobject)
	Function Create:TFuncObject(name$,func:TActor(j:jsonobject))
		f:TFuncObject=New TFuncObject
		f.func=func
		constructors.insert name,f
		Return f
	End Function
End Type

Global actors:tmap
Type TActor
	Field name$
	Field components:tmap
	
	Method New()
		components=New tmap
	End Method
	
	Method register()
		actors.insert name,Self
	End Method
	
	Method unregister()
		actors.remove name
	End Method
	
	Function Create:TActor(j:jsonobject) Abstract
	
	Method addcomponent(a:TActor,name$)
		components.insert name,a
	End Method
	
	Function fetch:TActor(name$)
		Local args$[]
		args=name.split(".")
		a:TActor=TActor(actors.valueforkey(args[0]))
		Return a.fetchcomponent(args[1..])
	End Function
	
	Method fetchcomponent:TActor(args$[])
		If Not Len(args) Return Self
		a:TActor=TActor(components.valueforkey(args[0]))
		Return a.fetchcomponent(args[1..])
	End Method

	Method fetchfield:Object(args$[])
		If Not Len(args) Return Self
		Select args[0]
		Case "name"
			Return name
		Default
			If components.contains(args[0])
				a:TActor=fetchcomponent(args[0])
				Return a.fetchfield(args[1..])
			EndIf
		End Select
	End Method
	
		
	
	Method truth(args$[]) Abstract
	
	Method do(args$[])
		Select args[0]
		Case "addcomponent"
			cname$=args[1]
			a:TActor=TActor.fetch(args[2])
			Print "add component "+a.name+" as "+cname+" to "+name
			addcomponent a,cname
		Case "kill"
			unregister
		End Select
	End Method
End Type

addconstructor "play",TPlay.Create
Type TPlay Extends TActor
	Method New()
		name$="play"
		register
	End Method
	
	Function Create:TActor(j:jsonobject)
		New tplay
	End Function
	
	Method truth(args$[])
	End Method
	
	Method do(args$[])
		Select args[0]
		Case "end"
			state=Null
		Case "eval" 'debug, test out evaluation
			expr$=" ".join(args[1..])
			res:Object=eval(expr)
		Default
			Super.do(args)
		End Select
	End Method
End Type

addconstructor "user",TUser.Create
Type TUser Extends TActor
	Method New()
		name$="user"
		register
	End Method
	
	Function Create:TActor(j:jsonobject)
		Return New tuser
	End Function

	
	Method truth(args$[])
		Select args[0]
		Case "keyhit"
			code=Int(args[1])
			If KeyHit(code) Return 1
		End Select
	End Method
	
	Method do(args$[])
		Select args[0]
		Default
			Super.do(args)
		End Select
	End Method
End Type

addconstructor "narrative",TNarrative.Create
Type TNarrative Extends TActor
	Field stage
	Field paragraphs$[]
	
	Function Create:TNarrative(j:jsonobject)
		n:TNarrative=New TNarrative
		n.name=j.getstringvalue("name")
		n.stage=0
		n.paragraphs=j.getstringvalue("text").split("~n~n")
		n.register
		Return n
	End Function
	
	Method fetchfield:Object(args$[])
		Select args[0]
		Case "stage"
			Return String(stage)
		Case "paragraph"
			Return paragraphs[Int(args[1])]
		Default
			Return Super.fetchfield(args)
		End Select
	End Method
	
	Method truth(args$[])
		Select args[0]
		Case "stage"
			If stage=Int(args[1]) Return 1
		Case "finished"
			If stage>=Len(paragraphs) Return 1
		End Select
	End Method

	Method do(args$[])
		Select args[0]
		Case "advance"
			Print paragraphs[stage]+"~n"
			stage:+1
		Default
			Super.do(args)
		End Select
	End Method
End Type

Global faces:TList
addconstructor "face",TFace.Create
Type TFace Extends TActor
	Field image:Timage
	Field x#,y#
	Field scale#
	
	Method New()
		scale=1
		faces.addlast Self
	End Method
	
	Function Create:TFace(j:jsonobject)
		f:TFace=New TFace
		f.name=j.getstringvalue("name")
		f.image=LoadImage(j.getstringvalue("image"))
		f.x=j.getnumbervalue("x")
		f.y=j.getnumbervalue("y")
		f.register
		Return f
	End Function
	
	Method fetchfield:Object(args$[])
		Select args[0]
		Case "image"
			Return image
		Case "x"
			Return String(x)
		Case "y"
			Return String(y)
		Case "scale"
			Return String(scale)
		Default
			Return Super.fetchfield(args)
		End Select
	End Method
	
	Method truth(args$[])
		Select args[0]
		Case "x"
			If Int(x)=Int(args[1]) Return 1
		Case "y"
			If Int(y)=Int(args[1]) Return 1
		End Select
	End Method
	
	Method do(args$[])
		Select args[0]
		Case "move"
			x=Float(args[1])
			y=Float(args[2])
		Case "x"
			x=Float(args[1])
		Case "y"
			y=Float(args[1])
		Case "scale"
			scale=Float(args[1])
		Case "size"
			size=Float(args[1])/ImageWidth(image)
		Default
			Super.do(args)
		End Select
	End Method
	
	Method draw()
		drawzoomimage image,x,y,ImageWidth(image)*scale
	End Method
End Type

Global speechbubbles:TList=New TList
Type TSpeechBubble Extends TActor
	Field txt$
	Field width#,height#
	Field x#,y#
	
	Method New()
		speechbubbles.addlast Self
	End Method
	
	Function Create:TActor(j:jsonobject)
		s:TSpeechBubble=New TSpeechBubble
		s.name$=j.getstringvalue("name")
		s.txt=j.getstringvalue("text")
		s.x=j.getnumbervalue("x")
		s.y=j.getnumbervalue("y")
		s.fit
		Return s
	End Function
	
	Method fetchfield:Object(args$[])
		Select args[0]
		Case "text"
			 Return text
		Case "x"
			Return String(x)
		Case "y"
			Return String(y)
		Case "width"
			Return String(width)
		Case "height"
			Return String(height)
		Default
			Return Super.fetchfield(args)
		End Select
	End Method
	
	Method fit()
		area#=TextWidth(txt)*TextHeight(txt)
		side#=Sqr(area)
		width=area*Sqr(4.0/3)
		height=area/Sqr(4.0/3)
	End Method
	
	Method truth(args$[])
	End Method
	
	Method do(args$[])
		Select args[0]
		Case "kill"
			speechbubbles.remove Self
			Super.do args
		End Select
	End Method
End Type

Global states:tmap
Type TState
	Field name$
	Field transitions:TList
	Field actions:TList
	
	Method New()
		transitions=New TList
		actions=New TList
	End Method
	
	Function fetch:TState(name$)
		Return TState(states.valueforkey(name))
	End Function
	
	Function Create:Tstate(name$,actions$)
		s:tstate=New tstate
		s.name=name
		states.insert s.name,s
		s.addactions actions
		Return s
	End Function
	
	Method addactions(txt$)
		If Not txt Return
		For line$=EachIn txt.split("~n")
			line=Trim(line)
			addaction line
		Next
	End Method
	
	Method addaction(txt$)
		actions.addlast split(txt," ")
	End Method
	
	Method do()
		Local args$[]
		For args=EachIn actions
			a:TActor=TActor.fetch(args[0])
			a.do args[1..]
		Next
	End Method
	
	Method nxt:TState()
		For t:TTransition=EachIn transitions
			If t.truth() Return TState.fetch(t.nxt)
		Next
		Return Self
	End Method
	
	Method link(expression$,nxt$)
		transitions.addlast TTransition.Create(expression,nxt)
	End Method
End Type

Type TTransition
	Field expressions:TList
	Field nxt$

	Method New()
		expressions=New TList
	End Method
	
	Method truth()
		Local args$[]
		For args=EachIn expressions
			a:TActor=TActor.fetch(args[0])
			If Not a.truth(args[1..]) Return 0
		Next
		Return 1
	End Method
	
	Function Create:TTransition(txt$,nxt$)
		t:TTransition=New TTransition

		t.addexpressions txt		
		
		t.nxt=nxt
		
		Return t
	End Function
	
	Method addexpressions(txt$)
		If Not txt Return
		For line$=EachIn txt.split("~n")
			line=Trim(line)
			expressions.addlast line.split(" ")
		Next
	End Method
	
End Type

Global finishstate:tstate
Function initscene()
	actors=New tmap
	New TPlay
	New TUser
	faces=New TList
	speechbubbles=New TList
	states=New tmap

	tstate.Create "start",""
	finishstate=tstate.Create( "finish","")

End Function

Function loadscene(filename$)
	jd:jsondecoder=jsondecoder.Create(readall(ReadFile(filename)))
	jd.parse
	j:jsonobject=jsonobject(jd.things.first())
	
	aa:jsonarray=j.getarrayvalue("actors")
	sa:jsonarray=j.getarrayvalue("states")
	
	For ao:jsonobject=EachIn aa.values
		kind$=ao.getstringvalue("actor")
		constructactor kind,ao
	Next
	
	For so:jsonobject=EachIn sa.values
		name$=so.getstringvalue("name")
		s:TState=TState.Create(name,"")
		acta:jsonarray=so.getarrayvalue("actions")
		For sv:jsonstringvalue=EachIn acta.values
			s.addaction sv.txt
		Next	

		ta:jsonarray=so.getarrayvalue("transitions")
		For tra:jsonarray=EachIn ta.values
			expression$=jsonstringvalue(tra.values.valueatindex(0)).txt
			nxt$=jsonstringvalue(tra.values.valueatindex(1)).txt
			s.link expression,nxt
		Next
	Next
End Function

gwidth=800
gheight=800
Graphics gwidth,gheight,0
SetBlend ALPHABLEND

'n:TNarrative=TNarrative.Create("narrative","Once upon a time,~nIn a land called Hamble,~n~nThere was a small boy who liked To fish.~n~nHe went To the shops one day And bought a can of sardines.~n~nHe wanted some oranges as well.~n~nBut they were Not served at the fish counter.")
's1:TState=TState.Create( "wait for key", "" )
's2:TState=TState.Create( "advance","narrative advance" )
's1.link "narrative finished","finish"
's1.link "user keyhit 32","advance"
's2.link "","wait for key"
initscene
loadscene "scene.txt"


Global state:tstate=TState.fetch("start")
While state<>finishstate
	state.do
	state=state.nxt()

	For f:TFace=EachIn faces
		f.draw
	Next

	DrawText state.name,0,0
	Flip
	Cls
	
	If KeyHit(KEY_ESCAPE) Or AppTerminate()
		End
	EndIf
Wend

txt$="Fin."
DrawText txt,gwidth/2-TextWidth(txt)/2,gheight/2-TextHeight(txt)/2
Flip
WaitKey