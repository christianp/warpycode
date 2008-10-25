Type machine
	Field start:state
	Field h:head
	Field states:tlist
	Field in$
	Field result
	
	Method New()
		states = New tlist
		h=New head
	End Method
	
	Method addstate(s:state) 
		If states.count()=0
			start = s
			Print "!!!"
		EndIf
		states.addlast s
	End Method
	
	Method begin(inp$)
		h.c = start
		in = inp
		result = 0
	End Method
	
	Method run()
		If h.d >= 1
			h.d = 0
			h.c = h.t
		EndIf
		If h.d = 0
			If Len(in) = 0
				result = 1
				Return
			EndIf
			h.label = Chr(in[0])
			h.t=h.c.move(h.label)
			If h.t = Null
				result = - 1
			EndIf
			in = in[1..]
		'Else
		'	h.d:+.01
		EndIf
			h.d:+.01
	End Method
	
	Method draw()
		For s:state = EachIn states
			s.draw()
		Next
		h.draw()
	End Method
	
	Method positiongraph()
		leftest# = 800
		rightest# = 0
		uppest# = 800
		downest# = 0
		For s:state = EachIn states
			If s.x < leftest leftest = s.x
			If s.x > rightest rightest = s.x
			If s.y < uppest uppest = s.y
			If s.y > downest downest = s.y
		Next
		width# = rightest - leftest
		height# = downest - uppest
		diagonal# = Sqr(width * width + height * height)
		factor#=900/diagonal
		For s:state = EachIn states
			s.position(factor,states)
		Next
	End Method
End Type

Type head
	Field c:state , t:state
	Field d#
	Field stack$
	Field label$
	
	Method draw()
		If c=Null Return
		If t<>Null
			dx# = t.x - c.x
			dy# = t.y - c.y
			x# = c.x + dx * d
			y# = c.y + dy * d
		Else
			x = c.x
			y = c.y
		EndIf
		SetColor 255,255,0
		DrawRect x - 6 , y - 2 , 12 , 4
		DrawText label,x,y+2
	End Method
End Type

Type state
	Field edges:tlist
	Field accepting
	Field x# , y#
	Field name$
	
	Method New()
		edges = New tlist
	End Method
	
	Function create:state(name$,x#,y#,accepting=1) 
		s:state = New state
		s.x = x
		s.y = y
		s.name = name
		s.accepting=accepting
		Return s
	End Function
	
	Method addedge:edge(label$ , target:state)
		For e:edge = EachIn edges
			If e.label = label Return Null
		Next
		e=edge.create(label , target)
		edges.addlast e
		Return e
	End Method
	
	Method move:state(label$)
		For e:edge = EachIn edges
			If e.label = label
				Return e.target
			EndIf
		Next
		Return Null
	End Method
	
	Method draw()
		SetColor 0,0,0
		DrawText name , x , y + 5
		SetLineWidth 2
		For e:edge = EachIn edges
			dx# = e.target.x - x
			dy# = e.target.y - y
			SetColor 255 , 255 , 255
			DrawLine x , y , x + dx , y + dy
			SetColor 0,0,0
			DrawText e.label,x+dx/2,y+dy/2
		Next
		If accepting
			SetColor 0,0,255
			DrawOval x-4,y-4,8,8
		Else
			SetColor 255,0,0
			DrawRect x - 2 , y - 2 , 4 , 4
		EndIf
	End Method
	
	Method position(factor#,states:tlist)
		For t:state = EachIn states
			If t<>Self
				't:state = e.target
				dx# = t.x - x
				dy# = t.y - y
				d# = Sqr(dx * dx + dy * dy)
				f# = d-75*factor
				fx# = f * dx / d
				fy# = f * dy / d
				x:+ fx * .01
				y:+ fy * .01
				t.x:- fx * .01
				t.y:- fy * .01
				'DrawText Int(d),x,y
			EndIf
		Next
	End Method
End Type

Type edge
	Field label$
	Field target:state
	
	Function create:edge(label$ , target:state)
		e:edge = New edge
		e.label = label
		e.target = target
		Return e
	End Function
End Type


Graphics 800 , 800 , 0
SetClsColor 150 , 150 , 150
SetColor 0,0,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

m:machine = New machine
s1:state=state.create("E",400,400) 
m.addstate(s1)
s2:state=state.create("A",500,400,0)
m.addstate(s2 )
s3:state=state.create("B",400,500,0)
m.addstate(s3 )
s4:state=state.create("AB",500,500,0)
m.addstate(s4 )

s1.addedge("a" , s2) 
s1.addedge("b" , s3)
's2.addedge("a" , s1)
s2.addedge("b" , s4)
s3.addedge("a" , s4)


inp$="aaa"
m.begin(inp)
While Not finished
	If KeyHit(KEY_ESCAPE)
		End
	EndIf
	
	DrawText m.in , 0 , 0

	'm.positiongraph()
	m.draw()
	m.run()
	
	Flip
	Cls
Wend