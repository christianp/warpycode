Type grammar
	Field name$
	Field symbols:tmap
	Field rules:TList
	Field stacks:TList,nstacks:TList,ostacks:TList
	Field in$
	Field interpret(s:sentence)
	
	Method New()
		symbols=New tmap
		rules=New TList
	End Method
	
	Function find:grammar(name$)
		If grammars.contains(name)
			Return grammar(grammars.valueforkey(name))
		EndIf
	End Function
	
	Method addrule(rule$)
		'Print rule
		If rule[0]="#" Return
		Local words$[]
		words=rule.split(" ")
		symbol$=words[0]
		If Len(words)=1
			txt$=""
		Else
			txt$=words[1]
			If txt[0]=42
				reptxt$=txt[1..]
				txt=words[2]
				words=words[2..]
			Else
				reptxt$=txt
				words=words[1..]
			EndIf
		EndIf
		nxt:TList=ListFromArray(words[1..])
		gr:gnfrule=gnfrule.Create(symbol,txt,reptxt,nxt)
		Print gr.repr()
		rules.addlast gr
	End Method
	
	Method findrules:TList(symbol$)
		l:TList=New TList
		bl:TList=New TList
		For gr:gnfrule=EachIn rules
			If gr.symbol=symbol
				If issymbol(gr.txt)
					nsymbol$=gr.txt[1..]
					nl:TList=findrules(nsymbol)
					For ngr:gnfrule=EachIn nl
						n2gr:gnfrule=ngr.copy()
						If gr.reptxt<>gr.txt n2gr.reptxt=gr.reptxt
						For nxt$=EachIn gr.nxt
							n2gr.nxt.addlast nxt
						Next
						If Not bl.contains(ngr)
							l.addlast n2gr
							bl.addlast ngr
						EndIf
					 Next
				Else
					l.addlast gr
				EndIf
			EndIf
		Next
		Return l
	End Method
	
	Function fromfile:grammar(f:TStream,name$)
		g:grammar=New grammar
		g.name=name
		Local words$[]
		While Not Eof(f)
			line$=f.ReadLine().Trim()
			If line
				g.addrule(line)
			EndIf
		Wend
		Return g
	End Function
	
	Method init()
		stacks=New TList
		stack:tstack=New Tstack
		stacks.addlast stack
		stack.addlast "?$"
		in=""
	End Method
	
	Method issymbol(word$)
		If word="" Then Return(0)
		If Chr(word[0])="?" And Chr(word[1])<>"?"
			Return 1
		Else
			Return 0
		EndIf
	End Method
	
	Method options:TList(oword$="",morewords=0)
		ol:TList=New TList
		For stack:tstack=EachIn stacks
			If Not stack.count() Then Return(New TList)
			If oword
				word$=oword
			Else
				word$=String(stack.last())
			EndIf
			If word[..2]="??"
				For w$=EachIn guesses(word[2..])
					If w[..2]<>"??" Or in=""
						ol.addlast w
					Else
						If validinput(word[2..],in,0)
							ol.addlast w
						EndIf
					EndIf
				Next
			ElseIf issymbol(word)
				symbol$=word[1..]
				l:TList=findrules(symbol)
				For ngr:gnfrule=EachIn l
					If issymbol(ngr.txt) Or ngr.txt[..2]="??"
						For w2$=EachIn options(ngr.txt)
							ol.addlast w2
						Next
					ElseIf ngr.txt=""
						numadded=0
						For stack:tstack=EachIn stacks
							If stack.count()>=2
								dude$=String(stack.valueatindex(stack.count()-2))
								'If Not issymbol(dude)
									For w2$=EachIn options(dude)
										If Not ol.contains(w2)
											ol.addlast w2
											numadded:+1
										EndIf
									Next
								'EndIf
							EndIf
						Next
						If Not numadded
							ol.addlast "<nothing>"
						EndIf
					Else
						otxt$=ngr.txt
						If morewords
							If ngr.nxt.count()
								nl:TList=ngr.nxt.copy()
								nxt$=""
								While nl.count() And issymbol(nxt)=0 And nxt[..2]<>"??"
									nxt=String(nl.removefirst())
									If Not (issymbol(nxt) Or nxt[..2]="??") Then otxt:+" "+nxt
								Wend
							EndIf
						EndIf
						ol.addfirst otxt
					EndIf
				Next
			Else
				If word[0]<>92
					ol.addfirst word
				EndIf
			EndIf
		Next
		
		nl:TList=New TList
		For w$=EachIn ol
			If Not (Lower(w).startswith(Lower(in)) Or in="" Or w[..2]="??")
				ol.remove w
			Else
				If Not nl.contains(w)
					nl.addlast w
				EndIf
			EndIf
		Next

		Return nl
	End Method
	
	Method parse()
		caught=0
		nstacks=New TList
		ostacks:TList=New TList
		For stack:tstack=EachIn stacks
			If parsestack(stack)
				caught:+1
			ElseIf in<>""
				ostacks.addlast stack
			EndIf
		Next
		For stack:tstack=EachIn nstacks
			stacks.addlast stack
		Next
		If caught
			For stack:tstack=EachIn ostacks
				stacks.remove stack
			Next
		EndIf
		Return caught
	End Method
	
	Method parsestack(stack:tstack)
		word$=String(stack.last())
		'Print "word: "+word
		If word[..2]="??"
			validate$=validinput(word[2..],in)
			If validate
				in=validate
				stack.removelast
				symbol$=word[2..]
				
				stack.s.addparam symbol,in
				Return 1
			Else
				Return 0
			EndIf
		EndIf
		If issymbol(word)
			symbol$=word[1..]
			'Print "symbol: "+symbol
			l:TList=findrules(symbol)
			caught=0
			'For ngr:gnfrule=EachIn l
			'	Print ngr.repr()
			'Next
			For ngr:gnfrule=EachIn l
				If Lower(ngr.txt)=Lower(in)
						in=ngr.txt
						nstack:tstack=stack.copy()
						nstacks.addlast nstack
						'Print "ngr.reptxt: "+ngr.reptxt
						'Print "txt: "+ngr.txt
						'Print "ngr.symbol: "+ngr.symbol
						'Print "symbol: "+symbol
						'Print "ngr.nxt.count: "+String(ngr.nxt.count())
						'Print "ngr: "+ngr.repr()
						If ngr.reptxt<>ngr.txt And ngr.symbol<>symbol And ngr.nxt.count()
							If ngr.reptxt[0]<>63
								nstack.s.addparam symbol,ngr.reptxt
								nstack.s.addparam ngr.symbol,ngr.txt
							Else
								nstack.s.addparam symbol,ngr.txt
							EndIf
						Else
							nstack.s.addparam symbol,ngr.reptxt
						EndIf
						caught:+1
						nstack.removelast
						For word$=EachIn ngr.nxt.reversed()
							nstack.addlast word
						Next
						'Return 1
				EndIf
			Next
			'If word="?output" DebugStop
			If Not caught
				For ngr:gnfrule=EachIn l
					If ngr.txt=""
						'Print "nothing rule"
						If options().contains(in) And stack.count()
							'Print "in options and stack count"
							stack.s.addparam symbol,""
							nstack:tstack=stack.copy()
							nstack.removelast()
							If parsestack(nstack)
								nstacks.addlast nstack
								addword in
								caught:+1
							EndIf
						EndIf
					ElseIf ngr.txt[..2]="??" And in<>""
						validate$=validinput(ngr.txt[2..],in)
						If validate
							'Print "in: "+in
							'Print "validate: "+validate
							in=validate
							nstack:tstack=stack.copy()
							nstacks.addlast nstack
							If ngr.reptxt<>ngr.txt And ngr.symbol<>symbol
								If ngr.reptxt[0]<>63
									nstack.s.addparam symbol,ngr.reptxt
									nstack.s.addparam ngr.symbol,in
								Else
									nstack.s.addparam symbol,in
								EndIf
							Else
								nstack.s.addparam symbol,in
							EndIf
							'nstack.s.addparam symbol,in
							caught:+1
							nstack.removelast
							For word$=EachIn ngr.nxt.reversed()
								nstack.addlast word
							Next
							'Return 1
						EndIf
					EndIf
				Next
			EndIf
			If caught
				stacks.remove stack
				Return 1
			EndIf
		Else
			If Lower(word)=Lower(in)
				stack.removelast
				Return 1
			EndIf
		EndIf
		Return 0
	End Method
	
	Method draw(offx, offy)
		SetImageFont normalfont
		SetScale 1,1
		y=offy
		outtxt$=tstack(stacks.first()).s.txt
		x=TextWidth(outtxt)
		SetColor 0,0,0
		flines:TList=fittext(outtxt,scrollerwidth-10)
		For line$=EachIn flines
			DrawText line,offx,y
			y:+TextHeight(line)
		Next
		y:-TextHeight(line)
		x=TextWidth(line)
		If x+TextWidth(in+"_")>scrollerwidth
			x=0
			y:+TextHeight(outtxt)
		EndIf
		DrawText in+"_",offx+x,y
		
	
		y:+TextHeight(in)
		starty=y
		maxwidth=-1
		
		clickables=New TList
		columns:TList=New TList
		column:TList=New TList
		longstr$=""
		maxwidth=-1
		longest:TList=New TList
		
		opts:TList=options("",1)
		opts.sort
		For w$=EachIn opts
			If w[..2]="??"
				w="<"+clue(w[2..])+">"
			EndIf
			twidth=TextWidth(w)
			theight=TextHeight(w)
			If twidth>maxwidth Or maxwidth=-1
				maxwidth=twidth
				longstr=w
			EndIf
			If y+theight>gheight-10
				columns.addlast column
				column=New TList
				longest.addlast longstr
				longwidth:+TextWidth(longstr)+5
				maxwidth=-1
				y=starty
			Else
				column.addlast w
				y:+theight
			EndIf
		Next
		columns.addlast column
		longest.addlast longstr
		longwidth:+TextWidth(longstr)+5
		'End
		
		nx=justifyoption(longwidth,offx+x,scrollerwidth-10)
		For column=EachIn columns
			y=starty
			For w$=EachIn column
				SetColor 0,0,0
				DrawText w[..Len(in)],nx,y
				SetColor 100,100,100
				DrawText w[Len(in)..],nx+TextWidth(in),y
				clickable.Create w,nx,y,TextWidth(w),TextHeight(w)
				y:+TextHeight(w)
			Next
			longstr$=String(longest.removefirst())
			nx:+TextWidth(longstr)+5
		Next
		
		'Rem
		starty=200
		x=0
		For stack:tstack=EachIn stacks
			cy=starty
			For w$=EachIn stack.reversed()
				DrawText w,x,cy
				cy:+15
			Next
			x:+100
		Next
		'EndRem
	End Method
	
	Method justifyoption(twidth,x,width)
		If x+twidth<width
			Return x
		Else
			Return width-twidth
		EndIf
	End Method
	
	Method addword(in$)
		For stack:tstack=EachIn stacks
			stack.s.addword in
		Next
	End Method
	
	Method update()
		Local words$[]
		cr=GetChar()
		If cr
			Select cr
				Case 32,13,9
					opts:TList=options()
					If opts.count()=1
						opt$=String(opts.first())
						If opt[..2]<>"??" And opt<>"<nothing>"
							in=opt
						EndIf
					EndIf
					
					caught=parse()
					If caught
						If in<>""
							addword(in)
						EndIf
						If (cr=9 Or cr=13)' And stacks.count()=1
							in=""
							opts:TList=options()
							While opts.count()=1 And String(opts.first())[..2]<>"??"
								nstacks=New TList
								in=String(opts.first())
								addword in
								parse
								in=""
								opts=options()
							Wend
							in=""
						EndIf
					EndIf
					in=""
				Case 8
				Default
					If cr>=32
						in:+Chr(cr)
					EndIf
			End Select
		EndIf
		
		ms=MilliSecs()
		If KeyDown(KEY_BACKSPACE)
			If ms>nextdelete
				nextdelete=ms+120
				If Len(in)
					in=in[..Len(in)-1]
				Else
					stxt$=tstack(stacks.first()).s.txt
					words=stxt.split(" ")
					If Len(stxt)
						words=words[..Len(words)-1]
						init()
						For in$=EachIn words[..Len(words)-1]
							parse()
							addword(in)
						Next
						in=words[Len(words)-1]
					EndIf
				EndIf
			EndIf
		Else
			nextdelete=ms
		EndIf
		
		If MouseHit(1)
			mx=MouseX()
			my=MouseY()
			For cl:clickable=EachIn clickables
				If cl.contains(mx,my)
					If cl.txt="<nothing>"
						in=""
						parse
					Else
						words=cl.txt.split(" ")
						For w$=EachIn words
							in=w
							parse
							addword in
						Next
						in=""
					EndIf
				EndIf
			Next
		EndIf
		
		If stacks.count()=0 
			init
		EndIf
		If options().count()=0 And in=""
			For stack:tstack=EachIn stacks
				stack.s.txt=prettysentence(stack.s.txt)
				interpret(stack.s)
			Next
			init()
		EndIf

		If KeyHit(KEY_ESCAPE)
			If anytext()
				init
			ElseIf in
				in=""
			Else
				Select curgrammar.name
				Case "letter","contractletter"
					cancelletter
				Case "menu"
					changegrammar "main"
				Case "main"
					changegrammar "menu"
				End Select
			EndIf
		EndIf
		
	End Method
	
	Method anytext()
		For stack:tstack=EachIn stacks
			If stack.s.txt Return 1
		Next
	End Method
	
End Type

Type tstack Extends TList
	Field s:sentence
	
	Method New()
		s=New sentence
	End Method
	
	Method copy:tstack()
		stack:tstack=New tstack
		For o:Object=EachIn Self
			stack.addlast o
		Next
		stack.s=s.copy()
		Return stack
	End Method
End Type


Type clickable
	Field x#,y#,w#,h#
	Field txt$
	
	Method New()
		clickables.addlast Self
	End Method
	
	Function Create:clickable(txt$,x#,y#,w#,h#)
		cl:clickable=New clickable
		cl.txt=txt
		cl.x=x
		cl.y=y
		cl.w=w
		cl.h=h
		Return cl
	End Function
	
	Method contains(ix#,iy#)
		If ix>=x And ix<x+w And iy>+y And iy<y+h
			Return 1
		EndIf
	End Method
End Type

Type sentence
	Field txt$
	Field params:TList
	Field isroutine
	
	Method repr$()
		Return jsonise().repr()
		Local param$[2]
		out$=""
		'out$="~q"+txt+"~q"
		For param=EachIn params
			out:+"~n"+param[0]+" : "+param[1]
		Next
		Return out
	End Method
	
	Method New()
		params=New TList
	End Method
	
	Method jsonise:jsonvalue()
		Local param$[2]
		j:jsonobject=New jsonobject
		j.addnewpair("txt",jsonstringvalue.Create(txt))
		j.addnewpair("isroutine",jsonnumbervalue.Create(isroutine))
		pj:jsonobject=New jsonobject
		j.addnewpair("params",pj)
		For param=EachIn params
			pj.addnewpair(param[0],jsonstringvalue.Create(param[1]))
		Next
		Return j
	End Method
	
	Function Load:sentence(j:jsonobject)
		s:sentence=New sentence
		s.txt=j.getstringvalue("txt")
		s.isroutine=j.getnumbervalue("isroutine")
		pj:jsonobject=j.getobjectvalue("params")
		Local param$[2]
		For sp:jsonpair=EachIn pj.pairs
			symbol$=sp.name
			in$=jsonstringvalue(sp.value).txt
			s.addparam(symbol,in)
		Next
		Return s
	End Function
	
	Method addparam(symbol$,in$)
		Local param$[2]
		'Print "addparam: "+symbol+" , "+in
		param=[symbol,in]
		params.addlast param
	End Method
	
	Method addword(in$)
		If Not in Then Return
		If in[0]=92
			txt=txt[..Len(txt)-1]
			in=in[1..]
		EndIf
		txt:+in+" "
	End Method
	
	Method nextparam$(remove=1)
		If Not params.count()
			Return ""
		EndIf
		Local param$[2]
		If remove
			param=String[](params.removefirst())
		Else
			param=String[](params.first())
		EndIf
		Return param[1]
	End Method
	
	Method paramsymbol$()
		Local param$[2]
		param=String[](params.first())
		Return param[0]
	End Method
	
	Method copy:sentence()
		s:sentence=New sentence
		s.txt=txt
		s.params=params.copy()
		Return s
	End Method
	
	Method getparam$(symbol$)
		Local param$[2]
		For param=EachIn params
			If param[0]=symbol
				Return param[1]
			EndIf
		Next
	End Method
	
	Method split:TList(symbol$,in$="")
		Local param$[2]
		l:TList=New TList
		s:sentence=New sentence
		s.txt=txt
		s.isroutine=isroutine
		param=[symbol,""]
		s.params.addfirst param
		par:TList=params.copy()
		While par.count()
			param=String[](par.removefirst())
			If param[0]=symbol And (param[1]=in Or in="")
				If s.params.count()>1
					l.addlast s
				EndIf
				s:sentence=New sentence
				s.txt=txt
				s.isroutine=isroutine
			Else
			EndIf
			s.params.addlast param
		Wend
		If s.params.count()>1
			l.addlast s
		EndIf
		Return l
	End Method
	
	Function join:sentence(l:TList)
		s:sentence=New sentence
		For os:sentence=EachIn l
			s.isroutine=os.isroutine
			s.txt=os.txt
			Local param$[2]
			For param=EachIn os.params
				s.params.addlast param
			Next
		Next
		Return s
	End Function
End Type	


Type gnfrule
	Field txt$
	Field reptxt$
	Field nxt:TList
	Field symbol$
		
	Method New()
	End Method
	
	Function Create:gnfrule(symbol$,txt$,reptxt$,nxt:TList)
		gr:gnfrule=New gnfrule
		gr.symbol=symbol
		gr.txt=txt
		gr.reptxt=reptxt
		gr.nxt=nxt
		Return gr
	End Function
	
	Method copy:gnfrule()
		gr:gnfrule=gnfrule.Create(symbol,txt,reptxt,nxt.copy())
		Return gr
	End Method

	Method repr$()
		If txt="" Then Return(symbol+" -> <nothing>")
		p$=symbol+" -> *"+reptxt+" | "+txt+" "
		For word$=EachIn nxt
			p:+word+" "
		Next
		
		Return p
	End Method
End Type

Function evaluate#(s:sentence)
	l:TList=s.split("conjunction","or")
	If l.count()=1
		l=s.split("conjunction","and")
		If l.count()>1
			go=1
			For s:sentence=EachIn l
				s.nextparam
				go:*evaluate(s)
			Next
			Return go
		EndIf
	Else
		go=0
		For s:sentence=EachIn l
			s.nextparam
			go:+evaluate(s)
		Next
		Return go
	EndIf
	
	expression$=s.nextparam()
	Select expression
	Case "morethan"
		num1#=resolveamount(s.nextparam())
		num2#=resolveamount(s.nextparam())
		If num1>num2 
			Return 1
		Else
			Return 0
		EndIf
	Case "lessthan"
		num1#=resolveamount(s.nextparam())
		num2#=resolveamount(s.nextparam())
		If num1<num2 
			Return 1
		Else
			Return 0
		EndIf
	Case "equalto"
		num1#=resolveamount(s.nextparam())
		num2#=resolveamount(s.nextparam())
		If num1=num2 
			Return 1
		Else
			Return 0
		EndIf
	End Select
End Function

Rem
Function clue$(w$)
	Select w
	Case "product"
		Return "the name of a product"
	Case "pluralnumber"
		Return "a number"
	Case "number"
		Return "a number"
	Case "percentage"
		Return "a percentage"
	Case "money","strictmoney"
		Return "an amount of money"
	Case "name"
		Return "a name"
	Case "merchant","mymerchant"
		Return "the name of a merchant"
	Case "employee"
		Return "one of your agents"
	Case "port"
		Return "the name of a port"
	Case "output"
		Return "a product which can be made in a factory"
	Case "day"
		Return "a day number"
	Case "month"
		Return "a month of the year"
	End Select
End Function

Function guesses:TList(kind$)
	l:TList=New TList
	Select kind$
	Case "month"
		For i=0 To 11
			l.addlast monthname[i]
		Next
	Default
		l.addlast "??"+kind
	End Select
	l.sort
	Return l
End Function	

Function validinput$(kind$,in$,strictmatch=1)
	Select kind
	Case "number"
		For i=0 To Len(in)-1
			c=in[i]
			If c<>46 And (c<48 Or c>57)
				Return ""
			EndIf
		Next
		Return in
	Case "pluralnumber"
		If validinput("number",in)
			If Float(in)>1
				Return in
			EndIf
		EndIf
		Return ""
	Case "money","strictmoney"
		If in="" Then Return ""
		If in[0]=36
			If Len(in)=1 Then Return in
			numvalid$=validinput("number",in[1..])
			If numvalid
				Return moneystring(Float(numvalid))
			Else
				Return ""
			EndIf
		ElseIf kind="strictmoney"
			Return ""
		EndIf
		numvalid$=validinput("number",in)
		If numvalid
			Return moneystring(Float(numvalid))
		Else
			Return ""
		EndIf
		
	Case "percentage"
		If in="" Then Return in
		If in[Len(in)-1]<>37
			If Not strictmatch
				Return validinput("number",in)
			Else
				Return ""
			EndIf
		EndIf
		Return validinput("number" , in[..Len(in) - 1]) + "%"
	Case "day"
		c = 0
		While c < Len(in) And in[c] >= 48 And in[c] <= 57 
			c:+ 1
		Wend
		num = Int(in[..c]) 
		If ordinal(num)[..Len(in)]=in
			Return ordinal(num)
		Else
			Return ""
		EndIf
	Case "month"
		For i = 0 To 11
			If monthname[i] = in Return in
		Next
		Return ""
	Case "name"
		Return in
	Default
		Return in
	End Select
End Function

Function cancelletter()
End Function
Function changegrammar(g:grammar)
End Function


Include "jsondecoder.bmx"
Include "lettermaker.bmx"
Include "globals.bmx"
Include "helpers.bmx"

Function testinterpret(s:sentence)
	Print s.repr()
	command$=s.nextparam()
	Print command
	'evaluate(s)
End Function

gwidth=400
gheight=800
scrollerwidth=gwidth
scrollerheight=gheight
Graphics gwidth,gheight,0
SetClsColor 255,255,255
Global testgrammar:grammar=grammar.fromfile(ReadFile("testgrammar.txt"))
testgrammar.interpret=testinterpret
testgrammar.init
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	testgrammar.update
	
	testgrammar.draw(0,0)
	

	Flip
	Cls
Wend
