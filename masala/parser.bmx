Include "masala.bmx"

Function tokenize$[](txt$)
	l:TList=New TList
	n=0
	state=0
	word$=""
	numparens=0
	While n<Len(txt)
		c=txt[n]
		Select state
		Case 0 'normal
			Select c
			Case 34 'quote
				state=1
				word:+Chr(c)
			Case 40 'parens
				state=2
				numparens=1
				word:+Chr(c)
			Case 32
				If word
					l.addlast word
				EndIf
				word=""
			Default
				word:+Chr(c)
			End Select
		Case 1 'instring
			Select c
			Case 34
				state=0
			Default
			End Select
			word:+Chr(c)
		Case 2 'in parens
			Select c
			Case 40
				numparens:+1
			Case 41
				numparens:-1
				If numparens=0
					state=0
				EndIf
			Default
			End Select
			word:+Chr(c)
		End Select
		n:+1
	Wend
	If word l.addlast(word)
	
	Local tokens$[l.count()]
	For n=0 To Len(tokens)-1
		tokens[n]=String(l.removefirst())
		'Print tokens[n]
	Next
	Return tokens
End Function

Function isparens(token$)
	If token[0]=40 Return 1
End Function

Function precedence(f:TFunctor)
	Select f
	Case seteq
		Return 10
	Case intpow
		Return 8
	Case intmul,intdiv
		Return 7
	Case intadd,intsub
		Return 6
	Default
		Return 0
	End Select
End Function

Function parse:TFunctor( tokens$[], scope:TScope )
	'Print "parse:"
	'For token$=EachIn tokens
	'	Print "  "+token
	'Next

	Local functors:TFunctor[Len(tokens)]
	Local lf:TFunctor
	
	While n<Len(tokens)
		token$=tokens[n]
		'Print "token: "+token
		If isparens(token)
			txt$=token[1..Len(token)-1]
			lf=parse( tokenize(txt), scope )
			tokens[n]=lf.repr()
		Else
			lf=matchbuiltins(tokens[n])
		EndIf
		If Not lf
			lf=label( tokens[n], curry(getl, [tokens[n]]) )
		EndIf
		'show lf,"lf: "
		functors[n]=lf
		n:+1
	Wend
	
	Return TFunctor(doparse( functors, scope ))
End Function
	
Function doparse:Object( args:Object[], scope:TScope )
	Local functors:TFunctor[Len(args)]
	Local lf:TFunctor
	Local rf:TFunctor

	For n=0 To Len(args)-1
		functors[n]=TFunctor(args[n])
	Next

	showarray functors,"Parsing this:"
	
	While Len(functors)>1
		performed=1
		While performed 'perform prefix operators
			If TLabel(functors[0])
				pattern$=TLabel(functors[0]).pattern
				Select TLabel(functors[0]).pattern
				Case "let"
					nscope:TScope=FScope( scope )
					'Print "Letting"
					lf=TFunctor(doparse( functors[1..], nscope ))
					For l:TLabel=EachIn nscope.labels
						scope.addlabel l
					Next
					functors=[lf]
				Case "quit"
					Return Null
				Default
					lf=scope.matchname(pattern)
					If lf
						performed=1
						Print "matched function definition"
						numargs=Len(lf.kind)-1
						If numargs>0
							Print "needs "+numargs+" arguments"
							args=functors[1..numargs]
							res:TFunctor=TFunctor(apply(lf, args, scope))
							functors=[res]+functors[numargs+1..]
						EndIf
					EndIf
					performed=0
				End Select
			Else
				performed=0
			EndIf
		Wend
		performed=1
		While performed 'perform all infix operators
			highest=0
			hn=0
			performed=0
			For n=0 To Len(functors)-1
				pr=precedence(functors[n])
				Print "precedence of "+functors[n].repr()+" is "+pr
				If pr>highest
					highest=pr
					hn=n
				EndIf
			Next
			If highest>0
				performed=1
				Local lfunctors:TFunctor[],rfunctors:TFunctor[]
				Select functors[hn]
				Case seteq
					rf=TCurry.Create( applyparse, ["Functor"], functors[hn+1..] )
					args=functors[..hn]+[rf]
					res:TFunctor=TFunctor(apply( functors[hn], args, scope))
					lfunctors=Null
					rfunctors=Null
				Default
					args=[functors[hn-1],functors[hn+1]]
					lfunctors=functors[..hn-1]
					rfunctors=functors[hn+2..]
				End Select
				Print "performing "+functors[hn].repr()
				res:TFunctor=TFunctor(apply( functors[hn], args, scope))
				functors=lfunctors+[res]+rfunctors
			EndIf
		Wend
	Wend
	lf=functors[0]
	'Print "returning "+lf.repr()
	Return lf
End Function

Function applyparse:Object( args:Object[], scope:TScope )
	lf:TFunctor=TFunctor(doparse( args, scope ))
	Return apply(lf, Null, scope)
End Function

'show matchbuiltins("~qhi~q")
'show matchbuiltins("~qhi")
'show matchbuiltins("123")
'show matchbuiltins("12a")
'show matchbuiltins("+")
'show matchbuiltins("getl")


scope:TScope=FScope()
line$=""
Local res:TFunctor = New TFunctor
While res
	line=Input(">")
	Local tokens$[]=tokenize(line)	
	res=parse(tokens, scope)
	show res
	'show apply(res, Null, scope)
	show scope
Wend
