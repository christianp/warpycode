Include "charsets.bmx"

Function listsame(l1:TList,l2:TList)
	If l1.count()<>l2.count() Return False
	
	For o:Object=EachIn l1
		If Not l2.contains(o) Return False
	Next

	Return True
End Function

Type fsa
	Field accepting
	Field transitions:TList
	Field name$
	
	Method New()
		transitions=New TList
	End Method
	
	Method addtransition(symbol$,dest:fsa)
		transitions.addlast transition.Create(symbol,dest)
	End Method
	
	Method matches:TList(symbol$,l:TList=Null)
		If Not l l=New TList
		For t:transition=EachIn transitions
			If t.cs.match(symbol)
				l.addlast t.dest
			EndIf
		Next
		Return l
	End Method
	
	Method evaluate(pattern$,spaces$="")
		Print spaces+name+"?"+pattern
		If Not pattern
			Print "end at "+name
			Return accepting
		EndIf
		symbol$=Chr(pattern[0])
		npattern$=pattern[1..]
		For f:fsa=EachIn matches(symbol)
			If f.evaluate(npattern,spaces+" ") Return True
		Next
		For t:transition=EachIn transitions
			If t.cs=emptyset
				Print spaces+"empty move to "+t.dest.name
				If t.dest.evaluate(pattern,spaces) Return True
			EndIf
		Next
		
		Print spaces+"fail at "+name

		Return False
	End Method
	
	Method emptymoves:TList(l:TList=Null,addself=0)
		If Not l l=New TList
		If addself l.addlast(Self)
		For t:transition=EachIn transitions
			If t.cs=emptyset
				If Not l.contains(t.dest)
					l.addlast t.dest
					t.dest.emptymoves(l)
				EndIf
			EndIf
		Next
		Return l
	End Method
	
	Method repr$()
		txt$=name+"~n"
		If accepting txt:+"accepting~n"
		For t:transition=EachIn transitions
			txt:+"  "+t.repr()+"~n"
		Next
		Return txt
	End Method
	
	Function collapse:tmap(f:fsa,checked:tmap=Null)
		If Not checked checked=New tmap
		If checked.contains(f) Return checked

		ntransitions:tmap=New tmap
		destinations:TList=New TList
		
		For t:transition=EachIn f.transitions
			If Not ntransitions.contains(t.cs.pattern)
				ntransitions.insert t.cs.pattern,New TList
			EndIf
			tl:TList=TList(ntransitions.valueforkey(t.cs.pattern))
			If Not tl.contains(t.dest)
				tl.addlast t.dest
			EndIf
			
			If Not destinations.contains(t.dest)
				destinations.addlast t.dest
			EndIf
		Next
		
		'Rem
		of:fsa=New fsa
		'Print of.name
		For key$=EachIn ntransitions.keys()
			If key
				tl:TList=TList(ntransitions.valueforkey(key))
				tname$=""
				For fp:fsa=EachIn tl
					For f2:fsa=EachIn fp.emptymoves()
						If Not tl.contains(f2)
							tl.addlast f2
						EndIf
					Next
				Next
				For f2:fsa=EachIn tl
					If tname tname:+","
					tname:+f2.name
				Next
				tname="{"+tname+"}"
				'Print "  "+key+" -> "+tname
			EndIf
		Next
		'EndRem
		
		checked.insert f,ntransitions
		
		For f2:fsa=EachIn destinations
			fsa.collapse f2,checked
		Next
		
		Return checked
	End Function
	
	Function powerset:fsa(startnode:fsa)
		Global allnodes:tmap,newnodes:TList
		
		maps:tmap=collapse(startnode)
		
		allnodes:tmap=New tmap
		
		Function findnode:fsa(l:TList,adding=1)
			For l2:TList=EachIn allnodes.keys()
				If listsame(l,l2) Return fsa(allnodes.valueforkey(l2))
			Next
			nf:fsa=New fsa
			nf.name=ziplist(l)
			'Print "new node "+nf.name
			allnodes.insert l,nf
			If adding
				newnodes.addlast l
			EndIf
			Return nf
		End Function
		
		Function ziplist$(l:TList)
			l=l.copy()
			l.sort
			txt$=""
			For f:fsa=EachIn l
				If txt txt:+","
				txt:+f.name
			Next
			Return "{"+txt+"}"
		End Function
		
		outstart:fsa=Null
		newnodes:TList=New TList
		newnodes.addlast startnode.emptymoves(Null,1)
		
		While newnodes.count()
		
			'get the new state we're making. It's a list of the old nodes
			l:TList=TList(newnodes.removefirst()) 
			
			nf:fsa=findnode(l,False)
			If Not outstart outstart=nf
			
			nf.name=ziplist(l)
			'Print "constructing "+nf.name
			
			ntransitions:tmap=New tmap
			
			'for each old node in the list, work out the transitions
			For f:fsa=EachIn l
				If f.accepting nf.accepting=1
			
				'get transition map for this nfsa node
				fnt:tmap=tmap(maps.valueforkey(f))
				
				'for each symbol, add all the destinations
				For key$=EachIn fnt.keys()
					If key
						If Not ntransitions.contains(key)
							ntransitions.insert key,New TList
						EndIf
						otl:TList=TList(fnt.valueforkey(key))
						ntl:TList=TList(ntransitions.valueforkey(key))
						For f2:fsa=EachIn otl
							If Not ntl.contains(f2) ntl.addlast f2
						Next
					EndIf
				Next
			Next
			
			'we now have a full set of transitions, make the node
			For key$=EachIn ntransitions.keys()
				df:fsa=findnode(TList(ntransitions.valueforkey(key)))
				nf.addtransition key,df
			Next
			'Print nf.repr()
		Wend
		
		Print "ALL NODES"
		For f:fsa=EachIn allnodes.values()
			Print f.repr()
		Next
		
		Return outstart
	End Function
	
End Type

Type transition
	Field cs:charset
	Field dest:fsa
	
	Function Create:transition(pattern$,dest:fsa)
		t:transition=New transition
		t.cs=charset.Create(pattern)
		t.dest=dest
		Return t
	End Function
	
	Method repr$()
		Return cs.repr()+" -> "+dest.name
	End Method
End Type


Global numnodes=0
Function compile:fsa[](pattern$,starts:fsa[],spaces$="")
	Print spaces+"compile: "+pattern
	

	Local bits$[]
	Local ends:fsa[],ostarts:fsa[],nends:fsa[]
	bits=splitpipes(pattern)
	
	'note that starts is not really a set of starting nodes for the whole machine,
	'but in fact the previous set of finals.

	If Len(bits)=1
		While Len(pattern)
			ostarts=starts
			symbol$=Chr(pattern[0])
			'Print spaces+">"+symbol
			nends=Null
			Select symbol
			Case "(" 'start brackets
				Print spaces+"brackets"
				inparens=1
				i=1
				While inparens
					Select Chr(pattern[i])
					Case "("
						inparens:+1
					Case ")"
						inparens:-1
					End Select
					i:+1
				Wend
				bit$=pattern[1..i-1]
				ends=compile(bit,starts,spaces+"  ")
				pattern=pattern[i..]
				
			Case "["
				Print spaces+"squares"
				i=1
				While Chr(pattern[i])<>"]"
					i:+1
				Wend
				bit$=pattern[0..i+1]
				'Print spaces+bit
				pattern=pattern[i+1..]
				nf:fsa=New fsa
				For f:fsa=EachIn starts
					f.addtransition bit,nf
				Next
				ends=[nf]
				
			Default 'normal character
				Print spaces+"character: "+symbol
				mf:fsa=New fsa
				mf.name="m"
				nf:fsa=New fsa
				numnodes:+1
				nf.name=String(numnodes)
				For f:fsa=EachIn starts
					f.addtransition symbol,mf
				Next
				mf.addtransition "",nf
				ends=[mf]
				nends=[nf]
				pattern=pattern[1..]
				
			End Select
			
			If Len(pattern)
				op$=Chr(pattern[0])
			Else
				op$=""
			EndIf
			Select op
			Case "*" 'kleene star closure - none or more times
				fin:fsa=New fsa
				For f:fsa=EachIn ends
					f.addtransition "",fin
				Next
				For f:fsa=EachIn starts
					f.addtransition "",fin
					fin.addtransition "",f
					If nends
						For f2:fsa=EachIn nends
							f.addtransition "",f2
						Next
					EndIf
				Next
				fin2:fsa=New fsa
				fin.addtransition "",fin2
				ends=[fin2]
				pattern=pattern[1..]
			Case "?" 'zero or one times
				fin:fsa=New fsa
				For f:fsa=EachIn starts
					f.addtransition "",fin
					If nends
						For f2:fsa=EachIn nends
							f.addtransition "",f2
						Next
					EndIf
				Next
				For f:fsa=EachIn ends
					f.addtransition "",fin
				Next
				ends=[fin]
				pattern=pattern[1..]
			Case "+" 'one or more times
				fin:fsa=New fsa
				For f:fsa=EachIn ends
					f.addtransition "",fin
				Next
				For f:fsa=EachIn starts
					fin.addtransition "",f
				Next
				ends=[fin]
				pattern=pattern[1..]
			End Select
			If nends
				ends=nends
			EndIf
			starts=ends
		Wend
	Else
		Print spaces+"pipes"
		For bit$=EachIn bits
			ends:+compile(bit,starts,spaces+"  ")
		Next
	EndIf
	
	Return ends
End Function

Function splitpipes$[](pattern$)
	'Print pattern
	inparens=0
	i=0
	Local bits$[0]
	starti=0
	While i<Len(pattern)
		Select Chr(pattern[i])
		Case "("
			inparens:+1
		Case ")"
			inparens:-1
		Case "|"
			If Not inparens
				bits:+[pattern[starti..i]]
				starti=i+1
			EndIf
		End Select
		i:+1
	Wend
	If starti<Len(pattern)
		bits=bits+[pattern[starti..]]
	EndIf
	
	'For bit$=EachIn bits
	'	Print bit
	'Next
	Return bits
End Function

'splitpipes("a|b+|(b*|a+)|[acd]")
start:fsa=New fsa
start.name="s"
re$="(([1-9]+[0-9]*)|0)(.[0-9]+)?"
re$="[a-z]*(,? [a-z]*)*"
re$="[0-9]?[0-9]"
're$=Input("re> ")
For f:fsa=EachIn compile(re,[start])
	f.accepting=1
Next
start=fsa.powerset(start)
'End

Rem
Local states:fsa[5]
For i=0 To 4
	states[i]=New fsa
	states[i].name=String(i+1)
Next
states[2].accepting=1
states[3].accepting=1

states[0].addtransition "0",states[1]
states[0].addtransition "",states[2]
states[1].addtransition "1",states[1]
states[1].addtransition "1",states[3]
states[2].addtransition "",states[1]
states[2].addtransition "0",states[3]
states[3].addtransition "0",states[2]

maps:tmap=fsa.collapse(states[0])
start:fsa=fsa.powerset(states[0])
'Print f.repr()
'End
EndRem

in$=""
While in<>"quit"
	in$=Input(">")
	If start.evaluate(in)
		Print "Yes"
	Else
		Print "No"
	EndIf
Wend