Include "grammar.bmx"

Function levenshtein(s$,t$)
	If s="" Return Len(t)
	Local row1[],row2[]
	row1=New Int[Len(t)+1]
	For j=0 To Len(t)
		row1[j]=j
	Next
	For i=1 To Len(s)
		row2=New Int[Len(t)+1]
		row2[0]=i
		dbg$=""
		For j=1 To Len(t)
			If s[i-1]=t[j-1] Then cost=0 Else cost=1
			row2[j]=Min(Min(row1[j]+1,row2[j-1]+1),row1[j-1]+cost)
			If dbg dbg:+" "
			dbg:+row2[j]
		Next
		Print dbg
		row1=row2
	Next
	Return row2[Len(t)]
End Function

Function commonstart(s$,t$)
	i=0
	While i<Len(s) And i<Len(t) And s[i]=t[i]
		i:+1
	Wend
	Return i
End Function



g:grammar=grammar.fromfile("grammar.txt")

While 1
	in$=Input()
	s:sentence=g.match(in)
	If s
		Print s.repr()
	Else
		Local bits$[]=in.split(" ")
		t$=""
		i=0
		While Not g.options(t).count()
			If t t:+" "
			t:+bits[i]
			i:+1
		Wend
		Print t
		Local iwords$[]=in[Len(t)..].split(" ")
		
		o:TList=g.options(t)
		most$=""
		For op$=EachIn o
			c=commonstart(in,op)
			If c>Len(most) most=op[..c]
		Next
		For op$=EachIn o
			If Not op.startswith(most)
				o.remove op
			EndIf
		Next
		
		
		For op$=EachIn o
			d=0
			Local owords$[]=op.split(" ")
			For c=0 To Len(owords)-1
				If c<Len(iwords)
					d:+levenshtein(iwords[c],owords[c])
				Else
					d:+Len(owords[c])
				EndIf
			Next
			Print d+op
		Next
	EndIf
Wend