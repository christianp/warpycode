Function parsefunction$[](expr$)
	expr=Trim(expr)
	If Not (expr.contains("(") And expr[Len(expr)-1]=Asc(")")) Return
	i=expr.find("(")
	Return [expr[..i]]+splitargs(expr[i+1..Len(expr)-1],",")
End Function

Function splitargs$[](expr$,del$=",")
	Local args$[]
	i=0
	oi=i
	inparens=0
	inquote=0
	While i<Len(expr)
		Select Chr(expr[i])
		Case "("
			If Not inquote
				inparens:+1
			EndIf
		Case ")"
			If Not inquote
				inparens:-1
			EndIf
		Case "~q"
			inquote=1-inquote
		Case del
			If Not (inparens Or inquote)
				args:+[expr[oi..i]]
				oi=i+1
			EndIf
		End Select
		i:+1
	Wend 
	args:+[expr[oi..]]
	Return args
End Function

Rem
While 1
	expr$=Input()
	For bit$=EachIn parsefunction(expr)
		Print " "+bit
	Next
Wend
endrem