Function readlines$[](f:TStream,close=1)
	txt$=""
	While Not Eof(f)
		txt:+f.ReadString(10000)
	Wend
	If close CloseStream(f)
	Return txt.split("~n")
End Function

Function readall$(f:TStream,close=1)
	txt$=""
	While Not Eof(f)
		txt:+f.ReadString(10000)
	Wend
	If close CloseStream(f)
	Return txt
End Function

Function split$[](txt$,m$,startb$="(",endb$=")")
	inbrackets=0
	Local o$[]
	i=0
	mlen=Len(m)
	sb=Asc(startb)
	eb=Asc(endb)
	While i<Len(txt)
		If inbrackets
			Select txt[i]
			Case sb
				Print ">"
				inbrackets:+1
			Case eb
				Print "<"
				inbrackets:-1
			End Select
		Else
			Select txt[i]
			Case sb
				Print ">"
				inbrackets:+1
			Default
				If txt[i..i+mlen]=m
					Print "gotcha"
					Print txt[0..i]
					o:+ [txt[0..i]]
					txt=txt[i+mlen..]
					Print txt
					i=-1
				EndIf
			End Select
		EndIf
		i:+1
	Wend
	o:+[txt]
	Return o
End Function