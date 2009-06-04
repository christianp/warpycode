Global templates:tmap=New tmap

Function maketemplates()
	dir=ReadDir("templates")
	
	If Not dir RuntimeError "failed to read current directory"
	
	Repeat
		t$=NextFile( dir )
		If t="" Exit
		If t="." Or t=".." Continue
		'Print t
		If Right(t,6)=".bhtml"
			f:TStream=ReadFile("templates/"+t)
			txt$=f.ReadString(f.size())
			CloseFile f
			templates.insert t[..Len(t)-6],maketemplate(txt)
		EndIf
	Forever
	
	CloseDir dir
End Function	

Function maketemplate$[](pattern$)
	Local bits$[]
	While Len(pattern)
		a=pattern.find("<%")
		If a>=0
			bits:+[pattern[..a]]
			pattern=pattern[a+2..]
			a=pattern.find("%>")
			bits:+[Trim(pattern[..a])]
			pattern=pattern[a+2..]
		Else
			bits:+[pattern]
			pattern=""
		EndIf
	Wend
	For n$=EachIn bits
		'Print n
	Next
	Return bits
End Function

maketemplates