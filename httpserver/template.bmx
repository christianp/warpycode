Global templates:tmap=New tmap

Function maketemplates(fname$="")
	If fname And fname[fname.length-1]<>Asc("/")
		fname:+"/"
	EndIf
	dirname$="views/"+fname
	dir=ReadDir(dirname)
	
	If Not dir RuntimeError "failed to read current directory"
	
	Repeat
		t$=NextFile( dir )
		If t="" Exit
		If t="." Or t=".." Continue
		'Print t
		Select FileType(dirname+t)
		Case 1	'file
			If Right(t,6)=".bhtml"
				f:TStream=ReadFile(dirname+t)
				txt$=f.ReadString(f.size())
				CloseFile f
				templates.insert fname+t[..Len(t)-6],maketemplate(txt)
			EndIf
		Case 2	'folder
			maketemplates fname+t
		End Select
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