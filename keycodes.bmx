f:TStream=ReadFile("scancodes.txt")
Local names:TList=New TList
i=0
While Not Eof(f)
	line$=ReadLine(f)
	Local bits$[]
	bits=line.split("~t")
	If Len(bits)>1
		name$=bits[0]
		num=Int(bits[1])
		While names.count()<num
			s$=""
			names.addlast s
		Wend
		names.addlast name
	EndIf
Wend
txt$=""
For name$=EachIn names
	If txt txt:+", "
	txt:+"~q"+name+"~q"
Next
Print "["+txt+"]"