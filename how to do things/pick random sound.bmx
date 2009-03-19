
Function loadsounds:TList(name$,n,ext$)
	l:TList=New TList
	For i=1 To n
		s:TSound=LoadSound(name+String(i)+ext)
		l.addlast s
	Next
	Return l
End Function

Function choosesound:TSound(l:TList)
	n=Rand(0,l.count()-1)
	Return TSound(l.valueatindex(n))
End Function