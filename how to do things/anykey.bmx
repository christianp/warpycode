Function anykey()
	out=0
	For i=0 To 200
		If KeyHit(i) out=1
	Next
	For i=0 To JoyCount()-1
		For b=0 To 9
			If JoyHit(b,i) out=1
		Next
	Next
	Return out
End Function


