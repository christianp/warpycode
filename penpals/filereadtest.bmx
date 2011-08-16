Function test$(func$(f:TStream),name$)
	oms=MilliSecs()
	f:TStream=ReadFile("save.txt")
	txt$=func(f)
	time=MilliSecs()-oms
	Print name+" took "+time/1000.0+" seconds"
	Return txt
End Function

Function readstrings$( f:TStream )
	txt$=""
	While Not Eof(f)
		txt:+ReadString(f,10000)
	Wend
	Return txt
End Function

Function readlines$( f:TStream )
	txt$=""
	While Not Eof(f)
		txt:+ReadLine(f)
	Wend
	Return txt
End Function


t1$=Trim(test(readstrings,"read in strings"))
Print t1
t2$=Trim(test(readlines,"read in lines"))
If t1<>t2
	Print "not same"
	Print t1[Len(t1)-50..]
	Print t2[Len(t2)-50..]
EndIf