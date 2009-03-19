SuperStrict

Function KnuthShuffle:Int[](n:Int)
	Local i:Int,j:Int,b:Int
	Local k:Int[n]
	For i=0 To n-1
		k[i]=i
	Next
	
	For i=0 To n-2
		j=Rand(i,n-1)
		b=k[i]
		k[i]=k[j]
		k[j]=b
	Next
	
	Return k
End Function

Global starttime:Int
Function splittime(label$="")
	Local endtime:Int=MilliSecs()
	Local time:Float=(endtime-starttime)/1000.0
	If label
		Print label+": "+time+"s"
	EndIf
	
	starttime=endtime
End Function

Function runtests(funcs()[],names$[],iterations:Int)
	SeedRnd MilliSecs()
	Local order:Int[]=KnuthShuffle(Len(funcs))
	Local n:Int,i:Int
	
	splittime
	For i=EachIn order
		For n=1 To iterations
			funcs[i]()
		Next
		splittime names[i]
	Next
End Function