Function picksomething()
   p#=rnd(0,1)
   t#=0
   i=0
   while t<=p
      t:+probabilities[i]
      i:+1
   wend
   return i - 1 
End Function



Function picksomethingbinary()
	p#=Rnd(0,1)
	i=numoptions/2
	move=i/2
	While 1
		If move=0 Then move=1
		s#=sumprobabilities[i]
		If s<p
			i:+move
		'	Print "+"+String(move)
		ElseIf s-probabilities[i]>p
			i:-move
		'	Print "-"+String(move)
		Else
		'	Print i
			Return i
		EndIf
		move=move/2
	Wend
End Function

Global numoptions=1000

Global probabilities#[numoptions]

Global sumprobabilities#[numoptions]
t#=0
For i=0 To numoptions-1
	p#=Rnd(.8,.99)*1.0/numoptions
	probabilities[i]=p
	t:+probabilities[i]
	sumprobabilities[i]=t
Next

txt1$=""
txt2$=""
For i=0 To numoptions-1
	probabilities[i]:/t
	txt1:+probabilities[i]+" , "
	sumprobabilities[i]:/t
	txt2:+sumprobabilities[i]+" , "
Next
'Print txt1
'Print txt2


ms=MilliSecs()
For i=1 To 1000000
	number=picksomethingbinary()
Next
diff=MilliSecs()-ms
Print "binary search method took "+String(diff)+"ms"

ms=MilliSecs()
For i=1 To 1000000
	number=picksomething()
Next
diff=MilliSecs()-ms
Print "lazy method took "+String(diff)+"ms"

