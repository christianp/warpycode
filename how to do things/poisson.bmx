Function poisson(lambda!)
	If lambda>500 Return poisson(lambda/2)+poisson(lambda/2)
	k=0
	u!=Rnd(0,1)
	fact=1
	p!=Exp(-lambda)
	u:-p
	While u>0
		k:+1
		fact:*k
		p:*lambda/k
		u:-p
	Wend
	Return k
End Function

Function poisson2(lambda!)
	l!=Exp(-lambda)
	k=0
	p!=1
	While p>=l
		k:+1
		u!=Rnd(0,1)
		p:*u
	Wend
	Return k-1
End Function


'  example - draws a graph of the probability density function
lambda!=500
numruns=10000
Local counts[lambda*2]
For i=1 To numruns
	'If (i Mod 10)=0 Then Print "."
	p=poisson(lambda)
	If p<lambda*2
		counts[p]:+1
	EndIf
Next

For i=0 To lambda*2-1
	Print counts[i]
Next
Print "----"

Graphics 800,800,0
scale#=800/200
c=0
total#=0
For i=0 To 199
	grog#=0
	biff#=numruns/200.0
	wongo#=i*biff
	While c<lambda*2 And total+counts[c]<wongo 
		grog:+counts[c]
		total:+counts[c]
		c:+1
	Wend
	If c<lambda*2 And total<wongo 
		bit#=counts[c]*(wongo-total)/biff
		grog:+bit
		total:+bit
		counts[c]:-bit
	EndIf
	DrawRect i*scale,0,scale,grog
	Print grog
Next
Flip
WaitKey()