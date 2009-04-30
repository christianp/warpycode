Function lcs[,](x$[],y$[])
	m=Len(x)
	n=Len(y)
	Local c[m+1,n+1]
	For i=0 To m
		c[i,0]=0
	Next
	For j=0 To n
		c[0,j]=0
	Next
	
	For i=1 To m
		For j=1 To n
			If x[i-1]=y[j-1]
				c[i,j]=c[i-1,j-1]+1
			Else
				c[i,j]=Max(c[i,j-1],c[i-1,j])
			EndIf
		Next
	Next
	Return c
End Function

Function backtrace$(c[,],x$[],y$[],i,j)
	If i=0 Or j=0
		Return ""
	ElseIf x[i-1]=y[j-1]
		Return backtrace(c,x,y,i-1,j-1)+x[i-1]
	Else
		If c[i,j-1] > c[i-1,j]
			Return backtrace(c,x,y,i,j-1)
		Else
			Return backtrace(c,x,y,i-1,j)
		EndIf
	EndIf
End Function

Function printdiff[](c[,],x$[],y$[],i,j)
	Global lastaction,lastline$
	lastaction=0
	Function output(res[])
		Select res[0]
		Case 0
			op$="s"
		Case 1
			op$="a"
		Case -1
			op$="d"
		End Select
		'Print res[1]+","+res[2]+op+res[3]+","+res[4]
	End Function
	Local res[],code
	If i>0 And j>0 And x[i-1]=y[j-1]
		res=printdiff(c,x,y,i-1,j-1)
		Print lastaction+"  "+x[i-1]
		code=0
	Else
		If j>0 And (i=0 Or c[i,j-1] >= c[i-1,j])
			res=printdiff(c,x,y,i,j-1)
			code=1
			Print lastaction+"+ "+y[j-1]
		ElseIf i>0 And (j=0 Or c[i,j-1] < c[i-1,j])
			res=printdiff(c,x,y,i-1,j)
			code=2
			Print lastaction+"- "+x[i-1]
			lastline=x[i-1]
		EndIf
	EndIf
	If lastaction=2 And code<>1
		Print "2K "+lastline
	EndIf
	lastaction=code
	If Len(res) And res[0]<>code
		output res
		Return [code,i,i,j,j]
	Else
		If Len(res)
			Return [code,res[1],i,res[2],j]
		Else
			Return [code,i,i,j,j]
		EndIf
	EndIf
End Function

Function loadlines$[](fname$)
	f:TStream=ReadFile(fname)
	s$=f.ReadString(StreamSize(f))
	Return s.split("~n")
End Function

Local x$[]=loadlines("test.bmx")
Local y$[]=loadlines("test2.bmx")
Local c[,]
c = lcs(x,y)
For i=0 To Len(x)
	s$=""
	For j=0 To Len(y)
		s:+c[i,j]+" "
	Next
'	Print s
Next
'Print c[Len(x),Len(y)]
'Print backtrace(c,x,y,Len(x),Len(y))
printdiff c,x,y,Len(x),Len(y)