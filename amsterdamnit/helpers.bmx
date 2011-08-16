Function loadtxt$(filename$)
	f:TStream=ReadFile(filename)
	txt$=""
	While Not Eof(f)
		txt:+f.ReadLine()+"~n"
	Wend
	txt=txt[..Len(txt)-1]
	CloseFile(f)
	Return txt
End Function

Function resolveamount#(amount$)
	If Not Len(amount)
		Return 0
	EndIf
	If amount[Len(amount)-1]=37
		Return Float(amount[..Len(amount)-1])/100.0
	EndIf
	If amount[0]=36
		Return Float(amount[1..])
	Else
		Return Float(amount)
	EndIf
End Function

Function resolvemonth(mname$)
	i=0
	While monthname[i]<>mname
		i:+1
	Wend
	Return i+1
End Function
	
Function resolveday(dname$) 
	If validinput("number" , dname) 
		dnumber = Int(dname) 
	Else
		numpart$ = dname[..Len(dname) - 2]
		suffix$ = dname[Len(dname) - 2..Len(dname)]
		If validinput("number",numpart)
			Select suffix
			Case "rd" , "st" , "nd","th"
				dnumber = Int(numpart) 
			Default
				dnumber = 0
			End Select
		Else
			dnumber=0
		EndIf
	EndIf
	If dnumber>0 And dnumber<=31
		Return dnumber
	Else
		Return 0
	EndIf
End Function

Function percentstring$(amount#)
	Return String(Int(amount*100))+"%"
End Function

Function moneystring$(amount#)
	amount:+.005
	units=Int(amount)
	pennies=Int((amount-Int(amount))*100)
	If pennies=0
		pennystr$=""
	ElseIf pennies<10
		pennystr$=".0"+String(pennies)
	Else
		pennystr="."+String(pennies)
	EndIf
	Return "$"+String(units)+pennystr$
End Function

Function prettysentence$(txt$)
	If Len(txt)<2 Return Upper(txt)
	ntxt$=rTrim(txt)
	rbit$=txt[Len(ntxt)..]
	ntxt=Upper(Chr(ntxt[0]))+ntxt[1..]
	Select ntxt[Len(ntxt)-1]
	Case 33,46,63,44
	Default
		ntxt:+"."
	End Select
	Return ntxt+rbit
End Function

Function prettylist$(list:TList) 
	If Not list.count() Return ""
	If list.count()=1
		Return String(list.first())
	EndIf
	list=list.copy()
	bit$=String(list.removelast())
	out$=" and "+bit
	While list.count()
		bit$=String(list.removelast())
		out=bit+out
		If list.count()
			out=", "+out
		EndIf
	Wend
	Return out
End Function

Function announ$(noun$)
	Select Chr(noun[0])
	Case "a","e","i","o","u"
		anoun$="an "+noun
	Default
		anoun$="a "+noun
	End Select
	Return anoun
End Function

Function pluralise$(name$,number=2,plural$="")
	If number=1
		Return name
	Else
		If plural
			Return plural
		Else
			Return name+"s"
		EndIf
	EndIf
End Function


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



Global dayname$[7]
dayname=["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
Global monthname$[12]
monthname=["January","February","March","April","May","June","July","August","September","October","November","December"]
Function calcdate()
	g=date-60
	y = (10000*g + 14780)/3652425
	ddd = g - (365*y + y/4 - y/100 + y/400)
	If ddd < 0
		y = y - 1
		ddd = g - (365*y + y/4 - y/100 + y/400)
	EndIf
	mi = (100*ddd + 52)/3060
	month = ((mi + 2) Mod 12) + 1
	year = y + (mi + 2)/12 + 1500
	day = ddd - (mi*306 + 5)/10 + 1
	
End Function

Function backdate(y , m , d) 
	y:-1500
	m = (m + 9) Mod 12
	y = y - m / 10
	d = 365 * y + y / 4 - y / 100 + y / 400 + (m * 306 + 5) / 10 + d - 1
	d:+60
	Return d
End Function

Function datename$()
	Return dayname[(day-1) Mod 7]+", "+ordinal(day)+" "+monthname[month-1]+" "+String(year)
End Function

Function ordinal$(n)
	Local sf$
	If n>=10 And n<20
		sf="th"
	Else
		Select n Mod 10
		Case 1
			sf="st"
		Case 2
			sf="nd"
		Case 3
			sf="rd"
		Default
			sf="th"
		End Select
	EndIf
	Return String(n)+sf
End Function

Function lTrim$(txt$)
	c=0
	While c<Len(txt)
		Select txt[c]
		Case 32,9
		Default
			Return txt[c..]
		End Select
		c:+1
	Wend
	Return ""
End Function

Function rTrim$(txt$)
	c=Len(txt)-1
	While c>=0
		Select txt[c]
		Case 32,9,10
		Default
			Return txt[..c+1]
		End Select
		c:-1
	Wend
	Return ""
End Function


