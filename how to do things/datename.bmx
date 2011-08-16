Global dayname$[7]
dayname=["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
Global monthname$[12]
monthname=["January","Febraury","March","April","May","June","July","August","September","October","November","December"]
Function datename$(g)
	g:-60
	y = (10000*g + 14780)/3652425
	ddd = g - (365*y + y/4 - y/100 + y/400)
	If ddd < 0
		y = y - 1
		ddd = g - (365*y + y/4 - y/100 + y/400)
	EndIf
	mi = (100*ddd + 52)/3060
	mm = ((mi + 2) Mod 12) + 1
	y = y + (mi + 2)/12 + 1500
	dd = ddd - (mi*306 + 5)/10 + 1
	
	Print "--"
	Print dayname[(dd-1) Mod 7]+", "+ordinal(dd)+" "+monthname[mm-1]+" "+String(y)

End Function

Function ordinal$(n)
	Local sf$
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
	Return String(n)+sf
End Function


For c=1 To 25
	d(c)
Next