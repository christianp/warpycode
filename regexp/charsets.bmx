Type charset
	Field pattern$
	Field ranges:TList
	
	Method New()
		ranges=New TList
	End Method
	
	Function Create:charset(pattern$)
		If Not pattern Return emptyset
		cs:charset=New charset
		cs.pattern=pattern
		While Len(pattern)
			If Len(pattern)>2 And Chr(pattern[1])="-"
				low=pattern[0]
				high=pattern[2]
				cs.addrange(low,high)
				pattern=pattern[3..]
			Else
				c=pattern[0]
				cs.addrange(c,c)
				pattern=pattern[1..]
			EndIf
		Wend
		Return cs
	End Function
	
	Method addrange(low,high)
		Local range[]=[low,high]
		ranges.addlast range
	End Method
	
	Method match(cr$)
		c=Asc(cr)
		Local range[]
		For range=EachIn ranges
			If c>=range[0] And c<=range[1]
				Return True
			EndIf
		Next
		Return False
	End Method
	
	Method repr$()
		Local range[]
		txt$=""
		For range=EachIn ranges
			If range[0]=range[1]
				txt:+Chr(range[0])
			Else
				txt:+Chr(range[0])+"-"+Chr(range[1])
			EndIf
		Next
		Return txt
	End Method
End Type

Global emptyset:charset=New charset
Global digits:charset=charset.Create("0-9")
Global alphanum:charset=charset.Create("0-9A-Za-z")
Global nonalphanum:charset=New charset
Global nondigits:charset=New charset

nondigits.addrange 0,47
nondigits.addrange 58,255 
nonalphanum.addrange 0,47
nonalphanum.addrange 58,64
nonalphanum.addrange 91,96
nonalphanum.addrange 123,255
