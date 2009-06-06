Type jsondecoder
	Field txt$
	Field i
	Field curchr$
	Field things:TList
	
	Method New()
		things=New TList
	End Method

	Method getnext(tokens$[],onlywhitespace=1)
		oldi=i
		While i<Len(txt)
			c$=Chr(txt[i])
			i:+1
			For token$=EachIn tokens
				If c=token 
					curchr=c
					Return 1
				EndIf
			Next
			If onlywhitespace And (Not (c=" " Or c="~t" Or c="~n" Or c="~r"))
				i:-1
				Return 0
			EndIf
		Wend
		i=oldi
		Return 0
	End Method
	
	Function Create:jsondecoder(txt$)
		j:jsondecoder=New jsondecoder
		j.txt=txt
		'j.i=i
		j.parse
		Return j
	End Function
	
	Method parse()
		While getnext(["{","["])
			Select curchr
			Case "{" 'new object
				o:jsonobject=parseobject()
				If Not o
					Print "error - couldn't parse object"
				EndIf
				things.addlast o
			Case "[" 'new array
				a:jsonarray=parsearray()
				If Not a
					Print "error - couldn't parse array"
				EndIf
				things.addlast a
			End Select
		Wend
	End Method	
	
	Method parseobject:jsonobject()
		o:jsonobject=New jsonobject
		While getnext(["~q","}"])
			Select curchr
			Case "~q"
				p:jsonpair=parsepair()
				If Not p
					Print "error reading pair"
				EndIf
				o.pairs.addlast p
				If Not getnext([",","}"])
					Print "error after reading pair - expected either , or }"
				EndIf
				If curchr="}"
					Return o
				EndIf
			Case "}"
				Return o
			End Select
		Wend
		
		Print "error reading Object - expected a } at least!"
	End Method
	
	Method parsepair:jsonpair()
		p:jsonpair=New jsonpair
		p.name=parsestring()
		If Not getnext([":"])
			Print "error reading pair - expected a :"
		EndIf
		v:jsonvalue=parsevalue()
		If Not v
			Print "error reading pair - couldn't read a value"
		EndIf
		p.value=v
		Return p
	End Method
	
	Method parsearray:jsonarray()
		a:jsonarray=New jsonarray
		While getnext(["~q","-","0","1","2","3","4","5","6","7","8","9","{","[","t","f","n","]"])
			Select curchr
			Case "~q","-","0","1","2","3","4","5","6","7","8","9","{","[","t","f","n"
				i:-1
				v:jsonvalue=parsevalue()
				a.values.addlast v
				If Not getnext([",","]"])
					Print "error - expecting , or ]"
				EndIf
				If curchr="]"
					Return a
				EndIf
				
			Case "]"
				Return a
			End Select
		Wend
		Print "error - expecting a value or ]"
	End Method
	
	Method parsestring$()
		oldi=i
		s$=""
		
		While getnext(["~q","\"],0)
			s:+txt[oldi..i-1]
			Select curchr
			Case "~q"
				Return s
			Case "\"
				Select Chr(txt[i])
				Case "~q"
					s:+"~q"
				Case "\"
					s:+"\"
				Case "/"
					s:+"/"
				Case "b"
					s:+Chr(8)
				Case "f"
					s:+Chr(12)
				Case "n"
					s:+"~n"
				Case "r"
					s:+"~r"
				Case "t"
					s:+"~t"
				Case "u"
					s:+parseunicode()
				End Select
				i:+1
			End Select
			oldi=i
		Wend
	End Method
	
	Method parseunicode$()
		n:Short=0
		For t=1 To 4
			n:*16
			c=txt[i+t]
			If c>48 And c<57
				n:+c-48
			ElseIf c>=65 And c<=70
				n:+c-55
			ElseIf c>=97 And c<=102
				n:+c-87
			EndIf
		Next
		i:+4
		Return Chr(n)
	End Method
	
	Method parsevalue:jsonvalue()
		If Not getnext(["~q","-","0","1","2","3","4","5","6","7","8","9","{","[","t","f","n"])
			Print "error - expecting the beginning of a value"
		EndIf
		Select curchr
		Case "~q"
			s$=parsestring()
			Return jsonstringvalue.Create(s,0)
		Case "-","0","1","2","3","4","5","6","7","8","9"
			n:Double=parsenumber()
			Return jsonnumbervalue.Create(n)
		Case "{"
			o:jsonobject=parseobject()
			Return o
		Case "["
			a:jsonarray=parsearray()
			Return a
		Case "t"
			i:+3
			Return jsonliteralvalue.Create(1)
		Case "f"
			i:+4
			Return jsonliteralvalue.Create(0)
		Case "n"
			i:+2
			Return jsonliteralvalue.Create(-1)
		End Select
	End Method
	
	Method parsenumber:Double()
		i:-1
		sign=1
		n:Double=0
		Select Chr(txt[i])
		Case "-"
			i:+2
			Return parsenumber()*(-1)
		Case "0"
			i:+1
			If getnext(["."])
				n=parsefraction()
			EndIf
		Case "1","2","3","4","5","6","7","8","9"
			n=parseinteger()
			If getnext(["."])
				n:+parsefraction()
			EndIf
		End Select
		
		If Chr(txt[i])="e" Or Chr(txt[i])="E"
			i:+1
			Select Chr(txt[i])
			Case "+"
				sign=1
			Case "-"
				sign=-1
			Default
				Print "error - not a + or - when reading exponent in number"
			End Select
			e=parseinteger()
			n:*10^(sign*e)
		EndIf
		Return n
	End Method
			
	Method parsefraction:Double()
		fdigits=0
		n:Double=0
		While txt[i]>=48 And txt[i]<=57 And i<Len(txt)
			n:*10
			n:+txt[i]-48
			fi:+1
			fdigits:+1
		Wend
		n:/(10^fdigits)
		If i=Len(txt)
			Print "error - reached EOF while reading number"
		EndIf
		Return n
	End Method
	
	Method parseinteger:Double()
		n:Double=0
		While txt[i]>=48 And txt[i]<=57 And i<Len(txt)
			n:*10
			n:+txt[i]-48
			i:+1
		Wend
		If i=Len(txt)
			Print "error - reached EOF while reading number"
		EndIf
		Return n
	End Method
			
End Type

Type jsonvalue

	Method repr$(tabs$="")
		Return tabs
	End Method
End Type

Type jsonobject Extends jsonvalue
	Field pairs:TList

	Method New()
		pairs=New TList
	End Method
	
	Method addnewpair(txt$,o:Object)
		If String(o)
			value:jsonvalue=jsonstringvalue.Create(String(o))
		Else
			value:jsonvalue=jsonvalue(o)
		EndIf
		pairs.addlast jsonpair.Create(txt,value)
	End Method
	
	Method repr$(tabs$="")
		t$="{"
		ntabs$=tabs+"~t"
		op:jsonpair=Null
		For p:jsonpair=EachIn pairs
			If op Then t:+","
			t:+"~n"+ntabs+p.repr(ntabs)
			op=p
		Next
		t:+"~n"+tabs+"}"
		Return t
	End Method
	
	Method getvalue:jsonvalue(name$)
		For p:jsonpair=EachIn pairs
			If p.name=name
				Return p.value
			EndIf
		Next
	End Method
	
	Method getstringvalue$(name$)
		v:jsonstringvalue=jsonstringvalue(getvalue(name))
		If v
			Return v.txt
		EndIf
	End Method
	
	Method getnumbervalue:Double(name$)
		v:jsonnumbervalue=jsonnumbervalue(getvalue(name))
		If v
			Return v.number
		EndIf
	End Method
	
	Method getliteralvalue(name$)
		v:jsonliteralvalue=jsonliteralvalue(getvalue(name))
		If v
			Return v.value
		EndIf
	End Method
	
	Method getarrayvalue:jsonarray(name$)
		v:jsonarray=jsonarray(getvalue(name))
		Return v
	End Method
	
	Method getobjectvalue:jsonobject(name$)
		v:jsonobject=jsonobject(getvalue(name))
		Return v
	End Method
				
	
End Type

Type jsonpair
	Field name$,value:jsonvalue

	Function Create:jsonpair(name$,value:jsonvalue)
		p:jsonpair=New jsonpair
		p.name=name
		p.value=value
		Return p
	End Function

	Method repr$(tabs$="")
		t$="~q"+name+"~q : "
		For i=1 To (Len(t)+7)/8
			tabs:+"~t"
		Next
		middo$=""
		For i=1 To (8-(Len(t) Mod 8))
			middo:+" "
		Next
		Return t+middo+value.repr(tabs)
	End Method
End Type

Type jsonarray Extends jsonvalue
	Field values:TList
	
	Method New()
		values=New TList
	End Method
	
	Method addlast(o:Object)
		If String(o)
			value:jsonvalue=jsonstringvalue.Create(String(o))
		Else
			value:jsonvalue=jsonvalue(o)
		EndIf
		values.addlast value
	End Method
		
	Method repr$(tabs$="")
		t$="["
		ntabs$=tabs+"~t"
		ov:jsonvalue=Null
		For v:jsonvalue=EachIn values
			If ov Then t:+","
			t:+"~n"+ntabs+v.repr(ntabs)
			ov=v
		Next
		t:+"~n"+tabs+"]"
		Return t
	End Method
End Type


Type jsonstringvalue Extends jsonvalue
	Field txt$
	
	Function Create:jsonstringvalue(txt$,pretty=1)
		jsv:jsonstringvalue=New jsonstringvalue
		
		If pretty
			otxt$=""
			i=0
			For i=0 To Len(txt)-1
				Select Chr(txt[i])
				Case "~q"
					otxt:+"\~q"
				Case "\"
					otxt:+"\\"
				Case "/"
					otxt:+"\/"
				Case Chr(8)
					otxt:+"\b"
				Case Chr(12)
					otxt:+"\f"
				Case "~n"
					otxt:+"\n"
				Case "~r"
					otxt:+"\r"
				Case "~t"
					otxt:+"\t"
				Default
					otxt:+Chr(txt[i])
				End Select
			Next
			jsv.txt=otxt
		Else
			jsv.txt=txt
		EndIf
		Return jsv
	End Function
	
	Method repr$(tabs$="")
		Return "~q"+txt+"~q"
	End Method
End Type

Type jsonnumbervalue Extends jsonvalue
	Field number:Double
	
	Function Create:jsonnumbervalue(n:Double)
		jnv:jsonnumbervalue=New jsonnumbervalue
		jnv.number=n
		Return jnv
	End Function
	
	Method repr$(tabs$="")
		If number Mod 1=0
			Return String(Int(number))
		Else
			Return String(number)
		EndIf
	End Method
End Type

Type jsonliteralvalue Extends jsonvalue
	Field value
	'1 - true
	'0 - false
	'-1 - nil
	
	Function Create:jsonliteralvalue(value)
		jlv:jsonliteralvalue=New jsonliteralvalue
		jlv.value=value
		Return jlv
	End Function
	
	Method repr$(tabs$="")
		Select value
		Case 1
			Return "true"
		Case 0
			Return "false"
		Case -1
			Return "nil"
		End Select
	End Method
End Type



Function jsonise:TList(o:Object,serialised:tmap=Null)
	If Not serialised serialised=New tmap
	If serialised.contains(o)
		Return New TList
	EndIf
	
	Global numkeys
	l:TList=New TList
	todo:TList=New TList
	todo.addlast o
	
	While todo.count()
		o:Object=todo.removefirst()
		tt:TTypeId=TTypeId.ForObject(o)
		numkeys:+1
		key$=numkeys
		serialised.insert o,key
		jo:jsonobject=New jsonobject
		jo.addnewpair "type",tt.name()
		jo.addnewpair "key",key
		If tt.name()[tt.name().length-2..]="[]"	'array
			jo.addnewpair "length",String(tt.arraylength(o))
			ia:jsonarray=New jsonarray
			For i=0 To tt.arraylength(o)-1
				o2:Object=tt.getarrayelement(o,i)
				If o2
					If String(o2)
						key$="string:"+String(o2)
					Else
						For jv:jsonvalue=EachIn jsonise(o2,serialised)
							l.addlast jv
						Next
						key$="object:"+String(serialised.valueforkey(o2))
					EndIf
				Else
					key="null"
				EndIf
				ia.addlast key
			Next
			jo.addnewpair "items",ia
		Else	'object
			fo:jsonobject=New jsonobject
			jo.addnewpair "fields",fo
			For f:TField=EachIn tt.enumfields()
				o2:Object=f.get(o)
				Local j2:jsonvalue
				Select f.typeid()
				Case StringTypeId,IntTypeId,DoubleTypeId,FloatTypeId,IntTypeId
					j2=jsonstringvalue.Create("string:"+String(o2))
				Default
					If o2
						If Not serialised.contains(o2)
							For jv:jsonvalue=EachIn jsonise(o2,serialised)
								l.addlast jv
							Next
						EndIf
						key$=String(serialised.valueforkey(o2))
						j2=jsonstringvalue.Create("object:"+key)
					Else
						j2=jsonstringvalue.Create("null")
					EndIf
				End Select
				fo.addnewpair f.name(),j2
			Next
		EndIf
		l.addlast jo
	Wend
	Return l
End Function

Function unjsonise:TList(data$)
	Local ov:Object

	jd:jsondecoder=jsondecoder.Create(data)
	jd.parse()
	objects:tmap=New tmap
	
	'create objects first so we can fulfil references
	For jo:jsonobject=EachIn jd.things
		tname$=jo.getstringvalue("type")
		Print tname
		If tname[tname.length-2..]="[]"	'array
			tt:TTypeId=TTypeId.ForName(tname)
			length=Int(jo.getstringvalue("length"))
			o:Object=tt.newarray(length)
		Else	'object
			tt:TTypeId=TTypeId.ForName(tname$)
			o:Object=tt.newobject()
		EndIf
		key$=jo.getstringvalue("key")
		objects.insert key,o
	Next
	
	l:TList=New TList
	'now fill in fields
	For jo:jsonobject=EachIn jd.things
		key$=jo.getstringvalue("key")
		o:Object=objects.valueforkey(key)
		tname$=jo.getstringvalue("type")
		If tname[tname.length-2..]="[]"	'array
			tt:TTypeId=TTypeId.ForName(tname)
			length=tt.arraylength(o)
			c=0
			For js:jsonstringvalue=EachIn jo.getarrayvalue("items").values
				value$=js.txt
				If value<>"null"
					i=value.find(":")
					Select value[..i]
					Case "string"
						ov=value[i+1..]
					Case "object"
						ov=objects.valueforkey(value[i+1..])
					End Select
					tt.setarrayelement o,c,ov
				EndIf
				c:+1
			Next
		Else		
			tt:TTypeId=TTypeId.ForName(tname)
			fo:jsonobject=jo.getobjectvalue("fields")
			For jp:jsonpair=EachIn fo.pairs
				name$=jp.name
				f:TField=tt.findfield(name)
				value$=jsonstringvalue(jp.value).txt
				If value<>"null"
					i=value.find(":")
					Select value[..i]
					Case "string"
						ov=value[i+1..]
					Case "object"
						ov=objects.valueforkey(value[i+1..])
					End Select
					f.set o,ov
				EndIf
			Next
		EndIf
		l.addlast o
	Next
	Return l
End Function
	

Rem
Type dude
	Field name$
	Field age
	Field owner:dude
End Type
b:dude=New dude
b.name="bob"
b.age=12
c:dude=New dude
c.name="jim"
c.age=112
c.owner=b

l:TList=New TList
l.addlast b
l.addlast c
txt$=""
ja:jsonarray=New jsonarray
Local arr:dude[]=[b,c,dude(Null)]
Local arr2[]=[1,2,3]
For j:jsonvalue=EachIn jsonise(arr)
'	Print j.repr()
	txt$:+j.repr()
	ja.addlast j
Next

Print "JSON"
Print ja.repr()


Print "~nRESULTS"
For arr=EachIn unjsonise(txt)
	For d:dude=EachIn arr
		Print "dude: "+d.name
		Print "------------"
		Print "age: "+d.age
		If d.owner
			Print "owner: "+d.owner.name
		EndIf
		Print ""
	Next
Next
endrem