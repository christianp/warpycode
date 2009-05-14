

Type genericenum
	Field arr:Object[],i
	Field enum:Object,hn:TMethod,no:TMethod
	
	Global args:Object[]	'for the reflection invoke method
	
	Function Create:genericenum(o:Object)
		ge:genericenum=New genericenum
		If Not o
			Print "!!!!!"
		EndIf
		
		ge.arr=Object[](o)
		If Not ge.arr
			ge.enum:Object=TTypeId.ForObject(o).findmethod("ObjectEnumerator").invoke(o,args)
			ge.hn=TTypeId.ForObject(ge.enum).findmethod("HasNext")
			ge.no=TTypeId.ForObject(ge.enum).findmethod("NextObject")
		EndIf
		Return ge
	End Function
	
	Method hasnext()
		If arr
			If i<Len(arr) Return True Else Return False
		Else
			res=Int(String(hn.invoke(enum,args)))
			Return res
		EndIf
	End Method
	
	Method nextobject:Object()
		If arr
			i:+1
			Return arr[i-1]
		Else
			Return no.invoke(enum,args)
		EndIf
	End Method
	
	Method objectenumerator:genericenum()
		Return Self
	End Method
End Type


Local strings$[]=["hi","there"]

ge:genericenum=genericenum.Create(strings)
Print String(ge.nextobject())
Print ge.hasnext()

For s$=EachIn ge
	Print s
Next