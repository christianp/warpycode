Type rope
	Field length,txt$,l:rope,r:rope
	
	Function Create:rope(txt$)
		r:rope=New rope
		r.txt=txt
		r.length=Len(txt)
		Return r
	End Function
	
	Method add:rope(r:rope)
		r2:rope=New rope
		r2.length=length+r.length
		r2.l=Self
		r2.r=r
		Return r2
	End Method
	
	Method value$()
		If txt Return txt
		Return l.value()+r.value()
	End Method
	
	Method index(i)
		If txt
			Return txt[i]
		Else
			If i<l.length
				Return l.index(i)
			Else
				Return r.index(i-l.length)
			EndIf
		EndIf
	End Method
	
	Method substr:rope(start,finish)
		If start<0 start=0				'these should be error conditions really...
		If finish>length finish=length
		
		If txt
			Return rope.Create(txt[start..finish])
		Else
			If start<l.length
				If finish<l.length
					Return l.substr(start,finish)
				Else
					Return l.substr(start,l.length).add(r.substr(0,finish-l.length))
				EndIf
			Else
				Return r.substr(0,finish-l.length)
			EndIf
		EndIf
	End Method
	
	Method ObjectEnumerator:RopeEnum()	'enumerator implemented for giggles
		Local enum:RopeEnum=New RopeEnum
		enum.r=Self
		Return enum
	End Method
	
	Method repr$()	'shows structure of rope
		If txt
			Return txt
		Else
			Return "("+l.repr()+","+r.repr()+")"
		EndIf
	End Method
End Type

Type RopeEnum
	Field r:rope,i
	
	Method hasnext()
		Return i<r.length
	End Method
	
	Method nextobject:Object()
		s$=Chr(r.index(i))
		i:+1
		Return s
	End Method
End Type


r:rope=rope.Create("hello ").add(rope.Create("there"))
Print r.substr(2,9).repr()