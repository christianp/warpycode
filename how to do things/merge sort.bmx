Function mergesort:TLink( link:TLink, size, cmp(o1:Object,o2:Object) )
	Print "mergesort "+size
	Select size
	Case 0,1
		Return link
	Case 2
		If cmp( link.value() , link._succ.value() ) < 0
			Return link
		Else
			l:TLink = link
			r:TLink = link._succ
			l._pred = r
			r._succ = l
			l._value = l
			'r._pred = Null
			Return r
		EndIf
	Default
		Local l:TLink=link, r:TLink=link
		size1=size/2
		size2=size-size1
		Print size1
		Print size2
		For i=0 To size1-1
			r=r._succ
		Next
		
		l=r
		'r=r._succ
		r=New TLink
		r._value=l._value
		r._pred=l._pred
		r._succ=l._succ
		Print "---"+String(r.value())
		l._value=l
		l=link
		
		'End
		scan l
		scan r
		
		l = mergesort( l, size1, cmp )
		r = mergesort( r, size2, cmp )

		scan l
		scan r
		Print "merge "+size1+" , "+size2
		result:TLink=merge( l, r, cmp )
		scan result
		Return result
	End Select
End Function

Function merge:TLink( l:TLink, r:TLink, cmp(o1:Object, o2:Object) )
	If Not l Return r
	If Not r Return l
	
	Print "merge"
	'scan l
	'scan r
	
	If cmp( l.value() , r.value() ) < 0
		link:TLink = r
		r = l
		l = link
	EndIf
	
	link = l
	
	While l._value<>l And r._value<>r
		While l._value<>l And cmp( l.value(), r.value() ) < 0
			l=l._succ
		Wend
		
		If l._value<>l
			r._pred = l._pred
			l._pred._succ = r
			nr:TLink = r._succ
			r._succ = l
			l._pred = r
			r = nr
		Else
			l=l._pred
			l._succ = r
			r._pred = l
			Return link
		EndIf
	Wend
	Return link
End Function

Function scan(link:TLink)
	Print "SCAN"
	Print String(link.value())
	While link._value<>link
		If link._succ = link
			Print "ENDSCAN same"
			 Return
		EndIf
		link=link._succ
		Print String(link.value())
	Wend
	Print "ENDSCAN"
	Return
End Function

l:TList=New TList
l.addlast "a"

l2:TList=New TList
l2.addlast "b"
l2.addlast "c"
'merge(l.firstlink(),l2.firstlink(),f)
For word$=EachIn l
'	Print word
Next
'End

Function f( o1:Object, o2:Object )
	Return o1.compare( o2 )
End Function

l=New TList
l2=New TList

l.addlast "a"
l.addlast "f"
l.addlast "c"
l.addlast "d"
l.addlast "b"
l.addlast "e"

mergesort( l.firstlink(), l.count(), f )
For word$=EachIn l
	Print word
Next