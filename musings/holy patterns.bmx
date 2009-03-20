Type point
	Field x#,y#
	Field an#,r#,n
End Type

Type shape
	Field x#,y#,r#,an#,fit#
	Field points:TList
	Field name$
	
	Method draw()
		SetColor 0,255,0
		op:point=point(points.last())
		For p:point=EachIn points
			DrawRect p.x,p.y,3,3
			DrawLine op.x,op.y,p.x,p.y
			op=p
		Next
		
		SetColor 255,0,0
		DrawLine x,y,x+Cos(an)*r,y+Sin(an)*r
	End Method
End Type

Function inscribe:shape(points:TList)
	If points.count()<2 Return
	npoints:TList=points.copy()
	Local tx#,ty#
	n=1
	For p:point=EachIn npoints
		tx:+p.x
		ty:+p.y
		p.n=n
		n:+1
	Next
	tx:/npoints.count()
	ty:/npoints.count()
	Local tr#
	For p=EachIn npoints
		dx#=p.x-tx
		dy#=p.y-ty
		p.an=ATan2(dy,dx)
		p.r=Sqr(dx*dx+dy*dy)
		tr:+p.r
	Next
	tr:/npoints.count()
	
	Function ancmp(o1:Object,o2:Object)
		If point(o1).an>point(o2).an
			Return 1
		Else
			Return -1
		EndIf
	End Function
	
	npoints.sort True,ancmp
	
	Local pattern[points.count()]
	i=0
	For p=EachIn npoints
		pattern[i]=p.n
		i:+1
		DrawText i,p.x,p.y
	Next
	s$=""
	For n=EachIn pattern
		s:+n+","
	Next
	DrawText s,0,0

	Local tdr#
	For p=EachIn points
		tdr:+Abs(Log(tr/p.r))
	Next
	tdr:/npoints.count()

	
	pa:shape=New shape
	pa.x=tx
	pa.y=ty
	pa.r=tr
	pa.fit=tdr
	pa.points=points
	p1:point=point(npoints.first())
	p2:point=point(npoints.last())
	pa.an=ATan2(p1.y-p2.y,p1.x-p2.x)
	
	If matcharr(pattern,[1,4,2,5,3]) Or matcharr(pattern,[1,3,5,2,4])
		DrawText "pentagram",0,13
		pa.name="pentagram"
	EndIf
	If matcharr(pattern, clockarr(points.count())) Or matcharr(pattern,anticlockarr(points.count()))
		DrawText "clock",0,13
		pa.name="clock"
	EndIf
	
	DrawText "tdr: "+(1/tdr),0,26
	SetAlpha .3
	DrawOval tx-tr,ty-tr,tr*2,tr*2
	SetAlpha 1
	
	Return pa
End Function

Function clockarr[](n)
	Local arr[n]
	For i=0 To n-1
		arr[i]=i+1
	Next
	Return arr
End Function

Function anticlockarr[](n)
	Local arr[n]
	For i=0 To n-1
		arr[i]=n-i
	Next
	Return arr
End Function

Function matcharr(arr1[],arr2[])
	If Len(arr1)<>Len(arr2) Return False
	off=-1
	l=Len(arr1)
	For i=0 To Len(arr1)-1
		If arr2[i]=arr1[n]
			off=i
			Exit
		EndIf
	Next
	If off=-1 Return 0
	For i=0 To Len(arr1)-1
		If arr1[i]<>arr2[(i+off) Mod l] Return False
	Next
	Return True
End Function

Graphics 600,600,0
SetBlend ALPHABLEND

points:TList=New TList
shapes:TList=New TList

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	If MouseHit(1)
		p:point=New point
		p.x=MouseX()
		p.y=MouseY()
		points.addlast p
	EndIf
	
	For pa:shape=EachIn shapes
		pa.draw
	Next
	
	If points.count()
		SetColor 255,255,255
		op:point=point(points.last())
		For p:point=EachIn points
			DrawRect p.x,p.y,3,3
			DrawLine op.x,op.y,p.x,p.y
			op=p
		Next
		pa:shape=inscribe(points)
		If MouseHit(2)
			If pa
				shapes.addlast pa
			EndIf
			points=New TList
		EndIf
	EndIf	

	Flip
	Cls
Wend
