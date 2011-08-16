Const width=800,height=600

Graphics width,height,0

Type point
	Field x#,y#
	Field ax#,ay#
End Type

Type stroke
	Field sx#,sy#
	Field pointslist:TList
	
	Method addpoint(x#,y#)
		p:point=New point
		If pointslist.isempty()
			p.x=0
			p.y=0
			p.ax=x-sx
			p.ay=y-sy
		Else
			l:point=point(pointslist.Last())
			p.ax=x-sx
			p.ay=y-sy
			p.x=p.ax-l.ax
			p.y=p.ay-l.ay
		EndIf
		ListAddLast pointslist,p
	End Method
End Type

Type letter
	Field strokeslist:TList
	
	Method addstroke(s:stroke)
		ListAddLast strokeslist,
End Type

Local strokes:TList=CreateList()
Local current:stroke
Local ms=MilliSecs()
Local lastadd=ms
While Not KeyHit(KEY_ESCAPE)
	mx=MouseX()
	my=MouseY()
	
	If MouseDown(1)
		If current=Null
			current=New stroke
			ListAddLast strokes,current			current.pointslist=CreateList()
			current.sx=mx
			current.sy=my
			lastadd=ms
		Else
			If current.pointslist.isempty()
				current.addpoint(mx,my)
			Else
				l:point=point(current.pointslist.Last())
				dx#=mx-current.sx-l.ax
				dy#=my-current.sy-l.ay
				l1#=Sqr(l.x*l.x+l.y*l.y)
				l2#=Sqr(dx*dx+dy*dy)
				dotprod#=(l.x*dx+l.y*dy)/(l1*l2)
				If dotprod<.97 Or l2>30 ' Or ms-lastadd>500 Or Len(current.pointslist)=1
					current.addpoint(mx,my)
					lastadd=ms
				EndIf
				DrawText dotprod,0,0
			EndIf
		EndIf
	Else
		If current<>Null
			current.addpoint(mx,my)
			current=Null
		EndIf
	EndIf
	
	For s:stroke=EachIn strokes
		link:TLink=s.pointslist.FirstLink()
		x#=s.sx
		y#=s.sy
		While link
			p:point=point(link.value())
			newx#=x+p.x
			newy#=y+p.y
			DrawLine x,y,newx,newy
			DrawRect x,y,3,3
			x=newx
			y=newy
			link=link.NextLink()
		Wend
		If s=current Then DrawLine x,y,mx,my
	Next
	
	oldms=ms
	ms=MilliSecs()
	Flip
	Cls
	FlushMem
Wend