Type settlement
	Field n
	Field children:settlement[],kids
	Field x#,y#,r#=1

	Method New()
		x=Rnd(-10,10)
		y=Rnd(-10,10)
	End Method
	
	Function Create:settlement(n,updates=2000)
		s:settlement=New settlement
		For c=1 To n
			s.add
		Next
		For c=1 To updates
			s.update
		Next
		Return s
	End Function
		
	Method update()
		x:+Rnd(-1,1)*.01
		y:+Rnd(-1,1)*.01
		If kids
			tx#=0
			ty#=0
			For k:settlement=EachIn children
				k.update
				tx:+k.x*k.n
				ty:+k.y*k.n
			Next
			tx:/n
			ty:/n
			r=0
			For k:settlement=EachIn children
				k.x:-tx
				k.y:-ty
				d#=Sqr(k.x*k.x+k.y*k.y)+k.r
				If d>r
					r=d
				EndIf
			Next
			For k:settlement=EachIn children
				dx#=tx-k.x
				dy#=ty-k.y
				d#=Sqr(dx*dx+dy*dy)
				k.x :+ .01*dx
				k.y :+ .01*dy
			Next
			For c=1 To 5
				For i=0 To kids-1
					s1:settlement=children[i]
					For j=i+1 To kids-1
						s2:settlement=children[j]
						dx#=s2.x-s1.x
						dy#=s2.y-s1.y
						d#=Sqr(dx*dx+dy*dy)
						If d<s1.r+s2.r
							dx:/d
							dy:/d
							d=s1.r+s2.r-d
							f1#=d*(s2.n+1)/(s1.n+s2.n+2)
							f2#=d*(s1.n+1)/(s1.n+s2.n+2)
							s1.x:-dx*f1
							s1.y:-dy*f1
							s2.x:+dx*f2
							s2.y:+dy*f2
						EndIf
					Next
				Next
			Next
			
		EndIf
	End Method

	Method add()
		n:+1
		If kids
			j=Rand(0,kids-1)
			children[j].add
		Else
			If n>Rand(3,6)
				kids=Rand(2,4)
				children = New settlement[kids]
				For i=0 To kids-1
					children[i] = New settlement
				Next
				For i=0 To n
					j=Rand(0,kids-1)
					children[j].add
				Next
			Else
				r=Sqr(n)*6
			EndIf
		EndIf
	End Method
	
	Method draw(ox#,oy#)
		px#=ox+x
		py#=oy+y
		SetAlpha .2
		drawcircle px,py,r
		SetAlpha 1
		For k:settlement=EachIn children
			k.draw px,py
		Next
		If Not kids
			DrawText n,px-4,py-7
		EndIf
	End Method
		
	Method all:TList(l:TList=Null)
		If Not l
			l=New TList
		EndIf
		If Not kids
			l.addlast Self
		EndIf
		For k:settlement=EachIn children
			k.all l
		Next
		Return l
	End Method
End Type

Function drawcircle(x#,y#,r#)
	ox#=x+r
	oy#=y
	For an=0 To 360 Step 15
		cx#=x+Cos(an)*r
		cy#=y+Sin(an)*r
		DrawLine ox,oy,cx,cy
		ox=cx
		oy=cy
	Next
	DrawLine ox,oy,x+r,y
End Function

Graphics 600,600,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

While Not KeyHit(KEY_ESCAPE)
	If KeyHit(KEY_SPACE)
		s:settlement=settlement.Create(Rand(50,200))
	EndIf
	
	If s
		If MouseHit(1)
			s.add
		EndIf
		
		's.update
		
		s.draw 300,300
		DrawText s.n,0,0
		DrawText s.all().count(),0,15
	EndIf
	
	
	Flip
	Cls
Wend
