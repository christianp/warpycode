Global shapes:TList=New TList
Type shape
	Method New()
		shapes.addlast Self
	End Method
	
	Function fit(points#[])
		For s:shape=EachIn shapes
			If s.match(points)
				Print TTypeId.ForObject(s).name()
				Return
			EndIf
		Next
		
	End Function
	
	Method match(points#[]) Abstract
End Type

Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan<-180 dan:+360
	If dan>180 dan:-360
	Return dan
End Function

New circle
Type circle Extends shape
	Method match(points#[])
		mean2d points,cx#,cy#
				
		ox#=points[0]
		oy#=points[1]
		oan#=ATan2(oy-cy,ox-cx)
		startan#=oan
		td#=0
		n=0
		For i=0 To Len(points)-1 Step 2
			dx#=points[i]-cx
			dy#=points[i+1]-cy
			d#=Sqr(dx*dx+dy*dy)
			td:+d
			an#=ATan2(dy,dx)
			If andiff(an,oan)<0 Return False
			oan=an
			n:+1
		Next
		td:/n
		'Print "startan-an: "+andiff(startan,an)
		If Abs(andiff(startan,an))<30
			node.Create cx,cy,td
			Return True
		EndIf
	End Method
End Type

Function mean2d(points#[],cx# Var, cy# Var)
	n=0
	For i=0 To Len(points)-1 Step 2
		cx:+points[i]
		cy:+points[i+1]
		n:+1
	Next
	cx:/n
	cy:/n
End Function

New line
Type line Extends shape
	Method match(points#[])
		t#=0
		n=0
		can#=ATan2(points[Len(points)-1]-points[1],points[Len(points)-2]-points[0])
		For i=2 To Len(points)-1 Step 2
			dx#=points[i]-points[0]
			dy#=points[i+1]-points[1]
			an#=ATan2(dy,dx)
			t:+Abs(andiff(an,can))
			n:+1
		Next
		t:/n
		'Print "t: "+t
		If Abs(t)<5
			pipe.Create points[0],points[1],points[Len(points)-2],points[Len(points)-1]
			Return True
		EndIf
	End Method
End Type

Global things:TList=New TList
Type thing
	Method New()
		things.addlast Self
	End Method

	Method update() Abstract
	Method draw() Abstract
End Type

Type node Extends thing
	Field x#,y#,r#
	Function Create:node(x#,y#,r#)
		n:node=New node
		n.x=x
		n.y=y
		n.r=r
		Return n
	End Function
	
	Method update()
	End Method
	
	Method draw()
		SetColor 0,0,255
		SetAlpha .4
		DrawOval x-r,y-r,2*r,2*r
	End Method
End Type

Type pipe Extends thing
	Field x1#,y1#,x2#,y2#
	
	Function Create:pipe(x1#,y1#,x2#,y2#)
		p:pipe=New pipe
		p.x1=x1
		p.y1=y1
		p.x2=x2
		p.y2=y2
		Return p
	End Function
	
	Method update()
	End Method
	
	Method draw()
		SetColor 255,0,0
		SetAlpha 1
		DrawLine x1,y1,x2,y2
	End Method
End Type

Graphics 600,600,0
SetBlend ALPHABLEND
Local points#[]

Local ox#,oy#,mx#,my#
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	ox=mx
	oy=my
	mx=MouseX()
	my=MouseY()
	If MouseDown(1)
		dx=mx-ox
		dy=my-oy
		If dx*dx+dy*dy>5
			points:+[mx,my]
		EndIf
		For i=0 To Len(points)-1 Step 2
			DrawRect points[i],points[i+1],1,1
		Next
	Else
		If Len(points)
			shape.fit(points)
			points=New Float[0]
		EndIf
	EndIf
	
	For t:thing=EachIn things
		t.update
	Next
	
	For t:thing=EachIn things
		t.draw
	Next
	SetAlpha 1
	SetColor 255,255,255

	Flip
	Cls
Wend