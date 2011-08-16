Global mousegestures:TList=New TList
Type tmousegesture
	Field done
	
	Method New()
		mousegestures.addlast Self
	End Method
	
	Function updateall()
		x=MouseX()
		y=MouseY()
		For mg:tmousegesture=EachIn mousegestures
			mg.update x,y
		Next
	End Function

	Method update(x,y) Abstract
	
	Method performed()
		odone=done
		done=0
		Return odone
	End Method
End Type

Global mouseshake:tmouseshake=New tmouseshake
Type tmouseshake Extends tmousegesture
	Field ox,oy
	Field dir,shakes,notshake
	
	Method update(x,y)
		dx=x-ox
		dy=y-oy
		If Abs(dy)<5 And Abs(dx)>5 And Sgn(dx)<>dir
			dir=Sgn(dx)
			shakes:+1
			If shakes>3
				done=1
			EndIf
			notshake=0
		Else
			notshake:+1
			If notshake>10
				shakes=0
				dir=0
			EndIf
		EndIf
		ox=x
		oy=y
	End Method
End Type

Global things:TList=New TList

Type thing
	Field size#
	Field x#,y#,ox#,oy#
	Field shakes,nonshake,dir
	Field links:TList
	
	Method New()
		links=New TList
	End Method
	
	Function Create:thing(x#,y#,size#)
		t:thing=New thing
		things.addlast t
		t.x=x
		t.y=y
		t.size=size
		Return t
	End Function
	
	Function pick:thing(x#,y#, closest:thing Var)
		mindist#=-1
		For t:thing=EachIn things
			dx#=x-t.x
			dy#=y-t.y
			d#=dx*dx+dy*dy
			If d<t.size*t.size And (d<mindist Or mindist=-1)
				closest=t
				mindist=d
			EndIf
		Next
		Return closest
	End Function
	
	Method drag(tx#,ty#)
		ox=x
		oy=y
		dx#=tx-ox
		dy#=ty-oy
		x:+dx
		y:+dy
		Rem
		d#=dx*dx+dy*dy
		If d>25 And (Sgn(dx)<>dir) And Abs(dy)<5
			shakes:+1
			nonshake=0
			If shakes>3
				resetlinks
				shakes=0
			EndIf
			dir=Sgn(dx)
		Else
			nonshake:+1
			If nonshake>10
				shakes=0
			EndIf
		EndIf
		EndRem
		If mouseshake.performed()
			resetlinks
		EndIf

		
		makelinks
	End Method
	
	Method resetlinks()
		For t:thing=EachIn links
			t.links.remove Self
		Next
		links=New TList
	End Method
	
	Method makelinks()
		For t:thing=EachIn things
		If t<>Self And (Not links.contains(t))
			dx#=x-t.x
			dy#=y-t.y
			d#=Sqr(dx*dx+dy*dy)-size-t.size
			If d<0
				linkto t
			EndIf
		EndIf
		Next
	End Method
	
	Method linkto(t:thing)
		If linkedto(t) Return
		
		links.addlast t
		t.linkto Self
	End Method
	
	Method linkedto(t:thing,l:TList=Null)
		If l=Null l=New TList
		If t=Self Return 1
		l.addlast Self
		For t2:thing=EachIn links
			If Not l.contains(t2)
				If t2.linkedto(t,l) Return 1
			EndIf
		Next
	End Method
	
	Method update()
		For t:thing=EachIn links
			alignto t
		Next
	End Method
	
	Method alignto(t:thing)
		dx#=t.ox-ox
		dy#=t.oy-oy
		mindist#=size+t.size+50
		an#=ATan2(dy,dx)
		f#=.1
		If an<-135 Or an>135 Or Abs(an)<45	'left or right quadrant
			y:+dy*f
			t.y:-dy*f
			x:+(dx-Sgn(dx)*mindist)*f*.5
		Else	'top or bottom quadrant
			x:+dx*f
			t.x:-dx*f
			y:+(dy-Sgn(dy)*mindist)*f
		EndIf
	End Method
	
	Method drawlinks()
		SetColor 255,255,255
		For t:thing=EachIn links
			DrawLine x,y,t.x,t.y
		Next
	End Method
			
	Method draw()
		
		SetColor 100,100,255
		DrawOval x-size,y-size,size*2,size*2
		SetColor 0,0,0
		DrawText shakes,x,y-15
		
	End Method
End Type


Graphics 600,600,0 


thing.Create 300,300,30

Local grab:thing
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	mx=MouseX()
	my=MouseY()
	tmousegesture.updateall
	
	If MouseDown(1)
		If Not grab
			If thing.pick(mx,my,grab)
				offx#=grab.x-mx
				offy#=grab.y-my
			EndIf
		EndIf
		If grab
			grab.drag mx+offx,my+offy
		EndIf
	Else
		If grab
			grab.shakes=0
			grab=Null
		EndIf
	EndIf
	
	If MouseHit(2)
		thing.Create mx,my,Rand(20,50)
	EndIf
	
	For t:thing=EachIn things
		t.ox=t.x
		t.oy=t.y
	Next
	
	For t:thing=EachIn things
		t.update
	Next
	
	For t:thing=EachIn things
		t.drawlinks
	Next
	
	For t:thing=EachIn things
		t.draw
	Next
	
	Flip
	Cls
Wend