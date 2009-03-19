Graphics 800,800,0

letters$="0123456789+-*=()abcdefghijklmnopqrstuvwxyz"

Type vector
	Field x#,y#
	Function create:vector(x#,y#)
		v:vector=New vector
		v.x=x
		v.y=y
		Return v
	End Function
End Type

vectors:tlist=New tlist
minx#=400
miny#=400
maxx#=0
maxy#=0
lettervectors:tlist=New tlist
mousestate=0
c=0
While Not KeyHit(KEY_ESCAPE)
	DrawLine 200,200,600,200
	DrawLine 600,200,600,600
	DrawLine 600,600,200,600
	DrawLine 200,600,200,200
	
	mx#=(MouseX()-200)/400.0
	my#=(MouseY()-200)/400.0
	
	Select mousestate
	Case 0
		If MouseDown(1)
			mousestate=1
		EndIf
	Case 1
		If Not MouseDown(1)
			vectors.addlast vector.create(-1,-1)
			mousestate=0
		EndIf
	End Select

	If mousestate
		If mx>0 And mx<1 And my>0 And my<1
			vectors.addlast vector.create(mx,my)
			If mx<minx minx=mx
			If mx>maxx maxx=mx
			If my<miny miny=my
			If my>maxy maxy=my
		EndIf
	EndIf
	
	x#=-1
	y#=-1
	SetLineWidth 10
	For v:vector=EachIn vectors
		If x>=0 And v.x>=0 
			DrawLine x*400+200,y*400+200,v.x*400+200,v.y*400+200
			DrawOval x*400+195,y*400+195,10,10
		EndIf
		x=v.x
		y=v.y
	Next
	SetLineWidth 1
	
	DrawText Chr(letters[c]),0,0
	
	If KeyHit(KEY_SPACE)
		dx#=maxx-minx+.1
		dy#=maxy-miny+.1
		midx#=(maxx+minx)/2.0
		midy#=(maxy+miny)/2.0
		If dx>dy d#=dx Else d#=dy
		For v:vector=EachIn vectors
			If v.x>=0
				v.x=(v.x-midx)/d+.5
				v.y=(v.y-midy)/d+.5
			EndIf
		Next
		lettervectors.addlast vectors
		vectors=New tlist
		c:+1
		minx#=400
		miny#=400
		maxx#=0
		maxy#=0
		If c=Len(letters) Exit
	EndIf
	
	Flip
	Cls
Wend

Cls
vectors:tlist=tlist(lettervectors.removefirst())
frame=0
c=0
pixmaps:tlist=New tlist
While Not KeyHit(KEY_ESCAPE)
	x=-1
	y=-1
	SetLineWidth 4
	For i=0 To frame-1
		v:vector=vector(vectors.valueatindex(i))
		If x>=0 And v.x>=0
			DrawLine x*50,y*50,v.x*50,v.y*50
			DrawOval x*50-2,y*50-2,4,4
		EndIf
		x=v.x
		y=v.y
	Next
	SetLineWidth 1
	pixmaps.addlast GrabPixmap(0,0,50,50)
	frame:+1
	
	If frame=vectors.count()
		p:tpixmap=CreatePixmap(30*50,50,PF_RGB888)
		For i=0 To 29
			frame=(i/30.0)*vectors.count()
			p.paste(tpixmap(pixmaps.valueatindex(frame)),50*i,0)
		Next
		pixmaps=New tlist
		name$=Chr(letters[c])
		Select name
		Case "+"
			name="plus"
		Case "-"
			name="minus"
		Case "/"
			name="divide"
		Case "*"
			name="multiply"
		Case "="
			name="equals"
		Case "("
			name="lbracket"
		Case ")"
			name="rbracket"
		End Select
		SavePixmapPNG p,"letters\"+name+".png"
		If lettervectors.count()
			vectors=tlist(lettervectors.removefirst())
			c:+1
			frame=0
		Else
			End
		EndIf
	EndIf
	
	Flip
	Cls
Wend