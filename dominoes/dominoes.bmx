Function KnuthShuffle[](n)
	Local k[n]
	For i=0 To n-1
		k[i]=i
	Next
	
	For i=0 To n-2
		j=Rand(i,n-1)
		b=k[i]
		k[i]=k[j]
		k[j]=b
	Next
	
	Return k
End Function

Function idpermutation[](size)
	Local a[size]
	For i=0 To size-1
		a[i]=i
	Next
	Return a
End Function


Function drawbox(x#,y#,w#,h#)
		DrawzoomLine x,y,x+w,y
		DrawzoomLine x,y,x,y+h
		DrawzoomLine x+w,y,x+w,y+h
		DrawzoomLine x,y+h,x+w,y+h
End Function

Function drawzoomline( x1#,y1#,x2#,y2#)
	DrawLine x1-panx,y1-pany,x2-panx,y2-pany
End Function


Global dominoes:TList
Type domino
	Field numbraids
	Field braid[]
	Field ends[]
	Field starts[]
	Field x#,y#
	Field w#,h#
	Field prv:domino
	'Field nxt:domino
	Field prvoff',nxtoff

	Function init()
		dominoes=New TList
	End Function
	
	Method New()
		dominoes.addlast Self
	End Method
	
	Function Create:domino(x#,y#,numbraids,braid[]=Null,w#=100,h#=20)
		d:domino=New domino
		d.x=x
		d.y=y
		d.w=w
		d.numbraids=numbraids
		d.h=h*(numbraids+1)
		If Not braid
			d.braid=knuthshuffle(numbraids)
		Else
			d.braid=braid
		EndIf
		d.ends=New Int[numbraids]
		d.starts=New Int[numbraids]
		'Print "----"
		For i=0 To numbraids-1
			Print d.braid[i]
			d.ends[d.braid[i]]=i
			d.starts[i]=i
		Next
		'Print "----"
		
		
		Return d
	End Function
	
	Method draw()
		SetColor 255,255,255
		drawbox x,y,w,h
		
		For i=0 To numbraids-1
			by#=(i+.5)*h/numbraids+y
			ey#=(braid[i]+.5)*h/numbraids+y
			'DrawText ends[i],x+w,ey
			If prv
				indexcolour(prv.ends[i])
			Else
				indexcolour i
			EndIf
			DrawzoomLine x,by,x+w,ey
		Next
	End Method
	
	Method join(d:domino,off=0,amalgamate=1)
		prv:domino=d
		prvoff=off
		For i=0 To numbraids-1
			ends[braid[i]]=prv.ends[i]
		Next
		starts=prv.ends
		
		If Not amalgamate Return
		
		p:domino=Self
		length=1
		Local k[numbraids]
		While p
			id=1
			For i=0 To numbraids-1
				If p.starts[i]<>ends[i] id=0
			Next
			If id And length>1
				Print "IDENTITY! "+length
				amalgam(p)
			EndIf
			length:+1
			p=p.prv
		Wend
	End Method
	
	Method amalgam(p:domino)
		length=1
		d:domino=Self
		w#=0
		While d<>p
			w:+d.w
			dominoes.remove d
			d=d.prv
			length:+1
		Wend
		w:+p.w
		dominoes.remove p
		d:domino=domino.Create(p.x,p.y+5,p.numbraids,idpermutation(p.numbraids),w)
		Print dominoes.count()
		If p.prv
			d.join(p.prv,0,0)
		EndIf
	End Method
End Type

Function indexcolour(n)
	Select n
	Case 0
		SetColor 255,0,0
	Case 1
		SetColor 0,255,0
	Case 2
		SetColor 0,0,255
	End Select
End Function

SeedRnd MilliSecs()
domino.init
Global panx#,pany#
AppTitle="dominoh no"
Graphics 800,800,0

p:domino=domino.Create(50,400,3)

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	For d:domino=EachIn dominoes
		d.draw
	Next
	If dominoes.count()
		d:domino=domino(dominoes.last())
		ex=d.x+d.w
		ey=d.y
	Else
		ex=50
		ey=400
	EndIf
	If KeyHit(KEY_SPACE)
		If dominoes.count()
			p=domino(dominoes.last())
		Else
			p=Null
		EndIf
		d:domino=domino.Create(ex,ey,3)
		Print dominoes.count()+" DOMINOES"
		If p d.join(p)
	EndIf
	
	'panx:+(ex-400-panx)*.1
	If MouseDown(1)
		panx=opanx+ox-MouseX()
	Else
		opanx=panx
		ox=MouseX()
	EndIf
	
	Flip
	Cls
Wend