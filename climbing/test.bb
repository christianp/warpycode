Type client
	Field stream,id$,name$
	Field lhx,lhy
	Field rhx,rhy
	Field llx,lly
	Field rlx,rly
	Field in$
End Type

Global numids

Global seed=MilliSecs()
in$=""
Graphics 640,480,32,2
SetBuffer BackBuffer()
server=CreateTCPServer(921)
in$=""
While Not KeyHit(1)
	stream=AcceptTCPStream(server)
	If stream
		Print "new!"
		newclient(stream)
	EndIf
	For c.client=Each client
		If ReadAvail(c\stream)
			byte=ReadByte(c\stream)
			If byte = 10
				parse(c)
				c\in=""
			Else
				c\in=c\in+Chr(byte)
			EndIf
		EndIf
		If Eof(c\stream)
			Print c\name+" quit"
			id=c\id
			Delete c
			out("q"+id)
		EndIf
	Next
	Flip
Wend

Function parse(c.client)
	in$=c\in
	Print "got "+in
	Select Left(in,1)
		Case "n"	;Joining game
			Print "joining game"
			name$=Mid(in,2)
			Print name+" is joining"
			c\name=name
			out("j"+c\id+":"+name)
		Case "m"	;Moving
			Print "moving"
			x$=Mid(in,2,2)
			y$=Mid(in,4,2)
			legs$=Mid(in,6,1)
			side$=Mid(in,7,1)
			moveclimber(c,x,y,legs,side)
			Print x+","+y+" "+legs+"  "+side
			out("m"+c\id+x+y+legs+side)
		Default
			Print "eh?"
	End Select
End Function

Function newclient(stream)
	c.client=New client
	c\stream=stream
	numids=numids+1
	c\id=pad(numids,2)
	outto(c,"s"+seed)
	outto(c,"n"+c\id)
	For ca.client=Each client
		If ca<>c
			outto(c,"j"+ca\id+":"+ca\name)
			outto(c,"m"+ca\id+pad(ca\lhx,2)+pad(ca\lhy,2)+"01")
			outto(c,"m"+ca\id+pad(ca\rhx,2)+pad(ca\rhy,2)+"02")
			outto(c,"m"+ca\id+pad(ca\llx,2)+pad(ca\lly,2)+"11")
			outto(c,"m"+ca\id+pad(ca\rlx,2)+pad(ca\rly,2)+"12")
		EndIf
	Next
End Function

Function moveclimber(c.client,x,y,legs,side)
	If legs
		If side And 1	;Left leg
			c\llx=x
			c\lly=y
		ElseIf side And 2	;Right leg
			c\rlx=x
			c\rly=y
		EndIf
	Else
		If side And 1	;Left arm
			c\lhx=x
			c\lhy=y
		ElseIf side And 2	;Right arm
			c\rhx=x
			c\rhy=y
		EndIf
	EndIf
End Function

Function out(out$)
	For c.client=Each client
		outto(c,out)
	Next
End Function

Function outto(c.client,out$)
	For ca=1 To Len(out)
		WriteByte c\stream,Asc(Mid(out,ca,1))
	Next
	WriteByte c\stream,10
End Function

Function pad$(in$,length)
	If Len(in)<length
		Print "padding"
		pad$=""
		For c=1 To length-Len(in)
			pad$=pad+"0"
		Next
		Print "padded to "+pad+in
		Return pad+in
	EndIf
	Return in
End Function