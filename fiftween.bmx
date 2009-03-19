Type cell
	Field neighbours:cell[4]
	Field numneighbours
	'0 up
	'1 left
	'2 down
	'3 right
	Field value

	Method New()
	End Method
	
	Method getneighbour:cell(dir)
		If Not neighbours[dir]
			'debugo "find neighbour"
			findneighbours(dir)
			If Not neighbours[dir]
				'debugo "create neighbour"
				c:cell=New cell
				neighbours[dir]=c
				c.neighbours[(dir+2) Mod 4]=Self
				numneighbours:+1
				c.numneighbours:+1
			EndIf
		EndIf

		Return neighbours[dir]
	End Method
	
	Method findneighbours(tdir)
		checked:TList=New TList
		onsearch(Self,checked,0,0,tdir)
		
	End Method
	
	Method onsearch(c:cell,checked:TList,x,y,tdir)
		If checked.contains(Self) Then Return
		
		checked.addlast Self
		If x=0
			If y=-1 'up
				dir=0
			Else 'down
				dir=2
			EndIf
		Else
			If x=-1 'left
				dir=1
			Else 'right
				dir=3
			EndIf
		EndIf
		
		If Abs(x)+Abs(y)=1 And c.neighbours[dir]=Null 'adjacent to original cell, not already known
			Select dir
			Case 0
				'debugo "found up"
			Case 2
				'debugo "found down"
			Case 1
				'debugo "found left"
			Case 3
				'debugo "found right"
			End Select
			c.neighbours[dir]=Self
			neighbours[(dir+2) Mod 4]=c
			c.numneighbours:+1
			numneighbours:+1
			If c.numneighbours=4 Then Return
			If tdir=dir Then Return
		EndIf
			
		For dir=0 To 3
			If neighbours[dir]
				dy=-Cos(dir*90)
				dx=-Sin(dir*90)
				'debugo "check ("+String(dx)+","+String(dy)+")"
				neighbours[dir].onsearch(c,checked,x+dx,y+dy,tdir)
				If c.numneighbours=4 
					'debugo "4 neighbours"
					Return
				EndIf
				If c.neighbours[tdir]
					'debugo "got wanted neighbour"
					Return
				EndIf
			EndIf
		Next
	End Method
	
	Method setvalue(n)
		debugo "new value "+String(n)
		value=n
	End Method
		
	
End Type

Type machine
	Field curcell:cell
	Field txt$,pos
	Field subs[1000]
	Field backtrace[100]
	Field tracesize
	
	Field compressedtxt$
	
	Method New()
		curcell=New cell
	End Method
	
	Function Create:machine(txt$)
		m:machine=New machine
		m.txt=txt+" "
		m.findsubs()
		Return m
	End Function
	
	Method compresscode(cmd$)
		compressedtxt:+cmd
	End Method
	
	Method findsubs()
		pos=0
		incomment=0
		While pos<Len(txt)
			If incomment
				If txt[pos]=39 Or txt[pos]=10
					incomment=0
				Else
					pos:+1
				EndIf
			Else
				cmd$=Chr(txt[pos])
				pos:+1
				If cmd=" " Or cmd="~t" Or cmd="~n" Or cmd="'"
				Else
					compresscode cmd
				EndIf
				Select cmd
				Case "s"
					n=getnumber()
					compresscode String(n)
					subs[n]=pos
					debugo "sub "+String(n)+" at "+String(pos)
				Case "'"
					incomment=1
				End Select
			EndIf
		Wend
		pos=0
	End Method
	
	Method go()
		cmd$=Chr(txt[pos])
		pos:+1
		debugo cmd
		Select cmd
		Case "w" 'write
			debugo "write"
			n=getnumber()
			curcell.setvalue n
		Case "d" 'duplicate
			debugo "duplicate"
			v=curcell.value
			debugo v
			move()
			curcell.setvalue v
		Case "m" 'move
			debugo "move"
			move()
		Case "s" 'sub
			debugo "sub"
			n=getnumber()
			subs[n]=pos
		Case "g" 'goto
			debugo "goto"
			n=getnumber()
			repos subs[n]
		Case "t" 'goto with backtrace
			debugo "goto with backtrace"
			n=getnumber()
			repos subs[n],1
		Case "r" 'return
			debugo "return"
			goback
		Case "p" 'print
			debugo "print"
			WriteStdout curcell.value
		Case "e" 'end
			debugo "end"
			pos=Len(txt)
		Case "+" 'add
			debugo "add"
			v=curcell.value
			debugo v
			move()
			curcell.setvalue curcell.value +v
		Case "-" 'subtract
			debugo "subtract"
			v=curcell.value
			move()
			curcell.setvalue curcell.value -v
		Case "*" 'multiply
			debugo "multiply"
			v=curcell.value
			move()
			curcell.setvalue curcell.value *v
		Case "/" 'divide
			debugo "divide"
			v=curcell.value
			move()
			curcell.setvalue curcell.value /v
		Case "%"
			debugo "modulo"
			v=curcell.value
			move()
			curcell.setvalue curcell.value Mod v
		Case "i" 'if
			n=getnumber()
			If curcell.value>0
				debugo "true: going to "+String(n)+" which is at "+String(subs[n])
				repos subs[n]
			EndIf
		Case "n" 'not
			If curcell.value
				curcell.setvalue 0
			Else
				curcell.setvalue 1
			EndIf
		Case "'" 'comment
			debugo "comment"
			cmt$=""
			While txt[pos]<>39 And txt[pos]<>10 And pos<Len(txt)
				cmt:+Chr(txt[pos])
				pos:+1
			Wend
			debugo cmt
		Case "!"	'start/stop debugging
			debugging=1-debugging
		Case "~q"	'print string
			debugo "print"
			outstr$=getstring()
			WriteStdout outstr
		End Select
		If pos>=Len(txt)
			Return 1
		EndIf
	End Method
	
	Method repos(n,r=0)
		If r
			backtrace[tracesize]=pos
			tracesize:+1
		EndIf
		pos=n
	End Method	
	
	Method goback()
		tracesize:-1
		pos=backtrace[tracesize]
	End Method	
	
	Method getnumber()
		If txt[pos]=46
			debugo "number current cell value = "+String(curcell.value)
			Return curcell.value
		EndIf 
		n=0
		While txt[pos]>=48 And txt[pos]<=57
			n=n*10+txt[pos]-48
			pos:+1
		Wend
		debugo "number "+String(n)
		Return n
	End Method
	
	Method getstring$()
		outstr$=""
		While txt[pos]<>34
			outstr:+Chr(txt[pos])
			pos:+1
		Wend
		pos:+1
		Return outstr
	End Method
		
	Method move()
		Select txt[pos]
		Case 94 '^ - up
			dir=0
			debugo "up"
		Case 118 'v - down
			dir=2
			debugo "down"
		Case 60 '< - left
			dir=1
			debugo "left"
		Case 62 '> - right
			dir=3
			debugo "right"
		End Select
		curcell=curcell.getneighbour(dir)
		pos:+1
	End Method
End Type

Global f:TStream=WriteFile("out.txt")

Function debugo(txt$)
	If debugging
		Print txt
	EndIf
	WriteLine f,txt
End Function

inp$=AppArgs[1]
If FileType(inp)=1
	f:TStream=ReadFile(inp)
	code$=""
	While Not f.Eof()
		code:+f.ReadLine()+"~n"
	Wend
Else
	code$=inp
EndIf

Global debugging

m:machine=machine.Create(code)
If FileType(inp)=1
	f:TStream=WriteFile("compressed."+inp)
Else
	f:TStream=WriteFile("compressed.fiftween")
EndIf
f.WriteLine m.compressedtxt
f.close

While 1
	If m.go()
		End
	EndIf
	If debugging
		i$=Input()
		If i="e" Then End
	EndIf
	'Delay 1000
Wend