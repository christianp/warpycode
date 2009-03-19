Const gwidth = 800 , gheight = 800

Type pip
	Field links[4] '0 up 1 right 2 down 3 left
	Field fade#
	Field hitdown,hitright
	
	Method New()
		For i = 0 To 3
			links[i] = Rand(0 , 1)
		Next
	End Method
	
	Method draw(x , y)
		SetAlpha fade
		cx# = (x + .5) * pipsize
		cy# = (y + .5) * pipsize
		DrawOval cx - pipsize / 4 , cy - pipsize / 4 , pipsize / 2 , pipsize / 2
		If links[0] DrawLine cx , cy , cx , cy - pipsize / 2
		If links[1] DrawLine cx , cy , cx + pipsize / 2 , cy
		If links[2] DrawLine cx , cy , cx , cy + pipsize / 2
		If links[3] DrawLine cx , cy , cx + pipsize / 2 , cy
	End Method
		
	
End Type

Function scanpip(x , y , dir , n=0)
	If x = bwidth Return 0
	If y = bheight Return 0
	
	Select dir
	Case 1 'right
		If n>0 And Not board[x,y].links[3] Return 0
		If board[x , y].links[1]
			Return scanpip(x + 1 , y , dir , n + 1) + 1
		EndIf
	Case 2 'down
		If n>0 And Not board[x,y].links[0] Return 0
		If board[x , y].links[2]
			Return scanpip(x , y + 1 , dir , n + 1) + 1
		EndIf
	End Select
	Return 1
End Function

Function fact(n)
	If n = 1 Return 1
	Return n * fact(n - 1)
End Function

Global bwidth=10,bheight=10
Global board:pip[bwidth , bheight]
Global hitboard[bwidth,bheight,2]

Global pipsize#

If gwidth/bwidth > gheight/bheight
	pipsize = gheight / bheight
Else
	pipsize = gwidth / bwidth
EndIf

For x = 0 To bwidth - 1
	For y = 0 To bheight - 1
		board[x , y] = New pip
	Next
Next


Graphics gwidth , gheight +50, 0
SetBlend ALPHABLEND

Global points = 0
Global selx = - 1 , sely = - 1
Global gotime=0
While Not KeyHit(KEY_ESCAPE) 
	DrawText points,0,gheight
	
	If selx >= 0
		SetAlpha .5
		DrawRect selx * pipsize , sely*pipsize,pipsize,pipsize
		SetAlpha 1
	EndIf
	
	For x = 0 To bwidth - 1
		For y = 0 To bheight - 1
			If board[x , y].hitdown + board[x , y].hitright
				board[x , y].fade:- .05
				If board[x , y].fade <= 0
					board[x , y] = New pip
				EndIf
				SetColor 255,0,0
			Else
				If board[x , y].fade < 1 board[x , y].fade:+ .1
				SetColor 0,0,255
			EndIf
			
			board[x , y].draw(x , y)
		Next
	Next
	SetColor 255,255,255
	SetAlpha 1
	
	chains = 0
	For x = 0 To bwidth - 1 'not -3 in case I change it so small chains are allowed
		For y = 0 To bheight - 1
			hpips = scanpip(x , y , 1)
			vpips = scanpip(x , y , 2)
			DrawText hpips , (x+.75) * pipsize,(y+.5)*pipsize
			DrawText vpips,(x+.5)*pipsize,(y+.75)*pipsize
			If hpips>=3 And board[x,y].hitright=0
				For i=0 To hpips-1
					board[x + i , y].hitright = 1
				Next
				chains:+ 1
				score:+ fact(hpips) * chains
				'Print "right "+String(hpips)+" "+String(x)+","+String(y)
			EndIf
			If vpips >= 3 And board[x , y].hitdown = 0
				For i=0 To vpips-1
					board[x , y + i].hitdown = 1
				Next
				chains:+1
				score:+ fact(vpips) * chains
				'Print "down "+String(vpips)+" "+String(x)+","+String(y)
			EndIf
		Next
	Next
	
	mx = MouseX()
	my = MouseY()
	bx = Int(mx / pipsize)
	by = Int(my / pipsize)
	If bx < bwidth And by < bheight
		If MouseHit(1) And board[bx,by].hitdown+board[bx,by].hitright=0
			If selx = - 1
				selx = bx
				sely = by
			Else
				p:pip = board[selx , sely]
				board[selx , sely] = board[bx , by]
				board[bx , by] = p
				selx = - 1
				If Not gotime
					gotime = 1
					score = 0
				EndIf
			EndIf
		EndIf
	EndIf

	If gotime	
		If score > 0
			score:- 1
			points:+ 1
		EndIf
	EndIf
	
	Flip
	Cls
Wend