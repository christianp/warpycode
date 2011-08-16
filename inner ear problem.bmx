Const screenwidth=800, screenheight=800
Const blocksize=50

Global scrollx#,scrolly#

Global blocks:tlist=New tlist
Type block
	Field x , y
	
	Method draw()
		SetAlpha .5
		drawscrollrect (x - .5) * blocksize , (y - .5) * blocksize , (x + .5) * blocksize , (y + .5) * blocksize)
		SetAlpha 1
	End Method
End Type

Type cursor
	Field x,y
	Field turn
	
	'isomorphism z4 -> turns:
	'0: l
	'1: u
	'2: r
	'3: d
	
	'group action z4 -> moves:
	'0: u
	'1: l
	'2: d
	'3: r
	
	Method New()
		turn = 1
	End Method
	
	Method go(ping)
		movedir = (turntomove(turn) + turntomove(ping) ) Mod 4
		Select movedir
		Case 0 'up
			y:- 1
		Case 1 'left
			x:- 1
		Case 2 'down
			y:+ 1
		Case 3 'right
			x:+ 1
			
		turn = (turn + ping) Mod 4
		
	End Method
	
	Method turntomove(n)
		Select n
		Case 0 'l -> 1
			Return 1
		Case 1 'u -> 0
			Return 0
		Case 2 'r -> 3
			Return 3
		Case 3 'd -> 2
			Return 2
		End Select
	End Method
	
	Method draw()
		px# = x * blocksize
		py# = y * blocksize
		ey# = py + .5 * blocksize
		DrawscrollLine px , py - .5 * blocksize , px , ey
		DrawscrollLine px , ey , px - .3 * blocksize , ey - .3 * blocksize
		DrawscrollLine px , ey , px + .3 * blocksize , ey - .3 * blocksize
	End Method
	
End Type

Function drawscrollline(x1 , y1 , x2 , y2)
	x1:+ screenwidth / 2 - scrollx
	y1:+ screenheight / 2 - scrolly
	x2:+ screenwidth / 2 - scrollx
	y2:+ screenheight / 2 - scrolly
	
	DrawLine x1,y1,x2,y2
End Function

Graphics screenwidth , screenheight , 0
SetBlend ALPHABLEND

cur:cursor = New cursor


While Not KeyHit(KEY_ESCAPE)
	
	If MouseHit(key_up)
		cur.go(1)
	ElseIf MouseHit(key_down)
		cur.go(3)
	ElseIf MouseHit(key_left)
		cur.go(0)
	ElseIf MouseHit(key_right)
		cur.go(2) 
	EndIf
	
	cur.draw()
Wend