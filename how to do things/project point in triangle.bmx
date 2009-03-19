'Project vector v onto lines l1 and l2
'set v=A*l1+B*l2
'solve simultaneous equation

Graphics 600, 600

dx1:Float = 100
dy1:Float = 30
dx2:Float = 30
dy2:Float = -80

an = 0
While Not KeyHit(KEY_ESCAPE) 

	an:+1
	dx2 = 100 * Cos(an) 
	dy2 = 100 * Sin(an) 

	x = MouseX() - 400
	y = MouseY() - 400
	
	If dy2 = 0
		Print "!!"
		b:Float = (x - y * dx1 / dy1) / (dx2 - dx1 * dy2 / dy1) 
		If dx1 = 0
			a:Float = (y - b * dy2) / dy1
		Else
			a:Float = (x - b * dx2) / dx1
		End If
	Else
		a:Float = (x - y * (dx2 / dy2)) / (dx1 - dy1 * dx2 / dy2) 
		If dx2 = 0
			b:Float = (y - a * dy1) / dy2
		Else
			b:Float = (x - a * dx1) / dx2
		EndIf
	End If
	
	DrawLine 400, 400, 400 + a * dx1 + b * dx2, 400 + a * dy1 + b * dy2
	DrawRect 400 + a * dx1, 400 + a * dy1, 3, 3
	DrawRect 400 + b * dx2, 400 + b * dy2, 3, 3
	
	DrawLine 400, 400, 400 + dx1, 400 + dy1
	DrawLine 400, 400, 400 + dx2, 400 + dy2
	
	Flip
	Cls
WEnd