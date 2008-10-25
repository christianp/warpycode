Const players = 3
Global picks[players+1,2]
Global picked[players + 1]
Global colours[players + 1,3]
colours[3 , 1] = 255
colours[2 , 0] = 255
colours[1 , 2] = 255

For i = 0 To players
	picks[i , 0] = i
	picked[i]=i
Next

AppTitle = "The make your bits the biggest game"
Graphics 600 , 600 , 0
SetClsColor 255 , 255 , 255
SetBlend ALPHABLEND

Global pickwidth#=500/(players+1)

Global state = 0
Global yscroll=0
go = 1
While Not KeyHit(KEY_ESCAPE)
	SetColor 0,0,0
	DrawText state,0,0
	Select state
	Case 0 'starting turn
		state = 1
		For i = 0 To players 'reset top row
			picks[i , 1] = -1
		Next
		pf = Rand(0 , players - 1)
		If pf >= picked[0] pf:+ 1
		picks[pf , 1] = 0
		picked[0]=pf
	Case 1 'taking go
		mx# = MouseX() - 50
		pos = Int(mx / pickwidth + .5)
		DrawText pos , 0 , 15
		DrawText picked[go],0,30
		If Not (picks[pos , 1]=-1 Or pos = picked[go]) 'can pick this spot, because it hasn't already been picked and it isn't straight up from our last pick
			'''draw line
			x = 50 + pickwidth * pos
			y = 50 + yscroll
			SetColor colours[go,0],colours[go,1],colours[go,2]
			DrawOval x-10,y-10,20,20
			If MouseHit(1)
				picks[pos , 1] = go
				picked[go] = pos
				go:+ 1
				If go = players+1 state = 2
			EndIf
		EndIf
	Case 2 'fill in last one
		'For i = 0 To players
		'	If picks[i , 1] = 0
		'		picked[0] = i
		'	EndIf
		'Next
		
		'state = 3
		If MouseHit(1)
			state = 3
		EndIf
	Case 3 'scrolling down
		yscroll:+ 3
		If yscroll >= 500
			For i = 0 To players
				picks[i , 0] = picks[i , 1]
			Next
			state = 0
			yscroll = 0
			go = 1
		EndIf
				
	End Select
	
	For i = 0 To players
		p1 = picks[i , 0]
		SetColor colours[p1 , 0] , colours[p1 , 1] , colours[p1 , 2]
		x = 50 + pickwidth * i
		y=550+yscroll
		DrawOval x - 5 , y - 5 , 10 , 10
		
		If go>p1
		x2 = 50 + pickwidth * picked[p1]
		y2 = 50 + yscroll
		DrawLine x,y,x2,y2
		EndIf
		
		p2 = picks[i , 1]
		SetColor colours[p2 , 0] , colours[p2 , 1] , colours[p2 , 2]
		y=50+yscroll
		DrawOval x - 5 , y - 5 , 10 , 10
	Next
	
	Flip
	Cls
Wend