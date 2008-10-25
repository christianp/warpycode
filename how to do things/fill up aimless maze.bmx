SeedRnd MilliSecs()
xsize = 8
ysize=xsize
Local cells:Int[xsize, ysize , 5]

Function fillcell(cells:Int[,,] , x , y)
	Local dim:Int[]
	dim = cells.dimensions()
	xsize = dim[0]
	ysize=dim[1]
	cells[x , y , 0] = 1
	If x > 0
		If Not cells[x - 1 , y , 0]
			If Rand(3) = 1
				cells[x , y , 4] = 1
				cells[x - 1 , y , 2] = 1
				fillcell(cells, x - 1 , y)
			EndIf
		EndIf
	EndIf
	If x < xsize - 1
		If Not cells[x + 1 , y , 0]
			If Rand(3) = 1
				cells[x , y , 2] = 1
				cells[x + 1 , y, 4] = 1
				fillcell(cells, x + 1 , y)
			EndIf
		EndIf
	EndIf
	
	If y > 0
		If Not cells[x , y - 1 , 0]
			If Rand(3)=1
				cells[x , y , 1] = 1
				cells[x , y - 1 , 3] = 1
				fillcell(cells, x , y - 1)
			EndIf
		EndIf
	EndIf
	
	If y < ysize - 1
		If Not cells[x , y + 1 , 0]
			If Rand(3) = 1
				cells[x , y , 3] = 1
				cells[x , y + 1 , 1] = 1
				fillcell(cells, x , y + 1)
			EndIf
		EndIf
	EndIf

	If x > 0
		If Not cells[x - 1 , y , 0]
				cells[x , y , 4] = 1
				cells[x - 1 , y , 2] = 1
				fillcell(cells, x - 1 , y)
		EndIf
	EndIf
	If x < xsize - 1
		If Not cells[x + 1 , y , 0]
				cells[x , y , 2] = 1
				cells[x + 1 , y, 4] = 1
				fillcell(cells, x + 1 , y)
		EndIf
	EndIf
	
	If y > 0
		If Not cells[x , y - 1 , 0]
				cells[x , y , 1] = 1
				cells[x , y - 1 , 3] = 1
				fillcell(cells, x , y - 1)
		EndIf
	EndIf
	
	If y < ysize - 1
		If Not cells[x , y + 1 , 0]
				cells[x , y , 3] = 1
				cells[x , y + 1 , 1] = 1
				fillcell(cells, x , y + 1)
		EndIf
	EndIf
End Function


Graphics xsize*50,ysize*50

While Not KeyHit(KEY_ESCAPE)
	fillcell(cells,xsize-1 , 0)
	
	For x = 0 To xsize - 1
		For y = 0 To ysize - 1
			If cells[x , y , 0]
				SetColor 100,100,100
				DrawRect x * 50 , y * 50 , 50 , 50
			EndIf
			SetColor 255,255,255
			If cells[x , y , 1]
				DrawLine x * 50 + 25 , y * 50 + 25 , x * 50 + 25 , y * 50 - 25
			EndIf
			If cells[x , y , 2]
				DrawLine x * 50 + 25 , y * 50 + 25 , x * 50 + 75 , y * 50 + 25
			EndIf
			If cells[x , y , 3]
				DrawLine x * 50 + 25 , y * 50 + 25 , x * 50 + 25 , y * 50 + 75
			EndIf
			If cells[x , y , 4]
				DrawLine x * 50 + 25 , y * 50 + 25 , x * 50 - 25 , y * 50 + 25
			EndIf
		Next
	Next
	Flip
	For x = 0 To xsize - 1
		For y = 0 To ysize - 1
			For i = 0 To 4
				cells[x , y , i] = 0
			Next
		Next
	Next
	WaitKey()
Wend