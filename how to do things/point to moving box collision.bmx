Function boxoverpoint(px#,py#,bx1#,by1#,bx2#,by2#,bw#,bh#)	'tells you if a point is inside the area covered by a box moving from bx1,by1 to bx2,by2

	'construct a six-sided polygon representing the sweep of the box
	Local poly#[12]
	If bx2<bx1
		If by2<by1	'box moves up-left
			poly=[bx2, by2, bx2+bw, by2, bx1+bw, by1, bx1+bw, by1+bh, bx1, by1+bh, bx2, by2+bh]
		Else		'box moves down-left
			poly=[bx1, by1, bx1+bw, by1, bx1+bw, by1+bh, bx2+bw, by2+bh, bx2, by2+bh, bx2, by2]
		EndIf
	Else
		If by2<by1	'box moves up-right
			poly=[bx1, by1, bx2, by2, bx2+bw, by2, bx2+bw, by2+bh, bx1+bw, by1+bh, bx1, by1+bh] 
		Else		'box moves down-right
			poly=[bx1, by1, bx1+bw, by1, bx2+bw, by2, bx2+bw, by2+bh, bx2, by2+bh, bx1, by1+bh]
		EndIf
	EndIf
	
	'draw the boxes, the poly and the point - to show it works!
	SetAlpha .2
	DrawRect bx1,by1,bw,bh
	DrawRect bx2,by2,bw,bh
	DrawPoly poly
	
	SetAlpha 1
	DrawOval px-3,py-3,6,6


	'check each triangle round the polygon to see if the point is inside
	For n=1 To 4
		If pointintriangle(px,py,poly[0],poly[1],poly[n*2],poly[n*2+1],poly[n*2+2],poly[n*2+3])
			Return True
		EndIf
	Next

End Function

'returns True if p1 and p2 are on the same side of the line a->b
Function sameside(p1x#,p1y#,p2x#,p2y#,ax#,ay#,bx#,by#)
	cp1# = (bx-ax)*(p1y-ay)-(p1x-ax)*(by-ay)
	cp2# = (bx-ax)*(p2y-ay)-(p2x-ax)*(by-ay)
	If cp1*cp2 >= 0 Then Return True
End Function	
	
'Clever little trick for telling if a point is inside a given triangle
'If for each pair of points AB in the triangle, P is on the same side of AB as 
'the other point in the triangle, then P is in the triangle. 
Function pointintriangle(px#,py#,ax#,ay#,bx#,by#,cx#,cy#)
	If sameside(px,py,ax,ay,bx,by,cx,cy) And sameside(px,py,bx,by,ax,ay,cx,cy) And sameside(px,py,cx,cy,ax,ay,bx,by)
		Return True
	Else
		Return False
	EndIf
End Function

Graphics 600,600,0
SetBlend ALPHABLEND

SeedRnd MilliSecs()
px#=Rand(600)
py#=Rand(600)
pan#=Rand(360)
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	pan:+Rand(-5,5)
	px:+Cos(pan)
	py:+Sin(pan)
	If px<0 px:+600
	If px>600 px:-600
	If py<0 py:+600
	If py>600 py:-600

	If boxoverpoint(px,py, 300,300, MouseX(),MouseY(), 100, 60)
		DrawText "yes!",0,0
	EndIf

	Flip
	Cls
Wend