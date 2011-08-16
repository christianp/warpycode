'

Graphics 800,800,0
w# = 400
h# = 50
r# = (w * w - h * h) / (2 * h) + h
theta# = ASin(w / r)
x# = 400
y# = 400 + r - h/2

an# = - theta
ox# = 0
oy# = 400+h/2
While an <= theta
	px# = x + Cos(an - 90) * r
	py# = y + Sin(an - 90) * r
	Print px
	Print py
	DrawLine ox , oy , px , py
	ox = px
	oy = py
	If an = theta
		an:+ 1
	Else
		an:+ 1
		If an > theta an = theta
	EndIf
Wend

DrawText r , 0 , 0
DrawText theta , 0 , 13

Flip
WaitKey()