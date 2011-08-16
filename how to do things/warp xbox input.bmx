Graphics 800,800,0

cornerR#=Sqr(1+Cos(30)*Cos(30))/1.17

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	x#=JoyX(0)
	y#=JoyY(0)
	r#=Sqr(x*x+y*y)
	an#=ATan2(y,x)
	
	If (an>-30 And an<30) Or Abs(an)>150
		nr#=Sqr(1+y*y)
		x:*cornerr/nr
		y:*cornerr/nr
	ElseIf (an>-120 And an<-60) Or (an>60 And an<120)
		nr#=Sqr(1+x*x)
		x:*cornerr/nr
		y:*cornerr/nr
	EndIf
	


	SetColor 0,0,0
	DrawRect 0,0,400,45
	SetColor 255,255,255
	DrawText x,0,0
	DrawText y,0,15
	DrawText r,200,0
	DrawText an,200,15
	DrawText Tan(an),200,30
	
	DrawRect x*200+400,y*200+400,1,1
	
	
	
	Flip
Wend
