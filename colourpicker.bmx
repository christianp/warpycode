Graphics 800,800

Function redness#(an#,r#)
	If r>1 Then r=1
	c#=Cos(an-90)
	If c<0 c=0
	Return 255*(r*c+(1-r)*(1-c))
End Function

Function greenness#(an#,r#)
	If r>1 Then r=1
	c#=Cos(an+30)
	If c<0 c=0
	Return 255*(r*c+(1-r)*(1-c))
End Function

Function blueness#(an#,r#)
	If r>1 Then r=1
	c#=Cos(an+150)
	If c<0 c=0
	Return 255*(r*c+(1-r)*(1-c))
End Function

While Not KeyHit(KEY_ESCAPE)
	For r#=.1 To 1 Step .1
	For an=0 To 350 Step 10
		SetColor redness(an,r),greenness(an,r),blueness(an,r)
		x#=400+Cos(an)*200*r
		y#=400+Sin(an)*200*r
		s#=r*10+10
		DrawOval x-s,y-s,2*s,2*s
	Next
	Next
	
	mx=MouseX()
	my=MouseY()
	an=ATan2(my-400,mx-400)
	dx#=mx-400
	dy#=my-400
	r#=Sqr(dx*dx+dy*dy)/200.0
	red#=redness(an,r)
	green#=greenness(an,r)
	blue#=blueness(an,r)
	SetColor red,green,blue
	DrawOval 350,350,100,100
	SetColor 255,255,255
	DrawText red,0,0
	DrawText green,0,15
	DrawText blue,0,30
	DrawText r,0,45
	
	Flip
	Cls
Wend

'r*c+(1-r)*(1-c)=rc+1-r-c+rc