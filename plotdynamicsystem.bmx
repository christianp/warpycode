Function nicenumber$(n#,sf)
	'Return String(n)
	If n=0 Return "0"
	If n<0
		start$="-"
		n=-n
	Else
		start$=""
	EndIf
	intbit=Int(n)
	n:-intbit
	middle$="."
	While n<.1 And Len(middle)<sf+1
		n:*10
		middle:+"0"
	Wend
	sfbit=Int(n*(10^sf))
	Return start+String(intbit)+middle+String(sfbit)
End Function

Graphics 800,800,0

centrex#=.5
centrey#=.5
rangex#=1.2
rangey#=1.2

steps#=40
stepsize#=800.0/steps

SetColor 50,50,50
yaxis#=-800*(centrex/rangex-.5)
If yaxis>0 And yaxis<800
	DrawLine yaxis,0,yaxis,800
	For cy#=0 To steps Step 2
		y#=(cy/steps-.5)*rangey+centrey
		DrawText nicenumber(y,2),yaxis,800-cy*stepsize
		DrawLine yaxis-5,800-cy*stepsize,yaxis+5,800-cy*stepsize
	Next
EndIf
xaxis#=800*(.5+centrey/rangey)
If xaxis>0 And xaxis<800
	DrawLine 0,xaxis,8000,xaxis
	For cx#=0 To steps Step 2
		x#=(cx/steps-.5)*rangex+centrex
		DrawText nicenumber(x,2),cx*stepsize,xaxis
		DrawLine cx*stepsize,xaxis-5,cx*stepsize,xaxis+5
	Next
EndIf

SetColor 255,255,255

For cx#=0 To steps
For cy#=0 To steps
	x#=(cx/steps-.5)*rangex+centrex
	y#=(cy/steps-.5)*rangey+centrey
	dx#=x*(1-x-y)
	dy#=y*(.75-y-x*.5)
	
	an#=ATan2(dy,dx)
	s#=Sin(an)
	c#=Cos(an)
	x2#=c*stepsize*.33
	y2#=s*stepsize*.33
	x1#=-x2
	y1#=-y2
	px=cx*stepsize
	py=cy*stepsize
	startx#=px+x1
	starty#=800-(py+y1)
	endx#=px+x2
	endy#=800-(py+y2)
	DrawLine startx,starty,endx,endy
	DrawLine endx,endy,endx+stepsize*.2*Sin(an+240),endy+stepsize*.2*Cos(an+240)
	DrawLine endx,endy,endx+stepsize*.2*Sin(an+300),endy+stepsize*.2*Cos(an+300)
Next
Next

Flip
WaitKey()
End