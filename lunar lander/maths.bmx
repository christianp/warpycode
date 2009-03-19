Function poisson(lambda!)
	If lambda>500 Return poisson(lambda/2)+poisson(lambda/2)
	k=0
	u!=Rnd(0,1)
	fact=1
	p!=Exp(-lambda)
	u:-p
	While u>0
		k:+1
		fact:*k
		p:*lambda/k
		u:-p
	Wend
	Return k
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

Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

Function inarc(an,an1#,an2#)
	'If an1>180 Then an1:-360
	'If an2<180 Then an2:+360
	d1#=andiff(an2,an1)
	If d1=0 Then d1=360
	If d1>0
		d2#=andiff(an,an1)
		d3#=-andiff(an,an2)
		If (d2>=0 And d2<=d1) Or (d3>=0 And d3<=d1)
			Return 1
		Else
		EndIf
	Else
		Return 1-inarc(an,an2,an1)
	EndIf
End Function

'simple version
Function linesintersect#(ax#,ay#,bx#,by#,cx#,cy#,dx#,dy#,fit=0)
	'fit, bitmask, set:
	' 1: doesn't need to be on first segment
	' 2: doesn't need to be on second segment
	bx:-ax
	by:-ay
	dx:-cx
	dy:-cy
	
	If dx<>0
		lambda#=(cy-ay+(ax-cx)*dy/dx)/(by-bx*dy/dx)
	Else
		lambda#=(cx-ax+(ay-cy)*dx/dy)/(bx-by*dx/dy)
	EndIf
	If bx<>0
		mu#=(ay-cy+(cx-ax)*by/bx)/(dy-dx*by/bx)
	Else
		mu#=(ax-cx+(cy-ay)*bx/by)/(dx-dy*bx/by)
	EndIf
	
	Rem
	Print String(ax)+"  ,  "+String(ay)
	Print String(bx)+"  ,  "+String(by)
	Print String(cx)+"  ,  "+String(cy)
	Print String(dx)+"  ,  "+String(dy)
	Print lambda
	Print mu
	WaitKey
	EndRem
	If (lambda#>=0 And lambda<=1) Or (fit & 1)
	 If (mu#>=0 And mu<=1) Or (fit & 2)
		Return lambda
	 EndIf
	EndIf
	Return -1
End Function

Function pointlinedistance#(px#,py#,ax#,ay#,bx#,by#)
	dx#=bx-ax
	dy#=by-ay
	an#=ATan2(dy,dx)
	nx#=Cos(an+90)
	ny#=Sin(an+90)
	lambda#=(py-ay+(ax-px)*dy/dx)/(nx*dy/dx-ny)
	Return lambda#
End Function