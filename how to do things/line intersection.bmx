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


'hard version
Type line
	Field x# , y# 'start point
	Field ex# , ey# 'end point
	Field dx# , dy# 'unit vector along line
	Field length# , an# 'length and direction of line segment
	Field nx#,ny# 'unit normal to line (a vector perpendicular to the line, of length 1)
	
	Function Create:line(x1# , y1# , x2# , y2#) 'create a line between (x1,y1) and (x2,y2)
		l:line = New line
		l.x = x1
		l.y = y1
		dx# = x2 - x1
		dy# = y2 - y1
		l.length=Sqr(dx*dx+dy*dy)
		l.dx=dx/l.length
		l.dy=dy/l.length
		l.an=ATan2(dy,dx)
		l.ex = x1+l.dx*l.length
		l.ey = y1 + l.dy * l.length
		l.nx = Cos(l.an - 90)
		l.ny=Sin(l.an-90)
		
		Return l
	End Function

	
	Method intersect#(l:line,fit=2) ' returns point on line where it intersects given line, or -1 if no intersection (on segment). fit=2 requires intersection point to be on second line segment, fit=1 just requires it to be on second line
		If l.dx = 0
			mu# = (l.x - x) / dx
		Else
			mu# = (l.y - y + (l.dy / l.dx) * (x - l.x) ) / (dy - l.dy * dx / l.dx)
		EndIf
		'DrawRect x + 400 + dx * mu , y + 400 + dy * mu , 5 , 5
		
		If fit = 0
			Return mu
		EndIf
		
		If l.dx=0
			lambda# = (y - l.y + mu * dy) / l.dy
		Else
			lambda# = (x - l.x + mu * dx) / l.dx
		EndIf
		
		'DrawText lambda,x+400+mu*dx,y+400+mu*dy
		If mu >= 0 And mu <= length
			Select fit
			Case 2
				If lambda <= l.length And lambda >= 0
					hit = mu
				Else
					hit = - 1
				EndIf
			Case 1
				If lambda >= 0
					hit = mu
				Else
					hit = - 1
				EndIf
			End Select
		Else
			hit = - 1
		EndIf
		Return hit
	End Method
	
	Method pointintersect#(px# , py#) 'returns point on line where normal to line contains given point
		pdx# = nx*5
		pdy# = ny*5
		l:line = line.Create(px , py , px + pdx , py + pdy)
		'l.draw(400 , 400)
		mu# = intersect(l , 1)
		Return mu
	End Method
	
	Method pointdistance#(px#,py#) 'returns (perpendicular) distance from line to point
		pdx# = nx*5
		pdy# = ny*5
		l:line = line.Create(px , py , px + pdx , py + pdy)
		'l.draw(400 , 400)
		mu# = l.intersect(Self , 0)
		Return mu
	End Method	
End Type
