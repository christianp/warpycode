Const screenwidth=200,screenheight=200

Type hub
	Field x#,y#
	Field dividers:TList
	Field red,green,blue
	
	Function create:hub(x#,y#,red,green,blue)
		h:hub=New hub
		h.x=x
		h.y=y
		h.red=red
		h.green=green
		h.blue=blue
		h.dividers=New TList
		h.dividers.addlast(line.create(0,0,Cos(0),Sin(0)))
		h.dividers.addlast(line.create(screenwidth-1,0,Cos(90),Sin(90)))
		h.dividers.addlast(line.create(screenwidth-1,screenheight-1,Cos(180),Sin(180)))
		h.dividers.addlast(line.create(0,screenheight-1,Cos(270),Sin(270)))
		For o:hub=EachIn hubs
			h.makedivider(o)
			o.makedivider(h)
		Next
		hubs.addlast h
		Return h
	End Function
	
	Method makedivider(o:hub)
		dx#=o.x-x
		dy#=o.y-y
		an#=ATan2(dy,dx)+90
		
		dividers.addlast line.create(x+dx/2,y+dy/2,Cos(an),Sin(an))
	End Method
	
	Method draw()
		closest:line=Null
		mindist=-1
		For l:line=EachIn dividers
			
			d#=Abs(l.distancefrompoint(x,y))
			If d<mindist Or mindist=-1
				closest=l
				mindist=d
			EndIf
		Next
		If closest=Null Then Return
		l:line=closest
		DrawLine l.x-200*l.vx,l.y-200*l.vy,l.x+200*l.vx,l.y+200*l.vy
	End Method
	
End Type

Type point
	Field x#,y#
	Function create:point(x#,y#)
		p:point=New point
		p.x=x
		p.y=y
		Return p
	End Function
End Type

Type line
	Field x#,y#,vx#,vy#
	
	Function create:line(x#,y#,vx#,vy#)
		l:line=New line
		l.x=x
		l.y=y
		l.vx=vx
		l.vy=vy
		Return l
	End Function
	
	Method getx#(ty#)
		Return x+vx*(ty-y)/vy
	End Method
	Method gety#(tx#)
		Return y+vy*(tx-x)/vx
	End Method
	
	Method getlambdafromx#(tx#)
		Return (tx-x)/vx
	End Method
	Method getlambdafromy#(ty#)
		Return (ty-y)/vy
	End Method
	
	Method distancefrompoint#(tx#,ty#)
		lambda#=(y-ty+(vy/vx)*(tx-x))/(vx+vy*vy/vx)
		Return lambda
	End Method
	
	Method intersectlambda#(ol:line)
		lambda#=(ol.y-y+(ol.vy/ol.vx)*(x-ol.x))/(vy-vx*ol.vy/ol.vx)
		Return lambda
	End Method
End Type

Rem
Function minmax[,](lines:TList)
	Local both[2,screenheight]
	For y=0 To screenheight-1
		both[0,y]=0
		both[1,y]=screenwidth-1
	Next
	For l:line=EachIn lines
		topx#=l.getx(0)
		bottomx#=l.getx(screenheight)
		If topx<0 Or topx>=screenwidth
			lefty#=l.gety(0)
			righty#=l.gety(screenwidth)
			If lefty>righty
				starty#=righty
				endy#=lefty
			Else
				starty=lefty
				endy=righty
			EndIf
			SetColor 255,0,0
		Else
			starty=0
			endy=screenheight
			SetColor 0,0,255
		EndIf
		For y=0 To screenheight-1
			x#=l.getx(y)
			If x>0 And x<screenwidth
				If (l.vx>0 And x>both[0,y]) Or both[0,y]=-1
					both[0,y]=x
				EndIf
				If (l.vx<0 And x<both[1,y]) Or both[1,y]=-1
					both[1,y]=x
				EndIf
			EndIf
		Next
	Next
	Return both
End Function
EndRem
Global hubs:TList=New TList

Graphics screenwidth,screenheight,0
While Not KeyHit(KEY_ESCAPE)
	mx=MouseX()
	my=MouseY()

	If MouseHit(1)
		hub.create(mx,my,Rand(10)*25,Rand(10)*25,Rand(10)*25)
	EndIf
	
	For h:hub=EachIn hubs
	'	h.draw()
	Next
	If hubs.count() Then hub(hubs.first()).draw()
	
	closest:hub=Null
	mindist=-1
	For h:hub=EachIn hubs
		SetColor 255-h.red,255-h.green,255-h.blue
		DrawOval h.x-5,h.y-5,10,10
		
		d#=(h.x-mx)*(h.x-mx)+(h.y-my)*(h.y-my)
		If d<mindist Or closest=Null
			closest=h
			mindist=d
		EndIf
	Next
	
	If closest<>Null
		SetColor 255,255,255
		For l:line=EachIn closest.dividers
			'DrawLine l.x,l.y,l.x+l.vx*50,l.y+l.vy*50
		'	DrawLine closest.x,closest.y,l.x,l.y
		Next
		If MouseHit(2)
			hubs.remove closest
		EndIf
	EndIf
	
	'FlushMem
	Flip
	Cls
Wend