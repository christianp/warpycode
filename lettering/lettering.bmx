Global strokes:TList=New TList
Type stroke
	Field points:TList
	'points are of the form [x,y,distance]
	Field length#
	Field minx#,miny#,maxx#,maxy#
	Field tx#,ty#
	
	Method New()
		points=New TList
	End Method
	
	Method addpoint(x#,y#)
		If x<minx minx=x
		If x>maxx maxx=x
		If y<miny miny=y
		If y>maxy maxy=y
		
		If points.count()
			Local opoint#[]=Float[](points.last())
			dx#=x-opoint[0]
			dy#=y-opoint[1]
			d#=Sqr(dx*dx+dy*dy)
		Else
			minx=x
			maxx=x
			miny=y
			maxy=y
			d#=0
		EndIf
		
		tx:+x
		ty:+y
		Local point#[]=[x,y,length+d]
		
		points.addlast point
		length:+d
	End Method
	
	Method draw(bx#,by#,scale#)
		Local ox#,oy#,x#,y#
		Local point#[]
		Local drawn=0
		
		For point=EachIn points
			x=point[0]*scale+bx
			y=point[1]*scale+by
			If drawn
				DrawLine ox,oy,x,y
			EndIf
			drawn=1
			ox=x
			oy=y
		Next
	End Method
	
	Method save()
		If points.count()<=1
			Return
		EndIf
		
		width#=maxx-minx
		height#=maxy-miny
		diag#=Sqr(width*width+height*height)
		meanx#=tx/points.count()
		
		Local point#[]
		For point=EachIn points
			'point[0]=(point[0]-meanx)/diag+.5
			'point[1]=(point[1]-miny)/diag
			point[2]:/length
		Next
	End Method
	
	Method getpoint#[](t#)
		Local opoint#[],point#[]
		opoint=Float[](points.first())
		For point=EachIn points
			If point[2]>=t
				p#=1-(t-opoint[2])/(point[2]-opoint[2])
				x#=opoint[0]*p + point[0]*(1-p)
				y#=opoint[1]*p + point[1]*(1-p)
				Return [x,y,t]
			EndIf
			opoint=point
		Next
		Return point
	End Method
	
	Function interpolate:stroke(ss:TList,weights#[])
		ns:stroke=New stroke
		
		tweight#=0
		For i=0 To ss.count()-1
			tweight:+weights[i]
		Next
		
		Local ox#,oy#,x#,y#
		For t#=0 To 1 Step .01
			x#=0
			y#=0
			Local point#[]
			i=0
			For s:stroke=EachIn ss
				point=s.getpoint(t)
				x:+point[0]*weights[i]/tweight
				y:+point[1]*weights[i]/tweight
				i:+1
			Next
			ns.addpoint x,y
		Next
		Return ns
	End Function
End Type

Type character
	Field strokes:TList[]
	Field offx#,offy#,scale#
	
	Method New()
		strokes=New TList[1]
		strokes[0]=New TList
		scale=1
	End Method
	
	Method addstroke(s:stroke,i)
		strokes[i].addlast s
	End Method
	
	Method morestrokes()
		strokes:+[New TList]
	End Method
	
	Method generate:TList()
		l:TList=New TList
		For i=0 To Len(strokes)-1
			l.addlast stroke.interpolate(strokes[i],evenweights(strokes[i].count()))
		Next
		Return l
	End Method
	
	Method draw()
		SetAlpha .2
		For i=0 To Len(strokes)-1
			For s:stroke=EachIn strokes[i]
				s.draw 200,200,200
			Next
		Next
		SetAlpha 1
		
		glyph.Create(200,200,200,Self).draw
	End Method
End Type

Type glyph
	Field strokes:TList
	Field x#,y#,size#
	
	Function Create:glyph(x#,y#,size#,ch:character)
		g:glyph=New glyph
		g.strokes=ch.generate()
		g.x=x+ch.offx
		g.y=y+ch.offy
		g.size=size*ch.scale
		Return g
	End Function
	
	Method draw()
		For s:stroke=EachIn strokes
			s.draw x,y,size
		Next
	End Method
End Type

Function KnuthShuffle[](n)
	Local k[n]
	For i=0 To n-1
		k[i]=i
	Next
	
	For i=0 To n-2
		j=Rand(i,n-1)
		b=k[i]
		k[i]=k[j]
		k[j]=b
	Next
	
	Return k
End Function

Function makeweights#[](n)
	Local weights#[n]
	
	c=1
	For i=EachIn knuthshuffle(n)
		weights[i]=c*Rnd(0,1)
		c:*1.1
	Next
	
	Return weights
End Function

Function evenweights#[](n)
	Local weights#[n]
	For i=0 To n-1
		weights[i]=1
	Next
	Return weights
End Function

Global curtool:tool
Type tool
	Field prvtool:tool

	Function change(t:tool)
		t.prvtool=curtool
		curtool=t
	End Function

	Method update() Abstract
	
	Method draw() Abstract
	
	Method back()
		curtool=prvtool
	End Method
	
	Method label$() Abstract
End Type

Type defaulttool Extends tool
	Field curchr:character
	Field curkey$
	Field chri
	
	Method New()
		changekey("a")
	End Method
	
	Method label$()
		key$=curkey+" ("+Len(curchr.strokes)+" strokes)"
		Return "Editing: "+key+"~n~nLeft-click To draw a stroke~nSpace to increase no. of strokes"
	End Method
	
	Method update()
		c=GetChar()
		If c>32
			changekey(Chr(c))
		EndIf
		
		If MouseDown(1)
			tool.change(New drawtool)
		EndIf
		
		If KeyHit(KEY_SPACE)
			curchr.morestrokes
		EndIf
		
		If KeyHit(KEY_BACKSPACE)
			undo
		EndIf
		
		If KeyHit(KEY_ENTER)
			tool.change aligntool.Create(curchr)
		EndIf
	End Method
	
	Method undo()
		chri:-1
		If chri<0 chri=Len(curchr.strokes)-1
		If curchr.strokes[chri].count()
			curchr.strokes[chri].removelast
		EndIf
	End Method
	
	Method changekey(c$)
		curkey=c
		If dict.contains(curkey)
			curchr=character(dict.valueforkey(curkey))
		Else
			curchr=New character
			dict.insert curkey,curchr
		EndIf
	End Method	
	
	Method draw()
		curchr.draw
	End Method
End Type

Type drawtool Extends tool
	Field s:stroke
	
	Method label$()
		Return "Drawing a stroke"
	End Method
	
	Method New()
		s:stroke=New stroke
	End Method
	
	Method update()
		x#=(mx-200)/200.0
		y#=(my-200)/200.0
		s.addpoint x,y
		
		If Not MouseDown(1)
			back
		EndIf
	End Method
	
	Method back()
		Super.back
		s.save
		dt:defaulttool=defaulttool(curtool)
		dt.curchr.strokes[dt.chri].addlast s
		dt.chri=(dt.chri+1) Mod Len(dt.curchr.strokes)
	End Method	

	Method draw()
		SetColor 255,255,255
		s.draw 200,200,200
	End Method
End Type

Type aligntool Extends tool
	Field curchr:character
	Function Create:aligntool( ch:character )
		a:aligntool=New aligntool
		a.curchr=ch
		Return a
	End Function
	
	Method update()
		If MouseDown(1)
			tool.change movetool.Create(curchr, mx, my)
		EndIf
		If MouseDown(2)
			tool.change scaletool.Create(curchr, mx, my)
		EndIf
		If KeyHit(KEY_ENTER)
			back
		EndIf
	End Method
	
	Method label$()
		Return "Aligning character.~n~nLeft click and drag to move~nRight click and drag to scale"
	End Method
	
	Method draw()
		curchr.draw
	End Method
End Type

Type movetool Extends tool
	Field curchr:character
	Field holdx,holdy,ox#,oy#
	Function Create:movetool( ch:character,x,y )
		m:movetool=New movetool
		m.curchr=ch
		m.holdx=x
		m.holdy=y
		m.ox=ch.offx
		m.oy=ch.offy
		Return m
	End Function
	
	Method update()
		curchr.offx=ox+mx-holdx
		curchr.offy=oy+my-holdy
		If Not MouseDown(1)
			back
		EndIf
	End Method
	
	Method label$()
		Return "Moving character"
	End Method
	
	Method draw()
		curchr.draw
	End Method
End Type

Type scaletool Extends tool
	Field curchr:character
	Field od#,oscale#
	Function Create:scaletool( ch:character,x,y )
		s:scaletool=New scaletool
		s.curchr=ch
		s.od=dist(x,y,ch.offx,ch.offy)
		s.oscale=ch.scale
		Return s
	End Function
	
	Method update()
		d#=dist(mx,my,curchr.offx,curchr.offy)
		curchr.scale=oscale*d/od
		If Not MouseDown(2)
			back
		EndIf
	End Method
	
	Method label$()
		Return "Scaling character"
	End Method
	
	Method draw()
		curchr.draw
	End Method
End Type
AppTitle="Letter"
Graphics 600,600,0
SetBlend ALPHABLEND

Global dict:tmap=New tmap

Global mx,my

curtool=New defaulttool

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())

	mx=MouseX()
	my=MouseY()
	
	curtool.update
	
	SetColor 0,0,100
	DrawLine 200,200,400,200
	DrawLine 200,200,200,400
	DrawLine 200,400,400,400
	DrawLine 400,200,400,400
	DrawLine 300,200,300,400
	DrawLine 200,300,400,300
	SetColor 255,255,255
	
	curtool.draw
	
	drawtextlines curtool.label(),0,0

	Flip
	Cls
Wend


Function drawtextlines$(txt$,x,y)
	Local lines$[]=txt.split("~n")

	For line$=EachIn lines
		DrawText line,x,y
		y:+13
	Next
End Function

Function dist#(x1#,y1#,x2#,y2#)
	dx#=x2-x1
	dy#=y2-y1
	Return Sqr(dx*dx+dy*dy)
End Function

