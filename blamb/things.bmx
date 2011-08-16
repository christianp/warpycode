'------------- imports
Import maxgui.maxgui		'import maxgui
Import maxgui.win32maxgui


Global loadedvertices:TList
Global state:oldstate
Global things:TList = New TList
Global pickablethings:TList = New TList
Global glyphs:TList = New TList
Global states:TList = New TList
Global gwidth = 800, gheight = 800
Global inputs:Float[20]
Global mx, my
Global scenename:String = "DANCE, MY PRETTIES"
Global win:TGadget, canvas:TGadget
Global anim:animation
Global makepanel:TGadget
Global rodbutton:TGadget
Global slidebutton:TGadget
Global moverbutton:TGadget
Global turnerbutton:TGadget
Global seesawbutton:TGadget
Global polebutton:TGadget
Global joinbutton:TGadget
Global recordverts:TList

Global myti:timelineitem
Global tlmx, tlmy
Global dragoffset
Global time
Global timelinescroll, timelineupscroll
Global timelineitems:TList
Global numtimelineslots = 10
Global timelinewidth = 2000, timelineheight = numtimelineslots * 40, timelinewinwidth = 800, timelinewinheight = 120
Global timelinewindow:TGadget
Global timelineslider:TGadget
Global timelineupslider:TGadget
Global timelinecanvas:TGadget


'************************************
'maths

Function andiff#(an1#,an2#)
	d#=an1-an2
	If d<-180 d:+360
	If d > 180 d:-360
	Return d
End Function


'*************************************
Type vertex
	Field x:Float, y:Float, an:Float
	Field id
	Field newx:Float, newy:Float
	Field w:Float
	
	Function Create:vertex(x:Float = 0, y:Float = 0, an:Float = 0) 
		v:vertex = New vertex
		v.x = x
		v.y = y
		v.newx = x
		v.newy = y
		v.an = an
		v.w = 1
		Return v
	End Function
	
	Method copy:vertex() 
		Return vertex.Create(x, y, an) 
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("vertex") 
		f.WriteLine(id) 
		f.WriteLine(x) 
		f.WriteLine(y) 
		f.WriteLine(an) 
	End Method
	
	Method update()
		'If newx <> x Or newy <> y Then Print "(" + String(x) + "," + String(y) + ") -> (" + String(newx) + "," + String(newy) + ")"
		x = newx
		y = newy
		DrawLine x, y, newx, newy
		
	End Method
	
	Method solveto(sx:Float, sy:Float, weight:Float =.9)
		newx:+(sx - x) * weight
		newy:+(sy - y) * weight
	End Method
	
	Function Load:vertex(f:TStream)
		id = Int(f.ReadLine()) 
		x = Float(f.ReadLine()) 
		y = Float(f.ReadLine()) 
		an = Float(f.ReadLine()) 
		v:vertex = vertex.Create(x, y, an) 
		v.id = id
		loadedvertices.AddLast v
		Return v
	End Function
End Type

Type vertexrecord
	Field v:vertex, x:Float, y:Float, an:Float
	
	Function Create:vertexrecord(v:vertex) 
		vr:vertexrecord = New vertexrecord
		vr.v = v
		vr.x = v.x
		vr.y = v.y
		vr.an = v.an
		Return vr
	End Function
End Type

Type thing
	Field pos:vertex
	
	Method New()
		things.addlast Self
		slide = 0
		pos = New vertex
	End Method
	
	Method update() 
		
	End Method
	
	Method solve()
		
	End Method
	
	Method draw() 
		
	End Method
	
	Method pickableverts:TList() 
		l:TList = New TList
		l.addlast pos
		Return l
	End Method
	
	Method joinverts(a:vertex, b:vertex) 
		If pos = a Then pos = b
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("thing") 
		f.WriteLine(pos.id) 
	End Method
	
	Function Load:thing(f:TStream) 
		pos:vertex = findvert(Int(f.ReadLine())) 
		t:thing = New thing
		t.pos = pos
		Return t
	End Function
	
	Method distancefrom:Float(x:Float, y:Float)
		dx:Float = x - pos.x
		dy:Float = y - pos.y
		Return Sqr(dx * dx + dy * dy)
	End Method
End Type

Type nothing Extends thing
	Function Create:nothing(pos:vertex) 
		n:nothing = New nothing
		n.pos = pos
		pickablethings.AddLast n
		Return n
	End Function
	
	Method draw() 
		SetColor 255, 0, 0
		DrawOval pos.x - 2, pos.y - 2, 4, 4
		SetColor 255, 255, 255
	End Method

	Method joinverts(a:vertex, b:vertex) 
		If pos = a Then pos = b
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("nothing") 
		f.WriteLine(pos.id) 
	End Method
	
	Function Load:thing(f:TStream) 
		pos:vertex = findvert(Int(f.ReadLine())) 
		n:nothing = nothing.Create(pos) 
		Return n
	End Function
End Type

Type rodthing Extends thing
	Field tip:vertex
	Field length:Float, an:Float
	Field posx:Float, posy:Float, tipx:Float, tipy:Float
	
	Function Create:rodthing(pos:vertex, length:Float, an:Float) 
		r:rodthing = New rodthing
		r.pos = pos
		r.tip = pos.copy() 
		r.length = length
		r.an = an
		Return r
	End Function
	
	Method update()
	End Method
	
	Method solve()
		dx:Float = tip.x - pos.x
		dy:Float = tip.y - pos.y
		d:Float = Sqr(dx * dx + dy * dy) 
		move:Float = (d - length) / d
		If d = 0
			Return
		End If
		w:Float = pos.w / (pos.w + tip.w)
		posx = pos.x + dx * move * w
		posy = pos.y + dy * move * w
		tipx = tip.x - dx * move * (1 - w)
		tipy = tip.y - dy * move * (1 - w)
		
		pos.solveto(posx, posy)
		tip.solveto(tipx, tipy)
	End Method
	
	Method distancefrom:Float(x:Float, y:Float)
		angle:Float = ATan2(tip.y - pos.y, tip.x - pos.x)
		lambda:Float = Sin(angle) * (y - pos.y) + Cos(angle) * (x - pos.x)
		If lambda < 0
			dx:Float = x - pos.x
			dy:Float = y - pos.y
			Return Sqr(dx * dx + dy * dy)
		ElseIf lambda > length
			dx:Float = x - tip.x
			dy:Float = y - tip.y
			Return Sqr(dx * dx + dy * dy)
		Else
			mu:Float = Cos(angle) * (pos.y - y) + Sin(angle) * (x - pos.x)
			Return Abs(mu)
		End If
	End Method
	
	Method draw() 
		DrawLine pos.x, pos.y, tip.x, tip.y
		DrawOval tip.x - 2, tip.y - 2, 4, 4
		
		'DrawText Int(distancefrom(mx, my)), pos.x, pos.y
	End Method

	Method pickableverts:TList() 
		l:TList = New TList
		l.addlast pos
		l.addlast tip
		Return l
	End Method

	Method joinverts(a:vertex, b:vertex) 
		If pos = a
			pos = b
		EndIf
		If tip = a Then tip = b
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("rodthing") 
		f.WriteLine(pos.id) 
		f.WriteLine(tip.id) 
		f.WriteLine(length) 
		f.WriteLine(an) 
	End Method
	
	Function Load:thing(f:TStream) 
		pos:vertex = findvert(Int(f.ReadLine())) 
		tip:vertex = findvert(Int(f.ReadLine())) 
		length:Float = Float(f.ReadLine()) 
		an:Float = Float(f.ReadLine()) 
		r:rodthing = rodthing.Create(pos, length, an) 
		r.tip = tip
		Return r
	End Function
End Type

Type slidething Extends thing
	Field angle:Float, length:Float
	Field tip:vertex
	Field button
	Field posx:Float, posy:Float, tipx:Float, tipy:Float
	Field slide:Float
	
	Function Create:slidething(pos:vertex, angle:Float, length:Float, button)
		s:slidething = New slidething
		s.pos = pos
		s.tip = pos.copy() 
		s.angle = angle
		s.length = length
		s.button = button
		Return s
	End Function
	
	Method update() 
		slide:Float = getinput(button)
		If slide < 0 slide = 0
		

	End Method
	
	Method solve()
		dx:Float = tip.x - pos.x
		dy:Float = tip.y - pos.y
		d:Float = Sqr(dx * dx + dy * dy)
		tipx:Float = pos.x + Cos(angle) * slide * length
		tipy:Float = pos.y + Sin(angle) * slide * length

		tip.solveto(tipx, tipy)
	End Method

	Method draw() 
		DrawLine pos.x, pos.y, tip.x, tip.y
		DrawOval tip.x - 2, tip.y - 2, 4, 4
		
		DrawText tip.x, pos.x, pos.y
		DrawText tip.y, pos.x, pos.y + 12
	End Method

	Method pickableverts:TList() 
		l:TList = New TList
		l.addlast pos
		l.addlast tip
		Return l
	End Method

	Method joinverts(a:vertex, b:vertex) 
		If pos = a Then pos = b
		If tip = a Then tip = b
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("slidething") 
		f.WriteLine(pos.id) 
		f.WriteLine(tip.id) 
		f.WriteLine(length) 
		f.WriteLine(angle) 
		f.WriteLine(button) 
	End Method
	
	Function Load:thing(f:TStream) 
		pos:vertex = findvert(Int(f.ReadLine())) 
		tip:vertex = findvert(Int(f.ReadLine())) 
		length:Float = Float(f.ReadLine()) 
		angle:Float = Float(f.ReadLine()) 
		button = Int(f.ReadLine()) 
		s:slidething = slidething.Create(pos, angle, length, button) 
		s.tip = tip
		Return s
	End Function
End Type

Type movething Extends thing
	Field button1, button2
	Field tip:vertex, xrange:Float, yrange:Float
	Field tipx:Float, tipy:Float
	Field dx:Float, dy:Float
	
	Function Create:movething(pos:vertex, xrange:Float, yrange:Float, button1, button2) 
		m:movething = New movething
		m.pos = pos
		m.tip = pos.copy() 
		m.xrange = xrange
		m.yrange = yrange
		m.button1 = button1
		m.button2 = button2
		Return m
	End Function
	
	Method update() 
		dx:Float = xrange * getinput(button1) 
		dy:Float = yrange * getinput(button2) 
	End Method
	
	Method solve()
		tipx = pos.x + dx
		tipy = pos.y + dy
		tip.solveto(tipx, tipy)
	End Method

	Method draw() 
		DrawOval tip.x - 2, tip.y - 2, 4, 4
	End Method

	Method pickableverts:TList() 
		l:TList = New TList
		l.AddLast pos
		l.addlast tip
		Return l
	End Method

	Method joinverts(a:vertex, b:vertex) 
		If pos = a Then pos = b
		If tip = a Then tip = b
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("movething") 
		f.WriteLine(pos.id) 
		f.WriteLine(tip.id) 
		f.WriteLine(xrange) 
		f.WriteLine(yrange) 
		f.WriteLine(button1) 
		f.WriteLine(button2) 
	End Method
	
	Function Load:thing(f:TStream) 
		pos:vertex = findvert(Int(f.ReadLine())) 
		tip:vertex = findvert(Int(f.ReadLine())) 
		xrange:Float = Float(f.ReadLine()) 
		yrange:Float = Float(f.ReadLine()) 
		button1 = Int(f.ReadLine()) 
		button2 = Int(f.ReadLine()) 
		m:movething = movething.Create(pos, xrange, yrange, button1, button2) 
		m.tip = tip
		Return m
	End Function
End Type

Type floatthing Extends thing
	Field xmove:Float, ymove:Float
	Field button1, button2
	Field posx:Float, posy:Float
	
	Function Create:floatthing(pos:vertex, xmove:Float, ymove:Float, button1, button2) 
		f:floatthing = New floatthing
		f.pos = pos
		f.xmove = xmove
		f.ymove = ymove
		f.button1 = button1
		f.button2 = button2
		Return f
	End Function
	
	Method update() 
		dx:Float = getinput(button1) 
		dy:Float = getinput(button2) 
		d:Float = dx * dx + dy * dy
		If d <.02
			dx = 0
			dy = 0
		EndIf
		posx = pos.x + xmove * dx
		posy = pos.y + ymove * dy
	End Method
	
	Method solve()
		pos.solveto(posx, posy)
	End Method

	Method draw() 
		DrawOval pos.x - 2, pos.y - 2, 4, 4
	End Method

	Method joinverts(a:vertex, b:vertex) 
		If pos = a Then pos = b
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("floatthing") 
		f.WriteLine(pos.id) 
		f.WriteLine(xmove) 
		f.WriteLine(ymove) 
		f.WriteLine(button1) 
		f.WriteLine(button2) 
	End Method
	
	Function Load:thing(f:TStream) 
		pos:vertex = findvert(Int(f.ReadLine())) 
		xmove:Float = Float(f.ReadLine()) 
		ymove:Float = Float(f.ReadLine()) 
		button1 = Int(f.ReadLine()) 
		button2 = Int(f.ReadLine()) 
		ft:floatthing = floatthing.Create(pos, xmove, ymove, button1, button2) 
		Return ft
	End Function
End Type

Type turnthing Extends thing
	Field button
	Field tip:vertex
	Field anmove:Float
	Field angle:Float, length:Float
	Field tipx:Float, tipy:Float
	Field an:Float
	
	Function Create:turnthing(pos:vertex, length:Float, angle:Float, anmove:Float, button) 
		tt:turnthing = New turnthing
		tt.pos = pos
		tt.tip = pos.copy() 
		tt.length = length
		tt.angle = angle
		tt.anmove = anmove
		tt.button = button
		Return tt
	End Function
	
	Method update() 
		an = angle + getinput(button) * anmove
	End Method
	
	Method solve()
		tipx = pos.x + Cos(an) * length
		tipy = pos.y + Sin(an) * length
		tip.solveto(tipx, tipy)
	End Method

	Method draw() 
		DrawLine pos.x, pos.y, tip.x, tip.y
		DrawOval tip.x - 2, tip.y - 2, 4, 4
	End Method

	Method pickableverts:TList() 
		l:TList = New TList
		l.addlast pos
		l.addlast tip
		Return l
	End Method

	Method joinverts(a:vertex, b:vertex) 
		If pos = a Then pos = b
		If tip = a Then tip = b
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("turnthing") 
		f.WriteLine(pos.id) 
		f.WriteLine(tip.id) 
		f.WriteLine(length) 
		f.WriteLine(angle) 
		f.WriteLine(anmove) 
		f.WriteLine(button) 
	End Method
	
	Function Load:thing(f:TStream) 
		pos:vertex = findvert(Int(f.ReadLine())) 
		tip:vertex = findvert(Int(f.ReadLine())) 
		length:Float = Float(f.ReadLine()) 
		angle = Float(f.ReadLine()) 
		anmove:Float = Float(f.ReadLine()) 
		button = Int(f.ReadLine()) 
		tt:turnthing = turnthing.Create(pos, length, angle, anmove, button) 
		tt.tip = tip
		Return tt
	End Function
End Type

Type seesawlinkage Extends thing
	Field tip:vertex, pivot:vertex
	Field length:Float
	Field posx:Float, posy:Float, tipx:Float, tipy:Float
	Field active
	
	Function Create:seesawlinkage(pos:vertex, pivot:vertex, length:Float) 
		ss:seesawlinkage = New seesawlinkage
		ss.pos = pos
		ss.tip = pos.copy() 
		ss.pivot = pivot
		ss.length = length
		Return ss
	End Function
	
	Method update()
	End Method
	
	Method solve()
		If length = 0 Or active = 0 Return
		w:Float = pos.w / (pos.w + tip.w)
		dx:Float = pivot.x - pos.x
		dy:Float = pivot.y - pos.y
		d:Float = Sqr(dx * dx + dy * dy)
		
		If d <> 0
			destx:Float = pos.x + (dx * length / d)
			desty:Float = pos.y + (dy * length / d)
			tipx = tip.x + (destx - tip.x) * (1 - w)
			tipy = tip.y + (desty - tip.y) * (1 - w)
		EndIf
		
		dx:Float = pivot.x - tip.x
		dy:Float = pivot.y - tip.y
		d:Float = Sqr(dx * dx + dy * dy)
		
		If d <> 0
			destx:Float = tip.x + (dx * length / d)
			desty:Float = tip.y + (dy * length / d)
			posx = pos.x + (destx - pos.x) * w
			posy = pos.y + (desty - pos.y) * w
		EndIf
		
		pos.solveto(posx, posy)
		tip.solveto(tipx, tipy)
	End Method

	Method draw() 
		DrawLine pos.x, pos.y, tip.x, tip.y
		DrawOval tip.x - 2, tip.y - 2, 4, 4
	End Method

	Method pickableverts:TList() 
		l:TList = New TList
		l.addlast pos
		l.addlast tip
		l.addlast pivot
		Return l
	End Method

	Method joinverts(a:vertex, b:vertex) 
		If pos = a Then pos = b
		If tip = a Then tip = b
		If pivot = a Then pivot = b
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("seesawlinkage") 
		f.WriteLine(pos.id) 
		f.WriteLine(tip.id) 
		f.WriteLine(pivot.id) 
		f.WriteLine(length) 
	End Method
	
	Function Load:thing(f:TStream) 
		pos:vertex = findvert(Int(f.ReadLine())) 
		tip:vertex = findvert(Int(f.ReadLine())) 
		pivot:vertex = findvert(Int(f.ReadLine())) 
		length:Float = Float(f.ReadLine()) 
		ss:seesawlinkage = seesawlinkage.Create(pos, pivot, length) 
		ss.tip = tip
		Return ss
	End Function
End Type

Type polelinkage Extends thing
	Field tip:vertex
	Field middle:vertex
	
	Function Create:polelinkage(pos:vertex, tip:vertex, middle:vertex)
		p:polelinkage = New polelinkage
		p.pos = pos
		p.tip = tip
		p.middle = middle
		Return p
	End Function
	
	Method update()
		
	End Method
	
	Method solve()
		dx1:Float = tip.x - pos.x
		dy1:Float = tip.y - pos.y
		d1:Float = Sqr(dx1 * dx1 + dy1 * dy1)
		dx1:/d1
		dy1:/d1
		If d1 = 0 Return
		
		dx2:Float = middle.x - pos.x
		dy2:Float = middle.y - pos.y
		d2:Float = Sqr(dx2 * dx2 + dy2 * dy2)
		dx2:/d2
		dy2:/d2
		If d2 = 0 Return
		
		dp:Float = dx1 * dx2 + dy1 * dy2
		midx:Float = pos.x + dp * dx1 * d2
		midy:Float = pos.y + dp * dy1 * d2
		
		middle.solveto(midx, midy)
		
	End Method
	
	Method draw() 
		DrawLine pos.x, pos.y, tip.x, tip.y
		DrawOval tip.x - 2, tip.y - 2, 4, 4
	End Method
	
	Method pickableverts:TList()
		l:TList = New TList
		l.addlast pos
		l.addlast tip
		l.addlast middle
		Return l
	End Method

	Method joinverts(a:vertex, b:vertex) 
		If pos = a Then pos = b
		If tip = a Then tip = b
		If middle = a Then middle = b
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("polelinkage") 
		f.WriteLine(pos.id) 
		f.WriteLine(middle.id)
		f.WriteLine(tip.id)
	End Method
	
	Function Load:thing(f:TStream) 
		pos:vertex = findvert(Int(f.ReadLine())) 
		middle:vertex = findvert(Int(f.ReadLine()))
		tip:vertex = findvert(Int(f.ReadLine()))
		p:polelinkage = polelinkage.Create(pos, tip, middle)
		Return p
	End Function
End Type


Function fillinputs()
	If JoyCount()
		For i = 0 To 9
			inputs[i + 1] = JoyDown(i)
		Next
		inputs[11] = JoyX()
		inputs[12] = JoyY()
		inputs[13] = JoyU()
		inputs[14] = JoyR()
		inputs[15] = JoyZ()
	EndIf
	inputs[16] = Float(mx) / gwidth
	inputs[17] = Float(my) / gheight
	
End Function

Function getinput:Float(button) 
	If button >= Len(inputs) Return 0
	Return inputs[Abs(button)] * Sgn(button) 
End Function

Type glyph
	Field imgfile:String
	Field img:TImage
	Field pos:vertex
	Field scale:Float
	Field an:Float
	Field size:Float
	
	Method New() 
		glyphs.AddLast Self
	End Method
	
	Function Create:glyph(imgfile:String, pos:vertex, scale:Float, an:Float) 
		g:glyph = New glyph
		g.imgfile = imgfile
		g.img = LoadImage(imgfile)
		g.size = Sqr(ImageWidth(g.img) ^ 2 + ImageHeight(g.img) ^ 2)
		g.pos = pos
		g.scale = scale
		g.an = an
	End Function
	
	Method draw() 
		SetRotation pos.an + an
		SetScale scale, scale
		DrawImage img, pos.x, pos.y
	End Method
	
	Method save(f:TStream) 
		f.WriteLine("glyph") 
		f.WriteLine(pos.id) 
		f.WriteLine(imgfile) 
		f.WriteLine(scale) 
		f.WriteLine(an) 
	End Method
	
	Method joinverts(a:vertex, b:vertex)
		If pos = a Then pos = b
	End Method
	
	Function Load:glyph(f:TStream) 
		pos = findvert(Int(f.ReadLine())) 
		imgfile:String = f.ReadLine() 
		scale:Float = Float(f.ReadLine()) 
		an:Float = Float(f.ReadLine()) 
		g:glyph = glyph.Create(imgfile, pos, scale, an) 
		Return g
	End Function
End Type


'----------------- timeline

'scrolling around functions
Function scrollxto(newx)
	x = timelinescroll
	If newx < x + 10
		'x = newx - 10
		x:-3
		tlmx:-3
		If x < 0 Then x = 0
		SetSliderValue timelineslider, x
	ElseIf newx > x + timelinewinwidth - 10
		'x = newx + 10 - timelinewinwidth
		x:+3
		tlmx:+3
		If x > timelinewidth - timelinewinwidth
			x = timelinewidth - timelinewinwidth
		End If
		SetSliderValue timelineslider, x
	End If
End Function

Function scrollyto(newy)
	y = timelineupscroll
	If newy <= y / 40
		y:-3
		tlmy:-3
		If y < 0 Then y = 0
		SetSliderValue timelineupslider, y
	ElseIf newy >= (y + timelinewinheight) / 40
		y:+3
		tlmy:+3
		If y > (timelineheight - timelinewinheight)
			y = (timelineheight - timelinewinheight)
		EndIf
		SetSliderValue timelineupslider, y
	End If
End Function

Function resizetimeline(width, height)
	timelinewidth = width
	timelineheight = height * 40
	SetSliderRange timelineslider, 0, timelinewidth - timelinewinwidth
	SetSliderRange timelineupslider, 0, timelineheight - timelinewinheight
End Function

Function updatetimeline()
	Select state.s
	Case 0 'normal
		
	Case 1001 'making element
		time = tlmx
	Case 1002 'dragging element
		newx = tlmx + dragoffset
		myti.movex(newx)
		
		y = tlmy / 40
		myti.movey(y)

		scrollxto tlmx
		scrollyto y
	Case 5 'playing
		If time - timelinescroll > timelinewinwidth / 2
			SetSliderValue timelineslider, time - timelinewinwidth / 2
		End If
	End Select
End Function

Function drawtimeline()
	SetGraphics CanvasGraphics(timelinecanvas)
	timelinescroll = SliderValue(timelineslider)
	timelineupscroll = SliderValue(timelineupslider)
	SetColor 100, 100, 100
	For i = 0 To timelinewidth Step 100
		SetColor 100, 100, 255 * i / Float(timelinewidth)
		DrawLine i - timelinescroll, 0, i - timelinescroll, timelinewinheight
	Next
	SetColor 30, 30, 30
	For i = 0 To timelineheight Step 40
		DrawLine 0, i - timelineupscroll, timelinewinwidth, i - timelineupscroll
	Next
	For ti:timelineitem = EachIn timelineitems
		ti.draw()
	Next
	
	SetColor 255, 0, 0
	DrawLine time - timelinescroll, 0, time - timelinescroll, timelinewinheight
	
	Flip
	Cls
End Function

Type timelineitem
	Field starttime, endtime
	Field y
	Field id
	
	Method New()
		timelineitems.AddLast Self

		y = -1
		go = 0
		While Not go
			y:+1
			go = 1
			For ti:timelineitem = EachIn timelineitems
				If ti <> Self And ti.y = y Then go = 0
			Next
			
		Wend
		If y >= numtimelineslots
			resizetimeline(timelinewidth, y + 1)
			numtimelineslots:+1
		End If
		id = timelineitems.Count()
	End Method
	
	Method draw()
		x1 = starttime - timelinescroll
		y1 = y * 40 + 2 - timelineupscroll
		x2 = endtime - timelinescroll
		y2 = y * 40 + 36 - timelineupscroll
		If state.s = 5 And time >= starttime And time <= endtime
			SetColor 255, 0, 0
		Else
			SetColor 255, 255, 255
		End If
		SetAlpha 0.5
		DrawRect x1, y1, x2 - x1, y2 - y1
		SetAlpha 1
		SetColor 255, 255, 255
		DrawLine x1, y1, x2, y1
		DrawLine x1, y1, x1, y2
		DrawLine x1, y2, x2, y2
		DrawLine x2, y1, x2, y2
		DrawText id, x1 + 3, (y1 + y2) / 2
	End Method
	
	Method update()
		If time >= starttime And time <= endtime
			DrawLine starttime - timelinescroll, y * 40 + 20 - timelineupscroll, time - timelinescroll, y * 40 + 20 - timelineupscroll
		End If
	End Method
	
	Method movex(newx, diffx = 0)
		If newx < 0 newx = 0
		length = endtime - starttime
		If newx + length > timelinewidth Then newx = timelinewidth - length
		If diffx = 0 Then diffx = newx - starttime
		If diffx < 0
			For ti:timelineitem = EachIn timelineitems
				If ti <> Self
					If ti.y = y And starttime > ti.endtime And newx <= ti.endtime
						ti.movex(ti.starttime + newx - ti.endtime)
						newx = ti.endtime + 1
					End If
				EndIf
			Next
		Else
			For ti:timelineitem = EachIn timelineitems
				If ti <> Self
					If ti.y = y And endtime < ti.starttime And newx + length >= ti.starttime
						ti.movex(newx + length + 1)
						newx = ti.starttime - 1 - length
					End If
				EndIf
			Next
		End If
		starttime = newx
		endtime = starttime + length
	End Method
	
	Method movey(newy)
		oldy = y
		If newy < 0 newy = 0
		If newy > timelineheight / 40 - 1 Then newy = timelineheight / 40 - 1
		go = 1
		For ti:timelineitem = EachIn timelineitems
			If ti <> Self
				If ti.y = newy And ((ti.starttime >= starttime And ti.starttime <= endtime) Or (starttime >= ti.starttime And starttime <= ti.endtime))
					go = 0
				EndIf
			EndIf
		Next
		If go
			y = newy
			Return 1
		Else
			Return 0
		End If
	End Method
End Type

Type animation Extends timelineitem
	Field frames:TList
	Method New()
		frames = New TList
		
		starttime = time
		endtime = starttime
	End Method
	
	Method update()
		For fr:frame = EachIn frames
			If fr.starttime = time - starttime
				fr.play()
			EndIf
		Next
	End Method
	
	Method record()
		fr:frame = New frame
		fr.starttime = time - starttime
		frames.AddLast fr
		endtime = starttime + frames.Count()
		movex(starttime, 1)
		scrollyto(y)
		scrollxto(endtime)
	End Method
End Type

rem
Type animation
	Field frames:TList
	Field curframe:TLink
	Field nextgo
	Field fps:Float
	
	Method New() 
		frames = New TList
		nextgo = MilliSecs() 
		fps:Float = 1000.0 / 100
	End Method
	
	Method start() 
		curframe = frames.FirstLink() 
	End Method
	
	Method play() 
		If MilliSecs() < nextgo Return
		nextgo:+fps
		If Not curframe Then Return
		fr:frame = frame(curframe.Value()) 
		fr.play() 
		curframe = curframe.NextLink() 
	End Method
	
	Method record() 
		If MilliSecs() < nextgo Return
		nextgo:+fps
		fr:frame = New frame
		frames.AddLast fr
	End Method
End Type
endrem

Type frame
	Field records:TList
	Field starttime
	
	Method New() 
		records = New TList
		For v:vertex = EachIn recordverts
			records.AddLast vertexrecord.Create(v) 
		Next
	End Method
	
	Method play()
		For vr:vertexrecord = EachIn records
			vr.v.x = vr.x
			vr.v.y = vr.y
			vr.v.an = vr.an
		Next
	End Method
End Type

Type oldstate
	Field s
	Field t:thing
	Field v:vertex
	Field g:glyph
	Field l:TList
	Field num, f1:Float, f2:Float
End Type

Function changestate(s, keep = 1) 
	If keep And state Then states.AddLast state
	state = New oldstate
	state.s = s
	If s <> 0
		DisableGadget makepanel
	EndIf
End Function
Function backstate() 
	state = oldstate(states.RemoveLast())
	If state.s = 0
		EnableGadget makepanel
	End If
End Function

Function joinverts(a:vertex, b:vertex) 
	For t:thing = EachIn things
		t.joinverts(a, b) 
	Next
	
	For g:glyph = EachIn glyphs
		g.joinverts(a, b)
	Next
End Function

Function pickthing:thing() 
	mindist:Float = -1
	closest:thing = Null
	For t:thing = EachIn pickablethings
		dx:Float = t.pos.x - mx
		dy:Float = t.pos.y - my
		d:Float = dx * dx + dy * dy
		If d < 40 And (d < mindist Or mindist = -1) 
			closest = t
			mindist = d
		End If
	Next
	If closest
		DrawOval closest.pos.x - 8, closest.pos.y - 8, 16, 16
	End If
	Return closest
End Function

Function findverts:TList() 
	pickableverts:TList = New TList
	For t:thing = EachIn things
		For v:vertex = EachIn t.pickableverts() 
			If Not pickableverts.Contains(v) pickableverts.AddLast v
		Next
	Next
	Return pickableverts
End Function

Function findvert:vertex(id) 
	For v:vertex = EachIn loadedvertices
		If v.id = id
			Return v
		End If
	Next
End Function

Function pickvert:vertex(makenew = 1) 
	mindist:Float = -1
	closest:vertex = Null
	pickableverts:TList = findverts() 
	For v:vertex = EachIn pickableverts
		dx:Float = v.x - mx
		dy:Float = v.y - my
		d:Float = dx * dx + dy * dy
		If d < 40 And (d < mindist Or mindist = -1) 
			closest = v
			mindist = d
		End If
	Next
	If closest
		DrawOval closest.x - 8, closest.y - 8, 16, 16
	ElseIf makenew
		closest = vertex.Create(mx, my) 
	End If
	Return closest
End Function

Function pickverts:TList() 
	l:TList = New TList
	pickableverts:TList = findverts() 
	For v:vertex = EachIn pickableverts
		dx:Float = v.x - mx
		dy:Float = v.y - my
		d:Float = dx * dx + dy * dy
		If d < 40
			l.addlast v
			DrawOval v.x - 8, v.y - 8, 16, 16
		End If
	Next
	Return l
End Function

Function pickbutton() 
	For i = 0 To Len(inputs) - 1
		If Abs(inputs[i]) >.9
			Return i * Sgn(inputs[i] ) 
		End If
	Next
	Return Len(inputs) 
End Function

Function updatestuff() 
	fillinputs()
	
	oldms = ms
	ms = MilliSecs()
	
	Select state.s
	Case 0 'nothing happening
	Case 1 'dragging a vertex
		For v:vertex = EachIn state.l
			v.x = mx
			v.y = my
		Next
	Case 2 'waiting for input to clear
		button = pickbutton() 
		If button = Len(inputs) 
			backstate()
		End If
	Case 3 'joining vertices
	Case 4 'recording
		anim.record()
		time:+1
	Case 5 'playing
		playing = 0
		For ti:timelineitem = EachIn timelineitems
			ti.update()
			If time <= ti.endtime Then playing:+1
		Next
		If playing
			time:+1
		Else
			time = 0
			backstate()
		End If
	Case 6 'rotating glyph
		g:glyph = state.g
		dx:Float = mx - g.pos.x
		dy:Float = my - g.pos.y
		an:Float = ATan2(dy, dx)
		g.an = state.f1 + andiff(an, state.f2)
	Case 20 'making a rod
	Case 21 'finishing a rod
	Case 30 'making a slide
	Case 31 'defining slide length
	Case 32 'picking button for slide
		s:slidething = slidething(state.t) 
		button = pickbutton()
		If button < Len(inputs) 
			s.button = button
			backstate() 
		End If
	Case 40 'picking base for movething
	Case 41 'defining range of movething
		m:movething = movething(state.t) 
		dx:Float = Abs(mx - m.pos.x)
		dy:Float = Abs(my - m.pos.y) 
		m.xrange = dx
		m.yrange = dy
	Case 42 'defining x movement button of movething
		m:movething = movething(state.t) 
		button = pickbutton()
		If button < Len(inputs) 
			m.button1 = button
			changestate(43, 0) 
			state.t = m
			changestate(2) 
		End If
	Case 43 'defining y movement button of movething
		m:movething = movething(state.t)
		button = pickbutton()
		If button < Len(inputs) 
			m.button2 = button
			backstate() 
		End If
	Case 50 'making a turnthing
	Case 51 'defining length and angle of turnthing
	Case 52 'defining range of turnthing
		tt:turnthing = turnthing(state.t) 
		dx:Float = mx - tt.pos.x
		dy:Float = my - tt.pos.y
		an:Float = ATan2(dy, dx) 
		tt.anmove = Abs(andiff(an, tt.angle + tt.pos.an))
	Case 53 'defining control for turnthing
		tt:turnthing = turnthing(state.t) 
		button = pickbutton()
		If button < Len(inputs) 
			tt.button = button
			backstate() 
		End If
	Case 60 'make seesawlinkage
	Case 61 'choose pivot of seesaw
	Case 62 'define length of seesaw
		ss:seesawlinkage = seesawlinkage(state.t) 
		dx:Float = mx - ss.pos.x
		dy:Float = my - ss.pos.y
		an:Float = ATan2(ss.pivot.y - ss.pos.y, ss.pivot.x - ss.pos.x) 
		ss.length = dx * Cos(an) + dy * Sin(an)
		ss.tip.x = ss.pos.x + ss.length * Cos(an)
		ss.tip.y = ss.pos.y + ss.length * Sin(an)
	Case 70 'make polelinkage
	Case 71 'pick end of pole
	End Select
	
	If state.s <> 5
		For t:thing = EachIn things
			t.update() 
		Next
		
		vlist:TList = findverts()
		For v:vertex = EachIn vlist
			v.newx = v.x
			v.newy = v.y
		Next
		For i = 0 To 10
			For t:thing = EachIn things
				t.solve()
			Next
			For v:vertex = EachIn vlist
				v.update()
			Next
		Next
		
	EndIf
		
End Function

Function drawstuff() 
	SetGraphics CanvasGraphics(canvas)
	
	SetColor 255, 255, 255
	For g:glyph = EachIn glyphs
		g.draw() 
	Next
	SetRotation 0
	SetScale 1, 1

	For t:thing = EachIn things
		t.draw() 
	Next
	
	SetColor 255, 255, 255
	For v:vertex = EachIn pickverts()
		DrawOval v.x - 8, v.y - 8, 16, 16
	Next
	SetColor 0, 255, 0
	For v:vertex = EachIn recordverts
		DrawOval v.x - 6, v.y - 6, 12, 12
	Next
	SetColor 255, 0, 0
	For v:vertex = EachIn findverts() 
		DrawOval v.x - 3, v.y - 3, 6, 6
	Next
	SetColor 255, 255, 255
	
	Select state.s
	Case 2
		DrawText String(button) + " " + String(getinput(button)), 0, 0
	Case 3
		If state.v
			DrawText "click on second vertex to join", 0, 0
		Else
			DrawText "click on first vertex to join", 0, 0
		End If
	Case 4
		DrawText "press space to stop recording", 0, 0
		DrawText anim.frames.Count(), 0, 15
	Case 20
		DrawText "click on base of rod", 0, 0
	Case 21
		DrawText "click on end of rod", 0, 0
		r:rodthing = rodthing(state.t) 
		DrawLine r.pos.x, r.pos.y, mx, my
	Case 30
		DrawText "click on start of slide", 0, 0
	Case 31
		DrawText "click on end of slide", 0, 0
		s:slidething = slidething(state.t) 
		DrawLine s.pos.x, s.pos.y, mx, my
	Case 32 'picking button for slide
		DrawText "press button or move stick in direction to control this slide", 0, 0
	Case 40 'picking base for movething
		DrawText "click on centre point of mover", 0, 0
	Case 41 'defining range of movething
		DrawText "click to define range of mover", 0, 0
		m:movething = movething(state.t) 
		SetAlpha.2
		DrawRect m.pos.x - m.xrange, m.pos.y - m.yrange, m.xrange * 2, m.yrange * 2
		SetAlpha 1
	Case 42 'defining x movement button of movething
		DrawText "press button or move stick in direction for control RIGHT of mover", 0, 0
	Case 43 'defining y movement button of movething
		DrawText "press button or move stick in direction for control DOWN of mover", 0, 0
	Case 50 'making a turnthing
		DrawText "click on start of turner", 0, 0
	Case 51 'defining length and angle of turnthing
		DrawText "click on end of turner", 0, 0
		tt:turnthing = turnthing(state.t) 
		DrawLine tt.pos.x, tt.pos.y, mx, my
	Case 52 'defining range of turnthing
		DrawText "click to define turning range", 0, 0
		tt:turnthing = turnthing(state.t) 
		steps = tt.anmove / 10
		If steps = 0 steps = 1
		Local poly:Float[] 
		poly = New Float[steps * 4 + 4] 
		poly[0] = tt.pos.x
		poly[1] = tt.pos.y
		n = 2
		For i = -steps To steps
			angle:Float = Float(i) / steps * tt.anmove + tt.angle + tt.pos.an
			poly[n] = tt.pos.x + Cos(angle) * tt.length
			poly[n + 1] = tt.pos.y + Sin(angle) * tt.length
			n:+2
		Next
		SetAlpha.2
		DrawPoly poly
		SetAlpha 1
	Case 53 'defining control for turnthing
		DrawText "press button or move stick for CLOCKWISE control of turner", 0, 0
	Case 60 'make seesawlinkage
		DrawText "click on start of seesaw", 0, 0
	Case 61 'choose pivot of seesaw
		DrawText "click on pivot point", 0, 0
		ss:seesawlinkage = seesawlinkage(state.t) 
		DrawLine ss.pos.x, ss.pos.y, mx, my
	Case 62 'define length of seesaw
		DrawText "click to define length of seesaw", 0, 0
		ss:seesawlinkage = seesawlinkage(state.t) 
		x:Float = ss.pos.x + ss.length * Cos(an)
		y:Float = ss.pos.y + ss.length * Sin(an)
		DrawLine ss.pos.x, ss.pos.y, x, y
	Case 70 'make polelinkage
		DrawText "click on start of pole", 0, 0
	Case 71 'pick middle of pole
		DrawText "click on middle of pole", 0, 0
		p:polelinkage = polelinkage(state.t) 
		DrawLine p.pos.x, p.pos.y, mx, my
	Case 72 'pick end of pole
		DrawText "click on end of pole", 0, 0
		p:polelinkage = polelinkage(state.t)
		DrawLine p.pos.x, p.pos.y, mx, my
	End Select
		
	DrawText state.s, 780, 0
	
	If ms = oldms Then fps = -1 Else fps = 1000 / (ms - oldms)
	DrawText fps, gwidth / 2, 0
	
	Flip
	Cls
End Function

Function savescene(filename:String) 
	f:TStream = WriteFile(filename) 
	f.WriteLine("scene") 
	f.WriteLine(scenename) 
	f.WriteLine(gwidth) 
	f.WriteLine(gheight) 

	verts:TList = findverts() 
	For v:vertex = EachIn verts
		v.save(f) 
	Next
	
	For t:thing = EachIn things
		t.save(f) 
	Next
	
	For g:glyph = EachIn glyphs
		g.save(f) 
	Next
	f.Close() 
End Function

Function loadscene(filename:String) 
	f:TStream = ReadFile(filename) 
	loadedvertices = New TList

	While Not f.Eof() 
		k:String = f.ReadLine() 
		Print k
		Select k
		Case "scene"
			scenename = f.ReadLine() 
			Print scenename
			gwidth = Float(f.ReadLine()) 
			gheight = Float(f.ReadLine()) 
			initscene() 
		Case "vertex"
			vertex.Load(f) 
		Case "thing"
			thing.Load(f) 
		Case "nothing"
			nothing.Load(f) 
		Case "rodthing"
			rodthing.Load(f) 
		Case "slidething"
			slidething.Load(f) 
		Case "movething"
			movething.Load(f) 
		Case "floatthing"
			floatthing.Load(f) 
		Case "turnthing"
			turnthing.Load(f) 
		Case "seesawlinkage"
			seesawlinkage.Load(f) 
		Case "glyph"
			glyph.Load(f) 
		End Select
	Wend
End Function


'm:movething = movething.Create(vertex.Create(400, 800), 50, 150, 12, 13) 
'f:floatthing = floatthing.Create(vertex.Create(400, 600), 8, 8, 10, 11) 
'ss:seesawlinkage = seesawlinkage.Create(m.tip, f.pos, 350) 
'tt:turnthing = turnthing.Create(ss.tip, 200, 90, 70, 14) 
'tt2:turnthing = turnthing.Create(ss.tip, 200, - 90, - 70, 14) 
's:slidething = slidething.Create(tt.tip, - 45, 200, 0) 

'f:floatthing = floatthing.Create(vertex.Create(400, 400), 4, 4, 10, - 14) 
's:slidething = slidething.Create(f.pos, - 90, 30, 14) 
'tt:turnthing = turnthing.Create(s.tip, 100, 90, 70, 14) 
'tt2:turnthing = turnthing.Create(s.tip, 100, - 90, - 70, 14) 

'glyph.Create(LoadImage("finger.png"), tt.tip, .3, 90) 
'glyph.Create(LoadImage("finger.png"), tt2.tip, .3, 90) 


Function initscene() 
	idcount = 0
	things:TList = New TList
	pickablethings:TList = New TList
	glyphs:TList = New TList
	states:TList = New TList
	recordverts = New TList
	If win
		FreeGadget win
		FreeGadget canvas
		
		FreeGadget timelinewindow
		FreeGadget timelinecanvas
	End If
	win:TGadget = CreateWindow(scenename, 0, 0, gwidth, gheight + 30, Null, WINDOW_TITLEBAR | WINDOW_ACCEPTFILES)
	canvas:TGadget = CreateCanvas(0, 25, gwidth, gheight, win)
	
	makepanel:TGadget = CreatePanel(0, 0, 400, 24, win, 0, "create items")
	rodbutton:TGadget = CreateButton("Rod", 0, 0, 50, 24, makepanel, BUTTON_PUSH)
	slidebutton:TGadget = CreateButton("Slide", 50, 0, 50, 24, makepanel, BUTTON_PUSH)
	moverbutton:TGadget = CreateButton("Mover", 100, 0, 50, 24, makepanel, BUTTON_PUSH)
	turnerbutton:TGadget = CreateButton("Turner", 150, 0, 50, 24, makepanel, BUTTON_PUSH)
	seesawbutton:TGadget = CreateButton("Seesaw", 200, 0, 50, 24, makepanel, BUTTON_PUSH)
	polebutton:TGadget = CreateButton("Pole", 250, 0, 50, 24, makepanel, BUTTON_PUSH)
	joinbutton:TGadget = CreateButton("Join", 300, 0, 50, 24, makepanel, BUTTON_PUSH)
	
	SetGraphics CanvasGraphics(canvas)
	SetBlend ALPHABLEND


	timelineitems:TList = New TList
	
	timelinewindow:TGadget = CreateWindow("Timeline", 0, 0, timelinewinwidth + 20, timelinewinheight + 50, Null, WINDOW_TITLEBAR)
	
	timelineslider:TGadget = CreateSlider(0, timelinewinheight, timelinewinwidth, 20, timelinewindow, SLIDER_HORIZONTAL | SLIDER_SCROLLBAR)
	SetSliderRange(timelineslider, 0, timelinewidth - timelinewinwidth)
	SetSliderValue(timelineslider, 0)
	timelineupslider:TGadget = CreateSlider(timelinewinwidth, 0, 20, timelinewinheight, timelinewindow, SLIDER_VERTICAL | SLIDER_SCROLLBAR)
	SetSliderRange(timelineupslider, 0, timelineheight - timelinewinheight)
	SetSliderValue(timelineupslider, 0)
	
	timelinecanvas:TGadget = CreateCanvas(0, 0, timelinewinwidth, timelinewinheight, timelinewindow)
	SetGraphics CanvasGraphics(timelinecanvas)
	SetBlend ALPHABLEND

	CreateTimer 60
	AutoMidHandle True
	'EnablePolledInput()
	changestate(0) 
End Function


AddHook EmitEventHook, eventhook
initscene() 

Global ms, oldms
oldms = MilliSecs()
While 1
	WaitEvent()
Wend
End

Function eventhook:Object(id, data:Object, context:Object)
	Local event:TEvent = TEvent(data)
	'Print "!"
	Select event.id
		Case EVENT_TIMERTICK
			updatestuff()
			RedrawGadget canvas

			updatetimeline()
			RedrawGadget timelinecanvas

		Case EVENT_GADGETPAINT
			Select event.source
			Case canvas
				drawstuff()
			Case timelinecanvas
				drawtimeline()
			End Select
				
		Case EVENT_MOUSEMOVE
			Select event.source
			Case canvas
				mx = event.x
				my = event.y
			Case timelinecanvas
				tlmx = Event.x + timelinescroll
				tlmy = Event.y + timelineupscroll
			End Select
			
		Case EVENT_MOUSEDOWN
			If event.data <= 2
				inputs[17 + event.data] = 1
			EndIf
			Select event.source
			Case canvas
				Select state.s
				Case 0
					Select event.data
					Case 1
						closestverts:TList = pickverts()
						If closestverts.count()
							changestate(1) 
							state.l = closestverts
						End If
					Case 2
						For g:glyph = EachIn glyphs
							dx:Float = mx - g.pos.x
							dy:Float = my - g.pos.y
							d:Float = dx * dx + dy * dy
							If d < g.size * g.size
								changestate(6)
								state.g = g
								state.f1 = g.an
								state.f2 = ATan2(dy, dx)
							End If
						Next
					End Select
				End Select
			Case timelinecanvas
			Select state.s
				Case 0
					y = tlmy / 40
					myti:timelineitem = Null
					For tib:timelineitem = EachIn timelineitems
						If y = tib.y And tlmx >= tib.starttime And tlmx <= tib.endtime
							myti = tib
						EndIf
					Next
					If myti
						changestate(1002)
						dragoffset = myti.starttime - tlmx
						state.num = MilliSecs()
					Else
						changestate(1001)
'						changestate(1001)
'						myti:timelineitem = New timelineitem
'						myti.starttime = tlmx
'						myti.endtime = myti.starttime
'						myti.y = tlmy / 40
					EndIf
				End Select
			End Select
			Case EVENT_MOUSEUP
			inputs[17 + event.data] = 0
			Select event.source
			Case canvas
				Select state.s
				Case 0
					If event.data = 2
						For v:vertex = EachIn pickverts()
							If recordverts.Contains(v)
								recordverts.Remove v
							Else
								recordverts.AddLast v
							End If
						Next
					End If
				Case 1
					backstate()
				Case 3
					Select event.data
					Case event.data = 1
						closestvert:vertex = pickvert(0)
						If state.v
							If closestvert
								joinverts(state.v, closestvert) 
								backstate() 
							EndIf
						Else
							If closestvert
								state.v = closestvert
							End If
						End If
					Case 3
						backstate()
					End Select
				Case 6
					If event.data = 2
						backstate()
					End If
				Case 20
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						r:rodthing = rodthing.Create(closestvert, 0, 0)
						changestate(21, 0) 
						state.t = r
					Case 2
						backstate()
					End Select
				Case 21
					r:rodthing = rodthing(state.t)
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						r.tip = closestvert
						dx:Float = r.tip.x - r.pos.x
						dy:Float = r.tip.y - r.pos.y
						r.length:Float = Sqr(dx * dx + dy * dy) 
						'r.an:Float = ATan2(dy, dx) - r.pos.an
						backstate()
					Case 2
						things.Remove r
						backstate() 
					End Select
				Case 30
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						s:slidething = slidething.Create(closestvert, 0, 0, Len(inputs))
						changestate(31, 0) 
						state.t = s
					Case 2
						backstate()
					End Select
				Case 31
					s:slidething = slidething(state.t)
					Select event.data
					Case 1
						closestvert:vertex = pickvert() 
						s.tip = closestvert
						dx:Float = s.tip.x - s.pos.x
						dy:Float = s.tip.y - s.pos.y
						s.length:Float = Sqr(dx * dx + dy * dy) 
						s.angle:Float = ATan2(dy, dx)
						changestate(32, 0) 
						state.t = s
						changestate(2)
					Case 2
						things.Remove s
						backstate() 
					End Select
				Case 40
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						m:movething = movething.Create(closestvert, 0, 0, Len(inputs), Len(inputs))
						changestate(41, 0) 
						state.t = m
					Case 2
						backstate()
					End Select
				Case 41
					m:movething = movething(state.t)
					Select event.data
					Case 1
						changestate(42, 0) 
						state.t = m
						changestate(2)
					Case 2
						things.Remove m
						backstate() 
					End Select
				Case 50
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						tt:turnthing = turnthing.Create(closestvert, 0, 0, 0, Len(inputs))
						changestate(51, 0) 
						state.t = tt
					Case 2
						backstate()
					End Select
				Case 51
					tt:turnthing = turnthing(state.t)
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						dx:Float = mx - tt.pos.x
						dy:Float = my - tt.pos.y
						tt.length:Float = Sqr(dx * dx + dy * dy) 
						tt.angle:Float = ATan2(dy, dx) - tt.pos.an
						tt.tip = closestvert
						changestate(52, 0) 
						state.t = tt
					Case 2
						things.Remove tt
						backstate() 
					End Select
				Case 52
					If event.data = 1
						tt:turnthing = turnthing(state.t)
						changestate(53, 0) 
						state.t = tt
						changestate(2) 
					End If
				Case 60
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						ss:seesawlinkage = seesawlinkage.Create(closestvert, closestvert.copy(), 0)
						changestate(61, 0) 
						state.t = ss
					Case 2
						backstate()
					End Select
				Case 61
					ss:seesawlinkage = seesawlinkage(state.t)
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						ss.pivot = closestvert
						changestate(62, 0) 
						state.t = ss
					Case 2
						things.Remove ss
						backstate() 
					End Select
				Case 62
					If event.data = 1
						ss:seesawlinkage = seesawlinkage(state.t)
						ss.active = 1
						backstate() 
					End If
				Case 70
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						p:polelinkage = polelinkage.Create(closestvert, closestvert.copy(), closestvert.copy())
						changestate(71, 0) 
						state.t = p
					Case 2
						backstate()
					End Select
				Case 71
					p:polelinkage = polelinkage(state.t)
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						p.middle = closestvert
						changestate(72, 0)
						state.t = p
					Case 2
						things.Remove p
						backstate()
					End Select
				Case 72
					p:polelinkage = polelinkage(state.t)
					Select event.data
					Case 1
						closestvert:vertex = pickvert()
						p.tip = closestvert
						backstate()
					Case 2
						things.Remove p
						backstate()
					End Select
				End Select
			Case timelinecanvas
				Select state.s
				Case 1001
					backstate()
				Case 1002
					If MilliSecs() - state.num < 100
						time = myti.starttime
						changestate(5, 0)
					Else
						backstate()
					EndIf
				End Select
			End Select
		Case EVENT_MOUSEWHEEL
			If event.source = timelinecanvas
				SetSliderValue timelineupslider, SliderValue(timelineupslider) - 5 * event.data
			EndIf
		Case EVENT_KEYDOWN
			'Print Event.data
		Case EVENT_KEYUP
			Select event.source
			Case canvas
				Select state.s
				Case 0
					Select event.data
					Case KEY_R 'make rod
						changestate(20)
					Case KEY_S 'make slider
						changestate(30)
					Case KEY_M 'make mover
						changestate(40)
					Case KEY_T 'make turner
						changestate(50)
					Case KEY_W 'make seesaw
						changestate(60)
					Case KEY_P 'make pole
						changestate(70)
					Case KEY_F1 'save scene
						savescene("scene.txt")
					Case KEY_J 'join vertices
						changestate(3)
					Case KEY_SPACE 'record
						anim = New animation
						changestate(4) 
					Case KEY_ENTER 'play
						changestate(5)
					End Select
				Case 4
					If event.data = KEY_SPACE
						backstate() 
					End If
				Case 5
					If event.data = KEY_ENTER
						backstate() 
					End If
				End Select
			End Select
		Case EVENT_WINDOWACCEPT
			path:String = String(Event.extra)
			typ$ = path[Len(path) - 3..]
			If typ = "jpg" Or typ = "bmp" Or typ = "gif" Or typ="png"
				n:nothing = nothing.Create(vertex.Create(mx, my)) 
				glyph.Create(path, n.pos, 1, 0) 
			EndIf
			
			If typ = "txt"
				Print "load " + path
				loadscene(path) 
			End If
			
		Case EVENT_GADGETACTION
			Select event.source
			Case rodbutton
				changestate(20)
			Case slidebutton
				changestate(30)
			Case moverbutton
				changestate(40)
			Case turnerbutton
				changestate(50)
			Case seesawbutton
				changestate(60)
			Case polebutton
				changestate(70)
			Case joinbutton
				changestate(3)
			End Select

		Case EVENT_WINDOWCLOSE
			FreeGadget Canvas
			End

		Case EVENT_APPTERMINATE
			End
	End Select
	Return data
End Function