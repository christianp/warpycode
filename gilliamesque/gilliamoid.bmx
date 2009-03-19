Global pieces:tlist=New tlist
Type piece
	Field img:timage
	Field w:Float, h:Float
	Field x# , y#
	Field hx#,hy#
	Field an#
	Field pins:tlist
	Field anchored
	Field mypin:pin
	
	Method New()
		pieces.addlast Self
		pins=New tlist
	End Method
	
	Function create:piece(img:timage , x# , y#)
		p:piece = New piece
		p.img = img
		p.w = ImageWidth(img)
		p.h = ImageHeight(img)
		p.hx = p.w / 2
		p.hy = p.h / 2
		p.mypin=pin.create(0,0)
		p.mypin.x=x
		p.mypin.y=y
				
		Return p
	End Function
	
	Method update()
		DrawText pins.count(),0,0
		For pp:pin = EachIn pins
			pp.rot=mypin.rot+an
			pp.x = mypin.x + pp.d * Cos(pp.an + an) 
			pp.y = mypin.y + pp.d * Sin(pp.an + an)
			'DrawText String(pp.x) + "," + String(pp.y) , 0 , 15
			'DrawText pp.d,0,30
			'DrawLine mypin.x,mypin.y,pp.x,pp.y
		Next
	End Method
	
	Method attach(p:piece , x# , y#)
		p.anchored=1
		x1# = x - mypin.x
		y1# = y - mypin.y
		Print x1
		Print y1
		d1# = Sqr(x1 * x1 + y1 * y1)
		an1#=ATan2(y1,x1)-an
		pp:pin = pin.create(an1 , d1)
		pins.addlast pp
		p.hx# = x - p.mypin.x + p.hx
		p.hy# = y - p.mypin.y + p.hy
		p.mypin = pp
	End Method
	
	Method draw()
		SetImageHandle img , hx , hy
		SetRotation an+mypin.rot
		DrawImage img , mypin.x , mypin.y
		SetRotation 0
		'SetColor 255,255,0
		'DrawOval mypin.x - 5 , mypin.y - 5 , 10 , 10
		'SetColor 255,255,255
	End Method
	
	Method drag(dx# , dy#)
		'DrawRect dx , dy , 5 , 5
		'DrawRect 0,0,10,10
		If anchored
			an#=ATan2(dy-mypin.y,dx-mypin.x)-offan
		Else
			mypin.x = dx-offx
			mypin.y = dy-offy
		EndIf
	End Method
	
	Method inside(dx# , dy#)
		x1# = mypin.x - hx
		y1# = mypin.y - hy
		x2# = x1 + w
		y2# = y1 + h
		If dx >= x1 And dx <= x2 And dy >= y1 And dy <= y2
			Return 1
		Else
			Return 0
		EndIf
	End Method
	
End Type

Type pin
	Field an# , d#
	Field x# , y#
	Field rot#

	Function create:pin(an# , d#)
		pp:pin = New pin
		pp.an = an
		pp.d = d	
		Return pp
	End Function
End Type

win:tgadget = CreateWindow("Gilliamoid" , 0 , 0 , 800 , 800 , Null , WINDOW_TITLEBAR | WINDOW_ACCEPTFILES)
canvas:tgadget=CreateCanvas(0,0,800,800,win)
CreateTimer 60

current:piece = Null
state = 0
Global offx#,offy#,offan#

Global mousestate[4]

While WaitEvent()
	Select EventID()
		Case EVENT_TIMERTICK
			
			For p:piece = EachIn pieces
				p.update()
			Next		
		
			Select state
			Case 0 'nowt
				current=Null
				For p:piece = EachIn pieces
					If p.inside(mx,my)
						current = p
						offx = mx - p.mypin.x
						offy = my - p.mypin.y
						offan#=ATan2(offy,offx)-p.an
					EndIf
				Next
				If mousestate[1]
					If current
						state = 1
					EndIf
				EndIf
				
			Case 1 'dragging
				current.drag(mx, my)
				If Not mousestate[1]
					state = 0
				EndIf
			End Select
			
			RedrawGadget canvas

		Case EVENT_GADGETPAINT
			SetGraphics CanvasGraphics(canvas)
			For p:piece = EachIn pieces
				p.draw()
			Next
			Flip
			Cls
			
		Case EVENT_MOUSEMOVE
			mx = EventX()
			my = EventY()
			
		Case EVENT_MOUSEDOWN
			mousestate[EventData()] = 1
		Case EVENT_MOUSEUP
			mousestate[EventData()] = 0
			If EventData()=2
				If current
					If current.anchored
						under:piece=Null
						For p:piece = EachIn pieces
							If p <> current And p.inside(mx,my)
								under = p
							EndIf
						Next
						If under
							current.attach(under , mx , my) 
						Else
							current.anchored=0
						EndIf
					Else
						current.anchored = 1
						under:piece=Null
						For p:piece = EachIn pieces
							If p <> current And p.inside(mx,my)
								under = p
							EndIf
						Next
						If under
							under.attach(current,mx,my)
						Else
							current.hx = mx - current.mypin.x + current.hx
							current.hy = my - current.mypin.y + current.hy
							current.mypin.x = mx
							current.mypin.y = my
						EndIf
					EndIf
				EndIf
			EndIf
						
		Case EVENT_WINDOWACCEPT
			path$ = EventText()
			typ$ = path[Len(path) - 3..]
			If typ = "jpg" Or typ = "bmp" Or typ = "gif" Or typ="png"
				img:timage = LoadImage(path)
			EndIf
			p:piece = piece.create(img , mx,my)

		Case EVENT_WINDOWCLOSE
			FreeGadget canvas
			End

		Case EVENT_APPTERMINATE
			End
	End Select
Wend

