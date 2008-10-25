Type sceneobj
	Field img:timage
	Field scale#
	Field x#,y#
End Type

win:TGadget = CreateWindow("Drag 'n' Drop", 0, 0, 800, 800, Null, WINDOW_TITLEBAR | WINDOW_ACCEPTFILES) 
Canvas:TGadget = CreateCanvas(0, 0, 800, 800, win) 
CreateTimer 60
AutoMidHandle True

Global pieces:TList = New TList
Type piece
	Field x:Float, y:Float, img:TImage
	
	Method New() 
		pieces.AddLast Self
	End Method
	
	Function Create:piece(img:TImage, x:Float, y:Float) 
		p:piece = New piece
		p.img = img
		p.x = x
		p.y = y
		Return p
	End Function
	
	Method draw() 
		DrawImage img, x, y
	End Method
End Type

While WaitEvent()
	Select EventID()
		Case EVENT_TIMERTICK
			RedrawGadget Canvas

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

		Case EVENT_MOUSEUP

		Case EVENT_WINDOWACCEPT
			path$ = EventText()
			typ$ = path[Len(path) - 3..]
			If typ = "jpg" Or typ = "bmp" Or typ = "gif" Or typ="png"
				img:timage = LoadImage(path)
			EndIf
			p:piece = piece.Create(img , mx,my)

		Case EVENT_WINDOWCLOSE
			FreeGadget canvas
			End

		Case EVENT_APPTERMINATE
			End
	End Select
Wend

