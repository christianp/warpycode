Import maxgui.maxgui		'import maxgui
Import maxgui.win32maxgui

Type timelineitem
	Field starttime, endtime
	Field y
	
	Method New()
		timelineitems.AddLast Self
	End Method
	
	Method draw()
		x1 = starttime - timelinescroll
		y1 = y * 40 + 2 - timelineupscroll
		x2 = endtime - timelinescroll
		y2 = y * 40 + 36 - timelineupscroll
		SetColor 255, 255, 255
		SetAlpha 0.5
		DrawRect x1, y1, x2 - x1, y2 - y1
		SetAlpha 1
		DrawLine x1, y1, x2, y1
		DrawLine x1, y1, x1, y2
		DrawLine x1, y2, x2, y2
		DrawLine x2, y1, x2, y2
	End Method
	
	Method update(time)
		If time >= starttime And time <= endtime
			DrawLine starttime - timelinescroll, y * 40 + 20 - timelineupscroll, time - timelinescroll, y * 40 + 20 - timelineupscroll
		End If
	End Method
	
	Method movex(newx)
		If newx < 0 newx = 0
		length = endtime - starttime
		If newx + length > timelinewidth Then newx = timelinewidth - length
		diffx = newx - starttime
		If diffx < 0
			For ti:timelineitem = EachIn timelineitems
				If ti.y = y And starttime > ti.endtime And newx <= ti.endtime
					ti.movex(ti.starttime + newx - ti.endtime)
					newx = ti.endtime + 1
				End If
			Next
		Else
			For ti:timelineitem = EachIn timelineitems
				If ti.y = y And endtime < ti.starttime And newx + length >= ti.starttime
					ti.movex(newx + length + 1)
					newx = ti.starttime - 1 - length
				End If
			Next
		End If
		starttime = newx
		endtime = starttime + length
	End Method
	
	Method movey(newy)
		If newy < 0 newy = 0
		If newy > timelineheight / 40 - 1 Then newy = timelineheight / 40 - 1
		go = 1
		For ti:timelineitem = EachIn timelineitems
			If ti.y = newy And ((ti.starttime >= starttime And ti.starttime <= endtime) Or (starttime >= ti.starttime And starttime <= ti.endtime))
				go = 0
			EndIf
		Next
		If go
			y = newy
		End If
	End Method
End Type

'------------gui creation

Global timelinestate = 0
Global myti:timelineitem
Global tlmx, tlmy
Global dragoffset
Global time
Global timelinescroll, timelineupscroll
Global timelineitems:TList = New TList
Global numtimelineslots = 10
Global timelinewidth = 2000, timelineheight = numtimelineslots * 40, timelinewinwidth = 800, timelinewinheight = 120
Global timelinewindow:TGadget
Global timelineslider:TGadget
Global timelineupslider:TGadget
Global timelinecanvas:TGadget

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


'-------test stuff

AddHook EmitEventHook, eventhook
CreateTimer 60
EnablePolledInput()



While Not KeyHit(KEY_ESCAPE)
Wend
End

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


'--------hook function

Function eventhook:Object(id, data:Object, context:Object)
	Local event:TEvent = TEvent(data)
	
	Select event.id
		Case EVENT_WINDOWCLOSE
			End
		Case EVENT_MOUSEMOVE
			tlmx = Event.x + timelinescroll
			tlmy = Event.y + timelineupscroll
		Case EVENT_MOUSEDOWN
			Select timelinestate
			Case 0
				y = tlmy / 40
				myti:timelineitem = Null
				For tib:timelineitem = EachIn timelineitems
					If y = tib.y And tlmx >= tib.starttime And tlmx <= tib.endtime
						myti = tib
					EndIf
				Next
				If myti
					timelinestate = 2
					dragoffset = myti.starttime - tlmx
				Else
					timelinestate = 1
					myti:timelineitem = New timelineitem
					myti.starttime = tlmx
					myti.endtime = myti.starttime
					myti.y = tlmy / 40
				EndIf
			End Select
		Case EVENT_MOUSEUP
			Select timelinestate
			Case 1
				timelinestate = 0
			Case 2
				timelinestate = 0
			End Select
		Case EVENT_MOUSEWHEEL
			SetSliderValue timelineupslider, SliderValue(timelineupslider) - 5 * event.data
		Case EVENT_KEYUP
			Select event.data
			Case KEY_TAB
				Select timelinestate
				Case 0
					Print "PLAYING"
					timelinestate = 3
					time = 0
				Case 3
					timelinestate = 0
				End Select
			End Select
		Case EVENT_TIMERTICK
			DrawText timelinestate, 0, 0
			Select timelinestate
			Case 0 'normal
				
			Case 1 'making element
				endtime = tlmx
				If endtime > myti.starttime
					myti.endtime = endtime
				Else
					myti.endtime = myti.starttime
				EndIf
				scrollxto tlmx
			Case 2 'dragging element
				newx = tlmx + dragoffset
				myti.movex(newx)
				
				y = tlmy / 40
				myti.movey(y)

				scrollxto tlmx
				scrollyto y
			Case 3 'playing
				playing = 0
				If time - timelinescroll > timelinewinwidth / 2
					SetSliderValue timelineslider, time - timelinewinwidth / 2
				End If
				For ti:timelineitem = EachIn timelineitems
					ti.update(time)
					If time <= ti.endtime Then playing:+1
				Next
				time:+1
				If Not playing
					timelinestate = 0
				End If
			End Select
			RedrawGadget timelinecanvas

		Case EVENT_GADGETPAINT
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
			
			If timelinestate = 3
				SetColor 255, 0, 0
				DrawLine time - timelinescroll, 0, time - timelinescroll, timelinewinheight
			End If
			
			Flip
			Cls
	EndSelect
	Return data
End Function
