Global keynames$[]=["", "", "", "", "", "", "", "", "Backspace", "Tab", "", "", "Clear", "Enter", "Return", "", "", "", "", "Pause", "Caps Lock", "", "", "", "", "", "", "Escape", "", "", "", "", "Space", "Page Up", "Page Down", "End", "Home", "Cursor (Left)", "Cursor (Up)", "Cursor (Right)", "Cursor (Down)", "Select", "Print", "Execute", "Screen", "Insert", "Delete", "Help", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "", "", "", "", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "Sys key (Left)", "Sys key (Right)", "", "", "", "Numpad 0", "Numpad 1", "Numpad 2", "Numpad 3", "Numpad 4", "Numpad 5", "Numpad 6", "Numpad 7", "Numpad 8", "Numpad 9", "Numpad *", "Numpad +", "", "Numpad -", "Numpad .", "Numpad /", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "Num Lock", "Scroll Lock", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "Shift (Left)", "Shift (Right)", "Control (Left)", "Control (Right)", "Alt key (Left)", "Alt key (Right)", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "Semi-colon", "Equals", "Comma", "Minus", "Period", "Slash", "Tilde", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "Bracket (Open)", "", "Bracket (Close)", "Quote", "", "", "", "Backslash", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""]
Global mousenames$[]=["Mouse Left","Mouse Right","Mouse Middle"]

Const input_keyboard=0
Const input_mousebutton=1
Const input_mousepos=2
Global tinputs:TList
Type tinput
	Field kind
	Field port
	Field code
	Field _value
	Field reset
	Field _hit
	Field hittable
	
	Function init()
		tinputs=New TList
	End Function
	
	Function Create:tinput(kind,code,port=0,reset=1)
		For ti:tinput=EachIn tinputs
			If ti.kind=kind And ti.code=code And ti.port=port And ti.reset=reset
				Return ti
			EndIf
		Next
		
		ti:tinput=New tinput
		ti.kind=kind
		ti.code=code
		ti.port=port
		ti.reset=reset
		tinputs.addlast ti
		'Print ti.repr()
		Return ti
	End Function
	
	Method value()
		Return _value
	End Method
	
	Method hit()
		b=_hit
		_hit=0
		Return b
	End Method
	
	Method update()
		ovalue=_value
		If reset _value=0
		Select kind
		Case input_keyboard
			_value=KeyDown(code) Or _value
		Case input_mousebutton
			_value=MouseDown(code) Or _value
		Case input_mousepos
			Select code
			Case 0 'X
				_value:+mousemovex
			Case 1 'Y
				_value:+mousemovey
			Case 2 'Z
				_value:+mouvemovez
			End Select
		End Select
		
		If (Not _value)
			hittable=1
		ElseIf hittable And _value
			_hit=1
			hittable=0
		EndIf
	End Method
	
	Function choose:tinput()
		For i=1 To 255
			If KeyDown(i)
				Return tinput.Create(input_keyboard,i)
			EndIf
		Next
		For i=1 To 3
			If MouseDown(i)
				Return tinput.Create(input_mousebutton,i)
			EndIf
		Next
		If Abs(mousemovex)>10
			Return tinput.Create(input_mousepos,0)
		EndIf
		If Abs(mousemovey)>10
			Return tinput.Create(input_mousepos,1)
		EndIf
		If mousemovez
			Return tinput.Create(input_mousepos,2)
		EndIf
	End Function
	
	Method repr$()
		Select kind
		Case input_keyboard
			txt$=code+" : "+keynames[code]
		Case input_mousebutton
			txt$=mousenames[code-1]
		Case input_mousepos
			Select code
			Case 0
				txt$="Mouse X"
			Case 1
				txt$="Mouse Y"
			Case 2
				txt$="Mouse Z"
			End Select
		End Select
		Return txt+" > "+_value+","+_hit
	End Method
End Type

Type manyinput Extends tinput
	Field inputs:tinput[]
	
	Method New()
		tinputs.addlast Self
	End method

	Function combine:manyinput(inputs:tinput[])
		ti:manyinput=New manyinput
		ti.inputs=inputs
		Return ti
	End Function
	
	Method addinput(ti:tinput)
		Print "  "+ti.repr()
		inputs:+[ti]
	End Method
	
	Method hit()
		For ti:tinput=EachIn inputs
			ti.hit()
		Next
		Return _hit
	End Method
	
	Method update()
		_value=0
		_hit=0
		For ti:tinput=EachIn inputs
			_value=_value Or ti.value()
			_hit=_hit Or ti._hit
		Next
	End Method
	
	Method repr$()
		name$=""
		For ti:tinput=EachIn inputs
			If name name:+", "
			name:+ti.code
		Next
		Return "many "+name+" > "+_value
	End Method
End Type

Global mx,my,mz,mousemovex,mousemovey,mousemovez
Function pollmouse()
	nmx=MouseX()
	nmy=MouseY()
	nmz=MouseZ()
	mousemovex=nmx-mx
	mousemovey=nmy-my
	mousemovez=nmz-mz
	mx=nmx
	my=nmy
	mz=nmz
End Function

Rem
Graphics 600,600,0
tinput.init
quitkey:tinput=tinput.Create(input_keyboard,KEY_ESCAPE)
While Not quitkey.value()
	y=0
	For ti:tinput=EachIn tinputs
		ti.update
		DrawText ti.repr(),0,y
		y:+15
	Next
	newti:tinput=tinput.choose()
	If newti
	EndIf
	
	pollmouse
	
	DrawLine 300,300,300+Cos(MilliSecs())*100,300+Sin(MilliSecs())*100
	Flip
	Cls
Wend
endrem