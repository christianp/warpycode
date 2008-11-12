Include "graphics.bmx"
Include "states.bmx"


Function initgame()
	state.init
	
	state=New menustate
End Function


initgfx
initgame

While 1
	state.update
	state.draw
	Flip
	Cls
Wend