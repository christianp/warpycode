Include "graphics.bmx"
Include "states.bmx"
Include "menu.bmx"
Include "game.bmx"
Include "input.bmx"
Include "maths.bmx"

Function initgame()
	tinput.init
	tmenu.init
	state=New menustate
	SeedRnd MilliSecs()
End Function

Function initgfx()
	gwidth=800
	gheight=800
	AppTitle="cp's boring Lunar Lander"
	Graphics gwidth,gheight,0
	SetBlend ALPHABLEND
	AutoMidHandle True
End Function

initgfx
initgame

ms=MilliSecs()
While 1
	state.update
	state.draw
	
	'y=0
	For ti:tinput=EachIn tinputs
		ti.update
		'DrawText ti.repr(),0,y
		'y:+15
	Next
	
	If AppTerminate() Or KeyHit(KEY_ESCAPE)
		endgame
	EndIf
	
	oms=ms
	ms=MilliSecs()
	fps#=1000.0/(ms-oms)
	DrawText Int(fps),gwidth-20,0
	
	Flip
	Cls
Wend

Function endgame()
	Print "Fin."
	End
End Function