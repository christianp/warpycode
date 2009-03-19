Global state:tstate

Type tstate
	Function init()
		state=Null
	End Function

	Method update()
	End Method
	
	Method draw()
	End Method
End Type


Type menustate Extends tstate
	Field menu:tmenu
	Field prvmenu:tmenu
	
	Method New()
		menu=mainmenu
	End Method
	
	Method update()
		menu.update
		If menubackinput.hit()
			If prvmenu
				menu=prvmenu
			Else
				endgame
			EndIf
		EndIf
	End Method
	
	Method draw()
		menu.draw
	End Method
End Type


Type gamestate Extends tstate

	Method New()
		newgame
	End Method
	
	Method update()
		updategame
	End Method
	
	Method draw()
		drawgame
	End Method
End Type