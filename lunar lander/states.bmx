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
	
	Method New()
		menu=mainmenu
	End Method
	
	Method update()
	
	End Method
	
	Method draw()
		menu.draw
	End Method
End Type