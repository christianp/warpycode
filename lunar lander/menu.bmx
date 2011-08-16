Global mainmenu:tmenu=tmenu.Create( [funcmenuitem.Create("New Game",menunewgame), treemenuitem.Create("Options",Null), funcmenuitem.Create("Quit", menuquitgame)] )
Global menuupinput:manyinput
Global menudowninput:manyinput
Global menudoinput:manyinput
Global menubackinput:manyinput

Type tmenu
	Field items:menuitem[]
	Field selected
	
	Function init()
		menuupinput=manyinput.combine([tinput.Create(input_keyboard,KEY_UP),tinput.Create(input_keyboard,KEY_LEFT)])
		menudowninput=manyinput.combine([tinput.Create(input_keyboard,KEY_DOWN),tinput.Create(input_keyboard,KEY_RIGHT)])
		menudoinput=manyinput.combine([tinput.Create(input_keyboard,KEY_ENTER),tinput.Create(input_keyboard,KEY_SPACE)])
		menubackinput=manyinput.combine([tinput.Create(input_keyboard,KEY_ESCAPE)])
	End Function
	
	Function Create:tmenu(items:menuitem[])
		m:tmenu=New tmenu
		m.items=items
		m.selected=0
		Return m
	End Function
	
	Method draw()
		x=100
		y=100
		For i=0 To Len(items)-1
			item:menuitem=items[i]
			If i=selected
				SetColor 0,0,255
			Else
				SetColor 255,255,255
			EndIf
			DrawText item.label,x,y
			y:+50
		Next
	End Method
	
	Method update()
		If menuupinput.hit()
			selected:-1
			If selected<0 selected=Len(items)-1
		EndIf
		If menudowninput.hit()
			selected:+1
			If selected=Len(items) selected=0
		EndIf
		items[selected].update
	End Method
	
End Type

Type menuitem
	Field label$
	
	Method update()
	End Method
	
End Type

Type treemenuitem Extends menuitem
	Field nxt:tmenu
	Field direction

	Function Create:menuitem(label$,nxt:tmenu,direction=1)
		item:treemenuitem=New treemenuitem
		item.label=label
		item.nxt=nxt
		item.direction=direction
		Return item
	End Function
	
	Method do()
		ms:menustate=menustate(state)
		If direction ms.prvmenu=ms.menu
		If nxt ms.menu=nxt
	End Method
	
	Method update()
		If menudoinput.hit()
			do
		EndIf
	End Method
End Type

Type funcmenuitem Extends menuitem
	Field func()
	
	Function Create:menuitem(label$,func())
		item:funcmenuitem=New funcmenuitem
		item.label=label
		item.func=func
		Return item
	End Function
	
	Method update()
		If menudoinput.hit()
			func
		EndIf
	End Method
End Type


Function menunewgame()
	state=New gamestate
End Function

Function menuquitgame()
	Print "quit game"
End Function
