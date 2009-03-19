Global joyN=0

Function assignjoy()
	If joyN>=JoyCount() Return -1
	joyN:+1
	Return joyN-1
End Function

Function SafeJoyX#(joy)
	jx#=JoyX(joy)
	If Abs(jx)<.2 Return 0
	Return jx
End Function

Function SafeJoyY#(joy)
	jy#=JoyY(joy)
	If Abs(jy)<.2 Return 0
	Return jy
End Function

