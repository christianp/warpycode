Include "trader.bmx"
Include "jsondecoder.bmx"
Include "lettermaker.bmx"
Include "grammar.bmx"
Include "helpers.bmx"
Include "globals.bmx"
Include "mapping.bmx"
Include "graphics.bmx"


Type gamemode
	Method arrive()
	End Method

	Method update()
	
	End Method
End Type

Function changemode(gm:gamemode)
	If curmode oldmodes.addlast curmode
	nextmode=gm
End Function

Function leavemode()
	If oldmodes.count()
		nextmode=gamemode(oldmodes.removelast())
	Else
		curmode=Null
	EndIf
End Function


changemode New tradermode

oldmodes=New TList
loadfonts
loadtemplates
Loadmap
normalfont:TImageFont = LoadImageFont("incbin::consolai.ttf", 16)
initgame
loadworld(ReadFile("world3.txt"))
sleep
checkmail


While curmode Or nextmode
	If nextmode
		curmode=nextmode
		curmode.arrive
		nextmode=Null
	EndIf
	curmode.update
	
	If AppTerminate()
		curmode=Null
	EndIf
Wend


Print "end!"
End