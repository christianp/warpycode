Global sounditems:TList=New TList
SetAudioDriver "DirectSound"

Type sounditem
	Field chan:TChannel
	Field loops
	Field sound:TSound
	
	Function Create:sounditem(sound:TSound,rate#,pan#,volume#,loops=0)
		si:sounditem=New sounditem
		si.chan=AllocChannel()
		SetChannelRate(si.chan,rate)
		SetChannelPan(si.chan,pan)
		SetChannelVolume si.chan , volume
		si.sound = sound
		si.loops=loops
		PlaySound(si.sound,si.chan)
		sounditems.AddLast si
		Return si
	End Function
	
	Method Delete()
		StopChannel chan
	End Method

	Method update()	
		If Not ChannelPlaying(chan)
			If loops
				PlaySound sound , chan
			Else
				sounditems.Remove sound
			EndIf
		EndIf
	End Method
	
End Type



While Not 1
	
	For sound:sounditem=EachIn sounditems
		sound.update()
	Next
Wend