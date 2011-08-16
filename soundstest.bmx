Graphics 800,800,0

Local sounds:TSound[]=[LoadSound("broomingstart2.wav"),LoadSound("bigwhitebroom.wav",True),LoadSound("broomingend2.wav")]

chan:TChannel=AllocChannel()
state=0
SetChannelVolume chan,0
PlaySound sounds[1],chan
While Not KeyHit(KEY_ESCAPE)
	ox=mx
	oy=my
	mx=MouseX()
	my=MouseY()
	v#=Sqr((mx-ox)^2+(my-oy)^2)

	volume#=volume*.8
	SetChannelVolume chan,volume*.4

	If MouseDown(1)
		volume:+(v/200.0)*.2
		'If Not ChannelPlaying(chan) Then PlaySound sounds[1],chan
	EndIf
	DrawText v,0,0
	Flip
	Cls
Wend