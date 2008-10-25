'Server
Graphics 320,240,0
Local My_server:TSocket=CreateTCPSocket()
BindSocket My_server,12345
SocketListen My_server


Repeat 
	Local client:TSocket=SocketAccept( My_server )
	If client
		stream:Tsocketstream = CreateSocketStream(client)
		Print DottedIP(SocketRemoteIP(client))+" is Connected"
		
		Repeat
		
			If SocketReadAvail(client)
				foo$ = ReadLine$(stream)
			EndIf
			DrawText foo$,0,0
			Flip ; Cls
			
		Until KeyHit(KEY_ESCAPE)
		
	EndIf
	
	DrawText "!",0,0
	
	
	
	
	Flip ; Cls
Until KeyHit(KEY_ESCAPE)



