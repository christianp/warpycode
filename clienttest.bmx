'Const LOCALHOST = 2130706433
Graphics 320,240,0

Global LHOST = Dotted_to_integer("127.0.0.1")


Local client:TSocket=CreateTCPSocket()
ConnectSocket client,LHOST,12345

If client
	stream:Tsocketstream = CreateSocketStream(client)
	' WriteLine(stream,"Hello World")
Else 
	Print "cant connect to server"
EndIf

Local sendstring$ = ">"
Repeat
		
	keytimer:-1
	If keytimer < 0
		stroke = GetChar()
		
		Select stroke 
			Case 0
				' do nothing
			Case KEY_BACKSPACE
				sendstring$ = sendstring$[0..Len(sendstring)-1]
			Case KEY_RETURN 
				WriteLine(stream,sendstring$)
				sendstring$ = ">"
				'sendstring$:+"[R]"
			Default sendstring$:+Chr(stroke)
		End Select
		keytimer = 10
	EndIf
	
	DrawText sendstring$,0,0
	

	


Flip ; Cls

Until KeyHit(KEY_ESCAPE)

Function Dotted_to_integer(Dotted_ip$)
		Dotted_ip$:+"."
		Local octet:String[4]
		For i = 0 To 3
			dot =  Dotted_ip$.find(".") ; octet[i] =  Dotted_ip$[0..dot]
			Dotted_ip$ = Dotted_ip$[dot+1..]
		Next
	Return (octet[0].toint() Shl 24) + (octet[1].toint() Shl 16) + (octet[2].toint() Shl 8) + octet[3].toint()
End Function 






