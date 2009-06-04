'based on James Boyd's BlitzServe 2

Include "regexp.bmx"
Include "list filter.bmx"
Include "iterator.bmx"

Include "session.bmx"
Include "routes.bmx"
Include "render_shortcuts.bmx"
Include "template.bmx"
Include "template_parsing.bmx"


'Global ThreadListMutex:TMutex = CreateMutex ()
'Global ThreadList:TList = CreateList ()
'Local thread:TThread

'http request object


'Global datamutex=CreateMutex()


Rem
Global folder:String = "C:\blitzmax\docs\html"
Function GetPath(hr:HTTPRequest, response:HTTPResponse)
	Local file$=hr.path[6..]
	Print "GetPath "+file
	If file <> ""

		' Convert any %xx (Hex) codes in URL to Chr (ascii) character...
		' (Eg. %20 is ascii 57, ie. Chr (57), ie. a Space.)
		
		file = UnHexURL (file)

		If file[0] <> Asc("/")
			file = "/" + file ' Strip leading "/" if any...
		EndIf

		' -------------------------------------------------------------
		' Does the requested file exist in our 'site' folder?
		' -------------------------------------------------------------

		Select FileType (folder + file)
		
			Case 0 ' File does not exist...
				response.status="404 Not Found"
				Return
			Case 1 ' File exists!
				response.status="200 OK"

					Local requested:TStream = ReadFile (folder + file)

					If requested

						Try
				
							response.body=requested.ReadString(requested.size())

							If requested
								CloseFile requested
							EndIf
	
						Catch error:Object
					
							' This can be triggered during file read/write While/Wend loop,
							' for reasons unknown (socket lost just after socket and stream
							' checked valid?)...
							
							Return
						
						End Try
				
					EndIf
				
			Case 2 ' Folder...
				response.status="200 OK"

				' You could return a list of files here, like an "Index of xyz/" page, or an error page...
				
				response.body="That's a folder, whoops"				
		End Select
		
	EndIf
	
End Function
EndRem

' -----------------------------------------------------------------------------
' Threaded file serve function...
' -----------------------------------------------------------------------------

Function ProcessConnection:Object (obj:Object)

	Local c:Connection = New Connection
	c.socket = TSocket (obj)
	
	If SocketConnected (c.socket)

		Print "~nRequest from " + DottedIP (SocketRemoteIP (c.socket))
		
		c.stream = CreateSocketStream (c.socket)

	Else
		
		Print "ERROR: No stream created from socket!"
		
		' No stream was created -- this can happen!

		KillConnection c
		
		Return Null

	EndIf
	
	Local eoc:Int
	Local eop:Int

	Local s:HTTPSession=HTTPSession.Create(c)
	
	Local incoming:String
	Local command:String
	Local parameter:String
	
	incoming=c.ReadLine()
	'Print ">>>"+incoming
	Local bits$[]=incoming.split(" ")
	s.req=New HTTPRequest.Create(bits[0],bits[1],bits[2])	

	Repeat
	
		If SocketConnected (c.socket)
		
			incoming = c.ReadLine()
			
		Else
		
			KillConnection c
			
			Return Null
			
		EndIf

		If incoming <> ""

			eoc = incoming.find(":")		' End of command part of incoming
			command = Lower (incoming[..eoc])		' Command part of incoming
			parameter = Trim(incoming[eoc+2..])	' Parameter part of incoming
			s.req.headers.insert command,parameter
		EndIf
			

	Until incoming = "" ' Got blank line after headers, so all done here...


	If s.req.headers.contains("cookie")
		cookie$=s.req.header("cookie")
		Local cookies$[]=cookie.split(";")
		For cookie=EachIn cookies
			bits=Trim(cookie).split("=")
			s.req.cookies.insert bits[0],"=".join(bits[1..])
		Next
	EndIf

	Print ""
	Local size=Int(s.req.header("content-length"))
	Print "body size: "+size
	s.req.body=c.stream.ReadString(size)
	If s.req.header("content-type")="application/x-www-form-urlencoded"
		bits=s.req.body.split("&")
		For bit$=EachIn bits
			Local i=bit.find("=")
			s.req.data.insert Trim(Lower(bit[..i])),unhexurl(Replace(bit[i+1..],"+"," "))
		Next
	EndIf

	If init
		init s
	endif
	s.Respond

	KillConnection c
	
	Return Null
	
End Function



' -----------------------------------------------------------------------------
' Each connection has a socket and an associated stream to read from...
' -----------------------------------------------------------------------------

Type Connection
	Field socket:TSocket
	Field stream:TSocketStream
	
	Method WriteLine(line$)
		Try
			stream.WriteLine line
		
		Catch error:Object
			Print "ERROR: Received non-HTTP 1.1 request, but stream failed outside error-checking!"
			
			KillConnection Self
			
			Return
		End Try
	End Method
	
	Method ReadLine$()
		Local line$
		If Not Eof(stream)
			line=stream.ReadLine()
			Print line
		EndIf
		Return line
	End Method
End Type

' Made separate as it's referenced a lot...

Function KillConnection (c:Connection)
	If SocketConnected (c.socket) Then CloseSocket c.socket
	If c.stream And Not Eof (c.stream) Then CloseStream c.stream
End Function

' Helper functions...

Function UnHexURL:String (url:String)
	Local pos:Int
	Repeat
		pos = Instr (url, "%")
		If pos
			Local hexx:String = Mid (url$, pos, 3)
			url = Replace (url, hexx, Chr (HexToDec (hexx)))
		EndIf
	Until pos = 0
	Return url
End Function

Function HexToDec:Int (h:String)

	' From PureBasic code by 'PB'...

	If h[0]=Asc("%") Then h=h[1..]
	h = Upper (h)

	Local a:String
	Local d:Int

	For Local r:Int = 1 To Len (h)
		d = d Shl 4; a = Mid (h, r, 1)
		If Asc (a) > 60
			d = d + Asc (a) - 55
		Else
			d = d + Asc (a) - 48
		EndIf
	Next
	
	Return d
	
End Function




Type tserver
	Field socket:tsocket
	Field init(s:httpsession)
	
	Function Create:tserver(port=80,init(s:httpsession)=Null)
		Local server:TSocket = CreateTCPSocket ()
		
		If server
		
			If BindSocket (server, port)
		
				SocketListen server, 5
				
				Print "server started and running and going"
				s:tserver=New tserver
				s.socket=server
				s.init=init
				Return s	
			Else
				Print "Couldn't bind to port 80!"
			EndIf
			
		Else
		
			Print "Couldn't create server!..."
			
		EndIf
	End Function

	Method run()
		Repeat
			Local remote:TSocket = SocketAccept (socket)
			If remote
				processconnection remote
			EndIf
			Rem
			If remote
				thread = CreateThread (ProcessConnection, remote)
				ListAddLast ThreadList, thread	
			EndIf
			
			For thread = EachIn ThreadList
				If Not ThreadRunning (thread)
					ListRemove ThreadList, thread
				EndIf
			Next
			EndRem
	
			Delay 10
			'Cls
			'DrawText "Direct ESC to this window, not IDE!", 20, 20
			'Flip
			
		Forever
	
						
				
		Print ""
		Print "Waiting for connections to close..."
		
		Rem
		LockMutex ThreadListMutex
			For thread = EachIn ThreadList
				WaitThread thread
			Next
		UnlockMutex ThreadListMutex
		EndRem
		
		CloseSocket socket
	End Method
	
End Type
