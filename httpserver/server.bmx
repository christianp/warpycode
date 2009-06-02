' IMPORTANT: Make sure "Threaded Build" is enabled in the Program -> Build Options menu!

' -----------------------------------------------------------------------------
' BlitzServe 2 -- a very crude multithreaded HTTP server...
' -----------------------------------------------------------------------------

'SuperStrict
Include "regexp.bmx"

' -----------------------------------------------------------------------------
' Set this to a folder on your hard drive containing the files to be served:
' -----------------------------------------------------------------------------

Global folder:String = "C:\blitzmax\docs\html"

' -----------------------------------------------------------------------------
' To test...
' -----------------------------------------------------------------------------

' 1) Run the program and launch a web browser;

' 2) In the browser's address bar, type 127.0.0.1 plus the file name, eg.

'		http://127.0.0.1/index.html
'		http://127.0.0.1/h0tchix.jpg

'	Don't type the folder name!

' 3) If your firewall complains, you need to unblock/allow this program!

' 4) Repeatedly hit F5/Refresh in your browser to see the server handle
'    cut-off requests (you'll see ERROR: xyz messages).

' -----------------------------------------------------------------------------

' -----------------------------------------------------------------------------
' OK...
' -----------------------------------------------------------------------------

AppTitle = "BlitzServe 2..."

' -----------------------------------------------------------------------------
' Remove trailing slash from server's folder (HTTP GET command prefixes file
' name with a forward slash)...
' -----------------------------------------------------------------------------

If (Right (folder, 1) = "/") Or (Right (folder, 1) = "\")	
	folder = Left (folder, Len (folder) - 1)
EndIf

If FileType (folder) = 0
	RuntimeError "Set folder:String to a folder on your computer!"
EndIf

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


'pattern matching stuff
Global patternMutex:TMutex=CreateMutex()
Global patterns:tmap=New tmap
responder.Create "/file/.*",GetPath
responder.Create "/user/[A-Za-z]+",GetUser

Type responder
	Field func(hr:HTTPRequest,response:HTTPResponse)
	Function Create:responder(pattern$,func(hr:HTTPRequest,response:HTTPResponse))
		r:responder=New responder
		r.func=func
		patterns.insert fsa.Create(pattern),r
	End Function
	
	Function Find:responder(path$)
		LockMutex patternMutex
		Print "MATCH" +path
		For f:fsa=EachIn patterns.keys()
			If f.evaluate(path)
				Print "MATCHED!"
				r:responder=responder(patterns.valueforkey(f))
				UnlockMutex patternMutex
				Return r
			EndIf
		Next
		UnlockMutex patternMutex
	End Function
End Type




' -----------------------------------------------------------------------------
' Thread list and mutex for safe access...
' -----------------------------------------------------------------------------

Global ThreadListMutex:TMutex = CreateMutex ()
Global ThreadList:TList = CreateList ()

' -----------------------------------------------------------------------------
' Temporary graphics window...
' -----------------------------------------------------------------------------

' This is just to allow keyboard input to be captured. Don't press ESC on
' the IDE output, as that just terminates the program. Press ESC with the
' graphics window highlighted so the program can close all connections. It won't
' cause any harm if you don't -- this is just for completeness' sake...

' Note that in real life, this would just be running in an infinite loop
' so there would be no need to test for ESC!

Graphics 320, 200

' -----------------------------------------------------------------------------
' Just a local pointer
' -----------------------------------------------------------------------------

Local thread:TThread

' -----------------------------------------------------------------------------
' Create HTTP server (always on port 80)...
' -----------------------------------------------------------------------------

Local server:TSocket = CreateTCPSocket ()

If server

	' Bind to port 80...
	
	If BindSocket (server, 80)

		' Start listening for incoming connections on port 80...
		
		' NOTE: Don't use default 0 for backlog parameter, as this will cause
		' web pages to fail occasionally. The web browser will be requesting
		' all the files in any HTML page you request, and a 0-sized backlog
		' won't process the requests quickly enough. If the backlog isn't
		' cleared between SocketListen and the call to SocketAccept, the
		' request will fail, meaning images, etc, fail to display.
		
		' 5 is an old Windows standard, but there is lots of disagreement
		' as to what it should be, and different use scenarios. A server
		' expecting to be Slashdotted will require a much larger amount,
		' while a lowly desktop serving local files like this won't need
		' as much...
		
		' More information here:
		
		' http://tangentsoft.net/wskfaq/advanced.html#backlog
		' http://stackoverflow.com/questions/114874/socket-listen-backlog-parameter-how-to-determine-this-value
		' http://patchwork.kernel.org/patch/2297/
		
		SocketListen server, 5
		
		Print
		Print "BlitzServe 2: awaiting incoming connections..."
		Print
		Print "Launch your web browser and direct it to http://127.0.0.1/myfilename.html"
		Print "where myfilename.html is a file inside the folder you specified..."
		Print ""
		
		Repeat
		
			' -------------------------------------------------------------------------
			' See if there's been an incoming connection attempt...
			' -------------------------------------------------------------------------
		
			Local remote:TSocket = SocketAccept (server)

			If remote
				thread = CreateThread (ProcessConnection, remote)
				ListAddLast ThreadList, thread	
			EndIf
			
			For thread = EachIn ThreadList
				If Not ThreadRunning (thread)
					ListRemove ThreadList, thread
				EndIf
			Next

			' -------------------------------------------------------------------------
			' Don't wanna hog CPU (also update temp graphics window)...
			' -------------------------------------------------------------------------
		
			' Graphics/Cls/Flip not needed in a real server!
			
			Delay 10
			Cls
			DrawText "Direct ESC to this window, not IDE!", 20, 20
			Flip
			
		Until KeyHit (KEY_ESCAPE)
		
		Print ""
		Print "Waiting for connections to close..."
		
		' -----------------------------------------------------------------------------
		' Free any open TCP streams...
		' -----------------------------------------------------------------------------

		' Wait for each thread to finish its current job and close its own stream/socket...

		LockMutex ThreadListMutex
			For thread = EachIn ThreadList
				WaitThread thread
			Next
		UnlockMutex ThreadListMutex

		' -----------------------------------------------------------------------------
		' All done!
		' -----------------------------------------------------------------------------
		
		CloseSocket server

	Else
		Print "Couldn't bind to port 80!"
	EndIf
	
Else

	Print "Couldn't create server!..."
	
EndIf

End


'http request object

Type HTTPRequest
	Field meth$
	Field path$
	Field http$
	Field body$
	Field headers:tmap
	
	Method New()
		headers=New tmap
	End Method
	
	Method Create:HTTPRequest(_meth$,_path$,_http$)
		meth=_meth
		path=_path
		http=_http
		Return Self
	End Method
	
	Method header$(name$)
		If headers.contains(name)
			Return String(headers.valueforkey(Lower(name)))
		EndIf
	End Method
	
End Type

Function Respond:HTTPResponse(hr:HTTPRequest, c:Connection)
	Print "request method: "+hr.meth
	Print "Requested file: " + hr.path
	Print "Requested by: " + hr.header("user-agent")
	Print "Requested HTTP version: " + hr.http
	Print "body: "+hr.body
	
	' ---------------------------------------------------------------------
	' OK, we barely know what we're doing, so only accept HTTP 1.1...
	' ---------------------------------------------------------------------
	
	Local response:HTTPResponse=New HTTPResponse.Create(c)
	
	If hr.http$ <> "HTTP/1.1"
		response.status="505 This server only accepts HTTP version 1.1"
		'Return response
	Else
		' -----------------------------------------------------------------
		' It was a HTTP 1.1 request...
		' -----------------------------------------------------------------
		Local func(hr:HTTPRequest,response:HTTPResponse)
		
		'pick response function
		r:responder=responder.Find(hr.path)
		If r
			r.func(hr,response)	
		Else
			response.status="404 Not Found"
		EndIf
	EndIf
	
	response.output
End Function


Type HTTPResponse
	Field hr:HTTPRequest
	Field status$
	Field headers:tmap
	Field body$
	Field c:connection
	
	Method New()
		headers=New tmap
	End Method
	
	Method Create:HTTPResponse(_c:Connection)
		c=_c
		Return Self
	End Method
	
	Method output()
		c.WriteLine "HTTP/1.1 "+status
		headers.insert "content-length",String(Len(body))
		For Local key$=EachIn headers.keys()
			c.WriteLine key+": "+String(headers.valueforkey(key))
		Next
		c.WriteLine ""
		c.WriteLine body
		'killconnection c
	End Method
End Type


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



Function GetUser(hr:HTTPRequest,response:HTTPResponse)
	response.status="200 OK"
	response.headers.insert "Content-Type","text/html"
	Local user$=hr.path[6..]
	response.body="<FORM action=~qadduser~q Method=~qpost~q>~n	<input type=~qtext~q id=~qname~q/>~n	<Input Type=~qsubmit~q value=~qSend~q>~n</FORM>"
End Function


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

	' ---------------------------------------------------------------------
	' Some variables (see further down for meanings)...
	' ---------------------------------------------------------------------
	
	Local eoc:Int
	Local eop:Int

	Local hr:HTTPRequest
	Local incoming:String
	Local command:String
	Local parameter:String
	
	' ---------------------------------------------------------------------
	' HTTP requests end with a blank line, so we read until we get that...
	' ---------------------------------------------------------------------
	
	incoming=c.ReadLine()
	'Print ">>>"+incoming
	Local bits$[]=incoming.split(" ")
	hr=New HTTPRequest.Create(bits[0],bits[1],bits[2])	

	Repeat
	
		' -----------------------------------------------------------------
		' Read a line from an incoming HTTP request...
		' -----------------------------------------------------------------

		' The format of an incoming request line is:
		
		'		"Command" [space] "parameters"

		' Examples...
						
		'		"GET /thisfile.txt"
		'		"User-Agent; AcmeBrowse"
		
		If SocketConnected (c.socket)
		
			incoming = c.ReadLine()
			
		Else
		
			KillConnection c
			
			Return Null
			
		EndIf

		If incoming <> ""

			' -------------------------------------------------------------
			' Got a line? Let's parse! Split command and parameter(s)...
			' -------------------------------------------------------------
			
			eoc = incoming.find(":")		' End of command part of incoming
			command = Lower (incoming[..eoc])		' Command part of incoming
			parameter = Trim(incoming[eoc+2..])	' Parameter part of incoming
			hr.headers.insert command,parameter
			'Print command+": "+parameter
		EndIf
			

	Until incoming = "" ' Got blank line after headers, so all done here...

	Print ""
	Local size=Int(hr.header("content-length"))
	Print "body size: "+size
	hr.body=c.stream.ReadString(size)
	' ---------------------------------------------------------------------
	' Lessee what we've got...
	' ---------------------------------------------------------------------
	Respond hr,c

	KillConnection c
	
	Return Null
	
End Function

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

	If Left (h, 1) = "%" Then h = Right (h, Len (h) - 1)
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
