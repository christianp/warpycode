Type HTTPRequest
	Field meth$
	Field path$
	Field pathbits$[]
	Field http$
	Field body$
	Field headers:tmap
	Field data:tmap
	
	Method New()
		headers=New tmap
		data=New tmap
	End Method
	
	Method Create:HTTPRequest(_meth$,_path$,_http$)
		meth=Lower(_meth)
		path=_path
		pathbits=path.split("/")
		http=_http
		Return Self
	End Method
	
	Method header$(name$)
		If headers.contains(name)
			Return String(headers.valueforkey(Lower(name)))
		EndIf
	End Method
	
	Method getdata$(name$)
		If data.contains(name)
			Return String(data.valueforkey(Lower(name)))
		EndIf
	End Method
	
End Type
 
Type HTTPResponse
	Field status$
	Field headers:tmap
	Field body$
	
	Method New()
		status="200 OK"
		headers=New tmap
		headers.insert "content-type","text/html"
		headers.insert "connection","close"
	End Method
	
	Method output$()
		If Not body
			body=status
		EndIf
		out$=""

		out:+"HTTP/1.1 "+status+"~n"
		headers.insert "content-length",String(Len(body))
		For Local key$=EachIn headers.keys()
			out:+key+": "+String(headers.valueforkey(key))+"~n"
		Next

		out:+"~n"+body		
		Print out
		Return out
	End Method
End Type

Type HTTPSession
	Field c:connection
	Field req:HTTPRequest
	Field res:HTTPResponse
	Field labels:tmap
	Field info:tmap
	
	Method New()
		res=New HTTPResponse
		labels=New tmap
	End Method
	
	Function Create:HTTPSession(c:connection)
		s:HTTPSession=New HTTPSession
		s.c=c
		Return s
	End Function
	
	Method Respond()
		Print "request method: "+req.meth
		Print "Requested file: " + req.path
		Print "Requested by: " + req.header("user-agent")
		Print "Requested HTTP version: " + req.http
		Print "body: "+req.body
		
		info=labels.copy()
		info.insert "session",Self
		
		If req.http$ <> "HTTP/1.1"
			render_error "505 This server only accepts HTTP version 1.1"
		Else
			If Len(req.path)>1 And req.path[Len(req.path)-1]=Asc("/") req.path=req.path[..Len(req.path)-1]
			labels=route.find(req.path)
			If Not labels
				render_error "404 No route matched ("+req.path+")"
			Else
				controller$=label("controller")
				If Not controller
					render_error "404 Route did not define a controller"
				Else
					Print controller
					tt:TTypeId=TTypeId.ForName(controller)
					If tt
						action$=label("action")
						If Not action
							render_error "404 Route did not give an action"
						Else
							f:tfunction=tt.findfunction(action)
							If f
								f.invoke([Self])
							Else
								render_error "404 View "+controller+"/"+action+" does not exist"
							EndIf
						EndIf
					Else
						render_error "404 Controller "+controller+" does not exist"
					EndIf
				EndIf
			EndIf
		EndIf
		Print "finished"
		
		If labels
			render "<div><p>debug info: Labels<br/><ul>"
			For key$=EachIn labels.keys()
				render "<li>"+key+" => "+label(key)+"</li>"
			Next
			render "</ul></p></div>"
		EndIf
		
		c.WriteLine res.output()
	End Method

	Method label$(name$)
		If labels.contains(name)
			Return String(labels.valueforkey(name))
		EndIf
	End Method
	
	Method render(txt$)
		res.body:+txt
	End Method
	
	Method render_template(name$)
		Local bits$[]
		If Not templates.contains(name)
			render "Can't find template "+name
			Return
		EndIf
		bits=String[](templates.valueforkey(name))
		render_bits bits
	End Method
	
	Method render_bits(bits$[])
		i=0
		While i<Len(bits)
			If i Mod 2
				Print bits[i]
				If bits[i][0]=Asc("=")
					render String(getinfo(Trim(bits[i][1..]),info))
				Else
					Local words$[]=bits[i].split(" ")
					Select words[0]
					Case "for"
						aka$=words[1]
						obj$=words[3]
						si=i
						While bits[i]<>"endfor"
							i:+2
						Wend
						For o:Object=EachIn iterate(getinfo(obj,info))
							info.insert aka,o
							render_bits(bits[si+1..i])
						Next
					End Select
				EndIf
			Else
				render bits[i]
			EndIf
			i:+1
		Wend
	End Method		
	
	Method render_error(err$)
		res.status=err
		render err
	End Method
End Type

Function getinfo:Object(name$,content:tmap)
	'Print "getinfo "+name
	Local bits$[]=name.split(".")
	If content.contains(bits[0])
		o:Object=content.valueforkey(bits[0])
		If Not o Return Null
		'Print "object"
		Return getproperty(o,bits[1..])
	Else
		Return Null
	EndIf
End Function


Function getproperty:Object(o:Object,bits$[])
	tt:TTypeId=TTypeId.ForObject(o)
	'Print "  "+tt.name()+": "+".".join(bits)
	If Not Len(bits)
		Return o
	EndIf
	f:TField=tt.findfield(bits[0])
	If f
		Select f.typeid()
		Case StringTypeId
			'Print "  stringfield"
			Return f.getstring(o)
		Case IntTypeId
			'Print "  intfield"
			Return String(f.getint(o))
		Case FloatTypeId
			'Print "  floatfield"
			Return String(f.getfloat(o))
		Case DoubleTypeId
			'Print "  doublefield"
			Return String(f.getdouble(o))
		Case LongTypeId
			'Print "  longfield"
			Return String(f.getlong(o))
		Default
			'Print "  objectfield"
			o:Object=f.get(o)
			Return getproperty(o,bits[1..])
		End Select
	Else
		m:TMethod=tt.findmethod(bits[0])
		If m
			'Print "  method"
			Return m.invoke(o)
		EndIf
	EndIf
End Function