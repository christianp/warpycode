Global stringre:fsa=fsa.Create("~q(.*)~q")
Global numberre:fsa=fsa.Create("(0|1[0-9]*)(.[1-9][0-9]*)?")

Type HTTPRequest
	Field meth$
	Field path$
	Field pathbits$[]
	Field http$
	Field body$
	Field headers:tmap
	Field data:tmap
	Field cookies:tmap
	
	Method New()
		headers=New tmap
		data=New tmap
		cookies=New tmap
	End Method
	
	Method Create:HTTPRequest(_meth$,_path$,_http$)
		meth=Lower(_meth)
		path=_path
		pathbits=path.split("/")
		http=_http
		Return Self
	End Method
	
	Method cookie$(name$)
		If cookies.contains(name)
			Return String(cookies.valueforkey(Lower(name)))
		EndIf
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
	Field cookies:TList
	Field body$
	
	Method New()
		status="200 OK"
		headers=New tmap
		headers.insert "content-type","text/html"
		headers.insert "connection","close"
		cookies=New TList
	End Method
	
	Method output$()
		If Not body
			body=status
		EndIf
		out$=""

		out:+"HTTP/1.1 "+status+"~n"
		headers.insert "content-length",String(Len(body))
		For key$=EachIn headers.keys()
			out:+key+": "+String(headers.valueforkey(key))+"~n"
		Next
		For cookie$=EachIn cookies
			out:+"set-cookie: "+cookie+"~n"
		Next

		out:+"~n"+body		
		'Print out
		Return out
	End Method
End Type

Type HTTPSession
	Field c:connection
	Field req:HTTPRequest
	Field res:HTTPResponse
	Field labels:tmap
	Field info:tmap
	Field rendered
	Field content:tmap
	
	Method New()
		res=New HTTPResponse
		labels=New tmap
		info=New tmap
		info.insert "session",Self
		content=New tmap
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
		
		

		If Len(req.path)>1 And req.path[Len(req.path)-1]=Asc("/") req.path=req.path[..Len(req.path)-1]
		labels=route.find(req.path)
		controller$=label("controller")
		action$=label("action")

		If req.http$ <> "HTTP/1.1"
			render_error "505 This server only accepts HTTP version 1.1"
		Else
			If Not labels
				render_error "404 No route matched ("+req.path+")"
			Else
				If Not controller
					render_error "404 Route did not define a controller"
				Else
					'Print controller
					tt:TTypeId=TTypeId.ForName(controller)
					If tt
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
		
		
		If Not rendered 'didn't render a different template, use one corresponding to this controller/action
			render controller+"/"+action
		EndIf
		
		apply_layout controller
		
		
		'Print "finished"
		Print "~n"
		
		Rem
		If labels
			render "<div><p>debug info: Labels<br/><ul>"
			For key$=EachIn labels.keys()
				render "<li>"+key+" => "+label(key)+"</li>"
			Next
			render "</ul></p></div>"
		EndIf
		EndRem
		
		c.WriteLine res.output()
	End Method

	Method label$(name$)
		If labels.contains(name)
			Return String(labels.valueforkey(name))
		EndIf
	End Method
	
		
	Method getinfo:Object(name$)
		'Print "getinfo "+name
		name=Trim(name)
		l:TList=New TList
		If stringre.evaluate(name,"",l)	'string literal
			'Print "MATCH STRING"
			Return l.removelast()
		EndIf
		l:TList=New TList
		If numberre.evaluate(name)	'int literal
			Return name
		EndIf
		Local bits$[]=splitargs(name,".")
		If info.contains(bits[0])
			o:Object=info.valueforkey(bits[0])
			If Not o Return Null
			'Print "object "+bits[0]
			Return getproperty(o,bits[1..])
		Else
			Return Null
		EndIf
	End Method
	
	
	Method getproperty:Object(o:Object,bits$[])
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
			Local args:Object[]=parsefunction(bits[0])
			Local mname$
			If args
				'Print "function with arguments!"
				'For arg$=EachIn args
					'Print arg
				'Next
				mname$=String(args[0])
				args=args[1..]
				For i=0 To Len(args)-1	'evaluate arguments
					args[i]=getinfo(String(args[i]))
				Next
			Else
				mname$=bits[0]
				args=Null
			EndIf
			m:TMethod=tt.findmethod(mname)
			If m
				'Print "  method"
				Return m.invoke(o,args)
			EndIf
		EndIf
	End Method
	
	Method addtext(txt$,cname$)
		If cname
			txt=getcontent(cname)+txt
			content.insert cname,txt
		Else
			res.body:+txt
		EndIf
	End Method
	
	Method getcontent$(cname$)
		If content.contains(cname)
			Return String(content.valueforkey(cname))
		EndIf
	End Method
	
	Method apply_layout(name$)
		Local layout$
		If templates.contains("layouts/"+name)
			layout="layouts/"+name
		Else
			layout="layouts/application"
		EndIf
		render layout,""
	End Method
	
	Method render(name$,cname$="main")
		rendered=1
		
		Local bits$[]
		If Not templates.contains(name)
			addtext "Can't find template "+name,cname
			Return
		EndIf
		bits=String[](templates.valueforkey(name))
		render_bits bits,cname
	End Method
	
	Method render_bits(bits$[],cname$="main")
		i=0
		While i<Len(bits)
			If i Mod 2
				'Print bits[i]
				If bits[i][0]=Asc("=")
					addtext String(getinfo(Trim(bits[i][1..]))),cname
				Else
					Local words$[]=bits[i].split(" ")
					Select words[0]
					Case "for"
						aka$=words[1]
						obj$=words[3]
						si=i
						infors=1
						While infors
							i:+2
							If Trim(bits[i]).split(" ")[0]="for" infors:+1
							If bits[i]="endfor" infors:-1
						Wend
						For o:Object=EachIn iterate(getinfo(obj))
							info.insert aka,o
							render_bits(bits[si+1..i],cname)
						Next
					Case "if"
						'Print "IF STATEMENT"
						expr$=" ".join(words[1..])
						'Print expr
						words=expr.split("=")
						si=i
						ei=si
						inifs=1
						While inifs
							i:+2
							If Trim(bits[i]).split(" ")[0]="if" inifs:+1
							If bits[i]="endif" inifs:-1
							If bits[i]="else" ei=i
						Wend
						'Print si+","+ei+","+i
						'Print String(getinfo(words[0]))
						'Print String(getinfo(words[1]))
						Select Len(words)
						Case 1
							success=getinfo(expr)<>Null
						Case 2
							success=getinfo(words[0]).compare(getinfo(words[1]))=0
						End Select
						If success
							'Print "yes!"
							If ei=si Then ei=i
							render_bits(bits[si+1..ei],cname)
						ElseIf ei>si
							'Print "else!"
							render_bits(bits[ei+1..i],cname)
						Else
							'Print "no"
						EndIf
					Case "include"
						render " ".join(words[1..]),cname
					Case "content_for"
						cname2$=Trim(words[1])
						si=i
						While bits[i]<>"end_content"
							i:+2
						Wend
						render_bits bits[si+1..i],cname2
					Case "yield"
						If Len(words)=1
							cname2$="main"
						Else
							cname2=words[1]
						EndIf
						addtext getcontent(cname2),cname
					End Select
				EndIf
			Else
				addtext bits[i],cname
			EndIf
			i:+1
		Wend
	End Method		
	
	Method render_error(err$)
		res.status=err
		addtext err,"main"
		rendered=1
	End Method
	
	Method redirect(dest$)
		res.status="303 See other"
		res.headers.insert "Location",dest
		rendered=1
	End Method
	
	Method set_cookie(name$,value$,path$="/")
		res.cookies.addlast name+"="+value+"; path="+path
	End Method
End Type

