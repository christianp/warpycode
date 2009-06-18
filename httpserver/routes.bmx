'Import "regexp.bmx"

Type route
	Field f:fsa
	Field names$[]
	Field deflabels:tmap
	
	Method New()
		deflabels=New tmap
	End Method

	Function Create:route(pattern$,deflabelpatterns$[]=Null)
		Local bits$[]=pattern.split("/")
		re$=""
		Local names$[]
		For bit$=EachIn bits
			'Print bit
			If Len(bit)
				Select bit[0]
				Case Asc(":")	'if this is a label
					re:+"(.+)"
					names:+[bit[1..]]
				Case Asc("\")
					re:+"("+bit[1..]+")"
					names:+["#"+String(Len(names)+1)]
				Default
					re:+bit
				End Select
			EndIf
			re:+"/"
		Next
		re:+"?"
		'Print re
		'For name$=EachIn names
		'	Print name
		'Next
		
		r:route=New route
		r.f=fsa.Create(re)
		r.names=names

		If deflabelpatterns
			For pattern$=EachIn deflabelpatterns
				bits=pattern.split("=>")
				If bits[0][0]=Asc(":")
					bits[0]=bits[0][1..]
				EndIf
				r.deflabels.insert Trim(bits[0]),Trim(bits[1])
			Next
		EndIf
		


		routes.addfirst r
		Return r
	End Function
	
	Method match:tmap(pattern$)
		Local l:TList=New TList
		res$=f.evaluate(pattern,"",l)
		If Not res Return Null
		If res<>pattern Return Null
		
		labels:tmap=deflabels.copy()
		i=0
'		While String(l.first())=""
'			l.removefirst()
'		Wend
		For bit$=EachIn l
			If bit
				If i<Len(names) And names[i]
					labels.insert names[i],unhexurl(bit)
					'Print names[i]+": "+bit
				EndIf
				i:+1
			EndIf
		Next
		Return labels
	End Method
	
	Function find:tmap(pattern$)
		For r:route=EachIn routes
			labels:tmap=r.match(pattern)
			If labels Return labels
		Next
	End Function
End Type


Global routes:TList=New TList
'default route
route.Create "/:controller",["action => list"]
route.Create "/:controller/:action"
route.Create "/:controller/:action/:object"


Function GenericRoute(hr:HTTPRequest,labels:tmap,response:HTTPResponse)
	For key$=EachIn labels.keys()
		value$=String(labels.valueforkey(key))
		If value
			response.body:+":"+key+" = "+value+"<br/>"
		EndIf
	Next
End Function


Rem
While 1
	
	labels:tmap=route.find(Input())
	If map
		For key$=EachIn map.keys()
			Print key+": "+String(map.valueforkey(key))
		Next
	EndIf
Wend
endrem