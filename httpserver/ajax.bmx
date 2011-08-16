Include "server.bmx"

Type ajax
	Global i
	Global words$[]=["hi","there","dude"]
	Function get(s:httpsession)
		s.addtext ";".join(words)
		i=(i+1) Mod Len(words)
		s.res.headers.insert "content-type","application/xml"
		s.rendered=1
	End Function
End Type


Type site
	Function view(s:httpsession)
	End Function
End Type


route.Create("/",["controller => site","action => view"])
server:tserver=tserver.Create(80)
server.run