Include "server.bmx"

Global users:TList=New TList
Global numusers=0
Type user
	Field name$
	Field n
	
	Method New()
		numusers:+1
		n=numusers
	End Method
	
	Function Create:user(name$)
		u:user=New user
		u.name=name
		users.addlast u
		Return u
	End Function
	
	Function test(s:HTTPSession)
	End Function
	
	Function view(s:HTTPSession)
		id$=s.label("object")
		u:user=find(id)
		If u
			s.info.insert "user",u
			s.render_template "view"
		Else
			s.render "No such user "+id
			s.render "<br/>"+link_to("/user","Back to list of users")
		EndIf
	End Function
	
	Function add(s:HTTPSession)
		Select s.req.meth
		Case "get"
			s.render_template "add"
		Case "post"
			u:user=user.Create(s.req.getdata("name"))
			s.info.insert "user",u
			s.render_template "added"
		End Select
	End Function
	
	Function remove(s:httpsession)
		name$=s.label("object")
		u:user=find(name)
		If u
			users.remove u
			s.info.insert "user",u
			s.render_template "remove"
		Else
			s.render "User "+name+" doesn't exist, can't delete!"
			s.render "<br/>"+link_to("/user/list","Back to list of users")
		EndIf
	End Function
	
	Function list(s:HTTPSession)
		listhtml$=""
		
		For u:user=EachIn users
			listhtml:+"<li>"+link_to("/user/view/"+u.name,u.name)+"</li>~n"
		Next
		s.info.insert "users",users
		s.info.insert "list",listhtml
		s.render_template "list"
	End Function
	
	Function find:user(id$)
		For u:user=EachIn users
			If u.name=id Or u.n=Int(id) Return u
		Next
	End Function
End Type

user.Create "bob"

route.Create "/",["controller => user","action => list"]

server:tsocket=start_server()
Print "go to http://127.0.0.1/"
run_server server