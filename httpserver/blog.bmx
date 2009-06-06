Include "server.bmx"

Global posts:TList=New TList
Global numposts
Type post
	Field id
	Field title$,content$,contentsrc$
	Field author:user
	
	Method New()
		numposts:+1
		id=numposts
		posts.addlast Self
	End Method
	
	Function Create:post(author:user,title$,content$)
		p:post=New post
		p.author=author
		p.title=title
		p.setcontent content
		savedb
		Return p
	End Function

	Function find:post(id)
		For p:post=EachIn posts
			If p.id=id Return p
		Next
	End Function
	
	Function add(s:httpsession)
		Select s.req.meth
		Case "get"
		Case "post"
			author:user=user(s.getinfo("you"))
			title$=s.req.getdata("title")
			content$=s.req.getdata("content")
			post.Create author,title,content
			s.redirect "/post/list"
		End Select
	End Function
	
	Function edit(s:httpsession)
		id=Int(s.label("object"))
		p:post=post.find(id)
		Select s.req.meth
		Case "get"
			s.info.insert "post",p
		Case "post"
			p.title=s.req.getdata("title")
			p.setcontent s.req.getdata("content")
			savedb
			s.redirect "/post/view/"+p.id		
		End Select
	End Function
	
	Function remove(s:httpsession)
		id=Int(s.label("object"))
		p:post=post.find(id)
		If p
			posts.remove p
		EndIf
		s.redirect "/post/list"
	End Function
	
	Function list(s:httpsession)
		s.info.insert "posts",posts
	End Function
	
	Function view(s:httpsession)
		id=Int(s.label("object"))
		p:post=post.find(id)
		s.info.insert "p",p
		mycomments:tquery=filter(comments,"owner=p",s)
		s.info.insert "comments",mycomments
	End Function
	
	Function addcomment(s:httpsession)
		id=Int(s.label("object"))
		p:post=post.find(id)
		author:user=user(s.getinfo("you"))
		txt$=s.req.getdata("txt")
		comment.Create p,author,txt
		s.redirect "/post/view/"+p.id
	End Function
	
	Method summary$()
		Function striptags$(in$)
			c=0
			oc=0
			out$=""
			While c<Len(in)
				If in[c]=Asc("<")
					out:+in[oc..c]
					While c<Len(in) And in[c]<>Asc(">")
						c:+1
					Wend
					oc=c+1
				EndIf
				c:+1
			Wend
			Return out
		End Function
		sum$=striptags(content)
		If Len(sum)>50
			Return sum[..47]+"..."
		Else
			Return sum
		EndIf
	End Method
	
	Method setcontent(c$)
		contentsrc=c
		content=textile(c)
	End Method
End Type

Global comments:TList=New TList
Global numcomments
Type comment
	Field id
	Field author:user
	Field owner:post
	Field txt$
	
	Method New()
		numcomments:+1
		id=numcomments
		comments.addlast Self
	End Method
	
	Function find:comment(id)
		For c:comment=EachIn comments
			If c.id=id Return c
		Next
	End Function
	
	Function Create:comment(p:post,author:user,txt$)
		c:comment=New comment
		c.owner=p
		c.author=author
		c.txt=txt
		savedb
		Return c
	End Function
	
	Function remove(s:httpsession)
		id=Int(s.label("object"))
		c:comment=comment.find(id)
		comments.remove c
		s.redirect "/post/view/"+c.owner.id
	End Function
End Type

Global users:TList=New TList
Global numusers
Type user
	Field id
	Field auth
	Field name$
	
	Method New()
		numusers:+1
		id=numusers
		users.addlast Self
	End Method
	
	Function Create:user(name$)
		u:user=New user
		u.name=name
		savedb
		Return u
	End Function
	
	Function find:user(id$)
		For u:user=EachIn users
			If u.id=Int(id) Or Lower(u.name)=Lower(id) Return u
		Next
	End Function
	
	
	Function login(s:httpsession)
		name$=s.req.getdata("name")
		u:user=user.find(name)
		If Not u u=user.Create(name)
		u.auth=Rand(1000000)
		savedb
		s.set_cookie "userid",u.id
		s.set_cookie "userauth",u.auth
		s.redirect s.req.header("referer")
	End Function
	
	Function logout(s:httpsession)
		s.set_cookie "userid","0"
		s.redirect s.req.header("referer")
	End Function
	
	Function view(s:httpsession)
		id$=s.label("object")
		u:user=user.find(id)
		If u
			s.info.insert "user",u
			uposts:tquery=filter(posts,"author=user",s)
			ucomments:tquery=filter(comments,"author=user",s)
			s.info.insert "posts",uposts
			s.info.insert "comments",ucomments
		Else
			s.render_error "404 No such user"
		EndIf
	End Function
	
End Type


Function init(s:httpsession)
	If s.req.cookies.contains("userid")
		u:user=user.find(s.req.cookie("userid"))
		If u
			Print u.auth
			Print s.req.cookie("userauth")
		EndIf
		If u And u.auth=Int(s.req.cookie("userauth"))
			s.info.insert "you",u
			Print "ARR"
		EndIf
	EndIf
	s.info.insert "users",users
End Function

Function savedb()
	'Print "SAVE"
	ja:jsonarray=New jsonarray
	serialised:tmap=New tmap
	Local txt$
	For p:post=EachIn posts
		For jv:jsonvalue=EachIn jsonise(p,serialised)
			txt:+jv.repr()+"~n"
		Next
	Next
	For u:user=EachIn users
		For jv:jsonvalue=EachIn jsonise(u,serialised)
			txt:+jv.repr()+"~n"
		Next
	Next		
	For c:comment=EachIn comments
		For jv:jsonvalue=EachIn jsonise(c,serialised)
			txt:+jv.repr()+"~n"
		Next
	Next
	f:TStream=WriteFile("db/db.txt")
	f.WriteString txt
	CloseFile f
End Function

Function loaddb()
	f:TStream=ReadFile("db/db.txt")
	txt$=f.ReadString(f.size())
	unjsonise txt
End Function

SeedRnd MilliSecs()
loaddb
'u:user=user.Create("Shakespeare")
'p:post=post.Create(u,"Hello","This is the first <b>post</b>. It is a long post about things.")
'comment.Create p,u,"First!"

route.Create("/",["controller => post","action => list"])

server:tserver=tserver.Create(80,init)
server.run