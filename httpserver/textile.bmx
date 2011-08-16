'Include "regexp.bmx"

Global url:fsa=fsa.Create("((([a-zA-Z](\w|\+|\.|\-)*:\/\/(\w(((\w|\-)*\w)?))(.\w(((\w|\-)*\w)?))*(\:\a+)?))\/?|\/|)(\w|\-|_|\.|!|~~|\*|'|\(|\)|\/|%)*")
Global relurl:fsa=fsa.Create("(\w|\-|_|\.|!|~~|\*|'|\(|\)|\/|%)*")

'txt$=LoadText("textiletest.txt")
'Print textile(txt)


Function textile$(in$)
	'Print in
	'Print "---------------"
	
	Function startlist(line$)
		If Not Len(line) Return False
		start=line[0]
		If Not (start=Asc("#") Or start=Asc("*")) Return False
		c=0
		While c<Len(line) And line[c]=start
			c:+1
		Wend
		If c<Len(line)
			If " <>=(){}[]".find(Chr(line[c]))>=0 Return True
		EndIf
	End Function
	
	Function starttable(line$)
		'If Not line.contains("|") Return False
		If Not line Return False
		If line[0]=Asc("|") Return True
		name$=""
		attr$=""
		endsig=textile_modify(line,name,attr)
		'Print name
		'Print attr
		'Print Chr(line[endsig])
		If (attr And name="" And line.contains("|")) Or name="table" Return True
	End Function

	Local lines$[]=in.split("~n")
	For i=0 To Len(lines)-1
		lines[i]=Trim(lines[i])
	Next
	
	out$=""
	i=0
	While i<Len(lines)
		start$=lines[i].split(" ")[0]
		Local env$,endenv$
		Local numlists[]
		Local listtypes$[]
		If startlist(lines[i])
			While i<Len(lines) And startlist(lines[i])
				name$=""
				attr$=""
				endsig=textile_modify(lines[i],name,attr)
				Print name
				Print attr
				lc$=Chr(name[0])
				Select lc
				Case "#"
					sl$="<ol"+attr+">"
					el$="</ol>"
				Case "*"
					sl$="<ul"+attr+">"
					el$="</ul>"
				End Select
				Print sl
				c=0
				While c<Len(name) And name[c]=Asc(lc)
					c:+1
				Wend
				'Print c+"  "+Len(numlists)
				If Len(numlists)=0
					out:+sl+"~n"
					numlists=[c]
					listtypes=[el]
				ElseIf numlists[0]<c
					Print "wowo"
					numlists=[c]+numlists
					listtypes=[el]+listtypes
					out:+"~n"+sl+"~n"
				ElseIf numlists[0]>c
					numlists=numlists[1..]
					out:+listtypes[0]+"~n"
					listtypes=listtypes[1..]
				ElseIf numlists[0]=c
					out:+"</li>~n"
				EndIf
				out:+"<li"+attr+">"+textile_lines([Trim(lines[i][endsig+1..])])
				i:+1
			Wend
			For c=1 To Len(numlists)-1
				out:+"</li>~n"+listtypes[0]
				listtypes=listtypes[1..]
			Next
			out:+"</li>~n"+listtypes[0]+"~n"
		ElseIf lines[i].startswith("- ")
			out:+"<dl>~n"
			While i<Len(lines) And lines[i]
				n=lines[i].find(":=")
				name$=textile_lines([Trim(lines[i][2..n])])
				rest$=Trim(lines[i][n+2..])
				If i<Len(lines)-1 And (Not lines[i+1].startswith("- "))
					i:+1
					si=i
					While i<Len(lines) And lines[i] And (Not lines[i].endswith("=:"))
						rest:+"~n"+lines[i]
						i:+1
					Wend
					rest:+"~n"+lines[i][..lines[i].length-2]
					rest=textile(rest)
				Else
					rest=textile_lines([rest])
				EndIf
				out:+"<dt>"+name+"</dt>~n<dd>"+rest+"</dd>~n"
				i:+1
			Wend
			out:+"</dl>~n"
		ElseIf starttable(lines[i])
			name$=""
			attr$=""
			textile_modify(lines[i],name,attr)
			If name="table"
				out:+"<table"+attr+">~n"
				i:+1
			Else
				out:+"<table>~n"
			EndIf
			While i<Len(lines) And starttable(lines[i])
				endsig=textile_modify(lines[i],name,attr)
				If name=""
					out:+"<tr"+attr+">~n"
					lines[i]=lines[i][endsig..]
				Else
					out:+"<tr>~n"
				EndIf
				Local bits$[]=lines[i].split("|")
				bits=bits[1..Len(bits)-1]
				For bit$=EachIn bits
					name$=""
					attr$=""
					endsig=textile_modify(bit,name,attr)
					'Print bit
					'print endsig
					'print name
					'print attr
					colspan$=""
					rowspan$=""
					celltag$="td"
					validname=0
					If name And name[0]=Asc("_")
						celltag$="th"
						name=name[1..]
						validname=1
					EndIf
					While Len(name) And (name[0]=Asc("\") Or name[0]=Asc("/"))
						validname=1
						c=1
						While c<Len(name) And name[c]>=Asc("0") And name[c]<=Asc("9")
							c:+1
						Wend
						num=Int(name[1..c])
						Select name[0]
						Case Asc("\")
							colspan=" colspan=~q"+num+"~q"
						Case Asc("/")
							rowspan=" rowspan=~q"+num+"~q"
						End Select
						name=name[c..]
					Wend
					If name="" Or validname Or attr
						bit=bit[endsig..]
					Else
						attr=""
					EndIf
					attr:+colspan+rowspan
					starttag$="<"+celltag+attr+">"
					endtag$="</"+celltag+">"
					out:+starttag+textile_lines([bit])+endtag+"~n"
				Next
				out:+"</tr>~n"
				i:+1
			Wend
			out:+"</table>~n"
			i:-1
		ElseIf lines[i]<>""
			If start.endswith(".")
				sig$=start[..start.find(".")]
				env$=""
				attr$=""
				endsig=textile_modify(sig,env,attr)
				Local startline$,endline$="<br/>"
				endenv=""
				Select env
				Case "h1","h2","h3","p","div"
					endenv="</"+env+">"
					env="<"+env+attr+">"
				Case "bq"
					env="<blockquote"+attr+">~n<p"+attr+">"
					endenv="</p>~n</blockquote>"
				Default
					If env[..2]="fn"
						name$=env[2..]
						env="<p class=~qfootnote~q id=~qfn"+name+"~q"+attr+"><sup>"+name+"</sup>"
						endenv="</p>"
					EndIf
				End Select
				If endenv
					lines[i]=lines[i][start.length+1..]
				Else
					env="<p"+attr+">"
					endenv="</p>"
				EndIf
			Else
				env="<p>"
				endenv="</p>"
			EndIf
			si=i
			While i<Len(lines) And lines[i] 
				i:+1
			Wend
			out:+env+textile_lines(lines[si..i],startline,endline)+endenv+"~n"
		EndIf
		i:+1
	Wend
	Return out
End Function

Function textile_lines$(lines$[],startline$="",endline$="")
	endline:+"~n"
	txt$=startline+(endline+startline).join(lines)
	
	'Print txt
	'Print ">>>>>>>>>>>>>>>"

	Function code$(n)
		Return "&#"+n+";"
	End Function
	
	Function startblock(txt$,c,b$)
		If c>0 And txt[c-1]<>32 Return False
		If txt[c..c+b.length]<>b Return False
		
		While c<=txt.length-b.length
			If txt[c..c+b.length]=b
				If c+b.length=txt.length Or txt[c+b.length]=32 Return True
			EndIf
			c:+1
		Wend
	End Function
	
	Function endblock(txt$,c,b$)
		If txt[c..c+b.length]<>b Return False
		If c+b.length=txt.length Or txt[c+b.length]=32
			Return True
		EndIf
	End Function
			

	Local src$,alt$,name$,attr$
	Local inquote=0,inem,instrong,ini,inb,incite,indel,inins,insup,insub,inspan
	out$=""

	While c<Len(txt)
		'Print c
		Select Chr(txt[c])
		Case "~q"
			oc=c
			endlink=txt[c+1..].find("~q:")
			If (Not inquote) And endlink>=0
				link$=txt[c+1..endlink+c+1]
				name$=""
				attr$=""
				endsig=textile_modify(link,name,attr)
				'Print endsig+","+name+","+attr
				If name="" And attr
					name=link[endsig..]
				Else
					name=link
				EndIf
				If name.endswith(")")
					startalt=name.find("(")
					alt=name[startalt+1..name.length-1]
					name=name[..startalt]
				Else
					alt$=""
				EndIf
				c:+endlink+3
				sc=c
				While c<Len(txt) And txt[c]<>32
					c:+1
				Wend
				targ$=txt[sc..c]
				If url.evaluate(targ)=targ Or relurl.evaluate(targ)=targ
					While targ.endswith(".")
						targ=targ[..targ.length-1]
						c:-1
					Wend
					out:+"<a href=~q"+targ+"~q title=~q"+alt+"~q"+attr+">"+Trim(name)+"</a>"
					If c<Len(txt)
						c:-1
					EndIf
				Else
					out:+code(8220+inquote)
					inquote=1-inquote
					c=oc
				EndIf
			Else
				out:+code(8220+inquote)
				inquote=1-inquote
			EndIf
		Case "'"
			out:+code(8217)
		Case "-"
			If indel And endblock(txt,c,"-")
				out:+"</del>"
				indel=0
			ElseIf (Not indel) And startblock(txt,c,"-")
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<del"+attr+">"
				indel=1
			Else
				If c<Len(txt)-1 And Chr(txt[c+1])="-"
					c:+1
					out:+code(8212)
				Else
					out:+code(8211)
				EndIf
			EndIf
		Case "="
			If startblock(txt,c,"==")
				endeq=txt[c+2..].find("==")
				'Print endeq
				If endeq>=0
					out:+txt[c+2..c+2+endeq]
					c:+endeq+1
				EndIf
			EndIf
		Case "."
			If txt[c..c+3]="..."
				c:+2
				out:+code(8230)
			Else
				out:+"."
			EndIf
		Case "x"
			If c>0 And txt[c-1]=32 And txt[c+1]=32
				out:+code(215)
			Else
				out:+"x"
			EndIf
		Case "("
			endbr=txt[c..].find(")")
			If endbr>=0
				Select txt[c..c+endbr]
				Case "TM"
					out:+code(8482)
				Case "R"
					out:+code(174)
				Case "C"
					out:+code(169)
				Default
					out:+"("
					endbr=c
				End Select
				c=endbr
			Else
				out:+"("
			EndIf
		Case "["
			endbr=txt[c..].find("]")
			name$=txt[c+1..c+endbr]
			out:+"<sup><a href=~q#fn"+name+"~q>"+name+"</a></sup>"
			c:+endbr
			'Print endbr
		Case "_"
			If ini
				If endblock(txt,c,"__")
					ini=0
					out:+"</i>"
					c:+1
				EndIf
			ElseIf inem
				If endblock(txt,c,"_")
					inem=0
					out:+"</em>"
				EndIf
			ElseIf startblock(txt,c,"__")
				c:+1
				ini=1
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<i"+attr+">"
			ElseIf startblock(txt,c,"_")
				inem=1
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<em"+attr+">"
			Else
				out:+"_"
			EndIf
		Case "*"
			If inb
				If endblock(txt,c,"**")
					inb=0
					out:+"</b>"
					c:+1
				EndIf
			ElseIf instrong
				If endblock(txt,c,"*")
					instrong=0
					out:+"</strong>"
				EndIf
			ElseIf startblock(txt,c,"**")
				c:+1
				inb=1
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<b"+attr+">"
			ElseIf startblock(txt,c,"*")
				instrong=1
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<strong"+attr+">"
			Else
				out:+"*"
			EndIf
		Case "?"
			If incite And endblock(txt,c,"??")
				incite=0
				out:+"</cite>"
				c:+1
			ElseIf (Not incite) And startblock(txt,c,"??")
				incite=1
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<cite"+attr+">"
				c:+1
			Else
				out:+"?"
			EndIf
		Case "+"
			If inins And endblock(txt,c,"+")
				inins=0
				out:+"</ins>"
			ElseIf (Not inins) And startblock(txt,c,"+")
				inins=1
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<ins"+attr+">"
			Else
				out:+"+"
			EndIf
		Case "^"
			If insup And endblock(txt,c,"^")
				insup=0
				out:+"</sup>"
			ElseIf (Not insup) And startblock(txt,c,"^")
				insup=1
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<sup"+attr+">"
			Else
				out:+"^"
			EndIf
		Case "~~"
			If insub And endblock(txt,c,"~~")
				insub=0
				out:+"</sub>"
			ElseIf (Not insub) And startblock(txt,c,"~~")
				insub=1
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<sub"+attr+">"
			Else
				out:+"~~"
			EndIf
		Case "%"
			If inspan And endblock(txt,c,"%")
				inspan=0
				out:+"</span>"
			ElseIf (Not inspan) And startblock(txt,c,"%")
				inspan=1
				endsig=textile_modify(txt[c+1..],name,attr)
				out:+"<span"+attr+">"
			Else
				out:+"%"
			EndIf
		Case "!"
			c:+1
			sc=c
			'Print "IMAGE"
			endsig=textile_modify(txt[c..],name,attr)
			'Print endsig+","+name+","+attr
			If name="" And attr
				c:+endsig
				sc=c
			EndIf
			inparens=0
			'print txt[c..]
			While c<Len(txt)
				Select Chr(txt[c])
				Case "!"
					If Not inparens
						If Not src
							src=txt[sc..c]
						EndIf
						Exit
					EndIf
				Case "("
					src=txt[sc..c]
					sc=c+1
					inparens=1
				Case ")"
					If src
						alt=txt[sc..c]
					EndIf
					inparens=0
				Case " "
					If Not src
						Exit
					EndIf
				End Select
				c:+1
			Wend
			'Print src
			If src
				imgtxt$="<img src=~q"+src+"~q alt=~q"+alt+"~q"+attr+"/>"
				If c<Len(txt)-1 And Chr(txt[c+1])=":"	'link follows
					ptarg$=txt[c+2..]
					targ$=url.evaluate(ptarg)
					If targ="" targ=relurl.evaluate(ptarg)
					While targ.length And Chr(targ[targ.length-1])="."
						targ=targ[..targ.length-1]
					Wend
					out:+"<a href=~q"+targ+"~q>"+imgtxt+"</a>"
					c:+targ.length+1
				Else
					out:+imgtxt
				EndIf
			Else
				c=sc-1
			EndIf
			
		Default
			out:+Chr(txt[c])
		End Select
		c:+1
	Wend
	
	Return out
End Function

Function textile_modify(in$,name$ Var,attr$ Var)
	name=""
	attr=""
	class$=""
	id$=""
	style$=""
	align$=""
	lang$=""
	lpads=0
	rpads=0
	c=0
	gotname=0
	While c<Len(in)
		isname=0
		sc=c
		Select Chr(in[c])
		Case "<"
			If c<Len(in)-1 And in[c+1]=Asc(">")
				align="justify"
				c:+1
			Else
				align="left"
			EndIf
		Case ">"
			align="right"
		Case "="
			align="center"
		Case "("
			If c<Len(in)-1 And in[c+1]<>Asc("(") And in[c+1]<>Asc(")")
				c:+1
				oc=c
				While c<Len(in) And in[c]<>Asc(")")
					c:+1
				Wend
				nclass$=in[oc..c]
				If c=Len(in) Or nclass.contains(" ") Or nclass.contains("(")
					c=oc-1
					lpads:+1
				Else
					class=nclass
					hash=class.find("#")
					If hash>=0
						id=class[hash+1..]
						class=class[..hash]
					EndIf
				EndIf
			Else
				lpads:+1
			EndIf
		Case ")"
			rpads:+1
		Case "{"
			'Print "STYLE"
			c:+1
			oc=c
			While c<Len(in) And in[c]<>Asc("}")
				c:+1
			Wend
			style=in[oc..c]
			If Not style.endswith(";")
				style:+";"
			EndIf
		Case "["
			c:+1
			oc=c
			While c<Len(in) And in[c]<>Asc("]")
				c:+1
			Wend
			lang=in[oc..c]
		Case "."
			If Not gotname
				name=in[..c]
			EndIf
			c:+1
			
			Exit
		Case " "
			If Not gotname
				name=in[..c]
			EndIf
			Exit
		Default
			isname=1
			If gotname
				Exit
			EndIf
		End Select
		
		If gotname=0 And isname=0 And name=""
			name=in[..sc]
			gotname=1
		EndIf
		
		c:+1
	Wend
		If align
			style:+"text-align:"+align+";"
		EndIf
		If lpads
			style:+"padding-left:"+lpads+"em;"
		EndIf
		If rpads
			style:+"padding-right:"+rpads+"em;"
		EndIf
		If style
			attr:+" style=~q"+style+"~q"
		EndIf
		If lang
			attr:+" lang=~q"+lang+"~q"
		EndIf
		If class
			attr:+" class=~q"+class+"~q"
		EndIf
		If id
			attr:+" id=~q"+id+"~q"
		EndIf
		If Not (name Or attr)
			name=in[..c]
		EndIf
		Return c
End Function