Type jsondecoder
	Field txt$
	Field i
	Field curchr$
	Field things:TList
	
	Method New()
		things=New TList
	End Method

	Method getnext(tokens$[],onlywhitespace=1)
		oldi=i
		While i<Len(txt)
			c$=Chr(txt[i])
			i=i+1
			For token$=EachIn tokens
				If c=token 
					curchr=c
					Return 1
				EndIf
			Next
			If onlywhitespace And (Not (c=" " Or c="~t" Or c="~n" Or c="~r"))
				i=i-1
				Return 0
			EndIf
		Wend
		i=oldi
		Return 0
	End Method
	
	Function Create:jsondecoder(txt$)
		j:jsondecoder=New jsondecoder
		j.txt=txt
		j.i=i
		Return j
	End Function
	
	Method parse()
		While getnext(["{","["])
			Select curchr
			Case "{" 'new object
				o:jsonobject=parseobject()
				If Not o
					Print "error - couldn't parse object"
				EndIf
				things.addlast o
			Case "[" 'new array
				a:jsonarray=parsearray()
				If Not a
					Print "error - couldn't parse array"
				EndIf
				things.addlast a
			End Select
		Wend
	End Method	
	
	Method parseobject:jsonobject()
		o:jsonobject=New jsonobject
		While getnext(["~q","}"])
			Select curchr
			Case "~q"
				p:jsonpair=parsepair()
				If Not p
					Print "error reading pair"
				EndIf
				o.pairs.addlast p
				If Not getnext([",","}"])
					Print "error after reading pair - expected either , or }"
				EndIf
				If curchr="}"
					Return o
				EndIf
			Case "}"
				Return o
			End Select
		Wend
		
		Print "error reading Object - expected a } at least!"
	End Method
	
	Method parsepair:jsonpair()
		p:jsonpair=New jsonpair
		p.name=parsestring()
		If Not getnext([":"])
			Print "error reading pair - expected a :"
		EndIf
		v:jsonvalue=parsevalue()
		If Not v
			Print "error reading pair - couldn't read a value"
		EndIf
		p.value=v
		Return p
	End Method
	
	Method parsearray:jsonarray()
		a:jsonarray=New jsonarray
		While getnext(["~q","-","0","1","2","3","4","5","6","7","8","9","{","[","t","f","n","]"])
			Select curchr
			Case "~q","-","0","1","2","3","4","5","6","7","8","9","{","[","t","f","n"
				i:-1
				v:jsonvalue=parsevalue()
				a.values.addlast v
				If Not getnext([",","]"])
					Print "error - expecting , or ]"
				EndIf
				If curchr="]"
					Return a
				EndIf
				
			Case "]"
				Return a
			End Select
		Wend
		Print "error - expecting a value or ]"
	End Method
	
	Method parsestring$()
		oldi=i
		s$=""
		
		While getnext(["~q","\"],0)
			s:+txt[oldi..i-1]
			Select curchr
			Case "~q"
				Return s
			Case "\"
				Select Chr(txt[i])
				Case "~q"
					s:+"~q"
				Case "\"
					s:+"\"
				Case "/"
					s:+"/"
				Case "b"
					s:+Chr(8)
				Case "f"
					s:+Chr(12)
				Case "n"
					s:+"~n"
				Case "r"
					s:+"~r"
				Case "t"
					s:+"~t"
				Case "u"
					s:+parseunicode()
				End Select
				i:+1
			End Select
			oldi=i
		Wend
	End Method
	
	Method parseunicode$()
		n:Short=0
		For t=1 To 4
			n:*16
			c=txt[i+t]
			If c>48 And c<57
				n:+c-48
			ElseIf c>=65 And c<=70
				n:+c-55
			ElseIf c>=97 And c<=102
				n:+c-87
			EndIf
		Next
		i:+4
		Return Chr(n)
	End Method
	
	Method parsevalue:jsonvalue()
		If Not getnext(["~q","-","0","1","2","3","4","5","6","7","8","9","{","[","t","f","n"])
			Print "error - expecting the beginning of a value"
		EndIf
		Select curchr
		Case "~q"
			s$=parsestring()
			Return jsonstringvalue.Create(s,0)
		Case "-","0","1","2","3","4","5","6","7","8","9"
			n:Double=parsenumber()
			Return jsonnumbervalue.Create(n)
		Case "{"
			o:jsonobject=parseobject()
			Return o
		Case "["
			a:jsonarray=parsearray()
			Return a
		Case "t"
			i:+3
			Return jsonliteralvalue.Create(1)
		Case "f"
			i:+4
			Return jsonliteralvalue.Create(0)
		Case "n"
			i:+2
			Return jsonliteralvalue.Create(-1)
		End Select
	End Method
	
	Method parsenumber:Double()
		i:-1
		sign=1
		n:Double=0
		Select Chr(txt[i])
		Case "-"
			i:+2
			Return parsenumber()*(-1)
		Case "0"
			i:+1
			If getnext(["."])
				n=parsefraction()
			EndIf
		Case "1","2","3","4","5","6","7","8","9"
			n=parseinteger()
			If getnext(["."])
				n:+parsefraction()
			EndIf
		End Select
		
		If Chr(txt[i])="e" Or Chr(txt[i])="E"
			i:+1
			Select Chr(txt[i])
			Case "+"
				sign=1
			Case "-"
				sign=-1
			Default
				Print "error - not a + or - when reading exponent in number"
			End Select
			e=parseinteger()
			n:*10^(sign*e)
		EndIf
		Return n
	End Method
			
	Method parsefraction:Double()
		digits=0
		n:Double=0
		While txt[i]>=48 And txt[i]<=57 And i<Len(txt)
			n:*10
			n:+txt[i]-48
			i:+1
			digits:+1
		Wend
		n:/(10^digits)
		If i=Len(txt)
			Print "error - reached EOF while reading number"
		EndIf
		Return n
	End Method
	
	Method parseinteger:Double()
		n:Double=0
		While txt[i]>=48 And txt[i]<=57 And i<Len(txt)
			n:*10
			n:+txt[i]-48
			i:+1
		Wend
		If i=Len(txt)
			Print "error - reached EOF while reading number"
		EndIf
		Return n
	End Method
			
End Type

Type jsonvalue

	Method repr$(tabs$="")
		Return tabs
	End Method
End Type

Type jsonobject Extends jsonvalue
	Field pairs:TList

	Method New()
		pairs=New TList
	End Method
	
	Method addnewpair(txt$,value:jsonvalue)
		pairs.addlast jsonpair.Create(txt,value)
	End Method
	
	Method repr$(tabs$="")
		t$="{"
		ntabs$=tabs+"~t"
		op:jsonpair=Null
		For p:jsonpair=EachIn pairs
			If op Then t:+","
			t:+"~n"+ntabs+p.repr(ntabs)
			op=p
		Next
		t:+"~n"+tabs+"}"
		Return t
	End Method
	
	Method getvalue:jsonvalue(name$)
		For p:jsonpair=EachIn pairs
			If p.name=name
				Return p.value
			EndIf
		Next
	End Method
	
	Method getstringvalue$(name$)
		v:jsonstringvalue=jsonstringvalue(getvalue(name))
		If v
			Return v.txt
		EndIf
	End Method
	
	Method getnumbervalue:Double(name$)
		v:jsonnumbervalue=jsonnumbervalue(getvalue(name))
		If v
			Return v.number
		EndIf
	End Method
	
	Method getliteralvalue(name$)
		v:jsonliteralvalue=jsonliteralvalue(getvalue(name))
		If v
			Return v.value
		EndIf
	End Method
	
	Method getarrayvalue:jsonarray(name$)
		v:jsonarray=jsonarray(getvalue(name))
		Return v
	End Method
	
	Method getobjectvalue:jsonobject(name$)
		v:jsonobject=jsonobject(getvalue(name))
		Return v
	End Method
				
	
End Type

Type jsonpair
	Field name$,value:jsonvalue

	Function Create:jsonpair(name$,value:jsonvalue)
		p:jsonpair=New jsonpair
		p.name=name
		p.value=value
		Return p
	End Function

	Method repr$(tabs$="")
		t$="~q"+name+"~q : "
		For i=1 To (Len(t)+7)/8
			tabs:+"~t"
		Next
		middo$=""
		For i=1 To (8-(Len(t) Mod 8))
			middo:+" "
		Next
		Return t+middo+value.repr(tabs)
	End Method
End Type

Type jsonarray Extends jsonvalue
	Field values:TList
	
	Method New()
		values=New TList
	End Method
	
	Method addvalue(v:jsonvalue)
		values.addlast v
	End Method
	
	Method repr$(tabs$="")
		t$="["
		ntabs$=tabs+"~t"
		ov:jsonvalue=Null
		For v:jsonvalue=EachIn values
			If ov Then t:+","
			t:+"~n"+ntabs+v.repr(ntabs)
			ov=v
		Next
		t:+"~n"+tabs+"]"
		Return t
	End Method
End Type


Type jsonstringvalue Extends jsonvalue
	Field txt$
	
	Function Create:jsonstringvalue(txt$,pretty=1)
		jsv:jsonstringvalue=New jsonstringvalue
		
		If pretty
			otxt$=""
			i=0
			For i=0 To Len(txt)-1
				Select Chr(txt[i])
				Case "~q"
					otxt:+"\~q"
				Case "\"
					otxt:+"\\"
				Case "/"
					otxt:+"\/"
				Case Chr(8)
					otxt:+"\b"
				Case Chr(12)
					otxt:+"\f"
				Case "~n"
					otxt:+"\n"
				Case "~r"
					otxt:+"\r"
				Case "~t"
					otxt:+"\t"
				Default
					otxt:+Chr(txt[i])
				End Select
			Next
			jsv.txt=otxt
		Else
			jsv.txt=txt
		EndIf
		Return jsv
	End Function
	
	Method repr$(tabs$="")
		Return "~q"+txt+"~q"
	End Method
End Type

Type jsonnumbervalue Extends jsonvalue
	Field number:Double
	
	Function Create:jsonnumbervalue(n:Double)
		jnv:jsonnumbervalue=New jsonnumbervalue
		jnv.number=n
		Return jnv
	End Function
	
	Method repr$(tabs$="")
		If number Mod 1=0
			Return String(Int(number))
		Else
			Return String(number)
		EndIf
	End Method
End Type

Type jsonliteralvalue Extends jsonvalue
	Field value
	'1 - true
	'0 - false
	'-1 - nil
	
	Function Create:jsonliteralvalue(value)
		jlv:jsonliteralvalue=New jsonliteralvalue
		jlv.value=value
		Return jlv
	End Function
	
	Method repr$(tabs$="")
		Select value
		Case 1
			Return "true"
		Case 0
			Return "false"
		Case -1
			Return "nil"
		End Select
	End Method
End Type



Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom

Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

Function ZoomX#(x#)
	Return (x - panx) * zoom + gwidth / 2
End Function
Function ZoomY#(y#)
	Return (y - pany) * zoom + gheight / 2
End Function

Function UnzoomX#(x#)
	Return (x - gwidth / 2) / zoom + panx
End Function
Function UnzoomY#(y#)
	Return (y - gheight / 2) / zoom + pany
End Function

Function DrawZoomPoly(poly#[],outline=False)
	poly=poly[..]
	While i < Len(poly)
		poly[i] = zoomx(poly[i])
		poly[i + 1] = zoomy(poly[i + 1]) 
		i:+ 2
	Wend
	If outline
		ox# = poly[0]
		oy# = poly[1]
		i = 2
		While i < Len(poly)
			DrawLine ox , oy , poly[i] , poly[i + 1]
			ox = poly[i]
			oy=poly[i+1]
			i:+ 2
			DrawLine poly[0],poly[1],ox,oy
		Wend
	Else
		DrawPoly poly
	EndIf
End Function

Function DrawZoomLine(ax# , ay# , bx# , by#)
	ax = zoomx(ax)
	ay = zoomy(ay)
	bx = zoomx(bx)
	by = zoomy(by)
	DrawLine ax,ay,bx,by
End Function
Function DrawZoomRect(x# , y# , width# , height#,zoomdimensions=1,filled=1)
	x = zoomx(x)
	y = zoomy(y)
	If zoomdimensions
		width:* zoom
		height:* zoom
	EndIf
	If filled
		DrawRect x , y , width , height
	Else
		DrawLine x , y , x + width , y
		DrawLine x + width , y , x + width , y + height
		DrawLine x , y , x , y + height
		DrawLine x , y + height , x + width , y + height
	EndIf
End Function
Function DrawZoomCircle(x# , y# , radius#)
	x = zoomx(x) 
	y = zoomy(y)
	radius:* zoom
	DrawOval x - radius , y - radius , 2 * radius , 2 * radius
End Function
Function DrawZoomText(txt$ , x# , y#)
	x = ZoomX(x)
	y = ZoomY(y)
	DrawText txt , x , y
End Function


Global numnetworks
Global allnetworks:TList
Type network
	Field number
	Field count
	Field money#
	Field root:node
	
	Method New()
		count=1
		numnetworks:+1
		number=numnetworks
		'money=1
	End Method
	
	Function save:jsonvalue()
		na:jsonarray=New jsonarray
		For tree:network=EachIn allnetworks
			na.addvalue tree.dosave()
		Next
		Return na
	End Function
	
	Method dosave:jsonobject()
		j:jsonobject=New jsonobject
		j.addnewpair( "number", jsonnumbervalue.Create( number ) )
		j.addnewpair( "count", jsonnumbervalue.Create( count ) )
		j.addnewpair( "money", jsonnumbervalue.Create( money ) )
		Return j
	End Method
	
	Function Load( na:jsonarray )
		For no:jsonobject=EachIn na.values
			allnetworks.addlast network.doload( no )
		Next
	End Function
	
	Function doload:network( j:jsonobject)
		tree:network=New network
		tree.number=j.getnumbervalue("number")
		tree.count=j.getnumbervalue("count")
		tree.money=j.getnumbervalue("money")
		Return tree
	End Function
	
	Function init()
		numnetworks=0
		allnetworks=New TList
	End Function
	
	Function find:network( number )
		For tree:network=EachIn allnetworks
			If tree.number=number Return tree
		Next
		
	End Function
	
	Method addmoney( amount# )
		If Not allnetworks.contains( Self )
			allnetworks.addlast Self
		EndIf
		
		money:+amount
		
	End Method
End Type

Type product
	Field kind
End Type

Global numnodes
Global visibles:TList
Global nodestolink:TList
Type node
	Field number
	Field x#,y#
	Field up:node,upnum
	Field down:TList
	Field tree:network
	
	Method New()
		down=New TList
		tree=New network
		tree.root=Self
		numnodes:+1
		number=numnodes
	End Method
	
	Method save:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair "number", jsonnumbervalue.Create( number )
		j.addnewpair "x", jsonnumbervalue.Create( x )
		j.addnewpair "y", jsonnumbervalue.Create( y )
		If up
			j.addnewpair "up", jsonnumbervalue.Create( up.number )
		EndIf
		If down.count()
			j.addnewpair "down",jsonnumbervalue.Create( down.count() )
		EndIf
		j.addnewpair "tree", jsonnumbervalue.Create( tree.number )
		Return j
	End Method
	
	Function Load:node( j:jsonobject )
		n:node=New node
		n.number=j.getnumbervalue("number")
		n.x=j.getnumbervalue("x")
		n.y=j.getnumbervalue("y")

		n.tree=network.find( j.getnumbervalue("tree") )
		If Not n.tree
			n.tree=New network
			n.tree.number=number
			n.tree.count=1
		EndIf

		If j.getvalue("up") Or j.getvalue("down")
			nodestolink.addlast n
			If j.getvalue("up")
				n.upnum=j.getnumbervalue("up")
			EndIf
		EndIf
		
		Return n
	End Function
	
	Function init()
		numnodes=0
		visibles=New TList
	End Function
	
	Function Create:node(x#,y#)
		n:node=New node
		n.x=x
		n.y=y

		Return n
	End Function
	
	Function pick:node(x#,y#,nodes:TList=Null,maxdist#=-1)
		mindist#=-1
		closest:node=Null
		For n:node=EachIn nodes
			d#=dist(x,y,n.x,n.y)
			If (d<mindist Or mindist=-1) And (d<=maxdist Or maxdist=-1)
				closest=n
				mindist=d
			EndIf
		Next
		Return closest
	End Function
	
	Method free:TList(nodes:TList)	
		l:TList=New TList
		For n:node=EachIn nodes
			If n.tree<>tree
				If costto(n)<= 1 + tree.money+n.tree.money
					l.addlast n
				EndIf
			EndIf
		Next
		Return l
	End Method
	
	Method root:node()
		n:node=Self
		While n.up
			n=n.up
		Wend
		Return n
	End Method
	
	Method costto#(n:node)
		dx#=n.x-x
		dy#=n.y-y
		d#=Sqr(dx*dx+dy*dy)
		cost#=4*(d/shellsize)
		Return cost
	End Method
		
	Method link(n:node)
		If n.up
			If up
				If n.tree.number<tree.number
					n.link Self
					Return
				EndIf
				linkdown n
				n.reverse Self
				n.settree tree
				Return
			Else
				n.link Self
			EndIf
		Else
			linkdown n
			n.up=Self
			n.settree tree
		EndIf
	End Method
	
	Method linkdown(n:node)
		down.addlast n
		cost#=costto(n)
		tree.addmoney 1-cost + n.tree.money
		If allnetworks.contains( n.tree )
			allnetworks.remove n.tree
		EndIf
	End Method
	
	Method reverse(n:node)
		oup:node=up
		up=n
		If oup
			oup.down.remove Self
			down.addlast oup
			oup.reverse Self
		EndIf
	End Method
	
	Method settree(newtree:network)
		newtree.count:+1
		tree=newtree
		For n:node=EachIn down
			n.settree newtree
		Next
	End Method

	Method draw()
		SetColor 255,255,255
		For n:node=EachIn down
			drawdirline x,y,n.x,n.y
		Next
		Drawzoomcircle x,y,5
		'DrawzoomText tree.number+","+tree.count,x+3,y+3
		'drawzoomtext jobnames[job],x+3,y+3
	End Method
End Type

Type segment
	Field number
	Field angle#
	Field r#
	Field width#
	Field nodes:TList
	
	Method save:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair "number", jsonnumbervalue.Create( number )
		j.addnewpair "angle", jsonnumbervalue.Create( angle )
		j.addnewpair "r", jsonnumbervalue.Create( r )
		j.addnewpair "width", jsonnumbervalue.Create( width )
		na:jsonarray=New jsonarray
		j.addnewpair "nodes", na
		For n:node=EachIn nodes
			na.addvalue n.save()
		Next
		Return j
	End Method
	
	Function Load:segment( j:jsonobject )
		se:segment=New segment
		se.number=j.getnumbervalue("number")
		se.angle=j.getnumbervalue("angle")
		se.r=j.getnumbervalue("r")
		se.width=j.getnumbervalue("width")
		na:jsonarray=j.getarrayvalue("nodes")
		For no:jsonobject=EachIn na.values
			se.nodes.addlast node.Load(j)
		Next
		Return se
	End Function
	
	Method New()
		nodes=New TList
	End Method
	
	Function Create:segment(number,angle#,r#,width#)
		se:segment=New segment
		se.number=number
		se.angle=angle
		se.r=r
		se.width=width
		
		For c=1 To 20
			an#=angle+Rnd(0,1)*width
			pr#=Rnd(r,r+shellsize)
			n:node=node.Create( Cos(an)*pr, Sin(an)*pr )
			se.nodes.addlast n
		Next
		Return se
	End Function
	
	Method draw()
		r1#=r
		r2#=r+shellsize
		ox1#=Cos(angle)*r1
		oy1#=Sin(angle)*r1
		ox2#=Cos(angle)*r2
		oy2#=Sin(angle)*r2
		drawzoomline ox1,oy1,ox2,oy2
		
		'return
		an#=angle
		Local poly#[]
		While an<angle+width-1
			an:+10
			If an>angle+width an=angle+width
			x1#=Cos(an)*r1
			y1#=Sin(an)*r1
			x2#=Cos(an)*r2
			y2#=Sin(an)*r2
			drawzoomline ox1,oy1,x1,y1
			drawzoomline ox2,oy2,x2,y2
			poly=[ox1,oy1,ox2,oy2,x2,y2,x1,y1]
			SetAlpha .2
			drawzoompoly poly
			SetAlpha 1
			ox1=x1
			oy1=y1
			ox2=x2
			oy2=y2
		Wend
		drawzoomline x1,y1,x2,y2
		
		'For n:node=EachIn nodes
		'	n.draw
		'Next
		
	End Method
End Type

Global shellsize#
Global curshell:shell
Type shell
	Field level
	Field segments:TList
	Field up:shell,down:shell
	
	Function save:jsonvalue()
		sa:jsonarray=New jsonarray
		sh:shell=curshell
		While sh.up
			sh=sh.up
		Wend
		While sh
			j:jsonobject=sh.dosave()
			sa.addvalue j
			sh=sh.down
		Wend
		Return sa
	End Function
	
	Method dosave:jsonobject()
		j:jsonobject=New jsonobject
		j.addnewpair( "level", jsonnumbervalue.Create( level ) )
		
		sa:jsonarray=New jsonarray
		j.addnewpair( "segments", sa )
		For se:segment=EachIn segments
			sa.addvalue se.save()
		Next
		Return j
	End Method
	
	Function Load( sa:jsonarray )
		curshell=Null
		For so:jsonobject=EachIn sa.values
			sh:shell=shell.doload(so)
			If curshell
				curshell.down=sh
				sh.up=curshell
			EndIf
			curshell=sh
		Next
	End Function
	
	Function doload:shell( j:jsonobject )
		sh:shell=New shell
		sh.level=j.getnumbervalue( "level" )
		sa:jsonarray=j.getarrayvalue( "segments" )
		For j:jsonobject=EachIn sa.values
			sh.segments.addlast segment.Load( j )
		Next
		
		Return sh
	End Function
	
	Method New()
		segments=New TList
	End Method
	
	Function init()
		shellsize#=(gwidth+gheight)/2
		curshell=shell.Create(0)
	End Function
	
	Function Create:shell(level)
		sh:shell=New shell
		sh.level=level
		Return sh
	End Function
	
	Method getsegment:segment(n,angle#)
		If angle<0 angle:+360
		If n<level
			Return up.getsegment(n,angle)
		ElseIf n>level
			If Not down
				down=shell.Create(level+1)
				down.up=Self
			EndIf
			Return down.getsegment(n,angle)
		Else
			segwidth#=360.0/(2*level+1)
			segnum=angle/segwidth
			getan#=segnum*segwidth
			For se:segment=EachIn segments
				If se.number=segnum Return se
			Next
			se:segment=segment.Create(segnum,getan,shellsize*level,segwidth)
			segments.addlast se
			Return se
		EndIf
	End Method
	
	Method draw()
		r#=level*shellsize
		ox#=r
		oy#=0
		an#=0
		While an<360
			an:+30.0/(level+1)
			If an>360 an=360
			x#=Cos(an)*r
			y#=Sin(an)*r
			DrawzoomLine ox,oy,x,y
			ox=x
			oy=y
		Wend
		'SetAlpha .5
		'For se:segment=EachIn segments
		'	se.draw
		'Next
		'SetAlpha 1
	End Method
	
	Method getvisible:TList(minlevel,maxlevel,minan#,maxan#,dir=0,l:TList=Null)
		hops:+1
		curshell=Self
		If Not l l=New TList
		If dir>=0 And level>=minlevel
			If up up.getvisible(minlevel,maxlevel,minan,maxan,1,l)
		EndIf
		If dir<=0 And level<=maxlevel
			If Not down
				down=shell.Create(level+1)
				down.up=Self
			EndIf
			down.getvisible(minlevel,maxlevel,minan,maxan,-1,l)
		EndIf
		If level<minlevel Or level>maxlevel Return l
		'draw
		segwidth#=360.0/(2*level+1)
		an#=minan
		l.addlast getsegment(level,minan)
		While an<maxan
			an:+segwidth
			If an>maxan an=maxan
			se:segment=getsegment(level,an)
			If Not l.contains(se) l.addlast se
		Wend
		Return l
	End Method	
End Type

Function dorect:TList(x1#,y1#,x2#,y2#)
	'SetColor 255,0,0
	'SetAlpha .5
	'Drawzoomrect x1,y1,x2-x1,y2-y1
	'SetAlpha 1
	'SetColor 255,255,255
	
	midx#=(x1+x2)/2
	midy#=(y1+y2)/2
	midan#=ATan2(midy,midx)
	Local points#[]=[x1,y1,x2,y1,x2,y2,x1,y2,midx,midy]
	minr=-1
	maxr=-1
	minan#=0
	maxan#=0
	For i=0 To Len(points)-1 Step 2
		dx#=points[i]
		dy#=points[i+1]
		r#=Sqr(dx*dx+dy*dy)
		If r<minr Or minr=-1 minr=r
		If r>maxr Or maxr=-1 maxr=r
		an#=ATan2(dy,dx)
		diff#=andiff(an,midan)
		If diff<minan minan=diff
		If diff>maxan maxan=diff
	Next
	
	minlevel=minr/shellsize
	maxlevel=maxr/shellsize
	
	segs:TList=curshell.getvisible(minlevel,maxlevel,minan+midan,maxan+midan)
	
	visibles:TList=New TList
	For se:segment=EachIn segs
		'se.draw
		For n:node=EachIn se.nodes
			visibles.addlast n
			If n.up And (Not visibles.contains(n.up))
				visibles.addlast n.up
			EndIf
		Next
	Next
	
	
	'SetColor 255,0,0
	'drawzoomline 0,0,Cos(midan+minan)*maxr,Sin(midan+minan)*maxr
	'drawzoomline 0,0,Cos(midan+maxan)*maxr,Sin(midan+maxan)*maxr
	'SetColor 255,255,255
	
	Return visibles
End Function

Global nbookmark
Global bookmarks:bookmark[]
Type bookmark
	Field x#,y#
	Field an#
	
	Method New()
		bookmarks:+[Self]
	End Method
	
	Function save:jsonvalue()
		ba:jsonarray=New jsonarray
		For b:bookmark=EachIn bookmarks
			ba.addvalue b.dosave()
		Next
	End Function
	
	Method dosave:jsonobject()
		j:jsonobject=New jsonobject
		j.addnewpair "x", jsonnumbervalue.Create( x )
		j.addnewpair "y", jsonnumbervalue.Create( y )
		j.addnewpair "an", jsonnumbervalue.Create( an )
		Return j
	End Method
	
	Function Load(ba:jsonarray)
		For bo:jsonobject=EachIn ba.values
			b:bookmark=New bookmark
			b.x=bo.getnumbervalue("x")
			b.y=bo.getnumbervalue("x")
			b.an=bo.getnumbervalue("x")
		Next
	End Function
	
	Function init()
		bookmarks=New bookmark[0]
	End Function
	
	Function Create:bookmark(x#,y#)
		closest:bookmark=Null
		mindist#=-1
		For b:bookmark=EachIn bookmarks
			dx#=x-b.x
			dy#=y-b.y
			d#=dist(dx,dy)
			If (d<mindist Or mindist=-1) And d<gwidth/2
				closest=b
				mindist=d
			EndIf
		Next
		If closest
			b=closest
			d#=dist(x,y,b.x,b.y)
			If d<30
				i=0
				While bookmarks[i]<>b
				 	i:+1
				Wend
				bookmarks=bookmarks[..i]+bookmarks[i+1..]
				Return
			EndIf
		Else
			b:bookmark=New bookmark
		EndIf
		b.x=x
		b.y=y
		b.an=Len(bookmarks)*15
		Return b
	End Function
	
	Method draw()
		dx#=Cos(an)*10
		dy#=Sin(an)*10
		nx#=-dy
		ny#=dx
		SetColor 0,0,255
		drawzoomline x-dx,y-dy,x+dx,y+dy
		drawzoomline x-nx,y-ny,x+nx,y+ny
	End Method
End Type


Function drawdirline(x1#,y1#,x2#,y2#)
	DrawzoomLine x1,y1,x2,y2
	dx#=x2-x1
	dy#=y2-y1
	an#=ATan2(dy,dx)
	midx#=x1+dx/2
	midy#=y1+dy/2
	DrawzoomLine midx,midy,midx+Cos(an+150)*10,midy+Sin(an+150)*10
	DrawzoomLine midx,midy,midx+Cos(an-150)*10,midy+Sin(an-150)*10
End Function

Function dist#(x1#,y1#,x2#=0,y2#=0)
	dx#=x2-x1
	dy#=y2-y1
	Return Sqr(dx*dx+dy*dy)
End Function

Type tstate
	Method update()
	End Method
	
	Method draw()
	End Method
End Type

Type defaultstate Extends tstate
	Field hover:node
	
	Method update()
		If MouseDown(2)
			state=dragcamstate.Create(zoomx(mx),zoomy(my))
		EndIf
		
		If MouseDown(1)
			n:node=node.pick(mx,my,visibles,20)
			If n
				state=drawpathstate.Create(n)
			EndIf
		EndIf
		
		If KeyHit(KEY_S)
			bookmark.Create(mx,my)
		EndIf
		
		If KeyHit(KEY_TAB)
			state=New pantostate
		EndIf
		
		If KeyHit(KEY_SPACE)
			state=New findrootstate
		EndIf
		
		If KeyHit(KEY_G)
			save
		EndIf
		If KeyHit(KEY_L)
			Load
		EndIf
		
		hover=node.pick(mx,my,visibles,20)
	End Method
	
	Method draw()
		If hover
			For n:node=EachIn hover.free(visibles)
				If hover.costto(n)>1
					SetColor 255,0,0
				Else
					SetColor 255,255,0
				EndIf
				Drawzoomcircle n.x,n.y,5
			Next
			SetColor 255,255,255
		EndIf
		
	End Method
End Type
	
Type drawpathstate Extends tstate
	Field startnode:node
	
	Function Create:drawpathstate(startnode:node)
		s:drawpathstate=New drawpathstate
		s.startnode=startnode
		Return s
	End Function
	
	Method getfree:TList()
	End Method
	
	Method update()
		If Not MouseDown(1)
			free:TList=startnode.free(visibles)
			n:node=node.pick(mx,my,free,20)
			If n
				startnode.link(n)
			EndIf
			state=New defaultstate
		EndIf
	End Method
	
	Method draw()
		DrawdirLine startnode.x,startnode.y,mx,my

		For n:node=EachIn startnode.free(visibles)
			If startnode.costto(n)>1
				SetColor 255,0,0
			Else
				SetColor 255,255,0
			EndIf
			Drawzoomcircle n.x,n.y,5
		Next
		SetColor 255,255,255
	End Method
End Type

Type dragcamstate Extends tstate
	Field gx#,gy#
	Field opanx#,opany#
	
	Function Create:dragcamstate(gx#,gy#)
		s:dragcamstate=New dragcamstate
		s.gx=gx
		s.gy=gy
		s.opanx=panx
		s.opany=pany
		Return s
	End Function
	
	Method update()
		panx=opanx-(zoomx(mx)-gx)/zoom
		pany=opany-(zoomy(my)-gy)/zoom
		'gx=zoomx(mx)
		'gy=zoomy(my)
		If Not MouseDown(2)
			state=New defaultstate
		EndIf
	End Method
End Type

Type pantostate Extends tstate
	Field sx#,sy#
	Field ex#,ey#
	Field t#
	
	Method New()
		If Len(bookmarks)
			nbookmark=(nbookmark+1) Mod Len(bookmarks)
			b:bookmark=bookmarks[nbookmark]
			sx=panx
			sy=pany
			ex=b.x
			ey=b.y
		Else
			state=New defaultstate
		EndIf
	End Method
		
	Function Create:pantostate(sx#,sy#,ex#,ey#)
		s:pantostate=New pantostate
		s.sx=sx
		s.sy=sy
		s.ex=ex
		s.ey=ey
		Return s
	End Function
	
	Method update()
		If KeyHit(KEY_TAB)
			state=New pantostate
		EndIf
		t:+.01
		If t>=1
			state=New defaultstate
			t=1
		EndIf
		panx=sx*(1-t)+ex*t
		pany=sy*(1-t)+ey*t
	End Method
End Type

Type findrootstate Extends tstate
	Field n:node
	Field t#
	
	Method New()
		closest:node=Null
		mindist#=-1
		For n:node=EachIn visibles
			If n.tree.count>1 And n.up
				dx#=mx-n.x
				dy#=my-n.y
				d#=dist(dx,dy)
				If d<mindist Or mindist=-1
					closest=n
					mindist=d
				EndIf
			EndIf
		Next
		If closest
			n=closest
			bookmark.Create(mx,my)
		Else
			state=New defaultstate
		EndIf
	End Method
	
	Method update()
		t:+.1
		tx#=n.x*(1-t)+n.up.x*t
		ty#=n.y*(1-t)+n.up.y*t
		panx:+(tx-panx)*.1
		pany:+(ty-pany)*.1
		If t>=1
			n=n.up
			If n.up
				t=0
			Else
				state=New defaultstate
			EndIf
		EndIf
	End Method
End Type

Global mx,my,mz
Global state:tstate
Global gwidth,gheight

Function init()
	SeedRnd MilliSecs()
	
	gwidth=800
	gheight=800
	AppTitle="Penpals"
	Graphics gwidth,gheight,0
	SetBlend ALPHABLEND
	panx#=0
	pany#=0
	zoom#=1
	tzoom#=zoom
	mz=MouseZ()
	
	bookmark.init
	network.init
	node.init

	shell.init
		
	done=0
	state=New defaultstate
End Function

Function save()
	f:TStream=WriteFile("save.txt")
	
	j:jsonobject=New jsonobject
	j.addnewpair "network", network.save()
	j.addnewpair "shell", shell.save()
	j.addnewpair "bookmark",bookmark.save()

	co:jsonobject=New jsonobject
	j.addnewpair "camera",co
	co.addnewpair "x",jsonnumbervalue.Create(panx)
	co.addnewpair "y",jsonnumbervalue.Create(pany)
	
	f.WriteLine j.repr()
	CloseFile f
	Print "saved"
End Function

Function Load()
End Function


init
While Not done
	mx=unzoomx(MouseX())
	my=unzoomy(MouseY())
	
	'omz=mz
	'mz=MouseZ()
	'diff=mz-omz+KeyHit(KEY_UP)-KeyHit(KEY_DOWN)
	'zoom:*(1-diff*.1)
	'DrawText mz,400,0
	
	state.update
	
	rsize=(shellsize/2)/zoom
	visibles:TList=dorect( panx-rsize,pany-rsize,panx+rsize,pany+rsize )
	
	For n:node=EachIn visibles
		n.draw
	Next
	
	For b:bookmark=EachIn bookmarks
		b.draw
	Next
	
	state.draw
	
	SetColor 255,255,255
	richest:network=Null
	biggest:network=Null
	total#=0
	y=0
	For tree:network=EachIn allnetworks
		If richest=Null Or tree.money>richest.money richest=tree
		If biggest=Null Or tree.count>biggest.count biggest=tree
		DrawText tree.money,500,y
		y:+15
		total:+tree.money
	Next
	DrawText total,500,y
	If richest
		DrawText "Richest: "+richest.number+" with "+richest.money,0,0
	EndIf
	If biggest
		DrawText "Biggest: "+biggest.number+" with "+biggest.count,0,15
	EndIf
	DrawText allnetworks.count()+" networks",0,30
	DrawText numnetworks+" points",0,45
	
	Flip
	Cls
	

	If KeyHit(KEY_ESCAPE) Or AppTerminate()
		done=1
	EndIf
Wend