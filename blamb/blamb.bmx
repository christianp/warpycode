'************************************
'maths

Function andiff#(an1#,an2#)
	d#=an1-an2
	If d<-180 d:+360
	If d>180 d:-360
	Return d
End Function


'*************************************
'file reading

Function readlines:TList(f:TStream) 
	lines:TList = New TList
	While Not Eof(f) 
		lines.AddLast f.ReadLine()
	Wend
	Return lines
End Function

Function counttabs(line$)
	n=0
	While line[n]=9
		n:+1
	Wend
	Return n
End Function

Function words$[](txt$)
	l:TList=New TList
	s=0
	c=0
	While c<Len(txt)
		If txt[c]=32
			l.addlast txt[s..c]
			s=c+1
		EndIf
		c:+1
	Wend
	l.addlast txt[s..c]
	Local words$[]
	words=New String[l.count()]
	i=0
	For w$=EachIn l
		words[i]=w
		i:+1
	Next
	Return words
End Function

Function nextblock$(blocks:TList)
	Return block(blocks.removefirst()).txt
End Function

Type block
	Field txt$
	Field subs:TList
	
	Method New()
		subs=New TList
	End Method
	
	Function Load:block(lines:TList)
		For l$=EachIn lines
			If Trim(l)="" lines.remove l
		Next
		b:block=New block
		b.txt=String(lines.removefirst())
		n=counttabs(b.txt)
		b.txt=b.txt[n..]
		While lines.count() And counttabs(String(lines.first()))>n
			b.subs.addlast block.Load(lines)
		Wend
		
		Return b
	End Function
	
	Method out(c=0)
		s$=""
		For i=1 To c
			s:+":"
		Next
		Print s+txt
		For b:block=EachIn subs
			b.out(c+1)
		Next
	End Method
End Type

Function makeblocks:TList(f:TStream)
	blocks:TList=New TList
	lines:TList=readlines(f)
	While lines.count()
		blocks.addlast block.Load(lines)
	Wend
	Return blocks
End Function


'******************************************
'actors and pieces and things like that

Type actor
	Field name$
	Field pieces:tmap
	Field constraints:TList
	
	Method New() 
		pieces = New tmap
		constraints=New TList
	End Method
	
	Function Load:actor(f:TStream) 
		a:actor=New actor

		blocks:TList=makeblocks(f)
		'For b:block=EachIn blocks
		'	b.out()
		'Next
		
		a.name$=nextblock(blocks)

		While blocks.count()
			b:block=block(blocks.removefirst())
			Select b.txt
			Case "piece"
				name$=nextblock(b.subs)
				If Not a.pieces.contains(name)
					a.pieces.insert(name,piece.Load(name,b.subs))
				Else
					Print "piece called '"+name+"' already exists"
				EndIf
			Case "constraint"
				Print "!!"
				kind$=nextblock(b.subs)
				weight#=Float(nextblock(b.subs))
				Select kind
				Case "link"
					a.constraints.addlast linkconstraint.Load(weight,b.subs,a.pieces)
				Case "angle"
					a.constraints.addlast angleconstraint.Load(weight,b.subs,a.pieces)
				Case "position"
					a.constraints.addlast posconstraint.Load(weight,b.subs,a.pieces)
				Case "force"
					a.constraints.addlast forceconstraint.Load(weight,b.subs,a.pieces)
				End Select
			End Select
		Wend
		Return a
	End Function
	
	Method out()
		Print "actor"
		For key$=EachIn pieces.keys()
			Print "piece "+key
			p:piece=piece(pieces.valueforkey(key))
			p.out()
		Next
		For c:constraint=EachIn constraints
			c.out()
		Next
	End Method
End Type

Type constraint
	Field weight#
	
	Function Load:constraint(weight#,blocks:TList,pieces:tmap)
		c:constraint=New constraint
		c.weight=weight
		Return c
	End Function
	
	Method apply()
		Return
	End Method
	
	Method out()
		Print "generic constraint"
	End Method
End Type


Type linkconstraint Extends constraint
	Field p1:piece,p2:piece
	Field ax#,ay#,an#,ad#
	Field balance#
	
	Function Load:constraint(weight#,blocks:TList,pieces:tmap)
		c:linkconstraint=New linkconstraint
		c.weight=weight
		p1$=nextblock(blocks)
		p2$=nextblock(blocks)
		c.p1=piece(pieces.valueforkey(p1))
		c.p2=piece(pieces.valueforkey(p2))
		c.ax#=Float(nextblock(blocks))-c.p1.hx
		c.ay#=Float(nextblock(blocks))-c.p1.hy
		c.an=ATan2(c.ay,c.ax)
		c.ad=Sqr(c.ax*c.ax+c.ay*c.ay)
		c.balance#=Float(nextblock(blocks))
		Return c
	End Function
	
	Method apply()
	
		dx#=p2.x-p1.x
		dy#=p2.y-p1.y
		an2#=an-ATan2(dy,dx)
		p1.nangle:+an2*weight
	
		d#=Sqr(dx*dx+dy*dy)
		dx:*(d-ad)/d
		dy:*(d-ad)/d
	
		p1.nx:+dx*(1-balance)*weight
		p1.ny:+dy*(1-balance)*weight
		p2.nx:-dx*balance*weight
		p2.ny:-dy*balance*weight
	End Method
	
	Method out()
		Print "link constraint"
		Print p1.name+" <-> "+p2.name
		Print String(ax)+" , "+String(ay)
		Print balance
	End Method
End Type

Type angleconstraint Extends constraint
	Field p:piece
	Field angle#
	
	Function Load:constraint(weight#,blocks:TList,pieces:tmap)
		c:angleconstraint=New angleconstraint
		c.weight=weight
		p$=nextblock(blocks)
		c.p=piece(pieces.valueforkey(p))
		c.angle=Float(nextblock(blocks))
		Return c
	End Function
	
	Method apply()
		da#=andiff(angle,p.angle)
		p.nangle:+da*weight
	End Method
	
	Method out()
		Print "angle constraint"
		Print p.name
		Print angle
	End Method
End Type

Type posconstraint Extends constraint
	Field pieces:TList
	Field signx,signy
	Field x#,y#
	
	Method New()
		pieces=New TList
		x=-1
		y=-1
	End Method
	
	Function Load:constraint(weight#,blocks:TList,pieces:tmap)
		c:posconstraint=New posconstraint
		c.weight=weight
		Local piecenames$[]
		piecenames=words(nextblock(blocks))
		For p$=EachIn piecenames
			c.pieces.addlast pieces.valueforkey(p)
		Next
		
		While blocks.count()
			Local l$[]
			l=words(nextblock(blocks))
			
			Select l[1]
			Case "<"
				sign=-1
			Case "="
				sign=0
			Case ">"
				sign=1
			End Select
			
			Select l[0]
			Case "x"
				c.signx=sign
				c.x=Float(l[2])
			Case "y"
				c.signy=sign
				c.y=Float(l[2])
			End Select
		Wend
		
		Return c
	End Function
	
	Method apply()
		For p:piece=EachIn pieces
			If x>=0
				If (p.x-x)*signx<0 Or (signx=0 And p.x<>x)
					dx#=x-p.x
					p.nx:+dx*weight
				EndIf
			EndIf
			If y>=0
				If (p.y-y)*signy<0 Or (signy=0 And p.y<>y)
					dy#=y-p.y
					p.ny:+dy*weight
				EndIf
			EndIf
		Next
	End Method
	
	Method out()
		Print "position constraint"
		a$=""
		For p:piece=EachIn pieces
			a:+p.name+" "
		Next
		Print a
		Print "x "+String(signx)+" "+String(x)
		Print "y "+String(signy)+" "+String(y)
	End Method
End Type

Type forceconstraint Extends constraint
	Field pieces:TList
	Field vx#,vy#
	
	Method New()
		pieces=New TList
	End Method
	
	Function Load:constraint(weight#,blocks:TList,pieces:tmap)
		c:forceconstraint=New forceconstraint
		c.weight=weight
		Local piecenames$[]
		piecenames=words(nextblock(blocks))
		For p$=EachIn piecenames
			c.pieces.addlast pieces.valueforkey(p)
		Next
		
		c.vx=Float(nextblock(blocks))
		c.vy=Float(nextblock(blocks))
		Return c
	End Function
	
	Method apply()
		For p:piece=EachIn pieces
			p.nx:+vx*weight
			p.ny:+vy*weight
		Next
	End Method
	
	Method out()
		Print "force constraint"
		a$=""
		For p:piece=EachIn pieces
			a:+p.name+" "
		Next
		Print a
		Print String(vx)+" , "+String(vy)
	End Method
End Type

Type piece
	Field name$
	Field img:timage,imgfile$
	Field hx#,hy#
	Field sx#,sy#
	Field angle#,nangle#
	Field x#,y#,nx#,ny#
	
	Function Load:piece(name$,blocks:TList)
		imgfile$=nextblock(blocks)
		hx#=Float(nextblock(blocks))
		hy#=Float(nextblock(blocks))
		p:piece=piece.Create(name,imgfile,hx,hy)
		While blocks.count()
			b:block=block(blocks.removefirst())
			command$=b.txt
			Select command
			Case "scale"
				p.sx#=Float(nextblock(b.subs))
				p.sy#=Float(nextblock(b.subs))
			Case "rotation"
				p.angle=Float(nextblock(b.subs))
			End Select
		Wend
		Return p
	End Function	
	
	Function Create:piece(name$,imgfile$,hx#,hy#)
		p:piece=New piece
		p.name=name
		p.imgfile=imgfile
		p.img=LoadImage(p.imgfile)
		p.hx=hx
		p.hy=hy
		SetImageHandle p.img,p.hx,p.hy
		Return p
	End Function
	
	Method out()
		Print imgfile
		Print String(hx)+" , "+String(hy)
	End Method
End Type



f:TStream=ReadFile("test.txt")
a:actor=actor.Load(f)
a.out()