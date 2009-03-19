Const OP_PLUS$="+",OP_MINUS$="-",OP_DIVIDE$="/",OP_MULTIPLY$="*",OP_EQUALS$="=",OP_BRACKET$=")"

Type atom
	Field l:atom,r:atom
	Field p:atom
	Field xpos,ypos
	Field myinstructions$
	Field myalpha#
	Field myred,mygreen,myblue
	
	Method New()
		myalpha=1
		myred=255
		mygreen=255
		myblue=255
	End Method
	
	Method setleft(a:atom)
		l=a
		l.p=Self
	End Method

	Method setright(a:atom)
		r=a
		r.p=Self
	End Method
	
	Method replacechild(a:atom,na:atom)
		If l=a
			setleft(na)
		ElseIf r=a
			setright(na)
		EndIf
	End Method
	
	Method totext$() Abstract
	Method width#() Abstract
	Method height#() Abstract
	
	Method place(x#=0,y#=0)
		xpos=x'*Cos(rotation)+y*Sin(rotation)
		ypos=y'*Cos(rotation)+x*Sin(rotation)
	End Method

	Method draw(offx#,offy#,rotation#,scale#,alpha#)
		If l l.draw(offx#,offy#,rotation#,scale#,alpha#)
		If r r.draw(offx#,offy#,rotation#,scale#,alpha#)
		SetAlpha myalpha*alpha
		SetColor myred,mygreen,myblue
		SetScale scale,scale
		SetRotation rotation
	End Method
	
	Method setcolours(alpha#=1,red=255,green=255,blue=255)
		If l l.setcolours(alpha,red,green,blue)
		If r r.setcolours(alpha,red,green,blue)
		myalpha=alpha
		myred=red
		mygreen=green
		myblue=blue
	End Method			
	
	Method pick:atom(x,y,instructions$="")
		myinstructions=instructions
		mywidth#=width#()
		myheight#=height()
		If x>xpos-mywidth#/2 And x<xpos+mywidth#/2 And y>ypos-myheight/2 And y<ypos+myheight/2
			DrawniceLine xpos-mywidth#/2,ypos-myheight/2,xpos+mywidth#/2,ypos-myheight/2
			DrawniceLine xpos-mywidth#/2,ypos+myheight/2,xpos+mywidth#/2,ypos+myheight/2
			DrawniceLine xpos-mywidth#/2,ypos-myheight/2,xpos-mywidth#/2,ypos+myheight/2
			DrawniceLine xpos+mywidth#/2,ypos-myheight/2,xpos+mywidth#/2,ypos+myheight/2
			If l
				a:atom=l.pick(x,y,instructions+"l")
				If a Return a
			EndIf
			If r
				a:atom=r.pick(x,y,instructions+"r")
				If a Return a
			EndIf
			Return Self
		Else
			Return Null
		EndIf
	End Method
	
	Method find:atom(instructions$)
		Print totext()
		If bracket(Self) Return l.find(instructions)
		If instructions="" Return Self
		Print "find "+instructions+" "+Chr(instructions[0])
		Select Chr(instructions[0])
		Case "."
			Return Self
		Case "l"
			Return l.find(instructions[1..])
		Case "r"
			Return r.find(instructions[1..])
		End Select
	End Method
	
	Method box(red,green,blue,alpha#,offx#,offy#,rotation#,scale#)
		mywidth#=width#()
		myheight#=height()
		SetAlpha alpha
		SetColor red,green,blue
		Local pos#[]=translate(xpos-mywidth#/2,ypos-myheight/2,rotation,scale)
		SetRotation rotation
		SetScale scale,scale
		DrawRect pos[0]+offx,pos[1]+offy,mywidth,myheight
		SetAlpha 1
		SetColor 255,255,255
	End Method
	
	Method printme(n,t$="")
		s$=""
		For c=1 To n
			s:+" "
		Next
		Print s+t
		If l l.printme(n+1)
		If r r.printme(n+1)
	End Method
	
	Method do#() Abstract
End Type

Type letter
	Field images:tlist
	Field frame#
	
	Function create:letter(s$)
		l:letter=New letter
		node:tnode=lettermap.findnode(s)
		If node
			l.images=tlist(node.value())
			drawletters.addlast l
		EndIf
		Return l
	End Function
	
	Method draw(x#,y#)
		If Not images Return
		If frame>29 frame=29
		GetScale sx#,sy#
		SetScale sx*.6,sy*.6
		DrawImage timage(images.valueatindex(frame)),x,y
		SetScale sx,sy
	End Method
End Type

Type divideletter Extends letter
	Method draws(x#,y#,off#[])
		If frame=0 Return
		DrawniceLine x,y,x+off[0]*frame/29,y+off[1]*frame/29
	End Method
End Type		

Type number Extends atom
	Field data$
	Field letters:tlist
	
	Function create:number(data$)
		a:number=New number
		a.data=data
		a.letters=New tlist
		While Len(data)
			a.letters.addlast letter.create(Chr(data[0]))
			data=data[1..]
		Wend
		Return a
	End Function
	
	Method totext$()
		Return data
	End Method
	
	Method width#()
		Return 30*letters.count()
	End Method
	
	Method height#()
		Return 30
	End Method
	
	Method draw(offx#,offy#,rotation#,scale#,alpha#)
		super.draw(offx#,offy#,rotation#,scale#,alpha#)
		x#=xpos-width#()/2
		y#=ypos-height()/2
		For c=0 To Len(data)-1
			Local pos#[]=translate(x,y,rotation,scale#)
			x:+30
			letter(letters.valueatindex(c)).draw(pos[0]+offx,pos[1]+offy)
		Next
	End Method
	
	Method printme(n,t$="")
		super.printme(n,data)
	End Method
	
	Method do#()
		Return Float(data)
	End Method
End Type

Type lineop Extends atom
	Field data$
	Field let:letter
	
	Function create:lineop(l:atom,r:atom,data$="+")
		a:lineop=New lineop
		a.setleft(l)
		a.setright(r)
		a.data=data
		Return a
	End Function
	
	Method totext$()
		Return l.totext()+" "+data+" "+r.totext()
	End Method
	
	Method width#()
		Return l.width#()+r.width#()+(TextWidth(data)+10)
	End Method
	
	Method height#()
		Return biggest(l.height(),r.height())
	End Method
	
	Method place(x#=0,y#=0)
		super.place(x,y)
		lwidth#=l.width#()
		rwidth#=r.width#()
		mywidth#=width#()
		myheight#=height()
		twidth#=TextWidth(data)
		x:-mywidth#/2
		l.place(x+lwidth#/2,y)
		x:+lwidth#+twidth#+10
		r.place(x+rwidth#/2,y)
	End Method
	
	Method draw(offx#,offy#,rotation#,scale#,alpha#)
		super.draw(offx#,offy#,rotation#,scale#,alpha#)
		theight#=TextHeight(data)
		x=xpos-width()/2+l.width()+5
		y=ypos-theight/2
		Local pos#[]=translate(x,y,rotation,scale#)
		'DrawText data,pos[0]+offx,pos[1]+offy
		'DrawniceRect pos[0]+offx,pos[1]+offy,5,5
		let.draw(pos[0]+offx,pos[1]+offy)
	End Method

	Method printme(n,t$="")
		super.printme(n,data)
	End Method
	
	Method do#()
		Return l.do()+r.do()
	End Method
End Type

Type plus Extends lineop
	Function create:plus(l:atom,r:atom,data$="+")
		a:plus=New plus
		a.setleft(l)
		a.setright(r)
		a.data=data
		a.let=letter.create("+")
		Return a
	End Function
End Type

Type minus Extends lineop
	Function create:minus(l:atom,r:atom,data$="-")
		a:minus=New minus
		a.setleft(l)
		a.setright(r)
		a.data=data
		a.let=letter.create("-")
		Return a
	End Function
End Type

Type equals Extends lineop
	Function create:plus(l:atom,r:atom,data$="=")
		a:plus=New plus
		a.setleft(l)
		a.setright(r)
		a.data=data
		a.let=letter.create("=")
		Return a
	End Function
End Type

Type divide Extends atom
	Field let:divideletter
	
	Function create:divide(l:atom,r:atom)
		'Print l.totext()
		'Print r.totext()
		a:divide=New divide
		If plus(l) Or minus(l) Or divide(l) l=bracket.create(l)
		If plus(r) Or minus(r) Or divide(r) r=bracket.create(r)

		a.setleft(l)
		a.setright(r)

		a.let = New divideletter
		drawletters.addlast a.let
		Return a
	End Function
	
	Method totext$()
		ltext$=l.totext()
		rtext$=r.totext()
		Return ltext+" / "+rtext
	End Method
	
	Method width#()
		ml:atom=l
		mr:atom=r
		lwidth#=l.width#()
		rwidth#=r.width#()
		If lwidth#>rwidth# 
			mywidth#=lwidth#
		Else
			mywidth#=rwidth#
		EndIf
		Return mywidth#+5
	End Method
	
	Method height#()
		Return l.height()+r.height()+20
	End Method
	
	Method place(x#=0,y#=0)
		super.place(x,y)
		mywidth#=width()
		lheight#=l.height()
		rheight#=r.height()
		y:-height()/2
		l.place(x,y+lheight/2)
		y:+lheight+20
		r.place(x,y+rheight/2)
	End Method
	
	Method draw(offx#,offy#,rotation#,scale#,alpha#)
		super.draw(offx#,offy#,rotation#,scale#,alpha#)
		mywidth#=width#()
		SetLineWidth 3*scale
		Local pos#[]=translate(xpos-mywidth#/2,ypos-height()/2+l.height()+10,rotation,scale#)
		Local off#[]=translate(mywidth,0,rotation,scale#)
		'DrawniceLine pos[0]+offx,pos[1]+offy,pos[0]+offx+off[0],pos[1]+offy+off[1]
		let.draws(pos[0]+offx,pos[1]+offy,off)
		SetLineWidth 1
		'box(x,y)
	End Method
	
	Method printme(n,t$="")
		super.printme(n,OP_DIVIDE)
	End Method

	Method do#()
		Return l.do()/r.do()
	End Method
End Type

Type multiply Extends lineop
	Function create:multiply(l:atom,r:atom,data$="*")
		a:multiply=New multiply
		If plus(l) Or minus(l) l=bracket.create(l)
		If plus(r) Or minus(r) r=bracket.create(r)

		a.setleft(l)
		a.setright(r)

		a.data=data
		a.let=letter.create("*")
		Return a
	End Function
End Type

Type bracket Extends atom
	Field llet:letter
	Field rlet:letter
	Function create:atom(l:atom)
		'Print "bracket "+l.totext()
		a:bracket=New bracket
		a.setleft(l)
		a.llet=letter.create("(")
		a.rlet=letter.create(")")
		Return a
	End Function
	
	Method width#()
		Return l.width#()+16
	End Method
	
	Method height#()
		Return l.height()*1.1
	End Method
	
	Method totext$()
		Return "( "+l.totext()+" )"
	End Method
	
	Method place(x#=0,y#=0)
		super.place(x,y)
		l.place(x,y)
	End Method
	
	Method draw(offx#,offy#,rotation#,scale#,alpha#)
		super.draw(offx#,offy#,rotation#,scale#,alpha#)
		lwidth#=l.width()
		lheight#=l.height()
		scaley#=(lheight*1.1)/30
		SetScale scale,scale*scaley
		Local pos1#[]=translate(xpos-lwidth/2-15,ypos-(30*scaley)/2,rotation,scale#)
		'DrawImage lbracket,pos1[0]+offx,pos1[1]+offy
		llet.draw(pos1[0]+offx,pos1[1]+offy)
		Local pos2#[]=translate(xpos+lwidth/2-15,ypos-(30*scaley)/2,rotation,scale#)
		'DrawImage rbracket,pos2[0]+offx,pos2[1]+offy
		rlet.draw(pos2[0]+offx,pos2[1]+offy)
	End Method
	
	Method printme(n,t$="")
		l.printme(n,t)
	End Method

	Method do#()
		Return l.do()
	End Method
End Type

Type placeholder Extends atom
	Field mywidth#,myheight#
	Field swidth#,sheight#
	Field startd#
	Function create:placeholder(width#,height#)
		a:placeholder=New placeholder
		a.mywidth=width
		a.myheight=height
		a.swidth=width
		a.sheight=height
		
		Return a
	End Function
	
'	Method place(x,y)
'		super.place(x,y)
'		mywidth:+(twidth-mywidth)*.1
'		myheight:+(theight-myheight)*.1
'	End Method
	
	Method width#()
		Return mywidth
	End Method
	
	Method height#()
		Return myheight
	End Method
	
	Method do#()
		Return 0
	End Method
	
	Method totext$()
		Return ""
	End Method
End Type

Type atomtree
	Field root:atom
	Field mode
	Field x#,y#
	Field r#,s#
	Field vx#,vy#,vr#
	Field alpha#
	Field targtree:atomtree,targatom:atom
	
	Function create:atomtree(mode,x#,y#,r#,s#,equation:tlist=Null,root:atom=Null)
		at:atomtree=New atomtree
		atomtrees.addlast at
		at.mode=mode
		If equation
			at.root=rpn(equation)
		Else
			at.root=root
			If root.p
				If root=root.p.l
					root.p.l=Null
				Else
					root.p.r=Null
				EndIf
				root.p=Null
			EndIf
		EndIf
		at.x=x
		at.y=y
		at.r=r
		at.s=s
		at.alpha=1
		
		Return at
	End Function
	
	Method update()
		Select mode
		Case 0 'stay in the same place, wobble
			vr=vr*.9-Sin(r)*5
		Case 1 'move to target
			Local pos#[]=translate(targatom.xpos,targatom.ypos,targtree.r,targtree.s)
			dx#=pos[0]+targtree.x-x
			dy#=pos[1]+targtree.x-y
			'vx=vx*.5+dx*.1
			'vy=vy*.5+dy*.1
			'vx=dx*.2
			'vy=dy*.2
			vx:+Sgn(vx)
			vy:+Sgn(vy)
			vr=vr*.9+Sin(targtree.vr-r)
			
			d#=dx*dx+dy*dy
			'If d<20
			If (vy<>0 And dy*Sgn(vy)<0) Or (vx<>0 And dx*Sgn(vx)<0)
				x:+dx
				y:+dy
				targatom.p.replacechild(targatom,root)
				atomtrees.remove Self
				dx=targatom.xpos
				dy=targatom.ypos
				an=ATan2(Sgn(dy),Sgn(dx))+90
				If Abs(vx)<.1 vx=0
				If Abs(vy)<.1 vy=0
				Local rot#[]=translate(dx,dy,-r,s)
				If Abs(rot[0])<5 rot[0]=0
				dotprod#=vx*Cos(an)+vy*Sin(an)
				If dx=0 And dy=0 dotprod=0
				v#=vx*vx+vy*vy
				targtree.vr:+.0003*v*dotprod'*Sgn(rot[0])
			EndIf
			If placeholder(targatom)
				ph:placeholder=placeholder(targatom)
				dd#=Sqr(d)/ph.startd
				ph.mywidth=ph.swidth*dd+root.width()*(1-dd)
				ph.myheight=ph.sheight*dd+root.height()*(1-dd)
			EndIf
		Case 2 'fall
			vy:+.5
			alpha:*.98
			If y>900 
				atomtrees.remove Self
			EndIf
		End Select
		x:+vx
		y:+vy
		r:+vr
		If Abs(vr)<.1 vr=0
		
		root.place()
	End Method
	
	Method draw()
		root.draw(x,y,r,s,alpha)
	End Method
	
	Method replaceatom(instructions$,newequation$)
		Print "replace "+instructions+" "+newequation
		'Print newbit.totext()
		a:atom=root.find(instructions)
		If newequation="do"
			newbit:atom=number.create(nicestring(a.do()))
		Else
			newbit:atom=rpn(splitstring(newequation))
		EndIf
		na:atom=a
		'Print a.totext()
		While bracket(na.p)
			Print na.totext()
			na=na.p
		Wend
		'Print na.totext()
		'Print na.p.totext()
		p:atom=na.p
		ph:placeholder=placeholder.create(na.width(),na.height())
		If p
			'Print p.totext()
			p.replacechild(na,ph)
		EndIf
		a.p=Null
		Local pos#[]=translate(a.xpos,a.ypos,r,s)
		falltree:atomtree=atomtree.explode(a,Self)
		
		side=Rand(0,1)
		If side
			side=Rand(0,1)
			newtree:atomtree=atomtree.create(1,pos[0]+x,side*800,r,s,Null,newbit)
			newtree.vy=1-side*2
		Else
			side=Rand(0,1)
			newtree:atomtree=atomtree.create(1,side*800,pos[1]+y,r,s,Null,newbit)
			newtree.vx=1-side*2
		EndIf
		newtree.targtree=Self
		newtree.targatom=ph
		ph.startd=Sqr((newtree.x-falltree.x)^2+(newtree.y-falltree.y)^2)
	End Method
	
	Function explode:atomtree(a:atom,maintree:atomtree)
		If a.l 'And Not placeholder(a.l)
			atomtree.explode(a.l,maintree)
			a.replacechild(a.l,placeholder.create(0,0))
		EndIf
		If a.r 'And Not placeholder(a.l)
			atomtree.explode(a.r,maintree)
			a.replacechild(a.r,placeholder.create(0,0))
		EndIf
		Local pos#[]=translate(a.xpos,a.ypos,maintree.r,maintree.s)
		falltree:atomtree=atomtree.create(2,pos[0]+maintree.x,pos[1]+maintree.y,maintree.r,maintree.s,Null,a)
		dir=Rand(0,1)*2-1
		speed#=Rnd(0.5,1)
		falltree.vx=speed*7*dir
		falltree.vy=-speed*20
		falltree.vr=speed*30*dir
		Return falltree
	End Function
End Type

Function rpn:atom(bits:tlist)
	stack:tlist=New tlist
	
	l:atom=Null
	r:atom=Null
	
	link:TLink=bits.firstlink()
	While link<>Null
		bit$=String(link.value())
		'Print bit+" ("+String(stack.count())+")"
		Select bit
		Case OP_EQUALS
			r=atom(stack.removelast())
			l=atom(stack.removelast())
			stack.addlast equals.create(l,r,"=")
		Case OP_PLUS
			r=atom(stack.removelast())
			l=atom(stack.removelast())
			stack.addlast plus.create(l,r)
		Case OP_MINUS
			r=atom(stack.removelast())
			l=atom(stack.removelast())
			stack.addlast minus.create(l,r,"-")
		Case OP_DIVIDE
			r=atom(stack.removelast())
			l=atom(stack.removelast())
			stack.addlast divide.create(l,r)
		Case OP_MULTIPLY
			r=atom(stack.removelast())
			l=atom(stack.removelast())
			stack.addlast multiply.create(l,r)
		Case OP_BRACKET
			Print "bracket"
			l=atom(stack.removelast())
			stack.addlast bracket.create(l)
		Default
			stack.addlast number.create(bit)
		End Select
		
		link=link.nextlink()
	Wend
	
	Return atom(stack.last())
End Function

Function splitstring:TList(in$)
	c=0
	in.trim()
	out:TList=New TList
	inbrackets=0
	beeninbrackets=0
	While c<in.length
		If in[c]=34
			inbrackets=1-inbrackets
		EndIf
		If inbrackets
			beeninbrackets=1
		Else
			If in[c]=32
				bit$=in[0..c]
				If beeninbrackets
					bit=bit[1..Len(bit)-1]
				EndIf
				For bc=0 To Len(bit)
					If bit[bc]=39 Then bit=bit[..bc]+Chr(34)+bit[bc+1..]
				Next
					
				Print bit
				out.addlast bit
				in=in[c+1..]
				Print in
				c=-1
				beeninbrackets=0
			EndIf
		EndIf
		c:+1
	Wend
	If in[0]=34
		Print "end quotes"
		Print in
		in=in[1..Len(in)-1]
		Print in
	EndIf
	If in.length Then out.addlast in
	Return out
End Function

Function biggest(a,b)
	If a>b Return a Else Return b
End Function

Function nicestring$(n#)
	If Int(n)=n Return String(Int(n))
	c#=1
	t=1
	While n*c<>Int(n*c) And t<9
		c:*10
		t:+1
	Wend
	s$=String(Abs(Int(n*c)))
	lbit$=s[..Len(s)-t+1]
	rbit$=s[Len(s)-t+1..]
	s=lbit+"."+rbit
	If n<0 Then s="-"+s
	Return s
End Function

Function getlines:tlist(f:tstream)
	lines:tlist=New tlist
	While Not Eof(f)
		line:tlist=splitstring(ReadLine(f))
		If line.count()	lines.addlast line
	Wend
	Return lines
End Function

Function drawniceline(x1#,y1#,x2#,y2#)
	dx#=x2-x1
	dy#=y2-y1
	GetScale scalex#,scaley#
	SetScale 1,1
	SetRotation 0
	DrawLine x1,y1,x1+dx,y1+dy
	SetRotation rotation
	SetScale scalex,scaley
End Function

Function drawnicerect(x#,y#,w#,h#)
	SetRotation 0
	GetScale sx#,sy#
	SetScale 1,1
	DrawRect x,y,w,h
	SetRotation rotation
	SetScale sx,sy
End Function

Function translate#[](x#,y#,r#,s#)
	newx#=x*Cos(r)-y*Sin(r)
	newy#=y*Cos(r)+x*Sin(r)
	Local out#[]=[newx*s,newy*s]
	Return out
End Function


Graphics 800,800,0
SetBlend ALPHABLEND
SetImageFont(LoadImageFont("c:\windows\fonts\Verdana.ttf",30,SMOOTHFONT))
Incbin "lbracket.png"
Incbin "rbracket.png"
Global lbracket:Timage=LoadImage("incbin::lbracket.png")
Global rbracket:Timage=LoadImage("incbin::rbracket.png")


Local letters$[]=["0","1","2","3","4","5","6","7","8","9","+","-","*","=","(",")","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]
Global lettermap:tmap=New tmap
For let$=EachIn letters
	name$=let
	Select name
	Case "+"
		name="plus"
	Case "-"
		name="minus"
	Case "/"
		name="divide"
	Case "*"
		name="multiply"
	Case "="
		name="equals"
	Case "("
		name="lbracket"
	Case ")"
		name="rbracket"
	End Select
	Print "letters\"+let+".png"
	p:tpixmap=LoadPixmapPNG("letters\"+name+".png")
	images:tlist=New tlist
	For i=0 To 29
		img:timage=LoadImage(PixmapWindow(p,i*50,0,50,50))
		images.addlast img
	Next
	lettermap.insert(let,images)
Next

Global drawletters:tlist=New tlist
Global atomtrees:tlist=New tlist

equation:tlist=splitstring("3 2 + 4 - 5 /")

f:tstream=OpenFile("instructions.txt")
lines:tlist=getlines(f)
CloseFile f
equation:tlist=tlist(lines.removefirst())
mainequation:atomtree=atomtree.create(0,400,400,0,1,equation)
'mainequation.vr=1
state=0

t=0
time=MilliSecs()
While Not KeyHit(KEY_ESCAPE)
	mhit1=MouseHit(1)

	SetScale 1,1
	SetRotation 0
	SetAlpha 1
	DrawText "Click the mouse!",0,0
	t:+1
	
	If drawletters.count()
		l:letter=letter(drawletters.first())
		speed#=drawletters.count()/10.0
		If speed<2 speed=2
		l.frame:+speed
		If l.frame>=29 drawletters.removefirst()
		state=1
	Else
		state=0
	EndIf
	
	w#=mainequation.root.width()
	h#=mainequation.root.height()
	If w>h s#=w Else s#=h
	If s<100 s=100
	mainequation.s=700.0/s
	
	Select state
	Case 0
		If mhit1
			If lines.count()
				line:tlist=tlist(lines.removefirst())
				While line.count()
					instructions$=String(line.removefirst())
					newequation$=String(line.removefirst())
					Print instructions
					If instructions="."
						atomtree.explode(mainequation.root,mainequation)
						atomtrees.remove mainequation
						mainequation=atomtree.create(0,400,400,0,1,splitstring(newequation))
					Else
						mainequation.replaceatom(instructions,newequation)
					EndIf
				Wend
			EndIf
		EndIf
	End Select
	
	SetColor 100,100,100
	DrawniceLine -400,0,400,0
	drawniceline 0,-400,0,400
	
	For at:atomtree=EachIn atomtrees
		at.update()
		at.draw()
	Next
	
	otime=time
	time=MilliSecs()
	If time>otime DrawText 1000/(time-otime),0,60
			
	Flip
	Cls
Wend