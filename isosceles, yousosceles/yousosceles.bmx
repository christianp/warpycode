Const sidelength#=100
Const fadetime=20
Global rowheight#=sidelength*Sqr(3)/2

Function gettile:Int[](x#,y#)
	Local tile[2]
	row=Int(y/rowheight)
	xoff=-(row Mod 2)*sidelength/2
	yoff#=(y-row*rowheight)/sidelength
	col=Int((x-xoff)/sidelength-yoff)
	If x>col*sidelength+(1-yoff)*sidelength+xoff
		col=col*2+1
	Else
		col=col*2
	EndIf
	tile[0]=col
	tile[1]=row
	Return tile
End Function

Function placetile:Float[](col,row)
	Local pos#[2]
	xoff=-(row Mod 2)*sidelength/2
	pos[0]=(col+1)*sidelength/2+xoff
	pos[1]=row*rowheight
	If col Mod 2
		pos[1]:+sidelength/Sqr(3)
	Else
		pos[1]:+rowheight-sidelength/Sqr(3)
	EndIf
	Return pos
End Function

Function drawtile(col,row)
	Local pos#[2]
	pos=placetile(col,row)
	ox#=-1
	oy#=-1
	n=0
	Local poly#[6]
	For an=0 To 360 Step 120
		pan=an+(col Mod 2)*180-30
		x#=pos[0]+Cos(pan)*sidelength/Sqr(3)
		y#=pos[1]+Sin(pan)*sidelength/Sqr(3)
		If ox>0
			poly[n*2]=x
			poly[n*2+1]=y
			DrawLine x,y,ox,oy
			n:+1
		EndIf
		ox=x
		oy=y
	Next
	DrawPoly poly
End Function


Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

Function drawarc(x#,y#,r#,an1#,an2#)
	ox#=x+Cos(an1)*r
	oy#=y+Sin(an1)*r
	Local poly#[6]
	For an#=an1 To an2 Step 5
		px#=x+Cos(an)*r
		py#=y+Sin(an)*r
		poly=[x,y,ox,oy,px,py]
		DrawPoly poly
		ox=px
		oy=py
	Next
End Function


Global triangles:TList=New TList
Global counted:TList
Type triangle
	Field x#,y#
	Field col,row
	Field state
	Field colours#[3],newcolours#[3],changed#,ocolours#[3]
	Field joins:triangle[3]
	Field angle#
	Field tangle#
	Field turns
	Field edges[3]
	Field fade#
	Field maxcol#
	Field mygroup:opengroup
	
	Method New()
		triangles.addlast Self
		n=Rand(0,2)
		colours[n]=1
		newcolours[n]=1
		ocolours[n]=1
		t=0
		For n=0 To 2
			edges[n]=Rand(0,1)
			t:+edges[n]
		Next
		If t=3
			edges[Rand(0,2)]=0
		EndIf
		fade=1
	End Method
	
	Method draw()
		anmod=angle+turns*120
		n=0
		Local poly#[6]
		SetBlend ALPHABLEND
		If current=Self
			SetAlpha .4*fade
		Else
			SetAlpha .8*fade
		EndIf
		For an=-30 To 210 Step 120
			pan#=an+anmod
			nx#=x+Cos(pan)*sidelength/Sqr(3)
			ny#=y+Sin(pan)*sidelength/Sqr(3)
			poly[n*2]=nx
			poly[n*2+1]=ny
			n:+1
		Next
		lum#=255/maxcol
		SetColor colours[0]*lum,colours[1]*lum,colours[2]*lum
		DrawPoly poly

		SetAlpha 1
		SetLineWidth 4
		SetColor 255,255,255
		For n=0 To 2
			If edges[(n+turns) Mod 3]
				DrawLine poly[n*2],poly[n*2+1],poly[(n*2+2) Mod 6],poly[(n*2+3) Mod 6]
			EndIf
			If joins[n]
				pan#=n*120+angle+30
				DrawLine x,y,x+(joins[n].x-x)/2,y+(joins[n].y-y)/2
				'DrawText n,x,y
			EndIf
		Next
		SetLineWidth 1
		tot#=0
		pan#=0
		If fade=1
			pieradius#=sidelength/(6*(changed+1))
			'DrawText changed,x,y-15
			For n=0 To 2
				'DrawText Int(colours[n]*100/maxcol),x,y+n*15
				tot:+colours[n]
			Next
			For n=0 To 2
				Select n
				Case 0
					SetColor 255,0,0
				Case 1
					SetColor 0,255,0
				Case 2
					SetColor 0,0,255
				End Select
				If colours[n]/maxcol<.9
					SetAlpha 1
				Else
					SetAlpha .2
				EndIf
				drawarc(x,y,pieradius,pan,pan+360*colours[n]/tot)
				pan:+360*colours[n]/tot
			Next
		EndIf
		SetAlpha 1
		'DrawText String(col),x,y
		'If row Mod 2
		'	DrawText "!",x,y+12
		'EndIf
		'DrawText String(edges[0])+","+String(edges[1])+","+String(edges[2]),x,y-12
	End Method
	
	Method changecolours()
		changed#=0
		maxcol#=0
		fadediff#=0
		For n=0 To 2
			If mygroup
				diff#=(mygroup.colours[n]-colours[n])
				If Abs(diff)>.001 changed:+Abs(diff)
				colours[n]:+diff*.1
			EndIf
			If colours[n]>maxcol maxcol=colours[n]
			
			If joins[n]
				If joins[n].fade<fade
					fadediff:+joins[n].fade-fade
				EndIf
			EndIf
		Next
		fade:+fadediff*.2
	End Method
	
	Method checkopen()
		'Print String(col)+","+String(row)
		If counted.contains(Self) Return 0
		counted.addlast Self
		For n=0 To 2
			If joins[n]
				'Print "checking join "+String(n)
				If joins[n].checkopen()
					'Print "this join open"
					Return 1
				Else
					'Print "this join closed"
				EndIf
				'Print "--"
			ElseIf Not edges[(3+n-turns) Mod 3]
				'Print String(n)+" open to space"
				Return 1
			Else
				'Print String(n)+" closed To space"
			EndIf
		Next
		Return 0
	End Method
	
	Method place()
		If tiles[col,row] Return
		turnstaken:+1
		state=1
		Local pos#[2]
		pos=placetile(col,row)
		x=pos[0]
		y=pos[1]
		tiles[col,row]=Self
		current=New triangle
		checkplace()
	End Method
	
	Method checkplace()
		If col Mod 2
			If row Mod 2
				joins[0]=tiles[col-1,row+1]
			Else
				joins[0]=tiles[col+1,row+1]
			EndIf
			joins[1]=tiles[col-1,row]
			joins[2]=tiles[col+1,row]
			For n=0 To 2
				p=(n+5) Mod 3
				If joins[n]
					mine=edges[(3+n-turns) Mod 3]
					theirs=joins[n].edges[(5+n-joins[n].turns) Mod 3]
					If mine Or theirs
						edges[(3+n-turns) Mod 3]=1
						joins[n].edges[(5+n-joins[n].turns) Mod 3]=1
						If Not theirs
							joins[n].checkforclosed()
						EndIf
						joins[n]=Null
					Else
						joins[n].joins[p]=Self
					EndIf
				EndIf
			Next
		Else
			joins[0]=tiles[col+1,row]
			joins[1]=tiles[col-1,row]
			If row Mod 2
				joins[2]=tiles[col-1,row-1]
			Else
				joins[2]=tiles[col+1,row-1]
			EndIf
			For n=0 To 2
				p=(n+1) Mod 3
				If joins[n]
					mine=edges[(3+n-turns) Mod 3]
					theirs=joins[n].edges[(4+n-joins[n].turns) Mod 3]
					If mine Or theirs
						edges[(3+n-turns) Mod 3]=1
						joins[n].edges[(4+n-joins[n].turns) Mod 3]=1
						If Not theirs
							joins[n].checkforclosed()
						EndIf
						joins[n]=Null
					Else
						joins[n].joins[p]=Self
					EndIf
				EndIf
			Next
		EndIf
		For n=0 To 2
			If joins[n]
				If mygroup
					joins[n].setgroup(mygroup)
				ElseIf joins[n].mygroup
					setgroup(joins[n].mygroup)
				EndIf
			EndIf
		Next
		If Not mygroup
			g:opengroup=New opengroup
			setgroup(g)
		EndIf
		
		checkforclosed()
	End Method
	
	Method setgroup(g:opengroup)
		Print "set group"
		If mygroup
			For tri:triangle=EachIn mygroup.mytriangles
				tri.mygroup=g
				g.mytriangles.addlast tri
			Next
		Else
			mygroup=g
			g.mytriangles.addlast Self
		EndIf
	End Method
			
	
	Method checkforclosed()
		counted=New TList
		'Print "CHECKING"
		If checkopen()
			'Print "OPEN"
		Else
			'Print "CLOSED"
			closedgroup.create(Self,counted)
		EndIf
	End Method
			
	Method update()
		angle:-Sgn(andiff(angle,tangle+turns*120))*5
		changecolours()
	End Method
	
	Method die()
		triangles.remove Self
		tiles[col,row]=Null
	EndMethod
End Type

Global opengroups:TList=New TList
Type opengroup
	Field mytriangles:TList
	Field colours#[3]
	
	Method New()
		mytriangles=New TList
		opengroups.addlast Self
	End Method
		
	Method update()
		tot#=0
		For tri:triangle=EachIn mytriangles
			For n=0 To 2
				colours[n]:+tri.ocolours[n]
				tot:+tri.ocolours[n]
			Next
		Next
		maxcol#=0
		For n=0 To 2
			If colours[n]>maxcol maxcol=colours[n]
		Next
		If Not maxcol Return
		For n=0 To 2
			colours[n]:/maxcol
		Next
	End Method
End Type

Global closedgroups:TList=New TList
Type closedgroup
	Field activator:triangle
	Field mytriangles:TList
	Field tick#,maxtick
	
	Method New()
		closedgroups.addlast Self
		maxtick=Rand(fadetime,fadetime*1.5)
	End Method
	
	Function create:closedgroup(activator:triangle,mytriangles:TList)
		cg:closedgroup=New closedgroup
		cg.mytriangles=mytriangles
		cg.activator=activator
	End Function
	
	Method update()
		changed=0
		thisscore=0
		If tick activator.fade=1-tick/maxtick
		tfade#=0
		For tri:triangle=EachIn mytriangles
			If tri.changed changed=1
			For n=0 To 2
				thisscore:+tri.colours[n]/tri.maxcol
			Next
			tfade:+tri.fade
		Next
		If Not changed
			If tick<maxtick tick:+1
			If tfade<.01*mytriangles.count()
				tri=triangle(mytriangles.first())
				Print "colours:"
				For n=0 To 2
					Print tri.colours[n]/tri.maxcol
				Next
				Print "DIE"
				'Print mytriangles.count()
				If thisscore=mytriangles.count() Or thisscore=mytriangles.count()*3
					thisscore:*3
					'Print "BONUS!"
				EndIf
				thisscore:*10
				Print thisscore
				If turnstaken
					thisscore:/turnstaken
					scoreadd:+thisscore
				Else
					scoreadd=(scoreadd+thisscore)*2
				EndIf
				For tri=EachIn mytriangles
					tri.die()
				Next
				closedgroups.remove Self
				turnstaken=0
			EndIf
		EndIf
	EndMethod
End Type


Global tiles:triangle[2*800/sidelength+1,800/rowheight+1]
AppTitle="Isosceles, Yousosceles"
Graphics 800,800,0
SetBlend ALPHABLEND
SeedRnd MilliSecs()

Global current:triangle=New triangle
Global score=0,scoreadd=0,turnstaken=0
While Not KeyHit(KEY_ESCAPE)
	mx=MouseX()
	my=MouseY()
	
	Local tile[2]
	tile=gettile(mx,my)
	'DrawText String(tile[0])+","+String(tile[1]),0,0
	SetBlend LIGHTBLEND
	SetColor 50,50,50
	For col=0 To 800/sidelength
	For row=0 To 800/rowheight
		xoff=-(row Mod 2)*sidelength/2
		x=col*sidelength+xoff
		y=row*rowheight
		DrawLine x,y,x+sidelength,y
		DrawLine x,y,x+sidelength/2,y+rowheight
		DrawLine x+sidelength/2,y+rowheight,x+sidelength,y
	Next
	Next
	SetBlend ALPHABLEND
	
	current.x=mx
	current.y=my
	current.col=tile[0]
	current.row=tile[1]
	current.tangle=(current.col Mod 2)*60
	If MouseHit(2)
		current.turns=(current.turns+1) Mod 3
	EndIf
	If MouseHit(1)
		current.place()
	EndIf
	
	For og:opengroup=EachIn opengroups
		og.update()
	Next
	For tri:triangle=EachIn triangles
		tri.update()
		tri.draw()
	Next
	For cg:closedgroup=EachIn closedgroups
		cg.update()
	Next
	
	If scoreadd
		score:+1
		scoreadd:-1
	EndIf
	
	DrawText score,400,0
	DrawText scoreadd,400,12
	
	Flip
	Cls
Wend