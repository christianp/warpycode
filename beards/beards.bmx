Const slip!=1.5

Type bit
	Field x!,y!,angle!
	
	Method moveto(nx!,ny!)
		If nx<gwidth-border And ny<gheight-border And nx>border And ny>border
			If counts[nx/segsize,ny/segsize]<10000
				counts[x/segsize,y/segsize]:-1
				counts[nx/segsize,ny/segsize]:+1
				angle=ATan2(ny-y,nx-x)
				x=nx
				y=ny
			EndIf
		EndIf
	End Method
	
	
	
	Method pull(brush,mx,my)
		If Rand(10)>2 Return
		dx!=mx-x
		dy!=my-y
		d!=dx*dx+dy*dy
		If d<brush*brush
				brush:*Rnd(0.5)
				an=Rand(360)
				nx!=mx+brush*Cos(an)
				ny!=my+brush*Sin(an)
				nx=x+(nx-x)*.3
				ny=y+(ny-y)*.3
				moveto(nx,ny)
		EndIf
	End Method		
		
	Method repel(f!,brush,mx!,my!,mdx!,mdy!,moved:TList,dir=1)
		'mdx:*Rnd(.3,1)
		'mdy:*Rnd(.3,1)
		dx!=mx-x
		dy!=my-y
		d!=dx*dx+dy*dy
		dp!=dx*mdx+dy*mdy
		If dp*dir<0 And d<brush*brush
			angle=ran
			'md!=Sqr(mdx*mdx+mdy*mdy)*Rnd(.5,1)
			'mdx:/md
			'mdy:/md
			moveto x+mdx*f,y+mdy*f
			moved.addlast Self
		EndIf
	End Method
	
	Method draw()
		c!=Cos(angle)*4
		s!=Sin(angle)*4
		DrawLine x-c,y-s,x+c,y+s
	End Method
End Type

Function andiff!(an1!,an2!)
	d!=(an1-an2) Mod 360
	If d<-180 Then d:+360
	If d>180 Then d:-360
	Return d
	
End Function

Function avcounts(x!,y!)
	cx=x/segsize
	cy=y/segsize
	If cx<0 cx=0
	If cy<0 cy=0
	If cx>xsegs cx=xsegs
	If cy>ysegs cy=ysegs
	px!=cx+segsize*.5
	py!=cy+segsize*.5
	dx!=x-px
	dy!=y-py
	t!=0
	If dx<0
		If cx>0
			sh!=dx/segsize
			t:+counts[cx-1,cy]*(1-sh)+counts[cx,cy]*sh
		Else
			t:+counts[cx,cy]
		EndIf
	Else
		If cx<xsegs
			sh!=dx/segsize
			t:+counts[cx+1,cy]*(1-sh)+counts[cx,cy]*sh
		Else
			t:+counts[cx,cy]
		EndIf
	EndIf
	If dy<0
		If cy>0
			sh!=dx/segsize
			t:+counts[cx,cy-1]*(1-sh)+counts[cx,cy]*sh
		Else
			t:+counts[cx,cy]
		EndIf
	Else
		If cy<ysegs
			sh!=dx/segsize
			t:+counts[cx,cy+1]*(1-sh)+counts[cx,cy]*sh
		Else
			t:+counts[cx,cy]
		EndIf
	EndIf
	t:/2
	Return t
End Function


'SetGraphicsDriver GLMax2DDriver()
Global gwidth=700
Global gheight=850
AppTitle="Beardy Weirdy"
Graphics gwidth,gheight,0
SetClsColor 248,236,194
SetBlend ALPHABLEND

Global segsize=20
Global xsegs=gwidth/segsize
Global ysegs=gheight/segsize
Global border=50
Global counts[xsegs+1,ysegs+1]
Global template[xsegs+1,ysegs+1]

face:timage=LoadImage("face.png")
Global templates:TList=loadtemplates()


Global bits:TList=New TList
For t=1 To 10000
	b:bit=New bit
	b.x=Rand(border,gwidth-border)
	b.y=Rand(gheight/2,gheight-border)
	b.angle=Rand(360)
	bits.addlast b
	counts[b.x/segsize,b.y/segsize]:+1
Next

drawing=0
bsize!=10
ox=MouseX()
oy=MouseY()
Global ran!
mode=0
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())
	SetColor 255,255,255
	SetScale 2,2
	DrawImage face,100,0
	SetScale 1,1
	
	mx=MouseX()
	my=MouseY()
	Select mode
	Case 0
		dx=mx-ox
		dy=my-oy
		d=dx*dx+dy*dy
		If d>25
			ox:+(mx-ox)*.5
			oy:+(my-oy)*.5
			ran:+andiff(ATan2(my-oy,mx-ox),ran)*.6
		EndIf
			SetColor 0,0,0
			SetRotation ran+90
			DrawRect ox+Sin(ran)*bsize/2,oy-Cos(ran)*bsize/2,bsize,10
			SetRotation 0
			
		If MouseDown(2) Or MouseDown(1)
			bsize:+1
			If bsize>50 Then bsize=50
		Else
			bsize:-3
			If bsize<30 Then bsize=30
		EndIf
		
		moved:TList=New TList
		If MouseDown(1)
			For b:bit=EachIn bits
				b.pull bsize,mx,my
			Next
		EndIf	
		If MouseDown(2)
			f!=1-bsize*bsize/20000.0
			For b:bit=EachIn bits
				b.repel f,bsize,mx,my,mx-ox,my-oy,moved
			Next
		EndIf

		SetLineWidth 3
		SetColor 0,0,0
		For b:bit=EachIn bits
			b.draw
		Next
		
		score
	Case 1
		If MouseDown(1)
			DrawRect mx,my,5,5
			template[mx/segsize,my/segsize]=1
		EndIf
		If MouseDown(2)
			template[mx/segsize,my/segsize]=0
		EndIf
		
		If KeyHit(KEY_S)
			savetemplate()
		EndIf

		For x=0 To xsegs-1
		For y=0 To ysegs-1
			If drawing
			a!=scoreseg(x,y)
			If a>.5
				SetAlpha a*a
				SetColor 255,0,0
				DrawRect x*segsize,y*segsize,segsize,segsize
			EndIf
			EndIf

			If mode=1
				If template[x,y]
					SetAlpha .9
					SetColor 0,0,0
					DrawRect x*segsize,y*segsize,segsize,segsize
				EndIf
			EndIf
			
		Next
		Next
		SetAlpha 1
	End Select

	If KeyHit(KEY_SPACE)
		drawing=1-drawing
	EndIf
	
	If KeyHit(KEY_ENTER)
		mode=1-mode
	EndIf
	
	DrawText templates.count(),0,15
	
	Flip
	Cls
Wend

Function scoreseg!(x,y)
	Return Sqr(Float(counts[x,y])/50.0)
End Function

Function savetemplate()
	n=1
	fname$="beard"+String(n)+".txt"
	While FileType(fname)
		n:+1
		fname="beard"+String(n)+".txt"
	Wend
	
	f:TStream=WriteFile(fname)
	For x=0 To xsegs-1
	For y=0 To ysegs-1
		WriteInt f,template[x,y]
	Next
	Next
	
	CloseFile f
	
	templates.addlast scoretemplate.Create("beard"+String(n),template)
	template=New Int[xsegs+1,ysegs+1]
End Function

Function loadtemplates:TList()
	l:TList=New TList
	Local a[,]
	
	dir=ReadDir("./")
	nf$=NextFile(dir)
	While nf
		If Len(nf)>4 
			If nf[Len(nf)-4..]=".txt"
				name$=nf[..Len(nf)-4]
				f:TStream=ReadFile(nf)
				a=New Int[xsegs+1,ysegs+1]
				For x=0 To xsegs-1
				For y=0 To ysegs-1
					a[x,y]=ReadInt(f)
				Next
				Next
				l.addlast scoretemplate.Create(name,a)
				CloseFile f
			EndIf
		EndIf
		nf=NextFile(dir)
	Wend
	Return l
End Function

Type scoretemplate
	Field a[,]
	Field name$
	Field n!

	Function Create:scoretemplate(name$,a[,])
		st:scoretemplate=New scoretemplate
		st.name=name
		st.a=a
		Return st
	End Function
	
	Method doscore!(b[,])
		tscore1!=0
		tscore2!=0
		n!=0
		n2!=0
		For x=0 To xsegs-1
		For y=0 To ysegs-1
			If a[x,y]=b[x,y] And a[x,y]
				tscore1:+1
			EndIf 
			If a[x,y]<>b[x,y]
				tscore2:+1
			EndIf
			If a[x,y]
				n1:+1
			Else
				n2:+1
			EndIf
		Next
		Next
		tscore1:/n1
		tscore2=1-tscore2/n2
		Return tscore1
		Return (tscore1+tscore2)/2
	End Method
	
	Method draw()
		SetColor 0,0,0
		For x=0 To xsegs-1
		For y=0 To ysegs-1
			If a[x,y]
				DrawRect x*segsize,y*segsize,segsize,segsize
			EndIf
		Next
		Next
		SetAlpha 1
	End Method
End Type

Function score()
	Local a[xsegs+1,ysegs+1]
	For x=0 To xsegs-1
	For y=0 To xsegs-1
		If scoreseg(x,y)>.3
			a[x,y]=1
		EndIf
	Next
	Next
	minscore!=-1
	maxt:scoretemplate=Null
	y=0
	'SetColor 255,255,255
	'DrawRect 300,0,300,templates.count()*15
	SetColor 0,0,0
	For st:scoretemplate=EachIn templates
		tscore#=st.doscore(a)
		'DrawText tscore,400,y
		'DrawText st.name,300,y
		'DrawText minscore,500,y
		y:+15
		If tscore>.5
			SetAlpha 1
			If tscore*st.n>minscore Or maxt=Null
				'DrawLine 300,y,500,y
				maxt=st
				minscore=tscore*st.n
			EndIf
		Else
			SetAlpha tscore
		EndIf
		st.draw
	Next
	SetAlpha 1
	If maxt
		SetScale 5,5
		SetColor 255,0,0
		txt$=maxt.name+"!"
		DrawText txt,gwidth/2-TextWidth(txt)*2.5,0
		SetScale 1,1
		'maxt.draw
	EndIf
End Function