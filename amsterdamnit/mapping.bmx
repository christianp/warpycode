Type bigimage
	Field slices:slice[,]
	Field xslices,yslices
	Field x,y
	Field width,height
	Field thumb:timage
	Field tnratio#
	
	Function Load:bigimage(name$)
		xslices=0
		While FileType(name+"_0_"+String(xslices)+".png")
			xslices:+1
		Wend
		yslices=0
		While FileType(name+"_"+String(yslices)+"_0.png")
			yslices:+1
		Wend
		
		width=0
		height=0
		b:bigimage=New bigimage
		b.thumb=LoadImage(name+"_small.png")
		b.slices=New slice[xslices,yslices]
		For x=0 To xslices-1
			height=0
			For y=0 To yslices-1
				b.slices[x,y]=slice.Load(name+"_"+String(y)+"_"+String(x)+".png",width,height)
				height:+b.slices[x,y].height
			Next
			width:+b.slices[x,0].width
		Next
		
		b.width=width
		b.height=height
		b.tnratio=Float(ImageWidth(b.thumb))/b.width
		
		
		b.xslices=xslices
		b.yslices=yslices
		For x=0 To xslices-1
		For y=0 To yslices-1
			b.slices[x,y].x:-width/2-b.slices[x,0].width/2
			b.slices[x,y].y:-height/2-b.slices[x,0].height/2
		Next
		Next
		
		Return b
	End Function
	
	Method draw()
		SetColor 255,255,255
		If zoom<tnratio
			DrawzoomImage thumb,x,y,width'*zoom
		Else
			For sx=0 To xslices-1
			For sy=0 To yslices-1
				slices[sx,sy].draw x,y
			Next
			Next
		EndIf
	End Method
End Type

Type slice
	Field x#,y#,height,width
	Field image:timage
	
	Function Load:slice(fname$,x,y)
		s:slice=New slice
		s.image=LoadImage(fname)
		s.width=ImageWidth(s.image)
		s.height=ImageHeight(s.image)
		s.x=x
		s.y=y
		Return s
	End Function
	
	Method draw(offx,offy)
		DrawzoomImage image,x+offx,y+offy
	End Method
End Type


Type tool
	Field state
	Field name$
	Field getskeys
	
	Method New()
		FlushMouse
		FlushKeys
	End Method

	Method update(x#,y#)
	End Method

	Method draw()
	End Method
End Type



Type labeller Extends tool
	Field l:label
	Field nextdelete
	Method New()
		name="Labeller"
		l=New label
	End Method
	
	Method update(x#,y#)
		drawzoomrect x,y,10,10
		ms=MilliSecs()
		Select state
		Case 0
			l.x=x
			l.y=y
			getskeys=0
			If MouseHit(1)
				state=1
				l.txt=""
				FlushKeys
			EndIf
		Case 1
			getskeys=1
			If KeyDown(KEY_BACKSPACE)
				If ms>nextdelete
					l.txt=l.txt[..Len(txt)-1]
					nextdelete=ms+100
				EndIf
			EndIf
			cr=GetChar()
			If cr>31
				l.txt:+Chr(cr)
			EndIf
			If KeyHit(KEY_ENTER)
				finish
			EndIf
		End Select
	End Method
	
	Method finish()
		labels.addlast l
		curtool=New labeller
	End Method
	
	Method draw()
		Select state
		Case 1
			l.draw1
			l.draw2 0
		End Select
	End Method
End Type

Type label
	Field x#,y#
	Field txt$
	Field tb:textblock
	
	Function Create:label(txt$,x#,y#)
		la:label=New label
		la.x=x
		la.y=y
		la.txt=txt
		la.calctb
		Return la
	End Function
	
	Function find:label(name$)
		For l:label=EachIn labels
			If l.txt=name Return l
		Next
	End Function
	
	Function findbypos:label(x#,y#)
		For l:label=EachIn labels
			If l.x=x And l.y=y Return l
		Next
	End Function
	
	Method jsonise:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair("x",jsonnumbervalue.Create(x))
		j.addnewpair("y",jsonnumbervalue.Create(y))
		j.addnewpair("name",jsonstringvalue.Create(txt))
		Return j
	End Method
	
	Method draw1()
		SetColor 205,157,50
		drawzoomcircle x,y,10
	End Method
	Method draw2(t#)
		SetColor 0,0,0
		SetImageFont mapfont
		DrawzoomText txt,x-TextWidth(txt)*.2,y-TextHeight(txt)*.3,.4
		
		If tb
			If onscreen(x-22,y+10,x+20,y+15+tb.y)
			If zoom>2
				alpha#=(zoom-2)
				If alpha>1 Then alpha=1
				SetAlpha alpha
				SetColor 248,236,194
				drawzoomrect x-22,y+10,42,tb.y+5
				SetColor 0,0,0
				For tl:typeline=EachIn tb.typelines
					tl.drawzoom x-20,y+12
				Next
				SetAlpha 1
			EndIf
			EndIf
		EndIf
	End Method
	
	Method calctb()
		If txt="" Return
		p:port=port.find(txt)
		If Not p Return
		ml:TList=New TList
		For m:merchant=EachIn p.residents
			ml.addlast m.name
		Next
		inserts:tmap=New tmap
		Select ml.count()
		Case 0
			restxt$="Nobody is trading here."
		Case 1
			restxt$=String(ml.first())+" trades here."
		Default
			restxt$=prettylist(ml)+" trade here."
		End Select
		london:port=port.find("London")
		If p<>london
			p.findroute(london)
			distancetxt$=p.name+" is "+String(Int(london.distance))+" "+pluralise("day's",london.distance,"days'")+" travel from London.~n~n"
		Else
			distancetxt$=""
		EndIf
		inserts.insert "residents",restxt
		inserts.insert "placename",txt
		inserts.insert "news",p.getnews()
		inserts.insert "distance",distancetxt
		tbtxt$=doinserts(gettemplate("mapsummary"),inserts)
		Print tbtxt
		tb:textblock=textblock.Create(tbtxt,36,.04)
		tb.render
		Print tb.typelines.count()
	End Method
End Type

Type drawpath Extends tool
	Field p:path
	
	Method New()
		name$="Draw a path"
		p=New path
	End Method
	
	Method update(x#,y#)
		If MouseHit(1)
			addpoint(x,y)
		EndIf
		If KeyHit(KEY_BACKSPACE)
			removepoint
		EndIf
		If KeyHit(KEY_ENTER)
			finish
		EndIf
	End Method
	
	Method addpoint(x#,y#)
		mindist#=-1
		cl:label=Null
		For l:label=EachIn labels
			dx#=l.x-x
			dy#=l.y-y
			d#=Sqr(dx*dx+dy*Dy)
			If d<10 And (d<mindist Or cl=Null)
				cl=l
				mindist=d
			EndIf
		Next
		If cl
			x=cl.x
			y=cl.y
		EndIf
		If p.points.count()=0 
			If cl=Null 
				Return
			Else
				p.l1=cl
			EndIf
		EndIf
		p.addpoint x,y
		If cl And p.points.count()>1
			p.l2=cl
			finish
		EndIf
	End Method
	
	Method removepoint()
		p.removepoint
	End Method

	Method finish()
		paths.addlast p
		curtool=New drawpath
	End Method
	
	Method draw()
		p.draw
	End Method
			
End Type

Type pathenumerator
	Field pl:TLink
	Field p0:Float[2],p1:Float[2],p2:Float[2],p3:Float[2]
	Field t#
	Field c
	Field points:TList
	
	Function Create:pathenumerator(points:TList)
		pe:pathenumerator=New pathenumerator
		pe.points=points
		pe.init
		Return pe
	End Function
	
	Method init()
		pl = points.firstlink()
		p0 = Float[](pl.value())
		pl = pl.nextlink()
		p1 = Float[](pl.value())
		pl = pl.nextlink()
		p2 = Float[](pl.value())
		pl = pl.nextlink()
		p3 = Float[](pl.value())
		t#=0
		c=0
	End Method
	
	Method hasnext()
		If points.count()<4 Return 0
		If t<1 Or pl.nextlink() Return 1 Else Return 0
	End Method
	
	Method nextobject:Object()
		Local point#[2]
		If t<=1
			x# = .5 * ( (2 * p1[0]) + (p2[0] - p0[0]) * t + (2 * p0[0] - 5 * p1[0] + 4 * p2[0] - p3[0]) * t * t + (3 * p1[0] - p0[0] - 3 * p2[0] + p3[0]) * t * t * t)
			y# = .5 * ( (2 * p1[1]) + (p2[1] - p0[1]) * t + (2 * p0[1] - 5 * p1[1] + 4 * p2[1] - p3[1]) * t * t + (3 * p1[1] - p0[1] - 3 * p2[1] + p3[1]) * t * t * t)
			If t<1
				t:+.1
				If t>1 t=1
			Else
				t=2
			EndIf
			point=[x,y]
			Return point
		Else
			p0 = p1
			p1 = p2
			p2 = p3
			pl=pl.nextlink()
			p3=Float[](pl.value())
			t=.1
			Return nextobject()
		EndIf	
	End Method
	
End Type

Type path
	Field l1:label
	Field l2:label
	Field points:TList
	Field minx#,maxx#,miny#,maxy#
	
	Method New()
		points=New TList
	End Method
	
	Method jsonise:jsonvalue()
		j:jsonobject=New jsonobject
		j.addnewpair("start",jsonstringvalue.Create(l1.txt))
		j.addnewpair("end",jsonstringvalue.Create(l2.txt))
		Local point#[2]
		pj:jsonarray=New jsonarray
		j.addnewpair("points",pj)
		For point=EachIn points
			pj.values.addlast jsonnumbervalue.Create(point[0])
			pj.values.addlast jsonnumbervalue.Create(point[1])
		Next
		Return j
	End Method
	
	Method addpoint(x#,y#)
		Local point#[2]
		point[0]=x
		point[1]=y
		If points.count()=0
			minx=x
			miny=y
			maxx=x
			maxy=y
		Else
			If x<minx minx=x
			If x>maxx maxx=x
			If y<miny miny=y
			If y>maxy maxy=y
		EndIf
		points.addlast point
	End Method
	
	Method removepoint()
		If points.count()
			points.removelast
		EndIf
	End Method
	
	Method objectenumerator:pathenumerator()
		dpoints:TList=points.copy()
		If dpoints.count() 
			dpoints.addlast dpoints.last()
			dpoints.addfirst dpoints.first()
		EndIf
		Return pathenumerator.Create(dpoints)
	End Method
	
	Method draw()
		If Not onscreen(minx,miny,maxx,maxy) Return
		dpoints:TList=points.copy()
		If dpoints.count() 
			dpoints.addlast dpoints.last()
			dpoints.addfirst dpoints.first()
		EndIf
		Local p0#[2],p1#[2],p2#[2],p3#[2]
		
		num=dpoints.count()
		If num<4 Then Return 'Check there are enough points to draw a spline

		Local point#[]
		go=0
		Local ox1#,oy1#,ox2#,oy2#,ox#,oy#
		Local poly#[4]
		SetColor 255,0,0
		For point=EachIn Self
			x#=point[0]
			y#=point[1]
			drawzoomrect x,y,1,1		
			dx#=x-ox
			dy#=y-oy
			an#=ATan2(dy,dx)+90
			x1#=zoomx(x+Cos(an)*4)
			y1#=zoomy(y+Sin(an)*4)
			x2#=zoomx(x-Cos(an)*4)
			y2#=zoomy(y-Sin(an)*4)
			If go
				poly=[ox1,oy1,ox2,oy2,x2,y2,x1,y1]
				DrawPoly poly
			EndIf
			ox = x
			oy = y
			ox1=x1
			oy1=y1
			ox2=x2
			oy2=y2
			go=1
		Next
	End Method
End Type

Function save()
	j:jsonobject=New jsonobject
	lj:jsonarray=New jsonarray
	j.addnewpair("ports",lj)
	For l:label=EachIn labels
		lj.values.addlast l.jsonise()
	Next
	pj:jsonarray=New jsonarray
	j.addnewpair("paths",pj)
	For p:path=EachIn paths
		pj.values.addlast p.jsonise()
	Next

	f:TStream=WriteFile("map.txt")
	f.WriteLine j.repr()
	CloseFile f
	
	Print j.repr()
End Function

Function Loadmap()
		labels=New TList
		paths=New TList

	f:TStream=ReadFile("map.txt")
	txt$=""
	While Not Eof(f)
		txt:+f.ReadLine()
	Wend
	CloseFile f
	
	jd:jsondecoder=jsondecoder.Create(txt)
	jd.parse()
	
	j:jsonobject=jsonobject(jd.things.first())

	lj:jsonarray=j.getarrayvalue("ports")
	n=0
	For po:jsonobject=EachIn lj.values
		l:label=New label
		l.x=po.getnumbervalue("x")
		l.y=po.getnumbervalue("y")
		l.txt=po.getstringvalue("name")
		If Not l.txt
			'l.txt=String(n)
			n:+1
		EndIf
		labels.addlast l
	Next
	
	pj:jsonarray=j.getarrayvalue("paths")
	Local point#[2]
	For po:jsonobject=EachIn pj.values
		p:path=New path
		name1$=po.getstringvalue("start")
		name2$=po.getstringvalue("end")
		pj2:jsonarray=po.getarrayvalue("points")
		While pj2.values.count()
			x#=jsonnumbervalue(pj2.values.removefirst()).number
			y#=jsonnumbervalue(pj2.values.removefirst()).number
			p.addpoint x,y
		Wend
		point=Float[](p.points.first())
		p.l1=label.findbypos(point[0],point[1])
		point=Float[](p.points.last())
		p.l2=label.findbypos(point[0],point[1])
		paths.addlast p
	Next
	
End Function

Function analyse()
	f:TStream=WriteFile("world3.txt")
	distances:tmap=New tmap
	seas:TList=New TList
	towns:TList=New TList
	n=0
	f.WriteLine "ports"
	For l:label=EachIn labels
		distances.insert l,New tmap
		If Int(l.txt)>0 Or l.txt="0" Or l.txt=""
			seas.addlast l
			'l.txt=String(n)
			n:+1
		Else
			towns.addlast l
			f.WriteLine l.txt+" .1 1000"
		EndIf
	Next
	f.WriteLine ""
	
	Local point#[2]
	For p:path=EachIn paths
		go=0
		Local ox#,oy#
		td#=0
		For point=EachIn p
			x#=point[0]
			y#=point[1]
			If go
				dx#=x-ox
				dy#=y-oy
				d#=Sqr(dx*dx+dy*dy)
				td:+d
			EndIf
			ox=x
			oy=y
			go=1
		Next
		dict:tmap=tmap(distances.valueforkey(p.l1))
		dict.insert p.l2,String(td)
		dict:tmap=tmap(distances.valueforkey(p.l2))
		dict.insert p.l1,String(td)
		Print "path "+p.l1.txt+" to "+p.l2.txt
	Next
	While seas.count()
		l:label=label(seas.removefirst())
		Print l.txt
			dict:tmap=tmap(distances.valueforkey(l))
			lo:TList=New TList
			For l1:label=EachIn dict.keys()
				lo.addlast l1
				dict1:tmap=tmap(distances.valueforkey(l1))
				dict1.remove l
				Print l1.txt+" remove "+l.txt
			Next
			While lo.count()
				l1:label=label(lo.removefirst())
				d1#=Float(String(dict.valueforkey(l1)))
				For l2:label=EachIn lo
					d2#=Float(String(dict.valueforkey(l2)))
					td#=d1+d2
					dict1:tmap=tmap(distances.valueforkey(l1))
					dict1.insert l2,String(td)
					dict2:tmap=tmap(distances.valueforkey(l2))
					dict2.insert l1,String(td)
				Next
			Wend
	Wend
	f.WriteLine "distances"
	For l:label=EachIn towns
			Print l.txt
			dict:tmap=tmap(distances.valueforkey(l))
			For l2:label=EachIn dict.keys()
				td#=Float(String(dict.valueforkey(l2)))
				days=Int(td/100)+1
				Print "   "+l2.txt+" => "+String(td)+"   "+String(days)
				f.WriteLine l.txt+" "+l2.txt+" "+String(days)
			Next
	Next
	CloseFile f
End Function

Type mapmode Extends gamemode
	Field ox,oy,opanx#,opany#
	Field mz,tz#
	Field showt#
	Field oldms
	Field tx#,ty#,lockedcam
	
	Method New()
		gwidth=800
		gheight=800
		'AppTitle="mapping"
		initgfx
		mapfont=LoadImageFont("fonts/AquilineTwo.ttf",60)
		AutoMidHandle True

		Print "INIT"
		SetClsColor 198,190,174
		
		mapimg:bigimage=bigimage.Load("maps/slice")
		
		panx=0
		pany=0
		zoom=0
		tzoom=0
	
		curtool:tool=Null

		ox=0
		oy=0
		opanx#=0
		opany#=0
		mz=MouseZ()
		tz=0
		oldms=MilliSecs()
	End Method
	
	Method arrive()
		If Not mapfont Print "???????"
		SetImageFont mapfont
		For l:label=EachIn labels
			l.calctb
		Next
		lockedcam=0
	End Method
		
	Method update()
		omz=mz
		mz=MouseZ()
		zoom=1.1^(tz)
	
		mx=MouseX()
		my=MouseY()
		
		If lockedcam
			panx:+(tx-panx)*.1
			pany:+(ty-pany)*.1
			tz:+Sgn(tzoom-tz)*.5
			dx#=panx-tx
			dy#=pany-ty
			d#=dx*dx+dy*dy
			If d<5 And tzoom=tz lockedcam=0
		Else
			'tz:+mz-omz
			scrollspeed#=200.0/zoom
			If mx>gwidth*.9
				panx:+(Float(mx)/gwidth-.9)*scrollspeed
			EndIf
			If mx<gwidth*.1
				panx:-(.1-Float(mx)/gwidth)*scrollspeed
			EndIf
			If my>gheight*.9
				pany:+(Float(my)/gheight-.9)*scrollspeed
			EndIf
			If my<gheight*.1
				pany:-(.1-Float(my)/gheight)*scrollspeed
			EndIf
		EndIf
		
		If MouseHit(1) And tz=0
			tx=unzoomx(mx)
			ty=unzoomy(my)
			For l:label=EachIn labels
				dx#=l.x-tx
				dy#=l.y-ty
				d#=dx*dx+dy*dy
				If d<200 And l.txt
					lockedcam=1
					tx=l.x
					ty=l.y+25
				EndIf
			Next
			tzoom=30
		EndIf
		
		If MouseHit(2) And tz>0
			lockedcam=1
			tzoom=0
			tx=panx
			ty=pany
		EndIf
		
		If MouseDown(2) And tz=0
			panx=opanx+(ox-mx)/zoom
			pany=opany+(oy-my)/zoom
		Else
			opanx=panx
			opany=pany
			ox=mx
			oy=my
		EndIf
		
		If curtool
			tx#=unzoomx(mx)
			ty#=unzoomy(my)
			curtool.update tx,ty
			If curtool.state=-1
				curtool=Null
			EndIf
		EndIf
		
		If curtool=Null Or curtool.getskeys=0
			cr$=Chr(GetChar())
			Select cr
			Case "l"
				curtool=New labeller
			Case "p"
				curtool=New drawpath
			Case "s"
				save
			Case "r"
				Loadmap
			Case "a"
				analyse
			End Select
		EndIf
		
		If zoom>1.5
			tshowt#=zoom-1.5
			If tshowt>1 Then tshowt=1
		Else
			tshowt#=0
		EndIf
		showt:+(tshowt-showt)*.3
		If showt>1 showt=1
		
		mapimg.draw
	
		For p:path=EachIn paths
			p.draw
		Next
		
		For l:label=EachIn labels
			l.draw1
		Next
		For l:label=EachIn labels
			l.draw2 showt
		Next
		
		If curtool
			curtool.draw
		EndIf
		
		SetScale 1,1
		ms=MilliSecs()
		'DrawText 1000.0/(ms-oldms),0,0
		oldms=ms

		Flip
		Cls
		
		If KeyHit(KEY_ESCAPE)
			leavemode
		EndIf
	End Method
End Type

Rem
Include "jsondecoder.bmx"

Global labels:TList
Global paths:TList
Global curtool:tool
Global mapimg:bigimage

curmode:gamemode=New mapmode
While Not KeyHit(KEY_ESCAPE)
	curmode.update
Wend