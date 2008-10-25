Global gwidth#,gheight#
Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom
Global joyN=0


'returns True if p1 and p2 are on the same side of the line a->b
Function sameside(p1x#,p1y#,p2x#,p2y#,ax#,ay#,bx#,by#)
	cp1# = (bx-ax)*(p1y-ay)-(p1x-ax)*(by-ay)
	cp2# = (bx-ax)*(p2y-ay)-(p2x-ax)*(by-ay)
	If cp1*cp2 >= 0 Then Return True
End Function	
	
'Clever little trick for telling if a point is inside a given triangle
'If for each pair of points AB in the triangle, P is on the same side of AB as 
'the other point in the triangle, then P is in the triangle. 
Function pointintriangle(px#,py#,ax#,ay#,bx#,by#,cx#,cy#)
	If sameside(px,py,ax,ay,bx,by,cx,cy) And sameside(px,py,bx,by,ax,ay,cx,cy) And sameside(px,py,cx,cy,ax,ay,bx,by)
		Return True
	Else
		Return False
	EndIf
End Function

Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function

Function transformpoly#[](poly#[] , px# , py# , an# , scale# = 1) 
	'px, py: translate
	'an: rotate
	'scale: duhhh thicko
	
	Local opoly#[Len(poly)]
	
	can# = Cos(an)
	san#=Sin(an)
	For n=0 To Len(poly)-1 Step 2
		x#=poly[n]*scale
		y#=poly[n+1]*scale
		opoly[n]=x*Can-y*San+px
		opoly[n+1]=x*San+y*Can+py
	Next
	Return opoly
End Function

Function zoompoly#[](poly#[])
	Local opoly#[Len(poly)]
	For n=0 To Len(poly)-1 Step 2
		opoly[n]=zoomx(poly[n])
		opoly[n+1]=zoomy(poly[n+1])
	Next
	Return opoly
End Function

Function drawrotatedline#(x1# , y1# , x2# , y2# , px# , py# , an# , scale# = 1)
	'px,py: translate
	'an: rotate
	'scale: duhhh thicko
	
	can# = Cos(an)
	san# = Sin(an)
	
	nx1#=(x1*can-y1*san)*scale+px
	ny1#=(x1*san+y1*can)*scale+py
	nx2#=(x2*can-y2*san)*scale+px
	ny2#=(x2*san+y2*can)*scale+py
	DrawLine nx1,ny1,nx2,ny2
End Function

Function drawoutline(poly#[],thickness=1)
	'DrawPoly poly
	SetLineWidth thickness
	l = Len(poly)
	For n=0 To l-3 Step 2
		x1#=poly[n]
		y1#=poly[n+1]
		x2#=poly[(n+2) Mod l]
		y2#=poly[(n+3) Mod l]
		DrawZoomLine x1,y1,x2,y2
	Next
	SetLineWidth 1
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

Function DrawZoomImage(image:TImage , x# , y#,width#,heighto=0)
	If heighto
		w# = width / ImageHeight(image)
	Else
		w# = width / ImageWidth(image)
	EndIf
	SetScale w*zoom , w*zoom
	DrawImage image , zoomx(x) , zoomy(y)
	SetScale 1,1
End Function

Global sounditems:TList=New TList

Type sounditem
	Field chan:TChannel
	Field loops
	Field sound:TSound
	
	Function Create:sounditem(sound:TSound,rate#,pan#,volume#,loops=0)
		si:sounditem=New sounditem
		si.chan=AllocChannel()
		SetChannelRate(si.chan,rate)
		SetChannelPan(si.chan,pan)
		SetChannelVolume si.chan , volume
		si.sound = sound
		si.loops=loops
		PlaySound(si.sound,si.chan)
		sounditems.AddLast si
		Return si
	End Function
	
	Method Delete()
		StopChannel chan
	End Method

	Method update()	
		If Not ChannelPlaying(chan)
			If loops
				PlaySound sound , chan
			Else
				sounditems.Remove sound
			EndIf
		EndIf
	End Method
	
End Type



Type line
	Field x# , y# 'start point
	Field ex# , ey# 'end point
	Field dx# , dy# 'unit vector along line
	Field length# , an# 'length and direction of line segment
	Field nx#,ny# 'unit normal to line (a vector perpendicular to the line, of length 1)
	
	Function Create:line(x1# , y1# , x2# , y2#) 'create a line between (x1,y1) and (x2,y2)
		l:line = New line
		l.x = x1
		l.y = y1
		dx# = x2 - x1
		dy# = y2 - y1
		l.length=Sqr(dx*dx+dy*dy)
		l.dx=dx/l.length
		l.dy=dy/l.length
		l.an=ATan2(dy,dx)
		l.ex = x1+l.dx*l.length
		l.ey = y1 + l.dy * l.length
		l.nx = Cos(l.an - 90)
		l.ny = Sin(l.an - 90)
		
		Return l
	End Function

	
	Method intersect#(l:line,fit=2) ' returns point on line where it intersects given line, or -1 if no intersection (on segment). fit=2 requires intersection point to be on second line segment, fit=1 just requires it to be on second line

		If l.dx = 0
			mu# = (l.x - x) / dx
		Else
			mu# = (l.y - y + (l.dy / l.dx) * (x - l.x) ) / (dy - l.dy * dx / l.dx)
		EndIf
		'DrawRect x + 400 + dx * mu , y + 400 + dy * mu , 5 , 5
		
		If fit = 0
			Return mu
		EndIf
		
		If l.dx=0
			lambda# = (y - l.y + mu * dy) / l.dy
		Else
			lambda# = (x - l.x + mu * dx) / l.dx
		EndIf
		
		'DrawText lambda,x+400+mu*dx,y+400+mu*dy
		If mu >= 0 And mu <= length
			Select fit
			Case 2
				If lambda <= l.length And lambda >= 0
					hit = mu
				Else
					hit = - 1
				EndIf
			Case 1
				If lambda >= 0
					hit = mu
				Else
					hit = - 1
				EndIf
			End Select
		Else
			hit = - 1
		EndIf
		Return hit
	End Method
	
	Method pointintersect#(px# , py#) 'returns point on line where normal to line contains given point
		pdx# = nx*5
		pdy# = ny*5
		l:line = line.Create(px , py , px + pdx , py + pdy)
		'l.draw(400 , 400)
		mu# = intersect(l , 1)
		Return mu
	End Method
	
	Method pointdistance#(px#,py#) 'returns (perpendicular) distance from line to point
		pdx# = nx*5
		pdy# = ny*5
		l:line = line.Create(px , py , px + pdx , py + pdy)
		'l.draw(400 , 400)
		mu# = l.intersect(Self , 0)
		Return mu
	End Method	
End Type




'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'***********************************************************************************************'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Type santa
	Field x# , y#
	Field ox#,oy#
	Field vx# , vy#
	Field joy,joycx#,joycy#
	Field state
	Field ho:house
	Field size#
	Field whoosh:sounditem
	Field nohits
	Field lasty#
	Field wobble#,vwobble#
	Field mypresents:TList
	Field presenttime
	Field cash,tcash,cashed
	Field throwan#
	Field collision#
	Field calmmusic:sounditem
	Field activemusic:sounditem
	
	Method New()
		santas.addlast Self
		state = -1
		ho = Null
		size = 1
		joy=-1
		whoosh=sounditem.Create(windsound,1,1,0,1)
		mypresents=New TList
		cash=0
		cashed=0
		activemusic=sounditem.Create(musics[0],1,1,0,1)
		calmmusic=sounditem.Create(musics[1],1,1,0,1)
	End Method
	
	Method addpresent(jingle=1)
		p:present=present.Create(Self,x+Rnd(-10,10),y-50)
		If mypresents.count()
			lp:present=present(mypresents.last())
			p.nxt=lp
		EndIf
		mypresents.addlast p
		presenttime=30
		If jingle
			sounditem.Create(jingles[Rand(0,1)],1,1,1)
		EndIf
	End Method
	
	Method control()
		If KeyHit(KEY_P) pause()
		If joy>=0
			If JoyHit(7,joy) pause()
			If JoyHit(6,joy) 
				done=2
				i=Rand(0,2)
				PlaySound roosters[i]
			EndIf
		EndIf
	
		Select state
		Case -1,0,2
			dx# = KeyDown(KEY_RIGHT) - KeyDown(KEY_LEFT)
			dy# = KeyDown(KEY_DOWN) - KeyDown(KEY_UP)
			
			If joy>=0
				dx:+safejoyx(joy)
				dy:+safejoyy(joy)
			EndIf
			vwobble:+(dx*90-wobble)*.003
			vwobble:-Abs(dy)*wobble*.01
			wobble:*(1-Abs(dy)*.01)

			Rem
			v#=vx*vx+vy*vy
			If Not (KeyDown(KEY_DOWN) Or KeyDown(KEY_UP))
				nohits:+1
				If nohits>10
					vy:+(lasty-y)*.01+Sin(nohits*.01)*.03
					vy:*.99
				EndIf
			Else
				nohits=1
				lasty=y
			EndIf
			EndRem
			
			d# = Sqr(dx * dx + dy * dy)
			If d>0
				dx:/ d
				dy:/ d
			
				Select state
				Case -1,0
					f#=1
				Case 2
					f#=.06*ho.size/30
				End Select
				vx:+ f*.1 * dx
				vy:+ f*.25 * dy
			EndIf
		End Select
		
		If state=2
			If KeyHit(KEY_SPACE)
				throwpresent()
			EndIf
			If joy>=0
				If JoyHit(0,joy)
					throwpresent()
				EndIf
			EndIf
		Else
			KeyHit(KEY_SPACE)
			If joy>=0
				JoyHit(0,joy)
			EndIf
		EndIf
		
		dx = (x - panx)*zoom
		dy = (y - pany)*zoom
		d = dx * dx + dy * dy
		If d > 10000
			d = Sqr(d)-100
			panx:+ dx * d * .0001
			pany:+ dy * d * .0001
		EndIf
		
		'panx:+ (x - panx) * .1
		'pany:+ (y - pany) * .1
		
	End Method
	
	Method throwpresent()
		If mypresents.count()
			p:present=present(mypresents.removefirst())
			p.x=x
			p.y=y
			throwan:+10
			an=Rand(-throwan,throwan)
			f#=ho.size*ho.xst*.03
			p.vx=f*Sin(an)
			p.vy=-f*Cos(an)
			p.an=-an
			p.state=1
			p.ho=ho
			ho.mypresents.addlast p
			sounditem.Create(jingles[Rand(0,1)],1,1,1)
			
			If mypresents.count()
				p=present(mypresents.first())
				p.nxt=Null
			EndIf
		EndIf
	End Method
	
	Method playthump(v#)
		If v < .05 Return
		i = Rand(0 , 2)
		v:* 3
		If v>1 v=1
		sounditem.Create(thumps[i],1,1,v)
	End Method
	
	Method collide(vl:line,walls:TList)
				
		For l:line = EachIn walls
			'DrawzoomText Int(l.nx * 100) , l.x+ho.x , l.y+ho.y
			'DrawzoomText Int(l.ny * 100) , l.x + ho.x , l.y + 3 + ho.y
			'drawzoomtext Int(l.an),l.x+ho.x,l.y+ho.y+6
			hit# = vl.intersect(l)
			If hit >= 0
				hit:/vl.length
				x = ox + hit * vx
				y = oy + hit * vy
				
				dp#=vx * l.nx + vy * l.ny
				
				collision:+Abs(dp)
				
				rx# = 2 * l.nx * (dp)
				ry# = 2 * l.ny * (dp)
				vx:- rx
				vy:- ry
				vx:* .5
				vy:*.5
				
				x:+ (1 - hit) * vx
				y:+ (1 - hit) * vy
				
				playthump(Abs(dp))
				
			EndIf
		Next
	End Method
	
	Method mixmusic(t#)
		SetChannelVolume activemusic.chan,.8*t
		SetChannelVolume calmmusic.chan,.8*(1-t)
	End Method
		
	Method update()
		
		ox# = x
		oy# = y
		
		Select state
		Case -1
			If vx*vx+vy*vy>0 state=0
			nohits:+1
			y:+1*Sin(nohits*10)
			
			mixmusic(1)
		Case 0 'flying!
			x:+ vx
			y:+ vy
			vy:+ .1
			vx:* .99
			vy:*.99
			
			If y<-500 And vy<0
				vy:*.95
			EndIf
			maxy#=-1
			For tor:hill=EachIn hills
				hi#=tor.height(x)
				If hi>maxy maxy=hi
			Next
			If y>maxy
				vy:-.2
			EndIf
			
			mindist# = - 1
			ch:house=Null

			For h:house = EachIn houses
				dx# = x - h.x
				dy# = y - h.y
				d# = dx * dx + dy * dy
				
				If d < mindist Or mindist = - 1
					mindist = d
					ch=h
				EndIf
				
				'DrawText dx - h.chleft , 0 , 0
				'DrawText h.chright - dx , 0 , 15
				'DrawText h.chup-dy,0,30


				vl:line = line.Create(ox - h.x , oy - h.y , x - h.x , y - h.y)
				collision=0
				collide(vl , h.outwalls)
				h.thump(collision)
				
				If h.inchimney(x,y)
					ho = h
					changestate(1)
				EndIf
			Next
			
			If ch
				tt#=50.0/Sqr(mindist)
				ch.t:+(tt-ch.t)*.5
				If ch.t > 1 ch.t = 1
			EndIf
			
			If presenttime>0
				presenttime:-1
			Else
				th#=0
				For tor:hill=EachIn hills
					hi#=tor.height(x)-150
					If hi<th th=hi
				Next
				If y<th And mypresents.count()<5
					addpresent()
					bigtext.Create("-$50",20,255,0,0)
					tcash:-50
				EndIf
			EndIf
			
			tzoom = 1
			size=1
		Case 1 'entering house
			chimney()
			mixmusic(ho.t)
			If ho.t < 0
				changestate(2)
			EndIf
		Case 2 'in house
			x:+ vx
			y:+ vy
			vy:+ .006*ho.size/30
			vx:* .99
			vy:*.99
			
			hx# = x - ho.x
			hy# = y - ho.y
			
			
			vl:line=line.Create(ox-ho.x,oy-ho.y,x-ho.x,y-ho.y)
			collision=0
			collide(vl , ho.walls)
			collide(vl, ho.outwalls)
			ho.thump(collision)
						
			ol:line = line.Create(ho.size*.3 , -ho.size*.2 , ho.size*.5 , 0)
			'drawzoomline ol.x+ho.x,ol.y+ho.y,ol.ex+ho.x,ol.ey+ho.y
			If vl.intersect(ol,2) >= 0 And vy<0
				changestate(3)
			EndIf
			
			Rem
			If hx < - 15
				vx = - .5 * vx
				x = ho.x- 15
			EndIf
			If hx > 15
				vx = - .5 * vx
				x = ho.x+15
			EndIf
			If hy > 30
				vy = - .5*vy
				y = ho.y+30
			EndIf
			
			If hy < 0
				state = 0
				ho.state = 0
			EndIf
			EndRem
		Case 3 'leaving house
			chimney()
			mixmusic(ho.t)
			If ho.t >1
				changestate(0)
			EndIf
		
		End Select
		
		throwan#:*.99
		'DrawText throwan,0,15
		
		vwobble:-.01*Sin(wobble)
		vwobble:*.95
		wobble:+vwobble

		If state = 0
			v# = Sqr(vx * vx + vy * vy)
			v:/5
			If v > 1 v = 1
			v:*v*v
			SetChannelVolume whoosh.chan , v
		Else
			SetChannelVolume whoosh.chan, 0
		EndIf
		
		If cash<>tcash
			cash:+Sgn(tcash-cash)
		EndIf
			
	End Method
	
	Method changestate(st)
		Select st
		Case 0
			Select state
			Case 3
				doprices()
				state = 0
				vx=Rnd(-.5,.5)
				vy= -3
				x:+vx
				y:+vy
				ho.t=1
				ho.leave()
				ho=Null
				vwobble:+Sgn(wobble)*Rnd(10,30)
				mixmusic(1)
			End Select
		Case 1
			state = 1
			ho.t=1
			ho.state = 1
			sounditem.Create(flutedown,1,1,1)
		Case 2
			state = 2
			vx = 0
			vy = 0
			ho.t=0
			If mypresents.count()
				cashed=0
			EndIf
			mixmusic(0)
			ho.enter()
		Case 3
			state = 3
			ho.state = 3
			ho.t = 0
			sounditem.Create(fluteup,1,1,1)
		End Select
	End Method
	
	Method doprices()
		cashed=1
		oldhocash=ho.cash
		If ho.collision=1
			For p:present=EachIn ho.mypresents
				p.state=3
			Next
			bigtext.Create("TOO NOISY",70,255,0,0)
			Return
		EndIf
		If sunheight=1 Return
		incash=0
		For p:present=EachIn ho.mypresents
			If p.state<3 'not yet cashed
				dx#=p.x-ho.fireplace.x
				dy#=p.y-ho.fireplace.y
				d#=Sqr(dx*dx+dy*dy)
				If d/(ho.size*ho.xst)<1 'if within one cell of fireplace
					If ho.cash>=100
						incash:+100
						ho.cash:-100
					EndIf
				EndIf
				p.state=3
			EndIf
		Next
		If incash>0
			bigtext.Create("+$"+String(incash),50,0,255,0)
			tcash:+incash
		EndIf
		If oldhocash>0 And ho.cash<=0
			bigtext.Create("THANK YOU!",50,255,255,0,30)
			sounditem.Create(hohoho,1,1,1)
			ho.closechimney()
		EndIf
	End Method
	
	Method chimney()
			x:+(ho.x+(ho.chleft + ho.chright) / 2 - x)*.1
			y:+ ((ho.y+ho.chup) * ho.t + ho.y * (1 - ho.t) - y)*.3
			tzoom = 1.0 + 4*ho.numcells * (1 - ho.t) * 30 / ho.size
			tsize#=(.5/ho.numcells) * ho.size / 30
			size=tsize+ho.t*ho.t*(1-tsize)
	End Method
		
	Method draw()
		For p:present=EachIn mypresents
			p.draw()
		Next

		SetColor 255 , 255,255
		SetRotation wobble
		drawzoomimage santahead,x,y,15*size
		SetRotation 0
		'DrawZoomCircle x,y,5*size
		
	End Method
End Type

Global housepoly#[]
'housepoly = [9.0 , - 4.0 , 15.0 , 0.0 , 15.0 , 30.0 , - 15.0 , 30.0 , - 15.0 , 0.0 , 0.0 , - 10.0 , 6.0 , - 6.0]

Type house
	Field x# , y#
	Field size#
	Field poly#[]
	Field t#
	Field state
	Field chleft# , chright# , chup#
	Field walls:TList
	Field outwalls:TList
	Field numcells
	Field cells[,,]
	Field xst# , yst#
	Field furnishings:TList
	Field fireplace:furniture
	Field wallpaper:timage
	Field windowimage:timage,windowed[,]
	Field doorimage:timage
	Field doorx
	Field mypresents:TList
	Field collision#
	Field cash
	
	Method New()
		houses.addlast Self
		t = 0
		state = 0
		walls = New TList
		furnishings=New TList
		mypresents=New TList
	End Method
	
	Method linkleft(x , y)
		'addwall((x+.5)*xst,(y+.5)*yst,(x-.5)*xst,(y+.5)*yst)
		If Rand(3)>1
			p#=Rnd(.1,.6)
			addwall(x*xst,y*yst,x*xst,(y+p)*yst)
			addwall(x * xst , (y + p + .3) * yst , x * xst , (y + 1) * yst)
		EndIf
		cells[x , y , 4] = 1
		cells[x - 1 , y , 2] = 1
		fillcell(x - 1 , y)
	End Method
			
	Method linkright(x , y)
		'addwall((x+.5)*xst,(y+.5)*yst,(x+1.5)*xst,(y+.5)*yst)
		If Rand(3)>1
			p#=Rnd(.1,.6)
			addwall((x+1)*xst,y*yst,(x+1)*xst,(y+p)*yst)
			addwall( (x + 1) * xst , (y + p + .3) * yst , (x + 1) * xst , (y + 1) * yst)
		EndIf
		cells[x , y , 2] = 1
		cells[x + 1 , y, 4] = 1
		fillcell(x + 1 , y)
	End Method
	
	Method linkup(x , y) 
		'addwall((x+.5)*xst,(y+.5)*yst,(x+.5)*xst,(y-.5)*yst)
		p#=Rnd(.1,.6)
		addwall(x*xst,y*yst,(x+p)*xst,y*yst)
		addwall((x+p+.3)*xst,y*yst,(x+1)*xst,y*yst)
		cells[x , y , 1] = 1
		cells[x , y - 1 , 3] = 1
		fillcell(x , y - 1)
	End Method
	
	Method linkdown(x , y)
		'addwall((x+.5)*xst,(y+.5)*yst,(x+.5)*xst,(y+1.5)*yst)
		p#=Rnd(.1,.6)
		addwall(x*xst,(y+1)*yst,(x+p)*xst,(y+1)*yst)
		addwall((x+p+.3)*xst,(y+1)*yst,(x+1)*xst,(y+1)*yst)
		cells[x , y , 3] = 1
		cells[x , y + 1 , 1] = 1
		fillcell(x , y + 1)
	End Method
	
	Method fillcell(x , y)
		cells[x , y , 0] = 1
		If x > 0
			If Not cells[x - 1 , y , 0]
				If Rand(3) = 1
					linkleft(x,y)
				EndIf
			EndIf
		EndIf
		If x < numcells - 1
			If Not cells[x + 1 , y , 0]
				If Rand(3) = 1
					linkright(x,y)
				EndIf
			EndIf
		EndIf
		
		If y > 0
			If Not cells[x , y - 1 , 0]
				If Rand(3) = 1
					linkup(x,y)
				EndIf
			EndIf
		EndIf
		
		If y < numcells - 1
			If Not cells[x , y + 1 , 0]
				If Rand(3) = 1
					linkdown(x,y)
				EndIf
			EndIf
		EndIf
	
		'end first (random) pass, check for empties and make walls
	
		If x > 0
			If Not cells[x - 1 , y , 0]
				linkleft(x,y)
			Else
				If Not cells[x,y,4]
					addwall(x * xst , y * yst , x * xst , (y + 1) * xst) 
				EndIf
			EndIf
		EndIf
		If x < numcells - 1
			If Not cells[x + 1 , y , 0]
				linkright(x,y)
			EndIf
		EndIf
		
		If y > 0
			If Not cells[x , y - 1 , 0]
				linkup(x,y)
			Else
				If Not cells[x , y , 1]
					addwall(x * xst , y * yst , (x + 1) * yst , y * yst) 
				EndIf
			EndIf
		EndIf
		
		If y < numcells - 1
			If Not cells[x , y + 1 , 0]
				linkdown(x,y)
			EndIf
		EndIf
	End Method
	
	Method makewalls()
		addwall(0 , 0 , 0 , 1)
		addwall(1 , 0 , 1 , 1)
		addwall(0 , 1 , 1 , 1)
		addwall(0 , 0 , .5 , -.5)
		addwall(.5 , -.5 , .8 , -.2)
		outwalls = walls
		walls = New TList
		
		chleft = size * .3
		chright = size * .8
		chup=size*.2

		cells = New Int[numcells , numcells , 5]
		xst# = 1.0 / numcells
		yst# = 1.0 / numcells
		
		fillcell(numcells - 1 , 0)
		addwall(0 , 0 , 1 - xst , 0)
		
		fx = numcells - 1
		fy = 0
		While (fx = numcells - 1 And fy = 0) Or cells[fx , fy , 3] = 1
			fx = Rand(numcells) - 1
			fy = numcells - 1
			While Rand(2) = 1 And fy > 0
				fy:- 1
			Wend
			'fy = Rand(numcells)-1
		Wend
		i = Rand(0 , 3)
		p# = Rnd(.2 , .8) 
		fireplace = furniture.Create(x + size * (fx + p) * xst - size/2 , y + size * (fy + 1) * yst , firesound,2.5*size/30,fireplaces[i],.4*size*xst)
		furnishings.addlast fireplace
		
		addfurniture(fx,fy,Null,0,christmastrees[Rand(0,3)],.5)
		
		i = Rand(0 , 3)
		wallpaper=wallpapers[i]
		
		windowed=New Int[numcells,numcells]
		windowimage=windows[Rand(0,3)]
		numwindows=1
		For cx=0 To numcells/2
		For cy=0 To numcells-2
			If Rand(numwindows)=1
				numwindows:+1
				windowed[cx,cy]=1
				windowed[numcells-1-cx,cy]=1
			EndIf
		Next
		Next
		
		Local bedded[numcells,numcells]
		maxcash=0
		For cx=0 To numcells-1
		For cy=0 To numcells-1
			If cells[cx,cy,3]=1 Or (cx=fx And cy=fy)
				bedded[cx,cy]=1
			Else
				maxcash:+1
			EndIf
		Next
		Next
		
		cash=Rand(numcells-2,maxcash/1.5)
		If cash<1 cash=1
		If cash>numcells*numcells-1
			cash=numcells*numcells-1
		EndIf
		
		For i=1 To cash
			bx=Rand(numcells)-1
			by=Rand(numcells)-1
			While bedded[bx,by] Or (bx=fx And by=fy)
				bx=Rand(numcells)-1
				by=Rand(numcells)-1
			Wend
			bedded[bx,by]=1
			addfurniture(bx,by,Null,0,beds[Rand(0,3)],.6)
		Next
		
		cash:*100
		
		doorimage=doors[Rand(0,3)]
		doorx=Rand(0,numcells-1)
		
	End Method
	
	Method addfurniture(cx , cy , sound:TSound,volume#,image:timage,width#)
		p# = Rnd(width/2 , 1-width/2) 
		f:furniture = furniture.Create(x + size * (cx + p) * xst - size/2 , y + size * (cy + 1) * yst , sound,volume,image,width*size*xst)
		furnishings.addlast f
	End Method
	
	
	Method addwall(x1# , y1# , x2# , y2#)
		l:line = line.Create(size * (x1 - .5), size * y1 , size * (x2 - .5), size * y2)
		walls.addlast l
	End Method
	
	Function Create:house(x# , y#,size#,numcells)
		h:house = New house
		h.x = x
		h.y = y-size*.9
		h.size=size
		h.numcells = numcells
		h.makewalls()
		Return h
	End Function
	
	Method enter()
		state = 2
		For f:furniture=EachIn furnishings
			If f.sound
				ResumeChannel f.sound.chan
			EndIf
		Next
	End Method
	
	Method leave()
		state=0
		For f:furniture = EachIn furnishings
			If f.sound
				PauseChannel f.sound.chan 
			EndIf
		Next
	End Method
	
	Method inchimney(px# , py#)
		If collision=1 Or cash=0 Return False
		px:- x
		py:- y
		chleft=size*.2-5
		l# = size * .3
		r# = size * .5
		chright=size*.6+5
		chup=-size*.4
		d# = -size * .2
		If pointintriangle(px , py , l , d , chleft , chup , chright , chup) Or pointintriangle(px , py , l , d , chright , chup , r , 0)
			Return True
		Else
			Return False
		EndIf
	End Method
	
	Method closechimney()
		outwalls.addlast line.Create(size*.3,-size*.2,size*.5,0)
	End Method	
	
	Method thump(v#)
		If collision=1 Return
		collision:+v*1.1
		If collision>=1
			collision=1
			sounditem.Create(cries[Rand(0,3)],1,1,1)
			If s.state=2
				s.changestate(3)
			EndIf
			closechimney()
		EndIf
	End Method

	Method update()
		Select state
		Case 0 'empty house
			t:*.97
		Case 1 'santa in chimney
			t:-.04
		Case 2 'santa in house
		Case 3 'santa leaving house
			t:+.04
		End Select
		
		If state > 0
			For f:furniture = EachIn furnishings
				f.update()
			Next
		EndIf
		
	End Method
	
	Method drawexterior()
		SetColor 255,255,255
		drawzoomimage bricks,x-size/2,y,size
		drawzoomimage roof,x-size/2,y,size
		For cx=0 To numcells-1
		For cy=0 To numcells-1
			If windowed[cx,cy]
				drawzoomimage windowimage,(cx+.5)*xst*size+x-size/2,(cy+.5)*yst*size+y,xst*size*.6
			EndIf
		Next
		Next
		
		drawzoomimage doorimage,(doorx+.5)*xst*size+x-size/2,y+size,xst*size*.9,1
	End Method
	
		
	Method draw() 
		If zoomx(x+size)<0 Or zoomx(x-size)>gwidth Return

		SetColor 255 , 255 , 255

		If state=0	
			drawexterior()	
			linewidth#=.2*size
			Local poly#[]
			If collision=1
				SetColor 255,0,0
				poly=[x-size/2,y,x-size/2+linewidth,y,x+size/2,y+size-linewidth,x+size/2,y+size,x+size/2-linewidth,y+size,x-size/2,y+linewidth]
				drawzoompoly poly
				poly=[x-size/2,y+size,x-size/2,y+size-linewidth,x+size/2-linewidth,y,x+size/2,y,x+size/2,y+linewidth,x-size/2+linewidth,y+size]
				drawzoompoly poly
			ElseIf cash=0
				SetColor 0,255,0
				poly=[x-size/2,y+size/2-linewidth/2,x,y+size-linewidth,x,y+size,x-linewidth/2,y+size,x-size/2,y+size/2+linewidth/2]
				drawzoompoly poly
				poly=[x,y+size,x,y+size-linewidth,x+size/2-linewidth,y,x+size/2,y,x+size/2,y+linewidth,x+linewidth/2,y+size]
				drawzoompoly poly
			EndIf
			
		Else
			col#=100+150*t
			SetColor col,col,col
			drawzoomimage roof,x-size/2,y,size

			If state=1 Or state=3
				SetAlpha t
				drawexterior()
				SetAlpha 1
			EndIf
		
			SetAlpha 1 - t
			SetColor 100,100,100
			For cx = 0 To numcells - 1
				For cy = 0 To numcells - 1
					DrawzoomImage wallpaper , x + cx * size * xst - size/2, y + cy * size * yst , size * xst
				Next
			Next
			
			
			SetLineWidth 4
			If collision=1
				SetColor 255,0,0
			Else
				col#=255*(1-collision)
				SetColor col,col,col
			EndIf
			For l:line = EachIn walls
				drawzoomline(l.x + x , l.y + y , l.ex + x , l.ey + y)
			Next
			For l:line = EachIn outwalls
				drawzoomline(l.x + x , l.y + y , l.ex + x , l.ey + y)
			Next
			drawzoomline x+size*.3,y-size*.2,x+size*.3,y-size*.3
			drawzoomline x+size*.5,y,x+size*.5,y-size*.3
			SetLineWidth 1
			SetAlpha 1


			For f:furniture = EachIn furnishings
				f.draw(1-t)
			Next

			SetColor 255,255,255
			For p:present=EachIn mypresents
				p.draw()
			Next
		EndIf
		
	End Method
End Type

Type furniture
	Field x# , y#
	Field sound:sounditem
	Field volume#
	Field image:timage
	Field width#
		
	Function Create:furniture(x# , y# , sound:TSound,volume#,image:timage,width#)
		f:furniture = New furniture
		f.x = x
		f.y = y
		If sound
			f.sound = sounditem.Create(sound , 1 , 1 , 0 , 1)
			f.volume=volume
		EndIf
		f.image = image
		f.width = width
		Return f
	End Function
	
	Method update()
		If sound
			dx# = s.x - x
			dy# = s.y - y
			d# = Sqr(dx * dx + dy * dy)
			v# = volume / d
			If v > 1 v = 1
			SetChannelVolume sound.chan , v
		EndIf
	End Method
	
	Method draw(alpha#)
		SetAlpha alpha
		SetColor 255,255,255
		drawzoomimage image,x,y,width
		SetAlpha 1
	End Method
End Type


Type present
	Field x#,y#,vx#,vy#,an#
	Field image:timage
	Field ox#,oy#
	Field state
	Field ho:house
	Field sn:santa
	Field nxt:present
	Field size#
	Field collided
	
	Method New()
		presents.addlast Self
		state=0
		
	End Method
	
	Function Create:present(sn:santa,x#,y#)
		p:present=New present
		p.sn=sn
		p.x=x
		p.y=y
		p.image=presentimages[Rand(0,3)]
		Return p
	End Function
	
	Method update()
		Select state
		Case 0 'following santa
			ox#=x
			oy#=y
			If sn.state=0
				vy:+.1*Sin(MilliSecs()/5.0)
	
				If nxt
					dx#=x-nxt.x
					dy#=y-nxt.y
				Else
					dx#=x-sn.x
					dy#=y-sn.y
				EndIf
				d#=Sqr(dx*dx+dy*dy)
				td#=20*sn.size
				If d>td
					f#=.03*(td-d)
					vx:+f*dx/d
					vy:+f*dy/d
					If nxt
						nxt.vx:-.5*f*dx/d
						nxt.vy:-.5*f*dy/d
					EndIf
				EndIf
			Else
				If nxt
					dx#=nxt.x-x
					dy#=nxt.y-y
				Else
					dx#=sn.x-x
					dy#=sn.y-y
				EndIf
				d#=Sqr(dx*dx+dy*dy)
				td#=10*sn.size
				If nxt And d>td
					x:+(nxt.x-td*dx/d-x)*.2
					y:+(nxt.y-td*dy/d-y)*.2
				EndIf
				f#=.01*d
				vx:+f*dx/d
				vy:+f*dy/d
				'vx:*.95
				'vy:*.95
			EndIf
			x:+vx
			y:+vy
			vx:*.9
			vy:*.9

			'For h:house=EachIn houses
			'	vl:line=line.Create(ox-h.x,oy-h.y,x-h.x,y-h.y)
			'	collide(vl,h.outwalls)
			'Next
			'If sn.ho
			'	vl:line=line.Create(ox-sn.ho.x,oy-sn.ho.y,x-sn.ho.x,y-sn.ho.y)
			'	collide(vl,sn.ho.walls)
				'collide(vl,sn.ho.outwalls)
			'EndIf
			
			tsize#=10*sn.size
			size:+(tsize-size)*.2
			
		Case 1,2,3 'thrown
			ox#=x
			oy#=y
			x:+vx
			y:+vy
			vy:+.01*ho.size/30
			an:*.99
			
			vl:line=line.Create(ox-ho.x,oy-ho.y,x-ho.x,y-ho.y)
			collided=0
			collide(vl,ho.walls)
			collide(vl,ho.outwalls)
			
			If collided
				v#=vx*vx+vy*vy
				If v<.001
				'	state=2
				EndIf
			EndIf
			
		Case 2 'landed
		
		End Select
		
	End Method
	
	Method draw()
		SetRotation an
		SetColor 255,255,255
		drawzoomimage image,x,y,size
		SetRotation 0
	End Method

	Method collide(vl:line,walls:TList)
				
		For l:line = EachIn walls
			'DrawzoomText Int(l.nx * 100) , l.x+ho.x , l.y+ho.y
			'DrawzoomText Int(l.ny * 100) , l.x + ho.x , l.y + 3 + ho.y
			'drawzoomtext Int(l.an),l.x+ho.x,l.y+ho.y+6
			hit# = vl.intersect(l)
			If hit >= 0
				hit:/vl.length
				x = ox + hit * vx
				y = oy + hit * vy
				
				dp#=vx * l.nx + vy * l.ny
				v#=Sqr(vx*vx+vy*vy)
				If Abs(dp/v)>.2 'bounce
				
					rx# = 2 * l.nx * (dp)
					ry# = 2 * l.ny * (dp)
					vx:- rx
					vy:- ry
					vx:* .5
					vy:*.5
					
					x:+ (1 - hit) * vx
					y:+ (1 - hit) * vy
				Else
					dp2#=vx*l.dx+vy*l.dy
					vx=l.dx*dp*.99
					vy=l.dy*dp*.99
					x:+(1-hit)*vx
					y:+(1-hit)*vy
				EndIf
				If Abs(vx*vx+vy*vy)<.01
					vx=0
					vy=0
				EndIf
					
				collided=1
			EndIf
		Next
	End Method
End Type

Type hill
	Field x# , y# , size#
	Field depth#
	
	Method New()
		hills.addlast Self
		depth=hills.count()
	End Method
	
	Function Create:hill(x# , y# , size#)
		h:hill = New hill
		h.x = x
		h.y = y
		h.size = size
		Return h
	End Function

	Method draw()
		Local tri#[6]
		Local poly#[10]
		oldx# = 0
		oldy# = 0
		ti = 0
		col# = 200 * depth / hills.count()
		SetColor col,col,col
		For ix# = unzoomx(0) To unzoomx(gwidth)+50 Step 50
			
			iy# = height(ix)
			
			If ti > 0
					drawzoomline oldx,oldy,ix,iy
					Rem
					If iy < oldy
						tri = [oldx , oldy , ix , iy , ix , oldy]
						'drawzoomrect oldx,oldy,ix-oldx,1000
					Else
						tri = [oldx , iy , oldx , oldy , ix , iy]
						'drawzoomrect oldx,iy,ix-oldx,1000
					EndIf
					
					tri=zoompoly(tri)
					'DrawPoly tri
					'DrawRect tri[0],tri[1],tri[4]-tri[0],gheight-tri[1]
					
					For i=0 To 5
						poly[i]=tri[i]
					Next
					poly[6]=tri[4]
					poly[7]=gheight
					poly[8]=tri[0]
					poly[9]=gheight
						
					'DrawPoly poly
					EndRem
			EndIf
			ti:+1
			oldx# = ix
			oldy# = iy
		Next
		SetAlpha 1
	End Method
	
	Method height#(x#)
		Return y + size * 100 * (1 - Cos( (x - ix) / size))
	End Method	
End Type


Type bigtext
	Field txt$
	Field t,life
	Field r,g,b
	
	Method New()
		bigtexts.addlast Self
	End Method
	
	Function Create:bigtext(txt$,life,r=255,g=255,b=255,del=0)
		bt:bigtext=New bigtext
		bt.txt=txt
		bt.life=life
		bt.r=r
		bt.g=g
		bt.b=b
		bt.t=-del
		
		Return bt
	End Function
	
	Method update()
		t:+1
		If t=life
			bigtexts.remove Self
		EndIf
	End Method
	
	Method draw()
		If t<0 Return
		si#=Sin(180*Float(t)/life)
		SetScale si*5,si*5
		w=TextWidth(txt)*si*5
		SetColor r,g,b
		DrawText txt,gwidth/2-w/2,si*200-50
		SetScale 1,1
	End Method
End Type

Type snow
	Field x#,y#
	Field vx#
	Field ox,oy
	Field t
	
	Method New()
		snows.addlast Self
		x=Rnd(-5000,5000)
		y=Rnd(-1500,1500)
		ox=zoomx(x)
		oy=zoomy(y)
	End Method

	Method update()
		vx:+Rnd(-.01,.01)
		vx:*.99
		x:+vx
		y:+1
		If y>1500
			y=-1500
			oy=y
		EndIf
		t:+1
	End Method
	
	Method draw()
		dx=zoomx(x)
		dy=zoomy(y)
		DrawLine dx,dy,ox,oy
		ox=dx
		oy=dy
	End Method
End Type	


Function drawzongo(txt$,y,j)
	SetScale 5,5
	w#=TextWidth(txt)*5
	Select j
	Case -1
		x#=0
	Case 0
		x#=(gwidth-w)/2
	Case 1
		x#=gwidth-w
	End Select
	DrawText txt,x,y
	SetScale 1,1
End Function

Function assignjoy()
	If joyN>=JoyCount() Return -1
	joyN:+1
	Return joyN-1
End Function

Function SafeJoyX#(joy)
	jx#=JoyX(joy)
	If Abs(jx)<.2 Return 0
	Return jx
End Function

Function SafeJoyY#(joy)
	jy#=JoyY(joy)
	If Abs(jy)<.2 Return 0
	Return jy
End Function



Function anykey()
	out=0
	For i=0 To 200
		If KeyHit(i) out=1
	Next
	For i=0 To JoyCount()-1
		For b=0 To 9
			If JoyHit(b,i) out=1
		Next
	Next
	Return out
End Function


''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'****************************************************************************************'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


'incbins
Incbin "thump-1.wav"
Incbin "thump-2.wav"
Incbin "thump-3.wav"
Incbin "flute up.wav"
Incbin "flute down.wav"
Incbin "fireloop.wav"
Incbin "windloop.wav"
Incbin "bb1.wav"
Incbin "bb2.wav"
Incbin "twobark.wav"
Incbin "catscream.wav"
Incbin "Silent Night.ogg"
Incbin "8bp038-02-nullsleep-silent-night.ogg"
Incbin "grelot 01.wav"
Incbin "grelot 03.wav"
Incbin "hoho.ogg"
Incbin "tick1.wav"
Incbin "Rooster_chicken_calls_1.ogg"
Incbin "Rooster_chicken_calls_2.ogg"
Incbin "rooster3.ogg"
Incbin "8bp038-04-paul_slocum-up_on_the_housetop.ogg"

Incbin "fireplace1.png"
Incbin "fireplace2.png"
Incbin "fireplace3.png"
Incbin "fireplace4.png"
Incbin "wallpaper1.png"
Incbin "wallpaper2.png"
Incbin "wallpaper3.png"
Incbin "wallpaper4.png"
Incbin "santahead.png"
Incbin "brick_wall_14.jpg"
Incbin "roofTiles.png"
Incbin "window1.png"
Incbin "window2.png"
Incbin "window3.png"
Incbin "window4.png"
Incbin "door1.png"
Incbin "door2.png"
Incbin "door3.png"
Incbin "door4.png"
Incbin "present1.png"
Incbin "present2.png"
Incbin "present3.png"
Incbin "present4.png"
Incbin "christmastree1.png"
Incbin "christmastree2.png"
Incbin "christmastree3.png"
Incbin "christmastree4.png"
Incbin "bed1.png"
Incbin "bed2.png"
Incbin "bed3.png"
Incbin "bed4.png"




'sounds
'EnableOpenALAudio
SetAudioDriver "DirectSound"

Global thumps:TSound[]=[LoadSound("incbin::thump-1.wav"),LoadSound("incbin::thump-2.wav"),LoadSound("incbin::thump-3.wav")]
Global fluteup:TSound = LoadSound("incbin::flute up.wav")
Global flutedown:TSound=LoadSound("incbin::flute down.wav")
Global firesound:TSound = LoadSound("incbin::fireloop.wav")
Global windsound:TSound=LoadSound("incbin::windloop.wav")
Global cries:TSound[]=[LoadSound("incbin::bb1.wav"),LoadSound("incbin::bb2.wav"),LoadSound("incbin::twobark.wav"),LoadSound("incbin::catscream.wav")]
Global musics:TSound[]=[LoadSound("incbin::8bp038-02-nullsleep-silent-night.ogg"),LoadSound("incbin::Silent Night.ogg"),LoadSound("incbin::8bp038-04-paul_slocum-up_on_the_housetop.ogg")]
Global jingles:TSound[]=[LoadSound("incbin::grelot 03.wav"),LoadSound("incbin::grelot 01.wav")]
Global hohoho:TSound=LoadSound("incbin::hoho.ogg")
Global tick:TSound=LoadSound("incbin::tick1.wav")
Global roosters:TSound[]=[LoadSound("incbin::Rooster_chicken_calls_1.ogg"),LoadSound("incbin::Rooster_chicken_calls_2.ogg"),LoadSound("incbin::rooster3.ogg")]
'PlaySound roosters[i]


'graphics init
SeedRnd MilliSecs()
gwidth = 600
gheight = 800
AppTitle = "Santa's Freelance Adventure"

'SetGraphicsDriver D3D7Max2DDriver()
Graphics gwidth , gheight
SetBlend ALPHABLEND


'images
Global fireplaces:timage[]=[LoadImage("incbin::fireplace1.png"),LoadImage("incbin::fireplace2.png"),LoadImage("incbin::fireplace3.png"),LoadImage("incbin::fireplace4.png")]
	For i=0 To 3
		SetImageHandle fireplaces[i] , ImageWidth(fireplaces[i]) / 2 , ImageHeight(fireplaces[i])
	Next
Global wallpapers:timage[] = [LoadImage("incbin::wallpaper1.png") , LoadImage("incbin::wallpaper2.png") , LoadImage("incbin::wallpaper3.png") , LoadImage("incbin::wallpaper4.png")]
Global santahead:timage = LoadImage("incbin::santahead.png")
	SetImageHandle santahead,ImageWidth(santahead)/2,ImageHeight(santahead)/2
Global bricks:timage=LoadImage("incbin::brick_wall_14.jpg")
Global roof:timage=LoadImage("incbin::roofTiles.png")
	SetImageHandle roof,0,ImageHeight(roof)
Global windows:timage[]=[LoadImage("incbin::window1.png"),LoadImage("incbin::window2.png"),LoadImage("incbin::window3.png"),LoadImage("incbin::window4.png")]
	For i=0 To 3
		SetImageHandle windows[i],ImageWidth(windows[i])/2,ImageHeight(windows[i])/2
	Next
Global doors:timage[]=[LoadImage("incbin::door1.png"),LoadImage("incbin::door2.png"),LoadImage("incbin::door3.png"),LoadImage("incbin::door4.png")]
	For i=0 To 3
		SetImageHandle doors[i],ImageWidth(doors[i])/2,ImageHeight(doors[i])
	Next
Global presentimages:timage[]=[LoadImage("incbin::present1.png"),LoadImage("incbin::present2.png"),LoadImage("incbin::present3.png"),LoadImage("incbin::present4.png")]
	For i=0 To 3
		SetImageHandle presentimages[i],ImageWidth(presentimages[i])/2,ImageHeight(presentimages[i])
	Next
Global christmastrees:timage[]=[LoadImage("incbin::christmastree1.png"),LoadImage("incbin::christmastree2.png"),LoadImage("incbin::christmastree3.png"),LoadImage("incbin::christmastree4.png")]
	For i=0 To 3
		SetImageHandle christmastrees[i],ImageWidth(christmastrees[i])/2,ImageHeight(christmastrees[i])
	Next
Global beds:timage[]=[LoadImage("incbin::bed1.png"),LoadImage("incbin::bed2.png"),LoadImage("incbin::bed3.png"),LoadImage("incbin::bed4.png")]
	For i=0 To 3
		SetImageHandle beds[i],ImageWidth(beds[i])/2,ImageHeight(beds[i])*.95
	Next
		
	
'******* GLOBALS ********
		
Global bigtexts:TList
Global hills:TList
Global presents:TList
Global santas:TList
Global houses:TList
Global snows:TList
Global s:santa
Global time,sunheight#,clockx#,done,ms,hour#



'------------------------------------------------------

Function introtext(t#)
		Flip
		Cls
		DrawText "Saint Nick in...",0,30
		SetScale 2.5,2.5
		DrawText "SANTA'S",30,100
		SetScale 4,4
		DrawText "FREELANCE",gwidth/2-TextWidth("FREELANCE")*2,130
		SetScale 2.5,2.5
		DrawText "ADVENTURE" , gwidth-30-TextWidth("ADVENTURE")*2.5, 180
		SetScale 1,1
		DrawRect 0 , gheight * .45 , gwidth * t , gheight * .1
		DrawText "Control Santa with arrow keys or joystick",0,gheight*.6
		DrawText "Fly down chimneys to get into houses",0,gheight*.6+15
		DrawText "Drop presents near the fireplace by pressing spacebar or button 1",0,gheight*.6+30
		DrawText "Don't wake the children up!",0,gheight*.6+45
		DrawText "Fly high in the sky to get more presents",0,gheight*.6+60
		DrawText "Finish before dawn!",0,gheight*.6+75
End Function

Function initgame()
	'intro music
	imusicchan:TChannel=PlaySound(musics[2])
	

	'init lists
	bigtexts=New TList
	hills=New TList
	presents=New TList
	santas=New TList
	houses=New TList
	sounditems=New TList
	snows=New TList
	
	
	panx#=0
	pany#=0
	zoom#=1
	tzoom#=zoom
	joyN=0
	
	For i=1 To 2000
		New snow
	Next
	
		
'create player santa
	s = New santa
	s.x = 0
	s.y = -250
	s.lasty=s.y
	pany=-150
	For i=1 To 5
		s.addpresent(0)
	Next
	s.joy=assignjoy()
	
	'create hills
	x# = 0
	y#=0
	For i = 1 To 5
		x:+ Rnd( 1000 , 2000)
		y:+Rnd(0,40)
		hill.Create(x , y , Rnd(100 , 500) )
		hill.Create(-x,y,Rnd(100,500))
	Next
	
	bigtext.Create("HO HO HO!",150,255,255,255)
	
	'create houses
	numhouses = 100
	its=0
	For i = 1 To numhouses
		introtext(Float(i)/numhouses)
		
		x# = Rnd( - 2000 , 2000)
		x#=-2000+4000*i/numhouses
		mindist# = - 1
		its=0
		While mindist < 100
			its:+1
			candidates:TList=New TList
			minheight#=0
			For tor:hill=EachIn hills.reversed()
				y# = tor.height(x)
				If y < minheight Or Not candidates.count()
					candidates.addlast tor
					minheight = y
				EndIf
				'Print y
			Next
			hi = Rand(0 , candidates.count() - 1)
			tor:hill = hill(candidates.valueatindex(hi) )
			y# = tor.height(x)
			size#=20*(1+2*tor.depth/hills.count())
			
			If houses.count()
				mindist=-1
				For h:house = EachIn houses
					dx# = x - h.x
					dy# = y - h.y
					d = dx * dx + dy * dy
					'If d < mindist Or mindist = - 1
					'	mindist = d
					'EndIf
					If Abs(dx) < mindist Or mindist = - 1
						mindist = dx
					EndIf
				Next
			Else
				mindist = 22500
			EndIf
			x:+ Rnd( 0 , 50)
		Wend
		house.Create(x , y , size, Rand(2 , 5) )
		While Rand(8)=1
			x:+size+4
			house.Create(x,tor.height(x),size,Rand(2,5))
		Wend
		
		If KeyHit(KEY_ESCAPE) Or AppTerminate() End
	Next
	
	FlushKeys()
	
	out=0
	While Not out 
		introtext(1)
		If MilliSecs() Mod 300<150
			SetScale 1.5,1.5
			txt$="PRESS ANY KEY TO START"
			DrawText txt,gwidth/2-TextWidth(txt)*.75,gheight*.6+120
			SetScale 1,1
		EndIf
		
		If KeyHit(KEY_ESCAPE) Or AppTerminate() End
		
		If anykey() out=1
		
		If Not ChannelPlaying(imusicchan)
			PlaySound musics[2],imusicchan
		EndIf
	Wend
	
	StopChannel imusicchan
End Function

Function drawworld()
		Flip
		Cls
		
		SetClsColor 255*sunheight,170*sunheight,51*sunheight
	
		chour#=(hour-.5)*12
		If chour<0 chour:+24
		cminutes$=String(Int((chour-Int(chour))*60))
		If Len(cminutes)=1 cminutes="0"+cminutes
		chours$=String(Int(chour))
		If Len(chours)=1 chours="0"+chours
		txt$=chours+":"+cminutes
		w=TextWidth(txt)
		he=TextHeight(txt)
		
		dx=-(1-(.9^Abs(panx/100.0)))*gwidth*.3*Sgn(panx)
		dy=(1-(.99^Abs(pany/5.0)))*60*Sgn(pany)
		SetScale 7*zoom,7*zoom
		
		clockx:+(panx-clockx)*.001
		x#=zoomx(clockx)
		y#=zoomy(-40+dy)
		w:*7*zoom/2
		he:*7*zoom/2
		'If time Mod 120<60
		'	SetColor 0,255,0
		'Else
		SetColor 255,255,255
		'EndIf
		SetAlpha .1
		DrawText txt,x-w,y-he
		SetScale 1,1
		SetAlpha 1
		'DrawText hour,0,30
		'DrawText sunheight,0,45
		'DrawText panx,0,60
		
		SetAlpha .3
		SetColor 255,255,0
		SetScale 5,5
		DrawText "$"+String(s.cash),0,0
		SetScale 1,1
		SetAlpha 1

		For tor:hill=EachIn hills
			tor.draw()
		Next
		
		For h:house = EachIn houses
			h.update()
			h.draw()
		Next
		For sn:santa=EachIn santas
			s.draw()
		Next

		SetAlpha .3
		For fl:snow=EachIn snows
			fl.draw()
		Next
		SetAlpha 1
		
		For bt:bigtext=EachIn bigtexts
			bt.draw()
		Next
		
		If sunheight=1
			If time Mod 30<15
				SetScale 5,5
				SetColor 255,255,255
				If hour>0
					Drawzongo "<- TOO EARLY",100,1
				Else
					Drawzongo "TOO LATE ->",100,-1
				EndIf
				SetScale 1,1
			EndIf
		EndIf
	
		If s.mypresents.count()=0 And s.y>-150 And s.cashed
			If time Mod 30<15
				SetScale 5,5
				SetColor 255,255,255
				Drawzongo "^ BUY MORE ^",gheight/2,0
			EndIf
		EndIf
End Function

Function pause()
	resumes:TList=New TList
	For si:sounditem=EachIn sounditems
		If ChannelPlaying(si.chan)
			PauseChannel si.chan
			resumes.addlast si
		EndIf
	Next
	While Not anykey()
		drawworld()
		SetAlpha .5
		SetColor 0,0,0
		DrawRect 0,0,gwidth,gheight
		SetAlpha 1
		
		If MilliSecs() Mod 300<150
			SetColor 255,255,255
			SetScale 5,5
			txt$="PAUSED"
			DrawText txt,gwidth/2-TextWidth(txt)*5/2,gheight/2-TextHeight(txt)*5/2
			SetScale 1,1
		EndIf
		If AppTerminate()
			End
		EndIf
		
	Wend
	
	For si=EachIn resumes
		ResumeChannel si.chan
	Next
End Function

Function rungame()
	time=0
	sunheight#=0
	clockx#=0
	
	'main loop
	
	done = 0
	ms=MilliSecs()
	While Not done
		
		'update zoom		
		zoom:+ (tzoom - zoom) * .1
		
		'work out time
		time:+1
		If time>20000
			done=2
			PlaySound roosters[Rand(0,2)]
		EndIf
		hour#=time/20000.0
		sunheight#=Cos(hour*90)
		sunheight=1-Sqr(Sqr(sunheight))
		

		'update objects
		
		For h:house = EachIn houses
			h.update()
		Next
		
		'New snow
		For fl:snow=EachIn snows
			fl.update()
		Next
		
		s.control()
		
		For sn:santa=EachIn santas
			s.update()
		Next
		
		For p:present=EachIn presents
			p.update()
		Next
		
		For bt:bigtext=EachIn bigtexts
			bt.update()
		Next
		
		For sound:sounditem = EachIn sounditems
			sound.update()
		Next
		

		'draw everything
		drawworld()
		
		
		oldms=ms
		ms = MilliSecs()
		fps# = 1000.0 / (ms - oldms)
		'DrawText Int(fps),0,0
		
		If KeyHit(KEY_ESCAPE) Or AppTerminate()
			done = 1
		EndIf
	Wend
	
	Return done	
End Function
	
Function showscores()

	
	For i=0 To 100
		drawworld()
		SetAlpha i/100.0
		SetColor 0,0,0
		DrawRect 0,0,gwidth,gheight
		SetAlpha 1
	Next
	
	fullhouses=0
	noisies=0
	For h:house=EachIn houses
		If h.cash=0
			fullhouses:+1
		EndIf
		If h.collision=1
			noisies:+1
		EndIf
	Next
	fullhouses:*100
	noisies:*100
	total=s.cash+fullhouses-noisies
	
	out=0
	showcash=-1
	showfullhouse=-1
	shownoise=-1
	showtotal=-1
	
	FlushKeys()
	time=0
	del=0

	SetClsColor 0,0,0

	While Not out
		Flip
		Cls
		SetColor 255,255,255
		
		time:+1
		
		If showcash<s.cash
			d=(s.cash-showcash)*.05
			If d<1 d=1
			showcash:+d
			If showcash=s.cash del=time+25
			sc#=(Sin(time*18)+2)
			txt1$="You earned: "
			txt2$=" $"+String(showcash)
			scorezongo(txt1,txt2,100,3)
			playtick()
		Else
			txt1$="You earned "
			txt2$=" $"+String(showcash)
			scorezongo(txt1,txt2,100,3)
			
			If showfullhouse<fullhouses
				d=(fullhouses-showfullhouse)*.05
				If d<1 d=1
				If time>Del
					playtick()
					showfullhouse:+d
					If showfullhouse=fullhouses del=time+25
					sc#=(Sin(time*18)+2)
					txt1$="Full house bonus: "
					txt2$=" $"+String(showfullhouse)
					scorezongo(txt1,txt2,150,3)
				EndIf
			Else
				txt1$="Full house bonus: "
				txt2$=" $"+String(showfullhouse)
				scorezongo(txt1,txt2,150,3)
				
				If shownoise<noisies
					d=(noisies-shownoise)*.05
					If d<1 d=1
					If time>Del 
						shownoise:+d
						playtick()
						If shownoise=noisies del=time+25
						sc#=(Sin(time*18)+2)
						txt1$="Noise penalty: "
						txt2$="-$"+String(shownoise)
						scorezongo(txt1,txt2,200,3)
					EndIf
				Else
					txt1$="Noise penalty: "
					txt2$="-$"+String(shownoise)
					scorezongo(txt1,txt2,200,3)

					If total<>showtotal
						d=(total-showtotal)*.05
						If Abs(d)<1 d=Sgn(total-showtotal)
						If time>Del 
							playtick()
							showtotal:+d
							If showtotal=total del=time+25
							sc#=(Sin(time*18)+2)
							txt1$="TOTAL: "
							txt2$=" $"+String(showtotal)
							scorezongo(txt1,txt2,gheight-200,3)
						EndIf
					Else
						txt1$="TOTAL: "
						txt2$=" $"+String(showtotal)
						scorezongo(txt1,txt2,gheight-200,3)
						
						If MilliSecs() Mod 300<150
							SetScale 1.5,1.5
							txt$="PRESS ANY KEY TO CONTINUE"
							DrawText txt,gwidth/2-TextWidth(txt)*.75,gheight-100
							SetScale 1,1
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
		
		If anykey() out=1
	Wend
End Function

Function playtick()
	ch:TChannel=AllocChannel()
	SetChannelRate ch,3
	PlaySound tick,ch
End Function

Function endgame()
	For si:sounditem=EachIn sounditems
		StopChannel si.chan
	Next
End Function

Function scorezongo(txt1$,txt2$,y,sc#=1)
	DrawText txt1,gwidth/2-TextWidth(txt1),y-TextHeight(txt1)/2
	SetScale sc,sc
	DrawText txt2,gwidth/2,y-TextHeight(txt2)*sc/2
	SetScale 1,1
End Function

	
While 1
	initgame()
	done=rungame()
	endgame()

	Select done
	Case 1
		End
	Case 2
		showscores()
	End Select
Wend