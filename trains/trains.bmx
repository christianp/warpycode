Function linesintersect#(ax#,ay#,bx#,by#,cx#,cy#,dx#,dy#,fit=0)
	'fit, bitmask, set:
	' 1: doesn't need to be on first segment
	' 2: doesn't need to be on second segment
	bx:-ax
	by:-ay
	dx:-cx
	dy:-cy
	
	If dx<>0
		lambda#=(cy-ay+(ax-cx)*dy/dx)/(by-bx*dy/dx)
	Else
		lambda#=(cx-ax+(ay-cy)*dx/dy)/(bx-by*dx/dy)
	EndIf
	If bx<>0
		mu#=(ay-cy+(cx-ax)*by/bx)/(dy-dx*by/bx)
	Else
		mu#=(ax-cx+(cy-ay)*bx/by)/(dx-dy*bx/by)
	EndIf
	
	Rem
	Print String(ax)+"  ,  "+String(ay)
	Print String(bx)+"  ,  "+String(by)
	Print String(cx)+"  ,  "+String(cy)
	Print String(dx)+"  ,  "+String(dy)
	Print lambda
	Print mu
	WaitKey
	EndRem
	If (lambda#>=0 And lambda<=1) Or (fit & 1)
	 If (mu#>=0 And mu<=1) Or (fit & 2)
		Return lambda
	 EndIf
	EndIf
	Return -1
End Function

Function pointdistance#(px#,py#,ax#,ay#,bx#,by#)
	dx#=bx-ax
	dy#=by-ay
	an#=ATan2(dy,dx)
	nx#=Cos(an+90)
	ny#=Sin(an+90)
	lambda#=(py-ay+(ax-px)*dy/dx)/(nx*dy/dx-ny)
	Return lambda#
End Function


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
	Field points:TList
	Field minx#,maxx#,miny#,maxy#
	Field outline:TList
	Field loops:TList
	
	Method New()
		points=New TList
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
		'If Not onscreen(minx,miny,maxx,maxy) Return
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
		'SetColor 255,0,0
		size#=40
		For point=EachIn Self
			x#=point[0]
			y#=point[1]
			DrawRect x,y,1,1		
			dx#=x-ox
			dy#=y-oy
			an#=ATan2(dy,dx)+90
			x1#=x+Cos(an)*size
			y1#=y+Sin(an)*size
			x2#=x-Cos(an)*size
			y2#=y-Sin(an)*size
			If go
				poly=[ox1,oy1,ox2,oy2,x2,y2,x1,y1]
				SetAlpha .2
				'DrawPoly poly
				DrawLine ox1,oy1,x1,y1
				DrawLine ox2,oy2,x2,y2
				SetAlpha 1
				DrawLine ox,oy,x,y
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
	

	Method cansee(cx#,cy#,viewx#,viewy#)
		go=0
		Local orx#,ory#,rx#,ry#
		Local point#[2]
		tx#=cx+(viewx-cx)*.99
		ty#=cy+(viewy-cy)*.99
		For point=EachIn Self
			rx=point[0]
			ry=point[1]
			If go
				lambda#=linesintersect(tx,ty,cx,cy,orx,ory,rx,ry)
				If lambda>=0
					Return 0
				EndIf
			EndIf
			orx=rx
			ory=ry
			go=1
		Next
		Return 1
	End Method
	
	Method canseeany:TList(cx#,cy#,vpoints:TList)
		Local vpoint#[2]
		go=0
		Local orx#,ory#,rx#,ry#
		Local point#[2]
		vpoints=vpoints.copy()
		For point=EachIn Self
			rx=point[0]
			ry=point[1]
			If go
				For vpoint=EachIn vpoints
					dx#=cx-vpoint[0]
					dy#=cy-vpoint[1]
					tx1#=vpoint[0]+dx*.01
					ty1#=vpoint[1]+dy*.01
					tx2#=cx-dx*.01
					ty2#=cy-dy*.01
					lambda#=linesintersect(tx1,ty1,tx2,ty2,orx,ory,rx,ry)
					If lambda>=0
						vpoints.remove vpoint
						If vpoints.count()=0 Return vpoints
					EndIf
				Next
			EndIf
			orx=rx
			ory=ry
			go=1
		Next
		
		Return vpoints
	End Method

	Method analyse:TList()
		Local point#[2]
		Local opoint#[2]
		Local ox1#,oy1#,ox2#,oy2#,ox#,oy#
		If points.count()<2 Return

		outline:TList=New TList
		go=0
		size#=40

		loops=New TList
		For point=EachIn Self
			x#=point[0]
			y#=point[1]
			dx#=x-ox
			dy#=y-oy
			an#=ATan2(dy,dx)+90
			x1#=x+Cos(an)*size
			y1#=y+Sin(an)*size
			x2#=x-Cos(an)*size
			y2#=y-Sin(an)*size
			If go
				outlinehits(ox1,oy1,x1,y1)
			EndIf
			opoint=[x1,y1]
			outline.addlast opoint
			ox = x
			oy = y
			ox1=x1
			oy1=y1
			ox2=x2
			oy2=y2
			go=1
		Next
		
		boloops:TList=loops.copy()
		While boloops.count()
			ldl:loop=loop(boloops.removefirst())
			For l2:loop=EachIn boloops
				If l2.points.contains(ldl.points.first()) Or ldl.points.contains(l2.points.first())
					If dist(l2.x,l2.y,ldl.x,ldl.y)>10
						ldl.neighbours.addlast l2
						l2.neighbours.addlast ldl
					EndIf
				EndIf
			Next
		Wend

		
		boloops:TList=loops.copy()	
		n=0
		While boloops.count()
			ldl:loop=pickonoutline(boloops,outline)
			If ldl
				n:+1
				drawpath ldl.points
				boloops.remove ldl
				c=0
				l:TList=makepath(ldl)
				pa:TList=New TList
				For l2:loop=EachIn l
					point=[l2.x,l2.y]
					pa.addlast point
				Next
				drawpath pa
				For point=EachIn outline
					If ldl.points.contains(point)
						opoint=point
					EndIf
				Next
				outline=splicelist(outline,pa,opoint)
			Else
				boloops=New TList
			EndIf
		Wend
		outline.removefirst()
		'SetLineWidth 2
		'drawpath outline
		'SetLineWidth 1
		'DrawText n,200,100
		
		Return outline
		go=0
		Local p1#[2],p2#[2],p3#[2]
		point=Float[](outline.first())
		ox=point[0]
		oy=point[1]
		cam:TList=New TList
		For p3=EachIn outline
			If go>2
				x=(p1[0]+p2[0]+p3[0])/3
				y=(p1[1]+p2[1]+p3[1])/3
				DrawLine ox,oy,x,y
				DrawRect x,y,4,4
				point=[x,y]
				cam.addlast point
				ox=x
				oy=y
			EndIf
			p1=p2
			p2=p3
			go:+1
		Next
		Return cam
	End Method
	
	Method makepath:TList(ldl:loop,l:TList=Null)
		If Not l Then l=New TList
		l.addlast ldl
		added=0
		For l2:loop=EachIn ldl.neighbours
			If cansee(l2.x,l2.y,ldl.x,ldl.y)
				If Not l.contains(l2)
					makepath(l2,l)
					added:+1
				EndIf
			EndIf
		Next
		If added
			'l.addlast ldl
		EndIf
		Return l
	End Method
		
	Method pickonoutline:loop(boloops:TList,outline:TList)
		For ldl:loop=EachIn boloops
			If intersect(ldl.points,outline).count() Return ldl
		Next
	End Method
	
	Method outlinehits(ox#,oy#,nx#,ny#)
		Local oox#,ooy#,nox#,noy#
		Local point#[2]
		Local opoint#[2]
		Local cpoint#[2]
		ogo=0
		boutline:TList=outline.copy()
		outline=New TList
		For opoint=EachIn boutline
			nox=opoint[0]
			noy=opoint[1]
			If ogo
				lambda#=linesintersect(ox,oy,nx,ny,oox,ooy,nox,noy)
				If lambda>0 And lambda<1
					point=[ox+(nx-ox)*lambda,oy+(ny-oy)*lambda]
					outline.addlast point
					bo=0
					loopl:TList=New TList
					For cpoint=EachIn boutline
						If bo
							'DrawLine cpoint[0],cpoint[1],800,800
							loopl.addlast cpoint
						Else
							If opoint=cpoint
								bo=1
								loopl.addlast point
							EndIf
						EndIf
					Next
					loopl.addlast point
					loops.addlast loop.Create(loopl)
					Return
				EndIf
			EndIf
			outline.addlast opoint
			oox=nox
			ooy=noy
			ogo=1
		Next
	End Method
	
End Type

Type loop
	Field points:TList
	Field x#,y#
	Field neighbours:TList
	
	Method New()
		neighbours=New TList
	End Method
	
	Function Create:loop(points:TList)
		ldl:loop=New loop
		ldl.points=points
		Local point#[2]
		tx#=0
		ty#=0
		For point=EachIn points
			tx:+point[0]
			ty:+point[1]
		Next
		tx:/points.count()
		ty:/points.count()
		ldl.x=tx
		ldl.y=ty
		Return ldl
	End Function
End Type

Type train
	Field rail:pathenumerator
	Field orail#[],nrail#[2]
	Field x#,y#
	Field ox#,oy#
	Field cam:TList
	Field camx#,camy#,camvx#,camvy#
	Field caman#
	Field tcamx#,tcamy#
	Field t#
	Field done
	Field p:path
	Field rpoints:TList
	Field camstack:TList
	Field ocam:TList
	Field camp1#[2],camp2#[2]
	
	Method New()
		camstack=New TList
	End Method
	
	Function Create:train(p:path)
		t:train=New train
		t.p=p
		t.rail=p.objectenumerator()
		t.cam=p.analyse()
		t.ocam=t.cam
		t.t=0
		t.orail=t.nextrail()
		t.nrail=t.nextrail()
		Local point#[2]
		point=Float[](t.cam.first())
		t.camx=point[0]
		t.camy=point[1]
		t.rpoints=New TList
		For point=EachIn p
			t.rpoints.addlast point
		Next
		
		t.update
		Return t
	End Function
	
	Method nextrail#[]()
		Local point#[2]
		If rail.hasnext()
			point=Float[](rail.nextobject())
			Return point
		Else
			done=1
		EndIf
	End Method
	
	Method update()
		ox=x
		oy=y
		x#=orail[0]*(1-t)+nrail[0]*t
		y#=orail[1]*(1-t)+nrail[1]*t
		
		
		
		
		dx#=nrail[0]-orail[0]
		dy#=nrail[1]-orail[1]
		d#=Sqr(dx*dx+dy*dy)
		t:+.5/d
		If t>=1
			orail=nrail
			nrail=nextrail()
			t:-1
		EndIf
	End Method
	
	Method updatecam()

		camvx:*.9
		camvy:*.9
		camx:+camvx
		camy:+camvy
		
		caman:+andiff(ATan2(y-camy,x-camx),caman)*.1
		
		If Not cam.count() Return

		Local point#[],point2#[],opoint#[]
		
		Global destx#,desty#
		destx=x
		desty=y
		
		Function distcmp(o1:Object,o2:Object)
			Local point1#[2],point2#[2]
			point1=Float[](o1)
			point2=Float[](o2)
			d1#=dist(point1[0],point1[1],destx,desty)
			d2#=dist(point2[0],point2[1],destx,desty)
			If d1<d2
				Return -1
			Else
				Return 1
			EndIf
		End Function
		
		If Not (facingline(x,y,camp1[0],camp1[1],camp2[0],camp2[1]) And cansee(camx,camy,x,y))
			'seen:TList=intersect(canseeany(x,y,cam),canseeany(camx,camy,cam))
			seen:TList=canseeany(camx,camy,cam)
			If seen.count()>=2
				seen.sort(True,distcmp)
				Local p1#[2],p2#[2]
				camp1=Float[](seen.removefirst())
				camp2=Float[](seen.removefirst())
			'ElseIf seen.count()
			'	camp1=Float[](seen.removefirst())
			'	camp2=camp1
			EndIf
		EndIf
		If camp1=camp2
			tcamx=camp1[0]
			tcamy=camp1[1]
		Else
			point=lineclosestpoint(x,y,camp1[0],camp1[1],camp2[0],camp2[1])
			tcamx=point[0]
			tcamy=point[1]
		EndIf
		SetLineWidth 2
		'DrawLine camp1[0],camp1[1],camp2[0],camp2[1]
		SetLineWidth 1
		'DrawLine camp1[0],camp1[1],0,0
		'DrawLine 0,0,camp2[0],camp2[1]
		
		'Return
		
		'DrawLine camx,camy,tcamx,tcamy
		dx#=tcamx-camx
		dy#=tcamy-camy
		d#=Sqr(dx*dx+dy*dy)
		dx:/d
		dy:/d
		If d<40
			camvx:*d/40
			camvy:*d/40
		EndIf
		If d<>0 And d>5
			speed#=.3
			camvx:+dx*speed
			camvy:+dy*speed
		EndIf
	End Method
	
	Method pickstack:TList()
		Local point#[],point2#[],opoint#[]
		stack:TList=New TList
		
		seen:TList=canseeany(x,y,cam)
		If seen.count()
			point=Float[](cam.first())
			If cansee(point[0],point[1],x,y)
				mindist#=-1
				For point=EachIn seen
					If cansee(point[0],point[1],camx,camy)
						d#=dist(point[0],point[1],x,y)
						If d<mindist Or mindist=-1
							mindist=d
							point2=point
						EndIf
					EndIf
				Next
				trimcam point2
				stack.addlast point2
			Else
				cam.removefirst
				stack.addlast point
				While Not cansee(point[0],point[1],x,y)
					point=Float[](cam.removefirst())
					stack.addlast point
				Wend
			EndIf
		Else
			Return stack
			closest:loop=Null
			mindist#=-1
			For ldl:loop=EachIn p.loops
				If cansee(ldl.x,ldl.y,x,y)
					d#=dist(ldl.x,ldl.y,x,y)
					If d<mindist Or mindist=-1
						mindist=d
						closest=ldl
					EndIf
				EndIf
			Next
			point=[closest.x,closest.y]
			stack.addlast point
		EndIf
		
		If Not stack.count()
			'cam=ocam
			'Return pickstack()
		EndIf
		
		Return stack
	End Method
		
	Method trimcam(dpoint#[])
		Local point#[]
		n=0
		For point=EachIn cam
			If dpoint=point
				Return
			EndIf
			cam.remove point
			n:+1
		Next
	End Method
	
		
	Method cansee(cx#,cy#,viewx#,viewy#)
		go=0
		Local orx#,ory#,rx#,ry#
		Local point#[2]
		tx#=cx+(viewx-cx)*.99
		ty#=cy+(viewy-cy)*.99
		For point=EachIn rpoints
			rx=point[0]
			ry=point[1]
			If go
				lambda#=linesintersect(tx,ty,cx,cy,orx,ory,rx,ry)
				If lambda>=0
					Return 0
				EndIf
			EndIf
			orx=rx
			ory=ry
			go=1
		Next
		Return 1
	End Method
	
	Method canseeany:TList(cx#,cy#,vpoints:TList)
		Local vpoint#[2]
		go=0
		Local orx#,ory#,rx#,ry#
		Local point#[2]
		vpoints=vpoints.copy()
		For point=EachIn rpoints
			rx=point[0]
			ry=point[1]
			If go
				For vpoint=EachIn vpoints
					dx#=cx-vpoint[0]
					dy#=cy-vpoint[1]
					tx1#=vpoint[0]+dx*.01
					ty1#=vpoint[1]+dy*.01
					tx2#=cx-dx*.01
					ty2#=cy-dy*.01
					lambda#=linesintersect(tx1,ty1,tx2,ty2,orx,ory,rx,ry)
					If lambda>=0
						vpoints.remove vpoint
						If vpoints.count()=0 Return vpoints
					EndIf
				Next
			EndIf
			orx=rx
			ory=ry
			go=1
		Next
		
		Return vpoints
	End Method
		
	Method anycanseeany:TList(spoints:TList,vpoints:TList)
		Local spoint#[2],vpoint#[2]
		olist:TList=New TList
		vpoints=vpoints.copy()
		For spoint=EachIn spoints
			viewed:TList=canseeany(spoint[0],spoint[1],vpoints)
			For vpoint=EachIn viewed
				vpoints.remove vpoint
				olist.addlast vpoint
			Next
		Next
		Return olist
	End Method
	
	Method draw()
		an#=ATan2(y-oy,x-ox)
		SetRotation an+90
		SetColor 0,0,255
		DrawRect x,y,10,20
		SetRotation 0
		
		DrawText camstack.count(),0,100

		If cansee(camx,camy,x,y)
			SetColor 255,255,255
		Else
			SetColor 255,0,0
		EndIf
		
		SetLineWidth 2
		drawpath cam
		SetLineWidth 1
		
		DrawRect camx-5,camy-5,10,10
		
		SetColor 255,255,255
	End Method
End Type

Function drawpath(points:TList)
	go=0
	Local ox#,oy#
	Local point#[2]
	For point=EachIn points
		x#=point[0]
		y#=point[1]
		If go
			DrawLine ox,oy,x,y
		EndIf
		ox=x
		oy=y
		go=1
	Next
End Function



Function renderpath(p:path,camx#,camy#,caman#)
	SetColor 255,0,0
	DrawLine camx,camy,camx+Cos(caman)*100,camy+Sin(caman)*100
	go=0
	Local point#[2]
	Local ox#,oy#,opx#,olambda#
	plines:TList=New TList
	n=0
	For point=EachIn p
		x#=point[0]
		y#=point[1]
			dx#=Cos(caman)
			dy#=Sin(caman)
			nx#=Cos(caman+90)
			ny#=Sin(caman+90)
			mu#=(y-camy+(camx-x)*dy/dx)/(ny-nx*dy/dx)
			lambda#=(x-camx-mu*nx)/dx
			px#=10*mu/(lambda+200)
			an#=ATan2(y-camy,x-camx)
			'px#=10*andiff(an,caman)/(lambda+200)
			SetColor 255,255,255
			'DrawText Int(lambda),x,y
			'DrawText Int(mu),x,y+15
			'DrawLine camx,camy,camx+lambda*dx+mu*nx,camy+lambda*dy+mu*ny
			n:+1
		If go And (lambda>0 Or olambda>0)
			pl:projline=projline.Create(opx,px,(lambda+olambda)/2)
			plines.addlast pl
			dx#=x-ox
			dy#=y-oy
			an#=ATan2(dy,dx)-90
			dan#=an-caman
			pl.shade#=Cos(dan)
			pl.shade=50.0/((lambda+olambda)/2)
			'If pl.shade>1 pl.shade=1
			pl.r=Cos(n*10)*100+155
			pl.g=Sin(n*20)*100+155
			pl.b=Cos(n*10+90)*100+155
			SetColor pl.shade*pl.r,pl.shade*pl.g,pl.shade*pl.b
			SetLineWidth 3
			DrawLine ox,oy,x,y
		EndIf
		ox#=x
		oy#=y
		opx=px
		olambda=lambda
		go=1
	Next
	
	Function plcmp(o1:Object,o2:Object)
		pl1:projline=projline(o1)
		pl2:projline=projline(o2)
		
		If pl1.dist<pl2.dist
			Return -1
		Else
			Return 1
		EndIf
	End Function
	
	plines.sort(False,plcmp)
	
	For pl:projline=EachIn plines
			SetColor pl.shade*pl.r,pl.shade*pl.g,pl.shade*pl.b
			x1#=400+200*pl.x1
			x2#=400+200*pl.x2
			If x1>x2
				a#=x1
				x1=x2
				x2=a
			EndIf
			y=400-(1-pl.shade)*100
			
			DrawRect x1,y,x2-x1,pl.shade*100
			SetLineWidth 1
	Next
End Function

Type projline
	Field x1#,x2#
	Field dist#
	Field shade#
	Field r,g,b
	
	Function Create:projline(x1#,x2#,dist#)
		pl:projline=New projline
		pl.x1=x1
		pl.x2=x2
		pl.dist=dist
		Return pl
	End Function
End Type


Function andiff#(an1#,an2#)
	dan#=(an1-an2) Mod 360
	If dan>180 dan:-360
	If dan<-180 dan:+360
	Return dan
End Function


Function lineclosestpoint#[](ox#,oy#,x1#,y1#,x2#,y2#)
	dx#=x2-x1
	dy#=y2-y1
	d#=Sqr(dx*dx+dy*dy)
	dx:/d
	dy:/d
	an#=ATan2(dy,dx)
	nx#=Cos(an+90)
	ny#=Sin(an+90)
	lambda#=(oy-y1+(x1-ox)*dy/dx)/(nx*dy/dx-ny)

	If dx<>0
		ix#=ox+lambda*nx
		mu#=(ix-x1)/dx
	Else
		iy#=oy+lambda*ny
		mu#=(iy-y1)/dy
	EndIf
	
	Local point#[2]
	If mu<0
		point=[x1,y1]
	ElseIf mu>d
		point=[x2,y2]
	Else
		x#=x1+mu*dx
		y#=y1+mu*dy
		point=[x,y]
	EndIf
	Return point
End Function

Function facingline(ox#,oy#,x1#,y1#,x2#,y2#)
	If x1=x2 And y1=y2 Return 0
	dx#=x2-x1
	dy#=y2-y1
	d#=Sqr(dx*dx+dy*dy)
	dx:/d
	dy:/d
	an#=ATan2(dy,dx)
	nx#=Cos(an+90)
	ny#=Sin(an+90)
	lambda#=(oy-y1+(x1-ox)*dy/dx)/(nx*dy/dx-ny)

	If dx<>0
		ix#=ox+lambda*nx
		mu#=(ix-x1)/dx
	Else
		iy#=oy+lambda*ny
		mu#=(iy-y1)/dy
	EndIf
	
	If mu<0 Or mu>d
		Return 0
	Else
		Return 1
	EndIf
End Function

Function dist#(x1#,y1#,x2#,y2#)
	dx#=x2-x1
	dy#=y2-y1
	d#=Sqr(dx*dx+dy*dy)
	Return d
End Function

Function intersect:TList(l1:TList,l2:TList)
	ol:TList=New TList
	For o:Object=EachIn l1
		If l2.contains(o) ol.addlast o
	Next
	Return ol
End Function

Function union:TList(l1:TList,l2:TList)
	ol:TList=l1.copy()
	For o:Object=EachIn l2
		If Not ol.contains(o) ol.addlast o
	Next
	Return ol
End Function

Function splicelist:TList(l1:TList,l2:TList,o:Object)
	l:TList=New TList
	For lo:Object=EachIn l1
		l.addlast lo
		If lo=o
			For lo2:Object=EachIn l2
				l.addlast lo2
			Next
			l.addlast lo
		EndIf
	Next
	Return l
End Function

Graphics 800,800,0
SetBlend ALPHABLEND

p:path=New path

While Not (KeyHit(KEY_ENTER) And p.points.count()>=2)
	If MouseHit(1) p.addpoint(MouseX(),MouseY())
	If MouseHit(2) p=New path
	
	DrawText "Draw a railway by clicking points",0,0
	DrawText "don't make it overlap or anything",0,15
	DrawText "I'm not that clever",0,30
	
	
	p.draw

	p.analyse
	
	If (KeyHit(KEY_ESCAPE) Or AppTerminate())
		End
	EndIf
	
	Flip
	Cls
Wend

t:train=train.Create(p)
oldms=MilliSecs()
While 1
	SetColor 255,255,255
	DrawText "Press space for choo-choo",0,0

	If t.done
		t=train.Create(p)
	EndIf
	
	If KeyDown(KEY_SPACE)
		t.update
	EndIf
	t.updatecam
	
	Local point#[2]
	'For point=EachIn p
	'	If t.cansee(t.camx,t.camy,point[0],point[1])
	'		DrawRect point[0],point[1],5,5
	'	EndIf
	'Next
	p.draw
	
	renderpath p,t.camx,t.camy,t.caman
	'p.analyse

	Rem	
	DrawText p.loops.count(),400,15
	SetColor 255,255,255
	n=0
	For ldl:loop=EachIn p.loops
		Local ox#,oy#
		go=0
		n:+1
		DrawText n,350,15+n*15
		DrawText n,ldl.x,ldl.y
		po=0
		For l2:loop=EachIn ldl.neighbours
			DrawLine ldl.x,ldl.y,l2.x,l2.y
			po:+1
		Next
		DrawText po,400,15+n*15
		For point=EachIn ldl.points
			x#=point[0]
			y#=point[1]
			If go
				SetLineWidth 3
				DrawLine ox,oy,x,y
				SetLineWidth 1
			EndIf
			ox=x
			oy=y
			go=1
		Next
	Next
	EndRem
	
	t.draw
	
	ms=MilliSecs()
	DrawText 1000.0/(ms-oldms),0,780
	oldms=ms
	

	If (KeyHit(KEY_ESCAPE) Or AppTerminate())
		End
	EndIf
	
	Flip
	Cls
Wend