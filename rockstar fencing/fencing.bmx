Global gwidth:Float, gheight:Float
Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom
Global tpanx:Float = 0, tpany:Float = 0
Global yellowpaper:TImage
Global bluepaper:TImage
Global brownpaper:TImage
Global greenpaper:TImage

Global sounditems:TList = New TList
SetAudioDriver "DirectSound"

Type sounditem
	Field chan:TChannel
	Field loops
	Field sound:TSound
	Field rate:Float
	
	Function Create:sounditem(sound:TSound,rate#,pan#,volume#,loops=0)
		si:sounditem=New sounditem
		si.chan = AllocChannel() 
		si.rate = rate
		SetChannelRate(si.chan,rate)
		SetChannelPan(si.chan,pan)
		SetChannelVolume si.chan , volume
		si.sound = sound
		si.loops=loops
		PlaySound(si.sound,si.chan)
		sounditems.AddLast si
		Return si
	End Function
	
	Method cease() 
		sounditems.Remove Self
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
		SetChannelRate chan, timezoom * rate
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
		l.ny=Sin(l.an-90)
		
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
		
		Local hit:Float
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
	dan:Float = (an1 - an2) Mod 360
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

Function DrawZoomTexturedPoly(image:TImage, poly:Float[] ) 
	poly = poly[..] 
	While i < Len(poly)
		poly[i] = ZoomX(poly[i] ) 
		poly[i + 1] = zoomy(poly[i + 1]) 
		i:+4
	Wend
	DrawTexturedPoly image, poly
End Function

Function DrawZoomLine(ax# , ay# , bx# , by#)
	ax = zoomx(ax)
	ay = zoomy(ay)
	bx = zoomx(bx)
	by = zoomy(by)
	DrawLine ax,ay,bx,by
End Function

Function DrawZoomRect(x:Float, y:Float, width:Float, height:Float, zoomdimensions = 1, filled = 1) 
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





Function DrawTexturedPoly(image:TImage, xyuv:Float[] , frame = 0, vertex = -1) 
	Local handle_x#,  handle_y#
	GetHandle handle_x#,  handle_y#
	Local origin_x#,  origin_y#
	GetOrigin origin_x#,  origin_y#	
	
	
	Assert Image, "Image not found"
	
	Local  OGLDriver:TGLMax2DDriver = TGLMax2DDriver(_max2dDriver)
	If OGLDriver Then
			DrawTexturedPolyOGL ..
				OGLDriver,..
				 TGLImageFrame(image.Frame(frame)), ..
				 xyuv, handle_x, handle_y, origin_x,origin_y,  vertex*4
		Return
	End If
End Function



Function DrawTexturedPolyOGL (Driver:TGLMax2DDriver, Frame:TGLImageFrame, xy#[],handle_x#,handle_y#,origin_x#,origin_y#, vertex) 
	Private
	Global TmpImage:TImage
	Public
	
	If xy.length<6 Return
	
	Local rot#  = GetRotation()
	Local tform_scale_x#, tform_scale_y#
	GetScale tform_scale_x, tform_scale_y
	
	Local s#=Sin(rot)
	Local c#=Cos(rot)
	Local ix= c*tform_scale_x
	Local iy=-s*tform_scale_y
	Local jx= s*tform_scale_x
	Local jy= c*tform_scale_y
	
	glBindTexture GL_TEXTURE_2D, Frame.name
	glEnable GL_TEXTURE_2D
	
	glBegin GL_POLYGON
	For Local i=0 Until Len xy Step 4
		If vertex > -1 And i >= vertex Then Exit
		Local x#=xy[i+0]+handle_x
		Local y#=xy[i+1]+handle_y
		Local u#=xy[i+2]
		Local v#=xy[i+3]
		glTexCoord2f u,v
		glVertex2f x*ix+y*iy+origin_x,x*jx+y*jy+origin_y
	Next
	glEnd
	If Not tmpImage Then tmpImage = CreateImage(1,1)
	DrawImage tmpImage, -100, - 100 ' Chtob zbit' flag texturi
End Function








'---------------------------------------------------------------------------
Type skeleton
	Field joints:TList, bones:TList
	Field lfoot:joint, lknee:joint, rfoot:joint, rknee:joint
	Field pelvis:joint, topspine:joint, lelbow:joint, relbow:joint, lhand:joint, rhand:joint
	Field size:Float
	Field tlfoot:Float, trfoot:Float, slfoot:Float, srfoot:Float, stept:Float
	Field lastrfoot, lastlfoot
	Field vertices:TList
	Field tris:TList
	Field stance
	Field sabre:sword
	Field bodytex:TImage, legtex:TImage, armtex:TImage
	Field dir
	Field advance:Float
	
	Method New() 
		joints = New TList
		bones = New TList
		vertices = New TList
		tris = New TList
		skeletons.AddLast Self
	End Method
	
	Function Create:skeleton(x:Float, y:Float, dir, size, bodytex:TImage, legtex:TImage, armtex:TImage) 
		s:skeleton = New skeleton
		s.size = size
		s.bodytex = bodytex
		s.legtex = legtex
		s.armtex = armtex
		s.dir = dir
		s.makebones() 

		For j:joint = EachIn s.joints
			j.x:+x
			j.y:+y
		Next
		
		Return s
	End Function
	
	Method makebones() 
		lfoot:joint = addjoint(0, 0, 1) 
		lknee:joint = addjoint(0, - 2) 
		rfoot:joint = addjoint(0, 0, 1) 
		rknee:joint = addjoint(0, - 2) 
		pelvis:joint = addjoint(0, - 4) 
		topspine:joint = addjoint(0, - 7, 0) 
		lelbow:joint = addjoint(2, topspine.y / size) 
		lhand:joint = addjoint(4, topspine.y / size) 
		relbow:joint = addjoint(- 2, topspine.y / size) 
		rhand:joint = addjoint(- 4, topspine.y / size) 
		
		llowleg:bone = addbone(lknee, lfoot, 2) 
		lupleg:bone = addbone(pelvis, lknee, 2) 
		rlowleg:bone = addbone(rknee, rfoot, 2) 
		rupleg:bone = addbone(pelvis, rknee, 2) 
		spine:bone = addbone(pelvis, topspine, 3) 
		luparm:bone = addbone(topspine, lelbow, 2) 
		lforearm:bone = addbone(lelbow, lhand, 2) 
		ruparm:bone = addbone(topspine, relbow, 2) 
		rforearm:bone = addbone(relbow, rhand, 2) 
		
		addbonevertex(rforearm, 1, .1)  'right hand
		addbonevertex(rforearm, 1, -.1) 
		addhingevertex(topspine, relbow, rhand, .25 * size)  	'right elbow
		addhingevertex(topspine, relbow, rhand, -.25 * size) 
		'addhingevertex(relbow, topspine, lelbow, -.35 * size) 	'neck
		addbonevertex(spine, 1.1, 0) 
		'addhingevertex(pelvis, topspine, relbow, -.65 * size)  	'right chest
		addbonevertex(spine, .9, -.2) 								'right chest
		addbonevertex(spine, 1, 0)   							'unused
		'bongo = addhingevertex(pelvis, topspine, lelbow, .65 * size, 0)        	'left chest
		addbonevertex(spine, .9, .2)  							'left chest
		addhingevertex(topspine, lelbow, lhand, -.25 * size)   	'left elbow
		addhingevertex(topspine, lelbow, lhand, .25 * size) 
		addbonevertex(lforearm, 1, -.1)  						'left hand
		addbonevertex(lforearm, 1, .1) 
		addhingevertex(topspine, pelvis, rknee, .35 * size)   	'right hip
		addhingevertex(topspine, pelvis, lknee, -.35 * size)  	'left hip
		addhingevertex(pelvis, rknee, rfoot, .25 * size)  		'right knee
		addhingevertex(pelvis, rknee, rfoot, -.25 * size) 
		addhingevertex(pelvis, lknee, lfoot, .25 * size)  		'left knee
		addhingevertex(pelvis, lknee, lfoot, -.25 * size) 
		addbonevertex(rlowleg, 1, .1)  							'right foot
		addbonevertex(rlowleg, 1, -.1) 
		addbonevertex(llowleg, 1, .1)  							'left foot
		addbonevertex(llowleg, 1, -.1) 
		addhingevertex(rknee, pelvis, lknee, .4 * size)  		'crotch
		addbonevertex(luparm, 0.2, .2)       					'left shoulder blade
		addbonevertex(ruparm, 0.2, -.2)  						'right shoulder blade
		addbonevertex(lforearm, .1, -.12)     					'left elbow forearm
		addbonevertex(lforearm, .1, .12) 
		addbonevertex(rforearm, .1, .12)  						'right elbow forearm
		addbonevertex(rforearm, .1, -.12) 
		
		addtri(0, 1, 28, brownpaper)         		'right forearm
		addtri(0, 28, 27, brownpaper) 
		addtri(27, 28, 2, brownpaper)     	 	'right elbow
		addtri(28, 2, 3, brownpaper) 
		addtri(2, 3, 24, brownpaper)      		'right upper arm
		addtri(2, 24, 4, brownpaper) 
		addtri(4, 5, 24, brownpaper)    			'right shoulder

		addtri(12, 14, 15, yellowpaper)        	'right upper leg
		addtri(12, 15, 22, yellowpaper) 
		addtri(14, 18, 19, yellowpaper)    		'right lower leg
		addtri(14, 19, 15, yellowpaper) 

		addtri(5, 12, 13, yellowpaper)        	'torso
		addtri(5, 13, 7, yellowpaper) 
		addtri(12, 22, 13, yellowpaper)      	'pelvis
		addtri(4, 5, 7, yellowpaper)       		'collar
		
		addtri(13, 22, 16, yellowpaper)      	'left upper leg
		addtri(13, 16, 17, yellowpaper) 
		addtri(16, 20, 17, yellowpaper)      	'left lower leg
		addtri(17, 20, 21, yellowpaper) 

		addtri(4, 23, 7, brownpaper)        		'Left shoulder
		addtri(4, 23, 8, brownpaper)       		'left upper arm
		addtri(23, 9, 8, brownpaper) 
		addtri(8, 9, 25, brownpaper)    			'left elbow
		addtri(9, 25, 26, brownpaper) 
		addtri(25, 26, 10, brownpaper)     		'left forearm
		addtri(26, 10, 11, brownpaper) 

		sabre = sword.Create(Self, lforearm) 
	End Method
		
	Method addjoint:joint(bx:Float, by:Float, fixed = 0) 
		j:joint = joint.Create(bx * size * dir, by * size, fixed) 
		joints.AddLast j
		Return j
	End Method
	
	Method addbone:bone(j1:joint, j2:joint, length:Float) 
		b:bone = bone.Create(j1, j2, length * size) 
		bones.AddLast b
		Return b
	End Method

	Method addbonevertex:texvertex(b:bone, d:Float, n:Float) 
		v:bonevertex = bonevertex.Create(b, d, n * dir) 
		vertices.AddLast v
		Return v
	End Method
	
	Method addhingevertex:hingevertex(j1:joint, j2:joint, j3:joint, n:Float, switchover = 1, switchunder = 1) 
		v:hingevertex = hingevertex.Create(j1, j2, j3, n * dir, switchover, switchunder)   'need *dir?
		vertices.AddLast v
		Return v
	End Method
	
	Method addtri:textri(a, b, c, image:TImage) 
		Local verts:Object[] = vertices.ToArray() 
		t:textri = textri.Create(image, texvertex(verts[a] ) , texvertex(verts[b] ) , texvertex(verts[c] )) 
		tris.AddLast t
		't.cuts.addlast cut.Create(.5, 0, 0,.5) 
		Return t
	End Method
	
	Method update(delta:Float) 
		For j:joint = EachIn joints
			j.update(delta) 
		Next
		
		ax:Float = lhand.x - 2 * size * dir + advance
		ay:Float = lhand.y - 1 * size
		DrawZoomRect ax - 2, floory, 4, 5
		topspine.x:+(ax + pelvis.x - 2 * topspine.x) *.2 * delta
		topspine.y:+(ay - topspine.y) *.1 * delta
		pelvis.x:+(topspine.x - pelvis.x) *.05 * delta
		pelvis.y:+(floory - size * 4 - pelvis.y) *.2 * delta
		If (lhand.x - topspine.x) * dir > 0
			rhand.x:+(ax - size * dir - rhand.x) *.1 * delta
			rhand.y:+(ay - size - rhand.y) *.1 * delta
		Else
			rhand.y:+(topspine.y + size * 2 * dir - rhand.y) *.05 * delta
		EndIf
		
		Select stance
		Case 0 'free to move
			If (ax - rfoot.x) * dir < 0 Or (ax - lfoot.x) * dir > 0
			'DrawZoomLine topspine.x, topspine.y, ax, floory
				If rfoot.fixed 'And ms - lastrfoot > 300 'And rfoot.x < lfoot.x
					trfoot:Float = ax
					'If Abs(trfoot - lfoot.x) < size * 2 trfoot = lfoot.x - size
					maxrfoot:Float = lfoot.x - size * dir
					If (trfoot - maxrfoot) * dir > 0 trfoot = maxrfoot
					If Abs(trfoot - rfoot.x) > size * 5 trfoot = rfoot.x + Sgn(trfoot - rfoot.x) * size * 5
					If Abs(rfoot.x - trfoot) > size * 3 And lfoot.fixed
						srfoot = rfoot.x
						stept = 0
						rfoot.fixed = 0
					EndIf
				EndIf
				If lfoot.fixed 'And ms - lastlfoot > 300 'And lfoot.x >= rfoot.x
					tlfoot:Float = ax + size * 2 * dir
					'tlfoot = lfoot.x + (ax - lfoot.x) * size * 3
					If Abs(tlfoot - lfoot.x) > size * 5 tlfoot = lfoot.x + Sgn(tlfoot - lfoot.x) * size * 5
					'DrawZoomLine lfoot.x, floory + 5, tlfoot, floory + 5
					If Abs(lfoot.x - tlfoot) > size * 3 And rfoot.fixed
						slfoot = lfoot.x
						stept = 0
						lfoot.fixed = 0
					EndIf
				EndIf
			EndIf
		Case 1 'standing ground
		End Select
		
		If Not rfoot.fixed
			'SetColor 255, 0, 0
			'DrawZoomCircle rfoot.x, rfoot.y, 10
			'DrawZoomCircle trfoot, floory, 5
			'SetColor 255, 255, 255
			rfoot.x = srfoot + (trfoot - srfoot) * stept
			rfoot.y = floory - stept * (1 - stept) * size * 4
			stept:+.1 * delta
			If stept >= 1
				rfoot.fixed = 1
				rfoot.y = floory
				lastrfoot = ms
				footsound() 
			EndIf
		EndIf
		
		If Not lfoot.fixed
			'SetColor 0, 255, 0
			'DrawZoomCircle lfoot.x, lfoot.y, 10
			'DrawZoomCircle tlfoot, floory, 5
			'SetColor 255, 255, 255
			lfoot.x = slfoot + (tlfoot - slfoot) * stept
			lfoot.y = floory - stept * (1 - stept) * size * 4
			stept:+.1 * delta
			If stept >= 1
				lfoot.fixed = 1
				lfoot.y = floory
				lastlfoot = ms
				footsound() 
			EndIf
		EndIf

		For i = 1 To 5
			For b:bone = EachIn bones
				b.constrain() 
			Next
			
			lfoot.boundary(floory) 
			rfoot.boundary(floorx) 
			hinge(lelbow, lhand.x, lhand.y, 0, 1) 
			hinge(relbow, rhand.x, rhand.y, - 1, 1) 
			hinge(lknee, lfoot.x, lfoot.y, 1, - 1) 
			hinge(rknee, rfoot.x, rfoot.y, 1, - 1) 
			hinge(topspine, pelvis.x, pelvis.y, 0, - 1) 
	
		Next
		
		For v:texvertex = EachIn vertices
			v.update(delta) 
		Next
		
		For t:textri = EachIn tris
			t.update(delta) 
		Next
		
		For b:bone = EachIn bones
			b.update(delta) 
		Next
		
		sabre.update(delta) 
	End Method
	
	Method footsound() 
		sounditem.Create(footsteps[Rand(0, numfootsteps - 1)] , 1, 0, 1) 
	End Method
	
	Method hinge(j:joint, x:Float, y:Float, xd, yd) 
		j.hinge(x, y, xd * dir, yd) 
	End Method
	
	Method control() 
		tx:Float = UnzoomX(MouseX()) 
		ty:Float = UnzoomY(MouseY()) 
		If ty > floory ty = floory
		dx:Float = tx - topspine.x
		dy = ty - topspine.y
		d = dx * dx + dy * dy
		If d > size * size * 16
			d = Sqr(d) 
			dx:*size * 4 / d
			dy:*size * 4 / d
			tx = topspine.x + dx
			ty = topspine.y + dy
		EndIf
		lhand.x = tx
		lhand.y = ty
		If MouseDown(1) 
			advance = -size * 5 * dir
		Else
			advance = 0
		EndIf
		
		If MouseDown(2) 
			timezoom:+(.1 - timezoom) *.02
		Else
			timezoom:+(1 - timezoom) *.1
		EndIf

		tpanx = lhand.x
		tpany = topspine.y
		
	End Method
	
	Method draw() 
	
		For v:texvertex = EachIn vertices
			'v.draw() 
		Next
	
		For b:bone = EachIn bones
			b.draw() 
		Next
		
		For j:joint = EachIn joints
			'j.draw() 
		Next
		
		For t:textri = EachIn tris
			t.draw() 
		Next
		
		sabre.draw() 
	End Method
End Type

Type sword
	Field x:Float, y:Float
	Field ex:Float, ey:Float, oex:Float, oey:Float
	Field an:Float, van:Float
	Field length:Float
	Field b:bone
	Field ensound:sounditem
	Field owner:skeleton
	Field contact:sword
	Field myclang:sounditem, myrub:sounditem
	Field rubrate:Float, rubv:Float
	
	Function Create:sword(owner:skeleton, b:bone, length:Float = 90) 
		r:sword = New sword
		r.owner = owner
		r.b = b
		r.length = length
		Return r
	End Function
	
	Method update(delta:Float) 
		ox:Float = x
		oy:Float = y
		x = b.j2.x
		y = b.j2.y
		f:Float = nicean(b.an * 7 / 6 + 15 - an) 
		If contact f:*.1
		oex = ex
		oey = ey
		an1:Float = ATan2(oey - y, oex - x) 
		dan:Float = nicean(an - an1) 
		van:+dan
		van:+f *.25 * delta
		van:*(1 -.4 * delta) 
		an:+van * delta
		ex = x + Cos(an) * length
		ey = y + Sin(an) * length
		
		'swoosh noise
		If ensound
			If Not ChannelPlaying(ensound.chan) 
				ensound = Null
			End If
		Else
			If Abs(van) > 15
				i = Rand(0, numswooshes - 1) 
				v:Float = (Abs(van) - 15) *.1
				ensound = sounditem.Create(swooshes[i] , 1, 0, v) 
			End If
		End If
		
		'clanging
		If myclang
			If Not ChannelPlaying(myclang.chan) 
				myclang = Null
			End If
		End If
		sl:line = line.Create(x, y, ex, ey) 
		ocontact:sword = contact
		contact = Null
		For s:skeleton = EachIn skeletons
		If s <> owner
			l:line = line.Create(s.sabre.x, s.sabre.y, s.sabre.ex, s.sabre.ey) 
			mu:Float = sl.intersect(l) / length
			If mu >= 0
				omx:Float = ox + mu * (oex - ox) 
				omy:Float = oy + mu * (oey - oy) 
				mx:Float = x + mu * (ex - x) 
				my:Float = y + mu * (ey - y) 
				vx:Float = mx - omx
				vy:Float = my - omy
				v:Float = Sqr(vx * vx + vy * vy) 
				dp:Float = vx * l.nx + vy * l.ny
				If owner = fb DrawZoomText dp, mx, my
				lambda:Float = l.intersect(sl) / s.sabre.length
				f:Float = dp *.9
				s.sabre.van:-f
				contact = s.sabre
				s.sabre.contact = Self
				If Not ocontact
					If Abs(f) > 1
						clang(f) 
					EndIf
				Else
					rub(Abs(f), mu * lambda) 
				End If
			EndIf
		EndIf
		Next
		If contact = Null
			If myrub
				rub(0, 0) 
				If rubv <.01
					myrub.cease
					myrub = Null
				EndIf
			EndIf
		End If
		
		'cutting
		sl:line = line.Create(oex, oey, ex, ey) 
		For s:skeleton = EachIn skeletons
		If s <> owner
		
		
			'cutting
			For t:textri = EachIn s.tris
				l1:line = line.Create(t.v1.x, t.v1.y, t.v2.x, t.v2.y) 
				i1:Float = sl.intersect(l1) 
				l2:line = line.Create(t.v2.x, t.v2.y, t.v3.x, t.v3.y) 
				i2:Float = sl.intersect(l2) 
				l3:line = line.Create(t.v3.x, t.v3.y, t.v1.x, t.v1.y) 
				i3:Float = sl.intersect(l3) 
				
				mini:Float = -1
				maxi:Float = 1
				If i1 >= 0
					If i1 < mini Or mini = -1 mini = i1
					If i1 > maxi Or maxi = 1 maxi = i1
				End If
				If i2 >= 0
					If i2 < mini Or mini = -1 mini = i2
					If i2 > maxi Or maxi = 1 maxi = i2
				End If
				If i3 >= 0
					If i3 < mini Or mini = -1 mini = i3
					If i3 > maxi Or maxi = 1 maxi = i3
				End If
				
				If pointintriangle(oex, oey, t.v1.x, t.v1.y, t.v2.x, t.v2.y, t.v3.x, t.v3.y) 
					spare:Float = mini
					mini = maxi
					maxi = spare
				EndIf
				
				If mini >= 0 'has intersected this triangle
					If maxi >= 0 'has left this triangle!
						t.makecut(sl.x + mini * sl.dx, sl.y + mini * sl.dy, sl.x + maxi * sl.dx, sl.y + maxi * sl.dy) 
					Else
						t.makecut(sl.x + mini * sl.dx, sl.y + mini * sl.dy, ex, ey) 
					End If
				End If
			Next
		EndIf
		Next
		
	End Method
	
	Method clang(f:Float) 
		i = Rand(0, numclangs - 1) 
		myclang = sounditem.Create(clangs[i] , 1, 0, 1) 
		contact.myclang = myclang
	End Method
	
	Method rub(f:Float, mu:Float) 
		'Return
		'If myclang Return
		If contact
			If contact.myrub Return
		EndIf
		If Not myrub
			myrub = sounditem.Create(metalrub, 0, 0, 0, 1) 
		EndIf
		trate:Float = mu *.4 + 1
		tv:Float = f *.1
		If tv > 1 tv = 1
		rubrate:+(trate - rubrate) *.2
		rubv:+(tv - rubv) *.2
		DrawLine 0, 30, rubrate * 50, 30
		DrawLine 0, 40, rubv * 50, 40
		myrub.rate = rubrate
		SetChannelVolume myrub.chan, rubv
	End Method
	
	Method draw() 
		SetLineWidth 2
		DrawZoomLine x, y, ex, ey
		SetLineWidth 1
	End Method
End Type

Function nicean:Float(an:Float) 
	an = an Mod 360
	If an > 180 an:-360
	If an < - 180 an:+360
	Return an
End Function

Type bone
	Field j1:joint, j2:joint
	Field length:Float, nlength:Float
	Field alongx:Float, alongy:Float, sidex:Float, sidey:Float
	Field an:Float
	
	Function Create:bone(j1:joint, j2:joint, length:Float) 
		b:bone = New bone
		b.j1 = j1
		b.j2 = j2
		b.length = length
		Return b
	End Function
	
	Method constrain() 
		dx:Float = j2.x - j1.x
		dy:Float = j2.y - j1.y
		d:Float = Sqr(dx * dx + dy * dy) 
		f:Float = (d - length) 
		If d = 0 d = 1
		dx:*f / d
		dy:*f / d
		If j1.fixed
			If j2.fixed
				Return
			Else
				j2.x:-dx
				j2.y:-dy
			EndIf
		Else
			If j2.fixed
				j1.x:+dx
				j1.y:+dy
			Else
				j1.x:+dx / 2
				j1.y:+dy / 2
				j2.x:-dx / 2
				j2.y:-dy / 2
			EndIf
		EndIf
	End Method
	
	Method update(delta:Float) 
		dx:Float = j2.x - j1.x
		dy:Float = j2.y - j1.y
		nlength = Sqr(dx * dx + dy * dy) 
		an:Float = ATan2(dy, dx) 
		alongx = Cos(an) 
		alongy = Sin(an) 
		sidex = Cos(an + 90) 
		sidey = Sin(an + 90) 
	End Method
	
	Method draw() 
		SetLineWidth 3
		DrawZoomLine j1.x, j1.y, j2.x, j2.y
		SetLineWidth 1
	End Method
End Type

Type joint
	Field x:Float, y:Float
	Field vx:Float, vy:Float
	Field ox:Float, oy:Float
	Field fixed
	
	Function Create:joint(x:Float, y:Float, fixed = 0) 
		j:joint = New joint
		j.x = x
		j.y = y
		j.fixed = fixed
		Return j
	End Function
	
	Method update(delta:Float) 
		vx = x - ox
		vy = y - oy
		ox = x
		oy = y
		'x:+vx * delta
		'y:+vy * delta
	End Method
	
	Method boundary(by:Float) 
		If y > by
			dx:Float = x - ox
			dy:Float = y - oy
			d:Float = Sqr(dx * dx + dy * dy) 
			If d = 0 Return
			vy = by - oy
			x = ox + dx * vy / d
			y = by
			
		EndIf
	End Method
	
	Method hinge(tx:Float, ty:Float, sx, sy) 
		dy:Float = ty - y
		If dy * sy > 0
			y = ty + dy
		EndIf
		
		dx:Float = tx - x
		If dx * sx > 0
			x = tx + dx
		EndIf
	End Method
		
	Method draw() 
		DrawZoomCircle x, y, 5
	End Method
End Type

Type texvertex
	Field x:Float, y:Float
	Field u:Float, v:Float

	Method New() 
		u = Rnd(0, .1) 
		v = Rnd(0, .1) 
	End Method
	
	Rem
	Function Create:texvertex(x:Float, y:Float) 
		v:texvertex = New texvertex
		v.x = x
		v.y = y
		Return v
	End Function
	end rem
	
	Method update(delta:Float) 
	End Method
	
	Method draw() 
		DrawZoomCircle x, y, 2
	End Method
End Type

Type bonevertex Extends texvertex
	Field b:bone
	Field d:Float, n:Float
	
	Function Create:bonevertex(b:bone, d:Float, n:Float) 
		v:bonevertex = New bonevertex
		v.b = b
		v.d = d
		v.n = n
		Return v
	End Function
	
	Method update(delta:Float) 
		x = b.nlength * d * b.alongx + b.length * n * b.sidex + b.j1.x
		y = b.nlength * (d * b.alongy + n * b.sidey) + b.j1.y
	End Method
	
End Type

Type hingevertex Extends texvertex
	Field j1:joint, j2:joint, j3:joint, n:Float
	Field switchover, switchunder
	Function Create:hingevertex(j1:joint, j2:joint, j3:joint, n:Float, switchunder = 1, switchover = 1) 
		v:hingevertex = New hingevertex
		v.j1 = j1
		v.j2 = j2
		v.j3 = j3
		v.n = n
		v.switchunder = switchunder
		v.switchover = switchover
		Return v
	End Function
	
	Method update(delta:Float) 
		dx1:Float = j2.x - j1.x
		dy1:Float = j2.y - j1.y
		dx2:Float = j3.x - j2.x
		dy2:Float = j3.y - j2.y
		
		an1:Float = ATan2(dy1, dx1) 
		an2:Float = ATan2(dy2, dx2) 
		dpan:Float = (an1 - an2) 
		If switchunder And dpan < - 180 dpan:+360
		If switchover And dpan > 180 dpan:-360
		dpan2:Float = 180 - dpan
		an:Float = dpan2 / 2 + an1
		x:Float = j2.x + n * Cos(an) 
		y:Float = j2.y + n * Sin(an) 
	End Method
End Type

Type textri
	Field v1:texvertex, v2:texvertex, v3:texvertex
	Field image:TImage
	Field cuts:TList
	
	Method New() 
		cuts = New TList
	End Method
	
	Function Create:textri(image:TImage, v1:texvertex, v2:texvertex, v3:texvertex) 
		t:textri = New textri
		t.image = image
		t.v1 = v1
		t.v2 = v2
		t.v3 = v3
		Return t
	End Function
	
	Method makecut(x1:Float, y1:Float, x2:Float, y2:Float) 
		'DrawZoomLine x1, y1, x2, y2
		'Flip
		'Delay 100
		x1:-v1.x
		y1:-v1.y
		x2:-v1.x
		y2:-v1.y
		
		'line 1: v1 -> v2
		'line 2: v1 -> v3
		
		dx1:Float = v2.x - v1.x
		dy1:Float = v2.y - v1.y
		d1:Float = Sqr(dx1 * dx1 + dy1 * dy1) 
		dx2:Float = v3.x - v1.x
		dy2:Float = v3.y - v1.y
		d2:Float = Sqr(dx2 * dx2 + dy2 * dy2) 
		
		
		If dy2 = 0
			b1:Float = (x1 - y1 * dx1 / dy1) / (dx2 - dx1 * dy2 / dy1) 
			If dx1 = 0
				a1:Float = (y1 - b1 * dy2) / dy1
			Else
				a1:Float = (x1 - b1 * dx2) / dx1
			End If
		Else
			a1:Float = (x1 - y1 * (dx2 / dy2)) / (dx1 - dy1 * dx2 / dy2) 
			If dx2 = 0
				b1:Float = (y1 - a1 * dy1) / dy2
			Else
				b1:Float = (x1 - a1 * dx1) / dx2
			EndIf
		End If

		If dy2 = 0
			b2:Float = (x2 - y2 * dx1 / dy1) / (dx2 - dx1 * dy2 / dy1) 
			If dx1 = 0
				a2:Float = (y2 - b2 * dy2) / dy1
			Else
				a2:Float = (x2 - b2 * dx2) / dx1
			End If
		Else
			a2:Float = (x2 - y2 * (dx2 / dy2)) / (dx1 - dy1 * dx2 / dy2) 
			If dx2 = 0
				b2:Float = (y2 - a2 * dy1) / dy2
			Else
				b2:Float = (x2 - a2 * dx1) / dx2
			EndIf
		End If
		
		dx:Float = x2 - x1
		dy:Float = y2 - y1
		d:Float = Sqr(dx * dx + dy * dy) 
		
		fw:cut = cut.Create(a1, b1, a2, b2, d) 
		cuts.AddLast fw
		
		Return
	End Method
	
	Method update(delta:Float) 
		dx1:Float = v2.x - v1.x
		dy1:Float = v2.y - v1.y
		dx2:Float = v3.x - v1.x
		dy2:Float = v3.y - v1.y
		For fw:cut = EachIn cuts
			fw.update(delta) 
			fw.position(v1.x, v1.y, dx1, dy1, dx2, dy2)
		Next
		If KeyDown(KEY_SPACE) 
			cuts = New TList
		End If
	End Method
	
	Method draw() 
		Local poly:Float[] 
		'poly =[v1.x, v1.y, v1.u, v1.v, v2.x, v2.y, v2.u, v2.v, v3.x, v3.y, v3.u, v3.v] 
		poly = panuv([v1.x, v1.y, v2.x, v2.y, v3.x, v3.y] ) 
		DrawZoomTexturedPoly image, poly
		'DrawZoomPoly poly, 1
		
		For fw:cut = EachIn cuts
			fw.draw() 
		Next
	End Method
End Type

Type cut
	Field i1:Float, j1:Float, i2:Float, j2:Float
	Field x1:Float, y1:Float, x2:Float, y2:Float
	Field ox1:Float, oy1:Float
	Field length:Float
	Field fade:Float
	Field nextbleed:Float
	
	Function Create:cut(i1:Float, j1:Float, i2:Float, j2:Float, length:Float) 
		fw:cut = New cut
		fw.i1 = i1
		fw.j1 = j1
		fw.i2 = i2
		fw.j2 = j2
		fw.length = length
		fw.bleed(0) 
		Return fw
	End Function
	
	Method update(delta:Float) 
		If fade < 1
			fade:+.01 * delta
			If fade > 1 fade = 1
		End If
		nextbleed:-delta
		If nextbleed < 0
			'DrawZoomText nextbleed - ms, x1, y1
			bleed
		End If
	End Method
	
	Method bleed(drop = 1) 
		nextbleed = Rnd(0, 1) * 500
		If drop
			p:Float = Rnd(0, 1) 
			an:Float = ATan2(y2 - y1, x2 - x1) + 90
			If an < 0 an:-180
			x:Float = x1 + (x2 - x1) * p
			y:Float = y1 + (y2 - y1) * p
			v:Float = Rnd(1, 2) 
			vx:Float = Cos(an) * v
			vy:Float = Sin(an) * v
			blood.Create(x, y, vx, vy) 
		End If
	End Method
	
	Method position(x:Float, y:Float, dx1:Float, dy1:Float, dx2:Float, dy2:Float) 
		x1 = x + dx1 * i1 + dx2 * j1
		y1 = y + dy1 * i1 + dy2 * j1
		x2 = x + dx1 * i2 + dx2 * j2
		y2 = y + dy1 * i2 + dy2 * j2
	End Method
	
	Method draw() 
		SetColor 255, 0, 0
		SetAlpha fade
		DrawZoomLine x1, y1, x2, y2
		SetAlpha 1
		SetColor 255, 255, 255
	End Method
	
End Type

Type blood
	Field x:Float, y:Float
	Field ox:Float, oy:Float
	Field vx:Float, vy:Float
	
	Method New() 
		bloods.addlast Self
		nextbleed = ms
	End Method
	
	Function Create:blood(x:Float, y:Float, vx:Float, vy:Float) 
		h:blood = New blood
		h.x = x
		h.y = y
		h.vx = vx
		h.vy = vy
		Return h
	End Function
	
	Method update(delta:Float) 
		If ZoomY(y) < 0 bloods.Remove Self
		vy:+.2 * delta
		ox = x
		oy = y
		x:+vx * delta
		y:+vy * delta
		
		If y > floory
			bloods.remove Self
		EndIf
	End Method
	
	Method draw() 
		SetColor 255, 0, 0
		DrawZoomLine ox, oy, x, y
		SetColor 255, 255, 255
	End Method
End Type

Function panuv:Float[] (poly:Float[] ) 
	Local opoly:Float[Len(poly) * 2] 
	i = 0
	While i < Len(poly) 
		opoly[i * 2] = poly[i] 
		opoly[i * 2 + 1] = poly[i + 1] 
		u:Float = ZoomX(poly[i] ) / gwidth
		'If u < 0 u:+1
		v:Float = ZoomY(poly[i + 1] ) / gheight
		'If v < 0 v:+1
		opoly[i * 2 + 2] = u
		opoly[i * 2 + 3] = v
		i:+2
	Wend
	Return opoly
End Function

'graphics init
Function initgfx()
	'SetGraphicsDriver d3d7max2ddriver() 
	AppTitle = "How appropriate. You fight like a cow."
	Graphics gwidth, gheight
	SetBlend ALPHABLEND
	SetClsColor 100, 100, 100
	HideMouse
	
	yellowpaper = LoadImage("yellowpaper.jpg") 
	bluepaper = LoadImage("bluepaper.jpg") 
	greenpaper = LoadImage("greenpaper.jpg") 
	brownpaper = LoadImage("brownpaper.jpg") 
End Function


'sound init
Global numfootsteps = 8
Global footsteps:TSound[numfootsteps] 
Global numswooshes = 5
Global swooshes:TSound[numswooshes] 
Global numclangs = 4
Global clangs:TSound[numclangs] 
Global metalrub:TSound
Function initsound() 
	fillsounds(footsteps, numfootsteps, "foot") 
	fillsounds(swooshes, numswooshes, "swoosh") 
	fillsounds(clangs, numclangs, "sword") 
	metalrub = LoadSound("metalrub.ogg") 
End Function

Function fillsounds(sounds:TSound[] , n, name:String) 
	For i = 1 To n
		sounds[i - 1] = LoadSound(name + String(i) + ".ogg") 
	Next
End Function

'game init
Global skeletons:TList
Global bloods:TList
Global floory:Float
Global ms
Global timezoom:Float
Global fb:skeleton, op:skeleton
Function initgame() 
	skeletons = New TList
	bloods = New TList
	floory = 0
	timezoom = 1

	fb:skeleton = skeleton.Create(0, 0, 1, 30, yellowpaper, brownpaper, brownpaper) 
	op:skeleton = skeleton.Create(300, 0, - 1, 30, brownpaper, yellowpaper, yellowpaper) 
	
	pany = floory - 200
	tpany = pany

End Function

Function updateworld(delta:Float) 
	For s:skeleton = EachIn skeletons
		s.update(delta) 
	Next
	
	For h:blood = EachIn bloods
		h.update(delta) 
	Next
	
	For sound:sounditem = EachIn sounditems
		sound.update()
	Next

	If Abs(panx - tpanx) * zoom > gwidth *.2
		panx:+(tpanx - panx) *.05 * delta
	EndIf
	If Abs(pany - tpany) * zoom > gwidth *.1
		pany:+(tpany - pany) *.1 * delta
	EndIf
	
	zoom:+(tzoom - zoom) *.1 * delta
End Function

'draw everything
Function drawworld() 
	DrawZoomLine UnzoomX(0), floory, UnzoomX(gwidth), floory
	Local poly:Float[] 
	x1:Float = UnzoomX(0) 
	x2:Float = UnzoomX(gwidth) 
	y1:Float = UnzoomY(0) 
	y2:Float = UnzoomY(gheight) 
	poly = panuv([x1, floory, x2, floory, x2, y2, x1, y2] ) 
	DrawZoomTexturedPoly greenpaper, poly
	poly = panuv([x1, y1, x2, y1, x2, floory, x1, floory] ) 
	SetBlend SHADEBLEND
	DrawZoomTexturedPoly bluepaper, poly
	SetBlend ALPHABLEND
	
	For s:skeleton = EachIn skeletons
		s.draw() 
	Next
	
	For h:blood = EachIn bloods
		h.draw() 
	Next
	
End Function



'START!
gwidth = 800
gheight = 600
initgfx() 
initsound() 
SeedRnd MilliSecs()

initgame() 

done = 0
ms = MilliSecs() 
zoom =.1
While Not done
	Flip
	Cls
	drawworld() 
	tzoom = gwidth *.5 / Abs(fb.topspine.x - op.topspine.x) 
	If tzoom > 1.5 tzoom = 1.5
	
	oldms = ms
	ms = MilliSecs() 
	delta:Float = (ms - oldms) *.03 * timezoom

	fb.control() 
	topx:Float = fb.lhand.x + 150
	'op.lhand.x:+(topx - op.lhand.x) *.3 * delta
	op.lhand.y:+(fb.lhand.y - op.lhand.y) *.3 * delta

	updateworld(delta) 

	
		
	If KeyHit(KEY_ESCAPE) Or AppTerminate() 
		done = 1
	EndIf
Wend