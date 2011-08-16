Global editormenustate=0
Global gridsplits=6
Global gridsize#=800/(2^gridsplits)
Global selectedwall:wall

Function drawsplit(minx,miny,maxx,maxy,level)
	If Not level Return
	midx=(minx+maxx)/2
	midy=(miny+maxy)/2
	SetColor 255*Float(level)/gridsplits,0,0
	DrawscaleLine midx,miny,midx,maxy
	DrawscaleLine minx,midy,maxx,midy
	drawsplit(minx,miny,midx,midy,level-1)
	drawsplit(minx,midy,midx,maxy,level-1)
	drawsplit(midx,miny,maxx,midy,level-1)
	drawsplit(midx,midy,maxx,maxy,level-1)
End Function

Function runeditor()
	Local drawpoints#[]=[0.0,0.0,0.0,0.0,0.0,0.0]
	pointc=0
	
	walls:tlist=New tlist
	bots:tlist=New tlist
	
	initgame(editorfilenamebutton.entrytext)
	
	finished=0
	editmode=0
	
	mousestate=0
	
	While Not finished
		updatemouse()
		drawx=gridsize*(Int(mx/gridsize+.5))
		drawy=gridsize*(Int(my/gridsize+.5))
		
		drawsplit(0,0,800,800,gridsplits)
		

		'////DRAWING		
		If pointc
			DrawscaleLine drawpoints[pointc*2-2],drawpoints[pointc*2-1],drawx,drawy
		EndIf
			
		For w:wall=EachIn walls
			w.draw
		Next

		If selectedwall
			SetColor 255,255,255
			SetAlpha .2
			DrawscalePoly selectedwall.points
			SetAlpha 1
			If selectedline>=0
				a=selectedline*2
				b=selectedline*2+1
				c=(selectedline*2+2) Mod 6
				d=(selectedline*2+3) Mod 6
				DrawscaleOval selectedwall.points[a]-5,selectedwall.points[b]-5,10,10
				DrawscaleOval selectedwall.points[c]-5,selectedwall.points[d]-5,10,10
				SetLineWidth 10
				SetAlpha selectedwall.drawedge[selectedline]*.5+.5
				DrawscaleLine selectedwall.points[a],selectedwall.points[b],selectedwall.points[c],selectedwall.points[d]
				SetAlpha 1
				SetLineWidth 1
			EndIf
			If selectedpoint>=0
				SetAlpha .2
				DrawscaleOval selectedwall.points[selectedpoint*2]-gridsize,selectedwall.points[selectedpoint*2+1]-gridsize,gridsize*2,gridsize*2
				SetAlpha 1
			EndIf
		EndIf
		
		SetColor 255,255,255
		If editmode=2 DrawscaleOval drawx-2,drawy-2,5,5
		For i=1 To pointc
			DrawscaleOval drawpoints[i*2-2]-2,drawpoints[i*2-1]-2,5,5
			If i<pointc
				DrawscaleLine drawpoints[i*2-2],drawpoints[i*2-1],drawpoints[i*2],drawpoints[i*2+1]
			EndIf
		Next
		
		SetColor 255,255,255
		DrawscaleText "Press S to save "+editorfilenamebutton.entrytext+".txt",0,0
			
		Select editormenustate
		Case 0 'menu not showing
			hit1=MouseHit(1)
			hit2=MouseHit(2)
			If Not mousestate
				selectedwall:wall=Null
				selectedline=-1
				selectedpoint=-1
				minpointdist=-1
			EndIf

			If KeyDown(KEY_LSHIFT)
				editmode=1
				'pointc=0
			ElseIf KeyDown(KEY_LCONTROL)
				editmode=2
			Else
				editmode=0
				pointc=0
			EndIf
			
			Select editmode
			Case 0 'move
				Select mousestate
				Case 0 'not down
					If MouseDown(1)
						mousestate=1
						grabx=drawx
						graby=drawy
					EndIf
					For w:wall=EachIn walls
						For i=0 To 2
							dx#=mx-w.points[i*2]
							dy#=my-w.points[i*2+1]
							dist#=dx*dx+dy*dy
							If (dist<minpointdist Or minpointdist=-1) And dist<gridsize*gridsize
								minpointdist=dist
								selectedpoint=i
								selectedwall=w
							EndIf
						Next
					Next	
					If Not selectedwall				
						For w:wall=EachIn walls
							If w.inside(mx,my)
								selectedwall=w
							EndIf
						Next
					EndIf
				Case 1 'down
					If selectedwall
						If selectedpoint>=0
							selectedwall.points[selectedpoint*2]=drawx
							selectedwall.points[selectedpoint*2+1]=drawy
							selectedwall.calcdiffs()
						Else
							dx#=drawx-grabx
							dy#=drawy-graby
							For i=0 To 2
								selectedwall.points[i*2]:+dx
								selectedwall.points[i*2+1]:+dy
							Next
						EndIf
					EndIf
					grabx=drawx
					graby=drawy
					If Not MouseDown(1) mousestate=0	
				End Select
			Case 1 'remove/change lines
				For w:wall=EachIn walls
					For i=0 To 2
						a=i*2
						b=i*2+1
						c=(i*2+2) Mod 6
						d=(i*2+3) Mod 6
						pickl=0
						If w.points[a]-w.points[c]=0
							dy=my-w.points[b]
							ldy=w.points[d]-w.points[b]
							If Abs(dy)<=Abs(ldy) And Abs(dy)>=0 And Sgn(dy)=Sgn(ldy)
								If Abs(mx-w.points[a])<=5
									pickl=1
								EndIf
							EndIf
						ElseIf w.points[b]-w.points[d]=0
							dx=mx-w.points[a]
							ldx=w.points[c]-w.points[a]
							If Abs(dx)<=Abs(ldx) And Abs(dx)>=0 And Sgn(dx)=Sgn(ldx)
								If Abs(my-w.points[b])<=5
									pickl=1
								EndIf
							EndIf
						Else
							sx#=w.pdiffs[a]
							sy#=w.pdiffs[b]
			
							lambda#=(my-w.points[b]+(sy/sx)*(w.points[a]-mx))/(w.diffs[b]-sy*w.diffs[a]/sx)
							'vlength#=Sqr(vy*vy+vx*vx)
							If lambda>=0 And lambda<=w.lengths[i]
								mu#=-(w.points[a]+w.diffs[a]*lambda-mx)/sx
								If mu>-5 And mu<5
									pickl=1
								EndIf
							EndIf
						EndIf
						If pickl
							selectedwall=w
							selectedline=i
						EndIf
					Next
					
				Next
				
				If Not selectedwall
					For w:wall=EachIn walls
						If w.inside(mx,my) And selectedline=-1
							selectedwall=w
						EndIf
					Next
				EndIf
				
				If hit1
					If selectedwall
						If selectedline>=0
							selectedwall.drawedge[selectedline]=1-selectedwall.drawedge[selectedline]
						EndIf
					EndIf
				EndIf
			Case 2 'draw walls
				For w:wall=EachIn walls
					If w.inside(mx,my)
						selectedwall=w
					EndIf
				Next
				If hit1
					drawpoints[pointc*2]=drawx
					drawpoints[pointc*2+1]=drawy
					pointc:+1
					If pointc=3
						wall.create(drawpoints,[1,1,1])
						pointc=0
					EndIf
				EndIf
				If hit2
					pointc=0
					If selectedwall
						walls.remove selectedwall
					EndIf
				EndIf
			End Select
						
			If KeyHit(KEY_ESCAPE)
				editormenustate=1
				changemenu(editormenu,0)
				prevmenu=Null
			EndIf
			
			If KeyHit(KEY_S) savelevel()
			
			
			
		Case 1 'menu showing
			domenus()
			If quitmenu
				quitmenu=0
				editormenustate=0
				If finishing
					finished=1
					finishing=0
					lockmouse=0
				EndIf
			EndIf
		End Select
		

		Flip
		Cls
	Wend
	Print "SAVING FOR QUIT!"
	savelevel()
	
	walls=New tlist
	bots=New tlist
	sweeps=New tlist
End Function

Function savelevel()
	Print "Saving "+editorfilenamebutton.entrytext
	f:TStream=OpenFile("levels\"+editorfilenamebutton.entrytext+".txt",0,1)
	WriteLine f,"radius "+String(Int(radius))
	WriteLine f,"roundlength "+String(Int(roundlength))
	WriteLine f,"repop "+String(Int(repop))+" "+String(Int(minrepop))+" "+String(repopdecay)
	For w:wall=EachIn walls
		line$="wall "
		For i=0 To 5
			line:+String(Int(w.points[i]))+" "
		Next
		For i=0 To 2
			line:+String(w.drawedge[i])+" "
		Next
		WriteLine f,line
	Next
	
	For bot:ai=EachIn bots
		line$="bot "+bot.itssweep.name+" "+String(bot.itssweep.red)+" "+String(bot.itssweep.green)+" "+String(bot.itssweep.blue)+" "+String(bot.speed)
		WriteLine f,line
	Next
	CloseFile f
End Function