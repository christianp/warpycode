' ****************
' create a perlin noise heightmap
' I'm not hugely bothered about the properties of the heightmap, so perlin will do.
' You'll want something a bit less erratic
Function perlin#[,](size,steps=4)
	Local map#[size,size]
	Local tmp#[size,size]
	
	Local tsize,x,y,i,j,u#,v#

	Local f = 1
	For s=1 To steps
		tsize = size/f
		For x=0 To tsize-1
		For y=0 To tsize-1
			tmp[x,y] = Rnd()/steps
		Next
		Next
		For x=0 To tsize-1
		For y=0 To tsize-1
			For i=0 To f-1
			For j=0 To f-1
				u#=Float(i)/f
				v#=Float(j)/f
				map[x*f+i,y*f+j] :+ (1-u)*(1-v)*tmp[x,y]
				If y<size-1 map[x*f+i,y*f+j] :+ (1-u)*v*tmp[x,y+1]
				If x<size-1 map[x*f+i,y*f+j] :+ u*(1-v)*tmp[x+1,y] 
				If y<size-1 And x<size-1 map[x*f+i,y*f+j] :+ u*v*tmp[x+1,y+1]
			Next
			Next
		Next
		Next
		f:*2
	Next
	
	Return map
End Function

Function round(n#)
	If (n Mod 1)<.5 Return Floor(n) Else Return Floor(n)+1
End Function

' round off heights to a certain number of discrete levels, so the map is less smooth
Function stepmap(map#[,],levels)
	Local size = Sqr(Len(map))
	Local x,y
	
	For x=0 To size-1
	For y=0 To size-1
		map[x,y] = round(map[x,y]*levels)/Float(levels)
	Next
	Next
	
End Function

' apply a symmetric exponential function to the heights, to increase the difference between highs and lows
Function curvemap(map#[,],pow#=.5)
	Local size = Sqr(Len(map))
	Local x,y
	Local t#,s
	
	For x=0 To size-1
	For y=0 To size-1
		t#=map[x,y]-.5
		s=Sgn(t)
		t=Abs(t)^pow
		map[x,y] = s*t+.5
	Next
	Next
	
End Function

' create a circular plateau, to make a space that a province can be seen to spread across quickly
Function hill(map#[,],x,y,minr#=.05,maxr#=.2)
	Local size = Sqr(Len(map))
	Local f# = Rnd(minr,maxr)
	Local r# = f*size
	
	
	Local h# = Rnd(0,1)
	
	Local i,j,w,u,v
	For i = -r To r
		u = x+i
		If u>=0 And u<size
			w = Sqr(r*r-i*i)
			For j=-w To w
				v = y+j
				If v>=0 And v<size
					map[u,v] :+ (h-map[u,v])*Rnd(.3,.7)
				EndIf
			Next
		EndIf
	Next
End Function


' end of heightmap functions
' ****

Global provinces:TList=New TList
Type province
	Field r#,g#,b#
	
	Method New()
		'pick a colour to represent this province
		'colours should all be roughly the same brightness, but random hues
		Local t#
		r=Rnd()
		g=Rnd()
		b=Rnd()
		t = (r+g+b)/2
		r :/ t
		g :/ t
		b :/ t
		
		provinces.addlast Self
	End Method

	'randomly pick a point on the map
	'if it belongs to this province, pick a random adjacent point
	'if that's free, probability of taking it over depends on height difference between the picked points.
	'bigger height difference means less likely to take over
	Method seize(control:province[,],map#[,])
		Local x,y,i,j
		Local size = Sqr(Len(map))
		Local g#
		
		x=Rand(0,size-1)
		y=Rand(0,size-1)
		If control[x,y] = Self
			i=x+Rand(-1,1)
			j=y+Rand(-1,1)
			If i>=0 And i<size And j>=0 And j<size And control[i,j]=Null
				g# = Abs(map[x,y]-map[i,j])
				g = Sqr(g)* impediment
				If Rnd(0,g)<.01
					control[i,j] = Self
				Else
				'	Print "no"
				EndIf
			EndIf
		EndIf
	End Method	
End Type

AppTitle="Now spreadable!"
Graphics 512,512,0
SeedRnd MilliSecs()

Global impediment#=20

'generate heightmap
Local size = 64
Local map#[,] = perlin(size,4)				'make some perlin noise
For c=1 To 4
	hill map,Rand(0,size-1),Rand(0,size-1)		'add some plateaux
Next

' generate control map and provinces
Local control:province[size,size]		'keeps track of who owns which points
Local p:province
For c=1 To 5
	p:province = New province
	

	'pick a starting point for this province	
	x = Rand(0,size-1)
	y = Rand(0,size-1)	
	While control[x,y]
		x = Rand(0,size-1)
		y = Rand(0,size-1)	
	Wend

	control[x,y] = p
	
	'add a plateau for this province, so it can spread quickly initially
	hill map,x,y							
Next

stepmap map,6								'unsmooth the heights
curvemap map,.7							'increase the contrast between highs and lows

Local scale# = 256/size, rot#=0, pitch#=0.5
While Not (KeyHit(KEY_ESCAPE) Or AppTerminate())

	rot :+ (KeyDown(KEY_LEFT)-KeyDown(KEY_RIGHT))*1
	pitch :+ (KeyDown(KEY_UP)-KeyDown(KEY_DOWN))*.01
	pitch = Max(0.1,Min(1,pitch))
	scale :+ (KeyDown(KEY_A)-KeyDown(KEY_Z))*.1

	'draw the map
	For x=0 To size-1
	For y=0 To size-1
		shade# = map[x,y]
		p = control[x,y]
		If p
			shade = (shade+.3)*255/1.3
			SetColor shade*p.r,shade*p.g,shade*p.b
		Else
			shade :* 255*.2
			SetColor shade,shade,shade
		EndIf
		u#=Float(x)/size-.5
		v#=Float(y)/size-.5
		u2#=u*Cos(rot)-v*Sin(rot)
		v2#=u*Sin(rot)+v*Cos(rot)
		u=u2+v2
		v=(u2-v2)*pitch
		
		u = (u+1)*256
		v = (v+1)*256-map[x,y]*50*(1-pitch)
		
		If x=0 And y=0
			SetColor 255,255,255
			DrawText u+","+v,0,0
		EndIf
		If x=size-1 And y=size-1
			SetColor 255,255,255
			DrawText u+","+v,0,15
		EndIf
		If x=size/2 And y=0
			SetColor 255,255,255
			DrawText u+","+v,0,30
		EndIf
		
		DrawRect u,v,scale,scale
	Next
	Next
	
	'give each province a few chances at taking control of an adjacent point
	For c=1 To 250
		For p=EachIn provinces
			p.seize control,map
		Next
	Next

	Flip
	Cls
Wend