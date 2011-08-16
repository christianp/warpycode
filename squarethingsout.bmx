'f$=Input("filename:")
f$="image_025.jpg"
img:TImage=LoadImage(f)

w=ImageWidth(img)
h=ImageHeight(img)

Graphics w,h,0

state=0
Local points#[4,2]
i=0
While Not state
	If KeyHit(KEY_ESCAPE)
		End
	EndIf
	
	DrawImage img,0,0
	If MouseHit(1)
		points[i,0]=MouseX()
		points[i,1]=MouseY()
		i:+1
		If i=4 Then state=1
	EndIf
	
	x#=points[0,0]
	y#=points[0,1]
	c=1
	While c<i
		ox=x
		oy=y
		x=points[c,0]
		y=points[c,1]
		DrawLine ox,oy,x,y
		c:+1
	Wend
	
	If i
		DrawLine x,y,MouseX(),MouseY()
	EndIf
	
	Flip
	Cls
Wend

dx1#=points[1,0]-points[0,0]
dy1#=points[1,1]-points[0,1]
dx2#=points[3,0]-points[0,0]
dy2#=points[3,1]-points[0,1]
dx3#=points[1,0]-points[2,0]
dy3#=points[1,1]-points[2,1]
dx4#=points[2,0]-points[3,0]
dy4#=points[2,1]-points[3,1]


'd1#=Sqr(dx1*dx1+dy1*dy1)
'dx1:/d1
'dy1:/d1
'd2#=Sqr(dx2*dx2+dy2*dy2)
'dx2:/d2
'dy2:/d2

pm:TPixmap=LockImage(img)
cm:TPixmap=CreatePixmap(201,201,PixmapFormat(pm))
For lambda#=0 To 200
	l#=lambda/200.0
For mu#=0 To 200-lambda
	m#=mu/200.0
	x#=points[0,0]+l*dx1+m*dx2
	y#=points[0,1]+l*dy1+m*dy2
	rgb=ReadPixel(pm,x,y)
	WritePixel cm,lambda,mu,rgb
	Plot x,y
Next
For mu#=0 To lambda
	m#=mu/200.0
	x#=points[3,0]+l*dx4+m*dx3
	y#=points[3,1]+l*dy4+m*dy3
	rgb=ReadPixel(pm,x,y)
	WritePixel cm,lambda,200-mu,rgb
	Plot x,y
Next
Next
DrawPixmap cm,0,0
Flip
WaitKey()