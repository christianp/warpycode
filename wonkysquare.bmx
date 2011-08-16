Function wonkysquare(x1#,y1#,x2#,y2#,x3#,y3#,x4#,y4#,n)
	If n=0 Return
	
	u#=Rnd(0,1)
	v#=Rnd(0,1)
	
	px1#=x1+(x2-x1)*u
	py1#=y1+(y2-y1)*u
	px3#=x3+(x4-x3)*u
	py3#=y3+(y4-y3)*u
	
	px4#=x1+(x4-x1)*v
	py4#=y1+(y3-y1)*v
	px2#=x3+(x2-x3)*v
	py2#=y3+(y2-y3)*v
	
	x#=px1+(px3-px1)*v
	y#=py1+(py3-py1)*v
	
	SetLineWidth n
	SetColor 0,100,0
	'DrawLine px1,py1,px2,py2
	SetColor 100,100,100
	'DrawLine x1,y1,x2,y2
	'DrawLine x2,y2,x3,y3
	'DrawLine x3,y3,x4,y4
	'DrawLine x4,y4,x1,y1
	SetColor 255,0,0
	DrawRect x-n/2,y-n/2,n*4,n*4
	
	px1#=(x1+x2)/2
	py1#=(y1+y2)/2
	px2#=(x3+x2)/2
	py2#=(y3+y2)/2
	px3#=(x3+x4)/2
	py3#=(y3+y4)/2
	px4#=(x1+x4)/2
	py4#=(y1+y4)/2
	
	n:-1
	wonkysquare x1,y1,px1,py1,x,y,px4,py4,n
	wonkysquare px1,py1,x2,y2,px2,py2,x,y,n
	wonkysquare x,y,px2,py2,x3,y3,px3,py3,n
	wonkysquare px4,py4,x,y,px3,py3,x4,y4,n
	
End Function

Graphics 600,600,0
While 1
	wonkysquare 0,0,600,0,600,600,0,600,4
	Flip
	WaitKey
	Cls
Wend

