Rem

Position - based dynamics (Muller et al 2006) 

HOW IT WORKS
There are objects , which are collections of vertices and constraints on how those
vertices act.
So you create an object , add a bunch of vertices , then add some constraints
Constraints can set the distance between vertices , make sure something doesn't bend too 
much, make sure an object retains its volume, whatever.
The update function then moves everything about for one timestep then finds an arrangement
for them to be in that satisfies the constraints

That's it!

You can make sure a vertex isn't moved by the solver by setting its inverse mass, w, to 0.
(For example when you've attached it to something)

Adding different kinds of constraints requires a little bit of maths and know - how ,
which you can find in the original paper: 
http:/ / www.matthiasmueller.info / publications / posBasedDyn.pdf

EndRem


Const numsteps=5


Global objects:TList=New TList
Type obj
	Field vertices:TList
	Field constraints:TList
	
	Method New()
		vertices = New TList
		constraints = New TList
		objects.addlast Self
	End Method
	
	Method update(tstep#)
		If tstep=0 Return
		Rem damping!
		sumxm# = 0
		sumym# = 0
		sumvxm# = 0
		sumvym# = 0
		summ# = 0
		
			sumxm:+ v.x.x * v.m
			sumym:+ v.x.y * v.m
			sumvxm:+ v.v.x * v.m
			sumvym:+ v.v.y * v.m
			summ:+ v.m
		Next
		xcm:vector = vector.create(sumxm / summ , sumym / summ)
		vcm:vector = vector.create(sumvxm / summ , sumvym / summ)
		For v:vector=EachIn 
		EndRem
		For v:vertex = EachIn vertices
			v.p = v.x.add(v.v.scale(tstep) ) 
			DrawRect v.p.x , v.p.y , 1 , 1
			v2:vector = v.v.scale(tstep)
			v.newp=vector.Create(0,0)
		Next
		collconstraints:TList=New TList
		For v:vertex = EachIn vertices
			If v.p.y >= 700
				'Print "collision!"
				diff:vector = v.p.subtract(v.x)
				If diff.y<>0
					qx# = v.p.x+diff.x * (700 - v.x.y) / diff.y
					q:vector = vector.Create(qx , 700)
					cc:CollisionConstraint = collisionconstraint.Create(v , q , vector.Create(0 , - 1) , .9 ) 
					'diff = v.p.subtract(q)
					'cc.oldv=diff
					collconstraints.addlast(cc)
				EndIf
			EndIf
		Next

		For i = 1 To numsteps
			For v:vertex = EachIn vertices
				v.p=v.p.add(v.newp)
				v.newp = vector.Create(0 , 0)
			Next
			For c:constraint = EachIn constraints
				c.project()
			Next
			For c:constraint = EachIn collconstraints
				c.project()
			Next
		Next
		
		For v:vertex = EachIn vertices
			v.v = v.p.subtract(v.x)
			v.v.scale(1.0 / tstep)
			v.x = v.p
		Next
		For v:vertex = EachIn vertices
			v.externalforces(tstep#)
		Next
		
		'friction, restitution coefficients
		For cc:collisionconstraint = EachIn collconstraints
			
			restitution#=2 * cc.oldv.dotproduct(cc.n)*cc.cor
			cc.v.v = cc.oldv.subtract(cc.n.scale(restitution ) )
			restforce# = cc.v.m * restitution
			frictionforce# = .1 * restforce
			nperp:vector = cc.n.perpendicular()
			friction:vector = nperp.scale(  frictionforce * cc.oldv.dotproduct(nperp) / cc.oldv.modulus() )
			v.v=v.v.add(friction)
		Next
	End Method
	
	Method draw()
		For v:vertex = EachIn vertices
			v.draw()
		Next
		For c:constraint=EachIn constraints
			c.draw()
		Next
	End Method
End Type

Type vector
	Field x# , y# , z#
	Field length#

	Method New()
		length = - 1
	End Method
	
	Function Create:vector(x# , y#, z# = 0)
		l:vector = New vector
		l.x = x
		l.y = y
		l.z = z
		
		Return l
	End Function
	
	Method add:vector(l:vector)
		Return vector.Create(x + l.x , y + l.y , z + l.z)
	End Method
	
	Method subtract:vector(l:vector)
		Return vector.Create(x - l.x , y - l.y , z - l.z)
	End Method
	
	Method modulus#()
		If length = - 1
			length = Sqr(x * x + y * y + z * z)
		EndIf
		Return length
	End Method
	
	Method dotproduct#(l:vector)
		Return (x * l.x + y * l.y + z * l.z)
	End Method
	
	Method crossproduct:vector(l:vector)
		Return vector.Create(y * l.z - z * l.y , z * l.x - x * l.z , x * l.y - y * l.x)
	End Method
	
	Method normal:vector()
		modulus()
		Return vector.Create(x / length , y / length , z / length)
	End Method
	
	Method scale:vector(lambda#)
		Return vector.Create(x * lambda , y * lambda , z * lambda)
	End Method
	
	Method perpendicular:vector()
		Return vector.Create( - y , x)
	End Method
End Type

Type vertex
	Field x:vector , v:vector, p:vector
	Field m# , w#
	Field f:vector
	Field newp:vector
	
	Function Create:vertex(x# , y# , m#)
		v:vertex = New vertex
		v.x = vector.Create(x , y)
		v.v = vector.Create(0 , 0)
		v.m=m
		If v.m <> 0
			v.w = 1 / v.m
		Else
			Print  "tried to make vertex with zero mass??"
			Return Null
		EndIf
		v.f=vector.Create(0,0)
		
		Return v
	End Function
	
	Method externalforces(tstep#)
		f = f.add(vector.Create(0 , m*1.5) )
		f = f.scale(tstep * w)
		v = v.add(f) 
		f=vector.Create(0,0)
		Return
	End Method
	
	Method draw()
		DrawRect x.x,x.y,2,2
	End Method
	
End Type

Type constraint
	Field k#
	Field equality
	
	Method New()
		equality = 1
		k=1
	End Method
	
	Method setstiffness(tk#)
		k#=1-(1-tk)^(1.0/numsteps)
	End Method
	
	Method project()
		Return
	End Method
	
	Method draw()
		Return
	End Method
End Type

Type DistanceConstraint Extends constraint
	Field v1:vertex , v2:vertex , d#
	
	Function Create:DistanceConstraint(v1:vertex , v2:vertex , d#, k#=1)
		dc:distanceconstraint = New distanceconstraint
		dc.v1 = v1
		dc.v2 = v2
		dc.d = d
		dc.setstiffness(k)
		Return dc
	End Function
	
	Method project()
		diff:vector = v1.p.subtract(v2.p)
		dp1:vector = diff.scale( - v1.w * (diff.modulus() - d) / ( (v1.w + v2.w) * diff.modulus() ) * k )
		dp2:vector = diff.scale( v2.w * (diff.modulus() - d) / ( (v1.w + v2.w) * diff.modulus() ) * k )
		DrawText dp1.x,0,0
		
		v1.newp=v1.newp.add(dp1)
		v2.newp=v2.newp.add(dp2)
	End Method
	
	Method draw()
		DrawLine v1.x.x,v1.x.y,v2.x.x,v2.x.y
	End Method
End Type

Type CollisionConstraint Extends constraint
	Field v:vertex , q:vector , n:vector
	Field oldv:vector
	Field cor#
	
	Function Create:CollisionConstraint(v:vertex , q:vector , n:vector,cor#=1,k#=1)
		cc:CollisionConstraint = New CollisionConstraint
		cc.v = v
		cc.q = q
		cc.n = n
		cc.cor=cor
		cc.oldv=v.p.subtract(v.x)
		cc.setstiffness(k)
		'v.w=0
		Return cc
	End Function
	
	Method project()
		diff:vector = v.p.subtract(q)
		scale# = diff.dotproduct(n)
		If scale>=0 Return
		dp:vector = n.scale( -scale * k )
		'Print v.p.y
		'Print String(dp.x) + "," + String(dp.y)
		'Print v.x.y
		'Print v.p.y
		'Print dp.y
		v.newp = v.newp.add(dp)
	End Method
End Type

'loop
Graphics 1040 , 720 , 0
SetBlend ALPHABLEND

o:obj = New obj

vert1:vertex = vertex.Create(0 , 300 , 1)
vert1.w=0
o.vertices.addlast vert1
overt:vertex=vert1
For i = 1 To 8
	vert:vertex = vertex.Create(i * 100 , 300 , 1)
	o.vertices.addlast vert
	o.constraints.addlast distanceconstraint.Create(overt , vert , 100)
	overt = vert
Next

vert:vertex = vertex.Create(200 , 100 , 1)
vert.w=0
o.vertices.addlast vert
o.constraints.addlast distanceconstraint.Create(vert , overt , 100)

o = New obj
vertjim:vertex = vertex.Create(520 , 360 , 1) 
vertjim.v=vector.Create(5,0)
o.vertices.addlast(vertjim)
vertjim2:vertex = vertex.Create(570 , 360 , 1)
vertjim3:vertex = vertex.Create(545, 360-Sqr(1875) , 1)
o.vertices.addlast vertjim2
o.vertices.addlast vertjim3
o.constraints.addlast distanceconstraint.Create(vertjim , vertjim2 , 50,.3)
o.constraints.addlast distanceconstraint.Create(vertjim , vertjim3 , 50,.3)
o.constraints.addlast distanceconstraint.Create(vertjim3 , vertjim2 , 50,.3)


finished = 0
oldms = MilliSecs()

While Not finished
	
	ms = MilliSecs()
	tstep#=(ms-oldms)/1000.0
	oldms = ms
	
	If KeyHit(KEY_ESCAPE)	
		finished = 1
	EndIf
	
	vert.x = vector.Create(MouseX() , MouseY() ) 
	
	For o:obj=EachIn objects
		o.update(1)
		o.draw()
	Next
	
	DrawText vertjim.x.x,520,0
	DrawText vertjim.x.y,520,15
	
	'Delay 100
	Flip
	Cls
Wend