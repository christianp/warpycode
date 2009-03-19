Function Rat:FRat( n, d )
	r:FRat =New FRat
	r.n = n
	r.d = d
	Return r
End Function
Type FRat
	Field n, d
End Type

Function add:FRat( r1:FRat, r2:FRat )
	Return rat( r1.n*r2.d + r2.n* r1.d , r1.d * r2.d )
End Function

Function sub:FRat( r1:FRat, r2:FRat )
	Return rat( r1.n*r2.d - r2.n* r1.d , r1.d * r2.d )
End Function

Function mul:FRat( r1:FRat, r2:FRat )
	Return rat( r1.n*r2.n , r1.d * r2.d )
End Function

Function div:FRat( r1:FRat, r2:FRat )
	Return rat( r1.n*r2.d , r1.d * r2.n )
End Function

Function tofloat#( r:FRat )
	Return Float(r.n)/Float(r.d)
End Function

Function balance:Frat( r:FRat )
	g = gcd( r.n, r.d )
	Return rat( r.n/g , r.d/g )
End Function

Function tostring$( r:FRat )
	Return String(r.n) +" / "+String(r.d)
End Function

Function gcd( a, b )
	If a < b
		c = a 
		a = b
		b = c
	EndIf
	
	While b <> 0
		t = b
		b = a Mod b
		a = t
	Wend
	Return a
End Function

r:FRat = mul( rat(3,5) , rat(15, 6) )
Print tostring( r )
Print tostring( balance( r ) )

n=1
i=0
While 1
	n:*2
	i:+1
	Print i+" : "+n
Wend