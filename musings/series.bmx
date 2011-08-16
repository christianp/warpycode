'base series type - all you need to do is define nextobject:object()
Type series
	Method hasnext()
		Return 1
	End Method
	
	Method objectenumerator:series()
		Return Self
	End Method
	
	Method nextobject:Object() Abstract
End Type

'fibonacci series
Type fib Extends series
	Field n1=1,n2=1
	Method nextobject:Object()
		a=n2
		n2:+n1
		n1=a
		Return String(n2)
	End Method
End Type

'prime numbers
Type primes Extends series
	Field primes[]=[2]
	Method nextobject:Object()
		n=primes[Len(primes)-1]+1
		While Not isprime(n)
			n:+1
		Wend
		primes:+[n]
		Return String(n)
	End Method
	
	Method isprime(n)
		For prime=EachIn primes
			If prime*prime>n Return 1
			If n Mod prime=0 Return 0
		Next
		Return 1
	End Method
End Type

'you can stop iterating, then come back to where you were:

Print "Primes to 100:"
s:series=New primes
For n$=EachIn s
	i=Int(n)
	Print i
	If i>100 Exit
Next

Print "~nPrimes to 200:"
For n$=EachIn s
	i=Int(n)
	Print i
	If i>200 Exit
Next