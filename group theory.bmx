Type TGroup Abstract
	Method Finite() Abstract
	Method Order() Abstract
	Method Elements:TGroupElement[]() Abstract
	Method Generators:TGroupElement[]() Abstract
	Method Contains(e:TGroupElement) Abstract
	Method Subset(G:TGroup) Abstract
	Method Equal(G:TGroup) Abstract
	Method Identity:TGroupElement() Abstract
	Method Inverse:TGroupElement(e:TGroupElement) Abstract
	Method Product:TGroupElement(l:TGroupElement, r:TGroupElement) Abstract
End Type

Type TFiniteGroup Extends TGroup Abstract
	Method Finite()
		Return True
	End Method
	
	Method Subset(G:TGroup)
		If Not G.Finite() Return False
		
		For e:TGroupElement=EachIn G.Elements()
			If Not contains(e) Return False
		Next
		Return True
	End Method
	
	Method Equal(G:TGroup)
		If Not G.Finite() Return False
		If G.Order() <> Order() Return False
		For e:TGroupElement=EachIn G.Elements()
			If Not Contains(e) Return False
		Next
		For e:TGroupElement=EachIn Elements()
			If Not G.Contains(e) Return False
		Next
		Return True
	End Method
End Type

Type TGroupElement Abstract
	Method repr$() Abstract
End Type

Type CyclicGroup Extends TFiniteGroup
	Field size
	
	Function Create:CyclicGroup(size)
		G:CyclicGroup = New CyclicGroup
		G.size = size
		Return G
	End Function

	Method Order()
		Return size
	End Method

	Method Elements:TGroupElement[]()
		Local elements:TGroupElement[]=New TGroupElement[size]
		
		For n=0 To size-1
			elements[n]=IntegerElement.Create(n)
		Next
		
		Return elements
	End Method
	
	Method Generators:TGroupElement[]()
		Local generators:TGroupElement[]=[IntegerElement.Create(1)]
		Return generators
	End Method
	
	Method Contains(e:TGroupElement)
		n:IntegerElement = IntegerElement( e )
		If Not n Return False
		If n.number>=size Return False
		Return True
	End Method
	
	Method Identity:TGroupElement()
		Return IntegerElement.Create( 0 )
	End Method
	
	Method Inverse:TGroupElement(e:TGroupElement)
		n:IntegerElement = IntegerElement(e)
		Return IntegerElement.Create( size - n.number )
	End Method

	Method Product:TGroupElement(l:TGroupElement, r:TGroupElement)
		n1:IntegerElement = IntegerElement(l)
		n2:IntegerElement = IntegerElement(r)
		Return IntegerElement.Create( (n1.number + n2.number) Mod size )
	End Method
	
	Method Random:TGroupElement()
		Local k:TGroupElement[] = elements()
		
		
		n=Len(k)

		Local names$[n]
		For i=0 To n-1
			names[i]=k[i].repr()
		Next
		Print ", ".join(names)
		
		For t=1 To 100
			i=Rand(0,n-1)
			j=Rand(0,n-2)
			If j>=i j:+1
			
			l:TGroupElement = k[i]
			r:TGroupElement = k[j]
			If Rand(0,1)=1 r=Inverse(r)

			If Rand(0,1)=1 'left
				k[i] = Product( l, r )
			Else
				k[i] = Product( r, l )
			EndIf
		Next
		Return k[ Rand(0,n-1) ]
	End Method
End Type

Type IntegerElement Extends TGroupElement
	Field number
	
	Function Create:IntegerElement( number )
		i:IntegerElement = New IntegerElement
		i.number = number
		Return i
	End Function
	
	Method repr$()
		Return String(number)
	End Method
End Type


SeedRnd MilliSecs()

cg:CyclicGroup=CyclicGroup.Create(5)

Print "order: "+cg.order()
Print "finite: "+cg.finite()
Print "elements:"
For e:TGroupElement=EachIn cg.Elements()
	Print e.repr()+": inverse = "+cg.inverse(e).repr()
Next
Print "generators: "
For e:TGroupElement=EachIn cg.Generators()
	Print e.repr()
Next

Print "4+3 = "+cg.product( integerelement.Create(4), integerelement.Create(3) ).repr()


Print cg.random().repr()