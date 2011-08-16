Include "masala.bmx"

' a little function to make nice output for the demo
Function headline(txt$)
	dashes$=""
	For c=1 To Len(txt)
		dashes:+"-"
	Next
	Print "~n"+txt+"~n"+dashes
End Function

' --------------------------------------
'   Concatenator function
'   Concatenates a list of strings
' --------------------------------------

Function conc:Object(args:Object[], scope:TScope)
	txt$=""
	For f:TFunctor=EachIn args
		'show f,"functor is "
		'show apply(f,args,scope),"string is "
		s$=String(apply(f,Null,scope))
		If txt txt:+", "
		txt:+s
	Next
	Return FString(txt)
End Function

headline "Curried functions."
Local c:TCurry
pc:TCurry=TCurry.Create( conc, ["String","String","String"] )
show pc
c=curry( pc, [FString("hello")] )
c=curry( c, [FString("there")] )
show apply(c)

' -------------------------------------
'    TData test
' -------------------------------------

headline "Data type definitions"
person:TData=FData( [FString("String"),FString("String"),FString("String"),FString("Person")], Null )
show person
bob:TFunctor=curry( person, [FString("Bob"),FString("23"),FString("Newcastle")] )
show apply(bob)

headline "Curried construction of data"
f:TFunctor=curry( person, [FString("Jim")] )
show f
jim:TFunctor=curry( f, [FString("14"),FString("Stockport")] )
show apply(jim)



' -------------------------------------
'    Biography function
'    Prints out a person's biography
' -------------------------------------

Function biography:Object(args:Object[], scope:TScope)
	If Len(args)<1 Return "too few arguments"
	Print "applying biography function on:"
	For o:Object=EachIn args
		show o,"  "
	Next
	f:TFunctor=TFunctor(args[0])
	'show f,"argument is "
	person:TData = TData( apply(f,Null,scope) )
	'show person,"person is "
	name$=String(apply(person.values[0]))
	age=Int(String(apply(person.values[1])))
	home$=String(apply(person.values[2]))
	txt$=name+", age "+age+", of "+home
	Return FString(txt)
End Function


' -------------------------------------
'   Lazy evaluation demo
' -------------------------------------
headline "Lazy evaluation. Only second argument of DoSecond is evaluated"
biog:TCurry=TCurry.Create( biography, ["Person","String"] )
show biog,"biog :: "

Function dosecond:Object( args:Object[], scope:TScope )
	Return apply( TCurry(args[1]) )
End Function

do2nd:TCurry=TCurry.Create( dosecond, ["Functor","Functor","Object"] )
show do2nd,"do2nd :: "
c:TCurry=curry( biog, [jim] )
c2:TCurry=curry( biog, [bob] )
show apply( do2nd, [c,c2] )


' -------------------------------------
'   Scope
' -------------------------------------

headline "Scope"
s:TScope=FScope()
s.addlabel( label("jim",jim) )
s.addlabel( label("Person",person) )

show s
show getlabel(["jim"], s)
show getlabel(["trudy"], s)
show getlabel(["Person"], s)


' ------------------------------------
'   Functions
' ------------------------------------

headline "Functions"
func:TFunctor = curry( biog, [curry( getl, ["bloke"] )] )
show func,"func = "
fn:TFunction=FFunction( ["String bloke"], func )
show fn,"fn = "
show apply( fn, [jim] )

func= curry( pc, [curry(getl, ["x"]), curry(getl, ["y"])] )
show func,"func = "
fn=FFunction( ["String x","String y"], func )
show fn,"fn = "
show apply( fn, [FString("howdy"),FString("pardner")] )

' ----------------------------------
'   Pattern matching
' ----------------------------------

headline "Pattern matching"
s:TScope=FScope()
s.addlabel label("Person", person)
s.addlabel label("Biography", biog)

Local args:Object[]=[FString("Geraldo"),FString("24"),FString("Buenos Aires")]
show apply( s.matchpattern( args ), args, s )
show apply( s.matchpattern( [jim] ), [jim], s )


' ---------------------------------
'   Integers
' ---------------------------------

headline "Integers"

Local ints:Object[]=[FInteger(bignum.fromint(6)), FInteger(bignum.fromint(2))]
show apply( intadd, ints ),"6 + 2 = "
show apply( intsub, ints ),"6 - 2 = "
show apply( intmul, ints ),"6 * 2 = "
show apply( intdiv, ints ),"6 / 2 = "
show apply( intpow, ints ),"6 ^ 2 = "

