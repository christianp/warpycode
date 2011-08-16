Global arg_ints[],arg_ints_stack:TList=New TList
Global arg_floats#[],arg_floats_stack:TList=New TList
Global arg_objects:Object[],arg_objects_stack:TList=New TList
Global res_ints[]
Global res_floats#[]
Global res_objects:Object[]

Function fi()(ints[],func())
	Print "set ints"
	arg_ints=ints
	res_ints=New Int[0]
	res_floats=New Float[0]
	res_objects=New Object[0]
	Return func
End Function

Function ff()(floats#[],func())
	arg_floats=floats
	res_ints=New Int[0]
	res_floats=New Float[0]
	res_objects=New Object[0]
	Return func
End Function

Function fo()(objects:Object[],func())
	arg_objects=objects
	res_ints=New Int[0]
	res_floats=New Float[0]
	res_objects=New Object[0]
	Return func
End Function

Function fum(func())
	func()
	Print "-stack"
	arg_ints=Int[](arg_ints_stack.removelast())
	arg_floats=Float[](arg_floats_stack.removelast())
	arg_objects=Object[](arg_objects_stack.removelast())
End Function

Function fee()(func())
	Print "+stack"
	arg_ints_stack.addlast arg_ints
	arg_floats_stack.addlast arg_floats
	arg_objects_stack.addlast arg_objects
	Return func
End Function

'example 1

Rem
Function middles()
	'takes 2 ints - startchar and endchar
	'takes an array of strings
	'returns the middle bits of the strings, between startchar and endchar

	startchar = arg_ints[0]
	endchar = arg_ints[1]
	
	For s$=EachIn arg_objects
		res_objects:+[s[startchar..endchar]]
	Next
End Function

fum(fi([2,7],fo(["rrhellorrr","rrthererrrr"],middles)))

For s$=EachIn res_objects
	Print s
Next

Function fact()
	Print "fact"
	n=arg_ints[0]
	If n=1
		res=1
	Else
		fum(fi([n-1],fee(fact)))
		res=n*res_ints[0]
	EndIf
	Print res
	res_ints=[res]
End Function
EndRem

Function pairup()
	If do_fee
		fum(fi(arg_ints,fee(square)))
	Else
		Print "no fee"
		fi(arg_ints,square)()
	EndIf
	For i=0 To Len(arg_ints)-1
		Print arg_ints[i]+"^2 = "+res_ints[i]
	Next
End Function

Function square()
	res_ints=New Int[Len(arg_ints)]
	For i=0 To Len(arg_ints)-1
		arg_ints[i]=arg_ints[i]^2
	Next
	res_ints=arg_ints
End Function



Global do_fee=1
fum(fi([5,10,15,20],pairup))
do_fee=0
fi([5,10,15,20],pairup)()
