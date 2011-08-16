Function curry(args:Object[])( fun(args:Object[]), args:Object[] )
	Global cargs:Object[]=args
	Global cfun(args:Object[]) = fun

	Function apply( args:Object[] )
		Print "curried"
		Local nargs:Object[Len(args)+Len(cargs)]
		l1=Len(args)
		l2=Len(cargs)
		For i=0 To l2-1
			nargs[i]=cargs[i]
		Next
		For i=0 To l1-1
			nargs[l2+i]=args[i]
		Next
		Return cfun( nargs )
	End Function
	
	Return apply
End Function

Function printall( args:Object[] )
	For s$=EachIn args
		Print s
	Next
End Function

Function applyfunc( fun(args:Object[]), args:Object[] )
	Return fun(args)
EndFunction

Local f(args:Object[])
f=curry(printall, ["hi"])
f=curry(f, ["bo"])
f(["there"])