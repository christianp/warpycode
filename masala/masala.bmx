Include "bignum.bmx"

' -----------------------------------
'   Generic functor
' -----------------------------------

Function apply:Object( f:TFunctor, args:Object[]=Null, scope:TScope=Null )
	'show f,"apply function: "
	If args
		'Print "args diff "+(Len(f.kind)-Len(args))
		'For f:TFunctor=EachIn args
		'	show f,"  "
		'Next
	EndIf
	If Not args args=New Object[0]
	Local nargs:Object[Len(args)+1]
	nargs[0]=f
	n=1
	For o:Object=EachIn args
		nargs[n]=o
		n:+1
	Next
	'show f
	Return f.apply( nargs, scope )
End Function

Type TFunctor
	Field kind$[]
	
	Function apply:Object( args:Object[]=Null, scope:TScope=Null )
	End Function

	Method reprkind$()
		Local kindtxt$=""
		For s$=EachIn kind
			If kindtxt kindtxt:+" -> "
			kindtxt:+s
		Next
		Return kindtxt
	End Method
	
	Method repr$()
		Return reprkind()
	End Method

	'Method repr$()
	'	Return name+" :: "+reprkind()
	'End Method
End Type

' ------------------------------------
'    Curried function
' ------------------------------------

Function curry:TCurry(f:TFunctor, args:Object[])
	If Not args args=New Object[0]
	Local nargs:Object[Len(args)+1]
	nargs[0]=f
	n=1
	For o:Object=EachIn args
		nargs[n]=o
		n:+1
	Next
	Local nkind$[]
	nkind=f.kind[Len(args)..]
	Return TCurry.Create( f.apply, nkind, nargs )
End Function

Type TCurry Extends TFunctor
	Field fun:Object(args:Object[], scope:TScope)
	Field pending:Object[]
	
	Function Create:TCurry(fun:Object(args:Object[], scope:TScope), kind$[], args:Object[]=Null)
		c:TCurry=New TCurry
		c.fun=fun
		c.kind=kind
		If Not args args=New Object[0]
		c.pending=args[..]
		Return c
	End Function
	
	Function apply:Object( args:Object[]=Null, scope:TScope=Null )
		If Not args
			args=New Object[0]
		EndIf
		'Print "curried"
		'For o:Object=EachIn args
		'	show o,"  "
		'Next
		c:TCurry=TCurry(args[0])
		'Print "apply "+c.repr()
		l1=Len(args)-1
		l2=Len(c.pending)
		Local nargs:Object[l1+l2]
		For i=0 To l2-1
			nargs[i]=c.pending[i]
		Next
		For i=0 To l1-1
			nargs[l2+i]=args[i+1]
		Next
		'Print "applying:"
		'For o:Object=EachIn nargs
		'	show(o)
		'Next
		Return c.fun( nargs, scope )
	End Function
	
End Type

' --------------------------------------
'   Integer type
' --------------------------------------
Function FInteger:TInteger( n:bignum )
	i:TInteger=New TInteger
	i.n=n
	Return i
End Function
Type TInteger Extends TFunctor
	Field n:bignum
	
	Method New()
		kind=["Integer"]
	End Method
	
	Function apply:Object( args:Object[]=Null, scope:TScope=Null )
		Return TInteger(args[0]).n
	End Function
	
	Method repr$()
		Return n.tostring()
	End Method
End Type

Function FAdd:Object( args:Object[], scope:TScope )
	f1:TFunctor=TFunctor(args[0])
	f2:TFunctor=TFunctor(args[1])
	n1:bignum=bignum(apply(f1,Null,scope))
	n2:bignum=bignum(apply(f2,Null,scope))
	Return FInteger( add(n1,n2) )
End Function

Function FSub:Object( args:Object[], scope:TScope )
	f1:TFunctor=TFunctor(args[0])
	f2:TFunctor=TFunctor(args[1])
	n1:bignum=bignum(apply(f1,Null,scope))
	n2:bignum=bignum(apply(f2,Null,scope))
	Return FInteger( sub(n1,n2) )
End Function

Function FMul:Object( args:Object[], scope:TScope )
	f1:TFunctor=TFunctor(args[0])
	f2:TFunctor=TFunctor(args[1])
	n1:bignum=bignum(apply(f1,Null,scope))
	n2:bignum=bignum(apply(f2,Null,scope))
	Return FInteger( mul(n1,n2) )
End Function

Function FDiv:Object( args:Object[], scope:TScope )
	f1:TFunctor=TFunctor(args[0])
	f2:TFunctor=TFunctor(args[1])
	n1:bignum=bignum(apply(f1,Null,scope))
	n2:bignum=bignum(apply(f2,Null,scope))
	Return FInteger( div(n1,n2) )
End Function

Function FPow:Object( args:Object[], scope:TScope )
	f1:TFunctor=TFunctor(args[0])
	f2:TFunctor=TFunctor(args[1])
	n1:bignum=bignum(apply(f1,Null,scope))
	n2:bignum=bignum(apply(f2,Null,scope))
	Return FInteger( pow(n1,n2) )
End Function

' --------------------------------------
'   String type
' --------------------------------------

Function FString:TString( txt$ )
	s:TString=New TString
	s.txt=txt
	Return s
End Function
Type TString Extends TFunctor
	Field txt$
	
	Method New()
		kind=["String"]
	End Method
	
	Function apply:Object( args:Object[]=Null, scope:TScope=Null )
		Return TString(args[0]).txt
	End Function
	
	Method repr$()
		Return "~q"+txt+"~q"
	End Method
End Type


' ------------------------------------
'   Data type
' ------------------------------------

Function FData:TData( args:Object[], scope:TScope )
	d:TData=New TData
	l=Len(args)
	Local kind$[l]
	For n=0 To l-1
		kind[n]=String(apply(TFunctor(args[n]),Null,scope))
	Next
	d.kind=kind
	Return d
End Function

Type TData Extends TFunctor
	Field values:TFunctor[]
	
	Function apply:Object(args:Object[]=Null, scope:TScope=Null)
		od:TData=TData(args[0])
		d:TData=New TData
		d.kind=od.kind[..]
		args=args[1..]

		'Print "constructing data"
		'For o:Object=EachIn args
		'	show o,"  "
		'Next
		d.values=New TFunctor[Len(args)]
		For i=0 To Len(args)-1
			d.values[i]=TFunctor(args[i])
		Next
		Return d
	End Function
	
	Method repr$()
		If values
			txt$=""
			For f:TFunctor=EachIn values
				If txt txt:+" , "
				txt:+f.repr()
			Next
			Return "{ "+txt+" }"
		Else
			Return reprkind()
		EndIf
	End Method
	
End Type


' ----------------------------------------------
'    Label type - for pattern matching and scope
' ----------------------------------------------


Function label:TLabel( pattern$, value:TFunctor )
	l:TLabel=New TLabel
	l.pattern=pattern
	l.value=value
	Return l
End Function

Type TLabel Extends TFunctor
	Field pattern$
	Field value:TFunctor
	
	Function apply:Object( args:Object[]=Null, scope:TScope=Null )
		l:TLabel=TLabel(args[0])
		Return l.value.apply( [l.value]+args[1..], scope )
	End Function
	
	Method repr$()
		Return pattern+" = "+value.repr()
	End Method
	
	Method matchname( name$ )
		If name=pattern Return 1
	End Method
	
	Method matchpattern(args:Object[])
		l=Len(value.kind)
		'Print value.repr()+" ??"
		If Len(args)<>l-1
			'Print "wrong number of arguments"
			Return 0
		EndIf
		For n=0 To l-2
			If value.kind[n]<>TFunctor(args[n]).reprkind()
				'Print "~q"+value.kind[n]+"~q <> ~q"+TFunctor(args[n]).reprkind()+"~q"
				Return 0
			EndIf
		Next
		'Print "match!"
		Return 1
	End Method
End Type

' ------------------------------------------
'    Scope type
' ------------------------------------------
Function FScope:TSCope(prv:TScope=Null)
	s:TScope=New TScope
	s.prv=prv
	Return s
End Function
Type TScope
	Field prv:TScope
	Field labels:TList
	
	Method New()
		labels=New TList
	End Method
	
	Method addlabel(l:TLabel)
		Print "adding label "+l.repr()
		For l2:TLabel=EachIn labels
			If l2.pattern=l.pattern
				labels.remove l2
				Print "trimming old label: "+l2.repr()
			EndIf
		Next
		labels.addlast l
	End Method
	
	Method matchname:TFunctor(name$)
		For l:TLabel=EachIn labels
			If l.matchname( name )
				Return l.value
			EndIf
		Next
		If prv
			f:TFunctor=prv.matchname(name)
			If f Return f
		Else
			f:TFunctor=matchbuiltins(name)
			If f Return f
		EndIf
		Print "no matching name: "+name
	End Method
	
	Method matchpattern:TFunctor(args:Object[])
		'txt$=""
		'For f:TFunctor=EachIn args
		'	If txt txt:+", "
		'	txt:+"("+f.repr()+")"
		'Next
		'Print "looking for match of "+txt
	
		For l:TLabel=EachIn labels
			If l.matchpattern( args )
				Return l.value
			EndIf
		Next
		If prv
			f:TFunctor=prv.matchpattern( args )
			If f Return f
		EndIf
		Print "no matching pattern"
	End Method
End Type

Function getlabel:Object(args:Object[], scope:TScope)
	pattern$=String(args[0])
	args=args[1..]
	f:TFunctor=scope.matchname( pattern )
	'Print "looking for "+pattern
	If f
		'show f,"found "
		Return apply(f,args,scope)
		'Return f
	EndIf
End Function


' ------------------------------------------
'    Function type
' ------------------------------------------
Function FFunction:TFunction( labels$[], f:TFunctor )
	fn:TFunction=New TFunction
	l=Len(labels)
	fn.labels=New String[l]
	fn.kind=New String[l]
	Local bits$[]
	For n=0 To l-1
		bits=labels[n].split(" ")
		fn.kind[n]=bits[0]
		fn.labels[n]=bits[1]
	Next
	fn.kind=fn.kind+f.kind
	fn.f=f
	Return fn
End Function
Type TFunction Extends TFunctor
	Field labels$[]
	Field f:TFunctor
	
	Function apply:Object( args:Object[]=Null, scope:TScope=Null )
		fn:TFunction=TFunction(args[0])
		args=args[1..]
		'Print "applying "+fn.f.repr()
		'Print "labels:"
		'For pattern$=EachIn fn.labels
		'	show pattern,"  "
		'Next
		'Print "args:"
		'For o:Object=EachIn args
		'	show o,"  "
		'Next
		scope=FScope(scope)
		l=Len(fn.labels)
		'Print l
		For n=0 To l-1
			scope.addlabel( label(fn.labels[n],TFunctor(args[n])) )
		Next
		'show scope
		args=[fn.f]+args[l..]
		Return fn.f.apply( args, scope )
	End Function
	
	Method repr$()
		'Return Super.repr()
		labelstxt$=""
		kindtxt$=""
		For n=0 To Len(labels)-1
			If labelstxt labelstxt:+", "
			If kindtxt kindtxt:+" -> "
			labelstxt:+"("+kind[n]+" "+labels[n]+")"
			kindtxt:+labels[n]
		Next
		If kindtxt kindtxt:+" -> "
		kindtxt:+f.reprkind()
		Return labelstxt+" => "+kindtxt
	End Method
End Type


' ------------------------------------------
'    Make a text representation of an object
' ------------------------------------------
Function show(o:Object,spaces$="")
	If String(o)
		Print spaces+"string: "+String(o)
	ElseIf bignum(o)
		Print spaces+bignum(o).tostring()
	ElseIf TScope(o)
		s:TScope=TScope(o)
		Print spaces+"Scope:"
		For l:TLabel=EachIn s.labels
			show l,spaces+"  "
		Next
	ElseIf TFunctor(o)
		Print spaces+TFunctor(o).repr()
	ElseIf TCurry(o)
		c:TCurry=TCurry(o)
		Print spaces+c.repr()
	ElseIf Object[](o)
		Print spaces+"object array:"
		Local oarr:Object[]=Object[](o)
		Print Len(oarr)
		Return
		If Len(oarr)
			For o2:Object=EachIn oarr
				show o,spaces+"  "
			Next
		EndIf
	ElseIf String[](o)
		Print "string array:"
		For txt$=EachIn String[](o)
			show txt,spaces+"  "
		Next
	Else
		Print "NULL"
	EndIf
End Function

Function showarray( arr:Object[], title$="", spaces$="")
	If title 
		Print spaces+title
		spaces:+"  "
	EndIf
	For o:Object=EachIn arr
		show o,spaces
	Next
End Function


' ----------------------------------------
'   Builtins
' ----------------------------------------

Function SetLabel:Object( args:Object[], scope:TScope )
	Print "Set Label"
	showarray args,"Args"
	
	If Len(args)=2
		l:TLabel=TLabel(args[0])
		name$=l.pattern
		
		f:TFunctor=TFunctor(args[1])
		
		'Print "let "+name+" = "+f.repr()
		
		l:TLabel=label(name,f)
		
	Else
		numargs=Len(args)
		f:tFunctor=TFunctor(args[numargs-1])
		name$=TLabel(args[0]).pattern
		Local labels$[(numargs-2)/2]
		i=0
		For n=1 To numargs-2 Step 2
			kind$=TLabel(args[n]).pattern
			lname$=TLabel(args[n+1]).pattern
			labels[i]=kind+" "+lname
			i:+1
		Next
		showarray labels,name+" => "+f.repr()
		fn:TFunction=FFunction( labels, f )
		l:TLabel=label(name, fn)
	EndIf
	scope.addlabel l
	Return l
End Function

Global seteq:TCurry=TCurry.Create( SetLabel, ["String","Functor","Label"] )


Global getl:TCurry=TCurry.Create( getlabel, ["String","Functor"] )
'FString txt$
'FInteger n:bignum

Global intadd:TCurry=TCurry.Create( FAdd, ["Integer","Integer","Integer"] )
Global intsub:TCurry=TCurry.Create( FSub, ["Integer","Integer","Integer"] )
Global intmul:TCurry=TCurry.Create( FMul, ["Integer","Integer","Integer"] )
Global intdiv:TCurry=TCurry.Create( FDiv, ["Integer","Integer","Integer"] )
Global intpow:TCurry=TCurry.Create( FPow, ["Integer","Integer","Integer"] )

Global data:TCurry=TCurry.Create( FData, ["*String","Type"])

Function isstring( token$ )
	If token[0]<>34 Return 0
	n=1
	While token[n]<>34
		n:+1
		If n=Len(token) Return 0
	Wend
	If n<Len(token)-1 Return 0
	Return 1
End Function

Function isinteger( token$ )
	For n=0 To Len(token)-1
		If token[n]<48 Or token[n]>57 Return 0
	Next
	Return 1
End Function

Function matchbuiltins:TFunctor( name$ )
	If isstring(name)
		Return FString(name[1..Len(name)-1])
	ElseIf isinteger(name)
		Return FInteger( bignum.fromstring(name) )
	Else
		Select name
		Case "="
			Return seteq
		Case "getl"
			Return getl
		Case "+"
			Return intadd
		Case "-"
			Return intsub
		Case "*"
			Return intmul
		Case "/"
			Return intdiv
		Case "^"
			Return intpow
		Case "data"
			Return data
		End Select
	EndIf
End Function