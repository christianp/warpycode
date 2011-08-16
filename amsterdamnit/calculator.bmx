'-------------------------------------

Function interpret(g:grammar)
	'Local param$[2]
	'For param=EachIn g.params
	'	Print param[0]+" : "+param[1]
	'Next
	command$=s.nextparam()
	'Print "command: "+command
	Select command
	Case "add"
		v1:variable=resolve(s.nextparam())
		v2:variable=resolve(s.nextparam())
		v2.value:+v1.value
		lines.addlast v1.name+" + "+v2.name+" = "+String(v2.value)
	Case "subtract"
		v1:variable=resolve(s.nextparam())
		v2:variable=resolve(s.nextparam())
		v2.value:-v1.value
		lines.addlast v2.name+" - "+v1.name+" = "+String(v2.value)
	Case "multiply"
		v1:variable=resolve(s.nextparam())
		v2:variable=resolve(s.nextparam())
		v1.value:*v2.value
		lines.addlast v1.name+" * "+v2.name+" = "+String(v1.value)
	Case "divide"			
		v1:variable=resolve(s.nextparam())
		v2:variable=resolve(s.nextparam())
		v1.value:/v2.value
		lines.addlast v1.name+" / "+v2.name+" = "+String(v1.value)
	Case "let"
		varname$=s.nextparam()
		dname$=s.nextparam()
		v:variable=variable.find(varname)
		v2:variable=resolve(dname)
		v.value=v2.value
		lines.addlast varname+" = "+v.value
	End Select
End Function

Function clue$(w$)
	Select w
	Case "variable"
		Return "a variable name"
	Case "data"
		Return "a number or a variable name"
	Default
		Return "anything"
	End Select
End Function

Function resolve:variable(dname$)
	If Float(dname)<>0 Or (Float(dname)=0 And dname="0")
		Return variable.Create(dname,Float(dname))
	Else
		v:variable=variable.find(dname)
		Return v
	EndIf
End Function
Type variable
	Field name$
	Field value#
	Function Create:variable(name$,value#)
		v:variable=New variable
		v.name=name
		v.value=value
		Return v
	End Function
	Function find:variable(name$)
		If vars.contains(name)
			Return variable(vars.valueforkey(name))
		Else
			v:variable=variable.Create(name,0)
			vars.insert(name,v)
			Return v
		EndIf
	End Function
			
End Type

Global vars:Tmap=New tmap

