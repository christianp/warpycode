Global grammars:tmap
Type grammar
	Field symbols:tmap
	Field name$
	
	Method New()
		symbols=New tmap
	End Method
	
	Function find:grammar(name$)
		If grammars.contains(name)
			Return grammar(grammars.valueforkey(name))
		EndIf
	End Function
	
	Function loadall()
		grammars=New tmap
		For path$=EachIn crawldir("grammars")
			grammar.fromfile(path)
		Next
	End Function
	
	Method addrules(in$)
		Local lines$[]=in.split(";")
		For line$=EachIn lines
			If line And line[Len(line)-1]=Asc(";")
				line=line[..Len(line)-1]
			EndIf
			If Trim(line)
				addrule line
			EndIf
		Next
	End Method
	
	Function fromfile:grammar(fname$,name$="")
		If Not name
			name=filename(fname)
		EndIf
		g:grammar=New grammar
		g.addfile fname
		g.name=name
		grammars.insert name,g
		Return g
	End Function
	
	Method addfile(fname$)
		addrules loadtxt(fname)
	End Method
	
	Method addrule(in$)
		in=ltrim(in)
		'Print "ADD RULE| "+in
		c=0
		While c<Len(in) And in[c]<>58
			c:+1
		Wend
		If c=Len(in)
			'Print "expecting :, didn't find it"
			Return
		EndIf
		
		Local sections$[]=in.split("::")
		name$=sections[0]
		If Len(sections)=2
			rule$=sections[1]
			category$=""
		Else
			category$=sections[1]
			'Print "category| "+category
			rule$=sections[2]
		EndIf
		'name$=Trim(in[..c])
		'rule$=lTrim(in[c+1..])
		name=Trim(name)
		rule=ltrim(rule,1)
		category=Trim(category)
		Local terms$[]=rule.split("~~") 'note: not two tildes in a row, this is a blitz escaped character
		mode=1
		l:TList=New TList
		For term$=EachIn terms
			If mode
				If term Or Len(terms)=1
					'Print "  add text| '"+term+"'"
					l.addlast gtext.Create(term)
				EndIf
			Else
				'Print "  add symbol| "+term
				If term[Len(term)-1]=Asc("*")
					l.addlast gmultisymbol.Create(findsymbol(term[..Len(term)-1]))
				Else
					l.addlast findsymbol(term)
				EndIf
			EndIf
			mode=1-mode
		Next
		Local bits:grule[l.count()]
		i=0
		For r:grule=EachIn l
			bits[i]=r
			i:+1
		Next
		findsymbol(name).addoption gseries.Create(bits,category)
	End Method
	
	Method findsymbol:gsymbol(name$)
		name=Lower(name)
		If specialsymbols.contains(name)
			Return gsymbol(specialsymbols.valueforkey(name))
		EndIf
		If symbols.contains(name)
			Return gsymbol(symbols.valueforkey(name))
		Else
			y:gsymbol=gsymbol.Create(name)
			symbols.insert name,y
			Return y
		EndIf
	End Method
	
	Method match:sentence(in$,startsymbol$="$")
		Local remainder:grule[]
		sn:sentence=New sentence
		findsymbol(startsymbol).match(in,sn)
		Return sn.getsymbol(startsymbol)
	End Method
	
	Method options:TList(in$,startsymbol$="$")
		l:TList=findsymbol(startsymbol).options(in)
		o:TList=New TList
		For option$=EachIn l
			If Not o.contains(option)
				o.addlast option
			EndIf
		Next
		Return o
	End Method
	
	Method fill$(startsymbol$="$")
		in$=""
		l:TList=options(in,startsymbol)
		While l.count()
			in:+String(picklist(l))
			l=options(in,startsymbol)
		Wend
		Return in
	End Method
End Type

Type grule
	
	Method match$(in$,sn:sentence,depth$="") Abstract
	
	Method options:TList(in$,depth$="") Abstract
End Type

Type gseries Extends grule
	Field bits:grule[]
	Field category$
	
	Function Create:gseries(bits:grule[],category$="")
		s:gseries=New gseries
		s.bits=bits
		s.category=category
		Return s
	End Function

	Method match$(in$,sn:sentence,depth$="")
		gdebugo depth+"series match ("+category+")| "+in
		o$=""
		If category
			sn.category=category
			'Print "SET CATEGORY| "+category
		EndIf
		For r:grule=EachIn bits
			res$=r.match(in,sn,depth+"  ")
			If Not res Return ""
			If res=~0 res=""
			o:+res
			in=in[Len(res)..]	'sketchy as, depends on not replacing matched text with something of different length
		Next
		If o="" o=~0
		Return o
	End Method
	
	Method options:TList(in$,depth$="")
		sn:sentence=New sentence
		gdebugo depth+"series options| "+in
		For r:grule=EachIn bits
			res$=r.match(in,sn,depth+"  ")
			If res
				If res=~0 res=""
				in=in[Len(res)..]
			Else
				'print depth+"no match, get options ("+in+")"
				'If Trim(in)
				'	Return New TList
				'Else
					Return r.options(in,depth+"  ")
				'EndIf
			EndIf
		Next
		Return New TList
	End Method
End Type

Type gtext Extends grule
	Field txt$
	Field reptxt$
	
	Function Create:gtext(txt$)
		t:gtext=New gtext
		t.txt=txt
		If txt=""
			t.reptxt=~0
		Else
			t.reptxt=txt
		EndIf
		Return t
	End Function

	Method match$(in$,sn:sentence,depth$="")
		gdebugo depth+"text match ("+txt+")| '"+in+"'"
		'Print depth+"in ~q"+Lower(in[0..Len(txt)])+"~q"
		'Print depth+"txt~q"+Lower(txt)+"~q "+Len(txt)
		If Len(in)>=Len(txt) And Lower(in[..Len(txt)])=Lower(txt)
			'print depth+"text match"
			If txt
				sn.addparam sentence.Create("",txt)
			EndIf
			Return reptxt
		Else
			'print depth+"text no match"
			Return ""
		EndIf
	End Method
	
	Method options:TList(in$,depth$="")
		gdebugo depth+"text options| "+in
		l:TList=New TList
		If in
			If Len(in)<Len(txt) And Lower(txt[..Len(in)])=Lower(in)
				l.addlast txt[Len(in)..]
			EndIf
		Else
			l.addlast txt
		EndIf
		Return l
	End Method
End Type

Type gsymbol Extends grule
	Field name$
	Field rules:TList
	
	Method New()
		rules=New TList
	End Method
	
	Function Create:gsymbol(name$)
		y:gsymbol=New gsymbol
		y.name=name
		Return y
	End Function
	
	Method addoption(r:grule)
		rules.addlast r
	End Method
	
	Method match$(in$,sn:sentence,depth$="")
		gdebugo depth+"symbol match ("+name+")| "+in

		'info$=game.getinfo(name)
		If info
			sn2:sentence=New sentence
			res$=gtext.Create(info).match(in,sn2,depth+"  ")
			If res
				sn.addparam sentence.Create(name,info)
			EndIf
			Return res
		EndIf
		sn2:sentence=sentence.Create(name,"")
		matches=0
		mres$=""
		For r:grule=EachIn rules
			'sn3:sentence=New sentence
			sn3:sentence=sentence.Create(name,"")
			res$=r.match(in,sn3,depth+"  ")
			If res
				'gdebugo depth+"add sn2"
				'For sn4:sentence=EachIn sn3.params
				'	sn2.addparam sn4
				'Next
				'If sn3.category
				'	sn2.category=sn3.category
				'EndIf
				If Len(res)>Len(mres) 
					sn2=sn3
					mres=res
				EndIf
				matches:+1
			EndIf
		Next
		If matches
			gdebugo depth+" max match|"+mres
			sn.addparam sn2
			Return mres
		Else
			Return ""
		EndIf
	End Method
	
	Method options:TList(in$,depth$="")
		gdebugo depth+"symbol options <"+name+">| "+in
		l:TList=New TList
		'info$=game.getinfo(name)
		If info
			For txt$=EachIn gtext.Create(info).options(in,depth+"  ")
				l.addlast txt
			Next
		EndIf
		For r:grule=EachIn rules
			For txt$=EachIn r.options(in,depth+"  ")
				l.addlast txt
			Next
		Next
		Return l
	End Method
End Type

Type gmultisymbol Extends grule
	Field y:gsymbol
	
	Function Create:gmultisymbol(y:gsymbol)
		ms:gmultisymbol=New gmultisymbol
		ms.y=y
		Return ms
	End Function
	
	Method match$(in$,sn:sentence,depth$="")
		gdebugo depth+"multisymbol match| "+in
		res$=y.match(in,sn,depth)
		o$=~0
		While res
			If res=~0
				res=""
				If o="" o=~0
			Else
				If o=~0
					o=""
				EndIf
				o:+res
			EndIf
			in=in[Len(res)..]
			res=y.match(in,sn,depth)
		Wend
		Return o
	End Method
	
	Method options:TList(in$,depth$="")
		gdebugo depth+"multisymbol options| "+in
		Return y.options(in,depth+"  ")
	End Method
End Type

Type gspecialsymbol Extends gsymbol
	Field name$
	Field mymatch$(in$,sn:sentence,depth$)
	Field myoptions:TList(in$,depth$)
		
	Method match$(in$,sn:sentence,depth$="")
		gdebugo depth+"specialsymbol match ("+name+")| "+in
		Return mymatch(in,sn,depth)
	End Method
	
	Method options:TList(in$,depth$="")
		Return myoptions(in,depth)
	End Method
End Type

Function addspecialsymbol( name$, match$(in$,sn:sentence,depth$), options:TList(in$,depth$) )
	g:gspecialsymbol=New gspecialsymbol
	g.mymatch=match
	g.myoptions=options
	g.name=name
	specialsymbols.insert name,g
End Function

Function numbermatch$(in$,sn:sentence,depth$="")
	If Not in Return
	If in[0]=Asc("-")
		res$=numbermatch(in[1..],New sentence,depth+"  ")
		If res
			res="-"+res
			sn.addparam sentence.Create("number",res,res)
			Return res
		EndIf
	Else
		c=0
		While c<Len(in)
			If in[c]<>46 And (in[c]<48 Or in[c]>57)	'non-number character
				If c	'already had some numbers
					sn.addparam sentence.Create("number",in[..c],in[..c])
					Return in[..c]
				Else
					Return ""
				EndIf
			Else
				c:+1
			EndIf
		Wend
		sn.addparam sentence.Create("number",in,in)
		Return in
	EndIf
End Function

Function numberoptions:TList(in$,depth$="")
	l:TList=New TList
	For c=0 To Len(in)-1
		If in[c]<>46 And (in[c]<48 Or in[c]>57)
			Return l
		EndIf
	Next
	If Not in.contains(".") 
		l.addlast "."
	EndIf
	For c=0 To 9
		l.addlast String(c)
	Next
	Return l
End Function

Function alphanummatch$(in$,sn:sentence,depth$="")
	c=0
	While c<Len(in)
		If in[c]<48 Or (in[c]>57 And in[c]<65) Or (in[c]>90 And in[c]<97) Or in[c]>122	'not alphanum character
			If c
				res$=in[..c]
				sn.addparam sentence.Create("alphanum",res,res)
				Return res
			Else
				Return ""
			EndIf
		Else
			c:+1
		EndIf
	Wend
	If c
		sn.addparam sentence.Create("alphanum",in,in)
		Return in
	EndIf
End Function

Function alphanumoptions:TList(in$,depth$="")
	l:TList=New TList
	For c=0 To Len(in)-1
		If in[c]<48 Or (in[c]>57 And in[c]<65) Or (in[c]>90 And in[c]<97) Or in[c]>122	'not alphanum character
			Return l
		EndIf
	Next
	Function addrange(l:TList,s,e)
		For c=s To e
			l.addlast Chr(c)
		Next
	End Function
	
	addrange l,48,57
	addrange l,65,90
	addrange l,97,122
	
	Return l
End Function

Global specialsymbols:tmap=New tmap
addspecialsymbol "number",numbermatch,numberoptions
addspecialsymbol "alphanum",alphanummatch,alphanumoptions

Type sentence
	Field symbol$,txt$
	Field params:TList
	Field category$
	
	Method New()
		params=New TList
	End Method
	
	Function Create:sentence(symbol$,txt$,category$="")
		sn:sentence=New sentence
		sn.symbol=symbol
		sn.txt=txt
		sn.category=category
		Return sn
	End Function
	
	Method addparam(sn:sentence)
		If Not sn Return
		'gdebugo "<<addparam>> "+sn.symbol+"|"+sn.category+"|"+sn.txt
		params.addlast sn
	End Method
	
	Method repr$(indent$="")
		s$=indent
		If symbol
			s:+symbol
		EndIf
		If category
			s:+" <"+category+"> "
		EndIf
		If txt
			s:+"~q"+txt+"~q"
		EndIf
		s:+"("+value()+")"
		For sn:sentence=EachIn params
			s:+"~n"+sn.repr(indent+"~t")
		Next
		Return s
	End Method
	
	Method value$()
		s$=txt
		For sn:sentence=EachIn params
			s:+sn.value()
		Next
		Return s
	End Method
	
	Method getparam$(name$)
		sn:sentence=getsymbol(name)
		If sn
			Return sn.category
		EndIf
	End Method
	
	Method nextsymbol:sentence(name$="")
		If name
			sn:sentence=getsymbol(name)
		Else
			For sn:sentence=EachIn params
				If sn.symbol Exit
			Next
		EndIf
		If sn
			params.remove sn
			Return sn
		EndIf
	End Method
	
	Method nextparam$(name$="")
		sn:sentence=nextsymbol(name)
		If sn
			Return sn.category
		EndIf
	End Method
	
	Method nextvalue$(name$="")
		sn:sentence=nextsymbol(name)
		If sn
			Return sn.value()
		EndIf
	End Method
		
	Method getsymbol:sentence(name$)
		For sn:sentence=EachIn params
			If sn.symbol=name Return sn
		Next
	End Method
	
	Method symbols:TList()
		l:TList=New TList
		For sn:sentence=EachIn params
			If sn.symbol
				l.addlast sn
			EndIf
		Next
		Return l
	End Method
End Type


Function ltrim$(in$,stop=-1)
	c=0
	If stop=-1 stop=Len(in)
	While c<stop And in[c]<=32
		c:+1
	Wend
	Return in[c..]
End Function


Global gdebugging=0
Function gdebugo(txt$)
	If gdebugging
		Print txt
	EndIf
End Function





Rem
For path$=EachIn crawldir("grammars")
	g:grammar=New grammar
	g.addfile path

	SeedRnd MilliSecs()
	For c=1 To 5
		Print g.fill()
	Next
Next
EndRem


Function crawldir:TList(path$,l:TList=Null)
	If Not l
		l:TList=New TList
	EndIf
	For t$=EachIn LoadDir(path)
		If Not (t="." Or t="..")
			fpath$=path+"/"+t
			Select FileType(fpath)
			Case 1
				l.addlast fpath
			Case 2
				crawldir fpath,l
			End Select
		EndIf
	Next
	Return l
End Function

Function filename$(path$)
	Local bits$[]=path.split("/")
	name$=bits[Len(bits)-1]
	c=Len(name)-1
	While c>=0 And name[c]<>46
		c:-1
	Wend
	If c=-1 Return name
	Return name[..c]
End Function

Function loadtxt$(filename$)
	f:TStream=ReadFile(filename)
	txt$=f.ReadString(StreamSize(f))
	CloseFile(f)
	Return txt
End Function

Function picklist:Object(l:TList,f!(min_value!,max_value!)=Rnd)
	n=f(0,1)*l.count()
	If n=l.count() n:-1
	Return l.valueatindex(n)
End Function

Function pluralise$(name$)
	If Chr(name[Len(name)-1])="s"
		Return name+"es"
	Else
		Return name+"s"
	EndIf
End Function

Function capitalise$(name$)
	If Len(name)=1 Return name
	Return Upper(name[..1])+name[1..]
End Function

Function indent$(txt$,tabs)
	o$=""
	t$=""
	For c=1 To tabs
		t:+"~t"
	Next
	For line$=EachIn txt.split("~n")
		If o o:+"~n"
		o:+t+line
	Next
	Return o
End Function

Function tabcount(in$)
	If Not in Return 0
	c=0
	While in[c]=9
		c:+1
	Wend
	Return c
End Function


'--------------------------------

Global rewrites:tmap=New tmap
rewrites.insert "velocity","v"
rewrites.insert "position","p"
Function rewrite$(name$)
	If rewrites.contains(name)
		Return String(rewrites.valueforkey(name))
	Else
		Return name
	EndIf
End Function


Global things:tmap
Global curthing:tthing
Type tthing
	Field name$
	Field isa$
	Field fields:tmap
	Field requires:TList
	Field actions:tmap
	Field inherits:TList
	Field belongs:TList
	Field possessions:tmap
	Field islisted
	Field kindof
	
	Method New()
		fields=New tmap
		actions=New tmap
		inherits=New TList
		belongs=New TList
		requires=New TList
		possessions=New tmap
	End Method
	
	Function find:tthing(name$)
		name=capitalise(name)
		If things.contains(name)
			Return tthing(things.valueforkey(name))
		EndIf
	End Function
	
	Function Create:tthing(name$,isa$)
		name=capitalise(name)
		If things.contains(name)
			Print "already defined "+name+"!"
			Return tthing.find(name)
		EndIf
		t:tthing=New tthing
		t.name=name
		t.isa=isa
		Return t
	End Function
	
	
	Method addfield(fieldname$,fieldtype$="")
		'Print "~tadd field "+fieldname+" : "+fieldtype
		fields.insert fieldname,fieldtype
	End Method
	
	Method addaction( a:taction )
		actions.insert a.name, a
	End Method
	
	Method addinheritance(name$)
		inherits.addlast name
	End Method
	
	Method addbelong(name$)
		belongs.addlast capitalise(name)
	End Method
	
	Method addrequirement(name$)
		requires.addlast name
	End Method
	
	Method addpossession(pname$,ptype$="")
		possessions.insert capitalise(pname),ptype
	End Method
	
	
	Function fieldrepr$(fieldname$,fieldtype$="")
		Select fieldtype$
		Case "string"
			o$=fieldname+"$"
		Case "number"
			o$=fieldname+"#"
		Case "vector"
			o$=fieldname+"X#, "+fieldname+"Y#"
		Case "flag"
			o$=fieldname
		Case ""
			t:tthing=tthing.find(fieldname)
			If t.kindof
				Return fieldrepr(t.name,t.name)
			Else
				Return fieldrepr(t.name,t.isa)
			EndIf
		Default
			o$=fieldname+":T"+capitalise(fieldtype)
		End Select
		Return o
	End Function
	
	Method repr$()
		Global o$
		o=""
		Function addline(line$="")
			If Not line
				o:+"~n"
			Else
				If o o:+"~n"
				o:+line
			EndIf
		End Function
		
		If islisted
			addline "Global "+pluralise(name)+":TList=New TList"
		EndIf
		
		Select isa
		Case "string","vector","list","number","basis","flag"
			Return
		Default
			addline "Type T"+name
		End Select
		
		If kindof
			o:+" Extends T"+capitalise(isa)
		EndIf
		
		For fieldname$=EachIn fields.keys()
			fieldtype$=String(fields.valueforkey(fieldname))
			addline "~tField "+tthing.fieldrepr(fieldname,fieldtype)
		Next
		
		pinitcode$=""
		For pname$=EachIn possessions.keys()
			ptype$=String(possessions.valueforkey(pname))
			If ptype
				listname$=pluralise(pname)
			Else
				listname$="my"+pname
			EndIf
			addline "~tField "+listname+":TList"
			pinitcode:+"~n~t~t"+listname+" = New TList"
		Next
		
		addline
		addline "~tMethod New()"
		If islisted
			addline "~t~t"+pluralise(name)+".addlast Self"
		EndIf
		'For fieldname$=EachIn possessions
		'	addline "~t~tmy"+fieldname+" = New TList"
		'Next
		addline pinitcode
		addline "~tEnd Method"
		addline
		
		reqs$=""
		For reqname$=EachIn requires
			reqtype$=String(fields.valueforkey(reqname))
			If reqs reqs:+", "
			reqs:+tthing.fieldrepr(reqname,reqtype)
		Next
		addline "~tFunction Create"+name+":T"+name+"( "+reqs+" )"
		addline
		addline "~tEnd Function"
		addline
		
		For a:taction=EachIn actions.values()
			addline a.repr(Self)
		Next
		
		'If Not kindof
			t:tthing=tthing.find(isa)
			If t
				For a:taction=EachIn t.actions.values()
					If Not actions.contains(a.name)
						addline a.repr(t)
					EndIf
				Next
			EndIf
		'EndIf
				
		addline "End Type"
		addline
		
		Return o
	End Method
End Type

Global curaction:taction,mainloop:taction
Type taction
	Field name$
	Field instructions:TList
	Field takes:TList
	Field startline$,endline$
	
	Method New()
		instructions=New TList
		takes=New TList
	End Method
	
	Method addinstruction( i:tinstruction )
		instructions.addlast i
	End Method
	
	Method addparam( name$ )
		'Print "addparam "+name
		takes.addlast name
	End Method
	
	Function Create:taction(name$,startline$="",endline$="")
		a:taction=New taction
		a.name=name
		If startline
			a.startline=startline
			a.endline=endline
		EndIf
		Return a
	End Function
	
	Method repr$(p:tthing)
		o$=""
		If Not startline
			params$=""
			For pname$=EachIn takes
				If params params:+", "
				params:+p.fieldrepr(pname)
			Next
			If params params=" "+params+" "
			startline="~tMethod "+capitalise(name)+"("+params+")"
			endline="~tEnd Method"
		EndIf
		o:+"~n"+startline
		tabs=tabcount(startline)
		For i:tinstruction=EachIn instructions
			o:+"~n"+indent(i.repr(p),tabs+1)
		Next
		o:+"~n"+endline
		o:+"~n"
		Return o
	End Method
End Type

Type tinstruction
	Method repr$(p:tthing) Abstract
End Type

Type tcodeinstruction Extends tinstruction
	Field code$
	
	Function Create:tcodeinstruction(code$)
		i:tcodeinstruction=New tcodeinstruction
		i.code=code
		Return i
	End Function
	
	Method repr$(p:tthing)
		Return code
	End Method
End Type

Type tmakeinstruction Extends tinstruction
	Field name$
	
	Function Create:tmakeinstruction(name$)
		i:tmakeinstruction=New tmakeinstruction
		i.name=name
		Return i
	End Function
	
	Method repr$(p:tthing)
		t:tthing=tthing.find(name)
		o$=name+":T"+capitalise(name)+" = New T"+capitalise(name)
		For iname$=EachIn t.inherits
			t2:tthing=tthing.find(iname)
			itype$=t2.isa
			Select itype
			Case "vector"
				o:+"~n"+name+"."+iname+"X = "+iname+"X"
				o:+"~n"+name+"."+iname+"Y = "+iname+"Y"
			Default
				o:+"~n"+name+"."+iname+" = "+iname
			End Select
		Next
		If t.belongs.contains(p.name)
			o:+"~n"+name+".parent"+p.name+" = Self"
		EndIf
		If p.possessions.contains(pluralise(t.name))
			o:+"~nmy"+pluralise(t.name)+".addlast "+name
		EndIf
		
		Return o
			
	End Method
End Type

Type taddvectorinstruction Extends tinstruction
	Field name1$,name2$
	
	Function Create:taddvectorinstruction(name1$,name2$)
		i:taddvectorinstruction = New taddvectorinstruction
		i.name1=name1
		i.name2=name2
		Return i
	End Function
	
	Method repr$(p:tthing)
		o$=""
		o:+name2+"X :+ "+name1+"X"
		o:+"~n"+name2+"Y :+ "+name1+"Y"
		Return o
	End Method
End Type

Type tdoeveryinstruction Extends tinstruction
	Field verb$,name$
	Field doother
	
	Function Create:tdoeveryinstruction(verb$,name$,doother=0)
		i:tdoeveryinstruction=New tdoeveryinstruction
		i.verb=verb
		i.name=name
		i.doother=doother
		Return i
	End Function
	
	Method repr$(p:tthing)
		If tthing.find(name)
			t:tthing=tthing.find(name)
			typename$="T"+t.name
			listname$=pluralise(name)
		ElseIf p
			If p.possessions.contains(capitalise(name))
				t:tthing=tthing.find(String(p.possessions.valueforkey(capitalise(name))))
				typename$="T"+t.name
				listname$=pluralise(name)
			EndIf
		EndIf
		listname=capitalise(listname)
		o$="For "+name+":"+typename+"=EachIn "+listname
		If p
			fncall$=verb+" "+name
		Else
			fncall$=name+"."+verb
		EndIf
		If doother
			o:+"~n~tIf "+name+" <> Self"
			o:+"~n~t~t"+fncall
			o:+"~n~tEndif"
		Else
			o:+"~n~t"+fncall
		EndIf
		o:+"~nNext~n"
		Return o
	End Method
End Type

grammars=New tmap
Global sketchgrammar:grammar=grammar.fromfile("sketchgrammar.txt")

makesketch AppArgs[1]

Function makesketch(fname$)
	Local lines$[]=loadtxt(fname).split("~n")
	things=New tmap
	mainloop=taction.Create("mainloop","While Not (KeyHit(KEY_ESCAPE) or AppTerminate())","Wend")
	readsketch lines
	
	mainloop.addinstruction tcodeinstruction.Create("Flip~nCls")
	
	f:TStream=WriteFile(fname[..Len(fname)-3]+"bmx")
	
	
	l:TList=New TList
	For tname$=EachIn things.keys()
		t:tthing=tthing(things.valueforkey(tname))
		If Not l.contains(t)
			s$=t.repr()
			If s
				f.WriteLine s
			EndIf
			l.addlast t
		EndIf
	Next
	
	f.WriteLine "~nGraphics 600,600,0~nSetblend ALPHABLEND~n"
	f.WriteLine mainloop.repr(Null)
	CloseFile f
End Function



Function readsketch(lines$[],tabs=0)
	While Len(lines)
		cmd$=ltrim(lines[0])
		'Print "<<<"+cmd
		If cmd
			sn:sentence=sketchgrammar.match(cmd)
			enformulate sn
			c=1
			n=tabcount(lines[0])
			While c<Len(lines) And tabcount(lines[c])>n
				c:+1
			Wend

			If c>1
				readsketch lines[1..c],tabs+1
			EndIf
			'If c<Len(lines)
				lines=lines[c..]
			'EndIf
		Else
			lines=lines[1..]
		EndIf
	Wend
End Function
	
Function enformulate(sn:sentence)
	'Print ">>>"+sn.category
	Select sn.category
			
	'main loop
	Case "mainloop"
		curaction=mainloop
	
	'define a new type
	Case "isa","kindof"
		name1$=rewrite(sn.nextvalue("thingname"))
		name2$=rewrite(sn.nextvalue("thingname"))
		'Print name1
		'Print name2
		curthing:tthing=tthing.Create(name1,name2)
		things.insert curthing.name,curthing
		things.insert pluralise(curthing.name),curthing
		If sn.category="kindof"
			curthing.kindof=1
		EndIf
		'If rewrites.contains(fieldname)
		'	fieldname=String(rewrites.valueforkey(fieldname))
		'EndIf
		
	'add properties to a type
	Case "hasa","requiresa"
		sn2:sentence=sn.nextsymbol("property")
		'Print sn2.category
		name$=rewrite(sn2.nextvalue("thingname"))
		Select sn2.category
		Case "single"
			'print name
			curthing.addfield name
		Case "double"
			name2$=rewrite(sn2.nextvalue("thingname"))
			'print name
			'print name2
			curthing.addfield name,name2
		End Select
		
		Select sn.category
		Case "requiresa"
			curthing.addrequirement name
		End Select
	Case "hasmany"
		sn2:sentence=sn.nextsymbol("property")
		'Print sn2.category
		name$=rewrite(sn2.nextvalue("thingname"))
		Select sn2.category
		Case "single"
			curthing.addpossession name
		Case "double"
			name2$=rewrite(sn2.nextvalue("thingname"))
			curthing.addpossession name,name2
		End Select
	Case "inherits"
		name$=rewrite(sn.nextvalue("thingname"))
		'print name
		curthing.addinheritance name
	Case "belongs"
		name$=rewrite(sn.nextvalue("thingname"))
		'print name
		If name="list"
			curthing.islisted=1
		Else
			curthing.addbelong name
			curthing.addfield "parent"+name,name
		EndIf
	Case "can"
		name$=rewrite(sn.nextvalue("thingname"))
		'print name
		curaction=taction.Create(name)
		curthing.addaction curaction
	
	'action instructions
	Case "makea"
		name$=rewrite(sn.nextvalue("thingname"))
		'print name
		curaction.addinstruction tmakeinstruction.Create(name)
	Case "addvector"
		name1$=rewrite(sn.nextvalue("thingname"))
		name2$=rewrite(sn.nextvalue("thingname"))
		curaction.addinstruction taddvectorinstruction.Create(name1,name2)
	Case "doevery","doeveryother"
		verb$=rewrite(sn.nextvalue("thingname"))
		name$=rewrite(sn.nextvalue("thingname"))
		If sn.category="doeveryother"
			doother=1
		Else
			doother=0
		EndIf
		curaction.addinstruction tdoeveryinstruction.Create(verb,name,doother)
	Case "takesa"
		name$=rewrite(sn.nextvalue("thingname"))
		curaction.addparam name
	End Select
End Function

