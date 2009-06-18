Include "regexp.bmx"

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
			line=Trim(line)
			If line And line[0]<>Asc("#")
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
		gdebugo "ADD RULE| "+in
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
		Local terms$[]=rule.split("~~") 'not two tildes in a row, this is a blitz escaped character
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
				Select term[Len(term)-1]
				Case Asc("*")	'zero or more times
					l.addlast gmultisymbol.Create(findsymbol(term[..Len(term)-1]))
				Case Asc("?")	'zero or one time
					l.addlast goptionalsymbol.Create(findsymbol(term[..Len(term)-1]))
				Case Asc("$")	'regular expression
					gdebugo "regexp: "+term[..Len(term)-1]
					l.addlast gregexpsymbol.Create(term[..Len(term)-1])
				Default
					l.addlast findsymbol(term)
				End Select
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
		gdebugo depth+"series match| "+in
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
			sn3:sentence=New sentence
			res$=r.match(in,sn3,depth+"  ")
			If res
				For sn4:sentence=EachIn sn3.params
					sn2.addparam sn4
				Next
				If sn3.category
					sn2.category=sn3.category
				EndIf
				If Len(res)>Len(mres) mres=res
				matches:+1
			EndIf
		Next
		If matches
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

Type goptionalsymbol Extends grule
	Field y:gsymbol
	
	Function Create:goptionalsymbol(y:gsymbol)
		os:goptionalsymbol=New goptionalsymbol
		os.y=y
		Return os
	End Function
	
	Method match$(in$,sn:sentence,depth$="")
		gdebugo depth+"optionalsymbol match| "+in
		res$=y.match(in,sn,depth+"  ")
		If res
			Return res
		Else
			Return ~0
		EndIf
	End Method
	
	Method options:TList(in$,depth$="")
		gdebugo "optionalsymbol options| "+in
		Return y.options(in,depth+"  ")
	End Method
End Type

Type gregexpsymbol Extends grule
	Field s:fsa
	Field re$
	
	Function Create:gregexpsymbol(re$)
		rs:gregexpsymbol=New gregexpsymbol
		rs.s=fsa.Create(re)
		rs.re=re
		Return rs
	End Function
	
	Method match$(in$,sn:sentence,depth$="")
		gdebugo depth+"regexpsymbol match ("+re+")| "+in
		res$=s.evaluate(in)
		If res
			If res=~0 nres$="" Else nres$=res
			sn.addparam sentence.Create("regexp",nres,nres)
			Return res
		EndIf
	End Method
	
	Method options:TList(in$,depth$="")
		Return New TList
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

Function stringmatch$(in$,sn:sentence,depth$="")
	If in[0]<>34 Return ""
	n=in[1..].find("~q")
	If n>=0
		n:+1
		sn.addparam sentence.Create("string",in[1..n],in[1..n])
		Return in[0..n+1]
	EndIf
End Function

Function stringoptions:TList(in$,depth$="")
	l:TList=New TList
	l.addlast "<string>"
End Function

Global specialsymbols:tmap=New tmap
addspecialsymbol "number",numbermatch,numberoptions

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
	
	Method getvalue$(name$)
		sn:sentence=getsymbol(name)
		If sn
			Return sn.value()
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






Rem
For path$=EachIn crawldir("grammars")
	g:grammar=New grammar
	g.addfile path

	SeedRnd MilliSecs()
	For c=1 To 5
		Print g.fill()
	Next
Next
'EndRem

grammars=New tmap
g:grammar=grammar.fromfile("testgrammar.txt")
	
While 1
	in$=Input(">")
	sn:sentence=g.match(in)
	If sn
		Print "match"
		Print sn.repr()
		Print sn.category
		While sn.params.count()
			Print sn.nextparam()
		Wend
		For sn2:sentence=EachIn sn.symbols()
			Print sn2.repr()
		Next
	Else
		Print "no match"
		Print "options:"
		For option$=EachIn g.options(in)
			Print " "+option
		Next
	EndIf
Wend
EndRem


Global gdebugging=0
Function gdebugo(txt$)
	If gdebugging
		Print txt
	EndIf
End Function

