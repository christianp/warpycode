Function readlines$[](f:TStream)
	t$=""
	While Not Eof(f)
		t:+f.ReadString(1000)
	Wend
	CloseFile f
	Local lines$[]=t.split("~n")
	Return lines
End Function

Function numtabs(txt$)
	i=0 
	While txt[i]=9 And i<Len(txt)
		i:+1
	Wend
	Return i
End Function

Function bintest(p#)
	If Rnd(0,1)<p Then Return 1
End Function

Type tree
	Field txt$
	Field members:TList
	
	Method New()
		members=New TList
	End Method
	
	Function Create:tree(txt$)
		t:tree=New tree
		t.txt=txt
		Return t
	End Function
	
	Function read:tree(lines$[],i Var)
		n=numtabs(lines[i])
		txt$=lines[i].Trim()
		i:+1
		If Not txt.Trim() Return
		t:tree=tree.Create(txt$)
		While i<Len(lines) And numtabs(lines[i])>n
			t.members.addlast tree.read(lines,i)
		Wend
		Return t
	End Function
	
	Method repr$(dips$="")
		If dips o$="~n"+dips Else o$=""
		o:+txt
		For t:tree=EachIn members
			o:+t.repr(dips+"-")
		Next
		Return o
	End Method
End Type


Global concepts:tmap=New tmap
Type concept
	Field name$
	Field ltext$,rtext$
	Field can_be:TList
	Field are_this:TList
	Field is_a:TList
	Field has_a:TList
	
	Method New()
		can_be=New TList
		are_this=New TList
		is_a=New TList
		has_a=New TList
	End Method
	
	Function find:concept(name$)
		If concepts.contains(name)
			Return concept(concepts.valueforkey(name))
		Else
			Return concept.Create(name)
		EndIf
	End Function

	Function Create:concept(name$)
		c:concept=New concept
		c.name=name
		c.rtext=c.name
		concepts.insert c.name,c
		Return c
	End Function

	Method elaborate(t:tree)
		For t2:tree=EachIn t.members
			Select t2.txt
			Case "can be"
				this_can_be t2
			Case "is a","is an"
				this_is_a t2
			Case "has a","has an"
				this_has_a t2
			End Select
		Next
	End Method
		
	Method this_can_be(t:tree)
		For t2:tree=EachIn t.members
			can_be.addlast concept.find(t2.txt)
		Next
	End Method
	
	Method this_is_a(t:tree)
		For t2:tree=EachIn t.members
			c2:concept=concept.find(t2.txt)
			c2.are_this.addlast Self
			is_a.addlast c2
		Next
	End Method

	Method this_has_a(t:tree)
		For t2:tree=EachIn t.members
			c2:concept=concept.find(t2.txt)
			has_a.addlast c2
		Next
	End Method
	
	Method pick_one:concept()
		n=Rand(0,are_this.count()-1)
		Return concept(are_this.valueatindex(n))
	End Method
	
	Method is_this_a(c:concept)
		If c=Self Return 1
		For c2:concept=EachIn is_a
			If c2.is_this_a(c) Return 1
		Next
		Return 0
	End Method
	
	Method repr$()
		o$=name
		If can_be.count()
			o:+"~n  can be:"
			For c:concept=EachIn can_be
				o:+"~n~t"+c.name
			Next
		EndIf
		If are_this.count()
			o:+"~n  are this:"
			For c:concept=EachIn are_this
				o:+"~n~t"+c.name
			Next
		EndIf
		If is_a.count()
			o:+"~n  is a:"
			For c:concept=EachIn is_a
				o:+"~n~t"+c.name
			Next
		EndIf
		If has_a.count()
			o:+"~n  has a:"
			For c:concept=EachIn has_a
				o:+"~n~t"+c.name
			Next
		EndIf
		Return o
	End Method
End Type

Type thing
	Field base:concept
	Field is:TList
	Field has:TList
	
	Method New()
		is=New TList
		has=New TList
	End Method
	
	Function Create:thing(c:concept)
		If Not c.are_this.isempty()
			c=c.pick_one()
		EndIf
		t:thing=New thing
		t.base=c
		p#=1.0/c.can_be.count()
		For c2:concept=EachIn c.can_be
			If bintest(p)
				t.this_is c2
			EndIf
		Next
		For c2:concept=EachIn c.is_a
			t.this_is c2
		Next
		For c2:concept=EachIn c.has_a
			t.this_has thing.Create(c2)
		Next
		Return t
	End Function
	
	Method this_is(c:concept)
		is.addlast c
	End Method
	
	Method this_has(t:thing)
		has.addlast t
	End Method
	
	Method is_this_a(c:concept)
		Return base.is_this_a(c)
	End Method
	
	Method repr$()
		o$=base.name
		If is.count()
			o:+"~nis"
			For c:concept=EachIn is
				o:+"~n~t"+c.name
			Next
		EndIf
		If Not has.isempty()
			o:+"~nhas"
			For t:thing=EachIn has
				o:+"~n~t"+t.base.name
			Next
		EndIf
		Return o
	End Method
End Type


Function make_a:thing(name$)
	Return thing.Create(concept.find(name))
End Function


Local lines$[]=readlines(ReadFile("test.txt"))
i=0
trees:TList=New TList
While i<Len(lines)
	t:tree=tree.read( lines,i )
	If t
		trees.addlast t
		Print t.repr()
		concept.find(t.txt).elaborate(t)
	EndIf
Wend

For c:concept=EachIn concepts.values()
	Print c.repr()
Next

For i=1 To 10
Print "------------------------"
Print make_a("princess").repr()
Next