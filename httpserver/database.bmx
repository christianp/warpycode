Include "b-tree.bmx"

Type pagefile
	Field size
	Field f:TStream
	Field startpos,endpos
	
	Function Create:pagefile(f:TStream,size,startpos=0,endpos=-1)
		If endpos=-1
			endpos=f.size()
		EndIf
		
		p:pagefile=New pagefile
		p.size=size
		p.f=f
		p.startpos=startpos
		p.endpos=endpos
		Return p
	End Function
	
	Method seekpage(i)
		f.seek i*(size+4)+startpos
		Print ">"+f.pos()
	End Method	
	
	Rem
	Method readpage$(i)
		seekpage i
		If f.pos()>f.size() Return ""
		n=f.ReadInt()
		page$=f.ReadString(n)
		Return page
	End Method
	
	Method writepage(i,s$)
		seekpage i
		f.WriteInt Len(s)
		f.WriteString s
	End Method
	EndRem
End Type

Rem
f:TStream=WriteFile("dbtest.txt")
p:pagefile=pagefile.Create(f,808)


'create a tree of order 4
bt:btree=btree.Create(4)

Local texts$[]="Hello there, this is a test. Of the database system. Which is nice. Cool".split(".")

'fill the tree with the letters of the alphabet
For i=0 To Len(Texts)-1
	ch$=Chr(Asc("a")+i)
	bt.insert ch,texts[i]
Next

Function savenode(b:bnode,p:pagefile,i)
	Print i+":: "+b.repr()+b.leaf
	p.seekpage i
	p.f.WriteInt b.leaf
	p.f.WriteInt b.n
	For c=0 To b.n-1
		pos=p.f.pos()
		p.f.WriteString String(b.keys[c])[..100]
		p.f.WriteString String(b.values[c])[..100]
	Next
	i:+1
	If Not b.leaf
		For c=0 To b.n
			i=savenode(b.children[c],p,i)
		Next
	EndIf
	Return i
End Function

savenode bt.root,p,0

CloseFile p.f


EndRem

Function loadnode:bnode(p:pagefile,i Var)
	p.seekpage i
	b:bnode=bnode.Create(4,Null,0)
	b.leaf=p.f.ReadInt()
	b.n=p.f.ReadInt()
	For c=0 To b.n-1
		b.keys[c]=Trim(p.f.ReadString(100))
		b.values[c]=Trim(p.f.ReadString(100))
	Next
	i:+1
	If Not b.leaf
		For c=0 To b.n
			b.children[c]=loadnode(p,i)
			b.children[c].parent=b
		Next
	EndIf
	Return b
End Function

f:TStream=ReadFile("dbtest.txt")
p:pagefile=pagefile.Create(f,808)
i=0
b:bnode=loadnode(p,i)
CloseFile f
'End
bt:btree=New btree
bt.order=4
bt.root=b
Print bt.repr()

