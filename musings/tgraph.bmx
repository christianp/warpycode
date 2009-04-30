Type TNode
	Field _edges:tmap
	
	Method New()
		_edges=New TMap
	End Method
	
	Method EdgeTo:TEdge( n:TNode )
		If edges.contains(n)
			Return tedge(edges.valueforkey(n))
		EndIf
	End Method
	
	Method AddEdge( e:TEdge)
		edges.insert e.opposite(Self), e
	End Method
	
	Method Neighbours:TGraphEnumerator()
		ne:TNeighbourEnumerator=New TNeighbourEnumerator()
		ne.l=edges.firstlink()
		ne.n=Self
		ge:tgraphenumerator=New tgraphenumerator
		ge._enumerator=ne
	End Method
	
	Method Edges:TList()
		Return _edges
	End Method
End Type



Rem
Type TNeighbourEnumerator
	Field l:TLink
	Field n:node

	Method objectenumerator:TNeighbourEnumerator()
		Return Self
	End Method
	
	Method hasnext()
		Return l.nextlink()<>Null
	End Method
	
	Method nextobject:Object()
		n2:TNode=TEdge(l.value()).opposite(n)
		l=l.nextlink()
		Return n2
	End Method
End Type


EndRem

Type TNodeEnumerator
	Method HasNext() Abstract
	Method NextObject:Object() Abstract
End Type

Type BreadthFirstEnumerator Extends TNodeEnumerator
	Field _queue:TList
	Field _checked:TList
	
	Method New()
		_queue=New TList
		_checked=New TList
	End Method
	
	Method hasnext()
		Return _queue.count()
	End Method
	
	Method nextobject:Object()
		n:TNode=tnode(_queue.removefirst())
		For n2:tnode=EachIn n.neighbours()
			If Not _checked.contains(n2)
				_checked.addlast n2
				_queue.addlast n2
			EndIf
		Next
		Return n
	End Method
End Type

Type TGraphEnumerator
	Method objectenumerator:tnodeenumerator()
		Return _enumerator
	End Method
	
	Field _enumerator
End Type

Type TEdge
	Field n1:TNode
	Field n2:TNode
	
	Method Opposite:TNode( n:TNode )
		If n1=n
			Return n2
		ElseIf n2=n
			Return n1
		EndIf
	End Method
End Type

Type TGraph
	Field _root:TNode
	
	Method AddEdge( e:TEdge )
		e.n1.addedge e
		e.n2.addedge e
	End Method
	
	Method BreadthFirst:TGraphEnumerator()
		be:BreadthFirstEnumerator=New BreadthFirstEnumerator
		be._queue.addlast _root
		ge:TGraphEnumerator=New TGraphEnumerator
		ge._enumerator = be
		Return ge
	End Method
		
End Type