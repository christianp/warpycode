Type node
	Field edges:tlist
	
	Method New()
		edges=New tlist
	End Method
	
	Method addedge(e:edge)
		edges.addlast e
	End Method
End Type

Type edge
	Field a:node,b:node
	Field length#
	
	Function create:edge(a:node,b:node,length#)
		e:edge=New edge
		e.a=a
		e.b=b
		e.length=length
		Return e
	End Function
	
	Method otherend(n:node)
		If a=n Return b
		If b=n Return a
		Throw "node not on this edge"
	End Method
End Type

Type TNumber
	Field n#
	
	Method create(n#)
		tn:tnumber=New tnumber
		tn.n=n
	End Method
End Type

Type graph
	Field nodes:tlist
	
	Method New()
		nodes=New tlist
	End Method
	
	Method addnode(n:node)
		If nodes.contains(n) Return
		nodes.addlast n
	End Method
	
	Method linknodes(a:node,b:node,length#=1)
		'make sure both nodes are in the graph
		addnode(a)
		addnode(b)
		
		'create the edge
		e:edge=edge.create(a,b,length)
		
		'add the edges to the two nodes
		a.addedge(e)
		b.addedge(e)
	End Method
	
	Method route:tlist(start:node,finish:node)
		If Not (nodes.contains(start) And nodes.contains(finish)) Return Null
		distances:tmap=calcdistances(start)

		n:node=finish
		route:tlist=New tlist
		While n<>start
			route.addfirst n
			mindist#=-1
			closest:node=Null
			For e:edge=EachIn n.edges
				on:node=e.otherend(n)
				dist#=TNumber(distances.FindNode(on)).n
				If dist<mindist Or mindist=-1
					closest=on
					mindist=dist
				EndIf
			Next
			n=closest
		Wend
		Return route
	End Method
	
	Method calcdistances:tmap(start:node)
		If Not nodes.contains(start) Return Null
		distances:tmap=New tmap
		finishednodes:tlist=New tlist
		finishednodes.addlast start
		distances.insert(start,TNumber.create(0))
		While finishednodes.count()<nodes.count()
			mindist#=-1
			closest:node=Null
			For n:node=EachIn finishednodes
				mydist#=TNumber(distances.FindNode(n)).n
				For e:edge=EachIn n.edges
					on:node=e.otherend(n)
					If Not finishednodes.contains(on)
						totaldist#=mydist+e.length
						If totaldist<mindist Or mindist=-1
							mindist=totaldist
							closest=on
						EndIf
					EndIf
				Next
			Next
			finishednodes.addlast closest
			distances.insert(closest,TNumber.create(mindist))
		Wend
		Return distances
	End Method
	
End Type

Type placednode
	Field node:node
	Field x#,y#
	Field vx#,vy#
	Field distances:tmap
End Type

Type placedgraph
	Field nodes:tmap
	
	Method New()
		nodes=New tmap
	End Method
	
	Function create:placedgraph(g:graph)
		For n:node=EachIn g.nodes
			distances:tmap=g.calcdistances(n)
			For on:node=EachIn g.nodes
				If on<>n
					dist#=tnumber(distances.findnode(on)).n
				EndIf
			Next
		Next
	End Function
End Type