Global Fixedknots:TList=New TList
Type TFixedknot Extends TKnot

	Method New()
		Fixedknots.addlast Self

	End Method

	Function CreateFixedknot:TFixedknot(  )

	End Function


	Method Update()
	End Method

End Type

Global Knots:TList=New TList
Type TKnot
	Field pX#, pY#
	Field myThreads:TList

	Method New()
		Knots.addlast Self

		myThreads = New TList
	End Method

	Function CreateKnot:TKnot( pX#, pY# )

	End Function


	Method Update()
	End Method

End Type

Type TThread
	Field k1:TKnot
	Field k2:TKnot

	Method New()

	End Method

	Function CreateThread:TThread(  )

	End Function

End Type


Graphics 600,600,0
Setblend ALPHABLEND


While Not (KeyHit(KEY_ESCAPE) or AppTerminate())
	Flip
	Cls
Wend

