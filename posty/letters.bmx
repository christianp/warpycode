Type letter
	Field address$
	Field x#,y#
	Field bag:postbag
	Field size#
	
	Method New()
		bag = New postbag
		bag.addlast Self
	End Method
		
	Function pick:letter(x# , y#)
		For l:letter = EachIn letters
			dx# = x - l.x
			dy# = y - l.y
			If dx * dx + dy * dy < l.size*l.size
				Return l
			EndIf
		Next
	End Function
	
	Method draw()
		If bag.count()>1
			SetColor bag.red , bag.green , bag.blue
		else
			SetColor 150 , 150 , 150
		endif
		DrawOval x - size , y - size , size*2 , size*2
		SetColor 0 , 0 , 0
		DrawText address,x-TextWidth(address)/2,y-TextHeight(address)/2
	End Method
End Type

Global postbags:TList=New TList
Type postbag Extends TList
	Field red , green , blue
	
	Method New()
		postbags.addlast Self
		Local col#[3]
		t# = 1
		For i = 0 To 1
			col[i] = Rnd(0 , t)
			t:- col[i]
		Next
		red = Sqr(col[0])*255
		green = Sqr(col[1])*255
		blue=Sqr(t)*255
	End Method
	Method remove(o:Object)
		Super.remove o
		If Not count()
			postbags.remove Self
		EndIf
	End Method
End Type

Graphics 600 , 600 , 0
SetBlend ALPHABLEND

Local starts$[] = ["Mid" , "Hamp" , "Comp"]
Local ends$[]=["ton","chester","bury"]

Global letters:TList=New TList
For c=1 To 20
	l:letter = New letter
	l.x = Rand(600)
	l.y = Rand(600)
	letters.addlast l
	l.address = starts[Rand(0 , 2)] + ends[Rand(0 , 2)]
	l.size=(TextWidth(l.address)+10)/2
Next

Local held:letter , offx# , offy#

While Not (KeyHit(KEY_ESCAPE) Or AppTerminate() )
	
	mx = MouseX()
	my = MouseY()
	
	If MouseDown(1)
		If Not held
			held = letter.pick(mx , my)
			offx = held.x - mx
			offy = held.y - my
			If held
				letters.remove held
				letters.addfirst held
			EndIf
		EndIf
		If held
			held.x = mx + offx
			held.y = my + offy
		EndIf
	Else
		If held
			close:letter=Null
			For l:letter = EachIn letters
				If l <> held
					dx# = l.x - held.x
					dy# = l.y - held.y
					d# = Sqr(dx * dx + dy * dy)
					If d < l.size+held.size
						close = l
						Exit
					EndIf
				EndIf
			Next
			If close
				held.bag.remove held
				held.bag=close.bag
				held.bag.addlast held
			ElseIf held.bag.count() > 1
				held.bag.remove held
				held.bag = New postbag
				held.bag.addlast held
			EndIf
			held=Null
		EndIf
	EndIf
	
	SetAlpha .7
	For l:letter = EachIn letters.reversed()
		l.draw
	Next
	SetAlpha 1
	
	SetColor 255,255,255
	DrawText postbags.count(),0,0
	
	Flip
	Cls
Wend
End

