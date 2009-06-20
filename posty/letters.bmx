Type letter
	Field address$[]
	Field x#,y#
	Field bag:postbag
	Field tw# , th#
	Field w#,h#
	
	Method New()
		bag = New postbag
		bag.addlast Self
	End Method
	
	Function Create:letter(x# , y# , address$[])
		l:letter = New letter
		l.x = x
		l.y = y
		l.address = address
		l.tw = 0
		For line$ = EachIn address
			l.tw = Max(l.tw , TextWidth(line) )
		Next
		l.th=TextHeight(address[0])
		l.w = 140
		l.h= 90
		Return l
	End function
		
	Function pick:letter(x# , y#)
		For l:letter = EachIn letters
			'dx# = x - l.x
			'dy# = y - l.y
			If x>l.x And y>l.y And x<l.x+l.w And y<l.y+l.h
			'If dx * dx + dy * dy < l.size*l.size
				Return l
			EndIf
		Next
	End Function
	
	Method overlap(l:letter)
		If x < l.x + l.w And x + w > l.x And y < l.y + l.h And y + h > l.y
			Return true
		endif
	End method
	
	Method drop(avoidbag:postbag = Null)
		bags:TList = New TList
		bags.addlast bag
		For l:letter = EachIn letters
			If l <> Self And l.bag<>avoidbag
				'dx# = l.x - x
				'dy# = l.y - y
				'd# = Sqr(dx * dx + dy * dy)
				If overlap(l)
				'If d < l.size + size
					If Not bags.contains(l.bag)
						bags.addlast l.bag
					endif
				EndIf
			EndIf
		Next
		'Print "bags: "+bags.count()
		
		check:TList = New TList
		For b:postbag = EachIn bags
			For l:letter = EachIn b
				check.addlast l
			Next
		next
		'Print "check: "+check.count()
		
		For l:letter = EachIn check
			close:letter=null
			For l2:letter = EachIn letters
				If l2 <> l
					'dx# = l2.x - l.x
					'dy# = l2.y - l.y
					'd# = Sqr(dx * dx + dy * dy)
					If l.overlap(l2)
					'If d < l.size + l2.size
						close = l2
						exit
					endif
				endif
			next		
			If close
				l.bag.remove l
				l.bag = close.bag	
				l.bag.addlast l
			ElseIf l.bag.count() > 1
				l.bag.remove l
				l.bag = New postbag
				l.bag.addlast l
			endif
		next
	End Method
		
	Method draw()
		If bag.count()>1
			SetColor bag.red , bag.green , bag.blue
		Else
			SetColor 150 , 150 , 150
		EndIf
		DrawRect x  , y,w,h
		SetColor 0 , 0 , 0
		tx# = x - tw/2+w/2
		ty# = y - Len(address)*th / 2+h/2
		For line$ = EachIn address
			DrawText line , tx , ty
			ty:+ th
		next
			
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

Local names$[]=["Jim","Bob","Flo"]
Local starts$[] = ["Mid" , "Hamp" , "Comp"]
Local ends$[]=["ton","chester","bury"]

Global letters:TList=New TList
For c = 1 To 20
	l:letter=letter.Create(Rand(600),Rand(600),[names[Rand(0,2)], starts[Rand(0 , 2)] + ends[Rand(0 , 2)]])
	letters.addlast l
	l.drop
Next

Local held:letter ,heldbag:postbag, offx# , offy#

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
			held.drop
			held=Null
		EndIf
	EndIf
	
	If MouseDown(2)
		If Not heldbag
			l:letter = letter.pick(mx , my)
			If l
				heldbag=l.bag
				offx = mx
				offy = my
			endif
		endif
		If heldbag
			For l:letter = EachIn heldbag
				l.x:+ mx - offx
				l.y:+ my - offy
			Next
			offx = mx
			offy = my
		endif
	Else
		If heldbag
			For l:letter = EachIn heldbag.copy()
				l.drop heldbag
			Next
			heldbag=Null
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

