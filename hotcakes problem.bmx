Type TUnscaled
	Field	Image:TImage
	
	Method	Load(file$)
		Image	=	LoadImage(file$)
	End Method
	
	Method	Draw(x:Int,y:Int)
		DrawImage Image,x,y
	End Method
End Type

Type TScaled Extends TUnscaled
	Field	scale:Int	 = 	4
	
	Method	Draw(x:Int , y:Int) 
		SetScale scale , scale
		Super.Draw x,y
		SetScale 1 , 1
	End Method
End Type


Type TTester
	Method	DoIt() 
	End Method
End Type


Type TGame
	Field	Image:TUnscaled	 = 	New TUnscaled
		
	Method	CallNextTest(test:TTester,x:Int,y:Int) 
		test.DoIt
		Image.Draw x,y
	End Method
End Type


Type TTest1 Extends TTester
	Method	DoIt()
		Game.Image.Load "santahat.png"
	End Method
End Type

Type TTest2 Extends TTester
	Field	Image:TScaled		 = 	New TScaled
	
	Method	DoIt()
		Image.Load "santahat.png"
		Game.Image=Image
	End Method
End Type


Function bongo()
	If tscaled(game.image)
		Print "scaled"
	Else
		Print "unscaled"
	EndIf
End Function

' phew!

Local	ShouldNotDrawScaled:TTest1
Local	ShouldDrawScaled:TTest2
Global	Game:TGame	 = 	New TGame
Graphics 800,600
ShouldNotDrawScaled	 = 	New TTest1
Game.CallNextTest ShouldNotDrawScaled,100,100
bongo()

ShouldDrawScaled	 = 	New TTest2
Game.CallNextTest ShouldDrawScaled,300,200
bongo()


ShouldNotDrawScaled	 = 	New TTest1
Game.CallNextTest ShouldNotDrawScaled,400,400
bongo()

Flip
WaitMouse
End
