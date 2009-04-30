Global Asteroids:TList=New TList
Type TAsteroid Extends TDrawable
	Field pX#, pY#
	Field radius#
	Field vX#, vY#

	Method New()
		Asteroids.addlast Self

	End Method

	Function CreateAsteroid:TAsteroid( pX#, pY#, vX#, vY#, radius# )

	End Function


	Method Attract( Asteroid:TAsteroid )
	End Method


	Method Update()
		For asteroid:TAsteroid=EachIn Asteroids
			If asteroid <> Self
				attract asteroid
			Endif
		Next
		
	End Method


	Method Draw()
	End Method

End Type

Type TBullet Extends TDrawable
	Field pX#, pY#
	Field parentship:TShip
	Field vX#, vY#

	Method New()

	End Method

	Function CreateBullet:TBullet( pX#, pY# )

	End Function


	Method Draw()
	End Method


	Method Update()
		pX :+ vX
		pY :+ vY
	End Method

End Type

Global Drawables:TList=New TList
Type TDrawable

	Method New()
		Drawables.addlast Self

	End Method

	Function CreateDrawable:TDrawable(  )

	End Function


	Method Draw()
	End Method


	Method Update()
		pX :+ vX
		pY :+ vY
	End Method

End Type

Type TDude

	Method New()

	End Method

	Function CreateDude:TDude(  )

	End Function

End Type

Global Ships:TList=New TList
Type TShip Extends TDrawable
	Field direction#
	Field Name$
	Field pX#, pY#
	Field vX#, vY#
	Field myBullets:TList
	Field Friends:TList

	Method New()
		Ships.addlast Self

		myBullets = New TList
		Friends = New TList
	End Method

	Function CreateShip:TShip( Name$, pX#, pY# )

	End Function


	Method Accelerate()
	End Method


	Method Shoot()
		bullet:TBullet = New TBullet
		bullet.vX = vX
		bullet.vY = vY
		bullet.pX = pX
		bullet.pY = pY
		bullet.parentShip = Self
		myBullets.addlast bullet
	End Method


	Method Turn()
	End Method


	Method Draw()
	End Method


	Method Update()
		pX :+ vX
		pY :+ vY
	End Method

End Type


Graphics 600,600,0
Setblend ALPHABLEND


While Not (KeyHit(KEY_ESCAPE) or AppTerminate())
	For drawable:TDrawable=EachIn Drawables
		drawable.update
	Next
	
	For drawable:TDrawable=EachIn Drawables
		drawable.draw
	Next
	
	Flip
	Cls
Wend

