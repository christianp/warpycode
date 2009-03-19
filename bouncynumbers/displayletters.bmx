Graphics 800,800,0

letters$="0123456789"
lettermap:tmap=New tmap
For c=0 To Len(letters)-1
	Print "letters\"+Chr(letters[c])+".png"
	p:tpixmap=LoadPixmapPNG("letters\"+Chr(letters[c])+".png")
	images:tlist=New tlist
	For i=0 To 29
		img:timage=LoadImage(PixmapWindow(p,i*50,0,50,50))
		images.addlast img
	Next
	lettermap.insert(Chr(letters[c]),images)
Next

c=0
frame=0
While Not KeyHit(KEY_ESCAPE)
	images:tlist=tlist(lettermap.findnode(Chr(letters[c])).value())
	DrawImage timage(images.valueatindex(frame)),375,375
	'DrawPixmap tpixmap(pixmaps.first()),0,0
	frame:+1
	
	If frame=30
		c=Rand(0,Len(letters)-1)
		frame=0
	EndIf
	
	Flip
	Cls
Wend