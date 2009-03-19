Graphics 800,800,0
SetImageFont(LoadImageFont("c:\windows\fonts\Verdana.ttf",150,SMOOTHFONT))

s$="("
DrawText s,400,400
img:tpixmap=GrabPixmap(400,400,TextWidth(s),TextHeight(s))
SavePixmapPNG img,"bracket.png"
incbin