'main
Global oldmodes:TList
Global curmode:gamemode
Global nextmode:gamemode

'graphics
Global panx#=0,pany#=0,zoom#=1,tzoom#=zoom
Global gwidth,gheight

'trader
Global scrollerwidth,scrollerheight
Global scrolling,scroll#,scrolllength
Global clickables:TList
Global products:tmap
Global merchants:tmap
Global tycoons:TList
Global blueprints:tmap
Global ports:tmap
Global output$
Global lines:TList,bgs:TList
Global date,day,month,year
Global lastscroll,nextdelete
Global normalfont:timagefont
Global names$[]
Global debugging=1
Global horriblekludge:Object

Global curgrammar:grammar
Global grammars:tmap
'Global lettergrammar:grammar,maingrammar:grammar,menugrammar:grammar
'Global contractlettergrammar:grammar
Global player:merchant
Global curletter:letter

'lettermaker
Global handwritingfonts:timagefont[]
Global printfonts:timagefont[]
Global headlinefonts:timagefont[]
Global textheights:tmap
Global textstarts:tmap
Global allfonts:tmap
Global typetemplates:tmap
Global dfonts:tmap


'mapping
Global labels:TList
Global paths:TList
Global curtool:tool
Global mapimg:bigimage
Global mapfont:timagefont