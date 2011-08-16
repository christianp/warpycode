Function link_to$(path$,label$,style$="")
	Return "<a href=~q"+path+"~q "+style+">"+label+"</a>"
End Function

Function nl2br$(txt$)
	Return txt.Replace("~n","<br/>")
End function