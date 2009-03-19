Strict

Import BRL.LinkedList

Type TDict
	Field tdictitems:Tlist
	
	'create a new TDict
	Method New()
		Local tdictitems=New tlist
	End Method
	
	'Set the mapping from key to item
	Method Set(key$,value:Object)
		Local i:TDictItem=getTDictItem(key)
		If i
			i.value=value
		Else
			tdictitems.addlast TDictItem.create(key,value)
		EndIf
	End Method

	'Get the object corresponding to key
	Method Get:Object(key$)
		Local i:TDictItem
		For i:TDictItem=EachIn tdictitems
			If i.key=key Then Return i.value
		Next
	End Method
	
	'Get the TDictItem corresponding to key
	Method GetTDictItem:TDictItem(key$)
		Local i:TDictItem
		For i:TDictItem=EachIn tdictitems
			If i.key=key Then Return i
		Next
	End Method		
	
	'Get the list of keys in this dictionary
	Method Keys:TList()
		Local list:TList=New TList
		Local i:TDictItem
		For i:TDictItem=EachIn tdictitems
			list.addlast i.key
		Next
		Return list
	End Method
	
	'Get the list of items matched to each key in this dictionary
	'(will include duplicates if one object is mapped to by several keys)
	Method Values:Tlist()
		Local list:TList=New TList
		Local i:TDictItem
		For i:TDictItem=EachIn tdictitems
			list.addlast i.value
		Next
		Return list
	End Method
		
	'Remove the mapping from key from the dictionary
	Method Remove(key$)
		Local i:TDictItem=getTDictItem(key)
		If Not i
			Throw "Key not in dictionary"
		EndIf
		tdictitems.remove i
	End Method
	
	'The number of keys in the dictionary
	Method Count()
		Return tdictitems.Count()
	End Method
	
	'Clear all the keys from the dictionary
	Method Clear()
		tdictitems=New tlist
	End Method
	
	'Make a copy of this dictionary
	Method Copy:TDict()
		Local d:TDict=New TDict
		Local i:TDictItem
		For i:TDictItem=EachIn tdictitems
			d.set(i.key,i.value)
		Next
		Return d
	End Method
	
	'Update this dictionary with all the mappings from b
	'leaving untouched keys that aren't in b
	Method Update(b:TDict)
		Local i:TDictItem
		For i:TDictItem=EachIn b.tdictitems
			set(i.key,i.value)
		Next
	End Method
	
	'Create a new dictionary from the given list of keys, with an optional list of items
	'to map them to. Called like this: TDict.fromkeys(keys,[items])
	Function FromKeys:TDict(keys:Tlist,values:Tlist=Null)
		Local d:TDict=New tdict
		Local keylink:TLink=keys.firstlink()
		Local valuelink:TLink
		Local value:Object
		If values<>Null
			If values.count()<>keys.count()
				Throw "Items list is not the same length as keys list"
			EndIf
			valuelink=values.firstlink()
		Else
			valuelink=Null
		EndIf
		While keylink
			If values=Null Then value:Object=Null Else value:Object=valuelink.value()
			d.set(String(keylink.value()),value)
			keylink=keylink.nextlink()
			If valuelink valuelink=valuelink.nextlink()
		Wend
		Return d
	End Function
	
	'Returns value relating to key if key is in dictionary
	'else returns passed value AND sets key to value.
	Method SetDefault:Object(key$,value:Object=Null)
		Local i:TDictItem=getTDictItem(key)
		If i
			Return i.value
		Else
			set(key,value)
			Return value
		EndIf
	End Method
	
	Method ObjectEnumerator:TListEnum()
		Return values().ObjectEnumerator()
	End Method
End Type

Type TDictItem
	Field key$,value:Object
	
	Function Create:TDictItem(key$,value:Object)
		Local i:TDictItem=New TDictItem
		i.key=key
		i.value=value
		Return i
	End Function
End Type
