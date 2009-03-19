'A simple test of the features of a TDict
Include "TDict.bmx"

'Create a TDict and set some values
d:TDict=New TDict
d.Set("a","1")
d.Set("b","2")

'Get the values corresponding to the keys
Print String(d.Get("a"))
Print String(d.Get("b"))

'SetDefault sets the mapping to the given default value if the key doesn't exist
Print String(d.SetDefault("c","3"))
Print String(d.Get("c"))
'But it won't change anything if the key already exists
Print String(d.SetDefault("c","4"))

Print "---"

'Create a copy of the dictionary and change one key
d2:TDict=d.Copy()
d2.Set("b","5")
For key$=EachIn d2.Keys()
	Print key+": "+String(d2.Get(key))
Next

Print "---"

'Add a key to the first dictionary, then update it with the values
'from the second dictionary. The new key isn't affected because it doesn't
'exist in the second dictionary.
d.Set("d","6")
d.Update(d2)
For key$=EachIn d.Keys()
	Print key+": "+String(d.Get(key))
Next

Print "---"

'Create a dictionary from a list of keys. The values are by default set to Null
somekeys:TList=ListFromArray(["a","b","c"])
d3:TDict=TDict.FromKeys(somekeys)
d3.Set("c","7")
For key$=EachIn d3.Keys()
	Print key+": "+String(d3.Get(key))
Next

Print "---"

'Create a dictionary from a list of keys and matching values
somevalues:Tlist=ListFromArray(["8","9","10"])
d4:TDict=TDict.FromKeys(somekeys,somevalues)
For key$=EachIn d4.Keys()
	Print key+": "+String(d4.Get(key))
Next

Print "---"

'You can iterate over a TDict - this works on the list of values matching to each key
For value$=EachIn d
	Print value
Next