SuperStrict

Import "testframework.bmx"

Function test1()
	Global i:Int,i2:Int
	i:+1
	i:+i2
	i:/13
	i=i Mod 256
	i2=i
End Function

Function test2()
	Global b:Byte,b2:Byte
	b:+1
	b:+b2
	b:/13
	b=b Mod 256
	b2=b
End Function

runtests([test1,test2],["Integer","Byte"],100000000)
