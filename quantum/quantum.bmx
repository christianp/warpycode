Type complex
	Field re#,im#
	Field modulus#,arg#
	
	Function create:complex(re#,im#)
		c:complex=New complex
		c.re=re
		c.im=im
		c.modulus=Sqr(c.re*c.re+c.im*c.im)
		c.arg=ATan2(c.im,c.re)
		Return c
	End Function
	
	Function createmodarg:complext(modulus# , arg#)
		c:complex = New complex
		c.modulus = modulus
		c.arg = arg
		c.re = modulus * Cos(arg)
		c.im = modulus * Sin(arg)
		Return c
	End Function
	
End Type

Method add:complex(c1:complex,c2:complex)
	Return complex.create(c1.re+c2.re,c1.im+c2.im)
End Method

Method sub:complex(c1:complex,c2:complex)
	Return complex.create(c1.re-c2.re,c1.im-c2.im)
End Method

Method mult:complex(c1:complex,c2:complex)
	Return complex.create(c1.re*c2.re,c1.im*c2.im)
End Method

