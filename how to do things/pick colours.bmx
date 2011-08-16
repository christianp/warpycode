Function pickcolour(r# Var, g# Var, b# Var)
	Local t#
	r=Rnd()
	g=Rnd()
	b=Rnd()
	t = (r+g+b)/2
	r :/ t
	g :/ t
	b :/ t
End Function