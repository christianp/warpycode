Function GaussianKernel#[](radius#)
	width=Ceil(radius)	'kernel will have a middle cell, and width on either side
	Local matrix#[width*2+1]
	sigma# = radius/3		'apparently this is all you need to get a good approximation
	norm# = 1.0 / (Sqr(2*Pi) * sigma)		'normalisation constant makes sure total of matrix is 1
	coeff# = 2*sigma*sigma	'the bit you divide x^2 by in the exponential
	total#=0
	For x = -width To width	'fill in matrix!
		g# = norm * Exp( -x*x/coeff )
		matrix[x+width] = g
		total:+g
	Next
	For x=0 To 2*width	'rescale things to get a total of 1, because of discretisation error
		matrix[x]:/total
	Next
	Return matrix
End Function

Graphics 600,600,0
Local matrix#[]=gaussiankernel(4)
t#=0
For x=0 To 8
	DrawRect x*500/8+50,600-matrix[x]*600,1,1
	t:+matrix[x]
Next
DrawText t,0,0
Flip
WaitKey