FPP:=gfortran -cpp -E -P

all: matrix.f90

%.f90: templates/%.f90
	$(FPP) -DVALUE_TYPE=real*4     -DTYPELETTER=f $< >  $@
	$(FPP) -DVALUE_TYPE=real*8     -DTYPELETTER=d $< >> $@
	$(FPP) -DVALUE_TYPE=complex*8  -DTYPELETTER=c $< >> $@
	$(FPP) -DVALUE_TYPE=complex*16 -DTYPELETTER=z $< >> $@
