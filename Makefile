
FC=pgf90
OPT=-O2
#OPT=-O2 -acc -ta=tesla:cc60 -Minfo=accel


MorphImage : src/MorphImage.f90
	${FC} ${OPT} src/MorphImage.f90 -o MorphImage

.PHONY : clean

clean :
	rm MorphImage
