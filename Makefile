
OPT=-O2 -Mpreprocess


.PHONY : MorphImage

MorphImage : CommonData.o MorphImage.o
	${FC} ${OPT} CommonData.o MorphImage.o -o MorphImage

CommonData.o : src/CommonData.f90
	${FC} ${OPT} -c src/CommonData.f90 -o $@

MorphImage.o : CommonData.o src/MorphImage.f90
	${FC} ${OPT} -c src/MorphImage.f90 -o $@


.PHONY : clean

clean :
	rm *.o *.mod
