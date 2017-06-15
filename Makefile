
FC=pgf90
#OPT=-O2
OPT=-O2 -acc -ta=tesla:cc60 -Minfo=accel
#OPT=-O2 -Mcuda=cc35,ptxinfo


.PHONY : MorphImage

MorphImage : CommonData.o MorphImage.o
	${FC} ${OPT} CommonData.o MorphImage.o -o MorphImage

MorphImage_cuda : CommonData.o MorphImageKernels.o MorphImage.o
	${FC} ${OPT} CommonData.o MorphImageKernels.o MorphImage.o -o MorphImage_cuda

CommonData.o : src/CommonData.f90
	${FC} ${OPT} -c src/CommonData.f90 -o $@

MorphImage.o : CommonData.o src/MorphImage.f90
	${FC} ${OPT} -c src/MorphImage.f90 -o $@

MorphImageKernels.o : CommonData.o src/MorphImageKernels.f90
	${FC} ${OPT} -c src/MorphImageKernels.f90 -o $@


.PHONY : clean

clean :
	rm *.o *.mod
