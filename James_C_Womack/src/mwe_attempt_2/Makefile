# Example bug for Debugging Numerical Software workshop, Bath, 4-5 June 2018
# James C. Womack, 25/05/18
#
# Change FC to use a different Fortran compiler, e.g.
# 	make FC=ifort
# Change FFLAGS to add/remove flags, e.g. to remove OpenMP support
# 	make FFLAGS=
FC := gfortran
FFLAGS := -fopenmp --std=f2003 -Wall
EXE_F90 := main.F90
EXE_OBJ := $(patsubst %.F90,%.o,${EXE_F90})
EXE := $(patsubst %.F90,%.exe,${EXE_F90})
FIX_ME_F90 := mwe.F90
FIX_ME_OBJ := $(patsubst %.F90,%.o,${FIX_ME_F90})
FIX_ME_MOD := $(patsubst %.F90,%.mod,${FIX_ME_F90})

.PHONY : clean clean_obj clean_mod

${EXE} : ${EXE_OBJ} ${FIX_ME_OBJ}
	${FC} ${FFLAGS} -o $@ ${FIX_ME_OBJ} ${EXE_OBJ}

${EXE_OBJ} : ${FIX_ME_OBJ}

%.o : %.F90
	${FC} ${FFLAGS} -c $<

clean : clean_obj clean_mod clean_exe

clean_obj :
	rm -vf ${FIX_ME_OBJ} ${EXE_OBJ}

clean_mod :
	rm -vf ${FIX_ME_MOD} ${EXE_OBJ}

clean_exe : 
	rm -vf ${EXE}
