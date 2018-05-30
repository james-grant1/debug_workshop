LIBS = -lblas -llapack

OBJS = banded_solve.o  coefficients.o  create_banded_matrices.o  neutron.o  test_banded.o

neutron: $(OBJS)
	gfortran -o neutron $(OBJS) $(LIBS)

%.o: %.f90
	gfortran -c $<

clean:
	rm -f neutron *.o

