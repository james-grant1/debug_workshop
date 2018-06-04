#FC=gfortran
FC=pgfortran
FFLAGS=-g -DPGI
LDFLAGS=-g


%: %.o
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.F90
	$(FC) $(FFLAGS) -c $<

all : debug

clean:
	rm *.o *.mod debug

tar:
	tar -cvf cray-bug.tar Makefile *.F90
	gzip cray-bug.tar

toast:
	@echo "I do not know how to make toast"

war:
	@echo "make peace man, not war"

# dependencies
dense_operator_mod.o: constants_mod.o linear_operator_mod.o vector_mod.o line_vector_mod.o log_mod.o
linear_operator_mod.o: constants_mod.o vector_mod.o
vector_mod.o: constants_mod.o
line_vector_mod.o: vector_mod.o constants_mod.o log_mod.o
log_mod.o: constants_mod.o
preconditioner_mod.o: vector_mod.o
diagonal_preconditioner_mod.o: constants_mod.o preconditioner_mod.o vector_mod.o line_vector_mod.o log_mod.o
iterative_solver_mod.o: constants_mod.o vector_mod.o linear_operator_mod.o preconditioner_mod.o log_mod.o 
debug.o: log_mod.o constants_mod.o line_vector_mod.o linear_operator_mod.o dense_operator_mod.o preconditioner_mod.o diagonal_preconditioner_mod.o iterative_solver_mod.o
debug: debug.o log_mod.o constants_mod.o line_vector_mod.o linear_operator_mod.o dense_operator_mod.o preconditioner_mod.o diagonal_preconditioner_mod.o vector_mod.o iterative_solver_mod.o



