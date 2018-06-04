build/main/main.o : \
	build/main/model.mod \
	build/main/simul.mod \
	build/main/statevars.mod

build/main/model.o : \
	build/main/statevars.mod

build/main/simul.o : \
	build/main/model.mod \
	build/main/statevars.mod
