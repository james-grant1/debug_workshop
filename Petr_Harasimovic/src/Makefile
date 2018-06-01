# Makefile for the primes_test.f90
#####################################

#FC=gfortran -fopenmp -fbacktrace -O0 -g -ffpe-trap=#
FC=ifort -traceback -g -p -ftrapuv -fpe0# -traceback#
EXE=main.exe
SRCDIR=./src# path to the source directory; can by overriden from the command line
BLDDIR=./build# path to the build directory; can by overriden from the command line


# set the compiler flag that allows specifying the directory to store .mod files
ifneq (,$(findstring ifort,$(FC)))
	mdirflag=-module # mind the space after module (has to be there)
	#--> -module flag for ifort
else
	mdirflag=-J#no space after the flag
	#--> -J flag for gfortran (and everything else for now)
endif


MODULES   := $(shell ls $(SRCDIR))
SRC_DIR   := $(addprefix $(SRCDIR)/,$(MODULES))
BUILD_DIR := $(addprefix $(BLDDIR)/,$(MODULES))

SRC       := $(foreach sdir,$(SRC_DIR),$(wildcard $(sdir)/*.f90))
OBJ       := $(patsubst $(SRCDIR)/%.f90,$(BLDDIR)/%.o,$(SRC))

INCLUDES  := $(addprefix -I,$(BUILD_DIR))

vpath %.f90 $(SRC_DIR)

.PHONY: all checkdirs clean run 

all: checkdirs build/$(EXE)

build/$(EXE) : $(OBJ)
	$(FC) -o $@ $^

define make-mod
$1/%.mod : $1/%.o %.f90
	@if test -f $$@; then true; else echo "building a missing .mod file"; echo $(FC) $(mdirflag)$1 $(INCLUDES) -c $$(word 2,$$^) -o $$<; fi
#	 -> test if the .mod file exists, if not then build it else don't touch it
endef

$(foreach bdir,$(BUILD_DIR),$(eval $(call make-mod,$(bdir))))
# -> builds mods from sources (no cross dependencies)


define make-obj
$1/%.o : %.f90
	@echo "building .o from source"
	$(FC) $(mdirflag)$1 $(INCLUDES) -c $$< -o $$@
endef

$(foreach bdir,$(BUILD_DIR),$(eval $(call make-obj,$(bdir))))
# -> builds objects from sources (no cross dependencies)


#==============================================================
# dependency rules
include deps.mak


# the 'build' directory
checkdirs: $(BUILD_DIR)

$(BUILD_DIR):
	@mkdir -p $@

clean:
	@rm -rf $(BUILD_DIR)

run:
	@./build/$(EXE) $(RFLAGS)

