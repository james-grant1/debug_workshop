My Name:      Jack Betteridge
Bug Name:     Recursive MPI calls and pytest
Requirements: python
              pytest
              openMPI (at least version 3.0)
              mpi4py
Description:  This bug occurs as part of the array of tests for 
              the Firedrake software framework, but this
              repository contains a minimal failing example.
              
              Try running pytest in the src directory.
              
              3 serial tests pass, 3 parallel tests fail. 
              
