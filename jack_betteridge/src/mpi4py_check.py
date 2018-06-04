import pytest
from subprocess import Popen, run, PIPE, STDOUT, check_call
from mpi4py import MPI

### This file trys to run some of the commands
### outside of the pytest framework with varying degrees of success

### You may want to use the environment variable:
# OMPI_MCA_rmaps_base_oversubscribe=1

nprocs = 3
fspath = "./test_testing.py"
name = "test_a_function_in_parallel"

### check_call doesn't work
### but try commenting out the mpi4py import line above!
call = ["mpiexec", "-n", "1", "hostname"]
call = ["mpiexec", "-n", "1", "python", "-m", "pytest", "--runxfail", "-s", "-q",
        "%s::%s" % (fspath, name)]
call.extend([":", "-n", "%d" % (nprocs - 1), "python", "-m", "pytest", "--runxfail", "--tb=no", "-q",
        "%s::%s" % (fspath, name)])
             
print('Command being called:', ' '.join(call))
print('Number of processes:', nprocs)
check_call(call)
print('Output:', run(call, stderr=STDOUT))

### run using Popen
with Popen(call, stdout=PIPE, stderr=PIPE) as proc:
  proc.wait()
  print(proc.stdout.read())
  print(proc.stderr.read())
  print("Return code:", proc.returncode)

with Popen(["hostname"], stdout=PIPE) as testproc:
  print(testproc.stdout.read())

