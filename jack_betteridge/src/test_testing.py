#!/usr/bin/python
import pytest

### This is just an example file to test on and does nothing interesting

def a_function(x):
  """
  A stupid function
  """
  return x + 1
  
@pytest.mark.parametrize("invalue,expected", [(1,2),(2,3),(10,11),])
def test_a_function(invalue, expected):
  """
  Test the function
  """
  assert a_function(invalue) == expected, "Should be one more"

@pytest.mark.parallel
@pytest.mark.parametrize("invalue,expected", [(1,2),(2,3),(10,11),])
def test_a_function_in_parallel(invalue, expected):
  """
  Test the function in parallel
  """
  assert a_function(invalue) == expected, "Should be one more"

if __name__ == '__main__':
  """
  MAIN
  """
  print("This is a test, run with pytest")
