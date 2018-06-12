program main

  ! Example bug for Debugging Numerical Software workshop, Bath, 4-5 June 2018
  ! Minimal working example (MWE) by James C. Womack and Ryan Pepper, 05/06/18
  !
  ! This program calls a modified version of the stripped-down reproduction 
  ! of the original routine provided with the original bug submission. Most of 
  ! the functionality and dependencies on other modules in ONETEP have been 
  ! removed. Procedure and variable names differ the original code and 
  ! fake data has been used to attempt to reproduce the original context of the
  ! bug.
  !
  ! In our investigations during the bug hunt session on 05/06/18, this new
  ! minimal working example was able to reproduce the bug, when compiled with
  ! Intel Fortran 17.0.2 and the -fopenmp flag. The bug did not seem to manifest
  ! with Intel Fortran 18.0.0, or with GFortran and PGI Fortran compilers.
  ! 
  ! It looks very likely that this was a compiler bug in Intel Fortran 17.0.2
  ! which has been resolved in more recent versions of the compiler.

  use, intrinsic :: iso_fortran_env
  use mwe, only : DP, prepare_boundary_conditions

  implicit none
  
  real(kind=DP), allocatable :: boundary_conditions(:,:,:) 
  real(kind=DP), allocatable :: charge_density(:,:,:)
  integer :: n1, n2, n3
  real(kind=DP) :: grid_vec(3)

  ! Set plausible grid sizes
  n1 = 193
  n2 = 193
  n3 = 193

  ! Set plausible grid spacing
  grid_vec = [ 0.25, 0.25, 0.25 ]


  allocate(boundary_conditions(n1,n2,n3))
  allocate(charge_density(n1,n2,n3))
 
  boundary_conditions = 0.0_DP
  charge_density      = 0.0_DP
 
  call prepare_boundary_conditions(boundary_conditions, charge_density, n1, n2, n3, &
       grid_vec)

  call prepare_boundary_conditions(boundary_conditions, charge_density, n1, n2, n3, &
       grid_vec)

  deallocate(boundary_conditions)
  deallocate(charge_density)


end program main

