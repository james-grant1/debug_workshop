program main

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

