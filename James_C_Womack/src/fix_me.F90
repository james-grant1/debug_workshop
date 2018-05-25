program fix_me

  use, intrinsic :: iso_fortran_env
  use, non_intrinsic :: fix_me_utils, only : DP, build_Rmat, apply_Rmat

  implicit none

  real(kind=DP), parameter :: PI = 4.0_DP * atan(1.0_DP)
  real(kind=DP), parameter :: angle_increment = 2*PI*1.0_DP/100
  integer, parameter :: n_rotations = 100000

  real(kind=DP) :: Rmat(2,2,n_rotations)
  real(kind=DP) :: vec_init(2)
  real(kind=DP) :: vec_result(2,n_rotations)
  real(kind=DP) :: theta
  integer       :: irot
  integer       :: ii
  integer       :: num_threads
  logical       :: theta_exceeded_2pi = .false.

  write(OUTPUT_UNIT,'(a)') "Example bug for Debugging Numerical Software workshop, Bath, 4-5 June 2018"
  write(OUTPUT_UNIT,'(a)') "James C. Womack, 25/05/18"


  call build_Rmat(n_rotations,angle_increment,Rmat)

  vec_init = [ 1.0, 0.0 ]

  call apply_Rmat(n_rotations,Rmat,vec_init,vec_result)

  vec_init = [ 0.0, 1.0 ]

  call apply_Rmat(n_rotations,Rmat,vec_init,vec_result)

end program
