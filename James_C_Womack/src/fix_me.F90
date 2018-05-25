program fix_me

  ! Example bug for Debugging Numerical Software workshop, Bath, 4-5 June 2018
  ! James C. Womack, 25/05/18
  !
  ! This minimal example was created to attempt to recreate a situation
  ! in our code (http://www.onetep.org) where we relied on an implicit SAVE
  ! attribute of a logical variable to ensure that a warning was only
  ! output a single time. When compiling with OpenMP support, it seems that
  ! the SAVE attribute was lost, and the warning was output multiple times.
  !
  ! At the time, we thought that the reason for this was that compilation with
  ! OpenMP with Intel Fortran to cause this implicit SAVEd variable to not be SAVEd,
  ! because -qopenmp implies -automatic, which causes "all local, non-SAVEd
  ! variables to be allocated to the run-time stack" (see ifort 17 man page).
  ! Adding an explicit save attribute to our logical variable resolved the
  ! problem.
  !
  ! This minimal example does not seem to demonstrate the effect. Possibly
  ! the example is too small for the run-time stack to be overwritten
  ! between subroutine calls, or possibly this explanation is incorrect.
  !
  ! If this example behaved like the original issue, then test_var in
  ! apply_Rmat would be reset to .false. on each call to  apply_Rmat when
  ! compiled with OpenMP, and not be reset to .false. when compiled without
  ! OpenMP. Additionally, if an explicit SAVE attribute was added to test_var
  ! within apply_Rmat, then the test_var would be set to .true. after the
  ! first call to apply_Rmat, as intended.

  use, intrinsic :: iso_fortran_env
  use, non_intrinsic :: fix_me_utils, only : DP, build_Rmat, apply_Rmat

  implicit none

  real(kind=DP), parameter :: PI = 4.0_DP * atan(1.0_DP)
  real(kind=DP), parameter :: angle_increment = 2*PI*1.0_DP/100
  integer, parameter :: n_rotations = 100

  real(kind=DP) :: Rmat(2,2,n_rotations)
  real(kind=DP) :: vec_init(2)
  real(kind=DP) :: vec_result(2,n_rotations)

  write(OUTPUT_UNIT,'(a)') "Example bug for Debugging Numerical Software workshop, Bath, 4-5 June 2018"
  write(OUTPUT_UNIT,'(a)') "James C. Womack, 25/05/18"


  call build_Rmat(n_rotations,angle_increment,Rmat)

  vec_init = [ 1.0, 0.0 ]

  call apply_Rmat(n_rotations,Rmat,vec_init,vec_result)

  vec_init = [ 0.0, 1.0 ]

  call apply_Rmat(n_rotations,Rmat,vec_init,vec_result)

end program
