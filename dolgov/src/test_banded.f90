subroutine test_banded(n,M1,M2,S)
!==================================
! Unit test banded_solve subroutine
! on a random true solution
!==================================

  implicit none
  integer, intent(in) :: n
  real(kind=8), dimension(4,n), intent(in) :: M1, M2
  real(kind=8), dimension(n), intent(in) :: S ! diagonal matrix

  real(kind=8), dimension(2*n) :: U_in, W, U
  real(kind=8) :: dnrm2

  call random_number(U_in)
  
  ! W1 = M1*U1
  ! W2 = M2*U2 - S*U1
  call dgbmv('N', n,n, 1,1, 1d0, M1(2:4,:), 4, U_in(1), 1, 0d0, W(1), 1)
  call dgbmv('N', n,n, 1,1, 1d0, M2(2:4,:), 4, U_in(n+1), 1, 0d0, W(n+1), 1)
  call dsbmv('U', n, 0, -1d0, S, 1, U_in(1), 1, 1d0, W(n+1), 1)

  ! solve
  call banded_solve(n,M1,M2,S,W,U)

  ! check the error
  call daxpy(2*n, -1d0, U_in, 1, U, 1)
  print *, 'banded_solve error = ', dnrm2(2*n, U, 1)/dnrm2(2*n, U_in, 1)
  
end subroutine test_banded
