subroutine banded_solve(n,M1,M2,S,W,U)
!========================================
! Solving a system
! [M1  0] * [U1] = [W1]
! [-S M2]   [U2]   [W2]
! with M1 banded (tridiag) and S diagonal
!========================================

  implicit none
  integer, intent(in) :: n
  real(kind=8), dimension(4,n), intent(in) :: M1, M2
  real(kind=8), dimension(n), intent(in) :: S ! diagonal matrix
  real(kind=8), dimension(2*n), intent(in) :: W
  real(kind=8), dimension(2*n), intent(out) :: U

  integer, dimension(n) :: ipiv
  integer :: info
  real(kind=8), dimension(4,n) :: Mtmp

  call dcopy(2*n, W, 1, U, 1)
  
  ! Backup the matrix
  call dcopy(4*n, M1, 1, Mtmp, 1)
  ! solve u1 = M1^{-1}*W1
  call dgbsv(n, 1,1, 1, Mtmp, 4, ipiv, U(1), n, info)
  
  ! form g2 = W2 + S*u1
  call dsbmv('U', n, 0, 1d0, S, 1, U(1), 1, 1d0, U(n+1), 1)
  
  ! solve u2 = M2^{-1}*g2
  call dcopy(4*n, M2, 1, Mtmp, 1)
  call dgbsv(n, 1,1, 1, Mtmp, 4, ipiv, U(n+1), n, info)
    
end subroutine banded_solve
