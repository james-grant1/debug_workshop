subroutine create_banded_matrices(n, M1, M2, S, F1, F2)
  !=====================================
  ! A subroutine for assembling matrices
  ! in banded storage
  !=====================================
  
  implicit none
  integer, intent(in) :: n
  ! Allocate 4 x n, since this is needed by banded LU
  ! Store diagonals in rows 2...4
  real(kind=8), dimension(4,n), intent(inout) :: M1, M2
  real(kind=8), dimension(n), intent(inout) :: S, F1, F2  ! just diagonal matrices
  
  integer :: i
  real(kind=8) :: h, xl, xr

  ! External functions defining coefficients
  real(kind=8) :: K1, K2, Sigma_a1, Sigma_a2, Sigma_s, Sigma_f1, Sigma_f2

  h = 1d0/n

  M1 = 0d0
  M2 = 0d0
  S = 0d0
  F1 = 0d0
  F2 = 0d0

  do i=1,n
     ! Left and right midpoints
     xl = h*(i-0.5d0)
     xr = xl + h

     ! S = Diag[Sigma_s],
     ! F1 = Diag[Sigma_f,1],
     ! F2 = Diag[Sigma_f,2]
     S(i) = (Sigma_s(xl) + Sigma_s(xr))*0.5d0
     F1(i) = (Sigma_f1(xl) + Sigma_f1(xr))*0.5d0
     F2(i) = (Sigma_f2(xl) + Sigma_f2(xr))*0.5d0
     
     ! M1 = FD[K1] + Diag[Sigma_a,1] + S     
     ! M2 = FD[K2] + Diag[Sigma_a,2]
     ! Diagonal entries of M1 and M2
     M1(3,i) = (K1(xl)+K1(xr))/(h**2) + (Sigma_a1(xl)+Sigma_a1(xr))*0.5d0 + S(i)
     M2(3,i) = (K2(xl)+K2(xr))/(h**2) + (Sigma_a2(xl)+Sigma_a2(xr))*0.5d0
     ! M1 and M2 contain also off-diagonals from FD
     if (i>1) then
        M1(4,i-1) = -K1(xl)/(h**2)
        M2(4,i-1) = -K2(xl)/(h**2)
     end if
     if (i<n) then
        M1(2,i+1) = -K1(xr)/(h**2)
        M2(2,i+1) = -K2(xr)/(h**2)
     else
        ! Neumann boundary condition on i==n
        M1(4,i-1) = -(K1(xl)+K1(xr))/(h**2)
        M2(4,i-1) = -(K2(xl)+K2(xr))/(h**2)
     end if
  end do  

end subroutine create_banded_matrices

