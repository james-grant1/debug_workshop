program neutron
  !==========================================
  !  Main program for solving
  !  1-dimensional neutron diffusion equation
  !  banded matrix version
  ! Shortened to just test the solver
  !=========================================
  
  implicit none
  ! Integer and real parameters
  integer :: n, i
  ! Matrices
  real(kind=8), allocatable, dimension(:,:) :: M1, M2
  real(kind=8), allocatable, dimension(:) :: U, S, F1, F2

  ! Read parameters from a file
  open(unit=1,file='input.dat')
  read (1,*) n
  ! other parameters are not needed here
  close(1)
  
  write (*, '(A,X,I5)') 'n = ', n

  ! Assemble individual matrices
  allocate(M1(4,n), M2(4,n), S(n), F1(n), F2(n), U(2*n))
  call create_banded_matrices(n, M1, M2, S, F1, F2)

  ! Test the banded solve
  call test_banded(n,M1,M2,S)
  
  
  !=================================================
  ! Here we would solve the eigenvalue problem,
  ! but we don't know what's wrong with banded_solve
  !=================================================

  ! Clean up the memory
  deallocate(M1,M2, S, F1, F2, U)
end program neutron
