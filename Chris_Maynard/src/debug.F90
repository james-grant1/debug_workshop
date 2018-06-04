program debug
  use constants_mod,               only : r_def, i_def
  use log_mod,                     only : init_log, finalise_log, log_event, &
                                          LOG_LEVEL_INFO, LOG_LEVEL_ERROR, &
                                          log_scratch_space
  use line_vector_mod,             only : line_vector_type
  use dense_operator_mod,          only : dense_operator_type
  use diagonal_preconditioner_mod, only : diagonal_preconditioner_type
  use iterative_solver_mod,        only : conjugate_gradient_type, &
                                          bicgstab_type,           &
                                          gmres_type
       
  
  implicit none
  
  type(line_vector_type) :: x, rhs, ans, Mrhs
  real(kind=r_def) :: rtol, atol
  type(dense_operator_type) :: M
  type(diagonal_preconditioner_type) :: P
  type(conjugate_gradient_type) :: cg
  type(bicgstab_type)           :: bicgstab
  type(gmres_type)              :: gmres
  integer(kind=i_def) :: nrank, gcrk
  
  call init_log()
  nrank = 8_i_def
  gcrk = 8_i_def
  write(log_scratch_space,'(A)') "debug: hello, world"
  call log_event(log_scratch_space,LOG_LEVEL_INFO)

  ans = line_vector_type(nrank)
  rhs = line_vector_type(nrank, 0.5_r_def, 1.0_r_def)
  write(log_scratch_space,'(A,E16.8)') "debug: ans:", rhs%norm()
  call log_event(log_scratch_space,LOG_LEVEL_INFO)
  call ans%copy(rhs)

  M = dense_operator_type(nrank)
  Mrhs = line_vector_type(nrank)
  call M%apply(rhs, Mrhs)
  write(log_scratch_space,'(A,E16.8)') "debug: applied op ans:", Mrhs%norm()
  call log_event(log_scratch_space,LOG_LEVEL_INFO)  
  call rhs%copy(Mrhs)
  
  rtol = 1.0e-4_r_def
  atol = 1.0e-12_r_def
  P = diagonal_preconditioner_type(nrank)
  cg = conjugate_gradient_type( M, P, rtol, atol, 100)
  bicgstab = bicgstab_type( M, P, rtol, atol, 100)
  gmres =    gmres_type( M, P, gcrk, rtol, atol, 100)
  x = line_vector_type(nrank)
  call x%set_scalar(0.0_r_def)
  write(log_scratch_space,'(A,E16.8)') "debug: calling cg:", x%norm()
  call log_event(log_scratch_space,LOG_LEVEL_INFO)
  call cg%apply( x, rhs )

  write(log_scratch_space,'(A,E16.8,",",E16.8)') "debug: solved:", x%norm(),ans%norm()
  call log_event(log_scratch_space,LOG_LEVEL_INFO)

  call x%set_scalar(0.0_r_def)
  write(log_scratch_space,'(A,E16.8)') "debug: calling bicgstab:", x%norm()
  call log_event(log_scratch_space,LOG_LEVEL_INFO)
  call bicgstab%apply( x, rhs )
  write(log_scratch_space,'(A,E16.8,",",E16.8)') "debug: solved:", x%norm(),ans%norm()
  call log_event(log_scratch_space,LOG_LEVEL_INFO)

  call x%set_scalar(0.0_r_def)
  write(log_scratch_space,'(A,E16.8)') "debug: calling gmres:", x%norm()
  call log_event(log_scratch_space,LOG_LEVEL_INFO)
  call gmres%apply( x, rhs )

  write(log_scratch_space,'(A,E16.8,",",E16.8)') "debug: solved:", x%norm(),ans%norm() 
  call log_event(log_scratch_space,LOG_LEVEL_INFO)  
  
  call finalise_log()
  
end program debug
