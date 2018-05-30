module dense_operator_mod
  use constants_mod,       only : r_def, i_def
  use linear_operator_mod, only : abstract_linear_operator_type
  use vector_mod,          only : abstract_vector_type
  use line_vector_mod,     only : line_vector_type
  use log_mod,             only : log_event, LOG_LEVEL_ERROR, log_scratch_space
  implicit none
  private

  type, public, extends(abstract_linear_operator_type) :: dense_operator_type
     private
     integer(kind=i_def) :: ndata
     real(kind=r_def), allocatable, dimension(:,:) :: op_data
   contains
     procedure, public  :: apply => apply_dense_op
     procedure, private :: apply_dense_op
     final              :: destroy_dense_op
  end type dense_operator_type

  interface dense_operator_type
     module procedure dense_operator_constructor
  end interface dense_operator_type

contains

  function dense_operator_constructor(nsize) result(self)
    implicit none
    integer(kind=i_def), intent(in) :: nsize
    type(dense_operator_type) :: self
    integer(kind=i_def) :: row, col

    ! Store (eek) a 1-d Poisson operator.
    ! Could be better stored as CSR if I could be bothered.
    
    self%ndata = nsize
    allocate( self%op_data(self%ndata, self%ndata) )

    self%op_data(:,:) = 0.0_r_def
    self%op_data(1,1) = 2.0_r_def
    self%op_data(1,2) = -1.0_r_def
    self%op_data(self%ndata,self%ndata) = 2.0_r_def
    self%op_data(self%ndata,(self%ndata)-1) = -1.0_r_def

    col = 1
    do row = 2, self%ndata -1
       self%op_data(row,col)   = -1.0_r_def
       self%op_data(row,col+1) = 2.0_r_def
       self%op_data(row,col+2) = -1.0_r_def       
       col = col + 1
    end do
    
  end function dense_operator_constructor

  subroutine destroy_dense_op(self)
    implicit none
    type(dense_operator_type), intent(inout) :: self
    if(allocated(self%op_data)) then
       deallocate(self%op_data)
    end if
  end subroutine destroy_dense_op

  subroutine apply_dense_op(self, x, y)
    implicit none    
    class(dense_operator_type), intent(in)    :: self
    class(abstract_vector_type),          intent(in)    :: x
    class(abstract_vector_type),          intent(inout) :: y

    integer(kind = i_def) :: row, col

    select type(x)
    type is(line_vector_type)
       select type(y)
       type is(line_vector_type)
          call y%set_scalar(0.0_r_def)
          do row = 1, self%ndata
             do col = 1, self%ndata
                y%vdata(row) = y%vdata(row) + self%op_data(row, col) * x%vdata(col)
             end do
          end do
       class default
          write(log_scratch_space,'(A)') "dense_operator type of y is not line_vector_type"
          call log_event(log_scratch_space, LOG_LEVEL_ERROR)
       end select
    class default
       write(log_scratch_space,'(A)') "dense_operator: type of x is not line_vector_type"
       call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end select
    
  end subroutine apply_dense_op
  
end module dense_operator_mod
