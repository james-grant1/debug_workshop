module line_vector_mod
  use constants_mod, only : r_def, i_def
  use vector_mod,    only : abstract_vector_type
  use log_mod,       only : log_event, log_scratch_space, LOG_LEVEL_ERROR, LOG_LEVEL_INFO

  private

  type, public, extends(abstract_vector_type) :: line_vector_type
     integer(kind=i_def)                         :: ndata
     real(kind=r_def), public, allocatable, dimension(:) :: vdata
   contains
     procedure, public :: set_scalar => set_scalar_lv
     procedure, public :: axpy       => axpy_lv
     procedure, public :: aypx       => aypx_lv
     procedure, public :: norm       => norm_lv
     procedure, public :: dot        => dot_lv
     procedure, public :: scale        => scale_lv     
     procedure, public :: duplicate  => duplicate_lv
     procedure, public :: copy       => copy_lv
     procedure, public :: p          => p_lv

     ! private procedures
     procedure, private :: set_scalar_lv
     procedure, private :: axpy_lv
     procedure, private :: aypx_lv
     procedure, private :: norm_lv
     procedure, private :: dot_lv
     procedure, private :: scale_lv
     procedure, private :: duplicate_lv
     procedure, private :: copy_lv
     procedure, private :: p_lv
     final :: line_vector_destructor
     
  end type line_vector_type

  interface line_vector_type 
     module procedure line_vector_constructor, line_vector_two_val_constructor
  end interface line_vector_type
  
contains

  function line_vector_constructor(ndata) result(self)
    implicit none
    integer, intent(in) :: ndata
    type(line_vector_type) :: self
    allocate(self%vdata(ndata))
    self%ndata = ndata
  end function line_vector_constructor

  function line_vector_two_val_constructor(ndata, d1, d2) result(self)
    implicit none
    integer,          intent(in) :: ndata
    real(kind=r_def), intent(in) :: d1
    real(kind=r_def), intent(in) :: d2
    type(line_vector_type) :: self
    integer(kind=i_def) :: nlp1, nlp2
    allocate(self%vdata(ndata))
    self%ndata = ndata

    do nlp1 = 1, self%ndata/2
       self%vdata(nlp1) = d1
    end do

    do nlp2 = nlp1, self%ndata
       self%vdata(nlp2) = d2
    end do
    
  end function line_vector_two_val_constructor
  
  subroutine set_scalar_lv(self, scalar)
    implicit none
    class(line_vector_type), intent(inout) :: self
    real(kind=r_def),        intent(in)    :: scalar

    integer :: nlp

    do nlp = 1, self%ndata
       self%vdata(nlp) = scalar
    end do
  end subroutine set_scalar_lv

  subroutine axpy_lv(self, alpha, x)
    implicit none
    class(line_vector_type),     intent(inout) :: self
    real(kind=r_def),            intent(in)    :: alpha
    class(abstract_vector_type), intent(inout) :: x

    integer(kind=i_def) :: nlp
    
    select type(x)
    type is(line_vector_type)
       ! compute y = alpha *x + y
       do nlp = 1, self%ndata
          self%vdata(nlp) = self%vdata(nlp) + alpha*x%vdata(nlp)
       end do
    class default
       write(log_scratch_space,'(A)') "line_vector:axpy: type of x is not line_vector_type"
       call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end select
    
  end subroutine axpy_lv

  subroutine aypx_lv(self, alpha, x)
    implicit none
    class(line_vector_type),     intent(inout) :: self
    real(kind=r_def),            intent(in)    :: alpha
    class(abstract_vector_type), intent(inout) :: x

    integer(kind=i_def) :: nlp
    
    select type(x)
    type is(line_vector_type)
       ! compute y = alpha *y + x
       do nlp = 1, self%ndata
          self%vdata(nlp) = alpha*self%vdata(nlp) + x%vdata(nlp)
       end do
    class default
       write(log_scratch_space,'(A)') "line_vector:aypx: type of x is not line_vector_type"
       call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end select    
    
  end subroutine aypx_lv

  function norm_lv(self) result(normal)
    implicit none
    class(line_vector_type), intent(in)  :: self
    real(kind=r_def)                     :: normal
    integer(kind=i_def) :: nlp
    normal = 0.0_r_def

    do nlp = 1, self%ndata
       normal = normal + ( self%vdata(nlp) * self%vdata(nlp) )
    end do
    normal = sqrt(normal)
  end function norm_lv

  function dot_lv(self, x) result(dot_prod)
    implicit none
    class(line_vector_type),     intent(in) :: self
    class(abstract_vector_type), intent(in) :: x
    real(kind=r_def)                        :: dot_prod

    integer(kind=i_def) :: nlp
    
    select type(x)
    type is(line_vector_type)
       dot_prod = 0.0_r_def
       do nlp = 1, self%ndata
          dot_prod = dot_prod + ( self%vdata(nlp) * x%vdata(nlp) )
       end do
    class default
       write(log_scratch_space,'(A)') "line_vector:dot: type of x is not line_vector_type"
       call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end select
    
  end function dot_lv

  subroutine scale_lv(self, scalar)
    implicit none
    class(line_vector_type), intent(inout) :: self
    real(kind=r_def),        intent(in)    :: scalar

    self%vdata(:) = self%vdata(:) * scalar
    
  end subroutine scale_lv

  subroutine duplicate_lv(self, vec)
    implicit none
    class(line_vector_type),                  intent(in)    :: self
    class(abstract_vector_type), allocatable, intent(inout) :: vec

    allocate(line_vector_type::vec)
    select type(vec)
    type is (line_vector_type)
       if(.not.(allocated(vec%vdata))) then
          vec%ndata = self%ndata
          allocate(vec%vdata(vec%ndata))
       end if
    class default
       write(log_scratch_space,'(A)') "line_vector:duplicate: type of vec is not line_vector_type"
       call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end select
    
  end subroutine duplicate_lv

  subroutine copy_lv(self, source)
    implicit none
    class(line_vector_type),     intent(out) :: self
    class(abstract_vector_type), intent(in)  :: source

    select type(source)
    type is(line_vector_type)
       if(.not.(allocated(self%vdata)))then
          self%ndata = source%ndata
          allocate(self%vdata(self%ndata))
       end if
       self%vdata(:) = source%vdata(:) 
    class default
       write(log_scratch_space,'(A)') "line_vector:copy: type of source is not line_vector_type"
       call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end select

    
  end subroutine copy_lv

  subroutine p_lv(self)
    implicit none
    class(line_vector_type), intent(inout) :: self

    select type(self)
    type is(line_vector_type)
       write(log_scratch_space,'(A,E16.8)') "line_vector:p:1st element:",self%vdata(1)
!       call log_event(log_scratch_space, LOG_LEVEL_INFO)
       write(*,*) trim(log_scratch_space)
    class default
       write(log_scratch_space,'(A)') "line_vector:p: type of vector is not line_vector_type"
       call log_event(log_scratch_space, LOG_LEVEL_ERROR)
    end select       
    
  end subroutine p_lv


  subroutine line_vector_destructor(self)
    implicit none
    type(line_vector_type), intent(inout) :: self

    if(allocated(self%vdata)) then
       deallocate(self%vdata)
    end if
    
  end subroutine line_vector_destructor
  
end module line_vector_mod
