    module statevars

      implicit none
    
    

    type :: stateVars1
    
            integer, dimension(:), allocatable, public :: sVec                ! state vector

            logical :: init_flag
    
        contains
    
            procedure :: init           => init_stateVars1
            procedure :: destroy        => dest_stateVars1
    
            procedure ::                   set_stateVars1_vec     
            !generic   :: set            => set_stateVars1_vec    
    
            procedure ::                   get_stateVars1_vec   
            generic   :: get            => get_stateVars1_vec  
    
            procedure :: copyThatInThis => copyThatInThis_stateVars1
            generic   :: assignment (=) => copyThatInThis
    
    end type stateVars1
    
    
    type, extends(stateVars1) :: stateVars2
    
            integer, dimension(2,1), public :: A      
            integer, dimension(1,1), public :: R     
            integer, dimension(1),   public :: eps        ! shock
    
        contains
    
            procedure :: init           => init_stateVars2        ! allocates memory for 'this%sVec'
            procedure :: destroy        => dest_stateVars2
    
            procedure ::                   set_stateVars2_any
            generic   :: set            => set_stateVars2_any        
    
            procedure ::                   get_stateVars2_any 
            generic   :: get            => get_stateVars2_any       
    
            procedure :: copyThatInThis => copyThatInThis_stateVars2

            procedure :: split          => split_stateVars2  
            procedure :: merge_         => merge_stateVars2  
    
    end type stateVars2
    
    
! INTERFACES
!--------------------------------------------------

      contains
    
    
    
    ! ==== INIT and DESTROY functions ==== !
    
    subroutine init_stateVars1(this,n)
    
    class(stateVars1), intent(out) :: this
    integer,           intent(in)  :: n
    
    !===============================================================
    !                allocate memory 
    
    allocate( this%sVec(n) )
    
    !           end of allocate memory 
    !===============================================================
    
    this%init_flag = .true. 

    end subroutine init_stateVars1
    
    subroutine dest_stateVars1(this)
    
    class(stateVars1), intent(inout) :: this
    
    !===============================================================
    !                deallocate memory 
    
   
       deallocate( this%sVec )
    
       this%init_flag = .false.
    
    !----------------------------------------------------------------
    
    end subroutine dest_stateVars1

    subroutine init_stateVars2(this,n)
    
    class(stateVars2), intent(out) :: this
    integer,           intent(in)  :: n
    
    call this%stateVars1%init(n)
    
    end subroutine init_stateVars2

    subroutine dest_stateVars2(this)
    
    class(stateVars2), intent(inout) :: this
    
    
       call this%stateVars1%destroy()
    
    !----------------------------------------------------------------
    
    end subroutine dest_stateVars2
    
    ! ==== SET functions ==== !
    
    subroutine set_stateVars1_vec(this,x)
    
    class(stateVars1),           intent(inout) :: this
    integer,       dimension(:), intent(in)    :: x
    
    this%sVec = x
    
    end subroutine set_stateVars1_vec
    
    subroutine set_stateVars2_any(this,x,A,R,eps)
            ! 'x' should never be supplied together with any of the other arguments,
            ! if it is then these arguments are ignored (NO warning is issued!!)
    
    class(stateVars2),       intent(inout)          :: this
    integer, dimension(:),   intent(in),   optional :: x     ! sVec
    integer, dimension(:,:), intent(in),   optional :: A
    integer, dimension(:,:), intent(in),   optional :: R
    integer, dimension(:),   intent(in),   optional :: eps
    
    if (present(x)) then
            this%sVec = x
            call this%split     ! copy 'this%sVec' (alias 'x') in 'A', 'R', and 'eps'
    else
    
            if (present(A))   this%A = A
            if (present(R))   this%R = R
            if (present(eps)) this%eps = eps
            
            if (present(eps) .and. (.not. present(A) .and. .not. present(R))) then
                    call this%merge_(eps=eps)
            else
                    call this%merge_(A=this%A,R=this%R,eps=this%eps)    ! copy 'A', 'R', and 'eps' in 'this%sVec' (alias 'x') 
            endif
    endif
    
    end subroutine set_stateVars2_any
    
subroutine copyThatInThis_stateVars1(this,that)
        ! Defines the overloaded `=` operator for the 'stateVars1' class

class(stateVars1), intent(inout) :: this      ! has to be intent(INOUT) otherwise 'this%sVec' is deallocated
class(stateVars1),  intent(in)   :: that

this%sVec = that%sVec

end subroutine copyThatInThis_stateVars1

subroutine copyThatInThis_stateVars2(this,that)
        ! Defines the overloaded `=` operator for the 'stateVars2' class

class(stateVars2), intent(inout) :: this      ! has to be intent(INOUT) otherwise 'this%sVec' is deallocated
class(stateVars1), intent(in)    :: that
!--> 'that' should be of class 'stateVars2' however that would not allow
!    procedure overriding, thus class 'stateVars1' is used


this%sVec = that%sVec
call this%split     ! copy 'this%sVec' (alias 'x') in 'A', 'R', and 'eps'

end subroutine copyThatInThis_stateVars2

    ! ==== GET functions ==== !
    
    subroutine get_stateVars1_vec(this,x)
    
    class(stateVars1),     intent(in)  :: this
    integer, dimension(:), intent(out) :: x
    
    x = this%sVec
    
    end subroutine get_stateVars1_vec
    
    subroutine get_stateVars2_any(this,x,A,R,eps)
    
    class(stateVars2),       intent(in)            :: this
    integer, dimension(:),   intent(out), optional :: x
    integer, dimension(:,:), intent(out), optional :: A
    integer, dimension(:,:), intent(out), optional :: R
    integer, dimension(:),   intent(out), optional :: eps
    
    if (present(x))   x   = this%sVec
    if (present(A))   A   = this%A
    if (present(R))   R   = this%R
    if (present(eps)) eps = this%eps
    
    end subroutine get_stateVars2_any
    
    
    ! supporting routines
    !---------------------
    
    subroutine split_stateVars2(this)
            ! copies the content of 'sVec' (alias 'x') in the arrays 'A', 'R', and 'eps'
    
    class(stateVars2), intent(inout) :: this
    
    integer, dimension(2,1) :: A
    integer, dimension(1,1) :: R
    integer, dimension(1)   :: eps
    
    integer :: nA
    integer :: nR
    integer :: nEps
    integer :: nx
    
    ASSOCIATE ( x => this%sVec )

    nA   = size(this%A)
    nR   = size(this%R)
    nEps = size(this%eps)
    nx   = size(this%sVec)
    
    this%A   = reshape(x(1:nA),shape(A))
    this%R   = reshape(x(nA+1:nA+nR),shape(R))
    this%eps = x(nA+nR+1:size(x))
    
    end associate
    
    end subroutine split_stateVars2
    
    subroutine merge_stateVars2(this,A,R,eps)
            ! copies the content of the arrays 'A', 'R', and 'eps' in 'sVec' (alias 'x' )
            ! does NOTHING if no argument present
    
    class(stateVars2),       intent(inout)          :: this
    integer, dimension(:,:), intent(in),   optional :: A
    integer, dimension(:,:), intent(in),   optional :: R
    integer, dimension(:),   intent(in),   optional :: eps
    
    integer :: nA, nR, nx
    
    nA = size(this%A)
    nR = size(this%R)
    nx = size(this%sVec)
    
    if (present(A)) then  
            this%sVec(1:nA) = reshape(A,[nA])
    endif
    if (present(R)) then  
            this%sVec(nA+1:nA+nR) = reshape(R,[nR])
    endif
    if (present(eps)) then  
            this%sVec(nA+nR+1:nx) = eps
    endif
    
    
    end subroutine merge_stateVars2
    
    
    end module statevars 
