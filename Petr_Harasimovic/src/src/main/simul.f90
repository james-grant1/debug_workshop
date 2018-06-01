module simul
  use model
  use stateVars

  implicit none

type :: simVars
        ! holds the values of variables of interest in a simulation

        integer, dimension(:,:), allocatable :: sVars    ! dim(simLength,nsVars)
        integer, dimension(:,:), allocatable :: eVars    ! dim(simLength,neVars)
        integer, dimension(:),   allocatable :: iRate

        integer                              :: count_   ! counts the number of observations in a time series (time wise) during simulations
        integer                              :: n        ! the number of observations in a time series (ie the size of the time series arrays, n = size(iRate,1); set at initialization and fixed)

! flags
        logical, private :: init_flag = .false.

    contains

        procedure :: isInit         => dispInitFlag_simVars
                                    
        procedure :: init           => init_simVars
        procedure :: destroy        => dest_simVars
        procedure :: add            => add_to_simVars            ! adds an observation to the time series
        procedure :: reset          => reset_counter_simvars     ! resets the counter so that 'simvars' can be filled again (old data is overwritten)

        procedure :: print_         => print_simvars            ! adds an observation to the time series
        procedure :: test           => test_simvars            ! adds an observation to the time series

end type simVars



! Define subroutines
contains


! Type bound procedures
!=======================

! ==== INIT and DESTROY procedures ==== !

function dispInitFlag_simVars(this) result(flag)

        class(simVars), intent(in) :: this
        logical                    :: flag

        flag = this%init_flag

end function dispInitFlag_simVars

subroutine init_simVars(this,n,sVarsIni)

class(simVars),    intent(inout) :: this
integer,           intent(in)    :: n              ! simulation length
class(stateVars1), intent(in)    :: sVarsIni       ! all state variables (ie shocks and the learning coefficients)

!===============================================================
!                allocate memory 
allocate( this%svars(n, size(sVarsIni%sVec))         )
allocate( this%evars(n,2)                            )    ! storing only pie_gap and y_gap
allocate( this%irate(n)                              )

!           end of allocate memory 
!===============================================================

this%n                    =  n
this%count_               =  0

this%init_flag = .true.

end subroutine init_simVars

subroutine dest_simVars(this)

class(simVars), intent(inout) :: this

if (.not. this%init_flag) then
        ! throw an error
        ! not allocated hence cannot deallocate error
        print*,'Instance of class(simVars) not allocated, hence, cannot be deallocated'
        print*,'The programme will stop.'
        stop
else

   deallocate( this%svars )
   deallocate( this%evars )
   deallocate( this%irate )

this%n                   = -1
this%count_              =  0

this%init_flag   = .false.
endif

end subroutine dest_simVars


! ==== SET procedures ==== !

subroutine add_to_simvars(this,sVars,eVars,iRate)


        class(simVars),        intent(inout) :: this
        class(stateVars1),     intent(in)    :: sVars
        integer, dimension(:), intent(in)    :: eVars
        integer,               intent(in)    :: iRate

        this%count_ = this%count_ + 1

        this%svars(this%count_,:) = sVars%sVec
        this%evars(this%count_,:) = eVars
        this%irate(this%count_) = iRate

end subroutine add_to_simvars

subroutine reset_counter_simvars(this)
        ! resets the counter allowing new values to be written over the old ones in simulation

        class(simVars), intent(inout) :: this

        this%count_               =  0
end subroutine reset_counter_simvars


! ==== I/O procedures ==== !
subroutine print_simvars(this)


        class(simVars), intent(inout) :: this

        integer :: i

        print*,'sVars (col 1-4), eVars (col 5-6), iRate (col 7): '
        do i = 1,this%count_
           write(*,'(7(I4,5X))')this%sVars(i,:),this%eVars(i,:),this%iRate(i)
        end do

end subroutine print_simvars


! ==== OTHER procedures ==== !
subroutine test_simvars(this)


        class(simVars), intent(inout) :: this

        integer, dimension(20) :: true_iRate
        logical                :: correct

        integer :: i

        true_iRate = [2,3,-1,2,0,3,-4,4,-3,7,-9,11,-10,15,-20,24,-24,33,-43,54]
        correct = all(this%iRate .eq. true_iRate)

        if ( correct ) then
           print*,'Test passed successfully.'
           print*,'------------'
           print*,'BUG FOUND!'
           print*,'------------'
        else
           print*,'Test failed.'
           print*,'--------------------'
           print*,'Bug still there :('
           print*,'--------------------'
        endif


end subroutine test_simvars

! END of Type bound procedures
!==============================


subroutine doSimulation(simHist,iniState,model)


        class(simVars),    intent(inout) :: simHist
        class(stateVars2), intent(in)    :: iniState
        class(model_type), intent(in)    :: model
                                                                               
        class(stateVars1), allocatable   :: iniState0  ! will be source allocated from 'iniState'
        class(stateVars1), allocatable   :: stateTod   ! will be source allocated from 'iniState'
        class(stateVars1), allocatable   :: stateTom   ! will be source allocated from 'iniState'
                                                                            
        integer, dimension(:,:), allocatable :: shocksHist      ! dim(nObs,nVars)
                                                               
        integer                          :: iRate
        integer, dimension(2)            :: Ycurr

        integer :: nVars
        integer :: nShocks
        integer :: year, nYears
        integer :: i, j
        integer :: stat


! Initialize the simulation:
!---------------------------

        nYears  = simHist%n
        nVars   = size(iniState%sVec)
        nShocks = size(iniState%eps)
        allocate( shocksHist(nYears,1), stat = stat )

        call drawShocks(shocksHist)
        
        allocate( iniState0, source=iniState, stat = stat )    ! "copies" 'iniState' in 'iniState0'

        shocksHist(1,:) = iniState0%sVec(nVars-nShocks+1:nVars)    ! replace the first random draw with the initial state supplied by the user

        ! initialize the state of the economy
        allocate( stateTod, source=iniState0, stat = stat )    ! "copies" 'iniState0' in 'stateTod'
        allocate( stateTom, source=iniState0, stat = stat )    ! "copies" 'iniState0' in 'stateTom'


! Do the simulation:
!-------------------

        do year = 1,nYears

           ! update the state variables
           stateTod = stateTom

           ! calculate current period variables
           call model%policy(stateTod,iRate)
           call model%gaps(stateTod,iRate,Ycurr)

           ! next period states
           select type (stateTod)
                class is (stateVars2)
                        select type (stateTom)
                            type is (stateVars2)
           
                                 call model%forward(stateTom,stateTod,iRate)
                                 call stateTom%set(eps=shocksHist(year+1,:))
           
                            class default
                                    print*,'stateTom has to be of the same dynamic type as stateTod'
                                    stop
                         end select    !  select type (stateTom)
           
                class default
                        print*,'Wrong type of the stateTod variable in simulation'
                        stop
           end select     !  select type (stateTod)
           
           ! save today's variables
           call simHist%add(stateTod,Ycurr,iRate)

        end do   ! end of simulation

call stateTod%destroy()
call stateTom%destroy()
call iniState0%destroy()

deallocate( shocksHist )
deallocate( stateTod   )
deallocate( stateTom   )
deallocate( iniState0  )


end subroutine doSimulation

subroutine drawShocks(shocksHist)

   integer, dimension(:,:) :: shocksHist      ! dim(nObs,nVars)

   integer, dimension(4) :: shocks

   integer :: i,j,ii

   shocks = [1,2,3,-1]

   do j = 1,size(shocksHist,2)
      do i = 1,size(shocksHist,1)
         ii = modulo(i,size(shocks))
         shocksHist(i,j) = shocks(ii+1)
      end do
   end do

end subroutine drawShocks

end module simul
