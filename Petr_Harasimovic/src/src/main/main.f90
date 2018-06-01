    program bug1
      use simul
      use model
    

      type(stateVars2) :: iniState       ! the initial condition for the simulations (see the 'statevars' module for 'stateVars2' class definition)
      type(simVars)    :: simHist        ! holds the simulated variables (see the 'simul' module for 'simVars' class definition)
      type(model_type) :: model_var

      integer :: nVars
      integer :: nYears
      integer :: i
    
      nVars  = 4
      nYears = 20


    ! run simulation
    call iniState%init(nVars)
    call iniState%set(x=[1,2,0,1])
    call simHist%init(nYears,iniState)

    call doSimulation(simHist,iniState,model_var)

    !call simHist%print_
    call simHist%test

    print*,'Program finished'

    
    end program bug1
