    module model
      use statevars

      implicit none
    

type :: model_type

        contains

            ! the law of motion:
            procedure :: policy  => policy_model_type
            procedure :: gaps    => gaps_model_type
            procedure :: forward => forward_model_type
                                                              
end type model_type                                            
                                                                
 

! INTERFACES
!--------------------------------------------------


contains



! MODEL type bound procedures
!============================


subroutine policy_model_type(model,svarsTod,iRate)

        class(model_type), intent(in)  :: model
        class(stateVars1), intent(in)  :: svarsTod
        integer,           intent(out) :: iRate

        iRate = sum(svarsTod%sVec) - 2

end subroutine policy_model_type
subroutine gaps_model_type(model,svarsTod,iRate,gaps)

        class(model_type),     intent(in)  :: model
        class(stateVars1),     intent(in)  :: svarsTod
        integer,               intent(in)  :: iRate
        integer, dimension(:), intent(out) :: gaps

        gaps(1) = 2*sum(svarsTod%svec) - iRate/2
        gaps(2) = iRate - 5

end subroutine gaps_model_type
subroutine forward_model_type(model,svarsTom,svarsTod,iRate)

        class(model_type), intent(in)    :: model
        class(stateVars2), intent(inout) :: svarsTom
        class(stateVars2), intent(in)    :: svarsTod
        integer,           intent(in)    :: iRate

        ASSOCIATE ( a1_tod   => svarsTod%A(1,1), &
                    a2_tod   => svarsTod%A(2,1), &
                    r_tod    => svarsTod%R(1,1), & 
                    eps_tod  => svarsTod%eps(1), & 
                    a1_tom   => svarsTom%A(1,1), &
                    a2_tom   => svarsTom%A(2,1), &
                    r_tom    => svarsTom%R(1,1) )

            a1_tom = (a1_tod+a2_tod)/(r_tod+1) + iRate/2
            a2_tom = (a1_tod-a2_tod)/(r_tod+1) - iRate
            r_tom  = max(r_tod - eps_tod,1)

        end associate

        call svarsTom%merge_(A=svarsTom%A,R=svarsTom%R,eps=svarsTom%eps)    ! copy 'A', 'R', and 'eps' in 'svarsTom%sVec' (alias 'x') 

end subroutine forward_model_type


! END of MODEL type bound procedures
!===================================


!-----------------------------------------------------------------


! other module procedures
!=========================


    
    end module model 
