!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Example bug for Debugging Numerical Software workshop, Bath, 4-5 June 2018
! James C. Womack, 30/05/18
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This is a stripped-down reproduction of the original routine in which the bug
! is manifested in ONETEP. Most of the functionality and dependencies on other 
! modules in ONETEP have been removed. Procedure and variable names differ to
! the original code.
!
! This is provided to demonstrate the  general structure of the routine in which 
! the bug occurs, to accompany the attempted minimal working example.
!
! The routine is called multiple times during a typical ONETEP calculation,
! but we only want a warning output once. We suppress this warning using an
! implicitly SAVEd logical variable.
!
! When compiled with GFortran, the correct behaviour is observed regardless 
! of whether the program is compiled with/without OpenMP (-fopenmp flag).
!
! When compiled with Intel Fortran (17.0), the correct behaviour is only 
! observed when compiled without OpenMP (-qopenmp). When compiling with OpenMP
! the value of the SAVEd logical variable appears to be lost
!
! This stripped-down reproduction should compile to an object file, but will 
! require additional work to make executable.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine prepare_boundary_conditions(boundary_conditions, charge_density, n1, n2, n3)

  use, intrinsic :: iso_fortran_env
!$use omp_lib, only: omp_get_max_threads()

  implicit none

  ! Parameters
  integer, parameter :: DP = kind(1.0d0)

  ! Arguments
  real(kind=DP), intent(out)  :: boundary_conditions(:,:,:)
  real(kind=DP), intent(in)   :: charge_density(:,:,:)
  integer, intent(in)         :: n1, n2, n3

  ! Local variables
  integer :: i1, i2, i3, j
  integer :: nf1, nf2, nf3
  integer :: face
  integer :: n_charge_first, n_charge_last
!$integer :: num_threads
  real(kind=DP), allocatable, dimension(:)      :: charge_array
  real(kind=DP), allocatable, dimension(:,:)    :: position_array
  real(kind=DP), allocatable, dimension(:,:)    :: v_face
  real(kind=DP) :: v_value
  real(kind=DP), dimension(3) :: grid_vec
  real(kind=DP), dimension(3) :: r_i, r_j
  real(kind=DP) :: d
  real(kind=DP) :: q
! logical, save :: warning_issued = .false.  ! Explicit SAVE attribute solves issue
  logical       :: warning_issued = .false.  ! Implicit SAVE attribute appears not
                                             ! to be respected when compiled with 
                                             ! OpenMP with Intel Fortran 17.0

  boundary_conditions = 0.0_DP

  write(OUTPUT_UNIT,*) "[Value on entry] warning_issued == ", warning_issued

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!! Initialize arrays of charges and their positions in cell !!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !grid_vec       = ...
  !position_array = ...
  !charge_array   = ...
  !n_charge_first = ...
  !n_charge_last  = ...
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Loop over faces of the simulation cell, compute potential on face
  ! and copy to boundary_conditions array.

  do face = 1, 6
     ! Top or bottom face (XY): 1=x, 2=y, 3=z
     if(face == 1 .or. face == 2) then
        nf1 = n1
        nf2 = n2
        nf3 = n3
     end if
     ! Left or right face (YZ): 1=y, 2=z, 3=x
     if(face == 3 .or. face == 4) then
        nf1 = n2
        nf2 = n3
        nf3 = n1
     end if
     ! Front or back (XZ): 1=x, 2=z, 3=y
     if(face == 5 .or. face == 6) then
        nf1 = n1
        nf2 = n3
        nf3 = n2
     end if

     allocate(v_face(nf1,nf2))

     v_face = 0.0_DP

     if(face == 1 .or. face == 3 .or. face == 5) then
        i3 = 1     ! top, left or front
     else
        i3 = nf3   ! bottom, right or back
     end if

!$   num_threads = omp_get_max_threads()

     ! Loop over points of the current face of simulation cell
!$OMP PARALLEL DO NUM_THREADS(n_threads_max) DEFAULT(NONE) &
!$OMP PRIVATE(i1,i2,r_i,r_j,v_value,j,q,d) &
!$OMP SHARED(i3,nf1,nf2,grid_vec,n_charge_first,n_charge_last, &
!$OMP      position_array,charge_array,warning_issued, &
!$OMP      v_face)
     do i2=1, nf2
        do i1=1, nf1

           r_i(1) = real((i1-1),kind=DP) * grid_vec(1)
           r_i(2) = real((i2-1),kind=DP) * grid_vec(2)
           r_i(3) = real((i3-1),kind=DP) * grid_vec(3)

           v_value = 0.0_DP

           do j = n_charge_first, n_charge_last
              r_j(1) = position_array(j,1)
              r_j(2) = position_array(j,2)
              r_j(3) = position_array(j,3)
              q = charge_array(j)
              d = sqrt(sum( (r_i(1:3) - r_j(1:3) )**2))

              if(abs(d) < 1D-2) then
                 if(.not. warning_issued) then
                    write(OUTPUT_UNIT,'(a)') 'WARNING: Non-zero charge close to face'
                 end if
                 if(abs(q) > 1D-7) then
                    write(OUTPUT_UNIT,'(a)') 'ERROR: Non-zero charge density on or &
                         &very close to cell boundary.'
                    !!! Call routine to abort program here !!!
                 else
                    ! jd: Unless it's just the ringing, then ignore it
                    if(.not. warning_issued) then
                       write(OUTPUT_UNIT,'(a)') 'WARNING: Possible ringing of density &
                            & on the face of the simulation cell.'
                       warning_issued = .true.
                    end if
                    ! Avoid the singularity from q / d
                    q = 0D0
                    d = 1D0
                 end if
              end if
              v_value = v_value + q / d
           end do

           v_face(i1,i2) = v_value

        end do
     end do
!$OMP END PARALLEL DO

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !!!!!!!!!!!!!!! Copy v_face to boundary_conditions array !!!!!!!!!!!!!!!!
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     deallocate(v_face)

  end do

  write(OUTPUT_UNIT,*) "[Value on exit]  warning_issued == ", warning_issued

end subroutine prepare_boundary_conditions
