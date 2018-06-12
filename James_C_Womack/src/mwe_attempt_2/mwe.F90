module mwe

  implicit none

  private

  public :: prepare_boundary_conditions

  integer, parameter, public :: DP = kind(1.0d0)

  contains


  subroutine prepare_boundary_conditions(boundary_conditions, charge_density, n1, n2, n3, &
             grid_vec)

    use, intrinsic :: iso_fortran_env
!$  use omp_lib, only: omp_get_max_threads

    implicit none

    ! Arguments
    real(kind=DP), intent(out)  :: boundary_conditions(:,:,:)
    real(kind=DP), intent(in)   :: charge_density(:,:,:)
    integer, intent(in)         :: n1, n2, n3
    real(kind=DP), intent(in)   :: grid_vec(3)

    ! Local variables
    integer :: i1, i2, i3, j
    integer :: nf1, nf2, nf3
    integer :: face
    integer :: n_charge_first, n_charge_last
!$  integer :: num_threads
    real(kind=DP), allocatable, dimension(:)      :: charge_array
    real(kind=DP), allocatable, dimension(:,:)    :: position_array
    real(kind=DP), allocatable, dimension(:,:)    :: v_face
    real(kind=DP) :: v_value
    real(kind=DP), dimension(3) :: r_i, r_j
    real(kind=DP) :: d
    real(kind=DP) :: q
  ! logical, save :: warning_issued = .false.  ! Explicit SAVE attribute solves issue
    logical       :: warning_issued = .false.  ! Implicit SAVE attribute appears not
                                               ! to be respected when compiled with
                                               ! OpenMP with Intel Fortran 17.0

    integer, parameter :: ncharge_batch = 100
    integer, parameter :: ifake = 42
    real(kind=DP), parameter :: charge_fake = 1.0e-8_DP
    real(kind=DP), parameter, dimension(3) :: position_fake = [ 1.0e-3_DP, 1.0e-3_DP, 1.0e-3_DP ]

    boundary_conditions = 0.0_DP

    write(OUTPUT_UNIT,*) "[Value on entry] warning_issued == ", warning_issued

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!! Initialize arrays of charges and their positions in cell !!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    allocate(charge_array(ncharge_batch))
    charge_array = 0.0_DP
    allocate(position_array(ncharge_batch,3))
    position_array = 0.0_DP

    ! Add fake charge near cell boundary
    charge_array(ifake) = charge_fake
    position_array(ifake,1) = position_fake(1)
    position_array(ifake,2) = position_fake(2)
    position_array(ifake,3) = position_fake(3)

    n_charge_first = 1
    n_charge_last = ncharge_batch

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
!$OMP PARALLEL DO NUM_THREADS(num_threads) DEFAULT(NONE) &
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

    deallocate(charge_array)
    deallocate(position_array)

  end subroutine prepare_boundary_conditions

end module mwe
