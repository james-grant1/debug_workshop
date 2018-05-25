module fix_me_utils

  use, intrinsic :: iso_fortran_env

  implicit none

  private

  integer, parameter, public :: DP = kind(1.0d0)

  public :: build_Rmat, apply_Rmat

  contains

    subroutine build_Rmat(n_rotations,angle_increment,Rmat)

!$    use, intrinsic :: omp_lib, only: omp_get_max_threads

      implicit none

      ! Arguments
      integer, intent(in)        :: n_rotations
      real(kind=DP), intent(in)  :: angle_increment
      real(kind=DP), intent(out) :: Rmat(2,2,n_rotations)

      ! Local variables
      integer       :: irot
      integer       :: num_threads
      real(kind=DP) :: theta, st, ct

      num_threads = 1
!$    num_threads = omp_get_max_threads()

!$OMP PARALLEL DO NUM_THREADS(num_threads) DEFAULT(NONE) &
!$OMP PRIVATE(irot,theta,st,ct) &
!$OMP SHARED(Rmat,angle_increment)
      do irot = 1, n_rotations
         theta = angle_increment*irot
         st = sin(theta)
         ct = cos(theta)

         Rmat(1:2,1,irot) = [ ct, -st ]
         Rmat(1:2,2,irot) = [ st, ct ]
      end do
!$OMP END PARALLEL DO
    end subroutine build_Rmat

    subroutine apply_Rmat(n_rotations,Rmat,vec_init,vec_result)

!$    use, intrinsic :: omp_lib, only: omp_get_max_threads
      
      implicit none

      ! Arguments
      integer, intent(in)        :: n_rotations
      real(kind=DP), intent(in)  :: Rmat(2,2,n_rotations)
      real(kind=DP), intent(in)  :: vec_init(2)
      real(kind=DP), intent(out) :: vec_result(2,n_rotations)

      ! Local variables
      integer       :: irot
      integer       :: num_threads
      integer       :: n_calls = 0

      num_threads = 1
!$    num_threads = omp_get_max_threads()

!$OMP PARALLEL DO NUM_THREADS(num_threads) DEFAULT(NONE) &
!$OMP PRIVATE(irot) &
!$OMP SHARED(Rmat,vec_result,vec_init,n_calls)
      do irot = 1, n_rotations
         vec_result(:,irot) = matmul( Rmat(:,:,irot), vec_init(:) )
      end do
!$OMP END PARALLEL DO

      n_calls = n_calls + 1

      write(OUTPUT_UNIT,'(a,i0,a)') "apply_Rmat called ", n_calls, " times."

    end subroutine apply_Rmat
end module fix_me_utils
