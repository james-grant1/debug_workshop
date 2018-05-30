module log_mod
  use constants_mod, only : str_long, str_max_filename
  implicit none

  integer, public, parameter :: LOG_LEVEL_ERROR   = 200
  integer, public, parameter :: LOG_LEVEL_WARNING = 150
  integer, public, parameter :: LOG_LEVEL_INFO    = 100
  integer, public, parameter :: LOG_LEVEL_DEBUG   =  50
  integer, public, parameter :: LOG_LEVEL_TRACE   =   0

  character( str_long + str_max_filename ), public :: log_scratch_space

  integer, private :: logging_level = LOG_LEVEL_INFO
  integer, private :: funit
  

  contains
    
    subroutine init_log()
      implicit none
      funit=12
      !open the file
      open(unit=funit,file="bug_test.dat",status="unknown") 

      call log_set_level(LOG_LEVEL_INFO)
    end subroutine init_log


  subroutine log_set_level(level)
    
    implicit none

    integer, intent( in ) :: level

    logging_level = level

  end subroutine log_set_level
  

  subroutine log_event(message, level)
    implicit none
    character (*), intent(in) :: message
    integer,       intent(in) :: level

    write(funit,'(A)') trim(message)
    if(level>=LOG_LEVEL_ERROR) then
       stop
    end if

  end subroutine log_event

  subroutine finalise_log()
    implicit none
    close(funit)
  end subroutine finalise_log

end module log_mod
