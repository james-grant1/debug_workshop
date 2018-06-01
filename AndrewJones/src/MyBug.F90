! MyBug.F90
!
! A bug in memory management.
! This is a highly idealised version of a latent non-critical bug
! recently discovered in our NAME dispersion model.
!
! Synopsis:
! Field values being stored in dynamically sized array within a derived type.
! Original code set up array using a pointer (as allocatable arrays within
! derived types are not in Fortran 90 standard) and worked for many years...
! A recent change to use allocatable arrays (now supported by compiler)
! caused the model to fail at start of second case with an allocate error.
!
! Andrew Jones, Met Office, May 2018

!-------------------------------------------------------------------------------

Program MyBug

  Use RoutinesModule

  Implicit None
  
  Integer, Parameter :: N = 100000
  Real(4), Parameter :: a = 1.0
  Real(4), Parameter :: b = 2.0
  
  Integer, Parameter :: nCases = 100
  Integer            :: iCase
  
  Type(MyData_) :: MyData
  
  Real(4)       :: Answer
  
  ! Loop over cases
  Do iCase = 1, nCases
    
    ! Initialise data structure to store required information
    Call InitMyData(MyData, N)
    
    ! Set up realisation
    Call SetUpMyData(MyData, a, b, 0.01 * iCase)
    
    ! Get answer
    Answer = CalcResult(MyData)
    Write (Unit = *, Fmt = *) "Answer for case ", iCase, " is ", Answer
    
  End Do

End Program MyBug

!-------------------------------------------------------------------------------
