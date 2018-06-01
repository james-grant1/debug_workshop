! Routines.F90
!
! A bug in memory management.
! This is a highly idealised version of a latent non-critical bug
! recently discovered in our NAME dispersion model.
!
! Andrew Jones, Met Office, May 2018

!-------------------------------------------------------------------------------

Module RoutinesModule

!-------------------------------------------------------------------------------

  Implicit None

!-------------------------------------------------------------------------------

! Derived type to model the eqn: y = a sin(kx) + b cos(kx), x = 1, ..., N.
! This is NOT used anywhere in NAME - it's just a simple example for storing
! some values in a derived type!

! Original version (using pointers)
Type :: MyData_
  Real(4)              :: a
  Real(4)              :: b
  Real(4)              :: k
  Real(4), Pointer     :: X(:)
  Real(4), Pointer     :: Y(:)
End Type MyData_

! Revised version (using allocatable arrays)
Type :: MyData2_
  Real(4)              :: a
  Real(4)              :: b
  Real(4)              :: k
  Real(4), Allocatable :: X(:)
  Real(4), Allocatable :: Y(:)
End Type MyData2_

!-------------------------------------------------------------------------------

  Contains
  
!-------------------------------------------------------------------------------

! Initialisation routine to allocate required array memory.
Subroutine InitMyData(MyData, N)

  Implicit None
  
  Type(MyData_), Intent(InOut) :: MyData
  Integer,       Intent(In)    :: N
  
  Integer :: i
  
  Allocate (MyData%X(N))
  Allocate (MyData%Y(N))
  
  Do i = 1, N
    MyData%X(i) = Real(i)
    MyData%Y(i) = 0.0
  End Do
  
End Subroutine InitMyData

!-------------------------------------------------------------------------------

! Set up each realisation.
Subroutine SetUpMyData(MyData, a, b, k)

  Implicit None
  
  Type(MyData_), Intent(InOut) :: MyData
  Real(4),       Intent(In)    :: a
  Real(4),       Intent(In)    :: b
  Real(4),       Intent(In)    :: k
  
  MyData%a = a
  MyData%b = b
  MyData%k = k

End Subroutine SetUpMyData

!-------------------------------------------------------------------------------

! Calculate some result.
Function CalcResult(MyData) Result(Answer)

  Implicit None
  
  Type(MyData_) :: MyData
  Real(4)       :: Answer
  
  MyData%Y(:) = MyData%a * Sin(MyData%k * MyData%X(:)) +  &
                MyData%b * Cos(MyData%k * MyData%X(:))
  
  Answer = Sum(MyData%Y(:))

End Function CalcResult

!-------------------------------------------------------------------------------

End Module RoutinesModule

!-------------------------------------------------------------------------------
