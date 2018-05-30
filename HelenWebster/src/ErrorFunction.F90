!-------------------------------------------------------------------------------------------------------------

Program Bug
! Calculates the solution of the equation: 2E17 * [1-Erf(X)] = 1 by interval
! bisection. Now 1-Erf(X) = Erfc(X). Also calculates the solution of the 
! equation 2E17 * Erfc(X) = 1 by interval bisection. Note two answers differ!

  Implicit None


  Real(4) :: X1, X2          ! Inital bounds for the root
  Real(4) :: XLower, XUpper  ! Bounds on the root
  Real(4) :: YLower, YUpper
    
  Real(8), Parameter :: Pi     = 3.141592653589793238462643_8 ! Pi
  Integer, Parameter :: nIter = 100   ! Number of iterations

  ! Set lower and upper intial bounds
  X1 = 1.0
  X2 = 7.0
  
  ! Calculates the solution of the equation 2E17 * [1-Erf(X)] = 1
  YLower = 2.0E17 * (1.0 - Erf(X1)) - 1.0
  YUpper = 2.0E17 * (1.0 - Erf(X2)) - 1.0
  Write(6,*) 'Y ', YLower,' at X ',X1
  Write(6,*) 'Y ', YUpper,' at X ',X2
  
  XLower = X1
  XUpper = X2
  If (YUpper * YLower < 0.0) Then
    Write(6,*) 'Solution of Erf eq exists between X = ',XLower,' and ',XUpper
    Call EqnBisection(XUpper, XLower, YLower, YUpper, nIter)
    Write(6,*) 'Solution of Erf eq exists between X = ',XLower,' and ',XUpper
  Else If (YUpper * YLower > 0.0) Then
    Write(6,*) 'No solution of Erf eq exists between X = ',XLower,' and ',XUpper
    Stop
  Else If (YUpper == 0.0) Then
    Write(6,*) 'Solution of Erf eq is X = ',XUpper
  Else If (YLower == 0.0) Then
    Write(6,*) 'Solution of Erf eq is X = ',XLower
  End If

  ! Calculates the solution of the equation 2E17 * Erfc(X) = 1
  YLower = 2.0E17 * Erfc(X1) - 1.0
  YUpper = 2.0E17 * Erfc(X2) - 1.0
  Write(6,*) 'Y ', YLower,' at X ',X1
  Write(6,*) 'Y ', YUpper,' at X ',X2
  
  XLower = X1
  XUpper = X2
  If (YUpper * YLower < 0.0) Then
    Write(6,*) 'Solution of Erfc eq exists between X = ',XLower,' and ',XUpper
    Call EqncBisection(XUpper, XLower, YLower, YUpper, nIter)
    Write(6,*) 'Solution of Erfc eq exists between X = ',XLower,' and ',XUpper
  Else If (YUpper * YLower > 0.0) Then
    Write(6,*) 'No solution of Erfc eq exists between X = ',XLower,' and ',XUpper
    Stop
  Else If (YUpper == 0.0) Then
    Write(6,*) 'Solution of Erfc eq is X = ',XUpper
  Else If (YLower == 0.0) Then
    Write(6,*) 'Solution of Erfc eq is X = ',XLower
  End If


End Program Bug

!-------------------------------------------------------------------------------------------------------------

Function Erf(X)
! Calculates the error function using an approximation given by Abramowitz and Stegun, Handbook of
! mathematical functions, Dover Publications (1965).
! X > 0

  Implicit None
  ! Argument list:
  Real(8), Intent(In) :: X ! Argument of error function.
  ! Function result:
  Real(8) :: Erf ! Value of error function at X.
  ! Local parameters:
  Real(8), Parameter :: P  =  0.3275911_8   !} Constants used in numerical approximation to the error
  Real(8), Parameter :: A1 =  0.254829592_8 !} function.
  Real(8), Parameter :: A2 = -0.284496736_8 !}
  Real(8), Parameter :: A3 =  1.421413741_8 !}
  Real(8), Parameter :: A4 = -1.453152027_8 !}
  Real(8), Parameter :: A5 =  1.061405429_8 !}
  ! Locals:
  Real(8) :: Temp ! Temporary variable.

  Temp = 1.0 / (1.0 + P * Abs(X))
  Erf = 1.0 - Exp( - X*X) * (A1*Temp + A2*Temp**2 + A3*Temp**3 + A4*Temp**4 + A5*Temp**5)

End Function Erf

!-------------------------------------------------------------------------------------------------------------

Function Erfc(X)
! Calculates the complementary error function using an approximation given by Abramowitz and Stegun, Handbook of
! mathematical functions, Dover Publications (1965).
! X > 0

  Implicit None
  ! Argument list:
  Real(8), Intent(In) :: X ! Argument of complementary error function.
  ! Function result:
  Real(8) :: Erfc ! Value of complementary error function at X.
  ! Local parameters:
  Real(8), Parameter :: P  =  0.3275911_8   !} Constants used in numerical approximation to the error
  Real(8), Parameter :: A1 =  0.254829592_8 !} function.
  Real(8), Parameter :: A2 = -0.284496736_8 !}
  Real(8), Parameter :: A3 =  1.421413741_8 !}
  Real(8), Parameter :: A4 = -1.453152027_8 !}
  Real(8), Parameter :: A5 =  1.061405429_8 !}
  ! Locals:
  Real(8) :: Temp ! Temporary variable.

  Temp = 1.0 / (1.0 + P * Abs(X))
  Erfc = Exp( - X*X) * (A1*Temp + A2*Temp**2 + A3*Temp**3 + A4*Temp**4 + A5*Temp**5)

End Function Erfc

!-------------------------------------------------------------------------------------------------------------

Subroutine EqncBisection(XUpper, XLower, EqnLower, EqnUpper, nIter)
! Solves complementary error function equation by interval bisection.

  Implicit None
  ! Argument list:
  Real(4) :: XUpper
  Real(4) :: XLower
  Real(4) :: EqnUpper
  Real(4) :: EqnLower
  Integer :: nIter
  
  ! Locals:
  Real(4) :: X
  Real(4) :: EqnMid
  Integer :: i
  
  Do i = 1,nIter
    X = (XUpper + XLower) / 2.0
    EqnMid = 2.0E17 * Erfc(X) - 1.0
    
    If (EqnMid == 0.0) Then
      Write(6,*) 'Solution of Eqn is X = ',X
      Exit
    Else If (EqnMid * EqnUpper > 0.0) Then
      XUpper = X
      EqnUpper = EqnMid
    Else
      XLower = X
      EqnLower = EqnMid
    End If      
    
  End Do
  
End Subroutine EqncBisection

!-------------------------------------------------------------------------------------------------------------

Subroutine EqnBisection(XUpper, XLower, EqnLower, EqnUpper, nIter)
! Solves equation by interval bisection.

  Implicit None
  ! Argument list:
  Real(4) :: XUpper
  Real(4) :: XLower
  Real(4) :: EqnUpper
  Real(4) :: EqnLower
  Integer :: nIter
  
  ! Locals:
  Real(4) :: X
  Real(4) :: EqnMid
  Integer :: i
  
  Do i = 1,nIter
    X = (XUpper + XLower) / 2.0
    EqnMid = 2.0E17 * (1.0 - Erf(X)) - 1.0
    
    If (EqnMid == 0.0) Then
      Write(6,*) 'Solution of Eqn is X = ',X
      Exit
    Else If (EqnMid * EqnUpper > 0.0) Then
      XUpper = X
      EqnUpper = EqnMid
    Else
      XLower = X
      EqnLower = EqnMid
    End If      
    
  End Do
  
End Subroutine EqnBisection

!-------------------------------------------------------------------------------------------------------------
