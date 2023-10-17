PROGRAM orbit
IMPLICIT NONE

!DECLARATION
INTEGER, PARAMETER :: P = 1200
REAL, PARAMETER :: PI = 3.14
INTEGER :: Theta, i
REAL :: R, Epsilon=0

!EXECUTION
 
outer :DO i=0, 2, 1 !LOOP THREE TIMES  
  inner: DO Theta=0, 360, 1 !LOOP OVER THETA
    R = P/(1-(Epsilon)*cos(2*PI*Theta/360)) 
    WRITE(*,*) R     
  END DO inner   
  Epsilon = Epsilon + 0.25 !INCREMENT   
END DO outer

STOP
END PROGRAM orbit
