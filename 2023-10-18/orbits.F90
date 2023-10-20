PROGRAM orbit
  IMPLICIT NONE

  !DECLARATION
  INTEGER, PARAMETER :: P = 1200
  REAL, PARAMETER :: PI = 3.14
  INTEGER :: Theta, i,iostat
  REAL :: R, Epsilon=0
  CHARACTER(len=100) :: Filename
  CHARACTER(3)::OverwriteYesNo = 'no'
  LOGICAL :: check
  
  !EXECUTION
  WRITE(*,*) 'Provide filename:'
  READ(*,*) filename
  INQUIRE(FILE = filename, EXIST = check)
  IF(check) THEN
     WRITE(*,'(A,A10,A)') 'File ',Filename,' already exists. Do you want to overwrite? (yes/no)'
     READ(*,*) OverwriteYesNo
     STOP
    ELSE IF (OverwriteYesNo=='yes') THEN
     OPEN(UNIT=30, FILE=Filename, STATUS='replace', FORM='FORMATTED', ACTION='WRITE',IOSTAT=iostat)
     outer1 :DO i=0, 2, 1 !LOOP THREE TIMES 
       WRITE(30,90)'Epsilon:', Epsilon 
       WRITE(30,'(A)') 'Theta(°)    Raggio(Km)'
       inner1: DO Theta=0, 360, 1 !LOOP OVER THETA
          R = P/(1-(Epsilon)*cos(2*PI*Theta/360)) 
          WRITE(30,100) Theta, R/1000     
       END DO inner1
       Epsilon = Epsilon + 0.25 !INCREMENT 
       WRITE(30,'(A)') '------------------------------------'
       WRITE(30,*) NEW_LINE('A')
     END DO outer1
  ELSE
     OPEN(UNIT=30, FILE=Filename, STATUS='new', FORM='FORMATTED', ACTION='WRITE',IOSTAT=iostat)
     outer :DO i=0, 2, 1 !LOOP THREE TIMES 
       WRITE(30,90)'Epsilon:', Epsilon 
       WRITE(30,'(A)') 'Theta(°)    Raggio(Km)'
       inner: DO Theta=0, 360, 1 !LOOP OVER THETA
          R = P/(1-(Epsilon)*cos(2*PI*Theta/360)) 
          WRITE(30,100) Theta, R/1000     
       END DO inner
       Epsilon = Epsilon + 0.25 !INCREMENT 
       WRITE(30,'(A)') '------------------------------------'
       WRITE(30,*) NEW_LINE('A')
     END DO outer
  END IF

  !FORMAT
90 FORMAT(A10, 1X, F4.2)
100 FORMAT(I3, 7X, F7.2)

END PROGRAM orbit
