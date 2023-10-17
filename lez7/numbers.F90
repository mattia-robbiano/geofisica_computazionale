PROGRAM numbers
  IMPLICIT NONE

!DICHIARO VARIABILI
  INTEGER :: number_to_convert, digit, divide
  INTEGER i
  i = 0
  divide = 1000
   
!IO SECTION
  WRITE(*,*) 'Insert number to be converted:'
  READ(*,*) number_to_convert

!CHECK NUMBER 4 DIGITS
  IF (number_to_convert<1000 .OR. number_to_convert>9999) THEN
    WRITE(*,*) 'The number is not 4 digits.'
    STOP
  END IF
  
  prova: DO i=1, 4, 1
!SPLITTING DIGITS     
    digit = number_to_convert/divide
    
    SELECT CASE(digit)
    CASE(0)
      WRITE(*,*) 'ZERO'
    CASE(1)
      WRITE(*,*) 'UNO'
    CASE(2)
      WRITE(*,*) 'DUE'
    CASE(3)
      WRITE(*,*) 'TRE'
    CASE(4)
      WRITE(*,*) 'QUATTRO'
    CASE(5)
      WRITE(*,*) 'CINQUE'
    CASE(6)
      WRITE(*,*) 'SEI'
    CASE(7)
      WRITE(*,*) 'SETTE'
    CASE(8)
      WRITE(*,*) 'OTTO'
    CASE(9)
      WRITE(*,*) 'NOVE'
    END SELECT
    
!UPDATE 
    number_to_convert = number_to_convert - digit * divide 
    divide = divide / 10
  END DO prova
  
  STOP
END PROGRAM numbers
