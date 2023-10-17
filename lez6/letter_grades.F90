!NOTA: IF STATEMENT INLINE E' POSSIBILE SOLO PER UN SINGOLO IF, E NON VA END IF, E' GIA CHIUSO

PROGRAM LETTER_GRADES
  IMPLICIT NONE
  
  !DECLARATIONS
  INTEGER :: numerical_grade
  CHARACTER (len=1) :: letter_grade 
  
  !EXECUTION
  
  !IO
  WRITE(*,*) 'Insert numerical grade: between 0 and 100'
  READ(*,*) numerical_grade
  
  !DECISIONAL STRUCTURE
  IF (numerical_grade >95 ) THEN
    letter_grade = 'A'
  ELSE IF (numerical_grade > 85) THEN 
    letter_grade = 'B'
  ELSE IF (numerical_grade > 76) THEN 
    letter_grade = 'C'
  ELSE IF (numerical_grade > 66) THEN 
    letter_grade = 'D'
  ELSE IF (numerical_grade >= 0) THEN 
    letter_grade = 'E'
  ELSE 
    WRITE(*,*)'ERROR: numerical grade is not valid!'
    letter_grade = 'F'
  END IF
  
  !RESULT
  IF (letter_grade /= 'F') THEN
  WRITE(*,*) 'Numerical grade is: ', letter_grade
  END IF

STOP
END PROGRAM LETTER_GRADES
