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
  SELECT CASE (numerical_grade)
  CASE(96:100)
    letter_grade = 'A'
  CASE(87:95)
    letter_grade = 'B'
  CASE(77:86)
    letter_grade = 'C'
  CASE(67:76)
    letter_grade = 'D'
  CASE(0:66)
    letter_grade = 'E'
  CASE DEFAULT
    letter_grade = 'F'
  END SELECT
  
  !RESULT OUTPUT
  WRITE(*,*) 'Numerical grade is: ', letter_grade

STOP
END PROGRAM LETTER_GRADES
