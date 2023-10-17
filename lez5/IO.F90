PROGRAM IO
  
  IMPLICIT NONE
  
  !DECLARATIONS
  INTEGER :: n
  
  !EXECUTION
  WRITE(*,*) 'Inserire numero intero'
  READ(*,*) n
  OPEN(UNIT=33, FILE='dati.dat', STATUS='NEW', IOSTAT=ios)  
  WRITE(33,*) n
  
  STOP
  
END PROGRAM
