PROGRAM circle
IMPLICIT NONE
!DECLARATION
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
REAL(KIND=DBL) :: r
CHARACTER(len=100) :: filename
LOGICAL :: fl_ok
!EXECUTION
WRITE(*,*) 'Fornire raggio cerchio e nome file output:'
READ(*,*) r, filename
WRITE(*,*) 'Raggio:', r,'filename:',filename
INQUIRE(file=filename,exist=fl_ok)
IF(fl_ok==FALSE) THEN
  WRITE(*,*) 'Il file', filename, 'esiste già.'
END IF
END PROGRAM circle
