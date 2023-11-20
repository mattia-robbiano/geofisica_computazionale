PROGRAM sonico
    USE IOSTREAM
    USE MATH

    IMPLICIT NONE
    TYPE :: AngoliEulero
        REAL :: phi, theta, psi
    END TYPE AngoliEulero

    CHARACTER (LEN=200) :: INPUT_FILENAME
    CHARACTER (LEN=200) :: OUTPUT_FILENAME
    CHARACTER (LEN=200) :: msg
    CHARACTER(LEN=1) :: Trash
    LOGICAL :: ERROR
    INTEGER :: NumberOfLines,NumberOfAcc, i, IO
    REAL, ALLOCATABLE, DIMENSION(:) :: Acc
    REAL :: Phi, Theta, Psi, c, x, y, z, xRot, yRot, zRot
    NAMELIST / Input / Phi, Theta, Psi
    TYPE(AngoliEulero) :: Angoli
    
    !RICHIEDO NOMI FILE
    WRITE(*,*) 'Enter input filename'
    READ(*,*) INPUT_FILENAME
    WRITE(*,*)
    WRITE(*,*) 'Enter output filename'
    READ(*,*) OUTPUT_FILENAME
    WRITE(*,*)

    !APRO FILE
    CALL OPEN_INPUT_FILE(20,INPUT_FILENAME,ERROR)
    IF(ERROR) STOP 'Error opening input file'
    CALL OPEN_OUTPUT_FILE(21,OUTPUT_FILENAME,ERROR)
    IF(ERROR) STOP 'Error opening output file'
    CALL OPEN_INPUT_FILE(22,'NAMELIST.txt',ERROR)
    IF(ERROR) STOP 'Error opening namelist file'

    !LETTURA NAMELIST
    READ(22,NML=input, IOSTAT=IO, iomsg=msg)
    IF(IO>0) STOP msg
    Angoli%phi = Phi
    Angoli%theta = Theta
    Angoli%psi = Psi

    !CONTO RIGHE
    CALL SKIP_LINE(20,4, ERROR)
    CALL COUNT_LINES(NumberOfLines, ERROR)
    WRITE(*,'(A22,1X,I10)') 'Number of input lines:',NumberOfLines
    REWIND(20)

    !LEGGO INTESTAZIONE
    CALL SKIP_LINE(20,1, ERROR)
    IF(ERROR) STOP 'Error skipping line'
    READ(20,*) Trash, Trash, Trash, NumberOfAcc
    WRITE(*,'(A31,1X,I1)') "Number of ancillary quantities:", NumberOfAcc
    REWIND(20)

    !ALLOCO ACC
    ALLOCATE(Acc(NumberOfAcc), STAT=IO)
    IF(IO/=0) STOP 'Error allocating Acc'

    !LEGGO INPUT E RUOTO PRIME TRE COMPONENTI CON VECTOR_ROTATION POI STAMPO SU FILE
    CALL SKIP_LINE(20,4, ERROR)
    DO i=1,NumberOfLines
        READ(20,*) x,y,z, c, Acc
        CALL VECTOR_ROTATION(x,y,z,xRot,yRot,zRot, Angoli%phi, Angoli%theta, Angoli%psi,.FALSE.)
        WRITE(21,*) xRot,yRot,zRot, c, Acc
    END DO

    !CHIUDO FILE
    CLOSE(20)
    CLOSE(21)
    CLOSE(22)
    DEALLOCATE(Acc)
    
END PROGRAM sonico
