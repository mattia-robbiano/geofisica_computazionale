PROGRAM sonico
    USE IOSTREAM
    USE MATH

    IMPLICIT NONE

    TYPE :: AngoliEulero
        REAL :: phi, theta, psi
    END TYPE AngoliEulero
    CHARACTER (LEN=500) :: INPUT
    CHARACTER (LEN=500) :: OUTPUT
    CHARACTER (LEN=500) :: msg
    CHARACTER(LEN=1) :: Trash
    REAL, ALLOCATABLE, DIMENSION(:) :: Acc
    REAL :: Phi, Theta, Psi, c, x, y, z, xRot, yRot, zRot
    REAL, PARAMETER :: null = 99.0
    INTEGER :: NumberOfLines,NumberOfAcc, i, IO
    LOGICAL :: ERROR
    TYPE(AngoliEulero) :: Angoli
    NAMELIST / Rotazioni / Phi, Theta, Psi
    NAMELIST / FileNames / INPUT, OUTPUT
    
    !APRO NAMELIST
    CALL OPEN_INPUT_FILE(22,'NAMELIST.nml',ERROR)
    IF(ERROR) STOP 'Error opening namelist file'
    !LETTURA ANGOLI
    READ(22,NML=Rotazioni, IOSTAT=IO, iomsg=msg)
    IF(IO>0) STOP msg
    Angoli%phi = Phi
    Angoli%theta = Theta
    Angoli%psi = Psi
    !LETTURE FILENAMES
    READ(22,NML=FileNames, IOSTAT=IO, iomsg=msg)
    IF(IO>0) STOP msg
    WRITE(*,*) 'Input file name: ', TRIM(INPUT)
    WRITE(*,*) 'Output file name: ', TRIM(OUTPUT)

    !APRO FILE
    CALL OPEN_INPUT_FILE(20,INPUT,ERROR)
    IF(ERROR) STOP 'Error opening input file'
    CALL OPEN_OUTPUT_FILE(21,OUTPUT,ERROR)
    IF(ERROR) STOP 'Error opening output file'

    !CONTO RIGHE
    CALL SKIP_LINE(20,4, ERROR)
    CALL COUNT_LINES(20,NumberOfLines, ERROR)
    WRITE(*,'(A22,1X,I10)') 'Number of input lines:',NumberOfLines
    REWIND(20)

    !LEGGO INTESTAZIONE
    CALL SKIP_LINE(20,1, ERROR)
    IF(ERROR) STOP 'Error skipping line'
    READ(20,*) Trash, Trash, Trash, NumberOfAcc
    WRITE(*,'(A31,1X,I1)') "Number of ancillary quantities:", NumberOfAcc
    REWIND(20)

    !ALLOCO ARRAY GRANDEZZE ACCESSORIE (ACC)
    ALLOCATE(Acc(NumberOfAcc), STAT=IO)
    IF(IO/=0) STOP 'Error allocating Acc'

    !LEGGO INPUT E RUOTO PRIME TRE COMPONENTI CON VECTOR_ROTATION POI STAMPO SU FILE
    CALL SKIP_LINE(20,4, ERROR)
    DO i=1,NumberOfLines
        READ(20,*) x,y,z, c, Acc
        !GESTISCO DATI NULLI
        IF(x>abs(null).OR. y>abs(null).OR. z>abs(null)) THEN
            WRITE(21,*) null,null,null, c, Acc
            WRITE(*,'(A,I10)') 'WARNING: null data in line:  ',i
            CYCLE
        END IF
        CALL VECTOR_ROTATION(x,y,z,xRot,yRot,zRot, Angoli%phi, Angoli%theta, Angoli%psi,.FALSE.)
        WRITE(21,*) xRot,yRot,zRot, c, Acc
    END DO

    !CHIUDO FILE
    CLOSE(20)
    CLOSE(21)
    CLOSE(22)
    DEALLOCATE(Acc)
    
END PROGRAM sonico
