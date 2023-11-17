PROGRAM sonico
    USE IOSTREAM
    USE MATH
    IMPLICIT NONE

    CHARACTER (LEN=200) :: INPUT_FILENAME = 'dati.txt'
    CHARACTER (LEN=200) :: OUTPUT_FILENAME = 'risultati.txt'
    CHARACTER (LEN=200) :: msg
    LOGICAL :: ERROR
    INTEGER :: NumberOfLines, i, IO, Acc1, Acc2, Acc3
    REAL :: Phi, Theta, Psi, c, 
    NAMELIST / ANGOLI / Phi, Theta, Psi
    
    !RICHIEDO NOMI FILE
    WRITE(*,*) 'Enter input filename'
    !READ(*,*) INPUT_FILENAME
    WRITE(*,*)
    WRITE(*,*) 'Enter output filename'
    !READ(*,*) OUTPUT_FILENAME
    WRITE(*,*)

    !APRO FILE
    CALL OPEN_INPUT_FILE(20,INPUT_FILENAME,ERROR)
    IF(ERROR) STOP 'Error opening input file'
    CALL OPEN_OUTPUT_FILE(21,OUTPUT_FILENAME,ERROR)
    IF(ERROR) STOP 'Error opening output file'
    CALL OPEN_INPUT_FILE(22,'NAMELIST.txt',ERROR)
    IF(ERROR) STOP 'Error opening namelist file'

    !LETTURA NAMELIST
    READ(22,NML=ANGOLI, IOSTAT=IO, iomsg=msg)
    IF(IO>0) STOP msg

    !CONTO RIGHE
    CALL SKIP_LINE(20,4, ERROR)
    CALL COUNT_LINES(NumberOfLines, ERROR)
    !WRITE(*,'(A21,1X,I10)') 'Number of input lines:',NumberOfLines
    REWIND(20)
    CALL SKIP_LINE(20,4, ERROR)

    !LEGGO INPUT E RUOTO PRIME TRE COMPONENTI CON VECTOR_ROTATION DI ANGOLI EULERO, POI STAMPO SU FILE
    DO i=1,NumberOfLines
        READ(20,*) AngoloEulero, c, Acc1, Acc2, Acc3
        AngoloEulero = VECTOR_ROTATION(/1,1,1/,/1,1,1/, Phi, Theta, Psi)
        WRITE(21,*) AngoloEulero
    END DO


    !CHIUDO FILE
    CLOSE(20)
    CLOSE(21)
    
END PROGRAM sonico
