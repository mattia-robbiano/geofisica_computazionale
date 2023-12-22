PROGRAM MAIN
    USE IOSTREAM
    USE RadioSondaggi
    IMPLICIT NONE

    CHARACTER (LEN=500) :: INPUT_FILENAME_A, INPUT_FILENAME_B, OUTPUT_FILENAME_A, OUTPUT_FILENAME_B
    NAMELIST /Files/ INPUT_FILENAME_A, INPUT_FILENAME_B, OUTPUT_FILENAME_A, OUTPUT_FILENAME_B
    CHARACTER (LEN=500) :: INTESTAZIONE_A, INTESTAZIONE_B
    CHARACTER (LEN=500) :: msg
    LOGICAL :: ERROR
    INTEGER :: IO, i, NumberOfLinesA, NumberOfLinesB, position1, position2, position3
    REAL :: h0A, p0A, T0A, h0B, p0B, T0B
    !----------------------------------------
    !VARIABILI ES1
    REAL :: z1, z2, p1, p2, T1, T2
    !----------------------------------------
    !VARIBILI ES2
    REAL, ALLOCATABLE :: DataA(:,:), DataB(:,:)
    REAL :: zMax

    !LETTURA NAMELIST
    OPEN(UNIT=20, FILE='input.nml', STATUS='OLD', ACTION='READ', IOSTAT=IO, iomsg=msg)
    IF(IO>0) STOP msg
    READ(20,NML=Files, IOSTAT=IO, iomsg=msg)
    IF(IO>0) STOP msg

    !APRO I FILE
    CALL OPEN_INPUT_FILE(21, INPUT_FILENAME_A, ERROR)
    IF(ERROR) STOP 'ERROR OPENING INPUT FILE A'
    CALL OPEN_INPUT_FILE(22, INPUT_FILENAME_B, ERROR)
    IF(ERROR) STOP 'ERROR OPENING INPUT FILE B'
    CALL OPEN_OUTPUT_FILE(23, OUTPUT_FILENAME_A, ERROR)
    IF(ERROR) STOP 'ERROR OPENING OUTPUT FILE A'
    CALL OPEN_OUTPUT_FILE(24, OUTPUT_FILENAME_B, ERROR)
    IF(ERROR) STOP 'ERROR OPENING OUTPUT FILE B'

    !CONTO RIGHE E CONTROLLO INTEGRITA'
    CALL COUNT_LINES(21, NumberOfLinesA, ERROR)
    IF(ERROR) STOP 'ERROR COUNTING LINES IN INPUT FILE A'
    REWIND(21)

    CALL COUNT_LINES(22, NumberOfLinesB, ERROR)
    IF(ERROR) STOP 'ERROR COUNTING LINES IN INPUT FILE B'
    REWIND(22)

    !LEGGO INTESTAZIONE
    READ(21,'(A)',IOSTAT=IO) INTESTAZIONE_A
    IF(IO>0) STOP 'ERROR READING INPUT FILE A'
    READ(22,'(A)',IOSTAT=IO) INTESTAZIONE_B
    IF(IO>0) STOP 'ERROR READING INPUT FILE B'

    !CERCO h0,p0, T0 NELL'INTESTAZIONE
    position1 = INDEX(INTESTAZIONE_A, 'h0')
    position2 = INDEX(INTESTAZIONE_A, 'p0')
    position3 = INDEX(INTESTAZIONE_A, 'T0')
    READ(INTESTAZIONE_A(position1+3:position2-1),*) h0A
    READ(INTESTAZIONE_A(position2+3:position3-1),*) p0A
    READ(INTESTAZIONE_A(position3+3:LEN_TRIM(INTESTAZIONE_A)),*) T0A

    position1 = INDEX(INTESTAZIONE_B, 'h0')
    position2 = INDEX(INTESTAZIONE_B, 'p0')
    position3 = INDEX(INTESTAZIONE_B, 'T0')
    READ(INTESTAZIONE_B(position1+3:position2-1),*) h0B
    READ(INTESTAZIONE_B(position2+3:position3-1),*) p0B
    READ(INTESTAZIONE_B(position3+3:LEN_TRIM(INTESTAZIONE_B)),*) T0B

    !SCRIVO DATI STAZIONE A TEMRINALE
    WRITE(*,'(A19,A10)') 'Dati file stazione: ', TRIM(INPUT_FILENAME_A)
    WRITE(*,'(3(A5,F7.1))') 'h0 =', h0A, ' p0 =', p0A, ' T0 =', T0A
    WRITE(*,'(A19,A10)') 'Dati file stazione: ', TRIM(INPUT_FILENAME_B)
    WRITE(*,'(3(A5,F7.1))') 'h0 =', h0B, ' p0 =', p0B, ' T0 =', T0B

    !----------------------------------------

    !IMPOSTO DATI INIZIALI, LEGGO DATI E SCRIVO SU FILE
    p1 = p0A
    T1 = T0A
    z1 = h0A
    DO i=1,NumberOfLinesA-1
        READ(21,*) p2, T2
        CALL GetAltitude(z2, z1, p2, p1, T2, T1)
        WRITE(23,'(F10.2,1X,F10.2,1X,F10.2)') z2, p2, T2
        p1 = p2
        T1 = T2
        z1 = z2
    END DO

    p1 = p0B
    T1 = T0B
    z1 = h0B
    DO i=1,NumberOfLinesB-1
        READ(22,*) p2, T2
        CALL GetAltitude(z2, z1, p2, p1, T2, T1)
        WRITE(24,'(F10.2,1X,F10.2,1X,F10.2)') z2, p2, T2
        p1 = p2
        T1 = T2
        z1 = z2
    END DO

    CLOSE(21)
    CLOSE(22)
    CLOSE(23)
    CLOSE(24)
    !----------------------------------------
    !APERTURA FILE
    CALL OPEN_INPUT_FILE(25, OUTPUT_FILENAME_A, ERROR)
    IF(ERROR) STOP 'ERROR OPENING OUTPUT FILE A'
    CALL OPEN_INPUT_FILE(26, OUTPUT_FILENAME_B, ERROR)
    IF(ERROR) STOP 'ERROR OPENING OUTPUT FILE B'
    CALL OPEN_OUTPUT_FILE(27, 'gridA.txt', ERROR)
    IF(ERROR) STOP 'ERROR OPENING OUTPUT FILE A'

    !RIEMPO DataA e DataB
    ALLOCATE(DataA(NumberOfLinesA,3))
    ALLOCATE(DataB(NumberOfLinesB,3))
    DO i=1,NumberOfLinesA-1
        READ(25,*) DataA(i,1), DataA(i,2), DataA(i,3)
    END DO
    DO i=1,NumberOfLinesB-1
        READ(26,*) DataB(i,1), DataB(i,2), DataB(i,3)
    END DO

    !Calcolo estremi dati sperimentali
    IF(DataA(1,1)<=DataB(1,1)) THEN
        z1 = DataB(1,1)
    ELSE
        z1 = DataA(1,1)
    END IF
    IF(DataA(NumberOfLinesA-1,1)>=DataB(NumberOfLinesB-1,1)) THEN
        zMax = DataB(NumberOfLinesB-1,1)
    ELSE
        zMax = DataA(NumberOfLinesA-1,1)
    END IF

    !Calcolo estremo inferiore griglia
    IF(abs(MOD(z1,200.0)-0.0)>0.000001) THEN
        z2 = z1 - MOD(z1,200.0) + 200.0
    ELSE
        z2 = z1 + 200
    END IF

    !Cerco temperatura strato nelle misure A
    write(*,*) z1,z2,p1,T1

    CLOSE(25)
    CLOSE(26)
END PROGRAM MAIN
