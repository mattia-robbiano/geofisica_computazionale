PROGRAM MAIN
    USE IOSTREAM
    USE RadioSondaggi
    IMPLICIT NONE

    CHARACTER (LEN=500) :: INPUT_FILENAME_A, INPUT_FILENAME_B, OUTPUT_FILENAME_A, OUTPUT_FILENAME_B, OUTPUT_FILENAME_DIFF
    NAMELIST /Files/ INPUT_FILENAME_A, INPUT_FILENAME_B, OUTPUT_FILENAME_A, OUTPUT_FILENAME_B, OUTPUT_FILENAME_DIFF
    CHARACTER (LEN=500) :: INTESTAZIONE_A, INTESTAZIONE_B
    CHARACTER (LEN=500) :: msg
    LOGICAL :: ERROR
    INTEGER :: IO, i,j, NumberOfLinesA, NumberOfLinesB, position1, position2, position3
    REAL :: h0A, p0A, T0A, h0B, p0B, T0B

    !----------------------------------------
    !VARIABILI ES1
    REAL :: z1, z2, p1, p2, T1, T2

    !----------------------------------------
    !VARIBILI ES2
    REAL, ALLOCATABLE :: DataA(:,:), DataB(:,:), GridA(:,:), GridB(:,:)
    REAL :: zMax, zMin, zGridBase, zGridTop
    INTEGER :: NumberOfGridPoints
    REAL :: z1A, z2A, p1A, p2A, T1A, T2A
    REAL :: z1B, z2B, p1B, p2B, T1B, T2B

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

    !Noti i dati al suolo, leggo i dati di pressione e temperatura e calcolo l'altitudine con la formula barometrica
    ! e scrivo su file le triplette (z,p,T)
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
    CALL OPEN_OUTPUT_FILE(29, OUTPUT_FILENAME_DIFF, ERROR)
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

    !----------------------------------------
    !Calcolo estremi dati sperimentali
    IF(DataA(1,1)<=DataB(1,1)) THEN
        zMin = DataB(1,1)
    ELSE
        zMin = DataA(1,1)
    END IF
    IF(DataA(NumberOfLinesA-1,1)>=DataB(NumberOfLinesB-1,1)) THEN
        zMax = DataB(NumberOfLinesB-1,1)
    ELSE
        zMax = DataA(NumberOfLinesA-1,1)
    END IF

    !Calcolo estremo inferiore griglia
    IF(abs(MOD(zMin,200.0)-0.0)>0.000001) THEN
        zGridBase = zMin - MOD(zMin,200.0) + 200.0
    ELSE
        zGridBase = zMin + 200
    END IF
    !Calcolo estremo superiore griglia zGridTop
    IF(abs(MOD(zMax,200.0)-0.0)>0.000001) THEN
        zGridTop = zMax - MOD(zMax,200.0)
    ELSE
        zGridTop = zMax
    END IF
    !Calcolo numero di punti griglia e alloco GridA e GridB
    NumberOfGridPoints = int(zGridTop - zGridBase)/200 + 1
    ALLOCATE(GridA(NumberOfGridPoints,3))
    ALLOCATE(GridB(NumberOfGridPoints,3))

    !----------------------------------------
    !Processing dati A

    !Cerco nel DataSet A il dato a quota massima inferiore a zGridBase e lo prendo come primo dato
    IF(zGridBase<DataA(1,1)) THEN
        T1A = T0A
        p1A = p0A
        z1A = h0A
        i  = 1
    ELSE 
        DO i=2,NumberOfLinesA-1
            IF(zGridBase<=DataA(i,1)) THEN
                T1A = DataA(i-1,3)
                p1A = DataA(i-1,2)
                z1A = DataA(i-1,1)
                EXIT
            END IF
        END DO
    END IF

    !Calcolo valori grandezze a zGridBase assegnando alla temperatura la media del substrato
    ! e calcolando la pressione con la formula barometrica
    z2A = zGridBase
    T2A = (T1A + DataA(i,3))/2
    CALL GetPressure(p2A,z2A,z1A,p1A,T2A,T1A)
    
    GridA(1,1) = z2A
    GridA(1,2) = p2A
    GridA(1,3) = T2A

    !----------------------------------------
    !Processing dati B
    !Cerco nel DataSet B il dato a quota massima inferiore a zGridBase
    IF(zGridBase<DataB(1,1)) THEN
        T1B = T0B
        p1B = p0B
        z1B = h0B
        i  = 1
    ELSE 
        DO i=2,NumberOfLinesB-1
            IF(zGridBase<=DataB(i,1)) THEN
                T1B = DataB(i-1,3)
                p1B = DataB(i-1,2)
                z1B = DataB(i-1,1)
                EXIT
            END IF
        END DO
    END IF
    !Calcolo valori grandezze a zGridBase assegnando alla temperatura la media del substrato
    ! e calcolando la pressione con la formula barometrica
    z2B = zGridBase
    T2B = (T1B + DataB(i,3))/2
    CALL GetPressure(p2B,z2B,z1B,p1B,T2B,T1B)
    
    GridB(1,1) = z2B
    GridB(1,2) = p2B
    GridB(1,3) = T2B

    !----------------------------------------
        DO i=1,NumberOfGridPoints-1
        z1A = GridA(i,1)
        p1A = GridA(i,2)
        T1A = GridA(i,3)
        z1B = GridB(i,1)
        p1B = GridB(i,2)
        T1B = GridB(i,3)

        z2A = z1A + 200
        z2B = z1B + 200
        DO j = 1,NumberOfLinesA
            IF(z2A<=DataA(j,1)) THEN
                T2A = (DataA(j,3)+DataA(j-1,3))/2
                EXIT
            END IF
        END DO
        DO j = 1,NumberOfLinesB
            IF(z2B<=DataB(j,1)) THEN
                T2B = (DataB(j,3)+DataB(j-1,3))/2
                EXIT
            END IF
        END DO
        CALL GetPressure(p2A,z2A,z1A,p1A,T2A,T1A)
        CALL GetPressure(p2B,z2B,z1B,p1B,T2B,T1B)

        GridA(i+1,1) = z2A
        GridA(i+1,2) = p2A
        GridA(i+1,3) = T2A
        GridB(i+1,1) = z2B
        GridB(i+1,2) = p2B
        GridB(i+1,3) = T2B
    END DO

    !----------------------------------------
    !Scrivo risultati

    DO i=1,NumberOfGridPoints
        WRITE(29,'(F10.2,1X,F10.2)') GridA(i,1), GridA(i,2)-GridB(i,2)
    END DO

    DEALLOCATE(DataA)
    DEALLOCATE(DataB)
    DEALLOCATE(GridA)
    DEALLOCATE(GridB)
    
    CLOSE(25)
    CLOSE(26)
    CLOSE(27)
    CLOSE(28)
    CLOSE(29)

END PROGRAM MAIN
