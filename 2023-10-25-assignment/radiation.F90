PROGRAM RADIATION
   IMPLICIT NONE

   CHARACTER(LEN=100) :: FILE_INPUT='radiation.dat', FILE_OUTPUT='results.dat'
   INTEGER :: IO, NumeroRighe=0, Riga=1, i=0,j=0
   REAL, ALLOCATABLE, DIMENSION(:,:) :: Dati
   REAL :: TotalShortwave=0, TotalInfrared=0
   INTEGER, DIMENSION(6) :: DataBuffer
   LOGICAL :: DataCheck

   OPEN(UNIT=20,FILE=FILE_INPUT,STATUS='OLD',ACTION='READ',IOSTAT=IO)
   OPEN(UNIT=21,FILE=FILE_OUTPUT,STATUS='replace',IOSTAT=IO)

   !CONTO RIGHE
   READ(20,*)  !SALTO INTESTAZIONE
   DO
      READ(20,*,IOSTAT=IO)
      IF (IO==0)THEN
         NumeroRighe=NumeroRighe+1
      ELSE
         EXIT
      END IF
   END DO

   !ALLOCAZIONE MATRICE
   ALLOCATE(Dati(NumeroRighe, 7))

   !RITORNO ALL'INIZIO DATI
   REWIND(20)
   READ(20,*)

   !LETTURA DATI
   DO Riga = 1, NumeroRighe
      READ(20,*) Dati(Riga, :)
   END DO

   !PROCESSING DATI
   DataBuffer(1:6) = INT(Dati(1, 1:6))
   Riga = 1
   DO Riga = 1, NumeroRighe

!CONTROLLO DATA
      DataCheck = .TRUE.  ! Initialize DataCheck to TRUE
      i=1
      DO i = 1, 3
         IF (INT(Dati(Riga, i)) /= DataBuffer(i)) THEN
            DataCheck = .FALSE.
            EXIT
         END IF
      END DO

!SE IL DATO E NULLO INTERPOLO tra i primi due valori non nulli
      i=Riga
      j=Riga
      IF(DataCheck .AND. Dati(Riga,7)<-998.0) THEN

            DO
                IF(Dati(i,7)>-998.0) EXIT
                i=i-1
            END DO

            DO
                IF(Dati(j,7)>-998.0) EXIT
                j=j+1
            END DO

            IF(j-i>5) THEN 
                WRITE(*,*) 'Troppi valori nulli tra', i+1+1, j-1+1 !TENGO CONTO CHE HO UNA RIGA DI INTESTAZIONE
                STOP
            END IF
            Dati(Riga,7) = (Dati(i,7)+Dati(j,7))/2.0
      END IF

      IF(DataCheck .AND. Dati(Riga,7)>=0.0) THEN
         TotalShortwave = TotalShortwave + Dati(Riga,7)
      ELSE IF(DataCheck  .AND. Dati(Riga,7)<0.0) THEN
         TotalInfrared = TotalInfrared + Dati(Riga,7)
      ELSE
         DataBuffer(1:3) = INT(Dati(Riga,1:3))
         WRITE(21,100) DataBuffer(1:3), TotalShortwave, TotalInfrared
         TotalShortwave = 0
         TotalInfrared = 0
      END IF

   END DO
!SCRITTURA ULTIMO GIORNO
   WRITE(21,100) DataBuffer(1:3), TotalShortwave, TotalInfrared

   CLOSE(20)
   CLOSE(21)
100 FORMAT(I2,1X,I2,1X,I4,1X,F10.2,1X,F10.2)

END PROGRAM RADIATION
