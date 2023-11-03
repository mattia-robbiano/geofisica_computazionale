PROGRAM RADIATION
   IMPLICIT NONE

   CHARACTER(LEN=100) :: FILE_INPUT='radiation.dat', FILE_OUTPUT='results.dat', TempChar
   INTEGER :: IO, NumeroRighe=0, Riga=1, i=0,j=0, k=0
   REAL, ALLOCATABLE, DIMENSION(:,:) :: Dati
   REAL :: TotalShortwave=0, TotalInfrared=0, m=0, q=0
   INTEGER, DIMENSION(6) :: DataBuffer
   LOGICAL :: DataCheck, ESISTE

   WRITE(*,*) 'Fornire file input: '
   READ(*,*) FILE_INPUT
   OPEN(UNIT=20,FILE=FILE_INPUT,STATUS='OLD',ACTION='READ',IOSTAT=IO)
   IF (IO/=0) STOP 'Errore apertura file input'

   WRITE(*,*) 'Fornire file output: '
   READ(*,*) FILE_OUTPUT
   INQUIRE(FILE=FILE_OUTPUT,EXIST=ESISTE)

   IF (ESISTE) THEN
      WRITE(*,*) 'Il file esiste gia, sovrascriverlo? (y/n)'
      READ(*,*) TempChar
      IF (TempChar=='N' .OR. TempChar=='n') STOP 'Termine programma'
      OPEN(UNIT=21,FILE=FILE_OUTPUT,STATUS='replace',IOSTAT=IO)
      IF (IO/=0) STOP 'Errore apertura file output'
   ELSE
      OPEN(UNIT=21,FILE=FILE_OUTPUT,STATUS='new',IOSTAT=IO)
      IF (IO/=0) STOP 'Errore apertura file output'
   END IF

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
   ALLOCATE(Dati(NumeroRighe, 7), STAT=IO)
   IF (IO/=0) STOP 'Errore allocazione matrice'

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

      !CONTROLLO SE E' CAMBIATO IL GIORNO
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
      interpolazione: IF(DataCheck .AND. Dati(Riga,7)<-998.0) THEN

         !CERCO PRIMO VALORE NON NULLO PRECEDENTE
         DO
            IF(Dati(i,7)>-998.0) EXIT
            i=i-1
         END DO
         !CERCO PRIMO VALORE NON NULLO SUCCESSIVO
         DO
            IF(Dati(j,7)>-998.0) EXIT
            j=j+1
         END DO
         !SE HO PIU DI 4 VALORI NULLI ESCO
         IF(j-i>5) THEN
            WRITE(*,*) 'Troppi valori nulli tra', i+1+1, j-1+1 !TENGO CONTO CHE HO UNA RIGA DI INTESTAZIONE PER LETTURA A MANO DA FILE
            STOP
         END IF

         !ALTRIMENTI INTERPOLAZIONE LINEARE
         WRITE(*,101) 'Interpolazione tra', INT(Dati(i+1,1:6)), 'e', INT(Dati(j-1,1:6))
         m = (Dati(j,7)-Dati(i,7))/(j-i)
         q = Dati(i,7)-m*i
         k=i+1
         DO
            Dati(k,7) = m*k+q
            WRITE(*,*) k, Dati(k,7)
            IF(k==j-1) EXIT
            k=k+1
         END DO

      END IF interpolazione

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

!CONVERTO IN MJ/m^2
   TotalShortwave = TotalShortwave * 6e-5
   TotalInfrared = TotalInfrared * 6e-5

!SCRITTURA ULTIMO GIORNO
   WRITE(21,100) DataBuffer(1:3), TotalShortwave, TotalInfrared

   CLOSE(20)
   CLOSE(21)
100 FORMAT(I2,1X,I2,1X,I4,1X,F10.4,1X,F10.4)
101 FORMAT(A,1X,I2,'/',I2,'/',I4,1X,I2,':',I2,':',I2,1X,A,1X,I2,'/',I2,'/',I4,1X,I2,':',I2,':',I2)

   DEALLOCATE(Dati)

END PROGRAM RADIATION
