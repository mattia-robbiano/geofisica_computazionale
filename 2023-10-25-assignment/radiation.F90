PROGRAM RADIATION
   IMPLICIT NONE

   INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)
   CHARACTER(LEN=100) :: FILE_INPUT='radiation.dat', FILE_OUTPUT='results.dat', TempChar
   INTEGER :: IO, NumeroRighe=0, Riga=1, i=0,j=0, k=0
   REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:,:) :: Dati
   REAL(KIND=DBL) :: TotalShortwave=0, TotalInfrared=0, m=0, q=0
   INTEGER, DIMENSION(6) :: DataBuffer
   LOGICAL :: DataCheck, ESISTE

   !Chiedi all'utente file input
   WRITE(*,*) 'Fornire file input: '
   READ(*,*) FILE_INPUT
   OPEN(UNIT=20,FILE=FILE_INPUT,STATUS='OLD',ACTION='READ',IOSTAT=IO)
   IF (IO/=0) STOP 'Errore apertura file input'

   !Chiedi all'utente file output
   WRITE(*,*) 'Fornire file output: '
   READ(*,*) FILE_OUTPUT
   INQUIRE(FILE=FILE_OUTPUT,EXIST=ESISTE)

   !CONTROLLO SE ESISTE GIA
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
   !SCRIVO INTESTAZIONE FILE OUTPUT
   WRITE(21,*) 'Sum of daily short wave and infrared radiation in MJ/m^2'
   WRITE(21,'(A6,7X,A10,3X,A8)') 'Giorno ', 'short-wave ', 'infrared'

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

 !-----------------------------------------------------------------------------------------------------------------------  
 !PROCESSING DATI
   DataBuffer(1:6) = INT(Dati(1, 1:6)) !SALVO PRIMO GIORNO
   Riga = 1
   DO Riga = 1, NumeroRighe

      !CONTROLLO SE E' CAMBIATO IL GIORNO
      DataCheck = .TRUE.
      i=1
      DO i = 1, 3
         IF (INT(Dati(Riga, i)) /= DataBuffer(i)) THEN
            DataCheck = .FALSE.
            EXIT
         END IF
      END DO

    !-----------------------------------------------------------------------------------------------------------------------
    !INTERPOLAZIONE LINEARE
    !SE IL DATO E' NULLO INTERPOLO TRA I DUE VALORI NON NULLI PIU VICINI E SOSTITUISCO I VALORI NULLI CON I VALORI 
      
      i=Riga
      j=Riga
      interpolazione: IF(DataCheck .AND. Dati(Riga,7)<-998.0) THEN
         !CERCO PRIMO VALORE NON NULLO PRECEDENTE (ANCHE SE IN QUESTO CASO IL PRECEDENTE E' SICURAMENTE NON NULLO)
         DO
            IF(Dati(i,7)>-998.0) EXIT
            i=i-1
         END DO
         !CERCO PRIMO VALORE NON NULLO SUCCESSIVO
         DO
            IF(Dati(j,7)>-998.0) EXIT
            j=j+1
         END DO
         !SE HO PIU DI 4 VALORI NULLI ESCI
         IF(j-i>5) THEN
            WRITE(*,*) 'Troppi valori nulli tra', i+1+1, j-1+1 !TENGO CONTO CHE HO UNA RIGA DI INTESTAZIONE PER LETTURA A MANO DA FILE
            STOP
         END IF
         !ALTRIMENTI INTERPOLAZIONE LINEARE
         WRITE(*,101) 'Interpolazione tra', INT(Dati(i+1,1:6)), 'e', INT(Dati(j-1,1:6))
         m = (Dati(j,7)-Dati(i,7))/(j-i) !COEFFICIENTE ANGOLARE RETTA
         q = Dati(i,7)-m*i !INTERCETTA RETTA
         k=i+1 !TORNO AL PRIMO VALORE NULLO
         !SOSTITUISCO I VALORI NULLI CON I VALORI INTERPOLATI
         DO
            Dati(k,7) = m*k+q
            WRITE(*,'(I4,1X,F10.4)') k, Dati(k,7)
            IF(k==j-1) EXIT
            k=k+1
         END DO
      END IF interpolazione

      !FINE INTERPOLAZIONE
      !-----------------------------------------------------------------------------------------------------------------------

      !SE NON E CAMBIATO IL GIORNO SOMMO I VALORI SHORTWAVE E INFRARED
      IF(DataCheck .AND. Dati(Riga,7)>=0.0) THEN
         TotalShortwave = TotalShortwave + Dati(Riga,7)
      ELSE IF(DataCheck  .AND. Dati(Riga,7)<0.0) THEN
         TotalInfrared = TotalInfrared + Dati(Riga,7)

      !SE E' CAMBIATO IL GIORNO SCRIVO I VALORI E RESETTO LE VARIABILI
      ELSE
         !CONVERTO IN MJ/m^2
         TotalShortwave = TotalShortwave * 6e-5
         TotalInfrared = TotalInfrared * 6e-5
         DataBuffer(1:3) = INT(Dati(Riga,1:3)) !SALVO NUOVO GIORNO
         WRITE(21,100) DataBuffer(1:3), TotalShortwave, TotalInfrared
         TotalShortwave = 0
         TotalInfrared = 0
      END IF
   END DO

   !FINE PROCESSING DATI
   !-----------------------------------------------------------------------------------------------------------------------

 !CONVERTO IN MJ/m^2
   TotalShortwave = TotalShortwave * 6e-5
   TotalInfrared = TotalInfrared * 6e-5

 !SCRITTURA ULTIMO GIORNO
 WRITE(21,100) DataBuffer(1:3), TotalShortwave, TotalInfrared
 !CHIUSURA FILE
 CLOSE(20)
 CLOSE(21)
 !FORMATI
 !COMMENTO SULLA PRECISIONE DEI DATI NELLO PSEUDOCODICE
 100 FORMAT(I2,'/',I2,'/',I4,1X,F10.3,1X,F10.3)
 101 FORMAT(A,1X,I2,'/',I2,'/',I4,1X,I2,':',I2,':',I2,1X,A,1X,I2,'/',I2,'/',I4,1X,I2,':',I2,':',I2)

 !DEALLOCAZIONE
 DEALLOCATE(Dati)

END PROGRAM RADIATION
