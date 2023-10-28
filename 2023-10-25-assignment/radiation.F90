PROGRAM RADIATION
   IMPLICIT NONE

   !DICHIARAZIONI
   CHARACTER(LEN=100) :: FILE_INPUT='radiation.dat', FILE_OUTPUT='results.dat', TempCharacter
   REAL, ALLOCATABLE, DIMENSION(:,:) :: Matrice
   REAL, ALLOCATABLE, DIMENSION(:) :: RadiazioneShortwave, RadiazioneInfrared
   REAL :: ShortwaveSomma, InfraredSomma
   INTEGER, DIMENSION(6) :: DataRiferimento
   INTEGER :: ValoreMancante = -999, TrashInteger=0, IO=0, i=0, NumeroRighe = 0, Riga = 0, NumeroDatiShortwave = 0
   INTEGER :: NumeroDatiInfrared = 0
   LOGICAL :: lexist, CheckData = .FALSE.


   !AUPERTURA FILE INGRESSO
!   WRITE(*,*) 'Inserire il nome del file da leggere.'
!   READ(*,*) FILE_INPUT
   OPEN(UNIT=20,FILE=FILE_INPUT,STATUS='OLD',ACTION='READ',IOSTAT=IO)
!   IF(IO/=0) STOP 'ERRORE APERTURA FILE INGRESSO'

   !APERTURA FILE USCITA
!   WRITE(*,*) 'Inserire il nome del file su cui scrivere.'
!  READ(*,*) FILE_OUTPUT
!   INQUIRE(FILE=FILE_OUTPUT,EXIST=lexist)
!   IF (.NOT.lexist)THEN
!      OPEN(UNIT=21,FILE=FILE_OUTPUT,STATUS='new',IOSTAT=IO)
!      IF (IO/=0) STOP 'ERRORE APERTURA FILE USCITA'
!   ELSE
!      WRITE(*,*)'ATTENZIONE file gia presente, sovrascrivo? (y/n)'
!      READ(*,*)TempCharacter
!      IF(TempCharacter=='y' .OR. TempCharacter=='Y')THEN
   OPEN(UNIT=21,FILE=FILE_OUTPUT,STATUS='replace',IOSTAT=IO)
!         IF (IO/=0) STOP 'ERRORE APERTURA FILE USCITA'
!      ELSE
!         STOP
!      END IF
!   END IF

   !LETTURA FILE
   READ(20,*) TempCharacter !SALTO INTESTAZIONE

   !CONTO RIGHE
   DO
      READ(20,*,IOSTAT=IO) TrashInteger
      IF(IO/=0 .AND. IO/=-1) STOP 'ERRORE LETTURA FILE'
      IF(IO==-1) EXIT
      NumeroRighe = NumeroRighe + 1
   END DO

   !ALLOCAZIONI
   ALLOCATE(Matrice(NumeroRighe,7))
   ALLOCATE(RadiazioneShortwave(NumeroRighe))
   ALLOCATE(RadiazioneInfrared(NumeroRighe))

   !RITORNO ALL'INIZIO DEL FILE
   REWIND(20)

   !LETTURA FILE
   READ(20,*) TempCharacter !SALTO INTESTAZIONE
   DO Riga = 1, NumeroRighe
      READ(20,*) Matrice(Riga,:)
   END DO

   !INIZIALIZZO DATA DI RIFERIMENTO
   DataRiferimento(1:6) = INT(Matrice(1,1:6))

   !CALCOLO RADIAZIONE SHORTWAVE E INFRARED TOTALE GIORNALIERA E SALVO IN ARRAY
   DO Riga = 1, NumeroRighe

      !CONTROLLO SE LA DATA LETTA E' DIVERSA DA QUELLA DI RIFERIMENTO
      CheckData = .FALSE.
      DO i = 1, 6
         IF ( Matrice(Riga,i) - REAL(DataRiferimento(i))>1.0D-5 ) EXIT
         CheckData = .TRUE.
      END DO

      !DATA LETTA DIVERSA DA DATA PRECEDENTE. SALVO DATI PRECEDENTI, AGGIORNO DATA DI RIFERIMENTO E RESETTO VARIABILI SOMMA
      IF(.NOT.CheckData) THEN
         RadiazioneShortwave(Riga) = ShortwaveSomma/REAL(NumeroDatiShortwave)
         RadiazioneInfrared(Riga) = InfraredSomma/REAL(NumeroDatiInfrared)
         DataRiferimento(1:6) = INT(Matrice(Riga,1:6)) !AGGIORNO DATA DI RIFERIMENTO
         ShortwaveSomma = 0.0
         InfraredSomma = 0.0
      END IF

      !RADIAZIONE SHORTWAVE. SOMMO DATO A SOMMA SHORTWAVE E AGGIORNO NUMERO DATI SHORTWAVE
      IF(Matrice(Riga,7) >= 0) THEN
         ShortwaveSomma = ShortwaveSomma + Matrice(Riga,7)
         NumeroDatiShortwave = NumeroDatiShortwave + 1

         !DATO MANANTE (-999). FACCIO INTERPOLAZIONE LINEARE
      ELSE IF ( INT(Matrice(Riga,7)) == ValoreMancante) THEN
         !FARE INTERPOLAZIONE LINEARE

      ELSE      !RADIAZIONE SHORTWAVE. SOMMO DATO A SOMMA INFRARED E AGGIORNO NUMERO DATI INFRARED
         InfraredSomma = InfraredSomma + Matrice(Riga,7)
         NumeroDatiInfrared = NumeroDatiInfrared + 1
      END IF

   END DO

   !SCRITTURA FILE
   DO Riga = 1, NumeroRighe
      WRITE(21,100) INT(Matrice(Riga,1:6)), RadiazioneShortwave(Riga), RadiazioneInfrared(Riga)
   END DO

   !CHIUSURA FILES
   CLOSE(20)
   CLOSE(21)

   !FORMATI
100 FORMAT(I3,I2,I5,3I3,2F5.2)

END PROGRAM RADIATION
