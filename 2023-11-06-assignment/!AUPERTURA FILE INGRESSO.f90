   !AUPERTURA FILE INGRESSO 
   OPEN(UNIT=20,FILE='monthly_temperature_sample.txt',STATUS='OLD',ACTION='READ',IOSTAT=IO)
   IF(IO/=0) STOP 'ERRORE APERTURA FILE INGRESSO'

   !APERTURA FILE USCITA MEDIE PER LATITUDINE
   INQUIRE(FILE='medie.txt',EXIST=lexist)
   IF (.NOT.lexist)THEN
      OPEN(UNIT=21,FILE='medie.txt',STATUS='new',IOSTAT=IO)
      IF (IO/=0) STOP 'ERRORE APERTURA FILE USCITA'
   ELSE
      WRITE(*,*)'ATTENZIONE file gia presente, sovrascrivo? (y/n)'
      READ(*,*)Trash
      IF(Trash=='y' .OR. Trash=='Y')THEN
         OPEN(UNIT=21,FILE='medie.txt',STATUS='replace',IOSTAT=IO)
         IF (IO/=0) STOP 'ERRORE APERTURA FILE USCITA'
      ELSE
         STOP
      END IF
   END IF

   !APERTURA FILE USCITA MATRICE MEDIATA
   INQUIRE(FILE='MatriceMedia.txt',EXIST=lexist)
   IF (.NOT.lexist)THEN

      OPEN(UNIT=22,FILE='MatriceMedia.txt',STATUS='new',IOSTAT=IO)
      IF (IO/=0) STOP 'ERRORE APERTURA FILE USCITA'
   ELSE
      WRITE(*,*)'ATTENZIONE file gia presente, sovrascrivo? (y/n)'
      READ(*,*)Trash
      IF(Trash=='y' .OR. Trash=='Y')THEN
         OPEN(UNIT=22,FILE='MatriceMedia.txt',STATUS='replace',IOSTAT=IO)
         IF (IO/=0) STOP 'ERRORE APERTURA FILE USCITA'
      ELSE
         STOP
      END IF
   END IF

   !APERTURA FILE USCITA MATRICE FILTRATA
    INQUIRE(FILE='MatriceFiltrata.txt',EXIST=lexist)
    IF (.NOT.lexist)THEN

        OPEN(UNIT=23,FILE='MatriceFiltrata.txt',STATUS='new',IOSTAT=IO)
        IF (IO/=0) STOP 'ERRORE APERTURA FILE USCITA'
    ELSE
        WRITE(*,*)'ATTENZIONE file gia presente, sovrascrivo? (y/n)'
        READ(*,*)Trash
        IF(Trash=='y' .OR. Trash=='Y')THEN
            OPEN(UNIT=23,FILE='MatriceFiltrata.txt',STATUS='replace',IOSTAT=IO)
            IF (IO/=0) STOP 'ERRORE APERTURA FILE USCITA'
        ELSE
            STOP
        END IF
    END IF