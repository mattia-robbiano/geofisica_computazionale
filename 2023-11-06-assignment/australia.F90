PROGRAM Australia

   USE SUB_AUST_4

   IMPLICIT NONE

!DECLARATIONS
   INTEGER :: IO, NumeroColonne, NumeroRighe, NODATA_value, Riga=1, NumeroRigheFiltrata, NumeroColonneFiltrata
   REAL :: XCorner, YCorner, CellSize, Latitudine
   CHARACTER :: Trash
   REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: VettoreTemperatureMedie(:)
   REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: MatriceTemperature(:,:), MatriceMedia(:,:), MatriceFiltrata(:,:)
   LOGICAL :: lexist

!---------------------------------------------------------------------------------------------------------------------
!IO SECTION

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


!---------------------------------------------------------------------------------------------------------------------
!LETTURE FILE

   !LETTURA INTESTAZIONE
   READ(20,*) Trash, NumeroColonne
   READ(20,*) Trash, NumeroRighe
   READ(20,*) Trash, XCorner
   READ(20,*) Trash, YCorner
   READ(20,*) Trash, CellSize
   READ(20,*) Trash, NODATA_value
   NumeroRigheFiltrata = NumeroRighe/9
   NumeroColonneFiltrata = NumeroColonne/9

   !ALLOCAZIONE MATRICE
   ALLOCATE(MatriceTemperature(NumeroRighe,NumeroColonne), VettoreTemperatureMedie(NumeroRighe), STAT=IO)
   IF(IO/=0) STOP 'ERRORE ALLOCAZIONE MATRICE'
   ALLOCATE(MatriceMedia(NumeroRighe,NumeroColonne),MatriceFiltrata(NumeroRigheFiltrata,NumeroColonneFiltrata), STAT=IO)
   IF(IO/=0) STOP 'ERRORE ALLOCAZIONE MATRICE'

   !LETTURA MATRICE
   DO Riga = 1, NumeroRighe
      READ(20,*) MatriceTemperature(Riga,:)
   END DO

!---------------------------------------------------------------------------------------------------------------------
!ESECUZIONE

   !CALCOLO MATRICE MEDIATA E FILTRATA VIA SUBROUTINE
   CALL MEDIARE(MatriceTemperature, MatriceMedia, NumeroRighe, NumeroColonne)
   CALL FILTRA(MatriceTemperature, MatriceFiltrata, NumeroRighe, NumeroColonne, NumeroRigheFiltrata, NumeroColonneFiltrata)

   !CALCOLO MEDIE (IN CASO DI FILE CON NODATAVALUE CICLO ESPLICITO SU COLONNE PER ESCLUDERE I NODATAVALUE DALLA SOMMA)
   DO Riga = 1, NumeroRighe
      VettoreTemperatureMedie(Riga) = SUM(MatriceTemperature(Riga,:))/NumeroColonne
   END DO

!---------------------------------------------------------------------------------------------------------------------
!SCRITTURA SU FILE

   !SCRITTURA SU FILE Media per latitudine
   DO Riga = 1, NumeroRighe
      Latitudine = YCorner + (NumeroRighe-Riga)*CellSize
      WRITE(21,*) Latitudine, VettoreTemperatureMedie(Riga)
   END DO

   !SCRITTURA SU FILE Matrice Mediata
   MatriceMedia = MatriceTemperature
   DO Riga = 1, NumeroRighe
      WRITE(22,100) MatriceMedia(Riga,:)
   END DO

   !SCRITTURA SU FILE Matrice Filtrata
    DO Riga = 1, NumeroRigheFiltrata
        WRITE(23,*) MatriceFiltrata(Riga,:)
    END DO

   !---------------------------------------------------------------------------------------------------------------------

   !CHIUDO FILE
   CLOSE(UNIT=20)
   CLOSE(UNIT=21)

   !FORMATI
   100 FORMAT(886(2X,F7.3))
   101 FORMAT(98(2X,F7.3))

   !DEALLOCAZIONE MATRICE
   DEALLOCATE(MatriceTemperature)
   DEALLOCATE(VettoreTemperatureMedie)
   DEALLOCATE(MatriceMedia)

END PROGRAM Australia

