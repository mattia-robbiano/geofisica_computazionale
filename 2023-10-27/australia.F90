PROGRAM Australia
   IMPLICIT NONE

   !DECLARATIONS
   INTEGER :: IO, NumeroColonne, NumeroRighe, NODATA_value, Riga=1
   REAL :: XCorner, YCorner, CellSize, Latitudine 
   CHARACTER :: Trash
   REAL, ALLOCATABLE, DIMENSION(:) :: MatriceTemperature(:,:), VettoreTemperatureMedie(:)
   LOGICAL :: lexist

   !AUPERTURA FILE INGRESSO E USCITA
   OPEN(UNIT=20,FILE='monthly_temperature_sample.txt',STATUS='OLD',ACTION='READ',IOSTAT=IO)
   IF(IO/=0) STOP 'ERRORE APERTURA FILE INGRESSO'
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

   !LETTURA INTESTAZIONE
   READ(20,*) Trash, NumeroColonne
   READ(20,*) Trash, NumeroRighe
   READ(20,*) Trash, XCorner
   READ(20,*) Trash, YCorner
   READ(20,*) Trash, CellSize
   READ(20,*) Trash, NODATA_value

   !ALLOCAZIONE MATRICE
   ALLOCATE(MatriceTemperature(NumeroRighe,NumeroColonne), VettoreTemperatureMedie(NumeroRighe), STAT=IO)
   IF(IO/=0) STOP 'ERRORE ALLOCAZIONE MATRICE'

   !LETTURA MATRICE
   DO Riga = 1, NumeroRighe
    READ(20,*) MatriceTemperature(Riga,:)
   END DO

   !CALCOLO MEDIE E LE METTO IN UN VETTORE (IN CASO DI FILE CON NODATAVALUE CICLO ESPLICITO SU COLONNE PER ESCLUDERE I NODATAVALUE DALLA SOMMA)
    DO Riga = 1, NumeroRighe
        VettoreTemperatureMedie(Riga) = SUM(MatriceTemperature(Riga,:))/NumeroColonne
    END DO

    !SCRITTURA SU FILE
    DO Riga = 1, NumeroRighe
        Latitudine = YCorner + (NumeroRighe-Riga)*CellSize
        WRITE(21,*) Latitudine, VettoreTemperatureMedie(Riga)
    END DO

   !CHIUDO FILE
   CLOSE(UNIT=20)
   CLOSE(UNIT=21)


END PROGRAM Australia
