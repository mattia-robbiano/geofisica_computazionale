PROGRAM Australia

   USE SUB_AUST
   USE PROCESSING
   USE IOSTREAM

   IMPLICIT NONE

   !DECLARATIONS
   INTEGER :: IO, NumeroColonne, NumeroRighe, NODATA_value, Riga=1, NumeroRigheMedia, NumeroColonneMedia
   REAL :: XCorner, YCorner, CellSize, Latitudine
   CHARACTER :: Trash
   REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: VettoreTemperatureMedie(:)
   REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: MatriceTemperature(:,:), MatriceMedia(:,:), MatriceFiltrata(:,:)
   LOGICAL :: ERROR

   !---------------------------------------------------------------------------------------------------------------------
   !IO SECTION

   CALL OPEN_INPUT_FILE(20, './monthly_temperature_sample.txt', ERROR)
   IF(ERROR) STOP
   CALL OPEN_OUTPUT_FILE(21, './temperature_latitudine.txt', ERROR)
   IF(ERROR) STOP
   CALL OPEN_OUTPUT_FILE(22, './temperature_filtrate.txt', ERROR)
   IF(ERROR) STOP
   CALL OPEN_OUTPUT_FILE(23, './temperature_mediate.txt', ERROR)
   IF(ERROR) STOP

   !---------------------------------------------------------------------------------------------------------------------
   !READING AND ALLOCATION SECTION

   !LETTURA INTESTAZIONE
   READ(20,*) Trash, NumeroColonne
   READ(20,*) Trash, NumeroRighe
   READ(20,*) Trash, XCorner
   READ(20,*) Trash, YCorner
   READ(20,*) Trash, CellSize
   READ(20,*) Trash, NODATA_value
   NumeroRigheMedia = NumeroRighe/9
   NumeroColonneMedia = NumeroColonne/9

   !ALLOCAZIONE MATRICE
   ALLOCATE(MatriceTemperature(NumeroRighe,NumeroColonne), VettoreTemperatureMedie(NumeroRighe), STAT=IO)
   IF(IO/=0) STOP 'ERRORE ALLOCAZIONE MATRICE'
   ALLOCATE(MatriceFiltrata(NumeroRighe,NumeroColonne),MatriceMedia(NumeroRigheMedia,NumeroColonneMedia), STAT=IO)
   IF(IO/=0) STOP 'ERRORE ALLOCAZIONE MATRICE'

   !LETTURA MATRICE
   DO Riga = 1, NumeroRighe
      READ(20,*) MatriceTemperature(Riga,:)
   END DO

   !---------------------------------------------------------------------------------------------------------------------
   !EXECUTION SECTION

   !CALCOLO MATRICE MEDIATA E FILTRATA VIA SUBROUTINE
   CALL FILTRO(MatriceTemperature, MatriceFiltrata, NumeroRighe, NumeroColonne)
   CALL MEDIA(MatriceTemperature, MatriceMedia, NumeroRighe, NumeroColonne, NumeroRigheMedia, NumeroColonneMedia)

   !CALCOLO MEDIE (IN CASO DI FILE CON NODATAVALUE CICLO ESPLICITO SU COLONNE PER ESCLUDERE I NODATAVALUE DALLA SOMMA)
   DO Riga = 1, NumeroRighe
      VettoreTemperatureMedie(Riga) = SUM(MatriceTemperature(Riga,:))/NumeroColonne
   END DO

   !---------------------------------------------------------------------------------------------------------------------
   !OUTPUT SECTION

   !SCRITTURA SU FILE temperature_latitudine
   DO Riga = 1, NumeroRighe
      Latitudine = YCorner + (NumeroRighe-Riga)*CellSize
      WRITE(21,*) Latitudine, VettoreTemperatureMedie(Riga)
   END DO

   !SCRITTURA SU FILE temperature_filtrate
   DO Riga = 1, NumeroRighe
      WRITE(22,100) MatriceFiltrata(Riga,:)
   END DO

   !SCRITTURA SU FILE temperature_mediate
    DO Riga = 1, NumeroRigheMedia
        WRITE(23,101) MatriceMedia(Riga,:)
    END DO

   !---------------------------------------------------------------------------------------------------------------------

   !CHIUDO FILE
   CLOSE(UNIT=20)
   CLOSE(UNIT=21)
   CLOSE(UNIT=22)
   CLOSE(UNIT=23)

   !FORMATI
   100 FORMAT(886(2X,F7.3))
   101 FORMAT(98(2X,F7.3))

   !DEALLOCAZIONE MATRICE
   DEALLOCATE(MatriceTemperature)
   DEALLOCATE(VettoreTemperatureMedie)
   DEALLOCATE(MatriceMedia)

END PROGRAM Australia
