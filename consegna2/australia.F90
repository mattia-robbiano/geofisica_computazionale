PROGRAM Australia

   USE SUB_AUST
   USE PROCESSING
   USE IOSTREAM

   IMPLICIT NONE

   !DECLARATIONS
   INTEGER :: IO, NumeroColonne, NumeroRighe, NODATA_value, Riga=1, NumeroRigheMedia, NumeroColonneMedia
   REAL :: XCorner, YCorner, CellSize, Latitudine
   CHARACTER(LEN=1) :: Trash
   CHARACTER(LEN=100) :: Format100, Format101
   CHARACTER(LEN=10) :: NumeroColonneStringa, NumeroColonneMediaStringa 
   REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: VettoreTemperatureMedie
   REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: MatriceTemperature, MatriceMedia, MatriceFiltrata
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

   !ALLOCAZIONE MATRICI
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

   !CALCOLO MEDIE
   DO Riga = 1, NumeroRighe
      VettoreTemperatureMedie(Riga) = SUM(MatriceTemperature(Riga,:))/NumeroColonne
   END DO

   !---------------------------------------------------------------------------------------------------------------------
   !OUTPUT SECTION

   !SCRITTURA SU FILE temperature_latitudine
   DO Riga = 1, NumeroRighe
      Latitudine = YCorner + (NumeroRighe-Riga)*CellSize
      WRITE(21,'(F7.3,1X,F7.3)') Latitudine, VettoreTemperatureMedie(Riga)
   END DO

   !COMPONGO FORMATI COME STRINGHE
   WRITE(NumeroColonneStringa,'(I0)') NumeroColonne
   WRITE(NumeroColonneMediaStringa,'(I0)') NumeroColonneMedia
   Format100 = '(' // TRIM(NumeroColonneStringa) // '(2X,F7.3))'
   Format101 = '(' // TRIM(NumeroColonneMediaStringa)//'(2X,F7.3))'

   !SCRITTURA SU FILE temperature_filtrate
   DO Riga = 1, NumeroRighe
      WRITE(22,Format100) MatriceFiltrata(Riga,:)
   END DO

   !SCRITTURA SU FILE temperature_mediate
    DO Riga = 1, NumeroRigheMedia
        WRITE(23,Format101) MatriceMedia(Riga,:)
    END DO

   !---------------------------------------------------------------------------------------------------------------------

   !CHIUDO FILE
   CLOSE(UNIT=20)
   CLOSE(UNIT=21)
   CLOSE(UNIT=22)
   CLOSE(UNIT=23)

   !DEALLOCAZIONE MATRICE
   DEALLOCATE(MatriceTemperature)
   DEALLOCATE(VettoreTemperatureMedie)
   DEALLOCATE(MatriceMedia)

END PROGRAM Australia
