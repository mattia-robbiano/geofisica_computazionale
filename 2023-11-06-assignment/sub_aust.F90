MODULE SUB_AUST_4

   USE PROCESSING
   CONTAINS

   SUBROUTINE MEDIA(MatriceTemperature,MatriceFiltrata, NumeroRighe,NumeroColonne,NumeroRigheFiltrata,NumeroColonneFiltrata)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NumeroRighe, NumeroColonne, NumeroRigheFiltrata, NumeroColonneFiltrata
      REAL(KIND =8), DIMENSION(NumeroRighe,NumeroColonne), INTENT(IN) :: MatriceTemperature
      REAL(KIND =8), DIMENSION(NumeroRigheFiltrata,NumeroColonneFiltrata), INTENT(OUT) :: MatriceFiltrata
      REAL(KIND =8), DIMENSION(9,9) :: MatriceFiltro

      !DEFINIZIONE FILTRO PER MEDIA (I)
      MatriceFiltro = 1

      !MEDIA
      CALL CrossCorrelation2D(MatriceTemperature,MatriceFiltrata,MatriceFiltro,9,9)
      MatriceFiltrata = MatriceFiltrata/81

   END SUBROUTINE MEDIA

   SUBROUTINE FILTRO(MatriceTemperature,MatriceMedia, NumeroRighe, NumeroColonne)
      IMPLICIT NONE
      INTEGER :: Colonna, Riga
      INTEGER, INTENT(IN) :: NumeroRighe, NumeroColonne
      REAL(KIND =8), DIMENSION(NumeroRighe,NumeroColonne), INTENT(IN) :: MatriceTemperature(NumeroRighe,NumeroColonne)
      REAL(KIND =8), INTENT(OUT) :: MatriceMedia(NumeroRighe,NumeroColonne)
      REAL(KIND =8), DIMENSION(NumeroRighe+2,NumeroColonne+2) :: MatriceTemperaturePadded
      REAL(KIND =8), DIMENSION(3,3) :: MatriceFiltro

      !INIZIALIZZAZIONE VARIABILI
      Colonna=0
      Riga=0
      MatriceFiltro = RESHAPE((/0.3,0.5,0.3,0.5,1.0,0.5,0.3,0.5,0.3/),(/3,3/))
      MatriceTemperaturePadded = 0.
      
      CALL Padding(MatriceTemperature,NumeroRighe,NumeroColonne,MatriceTemperaturePadded,1,1)
      CALL CrossCorrelation2D(MatriceTemperaturePadded,MatriceMedia,MatriceFiltro,1,1)

   END SUBROUTINE FILTRO
END MODULE SUB_AUST_4
