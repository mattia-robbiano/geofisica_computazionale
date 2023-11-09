MODULE SUB_AUST

   USE PROCESSING
   CONTAINS
   
   SUBROUTINE MEDIA(MatriceTemperature,MatriceMedia, NumeroRighe,NumeroColonne,NumeroRigheFiltrata,NumeroColonneFiltrata)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NumeroRighe, NumeroColonne, NumeroRigheFiltrata, NumeroColonneFiltrata
      REAL(KIND =8), DIMENSION(NumeroRighe,NumeroColonne), INTENT(IN) :: MatriceTemperature
      REAL(KIND =8), DIMENSION(NumeroRigheFiltrata,NumeroColonneFiltrata), INTENT(OUT) :: MatriceMedia
      REAL(KIND =8), DIMENSION(9,9) :: MatriceFiltro

      !DEFINIZIONE FILTRO PER MEDIA
      MatriceFiltro = 1

      !MEDIA
      CALL CrossCorrelation2D(MatriceTemperature,MatriceMedia,MatriceFiltro,9,9)
      MatriceMedia = MatriceMedia/81

   END SUBROUTINE MEDIA

   SUBROUTINE FILTRO(MatriceTemperature,MatriceFiltrata, NumeroRighe, NumeroColonne)
      IMPLICIT NONE
      INTEGER :: Colonna, Riga !Necessarie per loop normalizzazione matrice
      INTEGER, INTENT(IN) :: NumeroRighe, NumeroColonne
      REAL(KIND =8), DIMENSION(NumeroRighe,NumeroColonne), INTENT(IN) :: MatriceTemperature(NumeroRighe,NumeroColonne)
      REAL(KIND =8), INTENT(OUT) :: MatriceFiltrata(NumeroRighe,NumeroColonne)
      REAL(KIND =8), DIMENSION(NumeroRighe+2,NumeroColonne+2) :: MatriceTemperaturePadded
      REAL(KIND =8), DIMENSION(3,3) :: MatriceFiltro

      !INIZIALIZZAZIONE VARIABILI
      MatriceFiltro = RESHAPE((/0.3,0.5,0.3,0.5,1.0,0.5,0.3,0.5,0.3/),(/3,3/))
      MatriceTemperaturePadded = 0.
      Colonna=0
      Riga=0
      
      !RIEMPO MatriceTemperaturePadded CON MatriceTemperature ORLATA DI ZERI
      CALL Padding(MatriceTemperature,NumeroRighe,NumeroColonne,MatriceTemperaturePadded,1,1)
      !CROSS CORRELATION CON FILTRO (spiegazione in pseudocodice)
      CALL CrossCorrelation2D(MatriceTemperaturePadded, MatriceFiltrata, MatriceFiltro,1,1)


      !MOLTIPLICO PER FILTRO OGNI BLOCCO 3X3 CON STRIDE 1 PRIMA SU X POI SU Y


      !NORMALIZZO I QUATTRO ANGOLI
      MatriceFiltrata(1,2:NumeroColonne-1) = MatriceFiltrata(1,2:NumeroColonne-1)/3.1
      MatriceFiltrata(NumeroRighe,2:NumeroColonne-1) = MatriceFiltrata(NumeroRighe,2:NumeroColonne-1)/3.1
      MatriceFiltrata(2:NumeroRighe-1,1) = MatriceFiltrata(2:NumeroRighe-1,1)/3.1
      MatriceFiltrata(2:NumeroRighe-1,NumeroColonne) = MatriceFiltrata(2:NumeroRighe-1,NumeroColonne)/3.1
      
      !NORMALIZZO I QUATTRO LATI
      MatriceFiltrata(1,1) = MatriceFiltrata(1,1)/2.3
      MatriceFiltrata(1,NumeroColonne) = MatriceFiltrata(1,NumeroColonne)/2.3
      MatriceFiltrata(NumeroRighe,1) = MatriceFiltrata(NumeroRighe,1)/2.3
      MatriceFiltrata(NumeroRighe,NumeroColonne) = MatriceFiltrata(NumeroRighe,NumeroColonne)/2.3
      
      !NORMALIZZO IL CORPO DELLA MATRICE
      DO Riga=2,NumeroRighe-1
         DO Colonna=2,NumeroColonne-1
            MatriceFiltrata(Riga,Colonna) = MatriceFiltrata(Riga,Colonna)/4.2
         END DO
      END DO
      
   END SUBROUTINE FILTRO
END MODULE SUB_AUST
